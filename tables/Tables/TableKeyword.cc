//# TableKeyword.cc: A keyword value representing a table
//# Copyright (C) 1996,1997,1998,2000,2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$


//# Includes
#include <casacore/tables/Tables/TableKeyword.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/OS/Path.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableKeyword::TableKeyword (const String& tableDescName)
: table_p         (new Table),
  tableDescName_p (tableDescName)
{}

TableKeyword::TableKeyword (const Table& table, const String& tableDescName)
: table_p         (new Table),
  attr_p          (table),
  tableDescName_p (tableDescName)
{
    // Only keep the Table object if not persistent.
    if (table.isMarkedForDelete()) {
        *table_p = table;
    } else {
        *table_p = Table();
    }
}

TableKeyword::TableKeyword (const TableKeyword& that)
: table_p         (new Table(*that.table_p)),
  attr_p          (that.attr_p),
  tableDescName_p (that.tableDescName_p)
{}

TableKeyword& TableKeyword::operator= (const TableKeyword& that)
{
    if (this != &that) {
        *table_p = *that.table_p;
	attr_p = that.attr_p;
	tableDescName_p = that.tableDescName_p;
    }
    return *this;
}

TableKeyword& TableKeyword::operator= (const Table& table)
{
    if (!conform (table)) {
	throw (TableError ("TableKeyword::operator=; non-conforming table"));
    }
    attr_p.set (table);
    // Only keep the Table object is not persistent.
    if (table.isMarkedForDelete()) {
        *table_p = table;
    } else {
        *table_p = Table();
    }
    return *this;
}

TableKeyword::~TableKeyword()
{
    delete table_p;
}


void TableKeyword::set (const String& name, const TableAttr& parentAttr)
{
    attr_p = parentAttr;
    // If a directory was stripped off, add the directory of the parent.
    // Otherwise use the name as such.
    attr_p.setName (Path::addDirectory (name, parentAttr.name()));
}

void TableKeyword::setRW()
{
    attr_p.setRW();
    if (! table_p->isNull()) {
	if (Table::isWritable (attr_p.name())) {
	    table_p->reopenRW();
	}
    }
}

Bool TableKeyword::isMultiUsed (Bool checkSubTables) const
{
    if (! table_p->isNull()) {
        return table_p->isMultiUsed (checkSubTables);
    }
    // The Table is closed immediately (thus not left open unnecessarily).
    Table tab(attr_p.name(), Table::Old);
    return tab.isMultiUsed (checkSubTables);
}

void TableKeyword::renameTable (const String& newParentName,
				const String& oldParentName)
{
    // Remove common part of old name from subtable name.
    String old = tableName (oldParentName);
    if (old != attr_p.name()) {
	attr_p.setName (Path::addDirectory (old, newParentName));
    }
    // Note that renaming subtables of a subtable is not necessary,
    // because they are always relative to the subtable (which
    // is not really renamed).
}


String TableKeyword::tableName (const String& parentName) const
{
    // Get the directory of the parent (with the trailing /).
    // If it is contained in this name, return name without it.
    return Path::stripDirectory (attr_p.name(), parentName);
}

Table TableKeyword::table (const TableLock* lockOptions) const
{
    // Return the table object if already open.
    if (! table_p->isNull()) {
      return *table_p;
    }
    // Open for write when needed and possible.
    Table::TableOption option = Table::Old;
    if (attr_p.openWritable()  &&  Table::isWritable (attr_p.name())) {
      option = Table::Update;
    }
    // Note that the opened table is not kept to avoid possible leaks
    // if a table keyword refers to the table itself (like the SORTED_TABLE
    // in an MS).
    return Table(attr_p.name(),
                 lockOptions  ?  *lockOptions : attr_p.lockOptions(),
                 option);
}

void TableKeyword::close() const
{
    *table_p = Table();
}

void TableKeyword::flush (Bool fsync) const
{
    if (attr_p.openWritable()) {
        if (!table_p->isNull()) {
	    table_p->flush (fsync, True);
	} else {
	    // The table is not open here, but might be open elsewhere.
	    // So only flush if open elsewhere, thus in the TableCache.
            PlainTable* ptab = PlainTable::tableCache() (attr_p.name());
	    if (ptab) {
	        ptab->flush (fsync, True);
	    }
	}
    }
}

Bool TableKeyword::conform (const TableKeyword& that) const
{
    // Only check for conformance if a description is fixed.
    if (isFixed()) {
        return conform (that.table());
    }
    return True;
}

Bool TableKeyword::conform (const Table& that) const
{
    if (isFixed()  &&  tableDescName_p != that.tableDesc().getType()) {
	return False;
    }
    return True;
}

} //# NAMESPACE CASACORE - END

