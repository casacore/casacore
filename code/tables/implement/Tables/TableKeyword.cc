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
#include <tables/Tables/TableKeyword.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableAttr.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableError.h>
#include <casa/OS/Path.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableKeyword::TableKeyword (const String& tableDescName)
: table_p         (new Table),
  tableDescName_p (tableDescName)
{}

TableKeyword::TableKeyword (const Table& table, const String& tableDescName)
: table_p         (new Table(table)),
  attr_p          (table),
  tableDescName_p (tableDescName)
{}

// Do not copy an open table. In this way the copy of a keywordset in
// a TableDesc, does not create open Tables.
TableKeyword::TableKeyword (const TableKeyword& that)
: table_p         (new Table),
  attr_p          (that.attr_p),
  tableDescName_p (that.tableDescName_p)
{}

TableKeyword& TableKeyword::operator= (const TableKeyword& that)
{
    if (this != &that) {
	operator= (that.table());
    }
    return *this;
}

TableKeyword& TableKeyword::operator= (const Table& table)
{
    if (!conform (table)) {
	throw (TableError ("TableKeyword::operator=; non-conforming table"));
    }
    *table_p = table;
    attr_p.set (table);
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
    // When stripping off the parent table name changes the name,
    // the table is part of the parent table.
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

const Table& TableKeyword::table() const
{
    // Open the table when not opened yet.
    // Open for write when needed and possible.
    if (table_p->isNull()) {
	Table::TableOption option = Table::Old;
	if (attr_p.openWritable()  &&  Table::isWritable (attr_p.name())) {
	    option = Table::Update;
	}

	//// Backed out of it again as there seems to be a bug
       	*table_p = Table(attr_p.name(), attr_p.lockOptions(), option);
	////*table_p = Table(attr_p.name(), option);
    }
    return *table_p;
}

void TableKeyword::close() const
{
    *table_p = Table();
}

Bool TableKeyword::conform (const Table& that) const
{
    if (isFixed()  &&  tableDescName_p != that.tableDesc().getType()) {
	return False;
    }
    return True;
}

} //# NAMESPACE CASA - END

