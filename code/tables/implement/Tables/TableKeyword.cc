//# TableKeyword.cc: A keyword value representing a table
//# Copyright (C) 1996,1997,1998
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
#include <aips/Tables/TableKeyword.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/OS/Path.h>

TableKeyword::TableKeyword (const String& tableDescName)
: openWritable_p  (False),
  table_p (new Table),
  tableDescName_p (tableDescName)
{}

TableKeyword::TableKeyword (const Table& table, const String& tableDescName)
: name_p  (table.tableName()),
  openWritable_p  (table.isWritable()),
  table_p (new Table(table)),
  tableDescName_p (tableDescName)
{}

TableKeyword::TableKeyword (const TableKeyword& that)
: name_p  (that.name_p),
  openWritable_p  (that.openWritable_p),
  table_p (new Table(*that.table_p)),
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
    name_p   = table.tableName();
    openWritable_p = table.isWritable();
    return *this;
}

TableKeyword::~TableKeyword()
{
    delete table_p;
}


void TableKeyword::set (const String& name, Bool openWritable,
			const String& parentTableName)
{
    openWritable_p = openWritable;
    // If a directory was stripped off, add the directory of the parent.
    // Otherwise use the name as such.
    name_p = Path::addDirectory (name, parentTableName);
}

void TableKeyword::setRW()
{
    openWritable_p = True;
    if (! table_p->isNull()) {
	if (Table::isWritable (name_p)) {
	    table_p->reopenRW();
	}
    }
}

void TableKeyword::renameTable (const String& newParentName,
				const String& oldParentName)
{
    // When stripping off the parent table name changes the name,
    // the table is part of the parent table.
    String old = tableName (oldParentName);
    if (old != name_p) {
	String newName = Path::addDirectory (old, newParentName);
	name_p = newName;
    }
    // Note that renaming subtables of a subtable is not necessary,
    // because they are always relative to the subtable (which
    // is not really renamed).
}


String TableKeyword::tableName (const String& parentTableName) const
{
    // Get the directory of the parent (with the trailing /).
    // If it is contained in this name, return name without it.
    return Path::stripDirectory (name_p, parentTableName);
}

const Table& TableKeyword::table() const
{
    // Open the table when not opened yet.
    // Open for write when needed and possible.
    if (table_p->isNull()) {
	Table::TableOption option = Table::Old;
	if (openWritable_p  &&  Table::isWritable (name_p)) {
	    option = Table::Update;
	}
	*table_p = Table(name_p, option);
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
