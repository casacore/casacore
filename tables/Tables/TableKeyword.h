//# TableKeyword.h: A keyword value representing a table
//# Copyright (C) 1996,1997,1999,2000,2001,2002
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

#ifndef TABLES_TABLEKEYWORD_H
#define TABLES_TABLEKEYWORD_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Table;


// <summary>
// Keyword value representing a table
// </summary>

// <use visibility=local>

// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tTableRecord">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableRecord>TableRecord</linkto>
//   <li> <linkto class=Table>Table</linkto>
// </prerequisite>

// <synopsis>
// TableKeyword represents a record keyword field containing a table.
// It is used by class TableRecord, which in its turn is meant to be
// used by the Table class.
// It serves the following purposes:
// <ul>
// <li> A table is only opened on demand, i.e. when the keyword
//      is accessed for the first time. When opened, the function
//      closeTable makes it possible to close a table when not
//      needed anymore (provided the table is not used elsewhere).
//      It will automatically be reopened when used again.
// <li> A switch is maintained which indicates if the table
//      should be opened as readonly or read/write.
//      A table is opened as read/write when the switch is read/write and
//      when the table is writable. Otherwise it is opened as readonly.
//      When a parent table is read back, its TableKeyword's will be
//      read back and the switch will be set to the access-mode
//      (readonly or read/write) of the parent table.
//      When a new table is inserted, the access-mode is taken from the table.
// <li> When the parent table is reopened as read/write, the table in
//      this object will also be reopened as read/write (if the table is
//      writable).
// <li> When a TableKeyword gets written, only the table name will be
//      written. Reading it back will set the correct access-mode, while
//      the table will not be opened until necessary.
//      However, when reading a parent table back it is possible that it
//      is done from a different directory than where it was created.
//      Therefore the directory of the parent table is prepended to the
//      TableKeyword subtable name. Similarly, when written it is stripped off.
//      <br>E.g. parent table XX and subtable SUB are created in the working
//      directory WD. Reading back is done from another directory by
//      specifying WD/XX. WD will be prepended to SUB.
// </ul>
// </synopsis>

// <motivation>
// This class provides the extra functionality for keywords containing
// tables. This is needed because tables are much more complex entities
// than scalars or arrays.
// </motivation>

// <example>
// <srcblock>
// // Store a table in the keyword set.
// void someFunc (const Table& subTable)
// {
//     // Open the table and get access to the table keyword set.
//     Table table("table.data", Table::Update);
//     TableRecord& keyset = table.rwKeywordSet();
//     keyset.defineTable ("KeyTab", subTable);
// }
//
// // Open the table and get the table from keyword KeyTab.
// // It shows that this can be done in one statement.
// Table table("table.data");
// Table subTab = table.keywordSet().asTable ("KeyTab");
// </srcblock>
// </example>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableKeyword
{
public:
    // Construct a TableKeyword with the given tableDescName.
    // When the tableDescName is empty the keyword is variable structured.
    // Otherwise it is fixed structured, meaning that only tables with a
    // description of that name can be assigned to this keyword.
    TableKeyword (const String& tableDescName);

    // Construct a TableKeyword from a Table.
    // <br>
    // When the tableDescName is empty the keyword is variable structured.
    // Otherwise it is fixed structured, meaning that only tables with a
    // description of that name can be assigned to this keyword.
    TableKeyword (const Table& table, const String& tableDescName);

    // Copy constructor (full copy semantics).
    TableKeyword (const TableKeyword& that);

    // Assignment (leaves tableDescName_p untouched).
    // This is only possible when both objects conform.
    // <group>
    TableKeyword& operator= (const TableKeyword& that);
    TableKeyword& operator= (const Table& table);
    // </group>

    ~TableKeyword();

    // Set the name of the table and the writable switch.
    // This is used when reading back a keyword.
    void set (const String& name, const TableAttr& parentAttr);

    // Set the keyword to read/write access.
    // If the table is already open, it will be reopened with read/write
    // access if the table is writable.
    void setRW();

    // Is the table in use in another process?
    // If <src>checkSubTables</src> is set, it is also checked if
    // a subtable is used in another process.
    Bool isMultiUsed (Bool checkSubTables) const;

    // Get the name of the table.
    const String& tableName() const;

    // Get the name of the table relative to parent table.
    // <group>
    String tableName (const String& parentName) const;
    String tableName (const TableAttr& parentAttr) const
      { return tableName (parentAttr.name()); }
    // </group>

    // Get the table.
    // It will be opened when necessary.
    // If given, the lockOptions will be used instead of the ones in
    // the table attributes.
    Table table (const TableLock* lockOptions = 0) const;

    // Get the table attributes.
    const TableAttr& tableAttributes() const
      { return attr_p; }

    // Set the table attributes.
    void setTableAttributes (const TableAttr& attr)
      { attr_p = attr; }

    // Close the table.
    void close() const;

    // Flush and optionally fsync the table.
    void flush (Bool fsync) const;

    // Rename the table if its path contains the old parent table name.
    void renameTable (const String& newParentName,
		      const String& oldParentName);

    // Test if the table in other conforms this table keyword.
    // It conforms when this description name is blank or matches the
    // table description name of the other.
    // <group>
    Bool conform (const TableKeyword& that) const;
    Bool conform (const Table& that) const;
    // </group>

    // Has the table a fixed description name?
    // It has when its description name is not empty.
    Bool isFixed() const;

private:
    Table*    table_p;
    TableAttr attr_p;
    String    tableDescName_p;
};



inline const String& TableKeyword::tableName() const
{
    return attr_p.name();
}

inline Bool TableKeyword::isFixed() const
{
    return  (! tableDescName_p.empty());
}


} //# NAMESPACE CASACORE - END

#endif
