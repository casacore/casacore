//# TableParseUtil.h: Convenience functions for TableParse classes
//# Copyright (C) 1994-2022
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

#ifndef TABLES_TABLEPARSEUTIL_H
#define TABLES_TABLEPARSEUTIL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declarations
  class TableParseQuery;
  
  // <summary>
  // Convenience functions for TableParse classes
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis>
  // This file contains several static helper functions for TableParse.
  // They handle splitting a name into its shorthand, column and/or keyword
  // parts. A name can be given as
  // <src>shorthand.column::key.subkey1.subkey2...</src>
  // where each part is optional.
  // It also has functions to find the given table, column and keyword.
  // </synopsis>

  namespace TableParseUtil
  {
    // Make a Table object for given name, seqnr or so.
    // If <src>alwaysOpen=False</src> the table will only be looked up,
    // but not opened if not found. This is meant for concatenated tables
    // in TaQLNodeHandler.
    Table getTable (Int tabnr, const String& name,
                    const Table& ftab,
                    const std::vector<const Table*>& tempTables,
                    const std::vector<TableParseQuery*>& stack,
                    Bool alwaysOpen=True);

    // Open the parent table of a subtable.
    Table openParentTable (const String& fullName,
                           const String& subTableName,
                           const std::vector<const Table*>& tempTables,
                           const std::vector<TableParseQuery*>& stack);

    // Split a name into its parts (shorthand, column and field names).
    // A name can be given as
    // <src>[shorthand][column][::key1.key2.key3...]</src> where the
    // square brackets indicate optional parts. Note that a single name given
    // before :: is interpreted as a shorthand unless preceded by a period.
    // <br>True is returned if the name contains a keyword part.
    // In that case fieldNames contains the keyword name and the possible
    // subfields. The possible shorthand and the column name are
    // filled in if it is a column keyword.
    // If the name contains a column, fieldNames is filled with  the subfields
    // of the column (for the case where the column contains records).
    // <br>If isKeyword is True, the first part of name is a keyword,
    // even if no :: is given.
    // If allowNoKey is True, a single :: is allowed, otherwise the name is invalid.
    // If the name is invalid, exceptions are only thrown if checkError=True.
    // Otherwise the name is treated as a normal name without keyword.
    Bool splitName (String& shorthand, String& columnName,
                    Vector<String>& fieldNames, const String& name,
                    Bool checkError, Bool isKeyword, Bool allowNoKey);

    // Define a field with the given data type in the Record.
    void setRecFld (RecordInterface& rec, const String& name,
                    const String& dtype, const ValueHolder& vh);

    // Get the type string. If empty, it is made from the given
    // data type.
    String getTypeString (const String& typeStr, DataType type);

    // Find the names of all stored columns in a table.
    Block<String> getStoredColumns (const Table& tab);

    // Make an array from the contents of a column in a subquery.
    TableExprNode getColSet (const Table& table);
  }

  
} //# NAMESPACE CASACORE - END

#endif
