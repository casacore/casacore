//# TableParseTableList.h: Lists of tables used in a TaQL query
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

#ifndef TABLES_TABLEPARSETABLELIST_H
#define TABLES_TABLEPARSETABLELIST_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declarations
  class TableParseQuery;

  
  // <summary>
  // Class binding a shorthand to a table name.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis>
  // This class is used by TableParse to associate a Table object and its
  // shorthand name (as used in TaQL).
  // </synopsis>

  class TableParsePair
  {

  public:
    // Default constructor for container class.
    TableParsePair();

    // Associate the table and the shorthand.
    // The full name and the table number (from $i) can also be given.
    TableParsePair (const Table& table, Int tabnr,
                    const String& name, const String& shorthand);

    // Test if shorthand matches. If also matches if the given shorthand is empty.
    Bool test (const String& str) const
      { return (str.empty()  ||  shorthand_p == str); }

    // Get the given table number (of $i tables in TempTables)
    Int tabnr() const
      { return tabnr_p; }

    // Get the given table name.
    const String& name() const
      { return name_p; }

    // Get the shorthand.
    const String& shorthand() const
      { return shorthand_p; }

    // Get table object.
    const Table& table() const
      { return table_p; }
    Table& table()
      { return table_p; }

    // Replace the Table object.
    void replaceTable (const Table& table)
      { table_p = table; }

  private:
    Int     tabnr_p;
    String  name_p;
    String  shorthand_p;
    Table   table_p;
  };



  // <summary>
  // Class containing two lists of TableParsePair objects.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <synopsis>
  // This class is used by TableParse to hold two lists of TableParsePair objects.
  // One list is for the tables given in the WITH clause, the other list is for
  // the other tables given in e.g. FROM or UPDATE.
  // It has functions to operate on the lists, usually by means of the
  // shorthand name.
  // </synopsis>

  class TableParseTableList
  {
  public:
    // Is the FROM table list empty?
    Bool empty() const
      { return itsFromTables.empty(); }

    // Get the FROM tables.
    std::vector<TableParsePair>& fromTables()
      { return itsFromTables; }
    
    // Return the first FROM table (which is usually the table to operate on).
    const Table& first() const
      { return itsFromTables.at(0).table(); }
    
    // Add a table to the list of tables with the given shorthand name.
    // The table can be given in a few ways:
    // <br>- As a sequence number to be taken from tempTables.
    // <br>- As a string giving the table name path.
    // <br>- As a subtable name (starting with ::) which will be looked up in
    //       the stack of query objects.
    // <br>- As a temporary table (from a nested query) given in ttab.
    // <br>- As the shorthand name of another table which will be looked up in
    //       the stack of query objects.
    Table addTable (Int tabnr, const String& name,
                    const Table& ttab,
                    const String& shorthand,
                    Bool addToFromList,
                    const std::vector<const Table*>& tempTables,
                    const std::vector<TableParseQuery*>& stack);


    // Replace the first Table object in the FROM list with the given one.
    void replaceTable (const Table& table);

    // Find a table for the given shorthand.
    // Optionally the WITH tables are searched as well.
    // If no shorthand is given, the first FROM table is returned (if there).
    // If not found, a null Table object is returned.
    static Table findTable (const String& shorthand, Bool doWith,
                            const std::vector<TableParseQuery*>& stack);

    // Try to find the Table for the given shorthand in the table list.
    Table findTable (const String& shorthand, Bool doWith) const;

    // Find the keyword given in the <src>name</src> parameter which is
    // split into its shorthand, column and/or keyword parts.
    // It fills parameter <src>keyName</src> with the last keyword part
    // and returns the TableRecord containing that keyword.
    // It is a helper function for handleSetKey, etc.
    // If update=True, rwKeywordSet() is used to ensure the table is updated.
    // An exception is thrown in case a name is not found.
    TableRecord& findKeyword (const String& name, String& keyName,
                              Bool update=True);

  private:
    //# Data members
    std::vector<TableParsePair> itsFromTables;
    std::vector<TableParsePair> itsWithTables;
  };

  
} //# NAMESPACE CASACORE - END

#endif
