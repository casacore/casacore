//# tTableUtil.cc Test program for table utility functions
//# Copyright (C) 2022
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableUtil.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/iostream.h>

using namespace casacore;

// This program tests some table utility functions.

// Define the main table name.
const String mainName("tTableUtil_tmp/maindata");

// Create a table and various subtables.
void createTables()
{
  // Create a main table.
  TableDesc td("", "", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<Int>("RowNr"));
  Table tab(TableUtil::createTable(mainName, td, Table::New));
  // Create 3 subtables (in different directories).
  TableDesc std("", "", TableDesc::Scratch);
  std.addColumn (ScalarColumnDesc<Int>("col1"));
  SetupNewTable newtab2(mainName + "/SubTab2a", std, Table::New);
  Table subtab2(newtab2, 2);
  std.addColumn (ScalarColumnDesc<Int>("col2"));
  SetupNewTable newtab3(mainName + "/subdata", std, Table::New);
  Table subtab3(newtab3, 3);
  // Store one subtable as a keyword in the main table, the
  // other as a column keyword.
  tab.rwKeywordSet().defineTable ("SubTab2", subtab2);
  ScalarColumn<Int> col(tab, "RowNr");
  col.rwKeywordSet().defineTable ("SubTab3", subtab3);
  // Store another subtable as a keyword in SubTab3.
  std.addColumn (ScalarColumnDesc<Int>("col3"));
  SetupNewTable newtab4(mainName + "/subdata/sub4", std,
                        Table::New);
  Table subtab4(newtab4, 4);
  subtab3.rwKeywordSet().defineTable ("SubTab4", subtab4);
  {
    // Create a scratch table.
    Table stab(TableUtil::createTable(String(), td, Table::New));
    AlwaysAssertExit (stab.nrow() == 0);
  }
}

void readTables()
{
  // Reconstruct the main table.
  // Get the sub table from the keyword.
  Table tab = TableUtil::openTable(mainName);
  AlwaysAssertExit (tab.nrow() == 0);
  AlwaysAssertExit (tab.keywordSet().nfields() == 1);
  AlwaysAssertExit (tab.tableDesc().ncolumn() == 1);
  AlwaysAssertExit (tab.keywordSet().isDefined ("SubTab2"));

  Table subtab2 = tab.keywordSet().asTable ("SubTab2");
  AlwaysAssertExit (subtab2.nrow() == 2);
  AlwaysAssertExit (subtab2.tableDesc().ncolumn() == 1);
  ScalarColumn<Int> col(tab, "RowNr");
  Table subtab3 = col.keywordSet().asTable ("SubTab3");
  AlwaysAssertExit (subtab3.nrow() == 3);
  AlwaysAssertExit (subtab3.tableDesc().ncolumn() == 2);
  Table subtab4 = subtab3.keywordSet().asTable ("SubTab4");
  AlwaysAssertExit (subtab4.nrow() == 4);
  AlwaysAssertExit (subtab4.tableDesc().ncolumn() == 3);
}    

void testColons()
{
  {
    // Open the table using the :: syntax.
    Table subtab2(TableUtil::openTable(mainName + "::SubTab2"));
    AlwaysAssertExit (subtab2.nrow() == 2);
    AlwaysAssertExit (subtab2.tableDesc().ncolumn() == 1);
  }
  {
    // Create a subtable of the subtable. It will have 1 column and 10 rows.
    TableDesc td;
    td.addColumn(ScalarColumnDesc<String>("col2"));
    Table subtab4(TableUtil::createTable(mainName + "::SubTab2::SubSubTab4",
                                         td, Table::NewNoReplace, Table::Plain,
                                         StorageOption(), Record(), TableLock(), 10));
  }
  {
    // Open the subtable just created.
    Table tab(TableUtil::openTable(mainName + "::SubTab2::SubSubTab4"));
    AlwaysAssertExit (tab.nrow() == 10);
    AlwaysAssertExit (tab.tableDesc().ncolumn() == 1);
  }
  {
    // Do the same, but use slashes.
    Table tab(TableUtil::openTable(mainName + "/SubTab2a/SubSubTab4"));
    AlwaysAssertExit (tab.nrow() == 10);
    AlwaysAssertExit (tab.tableDesc().ncolumn() == 1);
  }
}

void testDelete()
{
  // Create and delete a subtable.
  {
    TableDesc td;
    Table tab(TableUtil::createTable(mainName + "::SubTab2::SubSubTab5",
                                     td, Table::NewNoReplace));
  }
  TableUtil::deleteTable (mainName + "::SubTab2::SubSubTab5");
  // Check it is deleted.
  Bool ok = False;
  try {
    Table tab(TableUtil::openTable(mainName + "::SubTab2::SubSubTab5"));
  } catch (const TableError& x) {
    ok = True;
    cout << "Expected exception: " << x.what() << endl;
  }
  AlwaysAssertExit (ok);
  {
    // Also check the keyword does not exist anymore.
    Table tab(TableUtil::openTable(mainName + "::SubTab2"));
    AlwaysAssertExit (! tab.keywordSet().isDefined("SubSubTab5"));
  }
  {
    // Do the same, but leave the created table open to make deletion fail.
    TableDesc td;
    Table tab(TableUtil::createTable(mainName + "::SubTab2::SubSubTab5",
                                     td, Table::NewNoReplace));
    ok = False;
    try {
      TableUtil::deleteTable (mainName + "::SubTab2::SubSubTab5");
    } catch (const TableError& x) {
      ok = True;
      cout << "Expected exception: " << x.what() << endl;
    }
    AlwaysAssertExit (ok);
  }
}

void testReplace()
{
  // Create a subtable.
  {
    TableDesc td;
    td.addColumn(ScalarColumnDesc<String>("col2"));
    Table tab(TableUtil::createTable(mainName + "::SubTab2::SubSubTab6",
                                     td, Table::NewNoReplace));
    AlwaysAssertExit (tab.nrow() == 0);
    AlwaysAssertExit (tab.tableDesc().ncolumn() == 1);
    AlwaysAssertExit (tab.tableDesc()[0].name() == "col2");
  }
  // Replace the subtable.
  {
    TableDesc td;
    td.addColumn(ScalarColumnDesc<String>("col1"));
    td.addColumn(ScalarColumnDesc<String>("col3"));
    Table tab(TableUtil::createTable(mainName + "::SubTab2::SubSubTab6",
                                     td, Table::New));
    AlwaysAssertExit (tab.nrow() == 0);
    AlwaysAssertExit (tab.tableDesc().ncolumn() == 2);
    AlwaysAssertExit (tab.tableDesc()[0].name() == "col1");
    AlwaysAssertExit (tab.tableDesc()[1].name() == "col3");
  }
}

void testErrors()
{
  // Do some erroneous createTable calls.
  TableDesc td;
  Bool ok = False;
  try {
    // SubTab22 does not exist
    TableUtil::createTable (mainName + "::SubTab22::SubSubTab4",
                            td, Table::New);
  } catch (const TableError& x) {
    ok = True;
    cout << "Expected exception: " << x.what() << endl;
  }
  AlwaysAssertExit (ok);
  ok = False;
  try {
    // main33data does not exist.
    TableUtil::createTable ("tTableUtil_tmp/main33data::SubTab2::SubSubTab4",
                            td, Table::NewNoReplace);
  } catch (const TableError& x) {
    ok = True;
    cout << "Expected exception: " << x.what() << endl;
  }
  AlwaysAssertExit (ok);
  ok = False;
  try {
    // SubSubTab4 already exists.
    TableUtil::createTable (mainName + "::SubTab2::SubSubTab4",
                            td, Table::NewNoReplace);
  } catch (const TableError& x) {
    ok = True;
    cout << "Expected exception: " << x.what() << endl;
  }
  AlwaysAssertExit (ok);
  ok = False;
  try {
    // Empty part given.
    TableUtil::createTable ("tTableUtil_tmp/main3data::::SubTab2::SubSubTab4",
                            td, Table::New);
  } catch (const TableError& x) {
    ok = True;
    cout << "Expected exception: " << x.what() << endl;
  }
  AlwaysAssertExit (ok);
}    

void testLayoutInfo (const String& tableName)
{
  cout << endl << "Test getLayout and tableInfo ..." << endl;
  // Get the description and #rows of the Table.
  TableDesc layout;
  cout << "TableUtil::getlayout #rows = "
       << TableUtil::getLayout (layout, tableName);
  layout.show (cout);
  cout << endl;
  TableInfo info(TableUtil::tableInfo (tableName));
  cout << "type = " << info.type() << endl;
  cout << "subtype = " << info.subType() << endl;
  cout << info.readme() << endl;
}

int main()
{
  try {
    // Create subdirectory with this name, which will be deleted by assay.
    Directory dir("tTableUtil_tmp");
    dir.create();
    createTables();
    readTables();
    testColons();
    testDelete();
    testReplace();
    testErrors();
    testLayoutInfo(mainName);
    testLayoutInfo(mainName + "::SubTab2");
  } catch (const std::exception& x) {
    cout << "Caught an exception : " << x.what() << endl;
    return 1;
  } 
  cout << "tTableUtil ended OK" << endl;
  return 0; 
}

