//# tTableKeywords.cc Test program for the table keywords
//# Copyright (C) 1994,1995,1996,1997,2000,2002
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

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/iostream.h>
#include <unistd.h>


#include <casacore/casa/namespace.h>
// This program tests some aspects of the table keywords, mainly
// if writing them and reading them back works fine.
// It was originally written by Mark Wieringa to track down a problem.


// Create tables in a subdirectory which is created first.
void createTables()
{
    // Create subdirectory with this name, so assay will delete everything.
    Directory dir("tTableKeywords_tmp");
    dir.create();
    // Create a main table.
    TableDesc td("", "", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<Int>("RowNr"));
    SetupNewTable newtab("tTableKeywords_tmp/maindata", td, Table::New);
    Table tab(newtab, 1);
    // Create 3 subtables (in different directories).
    TableDesc std("", "", TableDesc::Scratch);
    std.addColumn (ScalarColumnDesc<Int>("SourceNr"));
    SetupNewTable newtab2("tTableKeywords_tmp/submdata", std, Table::New);
    Table subtab2(newtab2, 2);
    SetupNewTable newtab3("tTableKeywords_tmp/maindata/subdata", std,
			  Table::New);
    Table subtab3(newtab3, 3);
    // Store one subtable as a keyword in the main table, the
    // other as a column keyword.
    tab.rwKeywordSet().defineTable ("SubTab2", subtab2);
    ScalarColumn<Int> col(tab, "RowNr");
    col.rwKeywordSet().defineTable ("SubTab3", subtab3);
    // Store another subtable as a keyword in SubTab3.
    SetupNewTable newtab4("tTableKeywords_tmp/maindata/subdata/sub4", std,
			  Table::New);
    Table subtab4(newtab4, 4);
    subtab3.rwKeywordSet().defineTable ("SubTab4", subtab4);
}

void readTables (const String& name, Bool swap)
{
    // Reconstruct the main table.
    // Get the sub table from the keyword .
    Table tab(name, Table::Update);
    AlwaysAssertExit (tab.nrow() == 1);
    AlwaysAssertExit (tab.keywordSet().nfields() == 1);
    AlwaysAssertExit (tab.keywordSet().isDefined ("SubTab2"));

    Table subtab2 = tab.keywordSet().asTable ("SubTab2");
    AlwaysAssertExit (subtab2.nrow() == 2);
    ScalarColumn<Int> col(tab, "RowNr");
    Table subtab3 = col.keywordSet().asTable ("SubTab3");
    AlwaysAssertExit (subtab3.nrow() == 3);
    Table subtab4 = subtab3.keywordSet().asTable ("SubTab4");
    AlwaysAssertExit (subtab4.nrow() == 4);

    // Now swap the keyword sets.
    if (swap) {
	tab.rwKeywordSet().defineTable ("SubTab2", subtab3);
	col.rwKeywordSet().defineTable ("SubTab3", subtab2);
    }
}    

void renameTables (const String& newName, const String& oldName)
{
    Table tab(oldName, Table::Update);
    tab.rename (newName, Table::New);
    tab.flush();
    // Try to open the table with the old name (should fail).
    Bool excp = False;
    try {
	Table tab1(oldName);
    } catch (AipsError x) {
	excp = True;
    } 
    AlwaysAssertExit (excp);
    // Try to open the table with new name (should succeed).
    Table tab2(newName);
}

void copyTables (const String& newName, const String& oldName)
{
    Table tab(oldName);
    tab.copy (newName, Table::New);
    // Try to open the table with the old name (should succeed).
    Table tab1(oldName);
    // Try to open the table with new name (should succeed).
    Table tab2(newName);
}

void readFromOtherDir()
{
  {
    // Reconstruct the main table.
    // Get the sub table from the keyword .
    Table tab("main3data");
    AlwaysAssertExit (tab.nrow() == 1);
    AlwaysAssertExit (tab.keywordSet().nfields() == 1);
    AlwaysAssertExit (tab.keywordSet().isDefined ("SubTab2"));
    Table subtab3 = tab.keywordSet().asTable ("SubTab2");
    AlwaysAssertExit (subtab3.nrow() == 3);
    ScalarColumn<Int> col(tab, "RowNr");
    Table subtab2 = col.keywordSet().asTable ("SubTab3");
    AlwaysAssertExit (subtab2.nrow() == 2);
    Table subtab4 = subtab3.keywordSet().asTable ("SubTab4");
    AlwaysAssertExit (subtab4.nrow() == 4);
  }
  {
    // Open the table using the :: syntax.
    Table subtab3 (Table::openTable("main3data::SubTab2"));
    AlwaysAssertExit (subtab3.nrow() == 3);
  }
}    


int main()
{
    try {
	createTables();
	readTables ("tTableKeywords_tmp/maindata", False);
	renameTables ("tTableKeywords_tmp/main2data", 
		      "tTableKeywords_tmp/maindata");
	readTables ("tTableKeywords_tmp/main2data", False);
	copyTables ("tTableKeywords_tmp/main3data",
		    "tTableKeywords_tmp/main2data");
	readTables ("tTableKeywords_tmp/main3data", False);
	readTables ("tTableKeywords_tmp/main2data", False);
	// Go to the subdirectory to test if renaming and
	// reading back from there succeeds.
	AlwaysAssertExit (chdir ("tTableKeywords_tmp") == 0);
	readTables ("main2data", False);
	renameTables ("main4data", 
		      "main2data");
	readTables ("main3data", True);
	readTables ("main4data", False);
	readFromOtherDir();
    } catch (AipsError x) {
        cout << "Caught an exception : " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0; 
}

