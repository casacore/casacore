//# tConcatTable3.cc: Test program for the ConcatTable class
//# Copyright (C) 2008
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

#include <tables/Tables/TableDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ExprNode.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Containers/Block.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

// <summary>
// Test program for the ConcatTable class
// </summary>


// First build a description.
void createTable(const String& name, Int stval, Int nrrow)
{
  // Build the table description.
  TableDesc td;
  td.addColumn (ScalarColumnDesc<Int>("aint"));
  td.addColumn (ScalarColumnDesc<Float>("afloat"));
  // Now create a new table from the description.
  SetupNewTable newtab(name, td, Table::New);
  Table tab(newtab, nrrow);
  // Fill the table.
  ScalarColumn<Int>   icol(tab, "aint");
  ScalarColumn<Float> fcol(tab, "afloat");
  for (Int i=0; i<nrrow; ++i) {
    icol.put (i, i+stval);
    fcol.put (i, i+stval+1.);
  }
}

void checkTable (Int stval, uInt nrow)
///void checkTable (const Table& tab, uInt nkey, uInt nsubrow, Int stval,
///		 Bool reorder=True, uInt nrow=10)
{
  Table tab("tConcatTable_tmp.conctab");
  AlwaysAssertExit (tab.nrow() == nrow);
  /*
  AlwaysAssertExit (tab.keywordSet().nfields() == nkey);
  AlwaysAssertExit (tab.keywordSet().asInt("key1") == 1);
  AlwaysAssertExit (tab.keywordSet().asString("key2") == "abc");
  if (nkey == 3) {
    AlwaysAssertExit (tab.keywordSet().asTable("keysub").nrow() == nsubrow);
  }
  */
  ROScalarColumn<Int> aint(tab, "aint");
  ROScalarColumn<Float> afloat(tab,  "afloat");
  for (uInt i=0; i<tab.nrow(); i++) {
    AlwaysAssertExit (aint(i) == stval);
    AlwaysAssertExit (afloat(i) == stval+1.);
    ++stval;
  }
}

void concatTables()
{
  Block<String> names(3);
  names[0] = "tConcatTable3_tmp.tab1";
  names[1] = "tConcatTable3_tmp.tab2";
  names[2] = "tConcatTable3_tmp.tab3";
  Table concTab (names, Block<String>(), Table::Old, TSMOption(), "SUBDIR");
  concTab.rename ("tConcatTable_tmp.conctab", Table::New);
}

int main()
{
  try {
    createTable ("tConcatTable3_tmp.tab1", 0, 10);
    createTable ("tConcatTable3_tmp.tab2", 10, 20);
    createTable ("tConcatTable3_tmp.tab3", 30, 5);
    concatTables();
    checkTable (0, 35);
  } catch (AipsError x) {
    cout << "Exception caught: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
