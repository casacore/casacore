//# tRefTable.cc: Test program for RefTable::addColumn
//# Copyright (C) 2010
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

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for RefTable::addColumn
// </summary>

void readTab (const String& tabName, uInt nrow, uInt ncol)
{
  cout << "read " << tabName << endl;
  Table tab(tabName);
  AlwaysAssertExit (tab.tableDesc().ncolumn() == ncol);
  AlwaysAssertExit (tab.nrow() == nrow);
  ScalarColumn<Int> ab(tab,"ab");
  ScalarColumn<Int> ac(tab,"ac");
  ScalarColumn<uInt> ad(tab,"ad");
  ScalarColumn<Int> ax(tab,"ax");
  for (uInt i=0; i<tab.nrow(); ++i) {
    AlwaysAssertExit (ab(i) == Int(i));
    AlwaysAssertExit (ac(i) == Int(i+1));
    AlwaysAssertExit (ad(i) == i+2);
    AlwaysAssertExit (ax(i) == Int(2*(i+1)));
  }
}

void makeTable()
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.comment() = "A test of class Table";
  td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
  td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
  td.addColumn (ScalarColumnDesc<DComplex>("ag"));
  // Now create a new table from the description.
  SetupNewTable newtab("tRefTable_tmp.data", td, Table::New);
  Table tab(newtab, 10);
  // Add a column.
  tab.addColumn (ScalarColumnDesc<Int> ("ac"));
  ScalarColumn<Int> ab(tab,"ab");
  ScalarColumn<Int> ac(tab,"ac");
  ScalarColumn<uInt> ad(tab,"ad");
  TableColumn ag(tab,"ag");
  for (Int i=0; i<10; i++) {
    ab.put (i, i);
    ac.put (i, i+1);
    ad.put (i, i+2);
    ag.put (i, ad);
  }
}

void makeRef()
{
  Table tab("tRefTable_tmp.data", Table::Update);
  Table reftab(tab.project(Block<String>(1, "ab")));
  AlwaysAssertExit (tab.tableDesc().ncolumn() == 4);
  AlwaysAssertExit (reftab.tableDesc().ncolumn() == 1);
  reftab.addColumn (ScalarColumnDesc<Int> ("ac"), False);
  AlwaysAssertExit (tab.tableDesc().ncolumn() == 4);
  AlwaysAssertExit (reftab.tableDesc().ncolumn() == 2);
  reftab.addColumn (ScalarColumnDesc<Int> ("ad"), True);
  AlwaysAssertExit (tab.tableDesc().ncolumn() == 4);
  AlwaysAssertExit (reftab.tableDesc().ncolumn() == 3);
  reftab.addColumn (ScalarColumnDesc<Int> ("ax"), True);
  AlwaysAssertExit (tab.tableDesc().ncolumn() == 5);
  AlwaysAssertExit (reftab.tableDesc().ncolumn() == 4);
  reftab.rename ("tRefTable_tmp.dataref", Table::New);
  reftab.flush();
  ScalarColumn<Int> ax(reftab, "ax");
  for (uInt i=0; i<reftab.nrow(); ++i) {
    ax.put (i, 2*(i+1));
  }
  readTab ("tRefTable_tmp.dataref", 10, 4);
}

int main()
{
  try {
    makeTable();
    makeRef();
    readTab ("tRefTable_tmp.data", 10, 5);
    readTab ("tRefTable_tmp.dataref", 10, 4);
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
