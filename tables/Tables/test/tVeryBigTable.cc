//# tVeryBigTable.cc: Test program for a very large table
//# Copyright (C) 2019
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for a very large table.
// </summary>

// This program tests a very large (i.e., with a large number of rows)
// using the IncrementalStMan storage manager to keep the file sizes very small.

rownr_t nrowStep = 500000000;      /// 2**32 = 4294967296

void createTableISM()
{
  cout << "Creating ISM table with 10*" << nrowStep << " rows" << endl;
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<Int>("ad"));
  // Now create a new table from the description.
  SetupNewTable newtab("tVeryBigTable_tmp.ism", td, Table::New);
  // Create a storage manager for it.
  IncrementalStMan sm1 ("ISM", 256, False);
  newtab.bindAll (sm1);
  Table tab(newtab, 0);
  ScalarColumn<Int> ad(tab,"ad");
  rownr_t rownr = 0;
  for (Int i=0; i<10; i++) {
    tab.addRow (nrowStep);
    ad.put (rownr, i);
    rownr += nrowStep;
  }
  AlwaysAssertExit (tab.nrow() == 10 * nrowStep);
}

void createTableSSM()
{
  cout << "Creating SSM table with 10*" << nrowStep << " rows" << endl;
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<Int>("ad"));
  // Now create a new table from the description.
  SetupNewTable newtab("tVeryBigTable_tmp.ssm", td, Table::New);
  // Create a storage manager for it.
  StandardStMan sm1 ("SSM", 4*1024*1024);
  newtab.bindAll (sm1);
  Table tab(newtab, 0);
  ScalarColumn<Int> ad(tab,"ad");
  rownr_t rownr = 0;
  for (Int i=0; i<10; i++) {
    tab.addRow (nrowStep);
    ad.put (rownr, i);
    ad.put (rownr+1, i);
    rownr +=  nrowStep/2;
    ad.put (rownr-1, i);
    ad.put (rownr, i);
    ad.put (rownr+1, i);
    rownr +=  nrowStep/2;
    ad.put (rownr-2, i);
    ad.put (rownr-1, i);
  }
  AlwaysAssertExit (tab.nrow() == 10 * nrowStep);
}

void createTableTSM()
{
  cout << "Creating TSM table with 10*" << nrowStep << " rows" << endl;
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Int>("ad", IPosition(1,1)));
  // Now create a new table from the description.
  SetupNewTable newtab("tVeryBigTable_tmp.tsm", td, Table::New);
  // Create a storage manager for it.
  ///TiledColumnStMan sm1 ("TSM", IPosition(2,1,1024*1024));
  TiledShapeStMan sm1 ("TSM", IPosition(2,1,1024*1024));
  newtab.bindAll (sm1);
  Table tab(newtab, 0);
  ArrayColumn<Int> ad(tab,"ad");
  Vector<Int> vec(1);
  rownr_t rownr = 0;
  for (Int i=0; i<10; i++) {
    tab.addRow (nrowStep);
    vec[0] = i;
    ad.put (rownr, vec);
    ad.put (rownr+1, vec);
    rownr +=  nrowStep/2;
    ad.put (rownr-1, vec);
    ad.put (rownr, vec);
    ad.put (rownr+1, vec);
    rownr +=  nrowStep/2;
    ad.put (rownr-2, vec);
    ad.put (rownr-1, vec);
  }
  AlwaysAssertExit (tab.nrow() == 10 * nrowStep);
}

void readTable (const String& name, Int add)
{
  cout << "Reading some rows from ISM table with " << 10*nrowStep << " rows" << endl;
  Table tab(name);
  AlwaysAssertExit (tab.nrow() == 10 * nrowStep);
  ScalarColumn<Int> ad(tab,"ad");
  rownr_t rownr = 0;
  for (Int i=0; i<10; i++) {
    AlwaysAssertExit (ad(rownr) == i+add);
    AlwaysAssertExit (ad(rownr+1) == i);
    rownr +=  nrowStep/2;
    AlwaysAssertExit (ad(rownr-1) == i);
    AlwaysAssertExit (ad(rownr) == i+add);
    AlwaysAssertExit (ad(rownr+1) == i);
    rownr +=  nrowStep/2;
    AlwaysAssertExit (ad(rownr-2) == i);
    AlwaysAssertExit (ad(rownr-1) == i+add);
  }
}

void testSelection()
{
  cout << "Testing selection ..." << endl;
  {
    // Take a few rows as a RefTable and make it persistent.
    Vector<rownr_t> rows(30);
    uInt inx = 0;
    rownr_t rownr = 0;
    for (uInt i=0; i<10; ++i) {
      rows[inx++] = rownr;
      rownr +=  nrowStep/2;
      rows[inx++] = rownr;
      rownr +=  nrowStep/2;
      rows[inx++] = rownr-1;
    }
    Table tab("tVeryBigTable_tmp.ism");
    Table seltab = tab(rows);
    seltab.rename ("tVeryBigTable_tmp.sel", Table::New);
  }
  {
    // Open the RefTable and check the data.
    Table tab("tVeryBigTable_tmp.sel");
    ScalarColumn<Int> ad(tab,"ad");
    rownr_t rownr = 0;
    for (Int i=0; i<10; i++) {
      AlwaysAssertExit (ad(rownr) == i);
      rownr++;
      AlwaysAssertExit (ad(rownr) == i);
      rownr++;
      AlwaysAssertExit (ad(rownr) == i);
      rownr++;
    }
  }
}

void testUpdate()
{
  Table tab("tVeryBigTable_tmp.ism", Table::Update);
  ScalarColumn<Int> ad(tab,"ad");
  rownr_t rownr = 0;
  for (uInt i=0; i<10; ++i) {
    ad.put (rownr, i+100);
    rownr +=  nrowStep/2;
    ad.put (rownr, i+100);
    rownr +=  nrowStep/2;
    ad.put (rownr-1, i+100);
  }
}

int main()
{
  try {
    createTableISM();
    readTable("tVeryBigTable_tmp.ism", 0);
    ///createTableSSM();
    ///readTable("tVeryBigTable_tmp.ssm", 0);
    ///createTableTSM();
    testSelection();
    testUpdate();
    readTable("tVeryBigTable_tmp.ism", 100);
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
