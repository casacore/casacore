//# tStMan1.cc: Test program for performance of the various storage managers
//# Copyright (C) 2000,2001,2003
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
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for performance of the various storage managers.
// </summary>


// Create and fill a new table.
void newtab (uInt nrrow, const DataManager& stman, uInt flushnr)
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<uInt>("int1"));

  Timer timer;
  {
    // Now create a new table from the description.
    // Use copy constructor to test if it works fine.
    // (newtab and newtabcp have the same underlying object).
    SetupNewTable newtab("tStMan1_tmp.data", td, Table::New);
    // Create a storage manager for it.
    newtab.bindAll (stman);
    Table tab(newtab, nrrow);
    timer.show ("table rows creation ");
    ScalarColumn<uInt> int1 (tab, "int1");
    for (uInt i=0; i<nrrow; i++) {
      int1.put (i, i);
    }
    timer.show ("table put non-add   ");
  }
  timer.show ("total + destructor  ");
  timer.mark();
  {
    // Now create a new table from the description.
    // Use copy constructor to test if it works fine.
    // (newtab and newtabcp have the same underlying object).
    SetupNewTable newtab("tStMan1_tmp.data", td, Table::New);
    // Create a storage manager for it.
    newtab.bindAll (stman);
    Table tab(newtab);
    timer.show ("table empty creation");
    ScalarColumn<uInt> int1 (tab, "int1");
    for (uInt i=0; i<nrrow; i++) {
      tab.addRow();
      int1.put (i, i);
    }
    timer.show ("table put with add  ");
  }
  timer.show ("total + destructor  ");
  timer.mark();
  {
    // Now create a new table from the description.
    // Use copy constructor to test if it works fine.
    // (newtab and newtabcp have the same underlying object).
    SetupNewTable newtab("tStMan1_tmp.data", td, Table::New);
    // Create a storage manager for it.
    newtab.bindAll (stman);
    Table tab(newtab, nrrow);
    timer.show ("table rows creation ");
    ScalarColumn<uInt> int1 (tab, "int1");
    for (uInt i=0; i<nrrow; i++) {
      int1.put (i, i);
      if (i>0  &&  i%flushnr == 0) {
	tab.flush();
      }
    }
    timer.show ("table put/fl non-add");
  }
  timer.show ("total + destructor  ");
}

// Read the table back.
void readtab()
{
  Timer timer;
  {
    Table tab("tStMan1_tmp.data");
    uInt nrrow = tab.nrow();
    timer.show ("table open          ");
    ScalarColumn<uInt> int1 (tab, "int1");
    for (uInt i=0; i<nrrow; i++) {
      AlwaysAssertExit (int1(i) == i);
    }
    timer.show ("table get rows      ");
    Vector<uInt> vec = int1.getColumn();
    timer.show ("table get column    ");
    for (uInt i=0; i<nrrow; i++) {
      AlwaysAssertExit (vec(i) == i);
    }
    timer.show ("table check column  ");
  }
  timer.show ("total + destructor  ");
}

void doTest (uInt nrrow, const DataManager& stman, uInt flushnr)
{
  newtab (nrrow, stman, flushnr);
  readtab();
}

int main (int argc, const char* argv[])
{
  uInt nrrow = 100000;
  uInt bucketSize = 32768;
  uInt flushnr = 1000;
  if (argc > 1) {
    istringstream istr(argv[1]);
    istr >> nrrow;
  }
  if (argc > 2) {
    istringstream istr(argv[2]);
    istr >> bucketSize;
  }
  if (argc > 3) {
    istringstream istr(argv[3]);
    istr >> flushnr;
  }

  try {
    cout << "StManAipsIO" << endl;
    StManAipsIO st1;
    doTest (nrrow, st1, flushnr);
    cout << endl << "StandardStMan" << endl;
    StandardStMan st2(max(bucketSize,100u));
    doTest (nrrow, st2, flushnr);
    cout << endl << "IncrementalStMan" << endl;
    IncrementalStMan st3(max(bucketSize,1000u), False);
    doTest (nrrow, st3, flushnr);
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
