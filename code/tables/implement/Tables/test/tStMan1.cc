//# tStMan1.cc: Test program for performance of the various storage managers
//# Copyright (C) 2000
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

#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/StandardStMan.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/Timer.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <strstream.h>

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
    ROScalarColumn<uInt> int1 (tab, "int1");
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

main (int argc, const char* argv[])
{
  uInt nrrow = 100000;
  uInt bucketSize = 32768;
  uInt flushnr = 1000;
  if (argc > 1) {
    istrstream istr(argv[1]);
    istr >> nrrow;
  }
  if (argc > 2) {
    istrstream istr(argv[2]);
    istr >> bucketSize;
  }
  if (argc > 3) {
    istrstream istr(argv[3]);
    istr >> flushnr;
  }

  try {
    cout << "StManAipsIO" << endl;
    doTest (nrrow, StManAipsIO(), flushnr);
    cout << endl << "StandardStMan" << endl;
    doTest (nrrow, StandardStMan (max(bucketSize,100u)), flushnr);
    cout << endl << "IncrementalStMan" << endl;
    doTest (nrrow, IncrementalStMan (max(bucketSize,1000u), False), flushnr);
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } end_try;
  return 0;                           // exit with success status
}
