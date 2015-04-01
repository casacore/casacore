//# tIncrementalStMan2.cc: Test program for the IncrementalStMan storage manager
//# Copyright (C) 2014
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
//# $Id: tIncrementalStMan.cc 21451 2014-06-10 07:48:08Z gervandiepen $

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/IncrStManAccessor.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the IncrementalStMan storage manager
// </summary>

// This program tests the IncrementalStMan storage manager, especially the
// get and put functions.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// First build a description.
uInt makeTab (uInt bucketSize)
{
  Table tab;
  DataManager::registerCtor ("IncrementalStMan",
                             IncrementalStMan::makeObject);
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<Bool>("c1"));	
  td.addColumn (ScalarColumnDesc<Bool>("c2"));	
  // Now create a new table from the description.
  SetupNewTable newtab("tIncrementalStMan2_tmp.data", td, Table::New);
  // Create a storage manager for it.
  IncrementalStMan sm1 ("ISM", bucketSize, False);
  StandardStMan sm2 ("SSM");
  newtab.bindColumn ("c1", sm1);
  newtab.bindColumn ("c2", sm2);
  tab = Table (newtab, 100000);
  ScalarColumn<Bool> c1(tab,"c1");
  ScalarColumn<Bool> c2(tab,"c2");
  for (uInt i=0; i<tab.nrow(); ++i) {
    c1.put (i, True);
    c2.put (i, True);
  }
  AlwaysAssertExit (allEQ(c1.getColumn(), True));
  return tab.nrow();
}

// Check if all rows are equal.
void checkTab()
{
  Table tab("tIncrementalStMan2_tmp.data");
  ScalarColumn<Bool> c1(tab,"c1");
  ScalarColumn<Bool> c2(tab,"c2");
  Vector<Bool> a1 = c1.getColumn();
  Vector<Bool> a2 = c2.getColumn();
  for (uInt i=0; i<a1.size(); ++i) {
    if (a1[i] != a2[i]) {
      cout << "mismatch at row " << i << ' '<<a1[i]<<' '<<a2[i]<<endl;
      AlwaysAssertExit (False);
    }
  }
}

// Update some rows by setting them to False.
void updateTab (uInt step)
{
  if (step > 0) {
    Table tab("tIncrementalStMan2_tmp.data", Table::Update);
    ROIncrementalStManAccessor acc(tab, "ISM");
    ScalarColumn<Bool> c1(tab,"c1");
    ScalarColumn<Bool> c2(tab,"c2");
    cout << "updateTab step=" << step << endl;
    for (uInt i=step; i<tab.nrow(); i+=step) {
      if (i == 61699  ||  i==61699-781) {
        c1.put (i, False);
      } else {
        c1.put (i, False);
      }
      c2.put (i, False);
    }
    acc.showIndexStatistics (cout);
    acc.showBucketLayout (cout);
  }
  checkTab();
}

int main (int argc, const char* argv[])
{
  uInt bucketSize = 100;
  if (argc > 1) {
    istringstream istr(argv[1]);
    istr >> bucketSize;
  }
  try {
    uInt nrow = makeTab (bucketSize);
    checkTab();
    // Now update some rows.
    // Do it in the middle, so ISM has to split buckets.
    uInt step = 2;
    for (uInt i=0; i<16; ++i) {
      updateTab (nrow/step);
      step *= 2;
    }
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
