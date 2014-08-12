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

#include <tables/Tables/TableDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/IncrementalStMan.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>
#include <casa/sstream.h>

#include <casa/namespace.h>

// <summary>
// Test program for the IncrementalStMan storage manager
// </summary>

// This program tests the IncrementalStMan storage manager, especially the
// get and put functions.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// First build a description.
void makeTab (uInt bucketSize)
{
  Table tab;
  DataManager::registerCtor ("IncrementalStMan",
                             IncrementalStMan::makeObject);
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<Bool>("af"));	
  // Now create a new table from the description.
  SetupNewTable newtab("tIncrementalStMan2_tmp.data", td, Table::New);
  // Create a storage manager for it.
  IncrementalStMan sm1 ("ISM", bucketSize, False);
  newtab.bindAll (sm1);
  tab = Table (newtab, 100000);
  ScalarColumn<Bool> af(tab,"af");
  af.put (0, True);
}

// Check if all rows are True.
void checkTab()
{
  Table tab("tIncrementalStMan2_tmp.data");
  ScalarColumn<Bool> af(tab,"af");
  AlwaysAssertExit (allEQ(af.getColumn(), True));
}

int main (int argc, const char* argv[])
{
  uInt nr = 100;
  if (argc > 1) {
    istringstream istr(argv[1]);
    istr >> nr;
  }
  try {
    makeTab (nr);
    checkTab();
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
