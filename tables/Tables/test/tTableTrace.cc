//# tTableTrace.cc: Test program for class TableTrace
//# Copyright (C) 2016
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

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>

// This program and script tTableTrace.run test the class TableTrace.


void testTable (rownr_t nrrow)
{
  {
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class Table";
    td.addColumn (ScalarColumnDesc<uint32_t>("ab"));
    td.addColumn (ArrayColumnDesc<int32_t>("ad"));
    // Now create a new table from the description.
    SetupNewTable newtab("tTableTrace_tmp.tab", td, Table::New);
    IncrementalStMan stman2;
    newtab.bindColumn ("ab", stman2);
    Table tab(newtab, TableLock(TableLock::PermanentLocking), nrrow);
    tab.tableInfo().setType ("testtype");
    tab.tableInfo().setSubType ("testsubtype");
    tab.tableInfo().readmeAddLine ("first readme line");
    tab.tableInfo().readmeAddLine ("second test readme line");
    // Write some data.
    ScalarColumn<uint32_t> ab1(tab, "ab");
    ArrayColumn<int32_t> ad(tab, "ad");
    for (rownr_t i=0; i<nrrow; i++) {
      ab1.put (i, i);
      ad.put (i, Vector<int32_t>(8,i/10));
    }
  }
  // Read data back.
  Table tab("tTableTrace_tmp.tab");
  ScalarColumn<uint32_t> ab1(tab, "ab");
  ArrayColumn<int32_t> ad(tab, "ad");
  Vector<uint32_t> abv = ab1.getColumn();
  Array<int32_t> adv = ad.getColumn();
  {
    // Get entire column (minus last cell).
    Vector<uint32_t> abv1 = ab1.getColumnRange (Slicer(IPosition(1,0),
                                                   IPosition(1,nrrow-1)));
    Array<int32_t> adv1 = ad.getColumnRange (Slicer(IPosition(1,0),
                                                IPosition(1,nrrow-1)));
  }
  {
    Vector<rownr_t> abv64(abv.size());
    convertArray (abv64, abv);
    Table rtab (tab(abv64));
    ScalarColumn<uint32_t> ab1(rtab,"ab");
    ArrayColumn<int32_t> ad(rtab,"ad");
    Vector<uint32_t> abv = ab1.getColumn();
    Array<int32_t> adv = ad.getColumn();
  }
}

int main()
{
  try {
    rownr_t nrrow = 5;
    testTable (nrrow);
  } catch (const std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
