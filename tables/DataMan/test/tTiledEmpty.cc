//# tTiledEmpty.cc: Test creation of a tiled array without write or flush
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
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// This program tests if the cration of a tiled array succeeds if the
// array is written nor flushed.

void writeTable(const TSMOption&, bool writeFlush);
void readTable(const TSMOption&, bool written);


int main() {
  try {
    for (uInt i=0; i<2; ++i) {
      writeTable(TSMOption::Cache, i==0);
      readTable(TSMOption::Cache, i==0);
      readTable(TSMOption::Buffer, i==0);
      readTable(TSMOption::MMap, i==0);
      writeTable(TSMOption::Buffer, i==0);
      readTable(TSMOption::Cache, i==0);
      readTable(TSMOption::Buffer, i==0);
      readTable(TSMOption::MMap, i==0);
      writeTable(TSMOption::MMap, i==0);
      readTable(TSMOption::Cache, i==0);
      readTable(TSMOption::Buffer, i==0);
      readTable(TSMOption::MMap, i==0);
    }
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}

// First build a description.
void writeTable (const TSMOption& tsmOpt, bool write)
{
  IPosition tileShape(2,64,64);
  IPosition cubeShape(2,256,256);
  // Build the table description.
  TableDesc td ("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<float>  ("Data", 2));
  // Now create a new table from the description.
  SetupNewTable newtab("tTiledEmpty_tmp.data", td, Table::New);
  // Create a storage manager for it.
  TiledCellStMan sm1 ("TSMExample", tileShape);
  Table table(newtab, 0, False, Table::LittleEndian, tsmOpt);
  table.addRow();
  ArrayColumn<float> col(table, "Data");
  col.setShape (0, cubeShape);
  if (write) {
    Array<float> arr(cubeShape);
    arr = 0.;
    col.put (0, arr);
  }
}

void readTable(const TSMOption& tsmOpt, bool written)
{
  Table table("tTiledEmpty_tmp.data", Table::Old, tsmOpt);
  ArrayColumn<float> col(table, "Data");
  Array<float> arr;
  col.getColumn (arr);
  AlwaysAssertExit (arr.shape() == IPosition(3,256,256,1));
  if (written) {
    AlwaysAssertExit (allEQ(arr, float(0.)));
  }
}
