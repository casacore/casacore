//# tTiledBool.cc: Test program for tiling a boolean column
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

#include <tables/Tables/TableDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/TiledShapeStMan.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// <summary>
// Test program for tiling a boolean column
// </summary>

// First build a description.
void writeTable (const TSMOption& tsmOpt, const IPosition& arrayShape,
                 const IPosition& tileShape)
{
  cout << "WriteTable ..." << endl;
  // Build the table description.
  TableDesc td ("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Bool> ("Flag", arrayShape.size(),
                                       ColumnDesc::FixedShape));
  // Now create a new table from the description.
  SetupNewTable newtab("tTiledBool_tmp.data", td, Table::New);
  // Create a storage manager for it.
  // Let the tile shape not fit integrally in the cube shape.
  TiledShapeStMan sm1 ("TSMExample", tileShape);
  newtab.setShapeColumn ("Flag", arrayShape);
  newtab.bindAll (sm1);
  Table table(newtab, 0, False, Table::LittleEndian, tsmOpt);

  ArrayColumn<Bool> flag (table, "Flag");
  Matrix<Bool> farray(arrayShape);
  Matrix<Bool> fresult(arrayShape);
  for (uInt i=0; i<101; i++) {
    for (uInt j=0; j<farray.nelements(); ++j) {
      farray.data()[j] = ((i+j)%(i+2) == 0);
    }
    table.addRow();
    flag.put (i, farray);
  }
  for (uInt i=0; i<table.nrow(); i++) {
    for (uInt j=0; j<farray.nelements(); ++j) {
      farray.data()[j] = ((i+j)%(i+2) == 0);
    }
    flag.get (i, fresult);
    if (! allEQ (farray, fresult)) {
      cout << "mismatch in flag row " << i << endl;
    }
  }
}

void readTable (const TSMOption& tsmOpt)
{
  Array<Bool> farray, fresult;
  Table table("tTiledBool_tmp.data", Table::Old, tsmOpt);
  cout << "Checking " << table.nrow() << " rows" << endl;
  ArrayColumn<Bool> flag (table, "Flag");
  for (uInt i=0; i<table.nrow(); i++) {
    flag.get (i, fresult);
    farray.resize (fresult.shape());
    for (uInt j=0; j<farray.nelements(); ++j) {
      farray.data()[j] = ((i+j)%(i+2) == 0);
    }
    if (! allEQ (farray, fresult)) {
      cout << "mismatch in flag row " << i << endl;
    }
  }
}

void testAll (const IPosition& arrayShape, const IPosition& tileShape)
{
  writeTable (TSMOption::Cache, arrayShape, tileShape);
  readTable (TSMOption::Cache);
  readTable (TSMOption::Buffer);
  readTable (TSMOption::MMap);
  writeTable (TSMOption::Buffer, arrayShape, tileShape);
  readTable (TSMOption::Cache);
  readTable (TSMOption::Buffer);
  readTable (TSMOption::MMap);
  writeTable (TSMOption::MMap, arrayShape, tileShape);
  readTable (TSMOption::Cache);
  readTable (TSMOption::Buffer);
  readTable (TSMOption::MMap);
}

int main()
{
  try {
    testAll (IPosition(2,4,256), IPosition(3,4,256,1));
    testAll (IPosition(2,4,256), IPosition(3,4,257,1));
    testAll (IPosition(2,4,256), IPosition(3,4,255,1));
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
