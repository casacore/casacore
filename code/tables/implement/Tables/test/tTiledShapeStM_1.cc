//# tTiledShapeStM_1.cc: Test program for performance of the TiledShapeStMan classes
//# Copyright (C) 2002
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
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/TiledShapeStMan.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/OS/Timer.h>
#include <aips/Exceptions/Error.h>
#include <aips/iostream.h>

// <summary>
// Test program for performance of the TiledShapeStMan class.
// </summary>

// This program tests the class TiledShapeStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


void readTable (const IPosition& shape, uInt nrrow)
{
  Table table("tTiledShapeStM_1_tmp.data");
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
         << nrrow << endl;
    return;
  }
  ROArrayColumn<Int> data (table, "Data");
  Array<Int> result;
  Array<Int> array(shape);
  indgen(array);
  Timer timer;
  for (uInt i=0; i<nrrow; i++) {
    data.get (i, result);
    if (! allEQ (array, result)) {
      cout << "mismatch in data row " << i << endl;
    }
    array += 1;
  }
  timer.show("Read/check");
}

void writeVar (const IPosition& shape, uInt nrrow)
{
  // Build the table description.
  TableDesc td ("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Int> ("Data", shape.nelements()));
  td.defineHypercolumn ("TSMExample",
			shape.nelements()+1,
			stringToVector ("Data"));
  
  // Now create a new table from the description.
  SetupNewTable newtab("tTiledShapeStM_1_tmp.data", td, Table::New);
  // Create a storage manager for it.
  TiledShapeStMan sm1 ("TSMExample", shape);
  newtab.bindAll (sm1);
  Table table(newtab);
  
  ArrayColumn<Int> data (table, "Data");
  Array<Int> array(shape);
  uInt i;
  indgen (array);
  Timer timer;
  try {
    for (i=0; i<nrrow; i++) {
      table.addRow();
      data.put (i, array);
      array += 1;
    }
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
  } 
  timer.show("Write     ");
}


int main (int argc, char* argv[])
{
  try {
    if (argc < 4) {
      cout << "Run as  tTiledShapeStM_1 nx ny nrow" << endl;
      return 0;
    }
    uInt nx, ny, nrow;
    {
      istrstream istr(argv[1]);
      istr >> nx;
    }
    {
      istrstream istr(argv[2]);
      istr >> ny;
    }
    {
      istrstream istr(argv[3]);
      istr >> nrow;
    }
    IPosition shape(2,nx,ny);
    writeVar (shape, nrow);
    readTable (shape, nrow);
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
