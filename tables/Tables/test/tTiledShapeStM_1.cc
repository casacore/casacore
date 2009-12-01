//# tTiledShapeStM_1.cc: Test program for performance of the TiledShapeStMan classes
//# Copyright (C) 2002,2003
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
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/TiledShapeStMan.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/Slicer.h>
#include <casa/OS/CanonicalConversion.h>
#include <casa/OS/LECanonicalConversion.h>
#include <casa/OS/Timer.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// <summary>
// Test program for performance of the TiledShapeStMan class.
// </summary>

// This program tests the class TiledShapeStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

// Outcomment and uncomment the correct typedef and define.
//typedef Int Type;
typedef Double Type;
#define ARRINIT indgen(array)
#define ARRINCR array += (Type)1

//typedef Bool Type;
//#define ARRINIT array = False
//#define ARRINCR array = !array

Bool readTable (Bool chk, const IPosition& shape, uInt nrrow)
{
  Bool ok = True;
  Table table("tTiledShapeStM_1_tmp.data");
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
	 << nrrow << endl;
    return False;
  }
  ROArrayColumn<Type> data (table, "Data");
  Array<Type> result;
  Array<Type> array(shape);
  ARRINIT;
  Timer timer;
  for (uInt i=0; i<nrrow; i++) {
    data.get (i, result);
    if (chk) {
      if (! allEQ (array, result)) {
	cout << "mismatch in data row " << i << endl;
	ok = False;
      }
      ARRINCR;
    }
  }
  timer.show("Read cell ");
  if (chk && ok) {
    cout << "  readVar checks successfull" << endl;
  }
  return ok;
}

Bool readSlices (Bool chk, const IPosition& shape, const IPosition& blc,
		 const IPosition& trc, const IPosition& inc, uInt nrrow)
{
  Bool ok = True;
  Table table("tTiledShapeStM_1_tmp.data");
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
	 << nrrow << endl;
    return False;
  }
  ROArrayColumn<Type> data (table, "Data");
  Array<Type> result;
  Array<Type> array(shape);
  ARRINIT;
  Slicer slicer(blc, trc, inc, Slicer::endIsLast);
  Array<Type> arraySlice(array(blc, trc, inc));
  Timer timer;
  for (uInt i=0; i<nrrow; i++) {
    data.getSlice (i, slicer, result);
    if (chk) {
      if (! allEQ (arraySlice, result)) {
	cout << "mismatch in data row " << i << endl;
	ok = False;
      }
      ARRINCR;
    }
  }
  timer.show("Read slice");
  if (chk && ok) {
    cout << "  readSlices checks successfull" << endl;
  }
  return ok;
}

Bool readColX (Bool chk, const IPosition& shape, const IPosition& blc,
	       const IPosition& trc, const IPosition& inc, uInt nrrow)
{
  Bool ok = True;
  Table table("tTiledShapeStM_1_tmp.data");
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
	 << nrrow << endl;
    return False;
  }
  ROArrayColumn<Type> data (table, "Data");
  Array<Type> result;
  uInt lastAxis = shape.nelements();
  IPosition shpa = shape.concatenate (IPosition(1,1));
  IPosition blca = blc.concatenate (IPosition(1,0));
  IPosition trca = trc.concatenate (IPosition(1,0));
  IPosition inca = inc.concatenate (IPosition(1,1));
  Array<Type> array(shpa);
  ARRINIT;
  Slicer slicer(blc, trc, inc, Slicer::endIsLast);
  IPosition bl, tr, ic;
  IPosition resShp = slicer.inferShapeFromSource (shape, bl, tr, ic);
  Timer timer;
  for (Int i=0; i<resShp(0); i++) {
    bl(0) = i;
    tr(0) = i;
    Slicer slc (bl, tr, ic, Slicer::endIsLast);
    data.getColumn (slc, result);
    if (chk) {
      IPosition end = result.shape() - 1;
      end(lastAxis) = 0;
      IPosition st = IPosition(end.nelements(), 0);
      for (uInt j=0; j<nrrow; j++) {
	st(lastAxis) = j;
	end(lastAxis) = j;
	blca(0) = i;
	trca(0) = i;
	Array<Type> arr (result(st, end));
	Array<Type> arraySlice(array(blca, trca, inca));
	if (! allEQ (arraySlice, arr)) {
	  cout << "mismatch in data row " << j << ", x " << i << endl;
	  ok = False;
	}
	ARRINCR;
      }
      ARRINIT;
    }
  }
  timer.show("Read col-x");
  if (chk && ok) {
    cout << "  readColX checks successfull" << endl;
  }
  return ok;
}

Bool readColY (Bool chk, const IPosition& shape, const IPosition& blc,
	       const IPosition& trc, const IPosition& inc, uInt nrrow)
{
  Bool ok = True;
  Table table("tTiledShapeStM_1_tmp.data");
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
	 << nrrow << endl;
    return False;
  }
  ROArrayColumn<Type> data (table, "Data");
  Array<Type> result;
  uInt lastAxis = shape.nelements();
  IPosition shpa = shape.concatenate (IPosition(1,1));
  IPosition blca = blc.concatenate (IPosition(1,0));
  IPosition trca = trc.concatenate (IPosition(1,0));
  IPosition inca = inc.concatenate (IPosition(1,1));
  Array<Type> array(shpa);
  ARRINIT;
  Slicer slicer(blc, trc, inc, Slicer::endIsLast);
  IPosition bl, tr, ic;
  IPosition resShp = slicer.inferShapeFromSource (shape, bl, tr, ic);
  Timer timer;
  for (Int i=0; i<resShp(1); i++) {
    bl(1) = i;
    tr(1) = i;
    Slicer slc (bl, tr, ic, Slicer::endIsLast);
    data.getColumn (slc, result);
    if (chk) {
      IPosition end = result.shape() - 1;
      end(lastAxis) = 0;
      IPosition st = IPosition(end.nelements(), 0);
      for (uInt j=0; j<nrrow; j++) {
	st(lastAxis) = j;
	end(lastAxis) = j;
	blca(1) = i;
	trca(1) = i;
	Array<Type> arr (result(st, end));
	Array<Type> arraySlice(array(blca, trca, inca));
	if (! allEQ (arraySlice, arr)) {
	  cout << "mismatch in data row " << j << ", x " << i << endl;
	  ok = False;
	}
	ARRINCR;
      }
      ARRINIT;
    }
  }
  timer.show("Read col-y");
  if (chk && ok) {
    cout << "  readColY checks successfull" << endl;
  }
  return ok;
}

Bool readCol (Bool chk, const IPosition& shape, const IPosition& blc,
	      const IPosition& trc, const IPosition& inc, uInt nrrow)
{
  Bool ok = True;
  Table table("tTiledShapeStM_1_tmp.data");
  if (table.nrow() != nrrow) {
    cout << "Table has " << table.nrow() << " rows; expected "
	 << nrrow << endl;
    return False;
  }
  ROArrayColumn<Type> data (table, "Data");
  Array<Type> result;
  uInt lastAxis = shape.nelements();
  IPosition shpa = shape.concatenate (IPosition(1,1));
  IPosition blca = blc.concatenate (IPosition(1,0));
  IPosition trca = trc.concatenate (IPosition(1,0));
  IPosition inca = inc.concatenate (IPosition(1,1));
  Array<Type> array(shpa);
  ARRINIT;
  Slicer slicer(blc, trc, inc, Slicer::endIsLast);
  Array<Type> arraySlice(array(blca, trca, inca));
  Timer timer;
  data.getColumn (slicer, result);
  if (chk) {
    IPosition end = result.shape() - 1;
    end(lastAxis) = 0;
    IPosition st = IPosition(end.nelements(), 0);
    for (uInt i=0; i<nrrow; i++) {
      st(lastAxis) = i;
      end(lastAxis) = i;
      Array<Type> arr (result(st, end));
      if (! allEQ (arraySlice, arr)) {
	cout << "mismatch in data row " << i << endl;
	ok = False;
      }
      ARRINCR;
    }
  }
  timer.show("Read col  ");
  if (chk && ok) {
    cout << "  readCol checks successfull" << endl;
  }
  return ok;
}

void writeVar (Bool chk, const IPosition& shape,
	       const IPosition& tileShape, uInt nrrow)
{
  // Build the table description.
  TableDesc td ("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Type> ("Data", shape.nelements()));
  td.defineHypercolumn ("TSMExample",
			shape.nelements()+1,
			stringToVector ("Data"));
  
  // Now create a new table from the description.
  SetupNewTable newtab("tTiledShapeStM_1_tmp.data", td, Table::New);
  // Create a storage manager for it.
  TiledShapeStMan sm1 ("TSMExample", tileShape);
  newtab.bindAll (sm1);
  Table table(newtab);
  
  ArrayColumn<Type> data (table, "Data");
  Array<Type> array(shape);
  uInt i;
  ARRINIT;
  Timer timer;
  try {
    for (i=0; i<nrrow; i++) {
      table.addRow();
      data.put (i, array);
      if (chk) {
	ARRINCR;
      }
    }
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
  }
  timer.show("Write     ");
}


int main (int argc, const char* argv[])
{
  Bool ok = True;
  try {
    if (argc < 5) {
      cout << "Run as  tTiledShapeStM_1 mode nrow nx ny [tx ty] [sx sy ex ey ix iy]" << endl;;
      cout << "Mode&1 = 1: check data read" << endl;
      cout << "    &2 = 1: read cell slices" << endl;
      cout << "    &4 = 1: read column slices in X" << endl;  
      cout << "    &8 = 1: read column slices in Y" << endl;  
      cout << "    &16= 1: read column slices" << endl;  
      return 0;
    }
    uInt mode, nrow, nx, ny;
    {
      istringstream istr(argv[1]);
      istr >> mode;
    }
    {
      istringstream istr(argv[2]);
      istr >> nrow;
    }
    {
      istringstream istr(argv[3]);
      istr >> nx;
    }
    {
      istringstream istr(argv[4]);
      istr >> ny;
    }
    uInt tx = nx;
    if (argc >= 6) {
      istringstream istr(argv[5]);
      istr >> tx;
    }
    uInt ty = ny;
    if (argc >= 7) {
      istringstream istr(argv[6]);
      istr >> ty;
    }
    uInt sx = 0;
    if (argc >= 8) {
      istringstream istr(argv[7]);
      istr >> sx;
    }
    uInt sy = 0;
    if (argc >= 9) {
      istringstream istr(argv[8]);
      istr >> sy;
    }
    uInt ex = nx-1;
    if (argc >= 10) {
      istringstream istr(argv[9]);
      istr >> ex;
    }
    uInt ey = ny-1;
    if (argc >= 11) {
      istringstream istr(argv[10]);
      istr >> ey;
    }
    uInt ix = 1;
    if (argc >= 12) {
      istringstream istr(argv[11]);
      istr >> ix;
    }
    uInt iy = 1;
    if (argc >= 13) {
      istringstream istr(argv[12]);
      istr >> iy;
    }
    IPosition shape(2,nx,ny);
    IPosition tileShape(2,tx,ty);
    IPosition blc(2,sx,sy);
    IPosition trc(2,ex,ey);
    IPosition inc(2,ix,iy);
    cout << "mode:      " << mode << endl;
    cout << "nrow:      " << nrow << endl;
    cout << "shape:     " << shape << endl;
    cout << "tileShape: " << tileShape << endl;
    cout << "sei:       " << blc << trc << inc << endl;
    cout << ">>>" << endl;

    writeVar (mode%2==1, shape, tileShape, nrow);
    if (! readTable (mode%2==1, shape, nrow)) {
      ok = False;
    }
    if ((mode&2) == 2) {
      if (! readSlices (mode%2==1, shape, blc, trc, inc, nrow)) {
	ok = False;
      }
    }
    if ((mode&4) == 4) {
      if (! readColX (mode%2==1, shape, blc, trc, inc, nrow)) {
	ok = False;
      }
    }
    if ((mode&8) == 8) {
      if (! readColY (mode%2==1, shape, blc, trc, inc, nrow)) {
	ok = False;
      }
    }
    if ((mode&16) == 16) {
      if (! readCol (mode%2==1, shape, blc, trc, inc, nrow)) {
	ok = False;
      }
    }
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  cout << "<<<" << endl;
  if (!ok) {
    return 1;
  }
  return 0;                           // exit with success status
}
