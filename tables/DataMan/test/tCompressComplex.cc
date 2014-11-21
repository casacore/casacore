//# tCompressComplex.cc: Test program for class CompressComplex
//# Copyright (C) 2001,2002
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
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/CompressComplex.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary> Test program for class CompressComplex </summary>

// This program tests the virtual column engine CompressComplex.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// First build a description.
void writeData (Bool isSD, Bool autoScale)
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.comment() = "A test of class TableDesc";
  td.addColumn (ArrayColumnDesc<Int> ("target1", 3));
  td.addColumn (ArrayColumnDesc<Complex> ("source1"));
  td.addColumn (ArrayColumnDesc<Complex> ("source2","",
					  IPosition(3,2,3,4),
					  ColumnDesc::Direct));
  td.addColumn (ScalarColumnDesc<Float> ("scale1"));
  td.addColumn (ScalarColumnDesc<Float> ("offset1"));
  td.defineHypercolumn ("tileddata", 4, stringToVector("target1"));

  // Now create a new table from the description.
  SetupNewTable newtab("tCompressComplex_tmp.data", td, Table::New);
  // Create the virtual column engine with the scale factors
  // and bind the columns to them.
  if (isSD) {
    CompressComplexSD engine1("source1", "target1", "scale1", "offset1",
			      autoScale);
    newtab.bindColumn ("source1", engine1);
  } else {
    CompressComplex engine1("source1", "target1", "scale1", "offset1",
			    autoScale);
    newtab.bindColumn ("source1", engine1);
  }
  TiledShapeStMan tsm("tileddata", IPosition(4,2,3,4,4));
  newtab.bindColumn ("target1", tsm);
  Table tab(newtab, 10);

  // Fill the table via the virtual columns.
  ArrayColumn<Complex> source1 (tab, "source1");
  ArrayColumn<Complex> source2 (tab, "source2");
  ScalarColumn<Float> scale1 (tab, "scale1");
  ScalarColumn<Float> offset1 (tab,"offset1");

  Cube<Complex> arrf(IPosition(3,2,3,4));
  uInt i;
  i=16;
  for (uInt i2=0; i2<4; i2++) {
    for (uInt i1=0; i1<3; i1++) {
      arrf(0,i1,i2) = Complex(i,i+36);
      i += 12;
      arrf(1,i1,i2) = Complex(i,0);
      i += 12;
    }
  }
  for (i=0; i<10; i++) {
    if (!autoScale) {
      scale1.put (i, 2.);
      offset1.put (i, 20.);
    }
    if (i != 5) {
      source1.put (i, arrf);
    }
    source2.put (i, arrf);
    arrf += Complex(12*arrf.nelements(), 12*arrf.nelements());
  }
  // Write the 5th row in Slices.
  arrf -= Complex(5*12*arrf.nelements(), 5*12*arrf.nelements());
  source1.setShape (5, arrf.shape());
  for (i=0; i<3; i++) {
    source1.putSlice (5, Slicer(IPosition(3,0,i,0), IPosition(3,2,1,4)),
		      arrf(IPosition(3,0,i,0), IPosition(3,1,i,3)));
  }

  //# Do an erronous thing.
  //# However, this fails to run on Linux (so outcommented).
  ///  SetupNewTable newtab2("tCompressComplex_tmp.dat2", td, Table::Scratch);
  ///  newtab2.bindColumn ("source2", engine1);
  ///  try {
  ///    Table tab2(newtab2, 10);                // bound to incorrect column
  ///  } catch (AipsError x) {
  ///    cout << x.getMesg() << endl;
  ///  } 
}

Bool checkData (Bool autoScale)
{
  Bool ok = True;
  // Read back the table.
  Table tab("tCompressComplex_tmp.data");
  ArrayColumn<Complex> source1 (tab, "source1");
  ArrayColumn<Complex> source2 (tab, "source2");
  ArrayColumn<Int> target1 (tab, "target1");
  ScalarColumn<Float> scale1 (tab, "scale1");
  ScalarColumn<Float> offset1 (tab,"offset1");
  Cube<Int> arri1(IPosition(3,2,3,4));
  Cube<Int> arrvali(IPosition(3,2,3,4));
  Cube<Complex> arrf1(IPosition(3,2,3,4));
  Cube<Complex> arrvalf(IPosition(3,2,3,4));
  RefRows refrows(1,9,2);
  Slicer slicer(IPosition(3,0,1,0), IPosition(3,2,2,2), IPosition(3,1,1,2));
  Array<Complex> arrCol1 (source1.getColumn());
  Array<Complex> arrColSlice1 (source1.getColumn(slicer));
  Array<Complex> arrCells1 (source1.getColumnCells(refrows));
  Array<Complex> arrCellsSlice1 (source1.getColumnCells(refrows,slicer));
  Array<Complex> arrCol2 (source2.getColumn());
  Array<Complex> arrColSlice2 (source2.getColumn(slicer));
  Array<Complex> arrCells2 (source2.getColumnCells(refrows));
  Array<Complex> arrCellsSlice2 (source2.getColumnCells(refrows,slicer));
  Slicer slicercol(IPosition(4,0,1,0,0), IPosition(4,2,2,2,10),
                   IPosition(4,1,1,2,1));
  Slicer slicercells(IPosition(4,0,0,0,1), IPosition(4,2,3,4,5),
                     IPosition(4,1,1,1,2));
  Slicer slicercsl (IPosition(4,0,1,0,1), IPosition(4,2,2,2,5),
                    IPosition(4,1,1,2,2));
  AlwaysAssertExit (allEQ(arrColSlice1,   arrCol1(slicercol)));
  AlwaysAssertExit (allEQ(arrCells1,      arrCol1(slicercells)));
  AlwaysAssertExit (allEQ(arrCellsSlice1, arrCol1(slicercsl)));
  AlwaysAssertExit (allEQ(arrColSlice2,   arrCol2(slicercol)));
  AlwaysAssertExit (allEQ(arrCells2,      arrCol2(slicercells)));
  AlwaysAssertExit (allEQ(arrCellsSlice2, arrCol2(slicercsl)));
  uInt i=0;
  for (uInt i2=0; i2<4; i2++) {
    for (uInt i1=0; i1<3; i1++) {
      arrf1(0,i1,i2) = Complex (16 + 12*i, 16 + 12*i + 36);
      arri1(0,i1,i2) = 65536 * (6*i - 2) + (6*i - 2 + 18);
      i++;
      arrf1(1,i1,i2) = Complex (16 + 12*i, 0);
      arri1(1,i1,i2) = 65536 * (6*i - 2) + -10;
      i++;
    }
  }
  ArrayIterator<Complex> iter1(arrCol1, 3);
  ArrayIterator<Complex> iter2(arrCol2, 3);
  for (i=0; i<10; i++) {
    cout << "get row " << i << endl;
    source1.get (i, arrvalf);
    if (!allNear (arrvalf, arrf1, 1e-4)) {
      cout << "error in source1 in row " << i << endl;
      cout << "Read: " << arrvalf << endl;
      cout << "Expected: " << arrf1 << endl;
      ok = False;
    }
    AlwaysAssertExit (allEQ(arrvalf, iter1.array()));
    AlwaysAssertExit (allEQ(arrvalf(slicer), source1.getSlice(i, slicer)));
    if (!autoScale) {
      target1.get (i, arrvali);
      if (!allEQ (arrvali, arri1)) {
	cout << "error in target1 in row " << i << endl;
	cout << "Read: " << arrvali << endl;
	cout << "Expected: " << arri1 << endl;
	ok = False;
      }
    } else {
      Float offs = offset1(i);
      Float so = (arrvalf(0,2,3).imag() + arrvalf(1,0,0).imag()) / 2;
      if (!near(offs, so)) {
	cout << "error in offset1 in row " << i << endl;
	cout << "Read: " << offs << endl;
	cout << "Expected: " << so << endl;
	ok = False;
      }
      Float scale = scale1(i);
      so = (arrvalf(0,2,3).imag() - arrvalf(1,0,0).imag()) / 65534;
      if (!near(scale, so)) {
	cout << "error in scale1 in row " << i << endl;
	cout << "Read: " << scale << endl;
	cout << "Expected: " << so << endl;
	ok = False;
      }
    }
    source2.get (i, arrvalf);
    if (!allEQ (arrvalf, arrf1)) {
      cout << "error in source2 in row " << i << endl;
      cout << "Read: " << arrvalf << endl;
      cout << "Expected: " << arrf1 << endl;
      ok = False;
    }
    AlwaysAssertExit (allEQ(arrvalf, iter2.array()));
    AlwaysAssertExit (allEQ(arrvalf(slicer), source2.getSlice(i, slicer)));
    arrf1 += Complex(12*arrf1.nelements(), 12*arrf1.nelements());
    arri1 += Int(65536 * 6*arri1.nelements() + 6*arri1.nelements());
    iter1.next();
    iter2.next();
  }
  return ok;
}

Bool checkDataSD (bool autoScale)
{
  Bool ok = True;
  // Read back the table.
  Table tab("tCompressComplex_tmp.data");
  ArrayColumn<Complex> source1 (tab, "source1");
  ArrayColumn<Complex> source2 (tab, "source2");
  ArrayColumn<Int> target1 (tab, "target1");
  ScalarColumn<Float> scale1 (tab, "scale1");
  ScalarColumn<Float> offset1 (tab,"offset1");
  Cube<Int> arri1(IPosition(3,2,3,4));
  Cube<Int> arrvali(IPosition(3,2,3,4));
  Cube<Complex> arrf1(IPosition(3,2,3,4));
  Cube<Complex> arrvalf(IPosition(3,2,3,4));
  uInt i=0;
  for (uInt i2=0; i2<4; i2++) {
    for (uInt i1=0; i1<3; i1++) {
      arrf1(0,i1,i2) = Complex (16 + 12*i, 16 + 12*i + 36);
      arri1(0,i1,i2) = 65536 * (6*i - 2) + 2*(3*i - 1 + 9) + 1;
      i++;
      arrf1(1,i1,i2) = Complex (16 + 12*i, 0);
      arri1(1,i1,i2) = 2 * 32768 * (6*i - 2);
      i++;
    }
  }
  cout << "get SD row 0" << endl;
  source1.get (0, arrvalf);
  if (!allNear (arrvalf, arrf1, 1e-4)) {
    cout << "error in source1 in row 0" << endl;
    cout << "Read: " << arrvalf << endl;
    cout << "Expected: " << arrf1 << endl;
    ok = False;
  }
  if (!autoScale) {
    target1.get (0, arrvali);
    if (!allEQ (arrvali, arri1)) {
      cout << "error in target1 in row 0" << endl;
      cout << "Read: " << arrvali << endl;
      cout << "Expected: " << arri1 << endl;
      ok = False;
    }
  } else {
    Float offs = offset1(0);
    Float so = (arrvalf(0,2,3).imag() + arrvalf(0,0,0).real()) / 2;
    if (!near(offs, so, 1e-4)) {
      cout << "error in offset1 in row 0" << endl;
      cout << "Read: " << offs << endl;
      cout << "Expected: " << so << endl;
      ok = False;
    }
    Float scale = scale1(0);
    so = (arrvalf(0,2,3).imag() - arrvalf(0,0,0).real()) / 65534;
    if (!near(scale, so, 1e-4)) {
      cout << "error in scale1 in row 0" << endl;
      cout << "Read: " << scale << endl;
      cout << "Expected: " << so << endl;
      ok = False;
    }
  }
  i = 1;
  for (uInt i2=0; i2<4; i2++) {
    for (uInt i1=0; i1<3; i1++) {
      arri1(1,i1,i2) = 65536 * (6*i - 2) + -10 + 1;
      i+=2;
    }
  }
  for (i=1; i<10; i++) {
    arrf1 += Complex(12*arrf1.nelements(), 12*arrf1.nelements());
    arri1 += Int(65536 * 6*arri1.nelements() + 6*arri1.nelements());
    cout << "get SD row " << i << endl;
    source1.get (i, arrvalf);
    if (!allNear (arrvalf, arrf1, 1e-4)) {
      cout << "error in source1 in row " << i << endl;
      cout << "Read: " << arrvalf << endl;
      cout << "Expected: " << arrf1 << endl;
      ok = False;
    }
    if (!autoScale) {
      target1.get (i, arrvali);
      if (!allEQ (arrvali, arri1)) {
	cout << "error in target1 in row " << i << endl;
	cout << "Read: " << arrvali << endl;
	cout << "Expected: " << arri1 << endl;
      ok = False;
      }
    } else {
      Float offs = offset1(i);
      Float so = (arrvalf(0,2,3).imag() + arrvalf(1,0,0).imag()) / 2;
      if (!near(offs, so, 1e-4)) {
	cout << "error in offset1 in row " << i << endl;
	cout << "Read: " << offs << endl;
	cout << "Expected: " << so << endl;
	ok = False;
      }
      Float scale = scale1(i);
      so = (arrvalf(0,2,3).imag() - arrvalf(1,0,0).imag()) / 65534;
      if (!near(scale, so, 1e-4)) {
	cout << "error in scale1 in row " << i << endl;
	cout << "Read: " << scale << endl;
	cout << "Expected: " << so << endl;
	ok = False;
      }
    }
    source2.get (i, arrvalf);
    if (!allEQ (arrvalf, arrf1)) {
      cout << "error in source2 in row " << i << endl;
      cout << "Read: " << arrvalf << endl;
      cout << "Expected: " << arrf1 << endl;
      ok = False;
    }
  }
  return ok;
}

void testSpeed()
{
  {
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ArrayColumnDesc<Int> ("target1",
					IPosition(3,2,3,4),
					ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Complex> ("source1",
					    IPosition(3,2,3,4),
					    ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Complex> ("source2","",
					    IPosition(3,2,3,4),
					    ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Int> ("target3",
					IPosition(3,2,3,4),
					ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Complex> ("source3",
					    IPosition(3,2,3,4),
					    ColumnDesc::Direct));
    td.addColumn (ScalarColumnDesc<Float> ("scale1"));
    td.addColumn (ScalarColumnDesc<Float> ("offset1"));

    // Now create a new table from the description.
    SetupNewTable newtab("tCompressComplex_tmp.data", td, Table::New);
    // Create the virtual column engine with the scale factors
    // and bind the columns to them.
    CompressComplex engine1("source1", "target1", "scale1", "offset1");
    CompressComplex engine3("source3", "target3", 2.0, 4.0);
    newtab.bindColumn ("source1", engine1);
    newtab.bindColumn ("source3", engine3);
    Table tab(newtab, 10000);

    // Fill the table via the virtual columns.
    ArrayColumn<Complex> source1 (tab, "source1");
    ArrayColumn<Complex> source2 (tab, "source2");
    ScalarColumn<Float> scale1 (tab,"scale1");
    ScalarColumn<Float> offset1 (tab,"offset1");

    Cube<Complex> arrf(IPosition(3,2,3,4));
    uInt i;
    i=20;
    for (uInt i2=0; i2<4; i2++) {
      for (uInt i1=0; i1<3; i1++) {
	arrf(0,i1,i2) = Complex(i,i+36);
	i += 6;
	arrf(1,i1,i2) = Complex(i,0);
	i += 6;
      }
    }
    for (i=0; i<10; i++) {
      scale1.put (i, 2.);
      offset1.put (i, 4.);
      source1.put (i, arrf);
      source2.put (i, arrf);
      arrf += Complex(6*arrf.nelements(), 6*arrf.nelements());
    }
  }
  {
    {
      // Time reading back column source1.
      Table tab("tCompressComplex_tmp.data");
      ArrayColumn<Complex> source (tab, "source1");
      Cube<Complex> arrvalf(IPosition(3,2,3,4));
      Timer timer;
      uInt nrow = tab.nrow();
      for (uInt i=0; i<nrow; i++) {
	source.get (i, arrvalf);
      }
      timer.show();
    }
    {
      // Time reading back column source2.
      Table tab("tCompressComplex_tmp.data");
      ArrayColumn<Complex> source (tab, "source2");
      Cube<Complex> arrvalf(IPosition(3,2,3,4));
      Timer timer;
      uInt nrow = tab.nrow();
      for (uInt i=0; i<nrow; i++) {
	source.get (i, arrvalf);
      }
      timer.show();
    }
    {
      // Time reading back column source3.
      Table tab("tCompressComplex_tmp.data");
      ArrayColumn<Complex> source (tab, "source3");
      Cube<Complex> arrvalf(IPosition(3,2,3,4));
      Timer timer;
      uInt nrow = tab.nrow();
      for (uInt i=0; i<nrow; i++) {
	source.get (i, arrvalf);
      }
      timer.show();
    }
    {
      // Time reading back column source1.
      Table tab("tCompressComplex_tmp.data");
      ArrayColumn<Complex> source (tab, "source1");
      Timer timer;
      source.getColumn();
      timer.show();
    }
    {
      // Time reading back column source2.
      Table tab("tCompressComplex_tmp.data");
      ArrayColumn<Complex> source (tab, "source2");
      Timer timer;
      source.getColumn();
      timer.show();
    }
    {
      // Time reading back column source3.
      Table tab("tCompressComplex_tmp.data");
      ArrayColumn<Complex> source (tab, "source3");
      Timer timer;
      source.getColumn();
      timer.show();
    }
  }
}


int main ()
{
  Int sts = 0;
  try {
    writeData (False, False);
    if (!checkData (False)) sts=1;
    writeData (False, True);
    if (!checkData (True)) sts=1;
    writeData (True, False);
    if (!checkDataSD (False)) sts=1;
    writeData (True, True);
    if (!checkDataSD (True)) sts=1;
    testSpeed();
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return sts;
}
