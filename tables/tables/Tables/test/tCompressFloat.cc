//# tCompressFloat.cc: Test program for class CompressFloat
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

#include <tables/Tables/TableDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/CompressFloat.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/Slicer.h>
#include <casa/OS/Timer.h>
#include <tables/Tables/TableError.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// <summary> Test program for class CompressFloat </summary>

// This program tests the virtual column engine CompressFloat.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// First build a description.
void writeData (Bool autoScale)
{
  // First register the virtual column engine.
  CompressFloat::registerClass();

  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.comment() = "A test of class TableDesc";
  td.addColumn (ArrayColumnDesc<Short> ("target1"));
  td.addColumn (ArrayColumnDesc<Float> ("source1"));
  td.addColumn (ArrayColumnDesc<Float> ("source2","",
					IPosition(3,2,3,4),
					ColumnDesc::Direct));
  td.addColumn (ScalarColumnDesc<Float> ("scale1"));
  td.addColumn (ScalarColumnDesc<Float> ("offset1"));

  // Now create a new table from the description.
  SetupNewTable newtab("tCompressFloat_tmp.data", td, Table::New);
  // Create the virtual column engine with the scale factors
  // and bind the columns to them.
  CompressFloat engine1("source1", "target1", "scale1", "offset1", autoScale);
  newtab.bindColumn ("source1", engine1);
  Table tab(newtab, 10);

  // Fill the table via the virtual columns.
  ArrayColumn<Float> source1 (tab, "source1");
  ArrayColumn<Float> source2 (tab, "source2");
  ScalarColumn<Float> scale1 (tab, "scale1");
  ScalarColumn<Float> offset1 (tab,"offset1");

  Cube<Float> arrf(IPosition(3,2,3,4));
  uInt i;
  i=2;
  for (uInt i2=0; i2<4; i2++) {
    for (uInt i1=0; i1<3; i1++) {
      for (uInt i0=0; i0<2; i0++) {
	arrf(i0,i1,i2) = i;
	i += 6;
      }
    }
  }
  for (i=0; i<10; i++) {
    if (!autoScale) {
      scale1.put (i, 2.);
      offset1.put (i, 4.);
    }
    if (i != 5) {
      source1.put (i, arrf);
    }
    source2.put (i, arrf);
    arrf += (Float)(6*arrf.nelements());
  }
  // Write the 5th row in Slices.
  arrf -= (Float)(5*6*arrf.nelements());
  source1.setShape (5, arrf.shape());
  for (i=0; i<3; i++) {
    source1.putSlice (5, Slicer(IPosition(3,0,i,0), IPosition(3,2,1,4)),
		      arrf(IPosition(3,0,i,0), IPosition(3,1,i,3)));
  }

  //# Do an erronous thing.
  //# However, this fails to run on Linux (so outcommented).
  ///  SetupNewTable newtab2("tCompressFloat_tmp.dat2", td, Table::Scratch);
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
  Table tab("tCompressFloat_tmp.data");
  ROArrayColumn<Float> source1 (tab, "source1");
  ROArrayColumn<Float> source2 (tab, "source2");
  ROArrayColumn<Short> target1 (tab, "target1");
  Cube<Short> arri1(IPosition(3,2,3,4));
  Cube<Short> arrvali(IPosition(3,2,3,4));
  Cube<Float> arrf1(IPosition(3,2,3,4));
  Cube<Float> arrvalf(IPosition(3,2,3,4));
  uInt i=0;
  for (uInt i2=0; i2<4; i2++) {
    for (uInt i1=0; i1<3; i1++) {
      for (uInt i0=0; i0<2; i0++) {
	arrf1(i0,i1,i2) = 2 + 6*i;
	arri1(i0,i1,i2) = 3*i - 1;
	i++;
      }
    }
  }
  for (i=0; i<10; i++) {
    cout << "get row " << i << endl;
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
    }
    source2.get (i, arrvalf);
    if (!allEQ (arrvalf, arrf1)) {
      cout << "error in source2 in row " << i << endl;
      cout << "Read: " << arrvalf << endl;
      cout << "Expected: " << arrf1 << endl;
      ok = False;
    }
    arrf1 += (Float)(6*arrf1.nelements());
    arri1 += (Short)(3*arri1.nelements());
  }
  return ok;
}

void testSpeed()
{
  {
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ArrayColumnDesc<Short> ("target1",
					  IPosition(3,2,3,4),
					  ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Float> ("source1",
					  IPosition(3,2,3,4),
					  ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Float> ("source2","",
					  IPosition(3,2,3,4),
					  ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Short> ("target3",
					  IPosition(3,2,3,4),
					  ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Float> ("source3",
					  IPosition(3,2,3,4),
					  ColumnDesc::Direct));
    td.addColumn (ScalarColumnDesc<Float> ("scale1"));
    td.addColumn (ScalarColumnDesc<Float> ("offset1"));

    // Now create a new table from the description.
    SetupNewTable newtab("tCompressFloat_tmp.data", td, Table::New);
    // Create the virtual column engine with the scale factors
    // and bind the columns to them.
    CompressFloat engine1("source1", "target1", "scale1", "offset1", False);
    CompressFloat engine3("source3", "target3", 2.0, 4.0);
    newtab.bindColumn ("source1", engine1);
    newtab.bindColumn ("source3", engine3);
    Table tab(newtab, 10000);

    // Fill the table via the virtual columns.
    ArrayColumn<Float> source1 (tab, "source1");
    ArrayColumn<Float> source2 (tab, "source2");
    ScalarColumn<Float> scale1 (tab,"scale1");
    ScalarColumn<Float> offset1 (tab,"offset1");

    Cube<Float> arrf(IPosition(3,2,3,4));
    uInt i;
    i=2;
    for (uInt i2=0; i2<4; i2++) {
      for (uInt i1=0; i1<3; i1++) {
	for (uInt i0=0; i0<2; i0++) {
	  arrf(i0,i1,i2) = i;
	  i += 6;
	}
      }
    }
    for (i=0; i<10; i++) {
      scale1.put (i, 2.);
      offset1.put (i, 4.);
      source1.put (i, arrf);
      source2.put (i, arrf);
      arrf += (Float)(6*arrf.nelements());
    }
  }
  {
    {
      // Time reading back column source1.
      Table tab("tCompressFloat_tmp.data");
      ROArrayColumn<Float> source (tab, "source1");
      Cube<Float> arrvalf(IPosition(3,2,3,4));
      Timer timer;
      uInt nrow = tab.nrow();
      for (uInt i=0; i<nrow; i++) {
	source.get (i, arrvalf);
      }
      timer.show();
    }
    {
      // Time reading back column source2.
      Table tab("tCompressFloat_tmp.data");
      ROArrayColumn<Float> source (tab, "source2");
      Cube<Float> arrvalf(IPosition(3,2,3,4));
      Timer timer;
      uInt nrow = tab.nrow();
      for (uInt i=0; i<nrow; i++) {
	source.get (i, arrvalf);
      }
      timer.show();
    }
    {
      // Time reading back column source3.
      Table tab("tCompressFloat_tmp.data");
      ROArrayColumn<Float> source (tab, "source3");
      Cube<Float> arrvalf(IPosition(3,2,3,4));
      Timer timer;
      uInt nrow = tab.nrow();
      for (uInt i=0; i<nrow; i++) {
	source.get (i, arrvalf);
      }
      timer.show();
    }
    {
      // Time reading back column source1.
      Table tab("tCompressFloat_tmp.data");
      ROArrayColumn<Float> source (tab, "source1");
      Timer timer;
      source.getColumn();
      timer.show();
    }
    {
      // Time reading back column source2.
      Table tab("tCompressFloat_tmp.data");
      ROArrayColumn<Float> source (tab, "source2");
      Timer timer;
      source.getColumn();
      timer.show();
    }
    {
      // Time reading back column source3.
      Table tab("tCompressFloat_tmp.data");
      ROArrayColumn<Float> source (tab, "source3");
      Timer timer;
      source.getColumn();
      timer.show();
    }
  }
}


int main ()
{
  Int sts=0;
  try {
    writeData (False);
    if (! checkData (False)) sts=1;
    writeData (True);
    if (! checkData (True)) sts=1;
    testSpeed();
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return sts;
}
