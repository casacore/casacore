//# tTiledColumnStMan.cc: Test program for the TiledColumnStMan classes
//# Copyright (C) 1994,1995,1996,1999,2000,2001,2003
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
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/TiledStManAccessor.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

rownr_t NROW = 551;

// <summary>
// Test program for the TiledColumnStMan class.
// </summary>

// This program tests the class TiledColumnStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void writeFixed(const TSMOption&, const IPosition&);
void readTable(const TSMOption&, Bool readKeys, const IPosition&);
void writeNoHyper(const TSMOption&);
void extendOnly(const TSMOption&);

int main () {
    try {
      {
        IPosition rowShape(2,13,1);
        writeFixed(TSMOption::Cache, rowShape);
	readTable(TSMOption::MMap, True, rowShape);
      }
      {
        IPosition rowShape(3,13,15,0);
        writeFixed(TSMOption::Cache, rowShape);
	readTable(TSMOption::MMap, True, rowShape);
      }
        writeFixed(TSMOption::Cache, IPosition(1,0));
	readTable(TSMOption::Cache, True, IPosition());
	writeNoHyper(TSMOption::MMap);
	readTable(TSMOption::Buffer, False, IPosition());
        writeFixed(TSMOption::Buffer, IPosition(1,0));
	readTable(TSMOption::Cache, False, IPosition());
        extendOnly(TSMOption::Cache);
    } catch (std::exception& x) {
	cout << "Caught an exception: " << x.what() << endl;
	return 1;
    }
    return 0;                           // exit with success status
}


void incrXYZ (float& x, float& y, float& z, uInt rownr, const IPosition& rowShape)
{
  if (rownr == 0) {
    // Initialize at first time.
    x = 34;
    y = 3;
    z = -2;
  } else {
    x += 5;
    if (rowShape.size() <= 1) {
      // No hypercube, so normal increment.
      y += -2;
      z += 0.5;
    } else {
      // hypercube, so X repeats every n rows and Y increments after n rows.
      if (rownr % rowShape[0] == 0) {
        x = 34;
        y += -2;
        // Same logic for Z.
        if (rowShape.size() == 2) {
          z += 0.5;
        } else {
          if (rownr % (rowShape[0]*rowShape[1]) == 0) {
            y = 3;
            z += 0.5;
          }
        }
      }
    }
  }
}
  
// First build a description.
void writeFixed(const TSMOption& tsmOpt, const IPosition& rowShape)
{
  cout << endl << "Test writeFixed " << rowShape << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("X"));
    td.addColumn (ScalarColumnDesc<float> ("Y"));
    td.addColumn (ScalarColumnDesc<float> ("Z"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,20),
					   ColumnDesc::FixedShape));
    Vector<String> allCoords (stringToVector("Pol,Freq,X,Y,Z"));
    td.defineHypercolumn ("TSMExample",
			  rowShape.size() + 2,
			  stringToVector ("Data,Weight"),
                          allCoords(Slice(0, rowShape.size()+2)));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledColumnStMan_tmp.data", td, Table::New);
    // Create a storage manager for it. First fill the tile shape.
    IPosition tileShape(2 + rowShape.size());
    tileShape[0] = 5;
    tileShape[1] = 6;
    for (uInt i=0; i<rowShape.size(); ++i) {
      tileShape[i+2] = rowShape[i] / (i+2);
      if (tileShape[i+2] <= 0) tileShape[i+2] = 1;
    }
    TiledColumnStMan sm1 ("TSMExample", tileShape, rowShape);
    newtab.setShapeColumn ("Freq", IPosition(1,20));
    newtab.setShapeColumn ("Data", IPosition(2,16,20));
    newtab.bindAll (sm1);
    StandardStMan ssm;
    if (tileShape.size() < 5) newtab.bindColumn ("Z", ssm);
    if (tileShape.size() < 4) newtab.bindColumn ("Y", ssm);
    Table table(newtab, 0, False, Table::LittleEndian, tsmOpt);

    Vector<float> freqValues(20);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float xValue = 0;
    float yValue = 0;
    float zValue = 0;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> x (table, "X");
    ScalarColumn<float> y (table, "Y");
    ScalarColumn<float> z (table, "Z");
    Matrix<float> array(IPosition(2,16,20));
    Matrix<float> result(IPosition(2,16,20));
    uInt i;
    indgen (array);
    for (i=0; i<NROW; i++) {
        incrXYZ (xValue, yValue, zValue, i, rowShape);
	table.addRow();
	data.put (i, array);
	weight.put (i, array+float(100));
	x.put (i, xValue);
	y.put (i, yValue);
	z.put (i, zValue);
	array += float(200);
    }
    freq.put (0, freqValues);
    pol.put (0, polValues);
    // Now read it back and check it.
    indgen (array);
    for (i=0; i<table.nrow(); i++) {
        incrXYZ (xValue, yValue, zValue, i, rowShape);
	data.get (i, result);
	if (! allEQ (array, result)) {
	    cout << "mismatch in data row " << i << endl;
	}
	weight.get (i, result);
	if (! allEQ (array + float(100), result)) {
	    cout << "mismatch in weight row " << i << endl;
	}
	if (! allEQ (freq(i), freqValues)) {
	    cout << "mismatch in freq row " << i << endl;
	}
	if (! allEQ (pol(i), polValues)) {
	    cout << "mismatch in pol row " << i << endl;
	}
	if (x(i) != xValue) {
	    cout << "mismatch in X row " << i << endl;
	}
	if (y(i) != yValue) {
	    cout << "mismatch in Y row " << i << endl;
	}
	if (z(i) != zValue) {
	    cout << "mismatch in Z row " << i << endl;
	}
	array += float(200);
    }
    ROTiledStManAccessor accessor (table, "TSMExample");
    accessor.showCacheStatistics (cout);
    cout << "hypercubeShape = " << accessor.getHypercubeShape(0) << endl;
    cout << "tileShape = " << accessor.getTileShape(0) << endl;
    AlwaysAssertExit (accessor.nhypercubes() == 1);
    AlwaysAssertExit (accessor.hypercubeShape(2) == accessor.getHypercubeShape(0));
    AlwaysAssertExit (accessor.tileShape(2) == accessor.getTileShape(0));
    AlwaysAssertExit (accessor.getBucketSize(0) == accessor.bucketSize(2));
    AlwaysAssertExit (accessor.getCacheSize(0) == accessor.cacheSize(2));
}

void readTable (const TSMOption& tsmOpt, Bool readKeys, const IPosition& rowShape)
{
    cout << endl << "Test readTable " << rowShape << endl;
    Table table("tTiledColumnStMan_tmp.data", Table::Old, tsmOpt);
    ROTiledStManAccessor accessor (table, "TSMExample");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> x (table, "X");
    ScalarColumn<float> y (table, "Y");
    ScalarColumn<float> z (table, "Z");
    Vector<float> freqValues(20);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float xValue = 0;
    float yValue = 0;
    float zValue = 0;
    Matrix<float> array(IPosition(2,16,20));
    Matrix<float> result(IPosition(2,16,20));
    uInt i;
    indgen (array);
    for (i=0; i<table.nrow(); i++) {
        incrXYZ (xValue, yValue, zValue, i, rowShape);
	data.get (i, result);
	if (! allEQ (array, result)) {
	    cout << "mismatch in data row " << i << endl;
	}
	weight.get (i, result);
	if (! allEQ (array + float(100), result)) {
	    cout << "mismatch in weight row " << i << endl;
	}
	if (! allEQ (freq(i), freqValues)) {
	    cout << "mismatch in freq row " << i << endl;
	}
	if (! allEQ (pol(i), polValues)) {
	    cout << "mismatch in pol row " << i << endl;
	}
	if (x(i) != xValue) {
	    cout << "mismatch in X row " << i << endl;
	}
	if (y(i) != yValue) {
	    cout << "mismatch in Y row " << i << endl;
	}
	if (z(i) != zValue) {
	    cout << "mismatch in Z row " << i << endl;
	}
	array += float(200);
    }
    accessor.showCacheStatistics (cout);
    accessor.clearCaches();
    if (readKeys) {
      AlwaysAssertExit (allEQ (accessor.getValueRecord(0).asArrayFloat("Freq"),
			       freqValues));
      AlwaysAssertExit (allEQ (accessor.getValueRecord(0).asArrayFloat("Pol"),
			       polValues));
      if (rowShape.size() > 1) {
        cout << "X = " << accessor.getValueRecord(0).asArrayFloat("X") << endl;
      }
      if (rowShape.size() > 2) {
        cout << "Y = " << accessor.getValueRecord(0).asArrayFloat("Y") << endl;
        cout << "Z = " << accessor.getValueRecord(0).asArrayFloat("Z") << endl;
      }
    }
    cout << "get's have been done" << endl;

    // Get the entire column.
    // (In a separate scope to delete the big array at the end).
    {
	Array<float> result;
	data.getColumn (result);
	if (result.shape() != IPosition (3,16,20,NROW)) {
	    cout << "shape of getColumn result is incorrect" << endl;
	}else{
	    indgen (array);
	    uInt i=0;
	    ArrayIterator<float> iter (result, 2);
	    while (! iter.pastEnd()) {
		if (! allEQ (iter.array(), array)) {
		    cout << "mismatch in getColumn data row " << i << endl;
		}
		array += float(200);
		i++;
		iter.next();
	    }
	}
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
	cout << "getColumn has been done" << endl;
    }

    // Get slices in the entire column.
    {
	uInt i, j;
	Cube<float> array(1,1,NROW);
	for (i=0; i<NROW; i++) {
	    array(0,0,i) = 200*i;
	}
	Array<float> result;
	for (j=0; j<20; j++) {
	    for (i=0; i<16; i++) {
		data.getColumn (Slicer (IPosition(2,i,j)), result);
		if (! allEQ (result, array)) {
		    cout << "mismatch in getColumnSlice " << i << "," << j
			 << endl;
		}
		array += float(1);
	    }
	}
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
	cout << "getColumnSlice's have been done" << endl;
    }

    // Get strided slices in the entire column.
    {
	uInt i, j;
	Cube<float> array(2,2,NROW);
	for (i=0; i<NROW; i++) {
	    array(0,0,i) = 200*i;
	    array(1,0,i) = 200*i + 16/2;
	    array(0,1,i) = 200*i + 16*20/2;
	    array(1,1,i) = 200*i + 16*20/2 + 16/2;
	}
	Array<float> result;
	for (j=0; j<20/2; j++) {
	    for (i=0; i<16/2; i++) {
		data.getColumn (Slicer (IPosition(2,i,j),
					IPosition(2,2,2),
					IPosition(2,16/2,20/2)),
				result);
		if (! allEQ (result, array)) {
		    cout << "mismatch in getColumnSlice " << i << "," << j
			 << endl;
		}
		array += float(1);
	    }
	    array += float(8);
	}
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
	cout << "strided getColumnSlice's have been done" << endl;
    }

    // Get slices from each cell.
    {
	uInt i, j, k;
	Matrix<float> array(2,4);
	Array<float> result;
	for (k=0; k<table.nrow(); k++) {
	    for (j=0; j<4; j++) {
		for (i=0; i<2; i++) {
		    array(i,j) = float(i + 16*j + 200*k);
		}
	    }
	    for (j=0; j<20/4; j++) {
		for (i=0; i<16/2; i++) {
		    data.getSlice (k, 
				   Slicer (IPosition(2,i*2,j*4),
					   IPosition(2,2,4)),
				   result);
		    if (! allEQ (result, array)) {
			cout << "mismatch in getSlice " << i << "," << j
			     << "," << k << endl;
		    }
		    array += float(2);
		}
		array += float((4-1) * 16);
	    }
	}
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
	cout << "getSlice's have been done" << endl;
    }

    // Get a strided slice from each cell.
    {
	uInt i, j, k;
	Matrix<float> array(8,5);
	Array<float> result;
	for (k=0; k<table.nrow(); k++) {
	    for (j=0; j<5; j++) {
		for (i=0; i<8; i++) {
		    array(i,j) = float(i*16/8 + 16*20/5*j + 200*k);
		}
	    }
	    for (j=0; j<20/5; j++) {
		for (i=0; i<16/8; i++) {
		    data.getSlice (k, 
				   Slicer (IPosition(2,i,j),
					   IPosition(2,8,5),
					   IPosition(2,16/8,20/5)),
				   result);
		    if (! allEQ (result, array)) {
			cout << "mismatch in strided getSlice " << i << ","
			     << j << "," << k << endl;
		    }
		    array += float(1);
		}
		array += float(16 - 16/8);
	    }
	}
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
	cout << "getSlice's with strides have been done" << endl;
    }
}

// First build a description.
void writeNoHyper(const TSMOption& tsmOpt)
{
    cout << endl << "Test writeNoHyper " << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("X"));
    td.addColumn (ScalarColumnDesc<float> ("Y"));
    td.addColumn (ScalarColumnDesc<float> ("Z"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,20),
					   ColumnDesc::FixedShape));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledColumnStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledColumnStMan sm1 ("TSMExample", IPosition(3,5,6,1));
    newtab.setShapeColumn ("Freq", IPosition(1,20));
    newtab.setShapeColumn ("Data", IPosition(2,16,20));
    newtab.bindColumn ("Data", sm1);
    newtab.bindColumn ("Weight", sm1);
    Table table(newtab, 0, False, Table::BigEndian, tsmOpt);

    Vector<float> freqValues(20);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float xValue = 0;
    float yValue = 0;
    float zValue = 0;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> x (table, "X");
    ScalarColumn<float> y (table, "Y");
    ScalarColumn<float> z (table, "Z");
    Matrix<float> array(IPosition(2,16,20));
    Matrix<float> result(IPosition(2,16,20));
    uInt i;
    indgen (array);
    for (i=0; i<NROW; i++) {
        incrXYZ (xValue, yValue, zValue, i, IPosition());
	table.addRow();
	data.put (i, array);
	weight.put (i, array+float(100));
	x.put (i, xValue);
	y.put (i, yValue);
	z.put (i, zValue);
	array += float(200);
	freq.put (i, freqValues);
	pol.put (i, polValues);
    }
    indgen (array);
    for (i=0; i<table.nrow(); i++) {
        incrXYZ (xValue, yValue, zValue, i, IPosition());
	data.get (i, result);
	if (! allEQ (array, result)) {
	    cout << "mismatch in data row " << i << endl;
	}
	weight.get (i, result);
	if (! allEQ (array + float(100), result)) {
	    cout << "mismatch in weight row " << i << endl;
	}
	if (! allEQ (freq(i), freqValues)) {
	    cout << "mismatch in freq row " << i << endl;
	}
	if (! allEQ (pol(i), polValues)) {
	    cout << "mismatch in pol row " << i << endl;
	}
	if (x(i) != xValue) {
	    cout << "mismatch in X row " << i << endl;
	}
	if (y(i) != yValue) {
	    cout << "mismatch in Y row " << i << endl;
	}
	if (z(i) != zValue) {
	    cout << "mismatch in Z row " << i << endl;
	}
	array += float(200);
    }
    ROTiledStManAccessor accessor (table, "TSMExample");
    accessor.showCacheStatistics (cout);
    cout << "hypercubeShape = " << accessor.getHypercubeShape(0) << endl;
    cout << "tileShape = " << accessor.getTileShape(0) << endl;
    AlwaysAssertExit (accessor.nhypercubes() == 1);
    AlwaysAssertExit (accessor.hypercubeShape(2) == accessor.getHypercubeShape(0));
    AlwaysAssertExit (accessor.tileShape(2) == accessor.getTileShape(0));
    AlwaysAssertExit (accessor.getBucketSize(0) == accessor.bucketSize(2));
    AlwaysAssertExit (accessor.getCacheSize(0) == accessor.cacheSize(2));
}

// First build a description.
void extendOnly(const TSMOption& tsmOpt)
{
    cout << endl << "Test extendOnly " << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,20),
					   ColumnDesc::FixedShape));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledColumnStMan_tmp2.data", td, Table::New);
    // Create a storage manager for it.
    TiledColumnStMan sm1 ("TSMExample", IPosition(3,5,6,420));
    newtab.bindColumn ("Weight", sm1);
    Table table(newtab, 0, False, Table::LocalEndian, tsmOpt);

    ArrayColumn<float> weight (table, "Weight");
    table.addRow (34);
    table.addRow (387);
    table.flush();
    ROTiledStManAccessor accessor (table, "TSMExample");
    accessor.showCacheStatistics (cout);
    cout << "hypercubeShape = " << accessor.getHypercubeShape(0) << endl;
    cout << "tileShape = " << accessor.getTileShape(0) << endl;
    AlwaysAssertExit (accessor.nhypercubes() == 1);
    AlwaysAssertExit (accessor.hypercubeShape(2) == accessor.getHypercubeShape(0));
    AlwaysAssertExit (accessor.tileShape(2) == accessor.getTileShape(0));
    AlwaysAssertExit (accessor.getBucketSize(0) == accessor.bucketSize(2));
    AlwaysAssertExit (accessor.getCacheSize(0) == accessor.cacheSize(2));
}
