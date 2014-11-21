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
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the TiledColumnStMan class.
// </summary>

// This program tests the class TiledColumnStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void writeFixed(const TSMOption&);
void readTable(const TSMOption&, Bool readKeys);
void writeNoHyper(const TSMOption&);

int main () {
    try {
        writeFixed(TSMOption::Cache);
	readTable(TSMOption::MMap, True);
	writeNoHyper(TSMOption::MMap);
	readTable(TSMOption::Buffer, False);
        writeFixed(TSMOption::Buffer);
	readTable(TSMOption::Cache, False);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    }
    return 0;                           // exit with success status
}

// First build a description.
void writeFixed(const TSMOption& tsmOpt)
{
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,20),
					   ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledColumnStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledColumnStMan sm1 ("TSMExample", IPosition(3,5,6,1));
    newtab.setShapeColumn ("Freq", IPosition(1,20));
    newtab.setShapeColumn ("Data", IPosition(2,16,20));
    newtab.bindAll (sm1);
    Table table(newtab, 0, False, Table::LittleEndian, tsmOpt);

    Vector<float> freqValues(20);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    Matrix<float> array(IPosition(2,16,20));
    Matrix<float> result(IPosition(2,16,20));
    uInt i;
    indgen (array);
    for (i=0; i<51; i++) {
	table.addRow();
	data.put (i, array);
	weight.put (i, array+float(100));
	time.put (i, timeValue);
	array += float(200);
	timeValue += 5;
    }
    freq.put (0, freqValues);
    pol.put (0, polValues);
    timeValue = 34;
    indgen (array);
    for (i=0; i<table.nrow(); i++) {
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
	if (time(i) != timeValue) {
	    cout << "mismatch in time row " << i << endl;
	}
	array += float(200);
	timeValue += 5;
    }
    ROTiledStManAccessor accessor (table, "TSMExample");
    accessor.showCacheStatistics (cout);
    AlwaysAssertExit (accessor.nhypercubes() == 1);
    AlwaysAssertExit (accessor.hypercubeShape(2) == IPosition(3,16,20,
							      table.nrow()));
    AlwaysAssertExit (accessor.tileShape(2) == IPosition(3,5,6,1));
    AlwaysAssertExit (accessor.getHypercubeShape(0) == IPosition(3,16,20,
								table.nrow()));
    AlwaysAssertExit (accessor.getTileShape(0) == IPosition(3,5,6,1));
    AlwaysAssertExit (accessor.getBucketSize(0) == accessor.bucketSize(2));
    AlwaysAssertExit (accessor.getCacheSize(0) == accessor.cacheSize(2));
}

void readTable (const TSMOption& tsmOpt, Bool readKeys)
{
  Table table("tTiledColumnStMan_tmp.data", Table::Old, tsmOpt);
    ROTiledStManAccessor accessor (table, "TSMExample");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    Vector<float> freqValues(20);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    Matrix<float> array(IPosition(2,16,20));
    Matrix<float> result(IPosition(2,16,20));
    uInt i;
    indgen (array);
    for (i=0; i<table.nrow(); i++) {
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
	if (time(i) != timeValue) {
	    cout << "mismatch in time row " << i << endl;
	}
	array += float(200);
	timeValue += 5;
    }
    accessor.showCacheStatistics (cout);
    accessor.clearCaches();
    if (readKeys) {
      AlwaysAssertExit (allEQ (accessor.getValueRecord(0).asArrayFloat("Freq"),
			       freqValues));
      AlwaysAssertExit (allEQ (accessor.getValueRecord(0).asArrayFloat("Pol"),
			       polValues));
      AlwaysAssertExit (allEQ (accessor.getValueRecord(0).asArrayFloat("Time"),
			       time.getColumn()));
    }
    cout << "get's have been done" << endl;

    // Get the entire column.
    // (In a separate scope to delete the big array at the end).
    {
	Array<float> result;
	data.getColumn (result);
	if (result.shape() != IPosition (3,16,20,51)) {
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
	Cube<float> array(1,1,51);
	for (i=0; i<51; i++) {
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
	Cube<float> array(2,2,51);
	for (i=0; i<51; i++) {
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
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
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
    float timeValue;
    timeValue = 34;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    Matrix<float> array(IPosition(2,16,20));
    Matrix<float> result(IPosition(2,16,20));
    uInt i;
    indgen (array);
    for (i=0; i<51; i++) {
	table.addRow();
	data.put (i, array);
	weight.put (i, array+float(100));
	time.put (i, timeValue);
	array += float(200);
	timeValue += 5;
	freq.put (i, freqValues);
	pol.put (i, polValues);
    }
    timeValue = 34;
    indgen (array);
    for (i=0; i<table.nrow(); i++) {
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
	if (time(i) != timeValue) {
	    cout << "mismatch in time row " << i << endl;
	}
	array += float(200);
	timeValue += 5;
    }
    ROTiledStManAccessor accessor (table, "TSMExample");
    accessor.showCacheStatistics (cout);
    AlwaysAssertExit (accessor.nhypercubes() == 1);
    AlwaysAssertExit (accessor.hypercubeShape(2) == IPosition(3,16,20,
							      table.nrow()));
    AlwaysAssertExit (accessor.tileShape(2) == IPosition(3,5,6,1));
    AlwaysAssertExit (accessor.getHypercubeShape(0) == IPosition(3,16,20,
								table.nrow()));
    AlwaysAssertExit (accessor.getTileShape(0) == IPosition(3,5,6,1));
    AlwaysAssertExit (accessor.getBucketSize(0) == accessor.bucketSize(2));
    AlwaysAssertExit (accessor.getCacheSize(0) == accessor.cacheSize(2));
}
