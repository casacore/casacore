//# tTiledDataStMan.cc: Test program for the TiledDataStMan class
//# Copyright (C) 1994,1995,1996,1999,2000,2001
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
#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/DataMan/TiledDataStManAccessor.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the TiledDataStMan class.
// </summary>

// This program tests the class TiledDataStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a();
void b();

int main()
{
    try {
	a ();
	b ();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

void init (Matrix<Float>& array, Matrix<Bool>& arrayb, Matrix<Complex>& arrayc)
{
       // The SGI compiler bug is the cause of this static_cast workaround.
    indgen (static_cast< Matrix<Float> &>(array));
    uInt i=0;
    for (uInt k=0; k<20; k++) {
	for (uInt j=0; j<12; j++) {
	    if (i%7 == 2) {
		arrayb(j,k) = False;
	    }else{
		arrayb(j,k) = True;
	    }
	    arrayc(j,k) = Complex (i+1.5, i+2.6);
	    i++;
	}
    }
}

// First build a description.
void a()
{
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ScalarColumnDesc<float> ("Baseline"));
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,12),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("Id"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,12,20),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Bool>   ("Flag", IPosition(2,12,20),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Complex>("OData", IPosition(2,12,20),
					   ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  4,
			  stringToVector ("Data,Weight,Flag"),
			  stringToVector ("Pol,Freq,Baseline,Time"),
			  stringToVector ("Id"));
    td.defineHypercolumn ("TSMExample2",
			  4,
			  stringToVector ("OData"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledDataStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledDataStMan sm0 ("TSMExample");
    TiledDataStMan sm2 ("TSMExample2");
    newtab.setShapeColumn ("Freq", IPosition(1,20));
    newtab.setShapeColumn ("Data", IPosition(2,12,20));
    newtab.bindAll (sm0);
    newtab.bindColumn ("OData", sm2);
    Table table(newtab, 42*30);
    TiledDataStManAccessor accessor(table, "TSMExample");
    TiledDataStManAccessor accessor2(table, "TSMExample2");

    Record values;
    Vector<float> timeValues(42);
    Vector<float> baselineValues(30);
    Vector<float> freqValues(20);
    Vector<float> polValues(12);
    indgen (timeValues);
    indgen (baselineValues, float(100));
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    values.define ("Time", timeValues);
    values.define ("Baseline", baselineValues);
    values.define ("Freq", freqValues);
    values.define ("Pol", polValues);
    values.define ("Id", float(5));
    accessor.addHypercube (IPosition(4,12,20,30,42),
			   IPosition(4,4,5,6,7), values);
    accessor2.addHypercube (IPosition(4,12,20,30,42),
			    IPosition(4,4,5,30,42), Record());
    ScalarColumn<float> time (table, "Time");
    ScalarColumn<float> baseline (table, "Baseline");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ScalarColumn<float> id (table, "Id");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ArrayColumn<Bool> flag (table, "Flag");
    ArrayColumn<Complex> odata (table, "OData");
    Matrix<float> array(IPosition(2,12,20));
    Matrix<float> result(IPosition(2,12,20));
    Matrix<Bool> arrayb(IPosition(2,12,20));
    Matrix<Bool> resultb(IPosition(2,12,20));
    Matrix<Complex> arrayc(IPosition(2,12,20));
    Matrix<Complex> resultc(IPosition(2,12,20));
    init (array, arrayb, arrayc);
    uInt i;

    for (i=0; i<30*42; i++) {
	data.put (i, array);
	weight.put (i, array+float(100));
	flag.put (i, arrayb);
	odata.put (i, arrayc);
	array += float(200);
	arrayc += Complex(200, 300);
    }
    init (array, arrayb, arrayc);
    for (i=0; i<table.nrow(); i++) {
	data.get (i, result);
	if (! allEQ (array, result)) {
	    cout << "mismatch in data row " << i << endl;
	}
	weight.get (i, result);
	if (! allEQ (array + float(100), result)) {
	    cout << "mismatch in weight row " << i << endl;
	}
	flag.get (i, resultb);
	if (! allEQ (arrayb, resultb)) {
	    cout << "mismatch in flag row " << i << endl;
	}
	odata.get (i, resultc);
	if (! allEQ (arrayc, resultc)) {
	    cout << "mismatch in odata row " << i << endl;
	}
	array += float(200);
	arrayc += Complex(200, 300);
    }

    // Add columns and a data manager.
    TableDesc td1 ("", "1", TableDesc::Scratch);
    td1.addColumn (ScalarColumnDesc<float>  ("Data1"));
    td1.defineHypercolumn ("TSMExample1",
			  2,
			  stringToVector ("Data1"));
    // Create a storage manager for it.
    TiledDataStMan sm1 ("TSMExample1");
    table.addColumn (td1, sm1);
    TiledDataStManAccessor accessor1(table, "TSMExample1");

    accessor1.addHypercube (IPosition(2,30,42),
			    IPosition(2,9,11), Record());
    ScalarColumn<float> data1 (table, "Data1");
    for (i=0; i<30*42; i++) {
	data1.put (i, i);
    }
    accessor.showCacheStatistics (cout);
    accessor2.showCacheStatistics (cout);
}

void b()
{
    Table table("tTiledDataStMan_tmp.data");
    ScalarColumn<float> time (table, "Time");
    ScalarColumn<float> baseline (table, "Baseline");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ScalarColumn<float> id (table, "Id");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ArrayColumn<Complex> odata (table, "OData");
    ArrayColumn<Bool> flag (table, "Flag");
    ScalarColumn<float> data1 (table, "Data1");
    Matrix<float> array(IPosition(2,12,20));
    Matrix<float> result;
    Matrix<Bool> arrayb(IPosition(2,12,20));
    Matrix<Bool> resultb;
    Matrix<Complex> arrayc(IPosition(2,12,20));
    Matrix<Complex> resultc;
    Vector<float> timeValues(42);
    Vector<float> baselineValues(30);
    Vector<float> freqValues(20);
    Vector<float> freqResult;
    Vector<float> polValues(12);
    Vector<float> polResult;
    Array<float> polArray(IPosition(2,12,1));
    Array<float> freqArray(IPosition(2,1,20));;
    float fvalue;
    String svalue;
    init (array, arrayb, arrayc);
    indgen (timeValues);
    indgen (baselineValues, float(100));
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    uInt i,j,i0,i1;
    i = 0;
    for (i0=0; i0<42; i0++) {
	for (i1=0; i1<30; i1++) {
	    data.get (i, result);
	    if (! allEQ (array, result)) {
		cout << "mismatch in data row " << i << endl;
	    }
	    weight.get (i, result);
	    if (! allEQ (array + float(100), result)) {
		cout << "mismatch in weight row " << i << endl;
	    }
	    odata.get (i, resultc);
	    if (! allEQ (arrayc, resultc)) {
		cout << "mismatch in odata row " << i << endl;
	    }
	    flag.get (i, resultb);
	    if (! allEQ (arrayb, resultb)) {
		cout << "mismatch in flag row " << i << endl;
	    }
	    pol.get (i, polResult);
	    if (! allEQ (polValues, polResult)) {
		cout << "mismatch in pol row " << i << endl;
	    }
	    freq.get (i, freqResult);
	    if (! allEQ (freqValues, freqResult)) {
		cout << "mismatch in freq row " << i << endl;
	    }
	    baseline.get (i, fvalue);
	    if (fvalue != baselineValues(i1)) {
		cout << "mismatch in baseline row " << i << endl;
	    }
	    time.get (i, fvalue);
	    if (fvalue != timeValues(i0)) {
		cout << "mismatch in time row " << i << endl;
	    }
	    id.get (i, fvalue);
	    if (fvalue != 5) {
		cout << "mismatch in id row " << i << endl;
	    }
	    if (data1(i) != i) {
		cout << "mismatch in data1 row " << i << endl;
	    }
	    for (j=0; j<20; j++) {
		data.getSlice (i, Slicer(Slice(0,12),Slice(j,1)), polArray);
		if (! allEQ (polArray, array(Slice(0,12), Slice(j,1)))) {
		    cout << "mismatch in pol-slice " << j << " row "<<i<<endl;
		}
	    }
	    for (j=0; j<12; j++) {
		data.getSlice (i, Slicer(Slice(j,1),Slice(0,20)), freqArray);
		if (! allEQ (freqArray, array(Slice(j,1), Slice(0,20)))) {
		    cout << "mismatch in freq-slice " << j << " row "<<i<<endl;
		}
	    }
	    array += float(200);
	    arrayc += Complex(200, 300);
	    i++;
	}
    }

    // Test if getting entire tiles works fine.
    // First get the entire column as a reference.
    Array<Complex> odatacol = odata.getColumn();
    for (j=0; j<20/5; j++) {
	for (i=0; i<12/4; i++) {
	    if (! allEQ (odata.getColumn (Slicer(IPosition(2,i*4,j*5),
						 IPosition(2,4,5))),
			 odatacol (IPosition(3,i*4,j*5,0),
				   IPosition(3,i*4+3,j*5+4,30*42-1)))) {
		cout << "mismatch in odata-tile " << i << "," << j << endl;
	    }
	}
    }

    ROTiledStManAccessor accessor (table, "TSMExample");
    accessor.showCacheStatistics (cout);
    ROTiledStManAccessor accessor2 (table, "TSMExample2");
    accessor2.showCacheStatistics (cout);
}
