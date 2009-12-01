//# tTiledShapeStMan.cc: Test program for the TiledShapeStMan classes
//# Copyright (C) 1998,1999,2000,2001,2003
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
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// <summary>
// Test program for the TiledShapeStMan class.
// </summary>

// This program tests the class TiledShapeStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// First build a description.
void writeFixed()
{
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,25),
					   ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.setShapeColumn ("Freq", IPosition(1,25));
    newtab.setShapeColumn ("Data", IPosition(2,16,25));
    newtab.bindAll (sm1);
    Table table(newtab);

    Vector<float> freqValues(25);
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
    Matrix<float> array(IPosition(2,16,25));
    Matrix<float> result(IPosition(2,16,25));
    uInt i;
    indgen (array);
    for (i=0; i<101; i++) {
	table.addRow();
	data.put (i, array);
	weight.put (i, array+float(100));
	time.put (i, timeValue);
	array += float(200);
	timeValue += 5;
    }
    freq.put (0, freqValues);
    pol.put (0, polValues);
    indgen (array);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    timeValue = 34;
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
}

void readTable (const IPosition& dwShape)
{
    Table table("tTiledShapeStMan_tmp.data");
    ROArrayColumn<float> freq (table, "Freq");
    ROArrayColumn<float> pol (table, "Pol");
    ROArrayColumn<float> data (table, "Data");
    ROArrayColumn<float> weight (table, "Weight");
    ROScalarColumn<float> time (table, "Time");
    float timeValue;
    timeValue = 34;
    uInt i;
    for (i=0; i<table.nrow(); i++) {
	Array<float> result(dwShape);
	data.get (i, result);
	Array<float> array(result.shape());
	indgen (array, i*float(200));
	Vector<float> freqValues (result.shape()(1));
	Vector<float> polValues (result.shape()(0));
	indgen (freqValues, float(200));
	indgen (polValues, float(300));
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
}

void writeVar()
{
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", 1));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", 2));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.bindAll (sm1);
    Table table(newtab);

    Vector<float> freqValues(25);
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
    Matrix<float> array(IPosition(2,16,25));
    uInt i;
    indgen (array);
    for (i=0; i<5; i++) {
	table.addRow();
	cout << " pol.isDefined=" << pol.isDefined(i) << endl;
	pol.setShape (i, IPosition(1,16), IPosition(1,1));
	cout << " pol.isDefined=" << pol.isDefined(i) << endl;
	cout << "data.isDefined=" << data.isDefined(i) << endl;
	data.setShape (i, IPosition(2,16,25), IPosition(2,12,10));
	cout << "weig.isDefined=" << weight.isDefined(i) << endl;
	cout << "freq.isDefined=" << freq.isDefined(i) << endl;
	cout << pol.shape(i) << freq.shape(i) << data.shape(i)
	     << weight.shape(i) << endl;
	data.put (i, array);
	weight.put (i, array+float(100));
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
	array += float(200);
	timeValue += 5;
    }
}

void writeFixVar()
{
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", 1));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,25),
					   ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.bindAll (sm1);
    Table table(newtab);

    Vector<float> freqValues(25);
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
    Matrix<float> array(IPosition(2,16,25));
    uInt i;
    indgen (array);
    for (i=0; i<5; i++) {
	table.addRow();
	cout << " pol.isDefined=" << pol.isDefined(i) << endl;
	pol.setShape (i, IPosition(1,16), IPosition(1,1));
	cout << " pol.isDefined=" << pol.isDefined(i) << endl;
	cout << "data.isDefined=" << data.isDefined(i) << endl;
	data.setShape (i, IPosition(2,16,25), IPosition(2,12,10));
	cout << "weig.isDefined=" << weight.isDefined(i) << endl;
	cout << "freq.isDefined=" << freq.isDefined(i) << endl;
	cout << pol.shape(i) << freq.shape(i) << data.shape(i)
	     << weight.shape(i) << endl;
	data.put (i, array);
	weight.put (i, array+float(100));
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
	array += float(200);
	timeValue += 5;
    }
}

void writeVarShaped()
{
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", 1));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", 2));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.bindAll (sm1);
    Table table(newtab);

    Vector<float> polValues(16);
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    uInt i;
    for (i=0; i<10; i++) {
        uInt n2 = 10 + i%3;
	table.addRow();
	cout << " pol.isDefined=" << pol.isDefined(i) << endl;
	pol.setShape (i, IPosition(1,16), IPosition(1,1));
	cout << " pol.isDefined=" << pol.isDefined(i) << endl;
	cout << "data.isDefined=" << data.isDefined(i) << endl;
	data.setShape (i, IPosition(2,16,n2), IPosition(2,12,10));
	cout << "weig.isDefined=" << weight.isDefined(i) << endl;
	cout << "freq.isDefined=" << freq.isDefined(i) << endl;
	cout << pol.shape(i) << freq.shape(i) << data.shape(i)
	     << weight.shape(i) << endl;
	Matrix<float> array(IPosition(2,16,n2));
	indgen (array, i*float(200));
	Vector<float> freqValues(n2);
	indgen (freqValues, float(200));
	data.put (i, array);
	weight.put (i, array+float(100));
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
	timeValue += 5;
    }
}

void writeNoHyper()
{
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<float>  ("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,25),
					   ColumnDesc::FixedShape));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.setShapeColumn ("Freq", IPosition(1,25));
    newtab.setShapeColumn ("Data", IPosition(2,16,25));
    newtab.bindColumn ("Data", sm1);
    newtab.bindColumn ("Weight", sm1);
    Table table(newtab);

    Vector<float> freqValues(25);
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
    Matrix<float> array(IPosition(2,16,25));
    Matrix<float> result(IPosition(2,16,25));
    uInt i;
    indgen (array);
    for (i=0; i<101; i++) {
	table.addRow();
	data.put (i, array);
	weight.put (i, array+float(100));
	time.put (i, timeValue);
	array += float(200);
	timeValue += 5;
	freq.put (i, freqValues);
	pol.put (i, polValues);
    }
    indgen (array);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    timeValue = 34;
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
}


int main () {
    try {
	writeFixed();
	readTable (IPosition(2,16,25));
	writeVar();
	readTable (IPosition(2,16,25));
	writeFixVar();
	readTable (IPosition(2,16,25));
	writeVarShaped();
	readTable (IPosition());
	writeNoHyper();
	readTable (IPosition(2,16,25));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
