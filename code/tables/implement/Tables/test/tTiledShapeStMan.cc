//# tTiledShapeStMan.cc: Test program for the TiledShapeStMan classes
//# Copyright (C) 1998
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
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/TiledShapeStMan.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>

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
    indgen (freqValues.ac(), float(200));
    indgen (polValues.ac(), float(300));
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
    indgen (array.ac());
    for (i=0; i<101; i++) {
	table.addRow();
	data.put (i, array);
	weight.put (i, array.ac()+float(100));
	time.put (i, timeValue);
	array.ac() += float(200);
	timeValue += 5;
    }
    freq.put (0, freqValues);
    pol.put (0, polValues);
    indgen (array.ac());
    indgen (freqValues.ac(), float(200));
    indgen (polValues.ac(), float(300));
    timeValue = 34;
    for (i=0; i<table.nrow(); i++) {
	data.get (i, result);
	if (! allEQ (array.ac(), result.ac())) {
	    cout << "mismatch in data row " << i << endl;
	}
	weight.get (i, result);
	if (! allEQ (array.ac() + float(100), result.ac())) {
	    cout << "mismatch in weight row " << i << endl;
	}
	if (! allEQ (freq(i), freqValues.ac())) {
	    cout << "mismatch in freq row " << i << endl;
	}
	if (! allEQ (pol(i), polValues.ac())) {
	    cout << "mismatch in pol row " << i << endl;
	}
	if (time(i) != timeValue) {
	    cout << "mismatch in time row " << i << endl;
	}
	array.ac() += float(200);
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
	indgen (array.ac(), i*float(200));
	Vector<float> freqValues (result.shape()(1));
	Vector<float> polValues (result.shape()(0));
	indgen (freqValues.ac(), float(200));
	indgen (polValues.ac(), float(300));
	if (! allEQ (array.ac(), result.ac())) {
	    cout << "mismatch in data row " << i << endl;
	}
	weight.get (i, result);
	if (! allEQ (array.ac() + float(100), result.ac())) {
	    cout << "mismatch in weight row " << i << endl;
	}
	if (! allEQ (freq(i), freqValues.ac())) {
	    cout << "mismatch in freq row " << i << endl;
	}
	if (! allEQ (pol(i), polValues.ac())) {
	    cout << "mismatch in pol row " << i << endl;
	}
	if (time(i) != timeValue) {
	    cout << "mismatch in time row " << i << endl;
	}
	array.ac() += float(200);
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
    indgen (freqValues.ac(), float(200));
    indgen (polValues.ac(), float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    Matrix<float> array(IPosition(2,16,25));
    uInt i;
    indgen (array.ac());
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
	weight.put (i, array.ac()+float(100));
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
	array.ac() += float(200);
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
    indgen (freqValues.ac(), float(200));
    indgen (polValues.ac(), float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<float> data (table, "Data");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    Matrix<float> array(IPosition(2,16,25));
    uInt i;
    indgen (array.ac());
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
	weight.put (i, array.ac()+float(100));
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
	array.ac() += float(200);
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
    indgen (polValues.ac(), float(300));
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
	indgen (array.ac(), i*float(200));
	Vector<float> freqValues(n2);
	indgen (freqValues.ac(), float(200));
	data.put (i, array);
	weight.put (i, array.ac()+float(100));
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
	timeValue += 5;
    }
}


main () {
    try {
	writeFixed();
	readTable (IPosition(2,16,25));
	writeVar();
	readTable (IPosition(2,16,25));
	writeFixVar();
	readTable (IPosition(2,16,25));
	writeVarShaped();
	readTable (IPosition());
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    return 0;                           // exit with success status
}
