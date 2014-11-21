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

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the TiledShapeStMan class.
// </summary>

// This program tests the class TiledShapeStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// First build a description.
void writeFixed (const TSMOption& tsmOpt)
{
    cout << "WriteFixed ..." << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<Complex>("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Bool>   ("Flag", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,25),
					   ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Flag,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    // Let the tile shape not fit integrally in the cube shape.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.setShapeColumn ("Freq", IPosition(1,25));
    newtab.setShapeColumn ("Data", IPosition(2,16,25));
    newtab.setShapeColumn ("Flag", IPosition(2,16,25));
    newtab.bindAll (sm1);
    Table table(newtab, 0, False, Table::LittleEndian, tsmOpt);

    Vector<float> freqValues(25);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<Complex> data (table, "Data");
    ArrayColumn<Bool> flag (table, "Flag");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    Matrix<Complex> darray(IPosition(2,16,25));
    Matrix<Bool> farray(IPosition(2,16,25));
    Matrix<float> warray(IPosition(2,16,25));
    Matrix<Complex> dresult(IPosition(2,16,25));
    Matrix<Bool> fresult(IPosition(2,16,25));
    Matrix<float> wresult(IPosition(2,16,25));
    indgen (darray);
    indgen (warray);
    for (uInt i=0; i<101; i++) {
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	table.addRow();
	data.put (i, darray);
        flag.put (i, farray);
	weight.put (i, warray);
	time.put (i, timeValue);
        darray += Complex(100, 10);
	warray += float(200);
	timeValue += 5;
    }
    freq.put (0, freqValues);
    pol.put (0, polValues);
    indgen (darray);
    indgen (warray);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    timeValue = 34;
    for (uInt i=0; i<table.nrow(); i++) {
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	data.get (i, dresult);
	if (! allEQ (darray, dresult)) {
	    cout << "mismatch in data row " << i << endl;
	}
	flag.get (i, fresult);
	if (! allEQ (farray, fresult)) {
	    cout << "mismatch in flag row " << i << endl;
	}
	weight.get (i, wresult);
	if (! allEQ (warray, wresult)) {
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
	darray += Complex(100, 10);
	warray += float(200);
	timeValue += 5;
    }
}

void readTable (const IPosition& dwShape, const TSMOption& tsmOpt)
{
  Table table("tTiledShapeStMan_tmp.data", Table::Old, tsmOpt);
    cout << "Checking " << table.nrow() << " rows" << endl;
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    ArrayColumn<Complex> data (table, "Data");
    ArrayColumn<Bool> flag (table, "Flag");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    float timeValue;
    timeValue = 34;
    for (uInt i=0; i<table.nrow(); i++) {
	Array<Complex> dresult(dwShape);
	Array<Bool> fresult(dwShape);
	Array<float> wresult(dwShape);
	data.get (i, dresult);
	flag.get (i, fresult);
	weight.get (i, wresult);
	Array<Complex> darray(dresult.shape());
	Array<Bool> farray(fresult.shape());
	Array<float> warray(wresult.shape());
	indgen (darray, float(i)*Complex(100,10));
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	indgen (warray, i*float(200));
	Vector<float> freqValues (dresult.shape()(1));
	Vector<float> polValues (dresult.shape()(0));
	indgen (freqValues, float(200));
	indgen (polValues, float(300));
	if (! allEQ (darray, dresult)) {
	    cout << "mismatch in data row " << i << endl;
            cout << dresult;
	}
	if (! allEQ (farray, fresult)) {
	    cout << "mismatch in flag row " << i << endl;
	}
	if (! allEQ (warray, wresult)) {
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
	timeValue += 5;
    }
}

void writeVar(const TSMOption& tsmOpt)
{
    cout << "WriteVar ..." << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", 1));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<Complex>("Data", 2));
    td.addColumn (ArrayColumnDesc<Bool>   ("Flag", 2));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", 2));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Flag,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    // Let the tile shape fit integrally in the cube shape.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,4,5));
    newtab.bindAll (sm1);
    Table table(newtab, 0, False, Table::BigEndian, tsmOpt);

    Vector<float> freqValues(25);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<Complex> data (table, "Data");
    ArrayColumn<Bool> flag (table, "Flag");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    Matrix<Complex> darray(IPosition(2,16,25));
    Matrix<Bool> farray(IPosition(2,16,25));
    Matrix<float> warray(IPosition(2,16,25));
    indgen (darray);
    indgen (warray);
    for (uInt i=0; i<5; i++) {
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
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	data.put (i, darray);
	flag.put (i, farray);
	weight.put (i, warray);
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
        darray += Complex(100, 10);
	warray += float(200);
	timeValue += 5;
    }
}

void writeFixVar(const TSMOption& tsmOpt)
{
    cout << "WriteFixVar ..." << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", 1));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<Complex>("Data", 2));
    td.addColumn (ArrayColumnDesc<Bool>   ("Flag", IPosition(2,16,25),
                                           ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,25),
					   ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Flag,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    // Let the tile shape match the cube shape.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,16,25,1));
    newtab.bindAll (sm1);
    Table table(newtab, 0, False, Table::LocalEndian, tsmOpt);

    Vector<float> freqValues(25);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<Complex> data (table, "Data");
    ArrayColumn<Bool> flag (table, "Flag");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    Matrix<Complex> darray(IPosition(2,16,25));
    Matrix<Bool> farray(IPosition(2,16,25));
    Matrix<float> warray(IPosition(2,16,25));
    indgen (darray);
    indgen (warray);
    for (uInt i=0; i<5; i++) {
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
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	data.put (i, darray);
	flag.put (i, farray);
	weight.put (i, warray);
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
        darray += Complex(100, 10);
	warray += float(200);
	timeValue += 5;
    }
}

void writeVarShaped(const TSMOption& tsmOpt)
{
    cout << "WriteVarShaped ..." << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", 1));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<Complex>("Data", 2));
    td.addColumn (ArrayColumnDesc<Bool>   ("Flag", 2));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", 2));
    td.defineHypercolumn ("TSMExample",
			  3,
			  stringToVector ("Data,Flag,Weight"),
			  stringToVector ("Pol,Freq,Time"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.bindAll (sm1);
    Table table(newtab, 0, False, Table::LittleEndian, tsmOpt);

    Vector<float> polValues(16);
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<Complex> data (table, "Data");
    ArrayColumn<Bool> flag (table, "Flag");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    for (uInt i=0; i<10; i++) {
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
	Matrix<Complex> darray(IPosition(2,16,n2));
	Matrix<Bool> farray(IPosition(2,16,n2));
	Matrix<float> warray(IPosition(2,16,n2));
	indgen (darray, float(i)*Complex(100, 10));
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	indgen (warray, i*float(200));
	Vector<float> freqValues(n2);
	indgen (freqValues, float(200));
	data.put (i, darray);
	flag.put (i, farray);
	weight.put (i, warray);
	freq.put (i, freqValues);
	pol.put (i, polValues);
	time.put (i, timeValue);
	timeValue += 5;
    }
}

void writeNoHyper(const TSMOption& tsmOpt)
{
    cout << "WriteNoHyper ..." << endl;
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float>  ("Pol", IPosition(1,16),
					   ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ScalarColumnDesc<float> ("Time"));
    td.addColumn (ArrayColumnDesc<Complex>("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Bool>   ("Flag", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>  ("Weight", IPosition(2,16,25),
					   ColumnDesc::FixedShape));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(2,5,6));
    newtab.setShapeColumn ("Freq", IPosition(1,25));
    newtab.setShapeColumn ("Data", IPosition(2,16,25));
    newtab.setShapeColumn ("Flag", IPosition(2,16,25));
    newtab.bindColumn ("Data", sm1);
    newtab.bindColumn ("Flag", sm1);
    newtab.bindColumn ("Weight", sm1);
    Table table(newtab, 0, False, Table::BigEndian, tsmOpt);

    Vector<float> freqValues(25);
    Vector<float> polValues(16);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    float timeValue;
    timeValue = 34;
    ArrayColumn<Complex> data (table, "Data");
    ArrayColumn<Bool> flag (table, "Flag");
    ArrayColumn<float> weight (table, "Weight");
    ScalarColumn<float> time (table, "Time");
    ArrayColumn<float> freq (table, "Freq");
    ArrayColumn<float> pol (table, "Pol");
    Matrix<Complex> darray(IPosition(2,16,25));
    Matrix<Bool> farray(IPosition(2,16,25));
    Matrix<float> warray(IPosition(2,16,25));
    Matrix<Complex> dresult(IPosition(2,16,25));
    Matrix<Bool> fresult(IPosition(2,16,25));
    Matrix<float> wresult(IPosition(2,16,25));
    indgen (darray);
    indgen (warray);
    for (uInt i=0; i<101; i++) {
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	table.addRow();
	data.put (i, darray);
	flag.put (i, farray);
	weight.put (i, warray);
	time.put (i, timeValue);
        darray += Complex(100, 10);
	warray += float(200);
	timeValue += 5;
	freq.put (i, freqValues);
	pol.put (i, polValues);
    }
    indgen (darray);
    indgen (warray);
    indgen (freqValues, float(200));
    indgen (polValues, float(300));
    timeValue = 34;
    for (uInt i=0; i<table.nrow(); i++) {
        for (uInt j=0; j<farray.nelements(); ++j) {
            farray.data()[j] = ((i+j)%(i+2) == 0);
        }
	data.get (i, dresult);
	if (! allEQ (darray, dresult)) {
	    cout << "dmismatch in data row " << i << endl;
	}
	flag.get (i, fresult);
	if (! allEQ (farray, fresult)) {
	    cout << "dmismatch in flag row " << i << endl;
	}
	weight.get (i, wresult);
	if (! allEQ (warray, wresult)) {
	    cout << "dmismatch in weight row " << i << endl;
	}
	if (! allEQ (freq(i), freqValues)) {
	    cout << "dmismatch in freq row " << i << endl;
	}
	if (! allEQ (pol(i), polValues)) {
	    cout << "dmismatch in pol row " << i << endl;
	}
	if (time(i) != timeValue) {
	    cout << "dmismatch in time row " << i << endl;
	}
        darray += Complex(100, 10);
	warray += float(200);
	timeValue += 5;
    }
}

// Tests for the non-existence of a specific bug that happened
// when writing boolean values using TSMOption::Buffer
void writeFlags()
{
    TSMOption tsmOpt = TSMOption::Buffer;

    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<Bool>   ("Flag", 2, ColumnDesc::FixedShape));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledShapeStMan_tmp.data", td, Table::New);

    // Create a storage manager for it.
    TiledShapeStMan sm1 ("TSMExample", IPosition(3,1,7,2));
    newtab.setShapeColumn ("Flag", IPosition(2,16,25));
    newtab.bindColumn ("Flag", sm1);
    Table table(newtab, 0, False, Table::BigEndian, tsmOpt);

    ArrayColumn<Bool> flag (table, "Flag");
    Matrix<Bool> ones(IPosition(2,16,25));
    Matrix<Bool> zeros(IPosition(2,16,25));
    Matrix<Bool> fresult(IPosition(2,16,25));

    table.addRow();
    table.addRow();
    table.addRow();
    table.addRow();

    for (uInt j=0; j < fresult.nelements(); ++j) {
        zeros.data()[j] = 0;
        ones.data()[j] = 1;
    }

    flag.put (3, zeros);
    flag.get (3, fresult);
    if (! allEQ (fresult, zeros)) {
        cout << "Problem writing row 3" << endl;
        return;
    }

    flag.put (0, ones);
    flag.put (1, ones);
    flag.put (2, ones);

    flag.get (3, fresult);

    if (! allEQ (fresult, zeros)) {
        cout << "Row 3 has changed since it was written!" << endl;
    }

    return;
}


int main () {
    try {
        writeFixed (TSMOption::MMap);
	readTable (IPosition(2,16,25), TSMOption::Buffer);
	writeVar (TSMOption::Buffer);
	readTable (IPosition(2,16,25), TSMOption::Cache);
        writeFixVar (TSMOption::Cache);
	readTable (IPosition(2,16,25), TSMOption::MMap);
	writeVarShaped (TSMOption::Default);
	readTable (IPosition(), TSMOption::Aipsrc);
	writeNoHyper (TSMOption::Aipsrc);
	readTable (IPosition(2,16,25), TSMOption::Default);

        writeFlags();

    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
