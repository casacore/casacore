//# tTableQuantum.cc: test program for Quantum columns in the TableMeasures
//#   	    	      module.
//# Copyright (C) 1994,1995,1996,1997,1998
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Exceptions.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Mathematics/Complex.h>
#include <trial/TableMeasures/TableQuantumDesc.h>
#include <trial/TableMeasures/ArrayQuantColumn.h>
#include <trial/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableDesc.h>
#include <iostream.h>

int main(int argc)
{
  try {
    Bool doExcep = ToBool(argc<2);

    cout << "Begin tTableQuantum.cc.\n";
    
    // Need a table to work with.  
    TableDesc td("tTableQuantum", "1", TableDesc::New);
    td.comment() = "Created by tTableQuantum.cc";
    
    // This test uses two ScalarQuantum columns. They will be a 
    // Quantum<Double> and a for Quantum<Complex> columns.  The Quantum<Double>
    // will have static "deg" units but the units for the Quantum<Complex>
    // column will be variable.  This requires an additional String column for
    // storing the units.
    ScalarColumnDesc<Double> scdQD("ScaQuantDouble", 
	"A scalar column of Quantum<Double> with units 'deg'.");
    ScalarColumnDesc<Complex> scdQC("ScaQuantComplex", 
	"A scalar column Quantum<Complex> with variable units.");
    ScalarColumnDesc<String> scdStr("varUnitsColumn", 
	"Units columns for column ScaQuantComplex");

    
    // Also create ArrayQuantum columns for the test
    ArrayColumnDesc<Double> acdQD("ArrQuantDouble", 
	"A Quantum<double> array column");
    ArrayColumnDesc<Double> acdQD3("ArrQuantDoubleNonVar", 
	"A Quantum<double> array column");
    ArrayColumnDesc<Double> acdQD2("ArrQuantScaUnits", 
	"A Quantum<double> array column");
    ArrayColumnDesc<String> acdStr("varArrUnitsColumn", 
	"String column for array of units");
    ScalarColumnDesc<String> ascdStr("varArrScaUnitsColumn", 
	"Scalar string column for variable units per row");
    
    // These must be added to the table descriptor
    cout << "Adding column descriptors to the table...\n";
    td.addColumn(scdQD);
    td.addColumn(scdQC);
    td.addColumn(acdQD);
    td.addColumn(acdQD2);
    td.addColumn(acdQD3);
    td.addColumn(scdStr);
    td.addColumn(acdStr);
    td.addColumn(ascdStr);
    
    // Now create the Table Quantum Descriptors.  Three are used below but
    // a couple of dummy objects are created to test assignment and the copy
    // constructor.  The object we finally want is tqdSQD.
    TableQuantumDesc tqddummy(td, "ScaQuantDouble", Unit("deg"));
    // test copy constructor
    TableQuantumDesc tqddummy2 = tqddummy;
    // test empty unit
    TableQuantumDesc tqdSQD(td, "ScaQuantDouble");
    // test assignment
    tqdSQD = tqddummy2;

    TableQuantumDesc tqdSQC(td, "ScaQuantComplex", "varUnitsColumn");
    TableQuantumDesc tqdAQC(td, "ArrQuantDouble", "varArrUnitsColumn");
    TableQuantumDesc tqdAQC2(td, "ArrQuantScaUnits", "varArrScaUnitsColumn");
    TableQuantumDesc tqdAQC3(td, "ArrQuantDoubleNonVar", Unit("deg"));

    // test the exceptions
    if (doExcep) {
	cout << "Testing TableQuantumDesc constructor exceptions...\n";
	try {
	    // no such column
	    TableQuantumDesc taexcep(td, "SillyName");
	} catch (AipsError x) {
	    cout << "A no such column message should follow\n";
	    cout << x.getMesg() << endl;
	} end_try;
    
	try {
	    // variable unit's column doesn't exist.
	    TableQuantumDesc taexcep(td, "ScaQuantComplex", "SillyName");
	} catch (AipsError x) {
	    cout << "A no such unit's column message should follow\n";
	    cout << x.getMesg() << endl;
	} end_try;
    
	try {
	    // The vaiable unit's column exists but the units type isn't String
	    ScalarColumnDesc<Int> eucol("testvarcolumn", 
		  "variable units column with incorrect type");
	    td.addColumn(eucol);
	    TableQuantumDesc taexcep(td, "ScaQuantComplex", "testvarcolumn");
	} catch (AipsError x) {
	    cout << "A message about an incorrect variable unit's type...\n";
	    cout << x.getMesg() << endl;
	} end_try;
    }
    // ...and make them persistant.
    tqdSQD.write(td);
    tqdSQC.write(td);
    tqdAQC.write(td);
    tqdAQC2.write(td);
    tqdAQC3.write(td);

    cout << "Column's name is: " + tqdSQD.columnName() << endl;
    if (tqdSQD.isUnitVariable()) {
	cout << "Quantum column " + tqdSQD.columnName()
	    << " has variable units.\n";
	cout << "\tIts units are stored in String column '" 
	    + tqdSQD.unitColumnName() << "' \n";
    }
    
    cout << "Column's name is: " + tqdSQC.columnName() << endl;
    
    // create a table with 5 rows.
    SetupNewTable newtab("TestTableMeasures", td, Table::New);
    Table qtab(newtab, 5);
    
    {
	// Play with a null object first
	cout << "Creating a null ScaQuantumCol()\n";
	ScalarQuantColumn<Double> sqCol;

	cout << "Check if isNull and then throwIfNull\n";
	if (sqCol.isNull()) {
	    if (doExcep) {
		// test isnull exception
		try {
		    sqCol.throwIfNull();
		} catch (AipsError x) {
		    cout << "Catch an AipsError. Column is null...\n";
		    cout << x.getMesg() << endl;
		} end_try;
	    }
	    cout << "Object says it is null...attach a column\n";
	    sqCol.attach(qtab, "ScaQuantDouble");
	    if (sqCol.isNull()) {
		cout << "Apparantly still null...this isn't correct!\n";
	    } else {
		cout << "No longer null...good\n";
	    }
	    sqCol.throwIfNull();
	}
	
	// This should be a quantum column with static units
	if (sqCol.isUnitVariable()) {
	    cout << "Columns units: " << sqCol.getUnits() << endl;
	}
	
	// add some quantums to the columns.  This is a non-variable units
	// column so it doesn't matter what the units are.
	Quantum<Double> q;
	for (uInt i=0; i<qtab.nrow(); i++) {
	    q.setValue(i * 3.12);
	    sqCol.put(i, q);
	}    
    }
    {
	// Could also read values from sqCol but instead a ROScalarQuantCol 
	// is created here to do that.
	// test attach member for this first
	ROScalarQuantColumn<Double> rosqCol;
	if (rosqCol.isNull()) {
	    rosqCol.attach(qtab, "ScaQuantDouble");
	}
	rosqCol.throwIfNull();
	cout << "Column's quantum units are: " << rosqCol.getUnits() << endl;
	uInt i;
	for (i=0; i<qtab.nrow(); i++) {
	    cout << "Quantum " << i << ": " << rosqCol(i) << endl;
	}
	
	// get them again but convert them to m/s
	Quantum<Double> q(0, "deg");
	for (i=0; i<qtab.nrow(); i++) {
	    cout << "Quantum " << i << ": " << rosqCol(i, Unit("m/s")) << endl;
	}
    }
    {
	// Store a column of complex quantums with variable units.
	ScalarQuantColumn<Complex> sqCol(qtab, "ScaQuantComplex");
	if (sqCol.isUnitVariable()) {
	    cout << "The units for ScaQuantComplex are variable.\n";
	    cout << "getUnits() should produced an empty string: " 
		<< sqCol.getUnits() << endl;
	} else {
	    cout << "The units for ScaQuantComplex are not variable.\n";
	    cout << "This is an error.\n";
	}
	Quantum<Complex> q(Complex(4., 0.21), "deg");
	sqCol.put(0, q);
	q.convert("ms");
	sqCol.put(1, q);
	q.convert("g");
	sqCol.put(2, q);
	q.convert("Jy");
	sqCol.put(3, q);
	q.convert("GHz");
	sqCol.put(4, q);	
    }
    {
	// Lets have a look at them
	ROScalarQuantColumn<Complex> rosqCol(qtab, "ScaQuantComplex");
	uInt i;
	for (i=0; i<qtab.nrow(); i++) {
	    cout << "Complex quantum (var unit) " << i << ": " << rosqCol(i) 
		<< endl;
	}
	Quantum<Complex> q(1., "m/s");
	for (i=0; i<qtab.nrow(); i++) {
	    // this get the units converted to the units in q
	    cout << "Complex quantum (var unit) " << i << ": " 
		<< rosqCol(i, q) << endl;
	}
    }
    {
	// One more time but here the ROScalarQuantColumn is created with
	// default units.  Creating a ROScalarQuantum object with default
	// units doesn't do conversion.  It simply replaces the value for
	// unit which may or may not exist for this column.
	// Create a null column and attach a legitament one.
	ROScalarQuantColumn<Double> rosqCol;
	rosqCol.attach(qtab, "ScaQuantDouble", "m/s");
	Quantum<Double> q;
	// Retrieve 2 quantums one with conversion to GHz.
	rosqCol.get(q, 1);
	cout << "Quantum<Double> (units (m/s)) " << q << endl;
	rosqCol.get(q, 2, "GHz");
	cout << "Quantum<Double> (units (GHz)) " << q << endl;
    }

    {
	// Cover the functions we haven't so far tested
	// ScalarQuantColumn copy constructor
	ScalarQuantColumn<Double> sqcol(qtab, "ScaQuantDouble", "m/s");
	ScalarQuantColumn<Double> sqcolCpy = sqcol;
	ScalarQuantColumn<Double> sqcolNull;
	sqcolNull.attach(qtab, "ScaQuantDouble", "m/s");
    }

    cout << "\nFinished test of Quantum scalar column...........\n";
    cout << "\nStart test of Quantum array column...........\n";

    {
	// Now for array columns.  This set up a Quant Array column with
	// variable units where the units vary per array element.
	ArrayQuantColumn<Double> aqCol;
	if (doExcep) {
	  try {
	      aqCol.throwIfNull();
	  } catch (AipsError x) {
	      cout << "Catch an AipsError. Array column is null...\n";
	      cout << x.getMesg() << endl;
	  } end_try;
	}
	if (aqCol.isNull()) {
	    cout << "Array Quantum Column is initially null.\n";
	    aqCol.attach(qtab, "ArrQuantDouble");
	}
	aqCol.throwIfNull();
	if (aqCol.isUnitVariable()) {
	    cout << "Array quantum column: units are variable.\n";
	} else {
	    cout << "Array quantum column units: " << aqCol.getUnits() << endl;
	}
	
	IPosition shape(2, 3, 2);
	Array<Quantum<Double> > qArr(shape);
	Bool deleteIt;
	Quantum<Double>* q_p = qArr.getStorage(deleteIt);
	q_p->setValue(1.41212);
	q_p->setUnit("GHz");
	q_p++;
	q_p->setValue(1.4921);
	q_p->setUnit("deg");
	q_p++;    
	q_p->setValue(1.4111);
	q_p->setUnit("ms-1");
	q_p++;    
	q_p->setValue(1.4003);
	q_p->setUnit("Jy");
	q_p++;    
	q_p->setValue(1.22);
	q_p->setUnit("GHz");
	q_p++;    
	q_p->setValue(1.090909);
	q_p->setUnit("g");	
	qArr.putStorage(q_p, deleteIt);
	
	// put the quantum array in the column
	aqCol.put(0, qArr);
    }
    {
	ROArrayQuantColumn<Double> roaqCol(qtab, "ArrQuantDouble");
	cout << roaqCol(0) << endl;
	
	cout << roaqCol(0, "m/s") << endl;
    }    
    {
	// A second ArrayQuantColumn with variable units but in this case
	// the units only vary once per row as opposed to per array element
	// per row as in the example above.  This can be done because the
	// TableQuantDesc for the row specified a ScalarColumn as the
	// units column.
	ArrayQuantColumn<Double> aqCol(qtab, "ArrQuantScaUnits");
	if (aqCol.isUnitVariable()) {
	    cout << "Array quantum column: units are variable.\n";
	} else {
	    cout << "Array quantum column units: " << aqCol.getUnits() << endl;
	}
	
	IPosition shape(2, 3, 2);
	Array<Quantum<Double> > qArr1(shape);
	Array<Quantum<Double> > qArr2(shape);
	Bool deleteIt;
	Quantum<Double>* q_p1 = qArr1.getStorage(deleteIt);
	// when these are eventually stored in the column all will be stored
	// with the common unit of "GHz" as this is the unit value of the 
	// first Quantum in the array
	q_p1->setValue(1.41212);
	q_p1->setUnit("GHz");
	q_p1++;
	q_p1->setValue(1.4921);
	q_p1->setUnit("deg");
	q_p1++;    
	q_p1->setValue(1.4111);
	q_p1->setUnit("ms-1");
	q_p1++;    
	q_p1->setValue(1.4003);
	q_p1->setUnit("Jy");
	q_p1++;    
	q_p1->setValue(1.22);
	q_p1->setUnit("GHz");
	q_p1++;    
	q_p1->setValue(1.090909);
	q_p1->setUnit("g");	
	qArr1.putStorage(q_p1, deleteIt);
	
	Quantum<Double>* q_p2 = qArr2.getStorage(deleteIt);
	// For row 2 the unit will be "deg"
	q_p2->setValue(1.41212);
	q_p2->setUnit("deg");
	q_p2++;
	q_p2->setValue(1.4921);
	q_p2->setUnit("deg");
	q_p2++;    
	q_p2->setValue(1.4111);
	q_p2->setUnit("ms-1");
	q_p2++;    
	q_p2->setValue(1.4003);
	q_p2->setUnit("Jy");
	q_p2++;    
	q_p2->setValue(1.22);
	q_p2->setUnit("GHz");
	q_p2++;    
	q_p2->setValue(1.090909);
	q_p2->setUnit("g");	
	qArr2.putStorage(q_p2, deleteIt);
	
	// put the quantum array in the column
	aqCol.put(0, qArr1);
	aqCol.put(1, qArr2);
    }
    {
	// another way of creating the object
	ROArrayQuantColumn<Double> roaqCol(qtab, "ArrQuantDouble", "deg");
	cout << roaqCol(0) << endl;	
	cout << roaqCol(0, "m/s") << endl;

	
	Quantum<Double> q(0.21, "deg");
	cout << roaqCol(0, q);
    }    
    {
	// another way of creating the object
	ROArrayQuantColumn<Double> roaqCol;
	roaqCol.attach(qtab, "ArrQuantDouble", "m/s");
	cout << roaqCol(0) << endl;	
	cout << roaqCol(0, "deg") << endl;

	// the copy constructor
	ROArrayQuantColumn<Double> roCpyCol = roaqCol;

	// another test of attach
	ROArrayQuantColumn<Double> roNullCol;
	roNullCol.attach(qtab, "ArrQuantDouble");
    }
    {
	// These should complete the coverage of the class
	// contructor
	ArrayQuantColumn<Double> aqc(qtab, "ArrQuantDouble", "m/s");
	// copy constructor
	ArrayQuantColumn<Double> aqc1 = aqc;
	// attach
	ArrayQuantColumn<Double> aqc2;
	aqc2.attach(qtab, "ArrQuantDouble", "m/s");

	// non-variable units column
	ROArrayQuantColumn<Double> aqc3(qtab, "ArrQuantDoubleNonVar");
	cout << aqc3.getUnits() << endl;
    }
    
    cout << "End tTableQuantum.cc.\n";
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;
  
    exit(0);
}
