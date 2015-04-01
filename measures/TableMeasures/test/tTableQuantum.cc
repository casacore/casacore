//# tTableQuantum.cc: test program for Quantum columns in TableMeasures module
//# Copyright (C) 1997,1998,1999,2000,2001,2004
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

#include <casacore/measures/TableMeasures/TableQuantumDesc.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main (int argc, const char* argv[])
{
  Bool doExcep = (argc<2);
  uInt nrrow = 5000;
  if (argc >= 2) {
    istringstream istr(argv[1]);
    istr >> nrrow;
  }

  try {

    cout << "Begin tTableQuantum.cc.\n";

    // Need a table to work with.
    TableDesc td("tTableQuantum_tmp", "1", TableDesc::New);
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
    ArrayColumnDesc<Double> acdQD4("ArrQuantDoubleNonVar2",
	"A Quantum<double> array column with 2 units");
    ArrayColumnDesc<Double> acdQD2("ArrQuantScaUnits",
	"A Quantum<double> array column");
    ArrayColumnDesc<String> acdStr("varArrUnitsColumn",
	"String column for array of units");
    ScalarColumnDesc<String> ascdStr("varArrScaUnitsColumn",
	"Scalar string column for variable units per row");

    ArrayColumnDesc<Double> bogusCol("BogusQuantCol",
	"an array column but won't be made a quantum column");

    // These must be added to the table descriptor
    cout << "Adding column descriptors to the table...\n";
    td.addColumn(scdQD);
    td.addColumn(scdQC);
    td.addColumn(acdQD);
    td.addColumn(acdQD2);
    td.addColumn(acdQD3);
    td.addColumn(acdQD4);
    td.addColumn(scdStr);
    td.addColumn(acdStr);
    td.addColumn(ascdStr);
    td.addColumn(bogusCol);

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

    Vector<String> un1(2);
    Vector<Unit> un2(2);
    un1(0) = "MHz";
    un1(1) = "GHz";
    un2(0) = "kHz";
    un2(1) = "MHz";
    TableQuantumDesc tqdSQC(td, "ScaQuantComplex", "varUnitsColumn");
    TableQuantumDesc tqdAQC(td, "ArrQuantDouble", "varArrUnitsColumn");
    TableQuantumDesc tqdAQC2(td, "ArrQuantScaUnits", "varArrScaUnitsColumn");
    TableQuantumDesc tqdAQC3(td, "ArrQuantDoubleNonVar", Unit("MHz"));
    TableQuantumDesc tqdAQC4(td, "ArrQuantDoubleNonVar2", un1);
    cout << tqdAQC4.getUnits() << endl;
    TableQuantumDesc tqdAQC4a(td, "ArrQuantDoubleNonVar2", un2);
    cout << tqdAQC4a.getUnits() << endl;

    // test the exceptions
    if (doExcep) {
      cout << "Testing TableQuantumDesc constructor exceptions...\n";
      try {
	// no such column
	TableQuantumDesc taexcep(td, "SillyName");
      } catch (AipsError x) {
	cout << "A no such column message should follow\n";
	cout << x.getMesg() << endl;
      } 

      try {
	// variable unit's column doesn't exist.
	TableQuantumDesc taexcep(td, "ScaQuantComplex", "SillyName");
      } catch (AipsError x) {
	cout << "A no such unit's column message should follow\n";
	cout << x.getMesg() << endl;
      } 

      try {
	// The variable unit's column exists but the units type isn't String
	ScalarColumnDesc<Int> eucol("testvarcolumn",
		  "variable units column with incorrect type");
	td.addColumn(eucol);
	TableQuantumDesc taexcep(td, "ScaQuantComplex", "testvarcolumn");
      } catch (AipsError x) {
	cout << "A message about an incorrect variable unit's type...\n";
	cout << x.getMesg() << endl;
      } 
    }
    // ...and make them persistent.
    // the last one is done later after the table is created
    // (to test if write() works fine with the Table object).
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
    SetupNewTable newtab("tTableQuantum_tmp.tab", td, Table::New);
    Table qtab(newtab, 5);

    // 
    tqdAQC4.write(qtab);

    // Check that columns contain quanta.
    AlwaysAssertExit (TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "ScaQuantDouble")));
    AlwaysAssertExit (TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "ScaQuantComplex")));
    AlwaysAssertExit (! TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "varUnitsColumn")));
    AlwaysAssertExit (TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "ArrQuantDouble")));
    AlwaysAssertExit (TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "ArrQuantDoubleNonVar")));
    AlwaysAssertExit (TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "ArrQuantDoubleNonVar2")));
    AlwaysAssertExit (TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "ArrQuantScaUnits")));
    AlwaysAssertExit (! TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "varArrUnitsColumn")));
    AlwaysAssertExit (! TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "varArrScaUnitsColumn")));
    AlwaysAssertExit (! TableQuantumDesc::hasQuanta
		             ( TableColumn (qtab, "BogusQuantCol")));
    {
      // Play with a null object first
      cout << "Creating a null ScaQuantumCol()\n";
      ScalarQuantColumn<Double> sq1Col;
      ScalarQuantColumn<Double> sqCol(sq1Col);

      cout << "Check if isNull and then throwIfNull\n";
      if (sqCol.isNull()) {
	if (doExcep) {
	  // test isnull exception
	  try {
	    sqCol.throwIfNull();
	  } catch (AipsError x) {
	    cout << "Catch an AipsError. Column is null...\n";
	    cout << x.getMesg() << endl;
	  } 
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

      // This should be a quantum column with fixed units
      if (sqCol.isUnitVariable()) {
	cout << "Columns units: " << sqCol.getUnits() << endl;
      }

      // put some quanta into the columns.
      Quantum<Double> q;
      for (uInt i=0; i<qtab.nrow(); i++) {
	q.setValue(i * 3.12);
	q.setUnit ("rad");
	sqCol.put(i, q);
      }
      ScalarQuantColumn<Double> sq2Col(sqCol);
      sq2Col.throwIfNull();
    }
    {
      // Could also read values from sqCol but instead a ScalarQuantCol
      // is created here to do that.
      // test attach member for this first
      ScalarQuantColumn<Double> rosq1Col;
      ScalarQuantColumn<Double> rosqCol(rosq1Col);
      if (rosqCol.isNull()) {
	rosqCol.attach(qtab, "ScaQuantDouble");
      }
      rosqCol.throwIfNull();
      cout << "Column's quantum units are: " << rosqCol.getUnits() << endl;
      uInt i;
      for (i=0; i<qtab.nrow(); i++) {
	cout << "Quantum " << i << ": " << rosqCol(i) << endl;
      }

      // get them again but convert them to arcmin
      Quantum<Double> q;
      for (i=0; i<qtab.nrow(); i++) {
	cout << "Quantum arcmin " << i << ": "
	     << rosqCol(i, Unit("arcmin")) << endl;
      }

      // get them again but convert them to arcsec.
      rosqCol.attach(qtab, "ScaQuantDouble", "arcsec");
      for (i=0; i<qtab.nrow(); i++) {
	cout << "Quantum arcsec " << i << ": " << rosqCol(i) << endl;
      }
      for (i=0; i<qtab.nrow(); i++) {
	cout << "Quantum arcmin " << i << ": "
	     << rosqCol(i, "arcmin") << endl;
      }
      ScalarQuantColumn<Double> rosq2Col(rosqCol);
      rosq2Col.throwIfNull();
    }
    {
      // Store a column of complex quantums with variable units.
      ScalarQuantColumn<Complex> sqCol(qtab, "ScaQuantComplex");
      if (sqCol.isUnitVariable()) {
	cout << "The units for ScaQuantComplex are variable.\n";
	cout << "getUnits() should produce an empty string: "
	     << sqCol.getUnits() << endl;
      } else {
	cout << "The units for ScaQuantComplex are not variable.\n";
	cout << "This is an error.\n";
      }
      Quantum<Complex> q(Complex(4., 0.21), "deg");
      sqCol.put(0, q);
      cout << q.get("m/s") << endl;
      q.convert("ms");
      sqCol.put(1, q);
      cout << q.get("m/s") << endl;
      q.convert("g");
      sqCol.put(2, q);
      cout << q.get("m/s") << endl;
      q.convert("Jy");
      sqCol.put(3, q);
      cout << q.get("m/s") << endl;
      q.convert("GHz");
      sqCol.put(4, q);	
      cout << q.get("m/s") << endl;
    }
    {
      // Lets have a look at them
      ScalarQuantColumn<Complex> rosqCol(qtab, "ScaQuantComplex");
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

    cout << "\nFinished test of Quantum scalar column...........\n";
    cout << "\nStart test of Quantum array column...........\n";

    // Fill an array with quanta.
    IPosition shape(2, 3, 2);
    Array<Quantum<Double> > quantArr(shape);
    Bool deleteIt;
    Quantum<Double>* q_p = quantArr.getStorage(deleteIt);
    q_p->setValue(1.41212);
    q_p->setUnit("GHz");
    q_p++;
    q_p->setValue(1.4921);
    q_p->setUnit("MHz");
    q_p++;
    q_p->setValue(1.4111);
    q_p->setUnit("kHz");
    q_p++;
    q_p->setValue(1.4003);
    q_p->setUnit("Hz");
    q_p++;
    q_p->setValue(1.22);
    q_p->setUnit("GHz");
    q_p++;
    q_p->setValue(1.090909);
    q_p->setUnit("Hz");
    quantArr.putStorage(q_p, deleteIt);

    {
      // Now for array columns.  This set up a Quant Array column with
      // variable units where the units vary per array element.
      ArrayQuantColumn<Double> tmpCol;
      if (doExcep) {
	try {
	  tmpCol.throwIfNull();
	} catch (AipsError x) {
	  cout << "Catch an AipsError. Array column is null...\n";
	  cout << x.getMesg() << endl;
	} 

	// test attaching a bogus quantum column
	try {
	  // create with a real column but not a quantum column
	  ArrayQuantColumn<Double> testCol(qtab, "BogusQuantCol");
	} catch (AipsError x) {
	  cout << "Exception should report not a quantum column...\n";
	  cout << x.getMesg() << endl;
	} 
      }
      if (tmpCol.isNull()) {
	cout << "Array Quantum Column is initially null.\n";
	tmpCol.attach(qtab, "ArrQuantDouble");
      }
      // cover copy constructor
      ArrayQuantColumn<Double> aqCol = tmpCol;
      aqCol.throwIfNull();
      if (aqCol.isUnitVariable()) {
	cout << "Array quantum column: units are variable.\n";
      } else {
	cout << "Array quantum column units: " << aqCol.getUnits() << endl;
      }

      // cover putting an empty array (which should be OK)
      Array<Quantum<Double> > emptyArr;
      aqCol.put(0, emptyArr);

      // put the quantum array in the column (having variable units).
      aqCol.put(0, quantArr);
    }
    {
      ArrayQuantColumn<Double> roaqColx(qtab, "ArrQuantDouble");
      ArrayQuantColumn<Double> roaqCol(roaqColx);

      // test array conformance error exception on get()
      if (doExcep) {
	try {
	  Array<Quantum<Double> > badShapeArr(IPosition(2,2));
	  roaqCol.get(0, badShapeArr, False);
	} catch (AipsError x) {
	  cout << "The following line should be a ";
	  cout << "Table array conformance error exception.\n";
	  cout << x.getMesg() << endl;
	} 
      }
      {
	// This should succeed.
	Array<Quantum<Double> > badShapeArr(IPosition(2,2));
	roaqCol.get(0, badShapeArr, True);
	cout << badShapeArr << endl;
      }

      cout << roaqCol(0) << endl;
      cout << roaqCol(0, "Hz") << endl;

      ArrayQuantColumn<Double> roaqCol1(qtab, "ArrQuantDouble", "kHz");
      cout << roaqCol1(0) << endl;
      cout << roaqCol1(0, "Hz") << endl;
      cout << roaqCol1(0, un2) << endl;

      ArrayQuantColumn<Double> roaqCol2;
      roaqCol2.attach (qtab, "ArrQuantDouble");
      roaqCol2.attach (qtab, "ArrQuantDouble", "MHz");
      roaqCol2.attach (qtab, "ArrQuantDouble", un2);
      roaqCol2.reference (roaqCol1);

      ArrayQuantColumn<Double> roaqCol3(qtab, "ArrQuantDouble", un2);
      cout << roaqCol3(0) << endl;
      cout << roaqCol3(0, "Hz") << endl;
    }
    {
      // A second ArrayQuantColumn with variable units but in this case
      // the units only vary once per row as opposed to per array element
      // per row as in the example above.  This can be done because the
      // TableQuantDesc for the row specified a ScalarColumn as the
      // units column.
      // Could just construct the column completely but test attach member
      ArrayQuantColumn<Double> aqCol;
      aqCol.attach(qtab, "ArrQuantScaUnits");
      if (aqCol.isUnitVariable()) {
	cout << "Array quantum column: units are variable.\n";
      } else {
	cout << "Array quantum column units: " << aqCol.getUnits() << endl;
      }

      // cover putting an empty array (which should be OK)
      Array<Quantum<Double> > emptyArr;
      aqCol.put(0, emptyArr);

      // Put the quantum array in the column
      // Use unit "kHz" for the 2nd row.
      aqCol.put(0, quantArr);
      quantArr(IPosition(2,0,0)).setUnit ("kHz");
      aqCol.put(1, quantArr);
    }
    {
      // another way of creating the object
      ArrayQuantColumn<Double> roaqCol(qtab, "ArrQuantScaUnits");
      cout << roaqCol(0) << endl;
      cout << roaqCol(0, "Hz") << endl;
      cout << roaqCol(1) << endl;
      cout << roaqCol(1, "Hz") << endl;

      Quantum<Double> q(0.21, "Hz");
      cout << roaqCol(0, q);
    }
    {
      // These should complete the coverage of the class
      // contructor
      ArrayQuantColumn<Double> aqc(qtab, "ArrQuantDouble");
      // copy constructor
      ArrayQuantColumn<Double> aqc1 = aqc;
      // attach
      ArrayQuantColumn<Double> aqc2;
      aqc2.attach(qtab, "ArrQuantDouble");

      // cover putting an empty array (which should be OK)
      Array<Quantum<Double> > emptyArr;

      // non-variable units column
      ArrayQuantColumn<Double> aqc3(qtab, "ArrQuantDoubleNonVar");
      aqc3.put(0, emptyArr);
      aqc3.put(0, quantArr);
      ArrayQuantColumn<Double> aqc4(qtab, "ArrQuantDoubleNonVar2");
      aqc4.put(0, emptyArr);
      aqc4.put(0, quantArr);

      ArrayQuantColumn<Double> aqc3a(qtab, "ArrQuantDoubleNonVar");
      cout << aqc3a.getUnits() << endl;
      cout << aqc3a(0) << endl;
      ArrayQuantColumn<Double> aqc4a(qtab, "ArrQuantDoubleNonVar2");
      cout << aqc4a.getUnits() << endl;
      cout << aqc4a(0) << endl;
    }

  } catch (AipsError x) {
    cout << "Unexpected exception1: " << x.getMesg() << endl;
    return 1;
  } 

  // Try it with a readonly table.
  try {
    Table qtab ("tTableQuantum_tmp.tab");
    // Could also read values from sqCol but instead a ScalarQuantCol
    // is created here to do that.
    // test attach member for this first
    ScalarQuantColumn<Double> rosqCol;
    if (rosqCol.isNull()) {
      rosqCol.attach(qtab, "ScaQuantDouble");
    }
    rosqCol.throwIfNull();
    cout << "Column's quantum units are: " << rosqCol.getUnits() << endl;
    uInt i;
    for (i=0; i<qtab.nrow(); i++) {
      cout << "Quantum " << i << ": " << rosqCol(i) << endl;
    }
  } catch (AipsError x) {
    cout << "Unexpected exception2: " << x.getMesg() << endl;
    return 1;
  } 

  // Now test the performance by putting arrays in nrrow rows.
  try {
    Table qtab ("tTableQuantum_tmp.tab", Table::Update);
    qtab.addRow(nrrow);
    IPosition shape(2, 3, 2);
    Array<Quantum<Double> > aqArr(shape);
    aqArr = Quantum<Double>(1.41212, "GHz");
    Array<Double> tabArr(shape);
    tabArr = 1.41212;
    ArrayQuantColumn<Double> aqCol(qtab, "ArrQuantDouble");
    ArrayQuantColumn<Double> aqCol2(qtab, "ArrQuantDoubleNonVar");
    ArrayColumn<Double> tabCol(qtab, "ArrQuantDouble");
    cout << ">>>" << endl;
    Timer timer;
    for (uInt i=0; i<nrrow; i++) {
      aqCol.put (i, aqArr);
    }
    timer.show ("put tq var arrays");
    timer.mark();
    for (uInt i=0; i<nrrow; i++) {
      aqCol2.put (i, aqArr);
    }
    timer.show ("put tq fix arrays");
    timer.mark();
    for (uInt i=0; i<nrrow; i++) {
      tabCol.put (i, tabArr);
    }
    timer.show ("put tab    arrays");
    timer.mark();
    for (uInt i=0; i<nrrow; i++) {
      aqCol.get (i, aqArr);
    }
    timer.show ("get tq var arrays");
    timer.mark();
    for (uInt i=0; i<nrrow; i++) {
      aqCol2.get (i, aqArr);
    }
    timer.show ("get tq fix arrays");
    timer.mark();
    for (uInt i=0; i<nrrow; i++) {
      tabCol.get (i, tabArr);
    }
    timer.show ("get tab    arrays");
    cout << "<<<" << endl;
  } catch (AipsError x) {
    cout << "Unexpected exception3: " << x.getMesg() << endl;
    return 1;
  } 

  cout << "\nExecution of tTableQuantum.cc ended normally.\n";
  return 0;
}
