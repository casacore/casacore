//# tTableMeasures.cc: test program for the TableMeasures class.
//# Copyright (C) 1994,1995,1996,1997,1998,1999
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

#include <iostream.h>
#include <aips/aips.h>
#include <aips/Exceptions.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Measures/MBaseline.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEarthMagnetic.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/Muvw.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableRecord.h>
#include <trial/TableMeasures/ArrayMeasColumn.h>
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/TableMeasValueDesc.h>
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <trial/TableMeasures/TableMeasDesc.h>

int main(int argc)
{
  try {
    Bool doExcep = ToBool(argc<2);
    if (doExcep)
	cout << "Test of TableMeasures classes.\n";
    else 
	cout << "Test of TableMeasures classes without execeptions.\n";

    // Need a table to work with.
    TableDesc td("tTableMeasure_desc", "1", TableDesc::New);
    td.comment() = "A test of TableMeasures class.";
    
    // Each measure column needs at exactly one ArrayColumn<Double> for storing
    // the value component of each measure.  Additional columns are required
    // for storing measure offsets and measure references when these components
    // vary per row.  The value column is always an ArrayColumn irrespective
    // of whether Scalar or Array measures are to be stored.

    // A scalar column of MPosition.  Static reference so no addional 
    // columns required.
    ArrayColumnDesc<Double> cdMPos("MPosColumn", "Simple mposition column");

    // A scalar MEpoch column with a fixed offset and refernce.  Fixed
    // references and offsets do not need additional columns as they are
    // stored as keywords.
    ArrayColumnDesc<Double> cdTOffset("TimeOffset", 
		  "MEpoch column with fix reference and offset");


    // The following three columns will be used to set up a Scalar MEpoch
    // column with variable references and offsets.  3 columns are needed.
    // The "main" column where the MEpoch will be stored
    ArrayColumnDesc<Double> cdTime("Time1", "An MEpoch column");
    // For the offsets. Offsets are also measures so this is effectively
    // another measure column.
    ArrayColumnDesc<Double> cdVarOffset("TimeVarOffset", 
					"Variable Offset col");
    // an int column for the variable references
    ScalarColumnDesc<Int> cdTimeRef("TimeRef", "Reference column for Time1");


    // a scalar measure column with a variable string reference
    // a column for the measures.  No offset or it is to be static so 
    // no offset column required.
    // The "main" column.
    ArrayColumnDesc<Double> cdMEVS("MEpochVarStr", "Another MEpoch column");
    // a string column for the variable string references
    ScalarColumnDesc<String> cdTimeRefStr("TimeRefStr", 
					  "String variable reference column");
    
    // An array measure column with a variable (int) reference
    // A column for the measures
    ArrayColumnDesc<Double> cdTimeArr("Time1Arr", "An MEpoch array column");
    // An int column for the variable references.
    ScalarColumnDesc<Int> cdTimeArrRef("TimeArrRef", "VarRef co for TimeArr");

    // An array measure column with a variable (int) reference array column
    // and a variable offset column
    ArrayColumnDesc<Double> cdTime2Arr("Time2Arr", "An MEpoch array column");
    // the offset column
    ArrayColumnDesc<Double> cdTime2ArrOffset("Time2ArrOffset", 
					     "Offset column for Time2Arr");
    // the reference column
    ArrayColumnDesc<Int> cdTime2ArrRef("Time2ArrRef", 
				      "Ref column for Time2Arr");

    // An array measure column with variable (string) reference array column
    // The "main" date column
    ArrayColumnDesc<Double> cdTime3Arr("Time3Arr", "An MEpoch array column");
    // The string array column for the references
    ArrayColumnDesc<String> cdTime3StrRef("Time3ArrStrRef",
					  "Array string reference column");

    // An array measure column with a scalar reference (string) column and
    // a scalar (per row) offset column.
    // That is, one reference stored per row
    // The "main" date column
    ArrayColumnDesc<Double> cdTime4Arr("Time4Arr", "An MEpoch array column");
    // A scalar column for the references
    ScalarColumnDesc<String> cdTime4StrRef("Time4StrRef",
					   "Scalar int reference column");
    // An array column for the variable offsets.  Even though we want to
    // stored offsets per row the column must be an Array column because
    // offsets are Measures, i.e., offsets are stored in a Measure column
    ArrayColumnDesc<Double> cdTime4ScaOffset("Time4ScaOffset",
					     "Scalar offset column");

    // a "spare" column used for testing purposes
    ArrayColumnDesc<Double> cdTestCol("SpareCol1",
				      "Test of exception column");
    // a spare offset column
    ArrayColumnDesc<Double> cdTestArrOffset("SpareArrOffset",
					    "Spare int array column");
    

    // All of the above column descriptors are added to the table as usual
    td.addColumn(cdTime);
    td.addColumn(cdTOffset);
    td.addColumn(cdVarOffset);
    td.addColumn(cdMPos);
    td.addColumn(cdTimeRef);
    td.addColumn(cdTimeRefStr);
    td.addColumn(cdMEVS);
    td.addColumn(cdTimeArr);
    td.addColumn(cdTimeArrRef);
    td.addColumn(cdTime2Arr);
    td.addColumn(cdTime2ArrOffset);
    td.addColumn(cdTime2ArrRef);
    td.addColumn(cdTime3Arr);
    td.addColumn(cdTime3StrRef);
    td.addColumn(cdTime4Arr);
    td.addColumn(cdTime4StrRef);
    td.addColumn(cdTime4ScaOffset);
    
    td.addColumn(cdTestCol);
    td.addColumn(cdTestArrOffset);

    // We have the columns we need but there not yet measure columns.

    {
	// The following creates an (empty) MPosition column.  The column
	// used is "MPosColumn".   This is the simplest useful TableMeasDesc 
	// declaration that can be done. Default MPosition::Ref is used 
	// for the reference.

	// The value desc. specifies the column to use for the measures
	TableMeasValueDesc tmvdMPos(td, "MPosColumn");
	// the TableMeasDesc gives the column a type
	TableMeasDesc<MPosition> tmdMPos(tmvdMPos);
	// writing create the measure column
	tmdMPos.write(td);
    }

    {
	// The following creates an (empty) Measure column.  This
	// particular column is to contain MEpoch with a fixed reference
	// and offset.  The column named "TimeOffset" is used for the
	// MEpochs.  Columns for the refernce and offset are not needed as
	// these are fixed (and stored as keywords in the TimeOffset column

	// The fixed offset which is itself an MEpoch
	MEpoch obsTime(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.)),
		       MEpoch::UTC);
	TableMeasOffsetDesc tmodObsTime(obsTime);

	// The reference descriptor associates the fixed reference and the
	// just declared offset descriptor
	TableMeasRefDesc tmrdObs(MEpoch::LAST, tmodObsTime);

	// The value descriptor specifies the column to use for the Measures
	TableMeasValueDesc tmvdObs(td, "TimeOffset");

	// TableMeasDesc associate the value desc. and the reference desc. and
	// gives the column a type (MEpoch).
	TableMeasDesc<MEpoch> tmdObs(tmvdObs, tmrdObs);
	
	// (test purposes only for purify - test assign and copy contructors
	TableMeasDesc<MEpoch> tmdObs1 = tmdObs;
	TableMeasDesc<MEpoch> tmdObs2(tmdObs1);

	// finally write the descriptor!  The column is now a TableMeasure 
	// column.
	tmdObs2.write(td);
    }
    
    {
    	// Set up a MEpoch column with variable references and offsets
	
        // An offset is a measure. So for variable offsets a Measure
	// column is needed.  The Value descriptor specifies the column to
	// use.
	TableMeasValueDesc tmvdObs(td, "TimeVarOffset");    
	// The descriptor gives the offset column a type.
	TableMeasDesc<MEpoch> tmMOS(tmvdObs);
	TableMeasOffsetDesc tmOsDesc(tmMOS);

	// NB: this descriptor is not written.  This is done via the write()
	// of the "main" column below.
	
	// Reference desc. specifies a column to use for the references and
	// associates the offset measure column.
	TableMeasRefDesc tmrd(td, "TimeRef", tmOsDesc);

	// The "main" measure column
	TableMeasValueDesc tmvd(td, "Time1");
	// The desc. associates the value "main" column with the reference
	// (which includes the offset measure column).
	TableMeasDesc<MEpoch> tmdMEpoch(tmvd, tmrd);
	// write creates the measure column
	tmdMEpoch.write(td);
    }
    
    {
	// A variable offset column is a Measure column, so a TableMeasDesc
	// is needed.
	TableMeasValueDesc tmvdObs(td, "TimeVarOffset");    
	TableMeasDesc<MEpoch> tmMOS(tmvdObs);
	TableMeasOffsetDesc tmODesc(tmMOS);

	// Simplest useful TableMeasDesc declaration that can be done.  
	// Default MPosition::Ref will be used for the reference.
	TableMeasRefDesc tmrd(td, "TimeRefStr",tmODesc);
	TableMeasValueDesc tmvdMEpoch2(td, "MEpochVarStr");    
	TableMeasDesc<MEpoch> tmdMEpoch2(tmvdMEpoch2, tmrd);
	tmdMEpoch2.write(td);
    }
        
    {
	// An array MEpoch column descriptor.  The TableMeasDesc for an Array
	// measure column is identical to the Scalar measure column.
	
	MEpoch mjdToday(MVEpoch(51234));
	TableMeasOffsetDesc tmodToday(mjdToday);
	//	TableMeasRefDesc tmrdLast(MEpoch::LAST, tmodToday);
	TableMeasRefDesc tmrdLast(td, "TimeArrRef", tmodToday);
	TableMeasValueDesc tmvdLast(td, "Time1Arr");
	// create a tmp and test if copy constructor and assignment work
	TableMeasDesc<MEpoch> tmp(tmvdLast, tmrdLast);
	TableMeasDesc<MEpoch> tmp2 = tmp;
	TableMeasDesc<MEpoch> tmdArrLast(tmp2);	
	
	tmdArrLast.write(td);
    }

    {
	// Used to demonstrate an exception, specifically, that a
	// ScalarMeasColumn cannot have an ArrayMeasColumn for its offsets

	// the measure offset column for the array measure column
	TableMeasValueDesc arrOffset(td, "SpareArrOffset");    
	// The descriptor gives the offset column a type.
	TableMeasDesc<MEpoch> tmdOffset(arrOffset);

	// the True says wants to have an ArrayMeasColumn for offset
	TableMeasOffsetDesc tmodOS(tmdOffset, True);

	// TableMeasRefDesc tmrdLast(MEpoch::LAST, tmodToday);
//	TableMeasRefDesc tmrdLast(td, "TimeArrRef", tmodToday);
	TableMeasRefDesc tmrdLast(MEpoch::LAST, tmodOS);
	TableMeasValueDesc tmvdLast(td, "SpareCol1");
	TableMeasDesc<MEpoch> tmdMDesc(tmvdLast, tmrdLast);	
	tmdMDesc.write(td);
    }

    {
	// An array MEpoch column desc. with variable offset and reference.


	// the measure offset column for the array measure column
	TableMeasValueDesc arrOffset(td, "Time2ArrOffset");    
	// The descriptor gives the offset column a type.
	TableMeasDesc<MEpoch> tmdOffset(arrOffset);
	TableMeasOffsetDesc tmOSDesc(tmdOffset, True);
	
	// measure reference column and associated offset
	TableMeasRefDesc tmARef(td, "Time2ArrRef", tmOSDesc);
	// the "main" value descriptor
	TableMeasValueDesc tmAVal(td, "Time2Arr");

	// create a tmp and test if copy constructor and assignment work
	TableMeasDesc<MEpoch> tmp(tmAVal, tmARef);
	TableMeasDesc<MEpoch> tmp2 = tmp;
	TableMeasDesc<MEpoch> tmdArray(tmp2);	
	
	tmdArray.write(td);
    }

    {
	// An array MEpoch column desc. with variable string references.


	// measure reference column and associated offset
	TableMeasRefDesc tmARef(td, "Time3ArrStrRef");
	// the "main" value descriptor
	TableMeasValueDesc tmAVal(td, "Time3Arr");

	// create a tmp and test if copy constructor and assignment work
	TableMeasDesc<MEpoch> tmdArray(tmAVal, tmARef);
	tmdArray.write(td);
    }

    {
	// Check various exception
	if (doExcep) {
	    try {
		// test TMRefDesc no such column
		TableMeasRefDesc tCol(td, "SillyColumnName");
	    } catch (AipsError x) {
		cout << "The following should report no such column ";
		cout << " for TableMeasRefDesc.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	    try {
		// test TMRefDesc - column exist but is of the wrong type
		TableMeasRefDesc tCol(td, "Time4ScaOffset");
	    } catch (AipsError x) {
		cout << "The following should report that the column's ";
		cout << "type is no good.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	    try {
		// test TableMeasValueDesc - column doesn't exist exception
		TableMeasValueDesc tCol(td, "SillyColumnName");
	    } catch (AipsError x) {
		cout << "The following should report no such column ";
		cout << "for TableMeasValueDesc.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	    try {
		// test TableMeasValueDesc - column exists but not array
		TableMeasValueDesc tCol(td, "Time4StrRef");
	    } catch (AipsError x) {
		cout << "The following should report that the column ";
		cout << "is not array for TabelMeasValueDesc.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	    try {
		// test TableMeasValueDesc - column exists but is double
	        TableMeasValueDesc tCol(td, "Time2ArrRef");
	    } catch (AipsError x) {
		cout << "The following should report that the column's ";
		cout << "type should be double for the TableMeasValueDesc.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	}


	// An array MEpoch column desc. with (string) references and offsets
	// per row.

	// the measure offset column for the array measure column
	TableMeasValueDesc scaOffset(td, "Time4ScaOffset");    
	// The descriptor gives the offset column a type.
	TableMeasDesc<MEpoch> tmdOffset(scaOffset);
	TableMeasOffsetDesc tmOsDesc(tmdOffset);

	// measure reference column and associated offset
	TableMeasRefDesc tmARef_tmp(td, "Time4StrRef", tmOsDesc);
	TableMeasRefDesc tmARef(td, "Time4StrRef");
	if (!tmARef.isOffsetVariable() && !tmARef.isOffsetArray()) {
	    cout << "PASS - TMRefDesc doesn't have an offset yet\n";
	} else {
	    cout << "FAIL - Reference should not yet have an offset\n";
	}
	tmARef = tmARef_tmp;
	// the "main" value descriptor..testing assignment too
	TableMeasValueDesc tmAVal_tmp(td, "Time4Arr");
	TableMeasValueDesc tmAVal(td, "Time3Arr");
	tmAVal = tmAVal_tmp;

	// create a tmp and test if copy constructor and assignment work
	// test assignment too
	TableMeasDesc<MEpoch> tmdArray_tmp(tmAVal, tmARef);
	TableMeasDesc<MEpoch> tmdArray(tmAVal);
	tmdArray = tmdArray_tmp;
	tmdArray.write(td);

	// test getting on the new descriptor reference
	TableMeasRefDesc testRef = tmdArray.getRefDesc();
	if (testRef.hasOffset()) {
	  cout << "PASS - Reference has column offset.\n";
	} else {
	  cout << "FAIL - Reference apparantly doesn't have an offset!\n";
	}
    }

    // Finally create the table

    SetupNewTable newtab("TestTableMeasures", td, Table::New);

    // At this point a table called "TestTableMeasures" has been created.
    // It contains a number of empty Measure columns.  The remainder of this
    // program tests the usage of Scalar(Array)MeasColumn objects for
    // putting and getting Measures into and out of the table.


    const uInt tabRows = 5;
    
    {
        Table tab(newtab, tabRows);
	cout << "Create MEpochScaCol from column TimeOffset...\n";
	cout << "A column of MEpochs where the reference and offset are ";
	cout << "non-variable.\n";
	// create first a null object and attach it and copy it etc.
	// show that these operation work.
	MEpochScaCol tmpCol;
	if (tmpCol.isNull()) {
	    tmpCol.attach(tab, "TimeOffset");
	}
	tmpCol.throwIfNull();
	cout << "Null MEpochScaCol successfully attached\n";
	// no assignment operator but there is a copy constructor
	MEpochScaCol timeCol(tmpCol);
	
	// print some details things about the column
	if (timeCol.isRefVariable()) {
	    cout << "The column has variable references.\n";
	} else {
	    cout << "The MeasRef for the column is: " << timeCol.getMeasRef() 
		<< endl;
	}
	
	cout << "Adding a few MEpochs to column TimeOffset...\n";
	MEpoch tm(MVEpoch(1234.));
	uInt i;
	for (i=0; i<tabRows; i++) {
	    tm.set(MVEpoch(1234 + (i/10.0)));
	    cout << "adding: " << tm << endl;
	    timeCol.put(i, tm);
	}

	cout << "Reading the MEpochs back from TimeOffset...\n";
	// Create read only measure column.  Create it null and then attach
	// a column (for coverage)
	ROMEpochScaCol timeColRead;
	timeColRead.attach(tab, "TimeOffset");
	for (i=0; i<tabRows; i++) {
	    if (timeColRead.isDefined(i)) {
		timeColRead.get(i, tm);
	    	cout << "retrieve: " << tm << endl;
		cout << "  " << tm.getRef() << endl;
    	    } else {
		cout << "Error: row " << i << " doesn't contain any data!\n";
	    }
	}
	
	// for coverage of reference member (via attach)
	cout << "TEST of attach/reference...\n";
	MEpochScaCol testVarStrCol;
	testVarStrCol.attach(tab, "MEpochVarStr");
	cout << "Column attached...\n";

	// copy constructor
	MEpochScaCol testCopy = testVarStrCol;
	// and get the values
	for (i=0; i<tabRows; i++) {
	    testCopy.put(i, MVEpoch(1.));
	}
	for (i=0; i<tabRows; i++) {
	    cout << testCopy(i) << endl;
	}

	// stuff to increase line coverage of classes
	if (doExcep) {
	    try {
		// try consrtucting with a non MeasureColumn
		ROMEpochScaCol tScaCol(tab, "TimeRef");
	    } catch (AipsError x) {
		cout << "The following line should report an error ";
		cout << "in reconstruct - invalid column exception.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	    try { 
		// test throw if null exception
		MEpochScaCol nullCol;
		nullCol.throwIfNull();
	    } catch (AipsError x) {
		cout << "The following line should be a ";
		cout << "null column exception.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	    try {
		// try constructing a ScalarMeasColumn with an Array Offset
		// column
		MEpochScaCol(tab, "SpareCol1");
	    } catch (AipsError x) {
		cout << "The following line should be an illegal ";
		cout << "offset column type exception.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	}
    }

    {
        // reopen the table RO and read the measures
	cout << "Reopening the table read-only and reading contents...\n";
        Table tab("TestTableMeasures", Table::Old);
	ROMEpochScaCol timeColRead(tab, "TimeOffset");
	MEpoch tm;
	for (uInt i=0; i<tabRows; i++) {
	    if (timeColRead.isDefined(i)) {
		timeColRead.get(i, tm);
	    	cout << "retrieve: " << tm << endl;
		cout << "  " << tm.getRef() << endl;
    	    } else {
		cout << "Error: row " << i << " doesn't contain any data!\n";
	    }
	}
    }
    
    {
        Table tab("TestTableMeasures", Table::Update);
	// A column of MPositions
	MPositionScaCol mposCol(tab, "MPosColumn");
	if (mposCol.isRefVariable()) {
	    cout << "Error: reference is variable!\n";
	}
	
	cout << "Filling the MPosition column MPosColumn\n";
	MPosition mpos;
	for (uInt i=0; i<tabRows; i++) {
	    mpos.set(MVPosition(Quantity(25 + i, "m"),
		    	    	Quantity(20, "deg"),
		    	    	Quantity(53, "deg")));
	    cout << "put: " << mpos << endl;	    
	    mposCol.put(i, mpos);
	}
    }
    
    {
        Table tab("TestTableMeasures", Table::Update);
	// A column of MPositions
	ROMPositionScaCol mposCol(tab, "MPosColumn");
	if (mposCol.isRefVariable()) {
	    cout << "Error: reference is variable!\n";
	}
	MPosition mpos;
	cout << "Reading from MPosition column MPosColumn\n";
	for (uInt i=0; i<tabRows; i++) {
	    cout << "retrieve: " << mposCol(i) << endl;
	}
    }
    
    {
        Table tab("TestTableMeasures", Table::Update);
	cout << "Test of column TimeVarOffset...\n";
	cout << "A column of MEpoch where the reference and offset components";
	cout << " are variable.\n";
	// create meCol to use but test copy contructor and attach() at the
	// same time.
	MEpochScaCol tmpMeCol;
	tmpMeCol.attach(tab, "Time1");
	MEpochScaCol meCol = tmpMeCol;
	
	MEpoch offset;
	MEpoch me;
	offset.set(MVEpoch(1234));
	me.set(MVEpoch(51234.1), MEpoch::Ref(MEpoch::LAST, offset));
	meCol.put(0, me);
	offset.set(MVEpoch(1234.1));
	me.set(MVEpoch(51234.2), MEpoch::Ref(MEpoch::UTC, offset));
	meCol.put(1, me);
	offset.set(MVEpoch(1234.2));
	me.set(MVEpoch(51234.3), MEpoch::Ref(MEpoch::TAI, offset));
	meCol.put(2, me);
	offset.set(MVEpoch(1234.3));
	me.set(MVEpoch(51234.4), MEpoch::Ref(MEpoch::UTC, offset));
	meCol.put(3, me);	
	// put one in with no offset
//	offset.set(MVEpoch(1234.4));
//	me.set(MVEpoch(51234.5), MEpoch::Ref(MEpoch::LAST, offset));
	me.set(MVEpoch(51234.5), MEpoch::Ref(MEpoch::LAST));
	meCol.put(4, me);
	
	for (uInt i=0; i<tabRows; i++) {
	    meCol.get(i, me);
	    cout << "retrieve: " << me << endl;
	    cout << "  " << me.getRef() << endl;
	}
	
    }
	

    {
        Table tab("TestTableMeasures", Table::Update);
	cout << "Creating an MEpoch Array Column\n";
	MEpochArrCol tmpArrCol;

	if (tmpArrCol.isNull()) {
	    tmpArrCol.attach(tab, "Time1Arr");
	}
	tmpArrCol.throwIfNull();
	cout << "Null MEpochArrCol successfully attached\n";
	// no assignment operator but there is a copy constructor
	MEpochArrCol arrayCol(tmpArrCol);

	MEpoch last(Quantity(13.45, "h"), MEpoch::Ref(MEpoch::TAI));
	Vector<MEpoch> ev(10);
	for (uInt i=0; i<10; i++) {
	    last.set(Quantity(13.45 + i, "h"));
	    ev(i) = last;
	}
	
	cout << "Adding a vector to the column at row 0.\n";

	// before adding something check the isDefined() member
	if (!arrayCol.isDefined(0)) {
	    cout << "PASS - nothing in the measure array column row yet\n";
	} else {
	    cout << "FAIL - there shouldn't be a valid value in the row!\n";
	}
	
	arrayCol.put(0, ev);
	Vector<MEpoch> ew;
	arrayCol.get(0,ew, True);

	// now row 0 should contain a valid entry
	if (arrayCol.isDefined(0)) {
	    cout << "PASS - valid entry in array column row 0\n";
	} else {
	    cout << "FAIL - there should be something in row 0!\n";
	}
		
	for (uInt i=0; i<10; i++) {
	    cout << "ev: " << ev(i) << " " << ev(i).getRef() << endl;
	    cout << "ew: " << ew(i) << " " << ew(i).getRef() << endl;
	}
    }

    {
        cout << "Open table again in RO mode to test ROArrayMeasColumn...\n";
        Table tab("TestTableMeasures", Table::Old);
	cout << "Creating an MEpoch Array Column\n";
	ROMEpochArrCol arrayCol(tab, "Time1Arr");	
	cout << arrayCol(0) << endl;
    }

    {
	// a bunch of extra tests

        Table tab("TestTableMeasures", Table::Update);
	MEpochArrCol arrMeasCol;
	arrMeasCol.attach(tab, "Time2Arr");

	// copy constructor
	MEpochArrCol testCopy = arrMeasCol;

	MEpoch utcE(Quantity(1.45, "h"), MEpoch::Ref(MEpoch::UTC));
	MEpoch taiE(Quantity(1.45, "h"), MEpoch::Ref(MEpoch::TAI));
	Vector<MEpoch> inArr(10);
	for (uInt i=0; i<10; i++) {
	    if (i%2 == 0) {
		utcE.set(Quantity(11.45 + i, "h"));
		utcE.setOffset(MEpoch(Quantity(12. - i, "h")));
		inArr(i) = utcE;
	    } else {
		taiE.set(Quantity(13.45 + i, "h"));
		taiE.setOffset(MEpoch(Quantity(15.6 - i, "h")));
		inArr(i) = taiE;
	    }
	}
	cout << "Adding vectors to the test measure column\n";
	for (uInt i=0; i<tabRows; i++) {
	    testCopy.put(i, inArr);
	}

	// attach
	ROMEpochArrCol testAttach;
	testAttach.attach(tab, "Time2Arr");
	Vector<MEpoch> outArr;
	testAttach.get(0, outArr, True);
	for (uInt i=0; i<10; i++) {
	    cout << "in:  " << inArr(i) <<  " " << inArr(i).getRef() 
		 << endl;
	    cout << "out: " << outArr(i) <<  " " << outArr(i).getRef() 
		 << endl;
	}

	// getRef for the column
	cout << "Reference for the ArrayMeasCol is: " << testCopy.getMeasRef();
	cout << endl;

	// stuff to increase line coverage of classes
	if (doExcep) {
	    try { 
		// test throw if null exception
		MEpochArrCol nullCol;
		nullCol.throwIfNull();
	    } catch (AipsError x) {
		cout << "The following line should be a ";
		cout << "null column exception.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	}

	// array column with variable string references
	MEpochArrCol varStrRefColtmp;
	varStrRefColtmp.attach(tab, "Time3Arr");
	MEpochArrCol varStrRefCol = varStrRefColtmp;
	for (uInt i=0; i<tabRows; i++) {
	    varStrRefCol.put(i, inArr);
	}
	varStrRefCol.get(0, outArr, True);
	for (uInt i=0; i<10; i++) {
	    cout << "in:  " << inArr(i) <<  " " << inArr(i).getRef() 
		 << endl;
	    cout << "out: " << outArr(i) <<  " " << outArr(i).getRef() 
		 << endl;
	}

	// test putting of an empty array
	Vector<MEpoch> dummy;
	varStrRefCol.put(0, dummy);

	// last thing to test.  Array columns with scalar column offsets
	// and reference.  First test attach and copy constructor.
	MEpochArrCol scaStrRefColtmp;
	scaStrRefColtmp.attach(tab, "Time4Arr");
	MEpochArrCol scaStrRefCol = scaStrRefColtmp;

	// Only one reference and offset are stored per row.  The reference
	// and offset storedis taken from the first element of each
	// Measure array stored.
	for (uInt i=0; i<tabRows; i++) {
	    scaStrRefCol.put(i, inArr);
	}
	scaStrRefCol.get(0, outArr, True);
	for (uInt i=0; i<10; i++) {
	    cout << "in:  " << inArr(i) <<  " " << inArr(i).getRef() 
		 << endl;
	    cout << "out: " << outArr(i) <<  " " << outArr(i).getRef() 
		 << endl;
	}
	// One last thing to test...test array conformance exception
	if (doExcep) {
	    try { 
		Array<MEpoch> badShapeArr(IPosition(2,2));
		scaStrRefCol.get(0, badShapeArr, False);
	    } catch (AipsError x) {
		cout << "The following line should be a ";
		cout << "Table array conformance error exception.\n";
		cout << x.getMesg() << endl;
	    } end_try;
	}
    }
	
    cout << "Test completed normally...bye.\n";  
  } catch (AipsError x) {
      cout << "An error occurred.  The test ended early with the following";
      cout << " message:\n";
      cout << x.getMesg() << endl;
  } end_try;
    exit(0);
}


