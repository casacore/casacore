//# tTableMeasures.cc: test program for the TableMeasures class.
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

#include <iostream.h>
#include <aips/aips.h>
#include <aips/Exceptions.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MVEpoch.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
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
//#ifdef COMMENT
#include <trial/TableMeasures/ArrayMeasColumn.h>
//#endif
#include <trial/TableMeasures/ScalarMeasColumn.h>
#include <trial/TableMeasures/TableMeasValueDesc.h>
#include <trial/TableMeasures/TableMeasOffsetDesc.h>
#include <trial/TableMeasures/TableMeasRefDesc.h>
#include <trial/TableMeasures/TableMeasDesc.h>

int main(void)
{
  try {
    cout << "Test of TableMeasures classes.\n";

    // Need a table to work with.
    TableDesc td("tTableMeasure_desc", "1", TableDesc::New);
    td.comment() = "A test of TableMeasures class.";
    
    // The underlying column for storing the Measure values is an Array<Double>
    // Create an underlying column for each Measure column.
    ArrayColumnDesc<Double> cdTime("Time1", "An MEpoch column");
    ArrayColumnDesc<Double> cdTOffset("TimeOffset", "Offsetcolumn for Time1");
    ArrayColumnDesc<Double> cdVarOffset("TimeVarOffset", "Variable Offset col");
    ArrayColumnDesc<Double> cdMPos("MPosColumn", "Simple mposition column");
    ScalarColumnDesc<Int> cdTimeRef("TimeRef", "Reference column for Time1");
    
    //#ifdef ARRAYMEAS
    ArrayColumnDesc<Double> cdTimeArr("Time1Arr", "An MEpoch array column");
    ScalarColumnDesc<Int> cdTimeArrRef("TimeArrRef", "VarRef co for TimeArr");
    //#endif

    td.addColumn(cdTime);
    td.addColumn(cdTOffset);
    td.addColumn(cdVarOffset);
    td.addColumn(cdMPos);
    td.addColumn(cdTimeRef);
    
    //#ifdef ARRAYMEAS
    td.addColumn(cdTimeArr);
    td.addColumn(cdTimeArrRef);
    //#endif
    
    {
    	// Another MEpoch column descriptor this one specifies a fixed offset 
	// with reference MEpoch::LAST.
	MEpoch obsTime(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.)),
		MEpoch::UTC);
	TableMeasOffsetDesc tmodObsTime(obsTime);
	TableMeasRefDesc tmrdObs(MEpoch::LAST, tmodObsTime);
	TableMeasValueDesc tmvdObs(td, "TimeOffset");    
	TableMeasDesc<MEpoch> tmdObs(tmvdObs, tmrdObs);
	
	// test assignment and copy constructor
	TableMeasDesc<MEpoch> tmdObs1 = tmdObs;
	TableMeasDesc<MEpoch> tmdObs2(tmdObs1);
	tmdObs2.write(td);
    }
    
    {
    	// Set up a MEpoch column with support for variable references and
	// offset
	
	// A variable offset column is a Measure column, so a TableMeasDesc
	// is needed.
	TableMeasValueDesc tmvdObs(td, "TimeVarOffset");    
	TableMeasDesc<MEpoch> tmMOS(tmvdObs);
	
        // This sets up a second MEpoch column.  Variable references are 
	// specified by supplying the name of a reference column "TimeRef" to
	// use.  tmMOS is used as a variable measure offset column.
	TableMeasRefDesc tmrd(td, "TimeRef", tmMOS);
	TableMeasValueDesc tmvd(td, "Time1");    
	TableMeasDesc<MEpoch> tmdMEpoch(tmvd, tmrd);
	tmdMEpoch.write(td);
    }
    
    {
	// Simplest useful TableMeasDesc declaration that can be done.  
	// Default MPosition::Ref will be used for the reference.
	TableMeasValueDesc tmvdMPos(td, "MPosColumn");    
	TableMeasDesc<MPosition> tmdMPos(tmvdMPos);
	tmdMPos.write(td);
    }
    
    
    //#ifdef ARRAYMEAS
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
    //#endif    
    // create the table
    SetupNewTable newtab("TestTableMeasures", td, Table::New);
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
	// Create read only measure column
	ROMEpochScaCol timeColRead(tab, "TimeOffset");
	for (i=0; i<tabRows; i++) {
	    if (timeColRead.isDefined(i)) {
		timeColRead.get(i, tm);
	    	cout << "retrieve: " << tm << endl;
		cout << "  " << tm.getRef() << endl;
    	    } else {
		cout << "Error: row " << i << " doesn't contain nay data!\n";
	    }
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
		cout << "Error: row " << i << " doesn't contain nay data!\n";
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
	MEpochScaCol meCol(tab, "Time1");
	
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
	offset.set(MVEpoch(1234.4));
	me.set(MVEpoch(51234.5), MEpoch::Ref(MEpoch::LAST, offset));
	meCol.put(4, me);
	
	for (uInt i=0; i<tabRows; i++) {
	    meCol.get(i, me);
	    cout << "retrieve: " << me << endl;
	    cout << "  " << me.getRef() << endl;
	}
	
    }
	

    //#ifdef ARRAYMEAS
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
	arrayCol.put(0, ev);
	Vector<MEpoch> ew;
	arrayCol.get(0,ew, True);
	
	for (i=0; i<10; i++) {
	    cout << "ev: " << ev(i) << " " << ev(i).getRef() << endl;
	    cout << "ew: " << ew(i) << " " << ew(i).getRef() << endl;
	}
    }
    //#ifdef ARRAYS
    {
        cout << "Open table again in RO mode to test ROArrayMeasColumn...\n";
        Table tab("TestTableMeasures", Table::Old);
	cout << "Creating an MEpoch Array Column\n";
	ROMEpochArrCol arrayCol(tab, "Time1Arr");	
	cout << arrayCol(0) << endl;
    }
    //#endif
    
    cout << "Bye.\n";  
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;
    exit(0);
}


