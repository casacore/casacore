// File:    	tTableMeasures.cc
// Date:    	97/08/22
// Author:  	Michael Haller
// Description: test or TableMeasures class

#include <iostream.h>
#include <aips/aips.h>
#include <aips/Exceptions.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MVEpoch.h>
#include <aips/Measures/MVTime.h>
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
#include <trial/TableMeasures/ArrayMeasColumn.h>
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
    ArrayColumnDesc<Double> cdB1950("SiderealColumn", "An MDirection column");
    ArrayColumnDesc<Double> cdTimeArr("Time1Arr", "An MEpoch array column");

    td.addColumn(cdTime);
    td.addColumn(cdB1950);
    td.addColumn(cdTimeArr);
    
    {
	// A TableMeasDesc for a simple MEpoch column "Time1" with reference
	// MEpoch::TAI
	TableMeasRefDesc tmrd(MEpoch::TAI);
	TableMeasValueDesc tmvd(td, "Time1");    
	TableMeasDesc<MEpoch> tmdMEpoch(tmvd, tmrd);
	tmdMEpoch.write(td);
    }    
    {
    	// Another MEpoch column descriptor this one specifies a fixed offset 
	// with reference MEpoch::LAST.
	MEpoch obsTime(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.)),
		MEpoch::UTC);
	TableMeasOffsetDesc tmodObsTime(obsTime);
	TableMeasRefDesc tmrdObs(MEpoch::LAST, tmodObsTime);
	TableMeasValueDesc tmvdObs(td, "SiderealColumn");    
	TableMeasDesc<MEpoch> tmdObs(tmvdObs, tmrdObs);
	tmdObs.write(td);
    }
    {
	// An array MEpoch column descriptor.  The TableMeasDesc for an Array
	// measure column is identical to the Scalar measure column.
	
	MEpoch mjdToday(MVEpoch(51234));
	TableMeasOffsetDesc tmodToday(mjdToday);
	TableMeasRefDesc tmrdLast(MEpoch::LAST, tmodToday);
	TableMeasValueDesc tmvdLast(td, "Time1Arr");
	// create a tmp and test if copy constructor and assignment work
	TableMeasDesc<MEpoch> tmp(tmvdLast, tmrdLast);
	TableMeasDesc<MEpoch> tmp2 = tmp;
	TableMeasDesc<MEpoch> tmdArrLast(tmp2);	
	
	tmdArrLast.write(td);
    }    
    // create the table
    SetupNewTable newtab("TestTableMeasures", td, Table::New);
    const uInt tabRows = 5;
    Table tab(newtab, tabRows);
    
    {
	cout << "Create MEpochScaCol from column Time1...\n";
	// create first a null object and attach it and copy it etc.
	// show that these operation work.
	MEpochScaCol tmpCol;
	if (tmpCol.isNull()) {
	    tmpCol.attach(tab, "SiderealColumn");
	}
	tmpCol.throwIfNull();
	cout << "Null MEpochScaCol successfully attached\n";
	MEpochScaCol timeCol(tmpCol);
	
	// print some details things about the column
	if (timeCol.isRefVariable()) {
	    cout << "The column has variable references.\n";
	} else {
	    cout << "The MeasRef for the column is: " << timeCol.getMeasRef() 
		<< endl;
	}
	
	MEpoch tm(MVEpoch(1234.));
	uInt i;
	for (i=0; i<tabRows; i++) {
	    tm.set(MVEpoch(MVTime(1996, 5, 17, 10.8/24.)));
	    timeCol.put(i, tm);
	}

	// Create read only measure column
	ROMEpochScaCol timeColRead(tab, "SiderealColumn");
	for (i=0; i<tabRows; i++) {
	    cout << timeColRead(i) << endl;
	}	
    }    
    
    {
	cout << "Creating an MEpoch Array Column\n";
	MEpochArrCol arrayCol(tab, "Time1Arr");

	MEpoch last(Quantity(13.45, "h"), MEpoch::Ref(MEpoch::TAI));
	Vector<MEpoch> ev(10);
	for (uInt i=0; i<10; i++) {
	    last.set(Quantity(13.45 + i, "h"));
	    ev(i) = last;
	}
	
	cout << "Adding a vector to the column at row 0.\n";
	arrayCol.put(0, ev);
	Vector<MEpoch> ew;
	arrayCol.get(ew, 0, True);
	
	for (i=0; i<10; i++) {
	    cout << "ev: " << ev(i) << " " << ev(i).getRef() << endl;
	    cout << "ew: " << ew(i) << " " << ew(i).getRef() << endl;
	}
//	cout << arrayCol(0) << endl;
    }
    
    cout << "Bye.\n";  
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;
    exit(0);
}


