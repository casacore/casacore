//# tRowCopier.cc : tests the RowCopier class
//# Copyright (C) 1995,1996,1999,2000,2001,2002
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

#include <casacore/tables/Tables/RowCopier.h>
#include <casacore/tables/Tables.h>
#include <casacore/tables/Tables/TabVecLogic.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>

#include <casacore/casa/namespace.h>
int main()
{
// first, create a short table with a few columns to use during testing
// Do the descriptor first

    cout << "Building some test tables." << endl;

    TableDesc td;
    td.addColumn(ScalarColumnDesc<Int>("ICol1"));
    td.addColumn(ScalarColumnDesc<Int>("ICol2"));
    td.addColumn(ScalarColumnDesc<Float>("FCol"));
    td.addColumn(ScalarColumnDesc<Double>("DCol"));
    td.addColumn(ArrayColumnDesc<Int>("IACol", "",
					   IPosition(1,5), ColumnDesc::Direct));

    //  Ok, now a table with 5 rows

    SetupNewTable newtab("tRowCopier_tmp_0",td, Table::Scratch);
    Table maintab(newtab, 5);

    // fill the above
    ScalarColumn<Int> ic1(maintab,"ICol1"), ic2(maintab,"ICol2");
    ScalarColumn<Float> fc(maintab,"FCol");
    ScalarColumn<Double> dc(maintab,"DCol");
    ArrayColumn<Int> ac(maintab,"IACol");
    Vector<Int> vtmp(5);

    for (Int i=0; i<5; i++) {
        ic1.put(i,i);
	ic2.put(i, i*10);
        fc.put(i,  Float(i)/10.);
	dc.put(i,  Double(i*i));
	indgen(vtmp, i*10, 10);
	ac.put(i, vtmp);
    }

    //  Create 2 tables to be used by RowCopier testing

    //  first, one exactly the same size and columns as maintab
    //  we can't simply reuse newtab because each table needs a unique name

    SetupNewTable newtab1("tRowCopier_tmp_1",td,Table::Scratch);
    Table exacttab(newtab1, 5);

    //  second, one with some of the same columns as maintab, plus others

    TableDesc td2;
    td2.addColumn(ScalarColumnDesc<Int>("ICol1"));
    td2.addColumn(ScalarColumnDesc<Int>("ICol3"));
    td2.addColumn(ScalarColumnDesc<uInt>("uICol"));
    td2.addColumn(ArrayColumnDesc<Int>("IACol","",
					    IPosition(1,5),ColumnDesc::Direct));
    td2.addColumn(ArrayColumnDesc<Int>("IACol2","",
					    IPosition(1,3),ColumnDesc::Direct));
    SetupNewTable newtab2("tRowCopier_tmp_2",td2, Table::Scratch);
    Table partialtab(newtab2, 5);
    ScalarColumn<Int> ic3col(partialtab,"ICol3");
    for (uInt j=0; j<partialtab.nrow(); j++) ic3col.put(j,-1);


    //  Ok, now some actual RowCopier testing

    {
	cout << "\nMake an exact copy using rowcopier" << endl;

	RowCopier exact(exacttab, maintab);
	uInt rownr;
	for (rownr=0; rownr<maintab.nrow(); rownr++) {
	    if ( !exact.copy(rownr)) {
		cout << "Ooops, exact.copy(" << rownr << ") returned False!" 
		    << endl;
		cout << "tRowCopier fails!" << endl;
		return 1;
	    }
	}
	// and compare each Scalar column as a TableVector
	TableVector<Int> ic1main(maintab, "ICol1"), ic1copy(exacttab, "ICol1");
        if (anyNE(ic1main, ic1copy)) {
	    cout << "An exact copy was not made of ICol1"
		<< endl;
	    cout << "tRowCopier fails!" << endl;
	    return 1;
	}
	TableVector<Int> ic2main(maintab, "ICol2"), ic2copy(exacttab, "ICol2");
        if (anyNE(ic2main, ic2copy)) {
	    cout << "An exact copy was not made of ICol2"
		<< endl;
	    cout << "tRowCopier fails!" << endl;
	    return 1;
	}
	TableVector<Float> fcmain(maintab, "FCol"), fccopy(exacttab, "FCol");
        if (anyNE(fcmain, fccopy)) {
	    cout << "An exact copy was not made of FCol"
		<< endl;
	    cout << "tRowCopier fails!" << endl;
	    return 1;
	}
	TableVector<Double> dcmain(maintab, "DCol"), dccopy(exacttab, "DCol");
        if (anyNE(dcmain, dccopy)) {
	    cout << "An exact copy was not made of DCol"
		<< endl;
	    cout << "tRowCopier fails!" << endl;
	    return 1;
	}
	// and check each Vector in IACol
	ArrayColumn<Int> iamain(maintab, "IACol"), iacopy(exacttab, "IACol");
	for (rownr = 0; rownr < maintab.nrow(); rownr++) {
	    if (anyNE(iamain(rownr), iacopy(rownr))) {
		cout << "An exact copy was not made of the array column "
		    << "at row number " << rownr << endl;
		cout << "tRowCopier fails!" << endl;
		return 1;
	    }
	}
	cout << "Exact copy passes." << endl;
    }

    // copy as much of maintab to partialtab as allowed
    // That pretty much amounts to ICol1 and IACol
    
    {
	cout << "\nCopy as much as allowed between two tables" << endl;
	RowCopier limited(partialtab, maintab);
	uInt rownr;
	for (rownr=0; rownr<maintab.nrow(); rownr++) {
	    if ( !limited.copy(rownr)) {
		cout << "Ooops, limited.copy(" << rownr << ") returned False!"
		    << endl;
		cout << "tRowCopier Fails!" << endl;
		return 1;
	    }
	}
	// If it gets here, it really must have passed, but just check that
	// ICol1 and IACol are exact copies
	TableVector<Int> ic1main(maintab, "ICol1");
	TableVector<Int> ic1part(partialtab, "ICol1");
	if (anyNE(ic1main, ic1part)) {
	    cout << "ICol1 copy differs!" << endl;
	    cout << "tRowCopier fails!" << endl;
	    return 1;
	}
	// and check each Vector in IACol
	ArrayColumn<Int> mia(maintab, "IACol"), pia(partialtab, "IACol");
	for (rownr = 0; rownr < maintab.nrow(); rownr++) {
	    if (anyNE(mia(rownr), pia(rownr))) {
		cout << "The array columns do not match "
		    << "at row number " << rownr << endl;
		cout << "tRowCopier fails!" << endl;
		return 1;
	    }
	}
	cout << "limited copy passes." << endl;
    }

    // copy using named columns from ICol1 to ICol3
    
    {
	cout << "\nNamed copy of ICol1 to ICol3" << endl;
	// first, verify that any of ICol1 and ICol3 are not already equal
	TableVector<Int> ic1main(maintab, "ICol1");
	TableVector<Int> ic3part(partialtab, "ICol3");
	if (anyEQ(ic1main, ic3part)) {
	    cout << "Hmm, ICol1 and ICol3 are already equal in some values!" 
		<< endl;
	    cout << "That should not happen yet" << endl;
	    for (uInt rownr=0; rownr < maintab.nrow(); rownr++) {
		cout << rownr << " "
		     << ic1main(rownr) << " "
		     << ic3part(rownr) << endl;
	    }
	    cout << "tRowCopier fails!" << endl;
	    return 1;
	}
	Vector<String> inname(1), outname(1);
	inname(0) = "ICol1";
	outname(0) = "ICol3";
	RowCopier named(partialtab, maintab, outname, inname);
    	for (uInt rownr=0; rownr<maintab.nrow(); rownr++) {
	    if ( !named.copy(rownr)) {
		cout << "Ooops, named.copy(" << rownr << ") returned False!"
		    << endl;
		cout << "tRowCopier fails!" << endl;
		return 1;
	    }
	}
	// now they should be equal
	
	if (anyNE(ic1main, ic3part)) {
	    cout << "ICol1 and ICol3 are not exact copies!" << endl;
	    cout << "tRowCopier fails!" << endl;
	    return 1;
	}
	cout << "Named copy, same row number, passes." << endl;
    }

    // copy ICol1 to ICol3 in reverse order
    {
	cout << "Named copy of ICol1 to ICol3, in reverse order" << endl;
	Vector<String> inname(1), outname(1);
	uInt inrownr, outrownr;
	inname(0) = "ICol1";
	outname(0) = "ICol3";
	RowCopier named(partialtab, maintab, outname, inname);
	TableVector<Int> ic1main(maintab, "ICol1");
	TableVector<Int> ic3part(partialtab, "ICol3");
    	for (inrownr=0, outrownr=maintab.nrow()-1; 
	     inrownr<maintab.nrow(); inrownr++, outrownr--) {
	    if ( !named.copy(outrownr, inrownr)) {
		cout << "Ooops, named.copy(" << inrownr << ") returned False!"
		    << endl;
		cout << "tRowCopier fails!" << endl;
		return 1;
	    }
	}
	for (inrownr = 0; inrownr < maintab.nrow(); inrownr++) {
	    outrownr = partialtab.nrow() - inrownr - 1;
	    if (ic1main(inrownr) != ic3part(outrownr)) {
		cout << "ICol1(" << inrownr << ") is not equal to ICol3("
		    << outrownr << ")" << endl;
		cout << "tRowCopier fails!" << endl;
		return 1;
	    }
	}
	cout << "named copy, reverse order, passes." << endl;
    }
    // OK, now lets try some things that should generate exceptions.
    cout << "\nChecking that exceptions are thrown properly." << endl;
    cout << "Note: since RowCopier is NOT derived from Cleanup, this" << endl;
    cout << "      section causes memory leaks." << endl;

    Bool caught = False;

    // construct a copier using a non-existant column
    try {
	cout << "Invalid column name : ";
	Vector<String> colname(1);
	colname(0) = "Garbage";
	RowCopier rc(partialtab, maintab, colname, colname);
    } catch (TableError x) {
	caught = True;
    } 
    if (caught) {
	cout << "OK" << endl;
	caught = False;
    } else {
	cout << "FAILS!" << endl;
	return 1;
    }

    // non-conformant columns
    // different types
    try {
	cout << "Different column types : ";
	Vector<String> inname(1), outname(1);
	inname(0) = "FCol"; outname(0) = "DCol";
	RowCopier rc(maintab, maintab, inname, outname);
    } catch (TableError x) {
	caught = True;
    } 
    if (caught) {
	cout << "OK" << endl;
	caught = False;
    } else {
	cout << "FAILS!" << endl;
	return 1;
    }

    // different dimensionality, Scalar versus Array
    try {
	cout << "Different column dimensionality, scalar vs array : ";
	Vector<String> inname(1), outname(1);
	inname(0) = "ICol1"; outname(0) = "IACol";
	RowCopier rc(maintab, maintab, inname, outname);
    } catch (TableError x) {
	caught = True;
    } 
    if (caught) {
	cout << "OK" << endl;
	caught = False;
    } else {
	cout << "FAILS!" << endl;
	return 1;
    }

    // different dimensionality of input strings
    try {
	cout << "Different dimensionality of the two Vector<String> : ";
	Vector<String> inname(1), outname(2);
	inname(0) = "IACol"; outname(0) = "IACol"; outname(1) = "DCol";
	RowCopier rc(maintab, maintab, inname, outname);
    } catch (TableError x) {
	caught = True;
    } 
    if (caught) {
	cout << "OK" << endl;
	caught = False;
    } else {
	cout << "FAILS!" << endl;
	return 1;
    }

    // and finally, some checks that False is returned when appropriate.
    cout << "\nChecking that copy() returns False when appropriate" << endl;
    {
	RowCopier rc(maintab, maintab);

	// inrow exceeds number of input rows
	cout << "Input row number exceeds number of rows in input column : ";
	if (rc.copy(maintab.nrow()+1), 0) {
	    cout << "FAILS!" << endl;
	    return 1;
	} else {
	    cout << "OK" << endl;
	}

	// outrow exceeds number of output rows
	cout << "Output row number exceeds number of rows in output column : ";
	if (rc.copy(0,maintab.nrow()+1)) {
	    cout << "FAILS!" << endl;
	    return 1;
	} else {
	    cout << "OK" << endl;
	}
    }

    cout << "\ntRowCopier finishes successfully" << endl;
    return 0;
}
