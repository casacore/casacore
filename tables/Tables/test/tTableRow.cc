//# tTableRow.cc: Test program for class (RO)TableRow
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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


#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScaRecordColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the class (RO)TableRow
// </summary>

// This program tests the classes ROTableRow and TableRow.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
//
// In the second part of the program a table is created and read back.
// It times accessing it as columns and as rows.
// The number of rows can be specified as the first argument (default 500).
// When an argument is given, no statements resulting in exceptions
// will be executed. This gives the possibility to check for memory leaks
// (because the emulated exceptions result in leaks).


// First build a description.
void a (Bool)
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class Table";
    td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
    td.addColumn (ScalarColumnDesc<DComplex>("ag"));
    td.addColumn (ArrayColumnDesc<float>("arr1",3,ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<String>("arr2",0));
    td.addColumn (ArrayColumnDesc<float>("arr3",0));
    td.addColumn (ScalarRecordColumnDesc("rec"));

    // Now create a new table from the description.
    SetupNewTable newtab("tTableRow_tmp.data", td, Table::New);
    newtab.setShapeColumn("arr1",IPosition(3,2,3,4));
    Table tab(newtab, 10);

    Cube<float> arrf(IPosition(3,2,3,4));
    Vector<String> arrs(stringToVector ("a,bc,def,ghij,klmno,qprstu,vxxyzab,"
					"cdefghij,klmnopqrs,tuvwxyzabc"));
    indgen (arrf);
    TableRow row (tab, stringToVector("ab,ad,ag,arr1,arr2,rec"));
    AlwaysAssertExit (row.rowNumber() == -1);
    TableRecord rec (row.record().description(), RecordInterface::Variable);
    AlwaysAssertExit (row.record().nfields() == 6);
    RecordFieldPtr<Int>            ab(rec, 0);
    RecordFieldPtr<uInt>           ad(rec, 1);
    RecordFieldPtr<DComplex>       ag(rec, 2);
    RecordFieldPtr<Array<float> >  arr1(rec, 3);
    RecordFieldPtr<Array<String> > arr2(rec, 4);
    RecordFieldPtr<TableRecord>    recfld(rec, 5);
    ArrayColumn<float> arr3(tab, "arr3");
    TableRecord r1;
    Int i;
    for (i=0; i<10; i++) {
	ab.define (i);
	ad.define (i+2);
	ag.define (DComplex(i+3,-i-1));
	arr1.define (arrf);
	arr2.define (arrs(Slice(0,i)));
	r1.define (i, i);
	recfld.define (r1);
	row.put (i, rec);
	if (i%2 == 0) {
	    arr3.put (i, arrf);
	}
	arrf += (float)(arrf.nelements());
    }
    // Test if the record has an extra field.
    rec.define ("extraField", Int(1));
    row.putMatchingFields (9, rec);
    AlwaysAssertExit (row.rowNumber() == -1);

    ScalarColumn<Int> colab (tab, "ab");
    ScalarColumn<uInt> colad (tab, "ad");
    ScalarColumn<DComplex> colag (tab, "ag");
    ScalarColumn<TableRecord> colrec (tab, "rec");
    ArrayColumn<float> colarr1 (tab, "arr1");
    ArrayColumn<String> colarr2 (tab, "arr2");
    ArrayColumn<float> colarr3 (tab, "arr3");
    Int abval;
    uInt adval;
    DComplex agval;
    TableRecord recval;
    Cube<float> arrval(IPosition(3,2,3,4));
    Cube<float> arr3val;
    Array<String> arrstr;
    arrf -= (float)(arrf.nelements()*tab.nrow());
    for (i=0; i<10; i++) {
	colab.get (i, abval);
	colad.get (i, adval);
	colag.get (i, agval);
	if (abval != i  ||  Int(adval) != i+2  || agval != DComplex(i+3,-i-1)) {
	    cout << "error in row " << i << ": " << abval
		 << ", " << adval << ", " << agval << endl;
	}
	colrec.get (i, recval);
	if (Int(recval.nfields()) != i+1) {
	    cout << "error in row " << i << ": " << recval.nfields()
		 << " fields; expected " << i+1 << endl;
	} else {
	    for (Int j=0; j<=i; j++) {
		if (recval.asInt(j) != j) {
		    cout << "error in row " << i << ": invalid record" << endl;
		}
	    }
	}
	colarr1.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr1 in row " << i << endl;
	}
	colarr2.get (i, arrstr, True);
	AlwaysAssertExit (arrstr.ndim() == 1);
	AlwaysAssertExit (arrstr.shape()(0) == i);
	if (!allEQ (arrstr, arrs(Slice(0,i)))) {
	    cout << "error in arr2 in row " << i << endl;
	}
	if (i%2 == 0) {
	    colarr3.get (i, arr3val, True);
	    AlwaysAssertExit (arr3val.ndim() == 3);
	    if (!allEQ (arr3val, arrf)) {
		cout << "error in arr3 in row " << i << endl;
	    }
	}else{
	    AlwaysAssertExit (! colarr3.isDefined (i));
	}
	arrf += (float)(arrf.nelements());
    }

    TableRow rowa;
    AlwaysAssertExit (! rowa.isAttached());
    TableRow rowb(rowa);
    AlwaysAssertExit (! rowb.isAttached());
    rowb = row;
    AlwaysAssertExit (rowb.isAttached());
    AlwaysAssertExit (row.record().description() ==
		                       rowb.record().description());
    rowb = rowa;
    AlwaysAssertExit (! rowb.isAttached());
    TableRow rowc(row);
    AlwaysAssertExit (row.record().description() ==
		                       rowc.record().description());
}

void b (Bool doExcp)
{
    Table tab("tTableRow_tmp.data");
    if (doExcp) {
	try {
	    TableRow row (tab);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;             // not writable
	} 
	try {
	    ROTableRow row (tab, stringToVector("ab,abb"));
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;             // abb not exists
	} 
    }
    ROTableRow rowx (tab, stringToVector("ab,arr1"));
    ROTableRow rowy (tab, stringToVector("ab,bcd,arr1"), True);
    RORecordFieldPtr<Int>            ab(rowx.record(), 0);
    RORecordFieldPtr<uInt>           ad(rowy.record(), 0);
    RORecordFieldPtr<DComplex>       ag(rowy.record(), 1);
    RORecordFieldPtr<Array<float> >  arr1(rowx.record(), 1);
    RORecordFieldPtr<Array<String> > arr2(rowy.record(), 2);
    RORecordFieldPtr<Array<float> >  arr3(rowy.record(), 3);
    Cube<float> arrf(IPosition(3,2,3,4));
    Vector<String> arrs(stringToVector ("a,bc,def,ghij,klmno,qprstu,vxxyzab,"
					"cdefghij,klmnopqrs,tuvwxyzabc"));
    indgen (arrf);
    Int i;
    for (i=0; i<10; i++) {
	cout << "get scalar row " << i << endl;
	rowx.get (i);
	AlwaysAssertExit (rowx.rowNumber() == i);
	rowy.get (i);
	if (*ab != i  ||  Int(*ad) != i+2  ||  *ag != DComplex(i+3,-i-1)) {
	    cout << "error in row " << i << ": " << *ab
		 << ", " << *ad << ", " << *ag << endl;
	}
	if (!allEQ (*arr1, arrf)) {
	    cout << "error in arr1 in row " << i << endl;
	}
	if (!allEQ (*arr2, arrs(Slice(0,i)))) {
	    cout << "error in arr2 in row " << i << endl;
	}
	if (i%2 == 0) {
	    if (!allEQ (*arr3, arrf)) {
		cout << "error in arr3 in row " << i << endl;
	    }
	}else{
	    if ((*arr3).ndim() != 0) {
		cout << "error in arr3 in row " << i << endl;
	    }
	}		
	arrf += (float)(arrf.nelements());
    }

    // This get operation should not do anything.
    rowx.get (i-1);
    AlwaysAssertExit (rowx.rowNumber() == i-1);

    ROTableRow rowa;
    AlwaysAssertExit (! rowa.isAttached());
    ROTableRow rowb(rowa);
    AlwaysAssertExit (! rowb.isAttached());
    rowb = rowy;
    AlwaysAssertExit (rowb.isAttached());
    AlwaysAssertExit (rowy.record().description() ==
		                       rowb.record().description());
    rowb = rowa;
    AlwaysAssertExit (! rowb.isAttached());
    ROTableRow rowc(rowy);
    AlwaysAssertExit (rowy.record().description() ==
		                       rowc.record().description());
}

// This function times how fast it can read data back.
void c (Int nrow)
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class Table";
    td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
    td.addColumn (ScalarColumnDesc<DComplex>("ag"));
    td.addColumn (ArrayColumnDesc<float>("arr1",3,ColumnDesc::Direct));

    // Now create a new table from the description.
    SetupNewTable newtab("tTableRow_tmp.data1", td, Table::New);
    newtab.setShapeColumn("arr1",IPosition(3,2,3,4));
    Table tab(newtab, nrow);

    Cube<float> arrf(IPosition(3,2,3,4));
    indgen (arrf);
    cout << ">>>" << endl;
    Timer timer;
    TableRow row (tab);
    RecordFieldPtr<Int>            ab(row.record(), 0);
    RecordFieldPtr<uInt>           ad(row.record(), 1);
    RecordFieldPtr<DComplex>       ag(row.record(), 2);
    RecordFieldPtr<Array<float> >  arr1(row.record(), 3);
    Int i;
    for (i=0; i<nrow; i++) {
	ab.define (i);
	ad.define (i+2);
	ag.define (DComplex(i+3,-i-1));
	arr1.define (arrf);
	row.put (i);
    }
    timer.show("   put row   ");

    // Now time how long it takes to read it back as columns and
    // as rows.
    timer.mark();
    ScalarColumn<Int> colab (tab, "ab");
    ScalarColumn<uInt> colad (tab, "ad");
    ScalarColumn<DComplex> colag (tab, "ag");
    Int abval;
    uInt adval;
    DComplex agval;
    for (i=0; i<nrow; i++) {
	colab.get (i, abval);
	colad.get (i, adval);
	colag.get (i, agval);
    }
    timer.show ("scalar column");
    
    timer.mark();
    ArrayColumn<float> colarr1 (tab, "arr1");
    Cube<float> arrval(IPosition(3,2,3,4));
    for (i=0; i<nrow; i++) {
	colarr1.get (i, arrval, True);
    }
    timer.show (" array column");

    timer.mark();
    ROTableRow rowx (tab, stringToVector("ab,ad,ag"));
    RORecordFieldPtr<Int>            abr(rowx.record(), 0);
    RORecordFieldPtr<uInt>           adr(rowx.record(), 1);
    RORecordFieldPtr<DComplex>       agr(rowx.record(), 2);
    for (i=0; i<nrow; i++) {
	rowx.get (i);
    }
    timer.show ("scalar row   ");

    timer.mark();
    ROTableRow rowy (tab, stringToVector("arr1"));
    RORecordFieldPtr<Array<float> >  arr1r(rowy.record(), 0);
    for (i=0; i<nrow; i++) {
	rowy.get (i);
    }
    timer.show (" array row   ");
    cout << "<<<" << endl;
}


int main (int argc, const char* argv[])
{
    uInt nr = 500;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    try {
	a ( (argc<2));
	b ( (argc<2));
	c (nr);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}
