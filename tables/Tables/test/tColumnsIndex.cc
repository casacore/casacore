//# tColumnsIndex.cc: Test program for the ColumnsIndex class
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
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for the ColumnsIndex class
// </summary>


// First build a description.
void a()
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class Table";
    td.addColumn (ScalarColumnDesc<Bool>("abool"));
    td.addColumn (ScalarColumnDesc<uChar>("auchar"));
    td.addColumn (ScalarColumnDesc<Short>("ashort"));
    td.addColumn (ScalarColumnDesc<Int>("aint"));
    td.addColumn (ScalarColumnDesc<uInt>("auint"));
    td.addColumn (ScalarColumnDesc<Float>("afloat"));
    td.addColumn (ScalarColumnDesc<Double>("adouble"));
    td.addColumn (ScalarColumnDesc<Complex>("acomplex"));
    td.addColumn (ScalarColumnDesc<DComplex>("adcomplex"));
    td.addColumn (ScalarColumnDesc<String>("astring"));

    // Now create a new table from the description.
    const Int nrrow = 10;
    SetupNewTable newtab("tColumnsIndex_tmp.data", td, Table::New);
    Table tab(newtab, nrrow);
    ScalarColumn<Bool> abool(tab, "abool");
    ScalarColumn<uChar> auchar(tab, "auchar");
    ScalarColumn<Short> ashort(tab, "ashort");
    ScalarColumn<Int> aint(tab, "aint");
    ScalarColumn<uInt> auint(tab, "auint");
    ScalarColumn<Float> afloat(tab,  "afloat");
    ScalarColumn<Double> adouble(tab,"adouble");
    ScalarColumn<Complex> acomplex(tab, "acomplex");
    ScalarColumn<DComplex> adcomplex(tab, "adcomplex");
    ScalarColumn<String> astring(tab, "astring");
    char str[8];
    for (Int i=0; i<nrrow; i++) {
	abool.put (i, (i%2 == 0));
	auchar.put (i, i);
	ashort.put (i, i);
	aint.put (i, i);
	auint.put (i, i);
	afloat.put (i, i);
	adouble.put (i, i);
	acomplex.put (i, Complex(i,0));
	adcomplex.put (i, DComplex(0,i));
	sprintf (str, "V%i", i);
	astring.put (i, str);
    }
}

void b()
{
    Table tab("tColumnsIndex_tmp.data", TableLock(TableLock::UserLocking));
    const uInt nrrow = tab.nrow();
    ColumnsIndex colInx0 (tab, "abool");
    ColumnsIndex colInx1 (tab, "auchar");
    ColumnsIndex colInx2 (tab, "ashort");
    ColumnsIndex colInx3 (tab, "aint");
    ColumnsIndex colInx4 (tab, "auint");
    ColumnsIndex colInx5 (tab, "afloat");
    ColumnsIndex colInx6 (tab, "adouble");
    ColumnsIndex colInx7 (tab, "acomplex");
    ColumnsIndex colInx8 (tab, "adcomplex");
    ColumnsIndex colInx9 (tab, "astring");
    AlwaysAssertExit (! colInx0.isUnique());
    AlwaysAssertExit (colInx1.isUnique());
    RecordFieldPtr<Bool> abool (colInx0.accessKey(), "abool");
    RecordFieldPtr<uChar> auchar (colInx1.accessKey(), "auchar");
    RecordFieldPtr<Short> ashort (colInx2.accessKey(), "ashort");
    RecordFieldPtr<Int> aint (colInx3.accessKey(), "aint");
    RecordFieldPtr<uInt> auint (colInx4.accessKey(), "auint");
    RecordFieldPtr<Float> afloat (colInx5.accessKey(), "afloat");
    RecordFieldPtr<Double> adouble (colInx6.accessKey(), "adouble");
    RecordFieldPtr<Complex> acomplex (colInx7.accessKey(), "acomplex");
    RecordFieldPtr<DComplex> adcomplex (colInx8.accessKey(), "adcomplex");
    RecordFieldPtr<String> astring (colInx9.accessKey(), "astring");
    Record rec;
    Vector<uInt> rows;
    Bool found;
    char str[8];
    // Test each individual type.
    uInt i;
    for (i=0; i<nrrow; i++) {
        rec.define ("auint", i);
        AlwaysAssertExit ( (colInx4.getRowNumber(found, rec) == i
				  && found));
        *auchar = i;
        *ashort = i;
        *aint = i;
        *auint = i;
        *afloat = i;
        *adouble = i;
        *acomplex = Complex(i,0);
        *adcomplex = DComplex(0,i);
	sprintf (str, "V%i", i);
        *astring = str;
        AlwaysAssertExit ( (colInx1.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx2.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx3.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx4.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx5.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx6.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx7.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx8.getRowNumber(found) == i  && found));
        AlwaysAssertExit ( (colInx9.getRowNumber(found) == i  && found));
    }
    // Make sure low and high value are not found.
    *aint = -1;
    colInx3.getRowNumber(found);
    AlwaysAssertExit ((!found));
    *aint = nrrow;
    colInx3.getRowNumber(found);
    AlwaysAssertExit ((!found));
    // Test a bool.
    *abool = True;
    try {
        colInx0.getRowNumber(found);
    } catch (AipsError x) {
        cout << x.getMesg() << endl;       // values are not unique
    } 
    rows = colInx0.getRowNumbers();
    AlwaysAssertExit (rows.nelements() == (nrrow+1)/2);
    for (i=0; i<rows.nelements(); i++) {
        AlwaysAssertExit (rows(i) == i*2);
    }
    *abool = False;
    rows.resize(0);
    rows = colInx0.getRowNumbers();
    AlwaysAssertExit (rows.nelements() == nrrow/2);
    for (i=0; i<rows.nelements(); i++) {
        AlwaysAssertExit (rows(i) == 1+i*2);
    }
    // Test a range.
    Record lower, upper;
    lower.define ("auint", uInt(2));
    upper.define ("auint", uInt(4));
    rows.resize(0);
    rows = colInx4.getRowNumbers (lower, upper, True, False);
    AlwaysAssertExit (rows.nelements()==2 && rows(0)==2 && rows(1)==3);
    rows.resize(0);
    rows = colInx4.getRowNumbers (lower, upper, False, True);
    AlwaysAssertExit (rows.nelements()==2 && rows(0)==3 && rows(1)==4);
    upper.define ("auint", uInt(1));
    rows.resize(0);
    rows = colInx4.getRowNumbers (lower, upper, True, True);
    AlwaysAssertExit (rows.nelements()==0);
}

Int tcompare (const Block<void*>& fieldPtrs, const Block<void*>& dataPtrs,
	      const Block<Int>& dataTypes, Int index)
{
    AlwaysAssert (dataTypes.nelements() == 2, AipsError);
    AlwaysAssert (dataTypes[0] == TpDouble  &&  dataTypes[1] == TpFloat,
		 AipsError);
    const Double keyTime = *(*(const RecordFieldPtr<Double>*)(fieldPtrs[0]));
    const Double time = ((const Double*)(dataPtrs[0]))[index];
    const Double width = ((const Float*)(dataPtrs[1]))[index];
    const Double start = time - width/2;
    const Double end = time + width/2;
    if (keyTime < start) {
        return -1;
    } else if (keyTime > end) {
        return 1;
    }
    return 0;
}

void c()
{
    Table tab("tColumnsIndex_tmp.data", Table::Update);
    // Create the index with the special compare function.
    ColumnsIndex colInx0 (tab, stringToVector("adouble,afloat"), tcompare);
    RecordFieldPtr<Double> keydouble (colInx0.accessKey(), "adouble");
    const Int nrrow = tab.nrow();
    ScalarColumn<Int> aint(tab, "aint");
    ScalarColumn<uInt> auint(tab, "auint");
    ScalarColumn<Float> afloat(tab, "afloat");
    // Change a the values of a few columns.
    Int i;
    for (i=0; i<nrrow; i++) {
	aint.put (i, -i);
	auint.put (i, 1+2*(i/3));
	afloat.put (i, 0.8);
    }
    // Tell the index that some columns have changed.
    colInx0.setChanged ("aint");
    colInx0.setChanged ("auint");
    colInx0.setChanged ("afloat");
    // Now test the special compare function.
    Bool found;
    *keydouble = -0.5;
    for (i=0; i<21; i++) {
        Int inx = colInx0.getRowNumber(found);
	if (i%2 == 0) {
	    AlwaysAssertExit ((!found));
	} else {
	    AlwaysAssertExit (inx == i/2  &&  found);
	}
        *keydouble += 0.5;
    }
}

void d()
{
    Table tab("tColumnsIndex_tmp.data", Table::Update);
    Int nrrow = tab.nrow();
    ColumnsIndex colInx3 (tab, "aint");
    ColumnsIndex colInx4 (tab, "auint");
    RecordFieldPtr<Int> aint (colInx3.accessKey(), "aint");
    RecordFieldPtr<uInt> auint (colInx4.accessKey(), "auint");
    Bool found;
    Int i;
    for (i=0; i<nrrow; i++) {
        *aint = -i;
        AlwaysAssertExit ( (Int(colInx3.getRowNumber(found)) == i
				  && found));
        *auint = 1+2*(i/3);
	cout << colInx4.getRowNumbers() << endl;
	*auint += 1;
	AlwaysAssertExit (colInx4.getRowNumbers().nelements() == 0);
    }
    // Now test an index consisting of multiple columns.
    ColumnsIndex colInx5 (tab, stringToVector("abool,auint"));
    RecordFieldPtr<Bool> abool1 (colInx5.accessKey(), "abool");
    RecordFieldPtr<uInt> auint1 (colInx5.accessKey(), "auint");
    for (i=0; i<(nrrow+2)/3; i++) {
        *auint1 = 1+2*i;
	*abool1 = True;
	cout << colInx5.getRowNumbers() << ' ';
	*abool1 = False;
	cout << colInx5.getRowNumbers() << endl;
    }
    // Now test a range of multiple columns.
    RecordFieldPtr<Bool> abool1l (colInx5.accessLowerKey(), "abool");
    RecordFieldPtr<uInt> auint1l (colInx5.accessLowerKey(), "auint");
    RecordFieldPtr<Bool> abool1u (colInx5.accessUpperKey(), "abool");
    RecordFieldPtr<uInt> auint1u (colInx5.accessUpperKey(), "auint");
    *abool1l = True;
    *abool1u = True;
    *auint1l = 3;
    *auint1u = 10;
    cout << colInx5.getRowNumbers (True, True) << ' ';
    cout << colInx5.getRowNumbers (False, False) << endl;
    *auint1l = 0;
    *auint1u = 10;
    cout << colInx5.getRowNumbers (True, True) << ' ';
    cout << colInx5.getRowNumbers (False, False) << endl;
    *auint1l = 2;
    *auint1u = 6;
    cout << colInx5.getRowNumbers (True, True) << ' ';
    cout << colInx5.getRowNumbers (False, False) << endl;
    *abool1l = False;
    *abool1u = False;
    cout << colInx5.getRowNumbers (True, True) << ' ';
    cout << colInx5.getRowNumbers (False, False) << endl;

    // Now test extending the table.
    // The index should be updated automatically, so create that first.
    ColumnsIndex colInx6 (tab, "adouble");
    RecordFieldPtr<Double> adouble (colInx6.accessKey(), "adouble");
    if (nrrow < 1000) {
        tab.addRow (1000-nrrow);
	nrrow = 1000;
    }
    ScalarColumn<Double> cdouble(tab, "adouble");
    // Change a the values of a few columns.
    for (i=0; i<nrrow; i++) {
	cdouble.put (i, i);
    }
    cout << ">>>" << endl;
    Timer timer;
    for (i=0; i<100*nrrow; i++) {
        *adouble = i/100;
        AlwaysAssertExit ( (Int(colInx6.getRowNumber(found)) == i/100
				  && found));
    }
    timer.show ("100000*find");
    cout << "<<<" << endl;
}

int main()
{
    try {
        a();
	b();
	c();
	d();
    } catch (AipsError x) {
        cout << "Exception caught: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;
}
