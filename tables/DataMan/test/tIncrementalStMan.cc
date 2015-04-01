//# tIncrementalStMan.cc: Test program for the IncrementalStMan storage manager
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001,2003
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
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/IncrStManAccessor.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the IncrementalStMan storage manager
// </summary>

// This program tests the IncrementalStMan storage manager, especially the
// get and put functions.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a (uInt bucketSize, uInt mode);
void b (const Vector<Bool>& removedRows);
void c();
void d();
void e (uInt nrrow);
void f();

int main (int argc, const char* argv[])
{
    uInt nr = 1000;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    try {
	a (nr, 0);
	c();
	a (0, 1);
	e (20);
	d();
	e (10);
	a (0, 2);
	e (20);
	a (nr, 0);
	f();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}


void init (Cube<Float>& arrf, Vector<DComplex>& arrdc, Cube<Bool>& arrb)
{
       // Hey it's a bug in the SGI compiler that's
       // why the static_cast
    indgen (static_cast< Cube<Float> &>(arrf));
    arrdc(0) = DComplex(1.2, 3.4);
    arrdc(1) = DComplex(-2.3, 5.6);
    IPosition shape(arrb.shape());
    uInt n = 0;
    for (Int i=0; i<shape(2); i++) {
	for (Int j=0; j<shape(1); j++) {
	    for (Int k=0; k<shape(0); k++) {
		if (n++%3 == 2) {
		    arrb(k,j,i) = True;
		}else{
		    arrb(k,j,i) = False;
		}
	    }
	}
    }
}


// First build a description.
void a (uInt bucketSize, uInt mode)
{
    Table tab;
    if (mode == 0) {
	DataManager::registerCtor ("IncrementalStMan",
				   IncrementalStMan::makeObject);
	// Build the table description.
	TableDesc td("", "1", TableDesc::Scratch);
	td.comment() = "A test of class TableDesc";
	td.addColumn (ScalarColumnDesc<Complex>("ac"));
	td.addColumn (ScalarColumnDesc<Int>("ad"));
	td.addColumn (ScalarColumnDesc<float>("ae"));
	td.addColumn (ScalarColumnDesc<String>("af"));
	td.addColumn (ArrayColumnDesc<float>("arr1",3,ColumnDesc::Direct));
	td.addColumn (ArrayColumnDesc<float>("arr2",0));
	td.addColumn (ArrayColumnDesc<float>("arr3",0,ColumnDesc::Direct));
	td.addColumn (ArrayColumnDesc<String>("arr4",0,ColumnDesc::Direct));
	td.addColumn (ArrayColumnDesc<DComplex>("arr5",0,ColumnDesc::Direct));
	td.addColumn (ArrayColumnDesc<Bool>("arr6",0,ColumnDesc::Direct));
	td.addColumn (ArrayColumnDesc<Bool>("arr7",0,ColumnDesc::FixedShape));
	
	// Now create a new table from the description.
	SetupNewTable newtab("tIncrementalStMan_tmp.data", td, Table::New);
	// Create a storage manager for it.
	IncrementalStMan sm1 ("ISM", bucketSize, False);
	newtab.bindAll (sm1);
	newtab.setShapeColumn("arr1",IPosition(3,2,3,4));
	newtab.setShapeColumn("arr3",IPosition(3,2,3,4));
	newtab.setShapeColumn("arr4",IPosition(1,8));
	newtab.setShapeColumn("arr5",IPosition(1,2));
	newtab.setShapeColumn("arr6",IPosition(3,5,7,11));
	newtab.setShapeColumn("arr7",IPosition(3,5,7,11));
	tab = Table (newtab, 10);
    } else {
	tab = Table ("tIncrementalStMan_tmp.data", Table::Update);
    }

    ScalarColumn<Complex> ac(tab,"ac");
    ScalarColumn<Int> ad(tab,"ad");
    ScalarColumn<float> ae(tab,"ae");
    ScalarColumn<String> af(tab,"af");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<float> arr3(tab,"arr3");
    ArrayColumn<String> arr4(tab,"arr4");
    ArrayColumn<DComplex> arr5(tab, "arr5");
    ArrayColumn<Bool> arr6(tab, "arr6");
    ArrayColumn<Bool> arr7(tab, "arr7");
    Cube<float> arrf(IPosition(3,2,3,4));
    Vector<DComplex> arrdc(2);
    Cube<Bool> arrb(IPosition(3,5,7,11));
    init (arrf, arrdc, arrb);
    uInt i;
    for (i=0; i<10; i++) {
	if (mode < 2) {
	    if (mode == 1) {
		tab.addRow();
	    }
	    if (i%2 == 1) {
		ac.put (i, Complex(i+1));
	    }
	    ad.put (i, i);
	    ae.put (i, 10);
	    arr1.put (i, arrf);
	    if (i%4 == 0) {
		arr2.put (i, arrf);
		arr3.put (i, arrf);
	    }
	    arr5.put (i, arrdc);
	    arr6.put (i, arrb);
	    if (i == 0) {
		arr7.put (i, arrb);
	    }
	}
	arrf += (float)(arrf.nelements());
	arrdc += DComplex(2, 3);
    }
    if (mode == 2) {
	Vector<float> vecae = ae.getColumn();
	vecae -= float(1);
	ae.putColumn (vecae);
    }
    String str("abc");
    for (i=0; i<10; i++) {
	tab.addRow();
	if (i%2 == 1) {
	    ac.put (i+10, Complex(i+10+1));
	}
	ad.put (i+10, i+10);
	ae.put (i+10, i/3);
	if (i%3 == 0)
	    str += 'a';
	af.put (i+10, str);
	if (i%3 != 0) {
	    arr1.put (i, arrf);
	    arr1.put (i+10, arrf);
	}
	if (i%3 == 1) {
	    arr2.put (i, arrf);
	    arr2.put (i+10, arrf);
	    arr3.put (i, arrf);
	    arr3.put (i+10, arrf);
	}
	arrf += (float)(arrf.nelements()); 
    }

    for (i=0; i<19; i++) {
	cout << ae(i) << ", ";
    }
    cout << ae(19) << endl;
    Vector<float> vecae = ae.getColumn();
    for (i=0; i<vecae.nelements(); i++) {
	cout << vecae(i) << ", ";
    }
    cout << endl;
    vecae += float(1);
    ae.putColumn (vecae);
    for (i=0; i<19; i++) {
	cout << af(i) << ", ";
    }
    cout << af(19) << endl;
    Vector<float> vecae2 = ae.getColumn();
    for (i=0; i<vecae2.nelements(); i++) {
	cout << vecae2(i) << ", ";
    }
    cout << endl;
    Vector<String> vstr(8);
    vstr(0) = "z";
    for (i=1; i<vstr.nelements(); i++) {
	vstr(i) = vstr(i-1) + vstr(i-1);
    }
    // All rows from 0 on get this value.
    arr4.put (0, vstr);
    cout << "arr4 = " << arr4.getColumn (Slicer(Slice(0,1))) << endl;
    // All rows from 1 on get this value.
    arr4.put (1, vstr);
    cout << "arr4 = " << arr4.getColumn (Slicer(Slice(0,1))) << endl;
    vstr(0) = "a";
    // Row 1 gets this value.
    arr4.put (1, vstr);
    cout << "arr4 = " << arr4.getColumn (Slicer(Slice(0,1))) << endl;
    // All rows from 1 on get this value.
    arr4.put (2, vstr);
    cout << "arr4 = " << arr4.getColumn (Slicer(Slice(0,1))) << endl;
    // Replace a value in the last Bool array.
    arrb(0,0,0) = True;
    arr7.putSlice (19, Slicer(IPosition(3,0,0,0),IPosition(3,1,1,1)),
		   arrb(IPosition(3,0,0,0),IPosition(3,0,0,0)));
}

void b (const Vector<Bool>& removedRows)
{
    //# Define the values of the scalars (for Strings the lengths).
    static float acvalues[] = {-1,2,2,4,4,6,6,8,8,10,
			       10,12,12,14,14,16,16,18,18,20};
    static int advalues[] = {0,1,2,3,4,5,6,7,8,9,
			     10,11,12,13,14,15,16,17,18,19};
    static float aevalues[] = {11,11,11,11,11,11,11,11,11,11,
			       1,1,1,2,2,2,3,3,3,4};
    static int afvalues[] = {0,0,0,0,0,0,0,0,0,0,
			     4,4,4,5,5,5,6,6,6,7};
    //# Define what first element of the array in each row should be.
    static float arr1Start[] = {0,11,12,3,14,15,6,17,18,9,
				9,11,12,12,14,15,15,17,18,18};
    static float arr2Start[] = {0,11,0,0,14,4,4,17,8,8,
				8,11,11,11,14,14,14,17,17,17};
    static float arr3Start[] = {0,11,0,0,14,4,4,17,8,8,
				8,11,11,11,14,14,14,17,17,17};
    Table tab ("tIncrementalStMan_tmp.data");
    ROIncrementalStManAccessor accessor (tab, "ISM"); 
    accessor.setCacheSize (2);
    ScalarColumn<Complex> ac(tab, "ac");
    ScalarColumn<Int> ad(tab, "ad");
    ScalarColumn<float> ae(tab, "ae");
    ScalarColumn<String> af(tab, "af");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<float> arr3(tab,"arr3");
    ArrayColumn<DComplex> arr5(tab,"arr5");
    ArrayColumn<Bool> arr6(tab,"arr6");
    ArrayColumn<Bool> arr7(tab,"arr7");
    cout << "#Rows " << tab.nrow() << endl;
    uInt i;
    if (tab.nrow() == 20) {
	for (i=0; i<19; i++) {
	    cout << ac(i) << ", ";
	}
	cout << ac(19) << endl;
	for (i=0; i<19; i++) {
	    cout << ad(i) << ", ";
	}
	cout << ad(19) << endl;
	for (i=0; i<19; i++) {
	    cout << ae(i) << ", ";
	}
	cout << ae(19) << endl;
	for (i=0; i<19; i++) {
	    cout << af(i) << ", ";
	}
	cout << af(19) << endl;
    }
    Cube<float> arrf(2,3,4);
    Vector<DComplex> arrdc(2);
    Cube<Bool> arrb(5,7,11);
    init (arrf, arrdc, arrb);
    // Check if all values match.
    uInt rownr = 0;
    for (i=0; i<19; i++) {
	if (!removedRows(i)) {
	    if (i>0  &&  ac(rownr) != Complex(acvalues[i]))
		cout << i << "," << rownr << " ac-mismatch: "
		    << ac(rownr) << endl;
	    if (ad(rownr) != advalues[i])
		cout << i << "," << rownr << " ad-mismatch: "
		    << ad(rownr) << endl;
	    if (ae(rownr) != aevalues[i])
		cout << i << "," << rownr << " ae-mismatch: "
		    << ae(rownr) << endl;
	    if (Int(af(rownr).length()) != afvalues[i])
		cout << i << "," << rownr << " af-mismatch: "
		    << af(rownr) << endl;
	    if (!allEQ (arr1(rownr), arrf + 24*arr1Start[i]))
		cout << i << "," << rownr << " arr1-mismatch: "
		    << arr1(rownr) << endl;
	    if (!allEQ (arr2(rownr), arrf + 24*arr2Start[i]))
		cout << i << "," << rownr << " arr2-mismatch: "
		    << arr2(rownr) << endl;
	    if (!allEQ(arr3(rownr), arrf + 24*arr3Start[i]))
		cout << i << "," << rownr << " arr3-mismatch: "
		    << arr3(rownr) << endl;
	    uInt j = min(9U,i);
	    if (!allEQ(arr5(rownr), arrdc + DComplex(2*j, 3*j)))
		cout << i << "," << rownr << " arr5-mismatch: "
		    << arr5(rownr) << endl;
	    if (!allEQ(arr6(rownr), arrb))
		cout << i << "," << rownr << " arr6-mismatch: "
		    << arr6(rownr) << endl;
	    if (i == 19) {
		arrb(0,0,0) = True;
	    }
	    if (!allEQ(arr7(rownr), arrb))
		cout << i << "," << rownr << " arr7-mismatch: "
		    << arr7(rownr) << endl;
	    rownr++;
	}
    }

    arrb(0,0,0) = False;
    if (tab.nrow() == 20) {
	cout << arr1(19) << endl;
	cout << arr2(19) << endl;
	cout << arr3(19) << endl;
	IPosition bshape = arrb.shape();
	Cube<Bool> arrb1 (bshape);
	for (i=0; i<19; i++) {
	    for (Int j=0; j<bshape(0); j++) {
		Array<Bool> result (arrb1(IPosition(3,j,0,0),
				      IPosition(3,j,bshape(1)-1,bshape(2)-1)));
		arr6.getSlice (i, Slicer(IPosition(3,j,0,0),
				      IPosition(3,1,bshape(1),bshape(2))),
			       result);
	    }
	    if (!allEQ(arrb1, arrb))
		cout << i << " arr6-slice-mismatch: " << arrb1 << ", ";
	    arrb1.set (True);
	}
	for (i=0; i<19; i++) {
	    if (i == 19) {
		arrb(0,0,0) = True;
	    }
	    for (Int j=0; j<bshape(0); j++) {
		Array<Bool> result (arrb1(IPosition(3,j,0,0),
				      IPosition(3,j,bshape(1)-1,bshape(2)-1)));
		arr7.getSlice (i, Slicer(IPosition(3,j,0,0),
				     IPosition(3,1,bshape(1),bshape(2))),
			       result);
	    }
	    if (!allEQ(arrb1, arrb))
		cout << i << " arr7-slice-mismatch: " << arrb1 << ", ";
	    arrb1.set (True);
	}
    }
    if (tab.nrow() % 5 == 0) {
	accessor.showCacheStatistics (cout);
    }
    accessor.clearCache();
}

void c()
{
    uInt i;
    Vector<Bool> removedRows(20);
    removedRows.set (False);
    b (removedRows);
    // Remove several rows.
    // Open the table as read/write for that purpose.
    Table rwtab ("tIncrementalStMan_tmp.data", Table::Update);
    rwtab.removeRow (17);
    AlwaysAssertExit (rwtab.nrow() == 19);
    removedRows(17) = True;
    b (removedRows);
    rwtab.removeRow (18);
    AlwaysAssertExit (rwtab.nrow() == 18);
    removedRows(19) = True;
    b (removedRows);
    rwtab.removeRow (10);
    AlwaysAssertExit (rwtab.nrow() == 17);
    removedRows(10) = True;
    b (removedRows);
    rwtab.removeRow (0);
    AlwaysAssertExit (rwtab.nrow() == 16);
    removedRows(0) = True;
    b (removedRows);
    rwtab.removeRow (10);
    AlwaysAssertExit (rwtab.nrow() == 15);
    removedRows(12) = True;                    // row 10 was old row 12
    b (removedRows);
    // Remove several rows.
    Vector<uInt> rows(5);
    for (i=0; i<5; i++) {
	rows(i)=i+2;
	removedRows(i+3) = True;
    }
    rwtab.removeRow (rows);
    AlwaysAssertExit (rwtab.nrow() == 10);
    b (removedRows);
    // Remove all remaining rows.
    for (i=0; i<10; i++) {
	rwtab.removeRow (0);
	AlwaysAssertExit (rwtab.nrow() == 9-i);
	for (uInt j=0; j<20; j++) {
	    if (!removedRows(j)) {
		removedRows(j) = True;
		break;
	    }
	}
	b (removedRows);
    }
}

void d()
{
    uInt i;
    // Remove the last 10 rows.
    // Open the table as read/write for that purpose.
    Table rwtab ("tIncrementalStMan_tmp.data", Table::Update);
    Vector<uInt> rows(10);
    for (i=0; i<10; i++) {
	rows(i)=i+10;
    }
    rwtab.removeRow (rows);
    AlwaysAssertExit (rwtab.nrow() == 10);
}

void e (uInt nrrow)
{
    uInt i;
    Vector<Bool> removedRows(20);
    removedRows.set (True);
    for (i=0; i<nrrow; i++) {
	removedRows(i) = False;
    }
    b (removedRows);
}

void f()
{
    // Open the table as read/write for that purpose.
    Table rwtab ("tIncrementalStMan_tmp.data", Table::Update);
    ArrayColumn<float> arr1(rwtab,"arr1");
    ArrayColumn<float> arr2(rwtab,"arr2");
    ArrayColumn<Bool> arr7(rwtab, "arr7");
    Vector<Bool> vecb(10);
    Vector<float> vecf(10);
    indgen (vecf);
    //# Try to change some arrays (which cannot be done).
    try {
	arr1.put (0, vecf);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;         // shape cannot change
    } 
    try {
	arr7.put (0, vecb);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;         // shape cannot change
    } 
    Vector<Bool> removedRows(20);
    removedRows.set (False);
    b (removedRows);
    //# Change an array which can be changed.
    //# Check value, change it back and check all values.
    Array<float> arrrow0  = arr2(0);
    Array<float> arrrow12 = arr2(12);
    arr2.put (0, vecf);
    arr2.put (12, vecf);
    AlwaysAssertExit (allEQ (arr2(0), vecf));
    AlwaysAssertExit (allEQ (arr2(12), vecf));
    arr2.put (0, arrrow0);
    arr2.put (12, arrrow12);
    b (removedRows);
}
