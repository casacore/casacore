//# tIncrementalStMan.cc: Test program for the IncrementalStMan storage manager
//# Copyright (C) 1994,1995,1996
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

#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/IncrStManAccessor.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Lattices/Slice.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <strstream.h>

// <summary>
// Test program for the IncrementalStMan storage manager
// </summary>

// This program tests the IncrementalStMan storage manager, especially the
// get and put functions.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a(uInt bucketSize);
void b();

main (int argc, char** argv) {
    uInt nr = 1000;
    if (argc > 1) {
	istrstream istr(argv[1]);
	istr >> nr;
    }
    try {
	a(nr);
	b();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    return 0;                           // exit with success status
}


void init (Cube<float>& arrf, Vector<DComplex>& arrdc, Cube<Bool>& arrb)
{
    indgen (arrf.ac());
    arrdc(0) = DComplex(1.2, 3.4);
    arrdc(1) = DComplex(-2.3, 5.6);
    IPosition shape(arrb.shape());
    uInt n = 0;
    for (uInt i=0; i<shape(2); i++) {
	for (uInt j=0; j<shape(1); j++) {
	    for (uInt k=0; k<shape(0); k++) {
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
void a (uInt bucketSize)
{
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
    Table tab(newtab, 10);

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
	arrf.ac() += (float)(arrf.nelements());
	arrdc.ac() += DComplex(2, 3);
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
	arrf.ac() += (float)(arrf.nelements()); 
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
    vecae.ac() += float(1);
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

void b()
{
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
    ROScalarColumn<Complex> ac(tab, "ac");
    ROScalarColumn<Int> ad(tab, "ad");
    ROScalarColumn<float> ae(tab, "ae");
    ROScalarColumn<String> af(tab, "af");
    ROArrayColumn<float> arr1(tab,"arr1");
    ROArrayColumn<float> arr2(tab,"arr2");
    ROArrayColumn<float> arr3(tab,"arr3");
    ROArrayColumn<DComplex> arr5(tab,"arr5");
    ROArrayColumn<Bool> arr6(tab,"arr6");
    ROArrayColumn<Bool> arr7(tab,"arr7");
    cout << "#Rows " << tab.nrow() << endl;
    uInt i;
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
    Cube<float> arrf(2,3,4);
    Vector<DComplex> arrdc(2);
    Cube<Bool> arrb(5,7,11);
    init (arrf, arrdc, arrb);
    for (i=0; i<19; i++) {
	if (!allEQ (arr1(i), arrf.ac() + 24*arr1Start[i]))
	    cout << i << " arr1-mismatch: " << arr1(i) << ", ";
    }
    cout << arr1(19) << endl;
    for (i=0; i<19; i++) {
	if (!allEQ (arr2(i), arrf.ac() + 24*arr2Start[i]))
	    cout << i << " arr2-mismatch: " << arr2(i) << ", ";
    }
    cout << arr2(19) << endl;
    for (i=0; i<19; i++) {
	if (!allEQ(arr3(i), arrf.ac() + 24*arr3Start[i]))
	    cout << i << " arr3-mismatch: " << arr3(i) << ", ";
    }
    cout << arr3(19) << endl;
    for (i=0; i<19; i++) {
	uInt j = min(9U,i);
	if (!allEQ(arr5(i), arrdc.ac() + DComplex(2*j, 3*j)))
	    cout << i << " arr5-mismatch: " << arr5(i) << ", ";
    }
    for (i=0; i<19; i++) {
	if (!allEQ(arr6(i), arrb.ac()))
	    cout << i << " arr6-mismatch: " << arr6(i) << ", ";
    }
    for (i=0; i<19; i++) {
	if (i == 19) {
	    arrb(0,0,0) = True;
	}
	if (!allEQ(arr7(i), arrb.ac()))
	    cout << i << " arr7-mismatch: " << arr7(i) << ", ";
    }
    arrb(0,0,0) = False;

    IPosition bshape = arrb.shape();
    Cube<Bool> arrb1 (bshape);
    for (i=0; i<19; i++) {
	for (uInt j=0; j<bshape(0); j++) {
	    Array<Bool> result (arrb1(IPosition(3,j,0,0),
				      IPosition(3,j,bshape(1)-1,bshape(2)-1)));
	    arr6.getSlice (i, Slicer(IPosition(3,j,0,0),
				     IPosition(3,1,bshape(1),bshape(2))),
			   result);

	}
	if (!allEQ(arrb1.ac(), arrb.ac()))
	    cout << i << " arr6-slice-mismatch: " << arrb1.ac() << ", ";
	arrb1.set (True);
    }
    for (i=0; i<19; i++) {
	if (i == 19) {
	    arrb(0,0,0) = True;
	}
	for (uInt j=0; j<bshape(0); j++) {
	    Array<Bool> result (arrb1(IPosition(3,j,0,0),
				      IPosition(3,j,bshape(1)-1,bshape(2)-1)));
	    arr7.getSlice (i, Slicer(IPosition(3,j,0,0),
				     IPosition(3,1,bshape(1),bshape(2))),
			   result);
	}
	if (!allEQ(arrb1.ac(), arrb.ac()))
	    cout << i << " arr7-slice-mismatch: " << arrb1.ac() << ", ";
	arrb1.set (True);
    }
    accessor.showCacheStatistics (cout);
    accessor.clearCache();
}
