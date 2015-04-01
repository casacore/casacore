//# tForwardCol.cc: Test program for class ForwardColumn
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002
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

#include <casacore/casa/stdio.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary> Test program for class ForwardColumn </summary>

// This program tests the virtual column engine ForwardCol.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

TableDesc makeDesc();
void a (const TableDesc&);
void b (const TableDesc&);
void c (const TableDesc&);
void check(const String& tableName, Int abOffset, Int acOffset);

int main ()
{
    try {
	TableDesc td = makeDesc();
	a (td);
	check("tForwardCol_tmp.data0", 0, 0);
	check("tForwardCol_tmp.sort0", 0, 0);
	b (td);
	check("tForwardCol_tmp.data1", 0, 0);
	c (td);
	check("tForwardCol_tmp.data0", 0, 10);
	check("tForwardCol_tmp.data1", 0, 10);
	check("tForwardCol_tmp.data2", 10, 10);
	check("tForwardCol_tmp.data3", 10, 10);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

// First build a description.
TableDesc makeDesc()
{
    // First register the virtual column engine.
    ForwardColumnEngine::registerClass();

    // Build the table description.
    TableDesc td("tTableDesc","1",TableDesc::Scratch);
    td.comment() = "A test of class ForwardColumn";
    td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<Int>("ac"));
    td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
    td.addColumn (ScalarColumnDesc<float>("ae"));
    td.addColumn (ScalarColumnDesc<String>("af"));
    td.addColumn (ScalarColumnDesc<DComplex>("ag"));
    td.addColumn (ArrayColumnDesc<float>("arr1",3,ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<float>("arr2",0));
    td.addColumn (ArrayColumnDesc<float>("arr3",0,ColumnDesc::Direct));
    return td;
}

void a (const TableDesc& td)
{
    // Now create a new table from the description.
    SetupNewTable newtab("tForwardCol_tmp.data0", td, Table::New);
    newtab.setShapeColumn("arr1",IPosition(3,2,3,4));
    newtab.setShapeColumn("arr3",IPosition(3,2,3,4));
    Table tab(newtab, 10);

    ScalarColumn<Int> ab1(tab,"ab");
    ScalarColumn<Int> ab2(tab,"ab");
    ScalarColumn<Int> ac (tab,"ac");
    ScalarColumn<uInt> ad(tab,"ad");
    ScalarColumn<float> ae(tab,"ae");
    ScalarColumn<String> af(tab,"af");
    TableColumn ag1(tab,"ag");
    ScalarColumn<DComplex> ag(tab,"ag");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<float> arr3(tab,"arr3");
    Cube<float> arrf(IPosition(3,2,3,4));
    uInt i;
    char str[8];
    indgen (arrf);
    for (i=0; i<10; i++) {
	ab1.put (i, i);
	ac.put (i, i+1);
	ad.put (i, i+2);
	ae.put (i, i+3);
	sprintf (str, "V%i", i);
	af.put (i, str);
	arr1.put(i,arrf);
	arr2.put(i,arrf);
	arr3.put(i,arrf);
	arrf += (float)(arrf.nelements());
    }
    ag1.putColumn (ad);

    // Sort the table.
    Table tabsort = tab.sort ("ac");
    tabsort.rename ("tForwardCol_tmp.sort0", Table::New);
}

void b (const TableDesc& td)
{
    Table tabsort ("tForwardCol_tmp.sort0", Table::Update);
    // Now use the description to make a table, which will use the
    // forwarding engine.
    SetupNewTable newtab1("tForwardCol_tmp.data1", td, Table::New);
    newtab1.setShapeColumn("arr1",IPosition(3,2,3,4));
    newtab1.setShapeColumn("arr3",IPosition(3,2,3,4));
    ForwardColumnEngine fce(tabsort);
    newtab1.bindAll (fce);
    Table forwTab(newtab1, 10);
}

void c (const TableDesc& tdin)
{
    Table tab("tForwardCol_tmp.data1", Table::Update);
    // Now use the description to make a table, which will use the
    // forwarding engine again.
    TableDesc td = tdin;
    td.removeColumn ("ac");
    SetupNewTable newtab1("tForwardCol_tmp.data2", td, Table::New);
    newtab1.setShapeColumn("arr1",IPosition(3,2,3,4));
    newtab1.setShapeColumn("arr3",IPosition(3,2,3,4));
    ForwardColumnEngine fce(tab, "ForwardEngine1");
    newtab1.bindAll (fce);
    StManAipsIO sm;
    newtab1.bindColumn("ab", sm);
    Table forwTab(newtab1, 10);

    // Update the values.
    // Column ac will be updated in the original table.
    ScalarColumn<Int> ab1(forwTab,"ab");
    uInt i;
    for (i=0; i<10; i++) {
	ab1.put (i, i+10);
    }
    forwTab.addColumn (ScalarColumnDesc<Int>("ac"), "ForwardEngine1", True);
    ScalarColumn<Int> ac (forwTab,"ac");
    for (i=0; i<10; i++) {
	ac.put (i, i+11);
    }

    // Now use the description to make a table, which will use the
    // forwarding engine again.
    SetupNewTable newtab2("tForwardCol_tmp.data3", tdin, Table::New);
    newtab2.setShapeColumn("arr1",IPosition(3,2,3,4));
    newtab2.setShapeColumn("arr3",IPosition(3,2,3,4));
    ForwardColumnEngine fce2(forwTab);
    newtab2.bindAll (fce2);
    Table forwTab2(newtab2, 10);
}

void check(const String& tableName, Int abOffset, Int acOffset)
{
    cout << "Checking table " << tableName << endl;
    // Read back the table and check the data.
    Table tab(tableName);
    ScalarColumn<Int> ab2(tab,"ab");
    ScalarColumn<Int> ac (tab,"ac");
    ScalarColumn<uInt> ad(tab,"ad");
    ScalarColumn<float> ae(tab,"ae");
    ScalarColumn<String> af(tab,"af");
    ScalarColumn<DComplex> ag(tab,"ag");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<float> arr3(tab,"arr3");
    Int i;
    Int abval, acval;
    uInt adval;
    float aeval;
    String afval;
    DComplex agval;
    char str[8];
    Cube<float> arrf(IPosition(3,2,3,4));
    Cube<float> arrval(IPosition(3,2,3,4));
    Cube<float> arrvalslice(arrval(Slice(0,1),Slice(0,1,2),Slice(0,2,2)));
    Slice tmp;
    Slicer nslice (tmp, tmp, tmp,  Slicer::endIsLength);
    Slicer nslice2(Slice(0,1), Slice(0,1,2), Slice(0,2,2),
		   Slicer::endIsLength);
    indgen (arrf);
    for (i=0; i<10; i++) {
	cout << "get scalar row " << i << endl;
	ab2.get (i, abval);
	ac.get (i, acval);
	ad.get (i, adval);
	ae.get (i, aeval);
	af.get (i, afval);
	ag.get (i, agval);
	sprintf (str, "V%i", i);
	if (abval != i+abOffset  ||  acval != i+1+acOffset
        ||  Int(adval) != i+2  ||  aeval != i+3
	||  afval != str  ||  agval != DComplex(i+2)) {
	    cout << "error in row " << i << ": " << abval
		 << ", " << acval << ", " << adval
		 << ", " << aeval << ", " << afval
		 << ", " << agval << endl;
	}
	arr1.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr1 in row " << i << endl;
	}
	arr2.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 in row " << i << endl;
	}
	cout << "get array row " << i << endl;
	arr3.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr3 in row " << i << endl;
	}
	arr2.getSlice (i, nslice, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 (entire slice) in row " << i << endl;
	}
	arr2.getSlice (i, nslice2, arrvalslice);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 (partial slice) in row " << i << endl;
	}
	arrf += (float)(arrf.nelements());
    }
    Vector<Int> abvec = ab2.getColumn();
    cout << tab.nrow() << " " << abvec.nelements() << endl;
    for (i=0; i<10; i++) {
	if (abvec(i) != i+abOffset) {
	    cout << "error in getColumn " << i << ": " << abvec(i) << endl;
	}
    }
    Array<float> arr1a = arr1.getColumn();
    if (arr1a.ndim() != 4) {
	cout << "arr1a not 4-dim" << endl;
    }
    i=0;
    uInt j0,j1,j2,j3;
    for (j3=0; j3<10; j3++)
	for (j2=0; j2<4; j2++)
	    for (j1=0; j1<3; j1++)
		for (j0=0; j0<2; j0++) {
		    if (arr1a(IPosition(4,j0,j1,j2,j3)) != i++) {
			cout <<"arr1a error at " <<j0<<" "<<j1<<" "<<j2<<" "
			    <<j3<<" should be: "<<i<<endl;
		    }
		}
    Array<float> arr1b = arr1.getColumn(nslice);
    if (arr1b.ndim() != 4) {
	cout << "arr1b not 4-dim" << endl;
    }
    i=0;
    for (j3=0; j3<10; j3++)
	for (j2=0; j2<4; j2++)
	    for (j1=0; j1<3; j1++)
		for (j0=0; j0<2; j0++) {
		    if (arr1b(IPosition(4,j0,j1,j2,j3)) != i++) {
			cout <<"arr1b error at " <<j0<<" "<<j1<<" "<<j2<<" "
			    <<j3<<" should be: "<<i<<endl;
		    }
		}

    i = 0;
    TableIterator iter (tab, "ad", TableIterator::Descending);
    while (! iter.pastEnd()) {
	if (iter.table().nrow() != 1) {
	    cout << "More than 1 row in TableIterator " << i << endl;
	}
	ScalarColumn<Int> ab (iter.table(), "ab");
	if (ab(0) != 9-i+abOffset) {
	    cout << "Invalid value " << ab(0) << " in TableIterator "
		 << i << endl;
	}
	iter.next();
	i++;
    }
}
