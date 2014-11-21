//# tVirtualTaQLColumn.cc: Test program for class VirtualTaQLColumn
//# Copyright (C) 2005
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
#include <casacore/tables/DataMan/VirtualTaQLColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/TaQL/TableParse.h>
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

// <summary> Test program for class VirtualTaQLColumn </summary>

// This program tests the virtual column engine VirtualTaQLColumn.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

TableDesc makeDesc();
void a (const TableDesc&);
void check(const Table& table, Bool showname);
void testSelect();

int main ()
{
    try {
      {
	TableDesc td = makeDesc();
	a (td);
	Table table("tVirtualTaQLColumn_tmp.data0");
	check(table, True);
	table.deepCopy ("tVirtualTaQLColumn_tmp.data1", Table::New, True);
	check (Table("tVirtualTaQLColumn_tmp.data1"), True);
	Table tab2 = table.copyToMemoryTable ("tVirtualTaQLColumn_tmp.data2");
	check (tab2, True);
      }
      testSelect();
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
    VirtualTaQLColumn::registerClass();

    // Build the table description.
    TableDesc td("tTableDesc","1",TableDesc::Scratch);
    td.comment() = "A test of class tVirtualTaQLColumn";
    td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<Int>("ac"));
    td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
    td.addColumn (ScalarColumnDesc<float>("ae"));
    td.addColumn (ScalarColumnDesc<String>("af"));
    td.addColumn (ScalarColumnDesc<DComplex>("ag"));
    td.addColumn (ScalarColumnDesc<float>("acalc"));
    td.addColumn (ScalarColumnDesc<Complex>("acalc2"));
    td.addColumn (ScalarColumnDesc<short>("acalc3"));
    td.addColumn (ArrayColumnDesc<float>("arr1",3,ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<float>("arr2",0));
    td.addColumn (ArrayColumnDesc<float>("arr3",0,ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<float>("arrcalc",0));
    return td;
}

void a (const TableDesc& td)
{
    // Now create a new table from the description.
    SetupNewTable newtab("tVirtualTaQLColumn_tmp.data0", td, Table::New);
    newtab.setShapeColumn("arr1",IPosition(3,2,3,4));
    newtab.setShapeColumn("arr3",IPosition(3,2,3,4));
    VirtualTaQLColumn vtc("ab+10.");
    VirtualTaQLColumn vtc2("ag+max(arr3)");
    VirtualTaQLColumn vtc3("ab*ac");
    VirtualTaQLColumn vtac("ab*arr3");
    newtab.bindColumn ("acalc", vtc);
    newtab.bindColumn ("acalc2", vtc2);
    newtab.bindColumn ("acalc3", vtc3);
    newtab.bindColumn ("arrcalc", vtac);
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
    VirtualTaQLColumn vtcm("acalc+acalc3+mean(arrcalc)");
    tab.addColumn (ScalarColumnDesc<Float>("acalc4"), vtcm);
}

void check(const Table& tab, Bool showname)
{
    if (!showname) cout << ">>>" << endl;
    cout << "Checking table " << tab.tableName() << endl;
    if (!showname) cout << "<<<" << endl;
    ScalarColumn<Int> ab2(tab,"ab");
    ScalarColumn<Int> ac (tab,"ac");
    ScalarColumn<uInt> ad(tab,"ad");
    ScalarColumn<float> ae(tab,"ae");
    ScalarColumn<String> af(tab,"af");
    ScalarColumn<DComplex> ag(tab,"ag");
    ScalarColumn<float> acalc(tab,"acalc");
    ScalarColumn<Complex> acalc2(tab,"acalc2");
    ScalarColumn<short> acalc3(tab,"acalc3");
    ScalarColumn<float> acalc4(tab,"acalc4");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<float> arr3(tab,"arr3");
    ArrayColumn<float> arrcalc(tab,"arrcalc");
    Int i;
    Short acalc3val;
    Int abval, acval;
    uInt adval;
    float aeval, acalcval, acalc4val;
    String afval;
    DComplex agval;
    Complex acalc2val;
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
	ab2.get (i, abval);
	ac.get (i, acval);
	ad.get (i, adval);
	ae.get (i, aeval);
	af.get (i, afval);
	ag.get (i, agval);
	acalc.get (i, acalcval);
	acalc2.get (i, acalc2val);
	acalc3.get (i, acalc3val);
	acalc4.get (i, acalc4val);
	sprintf (str, "V%i", i);
	if (abval != i  ||  acval != i+1
        ||  Int(adval) != i+2  ||  aeval != i+3
	||  afval != str  ||  agval != DComplex(i+2)
	||  acalcval != abval+10  ||  acalc3val != abval*acval) {
	    cout << "error in row " << i << ": " << abval
		 << ", " << acval << ", " << adval
		 << ", " << aeval << ", " << afval
		 << ", " << agval << ", " << acalcval
		 << ", " << acalc3val << endl;
	}
	arr1.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr1 in row " << i << endl;
	}
	arr2.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 in row " << i << endl;
	}
	arr3.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr3 in row " << i << endl;
	}
	if (acalc2val != Complex(agval) + max(arrval)) {
	  cout << "error in acalc2val in row " << i << ": "
	       << acalc2val << endl;
	}
	arrcalc.get (i, arrval);
	if (!allEQ (arrval, float(abval)*arrf)) {
	    cout << "error in arrcalc in row " << i << endl;
	}
	if (acalc4val != acalcval + acalc3val + mean(arrval)) {
	  cout << "error in acalc4val in row " << i << ": "
	       << acalc4val << endl;
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
    for (i=0; i<10; i++) {
	if (abvec(i) != i) {
	    cout << "error in ab getColumn " << i << ": " << abvec(i) << endl;
	}
    }
    Vector<Short> acalc3vec = acalc3.getColumn();
    for (i=0; i<10; i++) {
	if (acalc3vec(i) != i*(i+1)) {
	    cout << "error in acalc3 getColumn " << i << ": "
		 << acalc3vec(i) << endl;
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

    {
      Int i = 0;
      TableIterator iter (tab, "ad", TableIterator::Descending);
      while (! iter.pastEnd()) {
	if (iter.table().nrow() != 1) {
	    cout << "More than 1 row in ad TableIterator " << i << endl;
	}
	ScalarColumn<Int> ab (iter.table(), "ab");
	if (ab(0) != 9-i) {
	    cout << "Invalid value " << ab(0) << " in ad TableIterator "
		 << i << endl;
	}
	iter.next();
	i++;
      }
    }
    {
      Int i = 0;
      TableIterator iter (tab, "acalc", TableIterator::Descending);
      while (! iter.pastEnd()) {
	if (iter.table().nrow() != 1) {
	    cout << "More than 1 row in acalc TableIterator " << i << endl;
	}
	ScalarColumn<Int> ab (iter.table(), "ab");
	if (ab(0) != 9-i) {
	    cout << "Invalid value " << ab(0) << " in acalc TableIterator "
		 << i << endl;
	}
	iter.next();
	i++;
      }
    }
}

// Test if a tableCommand on a table containing a TaQL column works fine.
// Note it requires recursive parsing.
void testSelect()
{
  // Select all rows.
  Table subset = tableCommand("select from tVirtualTaQLColumn_tmp.data0 "
			      "where acalc > -1000");
  check (subset, False);
}
