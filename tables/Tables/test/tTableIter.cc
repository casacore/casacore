//# tTableIter.cc: Test program for table iterators
//# Copyright (C) 1994,1995,1996,1999,2000,2001,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
// <summary> Test program for table iterators </summary>

// This program tests the table iterators..
// It creates a description and a table.
// It reads back the table, sorts it and selects rows.
// The data read is written to stdout to be checked.
// The standard output file tTableIter.out is checked in as a reference.
// The script tTableIter.run executes tTableIter and compares its output
// with tTableIter.out.

void credes();
void cretab(uInt);
void doiter0();
void doiter1();
void doiter2();
void doiter3();
void test_cache_boundaries();

int main (int argc, const char* argv[])
{
    uInt nr = 5000;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    credes();                // make description
    cretab(nr);              // create table (and write it)
    doiter0();               // do no column iteration
    doiter1();               // do single column iteration
    doiter2();               // do two column iteration
    doiter3();               // do interval iteration
    test_cache_boundaries(); // test option to cache group boundaries
    return 0;                // successfully executed
}

// Create the description of the table and its subtable.
// Define some defaults (as scalar and array) to test if they
// are filled in correctly when a row is added to a table.
void credes () {
    TableDesc txp("tTableIter_tmp", TableDesc::New);
    txp.addColumn (ScalarColumnDesc<Int> ("col1"));
    txp.addColumn (ScalarColumnDesc<double> ("col2"));
    txp.addColumn (ScalarColumnDesc<float> ("col3"));
    txp.addColumn (ScalarColumnDesc<Complex> ("col4"));
}

// Write data into the table.
void cretab(uInt nr) {
    Int i;
    SetupNewTable newtab ("tTableIter_tmp.data","tTableIter_tmp",Table::New);
    Table tab(newtab, nr);
    ScalarColumn<Int>     col1 (tab, "col1");
    ScalarColumn<double>  col2 (tab, "col2");
    ScalarColumn<float>   col3 (tab, "col3");
    ScalarColumn<Complex> col4 (tab, "col4");
    for (i=0; i<Int(nr); i++) {
	col1.put (i, (100000+i) % 10);
	col2.put (i, (i+1) % 105);
	col3.put (i, (i+2) % 75);
	col4.put (i, i+3);
    }
    cout << "Filling done" << endl;
}

void doiter0()
{
    Table t;
    Table tab ("tTableIter_tmp.data");
    Block<String> iv0;
    TableIterator iter0(tab, iv0);
    Int nr = 0;
    while (!iter0.pastEnd()) {
	t = iter0.table();
        AlwaysAssertExit (t.nrow() == tab.nrow());
	nr++;
	iter0.next();
    }
    cout << "   #iter=" << nr << endl;

    iter0.reset();
    nr = 0;
    while (!iter0.pastEnd()) {
	t = iter0.table();
        AlwaysAssertExit (t.nrow() == tab.nrow());
	nr++;
	iter0.next();
    }
    cout << "   #iter1=" << nr << endl;
}

void doiter1()
{
    Table t;
    Table tab ("tTableIter_tmp.data");
    Block<String> iv0(1);
    iv0[0] = "col1";
    TableIterator iter0(tab, iv0);
    Int nr = 0;
    while (!iter0.pastEnd()) {
	t = iter0.table();
	ScalarColumn<Int> col1(t, "col1");
	Vector<Int> vec;
	col1.getColumn (vec);
	cout << t.nrow() << " ";
	if (!allEQ(vec, nr)) {
	    cout << "error in iter. " << nr << " " << vec(0)
		 << " " << vec(vec.nelements()-1) << " " << vec.nelements() << endl;
	}
	cout << "(" << iter0.keyChangeAtLastNext() << ") ";
	if ( (nr<9 && iter0.keyChangeAtLastNext()!=iv0[0]) ||
	     (nr==9 && iter0.keyChangeAtLastNext()!="")  ) {     // last iter is empty string
	  cout << "error in TableIter::keyChangeAtLastNext() " << iter0.keyChangeAtLastNext() << endl;
	}
	nr++;
	iter0.next();
    }
    cout << "   #iter=" << nr << endl;

    iter0.reset();
    nr = 0;
    while (!iter0.pastEnd()) {
	t = iter0.table();
	ScalarColumn<Int> col1(t, "col1");
	Vector<Int> vec;
	col1.getColumn (vec);
	cout << t.nrow() << " ";
	if (!allEQ(vec, nr)) {
	    cout << "error in iter. " << nr << " " << vec(0)
		 << " " << vec(vec.nelements()-1) << " " << vec.nelements() << endl;
	}
	nr++;
	iter0.next();
    }
    cout << "   #iter1=" << nr << endl;
}

void doiter2()
{
    Table tab1 ("tTableIter_tmp.data", Table::Update);
    Table t1;
    Block<String> iv1(2);
    iv1[0] = "col2";
    iv1[1] = "col1";
    TableIterator iter1(tab1, iv1);
    Int nr = 0;
    Int l1 = -1;
    double l2 = -1;
    while (!iter1.pastEnd()) {
	t1 = iter1.table();
	ScalarColumn<Int> col1(t1, "col1");
	ScalarColumn<double> col2(t1, "col2");
	Vector<Int> vec1;
	col1.getColumn (vec1);
	Vector<double> vec2;
	col2.getColumn (vec2);
	if (!(allEQ(vec1, vec1(0))  &&  allEQ(vec2, vec2(0)))) {
	    cout << "error in iter. " << nr << " " << vec1(0) << " "
		 << vec1(vec1.nelements()-1) << " " << vec2(0) << " "
		 << vec2(vec2.nelements()-1) << endl;
	}
	if (vec2(0) < l2  ||  (vec2(0) == l2  &&  vec1(0) <= l1)) {
	    cout << "order error " << vec2(0) << " " << l2 << ", "
		 << vec1(0) << " " << l1 << endl;
	}
	l1 = vec1(0);
	l2 = vec2(0);
	nr++;
	iter1.next();
    }
    cout << "   #iter2=" << nr << endl;
}

void doiter3()
{
    Table tab1 ("tTableIter_tmp.data");
    Table t1;
    Block<String> iv1(1);
    iv1[0] = "col3";
    Block<std::shared_ptr<BaseCompare>> compObj(1);
    compObj[0].reset (new CompareIntervalReal<Float>(10., 0.));
    Block<Int> orders(1);
    orders[0] = TableIterator::Ascending;
    TableIterator iter1(tab1, iv1, compObj, orders);

    TableIterator iter2;
    iter2 = iter1;
    AlwaysAssertExit(iter2.table().nrow() == iter1.table().nrow());

    // Test that copied new iterator gives the same number of rows
    int iter1count = 0;
    while(!iter1.pastEnd()) { iter1.next(); iter1count++; }
    int iter2count = 0;
    while(!iter2.pastEnd()) { iter2.next(); iter2count++; }
    AlwaysAssertExit(iter1count == iter2count);
    iter1.reset();
    iter2.reset();

    // Test that copied new iterator does not copy the state
    iter1.next();
    iter1.next(); // Advance iter1 by two, so state changes
    iter1count = 0;
    iter2 = iter1; // iter2 should be 'clean' and iterate 2 more than iter1
    TableIterator iter3 = iter1;
    iter3.copyState(iter1); // iter3 should iterate as much as iter1
    while(!iter1.pastEnd()) { iter1.next(); iter1count++; }
    iter2count = 0;
    while(!iter2.pastEnd()) { iter2.next(); iter2count++; }
    AlwaysAssertExit(iter2count == iter1count + 2);
    int iter3count = 0;
    while(!iter3.pastEnd()) { iter3.next(); iter3count++; }
    AlwaysAssertExit(iter3count == iter1count);

    iter1.reset();

    Int nr = 0;
    float l3 = -1;
    while (!iter1.pastEnd()) {
	t1 = iter1.table();
        cout << t1.nrow() << " ";
	ScalarColumn<Float> col3(t1, "col3");
	Vector<Float> vec3;
        col3.getColumn (vec3);
        if (max(vec3) - min(vec3) > 9.5) {
          cout << "Interval order error" << endl;
        }
	if (vec3(0) < l3) {
          cout << "order error " << vec3(0) << " " << l3 << endl;
	}
	l3 = vec3(0);
	nr++;
	iter1.next();
    }
    cout << "   #iter3=" << nr << endl;
}

void test_cache_boundaries()
{
    // Create two iterators, one that cached the boundaries between iterations
    // and one that does not.
    Table tab1 ("tTableIter_tmp.data");
    Table t1;
    Block<String> sortCols(2);
    sortCols[0] = "col2";
    sortCols[1] = "col1";
    Block<std::shared_ptr<BaseCompare>> compObj(2);   // use default compares
    Block<Int> orders(2);
    orders[0] = TableIterator::Ascending;
    orders[1] = TableIterator::Ascending;
    TableIterator iter1(tab1, sortCols, compObj, orders);
    TableIterator iter2(tab1, sortCols, compObj, orders, TableIterator::ParSort, true);

    iter1.reset();
    iter2.reset();

    while (!iter1.pastEnd()) {
        auto iter1Table = iter1.table();
        auto iter2Table = iter2.table();
        AlwaysAssertExit(iter2Table.nrow() == iter1Table.nrow());
        ScalarColumn<Int>     iter1Col1 (iter1Table, "col1");
        ScalarColumn<double>  iter1Col2 (iter1Table, "col2");
        ScalarColumn<float>   iter1Col3 (iter1Table, "col3");
        ScalarColumn<Complex> iter1Col4 (iter1Table, "col4");
        ScalarColumn<Int>     iter2Col1 (iter2Table, "col1");
        ScalarColumn<double>  iter2Col2 (iter2Table, "col2");
        ScalarColumn<float>   iter2Col3 (iter2Table, "col3");
        ScalarColumn<Complex> iter2Col4 (iter2Table, "col4");
        Vector<int>     iter1Vec1;
        Vector<double>  iter1Vec2;
        Vector<float>   iter1Vec3;
        Vector<Complex> iter1Vec4;
        Vector<int>     iter2Vec1;
        Vector<double>  iter2Vec2;
        Vector<float>   iter2Vec3;
        Vector<Complex> iter2Vec4;
        iter1Col1.getColumn (iter1Vec1);
        iter1Col2.getColumn (iter1Vec2);
        iter1Col3.getColumn (iter1Vec3);
        iter1Col4.getColumn (iter1Vec4);
        iter2Col1.getColumn (iter2Vec1);
        iter2Col2.getColumn (iter2Vec2);
        iter2Col3.getColumn (iter2Vec3);
        iter2Col4.getColumn (iter2Vec4);
        // Check that all the columns for this iteration have the same 
        // content in both iterators.
        AlwaysAssertExit(allEQ(iter1Vec1, iter2Vec1));
        AlwaysAssertExit(allEQ(iter1Vec2, iter2Vec2));
        AlwaysAssertExit(allEQ(iter1Vec3, iter2Vec3));
        AlwaysAssertExit(allEQ(iter1Vec4, iter2Vec4));
        // Check that the changing key is the same in both iterators.
        AlwaysAssertExit(iter1.keyChangeAtLastNext() == iter2.keyChangeAtLastNext());
        iter1.next();
        iter2.next();
    }
}
