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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

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
void doiter1();
void doiter2();
void doiter3();

int main (int argc, const char* argv[])
{
    uInt nr = 5000;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    credes();          // make description
    cretab(nr);        // create table (and write it)
    doiter1();         // do single column iteration
    doiter2();         // do two column iteration
    doiter3();         // do interval iteration
    return 0;          // successfully executed
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
    Block<CountedPtr<BaseCompare> > compObj(1);
    CompareIntervalReal<Float> xx(10., 0.);
    CountedPtr<BaseCompare> xxx (new CompareIntervalReal<Float>(10., 0.));
    compObj[0] = xxx;
    Block<Int> orders(1);
    orders[0] = TableIterator::Ascending;
    TableIterator iter1(tab1, iv1, compObj, orders);
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
