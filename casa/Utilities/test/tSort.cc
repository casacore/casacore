//# tSort.cc: Test program for the Sort class
//# Copyright (C) 1994,1995,1996,1997,1998,2001,2003
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

//# Includes

#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// This program test the class Sort.
// It sorts some data in ascending and/or descending order.
// The results are written to stdout. A script executing this program,
// compares the output with a reference output file.

void sortit (int opt)
{
    Int arr[10];
    Int64 ar2[10];
    Int ar3[10];
    uInt i;
    double ard[10];
    struct Ts {
	double ad;
	String as;
    };
    Ts arts[10];
    for (i=0; i<10; i++) {
	arr[i] = i;
	ar2[i] = 10-i;
        ar3[i] = 19-i;
	ard[i] = i/3;
	arts[i].ad = ard[i];
	arts[i].as = "abc";
    }
    // Note: ar3 is a specific test if 'last' in ParSort's merge works fine.
    ar3[9] = 20;
    arts[2].as = "ABC";
    arts[5].as = "xyzabc";

    Sort sort;
    sort.sortKey (arr,TpInt);                  // sort arr
    Vector<uInt> inxvec;
    uInt nr = sort.sort (inxvec,10,opt,False); // get indices back in inxvec
    for (i=0; i<nr; i++) {
	cout << " " << arr[inxvec(i)];
    }
    cout << endl;

    // Also test assignment.
    Sort sort2;
    sort2.sortKey (arr,TpInt,0,Sort::Descending);
    sort = sort2;
    nr = sort.sort (inxvec,10,opt,False);      // same, but now descending
    for (i=0; i<nr; i++) {
	cout << " " << arr[inxvec(i)];
    }
    cout << endl;

    // Also test copy constructor.
    Sort sort3(ar2,sizeof(Int64));
    sort3.sortKey (0,TpInt64,Sort::Ascending);
    Sort sort3a(sort3);
    nr = sort3a.sort (inxvec,10,opt,False);    // same, but now with original
    for (i=0; i<nr; i++) {                     // array in descending order
	cout << " " << ar2[inxvec(i)];
    }
    cout << endl;

    Sort sort4;
    sort4.sortKey (ar2,TpInt64,0,Sort::Descending);
    nr = sort4.sort (inxvec,10,opt,False);
    for (i=0; i<nr; i++) {
	cout << " " << ar2[inxvec(i)];
    }
    cout << endl;

    Sort sort5;
    sort5.sortKey (ar3,TpInt,0,Sort::Ascending);
    nr = sort5.sort (inxvec,10,opt,False);
    for (i=0; i<nr; i++) {
	cout << " " << ar3[inxvec(i)];
    }
    cout << endl;

    Sort sort6(arr,sizeof(Int));
    sort6.sortKey (ard,TpDouble);
    sort6.sortKey (0,TpInt,Sort::Descending);
    nr = sort6.sort (inxvec,10,opt,False);     // sort on 2 keys
    for (i=0; i<nr; i++) {
	cout << " " << ard[inxvec(i)] << "," << arr[inxvec(i)];
    }
    cout << endl;

    Sort sort7(arts,sizeof(Ts));
    uInt distad = (char*)&arts[0].ad - (char*)arts;
    uInt distas = (char*)&arts[0].as - (char*)arts;
    sort7.sortKey (distad, TpDouble);
    sort7.sortKey (distas, TpString,Sort::Descending);
    nr = sort7.sort (inxvec,10,opt,False);     // sort a struct, where the data
    for (i=0; i<nr; i++) {                     // are combined in one record
	cout << " " << arts[inxvec(i)].ad << "," << arts[inxvec(i)].as;
    }
    cout << endl;
    nr = sort7.sort (inxvec,10,opt|Sort::NoDuplicates,False); // unique keys
    for (i=0; i<nr; i++) {
	cout << " " << arts[inxvec(i)].ad << "," << arts[inxvec(i)].as;
    }
    cout << endl;
}

void sortdo (int options, Sort& sort, Sort::Order order,
	     Int* data, uInt nrdata)
{
    Vector<uInt> inxvec;
    uInt nr = sort.sort (inxvec, nrdata, options);
    uInt i;
    for (i=1; i<nr; i++) {
	if (order == Sort::Ascending) {
	    if (data[inxvec(i)] < data[inxvec(i-1)]) {
		cout << "Asc order error on index " << i << endl;
	    }
	}else{
	    if (data[inxvec(i)] > data[inxvec(i-1)]) {
		cout << "Desc order error on index " << i << endl;
	    }
	}
	if (data[inxvec(i)] == data[inxvec(i-1)]) {
	    if ((options & Sort::NoDuplicates) != 0) {
		cout << "Duplicate value on index" << i << endl;
	    }else{
              if (order == Sort::Ascending) {
		if (inxvec(i) < inxvec(i-1)) {
		    cout << "Asc equal order error on index " << i << endl;
		}
              }else{
		if (inxvec(i) > inxvec(i-1)) {
		    cout << "Desc equal order error on index " << i << endl;
		}
              }
	    }
	}
    }
    if ((options & Sort::NoDuplicates) == 0) {
        AlwaysAssertExit (nr == nrdata);
        AlwaysAssertExit (nr == inxvec.nelements());
    } else {
        Vector<uInt> inxvec2;
	sort.sort (inxvec2, nrdata, Sort::QuickSort);
	Vector<uInt> uniqvec;
	uInt nr2 = sort.unique (uniqvec, inxvec2);
        AlwaysAssertExit (nr2 == nr);
        AlwaysAssertExit (nr2 == uniqvec.nelements());
	for (i=0; i<nr2; i++) {
	  if (data[inxvec(i)] != data[inxvec2(uniqvec(i))]) {
	    cout << "Non matching value on index" << i << endl;
	  }
	  if (i > 0) {
	    if (data[inxvec2(uniqvec(i))] == data[inxvec2(uniqvec(i-1))]) {
	      cout << "Non-unique value on index" << i << endl;
	    }
	  }
	}
    }
}

// Test with 1 and 2 keys, because 1 key is short-circuited to GenSort.
void sortall (int options, Sort::Order order)
{
    const uInt nrdata = 10;
    Int data[nrdata];
    Int data2[nrdata];
    Sort sort;
    sort.sortKey (data, TpInt, 0, order);
    Sort sort2;
    sort2.sortKey (data, TpInt, 0, order);
    sort2.sortKey (data2, TpInt, 0, order);
    for (uInt i=0; i<nrdata; i++) {
      data[i] = i;
      data2[i] = 0;
    }
    sortdo (options, sort, order, data, nrdata);
    sortdo (options, sort2, order, data, nrdata);

    for (uInt i=0; i<nrdata; i++) {
      data[i] = nrdata - i;
    }
    sortdo (options, sort, order, data, nrdata);
    sortdo (options, sort2,order,  data, nrdata);

    for (uInt i=0; i<nrdata; i++) {
      data[i] = rand();
    }
    sortdo (options, sort, order, data, nrdata);
    sortdo (options, sort2, order, data, nrdata);

    for (uInt i=0; i<nrdata; i++) {
      data[i] = 1;
    }
    sortdo (options, sort, order, data, nrdata);
    sortdo (options, sort2, order, data, nrdata);

    for (uInt i=0; i<nrdata; i++) {
      data[i] = rand()%10;
    }
    sortdo (options, sort, order, data, nrdata);
    sortdo (options, sort2, order, data, nrdata);
}


int main()
{
    sortit (Sort::InsSort);
    sortit (Sort::ParSort);
    sortit (Sort::QuickSort);
    sortit (Sort::HeapSort);

    // Sort a longer array and check its result.
    sortall (Sort::InsSort, Sort::Ascending);
    sortall (Sort::ParSort, Sort::Ascending);
    sortall (Sort::QuickSort, Sort::Ascending);
    sortall (Sort::HeapSort, Sort::Ascending);
    sortall (Sort::InsSort | Sort::NoDuplicates, Sort::Ascending);
    sortall (Sort::ParSort | Sort::NoDuplicates, Sort::Ascending);
    sortall (Sort::QuickSort | Sort::NoDuplicates, Sort::Ascending);
    sortall (Sort::HeapSort | Sort::NoDuplicates, Sort::Ascending);
    sortall (Sort::InsSort, Sort::Descending);
    sortall (Sort::ParSort, Sort::Descending);
    sortall (Sort::QuickSort, Sort::Descending);
    sortall (Sort::HeapSort, Sort::Descending);
    sortall (Sort::InsSort | Sort::NoDuplicates, Sort::Descending);
    sortall (Sort::ParSort | Sort::NoDuplicates, Sort::Descending);
    sortall (Sort::QuickSort | Sort::NoDuplicates, Sort::Descending);
    sortall (Sort::HeapSort | Sort::NoDuplicates, Sort::Descending);

    return 0;                              // exit with success status
}
