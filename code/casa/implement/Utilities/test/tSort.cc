//# tSort.cc: Test program for the Sort class
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <aips/Utilities/Sort.h>
#include <aips/Arrays/Vector.h>

// This program test the class Sort.
// It sorts some data in ascending and/or descending order.
// The results are written to stdout. A script executing this program,
// compares the output with a reference output file.

void sortit (int opt)
{
    Int arr[10], ar2[10];
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
	ard[i] = i/3;
	arts[i].ad = ard[i];
	arts[i].as = "abc";
    }
    arts[2].as = "ABC";
    arts[5].as = "xyzabc";

    Sort sort;
    sort.sortKey (arr,TpInt);                 // sort arr
    Vector<uInt> inxvec;
    uInt nr = sort.sort (inxvec,10,opt);      // get indices back in inxvec
    for (i=0; i<nr; i++) {
	cout << " " << arr[inxvec(i)];
    }
    cout << endl;

    Sort sort2;
    sort2.sortKey (arr,TpInt,0,Sort::Descending);
    nr = sort2.sort (inxvec,10,opt);          // same, but now descending
    for (i=0; i<nr; i++) {
	cout << " " << arr[inxvec(i)];
    }
    cout << endl;

    Sort sort3(ar2,sizeof(Int));
    sort3.sortKey (0,TpInt,Sort::Ascending);
    nr = sort3.sort (inxvec,10,opt);          // same, but now with original
    for (i=0; i<nr; i++) {                    // array in descending order
	cout << " " << ar2[inxvec(i)];
    }
    cout << endl;

    Sort sort4;
    sort4.sortKey (ar2,TpInt,0,Sort::Descending);
    nr = sort4.sort (inxvec,10,opt);
    for (i=0; i<nr; i++) {
	cout << " " << ar2[inxvec(i)];
    }
    cout << endl;

    Sort sort6(arr,sizeof(Int));
    sort6.sortKey (ard,TpDouble);
    sort6.sortKey (0,TpInt,Sort::Descending);
    nr = sort6.sort (inxvec,10,opt);          // sort on 2 keys
    for (i=0; i<nr; i++) {
	cout << " " << ard[inxvec(i)] << "," << arr[inxvec(i)];
    }
    cout << endl;

    Sort sort7(arts,sizeof(Ts));
    uInt distad = (char*)&arts[0].ad - (char*)arts;
    uInt distas = (char*)&arts[0].as - (char*)arts;
    sort7.sortKey (distad, TpDouble);
    sort7.sortKey (distas, TpString,Sort::Descending);
    nr = sort7.sort (inxvec,10,opt);          // sort a struct, where the data
    for (i=0; i<nr; i++) {                    // are combined in one record
	cout << " " << arts[inxvec(i)].ad << "," << arts[inxvec(i)].as;
    }
    cout << endl;
    nr = sort7.sort (inxvec,10,opt|Sort::NoDuplicates);     // unique keys
    for (i=0; i<nr; i++) {
	cout << " " << arts[inxvec(i)].ad << "," << arts[inxvec(i)].as;
    }
    cout << endl;
}

void sortdo (int options, Sort::Order order, Int* data, uInt nrdata)
{
    Sort sort;
    sort.sortKey (data, TpInt, 0, order);
    Vector<uInt> inxvec;
    uInt nr = sort.sort (inxvec, nrdata, options);
    uInt i;
    for (i=1; i<nr; i++) {
	if (order == Sort::Ascending) {
	    if (data[inxvec(i)] < data[inxvec(i-1)]) {
		cout << "Order error on index " << i << endl;
	    }
	}else{
	    if (data[inxvec(i)] > data[inxvec(i-1)]) {
		cout << "Order error on index " << i << endl;
	    }
	}
	if (data[inxvec(i)] == data[inxvec(i-1)]) {
	    if ((options & Sort::NoDuplicates) != 0) {
		cout << "Duplicate value on index" << i << endl;
	    }else{
		if (inxvec(i) < inxvec(i-1)) {
		    cout << "Equal order error on index " << i << endl;
		}
	    }
	}
    }
    Vector<uInt> uniqvec;
    nr = sort.unique (uniqvec, inxvec);
    for (i=1; i<nr; i++) {
	if (data[inxvec(uniqvec(i))] == data[inxvec(uniqvec(i-1))]) {
	    cout << "Non-unique value on index" << i << endl;
	}
    }
}

void sortall (int options, Sort::Order order)
{
    const uInt nrdata = 1000;
    Int data[nrdata];
    uInt i;
    for (i=0; i<nrdata; i++) {
	data[i] = i;
    }
    sortdo (options, order, data, nrdata);
    for (i=0; i<nrdata; i++) {
	data[i] = nrdata - i;
    }
    sortdo (options, order, data, nrdata);
    for (i=0; i<nrdata; i++) {
	data[i] = rand();
    }
    sortdo (options, order, data, nrdata);
    for (i=0; i<nrdata; i++) {
	data[i] = 1;
    }
    sortdo (options, order, data, nrdata);
    for (i=0; i<nrdata; i++) {
	data[i] = rand()%10;
    }
    sortdo (options, order, data, nrdata);
}


main()
{
    sortit (Sort::InsSort);
    sortit (Sort::QuickSort);
    sortit (Sort::HeapSort);

    // Sort a longer array and test its result.
    sortall (Sort::InsSort, Sort::Ascending);
    sortall (Sort::QuickSort, Sort::Ascending);
    sortall (Sort::HeapSort, Sort::Ascending);
    sortall (Sort::InsSort | Sort::NoDuplicates, Sort::Ascending);
    sortall (Sort::QuickSort | Sort::NoDuplicates, Sort::Ascending);
    sortall (Sort::HeapSort | Sort::NoDuplicates, Sort::Ascending);
    sortall (Sort::InsSort, Sort::Descending);
    sortall (Sort::QuickSort, Sort::Descending);
    sortall (Sort::HeapSort, Sort::Descending);
    sortall (Sort::InsSort | Sort::NoDuplicates, Sort::Descending);
    sortall (Sort::QuickSort | Sort::NoDuplicates, Sort::Descending);
    sortall (Sort::HeapSort | Sort::NoDuplicates, Sort::Descending);

    return 0;                              // exit with success status
}
