//# tSort.cc: Test program for the Sort class
//# Copyright (C) 1994,1995,1996,1997
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

// This program test the class Sort.
// It sorts some data in ascending and/or descending order.
// The results are written to stdout. A script executing this program,
// compares the output with a reference output file.

void sortit (int opt)
{
    Int arr[10], ar2[10];
    Int i;
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
    sort.sortKey (arr,TpInt);              // sort arr
    uInt* ptr;
    ptr = 0;
    uInt nr = sort.sort (10,ptr,opt);      // get indices back in ptr
    for (i=0; i<nr; i++) {
	cout << " " << arr[ptr[i]];
    }
    cout << endl;

    Sort sort2;
    sort2.sortKey (arr,TpInt,0,Sort::Descending);
    nr = sort2.sort (10,ptr,opt);          // same, but now descending
    for (i=0; i<nr; i++) {
	cout << " " << arr[ptr[i]];
    }
    cout << endl;

    Sort sort3(ar2,sizeof(Int));
    sort3.sortKey (0,TpInt,Sort::Ascending);
    nr = sort3.sort (10,ptr,opt);          // same, but now with original
    for (i=0; i<nr; i++) {                 // array in descending order
	cout << " " << ar2[ptr[i]];
    }
    cout << endl;

    Sort sort4;
    sort4.sortKey (ar2,TpInt,0,Sort::Descending);
    nr = sort4.sort (10,ptr,opt);
    for (i=0; i<nr; i++) {
	cout << " " << ar2[ptr[i]];
    }
    cout << endl;

    Sort sort5;
    sort5.sortKey (ar2,TpInt,0,Sort::Ascending);
    uInt* ptr2 = 0;
    ptr[7] = ptr[9];
    nr = sort5.sort (8,ptr,ptr2,opt);      // sort the result of the
    for (i=0; i<nr; i++) {                 // previous sort
	cout << " " << ar2[ptr2[i]];
    }
    cout << endl;

    Sort sort6(arr,sizeof(Int));
    sort6.sortKey (ard,TpDouble);
    sort6.sortKey (0,TpInt,Sort::Descending);
    nr = sort6.sort (10,ptr,opt);          // sort on 2 keys
    for (i=0; i<nr; i++) {
	cout << " " << ard[ptr[i]] << "," << arr[ptr[i]];
    }
    cout << endl;

    Sort sort7(arts,sizeof(Ts));
    uInt distad = (char*)&arts[0].ad - (char*)arts;
    uInt distas = (char*)&arts[0].as - (char*)arts;
    sort7.sortKey (distad, TpDouble);
    sort7.sortKey (distas, TpString,Sort::Descending);
    nr = sort7.sort (10,ptr,opt);          // sort a struct, where the data
    for (i=0; i<nr; i++) {                 // are combined in one record
	cout << " " << arts[ptr[i]].ad << "," << arts[ptr[i]].as;
    }
    cout << endl;
    nr = sort7.sort (10,ptr,opt|Sort::NoDuplicates);     // unique keys
    for (i=0; i<nr; i++) {
	cout << " " << arts[ptr[i]].ad << "," << arts[ptr[i]].as;
    }
    cout << endl;
    delete [] ptr;
    delete [] ptr2;
}

void sortdo (int options, Sort::Order order, Int* data, uInt nrdata)
{
    Sort sort;
    sort.sortKey (data, TpInt, 0, order);
    uInt* inx = 0;
    uInt nr = sort.sort (nrdata, inx, options);
    for (uInt i=1; i<nr; i++) {
	if (order == Sort::Ascending) {
	    if (data[inx[i]] < data[inx[i-1]]) {
		cout << "Order error on index " << i << endl;
	    }
	}else{
	    if (data[inx[i]] > data[inx[i-1]]) {
		cout << "Order error on index " << i << endl;
	    }
	}
	if (data[inx[i]] == data[inx[i-1]]) {
	    if ((options & Sort::NoDuplicates) != 0) {
		cout << "Duplicate value on index" << i << endl;
	    }else{
		if (inx[i] < inx[i-1]) {
		    cout << "Equal order error on index " << i << endl;
		}
	    }
	}
    }
    delete [] inx;
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
