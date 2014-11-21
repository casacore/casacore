//# tSort_1.cc: This program tests the performance of the Sort class
//# Copyright (C) 1993,1994,1995,1996,1997,2001,2003
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
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/stdlib.h>

#include <casacore/casa/namespace.h>
//# Forward Declarations
Bool sortarr (Int*, uInt nr, int);
Bool sortall (Int*, uInt nr, uInt type);
Bool sort2 (uInt nr);

// Define file global variable for cmp-routine.
static Int* gbla;


// This program tests the speed of the Sort class .
// It sorts some data in ascending and/or descending order.
// The timing results are written to stdout.

int main(int argc, const char* argv[])
{
    Bool success = True;
    uInt nr=5000;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    cout << nr << " elements" << endl;
    Int* a1 = new Int[nr];
    Int* a2 = new Int[nr];
    Int* a3 = new Int[nr];
    Int* a4 = new Int[nr];
    Int* a5 = new Int[nr];
    Int* a6 = new Int[nr];
    Int* a7 = new Int[nr];
    if (a1==0 || a2==0 || a3==0 || a4==0 || a5==0 || a6==0 || a7==0) {
	cout << "Allocation error" << endl;
    }
    for (uInt i=0; i<nr; i++) {
	a1[i] = i;
	a2[i] = nr-i;
	a3[i] = rand();
	a4[i] = 1;
	a5[i] = rand()%2;
	a6[i] = rand()%5;
	a7[i] = rand()%10;
    }
    cout << "Sorting ordered array" << endl;
    if (! sortall (a1, nr, 0)) {
	success = False;
    }
    cout << "Sorting reversed array" << endl;
    if (! sortall (a2, nr, 0)) {
	success = False;
    }
    cout << "Sorting random array" << endl;
    if (! sortall (a3, nr, 0)) {
	success = False;
    }
    cout << "Sorting equal array" << endl;
    if (! sortall (a4, nr, 0)) {
	success = False;
    }
    cout << "Sorting random array with 2 different elements" << endl;
    if (! sortall (a5, nr, 2)) {
	success = False;
    }
    cout << "Sorting random array with 5 different elements" << endl;
    if (! sortall (a6, nr, 5)) {
	success = False;
    }
    cout << "Sorting random array with 10 different elements" << endl;
    if (! sortall (a7, nr, 10)) {
	success = False;
    }
    delete [] a1;
    delete [] a2;
    delete [] a3;
    delete [] a4;
    delete [] a5;
    delete [] a6;
    delete [] a7;

    sort2 (nr);

    if (success) {
	return 0;
    }
    return 1;
}

// Comparison routine for UNIX qsort.
int cmp(const void* i, const void * j)
     { return ObjCompare<Int>::compare (&gbla[*(uInt*)i],&gbla[*(uInt*)j]);}

void qksort (Int nr, uInt* inx)
{
    if (nr <= 1) {
	return;
    }
    // According to Sedgewick it is best to use a random partition element
    // to avoid degenerated cases (if the data is already in order for example)
    // rand is not a particularly good random number generator, but good
    // enough for this purpose.
    // Put this element at the beginning of the array.
    Int p = rand() % nr;
    uInt sav = inx[0];
    inx[0] = inx[p];
    inx[p] = sav;
    // Now shift all elements < partition-element to the left.
    // If an element is equal, shift every other element to avoid
    // degeneration. This trick is described by Jon Bentley in
    // UNIX Review, October 1992.
    Int j = 0;
    int cm, sw=0;
    for (Int i=1; i<nr; i++) {
	cm = cmp (inx, &inx[i]);
	if (cm > 0  ||  (cm == 0  &&  (sw = !sw))) {
	    sav = inx[i];
	    inx[i] = inx[++j];
	    inx[j] = sav;
	}
    }
    sav = inx[0];
    inx[0] = inx[j];
    inx[j] = sav;
    qksort (j, inx);
    qksort (nr-j-1, inx+j+1);
}

Bool sortall (Int* arr, uInt nr, uInt type)
{
    Bool success = True;
    if (nr <= 5000) {
	cout << "InsSort   ";
	if (! sortarr (arr, nr, Sort::InsSort)) {
	    success = False;
	}
    }
    cout << "ParSort   ";
    if (! sortarr (arr, nr, Sort::ParSort)) {
	success = False;
    }
    cout << "QuickSort ";
    if (! sortarr (arr, nr, Sort::QuickSort)) {
	success = False;
    }
    cout << "HeapSort  ";
    if (! sortarr (arr, nr, Sort::HeapSort)) {
	success = False;
    }
    Timer tim;
    uInt i;
    if (type==0 || (type==2 && nr<=10000) || (type==5 && nr<=20000)
    || (type==10 && nr<=100000)) {
	cout << "qsort     ";
	uInt* inx = new uInt[nr];
	if (inx == 0) {
	    cout << "Allocation Error" << endl;
	    return False;
	}
	for (i=0; i<nr; i++) {
	    inx[i] = i;
	}
	gbla = arr;               // make pointer global for cmp routine
	qsort ((char*)inx, nr, sizeof(uInt), cmp);
	tim.show();
	for (i=1; i<nr; i++) {
	    if (arr[inx[i]] < arr[inx[i-1]]) {
		cout << "Out of order";
		success = False;
		break;
	    }
	}
	delete [] inx;
    }
    cout << "qksort    ";
    tim.mark();
    uInt* inx = new uInt[nr];
    if (inx == 0) {
	cout << "Allocation Error" << endl;
	return False;
    }
    for (i=0; i<nr; i++) {
	inx[i] = i;
    }
    gbla = arr;               // make pointer global for cmp routine
    qksort (nr, inx);
    tim.show();
    for (i=1; i<nr; i++) {
	if (arr[inx[i]] < arr[inx[i-1]]) {
	    cout << "Out of order";
	    success = False;
	    break;
	}
    }
    delete [] inx;
    if (type==0 || (type==2 && nr<=10000) || (type==5 && nr<=20000)
    || (type==10 && nr<=100000)) {
	cout << "UNIX qsort";
	tim.mark();
	qsort ((char*)arr, nr, sizeof(Int), ObjCompare<Int>::compare);
	tim.show();
    }
    return success;
}

Bool sortarr1 (Int* arr, uInt nr, int opt)
{
    Bool success = True;
    Sort sort;
    sort.sortKey (arr,TpInt);
    Vector<uInt> ptr;
    Timer tim;
    sort.sort (ptr,nr,opt,False);
    tim.show();
    for (uInt i=1; i<nr; i++) {
	if (arr[ptr(i)] < arr[ptr(i-1)]) {
	    cout << "Out of order " <<arr[ptr(i)] << "," <<arr[ptr(i-1)]<<endl;
	    success = False;
	    break;
	}
	if (arr[ptr(i)] == arr[ptr(i-1)]  &&  ptr(i) <= ptr(i-1)) {
	    cout << "not stable " << ptr(i) << "<=" << ptr(i-1) << endl;
	    success = False;
	    break;
	}
    }
    return success;
}

Bool sortarr2 (Int* arr, uInt nr, int opt)
{
    Bool success = True;
    Sort sort;
    sort.sortKey (arr, CountedPtr<BaseCompare>(new ObjCompare<Int>), 4);
    Vector<uInt> ptr;
    Timer tim;
    sort.sort (ptr,nr,opt);
    ///sort.sort (ptr,nr,opt,False);
    tim.show("  with obj");
    for (uInt i=1; i<nr; i++) {
	if (arr[ptr(i)] < arr[ptr(i-1)]) {
	    cout << "Out of order " <<arr[ptr(i)] << "," <<arr[ptr(i-1)]<<endl;
	    success = False;
	    break;
	}
	if (arr[ptr(i)] == arr[ptr(i-1)]  &&  ptr(i) <= ptr(i-1)) {
	    cout << "not stable " << ptr(i) << "<=" << ptr(i-1) << endl;
	    success = False;
	    break;
	}
    }
    return success;
}

Bool sortarr (Int* arr, uInt nr, int opt)
{
  //    return sortarr1(arr,nr,opt) && sortarr2(arr,nr,opt);
    return sortarr2(arr,nr,opt);
}

// Sort two arrays using Sort or in a Combined way.
// It resembles sorting on baselines.
Bool sort2 (uInt nr)
{
  uInt nrbl = 45*46/2;
  uInt nrt = (nr+nrbl-1)/nrbl;
  Vector<Int> vec1(nrt*nrbl);
  Vector<Int> vec2(nrt*nrbl);
  uInt inx = 0;
  for (uInt i=0; i<nrt; ++i) {
    for (Int a1=0; a1<45; ++a1) {
      for (Int a2=0; a2<=a1; ++a2) {
        vec1[inx] = a1;
        vec2[inx] = a2;
        ++inx;
      }
    }
  }
  {
    Timer timer;
    Sort sort;
    sort.sortKey (vec1.data(), TpInt);
    sort.sortKey (vec2.data(), TpInt);
    Vector<uInt> inx;
    sort.sort (inx, vec1.size(), Sort::QuickSort);
    cout << "quicksort2";
    timer.show();
    timer.mark();
    Vector<uInt> inx1;
    sort.sort (inx1, vec1.size(), Sort::ParSort);
    cout << "parsort2  ";
    timer.show();
  }
  {
    Timer timer;
    Int nrant = 1 + max(max(vec1), max(vec2));
    Vector<Int> bl(vec1*nrant);
    bl += vec2;
    cout << "  fill    ";
    timer.show();
    Vector<uInt> inx;
    GenSortIndirect<Int>::sort (inx, bl);
    cout << "indsort   ";
    timer.show();
  }
  return True;
}
