//# tGenSort.cc: This program tests the global templated sort routines
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2001,2003
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

#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/iomanip.h>
#include <algorithm>

#include <casacore/casa/namespace.h>

void sortall (Int*, uInt, int, Sort::Order, Bool);

uInt doSort (Vector<uInt>& inx, const Int* arr, uInt nr,
             Sort::Order ord, int type)
{
  inx.resize (nr);
  indgen(inx);
  if ((type & Sort::QuickSort) != 0) {
    return GenSortIndirect<Int>::quickSort (inx.data(), arr, nr, ord, type);
  } else if ((type & Sort::HeapSort) != 0) {
    return GenSortIndirect<Int>::heapSort (inx.data(), arr, nr, ord, type);
  } else if ((type & Sort::InsSort) != 0) {
    return GenSortIndirect<Int>::insSort (inx.data(), arr, nr, ord, type);
  }
  return genSort (inx, arr, nr, ord, type);
}

uInt doSort (Int* arr, uInt nr, Sort::Order ord, int type)
{
  if ((type & Sort::QuickSort) != 0) {
    return GenSort<Int>::quickSort (arr, nr, ord, type);
  } else if ((type & Sort::HeapSort) != 0) {
    return GenSort<Int>::heapSort (arr, nr, ord, type);
  } else if ((type & Sort::InsSort) != 0) {
    return GenSort<Int>::insSort (arr, nr, ord, type);
  }
  return genSort (arr, nr, ord, type);
}


int main(int argc, const char* argv[])
{
    uInt nr=4000;
    int type=Sort::DefaultSort;
    Sort::Order ord = Sort::Ascending;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    if (argc > 2) {
	istringstream istr(argv[2]);
	istr >> type;
    }
    if (argc > 3) {
	ord = Sort::Descending;
    }

    if ((type & Sort::QuickSort) != 0) {
        cout << "quickSort";
    } else if ((type & Sort::InsSort) != 0) {
	cout << "insSort  ";
    } else if ((type & Sort::HeapSort) != 0) {
        cout << "heapSort ";
    } else if ((type & Sort::ParSort) != 0) {
        cout << "parSort  ";
    } else {
        cout << "defSort  ";
    }
    if (ord == Sort::Ascending) {
	cout << "   Ascending";
    }else{
	cout << "   Descending";
    }
    // Outcomment the resulting number for assay when duplicates are
    // skipped for a random array. This number may differ from run to run.
    Bool showFlag = True;
    if ((type & Sort::NoDuplicates) != 0) {
	cout << "   (no duplicates)";
	showFlag = False;
    }
    cout << endl;

    Int* a1 = new Int[nr];
    Int* a2 = new Int[nr];
    Int* a3 = new Int[nr];
    Int* a4 = new Int[nr];
    Int* a5 = new Int[nr];
    for (uInt i=0; i<nr; i++) {
	a1[i] = i;
	a2[i] = nr-i;
	a3[i] = rand();
	a4[i] = 1;
	a5[i] = rand()%10;
    }
    cout << "  ordered array ";
    sortall (a1, nr, type, ord, True);
    cout << "  reversed array";
    sortall (a2, nr, type, ord, True);
    cout << "  random array  ";
    sortall (a3, nr, type, ord, showFlag);
    cout << "  equal array   ";
    sortall (a4, nr, type, ord, True);
    cout << "  10 diff. array";
    sortall (a5, nr, type, ord, True);

    delete [] a1;
    delete [] a2;
    delete [] a3;
    delete [] a4;
    delete [] a5;
    return 0;                            // exit with success status
}


void sortall (Int* arr, uInt nr, int type, Sort::Order ord, Bool showFlag)
{
    if (nr <= 5000000) {
      // Do an indirect sort for 'smaller' arrays only.
      Vector<uInt> inx(nr);
      Vector<uInt> index(nr);
      indgen (index);              // fill with 0,1,2,...
      Timer tim1;
      Int n1 = doSort (inx, arr, nr, ord, type);
      cout <<":  Indirect / direct" << endl;
      if (!showFlag) {
	cout << ">>> Resulting number may vary" << endl;
      }
      cout << setw(8) << n1 << endl;
      if (!showFlag) {
	cout << "<<<" << endl;
      }
      cout << ">>> Indirect    ";
      tim1.show();
      cout << "<<<" << endl;
      if (ord == Sort::Ascending) {
	for (Int i=1; i<n1; i++) {
	    if (arr[inx(i)] < arr[inx(i-1)]) {
		cout << "asc order error on index " << i << endl;
		break;
	    }
	    if (arr[inx(i)] == arr[inx(i-1)]
	    &&  index(inx(i)) < index(inx(i-1))) {
		cout << "asc equal order error on index " << i << endl;
		break;
	    }
	}
      }else{
	for (Int i=1; i<n1; i++) {
	    if (arr[inx(i)] > arr[inx(i-1)]) {
		cout << "desc order error on index " << i << endl;
		break;
	    }
	    if (arr[inx(i)] == arr[inx(i-1)]
	    &&  index(inx(i)) > index(inx(i-1))) {
		cout << "desc equal order error on index " << i << endl;
		break;
	    }
	}
      }
      if ((type & Sort::NoDuplicates) != 0) {
	for (Int i=1; i<n1; i++) {
	    if (arr[inx(i)] == arr[inx(i-1)]) {
		cout << "dupl error on index " << i << endl;
		break;
	    }
	}
      }
    }

    // Save the original array.
    Int* cparr = new Int[nr];
    memcpy (cparr, arr, nr*sizeof(Int));
    Int* cp2arr = new Int[nr];
    memcpy (cp2arr, arr, nr*sizeof(Int));

    // Do an in-place sort.
    Timer tim;
    Int n = doSort (arr, nr, ord, type);
    if (!showFlag) {
	cout << ">>>" << endl;
    }
    cout << setw(8) << n << endl;
    if (!showFlag) {
	cout << "<<< Resulting number may vary" << endl;
    }
    cout << ">>> GenSort     ";
    tim.show();
    cout << "<<<" << endl;
    if (ord == Sort::Ascending) {
	for (Int i=1; i<n; i++) {
	    if (arr[i] < arr[i-1]) {
		cout << "asc order error on index " << i << endl;
		break;
	    }
	}
    }else{
	for (Int i=1; i<n; i++) {
	    if (arr[i] > arr[i-1]) {
		cout << "desc order error on index " << i << endl;
		break;
	    }
	}
    }
    if ((type & Sort::NoDuplicates) != 0) {
	for (Int i=1; i<n; i++) {
	    if (arr[i] == arr[i-1]) {
		cout << "dupl error on index " << i << endl;
		break;
	    }
	}
    }

    // Find middle element.
    // When duplicates were skipped, the array has to be copied again.
    // Note that n instead of nr has to be used.
    if ((type & Sort::NoDuplicates) != 0) {
	memcpy (cparr, arr, n*sizeof(Int));
    }
    // First do it indirectly (for smaller arrays only).
    if (nr <= 5000000) {
        tim.mark();
        uInt kth = GenSortIndirect<Int>::kthLargest (cparr, n, n/2);
        cout << ">>> ind kthLar: ";
        tim.show();
        cout << "<<<" << endl;
        uInt mid = n/2;
        if (ord == Sort::Descending) {
          mid = (n-1)/2;
        }
        if (cparr[kth] != arr[mid]) {
          cout << "ind kthLargest is " << kth << "; should be " << mid << endl;
        }
    }
    tim.mark();
    Int kth = GenSort<Int>::kthLargest (cparr, n, n/2);
    cout << ">>> kthLar:     ";
    tim.show();
    cout << "<<<" << endl;
    uInt mid = n/2;
    if (ord == Sort::Descending) {
	mid = (n-1)/2;
    }
    if (kth != arr[mid]) {
	cout << "kthLargest is " << kth << "; should be " << arr[mid] << endl;
    }
    // Test STL algorithms.
    cout << ">>>" << endl;
    if ((type & Sort::NoDuplicates) != 0) {
      memcpy (cparr, arr, n*sizeof(Int));
    } else {
      memcpy (cparr, cp2arr, n*sizeof(Int));
    }
    tim.mark();
    std::nth_element (cparr, cparr+n/2, cparr+n);
    tim.show ("STL-nth         ");
    memcpy (cparr, cp2arr, nr*sizeof(Int));
    tim.mark();
    std::sort (cp2arr, cp2arr+nr);
    tim.show ("STL-sort        ");
    tim.mark();
    std::stable_sort (cparr, cparr+nr);
    tim.show ("STL-stable      ");
    cout << "<<<" << endl;

    delete [] cparr;
    delete [] cp2arr;
}

/*
Test remarks on MacBook OS-X Tiger g++-4.01. -O2:
1. casa's quicksort and kthLargest are as fast as STL sort and nth_element.
2. median could use min(2nd partition) to determine element (n+1)/2.

*/
