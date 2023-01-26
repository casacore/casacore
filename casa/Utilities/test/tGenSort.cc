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

void sortall (int32_t*, uint32_t, int, Sort::Order, bool);

uint32_t doSort (Vector<uint32_t>& inx, const int32_t* arr, uint32_t nr,
             Sort::Order ord, int type)
{
  inx.resize (nr);
  indgen(inx);
  if ((type & Sort::QuickSort) != 0) {
    return GenSortIndirect<int32_t,uint32_t>::quickSort (inx.data(), arr, nr, ord, type);
  } else if ((type & Sort::HeapSort) != 0) {
    return GenSortIndirect<int32_t,uint32_t>::heapSort (inx.data(), arr, nr, ord, type);
  } else if ((type & Sort::InsSort) != 0) {
    return GenSortIndirect<int32_t,uint32_t>::insSort (inx.data(), arr, nr, ord, type);
  }
  return genSort (inx, arr, nr, ord, type);
}

uint32_t doSort (int32_t* arr, uint32_t nr, Sort::Order ord, int type)
{
  if ((type & Sort::QuickSort) != 0) {
    return GenSort<int32_t>::quickSort (arr, nr, ord, type);
  } else if ((type & Sort::HeapSort) != 0) {
    return GenSort<int32_t>::heapSort (arr, nr, ord, type);
  } else if ((type & Sort::InsSort) != 0) {
    return GenSort<int32_t>::insSort (arr, nr, ord, type);
  }
  return genSort (arr, nr, ord, type);
}


int main(int argc, const char* argv[])
{
    uint32_t nr=4000;
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
    bool showFlag = true;
    if ((type & Sort::NoDuplicates) != 0) {
	cout << "   (no duplicates)";
	showFlag = false;
    }
    cout << endl;

    int32_t* a1 = new int32_t[nr];
    int32_t* a2 = new int32_t[nr];
    int32_t* a3 = new int32_t[nr];
    int32_t* a4 = new int32_t[nr];
    int32_t* a5 = new int32_t[nr];
    for (uint32_t i=0; i<nr; i++) {
	a1[i] = i;
	a2[i] = nr-i;
	a3[i] = rand();
	a4[i] = 1;
	a5[i] = rand()%10;
    }
    cout << "  ordered array ";
    sortall (a1, nr, type, ord, true);
    cout << "  reversed array";
    sortall (a2, nr, type, ord, true);
    cout << "  random array  ";
    sortall (a3, nr, type, ord, showFlag);
    cout << "  equal array   ";
    sortall (a4, nr, type, ord, true);
    cout << "  10 diff. array";
    sortall (a5, nr, type, ord, true);

    delete [] a1;
    delete [] a2;
    delete [] a3;
    delete [] a4;
    delete [] a5;

    // test N^2 quicksort input to check introsort fallback
    // would crash due to large recursion without fallback
    nr = 150000;
    uint32_t * indx = new uint32_t[nr];
    int32_t * data = new int32_t[nr];
    for (uint32_t i=0; i < nr; i++) {
        data[i] = 1;
        indx[i] = i+1;
    }
    indx[nr - 1] = 0;
    GenSortIndirect<int32_t,uint32_t>::quickSort (indx, data, nr, Sort::Ascending, 0);
    for (uint32_t i=0; i < nr; i++) {
        data[i] = i;
    }
    data[nr - 1] = -1;
    GenSort<int32_t>::quickSort (data, nr, Sort::Ascending, 0);
    delete [] indx;
    delete [] data;

    return 0;                            // exit with success status
}


void sortall (int32_t* arr, uint32_t nr, int type, Sort::Order ord, bool showFlag)
{
    if (nr <= 5000000) {
      // Do an indirect sort for 'smaller' arrays only.
      Vector<uint32_t> inx(nr);
      Vector<uint32_t> index(nr);
      indgen (index);              // fill with 0,1,2,...
      Timer tim1;
      int32_t n1 = doSort (inx, arr, nr, ord, type);
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
	for (int32_t i=1; i<n1; i++) {
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
	for (int32_t i=1; i<n1; i++) {
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
	for (int32_t i=1; i<n1; i++) {
	    if (arr[inx(i)] == arr[inx(i-1)]) {
		cout << "dupl error on index " << i << endl;
		break;
	    }
	}
      }
    }

    // Save the original array.
    int32_t* cparr = new int32_t[nr];
    memcpy (cparr, arr, nr*sizeof(int32_t));
    int32_t* cp2arr = new int32_t[nr];
    memcpy (cp2arr, arr, nr*sizeof(int32_t));

    // Do an in-place sort.
    Timer tim;
    int32_t n = doSort (arr, nr, ord, type);
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
	for (int32_t i=1; i<n; i++) {
	    if (arr[i] < arr[i-1]) {
		cout << "asc order error on index " << i << endl;
		break;
	    }
	}
    }else{
	for (int32_t i=1; i<n; i++) {
	    if (arr[i] > arr[i-1]) {
		cout << "desc order error on index " << i << endl;
		break;
	    }
	}
    }
    if ((type & Sort::NoDuplicates) != 0) {
	for (int32_t i=1; i<n; i++) {
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
	memcpy (cparr, arr, n*sizeof(int32_t));
    }
    // First do it indirectly (for smaller arrays only).
    if (nr <= 5000000) {
        tim.mark();
        uint32_t kth = GenSortIndirect<int32_t,uint32_t>::kthLargest (cparr, n, n/2);
        cout << ">>> ind kthLar: ";
        tim.show();
        cout << "<<<" << endl;
        uint32_t mid = n/2;
        if (ord == Sort::Descending) {
          mid = (n-1)/2;
        }
        if (cparr[kth] != arr[mid]) {
          cout << "ind kthLargest is " << kth << "; should be " << mid << endl;
        }
    }
    tim.mark();
    int32_t kth = GenSort<int32_t>::kthLargest (cparr, n, n/2);
    cout << ">>> kthLar:     ";
    tim.show();
    cout << "<<<" << endl;
    uint32_t mid = n/2;
    if (ord == Sort::Descending) {
	mid = (n-1)/2;
    }
    if (kth != arr[mid]) {
	cout << "kthLargest is " << kth << "; should be " << arr[mid] << endl;
    }
    // Test STL algorithms.
    cout << ">>>" << endl;
    if ((type & Sort::NoDuplicates) != 0) {
      memcpy (cparr, arr, n*sizeof(int32_t));
    } else {
      memcpy (cparr, cp2arr, n*sizeof(int32_t));
    }
    tim.mark();
    std::nth_element (cparr, cparr+n/2, cparr+n);
    tim.show ("STL-nth         ");
    memcpy (cparr, cp2arr, nr*sizeof(int32_t));
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
