//# GenSort.cc: General sort functions
//# Copyright (C) 1993,1994,1995,1996,1997,1998
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <aips/Utilities/GenSort.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Block.h>
#include <aips/Exceptions/Error.h>

// Do a quicksort in ascending order.
// All speedups are from Sedgewick; Algorithms in C.
template<class T>
void GenSort<T>::quickSortAsc (T* data, Int nr)
{
    // QuickSorting small sets makes no sense.
    // It will be finished with an insertion sort.
    // The number 15 is experimentally determined on a SUN IPC.
    // It is not very critical.
    if (nr <= 15) {
	return;
    }
    // Choose a partition element by taking the median of the
    // first, middle and last element.
    // Store the partition element at the end.
    // Do not use Sedgewick\'s advise to store the partition element in
    // data[nr-2]. This has dramatic results for reversed ordered arrays.
    Int i = (nr-1)/2;                        // middle element
    T* sf = data;                            // first element
    T* sl = data+nr-1;                       // last element
    if (data[i] < *sf)
	swap (data[i], *sf);
    if (*sl < *sf)
	swap (*sl, *sf);
    if (data[i] < *sl)
	swap (data[i], *sl);
    T par = *sl;                             // partition element
    // Now partition until the pointers cross.
    for (;;) {
	while (*++sf < par) ;
	while (*--sl > par) ;
	if (sf >= sl) break;
	swap (*sf, *sl);
    }
    swap (*sf, data[nr-1]);
    i = sf-data;
    quickSortAsc (data, i);                  // sort left part
    quickSortAsc (sf+1, nr-i-1);             // sort right part
}

// Do a quicksort in descending order.
template<class T>
void GenSort<T>::quickSortDesc (T* data, Int nr)
{
    if (nr <= 15) {
	return;
    }
    Int i = (nr-1)/2;                        // middle element
    T* sf = data;                            // first element
    T* sl = data+nr-1;                       // last element
    if (data[i] > *sf)
	swap (data[i], *sf);
    if (*sl > *sf)
	swap (*sl, *sf);
    if (data[i] > *sl)
	swap (data[i], *sl);
    T par = *sl;                             // partition element
    // Now partition until the pointers cross.
    for (;;) {
	while (*++sf > par) ;
	while (*--sl < par) ;
	if (sf >= sl) break;
	swap (*sf, *sl);
    }
    swap (*sf, data[nr-1]);
    i = sf-data;
    quickSortDesc (data, i);                  // sort left part
    quickSortDesc (sf+1, nr-i-1);             // sort right part
}

// Find the k-th largest element using a partial quicksort.
template<class T>
T GenSort<T>::kthLargest (T* data, uInt nr, uInt k)
{
    if (k >= nr) {
	throw (AipsError ("kthLargest(data, nr, k): k must be < nr"));
    }
    Int st = 0;
    Int end = Int(nr) - 1;
    // Partition until a set of 1 element is left.
    while (end > st) {
	// Choose a partition element by taking the median of the
	// first, middle and last element.
	// Store the partition element at the end.
	// Do not use Sedgewick\'s advise to store the partition element in
	// data[nr-2]. This has dramatic results for reversed ordered arrays.
	Int i = (st+end)/2;                      // middle element
	T* sf = data+st;                         // first element
	T* sl = data+end;                        // last element
	if (data[i] < *sf)
	    swap (data[i], *sf);
	if (*sl < *sf)
	    swap (*sl, *sf);
	if (data[i] < *sl)
	    swap (data[i], *sl);
	T par = *sl;                             // partition element
	// Now partition until the pointers cross.
	for (;;) {
	    while (*++sf < par) ;
	    while (*--sl > par) ;
	    if (sf >= sl) break;
	    swap (*sf, *sl);
	}
	swap (*sf, data[end]);
	// Determine index of partitioning and update the start and end
	// to take left or right part.
	i = sf-data;
	if (i <= Int(k)) st = i;
	if (i >= Int(k)) end = i;
    }
    return data[k];
}

// Do an insertion sort in ascending order.
template<class T>
uInt GenSort<T>::insSortAsc (T* data, Int nr, int opt)
{
    if ((opt & Sort::NoDuplicates) == 0) {
	return insSortAscDup (data, nr);
    }else{
	return insSortAscNoDup (data, nr);
    }
}

// Do an insertion sort in ascending order.
// Keep duplicate elements.
template<class T>
uInt GenSort<T>::insSortAscDup (T* data, Int nr)
{
    Int  j;
    T cur;
    for (Int i=1; i<nr; i++) {
	j   = i;
	cur = data[i];
	while (--j>=0  &&  data[j] > cur) {
	    data[j+1] = data[j];
	}
	data[j+1] = cur;
    }
    return nr;
}

// Do an insertion sort in ascending order.
// Skip duplicate elements.
template<class T>
uInt GenSort<T>::insSortAscNoDup (T* data, Int nr)
{
    if (nr < 2) {
	return nr;                                // nothing to sort
    }
    Int  j, k;
    T cur;
    Int n = 1;
    for (Int i=1; i<nr; i++) {
	j   = n;
	cur = data[i];
	while (--j>=0  &&  data[j] > cur) {
	}
	if (j < 0  ||  !(data[j] == cur)) {       // no equal key
	    for (k=n-1; k>j; k--) {
		data[k+1] = data[k];              // now shift to right
	    }
	    data[j+1] = cur;                      // insert in right place
	    n++;
	}
    }
    return n;
}

// Do an insertion sort in descending order.
template<class T>
uInt GenSort<T>::insSortDesc (T* data, Int nr, int opt)
{
    if ((opt & Sort::NoDuplicates) == 0) {
	return insSortDescDup (data, nr);
    }else{
	return insSortDescNoDup (data, nr);
    }
}

// Do an insertion sort in descending order.
// Keep duplicate elements.
template<class T>
uInt GenSort<T>::insSortDescDup (T* data, Int nr)
{
    Int  j;
    T cur;
    for (Int i=1; i<nr; i++) {
	j   = i;
	cur = data[i];
	while (--j>=0  &&  data[j] < cur) {
	    data[j+1] = data[j];
	}
	data[j+1] = cur;
    }
    return nr;
}

// Do an insertion sort in descending order.
// Skip duplicate elements.
template<class T>
uInt GenSort<T>::insSortDescNoDup (T* data, Int nr)
{
    if (nr < 2) {
	return nr;                                // nothing to sort
    }
    Int  j, k;
    T cur;
    Int n = 1;
    for (Int i=1; i<nr; i++) {
	j   = n;
	cur = data[i];
	while (--j>=0  &&  data[j] < cur) {
	}
	if (j < 0  ||  !(data[j] == cur)) {       // no equal key
	    for (k=n-1; k>j; k--) {
		data[k+1] = data[k];              // now shift to right
	    }
	    data[j+1] = cur;                      // insert in right place
	    n++;
	}
    }
    return n;
}

// Do a heapsort in ascending order.
template<class T>
void GenSort<T>::heapSortAsc (T* data, Int nr)
{
    // Use the heapsort algorithm described by Jon Bentley in
    // UNIX Review, August 1992.
    data--;
    Int j;
    for (j=nr/2; j>=1; j--) {
	heapAscSiftDown (j, nr, data);
    }
    for (j=nr; j>=2; j--) {
	swap (data[1], data[j]);
	heapAscSiftDown (1, j-1, data);
    }
}

template<class T>
void GenSort<T>::heapAscSiftDown (Int low, Int up, T* data)
{
    T sav = data[low];
    Int c;
    Int i;
    for (i=low; (c=2*i)<=up; i=c) {
	if (c < up  &&  data[c+1] > data[c]) {
	    c++;
	}
	data[i] = data[c];
    }
    data[i] = sav;
    for ( ; (c=i/2)>= low; i=c) {
	if (!(data[i] > data[c])) {
	    break;
	}
	swap (data[c], data[i]);
    }
}

// Do a heapsort in descending order.
template<class T>
void GenSort<T>::heapSortDesc (T* data, Int nr)
{
    // Use the heapsort algorithm described by Jon Bentley in
    // UNIX Review, August 1992.
    data--;
    Int j;
    for (j=nr/2; j>=1; j--) {
	heapDescSiftDown (j, nr, data);
    }
    for (j=nr; j>=2; j--) {
	swap (data[1], data[j]);
	heapDescSiftDown (1, j-1, data);
    }
}

template<class T>
void GenSort<T>::heapDescSiftDown (Int low, Int up, T* data)
{
    T sav = data[low];
    Int c;
    Int i;
    for (i=low; (c=2*i)<=up; i=c) {
	if (c < up  &&  data[c+1] < data[c]) {
	    c++;
	}
	data[i] = data[c];
    }
    data[i] = sav;
    for ( ; (c=i/2)>= low; i=c) {
	if (!(data[i] < data[c])) {
	    break;
	}
	swap (data[c], data[i]);
    }
}


template<class T>
uInt GenSort<T>::insSort (T* data, uInt nr, Sort::Order ord, int opt)
{
    Int n = nr;
    if (ord == Sort::Descending) {
	return insSortDesc (data, n, opt);
    }else{
	return insSortAsc (data, n, opt);
    }
}

template<class T>
uInt GenSort<T>::quickSort (T* data, uInt nr, Sort::Order ord, int opt)
{
    Int n = nr;
    if (ord == Sort::Descending) {
	quickSortDesc (data, n);
	return insSortDesc (data, n, opt);
    }else{
	quickSortAsc (data, n);
	return insSortAsc (data, n, opt);
    }
}

template<class T>
uInt GenSort<T>::heapSort (T* data, uInt nr, Sort::Order ord, int opt)
{
    Int n = nr;
    if (ord == Sort::Descending) {
	heapSortDesc (data, n);
	if ((opt & Sort::NoDuplicates) != 0) {
	    return insSortDesc (data, n, opt);
	}
    }else{
	heapSortAsc (data, n);
	if ((opt & Sort::NoDuplicates) != 0) {
	    return insSortAsc (data, n, opt);
	}
    }
    return nr;
}



// Use quicksort if nothing given.
template<class T>
uInt GenSort<T>::sort (T* data, uInt nr, Sort::Order ord, int opt)
{
    if ((opt & Sort::HeapSort) != 0) {
	return heapSort (data, nr, ord, opt);
    }else{
	if ((opt & Sort::InsSort) != 0) {
	    return insSort (data, nr, ord, opt);
	}else{
	    return quickSort (data, nr, ord, opt);
	}
    }
}

template<class T>
uInt GenSort<T>::sort (Array<T>& data, Sort::Order ord, int opt)
{
    Bool del;
    T* dptr = data.getStorage(del);
    uInt nr = sort (dptr, data.nelements(), ord, opt);
    data.putStorage (dptr, del);
    return nr;
}

template<class T>
uInt GenSort<T>::sort (Block<T>& data, uInt nr, Sort::Order ord, int opt)
{
    return sort (data.storage(), min(nr, data.nelements()), ord, opt);
}





// Remove eventually - needed for g++
typedef Vector<uInt> forgnugpp_GenSort;

template<class T>
uInt GenSortIndirect<T>::sort (Vector<uInt>& indexVector, const Array<T>& data,
			       Sort::Order ord, int opt)
{
    Bool del;
    const T* dptr = data.getStorage(del);
    uInt nr = sort (indexVector, dptr, data.nelements(), ord, opt);
    data.freeStorage (dptr, del);
    return nr;
}

template<class T>
uInt GenSortIndirect<T>::sort (Vector<uInt>& indexVector, const Block<T>& data,
			       uInt nr, Sort::Order ord, int opt)
{
    return sort (indexVector, data.storage(), min(nr, data.nelements()),
		 ord, opt);
}

// Use quicksort if nothing given.
template<class T>
uInt GenSortIndirect<T>::sort (Vector<uInt>& indexVector, const T* data,
			       uInt nr, Sort::Order ord, int opt)
{
    // Fill the index vector with the indices.
    indexVector.resize (nr);
    indgen (indexVector.ac());
    // Pass the sort function a C-array of indices, because indexing
    // in there is (much) faster than in a vector.
    Bool del;
    uInt* inx = indexVector.getStorage (del);
    // Choose the sort required.
    uInt n;
    if ((opt & Sort::HeapSort) != 0) {
	n = heapSort (inx, data, nr, ord, opt);
    }else{
	if ((opt & Sort::InsSort) != 0) {
	    n = insSort (inx, data, nr, ord, opt);
	}else{
	    n = quickSort (inx, data, nr, ord, opt);
	}
    }
    indexVector.putStorage (inx, del);
    // If n < nr, some duplicates have been deleted.
    // This means we have to resize the Vector.
    if (n < nr) {
	Vector<uInt> vec(n);
	vec = indexVector (Slice(0,n));
	indexVector.reference (vec);
    }
    return n;
}

template<class T>
uInt GenSortIndirect<T>::insSort (uInt* inx, const T* data, uInt nr,
				  Sort::Order ord, int opt)
{
    Int n = nr;
    if (ord == Sort::Descending) {
	return insSortDesc (inx, data, n, opt);
    }else{
	return insSortAsc (inx, data, n, opt);
    }
}

template<class T>
uInt GenSortIndirect<T>::quickSort (uInt* inx, const T* data, uInt nr,
				    Sort::Order ord, int opt)
{
    Int n = nr;
    if (ord == Sort::Descending) {
	quickSortDesc (inx, data, n);
	return insSortDesc (inx, data, n, opt);
    }else{
	quickSortAsc (inx, data, n);
	return insSortAsc (inx, data, n, opt);
    }
}

template<class T>
uInt GenSortIndirect<T>::heapSort (uInt* inx, const T* data, uInt nr,
				   Sort::Order ord, int opt)
{
    Int n = nr;
    if (ord == Sort::Descending) {
	heapSortDesc (inx, data, n);
	if ((opt & Sort::NoDuplicates) != 0) {
	    return insSortDesc (inx, data, n, opt);
	}
    }else{
	heapSortAsc (inx, data, n);
	if ((opt & Sort::NoDuplicates) != 0) {
	    return insSortAsc (inx, data, n, opt);
	}
    }
    return nr;
}



template<class T>
void GenSortIndirect<T>::quickSortAsc (uInt* inx, const T* data, Int nr)
{
    if (nr <= 15) {
	return;                    // finish it off with insertion sort
    }
    uInt* mid= inx + (nr-1)/2;
    uInt* sf = inx;
    uInt* sl = inx+nr-1;
    if (isAscending (data, *sf, *mid))
	swapInx (*sf, *mid);
    if (isAscending (data, *sf, *sl))
	swapInx (*sf, *sl);
    if (isAscending (data, *sl, *mid))
	swapInx (*sl, *mid);
    T partVal = data[*sl];
    uInt partInx = *sl;
    // Compare indices in case the keys are equal.
    // This ensures that the sort is stable.
    for (;;) {
	while (data[*++sf] < partVal
	       ||  (partVal == data[*sf]  &&  *sf < partInx)) ;
	while (data[*--sl] > partVal
	       ||  (partVal == data[*sl]  &&  *sl > partInx)) ;
	if (sf >= sl) break;
	swapInx (*sf, *sl);
    }
    swapInx (*sf, inx[nr-1]);
    Int n = sf-inx;
    quickSortAsc (inx, data, n);
    quickSortAsc (sf+1, data, nr-n-1);
}

// Do a quicksort in descending order.
template<class T>
void GenSortIndirect<T>::quickSortDesc (uInt* inx, const T* data, Int nr)
{
    if (nr <= 15) {
	return;                    // finish it off with insertion sort
    }
    uInt* mid= inx + (nr-1)/2;
    uInt* sf = inx;
    uInt* sl = inx+nr-1;
    if (isDescending (data, *sf, *mid))
	swapInx (*sf, *mid);
    if (isDescending (data, *sf, *sl))
	swapInx (*sf, *sl);
    if (isDescending (data, *sl, *mid))
	swapInx (*sl, *mid);
    T partVal = data[*sl];
    uInt partInx = *sl;
    // Compare indices in case the keys are equal.
    // This ensures that the sort is stable.
    for (;;) {
	while (data[*++sf] > partVal
	       ||  (partVal == data[*sf]  &&  *sf < partInx)) ;
	while (data[*--sl] < partVal
	       ||  (partVal == data[*sl]  &&  *sl > partInx)) ;
	if (sf >= sl) break;
	swapInx (*sf, *sl);
    }
    swapInx (*sf, inx[nr-1]);
    Int n = sf-inx;
    quickSortDesc (inx, data, n);
    quickSortDesc (sf+1, data, nr-n-1);
}

// Do an insertion sort in ascending order.
template<class T>
uInt GenSortIndirect<T>::insSortAsc (uInt* inx, const T* data,
				     Int nr, int opt)
{
    if ((opt & Sort::NoDuplicates) == 0) {
	return insSortAscDup (inx, data, nr);
    }else{
	return insSortAscNoDup (inx, data, nr);
    }
}

// Do an insertion sort in ascending order.
// Keep duplicate elements.
template<class T>
uInt GenSortIndirect<T>::insSortAscDup (uInt* inx, const T* data, Int nr)
{
    Int  j;
    uInt cur;
    for (Int i=1; i<nr; i++) {
	j   = i;
	cur = inx[i];
	while (--j>=0  &&  isAscending (data, inx[j], cur)) {
	    inx[j+1] = inx[j];
	}
	inx[j+1] = cur;
    }
    return nr;
}

// Do an insertion sort in ascending order.
// Skip duplicate elements.
template<class T>
uInt GenSortIndirect<T>::insSortAscNoDup (uInt* inx, const T* data, Int nr)
{
    if (nr < 2) {
	return nr;                                // nothing to sort
    }
    Int  j, k;
    uInt cur;
    Int n = 1;
    for (Int i=1; i<nr; i++) {
	j   = n;
	cur = inx[i];
	while (--j>=0  &&  data[inx[j]] > data[cur]) {
	}
	if (j < 0  ||  !(data[inx[j]] == data[cur])) {   // no equal key
	    for (k=n-1; k>j; k--) {
		inx[k+1] = inx[k];               // now shift to right
	    }
	    inx[j+1] = cur;                      // insert in right place
	    n++;
	}
    }
    return n;
}

// Do an insertion sort in descending order.
template<class T>
uInt GenSortIndirect<T>::insSortDesc (uInt* inx, const T* data,
				      Int nr, int opt)
{
    if ((opt & Sort::NoDuplicates) == 0) {
	return insSortDescDup (inx, data, nr);
    }else{
	return insSortDescNoDup (inx, data, nr);
    }
}

// Do an insertion sort in descending order.
// Keep duplicate elements.
template<class T>
uInt GenSortIndirect<T>::insSortDescDup (uInt* inx, const T* data, Int nr)
{
    Int  j;
    uInt cur;
    for (Int i=1; i<nr; i++) {
	j   = i;
	cur = inx[i];
	while (--j>=0  &&  isDescending (data, inx[j], cur)) {
	    inx[j+1] = inx[j];
	}
	inx[j+1] = cur;
    }
    return nr;
}

// Do an insertion sort in descending order.
// Skip duplicate elements.
template<class T>
uInt GenSortIndirect<T>::insSortDescNoDup (uInt* inx, const T* data, Int nr)
{
    if (nr < 2) {
	return nr;                                // nothing to sort
    }
    Int  j, k;
    uInt cur;
    Int n = 1;
    for (Int i=1; i<nr; i++) {
	j   = n;
	cur = inx[i];
	while (--j>=0  &&  data[inx[j]] < data[cur]) {
	}
	if (j < 0  ||  !(data[inx[j]] == data[cur])) {   // no equal key
	    for (k=n-1; k>j; k--) {
		inx[k+1] = inx[k];               // now shift to right
	    }
	    inx[j+1] = cur;                      // insert in right place
	    n++;
	}
    }
    return n;
}

// Do a heapsort in ascending order.
template<class T>
void GenSortIndirect<T>::heapSortAsc (uInt* inx, const T* data, Int nr)
{
    // Use the heapsort algorithm described by Jon Bentley in
    // UNIX Review, August 1992.
    inx--;
    Int j;
    for (j=nr/2; j>=1; j--) {
	heapAscSiftDown (inx, j, nr, data);
    }
    for (j=nr; j>=2; j--) {
	swapInx (inx[1], inx[j]);
	heapAscSiftDown (inx, 1, j-1, data);
    }
}

template<class T>
void GenSortIndirect<T>::heapAscSiftDown (uInt* inx, Int low, Int up,
					  const T* data)
{
    uInt sav = inx[low];
    Int c;
    Int i;
    for (i=low; (c=2*i)<=up; i=c) {
	if (c < up  &&  isAscending (data, inx[c+1], inx[c])) {
	    c++;
	}
	inx[i] = inx[c];
    }
    inx[i] = sav;
    for ( ; (c=i/2)>= low; i=c) {
	if (isAscending (data, inx[c], inx[i])) {
	    break;
	}
	swapInx (inx[c], inx[i]);
    }
}

// Do a heapsort in descending order.
template<class T>
void GenSortIndirect<T>::heapSortDesc (uInt* inx, const T* data, Int nr)
{
    // Use the heapsort algorithm described by Jon Bentley in
    // UNIX Review, August 1992.
    inx--;
    Int j;
    for (j=nr/2; j>=1; j--) {
	heapDescSiftDown (inx, j, nr, data);
    }
    for (j=nr; j>=2; j--) {
	swapInx (inx[1], inx[j]);
	heapDescSiftDown (inx, 1, j-1, data);
    }
}

template<class T>
void GenSortIndirect<T>::heapDescSiftDown (uInt* inx, Int low, Int up,
					   const T* data)
{
    uInt sav = inx[low];
    Int c;
    Int i;
    for (i=low; (c=2*i)<=up; i=c) {
	if (c < up  &&  isDescending (data, inx[c+1], inx[c])) {
	    c++;
	}
	inx[i] = inx[c];
    }
    inx[i] = sav;
    for ( ; (c=i/2)>= low; i=c) {
	if (isDescending (data, inx[c], inx[i])) {
	    break;
	}
	swapInx (inx[c], inx[i]);
    }
}
