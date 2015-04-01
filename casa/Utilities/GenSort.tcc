//# GenSort.cc: General sort functions
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000
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

#ifndef CASA_GENSORT_TCC
#define CASA_GENSORT_TCC

#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Exceptions/Error.h>
#ifdef _OPENMP
# include <omp.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Do a quicksort in ascending order.
// All speedups are from Sedgewick; Algorithms in C.
template<class T>
void GenSort<T>::quickSortAsc (T* data, Int nr, Bool multiThread)
{
    // QuickSorting small sets makes no sense.
    // It will be finished with an insertion sort.
    // The number 32 is determined experimentally. It is not very critical.
    if (nr <= 32) {
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
    if (multiThread) {
#ifdef _OPENMP
#pragma omp parallel for
#endif
      for (int thr=0; thr<2; ++thr) {
        if (thr==0) quickSortAsc (data, i);             // sort left part
        if (thr==1) quickSortAsc (sf+1, nr-i-1);        // sort right part
      }
    } else {
      quickSortAsc (data, i);                  // sort left part
      quickSortAsc (sf+1, nr-i-1);             // sort right part
    }
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
    // Partition until a set of 1 or 2 elements is left.
    while (end > st+1) {
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
    if (end == st+1) {
      if (data[st] > data[end]) {
	swap (data[st], data[end]);
      }
    }
    return data[k];
}

// Do an insertion sort in ascending order.
template<class T>
uInt GenSort<T>::insSortAsc (T* data, Int nr, int opt)
{
  if ((opt & Sort::NoDuplicates) == 0) {
    return insSortAscDup (data, nr);
  }
  return insSortAscNoDup (data, nr);
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
	while (j>0  &&  data[j-1] > cur) {
	    data[j] = data[j-1];
            j--;
	}
	data[j] = cur;
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
	while (j>0  &&  data[j-1] > cur) {
            j--;
        }
	if (j <= 0  ||  !(data[j-1] == cur)) {    // no equal key
	    for (k=n-1; k>=j; k--) {
		data[k+1] = data[k];              // now shift to right
	    }
	    data[j] = cur;                        // insert in right place
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


template<class T>
uInt GenSort<T>::parSort (T* data, uInt nr, Sort::Order ord, int opt,
                          int nthread)
{
  int nthr = nthread;    // to avoid compiler warning
#ifdef _OPENMP
  if (nthread > 0) {
    nthr = nthread;
    // Do not use more threads than there are values.
    if (uInt(nthr) > nr) nthr = nr;
    omp_set_num_threads (nthr);
  } else {
    nthr = omp_get_max_threads();
    if (uInt(nthr) > nr) nthr = nr;
  }
#else
  nthr = 1;
#endif
  Block<uInt> index(nr+1);
  Block<uInt> tinx(nthr+1);
  Block<uInt> np(nthr);
  // Determine ordered parts in the array.
  // It is done in parallel, whereafter the parts are combined.
  int step = nr/nthr;
  for (int i=0; i<nthr; ++i) tinx[i] = i*step;
  tinx[nthr] = nr;
  // Use ifdef to avoid compiler warning.
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (int i=0; i<nthr; ++i) {
    int nparts = 1;
    index[tinx[i]] = tinx[i];
    for (uInt j=tinx[i]+1; j<tinx[i+1]; ++j) {
      if (data[j-1] > data[j]) {
        index[tinx[i]+nparts] = j;    // out of order, thus new part
        nparts++;
      }
    }
    np[i] = nparts;
  }
  // Make index parts consecutive by shifting to the left.
  // See if last and next part can be combined.
  uInt nparts = np[0];
  for (int i=1; i<nthr; ++i) {
    if (data[tinx[i]-1] > data[tinx[i]]) {
      index[nparts++] = index[tinx[i]];
    }
    if (nparts == tinx[i]+1) {
      nparts += np[i]-1;
    } else {
      for (uInt j=1; j<np[i]; ++j) {
	index[nparts++] = index[tinx[i]+j];
      }
    }
  }
  index[nparts] = nr;
  //cout<<"nparts="<<nparts<<endl;
  // Merge the array parts. Each part is ordered.
  if (nparts < nr) {
    Block<T> tmp(nr);
    T* res = merge (data, tmp.storage(), nr, index.storage(), nparts);
    // Skip duplicates if needed.
    if ((opt & Sort::NoDuplicates) != 0) {
      nr = insSortAscNoDup (res, nr);
    }
    // Result is in ascending order; reverse if descending is needed.
    if (ord == Sort::Descending) {
      reverse (data, res, nr);
    } else if (res != data) {
      // The final result must end up in data.
      objcopy (data, res, nr);
    }
  } else {
    // Each part has length 1, so the array is in descending order and unique.
    // Reverse if ascending is needed.
    if (ord == Sort::Ascending) {
      reverse (data, data, nr);
    }
  }
  return nr;
}  

template<class T>
void GenSort<T>::reverse (T* data, const T* res, uInt nr)
{
  // The result must end up in data.
  if (res == data) {
    for (uInt i=0; i<nr/2; ++i) {
      T tmp(data[i]);
      data[i] = data[nr-1-i];
      data[nr-i-1] = tmp;
    }
  } else {
    for (uInt i=0; i<nr; ++i) data[i] = res[nr-1-i];
  }
}

template<class T>
T* GenSort<T>::merge (T* data, T* tmp, uInt nr, uInt* index,
                      uInt nparts)
{
  T* a = data;
  T* b = tmp;
  int np = nparts;
  // If the nr of parts is odd, the last part is not merged. To avoid having
  // to copy it to the other array, a pointer 'last' is kept.
  // Note that merging the previous part with the last part works fine, even
  // if the last part is in the same buffer.
  T* last = data + index[np-1];
  while (np > 1) {
  // Use ifdef to avoid compiler warning.
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
    for (int i=0; i<np; i+=2) {
      if (i < np-1) {
        // Merge 2 subsequent parts of the array.
	T* f1 = a+index[i];
	T* f2 = a+index[i+1];
	T* to = b+index[i];
	uInt na = index[i+1]-index[i];
	uInt nb = index[i+2]-index[i+1];
        if (i == np-2) {
          //cout<<"swap last np=" <<np<<endl;
          f2 = last;
          last = to;
        }
	uInt ia=0, ib=0, k=0;
	while (ia < na && ib < nb) {
	  if (f1[ia] < f2[ib]) {
	    to[k] = f1[ia++];
	  } else {
	    to[k] = f2[ib++];
	  }
	  k++;
	}
	if (ia < na) {
	  for (uInt p=ia; p<na; p++,k++) to[k] = f1[p];
	} else {
	  for (uInt p=ib; p<nb; p++,k++) to[k] = f2[p];
	}
      }
    }
    // Collapse the index.
    int k=0;
    for (int i=0; i<np; i+=2) index[k++] = index[i];
    index[k] = nr;
    np = k;
    // Swap the index target and destination.
    T* c = a;
    a = b;
    b = c;
  }
  return a;
}

template<class T>
uInt GenSort<T>::insSort (T* data, uInt nr, Sort::Order ord, int opt)
{
  uInt n = insSortAsc (data, nr, opt);
  if (ord == Sort::Descending) {
    reverse (data, data, n);
  }
  return n;
}

template<class T>
uInt GenSort<T>::quickSort (T* data, uInt nr, Sort::Order ord, int opt)
{
  // Use quicksort to do rough sorting.
  quickSortAsc (data, nr, True);
  // Finish with an insertion sort (which also skips duplicates if needed).
  // Note: if quicksort keeps track of its boundaries, the insSort of all
  // parts could be done in parallel.
  return insSort (data, nr, ord, opt);
}

template<class T>
uInt GenSort<T>::heapSort (T* data, uInt nr, Sort::Order ord, int opt)
{
  uInt n = nr;
  heapSortAsc (data, nr);
  if ((opt & Sort::NoDuplicates) != 0) {
    n = insSortAscNoDup (data, nr);
  }
  if (ord == Sort::Descending) {
    reverse (data, data, n);
  }
  return n;
}



template<class T>
uInt GenSort<T>::sort (T* data, uInt nr, Sort::Order ord, int opt)
{
  // Determine the default sort to use.
  if (opt - (opt&Sort::NoDuplicates) == Sort::DefaultSort) {
    int nthr = 1;
#ifdef _OPENMP
    nthr = omp_get_max_threads();
#endif
    int type = (nr<1000 || nthr==1  ?  Sort::QuickSort : Sort::ParSort);
    opt = opt - Sort::DefaultSort + type;
  }
  // Do the sort.
  if ((opt & Sort::HeapSort) != 0) {
    return heapSort (data, nr, ord, opt);
  } else if ((opt & Sort::InsSort) != 0) {
    return insSort (data, nr, ord, opt);
  } else if ((opt & Sort::QuickSort) != 0) {
    return quickSort (data, nr, ord, opt);
  } else {
    return parSort (data, nr, ord, opt);
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
    indgen (indexVector);
    // Pass the sort function a C-array of indices, because indexing
    // in there is (much) faster than in a vector.
    Bool del;
    uInt* inx = indexVector.getStorage (del);
    // Choose the sort required.
    uInt n;
    // Determine the default sort to use.
    if (opt - (opt&Sort::NoDuplicates) == Sort::DefaultSort) {
        int nthr = 1;
#ifdef _OPENMP
        nthr = omp_get_max_threads();
#endif
        int type = (nr<1000 || nthr==1  ?  Sort::QuickSort : Sort::ParSort);
        opt = opt - Sort::DefaultSort + type;
    }
    // Do the sort.
    if ((opt & Sort::HeapSort) != 0) {
      n = heapSort (inx, data, nr, ord, opt);
    } else if ((opt & Sort::InsSort) != 0) {
      n = insSort (inx, data, nr, ord, opt);
    } else if ((opt & Sort::QuickSort) != 0) {
      n = quickSort (inx, data, nr, ord, opt);
    } else {
      n = parSort (inx, data, nr, ord, opt);
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
  uInt n = insSortAsc (inx, data, nr, opt);
  if (ord == Sort::Descending) {
    GenSort<uInt>::reverse (inx, inx, n);
  }
  return n;
}

template<class T>
uInt GenSortIndirect<T>::quickSort (uInt* inx, const T* data, uInt nr,
				    Sort::Order ord, int opt)
{
  // Use quicksort to do rough sorting.
  quickSortAsc (inx, data, nr, True);
  // Finish with an insertion sort (which also skips duplicates if needed).
  // Note: if quicksort keeps track of its boundaries, the insSort of all
  // parts could be done in parallel.
  return insSort (inx, data, nr, ord, opt);
}

template<class T>
uInt GenSortIndirect<T>::heapSort (uInt* inx, const T* data, uInt nr,
				   Sort::Order ord, int opt)
{
  uInt n = nr;
  heapSortAsc (inx, data, nr);
  if ((opt & Sort::NoDuplicates) != 0) {
    n = insSortAscNoDup (inx, data, nr);
  }
  if (ord == Sort::Descending) {
    GenSort<uInt>::reverse (inx, inx, n);
  }
  return n;
}

template<class T>
uInt GenSortIndirect<T>::parSort (uInt* inx, const T* data, uInt nr,
                                  Sort::Order ord, int opt, int nthread)
{
  int nthr = nthread;    // to avoid compiler warning
#ifdef _OPENMP
  if (nthread > 0) {
    nthr = nthread;
    // Do not use more threads than there are values.
    if (uInt(nthr) > nr) nthr = nr;
    omp_set_num_threads (nthr);
  } else {
    nthr = omp_get_max_threads();
    if (uInt(nthr) > nr) nthr = nr;
  }
#else
  nthr = 1;
#endif
  Block<uInt> index(nr+1);
  Block<uInt> tinx(nthr+1);
  Block<uInt> np(nthr);
  // Determine ordered parts in the array.
  // It is done in parallel, whereafter the parts are combined.
  int step = nr/nthr;
  for (int i=0; i<nthr; ++i) tinx[i] = i*step;
  tinx[nthr] = nr;
  // Use ifdef to avoid compiler warning.
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (int i=0; i<nthr; ++i) {
    int nparts = 1;
    index[tinx[i]] = tinx[i];
    for (uInt j=tinx[i]+1; j<tinx[i+1]; ++j) {
      if (data[inx[j-1]] > data[inx[j]]) {
        index[tinx[i]+nparts] = j;    // out of order, thus new part
        nparts++;
      }
    }
    np[i] = nparts;
  }
  // Make index parts consecutive by shifting to the left.
  // See if last and next part can be combined.
  uInt nparts = np[0];
  for (int i=1; i<nthr; ++i) {
    if (data[tinx[i]-1] > data[tinx[i]]) {
      index[nparts++] = index[tinx[i]];
    }
    if (nparts == tinx[i]+1) {
      nparts += np[i]-1;
    } else {
      for (uInt j=1; j<np[i]; ++j) {
	index[nparts++] = index[tinx[i]+j];
      }
    }
  }
  index[nparts] = nr;
  //cout<<"nparts="<<nparts<<endl;
  // Merge the array parts. Each part is ordered.
  if (nparts < nr) {
    Block<uInt> inxtmp(nr);
    uInt* res = merge (data, inx, inxtmp.storage(), nr,
                       index.storage(), nparts);
    // Skip duplicates if needed.
    if ((opt & Sort::NoDuplicates) != 0) {
      nr = insSortAscNoDup (res, data, nr);
    }
    // Result is in ascending order; reverse if descending is needed.
    if (ord == Sort::Descending) {
      GenSort<uInt>::reverse (inx, res, nr);
    } else if (res != inx) {
      // The final result must end up in inx.
      objcopy (inx, res, nr);
    }
  } else {
    // Each part has length 1, so the array is in reversed order and unique.
    // Reverse if ascending is needed.
    if (ord == Sort::Ascending) {
      GenSort<uInt>::reverse (inx, inx, nr);
    }
  }
  return nr;
}  

template<class T>
uInt* GenSortIndirect<T>::merge (const T* data, uInt* inx, uInt* tmp, uInt nr,
                                 uInt* index, uInt nparts)
{
  uInt* a = inx;
  uInt* b = tmp;
  int np = nparts;
  // If the nr of parts is odd, the last part is not merged. To avoid having
  // to copy it to the other array, a pointer 'last' is kept.
  // Note that merging the previous part with the last part works fine, even
  // if the last part is in the same buffer.
  uInt* last = inx + index[np-1];
  while (np > 1) {
  // Use ifdef to avoid compiler warning.
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
    for (int i=0; i<np; i+=2) {
      if (i < np-1) {
        // Merge 2 subsequent parts of the array.
	uInt* f1 = a+index[i];
	uInt* f2 = a+index[i+1];
	uInt* to = b+index[i];
	uInt na = index[i+1]-index[i];
	uInt nb = index[i+2]-index[i+1];
        if (i == np-2) {
          //cout<<"swap last np=" <<np<<endl;
          f2 = last;
          last = to;
        }
	uInt ia=0, ib=0, k=0;
	while (ia < na && ib < nb) {
	  if (data[f1[ia]] <= data[f2[ib]]) {
	    to[k] = f1[ia++];
	  } else {
	    to[k] = f2[ib++];
	  }
	  k++;
	}
	if (ia < na) {
	  for (uInt p=ia; p<na; p++,k++) to[k] = f1[p];
	} else {
	  for (uInt p=ib; p<nb; p++,k++) to[k] = f2[p];
	}
      }
    }
    // Collapse the index.
    int k=0;
    for (int i=0; i<np; i+=2) index[k++] = index[i];
    index[k] = nr;
    np = k;
    // Swap the index target and destination.
    uInt* c = a;
    a = b;
    b = c;
  }
  return a;
}



template<class T>
void GenSortIndirect<T>::quickSortAsc (uInt* inx, const T* data, Int nr,
                                       Bool multiThread)
{
    if (nr <= 32) {
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
    sf++;
    sl--;
    for (;;) {
	while (data[*sf] < partVal
	       ||  (partVal == data[*sf]  &&  *sf < partInx)) {
          sf++;
        }
	while (data[*sl] > partVal
	       ||  (partVal == data[*sl]  &&  *sl > partInx)) {
          sl--;
        }
	if (sf >= sl) break;
	swapInx (*sf, *sl);
    }
    swapInx (*sf, inx[nr-1]);
    Int n = sf-inx;
    if (multiThread) {
#ifdef _OPENMP
#pragma omp parallel for
#endif
      for (int thr=0; thr<2; ++thr) {
        if (thr==0) quickSortAsc (inx, data, n);
        if (thr==1) quickSortAsc (sf+1, data, nr-n-1);
      }
    } else {
      quickSortAsc (inx, data, n);
      quickSortAsc (sf+1, data, nr-n-1);
    }
}

// Find the k-th largest element using a partial quicksort.
template<class T>
uInt GenSortIndirect<T>::kthLargest (T* data, uInt nr, uInt k)
{
    if (k >= nr) {
	throw (AipsError ("kthLargest(data, nr, k): k must be < nr"));
    }
    // Create and fill an index vector.
    Vector<uInt> indexVector(nr);
    indgen(indexVector);
    uInt* inx = indexVector.data();
    Int st = 0;
    Int end = Int(nr) - 1;
    // Partition until a set of 1 or 2 elements is left.
    while (end > st+1) {
	// Choose a partition element by taking the median of the
	// first, middle and last element.
	// Store the partition element at the end.
	// Do not use Sedgewick\'s advise to store the partition element in
	// data[nr-2]. This has dramatic results for reversed ordered arrays.
	Int i = (st+end)/2;                      // middle element
	uInt* sf = inx+st;                       // first element
	uInt* sl = inx+end;                      // last element
	if (data[inx[i]] < data[*sf])
	    swapInx (inx[i], *sf);
	if (data[*sl] < data[*sf])
	    swapInx (*sl, *sf);
	if (data[inx[i]] < data[*sl])
	    swapInx (inx[i], *sl);
        T partVal = data[*sl];                   // partition element
	// Now partition until the pointers cross.
	for (;;) {
	    while (data[*++sf] < partVal) ;
	    while (data[*--sl] > partVal) ;
	    if (sf >= sl) break;
	    swapInx (*sf, *sl);
	}
	swapInx (*sf, inx[end]);
	// Determine index of partitioning and update the start and end
	// to take left or right part.
	i = sf-inx;
	if (i <= Int(k)) st = i;
	if (i >= Int(k)) end = i;
    }
    if (end == st+1) {
      if (data[inx[st]] > data[inx[end]]) {
	swapInx (inx[st], inx[end]);
      }
    }
    return inx[k];
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
	while (j>0  &&  isAscending (data, inx[j-1], cur)) {
	    inx[j] = inx[j-1];
            j--;
	}
	inx[j] = cur;
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
	while (j>0  &&  data[inx[j-1]] > data[cur]) {
            j--;
	}
	if (j <= 0  ||  !(data[inx[j-1]] == data[cur])) {   // no equal key
	    for (k=n-1; k>=j; k--) {
		inx[k+1] = inx[k];               // now shift to right
	    }
	    inx[j] = cur;                        // insert in right place
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

} //# NAMESPACE CASACORE - END

#endif
