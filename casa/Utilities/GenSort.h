//# GenSort.h: General sort functions
//# Copyright (C) 1993,1994,1995,1996,1997,1999
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

#ifndef CASA_GENSORT_H
#define CASA_GENSORT_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Sort.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations.
template<class T> class Array;
template<class T> class Vector;
template<class T> class Block;

// <summary> General in-place sort functions </summary>
// <use visibility=local>
// <reviewed reviewer="Friso Olnon" date="1995/03/16" tests="tGenSort" demos="">

// <synopsis> 
//
// The static member functions of this templated class are highly optimized
// sort functions.  They do an in-place sort of an array of values.  The
// functions are templated, so they can in principle be used with any
// data type. However, if used with non-builtin data types, their
// class must provide certain functions (see <em>Template Type Argument
// Requirements</em>).
//
// If it is impossible or too expensive to define these functions, the
// <linkto class=Sort>Sort</linkto> class can be used instead. This sorts
// indirectly using an index array. Instead of the functions mentioned
// above it requires a comparison routine.
//
// The <src>GenSort</src> functions can sort:
// <ul>
//  <li> C-arrays of values;
//  <li> <src>Array</src>s of values -- the array can have any shape
//         and the increment can be >1;
//  <li> <src>Block</src>s of values -- there is a special function to
//         sort less elements than the size of the <src>Block</src>.
// </ul>
//
// The sort order can be specified in the order field:
// <dl>
//   <dt> <src>Sort::Ascending</src> (default),
//   <dt> <src>Sort::Descending</src>.
// </dl>
//
// Previously the sort algorithm to use could be given in the options field.
// <dl>
//   <dt> <src>Sort::QuickSort</src> (default)
//   <dd> is the fastest. It is about 4-6 times faster
//     than the qsort function on the SUN. No worst case has been
//     found, even not for cases where qsort is terribly slow.
//   <dt> <src>Sort::HeapSort</src>
//   <dd> is about twice as slow as quicksort.
//     It has the advantage that the worst case is always o(n*log(n)),
//     while quicksort can have hypothetical inputs with o(n*n).
//   <dt> <src>Sort::InsSort</src>
//   <dd> is o(n*n) for random inputs. It is, however, the
//     only stable sort (i.e. equal values remain in the original order).
// </dl>
// However, these options are not used anymore because the sort now always
// uses a merge sort that is equally fast for random input and much faster for
// degenerated cases like an already ordered or reversely ordered array.
// Furthermore, merge sort is always stable and will be parallelized if OpenMP
// support is enabled giving a 6-fold speedup on 8 cores.
// <br><src>Sort::NoDuplicates</src> in the options field indicates that
// duplicate values will be removed (only the first occurrance is kept).
// <br>The previous sort functionality is still available through the functions
// quickSort, heapSort, and insSort.
// <p>
// All the sort functions return the number of values sorted as their
// function value; when duplicate values have been removed, the number of
// unique valuess will be returned.
// <p>
// The class also provides a function to find the k-th largest value
// in an array of values. This uses a stripped-down version of quicksort
// and is at least 6 times faster than a full quicksort.
// </synopsis> 

// <templating arg=T>
//   <li> <src>operator=</src> to assign when swapping elements
//   <li> <src>operator<</src>, <src>operator></src> and
//        <src>operator==</src>  to compare elements
//   <li> default constructor to allocate a temporary
// </templating>

template<class T> class GenSort
{
public:

    // Sort a C-array containing <src>nr</src> <src>T</src>-type objects.
    // The sort is done in place and is always stable (thus equal keys keep
    // their original order). It returns the number of values, which
    // can be different if a NoDuplicates sort is done.
    // <br>Insertion sort is used for short arrays (<50 elements). Otherwise,
    // a merge sort is used which will be parallelized if casacore is built
    // with OpenMP support.
    // <group>
    static uInt sort (T*, uInt nr, Sort::Order = Sort::Ascending,
		      int options = 0);

    static uInt sort (Array<T>&, Sort::Order = Sort::Ascending,
		      int options = 0);

    static uInt sort (Block<T>&, uInt nr, Sort::Order = Sort::Ascending,
		      int options = 0);
    // <group>
  
    // Find the k-th largest value.
    // <br>Note: it does a partial quicksort, thus the data array gets changed.
    static T kthLargest (T* data, uInt nr, uInt k);

    // Sort C-array using quicksort.
    static uInt quickSort (T*, uInt nr, Sort::Order = Sort::Ascending,
                           int options = 0);
    // Sort C-array using heapsort.
    static uInt heapSort   (T*, uInt nr, Sort::Order = Sort::Ascending,
                           int options = 0);
    // Sort C-array using insertion sort.
    static uInt insSort    (T*, uInt nr, Sort::Order = Sort::Ascending,
                           int options = 0);
    // Sort C-array using parallel merge sort (using OpenMP).
    // By default OpenMP determines the number of threads that can be used.
    static uInt parSort    (T*, uInt nr, Sort::Order = Sort::Ascending,
                            int options = 0, int nthread = 0);

    // Swap 2 elements in array.
    static inline void swap (T&, T&);

    // Reverse the elements in <src>res</src> and put them into <src>data</src>.
    // Care is taken if both pointers reference the same data.
    static void reverse (T* data, const T* res, uInt nrrec);

private:
    // The<src>data</src> buffer is divided in <src>nparts</src> parts.
    // In each part the values are in ascending order.
    // The index tells the nr of elements in each part.
    // Recursively each two subsequent parts are merged until only part is left
    // (giving the sorted array). Alternately <src>data</src> and <src>tmp</src>
    // are used for the merge result. The pointer containing the final result
    // is returned.
    // <br>If possible, merging the parts is done in parallel (using OpenMP).
    static T* merge (T* data, T* tmp, uInt nrrec, uInt* index,
                     uInt nparts);

    // Quicksort in ascending order.
    static void quickSortAsc (T*, Int, Bool multiThread=False);

    // Heapsort in ascending order.
    static void heapSortAsc (T*, Int);
    // Helper function for ascending heapsort.
    static void heapAscSiftDown (Int, Int, T*);

    // Insertion sort in ascending order.
    static uInt insSortAsc (T*, Int, int option);
    // Insertion sort in ascending order allowing duplicates.
    // This is also used by quicksort for its last steps.
    static uInt insSortAscDup (T*, Int);
    // Insertion sort in ascending order allowing no duplicates.
    // This is also used by the other sort algorithms to skip duplicates.
    static uInt insSortAscNoDup (T*, Int);
};


// <summary> General indirect sort functions </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="" demos="">

// <synopsis> 
// This class is similar to <linkto class=GenSort>GenSort</linkto>.
// The only difference is that the functions in this class sort
// indirectly instead of in-place.
// They return the result of the sort as an sorted vector of indices
// This is slower, because an extra indirection is involved in each
// comparison. However, this sort allows to sort const data.
// Another advantage is that this sort is always stable (i.e. equal
// values are kept in their original order).

template<class T> class GenSortIndirect
{
public:

    // Sort a C-array containing <src>nr</src> <src>T</src>-type objects.
    // The resulting index vector gives the sorted indices.
    static uInt sort (Vector<uInt>& indexVector, const T* data, uInt nr,
		      Sort::Order = Sort::Ascending,
		      int options = Sort::QuickSort);

    // Sort a C-array containing <src>nr</src> <src>T</src>-type objects.
    // The resulting index vector gives the sorted indices.
    static uInt sort (Vector<uInt>& indexVector, const Array<T>& data,
		      Sort::Order = Sort::Ascending,
		      int options = Sort::QuickSort);

    // Sort a C-array containing <src>nr</src> <src>T</src>-type objects.
    // The resulting index vector gives the sorted indices.
    static uInt sort (Vector<uInt>& indexVector, const Block<T>& data, uInt nr,
		      Sort::Order = Sort::Ascending,
		      int options = Sort::QuickSort);

    // Find the index of the k-th largest value.
    static uInt kthLargest (T* data, uInt nr, uInt k);

    // Sort container using quicksort.
    // The argument <src>inx</src> gives the index defining the order of the
    // values in the data array. Its length must be at least <src>nr</src>
    // and it must be filled with the index values of the data.
    // Usually this is 0..nr, but it could contain a selection of the data.
    static uInt quickSort (uInt* inx, const T* data,
			   uInt nr, Sort::Order, int options);
    // Sort container using heapsort.
    static uInt heapSort (uInt* inx, const T* data,
			  uInt nr, Sort::Order, int options);
    // Sort container using insertion sort.
    static uInt insSort (uInt* inx, const T* data,
			 uInt nr, Sort::Order, int options);
    // Sort container using parallel merge sort (using OpenMP).
    // By default the maximum number of threads is used.
    static uInt parSort (uInt* inx, const T* data,
			 uInt nr, Sort::Order, int options, int nthreads=0);

private:
    // Swap 2 indices.
    static inline void swapInx (uInt& index1, uInt& index2);

    // The<src>data</src> buffer is divided in <src>nparts</src> parts.
    // In each part the values are in ascending order.
    // The index tells the nr of elements in each part.
    // Recursively each two subsequent parts are merged until only part is left
    // (giving the sorted array). Alternately <src>data</src> and <src>tmp</src>
    // are used for the merge result. The pointer containing the final result
    // is returned.
    // <br>If possible, merging the parts is done in parallel (using OpenMP).
    static uInt* merge (const T* data, uInt* inx, uInt* tmp, uInt nrrec,
                        uInt* index, uInt nparts);

    // Check if 2 values are in ascending order.
    // When equal, the order is correct if index1<index2.
    static inline int isAscending (const T* data, Int index1, Int index2);


    // Quicksort in ascending order.
    static void quickSortAsc (uInt* inx, const T*, Int nr,
                              Bool multiThread=False);

    // Heapsort in ascending order.
    static void heapSortAsc (uInt* inx, const T*, Int nr);
    // Helper function for ascending heapsort.
    static void heapAscSiftDown (uInt* inx, Int, Int, const T*);

    // Insertion sort in ascending order.
    static uInt insSortAsc (uInt* inx, const T*, Int nr, int option);
    // Insertion sort in ascending order allowing duplicates.
    // This is also used by quicksort for its last steps.
    static uInt insSortAscDup (uInt* inx, const T*, Int nr);
    // Insertion sort in ascending order allowing no duplicates.
    // This is also used by the other sort algorithms to skip duplicates.
    static uInt insSortAscNoDup (uInt* inx, const T*, Int nr);
};


// <summary> Global in-place sort functions </summary>

// The following global functions are easier to use than the static
// <linkto class=GenSort>GenSort</linkto> member functions.
// They do an in-place sort of data, thus the data themselves are moved
// ending up in the requested order.
// <p>
// The default sorting method is QuickSort, which is the fastest.
// However, there are pathological cases where it can be slow.
// HeapSort is about twice a slow, but its speed is guaranteed.
// InsSort (insertion sort) can be very, very slow, but it is the only
// stable sort method (i.e. equal values are kept in their original order).
// However, <linkto name=genSortIndirect> indirect sorting methods </linkto>
// are available to make QuickSort and HeapSort stable.
// <p>
// All sort methods have an option to skip duplicate values. This is the
// only case where the returned number of values can be less than the
// original number of values.

// <group name=genSortInPlace>

template<class T>
inline
uInt genSort (T* data, uInt nr,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSort<T>::sort (data, nr, order, options); }

template<class T>
inline
uInt genSort (Array<T>& data,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSort<T>::sort (data, order, options); }

template<class T>
inline
uInt genSort (Block<T>& data,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSort<T>::sort (data, data.nelements(), order, options); }

template<class T>
inline
uInt genSort (Block<T>& data, uInt nr,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSort<T>::sort (data, nr, order, options); }
// </group>


// <summary> Global indirect sort functions </summary>

// The following global functions easier to use than the static
// <linkto class=GenSortIndirect>GenSortIndirect</linkto> member functions.
// They do an indirect sort of data, thus the data themselves are not moved.
// Rather an index vector is returned giving the sorted data indices.
// <p>
// The sorting method used is merge sort, which is always stable.
// It is the fastest, especially if it can use multiple threads.
// <p>
// Unlike the <linkto name=genSortInPlace> in-place sorting methods
// </linkto>, all indirect sorting methods are stable (i.e. equal
// values are left in their original order).
// <p>
// All sort methods have an option to skip duplicate values. This is the
// only case where the returned number of values can be less than the
// original number of values.

// <group name=genSortIndirect>

template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const T* data, uInt nr,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSortIndirect<T>::sort (indexVector, data, nr, order, options); }

template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Array<T>& data,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSortIndirect<T>::sort (indexVector, data, order, options); }

template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSortIndirect<T>::sort (indexVector, data, data.nelements(),
                                     order, options); }

template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data, uInt nr,
              Sort::Order order = Sort::Ascending, int options=0)
  { return GenSortIndirect<T>::sort (indexVector, data, nr, order, options); }
// </group>



// Implement inline member functions.

template<class T>
inline void GenSort<T>::swap (T& l, T& r)
{
    T t = l;
    l = r;
    r = t;
}
template<class T>
inline void GenSortIndirect<T>::swapInx (uInt& i, uInt& j)
{
    uInt t = i;
    i = j;
    j = t;
}
template<class T>
inline int GenSortIndirect<T>::isAscending (const T* data, Int i, Int j)
{
    return (data[i] > data[j]  ||  (data[i] == data[j]  &&  i > j));
}



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/GenSort.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
