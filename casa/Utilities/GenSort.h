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

#include <casa/aips.h>
#include <casa/Utilities/Sort.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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
// A sort algorithm can be given in the options field:
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
// <src>Sort::NoDuplicates</src> in the options field indicates that
// duplicate values should be removed. Multiple options can be given by
// or-ing them, e.g. <src>Sort::HeapSort | Sort::NoDuplicates</src>.
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
    // The sort is done in place.
    static uInt sort (T*, uInt nr, Sort::Order = Sort::Ascending,
		      int options = Sort::QuickSort);

    // Sort an <src>Array</src> of <src>T</src>-type objects
    // The sort is done in place.
    static uInt sort (Array<T>&, Sort::Order = Sort::Ascending,
		      int options = Sort::QuickSort);

    // Sort <src>nr</src> <src>T</src>-type objects in the <src>Block</src>.
    // The sort is done in place.
    static uInt sort (Block<T>&, uInt nr, Sort::Order = Sort::Ascending,
		      int options = Sort::QuickSort);

    // Find the k-th largest value.
    // Note: it does a partial sort, thus the data array gets changed..
    static T kthLargest (T* data, uInt nr, uInt k);

private:
    // Sort C-array using quicksort.
    static uInt quickSort (T*, uInt nr, Sort::Order, int options);
    // Sort C-array using heapsort.
    static uInt heapSort  (T*, uInt nr, Sort::Order, int options);
    // Sort C-array using insertion sort.
    static uInt insSort   (T*, uInt nr, Sort::Order, int options);

    // Swap 2 elements in array.
    static inline void swap (T&, T&);

    // Quicksort in ascending order.
    static void quickSortAsc (T*, Int);
    // Quicksort in descending order.
    static void quickSortDesc (T*, Int);

    // Heapsort in ascending order.
    static void heapSortAsc (T*, Int);
    // Heapsort in descending order.
    static void heapSortDesc (T*, Int);
    // Helper function for ascending heapsort.
    static void heapAscSiftDown (Int, Int, T*);
    // Helper function for descending heapsort.
    static void heapDescSiftDown (Int, Int, T*);

    // Insertion sort in ascending order.
    static uInt insSortAsc (T*, Int, int option);
    // Insertion sort in descending order.
    static uInt insSortDesc (T*, Int, int option);
    // Insertion sort in ascending order allowing duplicates.
    // This is also used by quicksort for its last steps.
    static uInt insSortAscDup (T*, Int);
    // Insertion sort in descending order allowing duplicates.
    // This is also used by quicksort for its last steps.
    static uInt insSortDescDup (T*, Int);
    // Insertion sort in ascending order allowing no duplicates.
    // This is also used by the other sort algorithms to skip duplicates.
    static uInt insSortAscNoDup (T*, Int);
    // Insertion sort in descending order allowing no duplicates.
    // This is also used by the other sort algorithms to skip duplicates.
    static uInt insSortDescNoDup (T*, Int);
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

private:
    // Sort container using quicksort.
    static uInt quickSort (uInt* inx, const T* data,
			   uInt nr, Sort::Order, int options);
    // Sort container using heapsort.
    static uInt heapSort (uInt* inx, const T* data,
			  uInt nr, Sort::Order, int options);
    // Sort container using insertion sort.
    static uInt insSort (uInt* inx, const T* data,
			 uInt nr, Sort::Order, int options);

    // Swap 2 indices.
    static inline void swapInx (uInt& index1, uInt& index2);

    // Check if 2 values are in ascending order.
    // When equal, the order is correct if index1<index2.
    static inline int isAscending (const T* data, Int index1, Int index2);

    // Check if 2 values are in descending order.
    // When equal, the order is correct if index1<index2.
    static inline int isDescending (const T*, Int index1, Int index2);


    // Quicksort in ascending order.
    static void quickSortAsc (uInt* inx, const T*, Int nr);
    // Quicksort in descending order.
    static void quickSortDesc (uInt* inx, const T*, Int nr);

    // Heapsort in ascending order.
    static void heapSortAsc (uInt* inx, const T*, Int nr);
    // Heapsort in descending order.
    static void heapSortDesc (uInt* inx, const T*, Int nr);
    // Helper function for ascending heapsort.
    static void heapAscSiftDown (uInt* inx, Int, Int, const T*);
    // Helper function for descending heapsort.
    static void heapDescSiftDown (uInt* inx, Int, Int, const T*);

    // Insertion sort in ascending order.
    static uInt insSortAsc (uInt* inx, const T*, Int nr, int option);
    // Insertion sort in descending order.
    static uInt insSortDesc (uInt* inx, const T*, Int nr, int option);
    // Insertion sort in ascending order allowing duplicates.
    // This is also used by quicksort for its last steps.
    static uInt insSortAscDup (uInt* inx, const T*, Int nr);
    // Insertion sort in descending order allowing duplicates.
    // This is also used by quicksort for its last steps.
    static uInt insSortDescDup (uInt* inx, const T*, Int nr);
    // Insertion sort in ascending order allowing no duplicates.
    // This is also used by the other sort algorithms to skip duplicates.
    static uInt insSortAscNoDup (uInt* inx, const T*, Int nr);
    // Insertion sort in descending order allowing no duplicates.
    // This is also used by the other sort algorithms to skip duplicates.
    static uInt insSortDescNoDup (uInt* inx, const T*, Int nr);
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

// In-place sort in ascending order using QuickSort.
// <group>
template<class T>
inline
uInt genSort (T* data, uInt nr)
    { return GenSort<T>::sort (data, nr, Sort::Ascending, Sort::QuickSort); }

template<class T>
inline
uInt genSort (Array<T>& data)
    { return GenSort<T>::sort (data, Sort::Ascending, Sort::QuickSort); }

template<class T>
inline
uInt genSort (Block<T>& data)
    { return GenSort<T>::sort (data, data.nelements(), Sort::Ascending,
			       Sort::QuickSort); }

template<class T>
inline
uInt genSort (Block<T>& data, uInt nr)
    { return GenSort<T>::sort (data, nr, Sort::Ascending, Sort::QuickSort); }
// </group>

// In-place sort in ascending order using given option.
// This option gives the sort method (Sort::QuickSort, Sort::HeapSort
// or Sort::InsSort) and/or the option Sort::NoDuplicates.
// <group>
template<class T>
inline
uInt genSort (T* data, uInt nr, int options)
    { return GenSort<T>::sort (data, nr, Sort::Ascending, options); }
template<class T>
inline
uInt genSort (Array<T>& data, int options)
    { return GenSort<T>::sort (data, Sort::Ascending, options); }
template<class T>
inline
uInt genSort (Block<T>& data, int options)
    { return GenSort<T>::sort (data, data.nelements(), Sort::Ascending,
			       options); }
template<class T>
inline
uInt genSort (Block<T>& data, uInt nr, int options)
    { return GenSort<T>::sort (data, nr, Sort::Ascending, options); }
// </group>

// In-place sort in given order using QuickSort.
// The order must be Sort::Ascending or Sort::Descending.
// <group>
template<class T>
inline
uInt genSort (T* data, uInt nr, Sort::Order order)
    { return GenSort<T>::sort (data, nr, order, Sort::QuickSort); }
template<class T>
inline
uInt genSort (Array<T>& data, Sort::Order order)
    { return GenSort<T>::sort (data, order, Sort::QuickSort); }
template<class T>
inline
uInt genSort (Block<T>& data, Sort::Order order)
    { return GenSort<T>::sort (data, data.nelements(), order,
			       Sort::QuickSort); } 
template<class T>
inline
uInt genSort (Block<T>& data, uInt nr, Sort::Order order)
    { return GenSort<T>::sort (data, nr, order, Sort::QuickSort); }
// </group>

// In-place sort in given order using given option.
// This option gives the sort method (Sort::QuickSort, Sort::HeapSort,
// or Sort::InsSort) and/or the option Sort::NoDuplicates.
// The order must be Sort::Ascending or Sort::Descending.
// <group>
template<class T>
inline
uInt genSort (T* data, uInt nr, Sort::Order order, int options)
    { return GenSort<T>::sort (data, nr, order, options); }
template<class T>
inline
uInt genSort (Array<T>& data, Sort::Order order, int options)
    { return GenSort<T>::sort (data, order, options); }
template<class T>
inline
uInt genSort (Block<T>& data, Sort::Order order, int options)
    { return GenSort<T>::sort (data, data.nelements(), order, options); }
template<class T>
inline
uInt genSort (Block<T>& data, uInt nr, Sort::Order order, int options)
    { return GenSort<T>::sort (data, nr, order, options); }
// </group>

// </group>



// <summary> Global indirect sort functions </summary>

// The following global functions easier to use than the static
// <linkto class=GenSortIndirect>GenSortIndirect</linkto> member functions.
// They do an indirect sort of data, thus the data themselves are not moved.
// Rather an index vector is returned giving the sorted data indices.
// <p>
// The default sorting method is QuickSort, which is the fastest.
// However, there are pathological cases where it can be slow.
// HeapSort is about twice a slow, but its speed is guaranteed.
// InsSort (insertion sort) can be very, very slow.
// <p>
// Unlike the <linkto name=genSortInPlace> in-place sorting methods
// </linkto>, all indirect sorting methods are stable (i.e. equal
// values are left in their original order).
// <p>
// All sort methods have an option to skip duplicate values. This is the
// only case where the returned number of values can be less than the
// original number of values.

// <group name=genSortIndirect>

// Indirect sort in ascending order using QuickSort.
// <group>
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const T* data, uInt nr)
    { return GenSortIndirect<T>::sort (indexVector, data, nr, Sort::Ascending,
				       Sort::QuickSort); }

template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Array<T>& data)
    { return GenSortIndirect<T>::sort (indexVector, data, Sort::Ascending,
				       Sort::QuickSort); }

template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data)
    { return GenSortIndirect<T>::sort (indexVector, data, data.nelements(),
				       Sort::Ascending, Sort::QuickSort); }

template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data, uInt nr)
    { return GenSortIndirect<T>::sort (indexVector, data, nr,
				       Sort::Ascending, Sort::QuickSort); }
// </group>

// Indirect sort in ascending order using given option.
// This option gives the sort method (Sort::QuickSort, Sort::HeapSort,
// or Sort::InsSort) and/or the option Sort::NoDuplicates.
// <group>
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const T* data, uInt nr, int options)
    { return GenSortIndirect<T>::sort (indexVector, data, nr,
				       Sort::Ascending, options); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Array<T>& data, int options)
    { return GenSortIndirect<T>::sort (indexVector, data,
				       Sort::Ascending, options); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data, int options)
    { return GenSortIndirect<T>::sort (indexVector, data, data.nelements(),
				       Sort::Ascending, options); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data,
	      uInt nr, int options)
    { return GenSortIndirect<T>::sort (indexVector, data, nr,
				       Sort::Ascending, options); }
// </group>

// Indirect sort in given order using QuickSort.
// The order must be Sort::Ascending or Sort::Descending.
// <group>
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const T* data,
	      uInt nr, Sort::Order order)
    { return GenSortIndirect<T>::sort (indexVector, data, nr, order,
				       Sort::QuickSort); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Array<T>& data,
	      Sort::Order order)
    { return GenSortIndirect<T>::sort (indexVector, data, order,
				       Sort::QuickSort); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data,
	      Sort::Order order)
    { return GenSortIndirect<T>::sort (indexVector, data, data.nelements(),
				       order, Sort::QuickSort); } 
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data,
	      uInt nr, Sort::Order order)
    { return GenSortIndirect<T>::sort (indexVector, data, nr, order,
				       Sort::QuickSort); }
// </group>

// Indirect sort in given order using given option.
// This option gives the sort method (Sort::QuickSort, Sort::HeapSort,
// or Sort::InsSort) and/or the option Sort::NoDuplicates.
// The order must be Sort::Ascending or Sort::Descending.
// <group>
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const T* data, uInt nr,
	      Sort::Order order, int options)
    { return GenSortIndirect<T>::sort (indexVector, data, nr,
				       order, options); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Array<T>& data,
	      Sort::Order order, int options)
    { return GenSortIndirect<T>::sort (indexVector, data, order, options); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data,
	      Sort::Order order, int options)
    { return GenSortIndirect<T>::sort (indexVector, data, data.nelements(),
				       order, options); }
template<class T>
inline
uInt genSort (Vector<uInt>& indexVector, const Block<T>& data, uInt nr,
	      Sort::Order order, int options)
    { return GenSortIndirect<T>::sort (indexVector, data, nr,
				       order, options); }
// </group>

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
template<class T>
inline int GenSortIndirect<T>::isDescending (const T* data, Int i, Int j)
{
    return (data[i] < data[j]  ||  (data[i] == data[j]  &&  i > j));
}



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Utilities/GenSort.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
