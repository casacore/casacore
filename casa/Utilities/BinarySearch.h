//# BinarySearch.h: Binary search through linear, sorted, data structures
//# Copyright (C) 1995,1996,1999
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
//#
//# $Id$


#ifndef CASA_BINARYSEARCH_H
#define CASA_BINARYSEARCH_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Binary search a sorted, linear, data structure.
// </summary>

// <reviewed reviewer="Ger van Diepen" date="1995/03/31" tests="tBinarySearch" demos="">
// </reviewed>

// <synopsis>
// These binary search functions work on sorted, linear data structures
// which have operator() or operator[] defined on them (<i>e.g.</i>
// C-array, Vector, IPosition, Block, ScalarColumn, <i>etc.</i>)
// Two versions of the functions are provided, one which uses
// parentheses () for indexing, one which uses square brackets [] (obviously
// the latter one can also be used for ordinary C-style pointers and arrays).
// It is assumed that the container uses zero-based indexing.
//
// The container must be sorted (sorting is available through the
// <linkto class="Sort">Sort</linkto> and 
// <linkto class="GenSort">GenSort</linkto>
// classes, and from various 
// <linkto class="Table">Table</linkto> sort functions). The returned index
// is in the range [0..n] inclusive. That is, from the first element of the
// container to one past the last element of the container (zero-based indices).
// If the container is sorted in ascending order, the returned index is the
// first one whose element is greater than or equal to the searched for value.
// If it is sorted in descending order, the returned index is the first which
// is less than or equal to the searched for value. That is, the returned
// index gives the position at which the value would be inserted (possibly
// either at the end, or requiring the existing values to be "pushed" to the
// right) maintaining the sort order. Obviously index n can only be
// returned if the value searched for is past the end of the array, thus
// has to be inserted at the end.
//
// The functions determine for themselves whether the container is sorted in
// ascending or descending order by comparing the first and last element.
// <note role=tip>
// While normally you want to search a container with indices in the range
// <src>[0 ... n-1]</src>, any desired lower bound may be used instead.
// </note>
// <note role=warning>
// The functions do not check if the container is valid, <i>i.e.</i> if
// the container is sorted and if the container does not contain duplicate
// values.
// </note>
//
// These functions loosely follow some written by Ger van Diepen in a more
// specialized context.
// </synopsis>
//
// <example>
// <srcblock>
// Vector<Int> vi;
// ...  // Sets vi somehow
// genSort(vi);
// Int val;
// Bool found;
// while (cin >> val && val != -999) {
//     Int where = binarySearch(found, vi, val, vi.nelements());
//     if (found) {
//       cout << "Found " << val << " at position " << where << endl;
//     } else {
//       cout << val << " is not in the vector, but it belongs at " <<
//            where << endl;
//     }
// } 
// </srcblock>
// </example>
//
// <motivation>
// I found that I (BEG) was writing binary search functions several times, 
// for example when checking whether the cached off and gain scans in time
// sorted data needed to be refilled. It generally seems like a useful little
// utility function.
// </motivation>
//
// <templating arg=Container>
//    <li> operator(Int) or operator[Int] needs to be defined.
//    <li> The index must be zero based.
//    <li> The result of that indexing must be an expression that can be 
//         compared with an object of class ElType. Normally in fact it would
//         be a temporary of class ElType.
// </templating>
// <templating arg=ElType>
//    <li> The less than operator (<) and greater than (>) operators need to
//         be defined, and have their usual ordering relations.
// </templating>
//
// <todo asof="yyyy/mm/dd">
//   <li> I suspect that an implementation is possible that only calls
//        operator() or [] once during each evaluation of the while loop.
//   <li> MACROize implementation so that code isn't repeated twice. Or, 
//        possibly implement one using the other (e.g. by introducing an adapter
//        class that turns (i) into [i].
// </todo>


// <group name=binarysearch>  

// Search <i>container</i> for <i>value</i>. There are assumed to be at least
// <i>n</i> elements in the container. The container will be searched for
// indices in the range <src>[lower ... lower + n - 1]</src> Return the index
// of the first element which is greater than or equal to (ascending order) or
// less than or equal to (descending order) the value.
// <group>
// This version of the function is for containers that use () for indexing.
template<class Container, class ElType>
     Int binarySearch(Bool &found, const Container &container, 
		      const ElType &value, uInt n, Int lower=0);
// This version of the function is for containers that use [] for indexing.
template<class Container, class ElType>
     Int binarySearchBrackets(Bool &found, const Container &container, 
			      const ElType &value, uInt n, Int lower=0);
// </group>
// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/BinarySearch.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
