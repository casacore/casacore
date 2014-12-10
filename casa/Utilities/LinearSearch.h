//# LinearSearch.h: Linear search through linear data structures
//# Copyright (C) 1997,1999
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


#ifndef CASA_LINEARSEARCH_H
#define CASA_LINEARSEARCH_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Linear search a linear data structure.
// </summary>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLinearSearch" demos="">
// </reviewed>

// <synopsis>
// These linear search functions work on linear data structures
// which have operator() or operator[] defined on them (<i>e.g.</i>
// C-array, Vector, IPosition, Block, ScalarColumn, <i>etc.</i>)
// Two versions of the functions are provided, one which uses
// parentheses () for indexing, one which uses square brackets [] (obviously
// the latter one can also be used for ordinary C-style pointers and arrays).
// It is assumed that the container uses zero-based indexing.
//
// The returned index is in the range [0..n-1]. When the value is
// not found, -1 is returned.
// <note role=tip>
// While normally you want to search a container with indices in the range
// <src>[0 ... n-1]</src>, any desired lower bound may be used instead.
// </note>
// <note role=caution>
// Linear searching should only be used for small arrays.
// For larger arrays sort and
// <linkto group=BinarySearch.h#binarysearch>binarySearch</linkto>
// should be used.
// </note>
// </synopsis>
//
// <example>
// <srcblock>
// Vector<Int> vi;
// ...  // Sets vi somehow
// Int val;
// Bool found;
// while (cin >> val  &&  val != -999) {
//     Int where = linearSearch(found, vi, val, vi.nelements());
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
// Neil Killeen needed a linear search on a Vector.
// Modelling it after BinarySearch was the logical step to take.
// </motivation>
//
// <templating arg=Container>
//    <li> operator(Int) or operator[Int] needs to be defined.
//    <li> The index must be zero based.
//    <li> The result of that indexing must be an expression that can be 
//         compared with an object of class ElType. Normally in fact it would
//         be a temporary of class ElType.
//    <li> Member function nelements() is needed when the shorthand is taken.
// </templating>
// <templating arg=ElType>
//    <li> The equal operator (==) need to be defined.
// </templating>
//
// <todo asof="yyyy/mm/dd">
//   <li> I suspect that an implementation is possible that only calls
//        operator() or [] once during each evaluation of the while loop.
//   <li> MACROize implementation so that code isn't repeated twice. Or, 
//        possibly implement one using the other (e.g. by introducing an adapter
//        class that turns (i) into [i].
// </todo>


// <group name=linearsearch>  

// Search <i>container</i> for <i>value</i>. There are assumed to be at least
// <i>n</i> elements in the container. The container will be searched for
// indices in the range <src>[lower ... lower + n - 1]</src> Return the index
// of the first element which is greater than or equal to (ascending order) or
// less than or equal to (descending order) the value.
// When not found, -1 is returned and found is set to False.
//# GvD 19971008: The functions need different names, because g++ gives errors
//# when instantiating.
// <group>
// This version of the function is for containers that use () for indexing.
template<class Container, class ElType>
Int linearSearch1 (const Container& container, const ElType& value,
		   uInt lower = 0);
template<class Container, class ElType>
Int linearSearch (Bool& found, const Container& container, 
		  const ElType& value, uInt n, uInt lower=0);
// This version of the function is for containers that use [] for indexing.
template<class Container, class ElType>
Int linearSearchBrackets1 (const Container& container, const ElType& value,
			   uInt lower = 0);
template<class Container, class ElType>
Int linearSearchBrackets (Bool& found, const Container& container, 
			  const ElType& value, uInt n, uInt lower=0);
// </group>
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/LinearSearch.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
