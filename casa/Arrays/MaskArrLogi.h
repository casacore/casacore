//# MaskArrLogi.h: Element by element logical operations on masked arrays.
//# Copyright (C) 1993,1994,1995,1999,2001
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

#ifndef CASA_MASKARRLOGI_H
#define CASA_MASKARRLOGI_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/MaskLogiArr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Logical operations for MaskedArrays, and between MaskedArrays and Arrays.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMaskArrLogi tMaskArrExcp">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto group="LogiArray.h#LogicalArray">LogicalArray</linkto>
//   <li> <linkto class=MaskedArray>MaskedArray</linkto>
// </prerequisite>
//
// <etymology>
// MaskArrLogi is short for MaskedArrayLogical, which is too long by the
// old AIPS++ file naming conventions.  This file contains global functions
// which perform element by element logical operations on masked arrays.
// </etymology>
//
// <synopsis>
// These functions perform element by element logical operations on
// masked arrays.  With two arrays, they must both conform, and the result
// is done element by element, for those locations where the mask of the
// MaskedArray is True.  For two MaskedArrays, the "and" of the masks is used.
//
// There are two classes of functions.  One class returns a MaskedLogicalArray.
// In these functions, the value of an element of the MaskedLogicalArray is
// the value of the logical operation applied to the corresponding elements
// of the input MaskedArrays.  The other class of functions returns a single
// Bool.  The return value is True if the logical operation returns True for
// all elements of the input masked arrays for the "all" functions
// (e.g. allLE()), and returns True if the logical operation returns True for
// any elements of the input masked arrays for the "any" functions
// (e.g. anyLE()).  The functions which return a single Bool throw an exception
// if the AND of the masks of the input masked arrays has no True elements.
//
// For instance allLE (a, b) imples that every element of a is
// less than or equal to every element of b. Note that with this definition
// allLE (a, b) and allGE (a, b) can both be false (e.g. a = [1,0] b = [0,1]).
//
// NB comparison between two zero-sized arrays is not defined (should it
// throw an exception?).
// </synopsis>
//
// <example>
// <srcblock>
//   Vector<Int> a(10);
//   Vector<Int> b(10);
//   LogicalVector l(10);
//      . . .
//   l = a(a>0) < b(b>0);
// </srcblock>
// This example sets those elements of l where ((a>0) && (b>0)) to (a<b).
// Elements of l where !((a>0) && (b>0)) are unchanged.  The result of
// the comparison is a MaskedLogicalArray.  The assignment from this
// MaskedLogicalArray to the LogicalArray l only assigns those elements
// where the mask is True.
// </example>
//
// <example>
// <srcblock>
//   Vector<Int> a(10);
//   Vector<Int> b(10);
//   Bool result;
//      . . .
//   result = allLT (a(a>0), b(b>0));
// </srcblock>
// This example sets result to True if, for all elements where
// ((a>0) && (b>0)),  a<b.
// </example>
//
// <motivation>
// One wants to be able to mask arrays and perform logical operations on
// those masked arrays.  Since the masked arrays are only defined where
// the masks are True, the result must be a MaskedLogicalArray, or a single
// Bool.
// </motivation>
//
// <todo asof="$DATE:$>
//   <li> Reconsider where the origin of the returned LogicalArray should
//          be located.
// </todo>
//
// <linkfrom anchor="MaskedArray logical operations" classes="MaskedArray Array Vector Matrix Cube">
//    <here>MaskedArray logical operations</here> -- Logical operations
//    for MaskedArrays, and between MaskedArrays and Arrays.
// </linkfrom>
//
// <group name="MaskedArray logical operations">


// 
// Element by element comparisons between the "l" and "r" arrays. The result
// is true only if the comparison is true for every element of the arrays
// for which the mask of the MaskedArray is True.  For two MaskedArrays,
// the "and" of the masks is used.
//
// <thrown>
//   <li> ArrayConformanceError
//   <li> ArrayError
// </thrown>
//
// <group>
template<class T> Bool allLE (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool allLT (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool allGE (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool allGT (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool allEQ (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool allNE (const MaskedArray<T> &l, const Array<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> Bool allAND (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool allOR (const MaskedArray<T> &l, const Array<T> &r);
// </group>

template<class T> Bool allLE (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool allLT (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool allGE (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool allGT (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool allEQ (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool allNE (const Array<T> &l, const MaskedArray<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> Bool allAND (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool allOR (const Array<T> &l, const MaskedArray<T> &r);
// </group>

template<class T>
  Bool allLE (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool allLT (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool allGE (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool allGT (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool allEQ (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool allNE (const MaskedArray<T> &l, const MaskedArray<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T>
  Bool allAND (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool allOR (const MaskedArray<T> &l, const MaskedArray<T> &r);
// </group>

// </group>


// 
// Element by element comparisons between the "l" and "r" arrays. The result
// is a MaskedLogicalArray.
//
// The arrays must conform or an exception is thrown.
//
// <thrown>
//   <li> ArrayConformanceError
// </thrown>
//
// <group>
template<class T>
  MaskedLogicalArray operator <= (const MaskedArray<T> &l, const Array<T> &r);
template<class T>
  MaskedLogicalArray operator <  (const MaskedArray<T> &l, const Array<T> &r);
template<class T>
  MaskedLogicalArray operator >= (const MaskedArray<T> &l, const Array<T> &r);
template<class T>
  MaskedLogicalArray operator >  (const MaskedArray<T> &l, const Array<T> &r);
template<class T>
  MaskedLogicalArray operator == (const MaskedArray<T> &l, const Array<T> &r);
template<class T>
  MaskedLogicalArray operator != (const MaskedArray<T> &l, const Array<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T>
  MaskedLogicalArray operator && (const MaskedArray<T> &l, const Array<T> &r);
template<class T>
  MaskedLogicalArray operator || (const MaskedArray<T> &l, const Array<T> &r);
// </group>

template<class T>
  MaskedLogicalArray operator <= (const Array<T> &l, const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator <  (const Array<T> &l, const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator >= (const Array<T> &l, const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator >  (const Array<T> &l, const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator == (const Array<T> &l, const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator != (const Array<T> &l, const MaskedArray<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T>
  MaskedLogicalArray operator && (const Array<T> &l, const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator || (const Array<T> &l, const MaskedArray<T> &r);
// </group>

template<class T>
  MaskedLogicalArray operator <= (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator <  (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator >= (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator >  (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator == (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator != (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T>
  MaskedLogicalArray operator && (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
template<class T>
  MaskedLogicalArray operator || (const MaskedArray<T> &l,
                                  const MaskedArray<T> &r);
// </group>

// </group>


// 
// Logical negation of a MaskedArray.  This only makes sense if the array
// element type is logical valued.
template<class T>
MaskedLogicalArray operator ! (const MaskedArray<T> &marray);


// 
// Element by element comparisons between an array and a scalar, which
// behaves as if it were a conformant array filled with the value "val."
// The result is true only if the comparison is true for every element
// for which the mask of the MaskedArray is True.
// <thrown>
//   <li> ArrayError
// </thrown>
//
// <group>
template<class T> Bool allLE (const MaskedArray<T> &array, const T &val);
template<class T> Bool allLE (const T &val, const MaskedArray<T> &array);
template<class T> Bool allLT (const MaskedArray<T> &array, const T &val);
template<class T> Bool allLT (const T &val, const MaskedArray<T> &array);
template<class T> Bool allGE (const MaskedArray<T> &array, const T &val);
template<class T> Bool allGE (const T &val, const MaskedArray<T> &array);
template<class T> Bool allGT (const MaskedArray<T> &array, const T &val);
template<class T> Bool allGT (const T &val, const MaskedArray<T> &array);
template<class T> Bool allEQ (const MaskedArray<T> &array, const T &val);
template<class T> Bool allEQ (const T &val, const MaskedArray<T> &array);
template<class T> Bool allNE (const MaskedArray<T> &array, const T &val);
template<class T> Bool allNE (const T &val, const MaskedArray<T> &array);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> Bool allAND (const MaskedArray<T> &array, const T &val);
template<class T> Bool allAND (const T &val, const MaskedArray<T> &array);
template<class T> Bool allOR (const MaskedArray<T> &array, const T &val);
template<class T> Bool allOR (const T &val, const MaskedArray<T> &array);
// </group>
//
// </group>


// 
// Element by element comparisons between an array and a scalar, which
// behaves as if it were a conformant array filled with the value "val."
// The result is an MaskedLogicalArray.
// <group>
//
template<class T>
  MaskedLogicalArray operator <= (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator <= (const T &val, const MaskedArray<T> &array);
template<class T>
  MaskedLogicalArray operator <  (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator <  (const T &val, const MaskedArray<T> &array);
template<class T>
  MaskedLogicalArray operator >= (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator >= (const T &val, const MaskedArray<T> &array);
template<class T>
  MaskedLogicalArray operator >  (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator >  (const T &val, const MaskedArray<T> &array);
template<class T>
  MaskedLogicalArray operator == (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator == (const T &val, const MaskedArray<T> &array);
template<class T>
  MaskedLogicalArray operator != (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator != (const T &val, const MaskedArray<T> &array);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T>
  MaskedLogicalArray operator && (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator && (const T &val, const MaskedArray<T> &array);
template<class T>
  MaskedLogicalArray operator || (const MaskedArray<T> &array, const T &val);
template<class T>
  MaskedLogicalArray operator || (const T &val, const MaskedArray<T> &array);
// </group>
//
// </group>


//# With two arrays, they must both conform, and the result is done element
//# by element. For instance anyLE (a, b) imples that some element of a is
//# less than or equal to every element of b.
//# NB comparison between two zero-sized arrays is not defined (should it
//# throw an exception?).

// 
// Element by element comparisons between the "l" and "r" arrays. The result
// is true only if the comparison is true for some element of the arrays
// for which the mask of the MaskedArray is True.  For two MaskedArrays,
// the "and" of the masks is used.
//
// <thrown>
//   <li> ArrayConformanceError
//   <li> ArrayError
// </thrown>
//
// <group>
//
template<class T> Bool anyLE (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool anyLT (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool anyGE (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool anyGT (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool anyEQ (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool anyNE (const MaskedArray<T> &l, const Array<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> Bool anyAND (const MaskedArray<T> &l, const Array<T> &r);
template<class T> Bool anyOR (const MaskedArray<T> &l, const Array<T> &r);
// </group>


template<class T> Bool anyLE (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool anyLT (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool anyGE (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool anyGT (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool anyEQ (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool anyNE (const Array<T> &l, const MaskedArray<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> Bool anyAND (const Array<T> &l, const MaskedArray<T> &r);
template<class T> Bool anyOR (const Array<T> &l, const MaskedArray<T> &r);
// </group>


template<class T>
  Bool anyLE (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool anyLT (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool anyGE (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool anyGT (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool anyEQ (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool anyNE (const MaskedArray<T> &l, const MaskedArray<T> &r);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T>
  Bool anyAND (const MaskedArray<T> &l, const MaskedArray<T> &r);
template<class T>
  Bool anyOR (const MaskedArray<T> &l, const MaskedArray<T> &r);
// </group>

// </group>


// 
// Element by element comparisons between an array and a scalar, which
// behaves as if it were a conformant array filled with the value "val."
// The result is true only if the comparison is true for some element
// for which the mask of the MaskedArray is True.
//
// <thrown>
//   <li> ArrayError
// </thrown>
//
// <group>
//
template<class T> Bool anyLE (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyLE (const T &val, const MaskedArray<T> &array);
template<class T> Bool anyLT (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyLT (const T &val, const MaskedArray<T> &array);
template<class T> Bool anyGE (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyGE (const T &val, const MaskedArray<T> &array);
template<class T> Bool anyGT (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyGT (const T &val, const MaskedArray<T> &array);
template<class T> Bool anyEQ (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyEQ (const T &val, const MaskedArray<T> &array);
template<class T> Bool anyNE (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyNE (const T &val, const MaskedArray<T> &array);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> Bool anyAND (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyAND (const T &val, const MaskedArray<T> &array);
template<class T> Bool anyOR (const MaskedArray<T> &array, const T &val);
template<class T> Bool anyOR (const T &val, const MaskedArray<T> &array);
// </group>
//
// </group>

// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/MaskArrLogi.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
