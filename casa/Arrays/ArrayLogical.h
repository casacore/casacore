//# ArrayLogical.h: Element by element logical operations on arrays.
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

#ifndef CASA_ARRAYLOGICAL_2_H
#define CASA_ARRAYLOGICAL_2_H

//# Includes
#include "ArrayFwd.h"
#include "IPosition.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Logical operations for Arrays.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArrayLogical">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
// </prerequisite>
//
// <etymology>
// This file contains global functions which perform element by element logical
// operations on arrays.
// </etymology>
//
// <synopsis>
// These functions perform element by element logical operations on
// arrays.  The two arrays must conform, except for allEQ which returns
// false if the arrays do not conform.
//
// There are two classes of functions.  One class returns a LogicalArray.
// In these functions, the value of an element of the LogicalArray is
// the value of the logical operation applied to the corresponding elements
// of the input Arrays.  The other class of functions returns a single
// bool.  The return value is true if the logical operation returns true for
// all elements of the input arrays for the "all" functions
// (e.g. allLE()), and returns true if the logical operation returns true for
// any elements of the input arrays for the "any" functions
// (e.g. anyLE()).
//
// For instance allLE (a, b) implies that every element of a is
// less than or equal to every element of b. Note that with this definition
// allLE (a, b) and allGE (a, b) can both be false (e.g. a = [1,0] b = [0,1]).
//
// <note role=caution> Comparison between two zero-sized arrays is not defined
// (should it throw an exception?).
// </note>
//
// </synopsis>
//
// <example>
// <srcblock>
//   Vector<int> a(10);
//   Vector<int> b(10);
//   LogicalVector l(10);
//      . . .
//   l = a < b;
// </srcblock>
// This example sets the elements of l (a<b).
// The result of the comparison is a LogicalArray.
// </example>
//
// <example>
// <srcblock>
//   Vector<int> a(10);
//   Vector<int> b(10);
//   bool result;
//      . . .
//   result = allLT (a, b);
// </srcblock>
// This example sets result to true if, for all elements, a<b.
// </example>
//
// <motivation>
// One wants to be able to perform logical operations on arrays.
// </motivation>
//
// <todo asof="$DATE:$>
//   <li> Reconsider where the origin of the returned LogicalArray should
//          be located.
// </todo>
//
// <linkfrom anchor="Array logical operations" classes="Array Vector Matrix Cube">
//    <here>Array logical operations</here> -- Logical operations for Arrays.
// </linkfrom>
//
// <group name="Array logical operations">


// Determine if the comparisons between corresponding array elements yield true.
// <group>
template<typename T, typename CompareOperator>
bool arrayCompareAll (const Array<T>& left, const Array<T>& right,
                      CompareOperator op);
template<typename T, typename CompareOperator>
bool arrayCompareAll (const Array<T>& left, T right,
                      CompareOperator op);
template<typename T, typename CompareOperator>
bool arrayCompareAll (T left, const Array<T>& right,
                      CompareOperator op);
// </group>

// Determine if the comparisons between corresponding array elements yield true.
// <group>
template<typename T, typename CompareOperator>
bool arrayCompareAny (const Array<T>& left, const Array<T>& right,
                      CompareOperator op);
template<typename T, typename CompareOperator>
bool arrayCompareAny (const Array<T>& left, T right,
                      CompareOperator op);
template<typename T, typename CompareOperator>
bool arrayCompareAny (T left, const Array<T>& right,
                      CompareOperator op);
// </group>

// 
// Element by element comparisons between the "l" and "r" arrays. The result
// is true only if the comparison is true for every element of the arrays.
//
// The operator forms of array logical operations which return a single bool
// have been replaced by these "all" functions.
// The operator forms of array logical operations now return a LogicalArray.
//
// The arrays must conform except for allEQ, which will return false if the
// arrays have different shapes.
//
// <thrown>
//    <li> ArrayConformanceError
// </thrown>
//
// <group>
template<class T> bool allLE (const Array<T> &l, const Array<T> &r);
template<class T> bool allLT (const Array<T> &l, const Array<T> &r);
template<class T> bool allGE (const Array<T> &l, const Array<T> &r);
template<class T> bool allGT (const Array<T> &l, const Array<T> &r);
template<class T> bool allEQ (const Array<T> &l, const Array<T> &r);
template<class T> bool allNE (const Array<T> &l, const Array<T> &r);
template<class T> bool allNear (const Array<T> &l, const Array<T> &r,
				double tol);
template<class T> bool allNearAbs (const Array<T> &l, const Array<T> &r,
				   double tol);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> bool allAND (const Array<T> &l, const Array<T> &r);
template<class T> bool allOR (const Array<T> &l, const Array<T> &r);
// </group>
//
// </group>


// 
// Element by element comparisons between the "l" and "r" arrays. The result
// is a LogicalArray.
// The arrays must conform or an exception is thrown.
//
// The Vector, Matrix and Cube version are present to bypass the problems
// due to the existence of automatic comparison inline templates in standard
// algorithm library, producing a single bool value.
//
// <group>
template<class T> LogicalArray operator <= (const Array<T> &l,
					    const Array<T> &r);
template<class T> LogicalArray operator <  (const Array<T> &l,
					    const Array<T> &r);
template<class T> LogicalArray operator >= (const Array<T> &l,
					    const Array<T> &r);
template<class T> LogicalArray operator >  (const Array<T> &l,
					    const Array<T> &r);
template<class T> LogicalArray operator == (const Array<T> &l,
					    const Array<T> &r);
template<class T> LogicalArray operator != (const Array<T> &l,
					    const Array<T> &r);

template<class T> LogicalArray near(const Array<T> &l, const Array<T> &r,
				    double tol);
template<class T> LogicalArray nearAbs(const Array<T> &l, const Array<T> &r,
                                       double tol);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> LogicalArray operator && (const Array<T> &l, const Array<T> &r);
template<class T> LogicalArray operator || (const Array<T> &l, const Array<T> &r);
// </group>
//
// </group>


// 
// Logical negation of an array.  This only makes sense if the array
// element type is logical valued.
template<class T> LogicalArray operator ! (const Array<T> &l);


// 
// Element by element comparisons between an array and a scalar, which
// behaves as if it were a conformant array filled with the value "val."
// The result is true only if the comparison is true for every element
// of the array.
// <group>
template<class T> bool allLE (const Array<T> &array, const T &val);
template<class T> bool allLE (const T &val, const Array<T> &array);
template<class T> bool allLT (const Array<T> &array, const T &val);
template<class T> bool allLT (const T &val, const Array<T> &array);
template<class T> bool allGE (const Array<T> &array, const T &val);
template<class T> bool allGE (const T &val, const Array<T> &array);
template<class T> bool allGT (const Array<T> &array, const T &val);
template<class T> bool allGT (const T &val, const Array<T> &array);
template<class T> bool allEQ (const Array<T> &array, const T &val);
template<class T> bool allEQ (const T &val, const Array<T> &array);
template<class T> bool allNE (const Array<T> &array, const T &val);
template<class T> bool allNE (const T &val, const Array<T> &array);
template<class T> bool allNear (const Array<T> &array, const T &val, double tol);
template<class T> bool allNear (const T &val, const Array<T> &array, double tol);
template<class T> bool allNearAbs (const Array<T> &array, const T &val,
				   double tol);
template<class T> bool allNearAbs (const T &val, const Array<T> &array,
				   double tol);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> bool allAND (const Array<T> &array, const T &val);
template<class T> bool allAND (const T &val, const Array<T> &array);
template<class T> bool allOR (const Array<T> &array, const T &val);
template<class T> bool allOR (const T &val, const Array<T> &array);
// </group>
//
// </group>


// Test if all elements in an array are the same.
template<class T> bool allSame (const Array<T> &a)
  { return a.size() <= 1  ||  allEQ(*a.data(), a); }


// Element by element test for NaN or (In)finity.
// <group>
template<class T> LogicalArray isNaN    (const Array<T> &array);
template<class T> LogicalArray isInf    (const Array<T> &array);
template<class T> LogicalArray isFinite (const Array<T> &array);
// </group>

// 
// Element by element comparisons between an array and a scalar, which
// behaves as if it were a conformant array filled with the value "val."
// The result is a LogicalArray.
//
// <thrown>
//    <li> ArrayConformanceError
// </thrown>
//
// <group>
template<class T> LogicalArray operator <= (const Array<T> &array, const T &val);
template<class T> LogicalArray operator <= (const T &val, const Array<T> &array);
template<class T> LogicalArray operator <  (const Array<T> &array, const T &val);
template<class T> LogicalArray operator <  (const T &val, const Array<T> &array);
template<class T> LogicalArray operator >= (const Array<T> &array, const T &val);
template<class T> LogicalArray operator >= (const T &val, const Array<T> &array);
template<class T> LogicalArray operator >  (const Array<T> &array, const T &val);
template<class T> LogicalArray operator >  (const T &val, const Array<T> &array);
template<class T> LogicalArray operator == (const Array<T> &array, const T &val);
template<class T> LogicalArray operator == (const T &val, const Array<T> &array);
template<class T> LogicalArray operator != (const Array<T> &array, const T &val);
template<class T> LogicalArray operator != (const T &val, const Array<T> &array);
template<class T> LogicalArray near (const Array<T> &array, const T &val,
				     double tol);
template<class T> LogicalArray near (const T &val, const Array<T> &array,
				      double tol);
template<class T> LogicalArray nearAbs (const Array<T> &array, const T &val,
				     double tol);
template<class T> LogicalArray nearAbs (const T &val, const Array<T> &array,
				      double tol);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> LogicalArray operator && (const Array<T> &array, const T &val);
template<class T> LogicalArray operator && (const T &val, const Array<T> &array);
template<class T> LogicalArray operator || (const Array<T> &array, const T &val);
template<class T> LogicalArray operator || (const T &val, const Array<T> &array);
// </group>
//
// </group>


//# With two arrays, they must both conform, and the result is done element
//# by element. For instance anyLE (a, b) implies that some element of a is
//# less than or equal to the corresponding element of b.
//# NB comparison between two zero-sized arrays is not defined (should it
//# throw an exception?).

// 
// Element by element comparisons between the "l" and "r" arrays. The result
// is true if the comparison is true for some element of the arrays.
//
// <thrown>
//    <li> ArrayConformanceError
// </thrown>
//
// <group>

template<class T> bool anyLE (const Array<T> &l, const Array<T> &r);
template<class T> bool anyLT (const Array<T> &l, const Array<T> &r);
template<class T> bool anyGE (const Array<T> &l, const Array<T> &r);
template<class T> bool anyGT (const Array<T> &l, const Array<T> &r);
template<class T> bool anyEQ (const Array<T> &l, const Array<T> &r);
template<class T> bool anyNE (const Array<T> &l, const Array<T> &r);
template<class T> bool anyNear (const Array<T> &l, const Array<T> &r, 
				double tol);
template<class T> bool anyNearAbs (const Array<T> &l, const Array<T> &r,
				   double tol);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> bool anyAND (const Array<T> &l, const Array<T> &r);
template<class T> bool anyOR (const Array<T> &l, const Array<T> &r);
// </group>
//
// </group>

// 
// Element by element comparisons between an array and a scalar, which
// behaves as if it were a conformant array filled with the value "val."
// The result is true if the comparison is true for some element of the array.
// At some point operators will be available that return masks where the
// comparison is true.
// <group>

template<class T> bool anyLE (const Array<T> &array, const T &val);
template<class T> bool anyLE (const T &val, const Array<T> &array);
template<class T> bool anyLT (const Array<T> &array, const T &val);
template<class T> bool anyLT (const T &val, const Array<T> &array);
template<class T> bool anyGE (const Array<T> &array, const T &val);
template<class T> bool anyGE (const T &val, const Array<T> &array);
template<class T> bool anyGT (const Array<T> &array, const T &val);
template<class T> bool anyGT (const T &val, const Array<T> &array);
template<class T> bool anyEQ (const Array<T> &array, const T &val);
template<class T> bool anyEQ (const T &val, const Array<T> &array);
template<class T> bool anyNE (const Array<T> &array, const T &val);
template<class T> bool anyNE (const T &val, const Array<T> &array);
template<class T> bool anyNear (const Array<T> &array, const T &val, double tol);
template<class T> bool anyNear (const T &val, const Array<T> &array, double tol);
template<class T> bool anyNearAbs (const Array<T> &array, const T &val,
				   double tol);
template<class T> bool anyNearAbs (const T &val, const Array<T> &array,
				   double tol);
//
// This only makes sense if the array element type is logical valued.
// <group>
template<class T> bool anyAND (const Array<T> &array, const T &val);
template<class T> bool anyAND (const T &val, const Array<T> &array);
template<class T> bool anyOR (const Array<T> &array, const T &val);
template<class T> bool anyOR (const T &val, const Array<T> &array);
// </group>
//
// </group>


// Are all elements true?
template<typename Alloc>
inline bool allTrue (const Array<bool, Alloc>& array)
  { return allEQ (array, true); }

// Is any element true?
template<typename Alloc>
inline bool anyTrue (const Array<bool, Alloc>& array)
  { return anyEQ (array, true); }

// The same functions as above, but for selected axes.
Array<bool> partialAllTrue (const Array<bool>& array,
                            const IPosition& collapseAxes);
Array<bool> partialAnyTrue (const Array<bool>& array,
                            const IPosition& collapseAxes);

// Determine the number of true or false elements.
// Note: it is meant for bool arrays, but can also be used for
// e.g. int arrays.
// <group>

// Determine it for the full array.
// <group>
template<class T> size_t nfalse (const Array<T> &array);
template<class T> size_t ntrue (const Array<T> &array)
  { return array.nelements() - nfalse(array); }
// </group>

// The same functions as above, but determine ntrue and nfalse for the
// given axes only. The result is an array with a shape formed by the
// remaining axes.
// For example, for an array with shape [3,4,5], collapsing axis 0
// results in an array with shape [4,5] containing ntrue or nfalse for
// each X line.
// Summing for axes 0 and 2 results in an array with shape [4] containing
// ntrue or nfalse for each XZ plane.
// <group>
template<class T> Array<size_t> partialNTrue (const Array<T>& array,
					    const IPosition& collapseAxes);
template<class T> Array<size_t> partialNFalse (const Array<T>& array,
					     const IPosition& collapseAxes);
// </group>

// </group>

// </group>
} // end of casacore namespace

#include "ArrayMathBase.h"

namespace casacore {
// Define logical Functors.
// <group>
template<typename T> class AllFunc : public ArrayFunctorBase<T,bool> {
public:
  virtual ~AllFunc() {}
  virtual bool operator() (const Array<T>& arr) const { return allTrue(arr); }
};
template<typename T> class AnyFunc : public ArrayFunctorBase<T,bool> {
public:
  virtual ~AnyFunc() {}
  virtual bool operator() (const Array<T>& arr) const { return anyTrue(arr); }
};

template<typename T, typename RES=size_t>
class NTrueFunc : public ArrayFunctorBase<T,RES> {
public:
  virtual ~NTrueFunc() {}
  virtual RES operator() (const Array<T>& arr) const { return ntrue(arr); }
};
template<typename T, typename RES=size_t>
class NFalseFunc : public ArrayFunctorBase<T,RES> {
public:
  virtual ~NFalseFunc() {}
  virtual RES operator() (const Array<T>& arr) const { return nfalse(arr); }
};
// </group>

} //# NAMESPACE CASACORE - END

#include "ArrayLogical.tcc"

#endif
