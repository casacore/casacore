//# ArrayMath.h: ArrayMath: Simple mathematics done on an entire array.
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2001,2003
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

#ifndef CASA_ARRAYMATH_2_H
#define CASA_ARRAYMATH_2_H

#include "Array.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <numeric>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Mathematical operations for Arrays.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArray">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
// </prerequisite>
//
// <etymology>
// This file contains global functions which perform element by element
// mathematical operations on arrays.
// </etymology>
//
// <synopsis>
// These functions perform element by element mathematical operations on
// arrays.  The two arrays must conform.
//
// Furthermore it defines functions a la std::transform to transform one or
// two arrays by means of a unary or binary operator. All math and logical
// operations on arrays can be expressed by means of these transform functions.
// <br>It also defines an in-place transform function because for non-trivial
// iterators it works faster than a transform where the result is an iterator
// on the same data object as the left operand.
// <br>The transform functions distinguish between contiguous and non-contiguous
// arrays because iterating through a contiguous array can be done in a faster
// way.
// <br> Similar to the standard transform function these functions do not check
// if the shapes match. The user is responsible for that.
// </synopsis>
//
// <example>
// <srcblock>
//   Vector<int> a(10);
//   Vector<int> b(10);
//   Vector<int> c(10);
//      . . .
//   c = a + b;
// </srcblock>
// This example sets the elements of c to (a+b). It checks if a and b have the
// same shape.
// The result of this operation is an Array.
// </example>
//
// <example>
// <srcblock>
//   c = arrayTransformResult (a, b, std::plus<double>());
// </srcblock>
// This example does the same as the previous example, but expressed using
// the transform function (which, in fact, is used by the + operator above).
// However, it is not checked if the shapes match.
// </example>

// <example>
// <srcblock>
//   arrayContTransform (a, b, c, std::plus<double>());
// </srcblock>
// This example does the same as the previous example, but is faster because
// the result array already exists and does not need to be allocated.
// Note that the caller must be sure that c is contiguous.
// </example>

// <example>
// <srcblock>
//   Vector<double> a(10);
//   Vector<double> b(10);
//   Vector<double> c(10);
//      . . .
//   c = atan2 (a, b);
// </srcblock>
// This example sets the elements of c to atan2 (a,b).
// The result of this operation is an Array.
// </example>
//
// <example>
// <srcblock>
//   Vector<int> a(10);
//   int result;
//      . . .
//   result = sum (a);
// </srcblock>
// This example sums a.
// </example>
//
// <motivation>
// One wants to be able to perform mathematical operations on arrays.
// </motivation>
//
// <linkfrom anchor="Array mathematical operations" classes="Array Vector Matrix Cube">
//    <here>Array mathematical operations</here> -- Mathematical operations for
//    Arrays.
// </linkfrom>
//
// <group name="Array mathematical operations">


  // The myxtransform functions are defined to avoid a bug in g++-4.3.
  // That compiler generates incorrect code when only -g is used for
  // a std::transform with a bind1st or bind2nd for a complex<float>.
  // So, for example, the multiplication of a std::complex<float> array and std::complex<float> scalar
  // would fail (see g++ bug 39678).
  // <group>
  // sequence = scalar OP sequence
  template<typename _InputIterator1, typename T,
	   typename _OutputIterator, typename _BinaryOperation>
    void
    myltransform(_InputIterator1 __first1, _InputIterator1 __last1,
                 _OutputIterator __result, T left,
                 _BinaryOperation __binary_op)
    {
      for ( ; __first1 != __last1; ++__first1, ++__result)
	*__result = __binary_op(left, *__first1);
    }
  // sequence = sequence OP scalar
  template<typename _InputIterator1, typename T,
	   typename _OutputIterator, typename _BinaryOperation>
    void
    myrtransform(_InputIterator1 __first1, _InputIterator1 __last1,
                 _OutputIterator __result, T right,
                 _BinaryOperation __binary_op)
    {
      for ( ; __first1 != __last1; ++__first1, ++__result)
	*__result = __binary_op(*__first1, right);
    }
  // sequence OP= scalar
  template<typename _InputIterator1, typename T,
	   typename _BinaryOperation>
    void
    myiptransform(_InputIterator1 __first1, _InputIterator1 __last1,
		  T right,
		  _BinaryOperation __binary_op)
    {
      for ( ; __first1 != __last1; ++__first1)
	*__first1 = __binary_op(*__first1, right);
    }
  // </group>


// Functions to apply a binary or unary operator to arrays.
// They are modeled after std::transform.
// They do not check if the shapes conform; as in std::transform the
// user must take care that the operands conform.
// <group>
// Transform left and right to a result using the binary operator.
// Result MUST be a contiguous array.
template<typename L, typename AllocL, typename R, typename AllocR, typename RES, typename AllocRES, typename BinaryOperator>
inline void arrayContTransform (const Array<L, AllocL>& left, const Array<R, AllocR>& right,
                                Array<RES, AllocRES>& result, BinaryOperator op)
{
  assert (result.contiguousStorage());
  if (left.contiguousStorage()  &&  right.contiguousStorage()) {
    std::transform (left.cbegin(), left.cend(), right.cbegin(),
                    result.cbegin(), op);
  } else {
    std::transform (left.begin(), left.end(), right.begin(),
                    result.cbegin(), op);
  }
}

// Transform left and right to a result using the binary operator.
// Result MUST be a contiguous array.
template<typename L, typename AllocL, typename R, typename RES, typename AllocRES, typename BinaryOperator>
inline void arrayContTransform (const Array<L, AllocL>& left, R right,
                                Array<RES, AllocRES>& result, BinaryOperator op)
{
  assert (result.contiguousStorage());
  if (left.contiguousStorage()) {
    myrtransform (left.cbegin(), left.cend(),
                 result.cbegin(), right, op);
    ////    std::transform (left.cbegin(), left.cend(),
    ////                    result.cbegin(), bind2nd(op, right));
  } else {
    myrtransform (left.begin(), left.end(),
                 result.cbegin(), right, op);
    ////    std::transform (left.begin(), left.end(),
    ////                    result.cbegin(), bind2nd(op, right));
  }
}

// Transform left and right to a result using the binary operator.
// Result MUST be a contiguous array.
template<typename L, typename R, typename AllocR, typename RES, typename AllocRES, typename BinaryOperator>
inline void arrayContTransform (L left, const Array<R, AllocR>& right,
                                Array<RES, AllocRES>& result, BinaryOperator op)
{
  assert (result.contiguousStorage());
  if (right.contiguousStorage()) {
    myltransform (right.cbegin(), right.cend(),
                  result.cbegin(), left, op);
    ////    std::transform (right.cbegin(), right.cend(),
    ////                    result.cbegin(), bind1st(op, left));
  } else {
    myltransform (right.begin(), right.end(),
                  result.cbegin(), left, op);
    ////    std::transform (right.begin(), right.end(),
    ////                    result.cbegin(), bind1st(op, left));
  }
}

// Transform array to a result using the unary operator.
// Result MUST be a contiguous array.
template<typename T, typename Alloc, typename RES, typename AllocRES, typename UnaryOperator>
inline void arrayContTransform (const Array<T, Alloc>& arr,
                                Array<RES, AllocRES>& result, UnaryOperator op)
{
  assert (result.contiguousStorage());
  if (arr.contiguousStorage()) {
    std::transform (arr.cbegin(), arr.cend(), result.cbegin(), op);
  } else {
    std::transform (arr.begin(), arr.end(), result.cbegin(), op);
  }
}

// Transform left and right to a result using the binary operator.
// Result need not be a contiguous array.
template<typename L, typename R, typename RES, typename BinaryOperator, typename AllocL, typename AllocR, typename AllocRES>
void arrayTransform (const Array<L, AllocL>& left, const Array<R, AllocR>& right,
                     Array<RES, AllocRES>& result, BinaryOperator op);

// Transform left and right to a result using the binary operator.
// Result need not be a contiguous array.
template<typename L, typename R, typename RES, typename BinaryOperator, typename Alloc, typename AllocRES>
void arrayTransform (const Array<L, Alloc>& left, R right,
                     Array<RES, AllocRES>& result, BinaryOperator op);

// Transform left and right to a result using the binary operator.
// Result need not be a contiguous array.
template<typename L, typename R, typename RES, typename BinaryOperator, typename Alloc, typename AllocRES>
void arrayTransform (L left, const Array<R, Alloc>& right,
                     Array<RES, AllocRES>& result, BinaryOperator op);

// Transform array to a result using the unary operator.
// Result need not be a contiguous array.
template<typename T, typename RES, typename UnaryOperator, typename Alloc, typename AllocRES>
void arrayTransform (const Array<T, Alloc>& arr,
                     Array<RES, AllocRES>& result, UnaryOperator op);

// Transform left and right to a result using the binary operator.
// The created and returned result array is contiguous.
template<typename T, typename BinaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (const Array<T, Alloc>& left, const Array<T, Alloc>& right,
                               BinaryOperator op);

// Transform left and right to a result using the binary operator.
// The created and returned result array is contiguous.
template<typename T, typename BinaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (const Array<T, Alloc>& left, T right, BinaryOperator op);

// Transform left and right to a result using the binary operator.
// The created and returned result array is contiguous.
template<typename T, typename BinaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (T left, const Array<T, Alloc>& right, BinaryOperator op);

// Transform array to a result using the unary operator.
// The created and returned result array is contiguous.
template<typename T, typename UnaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (const Array<T, Alloc>& arr, UnaryOperator op);

// Transform left and right in place using the binary operator.
// The result is stored in the left array (useful for e.g. the += operation).
template<typename L, typename R, typename BinaryOperator, typename AllocL, typename AllocR>
inline void arrayTransformInPlace (Array<L, AllocL>& left, const Array<R, AllocR>& right,
                                   BinaryOperator op)
{
  if (left.contiguousStorage()  &&  right.contiguousStorage()) {
    std::transform(left.cbegin(), left.cend(), right.cbegin(), left.cbegin(), op);
  } else {
    std::transform(left.begin(), left.end(), right.begin(), left.begin(), op);
  }
}

// Transform left and right in place using the binary operator.
// The result is stored in the left array (useful for e.g. the += operation).
template<typename L, typename R, typename BinaryOperator, typename Alloc>
inline void arrayTransformInPlace (Array<L, Alloc>& left, R right, BinaryOperator op)
{
  if (left.contiguousStorage()) {
    myiptransform (left.cbegin(), left.cend(), right, op);
    ////    transformInPlace (left.cbegin(), left.cend(), bind2nd(op, right));
  } else {
    myiptransform (left.begin(), left.end(), right, op);
    ////    transformInPlace (left.begin(), left.end(), bind2nd(op, right));
  }
}

// Transform the array in place using the unary operator.
// E.g. doing <src>arrayTransformInPlace(array, Sin<T>())</src> is faster than
// <src>array=sin(array)</src> as it does not need to create a temporary array.
template<typename T, typename UnaryOperator, typename Alloc>
inline void arrayTransformInPlace (Array<T, Alloc>& arr, UnaryOperator op)
{
  if (arr.contiguousStorage()) {
    std::transform(arr.cbegin(), arr.cend(), arr.cbegin(), op);
  } else {
    std::transform(arr.begin(), arr.end(), arr.begin(), op);
  }
}
// </group>

// 
// Element by element arithmetic modifying left in-place. left and other
// must be conformant.
// <group>
template<typename T, typename Alloc> void operator+= (Array<T, Alloc> &left, const Array<T, Alloc> &other);
template<typename T, typename Alloc> void operator-= (Array<T, Alloc> &left, const Array<T, Alloc> &other);
template<typename T, typename Alloc> void operator*= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "*=");
    arrayTransformInPlace (left, other, std::multiplies<T>());
}

template<typename T, typename Alloc> void operator/= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "/=");
    arrayTransformInPlace (left, other, std::divides<T>());
}
template<typename T, typename Alloc> void operator%= (Array<T, Alloc> &left, const Array<T, Alloc> &other);
template<typename T, typename Alloc> void operator&= (Array<T, Alloc> &left, const Array<T, Alloc> &other);
template<typename T, typename Alloc> void operator|= (Array<T, Alloc> &left, const Array<T, Alloc> &other);
template<typename T, typename Alloc> void operator^= (Array<T, Alloc> &left, const Array<T, Alloc> &other);
// </group>

// 
// Element by element arithmetic modifying left in-place. The scalar "other"
// behaves as if it were a conformant Array to left filled with constant values.
// <group>
template<typename T, typename Alloc> void operator+= (Array<T, Alloc> &left, const T &other);
template<typename T, typename Alloc> void operator-= (Array<T, Alloc> &left, const T &other);
template<typename T, typename Alloc> void operator*= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::multiplies<T>());
}
template<typename T, typename Alloc> void operator/= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::divides<T>());
}
template<typename T, typename Alloc> void operator%= (Array<T, Alloc> &left, const T &other);
template<typename T, typename Alloc> void operator&= (Array<T, Alloc> &left, const T &other);
template<typename T, typename Alloc> void operator|= (Array<T, Alloc> &left, const T &other);
template<typename T, typename Alloc> void operator^= (Array<T, Alloc> &left, const T &other);
// </group>

// Unary arithmetic operation.
// 
// <group>
template<typename T, typename Alloc> Array<T, Alloc> operator+(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> operator-(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> operator~(const Array<T, Alloc> &a);
// </group>

// 
// Element by element arithmetic on two arrays, returning an array.
// <group>
template<typename T, typename Alloc> 
  Array<T, Alloc> operator+ (const Array<T, Alloc> &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc> 
  Array<T, Alloc> operator- (const Array<T, Alloc> &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc>
  Array<T, Alloc> operator*(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "*");
    return arrayTransformResult (left, right, std::multiplies<T>());
}
template<typename T, typename Alloc> 
  Array<T, Alloc> operator/ (const Array<T, Alloc> &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc> 
  Array<T, Alloc> operator% (const Array<T, Alloc> &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc> 
  Array<T, Alloc> operator| (const Array<T, Alloc> &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc> 
  Array<T, Alloc> operator& (const Array<T, Alloc> &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc> 
  Array<T, Alloc> operator^ (const Array<T, Alloc> &left, const Array<T, Alloc> &right);
// </group>

// 
// Element by element arithmetic between an array and a scalar, returning
// an array.
// <group>
template<typename T, typename Alloc> 
    Array<T, Alloc> operator+ (const Array<T, Alloc> &left, const T &right);
template<typename T, typename Alloc> 
    Array<T, Alloc> operator- (const Array<T, Alloc> &left, const T &right);
template<class T, typename Alloc>
Array<T, Alloc> operator* (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, std::multiplies<T>());
}
template<typename T, typename Alloc> 
    Array<T, Alloc> operator/ (const Array<T, Alloc> &left, const T &right);
template<typename T, typename Alloc> 
    Array<T, Alloc> operator% (const Array<T, Alloc> &left, const T &right);
template<typename T, typename Alloc> 
    Array<T, Alloc> operator| (const Array<T, Alloc> &left, const T &right);
template<typename T, typename Alloc> 
    Array<T, Alloc> operator& (const Array<T, Alloc> &left, const T &right);
template<typename T, typename Alloc> 
    Array<T, Alloc> operator^ (const Array<T, Alloc> &left, const T &right);
// </group>

// 
// Element by element arithmetic between a scalar and an array, returning
// an array.
// <group>
template<typename T, typename Alloc>  
    Array<T, Alloc> operator+ (const T &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc>  
    Array<T, Alloc> operator- (const T &left, const Array<T, Alloc> &right);
template<class T, typename Alloc> 
Array<T, Alloc> operator* (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, std::multiplies<T>());
}

template<typename T, typename Alloc>  
    Array<T, Alloc> operator/ (const T &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc>  
    Array<T, Alloc> operator% (const T &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc>  
    Array<T, Alloc> operator| (const T &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc>  
    Array<T, Alloc> operator& (const T &left, const Array<T, Alloc> &right);
template<typename T, typename Alloc>  
    Array<T, Alloc> operator^ (const T &left, const Array<T, Alloc> &right);
// </group>

// 
// Transcendental function that can be applied to essentially all numeric
// types. Works on an element-by-element basis.
// <group>
template<typename T, typename Alloc> Array<T, Alloc> cos(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> cosh(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> exp(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> log(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> log10(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> pow(const Array<T, Alloc> &a, const Array<T, Alloc> &b);
template<typename T, typename Alloc> Array<T, Alloc> pow(const T &a, const Array<T, Alloc> &b);
template<typename T, typename Alloc> Array<T, Alloc> sin(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> sinh(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> sqrt(const Array<T, Alloc> &a);
// </group>

// 
// Transcendental function applied to the array on an element-by-element
// basis. Although a template function, this does not make sense for all
// numeric types.
// <group>
template<typename T, typename Alloc> Array<T, Alloc> acos(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> asin(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> atan(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> atan2(const Array<T, Alloc> &y, const Array<T, Alloc> &x);
template<typename T, typename Alloc> Array<T, Alloc> atan2(const T &y, const Array<T, Alloc> &x);
template<typename T, typename Alloc> Array<T, Alloc> atan2(const Array<T, Alloc> &y, const T &x);
template<typename T, typename Alloc> Array<T, Alloc> ceil(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> fabs(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> abs(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> floor(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> round(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> sign(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> fmod(const Array<T, Alloc> &a, const Array<T, Alloc> &b);
template<typename T, typename Alloc> Array<T, Alloc> fmod(const T &a, const Array<T, Alloc> &b);
template<typename T, typename Alloc> Array<T, Alloc> fmod(const Array<T, Alloc> &a, const T &b);
template<typename T, typename Alloc> Array<T, Alloc> floormod(const Array<T, Alloc> &a, const Array<T, Alloc> &b);
template<typename T, typename Alloc> Array<T, Alloc> floormod(const T &a, const Array<T, Alloc> &b);
template<typename T, typename Alloc> Array<T, Alloc> floormod(const Array<T, Alloc> &a, const T &b);
template<typename T, typename Alloc> Array<T, Alloc> pow(const Array<T, Alloc> &a, const T &b);
template<typename T, typename Alloc> Array<std::complex<T>, Alloc> pow(const Array<std::complex<T>, Alloc> &a, const T &b);
template<typename T, typename Alloc> Array<T, Alloc> tan(const Array<T, Alloc> &a);
template<typename T, typename Alloc> Array<T, Alloc> tanh(const Array<T, Alloc> &a);
// N.B. fabs is deprecated. Use abs.
template<typename T, typename Alloc> Array<T, Alloc> fabs(const Array<T, Alloc> &a);
// </group>

// 
// <group>
// Find the minimum and maximum values of an array, including their locations.
template<typename ScalarType, typename Alloc>
void minMax(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos, 
	    IPosition &maxPos, const Array<ScalarType, Alloc> &array);
// The array is searched at locations where the mask equals <src>valid</src>.
// (at least one such position must exist or an exception will be thrown).
// MaskType should be an Array of bool.
template<typename ScalarType, typename Alloc>
void minMax(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos,
	    IPosition &maxPos, const Array<ScalarType, Alloc> &array, 
	    const Array<bool> &mask, bool valid=true);
// The array * weight is searched 
template<typename ScalarType, typename Alloc>
void minMaxMasked(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos,
		  IPosition &maxPos, const Array<ScalarType, Alloc> &array, 
		  const Array<ScalarType, Alloc> &weight);
// </group>

// 
// The "min" and "max" functions require that the type "T" have comparison 
// operators.
// <group>
//
// This sets min and max to the minimum and maximum of the array to 
// avoid having to do two passes with max() and min() separately.
template<typename T, typename Alloc> void minMax(T &min, T &max, const Array<T, Alloc> &a);
//
// The minimum element of the array.
// Requires that the type "T" has comparison operators.
template<typename T, typename Alloc>  T min(const Array<T, Alloc> &a);
// The maximum element of the array.
// Requires that the type "T" has comparison operators.
template<typename T, typename Alloc>  T max(const Array<T, Alloc> &a);

// "result" contains the maximum of "a" and "b" at each position. "result",
// "a", and "b" must be conformant.
template<typename T, typename Alloc> void max(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const Array<T, Alloc> &b);
// "result" contains the minimum of "a" and "b" at each position. "result",
// "a", and "b" must be conformant.
template<typename T, typename Alloc> void min(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const Array<T, Alloc> &b);
// Return an array that contains the maximum of "a" and "b" at each position.
// "a" and "b" must be conformant.
template<typename T, typename Alloc> Array<T, Alloc> max(const Array<T, Alloc> &a, const Array<T, Alloc> &b);
template<typename T, typename Alloc> Array<T, Alloc> max(const T &a, const Array<T, Alloc> &b);
// Return an array that contains the minimum of "a" and "b" at each position.
// "a" and "b" must be conformant.
template<typename T, typename Alloc> Array<T, Alloc> min(const Array<T, Alloc> &a, const Array<T, Alloc> &b);

// "result" contains the maximum of "a" and "b" at each position. "result",
// and "a" must be conformant.
template<typename T, typename Alloc> void max(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const T &b);
template<typename T, typename Alloc> inline void max(Array<T, Alloc> &result, const T &a, 
                                  const Array<T, Alloc> &b)
  { max (result, b, a); }
// "result" contains the minimum of "a" and "b" at each position. "result",
// and "a" must be conformant.
template<typename T, typename Alloc> void min(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const T &b);
template<typename T, typename Alloc> inline void min(Array<T, Alloc> &result, const T &a, 
                                  const Array<T, Alloc> &b)
  { min (result, b, a); }
// Return an array that contains the maximum of "a" and "b" at each position.
template<typename T, typename Alloc> Array<T, Alloc> max(const Array<T, Alloc> &a, const T &b);
template<typename T, typename Alloc> inline Array<T, Alloc> max(const T &a, const Array<T, Alloc> &b)
  { return max(b, a); }
// Return an array that contains the minimum of "a" and "b" at each position.
template<typename T, typename Alloc> Array<T, Alloc> min(const Array<T, Alloc> &a, const T &b);
template<typename T, typename Alloc> inline Array<T, Alloc> min(const T &a, const Array<T, Alloc> &b)
  { return min(b, a); }
// </group>

// 
// Fills all elements of "array" with a sequence starting with "start"
// and incrementing by "inc" for each element. The first axis varies
// most rapidly.
template<typename T, typename Alloc> void indgen(Array<T, Alloc> &a, T start, T inc);
// 
// Fills all elements of "array" with a sequence starting with 0
// and ending with nelements() - 1. The first axis varies
// most rapidly.
template<typename T, typename Alloc> inline void indgen(Array<T, Alloc> &a)
  { indgen(a, T(0), T(1)); }
// 
// Fills all elements of "array" with a sequence starting with start
// incremented by one for each position in the array. The first axis varies
// most rapidly.
template<typename T, typename Alloc> inline void indgen(Array<T, Alloc> &a, T start)
  { indgen(a, start, T(1)); }

// Create a Vector of the given length and fill it with the start value
// incremented with <code>inc</code> for each element.
template<typename T, typename Alloc=std::allocator<T>> inline Vector<T, Alloc> indgen(size_t length, T start, T inc)
{
  Vector<T, Alloc> x(length);
  indgen(x, start, inc);
  return x;
}


// Sum of every element of the array.
template<typename T, typename Alloc> T sum(const Array<T, Alloc> &a);
// 
// Sum the square of every element of the array.
template<typename T, typename Alloc> T sumsqr(const Array<T, Alloc> &a);
// 
// Product of every element of the array. This could of course easily
// overflow.
template<typename T, typename Alloc> T product(const Array<T, Alloc> &a);

// 
// The mean of "a" is the sum of all elements of "a" divided by the number
// of elements of "a".
template<typename T, typename Alloc> T mean(const Array<T, Alloc> &a);

// The variance of "a" is the sum of (a(i) - mean(a))**2/(a.nelements() - ddof).
// Similar to numpy the argument ddof (delta degrees of freedom) tells if the
// population variance (ddof=0) or the sample variance (ddof=1) is taken.
// The variance functions proper use ddof=1.
// <br>Note that for a complex valued T the absolute values are used; in that way
// the variance is equal to the sum of the variances of the real and imaginary parts.
// Hence the imaginary part in the return value is 0.
template<typename T, typename Alloc> T variance(const Array<T, Alloc> &a);
template<typename T, typename Alloc> T pvariance(const Array<T, Alloc> &a, size_t ddof=0);
// Rather than using a computed mean, use the supplied value.
template<typename T, typename Alloc> T variance(const Array<T, Alloc> &a, T mean);
template<typename T, typename Alloc> T pvariance(const Array<T, Alloc> &a, T mean, size_t ddof=0);

// The standard deviation of "a" is the square root of its variance.
template<typename T, typename Alloc> T stddev(const Array<T, Alloc> &a);
template<typename T, typename Alloc> T pstddev(const Array<T, Alloc> &a, size_t ddof=0);
template<typename T, typename Alloc> T stddev(const Array<T, Alloc> &a, T mean);
template<typename T, typename Alloc> T pstddev(const Array<T, Alloc> &a, T mean, size_t ddof=0);

// 
// The average deviation of "a" is the sum of abs(a(i) - mean(a))/N. (N.B.
// N, not N-1 in the denominator).
template<typename T, typename Alloc> T avdev(const Array<T, Alloc> &a);
// 
// The average deviation of "a" is the sum of abs(a(i) - mean(a))/N. (N.B.
// N, not N-1 in the denominator).
// Rather than using a computed mean, use the supplied value.
template<typename T, typename Alloc> T avdev(const Array<T, Alloc> &a,T mean);

//
// The root-mean-square of "a" is the sqrt of sum(a*a)/N.
template<typename T, typename Alloc> T rms(const Array<T, Alloc> &a);


// The median of "a" is a(n/2).
// If a has an even number of elements and the switch takeEvenMean is set,
// the median is 0.5*(a(n/2) + a((n+1)/2)).
// According to Numerical Recipes (2nd edition) it makes little sense to take
// the mean if the array is large enough (> 100 elements). Therefore
// the default for takeEvenMean is false if the array has > 100 elements,
// otherwise it is true.
// <br>If "sorted"==true we assume the data is already sorted and we
// compute the median directly. Otherwise the function GenSort::kthLargest
// is used to find the median (kthLargest is about 6 times faster
// than a full quicksort).
// <br>Finding the median means that the array has to be (partially)
// sorted. By default a copy will be made, but if "inPlace" is in effect,
// the data themselves will be sorted. That should only be used if the
// data are used not thereafter.
// <note>The function kthLargest in class GenSortIndirect can be used to
// obtain the index of the median in an array. </note>
// <group>
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T median(const Array<T, Alloc> &a, std::vector<T> &scratch, bool sorted,
			   bool takeEvenMean, bool inPlace=false);
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T median(const Array<T, Alloc> &a, bool sorted, bool takeEvenMean,
			   bool inPlace=false)
    { std::vector<T> scratch; return median (a, scratch, sorted, takeEvenMean, inPlace); }
template<typename T, typename Alloc> inline T median(const Array<T, Alloc> &a, bool sorted)
    { return median (a, sorted, (a.nelements() <= 100), false); }
template<typename T, typename Alloc> inline T median(const Array<T, Alloc> &a)
    { return median (a, false, (a.nelements() <= 100), false); }
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> inline T medianInPlace(const Array<T, Alloc> &a, bool sorted=false)
    { return median (a, sorted, (a.nelements() <= 100), true); }
// </group>

// The median absolute deviation from the median. Interface is as for
// the median functions
// <group>
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T madfm(const Array<T, Alloc> &a, std::vector<T> &tmp, bool sorted, 
                         bool takeEvenMean, bool inPlace = false);
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T madfm(const Array<T, Alloc> &a, bool sorted, bool takeEvenMean,
                          bool inPlace=false)
    { std::vector<T> tmp; return madfm(a, tmp, sorted, takeEvenMean, inPlace); }
template<typename T, typename Alloc> inline T madfm(const Array<T, Alloc> &a, bool sorted)
    { return madfm(a, sorted, (a.nelements() <= 100), false); }
template<typename T, typename Alloc> inline T madfm(const Array<T, Alloc> &a)
    { return madfm(a, false, (a.nelements() <= 100), false); }
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> inline T madfmInPlace(const Array<T, Alloc> &a, bool sorted=false)
    { return madfm(a, sorted, (a.nelements() <= 100), true); }
// </group>

// Return the fractile of an array.
// It returns the value at the given fraction of the array.
// A fraction of 0.5 is the same as the median, be it that no mean of
// the two middle elements is taken if the array has an even nr of elements.
// It uses kthLargest if the array is not sorted yet.
// <note>The function kthLargest in class GenSortIndirect can be used to
// obtain the index of the fractile in an array. </note>
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T fractile(const Array<T, Alloc> &a, std::vector<T> &tmp, float fraction,
			     bool sorted=false, bool inPlace=false);
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T fractile(const Array<T, Alloc> &a, float fraction,
			     bool sorted=false, bool inPlace=false)
  { std::vector<T> tmp; return fractile (a, tmp, fraction, sorted, inPlace); }

// Return the inter-fractile range of an array.  
// This is the full range between the bottom and the top fraction.
// <group>
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T interFractileRange(const Array<T, Alloc> &a, std::vector<T> &tmp,
                                       float fraction,
                                       bool sorted=false, bool inPlace=false);
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T interFractileRange(const Array<T, Alloc> &a, float fraction,
                                       bool sorted=false, bool inPlace=false)
  { std::vector<T> tmp; return interFractileRange(a, tmp, fraction, sorted, inPlace); }
// </group>

// Return the inter-hexile range of an array.  
// This is the full range between the bottom sixth and the top sixth
// of ordered array values. "The semi-interhexile range is very nearly
// equal to the rms for a Gaussian distribution, but it is much less
// sensitive to the tails of extended distributions." (Condon et al
// 1998)
// <group>
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T interHexileRange(const Array<T, Alloc> &a, std::vector<T> &tmp,
                                     bool sorted=false, bool inPlace=false)
  { return interFractileRange(a, tmp, 1./6., sorted, inPlace); }
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T interHexileRange(const Array<T, Alloc> &a, bool sorted=false,
                                     bool inPlace=false)
  { return interFractileRange(a, 1./6., sorted, inPlace); }
// </group>

// Return the inter-quartile range of an array.  
// This is the full range between the bottom quarter and the top
// quarter of ordered array values.
// <group>
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T interQuartileRange(const Array<T, Alloc> &a, std::vector<T> &tmp,
                                       bool sorted=false, bool inPlace=false)
  { return interFractileRange(a, tmp, 0.25, sorted, inPlace); }
// TODO shouldn't take a const Array for in place sorting
template<typename T, typename Alloc> T interQuartileRange(const Array<T, Alloc> &a, bool sorted=false,
                                       bool inPlace=false)
  { return interFractileRange(a, 0.25, sorted, inPlace); }
// </group>


// Methods for element-by-element scaling of complex and real.
// Note that std::complex<float> and std::complex<double> are typedefs for std::complex.
//<group>
template<typename T, typename AllocC, typename AllocR>
void operator*= (Array<std::complex<T>, AllocC> &left, const Array<T, AllocR> &other)
{
  checkArrayShapes (left, other, "*=");
  arrayTransformInPlace (left, other,
                         [](std::complex<T> left, T right) { return left*right; });
}

template<typename T, typename Alloc>
void operator*= (Array<std::complex<T>, Alloc> &left, const T &other)
{
  arrayTransformInPlace (left, other,
                         [](std::complex<T> left, T right) { return left*right; });
}

template<typename T, typename AllocC, typename AllocR>
void operator/= (Array<std::complex<T>, AllocC> &left, const Array<T, AllocR> &other)
{
  checkArrayShapes (left, other, "/=");
  arrayTransformInPlace (left, other,
                         [](std::complex<T> left, T right) { return left/right; });
}

template<typename T, typename Alloc>
void operator/= (Array<std::complex<T>, Alloc> &left, const T &other)
{
  arrayTransformInPlace (left, other,
                         [](std::complex<T> left, T right) { return left/right; });
}

template<typename T, typename AllocC, typename AllocR>
Array<std::complex<T>, AllocC> operator* (const Array<std::complex<T>, AllocC> &left,
                                   const Array<T, AllocR> &right)
{
  checkArrayShapes (left, right, "*");
  Array<std::complex<T>, AllocC> result(left.shape());
  arrayContTransform (left, right, result,
                      [](std::complex<T> left, T right) { return left*right; });
  return result;
}
template<typename T, typename Alloc>
Array<std::complex<T> > operator* (const Array<std::complex<T>, Alloc> &left,
                                   const T &other)
{
  Array<std::complex<T> > result(left.shape());
  arrayContTransform (left, other, result,
                      [](std::complex<T> left, T right) { return left*right; });
  return result;
}
template<typename T, typename Alloc>
Array<std::complex<T> > operator*(const std::complex<T> &left,
                                  const Array<T, Alloc> &other)
{
  Array<std::complex<T> > result(other.shape());
  arrayContTransform (left, other, result,
                      [](std::complex<T> left, T right) { return left*right; });
  return result;
}

template<typename T, typename AllocC, typename AllocR>
Array<std::complex<T>, AllocC> operator/ (const Array<std::complex<T>, AllocC> &left,
                                   const Array<T, AllocR> &right)
{
  checkArrayShapes (left, right, "/");
  Array<std::complex<T>, AllocC> result(left.shape());
  arrayContTransform (left, right, result,
                      [](std::complex<T> l, T r) { return l/r; });
  return result;
}
template<typename T, typename Alloc>
Array<std::complex<T>, Alloc> operator/ (const Array<std::complex<T>, Alloc> &left,
                                   const T &other)
{
  Array<std::complex<T>, Alloc> result(left.shape());
  arrayContTransform (left, other, result,
                      [](std::complex<T> left, T right) { return left/right; });
  return result;
}
template<typename T, typename Alloc>
Array<std::complex<T>> operator/(const std::complex<T> &left,
                                  const Array<T, Alloc> &other)
{
  Array<std::complex<T>> result(other.shape());
  arrayContTransform (left, other, result,
                      [](std::complex<T> left, T right) { return left/right; });
  return result;
}
// </group>

// Returns the complex conjugate of a complex array.
//<group>
Array<std::complex<float>> conj(const Array<std::complex<float>> &carray);
Array<std::complex<double>> conj(const Array<std::complex<double>> &carray);
// Modifies rarray in place. rarray must be conformant.
void conj(Array<std::complex<float>> &rarray, const Array<std::complex<float>> &carray);
void conj(Array<std::complex<double>> &rarray, const Array<std::complex<double>> &carray);
//# The following are implemented to make the compiler find the right conversion
//# more often.
Matrix<std::complex<float>> conj(const Matrix<std::complex<float>> &carray);
Matrix<std::complex<double>> conj(const Matrix<std::complex<double>> &carray);
//</group>

// Form an array of complex numbers from the given real arrays.
// Note that std::complex<float> and std::complex<double> are simply typedefs for std::complex<float>
// and std::complex<double>, so the result is in fact one of these types.
// <group>
template<typename T, typename Alloc>
Array<std::complex<T> > makeComplex(const Array<T, Alloc> &real, const Array<T, Alloc>& imag);
template<typename T, typename Alloc>
Array<std::complex<T> > makeComplex(const T &real, const Array<T, Alloc>& imag);
template<typename T, typename Alloc>
Array<std::complex<T> > makeComplex(const Array<T, Alloc> &real, const T& imag);
// </group>

// Set the real part of the left complex array to the right real array.
template<typename C, typename R, typename AllocC, typename AllocR>
void setReal(Array<C, AllocC> &carray, const Array<R, AllocR> &rarray);

// Set the imaginary part of the left complex array to right real array.
template<typename C, typename R, typename AllocC, typename AllocR>
void setImag(Array<C, AllocC> &carray, const Array<R, AllocR> &rarray);

// Extracts the real part of a complex array into an array of floats.
// <group>
Array<float>  real(const Array<std::complex<float>> &carray);
Array<double> real(const Array<std::complex<double>> &carray);
// Modifies rarray in place. rarray must be conformant.
void         real(Array<float> &rarray, const Array<std::complex<float>> &carray);
void         real(Array<double> &rarray, const Array<std::complex<double>> &carray);
// </group>

// 
// Extracts the imaginary part of a complex array into an array of floats.
// <group>
Array<float>  imag(const Array<std::complex<float>> &carray);
Array<double> imag(const Array<std::complex<double>> &carray);
// Modifies rarray in place. rarray must be conformant.
void         imag(Array<float> &rarray, const Array<std::complex<float>> &carray);
void         imag(Array<double> &rarray, const Array<std::complex<double>> &carray);
// </group>

// 
// Extracts the amplitude (i.e. sqrt(re*re + im*im)) from an array
// of complex numbers. N.B. this is presently called "fabs" for a single
// complex number.
// <group>
Array<float>  amplitude(const Array<std::complex<float>> &carray);
Array<double> amplitude(const Array<std::complex<double>> &carray);
// Modifies rarray in place. rarray must be conformant.
void         amplitude(Array<float> &rarray, const Array<std::complex<float>> &carray);
void         amplitude(Array<double> &rarray, const Array<std::complex<double>> &carray);
// </group>

// 
// Extracts the phase (i.e. atan2(im, re)) from an array
// of complex numbers. N.B. this is presently called "arg"
// for a single complex number.
// <group>
Array<float>  phase(const Array<std::complex<float>> &carray);
Array<double> phase(const Array<std::complex<double>> &carray);
// Modifies rarray in place. rarray must be conformant.
void         phase(Array<float> &rarray, const Array<std::complex<float>> &carray);
void         phase(Array<double> &rarray, const Array<std::complex<double>> &carray);
// </group>

// Copy an array of complex into an array of real,imaginary pairs. The
// first axis of the real array becomes twice as long as the complex array.
// In the future versions which work by reference will be available; presently
// a copy is made.
Array<float> ComplexToReal(const Array<std::complex<float>> &carray);
Array<double> ComplexToReal(const Array<std::complex<double>> &carray);
// Modify the array "rarray" in place. "rarray" must be the correct shape.
// <group>
void ComplexToReal(Array<float> &rarray, const Array<std::complex<float>> &carray);
void ComplexToReal(Array<double> &rarray, const Array<std::complex<double>> &carray);
// </group>

// Copy an array of real,imaginary pairs into a complex array. The first axis
// must have an even length.
// In the future versions which work by reference will be available; presently
// a copy is made.
Array<std::complex<float>>  RealToComplex(const Array<float> &rarray);
Array<std::complex<double>> RealToComplex(const Array<double> &rarray);
// Modify the array "carray" in place. "carray" must be the correct shape.
// <group>
void  RealToComplex(Array<std::complex<float>> &carray, const Array<float> &rarray);
void  RealToComplex(Array<std::complex<double>> &carray, const Array<double> &rarray);
// </group>

// Make a copy of an array of a different type; for example make an array
// of doubles from an array of floats. Arrays to and from must be conformant
// (same shape). Also, it must be possible to convert a scalar of type U 
// to type T.
template<typename T, typename U, typename AllocT, typename AllocU>
void convertArray(Array<T, AllocT> &to, const Array<U, AllocU> &from);

// Returns an array where every element is squared.
template<typename T, typename Alloc> Array<T, Alloc> square(const Array<T, Alloc> &val);

// Returns an array where every element is cubed.
template<typename T, typename Alloc> Array<T, Alloc> cube(const Array<T, Alloc> &val);

// Helper function for expandArray using recursion for each axis.
template<typename T>
T* expandRecursive (int axis, const IPosition& shp, const IPosition& mult,
                    const IPosition& inSteps,
                    const T* in, T* out, const IPosition& alternate)
{
  if (axis == 0) {
    if (alternate[0]) {
      // Copy as 1,2,3 1,2,3, etc.
      for (ssize_t j=0; j<mult[0]; ++j) {
        const T* pin = in;
        for (ssize_t i=0; i<shp[0]; ++i) {
          *out++ = *pin;
          pin += inSteps[0];
        }
      }
    } else {
      // Copy as 1,1,1 2,2,2 etc.
      for (ssize_t i=0; i<shp[0]; ++i) {
        for (ssize_t j=0; j<mult[0]; ++j) {
          *out++ = *in;
        }
        in += inSteps[0];
      }
    }
  } else {
    if (alternate[axis]) {
      for (ssize_t j=0; j<mult[axis]; ++j) {
        const T* pin = in;
        for (ssize_t i=0; i<shp[axis]; ++i) {
          out = expandRecursive (axis-1, shp, mult, inSteps,
                                 pin, out, alternate);
          pin += inSteps[axis];
        }
      }
    } else {
      for (ssize_t i=0; i<shp[axis]; ++i) {
        for (ssize_t j=0; j<mult[axis]; ++j) {
          out = expandRecursive (axis-1, shp, mult, inSteps,
                                 in, out, alternate);
        }
        in += inSteps[axis];
      }
    }
  }
  return out;
}

// Expand the values of an array. The arrays can have different dimensionalities.
// Missing input axes have length 1; missing output axes are discarded.
// The length of each axis in the input array must be <= the length of the
// corresponding axis in the output array and divide evenly.
// For each axis <src>mult</src> is set to output/input.
// <br>The <src>alternate</src> argument determines how the values are expanded.
// If a row contains values '1 2 3', they can be expanded "linearly"
// as '1 1 2 2 3 3'  or  alternately as '1 2 3 1 2 3'
// This choice can be made for each axis; a value 0 means linearly,
// another value means alternately. If the length of alternate is less than
// the dimensionality of the output array, the missing ones default to 0.
template<typename T, typename Alloc>
void expandArray (Array<T, Alloc>& out, const Array<T, Alloc>& in,
                  const IPosition& alternate=IPosition())
{
  IPosition mult, inshp, outshp;
  IPosition alt = checkExpandArray (mult, inshp,
                                    in.shape(), out.shape(), alternate);
  Array<T, Alloc> incp(in);
  if (in.ndim() < inshp.size()) {
    incp.reference (in.reform(inshp));
  }
  // Make sure output is contiguous.
  bool deleteIt;
  T* outPtr = out.getStorage (deleteIt);
  expandRecursive (out.ndim()-1, inshp, mult, incp.steps(),
                   incp.data(), outPtr, alt);
  out.putStorage (outPtr, deleteIt);
}

// Check array shapes for expandArray. It returns the alternate argument,
// where possibly missing values are appended (as 0).
// It fills in mult and inshp (with possibly missing axes of length 1).
// <br><code>inShape</code> defines the shape of the input array.
// <br><code>outShape</code> defines the shape of the output array.
// <br><code>alternate</code> tells per axis if value expansion uses alternation.
// <br><code>newInShape</code> is the input shape with new axes (of length 1) added as needed
// <br><code>mult</code> is the multiplication (expansion) factor per output axis
// Returned is the alternation per output axis; new axes have value 0 (linear expansion)
IPosition checkExpandArray (IPosition& mult, IPosition& newInShape,
                            const IPosition& inShape,
                            const IPosition& outShape,
                            const IPosition& alternate);


// </group>


} //# NAMESPACE CASACORE - END

#include "ArrayMath.tcc"

#endif
