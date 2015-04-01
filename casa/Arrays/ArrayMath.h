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

#ifndef CASA_ARRAYMATH_H
#define CASA_ARRAYMATH_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicMath/Functors.h>
#include <casacore/casa/Arrays/Array.h>
//# Needed to get the proper Complex typedef's
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <numeric>
#include <functional>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Matrix;

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
//   Vector<Int> a(10);
//   Vector<Int> b(10);
//   Vector<Int> c(10);
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
//   c = arrayTransformResult (a, b, std::plus<Double>());
// </srcblock>
// This example does the same as the previous example, but expressed using
// the transform function (which, in fact, is used by the + operator above).
// However, it is not checked if the shapes match.
// </example>

// <example>
// <srcblock>
//   arrayContTransform (a, b, c, std::plus<Double>());
// </srcblock>
// This example does the same as the previous example, but is faster because
// the result array already exists and does not need to be allocated.
// Note that the caller must be sure that c is contiguous.
// </example>

// <example>
// <srcblock>
//   Vector<Double> a(10);
//   Vector<Double> b(10);
//   Vector<Double> c(10);
//      . . .
//   c = atan2 (a, b);
// </srcblock>
// This example sets the elements of c to atan2 (a,b).
// The result of this operation is an Array.
// </example>
//
// <example>
// <srcblock>
//   Vector<Int> a(10);
//   Int result;
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
  // So, for example, the multiplication of a Complex array and Complex scalar
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


// Function to check the shapes. It throws an exception if not equal.
// <group>
void throwArrayShapes (const char* name);
inline void checkArrayShapes (const ArrayBase& left, const ArrayBase& right,
                              const char* name)
{
  if (! left.shape().isEqual (right.shape())) {
    throwArrayShapes (name);
  }
}
// </group>


// Functions to apply a binary or unary operator to arrays.
// They are modeled after std::transform.
// They do not check if the shapes conform; as in std::transform the
// user must take care that the operands conform.
// <group>
// Transform left and right to a result using the binary operator.
// Result MUST be a contiguous array.
template<typename L, typename R, typename RES, typename BinaryOperator>
inline void arrayContTransform (const Array<L>& left, const Array<R>& right,
                                Array<RES>& result, BinaryOperator op)
{
  DebugAssert (result.contiguousStorage(), AipsError);
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
template<typename L, typename R, typename RES, typename BinaryOperator>
inline void arrayContTransform (const Array<L>& left, R right,
                                Array<RES>& result, BinaryOperator op)
{
  DebugAssert (result.contiguousStorage(), AipsError);
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
template<typename L, typename R, typename RES, typename BinaryOperator>
inline void arrayContTransform (L left, const Array<R>& right,
                                Array<RES>& result, BinaryOperator op)
{
  DebugAssert (result.contiguousStorage(), AipsError);
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
template<typename T, typename RES, typename UnaryOperator>
inline void arrayContTransform (const Array<T>& arr,
                                Array<RES>& result, UnaryOperator op)
{
  DebugAssert (result.contiguousStorage(), AipsError);
  if (arr.contiguousStorage()) {
    std::transform (arr.cbegin(), arr.cend(), result.cbegin(), op);
  } else {
    std::transform (arr.begin(), arr.end(), result.cbegin(), op);
  }
}

// Transform left and right to a result using the binary operator.
// Result need not be a contiguous array.
template<typename L, typename R, typename RES, typename BinaryOperator>
void arrayTransform (const Array<L>& left, const Array<R>& right,
                     Array<RES>& result, BinaryOperator op);

// Transform left and right to a result using the binary operator.
// Result need not be a contiguous array.
template<typename L, typename R, typename RES, typename BinaryOperator>
void arrayTransform (const Array<L>& left, R right,
                     Array<RES>& result, BinaryOperator op);

// Transform left and right to a result using the binary operator.
// Result need not be a contiguous array.
template<typename L, typename R, typename RES, typename BinaryOperator>
void arrayTransform (L left, const Array<R>& right,
                     Array<RES>& result, BinaryOperator op);

// Transform array to a result using the unary operator.
// Result need not be a contiguous array.
template<typename T, typename RES, typename UnaryOperator>
void arrayTransform (const Array<T>& arr,
                     Array<RES>& result, UnaryOperator op);

// Transform left and right to a result using the binary operator.
// The created and returned result array is contiguous.
template<typename T, typename BinaryOperator>
Array<T> arrayTransformResult (const Array<T>& left, const Array<T>& right,
                               BinaryOperator op);

// Transform left and right to a result using the binary operator.
// The created and returned result array is contiguous.
template<typename T, typename BinaryOperator>
Array<T> arrayTransformResult (const Array<T>& left, T right, BinaryOperator op);

// Transform left and right to a result using the binary operator.
// The created and returned result array is contiguous.
template<typename T, typename BinaryOperator>
Array<T> arrayTransformResult (T left, const Array<T>& right, BinaryOperator op);

// Transform array to a result using the unary operator.
// The created and returned result array is contiguous.
template<typename T, typename UnaryOperator>
Array<T> arrayTransformResult (const Array<T>& arr, UnaryOperator op);

// Transform left and right in place using the binary operator.
// The result is stored in the left array (useful for e.g. the += operation).
template<typename L, typename R, typename BinaryOperator>
inline void arrayTransformInPlace (Array<L>& left, const Array<R>& right,
                                   BinaryOperator op)
{
  if (left.contiguousStorage()  &&  right.contiguousStorage()) {
    transformInPlace (left.cbegin(), left.cend(), right.cbegin(), op);
  } else {
    transformInPlace (left.begin(), left.end(), right.begin(), op);
  }
}

// Transform left and right in place using the binary operator.
// The result is stored in the left array (useful for e.g. the += operation).
template<typename L, typename R, typename BinaryOperator>
inline void arrayTransformInPlace (Array<L>& left, R right, BinaryOperator op)
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
template<typename T, typename UnaryOperator>
inline void arrayTransformInPlace (Array<T>& arr, UnaryOperator op)
{
  if (arr.contiguousStorage()) {
    transformInPlace (arr.cbegin(), arr.cend(), op);
  } else {
    transformInPlace (arr.begin(), arr.end(), op);
  }
}
// </group>

// 
// Element by element arithmetic modifying left in-place. left and other
// must be conformant.
// <group>
template<class T> void operator+= (Array<T> &left, const Array<T> &other);
template<class T> void operator-= (Array<T> &left, const Array<T> &other);
template<class T> void operator*= (Array<T> &left, const Array<T> &other);
template<class T> void operator/= (Array<T> &left, const Array<T> &other);
template<class T> void operator%= (Array<T> &left, const Array<T> &other);
template<class T> void operator&= (Array<T> &left, const Array<T> &other);
template<class T> void operator|= (Array<T> &left, const Array<T> &other);
template<class T> void operator^= (Array<T> &left, const Array<T> &other);
// </group>

// 
// Element by element arithmetic modifying left in-place. The scalar "other"
// behaves as if it were a conformant Array to left filled with constant values.
// <group>
template<class T> void operator+= (Array<T> &left, const T &other);
template<class T> void operator-= (Array<T> &left, const T &other);
template<class T> void operator*= (Array<T> &left, const T &other);
template<class T> void operator/= (Array<T> &left, const T &other);
template<class T> void operator%= (Array<T> &left, const T &other);
template<class T> void operator&= (Array<T> &left, const T &other);
template<class T> void operator|= (Array<T> &left, const T &other);
template<class T> void operator^= (Array<T> &left, const T &other);
// </group>

// Unary arithmetic operation.
// 
// <group>
template<class T> Array<T> operator+(const Array<T> &a);
template<class T> Array<T> operator-(const Array<T> &a);
template<class T> Array<T> operator~(const Array<T> &a);
// </group>

// 
// Element by element arithmetic on two arrays, returning an array.
// <group>
template<class T> 
  Array<T> operator+ (const Array<T> &left, const Array<T> &right);
template<class T> 
  Array<T> operator- (const Array<T> &left, const Array<T> &right);
template<class T> 
  Array<T> operator* (const Array<T> &left, const Array<T> &right);
template<class T> 
  Array<T> operator/ (const Array<T> &left, const Array<T> &right);
template<class T> 
  Array<T> operator% (const Array<T> &left, const Array<T> &right);
template<class T> 
  Array<T> operator| (const Array<T> &left, const Array<T> &right);
template<class T> 
  Array<T> operator& (const Array<T> &left, const Array<T> &right);
template<class T> 
  Array<T> operator^ (const Array<T> &left, const Array<T> &right);
// </group>

// 
// Element by element arithmetic between an array and a scalar, returning
// an array.
// <group>
template<class T> 
    Array<T> operator+ (const Array<T> &left, const T &right);
template<class T> 
    Array<T> operator- (const Array<T> &left, const T &right);
template<class T> 
    Array<T> operator* (const Array<T> &left, const T &right);
template<class T> 
    Array<T> operator/ (const Array<T> &left, const T &right);
template<class T> 
    Array<T> operator% (const Array<T> &left, const T &right);
template<class T> 
    Array<T> operator| (const Array<T> &left, const T &right);
template<class T> 
    Array<T> operator& (const Array<T> &left, const T &right);
template<class T> 
    Array<T> operator^ (const Array<T> &left, const T &right);
// </group>

// 
// Element by element arithmetic between a scalar and an array, returning
// an array.
// <group>
template<class T>  
    Array<T> operator+ (const T &left, const Array<T> &right);
template<class T>  
    Array<T> operator- (const T &left, const Array<T> &right);
template<class T>  
    Array<T> operator* (const T &left, const Array<T> &right);
template<class T>  
    Array<T> operator/ (const T &left, const Array<T> &right);
template<class T>  
    Array<T> operator% (const T &left, const Array<T> &right);
template<class T>  
    Array<T> operator| (const T &left, const Array<T> &right);
template<class T>  
    Array<T> operator& (const T &left, const Array<T> &right);
template<class T>  
    Array<T> operator^ (const T &left, const Array<T> &right);
// </group>

// 
// Transcendental function that can be applied to essentially all numeric
// types. Works on an element-by-element basis.
// <group>
template<class T> Array<T> cos(const Array<T> &a);
template<class T> Array<T> cosh(const Array<T> &a);
template<class T> Array<T> exp(const Array<T> &a);
template<class T> Array<T> log(const Array<T> &a);
template<class T> Array<T> log10(const Array<T> &a);
template<class T> Array<T> pow(const Array<T> &a, const Array<T> &b);
template<class T> Array<T> pow(const T &a, const Array<T> &b);
template<class T> Array<T> sin(const Array<T> &a);
template<class T> Array<T> sinh(const Array<T> &a);
template<class T> Array<T> sqrt(const Array<T> &a);
// </group>

// 
// Transcendental function applied to the array on an element-by-element
// basis. Although a template function, this does not make sense for all
// numeric types.
// <group>
template<class T> Array<T> acos(const Array<T> &a);
template<class T> Array<T> asin(const Array<T> &a);
template<class T> Array<T> atan(const Array<T> &a);
template<class T> Array<T> atan2(const Array<T> &y, const Array<T> &x);
template<class T> Array<T> atan2(const T &y, const Array<T> &x);
template<class T> Array<T> atan2(const Array<T> &y, const T &x);
template<class T> Array<T> ceil(const Array<T> &a);
template<class T> Array<T> fabs(const Array<T> &a);
template<class T> Array<T> abs(const Array<T> &a);
template<class T> Array<T> floor(const Array<T> &a);
template<class T> Array<T> round(const Array<T> &a);
template<class T> Array<T> sign(const Array<T> &a);
template<class T> Array<T> fmod(const Array<T> &a, const Array<T> &b);
template<class T> Array<T> fmod(const T &a, const Array<T> &b);
template<class T> Array<T> fmod(const Array<T> &a, const T &b);
template<class T> Array<T> pow(const Array<T> &a, const Double &b);
template<class T> Array<T> tan(const Array<T> &a);
template<class T> Array<T> tanh(const Array<T> &a);
// N.B. fabs is deprecated. Use abs.
template<class T> Array<T> fabs(const Array<T> &a);
// </group>

// 
// <group>
// Find the minimum and maximum values of an array, including their locations.
template<class ScalarType>
void minMax(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos, 
	    IPosition &maxPos, const Array<ScalarType> &array);
// The array is searched at locations where the mask equals <src>valid</src>.
// (at least one such position must exist or an exception will be thrown).
// MaskType should be an Array of Bool.
template<class ScalarType>
void minMax(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos,
	    IPosition &maxPos, const Array<ScalarType> &array, 
	    const Array<Bool> &mask, Bool valid=True);
// The array * weight is searched 
template<class ScalarType>
void minMaxMasked(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos,
		  IPosition &maxPos, const Array<ScalarType> &array, 
		  const Array<ScalarType> &weight);
// </group>

// 
// The "min" and "max" functions require that the type "T" have comparison 
// operators.
// <group>
//
// This sets min and max to the minimum and maximum of the array to 
// avoid having to do two passes with max() and min() separately.
template<class T> void minMax(T &min, T &max, const Array<T> &a);
//
// The minimum element of the array.
// Requires that the type "T" has comparison operators.
template<class T>  T min(const Array<T> &a);
// The maximum element of the array.
// Requires that the type "T" has comparison operators.
template<class T>  T max(const Array<T> &a);

// "result" contains the maximum of "a" and "b" at each position. "result",
// "a", and "b" must be conformant.
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b);
// "result" contains the minimum of "a" and "b" at each position. "result",
// "a", and "b" must be conformant.
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b);
// Return an array that contains the maximum of "a" and "b" at each position.
// "a" and "b" must be conformant.
template<class T> Array<T> max(const Array<T> &a, const Array<T> &b);
template<class T> Array<T> max(const T &a, const Array<T> &b);
// Return an array that contains the minimum of "a" and "b" at each position.
// "a" and "b" must be conformant.
template<class T> Array<T> min(const Array<T> &a, const Array<T> &b);

// "result" contains the maximum of "a" and "b" at each position. "result",
// and "a" must be conformant.
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const T &b);
template<class T> inline void max(Array<T> &result, const T &a, 
                                  const Array<T> &b)
  { max (result, b, a); }
// "result" contains the minimum of "a" and "b" at each position. "result",
// and "a" must be conformant.
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const T &b);
template<class T> inline void min(Array<T> &result, const T &a, 
                                  const Array<T> &b)
  { min (result, b, a); }
// Return an array that contains the maximum of "a" and "b" at each position.
template<class T> Array<T> max(const Array<T> &a, const T &b);
template<class T> inline Array<T> max(const T &a, const Array<T> &b)
  { return max(b, a); }
// Return an array that contains the minimum of "a" and "b" at each position.
template<class T> Array<T> min(const Array<T> &a, const T &b);
template<class T> inline Array<T> min(const T &a, const Array<T> &b)
  { return min(b, a); }
// </group>

// 
// Fills all elements of "array" with a sequence starting with "start"
// and incrementing by "inc" for each element. The first axis varies
// most rapidly.
template<class T> void indgen(Array<T> &a, T start, T inc);
// 
// Fills all elements of "array" with a sequence starting with 0
// and ending with nelements() - 1. The first axis varies
// most rapidly.
template<class T> inline void indgen(Array<T> &a)
  { indgen(a, T(0), T(1)); }
// 
// Fills all elements of "array" with a sequence starting with start
// incremented by one for each position in the array. The first axis varies
// most rapidly.
template<class T> inline void indgen(Array<T> &a, T start)
  { indgen(a, start, T(1)); }

// Create a Vector of the given length and fill it with the start value
// incremented with <code>inc</code> for each element.
template<class T> inline Vector<T> indgen(uInt length, T start, T inc)
{
  Vector<T> x(length);
  indgen(x, start, inc);
  return x;
}


// Sum of every element of the array.
template<class T> T sum(const Array<T> &a);
// 
// Product of every element of the array. This could of course easily
// overflow.
template<class T> T product(const Array<T> &a);

// 
// The mean of "a" is the sum of all elements of "a" divided by the number
// of elements of "a".
template<class T> T mean(const Array<T> &a);

// 
// The variance of "a" is the sum of (a(i) - mean(a))**2/(a.nelements() - 1).
// N.B. N-1, not N in the denominator).
template<class T> T variance(const Array<T> &a);
// 
// The variance of "a" is the sum of (a(i) - mean(a))**2/(a.nelements() - 1).
// N.B. N-1, not N in the denominator).
// Rather than using a computed mean, use the supplied value.
template<class T> T variance(const Array<T> &a, T mean);

// 
// The standard deviation of "a" is the square root of its variance.
template<class T> T stddev(const Array<T> &a);
// 
// The standard deviation of "a" is the square root of its variance.
// Rather than using a computed mean, use the supplied value.
template<class T> T stddev(const Array<T> &a, T mean);

// 
// The average deviation of "a" is the sum of abs(a(i) - mean(a))/N. (N.B.
// N, not N-1 in the denominator).
template<class T> T avdev(const Array<T> &a);
// 
// The average deviation of "a" is the sum of abs(a(i) - mean(a))/N. (N.B.
// N, not N-1 in the denominator).
// Rather than using a computed mean, use the supplied value.
template<class T> T avdev(const Array<T> &a,T mean);

//
// The root-mean-square of "a" is the sqrt of sum(a*a)/N.
template<class T> T rms(const Array<T> &a);


// The median of "a" is a(n/2).
// If a has an even number of elements and the switch takeEvenMean is set,
// the median is 0.5*(a(n/2) + a((n+1)/2)).
// According to Numerical Recipes (2nd edition) it makes little sense to take
// the mean if the array is large enough (> 100 elements). Therefore
// the default for takeEvenMean is False if the array has > 100 elements,
// otherwise it is True.
// <br>If "sorted"==True we assume the data is already sorted and we
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
template<class T> T median(const Array<T> &a, Block<T> &tmp, Bool sorted,
			   Bool takeEvenMean, Bool inPlace=False);
template<class T> T median(const Array<T> &a, Bool sorted, Bool takeEvenMean,
			   Bool inPlace=False)
    { Block<T> tmp; return median (a, tmp, sorted, takeEvenMean, inPlace); }
template<class T> inline T median(const Array<T> &a, Bool sorted)
    { return median (a, sorted, (a.nelements() <= 100), False); }
template<class T> inline T median(const Array<T> &a)
    { return median (a, False, (a.nelements() <= 100), False); }
template<class T> inline T medianInPlace(const Array<T> &a, Bool sorted=False)
    { return median (a, sorted, (a.nelements() <= 100), True); }
// </group>

// The median absolute deviation from the median. Interface is as for
// the median functions
// <group>
template<class T> T madfm(const Array<T> &a, Block<T> &tmp, Bool sorted, 
                         Bool takeEvenMean, Bool inPlace = False);
template<class T> T madfm(const Array<T> &a, Bool sorted, Bool takeEvenMean,
                          Bool inPlace=False)
    { Block<T> tmp; return madfm(a, tmp, sorted, takeEvenMean, inPlace); }
template<class T> inline T madfm(const Array<T> &a, Bool sorted)
    { return madfm(a, sorted, (a.nelements() <= 100), False); }
template<class T> inline T madfm(const Array<T> &a)
    { return madfm(a, False, (a.nelements() <= 100), False); }
template<class T> inline T madfmInPlace(const Array<T> &a, Bool sorted=False)
    { return madfm(a, sorted, (a.nelements() <= 100), True); }
// </group>

// Return the fractile of an array.
// It returns the value at the given fraction of the array.
// A fraction of 0.5 is the same as the median, be it that no mean of
// the two middle elements is taken if the array has an even nr of elements.
// It uses kthLargest if the array is not sorted yet.
// <note>The function kthLargest in class GenSortIndirect can be used to
// obtain the index of the fractile in an array. </note>
template<class T> T fractile(const Array<T> &a, Block<T> &tmp, Float fraction,
			     Bool sorted=False, Bool inPlace=False);
template<class T> T fractile(const Array<T> &a, Float fraction,
			     Bool sorted=False, Bool inPlace=False)
  { Block<T> tmp; return fractile (a, tmp, fraction, sorted, inPlace); }

// Return the inter-fractile range of an array.  
// This is the full range between the bottom and the top fraction.
// <group>
template<class T> T interFractileRange(const Array<T> &a, Block<T> &tmp,
                                       Float fraction,
                                       Bool sorted=False, Bool inPlace=False);
template<class T> T interFractileRange(const Array<T> &a, Float fraction,
                                       Bool sorted=False, Bool inPlace=False)
  { Block<T> tmp; return interFractileRange(a, tmp, fraction, sorted, inPlace); }
// </group>

// Return the inter-hexile range of an array.  
// This is the full range between the bottom sixth and the top sixth
// of ordered array values. "The semi-interhexile range is very nearly
// equal to the rms for a Gaussian distribution, but it is much less
// sensitive to the tails of extended distributions." (Condon et al
// 1998)
// <group>
template<class T> T interHexileRange(const Array<T> &a, Block<T> &tmp,
                                     Bool sorted=False, Bool inPlace=False)
  { return interFractileRange(a, tmp, 1./6., sorted, inPlace); }
template<class T> T interHexileRange(const Array<T> &a, Bool sorted=False,
                                     Bool inPlace=False)
  { return interFractileRange(a, 1./6., sorted, inPlace); }
// </group>

// Return the inter-quartile range of an array.  
// This is the full range between the bottom quarter and the top
// quarter of ordered array values.
// <group>
template<class T> T interQuartileRange(const Array<T> &a, Block<T> &tmp,
                                       Bool sorted=False, Bool inPlace=False)
  { return interFractileRange(a, tmp, 0.25, sorted, inPlace); }
template<class T> T interQuartileRange(const Array<T> &a, Bool sorted=False,
                                       Bool inPlace=False)
  { return interFractileRange(a, 0.25, sorted, inPlace); }
// </group>


// Methods for element-by-element scaling of complex and real.
// Note that Complex and DComplex are typedefs for std::complex.
//<group>
template<typename T>
void operator*= (Array<std::complex<T> > &left, const Array<T> &other);
template<typename T>
void operator*= (Array<std::complex<T> > &left, const T &other);
template<typename T>
void operator/= (Array<std::complex<T> > &left, const Array<T> &other);
template<typename T>
void operator/= (Array<std::complex<T> > &left, const T &other);
template<typename T>
Array<std::complex<T> > operator* (const Array<std::complex<T> > &left,
                                   const Array<T> &right);
template<typename T>
Array<std::complex<T> > operator* (const Array<std::complex<T> > &left,
                                   const T &right);
template<typename T>
Array<std::complex<T> > operator* (const std::complex<T> &left,
                                   const Array<T> &right);
template<typename T>
Array<std::complex<T> > operator/ (const Array<std::complex<T> > &left,
                                   const Array<T> &right);
template<typename T>
Array<std::complex<T> > operator/ (const Array<std::complex<T> > &left,
                                   const T &right);
template<typename T>
Array<std::complex<T> > operator/ (const std::complex<T> &left,
                                   const Array<T> &right);
// </group>

// Returns the complex conjugate of a complex array.
//<group>
Array<Complex> conj(const Array<Complex> &carray);
Array<DComplex> conj(const Array<DComplex> &carray);
// Modifies rarray in place. rarray must be conformant.
void         conj(Array<Complex> &rarray, const Array<Complex> &carray);
void         conj(Array<DComplex> &rarray, const Array<DComplex> &carray);
//# The following are implemented to make the compiler find the right conversion
//# more often.
Matrix<Complex> conj(const Matrix<Complex> &carray);
Matrix<DComplex> conj(const Matrix<DComplex> &carray);
//</group>

// Form an array of complex numbers from the given real arrays.
// Note that Complex and DComplex are simply typedefs for std::complex<float>
// and std::complex<double>, so the result is in fact one of these types.
template<typename T>
Array<std::complex<T> > makeComplex(const Array<T> &real, const Array<T>& imag);

// Set the real part of the left complex array to the right real array.
template<typename L, typename R>
void setReal(Array<L> &carray, const Array<R> &rarray);

// Set the imaginary part of the left complex array to right real array.
template<typename R, typename L>
void setImag(Array<R> &carray, const Array<L> &rarray);

// Extracts the real part of a complex array into an array of floats.
// <group>
Array<Float>  real(const Array<Complex> &carray);
Array<Double> real(const Array<DComplex> &carray);
// Modifies rarray in place. rarray must be conformant.
void         real(Array<Float> &rarray, const Array<Complex> &carray);
void         real(Array<Double> &rarray, const Array<DComplex> &carray);
// </group>

// 
// Extracts the imaginary part of a complex array into an array of floats.
// <group>
Array<Float>  imag(const Array<Complex> &carray);
Array<Double> imag(const Array<DComplex> &carray);
// Modifies rarray in place. rarray must be conformant.
void         imag(Array<Float> &rarray, const Array<Complex> &carray);
void         imag(Array<Double> &rarray, const Array<DComplex> &carray);
// </group>

// 
// Extracts the amplitude (i.e. sqrt(re*re + im*im)) from an array
// of complex numbers. N.B. this is presently called "fabs" for a single
// complex number.
// <group>
Array<Float>  amplitude(const Array<Complex> &carray);
Array<Double> amplitude(const Array<DComplex> &carray);
// Modifies rarray in place. rarray must be conformant.
void         amplitude(Array<Float> &rarray, const Array<Complex> &carray);
void         amplitude(Array<Double> &rarray, const Array<DComplex> &carray);
// </group>

// 
// Extracts the phase (i.e. atan2(im, re)) from an array
// of complex numbers. N.B. this is presently called "arg"
// for a single complex number.
// <group>
Array<Float>  phase(const Array<Complex> &carray);
Array<Double> phase(const Array<DComplex> &carray);
// Modifies rarray in place. rarray must be conformant.
void         phase(Array<Float> &rarray, const Array<Complex> &carray);
void         phase(Array<Double> &rarray, const Array<DComplex> &carray);
// </group>

// Copy an array of complex into an array of real,imaginary pairs. The
// first axis of the real array becomes twice as long as the complex array.
// In the future versions which work by reference will be available; presently
// a copy is made.
Array<Float> ComplexToReal(const Array<Complex> &carray);
Array<Double> ComplexToReal(const Array<DComplex> &carray);
// Modify the array "rarray" in place. "rarray" must be the correct shape.
// <group>
void ComplexToReal(Array<Float> &rarray, const Array<Complex> &carray);
void ComplexToReal(Array<Double> &rarray, const Array<DComplex> &carray);
// </group>

// Copy an array of real,imaginary pairs into a complex array. The first axis
// must have an even length.
// In the future versions which work by reference will be available; presently
// a copy is made.
Array<Complex>  RealToComplex(const Array<Float> &rarray);
Array<DComplex> RealToComplex(const Array<Double> &rarray);
// Modify the array "carray" in place. "carray" must be the correct shape.
// <group>
void  RealToComplex(Array<Complex> &carray, const Array<Float> &rarray);
void  RealToComplex(Array<DComplex> &carray, const Array<Double> &rarray);
// </group>

// Make a copy of an array of a different type; for example make an array
// of doubles from an array of floats. Arrays to and from must be conformant
// (same shape). Also, it must be possible to convert a scalar of type U 
// to type T.
template<class T, class U> void convertArray(Array<T> &to,
					     const Array<U> &from);


// Returns an array where every element is squared.
template<class T> Array<T> square(const Array<T> &val);

// Returns an array where every element is cubed.
template<class T> Array<T> cube(const Array<T> &val);


// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/ArrayMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
