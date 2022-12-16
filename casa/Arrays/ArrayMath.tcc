//# ArrayMath.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2001,2003
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

#ifndef CASA_ARRAYMATH_2_TCC
#define CASA_ARRAYMATH_2_TCC

#include "ArrayMath.h"

#include "Array.h"
#include "ArrayIter.h"
#include "ArrayUtil.h"
#include "VectorIter.h"
#include "ArrayError.h"
#include "ElementFunctions.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <functional>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<typename L, typename R, typename RES, typename BinaryOperator, typename AllocL, typename AllocR, typename AllocRES>
void arrayTransform (const Array<L, AllocL>& left, const Array<R, AllocR>& right,
                     Array<RES, AllocRES>& result, BinaryOperator op)
{
  if (result.contiguousStorage()) {
    arrayContTransform (left, right, result, op);
  } else {
    if (left.contiguousStorage()  &&  right.contiguousStorage()) {
      std::transform (left.cbegin(), left.cend(), right.cbegin(),
                      result.begin(), op);
    } else {
      std::transform (left.begin(), left.end(), right.begin(),
                      result.begin(), op);
    }
  }
}

template<typename L, typename R, typename RES, typename BinaryOperator, typename AllocL, typename AllocRES>
void arrayTransform (const Array<L, AllocL>& left, R right,
                     Array<RES, AllocRES>& result, BinaryOperator op)
{
  if (result.contiguousStorage()) {
    arrayContTransform (left, right, result, op);
  } else {
    if (left.contiguousStorage()) {
      myrtransform (left.cbegin(), left.cend(), result.begin(), right, op);
    } else {
      myrtransform (left.begin(), left.end(), result.begin(), right, op);
    }
  }
}

template<typename L, typename R, typename RES, typename BinaryOperator, typename AllocR, typename AllocRES>
void arrayTransform (L left, const Array<R, AllocR>& right,
                     Array<RES, AllocRES>& result, BinaryOperator op)
{
  if (result.contiguousStorage()) {
    arrayContTransform (left, right, result, op);
  } else {
    if (right.contiguousStorage()) {
      myltransform (right.cbegin(), right.cend(), result.begin(), left, op);
    } else {
      myltransform (right.begin(), right.end(), result.begin(), left, op);
    }
  }
}

template<typename T, typename RES, typename UnaryOperator, typename AllocL, typename AllocRES>
void arrayTransform (const Array<T, AllocL>& arr,
                     Array<RES, AllocRES>& result, UnaryOperator op)
{
  if (result.contiguousStorage()) {
    arrayContTransform (arr, result, op);
  } else {
    if (arr.contiguousStorage()) {
      std::transform (arr.cbegin(), arr.cend(), result.begin(), op);
    } else {
      std::transform (arr.begin(), arr.end(), result.begin(), op);
    }
  }
}

template<typename T, typename BinaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (const Array<T, Alloc>& left, const Array<T, Alloc>& right,
                               BinaryOperator op)
{
  Array<T, Alloc> res(left.shape());
  arrayContTransform (left, right, res, op);
  return res;
}

template<typename T, typename BinaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (const Array<T, Alloc>& left, T right, BinaryOperator op)
{
  Array<T, Alloc> res(left.shape());
  arrayContTransform (left, right, res, op);
  return res;
}

template<typename T, typename BinaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (T left, const Array<T, Alloc>& right, BinaryOperator op)
{
  Array<T, Alloc> res(right.shape());
  arrayContTransform (left, right, res, op);
  return res;
}

template<typename T, typename UnaryOperator, typename Alloc>
Array<T, Alloc> arrayTransformResult (const Array<T, Alloc>& arr, UnaryOperator op)
{
  Array<T, Alloc> res(arr.shape());
  arrayContTransform (arr, res, op);
  return res;
}


// <thrown>
//   <item> ArrayError
// </thrown>
template<typename T, typename Alloc> 
void minMax(T &minVal, T &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<T, Alloc> &array) 
{
  size_t n = array.nelements();
  if (n == 0) {
    throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
                     "IPosition &maxPos, const Array<T, Alloc> &array) - "
                     "Array has no elements"));	
  }
  size_t minp = 0;
  size_t maxp = 0;
  T minv = array.data()[0];
  T maxv = minv;
  if (array.contiguousStorage()) {
    typename Array<T, Alloc>::const_contiter iter = array.cbegin();
    for (size_t i=0; i<n; ++i, ++iter) {
      if (*iter < minv) {
        minv = *iter;
        minp = i;
      } else if (*iter > maxv) {
        maxv = *iter;
        maxp = i;
      }
    }
  } else {
    typename Array<T, Alloc>::const_iterator iter = array.begin();
    for (size_t i=0; i<n; ++i, ++iter) {
      if (*iter < minv) {
        minv = *iter;
        minp = i;
      } else if (*iter > maxv) {
        maxv = *iter;
        maxp = i;
      }
    }
  }
  minPos.resize (array.ndim());
  maxPos.resize (array.ndim());
  minPos = toIPositionInArray (minp, array.shape());
  maxPos = toIPositionInArray (maxp, array.shape());
  minVal = minv;
  maxVal = maxv;
}



// <thrown>
//   <item> ArrayError
// </thrown>
template<typename T, typename Alloc> 
void minMaxMasked(T &minVal, T &maxVal, 
                  IPosition &minPos, IPosition &maxPos,
                  const Array<T, Alloc> &array, const Array<T, Alloc> &weight) 
{
  size_t n = array.nelements();
  if (n == 0) {
    throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
                     "IPosition &maxPos, const Array<T, Alloc> &array) - "
                     "const Array<T, Alloc> &weight) - " 
                     "Array has no elements"));	
  }
  if (! array.shape().isEqual (weight.shape())) {
    throw(ArrayConformanceError("void minMaxMasked(T &min, T &max,"
                                "IPosition &minPos, IPosition &maxPos, "
                                "const Array<T, Alloc> &array, "
                                "const Array<T, Alloc> &weight) - array " 
                                "and weight do not have the same shape()"));
  }
  size_t minp = 0;
  size_t maxp = 0;
  T minv = array.data()[0] * weight.data()[0];
  T maxv = minv;
  if (array.contiguousStorage()  &&  weight.contiguousStorage()) {
    typename Array<T, Alloc>::const_contiter iter = array.cbegin();
    typename Array<T, Alloc>::const_contiter witer = weight.cbegin();
    for (size_t i=0; i<n; ++i, ++iter, ++witer) {
      T tmp = *iter * *witer;
      if (tmp < minv) {
        minv = tmp;
        minp = i;
      } else if (tmp > maxv) {
        maxv = tmp;
        maxp = i;
      }
    }
  } else {
    typename Array<T, Alloc>::const_iterator iter = array.begin();
    typename Array<T, Alloc>::const_iterator witer = weight.begin();
    for (size_t i=0; i<n; ++i, ++iter, ++witer) {
      T tmp = *iter * *witer;
      if (tmp < minv) {
        minv = tmp;
        minp = i;
      } else if (tmp > maxv) {
        maxv = tmp;
        maxp = i;
      }
    }
  }
  minPos.resize (array.ndim());
  maxPos.resize (array.ndim());
  minPos = toIPositionInArray (minp, array.shape());
  maxPos = toIPositionInArray (maxp, array.shape());
  minVal = minv;
  maxVal = maxv;
}


// <thrown>
//   <item> ArrayError
//   <item> AipsError
// </thrown>
template<typename T, typename Alloc> 
void minMax(T &minVal, T &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<T, Alloc> &array, const Array<bool> &mask, bool valid)
{
  size_t n = array.nelements();
  if (n == 0) {
    throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
                     "IPosition &maxPos, const Array<T, Alloc> &array, "
                     "const Array<bool> &mask) - "
                     "Array has no elements"));	
  }
  if (! array.shape().isEqual (mask.shape())) {
    throw(ArrayConformanceError("void minMax(T &min, T &max,"
                                "IPosition &minPos, IPosition &maxPos,"
                                "const Array<T, Alloc> &array, "
                                "const Array<bool> &mask) - " 
                                "array and mask do not have the same shape()"));
  }
  size_t minp;
  size_t maxp;
  T minv = T();
  T maxv = T();
  if (array.contiguousStorage()  &&  mask.contiguousStorage()) {
    typename Array<T, Alloc>::const_contiter iter = array.cbegin();
    typename Array<bool>::const_contiter miter = mask.cbegin();
    size_t i=0;
    for (; i<n; ++i, ++iter, ++miter) {
      if (*miter == valid) {
        minv = maxv = *iter;
        break;
      }
    }
    minp = maxp = i;
    for (; i<n; ++i, ++iter, ++miter) {
      if (*miter == valid) {
        if (*iter < minv) {
          minv = *iter;
          minp = i;
        } else if (*iter > maxv) {
          maxv = *iter;
          maxp = i;
        }
      }
    }
  } else {
    typename Array<T, Alloc>::const_iterator iter = array.begin();
    typename Array<bool>::const_iterator miter = mask.begin();
    size_t i=0;
    for (; i<n; ++i, ++iter, ++miter) {
      if (*miter == valid) {
        minv = maxv = *iter;
        break;
      }
    }
    minp = maxp = i;
    for (; i<n; ++i, ++iter, ++miter) {
      if (*miter == valid) {
        if (*iter < minv) {
          minv = *iter;
          minp = i;
        } else if (*iter > maxv) {
          maxv = *iter;
          maxp = i;
        }
      }
    }
  }
  if (minp ==n) {
    throw(std::runtime_error("void minMax(T &min, T &max,"
                    "IPosition &minPos, IPosition &maxPos,"
                    "const Array<T, Alloc> &array, "
                    "const Array<bool> &mask) - no valid array elements"));
  }
  minPos.resize (array.ndim());
  maxPos.resize (array.ndim());
  minPos = toIPositionInArray (minp, array.shape());
  maxPos = toIPositionInArray (maxp, array.shape());
  minVal = minv;
  maxVal = maxv;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void operator+= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "+=");
    arrayTransformInPlace (left, other, std::plus<T>());
}

template<typename T, typename Alloc>  T min(const Array<T, Alloc> &a)
    { T Min, Max; minMax(Min, Max, a); return Min; }

template<typename T, typename Alloc>  T max(const Array<T, Alloc> &a)
    { T Min, Max; minMax(Min, Max, a); return Max; }

template<typename T, typename Alloc> void operator+= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::plus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void operator-= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "-=");
    arrayTransformInPlace (left, other, std::minus<T>());
}

template<typename T, typename Alloc> void operator-= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::minus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void operator%= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "%=");
    arrayTransformInPlace (left, other, std::modulus<T>());
}

template<typename T, typename Alloc> void operator%= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::modulus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void operator&= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "&=");
    arrayTransformInPlace (left, other, [](T a, T b){ return a&b; });
}

template<typename T, typename Alloc> void operator&= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, [](T a, T b){ return a&b; });
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void operator|= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "|=");
    arrayTransformInPlace (left, other, [](T a, T b){ return a|b; });
}

template<typename T, typename Alloc> void operator|= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, [](T a, T b){ return a|b; });
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void operator^= (Array<T, Alloc> &left, const Array<T, Alloc> &other)
{
    checkArrayShapes (left, other, "^=");
    arrayTransformInPlace (left, other, [](T a, T b){ return a^b; });
}

template<typename T, typename Alloc> void operator^= (Array<T, Alloc> &left, const T &other)
{
    arrayTransformInPlace (left, other, [](T a, T b){ return a^b; });
}

template<typename T, typename Alloc> Array<T, Alloc> operator+(const Array<T, Alloc> &a)
{
    return a.copy();
}

template<typename T, typename Alloc> Array<T, Alloc> operator-(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, std::negate<T>());
}

template<typename T, typename Alloc> Array<T, Alloc> operator~(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T val) { return ~val; }); // bit_not would be nicer, but is C++14
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc>
   Array<T, Alloc> operator+(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "+");
    return arrayTransformResult (left, right, std::plus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc>
   Array<T, Alloc> operator-(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "-");
    return arrayTransformResult (left, right, std::minus<T>());
}


// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc>
   Array<T, Alloc> operator/(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "/");
    return arrayTransformResult (left, right, std::divides<T>());
}

template<typename T, typename Alloc>
   Array<T, Alloc> operator%(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, std::modulus<T>());
}

template<typename T, typename Alloc>
   Array<T, Alloc> operator&(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, [](T a, T b){ return a&b; });
}

template<typename T, typename Alloc>
   Array<T, Alloc> operator|(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, [](T a, T b){ return a|b; });
}

template<typename T, typename Alloc>
   Array<T, Alloc> operator^(const Array<T, Alloc> &left, const Array<T, Alloc> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, [](T a, T b){ return a^b; });
}

template<class T, typename Alloc> 
Array<T, Alloc> operator+ (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, std::plus<T>());
}

template<class T, typename Alloc> 
Array<T, Alloc> operator- (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, std::minus<T>());
}



template<class T, typename Alloc> 
Array<T, Alloc> operator/ (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, std::divides<T>());
}

template<class T, typename Alloc> 
Array<T, Alloc> operator% (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, std::modulus<T>());
}

template<class T, typename Alloc> 
Array<T, Alloc> operator& (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, [](T a, T b){ return a&b; });
}

template<class T, typename Alloc> 
Array<T, Alloc> operator| (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, [](T a, T b){ return a|b; });
}

template<class T, typename Alloc> 
Array<T, Alloc> operator^ (const Array<T, Alloc> &left, const T &right)
{
    return arrayTransformResult (left, right, [](T a, T b){ return a^b; });
}

template<class T, typename Alloc> 
Array<T, Alloc> operator+ (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, std::plus<T>());
}

template<class T, typename Alloc> 
Array<T, Alloc> operator- (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, std::minus<T>());
}


template<class T, typename Alloc> 
Array<T, Alloc> operator/ (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, std::divides<T>());
}

template<class T, typename Alloc> 
Array<T, Alloc> operator% (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, std::modulus<T>());
}

template<class T, typename Alloc> 
Array<T, Alloc> operator& (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, [](T a, T b){ return a&b; });
}

template<class T, typename Alloc> 
Array<T, Alloc> operator| (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, [](T a, T b){ return a|b; });
}

template<class T, typename Alloc> 
Array<T, Alloc> operator^ (const T &left, const Array<T, Alloc> &right)
{
    return arrayTransformResult (left, right, [](T a, T b){ return a^b; });
}

// <thrown>
//   </item> ArrayError
// </thrown>
template<typename T, typename Alloc> void minMax(T &minVal, T &maxVal, const Array<T, Alloc> &array)
{
  if (array.nelements() == 0) {
    throw(ArrayError("void minMax(T &min, T &max, const Array<T, Alloc> &array) - "
                     "Array has no elements"));	
  }
  if (array.contiguousStorage()) {
    // minimal scope as some compilers may spill onto stack otherwise
    T minv = array.data()[0];
    T maxv = minv;
    typename Array<T, Alloc>::const_contiter iterEnd = array.cend();
    for (typename Array<T, Alloc>::const_contiter iter = array.cbegin();
         iter!=iterEnd; ++iter) {
      if (*iter < minv) {
        minv = *iter;
      }
      // no else allows compiler to use branchless instructions
      if (*iter > maxv) {
        maxv = *iter;
      }
    }
    maxVal = maxv;
    minVal = minv;
  } else {
    T minv = array.data()[0];
    T maxv = minv;
    typename Array<T, Alloc>::const_iterator iterEnd = array.end();
    for (typename Array<T, Alloc>::const_iterator iter = array.begin();
         iter!=iterEnd; ++iter) {
      if (*iter < minv) {
        minv = *iter;
      }
      if (*iter > maxv) {
        maxv = *iter;
      }
    }
    maxVal = maxv;
    minVal = minv;
  }
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void max(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const Array<T, Alloc> &b)
{
    checkArrayShapes (a, b, "max");
    checkArrayShapes (a, result, "max");
    arrayTransform (a, b, result, [](T l, T r){ return std::max<T>(l, r); });
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void min(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const Array<T, Alloc> &b)
{
    checkArrayShapes (a, b, "min");
    checkArrayShapes (a, result, "min");
    arrayTransform (a, b, result, [](T l, T r){ return std::min<T>(l, r); });
}

template<typename T, typename Alloc> Array<T, Alloc> max(const Array<T, Alloc> &a, const Array<T, Alloc> &b)
{
    Array<T, Alloc> result(a.shape());
    max(result, a, b);
    return result;
}

template<typename T, typename Alloc> Array<T, Alloc> min(const Array<T, Alloc> &a, const Array<T, Alloc> &b)
{
    Array<T, Alloc> result(a.shape());
    min(result, a, b);
    return result;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void max(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const T &b)
{
    checkArrayShapes (a, result, "max");
    arrayTransform (a, b, result, [](T a, T b){ return std::max<T>(a, b); });
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> void min(Array<T, Alloc> &result, const Array<T, Alloc> &a, 
			   const T &b)
{
    checkArrayShapes (a, result, "min");
    arrayTransform (a, b, result, [](T a, T b){ return std::min<T>(a, b); });
}

template<typename T, typename Alloc> Array<T, Alloc> max(const Array<T, Alloc> &a, const T &b)
{
    Array<T, Alloc> result(a.shape());
    max(result, a, b);
    return result;
}

template<typename T, typename Alloc> Array<T, Alloc> min(const Array<T, Alloc> &a, const T &b)
{
    Array<T, Alloc> result(a.shape());
    min(result, a, b);
    return result;
}

template<typename T, typename Alloc>
void indgen(Array<T, Alloc> &a, T start, T inc)
{
  if (a.contiguousStorage()) {
    typename Array<T, Alloc>::contiter aend = a.cend();
    for (typename Array<T, Alloc>::contiter iter=a.cbegin(); iter!=aend; ++iter) {
      *iter = start;
      start += inc;
    }
  } else {
    typename Array<T, Alloc>::iterator aend = a.end();
    for (typename Array<T, Alloc>::iterator iter=a.begin(); iter!=aend; ++iter) {
      *iter = start;
      start += inc;
    }
  }
}

template<typename T, typename Alloc> Array<T, Alloc> cos(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T v){ return std::cos(v); });
}

template<typename T, typename Alloc> Array<T, Alloc> cosh(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T v){ return std::cosh(v); });
}

template<typename T, typename Alloc> Array<T, Alloc> exp(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T v){ return std::exp(v); });
}

template<typename T, typename Alloc> Array<T, Alloc> log(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T v){ return std::log(v); });
}

template<typename T, typename Alloc> Array<T, Alloc> log10(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T v){ return std::log10(v); });
}

template<typename T, typename Alloc> Array<T, Alloc> sin(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T v){ return std::sin(v); });
}

template<typename T, typename Alloc> Array<T, Alloc> sinh(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T v){ return std::sinh(v); });
}

template<typename T, typename Alloc> Array<T, Alloc> sqrt(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T a){ return std::sqrt(a);});
}

template<typename T, typename Alloc> Array<T, Alloc> square(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T a){ return a*a; });
}

template<typename T, typename Alloc> Array<T, Alloc> cube(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T a){ return a*a*a; });
}

template<typename T, typename Alloc> Array<T, Alloc> acos(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T a){ return std::acos(a); });
}

template<typename T, typename Alloc> Array<T, Alloc> asin(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T a){ return std::asin(a); });
}

template<typename T, typename Alloc> Array<T, Alloc> atan(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T a){ return std::atan(a); });
}

template<typename T, typename Alloc> Array<T, Alloc> ceil(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T val) { return std::ceil(val);});
}

template<typename T, typename Alloc> Array<T, Alloc> fabs(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T t){ return std::abs(t); });
}

template<typename T, typename Alloc> Array<T, Alloc> abs(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T t){ return std::abs(t); });
}

template<typename T, typename Alloc> Array<T, Alloc> floor(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T t){ return std::floor(t); });
}

template<typename T, typename Alloc> Array<T, Alloc> round(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T t){ return std::round(t); });
}

template<typename T, typename Alloc> Array<T, Alloc> sign(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T value) { return (value<0 ? -1 : (value>0 ? 1:0));});
}

template<typename T, typename Alloc> Array<T, Alloc> tan(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T t){ return std::tan(t); });
}

template<typename T, typename Alloc> Array<T, Alloc> tanh(const Array<T, Alloc> &a)
{
    return arrayTransformResult (a, [](T t){ return std::tanh(t); });
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Array<T, Alloc> pow(const Array<T, Alloc> &a, const Array<T, Alloc> &b)
{
    checkArrayShapes (a, b, "pow");
    return arrayTransformResult (a, b, [](T l, T r) { return std::pow(l, r); });
}

template<typename T, typename Alloc> Array<T, Alloc> pow(const T &a, const Array<T, Alloc> &b)
{
    return arrayTransformResult (a, b, [](T l, T r) { return std::pow(l, r); });
}

template<typename T, typename Alloc> Array<T, Alloc> pow(const Array<T, Alloc> &a, const double &b)
{
    Array<T, Alloc> result(a.shape());
    arrayContTransform (a, b, result, [](T l, T r) { return std::pow(l, r); });
    return result;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Array<T, Alloc> atan2(const Array<T, Alloc> &a, const Array<T, Alloc> &b)
{
    checkArrayShapes (a, b, "atan2");
    return arrayTransformResult (a, b, [](T l, T r) { return std::atan2(l,r);});
}

template<typename T, typename Alloc> Array<T, Alloc> atan2(const T &a, const Array<T, Alloc> &b)
{
    return arrayTransformResult (a, b, [](T l, T r) { return std::atan2(l,r);});
}

template<typename T, typename Alloc> Array<T, Alloc> atan2(const Array<T, Alloc> &a, const T &b)
{
    return arrayTransformResult (a, b, [](T l, T r) { return std::atan2(l,r);});
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Array<T, Alloc> fmod(const Array<T, Alloc> &a, const Array<T, Alloc> &b)
{
    checkArrayShapes (a, b, "fmod");
    return arrayTransformResult (a, b, [](T l, T r) { return std::fmod(l,r);});
}

template<typename T, typename Alloc> Array<T, Alloc> fmod(const T &a, const Array<T, Alloc> &b)
{
    return arrayTransformResult (a, b, [](T l, T r) { return std::fmod(l,r);});
}

template<typename T, typename Alloc> Array<T, Alloc> fmod(const Array<T, Alloc> &a, const T &b)
{
    return arrayTransformResult (a, b, [](T l, T r) { return std::fmod(l,r);});
}


// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T, typename Alloc> Array<T, Alloc> floormod(const Array<T, Alloc> &a, const Array<T, Alloc> &b)
{
    checkArrayShapes (a, b, "floormod");
    return arrayTransformResult (a, b, static_cast<T (*)(T,T)>(arrays_internal::floormod));
}

template<typename T, typename Alloc> Array<T, Alloc> floormod(const T &a, const Array<T, Alloc> &b)
{
    return arrayTransformResult (a, b, static_cast<T (*)(T,T)>(arrays_internal::floormod));
}

template<typename T, typename Alloc> Array<T, Alloc> floormod(const Array<T, Alloc> &a, const T &b)
{
    return arrayTransformResult (a, b, static_cast<T (*)(T,T)>(arrays_internal::floormod));
}


// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T sum(const Array<T, Alloc> &a)
{
  return a.contiguousStorage() ?
    std::accumulate(a.cbegin(), a.cend(), T(), std::plus<T>()) :
    std::accumulate(a.begin(),  a.end(),  T(), std::plus<T>());
}

template<typename T, typename Alloc> T sumsqr(const Array<T, Alloc> &a)
{
  auto sumsqr = [](T left, T right) { return left + right*right;};
  return a.contiguousStorage() ?
    std::accumulate(a.cbegin(), a.cend(), T(), sumsqr) :
    std::accumulate(a.begin(),  a.end(),  T(), sumsqr);
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T product(const Array<T, Alloc> &a)
{
  if (a.empty()) {
    return T();
  }
  // Get first element, because T(1) may not work for all types.
  T prod = *a.data();
  if (a.contiguousStorage()) {
    typename Array<T, Alloc>::const_contiter iter(a.cbegin());
    ++iter;
    return std::accumulate(iter, a.cend(), prod, std::multiplies<T>());
  } else {
    typename Array<T, Alloc>::const_iterator iter(a.begin());
    ++iter;
    return std::accumulate(iter, a.end(),  prod, std::multiplies<T>());
  }
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T mean(const Array<T, Alloc> &a)
{
    if (a.empty()) {
	throw(ArrayError("::mean(const Array<T, Alloc> &) - 0 element array"));
    }
    return T(sum(a)/T(1.0*a.nelements()));
}

// <thrown>
//    </item> ArrayError
// </thrown>
// Similar to numpy the ddof argument can be used to get the population
// variance (ddof=0) or the sample variance (ddof=1).
template<typename T, typename Alloc> T pvariance(const Array<T, Alloc> &a, T mean, size_t ddof)
{
  if (a.nelements() < ddof+1) {
    throw(ArrayError("::variance(const Array<T, Alloc> &) - Need at least " +
                     std::to_string(ddof+1) + 
                     " elements"));
  }
  T sum = a.contiguousStorage() ?
    std::accumulate(a.cbegin(), a.cend(), T(), arrays_internal::SumSqrDiff<T>(mean)) :
    std::accumulate(a.begin(),  a.end(),  T(), arrays_internal::SumSqrDiff<T>(mean));
  return T(sum/T(1.0*a.nelements() - ddof));
}
template<typename T, typename Alloc> T variance(const Array<T, Alloc> &a, T mean)
{
  return pvariance (a, mean, 1);
}
template<typename T, typename Alloc> T pvariance(const Array<T, Alloc> &a, size_t ddof)
{
  return pvariance(a, mean(a), ddof);
}
template<typename T, typename Alloc> T variance(const Array<T, Alloc> &a)
{
  return pvariance(a, mean(a), 1);
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T pstddev(const Array<T, Alloc> &a, T mean, size_t ddof)
{
  if (a.nelements() < ddof+1) {
    throw(ArrayError("::stddev(const Array<T, Alloc> &) - Need at least " +
                     std::to_string(ddof+1) + 
                     " elements"));
  }
  return std::sqrt(pvariance(a, mean, ddof));
}
template<typename T, typename Alloc> T stddev(const Array<T, Alloc> &a, T mean)
{
  return pstddev (a, mean, 1);
}
template<typename T, typename Alloc> T pstddev(const Array<T, Alloc> &a, size_t ddof)
{
  return pstddev (a, mean(a), ddof);
}
template<typename T, typename Alloc> T stddev(const Array<T, Alloc> &a)
{
  return pstddev (a, mean(a), 1);
}


// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T avdev(const Array<T, Alloc> &a)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::avdev(const Array<T, Alloc> &,) - Need at least 1 "
			 "element"));
    }
    return avdev(a, mean(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T avdev(const Array<T, Alloc> &a, T mean)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::avdev(const Array<T, Alloc> &,T) - Need at least 1 "
			 "element"));
    }
    auto sumabsdiff = [mean](T left, T right) { return left + std::abs(right-mean); };
    T sum = a.contiguousStorage() ?
      std::accumulate(a.cbegin(), a.cend(), T(), sumabsdiff) :
      std::accumulate(a.begin(),  a.end(),  T(), sumabsdiff);
    return T(sum/T(1.0*a.nelements()));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T rms(const Array<T, Alloc> &a)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::rms(const Array<T, Alloc> &) - Need at least 1 "
			 "element"));
    }
    auto sumsqr = [](T left, T right) { return left + right*right; };
  T sum = a.contiguousStorage() ?
      std::accumulate(a.cbegin(), a.cend(), T(), sumsqr) :
      std::accumulate(a.begin(),  a.end(),  T(), sumsqr);
    return T(std::sqrt(sum/T(1.0*a.nelements())));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T median(const Array<T, Alloc> &a, std::vector<T>& scratch, bool sorted,
  bool takeEvenMean, bool inPlace)
{
  T medval=T();
  size_t nelem = a.nelements();
  if (nelem < 1) {
    throw(ArrayError("::median(T*) - array needs at least 1 element"));
  }
  //# Mean does not have to be taken for odd number of elements.
  if (nelem%2 != 0) {
    takeEvenMean = false;
  }
  // A copy is needed if not contiguous or if not in place.
  const T* storage;
  if (!a.contiguousStorage() || !inPlace)
  {
    a.tovector(scratch);
    storage = scratch.data();
  }
  else {
    storage = a.data();
  }
  T* data = const_cast<T*>(storage);
  size_t n2 = (nelem - 1)/2;
  if (!sorted)
  {
    std::nth_element(data, data+n2, data+nelem);
    medval = data[n2];
    if (takeEvenMean)
    {
      std::nth_element(data, data+n2+1, data+nelem);
      medval = T(0.5 * (medval + data[n2+1]));
    }
  }
  else {
    if (takeEvenMean) {
        medval = T(0.5 * (data[n2] + data[n2+1]));
    } else {
        medval = data[n2];
    }
  }
  return medval;
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T madfm(const Array<T, Alloc> &a, std::vector<T>& scratch, bool sorted,
  bool takeEvenMean, bool inPlace)
{
  T med = median(a, scratch, sorted, takeEvenMean, inPlace);
  Array<T, Alloc> atmp;
  if (inPlace  &&  a.contiguousStorage()) {
    atmp.reference (a);   // remove constness
  } else {
    // A copy of a has been made to scratch.
    // Using it saves computing.
    // (this was changed for array2: sharing storage is no longer possible)
    assert(a.size() == scratch.size());
    atmp.resize(a.shape());
    atmp.assign_conforming (Array<T, Alloc>(a.shape(), scratch.data()));
  }
  T* aptr = atmp.data();
  for (size_t i=0; i<atmp.size(); ++i) {
    aptr[i] = std::abs(aptr[i] - med);
  }
  return median(atmp, scratch, false, takeEvenMean, true);
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc> T fractile(const Array<T, Alloc> &a, std::vector<T>& scratch, float fraction,
			     bool sorted, bool inPlace)
{
  if (fraction < 0  ||  fraction > 1) {
    throw(ArrayError("::fractile(const Array<T, Alloc>&) - fraction <0 or >1 "));
  }    
  size_t nelem = a.nelements();
  if (nelem < 1) {
    throw(ArrayError("::fractile(const Array<T, Alloc>&) - Need at least 1 "
      "elements"));
  }
  // A copy is needed if not contiguous or if not in place.
  const T* storage = a.data();
  if (!a.contiguousStorage() || !inPlace)
  {
    a.tovector(scratch);
    storage = scratch.data();
  }
  T* data = const_cast<T*>(storage);
  size_t n2 = size_t((nelem - 1) * double(fraction) + 0.01);
  if (!sorted)
  {
    std::nth_element(data, data+n2, data+nelem);
  }  
  return data[n2];
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<typename T, typename Alloc>
T interFractileRange(const Array<T, Alloc> &a, std::vector<T>& scratch,
  float fraction, bool sorted, bool inPlace)
{
  if (!(fraction>0  &&  fraction<0.5))
    throw std::runtime_error("interFractileRange: invalid parameter");
  T hex1, hex2;
  hex1 = fractile(a, scratch, fraction, sorted, inPlace);
  if (inPlace  &&  a.contiguousStorage()) {  
    hex2 = fractile(a, scratch, 1-fraction, sorted, inPlace);
  } else {
    // In this case a copy of a has been made to scratch.
    // Using it saves making another copy.
    if (a.size() != scratch.size())
      throw std::runtime_error("interFractileRange: array sizes don't match");
    Array<T, Alloc> atmp(a.shape(), scratch.data(), SHARE);
    hex2 = fractile(atmp, scratch, 1-fraction, sorted, inPlace);
  }
  return (hex2 - hex1);
}

template<typename T, typename Alloc>
Array<std::complex<T> > makeComplex(const Array<T, Alloc> &left, const Array<T, Alloc>& right)
{
  checkArrayShapes (left, right, "makeComplex");
  Array<std::complex<T> > res(left.shape());
  arrayContTransform (left, right, res,
                      [](T r, T i) { return std::complex<T>(r, i);});
  return res;
}

template<typename T, typename Alloc>
Array<std::complex<T> > makeComplex(const T &left, const Array<T, Alloc>& right)
{
  Array<std::complex<T> > res(right.shape());
  arrayContTransform (left, right, res,
                      [](T r, T i) { return std::complex<T>(r, i);});
  return res;
}

template<typename T, typename Alloc>
Array<std::complex<T> > makeComplex(const Array<T, Alloc> &left, const T& right)
{
  Array<std::complex<T> > res(left.shape());
  arrayContTransform (left, right, res,
                      [](T r, T i) { return std::complex<T>(r, i);});
  return res;
}

template<typename C, typename R, typename AllocC, typename AllocR>
void setReal(Array<C, AllocC> &carray, const Array<R, AllocR> &rarray)
{
  checkArrayShapes (carray, rarray, "setReal");
  // Cannot be done in place, because imag is taken from second operand.
  arrayTransform (rarray, carray, carray, 
                  [](R l, C r)->C { return C(l, std::imag(r)); });
}

template<typename C, typename R, typename AllocC, typename AllocR>
void setImag(Array<C, AllocC> &carray, const Array<R, AllocR> &rarray)
{
  checkArrayShapes (carray, rarray, "setImag");
  arrayTransformInPlace (carray, rarray,
                  [](C l, R r)->C { return C(std::real(l), r); });
}

template<typename T, typename U, typename AllocT, typename AllocU>
void convertArray(Array<T, AllocT> &to, const Array<U, AllocU> &from)
{
    if (to.nelements() == 0 && from.nelements() == 0) {
	return;
    }
    if (to.shape() != from.shape()) {
	throw(ArrayConformanceError("void ::convertArray(Array<T, Alloc> &to, "
				    "const Array<U, AllocU> &from)"
				    " - arrays do not conform"));
    }
    if (to.contiguousStorage()  &&  from.contiguousStorage()) {
      typename Array<U, AllocU>::const_contiter endFrom = from.cend();
      typename Array<U, AllocU>::const_contiter iterFrom = from.cbegin();
      for (typename Array<T, AllocT>::contiter iterTo = to.cbegin();
	   iterFrom != endFrom;
	   ++iterFrom, ++iterTo) {
	arrays_internal::convertScalar (*iterTo, *iterFrom);
      }
    } else {
      typename Array<U, AllocU>::const_iterator endFrom = from.end();
      typename Array<U, AllocU>::const_iterator iterFrom = from.begin();
      for (typename Array<T, AllocT>::iterator iterTo = to.begin();
	   iterFrom != endFrom;
	   ++iterFrom, ++iterTo) {
	arrays_internal::convertScalar (*iterTo, *iterFrom);
      }
    }
}

} //# NAMESPACE CASACORE - END

#endif
