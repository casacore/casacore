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
//#
//# $Id$

#ifndef CASA_ARRAYMATH_TCC
#define CASA_ARRAYMATH_TCC

#include <casacore/casa/iostream.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicMath/ConvertScalar.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <algorithm>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


template<typename L, typename R, typename RES, typename BinaryOperator>
void arrayTransform (const Array<L>& left, const Array<R>& right,
                     Array<RES>& result, BinaryOperator op)
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

template<typename L, typename R, typename RES, typename BinaryOperator>
void arrayTransform (const Array<L>& left, R right,
                     Array<RES>& result, BinaryOperator op)
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

template<typename L, typename R, typename RES, typename BinaryOperator>
void arrayTransform (L left, const Array<R>& right,
                     Array<RES>& result, BinaryOperator op)
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

template<typename T, typename RES, typename UnaryOperator>
void arrayTransform (const Array<T>& arr,
                     Array<RES>& result, UnaryOperator op)
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

template<typename T, typename BinaryOperator>
Array<T> arrayTransformResult (const Array<T>& left, const Array<T>& right,
                               BinaryOperator op)
{
  Array<T> res(left.shape());
  arrayContTransform (left, right, res, op);
  return res;
}

template<typename T, typename BinaryOperator>
Array<T> arrayTransformResult (const Array<T>& left, T right, BinaryOperator op)
{
  Array<T> res(left.shape());
  arrayContTransform (left, right, res, op);
  return res;
}

template<typename T, typename BinaryOperator>
Array<T> arrayTransformResult (T left, const Array<T>& right, BinaryOperator op)
{
  Array<T> res(right.shape());
  arrayContTransform (left, right, res, op);
  return res;
}

template<typename T, typename UnaryOperator>
Array<T> arrayTransformResult (const Array<T>& arr, UnaryOperator op)
{
  Array<T> res(arr.shape());
  arrayContTransform (arr, res, op);
  return res;
}


// <thrown>
//   <item> ArrayError
// </thrown>
template<class T> 
void minMax(T &minVal, T &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<T> &array) 
{
  size_t n = array.nelements();
  if (n == 0) {
    throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
                     "IPosition &maxPos, const Array<T> &array) - "
                     "Array has no elements"));	
  }
  size_t minp = 0;
  size_t maxp = 0;
  T minv = array.data()[0];
  T maxv = minv;
  if (array.contiguousStorage()) {
    typename Array<T>::const_contiter iter = array.cbegin();
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
    typename Array<T>::const_iterator iter = array.begin();
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
template<class T> 
void minMaxMasked(T &minVal, T &maxVal, 
                  IPosition &minPos, IPosition &maxPos,
                  const Array<T> &array, const Array<T> &weight) 
{
  size_t n = array.nelements();
  if (n == 0) {
    throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
                     "IPosition &maxPos, const Array<T> &array) - "
                     "const Array<T> &weight) - " 
                     "Array has no elements"));	
  }
  if (! array.shape().isEqual (weight.shape())) {
    throw(ArrayConformanceError("void minMaxMasked(T &min, T &max,"
                                "IPosition &minPos, IPosition &maxPos, "
                                "const Array<T> &array, "
                                "const Array<T> &weight) - array " 
                                "and weight do not have the same shape()"));
  }
  size_t minp = 0;
  size_t maxp = 0;
  T minv = array.data()[0] * weight.data()[0];
  T maxv = minv;
  if (array.contiguousStorage()  &&  weight.contiguousStorage()) {
    typename Array<T>::const_contiter iter = array.cbegin();
    typename Array<T>::const_contiter witer = weight.cbegin();
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
    typename Array<T>::const_iterator iter = array.begin();
    typename Array<T>::const_iterator witer = weight.begin();
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
template<class T> 
void minMax(T &minVal, T &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<T> &array, const Array<Bool> &mask, Bool valid)
{
  size_t n = array.nelements();
  if (n == 0) {
    throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
                     "IPosition &maxPos, const Array<T> &array, "
                     "const Array<Bool> &mask) - "
                     "Array has no elements"));	
  }
  if (! array.shape().isEqual (mask.shape())) {
    throw(ArrayConformanceError("void minMax(T &min, T &max,"
                                "IPosition &minPos, IPosition &maxPos,"
                                "const Array<T> &array, "
                                "const Array<Bool> &mask) - " 
                                "array and mask do not have the same shape()"));
  }
  size_t minp;
  size_t maxp;
  T minv = T();
  T maxv = T();
  if (array.contiguousStorage()  &&  mask.contiguousStorage()) {
    typename Array<T>::const_contiter iter = array.cbegin();
    typename Array<Bool>::const_contiter miter = mask.cbegin();
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
    typename Array<T>::const_iterator iter = array.begin();
    typename Array<Bool>::const_iterator miter = mask.begin();
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
    throw(AipsError("void minMax(T &min, T &max,"
                    "IPosition &minPos, IPosition &maxPos,"
                    "const Array<T> &array, "
                    "const Array<Bool> &mask) - no valid array elements"));
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
template<class T> void operator+= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "+=");
    arrayTransformInPlace (left, other, std::plus<T>());
}

template<class T>  T min(const Array<T> &a)
    { T Min, Max; minMax(Min, Max, a); return Min; }

template<class T>  T max(const Array<T> &a)
    { T Min, Max; minMax(Min, Max, a); return Max; }

template<class T> void operator+= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::plus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator-= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "-=");
    arrayTransformInPlace (left, other, std::minus<T>());
}

template<class T> void operator-= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::minus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator*= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "*=");
    arrayTransformInPlace (left, other, std::multiplies<T>());
}

template<class T> void operator*= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::multiplies<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator/= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "/=");
    arrayTransformInPlace (left, other, std::divides<T>());
}

template<class T> void operator/= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, std::divides<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator%= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "%=");
    arrayTransformInPlace (left, other, Modulo<T>());
}

template<class T> void operator%= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, Modulo<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator&= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "&=");
    arrayTransformInPlace (left, other, BitAnd<T>());
}

template<class T> void operator&= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, BitAnd<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator|= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "|=");
    arrayTransformInPlace (left, other, BitOr<T>());
}

template<class T> void operator|= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, BitOr<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void operator^= (Array<T> &left, const Array<T> &other)
{
    checkArrayShapes (left, other, "^=");
    arrayTransformInPlace (left, other, BitXor<T>());
}

template<class T> void operator^= (Array<T> &left, const T &other)
{
    arrayTransformInPlace (left, other, BitXor<T>());
}

template<class T> Array<T> operator+(const Array<T> &a)
{
    return a.copy();
}

template<class T> Array<T> operator-(const Array<T> &a)
{
    return arrayTransformResult (a, std::negate<T>());
}

template<class T> Array<T> operator~(const Array<T> &a)
{
    return arrayTransformResult (a, BitNegate<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator+(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "+");
    return arrayTransformResult (left, right, std::plus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator-(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "-");
    return arrayTransformResult (left, right, std::minus<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator*(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "*");
    return arrayTransformResult (left, right, std::multiplies<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T>
   Array<T> operator/(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "/");
    return arrayTransformResult (left, right, std::divides<T>());
}

template<class T>
   Array<T> operator%(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, Modulo<T>());
}

template<class T>
   Array<T> operator&(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, BitAnd<T>());
}

template<class T>
   Array<T> operator|(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, BitOr<T>());
}

template<class T>
   Array<T> operator^(const Array<T> &left, const Array<T> &right)
{
    checkArrayShapes (left, right, "%");
    return arrayTransformResult (left, right, BitXor<T>());
}

template<class T> 
Array<T> operator+ (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, std::plus<T>());
}

template<class T> 
Array<T> operator- (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, std::minus<T>());
}

template<class T>
Array<T> operator* (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, std::multiplies<T>());
}

template<class T> 
Array<T> operator/ (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, std::divides<T>());
}

template<class T> 
Array<T> operator% (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, Modulo<T>());
}

template<class T> 
Array<T> operator& (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, BitAnd<T>());
}

template<class T> 
Array<T> operator| (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, BitOr<T>());
}

template<class T> 
Array<T> operator^ (const Array<T> &left, const T &right)
{
    return arrayTransformResult (left, right, BitXor<T>());
}

template<class T> 
Array<T> operator+ (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, std::plus<T>());
}

template<class T> 
Array<T> operator- (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, std::minus<T>());
}

template<class T> 
Array<T> operator* (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, std::multiplies<T>());
}

template<class T> 
Array<T> operator/ (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, std::divides<T>());
}

template<class T> 
Array<T> operator% (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, Modulo<T>());
}

template<class T> 
Array<T> operator& (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, BitAnd<T>());
}

template<class T> 
Array<T> operator| (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, BitOr<T>());
}

template<class T> 
Array<T> operator^ (const T &left, const Array<T> &right)
{
    return arrayTransformResult (left, right, BitXor<T>());
}


// Mixed-type *=, /=, *, & / operators:
// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T>
void operator*= (Array<std::complex<T> > &left,
                 const Array<T> &other)
{
  checkArrayShapes (left, other, "*=");
  arrayTransformInPlace (left, other,
                         casacore::Multiplies<std::complex<T>,T>());
}

template<typename T>
void operator*= (Array<std::complex<T> > &left,
                 const T &other)
{
  arrayTransformInPlace (left, other,
                         casacore::Multiplies<std::complex<T>,T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T>
void operator/= (Array<std::complex<T> > &left,
                 const Array<T> &other)
{
  checkArrayShapes (left, other, "/=");
  arrayTransformInPlace (left, other,
                         casacore::Divides<std::complex<T>,T>());
}

template<typename T>
void operator/= (Array<std::complex<T> > &left,
                 const T &other)
{
  arrayTransformInPlace (left, other,
                         casacore::Divides<std::complex<T>,T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T>
Array<std::complex<T> > operator*(const Array<std::complex<T> > &left,
                                  const Array<T> &other)
{
  checkArrayShapes (left, other, "*");
  Array<std::complex<T> > result(left.shape());
  arrayContTransform (left, other, result,
                      casacore::Multiplies<std::complex<T>,T>());
  return result;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<typename T>
Array<std::complex<T> > operator/(const Array<std::complex<T> > &left,
                                  const Array<T> &other)
{
  checkArrayShapes (left, other, "/");
  Array<std::complex<T> > result(left.shape());
  arrayContTransform (left, other, result,
                      casacore::Divides<std::complex<T>,T>());
  return result;
}

template<typename T>
Array<std::complex<T> > operator* (const Array<std::complex<T> > &left,
                                   const T &other)
{
  Array<std::complex<T> > result(left.shape());
  arrayContTransform (left, other, result,
                      casacore::Multiplies<std::complex<T>,T>());
  return result;
}

template<typename T>
Array<std::complex<T> > operator/ (const Array<std::complex<T> > &left,
                                   const T &other)
{
  Array<std::complex<T> > result(left.shape());
  arrayContTransform (left, other, result,
                      casacore::Divides<std::complex<T>,T>());
  return result;
}

template<typename T>
Array<std::complex<T> > operator*(const std::complex<T> &left,
                                  const Array<T> &other)
{
  Array<std::complex<T> > result(other.shape());
  arrayContTransform (left, other, result,
                      casacore::Multiplies<std::complex<T>,T>());
  return result;
}

template<typename T>
Array<std::complex<T> > operator/(const std::complex<T> &left,
                                  const Array<T> &other)
{
  Array<std::complex<T> > result(other.shape());
  arrayContTransform (left, other, result,
                      casacore::Divides<std::complex<T>,T>());
  return result;
}


// <thrown>
//   </item> ArrayError
// </thrown>
template<class T> void minMax(T &minVal, T &maxVal, const Array<T> &array)
{
  if (array.nelements() == 0) {
    throw(ArrayError("void minMax(T &min, T &max, const Array<T> &array) - "
                     "Array has no elements"));	
  }
  T minv = array.data()[0];
  T maxv = minv;
  if (array.contiguousStorage()) {
    typename Array<T>::const_contiter iterEnd = array.cend();
    for (typename Array<T>::const_contiter iter = array.cbegin();
         iter!=iterEnd; ++iter) {
      if (*iter < minv) {
        minv = *iter;
      } else if (*iter > maxv) {
        maxv = *iter;
      }
    }
  } else {
    typename Array<T>::const_iterator iterEnd = array.end();
    for (typename Array<T>::const_iterator iter = array.begin();
         iter!=iterEnd; ++iter) {
      if (*iter < minv) {
        minv = *iter;
      } else if (*iter > maxv) {
        maxv = *iter;
      }
    }
  }
  maxVal = maxv;
  minVal = minv;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b)
{
    checkArrayShapes (a, b, "max");
    checkArrayShapes (a, result, "max");
    arrayTransform (a, b, result, casacore::Max<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b)
{
    checkArrayShapes (a, b, "min");
    checkArrayShapes (a, result, "min");
    arrayTransform (a, b, result, casacore::Min<T>());
}

template<class T> Array<T> max(const Array<T> &a, const Array<T> &b)
{
    Array<T> result(a.shape());
    max(result, a, b);
    return result;
}

template<class T> Array<T> min(const Array<T> &a, const Array<T> &b)
{
    Array<T> result(a.shape());
    min(result, a, b);
    return result;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const T &b)
{
    checkArrayShapes (a, result, "max");
    arrayTransform (a, b, result, casacore::Max<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const T &b)
{
    checkArrayShapes (a, result, "min");
    arrayTransform (a, b, result, casacore::Min<T>());
}

template<class T> Array<T> max(const Array<T> &a, const T &b)
{
    Array<T> result(a.shape());
    max(result, a, b);
    return result;
}

template<class T> Array<T> min(const Array<T> &a, const T &b)
{
    Array<T> result(a.shape());
    min(result, a, b);
    return result;
}

template<class T>
void indgen(Array<T> &a, T start, T inc)
{
  if (a.contiguousStorage()) {
    typename Array<T>::contiter aend = a.cend();
    for (typename Array<T>::contiter iter=a.cbegin(); iter!=aend; ++iter) {
      *iter = start;
      start += inc;
    }
  } else {
    typename Array<T>::iterator aend = a.end();
    for (typename Array<T>::iterator iter=a.begin(); iter!=aend; ++iter) {
      *iter = start;
      start += inc;
    }
  }
}

template<class T> Array<T> cos(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Cos<T>());
}

template<class T> Array<T> cosh(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Cosh<T>());
}

template<class T> Array<T> exp(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Exp<T>());
}

template<class T> Array<T> log(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Log<T>());
}

template<class T> Array<T> log10(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Log10<T>());
}

template<class T> Array<T> sin(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Sin<T>());
}

template<class T> Array<T> sinh(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Sinh<T>());
}

template<class T> Array<T> sqrt(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Sqrt<T>());
}

template<class T> Array<T> square(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Sqr<T>());
}

template<class T> Array<T> cube(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Pow3<T>());
}

template<class T> Array<T> acos(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Acos<T>());
}

template<class T> Array<T> asin(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Asin<T>());
}

template<class T> Array<T> atan(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Atan<T>());
}

template<class T> Array<T> ceil(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Ceil<T>());
}

template<class T> Array<T> fabs(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Abs<T>());
}

template<class T> Array<T> abs(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Abs<T>());
}

template<class T> Array<T> floor(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Floor<T>());
}

template<class T> Array<T> round(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Round<T>());
}

template<class T> Array<T> sign(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Sign<T>());
}

template<class T> Array<T> tan(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Tan<T>());
}

template<class T> Array<T> tanh(const Array<T> &a)
{
    return arrayTransformResult (a, casacore::Tanh<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> pow(const Array<T> &a, const Array<T> &b)
{
    checkArrayShapes (a, b, "pow");
    return arrayTransformResult (a, b, casacore::Pow<T>());
}

template<class T> Array<T> pow(const T &a, const Array<T> &b)
{
    return arrayTransformResult (a, b, casacore::Pow<T>());
}

template<class T> Array<T> pow(const Array<T> &a, const Double &b)
{
    Array<T> result(a.shape());
    arrayContTransform (a, b, result, casacore::Pow<T,Double>());
    return result;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> atan2(const Array<T> &a, const Array<T> &b)
{
    checkArrayShapes (a, b, "atan2");
    return arrayTransformResult (a, b, casacore::Atan2<T>());
}

template<class T> Array<T> atan2(const T &a, const Array<T> &b)
{
    return arrayTransformResult (a, b, casacore::Atan2<T>());
}

template<class T> Array<T> atan2(const Array<T> &a, const T &b)
{
    return arrayTransformResult (a, b, casacore::Atan2<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> fmod(const Array<T> &a, const Array<T> &b)
{
    checkArrayShapes (a, b, "fmod");
    return arrayTransformResult (a, b, casacore::Fmod<T>());
}

template<class T> Array<T> fmod(const T &a, const Array<T> &b)
{
    return arrayTransformResult (a, b, casacore::Fmod<T>());
}

template<class T> Array<T> fmod(const Array<T> &a, const T &b)
{
    return arrayTransformResult (a, b, casacore::Fmod<T>());
}


// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T sum(const Array<T> &a)
{
  return a.contiguousStorage() ?
    std::accumulate(a.cbegin(), a.cend(), T(), std::plus<T>()) :
    std::accumulate(a.begin(),  a.end(),  T(), std::plus<T>());
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T product(const Array<T> &a)
{
  if (a.empty()) {
    return T();
  }
  // Get first element, because T(1) may not work for all types.
  T prod = *a.data();
  if (a.contiguousStorage()) {
    typename Array<T>::const_contiter iter(a.cbegin());
    ++iter;
    return std::accumulate(iter, a.cend(), prod, std::multiplies<T>());
  } else {
    typename Array<T>::const_iterator iter(a.begin());
    ++iter;
    return std::accumulate(iter, a.end(),  prod, std::multiplies<T>());
  }
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T mean(const Array<T> &a)
{
    if (a.empty()) {
	throw(ArrayError("::mean(const Array<T> &) - 0 element array"));
    }
    return T(sum(a)/(1.0*a.nelements()));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T variance(const Array<T> &a, T mean)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::variance(const Array<T> &,T) - Need at least 2 "
			 "elements"));
    }
    T sum = a.contiguousStorage() ?
      std::accumulate(a.cbegin(), a.cend(), T(), casacore::SumSqrDiff<T>(mean)) :
      std::accumulate(a.begin(),  a.end(),  T(), casacore::SumSqrDiff<T>(mean));
    return T(sum/(1.0*a.nelements() - 1));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T variance(const Array<T> &a)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::variance(const Array<T> &) - Need at least 2 "
			 "elements"));
    }
    return variance(a, mean(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T stddev(const Array<T> &a)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::stddev(const Array<T> &) - Need at least 2 "
			 "elements"));
    }
    return sqrt(variance(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T stddev(const Array<T> &a, T mean)
{
    if (a.nelements() < 2) {
	throw(ArrayError("::stddev(const Array<T> &,T) - Need at least 2 "
			 "elements"));
    }
    return sqrt(variance(a, mean));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T avdev(const Array<T> &a)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::avdev(const Array<T> &,) - Need at least 1 "
			 "element"));
    }
    return avdev(a, mean(a));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T avdev(const Array<T> &a, T mean)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::avdev(const Array<T> &,T) - Need at least 1 "
			 "element"));
    }
    T sum = a.contiguousStorage() ?
      std::accumulate(a.cbegin(), a.cend(), T(), casacore::SumAbsDiff<T>(mean)) :
      std::accumulate(a.begin(),  a.end(),  T(), casacore::SumAbsDiff<T>(mean));
    return T(sum/(1.0*a.nelements()));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T rms(const Array<T> &a)
{
    if (a.nelements() < 1) {
	throw(ArrayError("::rms(const Array<T> &) - Need at least 1 "
			 "element"));
    }
    T sum = a.contiguousStorage() ?
      std::accumulate(a.cbegin(), a.cend(), T(), casacore::SumSqr<T>()) :
      std::accumulate(a.begin(),  a.end(),  T(), casacore::SumSqr<T>());
    return T(sqrt(sum/(1.0*a.nelements())));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T median(const Array<T> &a, Block<T> &tmp, Bool sorted,
			   Bool takeEvenMean, Bool inPlace)
{
    T medval=T();
    size_t nelem = a.nelements();
    if (nelem < 1) {
	throw(ArrayError("::median(T*) - array needs at least 1 element"));
    }
    //# Mean does not have to be taken for odd number of elements.
    if (nelem%2 != 0) {
	takeEvenMean = False;
    }
    // A copy is needed if not contiguous or if not in place.
    const T* storage = a.data();
    if (!(a.contiguousStorage() && inPlace)) {
      tmp.resize (a.size(), False, False);
      storage = tmp.storage();
      if (a.contiguousStorage()) {
	objcopy (tmp.storage(), a.data(), a.size());
      } else {
        // A non-contiguous array, so do the assignment through an array.
	Array<T> tmpa(a.shape(), tmp.storage(), SHARE);
	tmpa = a;
      }
    }
    T* data = const_cast<T*>(storage);
    size_t n2 = (nelem - 1)/2;
    if (!sorted) {
	// If needed take the mean for an even number of elements.
	// If the array is small, it is faster to fully sort it.
	if (nelem > 20) {
	    medval = GenSort<T>::kthLargest (data, nelem, n2);
	    if (takeEvenMean) {
		medval = T(0.5 * (medval +
				  GenSort<T>::kthLargest (data, nelem, n2+1)));
	    }
	} else {
	    GenSort<T>::sort (data, nelem);
	    sorted = True;
	}
    }
    if (sorted) {
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
template<class T> T madfm(const Array<T> &a, Block<T> &tmp, Bool sorted,
                          Bool takeEvenMean, Bool inPlace)
{
    T med = median(a, tmp, sorted, takeEvenMean, inPlace);
    Array<T> atmp;
    if (inPlace  &&  a.contiguousStorage()) {
      atmp.reference (a);   // remove constness
    } else {
      // A copy of a has been made to tmp.
      // Using it saves making another copy.
      AlwaysAssert (a.size() == tmp.size(), AipsError);
      atmp.reference (Array<T>(a.shape(), tmp.storage(), SHARE));
    }
    T* aptr = atmp.data();
    for (size_t i=0; i<atmp.size(); ++i) {
      aptr[i] = std::abs(aptr[i] - med);
    }
    return median(atmp, tmp, False, takeEvenMean, True);
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T fractile(const Array<T> &a, Block<T>& tmp, Float fraction,
			     Bool sorted, Bool inPlace)
{
    if (fraction < 0  ||  fraction > 1) {
        throw(ArrayError("::fractile(const Array<T>&) - fraction <0 or >1 "));
    }    
    size_t nelem = a.nelements();
    if (nelem < 1) {
	throw(ArrayError("::fractile(const Array<T>&) - Need at least 1 "
			 "elements"));
    }
    T fracval = T();
    // A copy is needed if not contiguous or if not in place.
    const T* storage = a.data();
    if (!(a.contiguousStorage() && inPlace)) {
      tmp.resize (a.size(), False, False);
      if (a.contiguousStorage()) {
	objcopy (tmp.storage(), a.data(), a.size());
      } else {
      // A non-contiguous array, so do the assignment through an array.
	Array<T> tmpa(a.shape(), tmp.storage(), SHARE);
	tmpa = a;
	storage = tmp.storage();
      }
    }
    T* data = const_cast<T*>(storage);
    size_t n2 = uInt((nelem - 1) * Double(fraction) + 0.01);
    if (!sorted) {
	// If the array is small, it is faster to fully sort it.
	if (nelem > 20) {
	    fracval = GenSort<T>::kthLargest (data, nelem, n2);
	} else {
	    GenSort<T>::sort (data, nelem);
	    sorted = True;
	}
    }
    if (sorted) {
        fracval = data[n2];
    }
    return fracval;
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T interFractileRange(const Array<T> &a, Block<T> &tmp,
                                       Float fraction,
                                       Bool sorted, Bool inPlace)
{
  AlwaysAssert (fraction>0  &&  fraction<0.5, AipsError);
  T hex1, hex2;
  hex1 = fractile(a, tmp, fraction, sorted, inPlace);
  if (inPlace  &&  a.contiguousStorage()) {  
    hex2 = fractile(a, tmp, 1-fraction, sorted, inPlace);
  } else {
    // In this case a copy of a has been made to tmp.
    // Using it saves making another copy.
    AlwaysAssert (a.size() == tmp.size(), AipsError);
    Array<T> atmp(a.shape(), tmp.storage(), SHARE);
    hex2 = fractile(atmp, tmp, 1-fraction, sorted, inPlace);
  }
  return (hex2 - hex1);
}


template<typename T>
Array<std::complex<T> > makeComplex(const Array<T> &left, const Array<T>& right)
{
  checkArrayShapes (left, right, "makeComplex");
  Array<std::complex<T> > res(left.shape());
  arrayContTransform (left, right, res,
                      casacore::MakeComplex<T,T,std::complex<T> >());
  return res;
}

template<typename C, typename R>
void setReal(Array<C> &carray, const Array<R> &rarray)
{
  checkArrayShapes (carray, rarray, "setReal");
  // Cannot be done in place, because imag is taken from second operand.
  arrayTransform (rarray, carray, carray, casacore::MakeComplexImag<R,C,C>());
}

template<typename C, typename R>
void setImag(Array<C> &carray, const Array<R> &rarray)
{
  checkArrayShapes (carray, rarray, "setImag");
  arrayTransformInPlace (carray, rarray, casacore::MakeComplexReal<C,R,C>());
}


template<class T, class U> void convertArray(Array<T> &to,
					     const Array<U> &from)
{
    if (to.nelements() == 0 && from.nelements() == 0) {
	return;
    }
    if (to.shape() != from.shape()) {
	throw(ArrayConformanceError("void ::convertArray(Array<T> &to, "
				    "const Array<U> &from)"
				    " - arrays do not conform"));
    }
    if (to.contiguousStorage()  &&  from.contiguousStorage()) {
      typename Array<U>::const_contiter endFrom = from.cend();
      typename Array<U>::const_contiter iterFrom = from.cbegin();
      for (typename Array<T>::contiter iterTo = to.cbegin();
	   iterFrom != endFrom;
	   ++iterFrom, ++iterTo) {
	convertScalar (*iterTo, *iterFrom);
      }
    } else {
      typename Array<U>::const_iterator endFrom = from.end();
      typename Array<U>::const_iterator iterFrom = from.begin();
      for (typename Array<T>::iterator iterTo = to.begin();
	   iterFrom != endFrom;
	   ++iterFrom, ++iterTo) {
	convertScalar (*iterTo, *iterFrom);
      }
    }
}

} //# NAMESPACE CASACORE - END

#endif
