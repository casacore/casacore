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

#include <casa/iostream.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayIter.h>
#include <casa/Arrays/VectorIter.h>
#include <casa/Arrays/ArrayError.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicMath/ConvertScalar.h>
#include <casa/Utilities/GenSort.h>
#include <algorithm>

namespace casa { //# NAMESPACE CASA - BEGIN


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
      std::transform (left.cbegin(), left.cend(),
                      result.begin(), bind2nd(op, right));
    } else {
      std::transform (left.begin(), left.end(),
                      result.begin(), bind2nd(op, right));
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
      std::transform (right.cbegin(), right.cend(),
                      result.begin(), bind1st(op, left));
    } else {
      std::transform (right.begin(), right.end(),
                      result.begin(), bind1st(op, left));
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
template<class ScalarType> 
void minMax(ScalarType &minVal, ScalarType &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<ScalarType> &array) 
{
    if (array.nelements() == 0) {
        throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
			 "IPosition &maxPos, const Array<T> &array) - "
                         "Array has no elements"));	
    }
    minPos.resize (array.ndim());
    maxPos.resize (array.ndim());

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ScalarType val;

    // Initialize
    minPos = 0;
    maxPos = 0;
    minVal = maxVal = array(minPos);
    size_t n = ai.vector().nelements();

    while (! ai.pastEnd()) {
	for (size_t i=0; i<n; i++) {
	    val = ai.vector()(i);
	    if (val < minVal) {
	        minVal = val;
		minPos = ai.pos();
		minPos(0) += i;
	    }
	    if (val > maxVal) {
	        maxVal = val;
		maxPos = ai.pos();
		maxPos(0) += i;
	    }
	}
	ai.next(); 
    }
}



// <thrown>
//   <item> ArrayError
// </thrown>
template<class ScalarType> 
void minMaxMasked(ScalarType &minVal, ScalarType &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<ScalarType> &array, const Array<ScalarType> &mask) 
{
    if (array.nelements() == 0) {
      throw(ArrayError("void minMaxMasked(T &min, T &max, IPosition &minPos,"
		       "IPosition &maxPos, const Array<T> &array) - "
		       "Array has no elements"));	
    }
    if (array.shape() != mask.shape()) {
      throw(ArrayConformanceError("void minMaxMasked(T &min, T &max,"
				  "IPosition &minPos, IPosition &maxPos, const Array<T> &array, "
				  "const Array<T> &mask) - " 
				  "array and mask do not have the same shape()"));
    } 
    minPos.resize (array.ndim());
    maxPos.resize (array.ndim());

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ReadOnlyVectorIterator<ScalarType> mi(mask);
    ScalarType val;

    // Initialize
    minPos = 0;
    maxPos = 0;
    minVal = maxVal = array(minPos) * mask(minPos);
    size_t n = ai.vector().nelements();

    while (! ai.pastEnd()) {
	for (size_t i=0; i<n; i++) {
	    val = (ai.vector()(i)) * (mi.vector()(i));
	    if (val < minVal) {
	        minVal = val;
		minPos = ai.pos();
		minPos(0) += i;
	    }
	    if (val > maxVal) {
	        maxVal = val;
		maxPos = ai.pos();
		maxPos(0) += i;
	    }
	}
	ai.next(); 
    }
}


// <thrown>
//   <item> ArrayError
//   <item> AipsError
// </thrown>
template<class ScalarType> 
void minMax(ScalarType &minVal, ScalarType &maxVal, 
	    IPosition &minPos, IPosition &maxPos,
	    const Array<ScalarType> &array, const Array<Bool> &mask)
{
    if (array.nelements() == 0) {
      throw(ArrayError("void minMax(T &min, T &max, IPosition &minPos,"
		       "IPosition &maxPos, const Array<T> &array, "
		       "const Array<Bool> &mask) - "
		       "Array has no elements"));	
    }
    if (array.shape() != mask.shape()) {
      throw(ArrayConformanceError("void minMax(T &min, T &max,"
	    "IPosition &minPos, IPosition &maxPos, const Array<T> &array, "
	    "const Array<Bool> &mask) - " 
	    "array and mask do not have the same shape()"));
    } 
    minPos.resize (array.ndim());
    maxPos.resize (array.ndim());

    ReadOnlyVectorIterator<ScalarType> ai(array);
    ReadOnlyVectorIterator<Bool> mi(mask);

    // Initialize
    // have to find a valid value in array to init min and max
    Bool found=False;
    size_t ifound=0;
    size_t n = mi.vector().nelements();
    while (! mi.pastEnd() && !found) {
	for (size_t i=0; i<n; i++) {
	    if (mi.vector()(i)) {
		maxPos = ai.pos();
		maxPos(0) += i;
		found =  True;
                ifound = i;
		break;
	    }
	}
        if (!found) {
	    mi.next();
	    ai.next();
        }
    }
    if (!found) {
      throw(AipsError("void minMax(T &min, T &max,"
	    "IPosition &minPos, IPosition &maxPos, const Array<T> &array, "
	    "const Array<Bool> &mask) - mask==False, no valid array elements"));
    }
    minPos = maxPos;
    minVal = maxVal = array(minPos);

    ScalarType val; 

    // Finish with vector where first value was found.
    {
        for (size_t i=++ifound; i<n; i++) {
            if (mi.vector()(i)) {
                val = ai.vector()(i);
                if (val < minVal) {
                    minVal = val;
                    minPos = ai.pos();
                    minPos(0) += i;
                }
                if (val > maxVal) {
                    maxVal = val;
                    maxPos = ai.pos();
                    maxPos(0) += i;
                }
            }
        }
        ai.next(); mi.next();
    }

    // Continue with the rest of the array.
    while (! ai.pastEnd()) {
        for (size_t i=0; i<n; i++) {
            if (mi.vector()(i)) {
                val = ai.vector()(i);
                if (val < minVal) {
                    minVal = val;
                    minPos = ai.pos();
                    minPos(0) += i;
                }
                if (val > maxVal) {
                    maxVal = val;
                    maxPos = ai.pos();
                    maxPos(0) += i;
                }
            }
        }
	ai.next(); mi.next();
    }
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

template<class T>  void indgen(Array<T> &a) {indgen(a, T(0), T(1));}

template<class T>  void indgen(Array<T> &a, T start) 
{indgen(a, start, T(1));}

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

template<class T> Array<T> operator+(const Array<T> &a)
{
    return a.copy();
}

template<class T> Array<T> operator-(const Array<T> &a)
{
    return arrayTransformResult (a, std::negate<T>());
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


// <thrown>
//   </item> ArrayError
// </thrown>
template<class T> void minMax(T &min, T &max, const Array<T> &a)
{
    size_t ntotal = a.nelements();
    if (ntotal == 0) {
	throw(ArrayError("void minMax(T &min, T &max, const Array<T> &a) - "
			 "Array has no elements"));
    }

    Bool deleteIt;
    const T *storage = a.getStorage(deleteIt);

    min = *storage;
    max = *storage;

    // Account for the fact we've seen the first position
    ntotal--;
    const T *ts = storage + 1;

    while (ntotal--) {
	min = (min < (*ts)) ? min : (*ts);
	max = (max > (*ts)) ? max : (*ts);
	ts++;
    }

    a.freeStorage(storage, deleteIt);
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b)
{
    checkArrayShapes (a, b, "max");
    checkArrayShapes (a, result, "max");
    arrayTransform (a, b, result, casa::Max<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const Array<T> &b)
{
    checkArrayShapes (a, b, "min");
    checkArrayShapes (a, result, "min");
    arrayTransform (a, b, result, casa::Min<T>());
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
    arrayTransform (a, b, result, casa::Max<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const T &b)
{
    checkArrayShapes (a, result, "min");
    arrayTransform (a, b, result, casa::Min<T>());
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
    return arrayTransformResult (a, casa::Cos<T>());
}

template<class T> Array<T> cosh(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Cosh<T>());
}

template<class T> Array<T> exp(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Exp<T>());
}

template<class T> Array<T> log(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Log<T>());
}

template<class T> Array<T> log10(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Log10<T>());
}

template<class T> Array<T> sin(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Sin<T>());
}

template<class T> Array<T> sinh(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Sinh<T>());
}

template<class T> Array<T> sqrt(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Sqrt<T>());
}

template<class T> Array<T> acos(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Acos<T>());
}

template<class T> Array<T> asin(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Asin<T>());
}

template<class T> Array<T> atan(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Atan<T>());
}

template<class T> Array<T> ceil(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Ceil<T>());
}

template<class T> Array<T> fabs(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Abs<T>());
}

template<class T> Array<T> abs(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Abs<T>());
}

template<class T> Array<T> floor(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Floor<T>());
}

template<class T> Array<T> tan(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Tan<T>());
}

template<class T> Array<T> tanh(const Array<T> &a)
{
    return arrayTransformResult (a, casa::Tanh<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> pow(const Array<T> &a, const Array<T> &b)
{
    checkArrayShapes (a, b, "pow");
    return arrayTransformResult (a, b, casa::Pow<T>());
}

template<class T> Array<T> pow(const T &a, const Array<T> &b)
{
    return arrayTransformResult (a, b, casa::Pow<T>());
}

template<class T> Array<T> pow(const Array<T> &a, const Double &b)
{
    Array<T> result(a.shape());
    arrayContTransform (a, b, result, casa::Pow<T,Double>());
    return result;
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> atan2(const Array<T> &a, const Array<T> &b)
{
    checkArrayShapes (a, b, "atan2");
    return arrayTransformResult (a, b, casa::Atan2<T>());
}

// <thrown>
//   </item> ArrayConformanceError
// </thrown>
template<class T> Array<T> fmod(const Array<T> &a, const Array<T> &b)
{
    checkArrayShapes (a, b, "fmod");
    return arrayTransformResult (a, b, casa::Fmod<T>());
}

template<class T> Array<T> fmod(const T &a, const Array<T> &b)
{
    return arrayTransformResult (a, b, casa::Fmod<T>());
}

template<class T> Array<T> fmod(const Array<T> &a, const T &b)
{
    return arrayTransformResult (a, b, casa::Fmod<T>());
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
      std::accumulate(a.cbegin(), a.cend(), T(), casa::SumSqrDiff<T>(mean)) :
      std::accumulate(a.begin(),  a.end(),  T(), casa::SumSqrDiff<T>(mean));
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
      std::accumulate(a.cbegin(), a.cend(), T(), casa::AbsDiff<T>(mean)) :
      std::accumulate(a.begin(),  a.end(),  T(), casa::AbsDiff<T>(mean));
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
      std::accumulate(a.cbegin(), a.cend(), T(), casa::SumSqr<T>()) :
      std::accumulate(a.begin(),  a.end(),  T(), casa::SumSqr<T>());
    return T(sqrt(sum/(1.0*a.nelements())));
}

// <thrown>
//    </item> ArrayError
// </thrown>
template<class T> T median(const Array<T> &a, Block<T> &tmp, Bool sorted,
			   Bool takeEvenMean, Bool inPlace)
{
    T medval;
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
    size_t n2 = size_t((nelem - 1) * Double(fraction));
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

} //# NAMESPACE CASA - END
