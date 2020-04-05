//# MaskArrMath.h: Simple mathematics done with MaskedArray's.
//# Copyright (C) 1993,1994,1995,1996,1999,2001
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

#ifndef CASA_MASKARRMATH_2_H
#define CASA_MASKARRMATH_2_H

#include "Array.h"
#include "MaskedArray.h"
#include "IPosition.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Mathematical operations for MaskedArrays (and with Arrays) </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMaskArrMath0 tMaskArrMath1 tMaskArrMath2 tMaskArrExcp">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto class=MaskedArray>MaskedArray</linkto>
// </prerequisite>
//
// <etymology>
// MaskArrMath is short for MaskedArrayMath, which is too long by the old
// AIPS++ file naming conventions.  This file contains global functions
// which perform element by element mathematical operations on masked arrays.
// </etymology>
//
// <synopsis>
// These functions perform element by element mathematical operations on
// masked arrays.  With two arrays, they must both conform, and the result
// is done element by element, for those locations where the mask of the
// MaskedArray is true.  For two MaskedArrays, the "and" of the masks is used.
// </synopsis>
//
// <example>
// <srcblock>
//   Vector<int> a(10);
//   Vector<int> b(10);
//   Vector<int> c(10);
//      . . .
//   c = a(a>0) + b(b>0);
// </srcblock>
// This example sets those elements of c where ((a>0) && (b>0)) to (a+b).
// Elements of c where !((a>0) && (b>0)) are unchanged.  The result of
// this operation is a MaskedArray.  The assignment from this
// MaskedArray to the Vector c only assigns those elements
// where the mask is true.
// </example>
//
// <example>
// <srcblock>
//   Vector<double> a(10);
//   Vector<double> b(10);
//   Vector<double> c(10);
//      . . .
//   c = atan2 (a, b(b>0);
// </srcblock>
// This example sets those elements of c where (b>0) to atan2 (a,b).
// Elements of c where !(b>0) are unchanged.  The result of
// this operation is a MaskedArray.  The assignment from this
// MaskedArray to the Vector c only assigns those elements
// where the mask is true.
// </example>
//
// <example>
// <srcblock>
//   Vector<int> a(10);
//   int result;
//      . . .
//   result = sum (a(a>0));
// </srcblock>
// This example sums a, for those elements of a which are greater than 0.
// </example>
//
// <motivation>
// One wants to be able to mask arrays and perform mathematical operations on
// those masked arrays.  Since the masked arrays are only defined where
// the masks are true, the result must be a MaskedArray, or a simple number.
// </motivation>
//
// <linkfrom anchor="MaskedArray mathematical operations" classes="MaskedArray Array Vector Matrix Cube">
//    <here>MaskedArray mathematical operations</here> -- Mathematical
//    operations for MaskedArrays, and between MaskedArrays and Arrays.
// </linkfrom>
//
// <group name="MaskedArray mathematical operations">

// Element by element arithmetic modifying left in-place. left and other
// must be conformant.
// 
// <thrown>
//   <li> ArrayConformanceError
// </thrown>
//
// <group>
template<class T> const MaskedArray<T> & operator+= (const MaskedArray<T> &left, const Array<T> &other);
template<class T> const MaskedArray<T> & operator-= (const MaskedArray<T> &left, const Array<T> &other);
template<class T> const MaskedArray<T> & operator*= (const MaskedArray<T> &left, const Array<T> &other);
template<class T> const MaskedArray<T> & operator/= (const MaskedArray<T> &left, const Array<T> &other);
template<class T> Array<T> & operator+= (Array<T> &left, const MaskedArray<T> &other);
template<class T> Array<T> & operator-= (Array<T> &left, const MaskedArray<T> &other);
template<class T> Array<T> & operator*= (Array<T> &left, const MaskedArray<T> &other);
template<class T> Array<T> & operator/= (Array<T> &left, const MaskedArray<T> &other);
template<class T> const MaskedArray<T> & operator+= (const MaskedArray<T> &left, const MaskedArray<T> &other);
template<class T> const MaskedArray<T> & operator-= (const MaskedArray<T> &left, const MaskedArray<T> &other);
template<class T> const MaskedArray<T> & operator*= (const MaskedArray<T> &left, const MaskedArray<T> &other);
template<class T> const MaskedArray<T> & operator/= (const MaskedArray<T> &left,const MaskedArray<T> &other);
template<class T,class S> const MaskedArray<T> & operator/= (const MaskedArray<T> &left,const MaskedArray<S> &other);
// </group>

// 
// Element by element arithmetic modifying left in-place. The scalar "other"
// behaves as if it were a conformant Array to left filled with constant values.
// <group>
template<class T> const MaskedArray<T> & operator+= (const MaskedArray<T> &left,const T &other);
template<class T> const MaskedArray<T> & operator-= (const MaskedArray<T> &left,const T &other);
template<class T> const MaskedArray<T> & operator*= (const MaskedArray<T> &left,const T &other);
template<class T> const MaskedArray<T> & operator/= (const MaskedArray<T> &left,const T &other);
// </group>

// Unary arithmetic operation.
// 
// <group>
template<class T> MaskedArray<T> operator+(const MaskedArray<T> &a);
template<class T> MaskedArray<T> operator-(const MaskedArray<T> &a);
// </group>

// 
// Element by element arithmetic on MaskedArrays, returns a MaskedArray.
//
// <thrown>
//   <li> ArrayConformanceError
// </thrown>
//
// <group>
template<class T> MaskedArray<T> operator+ (const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> operator- (const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> operator* (const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> operator/ (const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> operator+ (const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> operator- (const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> operator* (const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> operator/ (const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> operator+ (const MaskedArray<T> &left,const MaskedArray<T> &right);
template<class T> MaskedArray<T> operator- (const MaskedArray<T> &left,const MaskedArray<T> &right);
template<class T> MaskedArray<T> operator* (const MaskedArray<T> &left,const MaskedArray<T> &right);
template<class T> MaskedArray<T> operator/ (const MaskedArray<T> &left,const MaskedArray<T> &right);
// </group>

// 
// Element by element arithmetic between a MaskedArray and a scalar, returning
// a MaskedArray.
// <group>
template<class T> MaskedArray<T> operator+ (const MaskedArray<T> &left, const T &right);
template<class T> MaskedArray<T> operator- (const MaskedArray<T> &left, const T &right);
template<class T> MaskedArray<T> operator* (const MaskedArray<T> &left, const T &right);
template<class T> MaskedArray<T> operator/ (const MaskedArray<T> &left, const T &right);
                  MaskedArray<std::complex<float>> operator* (const MaskedArray<std::complex<float>> &left, const float &right);
// </group>

// 
// Element by element arithmetic between a scalar and a MaskedArray, returning
// a MaskedArray.
// <group>
template<class T>  MaskedArray<T> operator+ (const T &left, const MaskedArray<T> &right);
template<class T>  MaskedArray<T> operator- (const T &left, const MaskedArray<T> &right);
template<class T>  MaskedArray<T> operator* (const T &left, const MaskedArray<T> &right);
template<class T>  MaskedArray<T> operator/ (const T &left, const MaskedArray<T> &right);
                   MaskedArray<std::complex<float>> operator* (const float &left, const MaskedArray<std::complex<float>> &right);
// </group>

// 
// Transcendental function applied to the array on an element-by-element
// basis. Although a template function, this may not make sense for all
// numeric types.
// <group>
template<class T> MaskedArray<T> sin(const MaskedArray<T> &left);
template<class T> MaskedArray<T> cos(const MaskedArray<T> &left);
template<class T> MaskedArray<T> tan(const MaskedArray<T> &left);
template<class T> MaskedArray<T> asin(const MaskedArray<T> &left);
template<class T> MaskedArray<T> acos(const MaskedArray<T> &left);
template<class T> MaskedArray<T> atan(const MaskedArray<T> &left);
template<class T> MaskedArray<T> sinh(const MaskedArray<T> &left);
template<class T> MaskedArray<T> cosh(const MaskedArray<T> &left);
template<class T> MaskedArray<T> tanh(const MaskedArray<T> &left);
template<class T> MaskedArray<T> exp(const MaskedArray<T> &left);
template<class T> MaskedArray<T> log(const MaskedArray<T> &left);
template<class T> MaskedArray<T> log10(const MaskedArray<T> &left);
template<class T> MaskedArray<T> sqrt(const MaskedArray<T> &left);
template<class T> MaskedArray<T> abs(const MaskedArray<T> &left);
template<class T> MaskedArray<T> fabs(const MaskedArray<T> &left);
template<class T> MaskedArray<T> ceil(const MaskedArray<T> &left);
template<class T> MaskedArray<T> floor(const MaskedArray<T> &left);
// </group>

// Transcendental functions requiring two arguments applied on an element-by-element
// basis. Although a template function, this may not make sense for all
// numeric types.
// <thrown>
//   <li> ArrayConformanceError
// </thrown>
//
// <group>
template<class T> MaskedArray<T> atan2(const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> fmod(const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> atan2(const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> fmod(const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> atan2(const MaskedArray<T> &left,const MaskedArray<T> &right);
template<class T> MaskedArray<T> fmod(const MaskedArray<T> &left,const MaskedArray<T> &right);
template<class T> MaskedArray<T> atan2(const MaskedArray<T> &left, const T &right);
template<class T> MaskedArray<T> fmod(const MaskedArray<T> &left, const T &right);
template<class T> MaskedArray<T> atan2(const T &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> fmod(const T &left, const MaskedArray<T> &right);
template<class T, class U> MaskedArray<T> pow(const MaskedArray<T> &left, const Array<U> &right);
template<class T, class U> MaskedArray<T> pow(const Array<T> &left, const MaskedArray<U> &right);
template<class T, class U> MaskedArray<T> pow(const MaskedArray<T> &left,const MaskedArray<U> &right);
template<class T> MaskedArray<T> pow(const MaskedArray<T> &left, const double &right);
// </group>


// Extracts the real part of a complex array into an array of floats.
template<class T>
MaskedArray<T> real(const MaskedArray<std::complex<T>> &carray)
  { return MaskedArray<T> (real(carray.getArray()), carray.getMask()); }

// 
// Extracts the imaginary part of a complex array into an array of floats.
template<class T>
MaskedArray<T>  imag(const MaskedArray<std::complex<T>> &carray)
  { return MaskedArray<T> (imag(carray.getArray()), carray.getMask()); }


// 
// Find the minimum and maximum values of a MaskedArray.
// Also find the IPositions of the minimum and maximum values.
//
// <thrown>
//    <li> ArrayError
// </thrown>
//
// <group>
template<class T> void minMax(T &minVal, T &maxVal, IPosition &minPos, IPosition &maxPos,const MaskedArray<T> &marray);
template<class T> void minMax(T &minVal, T &maxVal,const MaskedArray<T> &marray);
// </group>


// 
// The "min" and "max" functions require that the type "T" have comparison 
// operators.
// The minimum element of the array.
template<class T> T min(const MaskedArray<T> &left);


// Return an array that contains the minimum of "left" and "right" at each
// position.
//
// "left" and "right" must be conformant.
//
// <thrown>
//    <li> ArrayError
// </thrown>
// <group>
template<class T> MaskedArray<T> min(const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> min(const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> min(const MaskedArray<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> min(const T &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> min(const MaskedArray<T> &left, const T &right);
// </group>


// "result" contains the minimum of "left" and "right" at each position.
// "result", "left", and "right" must be conformant.
//
// <thrown>
//   <li> ArrayConformanceError
// </thrown>
//
template<class T> void min(const MaskedArray<T> &result, const Array<T> &left, const Array<T> &right);


// The maximum element of the array.
template<class T> T max(const MaskedArray<T> &left);


// Return an array that contains the maximum of "left" and "right" at each
// position.
//
// "left" and "right" must be conformant.
// <thrown>
//    <li> ArrayError
// </thrown>
//
// <group>
template<class T> MaskedArray<T> max(const MaskedArray<T> &left, const Array<T> &right);
template<class T> MaskedArray<T> max(const Array<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> max(const MaskedArray<T> &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> max(const T &left, const MaskedArray<T> &right);
template<class T> MaskedArray<T> max(const MaskedArray<T> &left, const T &right);
// </group>


// "result" contains the maximum of "left" and "right" at each position.
// "result", "left", and "right" must be conformant.
//
// <thrown>
//   <li> ArrayConformanceError
// </thrown>
//
template<class T> void max(const MaskedArray<T> &result,const Array<T> &left, const Array<T> &right);

// 
// Fills all elements of "array" where the mask is true with a sequence
// starting with "start" and incrementing by "inc" for each element
// where the mask is true.
// The first axis varies most rapidly.
template<class T> void indgen(MaskedArray<T> &a, T start, T inc);

// 
// Fills all elements of "array" where the mask is true with a sequence
// starting with 0 and incremented by one for each element
// where the  mask is true.
// The first axis varies most rapidly.
template<class T>  void indgen(MaskedArray<T> &a);

// 
// Fills all elements of "array" where the mask is true with a sequence
// starting with "start" and incremented by one for each element
// where the  mask is true.
// The first axis varies most rapidly.
template<class T>  void indgen(MaskedArray<T> &a, T start);


// <thrown>
//    <li> ArrayError
// </thrown>
//
// Sum of every element of the MaskedArray where the Mask is true.
template<class T> T sum(const MaskedArray<T> &a);

// 
// Sum of the squares of every element of the MaskedArray where the Mask is true.
template<class T> T sumsquares(const MaskedArray<T> &a);

// 
// Product of every element of the MaskedArray where the Mask is true.
// This could of course easily overflow.
template<class T> T product(const MaskedArray<T> &a);

// 
// The mean of "a" is the sum of all elements of "a" divided by the number
// of elements of "a".
template<class T> T mean(const MaskedArray<T> &a);

// 
// The variance of "a" is the sum of (a(i) - mean(a))**2/(a.nelements() - ddof).
// Similar to numpy the argument ddof tells if the population variance (ddof=0)
// or the sample variance (ddof=1) is taken.
// The variance functions proper use ddof=1.
// <br>Note that for a complex valued T the absolute values are used; in that way
// the variance is equal to the sum of the variances of the real and imaginary parts.
// Hence the imaginary part in the return value is 0.
template<class T> T variance(const MaskedArray<T> &a);
template<class T> T pvariance(const MaskedArray<T> &a, size_t ddof=0);
// Rather than using a computed mean, use the supplied value.
template<class T> T variance(const MaskedArray<T> &a, T mean);
template<class T> T pvariance(const MaskedArray<T> &a, T mean, size_t ddof=0);

// The standard deviation of "a" is the square root of its variance.
template<class T> T stddev(const MaskedArray<T> &a);
template<class T> T pstddev(const MaskedArray<T> &a, size_t ddof=0);
template<class T> T stddev(const MaskedArray<T> &a, T mean);
template<class T> T pstddev(const MaskedArray<T> &a, T mean, size_t ddof=0);

// 
// The average deviation of "a" is the sum of abs(a(i) - mean(a))/N. (N.B.
// N, not N-1 in the denominator).
template<class T> T avdev(const MaskedArray<T> &a);

// 
// The average deviation of "a" is the sum of abs(a(i) - mean(a))/N. (N.B.
// N, not N-1 in the denominator).
// Rather than using a computed mean, use the supplied value.
template<class T> T avdev(const MaskedArray<T> &a,T mean);

// 
// The root-mean-square of "a" is the sqrt of sum(a*a)/N.
template<class T> T rms(const MaskedArray<T> &a);

// 
// The median of "a" is a(n/2).
// When a has an even number of elements and the switch takeEvenMean is set,
// the median is 0.5*(a(n/2) + a((n+1)/2)).
// According to Numerical Recipes (2nd edition) it makes little sense to take
// the mean when the array is large enough (> 100 elements). Therefore
// the default for takeEvenMean is false when the array has > 100 elements,
// otherwise it is true.
// <br>If "sorted"==true we assume the data is already sorted and we
// compute the median directly. Otherwise the function GenSort::kthLargest
// is used to find the median (kthLargest is about 6 times faster
// than a full quicksort).
// <group>
template<class T> inline T median(const MaskedArray<T> &a, bool sorted=false)
    { return median (a, sorted, (a.nelements() <= 100)); }
template<class T> T median(const MaskedArray<T> &a, bool sorted,
			   bool takeEvenMean);
// </group>

// The median absolute deviation from the median. Interface is as for
// the median functions
// <group>
template<class T> inline T madfm(const MaskedArray<T> &a, bool sorted=false)
    { return madfm (a, sorted, (a.nelements() <= 100)); }
template<class T> T madfm(const MaskedArray<T> &a, bool sorted,
                          bool takeEvenMean);
// </group>
 

// Returns a MaskedArray where every element is squared.
template<class T> MaskedArray<T> square(const MaskedArray<T> &val);

// Returns a MaskedArray where every element is cubed.
template<class T> MaskedArray<T> cube(const MaskedArray<T> &val);

// </group>


template<typename T> class MaskedSumFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return sum(arr); }
};
template<typename T> class MaskedProductFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return product(arr); }
};
template<typename T> class MaskedMinFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return min(arr); }
};
template<typename T> class MaskedMaxFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return max(arr); }
};
template<typename T> class MaskedMeanFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return mean(arr); }
};
template<typename T> class MaskedVarianceFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return variance(arr); }
};
template<typename T> class MaskedStddevFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return stddev(arr); }
};
template<typename T> class MaskedAvdevFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return avdev(arr); }
};
template<typename T> class MaskedRmsFunc {
public:
  T operator() (const MaskedArray<T>& arr) const { return rms(arr); }
};
template<typename T> class MaskedMedianFunc {
public:
  explicit MaskedMedianFunc (bool sorted=false, bool takeEvenMean=true)
    : itsSorted(sorted), itsTakeEvenMean(takeEvenMean) {}
  T operator() (const MaskedArray<T>& arr) const
    { return median(arr, itsSorted, itsTakeEvenMean); }
private:
  bool     itsSorted;
  bool     itsTakeEvenMean;
  bool     itsInPlace;
};
template<typename T> class MaskedMadfmFunc {
public:
  explicit MaskedMadfmFunc(bool sorted=false, bool takeEvenMean=true)
    : itsSorted(sorted), itsTakeEvenMean(takeEvenMean) {}
  float operator()(const MaskedArray<float>& arr) const
    { return madfm(arr, itsSorted, itsTakeEvenMean); }
private:
  bool     itsSorted;
  bool     itsTakeEvenMean;
  bool     itsInPlace;
};

// Apply the given ArrayMath reduction function objects
// to each box in the array.
// <example>
// Downsample an array by taking the mean of every [25,25] elements.
// <srcblock>
//    Array<float> downArr = boxedArrayMath(in, IPosition(2,25,25),
//                                          MaskedMeanFunc<float>());
// </srcblock>
// </example>
// The dimensionality of the array can be larger than the box; in that
// case the missing axes of the box are assumed to have length 1.
// A box axis length <= 0 means the full array axis.
template <typename T, typename FuncType>
MaskedArray<T> boxedArrayMath (const MaskedArray<T>& array,
			       const IPosition& boxSize,
			       const FuncType& funcObj);

// Apply for each element in the array the given ArrayMath reduction function
// object to the box around that element. The full box is 2*halfBoxSize + 1.
// It can be used for arrays and boxes of any dimensionality; missing
// halfBoxSize values are set to 1.
// <example>
// Determine for each element in the array the median of a box
// with size [51,51] around that element:
// <srcblock>
//    Array<float> medians = slidingArrayMath(in, IPosition(2,25,25),
//                                            MaskedMedianFunc<float>());
// </srcblock>
// This is a potentially expensive operation. On a high-end PC it took
// appr. 27 seconds to get the medians for an array of [1000,1000] using
// a halfBoxSize of [50,50].
// </example>
// <br>The fillEdge argument determines how the edge is filled where
// no full boxes can be made. true means it is set to zero; false means
// that the edge is removed, thus the output array is smaller than the
// input array.
// <note> This brute-force method of determining the medians outperforms
// all kinds of smart implementations. For a vector it is about as fast
// as class <linkto class=MedianSlider>MedianSlider</linkto>, for a 2D array
// it is much, much faster.
// </note>
template <typename T, typename FuncType>
Array<T> slidingArrayMath (const MaskedArray<T>& array,
			   const IPosition& halfBoxSize,
			   const FuncType& funcObj,
			   bool fillEdge=true);


} //# NAMESPACE CASACORE - END

#include "MaskArrMath.tcc"

#endif
