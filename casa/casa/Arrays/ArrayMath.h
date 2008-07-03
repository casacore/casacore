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

#include <casa/aips.h>
#include <casa/BasicMath/Math.h>
#include <casa/Arrays/Array.h>
//# Needed to get the proper Complex typedef's
#include <casa/BasicSL/Complex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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
// This example sets the elements of c to (a+b).
// The result of this operation is an Array.
// </example>
//
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


// 
// Function to print "deprecated" message once per program.
void ArrayMinMaxPrintOnceDeprecated ();


// 
// Element by element arithmetic modifying left in-place. left and other
// must be conformant.
// <group>
template<class T> void operator+= (Array<T> &left, const Array<T> &other);
template<class T> void operator-= (Array<T> &left, const Array<T> &other);
template<class T> void operator*= (Array<T> &left, const Array<T> &other);
template<class T> void operator/= (Array<T> &left, const Array<T> &other);
// </group>

// 
// Element by element arithmetic modifying left in-place. The scalar "other"
// behaves as if it were a conformant Array to left filled with constant values.
// <group>
template<class T> void operator+= (Array<T> &left, const T &other);
template<class T> void operator-= (Array<T> &left, const T &other);
template<class T> void operator*= (Array<T> &left, const T &other);
template<class T> void operator/= (Array<T> &left, const T &other);
// </group>

// Unary arithmetic operation.
// 
// <group>
template<class T> Array<T> operator+(const Array<T> &a);
template<class T> Array<T> operator-(const Array<T> &a);
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
template<class T> Array<T> ceil(const Array<T> &a);
template<class T> Array<T> fabs(const Array<T> &a);
template<class T> Array<T> abs(const Array<T> &a);
template<class T> Array<T> floor(const Array<T> &a);
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
// The array is only searched at locations where the mask is True. (at least
// one such position must exist or an exception will be thrown). MaskType
// should be an Array of Bool.
//# See the comments at the beginning of ArrayMath.cc for workarounds for a
//# CFront "core dump or something nasty like that" bug.
template<class ScalarType>
void minMax(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos,
	    IPosition &maxPos, const Array<ScalarType> &array, 
	    const Array<Bool> &mask);
// The array * mask is searched 
//# See the comments at the beginning of ArrayMath.cc for workarounds for a
//# CFront "core dump or something nasty like that" bug.
template<class ScalarType>
void minMaxMasked(ScalarType &minVal, ScalarType &maxVal, IPosition &minPos,
		  IPosition &maxPos, const Array<ScalarType> &array, 
		  const Array<ScalarType> &mask);
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
// This version is deprecated, due to its nonstandard argument order.
template<class T> inline void minMax(const Array<T> &a, T &min, T &max)
{
    ArrayMinMaxPrintOnceDeprecated ();
    minMax (min, max, a);
}
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
// Return an array that contains the minimum of "a" and "b" at each position.
// "a" and "b" must be conformant.
template<class T> Array<T> min(const Array<T> &a, const Array<T> &b);

// "result" contains the maximum of "a" and "b" at each position. "result",
// and "a" must be conformant.
template<class T> void max(Array<T> &result, const Array<T> &a, 
			   const T &b);
// "result" contains the minimum of "a" and "b" at each position. "result",
// and "a" must be conformant.
template<class T> void min(Array<T> &result, const Array<T> &a, 
			   const T &b);
// Return an array that contains the maximum of "a" and "b" at each position.
template<class T> Array<T> max(const Array<T> &a, const T &b);
// Return an array that contains the minimum of "a" and "b" at each position.
template<class T> Array<T> min(const Array<T> &a, const T &b);
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
template<class T>  void indgen(Array<T> &a);
// 
// Fills all elements of "array" with a sequence starting with start
// incremented by one for each position in the array. The first axis varies
// most rapidly.
template<class T>  void indgen(Array<T> &a, T start);


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
// When a has an even number of elements and the switch takeEvenMean is set,
// the median is 0.5*(a(n/2) + a((n+1)/2)).
// According to Numerical Recipes (2nd edition) it makes little sense to take
// the mean when the array is large enough (> 100 elements). Therefore
// the default for takeEvenMean is False when the array has > 100 elements,
// otherwise it is True.
// <br>If "sorted"==True we assume the data is already sorted and we
// compute the median directly. Otherwise the function GenSort::kthLargest
// is used to find the median (kthLargest is about 6 times faster
// than a full quicksort).
// <br>Finding the median means that the array has to be (partially)
// sorted. By default a copy will be made, but if "inPlace" is in effect,
// the data themselves will be sorted. That should only be used if the
// data are used not thereafter.
// <group>
template<class T> inline T median(const Array<T> &a)
    { return median (a, False, (a.nelements() <= 100), False); }
template<class T> inline T median(const Array<T> &a, Bool sorted)
    { return median (a, sorted, (a.nelements() <= 100), False); }
template<class T> inline T medianInPlace(const Array<T> &a,
					 Bool sorted = False)
    { return median (a, sorted, (a.nelements() <= 100), True); }
template<class T> T median(const Array<T> &a, Bool sorted, Bool takeEvenMean,
			   Bool inPlace = False);
// </group>

// Return the fractile of an array.
// It returns the value at the given fraction of the array.
// A fraction of 0.5 is the same as the median, be it that no mean of
// the two middle elements is taken if the array has an even nr of elements.
// It uses kthLargest if the array is not sorted yet.
template<class T> T fractile(const Array<T> &a, Float fraction,
			     Bool sorted = False, Bool inPlace = False);

// The same functions as above, but determine the sum, etc. for the
// given axes only. The result is an array with a shape formed by the
// remaining axes.
// For example, for an array with shape [3,4,5], collapsing axis 0
// results in an array with shape [4,5] containing, say, the sum for
// each X line.
// Summing for axes 0 and 2 results in an array with shape [4] containing,
// say, the sum for each XZ plane.
// <note>
// ArrayLogical.h contains the functions ntrue, nfalse, partialNTrue and
// partialNFalse to count the number of true or false elements in an array.
// </note>
// <group>
template<class T> Array<T> partialSums (const Array<T>& array,
					const IPosition& collapseAxes);
template<class T> Array<T> partialProducts (const Array<T>& array,
					    const IPosition& collapseAxes);
template<class T> Array<T> partialMins (const Array<T>& array,
					const IPosition& collapseAxes);
template<class T> Array<T> partialMaxs (const Array<T>& array,
					const IPosition& collapseAxes);
template<class T> Array<T> partialMeans (const Array<T>& array,
					 const IPosition& collapseAxes);
template<class T> inline Array<T> partialVariances (const Array<T>& array,
					     const IPosition& collapseAxes)
{
    return partialVariances (array, collapseAxes,
			     partialMeans (array, collapseAxes));
}
template<class T> Array<T> partialVariances (const Array<T>& array,
					     const IPosition& collapseAxes,
					     const Array<T>& means);
template<class T> inline Array<T> partialStddevs (const Array<T>& array,
					   const IPosition& collapseAxes)
{
    return sqrt (partialVariances (array, collapseAxes,
				   partialMeans (array, collapseAxes)));
}
template<class T> inline Array<T> partialStddevs (const Array<T>& array,
					   const IPosition& collapseAxes,
					   const Array<T>& means)
{
    return sqrt (partialVariances (array, collapseAxes, means));
}
template<class T> inline Array<T> partialAvdevs (const Array<T>& array,
					  const IPosition& collapseAxes)
{
    return partialAvdevs (array, collapseAxes,
			  partialMeans (array, collapseAxes));
}
template<class T> Array<T> partialAvdevs (const Array<T>& array,
					  const IPosition& collapseAxes,
					  const Array<T>& means);
template<class T> Array<T> partialRmss (const Array<T>& array,
					const IPosition& collapseAxes);
template<class T> Array<T> partialMedians (const Array<T>& array,
					   const IPosition& collapseAxes,
					   Bool takeEvenMean=False,
					   Bool inPlace=False);
template<class T> Array<T> partialFractiles (const Array<T>& array,
					     const IPosition& collapseAxes,
					     Float fraction,
					     Bool inPlace=False);
// </group>

// Methods for element-by-element scaling of Complex by Float
//<group>
void operator*= (Array<Complex> &left, const Array<Float> &other);
void operator*= (Array<Complex> &left, const Float &other);
void operator/= (Array<Complex> &left, const Array<Float> &other);
void operator/= (Array<Complex> &left, const Float &other);
Array<Complex> operator* (const Array<Complex> &left, const Array<Float> &right);
Array<Complex> operator* (const Array<Complex> &left, const Float &right);
Array<Complex> operator/ (const Array<Complex> &left, const Array<Float> &right);
Array<Complex> operator/ (const Array<Complex> &left, const Float &right);
// </group>

// Returns the complex conjugate of a complex array.
//<group>
Array<Complex> conj(const Array<Complex> &carray);
Array<DComplex> conj(const Array<DComplex> &carray);
//# The following are implemented to make the compiler find the right conversion
//# more often.
Matrix<Complex> conj(const Matrix<Complex> &carray);
Matrix<DComplex> conj(const Matrix<DComplex> &carray);
//</group>

// 
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

// Apply the given ArrayMath reduction function to each box in the array.
// <example>
// Downsample an array by taking the mean of every [25,25] elements.
// <srcblock>
//    Array<Float> downArr = boxedArrayMath(in, IPosition(2,25,25),
//                                          casa::mean);
// </srcblock>
// </example>
// The dimensionality of the array can be larger than the box; in that
// case the missing axes of the box are assumed to have length 1.
// A box axis length <= 0 means the full array axis.
template <typename T>
Array<T> boxedArrayMath (const Array<T>& array,
			 const IPosition& boxSize,
			 T (*reductionFunc) (const Array<T>&));

// Apply for each element in the array the given ArrayMath reduction function
// to the box around that element. The full box is 2*halfBoxSize + 1.
// It can be used for arrays and boxes of any dimensionality; missing
// halfBoxSize values are set to 1.
// <example>
// Determine for each element in the array the median of a box
// with size [51,51] around that element:
// <srcblock>
//    Array<Float> medians = boxedArrayMath(in, IPosition(2,25,25),
//                                          casa::median);
// </srcblock>
// This is a potentially expensive operation. On a high-end PC it took
// appr. 27 seconds to get the medians for an array of [1000,1000] using
// a halfBoxSize of [50,50].
// </example>
// <br>The fillEdge argument determines how the edge is filled where
// no full boxes can be made. True means it is set to zero; False means
// that the edge is removed, thus the output array is smaller than the
// input array.
// <note> This brute-force method of determining the medians outperforms
// all kinds of smart implementations. For a vector it is about as fast
// as class <linkto class=MedianSlider>MedianSlider</linkto>, for a 2D array
// it is much, much faster.
// </note>
template <typename T>
Array<T> slidingArrayMath (const Array<T>& array,
			   const IPosition& halfBoxSize,
			   T (*reductionFunc) (const Array<T>&),
			   Bool fillEdge=True);


// Make a copy of an array of a different type; for example make an array
// of doubles from an array of floats. Arrays to and from must be conformant
// (same shape). Also, it must be possible to convert a scalar of type U 
// to type T.
template<class T, class U> void convertArray(Array<T> &to,
					     const Array<U> &from);


// Returns an array where every element is squared.
template<class T> inline Array<T> square(const Array<T> &val)
{
    Array<T> retval(val.copy());
    retval *= retval;
    return retval;
}

// Returns an array where every element is cubed.
template<class T> inline Array<T> cube(const Array<T> &val)
{
    Array<T> retval(val.copy());
    retval *= val;
    retval *= val;
    return retval;
}

// </group>


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Arrays/ArrayMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
