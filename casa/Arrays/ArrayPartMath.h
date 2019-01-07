//# ArrayPartMath.h: mathematics done on an array parts.
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
//# $Id: ArrayPartMath.h 21262 2012-09-07 12:38:36Z gervandiepen $

#ifndef CASA_ARRAYPARTMATH_H
#define CASA_ARRAYPARTMATH_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayMathBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Mathematical and logical operations for Array parts.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArray">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
// </prerequisite>
//
// <etymology>
// This file contains global functions which perform part by part
// mathematical or logical operations on arrays.
// </etymology>
//
// <synopsis>
// These functions perform chunk by chunk mathematical operations on
// arrays.
// In particular boxed and sliding operations are possible. E.g. to calculate
// the median in sliding windows making it possible to subtract the background
// in an image.
//
// The operations to be performed are defined by means of functors that
// reduce an array subset to a scalar. Those functors are wrappers for
// ArrayMath and ArrayLogical functions like sum, median, and ntrue. 
//
// The <src>partialXX</src> functions are a special case of the 
// <src>BoxedArrayMath</src> function.
// They reduce one or more entire axes which can be done in a faster way than
// the more general <src>boxedArrayMath</src> function.
// </synopsis>
//
// <example>
// <srcblock>
// Array<Double> data(...);
// Array<Double> means = partialMeans (data, IPosition(2,0,1));
// </srcblock>
// This example calculates the mean of each plane in the data array.
// </example>
//
// <example>
// <srcblock>
// IPosition shp = data.shape();
// Array<Double> means = boxedArrayMath (data, IPosition(2,shp[0],shp[1]),
//                                       SumFunc<Double>());
// </srcblock>
// does the same as the first example.
// Note that in this example the box is formed by the entire axes, but it
// could also be a subset of it to average, say, boxes of 5*5 elements.
// </example>
//
// <linkfrom anchor="Array mathematical operations" classes="Array Vector Matrix Cube">
//    <here>Array mathematical operations</here> -- Mathematical operations for
//    Arrays.
// </linkfrom>
//
// <group name="Array partial operations">


// Determine the sum, product, etc. for the given axes only.
// The result is an array with a shape formed by the remaining axes.
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
template<class T> Array<T> partialSumSqrs (const Array<T>& array,
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
                                                    const IPosition& collapseAxes,
                                                    uInt ddof=1)
{
    return partialVariances (array, collapseAxes,
			     partialMeans (array, collapseAxes), ddof);
}
template<class T> Array<T> partialVariances (const Array<T>& array,
					     const IPosition& collapseAxes,
					     const Array<T>& means);
template<class T> Array<T> partialVariances (const Array<T>& array,
					     const IPosition& collapseAxes,
					     const Array<T>& means,
                                             uInt ddof);
template<class T> Array<std::complex<T>> partialVariances (const Array<std::complex<T>>& array,
                                                           const IPosition& collapseAxes,
                                                           const Array<std::complex<T>>& means,
                                                           uInt ddof);
template<class T> inline Array<T> partialStddevs (const Array<T>& array,
                                                  const IPosition& collapseAxes,
                                                  uInt ddof=1)
{
    return sqrt (partialVariances (array, collapseAxes,
				   partialMeans (array, collapseAxes), ddof));
}
template<class T> inline Array<T> partialStddevs (const Array<T>& array,
                                                  const IPosition& collapseAxes,
                                                  const Array<T>& means,
                                                  uInt ddof=1)
{
  return sqrt (partialVariances (array, collapseAxes, means, ddof));
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
template<class T> Array<T> partialMadfms (const Array<T>& array,
                                          const IPosition& collapseAxes,
                                          Bool takeEvenMean=False,
                                          Bool inPlace=False);
template<class T> Array<T> partialFractiles (const Array<T>& array,
                                             const IPosition& collapseAxes,
                                             Float fraction,
                                             Bool inPlace=False);
template<class T> Array<T> partialInterFractileRanges (const Array<T>& array,
                                                       const IPosition& collapseAxes,
                                                       Float fraction,
                                                       Bool inPlace=False);
template<class T> Array<T> partialInterHexileRanges (const Array<T>& array,
                                                     const IPosition& collapseAxes,
                                                     Bool inPlace=False)
  { return partialInterFractileRanges (array, collapseAxes, 1./6., inPlace); }
template<class T> Array<T> partialInterQuartileRanges (const Array<T>& array,
                                                      const IPosition& collapseAxes,
                                                      Bool inPlace=False)
  { return partialInterFractileRanges (array, collapseAxes, 0.25, inPlace); }
// </group>



  // Define functors to perform a reduction function on an Array object.
  // Use virtual functions instead of templates to avoid code bloat
  // in partialArrayMath, etc.
  template<typename T> class SumFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~SumFunc() {}
    virtual T operator() (const Array<T>& arr) const { return sum(arr); }
  };
  template<typename T> class SumSqrFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~SumSqrFunc() {}
    virtual T operator() (const Array<T>& arr) const { return sumsqr(arr); }
  };
  template<typename T> class ProductFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~ProductFunc() {}
    virtual T operator() (const Array<T>& arr) const { return product(arr); }
  };
  template<typename T> class MinFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~MinFunc() {}
    virtual T operator() (const Array<T>& arr) const { return min(arr); }
  };
  template<typename T> class MaxFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~MaxFunc() {}
    virtual T operator() (const Array<T>& arr) const { return max(arr); }
  };
  template<typename T> class MeanFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~MeanFunc() {}
    virtual T operator() (const Array<T>& arr) const { return mean(arr); }
  };
  template<typename T> class VarianceFunc : public ArrayFunctorBase<T> {
  public:
    explicit VarianceFunc (uInt ddof)
      : itsDdof(ddof) {}
    virtual ~VarianceFunc() {}
    virtual T operator() (const Array<T>& arr) const { return pvariance(arr, itsDdof); }
  private:
    uInt itsDdof;
  };
  template<typename T> class StddevFunc : public ArrayFunctorBase<T> {
  public:
    explicit StddevFunc (uInt ddof)
      : itsDdof(ddof) {}
    virtual ~StddevFunc() {}
    virtual T operator() (const Array<T>& arr) const { return pstddev(arr, itsDdof); }
  private:
    uInt itsDdof;
  };
  template<typename T> class AvdevFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~AvdevFunc() {}
    virtual T operator() (const Array<T>& arr) const { return avdev(arr); }
  };
  template<typename T> class RmsFunc : public ArrayFunctorBase<T> {
  public:
    virtual ~RmsFunc() {}
    virtual T operator() (const Array<T>& arr) const { return rms(arr); }
  };
  template<typename T> class MedianFunc : public ArrayFunctorBase<T> {
  public:
    explicit MedianFunc (Bool sorted=False, Bool takeEvenMean=True,
                          Bool inPlace = False)
      : itsSorted(sorted), itsTakeEvenMean(takeEvenMean), itsInPlace(inPlace) {}
    virtual ~MedianFunc() {}
    virtual T operator() (const Array<T>& arr) const
      { return median(arr, itsTmp, itsSorted, itsTakeEvenMean, itsInPlace); }
  private:
    Bool     itsSorted;
    Bool     itsTakeEvenMean;
    Bool     itsInPlace;
    mutable Block<T> itsTmp;
  };
  template<typename T> class MadfmFunc {
  public:
    explicit MadfmFunc(Bool sorted = False, Bool takeEvenMean = True,
                       Bool inPlace = False)
      : itsSorted(sorted), itsTakeEvenMean(takeEvenMean), itsInPlace(inPlace) {}
    virtual ~MadfmFunc() {}
    virtual T operator()(const Array<T>& arr) const
      { return madfm(arr, itsTmp, itsSorted, itsTakeEvenMean, itsInPlace); }
  private:
    Bool     itsSorted;
    Bool     itsTakeEvenMean;
    Bool     itsInPlace;
    mutable Block<Float> itsTmp;
  };
  template<typename T> class FractileFunc : public ArrayFunctorBase<T> {
  public:
    explicit FractileFunc (Float fraction,
                            Bool sorted = False, Bool inPlace = False)
      : itsFraction(fraction), itsSorted(sorted), itsInPlace(inPlace) {}
    virtual ~FractileFunc() {}
    virtual T operator() (const Array<T>& arr) const
      { return fractile(arr, itsTmp, itsFraction, itsSorted, itsInPlace); }
  private:
    float    itsFraction;
    Bool     itsSorted;
    Bool     itsInPlace;
    mutable Block<T> itsTmp;
  };
  template<typename T> class InterFractileRangeFunc {
  public:
    explicit InterFractileRangeFunc(Float fraction,
                                    Bool sorted = False, Bool inPlace = False)
      : itsFraction(fraction), itsSorted(sorted), itsInPlace(inPlace) {}
    virtual ~InterFractileRangeFunc() {}
    virtual T operator()(const Array<T>& arr) const
      { return interFractileRange(arr, itsTmp, itsFraction,
                                  itsSorted, itsInPlace); }
  private:
    float    itsFraction;
    Bool     itsSorted;
    Bool     itsInPlace;
    mutable Block<Float> itsTmp;
  };
  template<typename T> class InterHexileRangeFunc: public InterFractileRangeFunc<T> {
  public:
    explicit InterHexileRangeFunc(Bool sorted = False, Bool inPlace = False)
      : InterFractileRangeFunc<T> (1./6., sorted, inPlace)
    {}
    virtual ~InterHexileRangeFunc() {}
  };
  template<typename T> class InterQuartileRangeFunc: public InterFractileRangeFunc<T> {
  public:
    explicit InterQuartileRangeFunc(Bool sorted = False, Bool inPlace = False)
      : InterFractileRangeFunc<T> (0.25, sorted, inPlace)
    {} 
    virtual ~InterQuartileRangeFunc() {}
  };



  // Do partial reduction of an Array object. I.e., perform the operation
  // on a subset of the array axes (the collapse axes).
  template<typename T>
  inline Array<T> partialArrayMath (const Array<T>& a,
                                    const IPosition& collapseAxes,
                                    const ArrayFunctorBase<T>& funcObj)
  {
    Array<T> res;
    partialArrayMath (res, a, collapseAxes, funcObj);
    return res;
  }
  template<typename T, typename RES>
  void partialArrayMath (Array<RES>& res,
                         const Array<T>& a,
                         const IPosition& collapseAxes,
                         const ArrayFunctorBase<T,RES>& funcObj);


// Apply the given ArrayMath reduction function objects
// to each box in the array.
// <example>
// Downsample an array by taking the median of every [25,25] elements.
// <srcblock>
//    Array<Float> downArr = boxedArrayMath(in, IPosition(2,25,25),
//                                          MedianFunc<Float>());
// </srcblock>
// </example>
// The dimensionality of the array can be larger than the box; in that
// case the missing axes of the box are assumed to have length 1.
// A box axis length <= 0 means the full array axis.
  template<typename T>
  inline Array<T> boxedArrayMath (const Array<T>& a,
                                  const IPosition& boxSize,
                                  const ArrayFunctorBase<T>& funcObj)
  {
    Array<T> res;
    boxedArrayMath (res, a, boxSize, funcObj);
    return res;
  }
  template<typename T, typename RES>
  void boxedArrayMath (Array<RES>&,
                       const Array<T>& array,
                       const IPosition& boxSize,
                       const ArrayFunctorBase<T,RES>& funcObj);

// Apply for each element in the array the given ArrayMath reduction function
// object to the box around that element. The full box is 2*halfBoxSize + 1.
// It can be used for arrays and boxes of any dimensionality; missing
// halfBoxSize values are set to 0.
// <example>
// Determine for each element in the array the median of a box
// with size [51,51] around that element:
// <srcblock>
//    Array<Float> medians = slidingArrayMath(in, IPosition(2,25,25),
//                                            MedianFunc<Float>());
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
  template<typename T>
  inline Array<T> slidingArrayMath (const Array<T>& a,
                                    const IPosition& halfBoxSize,
                                    const ArrayFunctorBase<T>& funcObj,
                                    Bool fillEdge=True)
  {
    Array<T> res;
    slidingArrayMath (res, a, halfBoxSize, funcObj, fillEdge);
    return res;
  }
  template<typename T, typename RES>
  void slidingArrayMath (Array<RES>& res,
                         const Array<T>& array,
                         const IPosition& halfBoxSize,
                         const ArrayFunctorBase<T,RES>& funcObj,
                         Bool fillEdge=True);

// </group>

// <group>
// Helper functions for boxed and sliding functions.
// Determine full box shape and shape of result for a boxed operation.
void fillBoxedShape (const IPosition& shape, const IPosition& boxShape,
                     IPosition& fullBoxShape, IPosition& resultShape);
// Determine the box end and shape of result for a sliding operation.
// It returns False if the result is empty.
Bool fillSlidingShape (const IPosition& shape, const IPosition& halfBoxSize,
                       IPosition& boxEnd, IPosition& resultShape);
// </group>

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/ArrayPartMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
