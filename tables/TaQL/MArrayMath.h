//# MArrayMath.h: Mathematical operations on MArray objects
//# Copyright (C) 2012
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

#ifndef CASA_MARRAYMATH_H
#define CASA_MARRAYMATH_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/MArray.h>
#include <casacore/tables/TaQL/MArrayMathBase.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/BasicMath/Functors.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore {

  // <summary>
  // Mathematical operations for MArray objects.
  // </summary>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tMArrayMath">
  //
  // <prerequisite>
  //   <li> <linkto class=MArray>MArray</linkto>
  // </prerequisite>
  //
  // <synopsis>
  // These functions perform element by element mathematical operations on
  // optionally masked arrays and/or scalars.
  // If two arrays are used, the arrays must conform, except for allEQ which
  // returns False if the arrays do not conform.
  //
  // The functions in this file can be divided in 3 groups:
  // <ul>
  //  <li> Full array operations like ==, near, etc.
  //       They are defined for array-array and array-scalar operations. Arrays
  //       shapes have to be conformant. They operate on all elements
  //       (also the masked ones). The result is an MArray with the same
  //       shape as the input array(s). It will have a mask if one of the
  //       operands has a mask. If both operands have a mask, the resulting
  //       mask is the OR of both masks.
  //  <li> Full reduction functions like ntrue, all, allEQ, etc.
  //       They operate on the unmasked elements only. If there are no unmasked
  //       elements, the results is 0 or True.
  //  <li> Reduction functions working on unmasked elements in parts of the
  //       input array. The result is an MArray that has a mask if the input
  //       array has a mask. An output element is masked off if its input
  //       part has no unmasked elements.
  //       The functors defined at the beginning of this file are used to
  //       operate on each part.
  //       There are 3 flavours:
  //   <ul>
  //    <li> partialXXX reduces one or more axes. E.g. one can count the
  //         number of True elements for particular array axes.
  //         The result is an array with a lower dimensionality.
  //         They can be seen as a special versions of the boxedXXX functions.
  //    <li> slidingXXX operates in a sliding window over the array. So the
  //         result is an array with the same shape as the input, although
  //         the output array is smaller if the edge is not filled.
  //    <li> boxedXXX divides the input array in boxes with the given size
  //         and operates on each box. The result is an array with the same
  //         dimensionality, but with a smaller size.
  //         If the box size does not fit integrally, the edge box is smaller.
  //   </ul>
  // </ul>
  // </synopsis>
  //
  // <group name="MArray mathematical operations">

  // Define functors to perform a reduction function on an MArray object.
  // <group>
  template<typename T> class MSumFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MSumFunc() {}
    T operator() (const MArray<T>& arr) const { return sum(arr); }
  };
  template<typename T> class MSumSqrFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MSumSqrFunc() {}
    T operator() (const MArray<T>& arr) const { return sumsqr(arr); }
  };
  template<typename T> class MProductFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MProductFunc() {}
    T operator() (const MArray<T>& arr) const { return product(arr); }
  };
  template<typename T> class MMinFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MMinFunc() {}
    T operator() (const MArray<T>& arr) const { return min(arr); }
  };
  template<typename T> class MMaxFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MMaxFunc() {}
    T operator() (const MArray<T>& arr) const { return max(arr); }
  };
  template<typename T> class MMeanFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MMeanFunc() {}
    T operator() (const MArray<T>& arr) const { return mean(arr); }
  };
  template<typename T> class MVarianceFunc : public MArrayFunctorBase<T> {
  public:
    explicit MVarianceFunc(uInt ddof=0)
      : itsDdof(ddof) {}
    virtual ~MVarianceFunc() {}
    T operator() (const MArray<T>& arr) const { return variance(arr, itsDdof); }
  private:
    uInt itsDdof;
  };
  template<typename T> class MStddevFunc : public MArrayFunctorBase<T> {
  public:
    explicit MStddevFunc(uInt ddof=0)
      : itsDdof(ddof) {}
    ~MStddevFunc() {}
    T operator() (const MArray<T>& arr) const { return stddev(arr, itsDdof); }
  private:
    uInt itsDdof;
  };
  template<typename T> class MAvdevFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MAvdevFunc() {}
    T operator() (const MArray<T>& arr) const { return avdev(arr); }
  };
  template<typename T> class MRmsFunc : public MArrayFunctorBase<T> {
  public:
    virtual ~MRmsFunc() {}
    T operator() (const MArray<T>& arr) const { return rms(arr); }
  };
  template<typename T> class MMedianFunc : public MArrayFunctorBase<T> {
  public:
    explicit MMedianFunc (Bool sorted=False, Bool takeEvenMean=True,
                          Bool inPlace = False)
      : itsSorted(sorted), itsTakeEvenMean(takeEvenMean), itsInPlace(inPlace) {}
    virtual ~MMedianFunc() {}
    T operator() (const MArray<T>& arr) const
      { return median(arr, itsSorted, itsTakeEvenMean, itsInPlace); }
  private:
    Bool itsSorted;
    Bool itsTakeEvenMean;
    Bool itsInPlace;
  };
  template<typename T> class MFractileFunc : public MArrayFunctorBase<T> {
  public:
    explicit MFractileFunc (Float fraction,
                            Bool sorted = False, Bool inPlace = False)
      : itsFraction(fraction), itsSorted(sorted), itsInPlace(inPlace) {}
    virtual ~MFractileFunc() {}
    T operator() (const MArray<T>& arr) const
      { return fractile(arr, itsFraction, itsSorted, itsInPlace); }
  private:
    float itsFraction;
    Bool  itsSorted;
    Bool  itsInPlace;
  };


  // Do partial reduction of an MArray object. I.e., perform the operation
  // on a subset of the array axes (the collapse axes).
  template<typename T>
  inline MArray<T> partialArrayMath (const MArray<T>& a,
                                     const IPosition& collapseAxes,
                                     const MArrayFunctorBase<T>& funcObj)
  {
    MArray<T> res;
    partialArrayMath (res, a, collapseAxes, funcObj);
    return res;
  }
  template<typename T, typename RES>
  void partialArrayMath (MArray<RES>& res,
                         const MArray<T>& a,
                         const IPosition& collapseAxes,
                         const MArrayFunctorBase<T,RES>& funcObj)
  {
    AlwaysAssert (a.hasMask(), AipsError);
    // This can also be done as boxedArrayMath with a removeDegenerate thereafter.
    //
    // It should be possible to parallelize this loop.
    // Determine nr of iteration steps and iterate over that as an int.
    // Do not use Array slicing, because that is not thread-safe.
    // Instead create ArraySTLIterator directly from Array and blc,trc,
    // so funcObj should accept iterators instead of Array.
    // However, ArraySTLIterator needs the sliced array, not original.
    // Maybe keep ref of itsSteps in iterator instead of array.
    // Hmm, tricky for median and fractile.
    // Better to make Array copy ctor thread-safe (thus use boost shared_ptr).
    ReadOnlyArrayIterator<T>    aiter(a.array(), collapseAxes);
    ReadOnlyArrayIterator<Bool> miter(a.mask(),  collapseAxes);
    IPosition shape(a.array().shape().removeAxes (collapseAxes));
    /*
    Int64 nr = 1;
    for (uInt i=0; i<collapseAxes.size(); ++i) {
      nr *= a.array().shape()[collapseAxes[i]];
    }
    ///#pragma omp parallel
    for (Int64 i=0; i<nr; ++i) {
      IPosition pos = findPos(i);
      IPosition endPos = pos + cursorShape - 1;
      *data[pos] = funcObj(MArray<T>(a.array()(pos,endPos), a.mask()(pos,endpos)));
    }
    */
    ///IPosition shape(a.array().shape().removeAxes (collapseAxes));
    res.resize (shape, False);
    Array<Bool> resMask(shape);
    RES* data = res.array().data();
    Bool* mask = resMask.data();
    while (!aiter.pastEnd()) {
      if (allTrue(miter.array())) {
        *mask++ = True;
        *data++ = RES();
      } else {
        *mask++ = False;
        *data++ = funcObj(MArray<T> (aiter.array(), miter.array()));
      }
      aiter.next();
      miter.next();
    }
    res.setMask (resMask);
  }
  // </group>


  template<typename T>
  inline MArray<T> boxedArrayMath (const MArray<T>& a,
                                   const IPosition& boxShape,
                                   const MArrayFunctorBase<T>& funcObj)
  {
    MArray<T> res;
    boxedArrayMath (res, a, boxShape, funcObj);
    return res;
  }
  template<typename T, typename RES>
  void boxedArrayMath (MArray<RES>& res,
                       const MArray<T>& array,
                       const IPosition& boxShape,
                       const MArrayFunctorBase<T,RES>& funcObj)
  {
    AlwaysAssert (array.hasMask(), AipsError);
    const IPosition& shape = array.shape();
    uInt ndim = shape.size();
    IPosition fullBoxShape, resShape;
    fillBoxedShape (shape, boxShape, fullBoxShape, resShape);
    res.resize (resShape, False);
    Array<Bool> resMask(resShape);
    RES* data = res.array().data();
    Bool* mask = resMask.data();
    // Loop through all data and assemble as needed.
    IPosition blc(ndim, 0);
    IPosition trc(fullBoxShape-1);
    while (True) {
      Array<Bool> subMask (array.mask()(blc,trc));
      if (allTrue(subMask)) {
        *data++ = RES();
        *mask++ = True;
      } else {
        *data++ = funcObj (MArray<T>(array.array()(blc,trc), subMask));
        *mask++ = False;
      }
      uInt ax;
      for (ax=0; ax<ndim; ++ax) {
        blc[ax] += fullBoxShape[ax];
        if (blc[ax] < shape[ax]) {
          trc[ax] += fullBoxShape[ax];
          if (trc[ax] >= shape[ax]) {
            trc[ax] = shape[ax]-1;
          }
          break;
        }
        blc[ax] = 0;
        trc[ax] = fullBoxShape[ax]-1;
      }
      if (ax == ndim) {
        break;
      }
    }
    res.setMask (resMask);
  }

  template <typename T>
  inline MArray<T> slidingArrayMath (const MArray<T>& array,
                                     const IPosition& halfBoxShape,
                                     const MArrayFunctorBase<T>& funcObj,
                                     Bool fillEdge=True)
  {
    MArray<T> res;
    slidingArrayMath (res, array, halfBoxShape, funcObj, fillEdge);
    return res;
  }
  template <typename T, typename RES>
  void slidingArrayMath (MArray<RES>& res,
                         const MArray<T>& array,
                         const IPosition& halfBoxShape,
                         const MArrayFunctorBase<T,RES>& funcObj,
                         Bool fillEdge=True)
  {
    AlwaysAssert (array.hasMask(), AipsError);
    const IPosition& shape = array.shape();
    uInt ndim = shape.size();
    IPosition boxEnd, resShape;
    Bool empty = fillSlidingShape (shape, halfBoxShape, boxEnd, resShape);
    if (fillEdge) {
      res.resize (shape, False);
      res.array() = RES();
      Array<Bool> mask(shape, True);
      res.setMask (mask);
    } else {
      res.resize (resShape, True);
    }
    if (!empty) {
      Array<RES>  resa (res.array());
      Array<Bool> resm (res.mask());
      if (fillEdge) {
        IPosition boxEnd2 (boxEnd/2);
        resa.reference (resa(boxEnd2, resShape+boxEnd2-1));
        resm.reference (resm(boxEnd2, resShape+boxEnd2-1));
      }
      typename Array<RES>::iterator  iterarr(resa.begin());
      typename Array<Bool>::iterator itermask(resm.begin());
      // Loop through all data and assemble as needed.
      IPosition blc(ndim, 0);
      IPosition trc(boxEnd);
      IPosition pos(ndim, 0);
      while (True) {
        Array<Bool> subMask (array.mask()(blc,trc));
        if (allTrue(subMask)) {
          *iterarr  = RES();
          *itermask = True;
        } else {
          *iterarr  = funcObj (MArray<T>(array.array()(blc,trc), subMask));
          *itermask = False;
        }
        ++iterarr;
        ++itermask;
        uInt ax;
        for (ax=0; ax<ndim; ++ax) {
          if (++pos[ax] < resShape[ax]) {
            blc[ax]++;
            trc[ax]++;
            break;
          }
          pos(ax) = 0;
          blc[ax] = 0;
          trc[ax] = boxEnd[ax];
        }
        if (ax == ndim) {
          break;
        }
      }
    }
  }


  // Add, subtract, etc. 2 arrays or array and scalar.
  // <group>
  template<typename T>
  MArray<T> operator+ (const MArray<T>& left, const MArray<T>& right)
  { return (left.isNull() || right.isNull()  ?  MArray<T>() :
            MArray<T> (left.array() + right.array(),
                       left.combineMask(right))); }

  template<typename T>
  MArray<T> operator- (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (left.array() - right.array(),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> operator* (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (left.array() * right.array(),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> operator/ (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (left.array() / right.array(),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> operator% (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (left.array() % right.array(),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> operator& (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (left.array() & right.array(),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> operator| (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (left.array() | right.array(),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> operator^ (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (left.array() ^ right.array(),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> operator+ (const MArray<T>& left, const T& right)
  { return MArray<T> (left.array() + right, left); }

  template<typename T>
  MArray<T> operator- (const MArray<T>& left, const T& right)
    { return MArray<T> (left.array() - right, left); }

  template<typename T>
  MArray<T> operator* (const MArray<T>& left, const T& right)
    { return MArray<T> (left.array() * right, left); }

  template<typename T>
  MArray<T> operator/ (const MArray<T>& left, const T& right)
    { return MArray<T> (left.array() / right, left); }

  template<typename T>
  MArray<T> operator% (const MArray<T>& left, const T& right)
    { return MArray<T> (left.array() % right, left); }

  template<typename T>
  MArray<T> operator& (const MArray<T>& left, const T& right)
    { return MArray<T> (left.array() & right, left); }

  template<typename T>
  MArray<T> operator| (const MArray<T>& left, const T& right)
    { return MArray<T> (left.array() | right, left); }

  template<typename T>
  MArray<T> operator^ (const MArray<T>& left, const T& right)
    { return MArray<T> (left.array() ^ right, left); }

  template<typename T>
  MArray<T> operator+ (const T& left, const MArray<T>& right)
    { return MArray<T> (left + right.array(), right); }

  template<typename T>
  MArray<T> operator- (const T& left, const MArray<T>& right)
    { return MArray<T> (left - right.array(), right); }

  template<typename T>
  MArray<T> operator* (const T& left, const MArray<T>& right)
    { return MArray<T> (left * right.array(), right); }

  template<typename T>
  MArray<T> operator/ (const T& left, const MArray<T>& right)
    { return MArray<T> (left / right.array(), right); }

  template<typename T>
  MArray<T> operator% (const T& left, const MArray<T>& right)
    { return MArray<T> (left % right.array(), right); }

  template<typename T>
  MArray<T> operator& (const T& left, const MArray<T>& right)
    { return MArray<T> (left & right.array(), right); }

  template<typename T>
  MArray<T> operator| (const T& left, const MArray<T>& right)
    { return MArray<T> (left | right.array(), right); }

  template<typename T>
  MArray<T> operator^ (const T& left, const MArray<T>& right)
    { return MArray<T> (left ^ right.array(), right); }
  // </group>

  // Negate the elements in an array.
  template<typename T>
  MArray<T> operator- (const MArray<T>& a)
    { return MArray<T> (-a.array(), a); }

  // Take the complement of the elements in an array.
  template<typename T>
  MArray<T> operator~ (const MArray<T>& a)
    { return MArray<T> (~a.array(), a); }

  // Perform mathematical function on each element in an array.
  // <group>
  template<typename T>
  MArray<T> sin(const MArray<T>& a)
    { return MArray<T> (sin(a.array()), a); }

  template<typename T>
  MArray<T> cos(const MArray<T>& a)
    { return MArray<T> (cos(a.array()), a); }

  template<typename T>
  MArray<T> tan(const MArray<T>& a)
    { return MArray<T> (tan(a.array()), a); }

  template<typename T>
  MArray<T> sinh(const MArray<T>& a)
    { return MArray<T> (sinh(a.array()), a); }

  template<typename T>
  MArray<T> cosh(const MArray<T>& a)
    { return MArray<T> (cosh(a.array()), a); }

  template<typename T>
  MArray<T> tanh(const MArray<T>& a)
    { return MArray<T> (tanh(a.array()), a); }

  template<typename T>
  MArray<T> asin(const MArray<T>& a)
    { return MArray<T> (asin(a.array()), a); }

  template<typename T>
  MArray<T> acos(const MArray<T>& a)
    { return MArray<T> (acos(a.array()), a); }

  template<typename T>
  MArray<T> atan(const MArray<T>& a)
    { return MArray<T> (atan(a.array()), a); }

  template<typename T>
  MArray<T> atan2(const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (atan2(left.array(), right.array()),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> atan2(const MArray<T>& left, const T& right)
    { return MArray<T> (atan2(left.array(), right), left); }

  template<typename T>
  MArray<T> atan2(const T& left, const MArray<T>& right)
    { return MArray<T> (atan2(left, right.array()), right); }

  template<typename T>
  MArray<T> exp(const MArray<T>& a)
    { return MArray<T> (exp(a.array()), a); }

  template<typename T>
  MArray<T> log(const MArray<T>& a)
    { return MArray<T> (log(a.array()), a); }

  template<typename T>
  MArray<T> log10(const MArray<T>& a)
    { return MArray<T> (log10(a.array()), a); }

  template<typename T>
  MArray<T> sqrt(const MArray<T>& a)
    { return MArray<T> (sqrt(a.array()), a); }

  template<typename T>
  MArray<T> square(const MArray<T>& a)
    { return MArray<T> (square(a.array()), a); }

  template<typename T>
  MArray<T> cube(const MArray<T>& a)
    { return MArray<T> (cube(a.array()), a); }

  template<typename T>
  MArray<T> pow(const MArray<T>& a, const MArray<T>& exp)
    { return (a.isNull() || exp.isNull()  ?  MArray<T>() :
              MArray<T> (pow(a.array(), exp.array()),
                         a.combineMask(exp))); }

  template<typename T>
  MArray<T> pow(const T& a, const MArray<T>& exp)
    { return MArray<T> (pow(a, exp.array()), exp); }

  template<typename T>
  MArray<T> pow(const MArray<T>& a, const Double& exp)
    { return MArray<T> (pow(a.array(), exp), a); }

  template<typename T>
  MArray<T> min(const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (min(left.array(), right.array()),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> min(const MArray<T>& left, const T& right)
    { return MArray<T> (min(left.array(), right), left); }

  template<typename T>
  MArray<T> min(const T& left, const MArray<T>& right)
    { return MArray<T> (min(left, right.array()), right); }

  template<typename T>
  MArray<T> max(const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (max(left.array(), right.array()),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> max(const MArray<T>& left, const T& right)
    { return MArray<T> (max(left.array(), right), left); }

  template<typename T>
  MArray<T> max(const T& left, const MArray<T>& right)
    { return MArray<T> (max(left, right.array()), right); }

  template<typename T>
  MArray<T> ceil(const MArray<T>& a)
    { return MArray<T> (ceil(a.array()), a); }

  template<typename T>
  MArray<T> floor(const MArray<T>& a)
    { return MArray<T> (floor(a.array()), a); }

  template<typename T>
  MArray<T> round(const MArray<T>& a)
    { return MArray<T> (round(a.array()), a); }

  template<typename T>
  MArray<T> sign(const MArray<T>& a)
    { return MArray<T> (sign(a.array()), a); }

  template<typename T>
  MArray<T> abs(const MArray<T>& a)
    { return MArray<T> (abs(a.array()), a); }

  template<typename T>
  MArray<T> fabs(const MArray<T>& a)
    { return MArray<T> (fabs(a.array()), a); }

  template<typename T>
  MArray<T> fmod(const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (fmod(left.array(), right.array()),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> fmod(const MArray<T>& left, const T& right)
    { return MArray<T> (fmod(left.array(), right), left); }

  template<typename T>
  MArray<T> fmod(const T& left, const MArray<T>& right)
    { return MArray<T> (fmod(left, right.array()), right); }

  template<typename T>
  MArray<T> floormod(const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<T>() :
              MArray<T> (floormod(left.array(), right.array()),
                         left.combineMask(right))); }

  template<typename T>
  MArray<T> floormod(const MArray<T>& left, const T& right)
    { return MArray<T> (floormod(left.array(), right), left); }

  template<typename T>
  MArray<T> floormod(const T& left, const MArray<T>& right)
    { return MArray<T> (floormod(left, right.array()), right); }

  template<typename T>
  MArray<T> conj(const MArray<T>& arr)
    { return MArray<T> (conj(arr.array()), arr); }

  inline MArray<Float> real(const MArray<Complex> &arr)
    { return MArray<Float> (real(arr.array()), arr); }

  inline MArray<Float> imag(const MArray<Complex> &arr)
    { return MArray<Float> (imag(arr.array()), arr); }

  inline MArray<Float> amplitude(const MArray<Complex> &arr)
    { return MArray<Float> (amplitude(arr.array()), arr); }

  inline MArray<Float> phase(const MArray<Complex> &arr)
    { return MArray<Float> (phase(arr.array()), arr); }

  inline MArray<Double> real(const MArray<DComplex> &arr)
    { return MArray<Double> (real(arr.array()), arr); }

  inline MArray<Double> imag(const MArray<DComplex> &arr)
    { return MArray<Double> (imag(arr.array()), arr); }

  inline MArray<Double> amplitude(const MArray<DComplex> &arr)
    { return MArray<Double> (amplitude(arr.array()), arr); }

  inline MArray<Double> phase(const MArray<DComplex> &arr)
    { return MArray<Double> (phase(arr.array()), arr); }
  // </group>


  // Reduce an array to a scalar using the unmasked elements only.
  // The result is 0 if there are no unmasked elements.
  // <group>
  template<typename T>
  T sum(const MArray<T>& a)
  {
    if (a.hasMask()) {
      return a.array().contiguousStorage() && a.mask().contiguousStorage() ?
        accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                            a.mask().cbegin(), T(), std::plus<T>()) :
        accumulateMasked<T>(a.array().begin(),  a.array().end(),
                            a.mask().begin(),  T(), std::plus<T>());
    }
    return sum(a.array());
  }

  template<typename T>
  T sumsqr(const MArray<T>& a)
  {
    if (a.hasMask()) {
      return a.array().contiguousStorage() && a.mask().contiguousStorage() ?
        accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                            a.mask().cbegin(), T(), SumSqr<T>()) :
        accumulateMasked<T>(a.array().begin(),  a.array().end(),
                            a.mask().begin(),  T(), SumSqr<T>());
    }
    return sumsqr(a.array());
  }

  template<typename T>
  T product(const MArray<T>& a)
  {
    if (a.hasMask()) {
      return a.array().contiguousStorage() && a.mask().contiguousStorage() ?
        accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                            a.mask().cbegin(), std::multiplies<T>()) :
        accumulateMasked<T>(a.array().begin(),  a.array().end(),
                            a.mask().begin(),  std::multiplies<T>());
    }
    return product(a.array());
  }

  template<typename T>
  T min(const MArray<T>& a)
  {
    if (a.hasMask()) {
      return a.array().contiguousStorage() && a.mask().contiguousStorage() ?
        accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                            a.mask().cbegin(), Min<T>()) :
        accumulateMasked<T>(a.array().begin(),  a.array().end(),
                            a.mask().begin(),  Min<T>());
    }
    return min(a.array());
  }

  template<typename T>
  T max(const MArray<T>& a)
  {
    if (a.hasMask()) {
      return a.array().contiguousStorage() && a.mask().contiguousStorage() ?
        accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                            a.mask().cbegin(), Max<T>()) :
        accumulateMasked<T>(a.array().begin(),  a.array().end(),
                            a.mask().begin(),  Max<T>());
    }
    return max(a.array());
  }

  template<typename T>
  T mean(const MArray<T>& a)
  {
    Int64 nv = a.nvalid();
    if (nv == 0) return T();
    if (! a.hasMask()) return mean(a.array());
    return T(sum(a) / (1.0*nv));
  }

  template<typename T>
  T variance(const MArray<T>& a, T mean, uInt ddof)
  {
    Int64 nv = a.nvalid();
    if (nv < ddof+1) return T();
    if (! a.hasMask()) return pvariance(a.array(), mean, ddof);
    T sum = a.array().contiguousStorage() && a.mask().contiguousStorage() ?
      accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                          a.mask().cbegin(), T(), SumSqrDiff<T>(mean)) :
      accumulateMasked<T>(a.array().begin(),  a.array().end(),
                          a.mask().begin(),  T(), SumSqrDiff<T>(mean));
    return T(sum / (1.0*nv - ddof));
  }

  template<typename T>
  T variance(const MArray<T>& a, uInt ddof)
  {
    return variance(a, mean(a), ddof);
  }

  template<typename T>
  T stddev(const MArray<T>& a, uInt ddof)
  {
    return sqrt(variance(a, ddof));
  }

  template<typename T>
  T stddev(const MArray<T>& a, T mean, uInt ddof)
  {
    return sqrt(variance(a, mean, ddof));
  }

  template<typename T>
  T avdev(const MArray<T>& a, T mean)
  {
    Int64 nv = a.nvalid();
    if (nv == 0) return T();
    if (! a.hasMask()) return avdev(a.array(), mean);
    T sum = a.array().contiguousStorage() && a.mask().contiguousStorage() ?
      accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                          a.mask().cbegin(), T(), SumAbsDiff<T>(mean)) :
      accumulateMasked<T>(a.array().begin(),  a.array().end(),
                          a.mask().begin(),  T(), SumAbsDiff<T>(mean));
    return T(sum / (1.0*nv));
  }

  template<typename T>
  T avdev(const MArray<T>& a)
  {
    return avdev(a, mean(a));
  }

  template<typename T>
  T rms(const MArray<T>& a)
  {
    Int64 nv = a.nvalid();
    if (nv == 0) return T();
    if (! a.hasMask()) return rms(a.array());
    T sum = a.array().contiguousStorage() && a.mask().contiguousStorage() ?
      accumulateMasked<T>(a.array().cbegin(), a.array().cend(),
                          a.mask().cbegin(), T(), SumSqr<T>()) :
      accumulateMasked<T>(a.array().begin(),  a.array().end(),
                          a.mask().begin(),  T(), SumSqr<T>());
    return T(sqrt(sum / (1.0*nv)));
  }

  template<typename T>
  T median(const MArray<T> &a, Bool sorted, Bool takeEvenMean,
           Bool inPlace=False)
  {
    // The normal median function needs at least one element, so shortcut.
    if (a.empty()) return T();
    if (! a.hasMask()) return median(a.array(), sorted, takeEvenMean, inPlace);
    Block<T> buf(a.size());
    Int64 nv = a.flatten (buf.storage(), buf.size());
    if (nv == 0) return T();
    Array<T> arr(IPosition(1, nv), buf.storage(), SHARE);
    // Median can be taken in place.
    return median (arr, sorted, takeEvenMean, True);
  }
  template<typename T>
  inline T median(const MArray<T> &a)
    { return median (a, False, (a.size() <= 100), False); }
  template<typename T>
  inline T median(const MArray<T> &a, Bool sorted)
    { return median (a, sorted, (a.nelements() <= 100), False); }
  template<typename T>
  inline T medianInPlace(const MArray<T> &a, Bool sorted = False)
    { return median (a, sorted, (a.nelements() <= 100), True); }

  // Return the fractile of an array.
  // It returns the value at the given fraction of the array.
  // A fraction of 0.5 is the same as the median, be it that no mean of
  // the two middle elements is taken if the array has an even nr of elements.
  // It uses kthLargest if the array is not sorted yet.
  template<typename T>
  T fractile(const MArray<T> &a, Float fraction, Bool sorted=False,
             Bool inPlace=False)
  {
    // The normal fractile function needs at least one element, so shortcut.
    if (a.empty()) return T();
    if (! a.hasMask()) return fractile(a.array(), fraction, sorted, inPlace);
    Block<T> buf(a.size());
    Int64 nv = a.flatten (buf.storage(), a.size());
    if (nv == 0) return T();
    Array<T> arr(IPosition(1, nv), buf.storage(), SHARE);
    return fractile (arr, fraction, sorted, True);
  }
  // </group>

  // Get partial sums, etc.
  // <group>
  template<typename T>
  MArray<T> partialSums (const MArray<T>& a,
                         const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialSums (a.array(), collapseAxes));
    }
    return partialArrayMath (a, collapseAxes, MSumFunc<T>());
  }
  template<typename T>
  MArray<T> partialSumSqrs (const MArray<T>& a,
                            const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialArrayMath (a.array(), collapseAxes,
                                         SumSqrFunc<T>()));
    }
    return partialArrayMath (a, collapseAxes, MSumSqrFunc<T>());
  }
  template<typename T>
  MArray<T> partialProducts (const MArray<T>& a,
                             const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialProducts (a.array(), collapseAxes));
    }
    return partialArrayMath (a, collapseAxes, MProductFunc<T>());
  }
  template<typename T>
  MArray<T> partialMins (const MArray<T>& a,
                         const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialMins (a.array(), collapseAxes));
    }
    return partialArrayMath (a, collapseAxes, MMinFunc<T>());
  }
  template<typename T>
  MArray<T> partialMaxs (const MArray<T>& a,
                         const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialMaxs (a.array(), collapseAxes));
    }
    return partialArrayMath (a, collapseAxes, MMaxFunc<T>());
  }
  template<typename T>
  MArray<T> partialMeans (const MArray<T>& a,
                          const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialMeans (a.array(), collapseAxes));
    }
    return partialArrayMath (a, collapseAxes, MMeanFunc<T>());
  }
  template<typename T>
  MArray<T> partialVariances (const MArray<T>& a,
                              const IPosition& collapseAxes,
                              uInt ddof)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialVariances (a.array(), collapseAxes, ddof));
    }
    return partialArrayMath (a, collapseAxes, MVarianceFunc<T>(ddof));
  }
  template<typename T>
  MArray<T> partialStddevs (const MArray<T>& a,
                            const IPosition& collapseAxes,
                            uInt ddof)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialStddevs (a.array(), collapseAxes, ddof));
    }
    return partialArrayMath (a, collapseAxes, MStddevFunc<T>(ddof));
  }
  template<typename T>
  MArray<T> partialAvdevs (const MArray<T>& a,
                           const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialAvdevs (a.array(), collapseAxes));
    }
    return partialArrayMath (a, collapseAxes, MAvdevFunc<T>());
  }
  template<typename T>
  MArray<T> partialRmss (const MArray<T>& a,
                         const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialRmss (a.array(), collapseAxes));
    }
    return partialArrayMath (a, collapseAxes, MRmsFunc<T>());
  }
  template<typename T>
  MArray<T> partialMedians (const MArray<T>& a,
                            const IPosition& collapseAxes,
                            Bool takeEvenMean=False,
                            Bool inPlace=False)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialMedians (a.array(), collapseAxes,
                                       takeEvenMean, inPlace));
    }
    return partialArrayMath (a, collapseAxes,
                             MMedianFunc<T>(False, takeEvenMean, inPlace));
  }
  template<typename T>
  MArray<T> partialFractiles (const MArray<T>& a,
                              const IPosition& collapseAxes,
                              Float fraction,
                              Bool inPlace=False)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(partialFractiles (a.array(), collapseAxes,
                                         fraction, inPlace));
    }
    return partialArrayMath (a, collapseAxes,
                             MFractileFunc<T>(fraction, False, inPlace));
  }
  // </group>

  // Get sliding sums.
  // <group>
  template<typename T>
  MArray<T> slidingSums (const MArray<T>& a,
                         const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          SumFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MSumFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingSumSqrs (const MArray<T>& a,
                            const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                         SumSqrFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MSumSqrFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingProducts (const MArray<T>& a,
                             const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          ProductFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MProductFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingMins (const MArray<T>& a,
                         const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          MinFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MMinFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingMaxs (const MArray<T>& a,
                         const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          MaxFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MMaxFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingMeans (const MArray<T>& a,
                          const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          MeanFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MMeanFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingVariances (const MArray<T>& a,
                              const IPosition& halfBoxSize,
                              uInt ddof,
                              Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          VarianceFunc<T>(ddof), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MVarianceFunc<T>(ddof), fillEdge);
  }
  template<typename T>
  MArray<T> slidingStddevs (const MArray<T>& a,
                            const IPosition& halfBoxSize,
                            uInt ddof,
                            Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          StddevFunc<T>(ddof), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MStddevFunc<T>(ddof), fillEdge);
  }
  template<typename T>
  MArray<T> slidingAvdevs (const MArray<T>& a,
                           const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          AvdevFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MAvdevFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingRmss (const MArray<T>& a,
                         const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                          RmsFunc<T>(), fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize, MRmsFunc<T>(), fillEdge);
  }
  template<typename T>
  MArray<T> slidingMedians (const MArray<T>& a,
                            const IPosition& halfBoxSize,
                            Bool takeEvenMean=False,
                            Bool inPlace=False,
                            Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                         MedianFunc<T>(False, takeEvenMean,
                                                       inPlace),
                                         fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize,
                             MMedianFunc<T>(False, takeEvenMean, inPlace),
                             fillEdge);
  }
  template<typename T>
  MArray<T> slidingFractiles (const MArray<T>& a,
                              const IPosition& halfBoxSize,
                              Float fraction,
                              Bool inPlace=False,
                              Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(slidingArrayMath (a.array(), halfBoxSize,
                                         FractileFunc<T>(fraction, False,
                                                         inPlace),
                                         fillEdge));
    }
    return slidingArrayMath (a, halfBoxSize,
                             MFractileFunc<T>(fraction, False, inPlace),
                             fillEdge);
  }
  // </group>

  // Get boxed sums.
  // <group>
  template<typename T>
  MArray<T> boxedSums (const MArray<T>& a,
                       const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, SumFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MSumFunc<T>());
  }
  template<typename T>
  MArray<T> boxedSumSqrs (const MArray<T>& a,
                          const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, SumSqrFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MSumSqrFunc<T>());
  }
  template<typename T>
  MArray<T> boxedProducts (const MArray<T>& a,
                           const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, ProductFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MProductFunc<T>());
  }
  template<typename T>
  MArray<T> boxedMins (const MArray<T>& a,
                       const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, MinFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MMinFunc<T>());
  }
  template<typename T>
  MArray<T> boxedMaxs (const MArray<T>& a,
                       const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, MaxFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MMaxFunc<T>());
  }
  template<typename T>
  MArray<T> boxedMeans (const MArray<T>& a,
                        const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, MeanFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MMeanFunc<T>());
  }
  template<typename T>
  MArray<T> boxedVariances (const MArray<T>& a,
                            const IPosition& boxSize,
                            uInt ddof)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, VarianceFunc<T>(ddof)));
    }
    return boxedArrayMath (a, boxSize, MVarianceFunc<T>(ddof));
  }
  template<typename T>
  MArray<T> boxedStddevs (const MArray<T>& a,
                          const IPosition& boxSize,
                          uInt ddof)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, StddevFunc<T>(ddof)));
    }
    return boxedArrayMath (a, boxSize, MStddevFunc<T>(ddof));
  }
  template<typename T>
  MArray<T> boxedAvdevs (const MArray<T>& a,
                         const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, AvdevFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MAvdevFunc<T>());
  }
  template<typename T>
  MArray<T> boxedRmss (const MArray<T>& a,
                       const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize, RmsFunc<T>()));
    }
    return boxedArrayMath (a, boxSize, MRmsFunc<T>());
  }
  template<typename T>
  MArray<T> boxedMedians (const MArray<T>& a,
                          const IPosition& boxSize,
                          Bool takeEvenMean=False,
                          Bool inPlace=False)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize,
                                       MedianFunc<T>(False, takeEvenMean,
                                                     inPlace)));
    }
    return boxedArrayMath (a, boxSize,
                           MMedianFunc<T>(False, takeEvenMean, inPlace));
  }
  template<typename T>
  MArray<T> boxedFractiles (const MArray<T>& a,
                            const IPosition& boxSize,
                            Float fraction,
                            Bool inPlace=False)
  {
    if (a.isNull()) {
      return MArray<T>();
    } else if (! a.hasMask()) {
      return MArray<T>(boxedArrayMath (a.array(), boxSize,
                                       FractileFunc<T>(fraction, False,
                                                       inPlace)));
    }
    return boxedArrayMath (a, boxSize,
                           MFractileFunc<T>(fraction, False, inPlace));
  }
  // </group>

  // </group>

} //# end namespace

#endif
