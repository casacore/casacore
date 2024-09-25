//# MArrayLogical.h: Logical operations on MArray objects
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_MARRAYLOGICAL_H
#define CASA_MARRAYLOGICAL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/MArrayMathBase.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/BasicMath/Functors.h>

namespace casacore {

  // <summary>
  // Logical operations for MArray objects.
  // </summary>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tMArrayMath">
  //
  // <prerequisite>
  //   <li> <linkto class=MArray>MArray</linkto>
  // </prerequisite>
  //
  // <synopsis>
  // These functions perform element by element logical operations on
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
  // <group name="MArray logical operations">


  // Define functors to perform a reduction function on an MArray object.
  // <group>
  template<typename T, typename RES=size_t>
  class MNTrueFunc : public MArrayFunctorBase<T,RES> {
  public:
    virtual ~MNTrueFunc() {}
    RES operator() (const MArray<T>& arr) const { return ntrue(arr); }
  };
  template<typename T, typename RES=size_t>
  class MNFalseFunc : public MArrayFunctorBase<T,RES> {
  public:
    virtual ~MNFalseFunc() {}
    RES operator() (const MArray<T>& arr) const { return nfalse(arr); }
  };
  template<typename T> class MAllFunc : public MArrayFunctorBase<T,Bool> {
  public:
    virtual ~MAllFunc() {}
    Bool operator() (const MArray<T>& arr) const { return allTrue(arr); }
  };
  template<typename T> class MAnyFunc : public MArrayFunctorBase<T,Bool> {
  public:
    virtual ~MAnyFunc() {}
    Bool operator() (const MArray<T>& arr) const { return anyTrue(arr); }
  };
  // </group>

  // Define comparison functions between 2 MArray objects and
  // between MArray object and scalar.
  // <group>
  template<typename T>
  MArray<Bool> operator== (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() == right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator<= (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() <= right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator< (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() < right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator>= (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() >= right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator> (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() > right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator!= (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() != right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator|| (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() || right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator&& (const MArray<T>& left, const MArray<T>& right)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (left.array() && right.array(),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> operator== (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() == right, left); }

  template<typename T>
  MArray<Bool> operator<= (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() <= right, left); }

  template<typename T>
  MArray<Bool> operator< (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() < right, left); }

  template<typename T>
  MArray<Bool> operator>= (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() >= right, left); }

  template<typename T>
  MArray<Bool> operator> (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() > right, left); }

  template<typename T>
  MArray<Bool> operator!= (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() != right, left); }

  template<typename T>
  MArray<Bool> operator|| (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() || right, left); }

  template<typename T>
  MArray<Bool> operator&& (const MArray<T>& left, const T& right)
    { return MArray<Bool> (left.array() && right, left); }

  template<typename T>
  MArray<Bool> operator== (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left == right.array(), right); }

  template<typename T>
  MArray<Bool> operator<= (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left <= right.array(), right); }

  template<typename T>
  MArray<Bool> operator< (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left < right.array(), right); }

  template<typename T>
  MArray<Bool> operator>= (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left >= right.array(), right); }

  template<typename T>
  MArray<Bool> operator> (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left > right.array(), right); }

  template<typename T>
  MArray<Bool> operator!= (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left != right.array(), right); }
  // </group>

  // The logical OR of 2 MArray objects (normally Bool type)
  template<typename T>
  MArray<Bool> operator|| (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left || right.array(), right); }

  // The logical AND of 2 MArray objects (normally Bool type).
  template<typename T>
  MArray<Bool> operator&& (const T& left, const MArray<T>& right)
    { return MArray<Bool> (left && right.array(), right); }

  // The logical NOT of an MArray object (normally Bool type).
  template<typename T>
  MArray<Bool> operator! (const MArray<T>& a)
    { return MArray<Bool> (!a.array(), a); }


  // Compare with a given relative or absolute tolerance.
  // <group>
  template<typename T>
  MArray<Bool> near (const MArray<T>& left, const MArray<T>& right,
                     Double tol)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (near(left.array(), right.array(), tol),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> nearAbs (const MArray<T>& left, const MArray<T>& right,
                        Double tol)
    { return (left.isNull() || right.isNull()  ?  MArray<Bool>() :
              MArray<Bool> (nearAbs(left.array(), right.array(), tol),
                            left.combineMask(right))); }

  template<typename T>
  MArray<Bool> near (const MArray<T>& left, const T& right,
                     Double tol)
    { return MArray<Bool> (near(left.array(), right, tol), left); }

  template<typename T>
  MArray<Bool> nearAbs (const MArray<T>& left, const T& right,
                     Double tol)
    { return MArray<Bool> (nearAbs(left.array(), right, tol), left); }

  template<typename T>
  MArray<Bool> near (const T& left, const MArray<T>& right,
                     Double tol)
    { return MArray<Bool> (near(left, right.array(), tol), right); }

  template<typename T>
  MArray<Bool> nearAbs (const T& left, const MArray<T>& right,
                        Double tol)
    { return MArray<Bool> (nearAbs(left, right.array(), tol), right); }
  // </group>


  // Test which elements are NaN.
  template<typename T>
  MArray<Bool> isNaN (const MArray<T>& arr)
    { return MArray<Bool> (isNaN(arr.array()), arr); }

  // Test which elements are infinite.
  template<typename T>
  MArray<Bool> isInf (const MArray<T>& arr)
    { return MArray<Bool> (isInf(arr.array()), arr); }

  // Test which elements have a finite value.
  template<typename T>
  MArray<Bool> isFinite (const MArray<T>& arr)
    { return MArray<Bool> (isFinite(arr.array()), arr); }


  // Are all unmasked elements equal?
  // The result is True if there are no unmasked elements.
  // <group>
  template <typename T>
  Bool allEQ (const MArray<T>& left, const MArray<T>& right)
    { if (left.isNull() || right.isNull()) {
        return False;
      } else if (left.hasMask()) {
        if (right.hasMask()) {
          return compareAllMasked (left.array().begin(), left.array().end(),
                                   right.array.begin(), left.mask().begin(),
                                   right.mask().begin(), std::equal_to<T>());
        } else {
          return compareAllMasked (left.array().begin(), left.array().end(),
                                   right.array.begin(), left.mask().begin(),
                                   std::equal_to<T>());
        }
      } else if (right.hasMask()) {
        return compareAllMasked (left.array().begin(), left.array().end(),
                                 right.array.begin(), right.mask().begin(),
                                 std::equal_to<T>());
      }
      return allEQ (left.array(), right.array());
    }
  template <typename T>
  Bool allEQ (const MArray<T>& array, const T& value)
    { return array.isNull()  ?  False :
        array.hasMask() ?
        compareAllRightMasked (array.array().begin(), array.array().end(),
                               value, array.mask().begin(), std::equal_to<T>())
        : allEQ (array.array(), value);
    }
  template <typename T>
  inline Bool allEQ (const T& value, const MArray<T>& array)
    { return allEQ (array, value); }
  // </group>

  // Is any unmasked element equal?
  // The result is False if there are no unmasked elements.
  // <group>
  template <typename T>
  Bool anyEQ (const MArray<T>& left, const MArray<T>& right)
    { if (left.isNull() || right.isNull()) {
        return False;
      } else if (left.hasMask()) {
        if (right.hasMask()) {
          return compareAnyMasked (left.array().begin(), left.array().end(),
                                   right.array.begin(), left.mask().begin(),
                                   right.mask().begin(), std::equal_to<T>());
        } else {
          return compareAnyMasked (left.array().begin(), left.array().end(),
                                   right.array.begin(), left.mask().begin(),
                                   std::equal_to<T>());
        }
      } else if (right.hasMask()) {
        return compareAnyMasked (left.array().begin(), left.array().end(),
                                 right.array.begin(), right.mask().begin(),
                                 std::equal_to<T>());
      }
      return anyEQ (left.array(), right.array());
    }
  template <typename T>
  Bool anyEQ (const MArray<T>& array, const T& value)
    { return array.isNull()  ?  False :
        array.hasMask() ?
        compareAnyRightMasked (array.array().begin(), array.array().end(),
                               value, array.mask().begin(), std::equal_to<T>())
        : anyEQ (array.array(), value);
  }
  template <typename T>
  inline Bool anyEQ (const T& value, const MArray<T>& array)
    { return anyEQ (array, value); }
  // </group>

  // Are all unmasked elements true?
  inline Bool allTrue (const MArray<Bool>& array)
    { return allEQ (array, True); }

  // Is any unmasked element true?
  inline Bool anyTrue (const MArray<Bool>& array)
    { return anyEQ (array, True); }

  // Count the number of unmasked elements that are True.
  template<typename T>
  size_t ntrue(const MArray<T>& a)
  {
    if (a.hasMask()) {
      return a.array().contiguousStorage() && a.mask().contiguousStorage() ?
        countNEMasked<T>(a.array().cbegin(), a.array().cend(),
                         a.mask().cbegin(), T())  :
        countNEMasked<T>(a.array().begin(),  a.array().end(),
                         a.mask().begin(),  T());
    }
    return ntrue(a.array());
  }

  // Count the number of unmasked elements that are False.
  template<typename T>
  size_t nfalse(const MArray<T>& a)
  {
    if (a.hasMask()) {
      return a.array().contiguousStorage() && a.mask().contiguousStorage() ?
        countMasked<T>(a.array().cbegin(), a.array().cend(),
                       a.mask().cbegin(), T())  :
        countMasked<T>(a.array().begin(),  a.array().end(),
                       a.mask().begin(),  T());
    }
    return nfalse(a.array());
  }


  // Get partial ntrues.
  template<typename T>
  MArray<size_t> partialNTrue (const MArray<T>& a,
                             const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<size_t>();
    } else if (! a.hasMask()) {
      return MArray<size_t>(partialNTrue (a.array(), collapseAxes));
    }
    MArray<size_t> res;
    partialArrayMath (res, a, collapseAxes, MNTrueFunc<T,size_t>());
    return res;
  }
  // Get partial nfalses.
  template<typename T>
  MArray<size_t> partialNFalse (const MArray<T>& a,
                              const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<size_t>();
    } else if (! a.hasMask()) {
      return MArray<size_t>(partialNFalse (a.array(), collapseAxes));
    }
    MArray<size_t> res;
    partialArrayMath (res, a, collapseAxes, MNFalseFunc<T,size_t>());
    return res;
  }
  // Get partial all.
  template<typename T>
  MArray<Bool> partialAlls (const MArray<T>& a,
                            const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<Bool>();
    } else if (! a.hasMask()) {
      Array<Bool> res;
      partialArrayMath (res, a.array(), collapseAxes, AllFunc<T>());
      return MArray<Bool>(res);
    }
    MArray<Bool> res;
    partialArrayMath (res, a, collapseAxes, MAllFunc<T>());
    return res;
  }
  // Get partial any.
  template<typename T>
  MArray<Bool> partialAnys (const MArray<T>& a,
                            const IPosition& collapseAxes)
  {
    if (a.isNull()) {
      return MArray<Bool>();
    } else if (! a.hasMask()) {
      Array<Bool> res;
      partialArrayMath (res, a.array(), collapseAxes, AnyFunc<T>());
      return MArray<Bool>(res);
    }
    MArray<Bool> res;
    partialArrayMath (res, a, collapseAxes, MAnyFunc<T>());
    return res;
  }

  // Get sliding ntrues.
  template<typename T>
  MArray<uInt> slidingNTrue (const MArray<T>& a,
                             const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<uInt>();
    } else if (! a.hasMask()) {
      Array<uInt> res;
      slidingArrayMath (res, a.array(), halfBoxSize,
                        NTrueFunc<T,uInt>(), fillEdge);
      return MArray<uInt>(res);
    }
    MArray<uInt> res;
    slidingArrayMath (res, a, halfBoxSize, MNTrueFunc<T,uInt>(), fillEdge);
    return res;
  }
  // Get sliding nfalses.
  template<typename T>
  MArray<uInt> slidingNFalse (const MArray<T>& a,
                              const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<uInt>();
    } else if (! a.hasMask()) {
      Array<uInt> res;
      slidingArrayMath (res, a.array(), halfBoxSize,
                        NFalseFunc<T,uInt>(), fillEdge);
      return MArray<uInt>(res);
    }
    MArray<uInt> res;
    slidingArrayMath (res, a, halfBoxSize, MNFalseFunc<T,uInt>(), fillEdge);
    return res;
  }
  // Get sliding all.
  template<typename T>
  MArray<Bool> slidingAlls (const MArray<T>& a,
                            const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<Bool>();
    } else if (! a.hasMask()) {
      Array<Bool> res;
      slidingArrayMath (res, a.array(), halfBoxSize, AllFunc<T>(), fillEdge);
      return MArray<Bool>(res);
    }
    MArray<Bool> res;
    slidingArrayMath (res, a, halfBoxSize, MAllFunc<T>(), fillEdge);
    return res;
  }
  // Get sliding any.
  template<typename T>
  MArray<Bool> slidingAnys (const MArray<T>& a,
                            const IPosition& halfBoxSize, Bool fillEdge=True)
  {
    if (a.isNull()) {
      return MArray<Bool>();
    } else if (! a.hasMask()) {
      Array<Bool> res;
      slidingArrayMath (res, a.array(), halfBoxSize, AnyFunc<T>(), fillEdge);
      return MArray<Bool>(res);
    }
    MArray<Bool> res;
    slidingArrayMath (res, a, halfBoxSize, MAnyFunc<T>(), fillEdge);
    return res;
  }

  // Get boxed ntrues.
  template<typename T>
  MArray<uInt> boxedNTrue (const MArray<T>& a,
                           const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<uInt>();
    } else if (! a.hasMask()) {
      Array<uInt> res;
      boxedArrayMath (res, a.array(), boxSize, NTrueFunc<T,uInt>());
      return MArray<uInt>(res);
    }
    MArray<uInt> res;
    boxedArrayMath (res, a, boxSize, MNTrueFunc<T,uInt>());
    return res;
  }
  // Get boxed nfalses.
  template<typename T>
  MArray<uInt> boxedNFalse (const MArray<T>& a,
                            const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<uInt>();
    } else if (! a.hasMask()) {
      Array<uInt> res;
      boxedArrayMath (res, a.array(), boxSize, NFalseFunc<T,uInt>());
      return MArray<uInt>(res);
    }
    MArray<uInt> res;
    boxedArrayMath (res, a, boxSize, MNFalseFunc<T,uInt>());
    return res;
  }
  // Get boxed all.
  template<typename T>
  MArray<Bool> boxedAlls (const MArray<T>& a,
                          const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<Bool>();
    } else if (! a.hasMask()) {
      Array<Bool> res;
      boxedArrayMath (res, a.array(), boxSize, AllFunc<T>());
      return MArray<Bool>(res);
    }
    MArray<Bool> res;
    boxedArrayMath (res, a, boxSize, MAllFunc<T>());
    return res;
  }
  // Get boxed any.
  template<typename T>
  MArray<Bool> boxedAnys (const MArray<T>& a,
                          const IPosition& boxSize)
  {
    if (a.isNull()) {
      return MArray<Bool>();
    } else if (! a.hasMask()) {
      Array<Bool> res;
      boxedArrayMath (res, a.array(), boxSize, AnyFunc<T>());
      return MArray<Bool>(res);
    }
    MArray<Bool> res;
    boxedArrayMath (res, a, boxSize, MAnyFunc<T>());
    return res;
  }

  // </group>

} //# end namespace

#endif
