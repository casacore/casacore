//# ArrayMathBase.h: Basic functions and classes for math on Array objects
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
//#
//# $Id$

#ifndef CASA_ARRAYMATHBASE_H
#define CASA_ARRAYMATHBASE_H

namespace casa {

  // Forward declarations.
  template<typename T> class Array;
  template<typename T> class MArray;


  // <summary>
  // Basic functions and classes for math on Array objects
  // </summary>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tMArrayMath">
  //
  // <prerequisite>
  //   <li> <linkto class=Array>Array</linkto>
  // </prerequisite>
  //
  // <synopsis>
  // This header file defines several STL-like functions to work on
  // iterators with a mask.
  //
  // Furthermore, abstract base classes are defined for functors to be used
  // in functions like slidingXXX. They are defined for both an Array and
  // an MArray object.
  // Virtual functions instead of templated functions are used to avoid
  // code bloat when used in functions like partialArrayMath. Because a
  // reduction operation usually takes much more time than the call, using
  // virtual functions hardly imposes a performance penalty.
  // </synopsis>
  //
  // <group name="Array basic functions">


  // Define STL-like accumulate function operating on arrays with masks.
  // A mask value True means masked-off, thus is not taken into account.
  // <group>
  // <br>The first function initializes the accumulator to the first
  // unmasked value. This is useful if it is not possible to initialize
  // it externally (e.g. for a function like min).
  template<typename T, typename ARRAYITER, typename MASKITER, typename OPER>
  T accumulateMasked (ARRAYITER abegin, ARRAYITER aend, MASKITER mbegin,
                      OPER oper)
  {
    T accum = T();
    for (; abegin!=aend; ++abegin, ++mbegin) {
      if (!*mbegin) { accum = *abegin; ++abegin; ++mbegin; break; }
    }
    for (; abegin!=aend; ++abegin, ++mbegin) {
      if (!*mbegin) accum = oper(accum, *abegin);
    }
    return accum;
  }

  // The second function uses an externally initialized accumulator
  // (e.g. needed for sum).
  template<typename T, typename ARRAYITER, typename MASKITER, typename OPER>
  T accumulateMasked (ARRAYITER abegin, ARRAYITER aend, MASKITER mbegin,
                      T accum, OPER oper)
  {
    for (; abegin!=aend; ++abegin, ++mbegin) {
      if (!*mbegin) accum = oper(accum, *abegin);
    }
    return accum;
  }
  // </group>

  // Count the number of unmasked values matching the given value.
  // It is similar to std::count, but a mask is applied.
  template<typename T, typename ARRAYITER, typename MASKITER>
    size_t countMasked (ARRAYITER abegin, ARRAYITER aend, MASKITER mbegin,
                        const T& value)
    {
      size_t n = 0;
      for (; abegin!=aend; ++abegin, ++mbegin) {
	if (!*mbegin  &&  *abegin == value) ++n;
      }
      return n;
    }

  // Count the number of unmasked values not matching the given value.
  // It is similar to std::count, but a mask is applied.
  template<typename T, typename ARRAYITER, typename MASKITER>
    size_t countNEMasked (ARRAYITER abegin, ARRAYITER aend, MASKITER mbegin,
                        const T& value)
    {
      size_t n = 0;
      for (; abegin!=aend; ++abegin, ++mbegin) {
	if (!*mbegin  &&  *abegin != value) ++n;
      }
      return n;
    }

  // Define a function to compare the unmasked elements of two sequences.
  // It returns true if all unmasked elements compare true or if there are
  // no unmasked elements.
  // An example compare operator is <src>std::equal_to</src>.
  // <group>
  template<typename InputIterator1, typename InputIterator2,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAllMasked (InputIterator1 first1, InputIterator1 last1,
                                InputIterator2 first2, 
                                MaskIterator mask1, MaskIterator mask2,
                                CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++first2, ++mask1, ++mask2) {
      if (!*mask1 && !*mask2) {
        if (!op(*first1, *first2)) return false;
      }
    }
    return true;
  }
  template<typename InputIterator1, typename InputIterator2,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAllMasked (InputIterator1 first1, InputIterator1 last1,
                                InputIterator2 first2, 
                                MaskIterator mask1,
                                CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++first2, ++mask1) {
      if (!*mask1) {
        if (!op(*first1, *first2)) return false;
      }
    }
    return true;
  }
  // For use with a constant left value.
  // This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
  // (see ArrayMath.h).
  template<typename InputIterator1, typename T,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAllLeftMasked (InputIterator1 first1, InputIterator1 last1,
                                    T left, MaskIterator mask1,
                                    CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++mask1) {
      if (!*mask1) {
        if (!op(left, *first1)) return false;
      }
    }
    return true;
  }
  // For use with a constant right value.
  // This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
  // (see ArrayMath.h).
  template<typename InputIterator1, typename T,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAllRightMasked(InputIterator1 first1, InputIterator1 last1,
                                    T right, MaskIterator mask1,
                                    CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++mask1) {
      if (!*mask1) {
        if (!op(*first1, right)) return false;
      }
    }
    return true;
  }
  // </group>

  // Define a function to compare the unmasked elements of two sequences.
  // It returns true if any element compares true.
  // If there are no unmasked elements, it returns false.
  // An example compare operator is <src>std::equal_to</src>.
  // <group>
  template<typename InputIterator1, typename InputIterator2,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAnyMasked (InputIterator1 first1, InputIterator1 last1,
                                InputIterator2 first2, 
                                MaskIterator mask1, MaskIterator mask2,
                                CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++first2, ++mask1, ++mask2) {
      if (!*mask1 && !*mask2) {
        if (op(*first1, *first2)) return true;
      }
    }
    return false;
  }
  template<typename InputIterator1, typename InputIterator2,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAnyMasked (InputIterator1 first1, InputIterator1 last1,
                                InputIterator2 first2, 
                                MaskIterator mask1,
                                CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++first2, ++mask1) {
      if (!*mask1) {
        if (op(*first1, *first2)) return true;
      }
    }
    return false;
  }
  // For use with a constant left value.
  // This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
  // (see ArrayMath.h).
  template<typename InputIterator1, typename T,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAnyLeftMasked (InputIterator1 first1, InputIterator1 last1,
                                    T left, MaskIterator mask1,
                                    CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++mask1) {
      if (!*mask1) {
        if (op(left, *first1)) return true;
      }
    }
    return false;
  }
  // For use with a constant right value.
  // This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
  // (see ArrayMath.h).
  template<typename InputIterator1, typename T,
           typename MaskIterator, typename CompareOperator>
  inline bool compareAnyRightMasked(InputIterator1 first1, InputIterator1 last1,
                                    T right, MaskIterator mask1,
                                    CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++mask1) {
      if (!*mask1) {
        if (op(*first1, right)) return true;
      }
    }
    return false;
  }
  // </group>


  // Define the base class for functors to perform a reduction function on an
  // Array object. The functors themselves are defined elsewhere.
  template<typename T, typename RES=T> class ArrayFunctorBase {
  public:
    virtual ~ArrayFunctorBase() {}
    virtual RES operator() (const Array<T>&) const = 0;
  };

  // Define the base class for functors to perform a reduction function on an
  // MArray object. The functors themselves are defined elsewhere.
  template<typename T, typename RES=T> class MArrayFunctorBase {
  public:
    virtual ~MArrayFunctorBase() {}
    virtual RES operator() (const MArray<T>&) const = 0;
  };

  // </group>

} //# end namespace

#endif
