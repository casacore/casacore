//# Functors.h: Define STL functors for basic math functions.
//# Copyright (C) 2008
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

#ifndef CASA_FUNCTORS_H
#define CASA_FUNCTORS_H

#include <casa/BasicMath/Math.h>
#include <functional>

namespace casa { //# NAMESPACE CASA - BEGIN


  // Define a function to do a binary transform in place.
  // It is functionally equivalent to std::transform where the first and result
  // iterator are the same, but it is faster for non-trivial iterators.
  template<typename InputIterator1, typename InputIterator2, typename BinaryOperator>
  inline void transformInPlace (InputIterator1 first1, InputIterator1 last1,
                                InputIterator2 first2, BinaryOperator op)
  {
    for (; first1!=last1; ++first1, ++first2) {
      *first1 = op(*first1, *first2);
    }
  }
  
  // Define a function to do a unary transform in place.
  // It is functionally equivalent to std::transform where the first and result
  // iterator are the same, but it is faster for non-trivial iterators.
  template<typename InputIterator1, typename UnaryOperator>
  inline void transformInPlace (InputIterator1 first1, InputIterator1 last1,
                                UnaryOperator op)
  {
    for (; first1!=last1; ++first1) {
      *first1 = op(*first1);
    }
  }

  // Define a function (similar to std::accumulate) to do accumulation of
  // elements for which the corresponding mask value is true.
  // The default accumulation is addition.
  template<typename InputIterator, typename MaskIterator, typename Accum, typename BinaryOperator>
  inline Accum accumulateTrue (InputIterator first, InputIterator last,
                               MaskIterator mask, Accum acc,
                               BinaryOperator op = std::plus<Accum>())
  {
    for (; first!=last; ++first, ++mask) {
      if (*mask) acc = op(acc, *first);
    }
    return acc;
  }
  
  // Define a function (similar to std::accumulate) to do accumulation of
  // elements for which the corresponding mask value is false.
  // The default accumulation is addition.
  template<typename InputIterator, typename MaskIterator, typename Accum, typename BinaryOperator>
  inline Accum accumulateFalse (InputIterator first, InputIterator last,
                                MaskIterator mask, Accum acc,
                                BinaryOperator op = std::plus<Accum>())
  {
    for (; first!=last; ++first, ++mask) {
      if (!*mask) acc = op(acc, *first);
    }
    return acc;
  }
  
  // Define a function to compare all elements of two sequences.
  // It returns true if all elements compare true.
  // An example compare operator is <src>std::equal_to</src>.
  // <group>
  template<typename InputIterator1, typename InputIterator2, typename CompareOperator>
  inline bool compareAll (InputIterator1 first1, InputIterator1 last1,
                          InputIterator2 first2, CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++first2) {
      if (!op(*first1, *first2)) return false;
    }
    return true;
  }
  // For use with a constant value (through bind1st or bind2nd).
  template<typename InputIterator1, typename CompareOperator>
  inline bool compareAll (InputIterator1 first1, InputIterator1 last1,
                          CompareOperator op)
  {
    for (; first1!=last1; ++first1) {
      if (!op(*first1)) return false;
    }
    return true;
  }
  // </group>

  // Define a function to compare all elements of two sequences.
  // It returns true if any element compares true.
  // An example compare operator is <src>std::equal_to</src>.
  // <group>
  template<typename InputIterator1, typename InputIterator2, typename CompareOperator>
  inline bool compareAny (InputIterator1 first1, InputIterator1 last1,
                          InputIterator2 first2, CompareOperator op)
  {
    for (; first1!=last1; ++first1, ++first2) {
      if (op(*first1, *first2)) return true;
    }
    return false;
  }
  // For use with a constant value (through bind1st or bind2nd).
  template<typename InputIterator1, typename CompareOperator>
  inline bool compareAny (InputIterator1 first1, InputIterator1 last1,
                          CompareOperator op)
  {
    for (; first1!=last1; ++first1) {
      if (op(*first1)) return true;
    }
    return false;
  }
  // </group>
  


  // Functor to add variables of possible different types.
  // This is unlike std::plus which requires equal types.
  template <typename L, typename R=L, typename RES=L>
  struct Plus : public std::binary_function<L,R,RES>
  {
    RES operator() (const L& x, const R& y) const
      { return RES(x)+y; }
  };

  // Functor to subtract variables of possible different types.
  // This is unlike std::minus which requires equal types.
  template <typename L, typename R=L, typename RES=L>
  struct Minus : public std::binary_function<L,R,RES>
  {
    RES operator() (const L& x, const R& y) const
      { return RES(x)-y; }
  };

  // Functor to multiply variables of possible different types.
  // This is unlike std::multiplies which requires equal types.
  template <typename L, typename R=L, typename RES=L>
  struct Multiplies : public std::binary_function<L,R,RES>
  {
    RES operator() (const L& x, const R& y) const
      { return RES(x)*y; }
  };

  // Functor to divide variables of possible different types.
  // This is unlike std::divides which requires equal types.
  template <typename L, typename R=L, typename RES=L>
  struct Divides : public std::binary_function<L,R,RES>
  {
    RES operator() (const L& x, const R& y) const
      { return RES(x)/y; }
  };

  // Functor to test for NaN.
  // It can be used in something like:
  // <srcblock>
  //   std::transform (array.begin(), array.end(),
  //                   result.begin(), IsNaN<T>());
  // </srcblock>
  template<typename T>
  struct IsNaN : public std::unary_function<T,bool>
  {
    bool operator() (T value) const
      { return isNaN (value); }
  };

  // Functor to test for infinity.
  template<typename T>
  struct IsInf : public std::unary_function<T,bool>
  {
    bool operator() (T value) const
      { return isInf (value); }
  };

  // Functor to test if two values are relatively near each other.
  // It can be used in something like:
  // <srcblock>
  //   std::transform (left.begin(), left.cend(), right.begin(),
  //                   result.cbegin(), Near<T>(tolerance));
  // </srcblock>
  template<typename T>
  struct Near : public std::binary_function<T,T,bool>
  {
    explicit Near (double tolerance=1e-5)
      : itsTolerance (tolerance)
    {}
    bool operator() (T left, T right) const
      { return near (left, right, itsTolerance); }
  private:
    double itsTolerance;
  };

  // Functor to test for if two values are absolutely near each other.
  template<typename T>
  struct NearAbs : public std::binary_function<T,T,bool>
  {
    explicit NearAbs (double tolerance=1e-13)
      : itsTolerance (tolerance)
    {}
    bool operator() (T left, T right) const
      { return nearAbs (left, right, itsTolerance); }
  private:
    double itsTolerance;
  };


  // Functor to apply sin.
  template<typename T>
  struct Sin : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return sin (value); }
  };

  // Functor to apply sinh.
  template<typename T>
  struct Sinh : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return sinh (value); }
  };

  // Functor to apply asin.
  template<typename T>
  struct Asin : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return asin (value); }
  };

  // Functor to apply cos.
  template<typename T>
  struct Cos : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return cos (value); }
  };

  // Functor to apply cosh.
  template<typename T>
  struct Cosh : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return cosh (value); }
  };

  // Functor to apply acos.
  template<typename T>
  struct Acos : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return acos (value); }
  };

  // Functor to apply tan.
  template<typename T>
  struct Tan : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return tan (value); }
  };

  // Functor to apply tanh.
  template<typename T>
  struct Tanh : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return tanh (value); }
  };

  // Functor to apply atan.
  template<typename T>
  struct Atan : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return atan (value); }
  };

  // Functor to apply atan2.
  template<typename T>
  struct Atan2 : public std::binary_function<T,T,T>
  {
    T operator() (T left, T right) const
      { return atan2 (left, right); }
  };

  // Functor to apply sqr.
  template<typename T>
  struct Sqr : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return value*value; }
  };

  // Functor to apply sqrt.
  template<typename T>
  struct Sqrt : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return sqrt (value); }
  };

  // Functor to apply exp.
  template<typename T>
  struct Exp : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return exp (value); }
  };

  // Functor to apply log.
  template<typename T>
  struct Log : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return log (value); }
  };

  // Functor to apply log10.
  template<typename T>
  struct Log10 : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return log10 (value); }
  };

  // Functor to apply abs.
  template<typename T>
  struct Abs : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return abs (value); }
  };

  // Functor to apply floor.
  template<typename T>
  struct Floor : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return floor (value); }
  };

  // Functor to apply ceil.
  template<typename T>
  struct Ceil : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return ceil (value); }
  };

  // Functor to apply complex function conj.
  template<typename T>
  struct Conj : public std::unary_function<T,T>
  {
    T operator() (T value) const
      { return conj (value); }
  };

  // Functor to apply complex function real.
  template<typename T, typename RES>
  struct Real : public std::unary_function<T,RES>
  {
    RES operator() (T value) const
      { return real (value); }
  };

  // Functor to apply complex function imag.
  template<typename T, typename RES>
  struct Imag : public std::unary_function<T,RES>
  {
    RES operator() (T value) const
      { return imag (value); }
  };

  // Functor to apply complex function arg.
  template<typename T, typename RES>
  struct CArg : public std::unary_function<T,RES>
  {
    RES operator() (T value) const
      { return arg (value); }
  };

  // Functor to apply complex function fabs.
  template<typename T, typename RES>
  struct CAbs : public std::unary_function<T,RES>
  {
    RES operator() (T value) const
      { return fabs (value); }
  };

  // Functor to apply pow.
  template<typename T, typename E=T>
  struct Pow : public std::binary_function<T,E,T>
  {
    T operator() (T left, E exponent) const
      { return pow (left, exponent); }
  };

  // Functor to apply fmod.
  template<typename T>
  struct Fmod : public std::binary_function<T,T,T>
  {
    T operator() (T left, T right) const
      { return fmod (left, right); }
  };

  // Functor to get minimum of two values.
  template<typename T>
  struct Min : public std::binary_function<T,T,T>
  {
    T operator() (T left, T right) const
      { return (left<right  ?  left : right); }
  };

  // Functor to get maximum of two values.
  template<typename T>
  struct Max : public std::binary_function<T,T,T>
  {
    T operator() (T left, T right) const
      { return (left<right  ?  right : left); }
  };

  // Functor to add square of right to left.
  template<typename T, typename Accum=T>
  struct SumSqr : public std::binary_function<Accum,T,Accum>
  {
    Accum operator() (Accum left, T right) const
      { return left + Accum(right)*Accum(right); }
  };

  // Functor to add squared diff of right and base value to left.
  // It can be used to calculate the standard deviation.
  template<typename T, typename Accum=T>
  struct SumSqrDiff : public std::binary_function<Accum,T,Accum>
  {
    explicit SumSqrDiff(T base) : itsBase(base) {}
    Accum operator() (Accum left, T right) const
      { return left + (right-itsBase)*(right-itsBase); }
  private:
    Accum itsBase;    // store as Accum, so subtraction results in Accum
  };

  // Functor to add absolute diff of right and base value to left.
  // It can be used to calculate the average deviation.
  template<typename T, typename Accum=T>
  struct AbsDiff : public std::binary_function<Accum,T,Accum>
  {
    explicit AbsDiff(T base) : itsBase(base) {}
    Accum operator() (Accum left, T right) const
      { return left + abs((right-itsBase)); }
  private:
    Accum itsBase;    // store as Accum, so subtracttion results in Accum
  };

} //# NAMESPACE CASA - END

#endif
