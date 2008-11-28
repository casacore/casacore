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

namespace casa { //# NAMESPACE CASA - BEGIN


  // Define a function to do a binary transform in place.
  // It is functionally equivalent to std::transform where the first and result
  // iterator are the same, but it is faster for non-trivial iterators.
  template<typename InputIterator1, typename InputIterator2, typename BinaryOperator>
  void transformInPlace (InputIterator1 first1, InputIterator1 last1,
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
  void transformInPlace (InputIterator1 first1, InputIterator1 last1,
                         UnaryOperator op)
  {
    for (; first1!=last1; ++first1) {
      *first1 = op(*first1);
    }
  }

  
  // Functor to test for NaN.
  // It can be used in something like:
  // <verbatim>
  //   std::transform (array.begin(), array.end(),
  //                   result.begin(), IsNaN<T>());
  // </verbatim>
  template<typename T> struct IsNaN
  {
    bool operator() (T value) const
      { return isNaN (value); }
  };

  // Functor to test for infinity.
  template<typename T> struct IsInf
  {
    bool operator() (T value) const
      { return isInf (value); }
  };

  // Functor to test if two values are relatively near each other.
  // It can be used in something like:
  // <verbatim>
  //   std::transform (left.begin(), left.cend(), right.begin(),
  //                   result.cbegin(), Near<T>(tolerance));
  // <verbatim>
  template<typename T> struct Near
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
  template<typename T> struct NearAbs
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
  template<typename T> struct Sin
  {
    T operator() (T value) const
      { return sin (value); }
  };

  // Functor to apply sinh.
  template<typename T> struct Sinh
  {
    T operator() (T value) const
      { return sinh (value); }
  };

  // Functor to apply asin.
  template<typename T> struct Asin
  {
    T operator() (T value) const
      { return asin (value); }
  };

  // Functor to apply cos.
  template<typename T> struct Cos
  {
    T operator() (T value) const
      { return cos (value); }
  };

  // Functor to apply cosh.
  template<typename T> struct Cosh
  {
    T operator() (T value) const
      { return cosh (value); }
  };

  // Functor to apply acos.
  template<typename T> struct Acos
  {
    T operator() (T value) const
      { return acos (value); }
  };

  // Functor to apply tan.
  template<typename T> struct Tan
  {
    T operator() (T value) const
      { return tan (value); }
  };

  // Functor to apply tanh.
  template<typename T> struct Tanh
  {
    T operator() (T value) const
      { return tanh (value); }
  };

  // Functor to apply atan.
  template<typename T> struct Atan
  {
    T operator() (T value) const
      { return atan (value); }
  };

  // Functor to apply atan2.
  template<typename T> struct Atan2
  {
    T operator() (T left, T right) const
      { return atan2 (left, right); }
  };

  // Functor to apply sqr.
  template<typename T> struct Sqr
  {
    T operator() (T value) const
      { return value*value; }
  };

  // Functor to apply sqrt.
  template<typename T> struct Sqrt
  {
    T operator() (T value) const
      { return sqrt (value); }
  };

  // Functor to apply exp.
  template<typename T> struct Exp
  {
    T operator() (T value) const
      { return exp (value); }
  };

  // Functor to apply log.
  template<typename T> struct Log
  {
    T operator() (T value) const
      { return log (value); }
  };

  // Functor to apply log10.
  template<typename T> struct Log10
  {
    T operator() (T value) const
      { return log10 (value); }
  };

  // Functor to apply abs.
  template<typename T> struct Abs
  {
    T operator() (T value) const
      { return abs (value); }
  };

  // Functor to apply floor.
  template<typename T> struct Floor
  {
    T operator() (T value) const
      { return floor (value); }
  };

  // Functor to apply ceil.
  template<typename T> struct Ceil
  {
    T operator() (T value) const
      { return ceil (value); }
  };

  // Functor to apply pow.
  template<typename T> struct Pow
  {
    T operator() (T left, T right) const
      { return pow (left, right); }
  };

  // Functor to apply fmod.
  template<typename T> struct Fmod
  {
    T operator() (T left, T right) const
      { return fmod (left, right); }
  };

  // Functor to get minimum of two values.
  template<typename T> struct Min
  {
    T operator() (T left, T right) const
      { return (left<right  ?  left : right); }
  };

  // Functor to get maximum of two values.
  template<typename T> struct Max
  {
    T operator() (T left, T right) const
      { return (left<right  ?  right : left); }
  };

  // Functor to add square of right to left.
  template<typename T, typename Accum=T> struct SumSqr
  {
    T operator() (Accum left, T right) const
      { return left + Accum(right)*Accum(right); }
  };

  // Functor to add squared diff of right and base value to left.
  // It can be used to calculate the standard deviation.
  template<typename T, typename Accum=T> struct SumSqrDiff
  {
    explicit SumSqrDiff(T base) : itsBase(base) {}
    T operator() (Accum left, T right) const
      { return left + (right-itsBase)*(right-itsBase); }
  private:
    Accum itsBase;    // store as Accum, so subtraction results in Accum
  };


} //# NAMESPACE CASA - END

#endif
