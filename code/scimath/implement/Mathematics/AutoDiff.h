//# AutoDiff.h: an automatic differential class for parameterized functions
//# Copyright (C) 1995,1998
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
//#
//# $Id$

#if !defined(AIPS_AUTO_DIFF_H)
#define AIPS_AUTO_DIFF_H

#include <aips/aips.h>
#include <aips/Arrays/ArrayLogical.h>
#include <trial/Mathematics/VectorPool.h>

// <summary>
// Class that computes partial derivatives by automatic differentiation.
// It does this by storing the value of a function and the values of its first 
// derivatives with respect to its independent parameters.  When a mathematical
// operation is applied to an AutoDiff object, the derivative values of the 
// resulting new object are computed according to chain rules 
// of differentiation.
// </summary>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tAutoDiff.cc" demos="">
// </reviewed>
//
// <prerequisite>
// <li> VectorPool
// </prerequisite>
//
// <etymology>
// Class that computes partial derivatives by automatic differentiation, thus
// AutoDiff.
// </etymology>
//
// <synopsis>
// Suppose we have a function f(x0,x1,...,xn) and its differential is
// <srcblock> 
// df = (df/dx0)*dx0 + (df/dx1)*dx1 + ... + (df/dxn)*dxn
// </srcblock> 
// We can build a class that have the value of the function, 
// f(x0,x1,...,xn), and the values of the derivatives, (df/dx0), (df/dx1), 
// ..., (df/dxn) at (x0,x1,...,xn), as class members. 
//
// Now if we have another function, g(x0,x1,...,xn) and its differential is
// dg = (dg/dx0)*dx0 + (dg/dx1)*dx1 + ... + (dg/dxn)*dxn,
// since 
// <srcblock> 
// d(f+g) = df + dg, 
// d(f*g) = g*df + f*dg, 
// d(f/g) = df/g - fdg/g^2,
// dsin(f) = cos(f)df, 
// ...,
// </srcblock> 
// we can calculate
// <srcblock>  
// d(f+g), d(f*g), ..., 
// </srcblock>  based on our information on
// <srcblock>  
// df/dx0, df/dx1, ..., dg/dx0, dg/dx1, ..., dg/dxn. 
// </srcblock>  
// All we need to do is to define the operators and derivatives of common 
// mathematic functions.
// </synopsis>
//
// <example>
// // To define a function of the form 
// // f(x,y,z) = x, f(10.0,y,z) = 10.0, df/dx = 1.0, df/dy = df/dz = 0.0
// AutoDiff<Double> x(10.0, 3, 0); 
// AutoDiff<Double> y(20.0, 3, 1); 
// AutoDiff<Double> z(30.0, 3, 2);
// AutoDiff<Double> result = x*y + sin(z);
// cout << result.value() << endl;
// cout << result.derivatives() << endl;
// </example>
//
// <motivation>
// The creation of the class was motivated by least-squares nonlinear fit where
// partial derivatives of a fitted function are needed. It would be tidious
// to create functionals for all partial derivatives of a function.
// </motivation>
//
// <todo asof="yyyy/mm/dd">
// </todo>

template <class T> class AutoDiff
{
public:
  // A constant of a value of zero.  Zero derivative.
  AutoDiff();

  // A constant of a value of v.  Zero derivative.
  explicit AutoDiff(const T &v);

  // A function f(x0,x1,...,xi,...,xn) = xi of a value of v.  The 
  // total number of derivatives is ndiffs, the ith derivative is one, and all 
  // others are zero. 
  AutoDiff(const T &v, const uInt ndiffs, const uInt i); 

  // Construct one from another
  AutoDiff(const AutoDiff<T> &other);

  // Construct a function f(x0,x1,...,xn) of a value v and a vector of 
  // derivatives derivs(0) = df/dx0, derivs(0) = df/dx1, ...
  AutoDiff(const T& v, const Vector<T>& derivs);

  ~AutoDiff();

  // Assignment operator.  Assign a constant to variable.  All derivatives
  // are zero.
  AutoDiff<T>& operator=(const T& v);

  // Assign one to another.
  AutoDiff<T>& operator=(const AutoDiff<T> &other);

  // Math operations
  // <group>
  AutoDiff<T>& operator*=(const AutoDiff<T> &other);
  AutoDiff<T>& operator/=(const AutoDiff<T> &other);
  AutoDiff<T>& operator+=(const AutoDiff<T> &other);
  AutoDiff<T>& operator-=(const AutoDiff<T> &other);
  // </group>

  // Returns the value of an AutoDiff
  T& value();
  const T& value() const;

  // Returns a vector of the derivatives of an AutoDiff
  Vector<T>& derivatives();
  const Vector<T>& derivatives() const;

  // Returns a specific derivative
  T& derivative(uInt which);

  // Returns a specific derivative
  const T& derivative(uInt which) const;

  // Return total number of derivatives
  uInt nDerivatives() const;

  // Is it a constant, i.e., with zero derivatives?
  Bool isConstant() const;

  // Resize the number of derivatives
  void resize(uInt ndivs);

  T value_;
  Vector<T> *gradient_;
  uInt nderivs;
  static Vector<T> null;
  static VectorPool<T> pool;
};

#endif
