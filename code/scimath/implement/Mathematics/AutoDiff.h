//# AutoDiff.h: an automatic differential class for parameterized functions
//# Copyright (C) 1995,1998,1999,2001
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

#if !defined(AIPS_AUTODIFF_H)
#define AIPS_AUTODIFF_H

//# Includes
#include <aips/aips.h>
#include <trial/Mathematics/AutoDiffRep.h>
#include <trial/Mathematics/ObjectPool.h>

//# Forward declarations
template <class T> class Vector;

// <summary>
// Class that computes partial derivatives by automatic differentiation.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tAutoDiff.cc" demos="">
// </reviewed>
//
// <prerequisite>
// <li> 
// </prerequisite>
//
// <etymology>
// Class that computes partial derivatives by automatic differentiation, thus
// AutoDiff.
// </etymology>
//
// <synopsis>
// Class that computes partial derivatives by automatic differentiation.
// It does this by storing the value of a function and the values of its first 
// derivatives with respect to its independent parameters.  When a mathematical
// operation is applied to an AutoDiff object, the derivative values of the 
// resulting new object are computed according to chain rules 
// of differentiation.

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
// <srcblock>
// // To define a function of the form 
// // f(x,y,z) = x, f(10.0,y,z) = 10.0, df/dx = 1.0, df/dy = df/dz = 0.0
// AutoDiff<Double> x(10.0, 3, 0); 
// AutoDiff<Double> y(20.0, 3, 1); 
// AutoDiff<Double> z(30.0, 3, 2);
// AutoDiff<Double> result = x*y + sin(z);
// cout << result.value() << endl;
// cout << result.derivatives() << endl;
// </srcblock>
// </example>
//
// <motivation>
// The creation of the class was motivated by least-squares nonlinear fit where
// partial derivatives of a fitted function are needed. It would be tidious
// to create functionals for all partial derivatives of a function.
// </motivation>
//
// <templating arg=T>
//  <li> any class that has the standard mathematical and comparisons
//	defined
// </templating>
//
// <todo asof="2001/06/07">
// <li> Nothing I know off
// </todo>

template <class T> class AutoDiff {
 public:
  //# Constructors
  // Construct a constant with a value of zero.  Zero derivatives.
  AutoDiff();

  // Construct a constant with a value of v.  Zero derivatives.
  explicit AutoDiff(const T &v);

  // A function f(x0,x1,...,xi,...,xn) with a value of v.  The 
  // total number of derivatives is ndiffs, the nth derivative is one, and all 
  // others are zero. 
  AutoDiff(const T &v, const uInt ndiffs, const uInt n); 

  // A function f(x0,x1,...,xi,...,xn) with a value of v.  The 
  // total number of derivatives is ndiffs.
  // All derivatives are zero. 
  AutoDiff(const T &v, const uInt ndiffs); 

  // Construct one from another
  AutoDiff(const AutoDiff<T> &other);

  // Construct a function f(x0,x1,...,xn) of a value v and a vector of 
  // derivatives derivs(0) = df/dx0, derivs(1) = df/dx1, ...
  AutoDiff(const T &v, const Vector<T> &derivs);

  ~AutoDiff();

  // Assignment operator.  Assign a constant to variable.  All derivatives
  // are zero.
  AutoDiff<T> &operator=(const T &v);

  // Assign one to another.
  AutoDiff<T> &operator=(const AutoDiff<T> &other);

  // Math operations
  // <group>
  void operator*=(const AutoDiff<T> &other);
  void operator/=(const AutoDiff<T> &other);
  void operator+=(const AutoDiff<T> &other);
  void operator-=(const AutoDiff<T> &other);
  void operator*=(const T other);
  void operator/=(const T other);
  void operator+=(const T other);
  void operator-=(const T other);
  // </group>

  // Returns the pointer to the area
  // <group>
  AutoDiffRep<T> *theRep() { return rep_p; };
  const AutoDiffRep<T> *theRep() const { return rep_p; };
  // </group>

  // Returns the value of the function
  // <group>
  T &value() { return rep_p->val_p; };
  const T &value() const { return rep_p->val_p; };
  // </group>
  
  // Returns a vector of the derivatives of an AutoDiff
  // <group>
  Vector<T> derivatives() const;
  void derivatives(Vector<T> &res) const;
  // </group>
  
  // Returns a specific derivative. The second set does not check for
  // nDerivatives() > 0.
  // <group>
  T &derivative(uInt which) { return rep_p->derivative(which); };
  const T &derivative(uInt which) const { return rep_p->derivative(which); };
  T &deriv(uInt which) { return rep_p->grad_p[which]; };
  const T &deriv(uInt which) const { return rep_p->grad_p[which]; };
  // </group>
  
  // Return total number of derivatives
  uInt nDerivatives() const { return rep_p->nd_p; };
  
  // Is it a constant, i.e., with zero derivatives?
  Bool isConstant() const { return rep_p->nd_p == 0; };

  // Indicate that we are going to use a temporary value for the last time
  // (e.g. at the of a function returning by value). This way superfluous
  // copying can be circumvented.
  const AutoDiff<T> &ref() { rep_p->nocopy_p = True; return *this; };
  
 private:
  //# Data
  // Pool of data blocks
  static ObjectPool<AutoDiffRep<T>, uInt> pool;
  // Value representation
  AutoDiffRep<T> *rep_p;

  //# Methods
  // Release a block of representation when necessary
  void release() {
    if (!rep_p->nocopy_p) pool.release(rep_p, rep_p->nd_p);
    else rep_p->nocopy_p = False;
  };

};

#endif
