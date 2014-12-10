//# AutoDiff.h: An automatic differentiating class for functions
//# Copyright (C) 1995,1998,1999,2001,2002
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

#ifndef SCIMATH_AUTODIFF_H
#define SCIMATH_AUTODIFF_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/AutoDiffRep.h>
#include <casacore/casa/Containers/ObjectPool.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template <class T> class Vector;
template <class T> class AutoDiff;

// <summary>
// Class that computes partial derivatives by automatic differentiation.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tAutoDiff.cc" demos="dAutoDiff.cc">
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
//
// Suppose we have a function f(x0,x1,...,xn) and its differential is
// <srcblock> 
// df = (df/dx0)*dx0 + (df/dx1)*dx1 + ... + (df/dxn)*dxn
// </srcblock> 
// We can build a class that has the value of the function, 
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
// mathematical functions.
//
// To be able to use the class as an automatic differentiator of a function
// object, we need a templated function object, i.e. an object with:
// <ul>
// <li> a <src> template <class T> T operator()(const T)</src>
// <li> or multiple variable input like: 
//		<src> template <class T> T operator()(const Vector<T> &)</src>
// <li> all variables and constants used in the calculation of the function
//	value should have been typed with T
// </ul>
// A simple example of such a function object could be:
// <srcblock>
//	template <class T> f {
//	public:
//	  T operator()(const T &x, const T &a, const T &b) {
//	  	return a*b*x; }
// 	};
//      // Instantiate the following versions:
//	template class f<Double>;
//	template class f<AutoDiff<Double> >;
// </srcblock>
// A call with values will produce the function value:
// <srcblock>
//	cout << f(7.0, 2.0, 3.0) << endl;
//	// will produce the value at x=7 for a=2; b=3:
//	42
//	// But a call indicating that we want derivatives to a and b:
//	cout << f(AutoDiff<Double>(7.0), AutoDiff<Double>(2.0, 2, 0),
//		  AutoDiff<Double>(3.0, 2, 1)) << endl;
//	// will produce the value at x=7 for a=2; b=3:
//	// and the partial derivatives wrt a and b at x=7:
//	(42, [21, 14])
//	// The following will calculate the derivate wrt x:
//	cout << f(AutoDiff<Double>(7.0, 1, 0), AutoDiff<Double>(2.0),
//		  AutoDiff<Double>(3.0)) << endl;
//	(42,[6])
// </srcblock>
// In actual practice, there are a few rules to obey for the structure of
// the function object if you want to use the function object and its
// derivatives in least squares fitting procedures in the Fitting
// module. The major one is to view the function object having 'fixed' and
// 'variable' parameters. I.e., rather than viewing the function as
// depending on parameters <em>a, b, x</em> (<src>f(a,b,x)</src>), the 
// function is considered to be <src>f(x; a,b)</src>, where <em>a, b</em> 
// are 'fixed' parameters, and <em>x</em> a variable parameter.
// Fixed parameters should be contained in a 
// <linkto class=FunctionParam>FunctionParam</linkto> container object;
// while the variable parameter(s) are given in the function
// <src>operator()</src>. See <linkto class=Function>Function</linkto> class
// for details.
//
// A Gaussian spectral profile would in general have the center frequency,
// the width and the amplitude as fixed parameters, and the frequency as
// a variable. Given a spectrum, you would solve for the fixed parameters,
// given spectrum values. However, in other cases the role of the
// parameters could be reversed. An example could be a whole stack of
// observed (in the laboratory) spectra at different temperatures at
// one frequency. In that case the width would be the variable parameter,
// and the frequency one of the fixed (and to be solved for)parameters. 
//
// Since the calculation of the derivatives is done with simple overloading, 
// the calculation of second (and higher) derivatives is easy. It should be
// noted that higher deivatives are inefficient in the current incarnation
// (there is no knowledge e.g. about symmetry in the Jacobian). However,
// it is a very good way to get the correct answers of the derivatives. In
// practice actual production code will be better off with specialization
// of the <src>f<AutoDiff<> ></src> implementation.
//
// The <src>AutoDiff</src> class is the class the user communicates with.
// Alias classes (<linkto class=AutoDiffA>AutoDiffA</linkto> and
// <linkto class=AutoDiffA>AutoDiffX</linkto>) exists
// to make it possible to have different incarnations of a templated
// method (e.g. a generic one and a specialized one). See the
// <src>dAutoDiff</src> demo for an example of its use.
//
// All operators and functions are declared in <linkto file=AutoDiffMath.h> 
// AutoDiffMath</linkto>. The output operator in 
// <linkto file=AutoDiffIO.h>AutoDiffIO</linkto>. The actual structure of the
// data block used by <src>AutoDiff</src> is described in 
// <linkto class=AutoDiffRep>AutoDiffRep</linkto>.
// </synopsis>
//
// <example>
// <srcblock>
// // First a simple example.
// // We have a function of the form f(x,y,z); and want to know the
// // value of the function for x=10; y=20; z=30; and for
// // the derivatives at those point.
// // Specify the values; and indicate 3 derivatives:
// 	AutoDiff<Double> x(10.0, 3, 0); 
// 	AutoDiff<Double> y(20.0, 3, 1); 
// 	AutoDiff<Double> z(30.0, 3, 2);
// // The result will be:
// 	AutoDiff<Double> result = x*y + sin(z);
// 	cout << result.value() << endl;
// // 199.012
// 	cout << result.derivatives() << endl;
// // [20, 10, 0.154251]
// // Note: sin(30) = -0.988; cos(30) =  0.154251;
// </srcblock>
//
// See for an extensive example the demo program dAutoDiff. It is
// based on the example given above, and shows also the use of second
// derivatives (which is just using <src>AutoDiff<AutoDiff<Double> ></src>
// as template argument). 
// <srcblock>
// 	// The function, with fixed parameters a,b:
//	template <class T> class f {
//	public:
//	  T operator()(const T& x) { return a_p*a_p*a_p*b_p*b_p*x; }
//	  void set(const T& a, const T& b) { a_p = a; b_p = b; }
//	private:
//	  T a_p;
//	  T b_p;
//	};
//	// Call it with different template arguments:
//	  Double a0(2), b0(3), x0(7);
//	  f<Double> f0; f0.set(a0, b0);
//	  cout << "Value:     " << f0(x0) << endl;
//	
//	  AutoDiff<Double> a1(2,2,0), b1(3,2,1), x1(7);
//	  f<AutoDiff<Double> > f1; f1.set(a1, b1);
//	  cout << "Diff a,b:   " << f1(x1) << endl;
//	
//	  AutoDiff<Double> a2(2), b2(3), x2(7,1,0);
//	  f<AutoDiff<Double> > f2; f2.set(a2, b2);
//	  cout << "Diff x:     " << f2(x2) << endl;
//	
//	  AutoDiff<AutoDiff<Double> > a3(AutoDiff<Double>(2,2,0),2,0),
//	    b3(AutoDiff<Double>(3,2,1),2,1), x3(AutoDiff<Double>(7),2);
//	  f<AutoDiff<AutoDiff<Double> > > f3; f3.set(a3, b3);
//	  cout << "Diff2 a,b:  " << f3(x3) << endl;
//	
//	  AutoDiff<AutoDiff<Double> > a4(AutoDiff<Double>(2),1),
//	    b4(AutoDiff<Double>(3),1),
//	    x4(AutoDiff<Double>(7,1,0),1,0);
//	  f<AutoDiff<AutoDiff<Double> > > f4; f4.set(a4, b4);
//	  cout << "Diff2 x:    " << f4(x4) << endl;
//
//    // Result will be:
//    // Value:     504
//    // Diff a,b:  (504, [756, 336])
//    // Diff x:    (504, [72])
//    // Diff2 a,b: ((504, [756, 336]), [(756, [756, 504]), (336, [504, 112])])
//    // Diff2 x:   ((504, [72]), [(72, [0])])
//
//    // It needed the template instantiations definitions:
//	template class f<Double>;
//	template class f<AutoDiff<Double> >;
//	template class f<AutoDiff<AutoDiff<Double> > >;
// </srcblock>
// </example>
//
// <motivation>
// The creation of the class was motivated by least-squares non-linear fit where
// partial derivatives of a fitted function are needed. It would be tedious
// to create functionals for all partial derivatives of a function.
// </motivation>
//
// <templating arg=T>
//  <li> any class that has the standard mathematical and comparisons
//	defined
// </templating>
//
// <todo asof="2001/06/07">
// <li> Nothing I know
// </todo>

template <class T> class AutoDiff {
 public:
  //# Typedefs
  typedef T 			value_type;
  typedef value_type&		reference;
  typedef const value_type&	const_reference;
  typedef value_type*		iterator;
  typedef const value_type*	const_iterator;

  //# Constructors
  // Construct a constant with a value of zero.  Zero derivatives.
  AutoDiff();

  // Construct a constant with a value of v.  Zero derivatives.
  AutoDiff(const T &v);

  // A function f(x0,x1,...,xn,...) with a value of v.  The 
  // total number of derivatives is ndiffs, the nth derivative is one, and all 
  // others are zero. 
  AutoDiff(const T &v, const uInt ndiffs, const uInt n); 

  // A function f(x0,x1,...,xn,...) with a value of v.  The 
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

  // Assignment operators
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

  // Returns the pointer to the structure of value and derivatives.
  // <group>
  AutoDiffRep<T> *theRep() { return rep_p; }
  const AutoDiffRep<T> *theRep() const { return rep_p; }
  // </group>

  // Returns the value of the function
  // <group>
  T &value() { return rep_p->val_p; }
  const T &value() const { return rep_p->val_p; }
  // </group>
  
  // Returns a vector of the derivatives of an AutoDiff
  // <group>
  Vector<T> derivatives() const;
  void derivatives(Vector<T> &res) const;
  // </group>
  
  // Returns a specific derivative. The second set does not check for
  // a valid which; the first set does through Vector addressing.
  // <group>
  T &derivative(uInt which) { return rep_p->grad_p(which); }
  const T &derivative(uInt which) const { return rep_p->grad_p(which); }
  T &deriv(uInt which) { return rep_p->grad_p[which]; }
  const T &deriv(uInt which) const { return rep_p->grad_p[which]; }
  // </group>
  
  // Return total number of derivatives
  uInt nDerivatives() const { return rep_p->nd_p; }
  
  // Is it a constant, i.e., with zero derivatives?
  Bool isConstant() const { return rep_p->nd_p == 0; }

  // Indicate that we are going to use a temporary value for the last time
  // (e.g. at the of a function returning by value). This way superfluous
  // copying can be circumvented.
  const AutoDiff<T> &ref() { rep_p->nocopy_p = True; return *this; }
  
 private:
  //# Data
  // Pool of data blocks
  static ObjectPool<AutoDiffRep<T>, uInt> theirPool;
  // Mutex for thread-safe access to theirPool.
  static Mutex theirMutex;
  // Value representation
  AutoDiffRep<T> *rep_p;

  //# Methods
  // Release a struct of value and derivative data
  void release() {
    if (!rep_p->nocopy_p) {
      ScopedMutexLock locker(theirMutex);
      theirPool.release(rep_p, rep_p->nd_p);
    } else {
      rep_p->nocopy_p = False;
    }
  }

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/AutoDiff.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
