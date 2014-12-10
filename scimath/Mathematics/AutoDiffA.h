//# AutoDiffA.h: An automatic differentiating class for functions
//# Copyright (C) 2001,2002
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

#ifndef SCIMATH_AUTODIFFA_H
#define SCIMATH_AUTODIFFA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template <class T> class Vector;

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
// <li> <linkto class=AutoDiff>AutoDiff</linkto>
// </prerequisite>
//
// <etymology>
// Class that computes partial derivatives by automatic differentiation, thus
// AutoDiff.
// </etymology>
//
// <synopsis>
// AutoDiffA is an <linkto class=AutoDiff>AutoDiff</linkto>. It is used
// to be able to distinguish between two template incarnations; e.g. to
// have one or more specializations, in addition to the general template
// version.
// </synopsis>
//
// <example>
// See for an extensive example the demo program dAutoDiff. It is
// based on the example given in the <linkto class=AutoDiff>AutoDiff</linkto>
// class, and shows how to have both an automatic and a specific version
// of a function object.
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
//	// The specialized function
//	template <> class f<AutoDiffA<Double> > {
//	public:
//	  T operator()(const T& x) { return a_p*a_p*a_p*b_p*b_p*x; }
//	  void set(const T& a, const T& b) { a_p = a; b_p = b; }
//	private:
//	  T a_p;
//	  T b_p;
//	};
//	// Call it with different template arguments:
//	  AutoDiff<Double> a1(2,2,0), b1(3,2,1), x1(7);
//	  f<AutoDiff<Double> > f1; f1.set(a1, b1);
//	  cout << "Diff a,b:   " << f1(x1) << endl;
//	
//	  f<AutoDiffA<Double> > f12; f12.set(a1, b1);
//	  cout << "Same....:   " << f12(x1) << endl;
//
//    // Result will be:
//    // Diff a,b:  (504, [756, 336])
//    // Same....:  (504, [756, 336])
//
//    // It needed the template instantiations definitions:
//	template class f<AutoDiff<Double> >;
// </srcblock>
// </example>
//
// <motivation>
// The class was created to enable separate calculations of the same
// function.
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

template <class T> class AutoDiffA : public AutoDiff<T> {
 public:
  //# Constructors
  // Construct a constant with a value of zero.  Zero derivatives.
  AutoDiffA() : AutoDiff<T>() {}

  // Construct a constant with a value of v.  Zero derivatives.
  AutoDiffA(const T &v) : AutoDiff<T>(v) {}

  // A function f(x0,x1,...,xn,...) with a value of v.  The 
  // total number of derivatives is ndiffs, the nth derivative is one, and all 
  // others are zero. 
  AutoDiffA(const T &v, const uInt ndiffs, const uInt n) :
    AutoDiff<T>(v, ndiffs, n) {} 

  // A function f(x0,x1,...,xn,...) with a value of v.  The 
  // total number of derivatives is ndiffs.
  // All derivatives are zero. 
  AutoDiffA(const T &v, const uInt ndiffs) : AutoDiff<T>(v, ndiffs) {}

  // Construct one from another
  AutoDiffA(const AutoDiff<T> &other) : AutoDiff<T>(other) {}

  // Construct a function f(x0,x1,...,xn) of a value v and a vector of 
  // derivatives derivs(0) = df/dx0, derivs(1) = df/dx1, ...
  AutoDiffA(const T &v, const Vector<T> &derivs) : AutoDiff<T>(v, derivs) {}

  ~AutoDiffA() {}

  // Assignment operator.  Assign a constant to variable.  All derivatives
  // are zero.
  AutoDiffA<T> &operator=(const T &v) {
    AutoDiff<T>::operator=(v);
    return *this;
  }

  // Assign one to another.
  AutoDiffA<T> &operator=(const AutoDiff<T> &other) {
    AutoDiff<T>::operator=(other);
    return *this;
  }

 private:
  //# Data

};


} //# NAMESPACE CASACORE - END

#endif
