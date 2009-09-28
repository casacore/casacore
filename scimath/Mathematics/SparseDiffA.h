//# SparseDiff!A.h: An automatic differentiating class for functions
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
//# $Id: SparseDiffA.h,v 1.1 2007/11/16 04:34:46 wbrouw Exp $

#ifndef SCIMATH_SPARSEDIFFA_H
#define SCIMATH_SPARSEDIFFA_H

//# Includes
#include <casa/aips.h>
#include <scimath/Mathematics/SparseDiff.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward declarations
  template <class T> class Vector;

  // <summary>
  // Class that computes partial derivatives by automatic differentiation.
  // </summary>
  //
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tSparseDiff.cc" demos="dSparseDiff.cc">
  // </reviewed>
  //
  // <prerequisite>
  // <li> <linkto class=SparseDiff>SparseDiff</linkto>
  // </prerequisite>
  //
  // <etymology>
  // Class that computes partial derivatives by automatic differentiation, thus
  // SparseDiff.
  // </etymology>
  //
  // <synopsis>
  // SparseDiffA is an <linkto class=SparseDiff>SparseDiff</linkto>. It is used
  // to be able to distinguish between two template incarnations; e.g. to
  // have one or more specializations, in addition to the general template
  // version.
  // </synopsis>
  //
  // <example>
  // See for an extensive example the demo program dSparseDiff. It is
  // based on the example given in the <linkto class=SparseDiff>SparseDiff</linkto>
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
  //	template <> class f<SparseDiffA<Double> > {
  //	public:
  //	  T operator()(const T& x) { return a_p*a_p*a_p*b_p*b_p*x; }
  //	  void set(const T& a, const T& b) { a_p = a; b_p = b; }
  //	private:
  //	  T a_p;
  //	  T b_p;
  //	};
  //	// Call it with different template arguments:
  //	  SparseDiff<Double> a1(2,0), b1(3,1), x1(7);
  //	  f<SparseDiff<Double> > f1; f1.set(a1, b1);
  //	  cout << "Diff a,b:   " << f1(x1) << endl;
  //	
  //	  f<SparseDiffA<Double> > f12; f12.set(a1, b1);
  //	  cout << "Same....:   " << f12(x1) << endl;
  //
  //    // Result will be:
  //    // Diff a,b:  (504, [756, 336])
  //    // Same....:  (504, [756, 336])
  //
  //    // It needed the template instantiations definitions:
  //	template class f<SparseDiff<Double> >;
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

  template <class T> class SparseDiffA : public SparseDiff<T> {
  public:
    //# Constructors
    // Construct a constant with a value of zero.  Zero derivatives.
    SparseDiffA() : SparseDiff<T>() {};

    // Construct a constant with a value of v.  Zero derivatives.
    SparseDiffA(const T &v) : SparseDiff<T>(v) {};

    // A function f(x0,x1,...,xn,...) with a value of v.
    // The nth derivative is one, and all others are zero. 
    SparseDiffA(const T &v, const uInt n) :
      SparseDiff<T>(v, n) {}; 

    // A function f(x0,x1,...,xn,...) with a value of v.  The 
    // nth derivative is der, and all other derivatives are zero. 
    SparseDiffA(const T &v, const uInt n, const T &der) :
      SparseDiff<T>(v, n, der) {}; 

    // Construct one from another
    SparseDiffA(const SparseDiff<T> &other) : SparseDiff<T>(other) {};

    ~SparseDiffA() {};

    // Assignment operator.  Assign a constant to variable.  All derivatives
    // are zero.
    SparseDiffA<T> &operator=(const T &v) {
      SparseDiff<T>::operator=(v);
      return *this;
    };

    // Assignment operator.  Add a gradient to variable.
    SparseDiffA<T> &operator=(const pair<uInt, T> &der) {
      SparseDiff<T>::operator=(der);
      return *this;
    };

    // Assignment operator.  Assign gradients to variable.
    SparseDiffA<T> &operator=(const vector<pair<uInt, T> > &der) {
      SparseDiff<T>::operator=(der);
      return *this;
    };

    // Assign one to another (deep copy).
    SparseDiffA<T> &operator=(const SparseDiff<T> &other) {
      SparseDiff<T>::operator=(other);
      return *this;
    };

  private:
    //# Data

  };


} //# NAMESPACE CASA - END

#endif
