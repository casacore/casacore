//# AutoDiffRep.h: Representation of an automatic differential class data
//# Copyright (C) 2001
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

#if !defined(AIPS_AUTODIFFREP_H)
#define AIPS_AUTODIFFREP_H

//# Includes
#include <aips/aips.h>

//# Forward declarations
template <class T> class Vector;

// <summary>
// Representation of an automatic differential class data
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tAutoDiff.cc" demos="">
// </reviewed>
//
// <prerequisite>
// <li> <linkto class=AutoDiff>AutoDiff</linkto>
// </prerequisite>
//
// <etymology>
// Class that represents partial derivatives obtained by automatic
// differentiation.
// </etymology>
//
// <synopsis>
// Structure (only a class since cxx2html cannot handle struct) representing
// the data necessary for automatic differentiation. The structure contains a
// value, and the derivatives of the value with respect to the number
// dependend variables.
//
// The actual differentiation and access is done through the 
// <linkto class=AutoDiff>AutoDiff</linkto> class.
//
// <example>
// See the example in <linkto class=AutoDiff>AutoDiff</linkto>
// </example>
//
// <motivation>
// To separate the data container from the actual calculations.
// To be able to create special conatiners; constructors and destructors
// (including memory allocation) to speed up processes.
//
// <templating arg=T>
//  <li> any class that has the standard mathematical and comparisons
//	defined
// </templating>
//
// <todo asof="20001/06/07">
// <li> Nothing I know off
// </todo>

template <class T> class AutoDiffRep {
  //# Friends

 public:
  //# Constructors
  // Construct a constant of a value of zero.  Zero derivative.
  AutoDiffRep();

  // Construct a constant of a value of v.  Zero derivative.
  explicit AutoDiffRep(const T &v);

  // Given a function f(x0,x1,...,xi,...,xn) with a value of v.  Construct with
  // a total number of derivatives ndiffs. The nth derivative is one, and all 
  // others are zero. 
  AutoDiffRep(const T &v, const uInt ndiffs, const uInt n); 

  // Given a function f(x0,x1,...,xi,...,xn) with a value of v.  Construct with
  // a total number of derivatives ndiffs. 
  // All derivatives are zero. 
  AutoDiffRep(const T &v, const uInt ndiffs); 

  // Construct with ndiffs derivatives. All values zero
  AutoDiffRep(const uInt ndiffs); 

  // Construct one from another
  AutoDiffRep(const AutoDiffRep<T> &other);

  // Construct a function f(x0,x1,...,xn) of a value v and a vector of 
  // derivatives derivs(0) = df/dx0, derivs(1) = df/dx1, ...
  AutoDiffRep(const T &v, const Vector<T> &derivs);

  // Destructor
  ~AutoDiffRep();

  //# Operators
  // Assign a constant to variable.  All derivatives
  // are zero.
  AutoDiffRep<T> &operator=(const T &v);

  // Assign one to another.
  AutoDiffRep<T> &operator=(const AutoDiffRep<T> &other);

  //# Member functions
  // Returns the value of the represented function
  // <group>
  T &value() { return val_p; };
  const T &value() const { return val_p; };
  // </group>

  // Returns a vector with the partial derivatives of the represented
  // function
  // <group>
  void derivatives(Vector<T> &res) const;
  // </group>

  // Returns a specific derivative
  // <group>
  const T &derivative(uInt which) const {
    return (grad_p ? grad_p[which] : aZero); };
  T &derivative(uInt which) {
    return (grad_p ? grad_p[which] : aZero); };
  // </group>

  // Return total number of derivatives
  uInt nDerivatives() const { return nd_p; };

  // Is it a constant, i.e., with zero derivatives?
  Bool isConstant() const { return nd_p == 0; };

  //# Data
  // A zero value
  static T aZero;
  // The function value
  T val_p;
  // The number of derivatives
  uInt nd_p;
  // A flag indicating that value will not be used anymore (to stop
  // superfluous copying)
  Bool nocopy_p;
  // The derivatives
  T *grad_p;
};

#endif
