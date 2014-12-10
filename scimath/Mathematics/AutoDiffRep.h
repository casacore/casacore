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

#ifndef SCIMATH_AUTODIFFREP_H
#define SCIMATH_AUTODIFFREP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>
// Representation of an automatic differential class data
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tAutoDiff.cc" demos="">
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
// value, and the derivatives of the value with respect to the number of 
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
//  <li> any class that has the standard mathematical and comparison
//	operators defined
// </templating>
//
// <todo asof="20001/06/07">
// <li> Nothing I know off
// </todo>

template <class T> class AutoDiffRep {
 public:
  //# Typedefs
  typedef T 			value_type;
  typedef value_type&		reference;
  typedef const value_type&	const_reference;
  typedef value_type*		iterator;
  typedef const value_type*	const_iterator;

  //# Constructors
  // Construct a constant with a value of zero.  Zero derivatives.
  AutoDiffRep();

  // Construct a constant with a value of v.  Zero derivatives.
  explicit AutoDiffRep(const T &v);

  // Given a function f(x0,x1,...,xn,...). Construct with
  // a total number of derivatives ndiffs. The nth derivative is one, and all 
  // others are zero. The value v is the value of xn.
  AutoDiffRep(const T &v, const uInt ndiffs, const uInt n); 

  // Given a function f(x0,x1,...,xn,...). Construct with
  // a total number of derivatives ndiffs, and a value of xn.
  // All derivatives are zero. 
  AutoDiffRep(const T &v, const uInt ndiffs); 

  // Construct with ndiffs derivatives. All values and derivatives zero
  AutoDiffRep(const uInt ndiffs); 

  // Construct one from another (deep copy)
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

  // Assign one to another (deep copy).
  AutoDiffRep<T> &operator=(const AutoDiffRep<T> &other);

  //# Member functions

  //# Data
  // The function value
  T val_p;
  // The number of derivatives
  uInt nd_p;
  // A flag indicating that value will not be used anymore (to stop
  // superfluous copying)
  Bool nocopy_p;
  // The derivatives
  Vector<T> grad_p;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/AutoDiffRep.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
