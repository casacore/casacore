//# WrapperBase.h: Aid in constructing function objects from C++ functions 
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
//# $Id$

#ifndef SCIMATH_WRAPPERBASE_H
#define SCIMATH_WRAPPERBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> Aid in constructing function objects from C++ functions 
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="FunctionWrapper">FunctionWrapper</linkto> class
//   <li> <linkto class="WrapperData">WrapperData</linkto> class
// </prerequisite>
//
// <synopsis>
// This base class is provided to enable compile time selection of the
// appropriate function call through <src>WrapperData</src>.
// </synopsis>
//
// <example>
// <srcblock>
// Float func(const Vector<Float>& x) { return x(0)*x(1); }        // x*y
// // Convert C++ functions to Functionals
// FunctionWrapper<Float> Func(func, 2);
// </srcblock>
//

template<class T> class WrapperBase {
 public:
  //# Constructors
  // Default constructor: zero dimension
  WrapperBase() : ndim_p(0), arg_p(0) {}
  // Standard constructor
  explicit WrapperBase(const uInt dim) : ndim_p(dim), arg_p(dim) {}

  // Destructor
  virtual ~WrapperBase() {}

  //# Operators    
  // Evaluate the function at <src>x</src>.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const = 0;
  // </group>

  //# Member functions
  // Get the dimensionality
  virtual uInt ndim() const { return ndim_p; }

 protected:
  //# Data
  // Dimensionality
  uInt ndim_p;
  // Vector argument interface
  mutable Vector<T> arg_p;

 private:
  // Copy constructor and assignment (not implemented)
  // <group>
  WrapperBase(const WrapperBase<T> &other);
  WrapperBase<T> &operator=(const WrapperBase<T> &other);
  // </group>

};


} //# NAMESPACE CASACORE - END

#endif
