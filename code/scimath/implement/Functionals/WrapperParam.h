//# NQWrapperParam.h: Parameter handling for wrapped function objects 
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

#if !defined(AIPS_NQWRAPPERPARAM_H)
#define AIPS_NQWRAPPERPARAM_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/Function.h>
#include <aips/Arrays/Vector.h>

//# Forward declarations

// <summary>  Parameter handling for wrapped function objects 
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> class
//   <li> <linkto class="NQFunctionWrapper">NQFunctionWrapper</linkto>
// </prerequisite>
//
// <synopsis>
// This class is provided to enable easy specialization for the actual
// <src>NQFunctionWrapper</src> class.
// </synopsis>

// <example>
// <srcblock>
// Float func(const Vector<Float>& x) {return x(0)*x(1);}        // x*y
// // Convert C++ functions to Function
// NQFunctionWrapper<Float> Func(func, 2);
// </srcblock>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

// <todo asof="2001/08/19">
//   <li> Nothing I know of
// </todo>


template <class T> class NQWrapperParam : public Function<T> {
 public:
  //# Constructors
  // Construct with the given parameters
  // <group>
  NQWrapperParam();
  explicit NQWrapperParam(const T &par);
  explicit NQWrapperParam(const Vector<T> &par);
  // </group>

  // Copy constructor (deep copy)
  NQWrapperParam(const NQWrapperParam<T> &other);

  // Copy assignment (deep copy)
  NQWrapperParam<T> &operator=(const NQWrapperParam<T> &other);

  // Destructor
  virtual ~NQWrapperParam();

  //# Operators    

  //# Member functions

 protected:

};

#endif
