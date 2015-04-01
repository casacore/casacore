//# WrapperParam.h: Parameter handling for wrapped function objects 
//# Copyright (C) 2001,2002,2005
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

#ifndef SCIMATH_WRAPPERPARAM_H
#define SCIMATH_WRAPPERPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>  Parameter handling for wrapped function objects 
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> class
//   <li> <linkto class="FunctionWrapper">FunctionWrapper</linkto>
// </prerequisite>
//
// <synopsis>
// This class is provided to enable easy specialization for the actual
// <src>FunctionWrapper</src> class.
// </synopsis>

// <example>
// <srcblock>
// Float func(const Vector<Float>& x) {return x(0)*x(1);}        // x*y
// // Convert C++ functions to Function
// FunctionWrapper<Float> Func(func, 2);
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


template <class T> class WrapperParam : public Function<T>
{
public:
  //# Constructors
  // Construct with the given parameters
  // <group>
  WrapperParam();
  explicit WrapperParam(const T &par);
  explicit WrapperParam(const Vector<T> &par);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  WrapperParam(const WrapperParam<T> &other);
  // </group>
  // Copy assignment (deep copy)
  WrapperParam<T> &operator=(const WrapperParam<T> &other);

  // Destructor
  virtual ~WrapperParam();

  //# Operators    

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("wrapper");
    return x; }

protected:
  //# Make members of parent classes known.
  using Function<T>::param_p;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/WrapperParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
