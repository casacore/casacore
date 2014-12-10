//# UnaryParam.h: Parameter handling for one dimensional unary function
//# Copyright (C) 2002,2005
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

#ifndef SCIMATH_UNARYPARAM_H
#define SCIMATH_UNARYPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> Parameter handling for one dimensional unary function
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>

// <etymology> 
// A 1-dimensional unary hat.
// </etymology>

// <synopsis> 
// A <src>Unary</src> is described by a height, a center and a width
// (halfwidth). The value is:
// <srcblock>
//      height          (|x-center| <  width)
//   0.5height          (|x-center| == width)
//      0               (|x-center| >  width)
// </srcblock>
// The parameters are enumerated by HEIGHT, CENTER and WIDTH. They have
// default values of (1, 0, 1).
// </synopsis> 
//
// <example>
// <srcblock>
//    UnaryFunction<Double> sf(5.0, 25.0, 7);
//    sf(25);            // = 5.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

template<class T> class UnaryParam : public Function<T>
{
public:
  //# Enumerations
  // Parameter numbers
  enum { HEIGHT=0, CENTER, WIDTH };
  
  //# Constructors
  // Constructs the Unary, Defaults:
  // height=1, center=0, width=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  UnaryParam();
  explicit UnaryParam(const T &height);
  UnaryParam(const T &height, const T &center);
  UnaryParam(const T &height, const T &center, const T &width);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  UnaryParam(const UnaryParam<T> &other);
  template <class W>
    UnaryParam(const UnaryParam<W> &other) :
    Function<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  UnaryParam<T> &operator=(const UnaryParam<T> &other);
    
  // Destructor
  virtual ~UnaryParam();

  //# Operators    
  virtual uInt ndim() const { return 1; }

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("unary");
    return x; }

  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/UnaryParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
