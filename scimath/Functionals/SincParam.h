//# SincParam.h: A one dimensional sin(x)/x
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

#ifndef SCIMATH_SINCPARAM_H
#define SCIMATH_SINCPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional sin(x)/x
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>

// <etymology> 
// A 1-dimensional sinc function.
// </etymology>

// <synopsis> 
// A <src>Sinc</src> is described by a height, a center and a width
// (halfwidth). The value is:
// <srcblock>
//      let y = (x-center)/width
//      height                              (x == center)
//      height*sin(pi*y)/(pi*y)             (x |= center)
// </srcblock>
// The parameters are enumerated by HEIGHT, CENTER and WIDTH. They have
// default values of (1, 0, 1).
// </synopsis> 
//
// <example>
// <srcblock>
//    SincFunction<Double> sf(5.0, 25.0, 7);
//    sf(25);            // = 5.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

template<class T> class SincParam : public Function<T>
{
public:
  //# Enumerations
  // Parameter numbers
  enum { HEIGHT=0, CENTER, WIDTH };
  
  //# Constructors
  // Constructs the Sinc, Defaults:
  // height=1, center=0, width=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  SincParam();
  explicit SincParam(const T &height);
  SincParam(const T &height, const T &center);
  SincParam(const T &height, const T &center, const T &width);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  SincParam(const SincParam<T> &other);
  template <class W>
    SincParam(const SincParam<W> &other) :
    Function<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  SincParam<T> &operator=(const SincParam<T> &other);
    
  // Destructor
  virtual ~SincParam();

  //# Operators    
  virtual uInt ndim() const { return 1; }

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("sinc");
    return x; }


  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/SincParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
