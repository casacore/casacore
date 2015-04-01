//# KaiserBParam.h: A one dimensional Kaiser-Bessel function
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

#ifndef SCIMATH_KAISERBPARAM_H
#define SCIMATH_KAISERBPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional Kaiser-Bessel function
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
// A Kaiser-Bessel function
// </etymology>

// <synopsis> 
// A <src>Kaiser-Bessel</src> is described by a height, a center, a width
// (halfwidth) and a parameter.
// The parameters are enumerated by HEIGHT, CENTER, WIDTH, KBPAR. They have
// default values of (1, 0, 1, 2.5).
// </synopsis> 
//
// <example>
// <srcblock>
//    KaiserBFunction<Double> sf;
//    sf(0);            // = 1.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

template<class T> class KaiserBParam : public Function<T>
{
public:
  //# Enumerations
  // Parameter numbers
  enum { HEIGHT=0, CENTER, WIDTH, KBPAR };
  
  //# Constructors
  // Constructs the KaiserB, Defaults:
  // height=1, center=0, width=1, kbpar=2.5.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  KaiserBParam();
  explicit KaiserBParam(const T &kbpar);
  // </group>

  // Copy constructor (deep copy)
  KaiserBParam(const KaiserBParam<T> &other);

  // Copy assignment (deep copy)
  KaiserBParam<T> &operator=(const KaiserBParam<T> &other);
  template <class W>
    KaiserBParam(const KaiserBParam<W> &other) :
    Function<T>(other) {}
    
  // Destructor
  virtual ~KaiserBParam();

  //# Operators    
  virtual uInt ndim() const { return 1; }

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("kaiserbessel");
    return x; }


  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/KaiserBParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
