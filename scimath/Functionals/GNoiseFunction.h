//# GNoiseFunction.h: A one dimensional normal distribution 
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

#ifndef SCIMATH_GNOISEFUNCTION_H
#define SCIMATH_GNOISEFUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/GNoiseParam.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional normal distribution 
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="GNoiseParam">GNoiseParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// Gaussian Noise generator.
// </etymology>

// <synopsis> 
// A <src>GNoise</src> is described by a mean and a variance (Note these are
// not parameters in the <src>Function</src> sense, but more like the
// order of a polynomial. The defaults are 0 and 1.
// </synopsis> 
//
// <example>
// <srcblock>
//    GNoiseFunction<Double> sf;
//    sf();            // = 0.12
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
// </thrown>
//

template<class T> class GNoiseFunction : public GNoiseParam<T>
{
public:
  //# Constructors
  // Constructs the GNoise, Defaults:
  // mean=0, var=1.0
  // <group>
  GNoiseFunction() : GNoiseParam<T>() {}
  GNoiseFunction(const Double &mean, const Double &var) :
    GNoiseParam<T>(mean, var) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  GNoiseFunction(const GNoiseFunction<T> &other) : GNoiseParam<T>(other) {}
  template <class W>
    GNoiseFunction(const GNoiseFunction<W> &other) : GNoiseParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  GNoiseFunction<T> &operator=(const GNoiseFunction<T> &other) {
    GNoiseParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~GNoiseFunction() {}

  //# Operators    
  // Evaluate the GNoise at <src>x</src>.
  // If a vector is used as the argument only its first element is used.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer. 
  // <group>
  virtual Function<T> *clone() const { return new GNoiseFunction<T>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using GNoiseParam<T>::param_p;
public:
  using GNoiseParam<T>::noise_p;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/GNoiseFunction.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
