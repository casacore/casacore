//# GNoiseParam.h: A one dimensional normal distribution 
//# Copyright (C) 2002
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

#ifndef SCIMATH_GNOISEPARAM_H
#define SCIMATH_GNOISEPARAM_H

//# Includes
#include <casa/aips.h>
#include <scimath/Functionals/Function.h>
#include <casa/BasicMath/Random.h>

//# Forward declarations

// <summary> A one dimensional normal distribution 
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

template<class T> class GNoiseParam : public Function<T> {
 public:
  //# Enumerations
  
  //# Constructors
  // Constructs the GNoise, Defaults:
  // mean=0, var=1.0
  // <group>
  GNoiseParam();
  GNoiseParam(const Double &mean, const Double &var);
  // </group>

  // Copy constructor (deep copy)
  GNoiseParam(const GNoiseParam<T> &other);

  // Copy assignment (deep copy)
  GNoiseParam<T> &operator=(const GNoiseParam<T> &other);
    
  // Destructor
  virtual ~GNoiseParam();

  //# Operators    
  virtual uInt ndim() const { return 0; };

  //# Member functions

 protected:
  //# Data
  // Random generator
  ACG genit_p;
  // Normal noise
  mutable Normal noise_p;

};

#endif
