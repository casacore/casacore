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

#ifndef SCIMATH_POISSONPARAM_H
#define SCIMATH_POISSONPARAM_H

//# Includes
#include <casa/aips.h>
#include <scimath/Functionals/Function.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward declarations

// <summary> A one dimensional Poisson function
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
// A 1-dimensional Poisson.
// </etymology>

// <synopsis> 
// A <src>Poisson</src> is described by lambda
// The value is:
// <srcblock>
//      height          (|x-center| == 0.0)
//      0               (|x-center| != 0.0)
// </srcblock>
// The parameters are enumerated by LAMDA. They have
// default values of 1.
// </synopsis> 
//
// <example>
// <srcblock>
//    PoissonFunction<Double> sf(5.0);
//    sf(25);            // = 5.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

template<class T> class PoissonParam : public Function<T>
{
public:
  //# Enumerations
  // Parameter numbers
  enum { LAMBDA=0, HEIGHT};
  
  //# Constructors
  // Constructs the Poisson, Defaults:
  // lambda=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  PoissonParam();
  explicit PoissonParam(const T &lambda);
  PoissonParam( const T &lambda, const T &height );
  // </group>
  
  // Copy constructor (deep copy)
  // <group>
  PoissonParam(const PoissonParam<T> &other);
  template <class W>
    PoissonParam(const PoissonParam<W> &other) :
    Function<T>(other) {}
  // </group>
  // Copy assignment (deep copy)
  PoissonParam<T> &operator=(const PoissonParam<T> &other);
    
  // Destructor
  virtual ~PoissonParam();

  //# Operators    
  virtual uInt ndim() const { return 1; }

  //# Member functions
  // Give name of function
  virtual const String &name() const {
	  static String x("poisson");
	  return x;
  }

  // Get or set lambda

   T lambda() const {
	   return param_p[LAMBDA];
   }
   void setLambda(const T &lambda) {
	   param_p[LAMBDA] = lambda;
   }

   T height() const {
   	   return param_p[HEIGHT];
      }
      void setHeight(const T &height) {
   	   param_p[HEIGHT] = height;
      }
  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
};





} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Functionals/PoissonParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
