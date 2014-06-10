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

//# Includes
#include <scimath/Functionals/PoissonFunction.h>
#include <casa/BasicMath/Math.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Constructors

//# Operators
template<class T>
AutoDiff<T> PoissonFunction<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp;
  if (param_p[LAMBDA].nDerivatives() > 0){
	 tmp = param_p[LAMBDA];
  }
  else if (param_p[HEIGHT].nDerivatives() > 0 ){
	  tmp = param_p[HEIGHT];
  }

  // function value
  int xVal = static_cast<int>(x[0]);
  double lambdaVal = param_p[LAMBDA].value();
  double heightVal = param_p[HEIGHT].value();
  tmp.value() = heightVal * ( pow( lambdaVal, xVal ) *
		  exp(-1 * lambdaVal ) / Combinatorics::factorial( xVal ));
  // get derivatives (assuming either all or none)
  if (tmp.nDerivatives()>0) {
    for (uInt j=0; j<tmp.nDerivatives(); j++){
    	tmp.deriv(j) = 0.0;
    }
    if (param_p.mask(LAMBDA)){
    	tmp.deriv(LAMBDA) = heightVal * ( xVal * pow(lambdaVal, xVal - 1) - pow(lambdaVal,xVal))*exp(-1*lambdaVal) /
    			Combinatorics::factorial( xVal);
    }
    if ( param_p.mask(HEIGHT)){
    	tmp.deriv(HEIGHT) = pow( lambdaVal, xVal )* exp(-1*lambdaVal) /
    			Combinatorics::factorial( xVal );
    }
  }

  return tmp;
}



//# Member functions

} //# NAMESPACE CASA - END

