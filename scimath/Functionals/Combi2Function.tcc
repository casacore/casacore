//# Combi2Function.cc:  Combination of Functions AutoDiff specialization
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

#ifndef SCIMATH_COMBI2FUNCTION_TCC
#define SCIMATH_COMBI2FUNCTION_TCC

//# Includes
#include <casacore/scimath/Functionals/CombiFunction.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T>
AutoDiff<T> CombiFunction<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp(0);
  for (uInt i=0; i<this->nparameters(); ++i) {
    if (this->param_p[i].nDerivatives() > 0) {
      tmp = this->param_p[i];
      break;
    }
  }
  for (uInt j=0; j<tmp.nDerivatives(); j++) tmp.deriv(j) = 0.0;
  tmp.value() = 0.0;
  // function value
  for (uInt i = 0; i< this->nparameters(); ++i) {
    T v = (this->function(i))(x).value();
    tmp.value() += this->param_p[i].value()*v;
    // get derivatives (assuming either all or none)
    if (tmp.nDerivatives()>0 && this->param_p.mask(i)) tmp.deriv(i) = v;
  }
  return tmp;
}

//# Member functions

} //# NAMESPACE CASACORE - END


#endif
