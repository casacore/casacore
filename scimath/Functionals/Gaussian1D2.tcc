//# Gaussian1D2.cc: One dimensional Gaussian class specialized for AutoDiff
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

#ifndef SCIMATH_GAUSSIAN1D2_TCC
#define SCIMATH_GAUSSIAN1D2_TCC

//# Includes
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T>
AutoDiff<T> Gaussian1D<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp;
  if (this->param_p[this->HEIGHT].nDerivatives() > 0) tmp = this->param_p[this->HEIGHT];
  else if (this->param_p[this->CENTER].nDerivatives() > 0) tmp = this->param_p[this->CENTER];
  else if (this->param_p[this->WIDTH].nDerivatives() > 0) tmp = this->param_p[this->WIDTH];
  T x_norm = (x[0] - this->param_p[this->CENTER].value())/
    this->param_p[this->WIDTH].value()/this->fwhm2int.value();
  T exponential = exp(-(x_norm*x_norm));
  // function value
  tmp.value() = this->param_p[this->HEIGHT].value() * exponential;
  // get derivatives (assuming either all or none)
  if (tmp.nDerivatives()>0) {
    for (uInt j=0; j<tmp.nDerivatives(); j++) tmp.deriv(j) = 0.0;
    // derivative wrt height
    T dev = exponential;
    if (this->param_p.mask(this->HEIGHT)) tmp.deriv(this->HEIGHT) = dev;
    // derivative wrt center
    dev *= this->param_p[this->HEIGHT].value()*x_norm*T(2.0)/this->param_p[this->WIDTH].value()/
      this->fwhm2int.value();
    if (this->param_p.mask(this->CENTER)) tmp.deriv(this->CENTER) = dev;
    // derivative wrt width
    if (this->param_p.mask(this->WIDTH)) tmp.deriv(this->WIDTH) = dev*
			       x_norm*this->fwhm2int.value();
  }
  return tmp;
}

//# Member functions

} //# NAMESPACE CASACORE - END


#endif
