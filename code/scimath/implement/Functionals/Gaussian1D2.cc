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

//# Includes
#include <aips/Functionals/Gaussian1D.h>
#include <aips/BasicMath/Math.h>

//# Constructors

//# Operators
template<class T>
AutoDiff<T> Gaussian1D<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp;
  if (param_p[HEIGHT].nDerivatives() > 0) tmp = param_p[HEIGHT];
  else if (param_p[CENTER].nDerivatives() > 0) tmp = param_p[CENTER];
  else if (param_p[WIDTH].nDerivatives() > 0) tmp = param_p[WIDTH];
  T x_norm = (x[0] - param_p[CENTER].value())/
    param_p[WIDTH].value()/fwhm2int.value();
  T exponential = exp(-(x_norm*x_norm));
  // function value
  tmp.value() = param_p[HEIGHT].value() * exponential;
  // get derivatives (assuming either all or none)
  if (tmp.nDerivatives()>0) {
    for (uInt j=0; j<tmp.nDerivatives(); j++) tmp.deriv(j) = 0.0;
    // derivative wrt height
    T dev = exponential;
    if (param_p.mask(HEIGHT)) tmp.deriv(HEIGHT) = dev;
    // derivative wrt center
    dev *= param_p[HEIGHT].value()*x_norm*T(2.0)/param_p[WIDTH].value()/
      fwhm2int.value();
    if (param_p.mask(CENTER)) tmp.deriv(CENTER) = dev;
    // derivative wrt width
    if (param_p.mask(WIDTH)) tmp.deriv(WIDTH) = dev*
			       x_norm*fwhm2int.value();
  };
  return tmp;
}

//# Member functions
