//# NQGaussian2D.cc: A two-dimensional Gaussian class
//# Copyright (C) 1994,1995,1996,1999,2001
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
#include <aips/Functionals/NQGaussian2D.h>
#include <aips/Mathematics/Math.h>

//# Constructors

//# Operators
template<class T>
T NQGaussian2D<T>::eval(Function<T>::FunctionArg x) const {
  T xnorm = x[0] - param_p[XCENTER];
  T ynorm = x[1] - param_p[YCENTER];
  if (theCpa != T(1.0)) {
    const T temp = xnorm;
    xnorm =   theCpa*temp  + theSpa*ynorm;
    ynorm = - theSpa*temp  + theCpa*ynorm;
  };
  xnorm = xnorm/theXwidth/fwhm2int;
  ynorm = ynorm/param_p[YWIDTH]/fwhm2int;
  return param_p[HEIGHT]*exp(-(xnorm*xnorm + ynorm*ynorm));
}

//# Member functions
