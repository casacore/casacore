//# Gaussian2D2.cc: Two dimensional Gaussian class specialized for AutoDiff
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
#include <aips/Functionals/Gaussian2D.h>
#include <aips/BasicMath/Math.h>

//# Constructors

//# Operators
template<class T>
AutoDiff<T> Gaussian2D<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp;
  if (param_p[HEIGHT].nDerivatives() > 0) tmp = param_p[HEIGHT];
  else if (param_p[XCENTER].nDerivatives() > 0) tmp = param_p[XCENTER];
  else if (param_p[YCENTER].nDerivatives() > 0) tmp = param_p[YCENTER];
  else if (param_p[YWIDTH].nDerivatives() > 0) tmp = param_p[YWIDTH];
  else if (param_p[RATIO].nDerivatives() > 0) tmp = param_p[RATIO];
  else if (param_p[PANGLE].nDerivatives() > 0) tmp = param_p[PANGLE];

  T x2mean = x[0] - param_p[XCENTER].value();
  T y2mean = x[1] - param_p[YCENTER].value();
  if (param_p[PANGLE] != thePA) {
    thePA = param_p[PANGLE];
    theCpa = cos(thePA);
    theSpa = sin(thePA);
  };
  T xnorm = x2mean*theCpa.value() + y2mean*theSpa.value();
  T ynorm = -x2mean*theSpa.value() + y2mean*theCpa.value();
  T xnorm2 = xnorm*xnorm;
  T ynorm2 = ynorm*ynorm;
  theXwidth.value() = param_p[YWIDTH].value() * param_p[RATIO].value();
  T xwidth2 = theXwidth.value()*theXwidth.value()*
    fwhm2int.value()*fwhm2int.value();
  T ywidth2 = param_p[YWIDTH].value()*param_p[YWIDTH].value()*
    fwhm2int.value()*fwhm2int.value();
  T x2w = T(2.0)*xnorm/xwidth2;
  T y2w = T(2.0)*ynorm/ywidth2;
  T x2w2 = x2w*xnorm;
  T y2w2 = y2w*ynorm;
  T exponential = exp(-(xnorm2/xwidth2 + ynorm2/ywidth2));
  // function value
  tmp.value() = param_p[HEIGHT].value()*exponential;
  // get derivatives (assuming either all or none)
  if (tmp.nDerivatives()>0) {
    for (uInt k = 0; k < tmp.nDerivatives(); k++) tmp.deriv(k) = 0.0;
    // derivative wrt height
    T dev = exponential;
    if (param_p.mask(HEIGHT)) tmp.deriv(HEIGHT) = dev;
    // derivative wrt x0 (mean)
    dev *= param_p[HEIGHT].value();
    if (param_p.mask(XCENTER)) tmp.deriv(XCENTER) = dev*
				 (x2w*theCpa.value() - y2w*theSpa.value());
    // derivative wrt y0 (mean)
    if (param_p.mask(YCENTER)) tmp.deriv(YCENTER) = dev*
				 (theSpa.value()*x2w + theCpa.value()*y2w);
    // derivative wrt wy (width)
    if (param_p.mask(YWIDTH)) tmp.deriv(YWIDTH) = dev*
				((x2w2+y2w2)/param_p[YWIDTH].value());
    // derivative wrt ratio (r=wx/wy, df/dr=(df/wx)*(dwx/dr), and dwx/dr=wy)
    if (param_p.mask(RATIO)) tmp.deriv(RATIO) = dev*
			       x2w2*param_p[YWIDTH].value()/
			       (theXwidth.value());
    // derivative wrt theta (rotation)
    if (param_p.mask(PANGLE)) tmp.deriv(PANGLE) = -dev*
				(x2w*(-x2mean*theSpa.value() +
				      y2mean*theCpa.value()) +
				 y2w*(-x2mean*theCpa.value() -
				      y2mean*theSpa.value()));
  };
  return tmp;
}

//# Member functions
