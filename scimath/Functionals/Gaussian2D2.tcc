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

#ifndef SCIMATH_GAUSSIAN2D2_TCC
#define SCIMATH_GAUSSIAN2D2_TCC

//# Includes
#include <casacore/scimath/Functionals/Gaussian2D.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T>
AutoDiff<T> Gaussian2D<AutoDiff<T> >::
eval(typename Function<AutoDiff<T> >::FunctionArg x) const {
  AutoDiff<T> tmp;
  if (this->param_p[this->HEIGHT].nDerivatives() > 0) tmp = this->param_p[this->HEIGHT];
  else if (this->param_p[this->XCENTER].nDerivatives() > 0) tmp = this->param_p[this->XCENTER];
  else if (this->param_p[this->YCENTER].nDerivatives() > 0) tmp = this->param_p[this->YCENTER];
  else if (this->param_p[this->YWIDTH].nDerivatives() > 0) tmp = this->param_p[this->YWIDTH];
  else if (this->param_p[this->RATIO].nDerivatives() > 0) tmp = this->param_p[this->RATIO];
  else if (this->param_p[this->PANGLE].nDerivatives() > 0) tmp = this->param_p[this->PANGLE];

  T x2mean = x[0] - this->param_p[this->XCENTER].value();
  T y2mean = x[1] - this->param_p[this->YCENTER].value();
  if (this->param_p[this->PANGLE] != this->thePA) {
    this->thePA = this->param_p[this->PANGLE];
    this->theCpa = cos(this->thePA);
    this->theSpa = sin(this->thePA);
  }
  T xnorm = x2mean*this->theCpa.value() + y2mean*this->theSpa.value();
  T ynorm = -x2mean*this->theSpa.value() + y2mean*this->theCpa.value();
  T xnorm2 = xnorm*xnorm;
  T ynorm2 = ynorm*ynorm;
  this->theXwidth.value() = this->param_p[this->YWIDTH].value() * this->param_p[this->RATIO].value();
  T xwidth2 = this->theXwidth.value()*this->theXwidth.value()*
    this->fwhm2int.value()*this->fwhm2int.value();
  T ywidth2 = this->param_p[this->YWIDTH].value()*this->param_p[this->YWIDTH].value()*
    this->fwhm2int.value()*this->fwhm2int.value();
  T x2w = T(2.0)*xnorm/xwidth2;
  T y2w = T(2.0)*ynorm/ywidth2;
  T x2w2 = x2w*xnorm;
  T y2w2 = y2w*ynorm;
  T exponential = exp(-(xnorm2/xwidth2 + ynorm2/ywidth2));
  // function value
  tmp.value() = this->param_p[this->HEIGHT].value()*exponential;
  // get derivatives (assuming either all or none)
  if (tmp.nDerivatives()>0) {
    for (uInt k = 0; k < tmp.nDerivatives(); k++) tmp.deriv(k) = 0.0;
    // derivative wrt height
    T dev = exponential;
    if (this->param_p.mask(this->HEIGHT)) tmp.deriv(this->HEIGHT) = dev;
    // derivative wrt x0 (mean)
    dev *= this->param_p[this->HEIGHT].value();
    if (this->param_p.mask(this->XCENTER)) tmp.deriv(this->XCENTER) = dev*
				 (x2w*this->theCpa.value() - y2w*this->theSpa.value());
    // derivative wrt y0 (mean)
    if (this->param_p.mask(this->YCENTER)) tmp.deriv(this->YCENTER) = dev*
				 (this->theSpa.value()*x2w + this->theCpa.value()*y2w);
    // derivative wrt wy (width)
    if (this->param_p.mask(this->YWIDTH)) tmp.deriv(this->YWIDTH) = dev*
				((x2w2+y2w2)/this->param_p[this->YWIDTH].value());
    // derivative wrt ratio (r=wx/wy, df/dr=(df/wx)*(dwx/dr), and dwx/dr=wy)
    if (this->param_p.mask(this->RATIO)) tmp.deriv(this->RATIO) = dev*
			       x2w2*this->param_p[this->YWIDTH].value()/
			       (this->theXwidth.value());
    // derivative wrt theta (rotation)
    if (this->param_p.mask(this->PANGLE)) tmp.deriv(this->PANGLE) = -dev*
				(x2w*(-x2mean*this->theSpa.value() +
				      y2mean*this->theCpa.value()) +
				 y2w*(-x2mean*this->theCpa.value() -
				      y2mean*this->theSpa.value()));
  }
  return tmp;
}

//# Member functions

} //# NAMESPACE CASACORE - END


#endif
