//# Gaussian3D2.cc: Three dimensional Gaussian class specialized for AutoDiff
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

#ifndef SCIMATH_GAUSSIAN3D2_TCC
#define SCIMATH_GAUSSIAN3D2_TCC

#include <casacore/scimath/Functionals/Gaussian3D.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> 
Gaussian3D<AutoDiff<T> >::Gaussian3D()   
: Gaussian3DParam<AutoDiff<T> >()
{}

template<class T> 
Gaussian3D<AutoDiff<T> >::Gaussian3D(AutoDiff<T> &height,
                                     AutoDiff<T> &xCenter, 
                                     AutoDiff<T> &yCenter,
                                     AutoDiff<T> &zCenter,
                                     AutoDiff<T> &xWidth,
                                     AutoDiff<T> &yWidth, 
                                     AutoDiff<T> &zWidth, 
                                     AutoDiff<T> &theta,  
                                     AutoDiff<T> &phi) 
: Gaussian3DParam<AutoDiff<T> >(height, xCenter, yCenter, zCenter, 
                                xWidth, yWidth, zWidth, theta, phi)
{}

template<class T> 
Gaussian3D<AutoDiff<T> >::Gaussian3D(const AutoDiff<T>& height,
                                     const Vector<AutoDiff<T> >& center,
                                     const Vector<AutoDiff<T> >& width, 
                                     const AutoDiff<T>& theta,
                                     const AutoDiff<T>& phi)
: Gaussian3DParam<AutoDiff<T> >(height, center, width, theta, phi)
{}

template<class T>
Gaussian3D<AutoDiff<T> >::~Gaussian3D()
{}

template<class T> 
Gaussian3D<AutoDiff<T> >::Gaussian3D(const Gaussian3D<AutoDiff<T> >& other)
: Gaussian3DParam<AutoDiff<T> >(other)
{}

template<class T> 
Gaussian3D<AutoDiff<T> >& Gaussian3D<AutoDiff<T> >::operator=(const Gaussian3D<AutoDiff<T> >& other)
{
  Gaussian3DParam<AutoDiff<T> >::operator=(other);
  return *this;
}


template<class T>
AutoDiff<T> Gaussian3D<AutoDiff<T> >::eval(typename Function<AutoDiff<T> >::FunctionArg x) const
{
  uInt k;
  AutoDiff<T> tmp;
//  

  if (this->stoT_p != this->param_p[Gaussian3DParam<AutoDiff<T> >::THETA]
   || this->stoP_p != this->param_p[Gaussian3DParam<AutoDiff<T> >::PHI]) {
    this->settrigvals();
  }

  const T cosTV = this->cosT_p.value();
  const T cosPV = this->cosP_p.value();
  const T sinTV = this->sinT_p.value();
  const T sinPV = this->sinP_p.value();
  const T cosTcosPV = this->cosTcosP_p.value();
  const T cosTsinPV = this->cosTsinP_p.value();
  const T sinTcosPV = this->sinTcosP_p.value();
  const T sinTsinPV = this->sinTsinP_p.value();

  if (this->param_p[Gaussian3DParam<AutoDiff<T> >::H].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::H];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::CX].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::CX];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::CY].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::CY];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::CZ].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::CZ];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::AX].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::AX];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::AY].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::AY];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::AZ].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::AZ];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::THETA].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::THETA];
  } else if (this->param_p[Gaussian3DParam<AutoDiff<T> >::PHI].nDerivatives() > 0) {
     tmp = this->param_p[Gaussian3DParam<AutoDiff<T> >::PHI];
  }

  T value;

  ///
  const T Ax = this->param_p[Gaussian3DParam<AutoDiff<T> >::AX].value() * this->fwhm2int.value();
  const T Ay = this->param_p[Gaussian3DParam<AutoDiff<T> >::AY].value() * this->fwhm2int.value();
  const T Az = this->param_p[Gaussian3DParam<AutoDiff<T> >::AZ].value() * this->fwhm2int.value();
  const T Nx = x[0] - this->param_p[Gaussian3DParam<AutoDiff<T> >::CX].value();
  const T Ny = x[1] - this->param_p[Gaussian3DParam<AutoDiff<T> >::CY].value();
  const T Nz = x[2] - this->param_p[Gaussian3DParam<AutoDiff<T> >::CZ].value();
  const T Ax2 = Ax * Ax;
  const T Ay2 = Ay * Ay;
  const T Az2 = Az * Az;
  const T xrowterm =  cosTcosPV*Nx + sinTV*Ny - cosTsinPV*Nz;
  const T yrowterm = -sinTcosPV*Nx + cosTV*Ny + sinTsinPV*Nz;
  const T zrowterm =  sinPV*Nx + cosPV*Nz;
  const T xwidthterm = xrowterm/Ax;
  const T ywidthterm = yrowterm/Ay;
  const T zwidthterm = zrowterm/Az;
  const T xwidthterm2 = xwidthterm * xwidthterm;
  const T ywidthterm2 = ywidthterm * ywidthterm;
  const T zwidthterm2 = zwidthterm * zwidthterm;

  const T expterm = exp(-xwidthterm2 - ywidthterm2 - zwidthterm2);

  value = expterm * this->param_p[Gaussian3DParam<AutoDiff<T> >::H].value();
  const T tvalue = value * 2.0;

  //function value
  tmp.value() = value;

  if (tmp.nDerivatives() > 0)
  {
    for (k = 0; k < tmp.nDerivatives(); k++) tmp.deriv(k) = 0.0;

    // derivative wrt height
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::H))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::H) = expterm;

    // derivative wrt Cx (mean)
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::CX))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::CX) = tvalue * (  cosTcosPV * xrowterm / Ax2
	 	                - sinTcosPV * yrowterm / Ay2  
		                +     sinPV * zrowterm / Az2);

    // derivative wrt Cy (mean)
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::CY))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::CY) = tvalue * (  sinTV * xrowterm / Ax2 
                                + cosTV * yrowterm / Ay2);

    // derivative wrt Cz (mean)
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::CZ))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::CZ) = tvalue * (- cosTsinPV * xrowterm / Ax2 
                                + sinTsinPV * yrowterm / Ay2
	                        +     cosPV * zrowterm / Az2);

    // derivative wrt Ax
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::AX))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::AX) = tvalue * xwidthterm2/this->param_p[Gaussian3DParam<AutoDiff<T> >::AX].value();

    // derivative wrt Ay
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::AY))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::AY) = tvalue * ywidthterm2/this->param_p[Gaussian3DParam<AutoDiff<T> >::AY].value();

    // derivative wrt Az
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::AZ))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::AZ) = tvalue * zwidthterm2/this->param_p[Gaussian3DParam<AutoDiff<T> >::AZ].value();

    // derivative wrt theta
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::THETA))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::THETA) = tvalue * (  xrowterm * yrowterm / Ay2
	 		           - xrowterm * yrowterm / Ax2);

    // derivative wrt phi
    if (this->param_p.mask(Gaussian3DParam<AutoDiff<T> >::PHI))
      tmp.deriv(Gaussian3DParam<AutoDiff<T> >::PHI) = -tvalue *(xrowterm * (-Nx*cosTsinPV - Nz*cosTcosPV)/ Ax2
			       + yrowterm * (Nx*sinTsinPV + Nz*sinTcosPV) / Ay2
                               + zrowterm * (Nx*cosPV - Nz*sinPV) / Az2);
  }
 
  return tmp;
}


template<class T> 
Function<AutoDiff<T> >* Gaussian3D<AutoDiff<T> >::clone() const 
{
  Function<AutoDiff<T> > *tmp = new Gaussian3D<AutoDiff<T> >(*this);
  return tmp;
}

template <class T>
T Gaussian3D<AutoDiff<T> >::sq(T v) const
{
  return v*v;
}





} //# NAMESPACE CASACORE - END


#endif
