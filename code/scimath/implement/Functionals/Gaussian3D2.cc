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

#include <trial/Functionals/Gaussian3D.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/Exceptions/Error.h>
#include <aips/BasicSL/Constants.h>
#include <aips/BasicMath/Math.h>

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

  if (stoT_p != param_p[THETA] || stoP_p != param_p[PHI]) settrigvals();

  const T cosTV = cosT_p.value();
  const T cosPV = cosP_p.value();
  const T sinTV = sinT_p.value();
  const T sinPV = sinP_p.value();
  const T cosTcosPV = cosTcosP_p.value();
  const T cosTsinPV = cosTsinP_p.value();
  const T sinTcosPV = sinTcosP_p.value();
  const T sinTsinPV = sinTsinP_p.value();

  if (param_p[H].nDerivatives() > 0) {
     tmp = param_p[H];
  } else if (param_p[CX].nDerivatives() > 0) {
     tmp = param_p[CX];
  } else if (param_p[CY].nDerivatives() > 0) {
     tmp = param_p[CY];
  } else if (param_p[CZ].nDerivatives() > 0) {
     tmp = param_p[CZ];
  } else if (param_p[AX].nDerivatives() > 0) {
     tmp = param_p[AX];
  } else if (param_p[AY].nDerivatives() > 0) {
     tmp = param_p[AY];
  } else if (param_p[AZ].nDerivatives() > 0) {
     tmp = param_p[AZ];
  } else if (param_p[THETA].nDerivatives() > 0) {
     tmp = param_p[THETA];
  } else if (param_p[PHI].nDerivatives() > 0) {
     tmp = param_p[PHI];
  }

  T value;

  ///
  const T Ax = param_p[AX].value() * fwhm2int.value();
  const T Ay = param_p[AY].value() * fwhm2int.value();
  const T Az = param_p[AZ].value() * fwhm2int.value();
  const T Nx = x[0] - param_p[CX].value();
  const T Ny = x[1] - param_p[CY].value();
  const T Nz = x[2] - param_p[CZ].value();
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

  value = expterm * param_p[H].value();
  const T tvalue = value * 2.0;

  //function value
  tmp.value() = value;

  if (tmp.nDerivatives() > 0)
  {
    for (k = 0; k < tmp.nDerivatives(); k++) tmp.deriv(k) = 0.0;

    // derivative wrt height
    if (param_p.mask(H))
      tmp.deriv(H) = expterm;

    // derivative wrt Cx (mean)
    if (param_p.mask(CX))
      tmp.deriv(CX) = tvalue * (  cosTcosPV * xrowterm / Ax2
	 	                - sinTcosPV * yrowterm / Ay2  
		                +     sinPV * zrowterm / Az2);

    // derivative wrt Cy (mean)
    if (param_p.mask(CY))
      tmp.deriv(CY) = tvalue * (  sinTV * xrowterm / Ax2 
                                + cosTV * yrowterm / Ay2);

    // derivative wrt Cz (mean)
    if (param_p.mask(CZ))
      tmp.deriv(CZ) = tvalue * (- cosTsinPV * xrowterm / Ax2 
                                + sinTsinPV * yrowterm / Ay2
	                        +     cosPV * zrowterm / Az2);

    // derivative wrt Ax
    if (param_p.mask(AX))
      tmp.deriv(AX) = tvalue * xwidthterm2/param_p[AX].value();

    // derivative wrt Ay
    if (param_p.mask(AY))
      tmp.deriv(AY) = tvalue * ywidthterm2/param_p[AY].value();

    // derivative wrt Az
    if (param_p.mask(AZ))
      tmp.deriv(AZ) = tvalue * zwidthterm2/param_p[AZ].value();

    // derivative wrt theta
    if (param_p.mask(THETA))
      tmp.deriv(THETA) = tvalue * (  xrowterm * yrowterm / Ay2
	 		           - xrowterm * yrowterm / Ax2);

    // derivative wrt phi
    if (param_p.mask(PHI))
      tmp.deriv(PHI) = -tvalue *(xrowterm * (-Nx*cosTsinPV - Nz*cosTcosPV)/ Ax2
			       + yrowterm * (Nx*sinTsinPV + Nz*sinTcosPV) / Ay2
                               + zrowterm * (Nx*cosPV - Nz*sinPV) / Az2);
  }
 
  return tmp;
}


template<class T> 
Function<AutoDiff<T> >* Gaussian3D<AutoDiff<T> >::clone() const 
{
  Function<AutoDiff<T> > *tmp = new Gaussian3D<AutoDiff<T> >(*this);
  if (!tmp)
    throw(AllocError("Gaussian3D<AutoDiff<T> >::cloneFunctionND()"
                     " - new failed", sizeof(Gaussian3D<T>)));
  return tmp;
}

template <class T>
T Gaussian3D<AutoDiff<T> >::sq(T v) const
{
  return v*v;
}




