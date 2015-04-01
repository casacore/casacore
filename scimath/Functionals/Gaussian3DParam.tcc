//# Gaussian3DParam.cc: A three-dimensional Gaussian class
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

#ifndef SCIMATH_GAUSSIAN3DPARAM_TCC
#define SCIMATH_GAUSSIAN3DPARAM_TCC

#include <casacore/scimath/Functionals/Gaussian3DParam.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class Type> 
Gaussian3DParam<Type>::Gaussian3DParam() 
  : Function<Type>(NPAR)
{
  param_p[H] = Type(1.0);
  param_p[CX] = Type(0.0);
  param_p[CY] = Type(0.0);
  param_p[CZ] = Type(0.0);
  param_p[AX] = Type(1.0);
  param_p[AY] = Type(1.0);
  param_p[AZ] = Type(1.0);
  param_p[THETA] = Type(0.0);
  param_p[PHI] = Type(0.0);
  fwhm2int = Type(1.0)/sqrt(log(Type(16.0)));
  settrigvals();
}


template<class Type> 
Gaussian3DParam<Type>::Gaussian3DParam(Type &height, 
                             Type &xCenter, Type &yCenter, Type &zCenter, 
                             Type &xWidth, Type &yWidth, Type &zWidth, 
                             Type &theta, Type &phi) 
  : Function<Type>(NPAR)
{
  param_p[H] = height;
  param_p[CX] = xCenter;
  param_p[CY] = yCenter;
  param_p[CZ] = zCenter;
  param_p[AX] = xWidth;
  param_p[AY] = yWidth;
  param_p[AZ] = zWidth;
  param_p[THETA] = theta;
  param_p[PHI] = phi;
  fwhm2int = Type(1.0)/sqrt(log(Type(16.0)));

  settrigvals();  
}


template<class Type> 
Gaussian3DParam<Type>::Gaussian3DParam(Type /*height*/, 
                                       const Vector<Type>& center, 
                                       const Vector<Type>& width, 
                                       Type T, Type P)
  : Function<Type>(NPAR)
{
  fwhm2int = Type(1.0)/sqrt(log(Type(16.0)));
  setCenter(center);
  setWidth(width);
  settheta(T); setphi(P);
  settrigvals();
}

template<class Type> 
Gaussian3DParam<Type>::Gaussian3DParam(const Gaussian3DParam<Type>& other)
  : Function<Type, Type>(other)
{  
   fwhm2int = Type(1.0)/sqrt(log(Type(16.0)));
   settrigvals();   //IMPR: could set vals explicitly to speed things up
}

template<class Type>
Gaussian3DParam<Type>::~Gaussian3DParam()
{}


template<class Type> 
Gaussian3DParam<Type>& Gaussian3DParam<Type>::operator=(const Gaussian3DParam<Type>& other)
{
  if (this != &other)
  {
    Function<Type>::operator=(other);
    settrigvals();  //IMPR: explicit
    fwhm2int = other.fwhm2int;
  }
  return *this;
}



template<class Type> 
Type Gaussian3DParam<Type>::height() const
{
  return param_p[H];
}

template<class Type> 
void Gaussian3DParam<Type>::setHeight(const Type& height) 
{
  param_p[H] = height;
}


template <class Type>
Type Gaussian3DParam<Type>::flux() const
{
  return param_p[H]*param_p[AX]*param_p[AY]*param_p[AZ]*
         fwhm2int*fwhm2int*fwhm2int*Type(C::pi*sqrt(C::pi));
}

template <class Type>
void Gaussian3DParam<Type>::setFlux(const Type& flux)
{
  param_p[H]= flux / (param_p[AX]*param_p[AY]*param_p[AZ]*
         fwhm2int*fwhm2int*fwhm2int*Type(C::pi*sqrt(C::pi)));
}


template<class Type> 
Vector<Type> Gaussian3DParam<Type>::center() const 
{
  Vector<Type> center(3);
  center(0) = param_p[CX];
  center(1) = param_p[CY];
  center(2) = param_p[CZ];
  return center;
}

template<class Type> 
void Gaussian3DParam<Type>::setCenter(const Vector<Type>& center) 
{
  if (center.nelements() != 3) 
    throw(AipsError("Gaussian3D<Type>::setCenter(const Vector<Type>& center)"
		    " - center must be of length 3"));
  param_p[CX] = center(0);
  param_p[CY] = center(1);
  param_p[CZ] = center(2);
}

template<class Type> 
Type Gaussian3DParam<Type>::xCenter() const 
{
  return param_p[CX];
}

template<class Type> 
Type Gaussian3DParam<Type>::yCenter() const 
{
  return param_p[CY];
}

template <class Type>
Type Gaussian3DParam<Type>::zCenter() const
{
  return param_p[CZ];
}

template<class Type> 
void Gaussian3DParam<Type>::setXcenter(const Type& xcenter) 
{
  param_p[CX] = xcenter;
}

template<class Type> 
void Gaussian3DParam<Type>::setYcenter(const Type& ycenter) 
{
  param_p[CY] = ycenter;
}

template<class Type>
void Gaussian3DParam<Type>::setZcenter(const Type& zcenter)
{
  param_p[CZ] = zcenter;
}

template<class Type> 
Vector<Type> Gaussian3DParam<Type>::width() const 
{
  Vector<Type> width(3);
  width(0) = param_p[AX];
  width(1) = param_p[AY];
  width(2) = param_p[AZ];
  return width;
}

template<class Type> 
void Gaussian3DParam<Type>::setWidth(const Vector<Type>& width) 
{
  if (width.nelements() != 3)
    throw(AipsError("Gaussian3DParam<Type>::setWidth"
                    "(const Vector<Type>& width)"
                    " - width must be of length 3"));
  param_p[AX] = width(0);
  param_p[AY] = width(1);
  param_p[AZ] = width(2);
}

template<class Type>
void Gaussian3DParam<Type>::setXwidth(const Type & xwidth)
{
  if (xwidth <= Type(0)) 
    throw(AipsError("Gaussian3DParam<Type>::setXwidth(const Type& xwidth)"
                    " - width must be positive"));
  param_p[AX] = xwidth;
}

template<class Type>
void Gaussian3DParam<Type>::setYwidth(const Type & ywidth)
{
  if (ywidth <= Type(0)) 
    throw(AipsError("Gaussian3DParam<Type>::setYwidth(const Type& ywidth)"
                    " - width must be positive"));
  param_p[AY] = ywidth; 
}

template<class Type>
void Gaussian3DParam<Type>::setZwidth(const Type & zwidth)
{
  if (zwidth <= Type(0)) 
    throw(AipsError("Gaussian3DParam<Type>::setZwidth(const Type& zwidth)"
                    " - width must be positive"));
  param_p[AZ] = zwidth;
}

template<class Type>
Type Gaussian3DParam<Type>::xWidth() const
{
  return param_p[AX];
}

template<class Type>
Type Gaussian3DParam<Type>::yWidth() const
{
  return param_p[AY];
}

template<class Type>
Type Gaussian3DParam<Type>::zWidth() const
{
  return param_p[AZ];
}

template<class Type> 
Type Gaussian3DParam<Type>::theta() const 
{
  //IMPR: force to be in stated range by using a correctParameters fn
  //      (see FitGaussian)
  return param_p[THETA];
}

template <class Type>
Type Gaussian3DParam<Type>::phi() const
{
  //IMPR: force to be in stated range
  return param_p[PHI];
}

template<class Type> 
void Gaussian3DParam<Type>::settheta(const Type& theta) 
{
  if (abs(theta) > Type(C::pi_4))   
    throw(AipsError("Gaussian3DParam<Type>::settheta(const Type& theta)"
		    " - theta must be in radians and between -pi/4 and pi/4"));
  
  param_p[THETA] = theta;
  settrigvals();
}

template<class Type> 
void Gaussian3DParam<Type>::setphi(const Type& phi) 
{
  if (abs(phi) > Type(C::pi_4)) 
    throw(AipsError("Gaussian3D<Type>::setphi(const Type& phi)"
		    " - phi must be in radians and between -pi/4 and pi/4"));
  
  param_p[PHI] = phi;
  settrigvals();
}




template <class Type>
void Gaussian3DParam<Type>::settrigvals() const
{
  stoT_p = param_p[THETA];
  stoP_p = param_p[PHI];
  sinT_p = sin(param_p[THETA]);
  cosT_p = cos(param_p[THETA]);
  sinP_p = sin(param_p[PHI]);
  cosP_p = cos(param_p[PHI]);
  cosTcosP_p = cosT_p * cosP_p;
  cosTsinP_p = cosT_p * sinP_p;
  sinTcosP_p = sinT_p * cosP_p;
  sinTsinP_p = sinT_p * sinP_p;
}


} //# NAMESPACE CASACORE - END


#endif
