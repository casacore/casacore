//# Gaussian3D.cc: A three-dimensional Gaussian class
//# Copyright (C) 1994,1995,1996,1999,2001,2002
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

#ifndef SCIMATH_GAUSSIAN3D_TCC
#define SCIMATH_GAUSSIAN3D_TCC


#include <casacore/scimath/Functionals/Gaussian3D.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> 
Gaussian3D<T>::Gaussian3D()   
: Gaussian3DParam<T>()
{}

template<class T> 
Gaussian3D<T>::Gaussian3D(T &height, T &xCenter, T &yCenter, T &zCenter, 
                          T &xWidth, T &yWidth, T &zWidth, 
                          T &theta, T &phi) 
: Gaussian3DParam<T>(height, xCenter, yCenter, zCenter, xWidth, yWidth,
                     zWidth, theta, phi)
{}

template<class T> 
Gaussian3D<T>::Gaussian3D(T height, const Vector<T>& center, 
                          const Vector<T>& width, T theta, T phi)
: Gaussian3DParam<T>(height, center, width, theta, phi)
{}

template<class T>
Gaussian3D<T>::~Gaussian3D()
{}

template<class T> 
Gaussian3D<T>::Gaussian3D(const Gaussian3D<T>& other)
  : Gaussian3DParam<T>(other)
{}

template<class T> 
Gaussian3D<T>& Gaussian3D<T>::operator=(const Gaussian3D<T>& other)
{
  Gaussian3DParam<T>::operator=(other);
  return *this;
}



template<class T>
T Gaussian3D<T>::eval(typename Function<T>::FunctionArg x) const
{
  T Nx = x[0] - param_p[CX];
  T Ny = x[1] - param_p[CY];
  T Nz = x[2] - param_p[CZ];
  T Ax = param_p[AX]*fwhm2int;
  T Ay = param_p[AY]*fwhm2int;
  T Az = param_p[AZ]*fwhm2int;

  T v;

  if (stoT_p != param_p[THETA] || stoP_p != param_p[PHI]) settrigvals();

  v = param_p[H] 
        * exp( - sq((cosTcosP_p*Nx + sinT_p*Ny - cosTsinP_p*Nz)/Ax)
	       - sq((-sinTcosP_p*Nx + cosT_p*Ny +sinTsinP_p*Nz)/Ay)
               - sq((sinP_p*Nx + cosP_p*Nz)/Az));
  return v;
  

}


template<class T> 
Function<T>* Gaussian3D<T>::clone() const 
{
  Function<T> *tmp = new Gaussian3D<T>(*this);
  return tmp;
}


template <class T>
T Gaussian3D<T>::sq(T v) const
{
  return v*v;
}


} //# NAMESPACE CASACORE - END


#endif
