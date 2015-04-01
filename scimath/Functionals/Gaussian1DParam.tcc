//# Gaussian1DParam.cc: Parameter handling for one-dimensional Gaussian class
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

#ifndef SCIMATH_GAUSSIAN1DPARAM_TCC
#define SCIMATH_GAUSSIAN1DPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/Gaussian1DParam.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
///template<class T>
///const T Gaussian1DParam<T>::fwhm2int = T(1.0)/sqrt(log(T(16.0)));

//# Constructors
template<class T>
Gaussian1DParam<T>::Gaussian1DParam() :
  Function1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = T(1.0);
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
}

template<class T>
Gaussian1DParam<T>::Gaussian1DParam(const T &height) :
  Function1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
}

template<class T>
Gaussian1DParam<T>::Gaussian1DParam(const T &height, const T &center) :
  Function1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = center;
  param_p[WIDTH] = T(1.0);
}

template<class T>
Gaussian1DParam<T>::Gaussian1DParam(const T &height, const T &center,
				    const T &width) :
  Function1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = center;
  param_p[WIDTH] = width;
}

template<class T>
Gaussian1DParam<T>::Gaussian1DParam(const Gaussian1DParam<T> &other) :
  Function1D<T>(other),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {}

template<class T>
Gaussian1DParam<T>::~Gaussian1DParam() {}

//# Operators
template<class T>
Gaussian1DParam<T> &
Gaussian1DParam<T>::operator=(const Gaussian1DParam<T> &other) {
  if (this != &other) {
    fwhm2int = other.fwhm2int;
    Function1D<T>::operator=(other);
  }
  return *this;
}

//# Member functions
template<class T>
T Gaussian1DParam<T>::flux() const {
  return param_p[HEIGHT]*abs(param_p[WIDTH])*fwhm2int/T(C::_1_sqrtpi);
}

template<class T>
void Gaussian1DParam<T>::setFlux(const T &flux) {
  param_p[HEIGHT] = flux*T(C::_1_sqrtpi)/abs(param_p[WIDTH])/fwhm2int;
}

} //# NAMESPACE CASACORE - END


#endif
