//# NQGaussian1DParam.cc: Parameter handling for one-dimensional Gaussian class
//# Copyright (C) 2001
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
#include <aips/Functionals/NQGaussian1DParam.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>

//# Statics
///template<class T>
///const T NQGaussian1DParam<T>::fwhm2int = T(1.0)/sqrt(log(T(16.0)));

//# Constructors
template<class T>
NQGaussian1DParam<T>::NQGaussian1DParam() :
  NQFunction1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = T(1.0);
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
}

template<class T>
NQGaussian1DParam<T>::NQGaussian1DParam(const T &height) :
  NQFunction1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
}

template<class T>
NQGaussian1DParam<T>::NQGaussian1DParam(const T &height, const T &center) :
  NQFunction1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = center;
  param_p[WIDTH] = T(1.0);
}

template<class T>
NQGaussian1DParam<T>::NQGaussian1DParam(const T &height, const T &center,
				    const T &width) :
  NQFunction1D<T>(3),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = center;
  param_p[WIDTH] = width;
}

template<class T>
NQGaussian1DParam<T>::NQGaussian1DParam(const NQGaussian1DParam<T> &other) :
  NQFunction1D<T>(other),
  fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {}

template<class T>
NQGaussian1DParam<T>::~NQGaussian1DParam() {}

//# Operators
template<class T>
NQGaussian1DParam<T> &
NQGaussian1DParam<T>::operator=(const NQGaussian1DParam<T> &other) {
  if (this != &other) {
    fwhm2int = other.fwhm2int;
    NQFunction1D<T>::operator=(other);
  };
  return *this;
}

//# Member functions
template<class T>
T NQGaussian1DParam<T>::flux() const {
  return param_p[HEIGHT]*abs(param_p[WIDTH])*fwhm2int/T(C::_1_sqrtpi);
}

template<class T>
void NQGaussian1DParam<T>::setFlux(const T &flux) {
  param_p[HEIGHT] = flux*T(C::_1_sqrtpi)/abs(param_p[WIDTH])/fwhm2int;
}
