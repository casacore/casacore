//# SPolynomialParam.cc: Parameter handling for scaled 1-D polynomials
//# Copyright (C) 2002
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

#ifndef SCIMATH_SPOLYNOMIALPARAM_TCC
#define SCIMATH_SPOLYNOMIALPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/SPolynomialParam.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
SPolynomialParam<T>::SPolynomialParam() :
  Function<T>(4) {
  param_p[0] = 1;
  param_p[2] = 1;
  for (uInt i=0; i<3; ++i) mask(i) = False;
}

template<class T>
SPolynomialParam<T>::SPolynomialParam(uInt order) :
  Function<T>(order+4) {
  param_p[0] = 1;
  param_p[2] = 1;
  for (uInt i=0; i<3; ++i) mask(i) = False;
}

template<class T>
SPolynomialParam<T>::SPolynomialParam(const SPolynomialParam<T> &other) :
  Function<T>(other) {}

template<class T>
SPolynomialParam<T>::~SPolynomialParam() {}

template<class T>
SPolynomialParam<T> &
SPolynomialParam<T>::operator=(const SPolynomialParam<T> &other) {
  if (this != &other) Function<T>::operator=(other);
  return *this;
}

template<class T>
Vector<T> SPolynomialParam<T>::coefficients() const {
  Vector<T> tmp(order()+1);
  for (uInt i=3; i<nparameters(); ++i) tmp(i-3) = param_p[i];
  return tmp;
}

template<class T> 
void SPolynomialParam<T>::setCoefficients(const Vector<T> &coefficients) {
  for (uInt i=3; i<nparameters(); ++i) param_p[i] = coefficients[i-3];
}

} //# NAMESPACE CASACORE - END


#endif
