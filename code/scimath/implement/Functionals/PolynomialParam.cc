//# NQPolynomialParam.cc: Parameter handling for one-dimensional polynomials
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
#include <aips/Functionals/NQPolynomialParam.h>
#include <aips/Arrays/Vector.h>

template<class T>
NQPolynomialParam<T>::NQPolynomialParam() :
  NQFunction1D<T>(1) {}

template<class T>
NQPolynomialParam<T>::NQPolynomialParam(uInt order) :
  NQFunction1D<T>(order+1) {}

template<class T>
NQPolynomialParam<T>::NQPolynomialParam(const NQPolynomialParam<T> &other) :
  NQFunction1D<T>(other) {}

template<class T>
NQPolynomialParam<T>::~NQPolynomialParam() {}

template<class T>
NQPolynomialParam<T> &
NQPolynomialParam<T>::operator=(const NQPolynomialParam<T> &other) {
  if (this != &other) NQFunction1D<T>::operator=(other);
  return *this;
}

template<class T>
const Vector<T> &NQPolynomialParam<T>::coefficients() const {
  return param_p.getParameters();
}

template<class T> 
void NQPolynomialParam<T>::setCoefficients(const Vector<T> &coefficients) {
  param_p.setParameters(coefficients);
}
