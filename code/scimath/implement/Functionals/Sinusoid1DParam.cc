//# NQSinusoid1DParam.cc: Parameter handling for one dimensional NQSinusoid class
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
#include <aips/Functionals/NQSinusoid1DParam.h>

template<class T>
NQSinusoid1DParam<T>::NQSinusoid1DParam() :
  NQFunction1D<T>(3) {
  param_p[AMPLITUDE] = T(1.0);
  param_p[X0] = T(0.0);
  param_p[PERIOD] = T(1.0);
}

template<class T>
NQSinusoid1DParam<T>::NQSinusoid1DParam(const T &amplitude) :
  NQFunction1D<T>(3) {
  param_p[AMPLITUDE] = T(amplitude);
  param_p[X0] = T(0.0);
  param_p[PERIOD] = T(1.0);
}

template<class T>
NQSinusoid1DParam<T>::NQSinusoid1DParam(const T &amplitude, const T &period) :
  NQFunction1D<T>(3) {
  param_p[AMPLITUDE] = T(amplitude);
  param_p[X0] = T(0.0);
  param_p[PERIOD] = T(period);
}

template<class T>
NQSinusoid1DParam<T>::NQSinusoid1DParam(const T &amplitude, const T &period,
					const T &x0) :
  NQFunction1D<T>(3) {
  param_p[AMPLITUDE] = T(amplitude);
  param_p[X0] = T(x0);
  param_p[PERIOD] = T(period);
}

template<class T>
NQSinusoid1DParam<T>::NQSinusoid1DParam(const NQSinusoid1DParam<T> &other) :
  NQFunction1D<T>(other) {}

template<class T>
NQSinusoid1DParam<T>::~NQSinusoid1DParam() {}

//# Operators
template<class T>
NQSinusoid1DParam<T> &
NQSinusoid1DParam<T>::operator=(const NQSinusoid1DParam<T> &other) {
  if (this != &other) NQFunction1D<T>::operator=(other);
  return *this;
}
