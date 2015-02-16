//# Sinusoid1DParam.cc: Parameter handling for one dimensional Sinusoid class
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

#ifndef SCIMATH_SINUSOID1DPARAM_TCC
#define SCIMATH_SINUSOID1DPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/Sinusoid1DParam.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
Sinusoid1DParam<T>::Sinusoid1DParam() :
  Function1D<T>(3) {
  param_p[AMPLITUDE] = T(1.0);
  param_p[X0] = T(0.0);
  param_p[PERIOD] = T(1.0);
}

template<class T>
Sinusoid1DParam<T>::Sinusoid1DParam(const T &amplitude) :
  Function1D<T>(3) {
  param_p[AMPLITUDE] = T(amplitude);
  param_p[X0] = T(0.0);
  param_p[PERIOD] = T(1.0);
}

template<class T>
Sinusoid1DParam<T>::Sinusoid1DParam(const T &amplitude, const T &period) :
  Function1D<T>(3) {
  param_p[AMPLITUDE] = T(amplitude);
  param_p[X0] = T(0.0);
  param_p[PERIOD] = T(period);
}

template<class T>
Sinusoid1DParam<T>::Sinusoid1DParam(const T &amplitude, const T &period,
					const T &x0) :
  Function1D<T>(3) {
  param_p[AMPLITUDE] = T(amplitude);
  param_p[X0] = T(x0);
  param_p[PERIOD] = T(period);
}

template<class T>
Sinusoid1DParam<T>::Sinusoid1DParam(const Sinusoid1DParam<T> &other) :
  Function1D<T>(other) {}

template<class T>
Sinusoid1DParam<T>::~Sinusoid1DParam() {}

//# Operators
template<class T>
Sinusoid1DParam<T> &
Sinusoid1DParam<T>::operator=(const Sinusoid1DParam<T> &other) {
  if (this != &other) Function1D<T>::operator=(other);
  return *this;
}

} //# NAMESPACE CASACORE - END


#endif
