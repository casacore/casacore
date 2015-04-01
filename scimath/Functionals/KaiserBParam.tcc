//# KaiserBParam.cc: A one dimensional Kaiser-Bessel function
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

#ifndef SCIMATH_KAISERBPARAM_TCC
#define SCIMATH_KAISERBPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/KaiserBParam.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
KaiserBParam<T>::KaiserBParam() :
  Function<T>(4) {
  param_p[HEIGHT] = T(1.0);
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
  param_p[KBPAR] = T(2.5);
}

template<class T>
KaiserBParam<T>::KaiserBParam(const T &kbpar) :
  Function<T>(4) {
  param_p[HEIGHT] = T(1.0);
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
  param_p[KBPAR] = kbpar;
}

template<class T>
KaiserBParam<T>::KaiserBParam(const KaiserBParam<T> &other) :
  Function<T>(other) {}

template<class T>
KaiserBParam<T>::~KaiserBParam() {}

//# Operators
template<class T>
KaiserBParam<T> &KaiserBParam<T>::operator=(const KaiserBParam<T> &other) {
  if (this != &other) Function<T>::operator=(other);
  return *this;
}

} //# NAMESPACE CASACORE - END


#endif
