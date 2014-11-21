//# SincParam.cc: A one dimensional sin(x)/x
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

//# Includes
#include <casacore/scimath/Functionals/SincParam.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
SincParam<T>::SincParam() :
  Function<T>(3) {
  param_p[HEIGHT] = T(1.0);
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
}

template<class T>
SincParam<T>::SincParam(const T &height) :
  Function<T>(3) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = T(0.0);
  param_p[WIDTH] = T(1.0);
}

template<class T>
SincParam<T>::SincParam(const T &height, const T &center) :
  Function<T>(3) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = center;
  param_p[WIDTH] = T(1.0);
}

template<class T>
SincParam<T>::SincParam(const T &height, const T &center,
			  const T &width) :
  Function<T>(3) {
  param_p[HEIGHT] = height;
  param_p[CENTER] = center;
  param_p[WIDTH] = width;
}

template<class T>
SincParam<T>::SincParam(const SincParam<T> &other) :
  Function<T>(other) {}

template<class T>
SincParam<T>::~SincParam() {}

//# Operators
template<class T>
SincParam<T> &SincParam<T>::operator=(const SincParam<T> &other) {
  if (this != &other) Function<T>::operator=(other);
  return *this;
}

} //# NAMESPACE CASA - END

