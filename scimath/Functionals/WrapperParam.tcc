//# WrapperParam.cc: Parameter handling for wrapped function objects 
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

#ifndef SCIMATH_WRAPPERPARAM_TCC
#define SCIMATH_WRAPPERPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/WrapperParam.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template<class T>
WrapperParam<T>::WrapperParam() :
  Function<T>(0) {}

template<class T>
WrapperParam<T>::WrapperParam(const T &par) :
  Function<T>(1) {
  param_p[0] = par;
}

template<class T>
WrapperParam<T>::WrapperParam(const Vector<T> &par) :
  Function<T>(par) {}


template<class T>
WrapperParam<T>::WrapperParam(const WrapperParam<T> &other) :
  Function<T>(other) {}

template<class T>
WrapperParam<T>::~WrapperParam() {}

//# Operators
template<class T>
WrapperParam<T> &
WrapperParam<T>::operator=(const WrapperParam<T> &other) {
  if (this != &other) Function<T>::operator=(other);
  return *this;
}

} //# NAMESPACE CASACORE - END


#endif
