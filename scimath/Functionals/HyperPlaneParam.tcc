//# HyperPlaneParam.cc: Parameters for a hyperplane function
//# Copyright (C) 2001,2002,2004
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef SCIMATH_HYPERPLANEPARAM_TCC
#define SCIMATH_HYPERPLANEPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/HyperPlaneParam.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
HyperPlaneParam<T>::HyperPlaneParam(uInt m) :
  Function<T>(m) {}

template<class T>
HyperPlaneParam<T>::HyperPlaneParam(const HyperPlaneParam<T> &other) :
  Function<T>(other) {}

template<class T>
HyperPlaneParam<T>::~HyperPlaneParam() {}

template<class T>
HyperPlaneParam<T> &
HyperPlaneParam<T>::operator=(const HyperPlaneParam<T> &other) {
  if (this != &other) Function<T>::operator=(other);
  return *this;
}

} //# NAMESPACE CASACORE - END


#endif
