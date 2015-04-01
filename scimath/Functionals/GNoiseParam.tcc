//# GNoiseParam.cc: A one dimensional normal distribution 
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

#ifndef SCIMATH_GNOISEPARAM_TCC
#define SCIMATH_GNOISEPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/GNoiseParam.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
GNoiseParam<T>::GNoiseParam() :
  Function<T>(0), genit_p(), noise_p(&genit_p, 0.0, 1.0) {}

template<class T>
GNoiseParam<T>::GNoiseParam(const Double &mean, const Double &var) :
  Function<T>(0), genit_p(), noise_p(&genit_p, mean, var) {}

template<class T>
GNoiseParam<T>::GNoiseParam(const GNoiseParam<T> &other) :
  Function<T>(other), genit_p(other.genit_p),
  noise_p(&genit_p, other.noise_p.mean(), other.noise_p.variance()) {}

template<class T>
GNoiseParam<T>::~GNoiseParam() {}

//# Operators
template<class T>
GNoiseParam<T> &GNoiseParam<T>::operator=(const GNoiseParam<T> &other) {
  if (this != &other) {
    Function<T>::operator=(other);
    genit_p = other.genit_p;
    noise_p = Normal(&genit_p, other.noise_p.mean(), other.noise_p.variance());
  }
  return *this;
}

} //# NAMESPACE CASACORE - END


#endif
