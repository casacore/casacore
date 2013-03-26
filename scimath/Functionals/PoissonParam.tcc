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

//# Includes
#include <scimath/Functionals/PoissonParam.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
PoissonParam<T>::PoissonParam() :
  Function<T>(2) {
  param_p[LAMBDA] = T(1.0);
  param_p[HEIGHT] = T(1.0);
}

template<class T>
PoissonParam<T>::PoissonParam(const T &lambda) :
  Function<T>(2) {
  param_p[LAMBDA] = lambda;
  param_p[HEIGHT] = T(1.0);
}

template<class T>
PoissonParam<T>::PoissonParam( const T &lambda, const T &height ) :
Function<T>(2){
	param_p[LAMBDA] = lambda;
	param_p[HEIGHT] = height;
}

template<class T>
PoissonParam<T>::PoissonParam(const PoissonParam<T> &other) :
  Function<T>(other) {}

template<class T>
PoissonParam<T>::~PoissonParam() {}

//# Operators
template<class T>
PoissonParam<T> &PoissonParam<T>::operator=(const PoissonParam<T> &other) {
  if (this != &other) Function<T>::operator=(other);
  return *this;
}



} //# NAMESPACE CASA - END

