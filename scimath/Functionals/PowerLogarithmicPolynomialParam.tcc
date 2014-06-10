//# PolynomialParam.cc: Parameter handling for one-dimensional polynomials
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
//# $Id: PolynomialParam.tcc 19879 2007-02-15 03:52:50Z Malte.Marquarding $

//# Includes
#include <scimath/Functionals/PowerLogarithmicPolynomialParam.h>
#include <casa/Arrays/Vector.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
PowerLogarithmicPolynomialParam<T>::PowerLogarithmicPolynomialParam() :
  Function1D<T>(2) {
	setCoefficient(0, 1);
	setCoefficient(1, 1);
}

template<class T>
PowerLogarithmicPolynomialParam<T>::PowerLogarithmicPolynomialParam(uInt n) :
  Function1D<T>(n) {
	if (n < 2) {
		throw AipsError("PowerLogarithmicPolynomialParam constructor: n must be at least 2");
	}
}

template<class T>
PowerLogarithmicPolynomialParam<T>::PowerLogarithmicPolynomialParam(const vector<T>& parms)
	: Function1D<T>(Vector<Double>(parms)) {
	if (parms.size() < 2) {
		throw AipsError("PowerLogarithmicPolynomialParam constructor: n must be at least 2");
	}
	if (parms[0] == 0) {
		throw AipsError("PowerLogarithmicPolynomialParam constructor: p0 cannot be zero");
	}
}

template<class T>
PowerLogarithmicPolynomialParam<T>::PowerLogarithmicPolynomialParam(const PowerLogarithmicPolynomialParam<T> &other) :
  Function1D<T>(other) {}

template<class T>
PowerLogarithmicPolynomialParam<T>::~PowerLogarithmicPolynomialParam() {}

template<class T>
PowerLogarithmicPolynomialParam<T> &
PowerLogarithmicPolynomialParam<T>::operator=(const PowerLogarithmicPolynomialParam<T> &other) {
  if (this != &other) Function1D<T>::operator=(other);
  return *this;
}

template<class T>
const Vector<T> &PowerLogarithmicPolynomialParam<T>::coefficients() const {
  return param_p.getParameters();
}

template<class T> 
void PowerLogarithmicPolynomialParam<T>::setCoefficients(const Vector<T> &coefficients) {
	if (coefficients.size() < 2) {
		throw AipsError("PowerLogarithmicPolynomialParam<T>::setCoefficients(): Number of coefficients must be at least 2");
	}


  param_p.setParameters(coefficients);
}

} //# NAMESPACE CASA - END

