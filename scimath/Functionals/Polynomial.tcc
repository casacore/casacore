//# Polynomial.cc: A one dimensional polynomial class
//# Copyright (C) 1994,1995,1996,1998,2001,2002
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

#ifndef SCIMATH_POLYNOMIAL_TCC
#define SCIMATH_POLYNOMIAL_TCC

//# Includes
#include <casacore/scimath/Functionals/Polynomial.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T>
T Polynomial<T>::eval(typename Function1D<T>::FunctionArg x) const {
  Int j = nparameters();
  T accum = param_p[--j];
  while (--j >= 0) {
    accum *= x[0];
    accum += param_p[j];
  }
  return accum;
}

template<class T>
Polynomial<T> Polynomial<T>::derivative() const {
  Int ord = order() - 1;
  if (ord < 0) return Polynomial<T>(0);
  Polynomial<T> result(ord);
  for (uInt i=1; i <= order(); ++i) result[i-1] = T(i)*(*this)[i];
  return result;
}

//# Member functions

} //# NAMESPACE CASACORE - END


#endif
