//# KaiserBFunction.cc: A one dimensional Kaiser-Bessel function
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

#ifndef SCIMATH_KAISERBFUNCTION_TCC
#define SCIMATH_KAISERBFUNCTION_TCC

#include <casacore/scimath/Functionals/KaiserBFunction.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T> 
T KaiserBFunction<T>::eval(typename Function<T>::FunctionArg x) const {
  T par2 = param_p[KBPAR] * param_p[KBPAR];
  T x1 = T(C::pi) * param_p[KBPAR];
  T x2 = T(C::pi) * sqrt(par2 - T(1.0));
  T x3 = T(C::pi) * sqrt(par2 - T(4.0));
  T a = sinh(x1);
  T b = sinh(x2) * T(2.0);
  T c = sinh(x3) * T(2.0);
  T sum = a + b + c;
  a /= sum;
  b /= sum;
  c /= sum;
  T y = (x[0]-param_p[CENTER]) * T(C::pi) / param_p[WIDTH];
  return param_p[HEIGHT]*(a + b * cos(y) + c * cos(T(2.0) * y));
}

//# Member functions

} //# NAMESPACE CASACORE - END


#endif
