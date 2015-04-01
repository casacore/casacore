//# GaussianND.cc: GaussianND class
//# Copyright (C) 1996,1998,1999,2001,2002
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

#ifndef SCIMATH_GAUSSIANND_TCC
#define SCIMATH_GAUSSIANND_TCC

#include <casacore/scimath/Functionals/GaussianND.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> 
T GaussianND<T>::eval(typename Function<T>::FunctionArg x) const {
  Vector<T> norm(itsDim);
  for (uInt i=0; i<itsDim; i++) norm[i] = x[i] - param_p[CENTER+i];

  T exponent(0);
  for (uInt i=0, k=0; i<itsDim; i++) {
    for (uInt j=i+1; j<itsDim; j++) {
      exponent += norm[i]*norm[j]*param_p[CENTER+itsDim+itsDim+k++];
    }
  }
  exponent *= 2;

  for (uInt i=0; i<itsDim; i++) {
    exponent += norm[i]*norm[i]*param_p[CENTER+itsDim+i];
  }
  return param_p[HEIGHT] * exp(-exponent/T(2));
}

} //# NAMESPACE CASACORE - END


#endif
