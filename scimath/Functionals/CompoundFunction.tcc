//# CompoundFunction.cc: Sum of functions to behave as a single function
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

#ifndef SCIMATH_COMPOUNDFUNCTION_TCC
#define SCIMATH_COMPOUNDFUNCTION_TCC

//# Includes
#include <casacore/scimath/Functionals/CompoundFunction.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors

//# Operators
template<class T>
T CompoundFunction<T>::eval(typename Function<T>::FunctionArg x) const {
  if (parset_p) fromParam_p();
  T tmp(0);
  for (uInt i = 0; i<nFunctions(); ++i) tmp += function(i)(x);
  return tmp;
}

//# Member functions
template <class T>
void CompoundFunction<T>::fromParam_p() const {
  if (parset_p) {
    parset_p = False;
    for (uInt i=0; i<nparameters(); ++i) {
      (*functionPtr_p[funpar_p[i]])[locpar_p[i]] = param_p[i];
      functionPtr_p[funpar_p[i]]->mask(locpar_p[i]) = param_p.mask(i);
    }
  }
}

template <class T>
void CompoundFunction<T>::toParam_p() {
  for (uInt i=0; i<nparameters(); ++i) {
    param_p[i] = (*functionPtr_p[funpar_p[i]])[locpar_p[i]];
    param_p.mask(i) = functionPtr_p[funpar_p[i]]->mask(locpar_p[i]);
  }
}

} //# NAMESPACE CASACORE - END


#endif
