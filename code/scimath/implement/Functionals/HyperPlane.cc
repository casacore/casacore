//# NQHyperPlane.cc: Defines NQHyperPlane 
//# Copyright (C) 2001
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
#include <aips/Functionals/NQHyperPlane.h>

//# Constructors

//# Operators
template<class T>
T NQHyperPlane<T>::eval(typename Function<T>::FunctionArg x) const {
  Int i= nparameters();
  T accum = param_p[--i];  // the last coefficient
  while (--i >= 0) accum += param_p[i]*x[i];
  return accum;
}

//# Member functions
template<class T>
Function<typename FunctionTraits<T>::DiffType>
*NQHyperPlane<T>::cloneAD() const {
  Function<typename FunctionTraits<T>::DiffType> *t =
    new NQHyperPlane<typename FunctionTraits<T>::DiffType>();
  for (uInt i=0; i<nparameters(); ++i) {
    (*t)[i] = typename FunctionTraits<T>::DiffType(param_p[i]);
  };
  return t;
}

template<class T>
Function<typename FunctionTraits<T>::BaseType>
*NQHyperPlane<T>::cloneBase() const {
  Function<typename FunctionTraits<T>::BaseType> *t =
    new NQHyperPlane<typename FunctionTraits<T>::BaseType>();
  for (uInt i=0; i<nparameters(); ++i) {
    (*t)[i] = typename FunctionTraits<T>::DiffType(param_p[i]).value();
  };
  return t;
}
