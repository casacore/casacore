//# NQCompoundFunction.cc: Sum of functions to behave as a single function
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
#include <aips/Functionals/NQCompoundFunction.h>

//# Constructors

//# Operators
template<class T>
T NQCompoundFunction<T>::eval(typename Function<T>::FunctionArg x) const {
  if (parset_p) fromParam_p();
  T tmp(0);
  for (uInt i = 0; i<nFunctions(); ++i) tmp += function(i)(x);
  return tmp;
}

//# Member functions
template<class T>
Function<typename FunctionTraits<T>::DiffType>
*NQCompoundFunction<T>::cloneAD() const {
  Function<typename FunctionTraits<T>::DiffType> *t =
    new NQCompoundFunction<typename FunctionTraits<T>::DiffType>();
  for (uInt i=0; i<nFunctions(); ++i) {
    dynamic_cast<NQCompoundFunction<typename FunctionTraits<T>::DiffType> *>
      (t)->addFunction(*(function(i).cloneAD()));
  };
  return t;
}

template<class T>
Function<typename FunctionTraits<T>::BaseType>
*NQCompoundFunction<T>::cloneBase() const {
  Function<typename FunctionTraits<T>::BaseType> *t =
    new NQCompoundFunction<typename FunctionTraits<T>::BaseType>();
  for (uInt i=0; i<nFunctions(); ++i) {
    dynamic_cast<NQCompoundFunction<typename FunctionTraits<T>::BaseType> *>
      (t)->addFunction(*(function(i).cloneBase()));
  };
  return t;
}

template <class T>
void NQCompoundFunction<T>::fromParam_p() const {
  if (parset_p) {
    parset_p = False;
    for (uInt i=0; i<nparameters(); ++i) {
      (*functionPtr_p[funpar_p[i]])[locpar_p[i]] = param_p[i];
    };
  };
}
