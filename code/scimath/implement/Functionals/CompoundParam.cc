//# NQCompoundParam.cc: Parameters for sum of parameterized  Functions
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
#include <aips/Functionals/NQCompoundParam.h>

template <class T>
NQCompoundParam<T>::NQCompoundParam() : 
  Function<T>(), ndim_p(0), updated_p(False),
  functionPtr_p(0),
  paroff_p(0), funpar_p(0), locpar_p(0) {}

template <class T>
NQCompoundParam<T>::NQCompoundParam(const NQCompoundParam<T> &other) :
  Function<T>(other), ndim_p(other.ndim_p), updated_p(other.updated_p),
  functionPtr_p(other.functionPtr_p.nelements()),
  paroff_p(other.paroff_p.nelements()),
  funpar_p(other.funpar_p.nelements()), 
  locpar_p(other.locpar_p.nelements()) { 
  for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
    functionPtr_p[i] = (*(other.functionPtr_p[i])).clone();
    paroff_p[i] = other.paroff_p[i];
  };
  for (uInt i=0; i<funpar_p.nelements(); ++i) {
    funpar_p[i] = other.funpar_p[i];
    locpar_p[i] = other.locpar_p[i];
  };
  toParam_p();
}

template <class T>
NQCompoundParam<T>::~NQCompoundParam() {
  for (uInt i=0; i<functionPtr_p.nelements(); i++) {
    delete functionPtr_p[i]; functionPtr_p[i] = 0;
  };
}

template <class T>
NQCompoundParam<T>& NQCompoundParam<T>::
operator=(const NQCompoundParam<T> &other) {
  if (this != &other) {
    Function<T>::operator=(other);
    ndim_p = other.ndim_p;
    updated_p = other.updated_p;
    for (uInt i=0; i<functionPtr_p.nelements(); i++) {
      delete functionPtr_p[i]; functionPtr_p[i] = 0;
    };
    functionPtr_p =  PtrBlock<Function<T> *>(other.functionPtr_p.nelements());
    paroff_p = Block<uInt>(other.paroff_p.nelements());
    funpar_p = Block<uInt>(other.funpar_p.nelements());
    locpar_p = Block<uInt>(other.locpar_p.nelements());
    for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
      functionPtr_p[i] = (*(other.functionPtr_p[i])).clone();
      paroff_p[i] = other.paroff_p[i];
    };
    for (uInt i=0; i<funpar_p.nelements(); ++i) {
      funpar_p[i] = other.funpar_p[i];
      locpar_p[i] = other.locpar_p[i];
    };
    toParam_p();
  };
  return *this;
}

// Operators
template <class T>
T &NQCompoundParam<T>::operator[](const uInt n) {
  fromParam_p();
  return (*functionPtr_p[funpar_p[n]])[locpar_p[n]];
}

template <class T>
const T &NQCompoundParam<T>::operator[](const uInt n) const {
  fromParam_p();
  return (*functionPtr_p[funpar_p[n]])[locpar_p[n]];
}

// Member functions
template <class T>
uInt NQCompoundParam<T>::addFunction(const Function<T> &newFunction) {
  if (functionPtr_p.nelements() != 0 && newFunction.ndim() != ndim_p) {
    throw(AipsError("NQCompoundParam::addFunction() -- "
		    "Inconsistent function dimension"));
  };
  // Add the function
  uInt i = functionPtr_p.nelements();
  functionPtr_p.resize(i+1);
  functionPtr_p[i] = newFunction.clone();
  ndim_p = (*(functionPtr_p[i])).ndim();
  // Set parameters
  uInt np = nparameters();
  paroff_p.resize(i+1);
  paroff_p[i] = np;
  FunctionParam<T> old(param_p);
  param_p = FunctionParam<T>(np + newFunction.nparameters());
  funpar_p.resize(np + newFunction.nparameters());
  locpar_p.resize(np + newFunction.nparameters());
  for (uInt j=0; j<np; ++j) param_p[j] = old[j];
  for (uInt j=np; j<np+newFunction.nparameters(); ++j) {
    param_p[j] = newFunction[j-np];
    funpar_p[j] = i;
    locpar_p[j] = j-paroff_p[funpar_p[j]];
  };
  return i;
}

template <class T>
Bool &NQCompoundParam<T>::mask(const uInt n) {
  fromParam_p();
  return (*functionPtr_p[funpar_p[n]]).mask(locpar_p[n]);
}

template <class T>
const Bool &NQCompoundParam<T>::mask(const uInt n) const {
  fromParam_p();
  return (*functionPtr_p[funpar_p[n]]).mask(locpar_p[n]);
}

template <class T>
const FunctionParam<T> &NQCompoundParam<T>::parameters() const {
  toParam_p();
  return param_p;
}

template <class T>
FunctionParam<T> &NQCompoundParam<T>::parameters() {
  toParam_p();
  updated_p = True;
  return param_p;
}

template <class T>
void NQCompoundParam<T>::toParam_p() const {
  if (!updated_p) {
    for (uInt i=0; i<nparameters(); ++i) {
      const_cast<FunctionParam<T> &>(param_p)[i] =
	(*functionPtr_p[funpar_p[i]])[locpar_p[i]];
    };
  };
}

template <class T>
void NQCompoundParam<T>::fromParam_p() const {
  if (updated_p) {
    updated_p = False;
    for (uInt i=0; i<nparameters(); ++i) {
      (*functionPtr_p[funpar_p[i]])[locpar_p[i]] = param_p[i];
    };
  };
}
