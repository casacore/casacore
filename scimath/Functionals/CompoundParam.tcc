//# CompoundParam.cc: Parameters for sum of parameterized  Functions
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

#ifndef SCIMATH_COMPOUNDPARAM_TCC
#define SCIMATH_COMPOUNDPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/CompoundParam.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
CompoundParam<T>::CompoundParam() : 
  Function<T>(), ndim_p(0),
  functionPtr_p(0),
  paroff_p(0), funpar_p(0), locpar_p(0) {}

template <class T>
CompoundParam<T>::CompoundParam(const CompoundParam<T> &other) :
  Function<T>(other), ndim_p(other.ndim_p),
  functionPtr_p(other.functionPtr_p.nelements()),
  paroff_p(other.paroff_p.nelements()),
  funpar_p(other.funpar_p.nelements()), 
  locpar_p(other.locpar_p.nelements()) { 
  for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
    functionPtr_p[i] = other.functionPtr_p[i]->clone();
    paroff_p[i] = other.paroff_p[i];
  }
  for (uInt i=0; i<funpar_p.nelements(); ++i) {
    funpar_p[i] = other.funpar_p[i];
    locpar_p[i] = other.locpar_p[i];
  }
}

template <class T>
CompoundParam<T>::~CompoundParam() {
  for (uInt i=0; i<functionPtr_p.nelements(); i++) {
    delete functionPtr_p[i]; functionPtr_p[i] = 0;
  }
}

template <class T>
CompoundParam<T>& CompoundParam<T>::
operator=(const CompoundParam<T> &other) {
  if (this != &other) {
    Function<T>::operator=(other);
    ndim_p = other.ndim_p;
    for (uInt i=0; i<functionPtr_p.nelements(); i++) {
      delete functionPtr_p[i]; functionPtr_p[i] = 0;
    }
    functionPtr_p =  PtrBlock<Function<T> *>(other.functionPtr_p.nelements());
    paroff_p = Block<uInt>(other.paroff_p.nelements());
    funpar_p = Block<uInt>(other.funpar_p.nelements());
    locpar_p = Block<uInt>(other.locpar_p.nelements());
    for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
      functionPtr_p[i] = other.functionPtr_p[i]->clone();
      paroff_p[i] = other.paroff_p[i];
    }
    for (uInt i=0; i<funpar_p.nelements(); ++i) {
      funpar_p[i] = other.funpar_p[i];
      locpar_p[i] = other.locpar_p[i];
    }
  }
  return *this;
}

// Operators

// Member functions
template <class T>
uInt CompoundParam<T>::addFunction(const Function<T> &newFunction) {
  if (functionPtr_p.nelements() != 0 && newFunction.ndim() != ndim_p) {
    throw(AipsError("CompoundParam::addFunction() -- "
		    "Inconsistent function dimension"));
  }
  // Add the function
  uInt i = functionPtr_p.nelements();
  functionPtr_p.resize(i+1);
  functionPtr_p[i] = newFunction.clone();
  ndim_p = functionPtr_p[i]->ndim();
  // Set parameters
  uInt np = nparameters();
  paroff_p.resize(i+1);
  paroff_p[i] = np;
  FunctionParam<T> old(param_p);
  param_p = FunctionParam<T>(np + newFunction.nparameters());
  funpar_p.resize(np + newFunction.nparameters());
  locpar_p.resize(np + newFunction.nparameters());
  for (uInt j=0; j<np; ++j) {
    param_p[j] = old[j];
    param_p.mask(j) = old.mask(j);
  }
  for (uInt j=np; j<np+newFunction.nparameters(); ++j) {
    param_p[j] = newFunction[j-np];
    param_p.mask(j) = newFunction.mask(j-np);
    funpar_p[j] = i;
    locpar_p[j] = j-paroff_p[funpar_p[j]];
  }
  return i;
}

} //# NAMESPACE CASACORE - END


#endif
