// # CombiParam.cc:  Parameters for a linear combination of Functions
// # Copyright (C) 2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef SCIMATH_COMBIPARAM_TCC
#define SCIMATH_COMBIPARAM_TCC

// # Includes
#include <casacore/scimath/Functionals/CombiParam.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

template <class T>
CombiParam<T>::CombiParam() : Function<T>(), ndim_p(0), functionPtr_p(0) {}

template <class T>
CombiParam<T>::CombiParam(const CombiParam<T> &other)
    : Function<T>(other), ndim_p(other.ndim_p), functionPtr_p(other.functionPtr_p.nelements()) {
  for (unsigned int i = 0; i < functionPtr_p.nelements(); ++i) {
    functionPtr_p[i] = (*(other.functionPtr_p[i])).clone();
  }
}

template <class T>
CombiParam<T>::~CombiParam() {
  for (unsigned int i = 0; i < functionPtr_p.nelements(); i++) {
    delete functionPtr_p[i];
    functionPtr_p[i] = nullptr;
  }
}

template <class T>
CombiParam<T> &CombiParam<T>::operator=(const CombiParam<T> &other) {
  if (this != &other) {
    Function<T>::operator=(other);
    ndim_p = other.ndim_p;
    for (unsigned int i = 0; i < functionPtr_p.nelements(); i++) {
      delete functionPtr_p[i];
      functionPtr_p[i] = nullptr;
    }
    functionPtr_p = Block<Function<T> *>(other.functionPtr_p.nelements());
    for (unsigned int i = 0; i < functionPtr_p.nelements(); ++i) {
      functionPtr_p[i] = (*(other.functionPtr_p[i])).clone();
    }
  }
  return *this;
}

template <class T>
unsigned int CombiParam<T>::addFunction(const Function<T> &newFunction) {
  if (functionPtr_p.nelements() != 0 && newFunction.ndim() != ndim_p) {
    throw(
        AipsError("CombiParam::addFunction() -- "
                  "Inconsistent function dimension"));
  }
  // Add the function
  unsigned int i = functionPtr_p.nelements();
  functionPtr_p.resize(i + 1);
  functionPtr_p[i] = newFunction.clone();
  ndim_p = (*(functionPtr_p[i])).ndim();
  // Set parameters
  this->param_p = FunctionParam<T>(i + 1);
  for (unsigned int j = 0; j < i + 1; ++j) this->param_p[j] = T(1.0);
  return i;
}

}  // namespace casacore

#endif
