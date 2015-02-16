//# FunctionParam.cc: Container of function parameters with masking flags
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

#ifndef SCIMATH_FUNCTIONPARAM_TCC
#define SCIMATH_FUNCTIONPARAM_TCC

#include <casacore/scimath/Functionals/FunctionParam.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
FunctionParam<T>::FunctionParam()
  : npar_p(0),
    param_p(npar_p), mask_p(npar_p),
    maskedPtr_p(0) {}

template<class T>
FunctionParam<T>::FunctionParam(const uInt n)
  : npar_p(n),
    param_p(npar_p), mask_p(npar_p, True),
    maskedPtr_p(0) {
  for (uInt i=0; i<npar_p; ++i) param_p[i] = T(0);
}

template<class T>
FunctionParam<T>::FunctionParam(const Vector<T> &in)
  : npar_p(in.nelements()),
    param_p(npar_p), mask_p(npar_p, True),
    maskedPtr_p(0) {
  for (uInt i=0; i<npar_p; ++i) param_p[i] = in[i];
}

template<class T>
FunctionParam<T>::FunctionParam(const FunctionParam<T> &other)
  : npar_p(other.param_p.nelements()),
    param_p(npar_p), mask_p(npar_p),
    maskedPtr_p(0) {
  for (uInt i=0; i<npar_p; ++i) param_p[i] = other.param_p[i];
  mask_p = other.mask_p;
}

template<class T>
FunctionParam<T>::~FunctionParam() {
  clearMaskedPtr();
}

//# Operators
template<class T>
FunctionParam<T> &FunctionParam<T>::operator=(const FunctionParam<T> &other) {
  if (this != &other) {
    npar_p = other.npar_p;
    param_p.resize(npar_p);
    param_p = other.param_p;
    mask_p.resize(npar_p);
    mask_p = other.mask_p;
    clearMaskedPtr();
  }
  return *this;
}

template<class T>
Bool FunctionParam<T>::operator==(const FunctionParam<T> &other) const {
  if (npar_p != other.npar_p) return False;
  for (uInt i=0; i<npar_p; ++i) {
    if (param_p[i] != other.param_p[i] ||
	mask_p[i] != other.mask_p[i]) return False;
  }
  return True;
}

template<class T>
Bool FunctionParam<T>::operator!=(const FunctionParam<T> &other) const {
  return (!((*this) == other));
}

//# Member functions
template<class T>
Bool &FunctionParam<T>::mask(const uInt n) {
  clearMaskedPtr();
  return mask_p[n];
}

template<class T>
void FunctionParam<T>::setParameters(const Vector<T> &params) {
  uInt n = ((params.nelements() < npar_p) ? params.nelements() : npar_p);
  for (uInt i=0; i<n; ++i) param_p[i] = params[i];
}

template<class T>
void FunctionParam<T>::setParamMasks(const Vector<Bool> &masks) {
  uInt n = ((masks.nelements() < npar_p) ? masks.nelements() : npar_p);
  for (uInt i=0; i<n; ++i) mask_p[i] = masks[i];
  clearMaskedPtr();
}

template<class T>
uInt FunctionParam<T>::nMaskedParameters() const {
  createMaskedPtr();
  return maskedPtr_p->nelements();
}

template<class T>
Vector<T> &FunctionParam<T>::getMaskedParameters() const {
  createMaskedPtr();
  return *maskedPtr_p;
}

template<class T>
void FunctionParam<T>::setMaskedParameters(Vector<T> &in) {
  for (uInt i(0), n(0); i<npar_p && n<in.nelements(); ++i) {
    if (mask_p[i]) param_p[i] = in[n++];
  }
  clearMaskedPtr();
}

template<class T>
void FunctionParam<T>::createMaskedPtr() const {
  if (!maskedPtr_p) {
    clearMaskedPtr();
    Vector<T> tmp(npar_p);
    uInt n(0);
    for (uInt i(0); i<npar_p; ++i) {
      if (mask_p[i]) tmp[n++] = param_p[i];
    }
    tmp.resize(n, True);
    maskedPtr_p = new Vector<T>(tmp);
  }
}

template<class T>
void FunctionParam<T>::clearMaskedPtr() const {
  delete maskedPtr_p; maskedPtr_p = 0;
}

//# Global functions
template<class T>
ostream &FunctionParam<T>::print(ostream &os) const {
  os << "[";
  for (uInt i=0; i<npar_p; i++) {
    if (i!=0) os << ", ";
    os << "(" << param_p[i] << ", " <<
      ((mask_p[i]) ? "True" : "False") << ")";
  }
  os << "]";
  return os;
}

} //# NAMESPACE CASACORE - END


#endif
