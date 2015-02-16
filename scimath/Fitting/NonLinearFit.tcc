//# NonLinearFit.cc: Class for non-linear least-squares fit.
//# Copyright (C) 1995,1999,2000,2001,2002,2004
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

#ifndef SCIMATH_NONLINEARFIT_TCC
#define SCIMATH_NONLINEARFIT_TCC

#include <casacore/scimath/Fitting/NonLinearFit.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
// Default convergence criterium
template<class T>
const Double NonLinearFit<T>::CRITERIUM = 0.001;

//# Constructors
template<class T>
NonLinearFit<T>::NonLinearFit(Bool svd) :
  GenericL2Fit<T>(),
  maxiter_p(MAXITER), curiter_p(MAXITER),
  criterium_p(CRITERIUM),
  converge_p(False) {
  svd_p = svd;
  if (!svd_p) set(0.0);
}

template<class T>
NonLinearFit<T>::NonLinearFit(const NonLinearFit &other) :
  GenericL2Fit<T>(other),
  maxiter_p(other.maxiter_p),
  curiter_p(other.curiter_p), criterium_p(other.criterium_p),
  converge_p(other.converge_p) {
  if (other.ptr_derive_p) ptr_derive_p = other.ptr_derive_p->clone();
  condEq_p = other.condEq_p;
  fullEq_p = other.fullEq_p;
  arg_p = other.arg_p;
  sol_p = other.sol_p;
  fsol_p = other.fsol_p;
  err_p = other.err_p;
  ferr_p = other.ferr_p;
  valder_p =other.valder_p;
}

template<class T>
NonLinearFit<T> &NonLinearFit<T>::operator=(const NonLinearFit &other) {
  if (this != &other) {
    GenericL2Fit<T>::operator=(other);
    maxiter_p = other.maxiter_p;
    curiter_p = other.curiter_p;
    criterium_p = other.criterium_p;
    converge_p = other.converge_p;
  }
  return *this;
}

template<class T>
NonLinearFit<T>::~NonLinearFit() {}

template<class T>
void NonLinearFit<T>::setMaxIter(uInt maxIter) {
  maxiter_p = (maxIter > 0 ? maxIter : 1);
  curiter_p = (curiter_p > maxiter_p ? maxiter_p : curiter_p);
}

} //# NAMESPACE CASACORE - END


#endif
