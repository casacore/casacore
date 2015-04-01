//# NonLinearFitLM.cc: Solve non-linear fit using Levenberg-Marquardt method.
//# Copyright (C) 1995,1999-2002,2004,2006
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

#ifndef SCIMATH_NONLINEARFITLM_TCC
#define SCIMATH_NONLINEARFITLM_TCC

//# Includes

#include <casacore/scimath/Fitting/NonLinearFitLM.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
NonLinearFitLM<T>::NonLinearFitLM(Bool svd) :
  NonLinearFit<T>(svd),
  lamda_p(0.001) {}

template<class T>
NonLinearFitLM<T>::NonLinearFitLM(const NonLinearFitLM &other) :
  NonLinearFit<T>(other),
  lamda_p(other.lamda_p) {}

template<class T>
NonLinearFitLM<T> &NonLinearFitLM<T>::operator=(const NonLinearFitLM &other) {
  if (this != &other) {
    NonLinearFit<T>::operator=(other);
    lamda_p = other.lamda_p;
  }
  return *this;
}

template<class T>
NonLinearFitLM<T>::~NonLinearFitLM() {}

template<class T>
Bool NonLinearFitLM<T>::
fitIt(Vector<typename FunctionTraits<T>::BaseType> &sol, 
      const Array<typename FunctionTraits<T>::BaseType> &x, 
      const Vector<typename FunctionTraits<T>::BaseType> &y,
      const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
      const Vector<Bool> *const mask) {
  // Initialise loops
  curiter_p = maxiter_p;
  converge_p = False;
  // Initialise fitter
  sol.resize(pCount_p);
  for (uInt i=0, k=0; i<pCount_p; ++i) {
    sol[i] = (*ptr_derive_p)[i].value();
    if (ptr_derive_p->mask(i)) sol_p[k++] = sol[i];
  }
  // And loop
  while (curiter_p > 0 && (!this->isReady() || curiter_p == maxiter_p)) {
    setMaskedParameterValues(sol_p);
    // Build normal equations
    this->buildMatrix(x, y, sigma, mask);
    // Build constraint equations
    buildConstraint();
    // Do an LM loop
    VectorSTLIterator<typename FunctionTraits<T>::BaseType> csolit(sol_p);
    if (!this->solveLoop(nr_p, csolit)) {
      throw(AipsError("NonLinearFitLM: error in loop solution"));
    }
    curiter_p--;
  }
  converge_p = curiter_p;
  solved_p = True;
  
  // Solve last time
  setMaskedParameterValues(sol_p);
  this->buildMatrix(x, y, sigma, mask);
  buildConstraint();
  this->invert(nr_p, True);
  this->solve(condEq_p);
  sol_p += condEq_p;
  this->getErrors(err_p);
  errors_p = True;
  for (uInt i=0, k=0; i<pCount_p; i++) {
    if (ptr_derive_p->mask(i)) sol[i] = sol_p[k++];
    (*ptr_derive_p)[i].value() = sol[i];
  }	
  solved_p = converge_p;
  return converge_p;
}

} //# NAMESPACE CASACORE - END


#endif
