//# NonLinearFitLM.cc: Solve non-linear fit using Levenberg-Marquardt method.
//# Copyright (C) 1995,1999,2000,2001,2002
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

#include <trial/Fitting/NonLinearFitLM.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/VectorSTLIterator.h>
#include <aips/Exceptions/Error.h>
#include <aips/Functionals/Function.h>
#include <aips/typeinfo.h>

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
  };
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
  Double fitit_p = 1.0;
  // Initialise fitter
  LSQFit::reset();
  set(aCount_ai,
      typename LSQTraits<typename FunctionTraits<T>::BaseType>::num_type());
  Double mu, me;
  sol.resize(pCount_p);
  for (uInt i=0, k=0; i<pCount_p; ++i) {
    sol[i] = (*ptr_derive_p)[i].value();
    if (ptr_derive_p->mask(i)) sol_p[k++] = sol[i];
  };
  // And loop
  while (curiter_p > 0 && (fitit_p > 0 || fitit_p < -0.001)) {
    setMaskedParameterValues(sol_p);
    // Build normal equations
    buildMatrix(x, y, sigma, mask);
    // Do an LM loop
    ///    if (!solveLoop(fitit_p, nr_p, sol_p)) {
    VectorSTLIterator<typename FunctionTraits<T>::BaseType> csolit(sol_p);;;
    if (!solveLoop(fitit_p, nr_p, csolit)) {
      throw(AipsError("NonLinearFitLM: error in loop solution"));
    };
    curiter_p--;
  };
  converge_p = curiter_p;
  solved_p = True;
  
  // Solve last time
  setMaskedParameterValues(sol_p);
  buildMatrix(x, y, sigma, mask);
  invert(nr_p, True);
  VectorSTLIterator<typename FunctionTraits<T>::BaseType> ceqit(condEq_p);;;
  solve(ceqit);
  ///solve(condEq_p);
  mu = getSD();
  me = getWeightedSD();
  sol_p += condEq_p;
  VectorSTLIterator<typename FunctionTraits<T>::BaseType> cerrit(err_p);;;
  ///LSQFit::getErrors(err_p);
  LSQFit::getErrors(cerrit);
  errors_p = True;
  for (uInt i=0, k=0; i<pCount_p; i++) {
    if (ptr_derive_p->mask(i)) {
      sol[i] = sol_p[k++];
      (*ptr_derive_p)[i].value() = sol[i];
    };    
  };	
  solved_p = converge_p;
  return converge_p;
}
