//# LQLinearFit.cc: Class for linear least-squares fit.
//#
//# Copyright (C) 1995,1999,2000,2001
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
#include <trial/Fitting/LQLinearFit.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/AutoDiffIO.h>
#include <aips/Functionals/Function.h>

//# Constants

template<class T>
LQLinearFit<T>::LQLinearFit() :
  LQGenericL2Fit<T>() {}

template<class T>
LQLinearFit<T>::LQLinearFit(LSQ::normType type) :
  LQGenericL2Fit<T>(type) {}

template<class T>
LQLinearFit<T>::LQLinearFit(const LQLinearFit &other) :
  LQGenericL2Fit<T>(other) {}

template<class T>
LQLinearFit<T> &LQLinearFit<T>::operator=(const LQLinearFit &other) {
  if (this != &other) {
    LQGenericL2Fit<T>::operator=(other);
  };
  return *this;
}

template<class T>
LQLinearFit<T>::~LQLinearFit() {}

template<class T>
Bool LQLinearFit<T>::
fitIt(Vector<typename FunctionTraits<T>::BaseType> &sol, 
      const Array<typename FunctionTraits<T>::BaseType> &x, 
      const Vector<typename FunctionTraits<T>::BaseType> &y,
      const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
      const Vector<Bool> *const mask) {
  // Initialise fitter
  Double mu, me;
  sol.resize(pCount_p);
  for (uInt i=0; i<pCount_p; ++i) sol[i] = (*ptr_derive_p)[i].value();
  for (uInt i=0; i<aCount_ai; i++) {
    sol_p[i] = (ptr_derive_p->parameters().getMaskedParameters()[i]).value();
  };
  // Build normal equations
  buildMatrix(x, y, sigma, mask);
  // Invert normal equations
  solved_p = invert(nr_p, svd_p);
  // Get solution and errors
  if (solved_p) {
    solve(condEq_p, mu, me);
    sol_p += condEq_p;
    ///    FitLSQ::getErrors(err_p);
    errors_p = True;
    for (uInt i=0, k=0; i<pCount_p; i++) {
      if (ptr_derive_p->mask(i)) sol[i] = sol_p[k++];
      (*ptr_derive_p)[i].value() = sol[i];
    };
  };
  return solved_p;
}
