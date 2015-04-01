//# LinearFit.cc: Class for linear least-squares fit.
//#
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

#ifndef SCIMATH_LINEARFIT_TCC
#define SCIMATH_LINEARFIT_TCC

//# Includes
#include <casacore/scimath/Fitting/LinearFit.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants

template<class T>
LinearFit<T>::LinearFit() :
  GenericL2Fit<T>() {}

template<class T>
LinearFit<T>::LinearFit(const LinearFit &other) :
  GenericL2Fit<T>(other) {}

template<class T>
LinearFit<T> &LinearFit<T>::operator=(const LinearFit &other) {
  if (this != &other) {
    GenericL2Fit<T>::operator=(other);
  }
  return *this;
}

template<class T>
LinearFit<T>::~LinearFit() {}

template<class T>
Bool LinearFit<T>::
fitIt(Vector<typename FunctionTraits<T>::BaseType> &sol, 
      const Array<typename FunctionTraits<T>::BaseType> &x, 
      const Vector<typename FunctionTraits<T>::BaseType> &y,
      const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
      const Vector<Bool> *const mask) {
  // Initialise fitter
  sol.resize(pCount_p);
  for (uInt i=0, k=0; i<pCount_p; ++i) {
    sol[i] = (*ptr_derive_p)[i].value();
    if (ptr_derive_p->mask(i)) sol_p[k++] = sol[i];
  }
  // Build normal equations
  this->buildMatrix(x, y, sigma, mask);
  // Build constraint equations
  buildConstraint();
  // Invert normal equations
  solved_p = this->invert(nr_p, svd_p);
  // Get solution and errors
  if (solved_p) {
    this->solve(condEq_p);
    sol_p += condEq_p;
    this->getErrors(err_p);
    errors_p = True;
    for (uInt i=0, k=0; i<pCount_p; i++) {
      if (ptr_derive_p->mask(i)) sol[i] = sol_p[k++];
      (*ptr_derive_p)[i].value() = sol[i];
    }
  }
  return solved_p;
}

} //# NAMESPACE CASACORE - END


#endif
