//# NQChebyshevParam.cc  a function class that defines a NQChebyshevParam polynomial
//# Copyright (C) 2000,2001
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
#include <aips/Functionals/NQChebyshevParam.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>

//# Constructors
template <class T>
NQChebyshevParam<T>::NQChebyshevParam() :
  NQFunction1D<T>(1), def_p(T(0)), 
  minx_p(T(-1)), maxx_p(T(1)), mode_p(CONSTANT) {} 

template <class T>
NQChebyshevParam<T>::NQChebyshevParam(const uInt n) :
  NQFunction1D<T>(n+1), def_p(T(0)), 
  minx_p(T(-1)), maxx_p(T(1)), mode_p(CONSTANT) {} 

template <class T>
NQChebyshevParam<T>::NQChebyshevParam(const T &min, const T &max,
				      const OutOfIntervalMode mode,
				      const T &defval) :
  NQFunction1D<T>(1), def_p(defval), mode_p(mode) {
  param_p[0] = 1;
  setInterval(min, max);
}

template <class T>
NQChebyshevParam<T>::NQChebyshevParam(const Vector<T> &coeffs,
				      const T &min, const T &max, 
				      const OutOfIntervalMode mode,
				      const T &defval) :
  NQFunction1D<T>(coeffs.nelements()), def_p(defval), mode_p(mode) { 
  setInterval(min, max);
  setCoefficients(coeffs);
}

template <class T>
NQChebyshevParam<T>::NQChebyshevParam(const NQChebyshevParam &other) :
  NQFunction1D<T>(other), def_p(other.def_p), 
  minx_p(other.minx_p), maxx_p(other.maxx_p), mode_p(other.mode_p) {} 

template <class T>
NQChebyshevParam<T> &
NQChebyshevParam<T>::operator=(const NQChebyshevParam<T> &other) {
  if (this != &other) {
    mode_p = other.mode_p;
    minx_p = other.minx_p;
    maxx_p = other.maxx_p;
    def_p = other.def_p;
  };
  return *this;
}

template <class T>
NQChebyshevParam<T>::~NQChebyshevParam() {}

//# Operators

//# Member functions
template <class T>
void NQChebyshevParam<T>::setCoefficients(const Vector<T> &coeffs) {
  if (coeffs.nelements() == 0) {
    throw AipsError("NQChebyshevParam<T>::setCoeffiecients(): "
		    "empty Vector passed");
  };
  for (uInt i=0; i<coeffs.nelements(); ++i) setCoefficient(i, coeffs[i]);
}

template <class T>
void NQChebyshevParam<T>::setCoefficient(const uInt which,
					 const T &value) {
  if (which >= nparameters()) {
    uInt sz = nparameters();
    FunctionParam<T> cfp(param_p);
    param_p = FunctionParam<T>(which+1);
    for (uInt i=0; i<sz; ++i) {
      param_p[i] = cfp[i];
      param_p.mask(i) =cfp.mask(i);
    };
  };
  param_p[which] = value;
}

template <class T>
const Vector<T> &NQChebyshevParam<T>::getCoefficients() const {
  return param_p.getParameters();
}

template <class T>
void NQChebyshevParam<T>::derivativeCoeffs(Vector<T> &coeffs,
					   const T &xmin, const T &xmax) {
    // first get power series coefficients
    Vector<T> ce(coeffs);
    chebyshevToPower(ce);
    // take the derivative
    Vector<T> &dce = coeffs;
    dce.resize(ce.nelements()-1);
    for (uInt i=1; i<ce.nelements(); ++i) dce(i-1) =
					    T(2*i)*ce(i) / (xmax-xmin);
    // convert back to NQChebyshevParam
    powerToNQChebyshev(dce);
}

template <class T>
void NQChebyshevParam<T>::powerToNQChebyshev(Vector<T> &coeffs) {
  uInt n = coeffs.nelements();
  // Create an inverse transformation matrix
  Matrix<T> poly(n, n, T(0));
  poly(0,0) = T(1);
  poly(1,1) = T(1);
  T scale;
  for (uInt i=2; i<n; i++) {
    scale = T(1) / pow(T(2), T(i-1));
    Int j;
    uInt k;
    for (j=i, k=1; j>1; j-=2, k++) {
      poly(j,i) = scale;
      scale *= T((i - k + 1) / k);
    };
    poly(j,i) = scale;
    if (j == 0) poly(j,i) /= 2;
  };
  // multiply transformation matrix by coefficient vector
  for (uInt i=0; i<n; i++) {
    coeffs(i) *= poly(i,i);
    for (uInt k=i+2; k<n; k += 2) coeffs(i) += poly(i,k)*coeffs(k);
  };
}

template <class T>
void NQChebyshevParam<T>::chebyshevToPower(Vector<T> &coeffs) {
  uInt n = coeffs.nelements();
  // Create a transformation matrix
  Matrix<T> cheb(n, n, T(0));
  cheb(0,0) = T(1);
  cheb(1,1) = T(1);
  for (uInt i=2; i<n; i++) {
    for (Int j=i; j>0; j -= 2) {
      if (j > 1) cheb(j-2,i) -= cheb(j-2,i-2);
      cheb(j,i) += T(2)*cheb(j-1,i-1);
    };
  };
  // multiply transformation matrix by coefficient vector
  for (uInt i=0; i<n; i++) {
    coeffs(i) *= cheb(i,i);
    for (uInt k=i+2; k<n; k += 2) coeffs(i) += cheb(i,k)*coeffs(k);
  };
}

