//# NQGaussianNDParam.cc: Multidimensional Gaussian class parameters
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

#include <aips/Functionals/NQGaussianNDParam.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>

template<class T> 
NQGaussianNDParam<T>::NQGaussianNDParam() : 
  Function<T>(6),
  itsDim(2), itsCovariance(2,2), 
  itsFlux2Hgt(pow(T(C::_2pi),T(-1))) {
  setFlux(T(1));
  for (uInt i=0; i<itsDim; ++i) param_p[CENTER+i] = T(0);
  for (uInt i=0; i<itsCovariance.nrow(); i++) {
    for (uInt j=0; j<itsCovariance.ncolumn(); j++) itsCovariance(i,j) = T(0);
    itsCovariance(i,i) = T(1);
  };
}

template<class T> 
NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim) :
  Function<T>((nDim+3)*nDim/2+1),
  itsDim(nDim), itsCovariance(nDim, nDim), 
  itsFlux2Hgt(pow(T(C::_2pi),-T(nDim)/T(2))) { 
  setFlux(T(1));
  for (uInt i=0; i<itsDim; i++) param_p[CENTER+i] = T(0);
  for (uInt i=0; i<itsCovariance.nrow(); i++) {
    for (uInt j=0; j<itsCovariance.ncolumn(); j++) itsCovariance(i,j) = T(0);
    itsCovariance(i,i) = T(1);
  };
}
  
template<class T>
NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, const T &height) :
  Function<T>((nDim+3)*nDim/2+1),
  itsDim(nDim), itsCovariance(nDim, nDim), 
  itsFlux2Hgt(pow(T(C::_2pi),-T(nDim)/T(2))) { 
  param_p[HEIGHT] = height;
  for (uInt i=0; i<nDim; i++) param_p[CENTER+i] = T(0);
  for (uInt i=0; i<itsCovariance.nrow(); i++) {
    for (uInt j=0; j<itsCovariance.ncolumn(); j++) itsCovariance(i,j) = T(0);
    itsCovariance(i,i) = T(1);
  };
}

template<class T> 
NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, const T &height,
					const Vector<T> &mean) :
  Function<T>((nDim+3)*nDim/2+1),
  itsDim(nDim), itsCovariance(nDim, nDim), 
  itsFlux2Hgt(pow(T(C::_2pi),-T(nDim)/T(2))) { 
  param_p[HEIGHT] = height;
  if (mean.nelements() != itsDim) {
    throw(AipsError("NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, "
		    "T height, "
		    "Vector<T> mean) - mean must have nDim values."));
  };
  for (uInt i=0; i<nDim; i++) param_p[CENTER+i] = mean[i];
  for (uInt i=0; i<itsCovariance.nrow(); i++) {
    for (uInt j=0; j<itsCovariance.ncolumn(); j++) itsCovariance(i,j) = T(0);
    itsCovariance(i,i) = T(1);
  };
}

template<class T> 
NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, const T &height, 
					const Vector<T> &mean, 
					const Vector<T> &variance) :
  Function<T>((nDim+3)*nDim/2+1),
  itsDim(nDim), itsCovariance(itsDim, itsDim) { 
  param_p[HEIGHT] = height;
  if (mean.nelements() != itsDim) {
    throw(AipsError("NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, "
		    "T height, "
		    "Vector<T> mean, Vector<T> variance)"
		    " - mean must have nDim values."));
  };  
  if (variance.nelements() != itsDim) {
    throw(AipsError("NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, "
		    "T height, "
		    "Vector<T> mean, Vector<T> variance)"
		    " - variance must have nDim values."));
  };
  for (uInt i=0; i<variance.nelements(); i++) {
    if (variance(i) <= T(0)) {
      throw(AipsError("NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim,"
		      " T height,"
		      "Vector<T> mean, Vector<T> variance) "
		      " - variance must be positive"));
    };
  };
  
  for (uInt i=0; i<nDim; i++) param_p[CENTER+i] = mean[i];
  for (uInt i=0; i<itsCovariance.nrow(); i++) {
    for (uInt j=0; j<itsCovariance.ncolumn(); j++) itsCovariance(i,j) = T(0);
    itsCovariance(i,i) = T(1)/variance(i);
  };
  T det = itsCovariance(0,0);
  for (uInt i=1; i<itsDim; i++) det *= itsCovariance(i,i);
  itsFlux2Hgt = pow(T(C::_2pi), -T(itsDim)/T(2)) * sqrt(det);
}

template<class T> 
NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, const T &height,
					const Vector<T> &mean, 
					const Matrix<T> &covar) :
  Function<T>((nDim+3)*nDim/2+1),
  itsDim(nDim) {
  param_p[HEIGHT] = height;
  if (mean.nelements() != itsDim) {
    throw(AipsError("NQGaussianNDParam<T>::NQGaussianNDParam(uInt nDim, "
		    "T height, "
		    "Vector<T> mean, Matrix<T> covar)"
		    " - mean must have nDim values."));
  };
  for (uInt i=0; i<nDim; i++) param_p[CENTER+i] = mean[i];
  setCovariance(covar);
}

template<class T> 
NQGaussianNDParam<T>::~NQGaussianNDParam() {}

template<class T> 
NQGaussianNDParam<T>::NQGaussianNDParam(const NQGaussianNDParam<T> &other) :
  Function<T>(other),
  itsDim(other.itsDim) {
  itsCovariance = other.itsCovariance;
  itsFlux2Hgt = other.itsFlux2Hgt;
}

template<class T>
NQGaussianNDParam<T> &NQGaussianNDParam<T>::
operator=(const NQGaussianNDParam<T> &other) {
  if (this != &other) {
    Function<T>::operator=(other);
    itsDim = other.itsDim;
    itsCovariance.resize(other.itsCovariance.shape());
    itsCovariance = other.itsCovariance;
    itsFlux2Hgt = other.itsFlux2Hgt;
  };
  return *this;
}

template<class T> 
T NQGaussianNDParam<T>::flux() const {
  return param_p[HEIGHT] / itsFlux2Hgt;
}

template<class T> 
void NQGaussianNDParam<T>::setFlux(const T &flux) {
  param_p[HEIGHT] = flux * itsFlux2Hgt;
}

template<class T> 
Vector<T> NQGaussianNDParam<T>::mean() const {
  Vector<T> m(itsDim);
  for (uInt i=0; i<itsDim; ++i) m = param_p[CENTER+i];
  return m;
}

template<class T> 
void NQGaussianNDParam<T>::setMean(const Vector<T> &mean) {
  if (mean.nelements() != itsDim) {
    throw(AipsError("NQGaussianNDParam<T>::setMean(const Vector<T> &mean)"
		    " - mean must have nDim values."));
  };
  for (uInt i=0; i<itsDim; ++i) param_p[CENTER+i] = mean[i];
}

template<class T> 
Vector<T> NQGaussianNDParam<T>::variance() const {
  Vector<T> variance(itsDim);
  for (uInt i=0; i<itsDim; i++) variance(i) = (covariance())(i,i);
  return variance;
}


template<class T> 
void NQGaussianNDParam<T>::setVariance(const Vector<T> &variance) {
  if (variance.nelements() != itsDim) {
    throw(AipsError("NQGaussianNDParam<T>::setVariance(const Vector<T> "
		    "&variance)"
		    " - variance must have nDim values."));
  };
  // This Matrix should be symmetric positive definite. invertSymPosDef
  // throws an exception if it is not.
  itsCovariance = invertSymPosDef(itsCovariance);
  for (uInt i=0; i<itsDim; i++) itsCovariance(i,i) = variance(i);
  setCovariance(itsCovariance);
}

template<class T> 
Matrix<T> NQGaussianNDParam<T>::covariance() const {
  return invertSymPosDef(itsCovariance);
}

template<class T> 
void NQGaussianNDParam<T>::setCovariance(const Matrix<T> &covar) {
  itsCovariance = covar;
  if (itsCovariance.shape() != IPosition(2,itsDim,itsDim)) {
    throw(AipsError("NQGaussianNDParam<T>::setCovariance("
		    "const Matrix<T> &covar)"
		    " - covariance must have nDim rows and columns"));
  };
  Vector<T> sigma(itsDim);
  for (uInt i=0; i<itsDim; i++)
    if (itsCovariance(i,i) > T(0)) sigma[i] = sqrt(itsCovariance(i,i));
    else throw(AipsError("NQGaussianNDParam<T>::setCovariance"
			 "(const Matrix<T> &covar)"
			 " - variance must be positive"));
  for (uInt i=0; i<itsDim - 1; i++) {
    for (uInt j=i+1; j<itsDim; j++) {
      if (!near(itsCovariance(i,j), itsCovariance(j,i))) {
 	if (near(itsCovariance(j,i), T(0))) {
	  itsCovariance(j,i) = itsCovariance(i,j);
	} else if (near(itsCovariance(i,j), T(0))) {
	  itsCovariance(i,j) = itsCovariance(j,i);
	} else throw(AipsError("NQGaussianNDParam<T>::setCovariance("
			       "const Matrix<T> &covar)"
			       " - covariance Matrix is not symmetric"
			       " or triangular"));
      };
      // Now check that each covariance is in a possible range. (-1 < rho < 1)
      if (abs(itsCovariance(i,j)) > sigma[i]*sigma[j]) {
	throw(AipsError("NQGaussianNDParam<T>::setCovariance("
			"const Matrix<T> &covar)"
			" - a covariance entry is too big"));
      };
    };
  };
  // This Matrix should be symmetric positive definite. invertSymPosDef
  // throws an exception if it is not.
  T det;
  invertSymPosDef(itsCovariance, det, itsCovariance);
  itsFlux2Hgt = pow(T(C::_2pi),-T(itsDim)/T(2)) / sqrt(abs(det));
}

template<class T> 
Vector<T> NQGaussianNDParam<T>::unpack(const Matrix<T> &covar) const {
   uInt nDim = covar.nrow();
   Vector<T> result(nDim*(nDim-1)/2);
   uInt k = 0;
   for (uInt row=0; row<nDim-1; row++) {
     for (uInt col=row+1; col<nDim; col++) result(k++) = covar(col,row);
   };
   return result;
}

template<class T> 
void NQGaussianNDParam<T>::repack(Matrix<T> &covar,
				const Vector<T> &params) const {
  uInt nDim = covar.nrow();
  if (nDim*(nDim-1)/2 != params.nelements()) {
    throw(AipsError("NQGaussianNDParam<T>::repack(Matrix<T> &covar, "
 		    "const Vector<T> &params) "
 		    "- params vector is wrong size"));
  };
  uInt k = 0;
  for (uInt row=0; row<nDim-1; row++) {
    for (uInt col=row+1; col<nDim; col++){
      covar(row, col) = covar(col, row) = params(k++);
    };
  };
}
