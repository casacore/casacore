//# GaussianNDParam.cc: Multidimensional Gaussian class parameters
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

#ifndef SCIMATH_GAUSSIANNDPARAM_TCC
#define SCIMATH_GAUSSIANNDPARAM_TCC

#include <casacore/scimath/Functionals/GaussianNDParam.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/scimath/Mathematics/MatrixMathLA.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> 
GaussianNDParam<T>::GaussianNDParam() : 
  Function<T>(6), itsDim(2),
  itsFlux2Hgt(pow(T(C::_2pi),T(-1))) {
  setFlux(T(1));
  for (uInt i=0; i<itsDim; ++i) param_p[CENTER+itsDim+i] = T(1);
}

template<class T> 
GaussianNDParam<T>::GaussianNDParam(uInt nDim) :
  Function<T>((nDim+3)*nDim/2+1), itsDim(nDim), 
  itsFlux2Hgt(pow(T(C::_2pi),-T(nDim)/T(2))) { 
  setFlux(T(1));
  for (uInt i=0; i<itsDim; ++i) param_p[CENTER+itsDim+i] = T(1);
}
  
template<class T>
GaussianNDParam<T>::GaussianNDParam(uInt nDim, const T &height) :
  Function<T>((nDim+3)*nDim/2+1), itsDim(nDim),
  itsFlux2Hgt(pow(T(C::_2pi),-T(nDim)/T(2))) { 
  param_p[HEIGHT] = height;
  for (uInt i=0; i<itsDim; ++i) param_p[CENTER+itsDim+i] = T(1);
}

template<class T> 
GaussianNDParam<T>::GaussianNDParam(uInt nDim, const T &height,
					const Vector<T> &mean) :
  Function<T>((nDim+3)*nDim/2+1), itsDim(nDim),
  itsFlux2Hgt(pow(T(C::_2pi),-T(nDim)/T(2))) { 
  param_p[HEIGHT] = height;
  if (mean.nelements() != itsDim) {
    throw(AipsError("GaussianNDParam<T>::GaussianNDParam(uInt nDim, "
		    "T height, "
		    "Vector<T> mean) - mean must have nDim values."));
  }
  for (uInt i=0; i<nDim; i++) {
    param_p[CENTER+i] = mean[i];
    param_p[CENTER+itsDim+i] = T(1);
  }
}

template<class T> 
GaussianNDParam<T>::GaussianNDParam(uInt nDim, const T &height, 
					const Vector<T> &mean, 
					const Vector<T> &variance) :
  Function<T>((nDim+3)*nDim/2+1), itsDim(nDim) {
  param_p[HEIGHT] = height;
  if (mean.nelements() != itsDim) {
    throw(AipsError("GaussianNDParam<T>::GaussianNDParam(uInt nDim, "
		    "T height, Vector<T> mean, Vector<T> variance)"
		    " - mean must have nDim values."));
  }  
  if (variance.nelements() != itsDim) {
    throw(AipsError("GaussianNDParam<T>::GaussianNDParam(uInt nDim, "
		    "T height, Vector<T> mean, Vector<T> variance)"
		    " - variance must have nDim values."));
  }
  for (uInt i=0; i<nDim; i++) {
    param_p[CENTER+i] = mean[i];
    if (variance[i] <= T(0)) {
      throw(AipsError("GaussianNDParam<T>::GaussianNDParam(uInt nDim,"
		      " T height, Vector<T> mean, Vector<T> variance) "
		      " - variance must be positive"));
    }
    param_p[CENTER+itsDim+i] = T(1)/variance[i];
  }
  
  T det = param_p[CENTER+itsDim];
  for (uInt i=1; i<itsDim; i++) det *= param_p[CENTER+itsDim+i];
  itsFlux2Hgt = pow(T(C::_2pi), -T(itsDim)/T(2)) * sqrt(det);
}

template<class T> 
GaussianNDParam<T>::GaussianNDParam(uInt nDim, const T &height,
					const Vector<T> &mean, 
					const Matrix<T> &covar) :
  Function<T>((nDim+3)*nDim/2+1), itsDim(nDim) {
  param_p[HEIGHT] = height;
  if (mean.nelements() != itsDim) {
    throw(AipsError("GaussianNDParam<T>::GaussianNDParam(uInt nDim, "
		    "T height, "
		    "Vector<T> mean, Matrix<T> covar)"
		    " - mean must have nDim values."));
  }
  for (uInt i=0; i<nDim; i++) param_p[CENTER+i] = mean[i];
  setCovariance(covar);
}

template<class T> 
GaussianNDParam<T>::~GaussianNDParam() {}

template<class T> 
GaussianNDParam<T>::GaussianNDParam(const GaussianNDParam<T> &other) :
  Function<T>(other),
  itsDim(other.itsDim), itsFlux2Hgt(other.itsFlux2Hgt) {}

template<class T>
GaussianNDParam<T> &GaussianNDParam<T>::
operator=(const GaussianNDParam<T> &other) {
  if (this != &other) {
    Function<T>::operator=(other);
    itsDim = other.itsDim;
    itsFlux2Hgt = other.itsFlux2Hgt;
  }
  return *this;
}

template<class T> 
T GaussianNDParam<T>::flux() const {
  return param_p[HEIGHT] / itsFlux2Hgt;
}

template<class T> 
void GaussianNDParam<T>::setFlux(const T &flux) {
  param_p[HEIGHT] = flux * itsFlux2Hgt;
}

template<class T> 
Vector<T> GaussianNDParam<T>::mean() const {
  Vector<T> m(itsDim);
  for (uInt i=0; i<itsDim; ++i) m[i] = param_p[CENTER+i];
  return m;
}

template<class T> 
void GaussianNDParam<T>::setMean(const Vector<T> &mean) {
  if (mean.nelements() != itsDim) {
    throw(AipsError("GaussianNDParam<T>::setMean(const Vector<T> &mean)"
		    " - mean must have nDim values."));
  }
  for (uInt i=0; i<itsDim; ++i) param_p[CENTER+i] = mean[i];
}

template<class T> 
Vector<T> GaussianNDParam<T>::variance() const {
  Vector<T> variance(itsDim);
  Matrix<T> locCovariance(covariance());
  for (uInt i=0; i<itsDim; i++) variance[i] = locCovariance(i, i);
  return variance;
}


template<class T> 
void GaussianNDParam<T>::setVariance(const Vector<T> &variance) {
  if (variance.nelements() != itsDim) {
    throw(AipsError("GaussianNDParam<T>::setVariance(const Vector<T> "
		    "&variance)"
		    " - variance must have nDim values."));
  }
  // This Matrix should be symmetric positive definite. invertSymPosDef
  // throws an exception if it is not.
  Matrix<T> locCovariance(itsDim, itsDim);
  repack(locCovariance);
  for (uInt i=0; i<itsDim; i++) locCovariance(i,i) = param_p[CENTER+itsDim+i];
  locCovariance = invertSymPosDef(locCovariance);
  for (uInt i=0; i<itsDim; i++) locCovariance(i,i) = variance[i];
  setCovariance(locCovariance);
}

template<class T> 
Matrix<T> GaussianNDParam<T>::covariance() const {
  Matrix<T> locCovariance(itsDim, itsDim);
  repack(locCovariance);
  return invertSymPosDef(locCovariance);
}

template<class T> 
void GaussianNDParam<T>::setCovariance(const Matrix<T> &covar) {
  Matrix<T> locCovariance(covar.shape());
  locCovariance = covar;
  if (locCovariance.shape() != IPosition(2,itsDim,itsDim)) {
    throw(AipsError("GaussianNDParam<T>::setCovariance("
		    "const Matrix<T> &covar)"
		    " - covariance must have nDim rows and columns"));
  }
  Vector<T> sigma(itsDim);
  for (uInt i=0; i<itsDim; i++) {
    if (locCovariance(i,i) > T(0)) sigma[i] = sqrt(locCovariance(i,i));
    else throw(AipsError("GaussianNDParam<T>::setCovariance"
			 "(const Matrix<T> &covar)"
			 " - variance must be positive"));
  }
  for (uInt i=0; i<itsDim - 1; i++) {
    for (uInt j=i+1; j<itsDim; j++) {
      if (!near(locCovariance(i,j), locCovariance(j,i))) {
 	if (near(locCovariance(j,i), T(0))) {
	  locCovariance(j,i) = locCovariance(i,j);
	} else if (near(locCovariance(i,j), T(0))) {
	  locCovariance(i,j) = locCovariance(j,i);
	} else throw(AipsError("GaussianNDParam<T>::setCovariance("
			       "const Matrix<T> &covar)"
			       " - covariance Matrix is not symmetric"
			       " or triangular"));
      }
      // Now check that each covariance is in a possible range. (-1 < rho < 1)
      if (abs(locCovariance(i,j)) > sigma[i]*sigma[j]) {
	throw(AipsError("GaussianNDParam<T>::setCovariance("
			"const Matrix<T> &covar)"
			" - a covariance entry is too big"));
      }
    }
  }
  // This Matrix should be symmetric positive definite. invertSymPosDef
  // throws an exception if it is not.
  T det;
  invertSymPosDef(locCovariance, det, locCovariance);
  unpack(locCovariance);
  itsFlux2Hgt = pow(T(C::_2pi),-T(itsDim)/T(2)) / sqrt(abs(det));
}

template<class T> 
void GaussianNDParam<T>::unpack(const Matrix<T> &covar) {
   for (uInt row=0, k=0; row<itsDim; ++row) {
     param_p[CENTER+itsDim+row] = covar(row, row);
     for (uInt col=row+1; col<itsDim; ++col) {
       param_p[CENTER+itsDim+itsDim+k++] = covar(col, row);
     }
   }
}

template<class T> 
void GaussianNDParam<T>::repack(Matrix<T> &covar) const {
  for (uInt row=0, k=0; row<itsDim; ++row) {
    covar(row, row) = param_p[CENTER+itsDim+row];
    for (uInt col=row+1; col<itsDim; ++col) {
      covar(row, col) = covar(col, row) = param_p[CENTER+itsDim+itsDim+k++];
    }
  }
}

} //# NAMESPACE CASACORE - END


#endif
