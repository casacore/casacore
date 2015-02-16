//# GenericL2Fit.cc: Generic base lass for least-squares fit.
//#
//# Copyright (C) 2001,2002,2003,2004,2005
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

#ifndef SCIMATH_GENERICL2FIT_TCC
#define SCIMATH_GENERICL2FIT_TCC

//# Includes
#include <casacore/scimath/Fitting/GenericL2Fit.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/scimath/Functionals/HyperPlane.h>

namespace casacore {  //# Begin namespace casa
//# Constants
// Default svd collinearity

template<class T>
GenericL2Fit<T>::GenericL2Fit() :
  LSQaips(),
  COLLINEARITY(1e-8),
  aCount_ai(0),
  svd_p(False), ptr_derive_p(0),
  constrFun_p(), constrArg_p(), constrVal_p(),
  pCount_p(0), ndim_p(0),
  needInit_p(True), solved_p(False),
  errors_p(False), ferrors_p(False),
  asweight_p(False), nr_p(0), 
  condEq_p(0), fullEq_p(0), arg_p(0), sol_p(0), fsol_p(0),
  err_p(0), ferr_p(0),
  valder_p(typename FunctionTraits<T>::DiffType(0)), consvd_p(0) {
  if (!svd_p) set(0.0);
}

template<class T>
GenericL2Fit<T>::GenericL2Fit(const GenericL2Fit &other) :
  LSQaips(other),
  COLLINEARITY(1e-8),
  aCount_ai(other.aCount_ai),
  svd_p(other.svd_p), ptr_derive_p(0),
  constrFun_p(other.constrFun_p.nelements()),
  constrArg_p(other.constrArg_p.nelements()),
  constrVal_p(other.constrVal_p.nelements()),
  pCount_p(other.pCount_p), ndim_p(other.ndim_p),
  needInit_p(other.needInit_p), solved_p(other.solved_p),
  errors_p(other.errors_p), ferrors_p(other.ferrors_p),
  asweight_p(other.asweight_p) , nr_p(other.nr_p),
  condEq_p(0), fullEq_p(0), arg_p(0), sol_p(0), fsol_p(0),
  err_p(0), ferr_p(0),
  valder_p(typename FunctionTraits<T>::DiffType(0)), consvd_p(0) {
  if (other.ptr_derive_p) ptr_derive_p = other.ptr_derive_p->clone();
  for (uInt i=0; i<other.constrFun_p.nelements(); ++i)
    constrFun_p[i] = other.constrFun_p[i]->clone();
  for (uInt i=0; i<other.constrArg_p.nelements(); ++i)
    constrArg_p[i] =
      new Vector<typename FunctionTraits<T>::BaseType>
      (other.constrArg_p[i]->copy());
  for (uInt i=0; i<other.constrVal_p.nelements(); ++i)
    constrVal_p[i] =
      new typename FunctionTraits<T>::BaseType(*(other.constrVal_p[i]));
  condEq_p = other.condEq_p;
  fullEq_p = other.fullEq_p;
  arg_p = other.arg_p;
  sol_p = other.sol_p;
  fsol_p = other.fsol_p;
  err_p = other.err_p;
  ferr_p = other.ferr_p;
  valder_p = other.valder_p;
  consvd_p = other.consvd_p;
}

template<class T>
GenericL2Fit<T> &GenericL2Fit<T>::operator=(const GenericL2Fit &other) {
  if (this != &other) {
    LSQaips::operator=(other);
    aCount_ai = other.aCount_ai;
    svd_p = other.svd_p;
    if (other.ptr_derive_p) ptr_derive_p = other.ptr_derive_p->clone();
    else ptr_derive_p = 0;
    constrFun_p.resize(other.constrFun_p.nelements());
    for (uInt i=0; i<other.constrFun_p.nelements(); ++i)
      constrFun_p[i] = other.constrFun_p[i]->clone();
    constrArg_p.resize(other.constrArg_p.nelements());
    for (uInt i=0; i<other.constrArg_p.nelements(); ++i)
      constrArg_p[i] =
	new Vector<typename FunctionTraits<T>::BaseType>
	(other.constrArg_p[i]->copy());
    constrVal_p.resize(other.constrVal_p.nelements());
    for (uInt i=0; i<other.constrVal_p.nelements(); ++i)
      constrVal_p[i] =
	new typename FunctionTraits<T>::BaseType(*(other.constrVal_p[i]));
    pCount_p = other.pCount_p;
    ndim_p = other.ndim_p;
    needInit_p = other.needInit_p;
    solved_p = other.solved_p;
    errors_p = other.errors_p;
    ferrors_p = other.ferrors_p;
    asweight_p = other.asweight_p;
    nr_p = other.nr_p;
    condEq_p = other.condEq_p;
    fullEq_p = other.fullEq_p;
    arg_p = other.arg_p;
    sol_p    = other.sol_p;
    fsol_p    = other.fsol_p;
    err_p    = other.err_p;
    ferr_p    = other.ferr_p;
    valder_p = other.valder_p;
    consvd_p = other.consvd_p;
  }
  return *this;
}

template<class T>
GenericL2Fit<T>::~GenericL2Fit() {
  resetFunction();
}

template<class T>
void GenericL2Fit<T>::
setFunctionEx() {
  pCount_p = ptr_derive_p->nparameters();
  aCount_ai = ptr_derive_p->parameters().nMaskedParameters();
  ndim_p = ptr_derive_p->ndim();
  initfit_p(aCount_ai);
}

template<class T>
Bool GenericL2Fit<T>::
setConstraintEx(const uInt n,
		const Vector<typename FunctionTraits<T>::BaseType> &x,
		const typename FunctionTraits<T>::BaseType y) {
  delete constrArg_p[n]; constrArg_p[n] = 0;
  constrArg_p[n] = new Vector<typename FunctionTraits<T>::BaseType>
    (x.copy());
  delete constrVal_p[n]; constrVal_p[n] = 0;
  constrVal_p[n] = new typename FunctionTraits<T>::BaseType(y);
  for (uInt i=0; i<pCount_p; ++i) {
    (*constrFun_p[n])[i] = typename FunctionTraits<T>::DiffType
      ((*constrFun_p[n])[i].value(), pCount_p, i);
  }
  return True;
}

template<class T>
Bool GenericL2Fit<T>::
setConstraint(const uInt n,
	      const Vector<typename FunctionTraits<T>::BaseType> &x,
	      const typename FunctionTraits<T>::BaseType y) {
  if (!ptr_derive_p) return False;
  HyperPlane<typename FunctionTraits<T>::DiffType>
    function(ptr_derive_p->nparameters());
  return setConstraint(n, function, x, y);
}

template<class T>
Bool GenericL2Fit<T>::
setConstraint(const uInt n,
	      const typename FunctionTraits<T>::BaseType y) {
  if (!ptr_derive_p) return False;
  HyperPlane<typename FunctionTraits<T>::DiffType>
    function(ptr_derive_p->nparameters());
  Vector<typename FunctionTraits<T>::BaseType> x(function.ndim());
  return setConstraint(n, function, x, y);
}

template<class T>
Bool GenericL2Fit<T>::
addConstraint(const Function<typename FunctionTraits<T>::DiffType> &function,
	      const Vector<typename FunctionTraits<T>::BaseType> &x,
	      const typename FunctionTraits<T>::BaseType y) {
  uInt n = constrFun_p.nelements();
  constrFun_p.resize(n+1); constrFun_p[n] = 0;
  constrArg_p.resize(n+1); constrArg_p[n] = 0;
  constrVal_p.resize(n+1); constrVal_p[n] = 0;
  return setConstraint(n, function, x, y);
}

template<class T>
Bool GenericL2Fit<T>::
addConstraint(const Vector<typename FunctionTraits<T>::BaseType> &x,
	      const typename FunctionTraits<T>::BaseType y) {
  if (!ptr_derive_p) return False;
  HyperPlane<typename FunctionTraits<T>::DiffType>
    function(ptr_derive_p->nparameters());
  return addConstraint(function, x, y);
}

template<class T>
Bool GenericL2Fit<T>::
addConstraint(const typename FunctionTraits<T>::BaseType y) {
  if (!ptr_derive_p) return False;
  HyperPlane<typename FunctionTraits<T>::DiffType>
    function(ptr_derive_p->nparameters());
  Vector<typename FunctionTraits<T>::BaseType> x(function.ndim());
  return addConstraint(function, x, y);
}

template<class T>
void GenericL2Fit<T>::asSVD(const Bool svd) {
  svd_p = svd;
  if (!svd_p) set(0.0);
  else set(COLLINEARITY);
}

template<class T>
void GenericL2Fit<T>::
setParameterValues(const Vector<typename FunctionTraits<T>::BaseType> &parms) {
  for (uInt i=0; i<pCount_p; ++i) (*ptr_derive_p)[i].value() = parms[i];
}

template<class T>
void GenericL2Fit<T>::setMaskedParameterValues
(const Vector<typename FunctionTraits<T>::BaseType> &parms) {
  for (uInt i=0, k=0; i<pCount_p; ++i) {
    if (ptr_derive_p->mask(i)) (*ptr_derive_p)[i].value() = parms[k++];
  }
}

template<class T>
Vector<typename LSQTraits<typename FunctionTraits<T>::
BaseType>::base> GenericL2Fit<T>::getSVDConstraint(uInt n) {
  Vector<typename LSQTraits<typename FunctionTraits<T>::
    BaseType>::base> tmp(pCount_p, 0.0);
  if (n >= consvd_p.nelements()) {
    throw(AipsError("GenericL2Fit::getSVDConstraint(n)"
		    " -- Illegal constraint number"));
  }
  for (uInt i=0, k=0; i<pCount_p; ++i) {
    if (ptr_derive_p->mask(i)) tmp[i] = consvd_p[n][k++];
  }
  return tmp;
}

template<class T>
Vector<typename FunctionTraits<T>::BaseType> GenericL2Fit<T>::
fit(const Vector<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y,
    const Vector<typename FunctionTraits<T>::BaseType> &sigma,
    const Vector<Bool> *const mask) {
  fitIt(fsol_p, x, y, &sigma, mask);
  return fsol_p;
}

template<class T>
Vector<typename FunctionTraits<T>::BaseType> GenericL2Fit<T>::
fit(const Matrix<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y,
    const Vector<typename FunctionTraits<T>::BaseType> &sigma,
    const Vector<Bool> *const mask) {
  fitIt(fsol_p, x, y, &sigma, mask);
  return fsol_p;
}

template<class T>
Vector<typename FunctionTraits<T>::BaseType> GenericL2Fit<T>::
fit(const Vector<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y,
    const Vector<Bool> *const mask) {
  fitIt(fsol_p, x, y,
	static_cast<const Vector<typename FunctionTraits<T>::BaseType>
	*const>(0), mask);
  return fsol_p;
}

template<class T>
Vector<typename FunctionTraits<T>::BaseType> GenericL2Fit<T>::
fit(const Matrix<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y,
    const Vector<Bool> *const mask) {
  fitIt(fsol_p, x, y,
	static_cast<const Vector<typename FunctionTraits<T>::BaseType> 
	*const>(0), mask);
  return fsol_p;
}

template<class T>
Vector<typename FunctionTraits<T>::BaseType> GenericL2Fit<T>::
fit(const Vector<Bool> *const mask) {
  fit(fsol_p, mask);
  return fsol_p;
}

template<class T>
Bool GenericL2Fit<T>::
fit(Vector<typename FunctionTraits<T>::BaseType> &sol,
    const Vector<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y,
    const Vector<typename FunctionTraits<T>::BaseType> &sigma,
    const Vector<Bool> *const mask) {
  return fitIt(sol, x, y, &sigma, mask);
}

template<class T>
Bool GenericL2Fit<T>::
fit(Vector<typename FunctionTraits<T>::BaseType> &sol, 
    const Matrix<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y,
    const Vector<typename FunctionTraits<T>::BaseType> &sigma,
    const Vector<Bool> *const mask) {
  return fitIt(sol, x, y, &sigma, mask);
}

template<class T>
Bool GenericL2Fit<T>::
fit(Vector<typename FunctionTraits<T>::BaseType> &sol, 
    const Vector<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y, 
    const typename FunctionTraits<T>::BaseType &,
    const Vector<Bool> *const mask) {
  return fitIt(sol, x, y,
	       static_cast<const Vector<typename FunctionTraits<T>::BaseType>
	       *const>(0), mask);
}

template<class T>
Bool GenericL2Fit<T>::
fit(Vector<typename FunctionTraits<T>::BaseType> &sol, 
    const Matrix<typename FunctionTraits<T>::BaseType> &x, 
    const Vector<typename FunctionTraits<T>::BaseType> &y, 
    const typename FunctionTraits<T>::BaseType &,
    const Vector<Bool> *const mask) {
  return fitIt(sol, x, y,
	       static_cast<const Vector<typename FunctionTraits<T>::BaseType>
	       *const>(0), mask);
}

template<class T>
Bool GenericL2Fit<T>::
fit(Vector<typename FunctionTraits<T>::BaseType> &,
    const Vector<Bool> *const) {
  throw(AipsError("GenericL2: A001: not implemented yet; ask Wim Brouw"));
  return False;
}

template<class T>
const Vector<typename FunctionTraits<T>::BaseType> &GenericL2Fit<T>::
errors() const {
  if (!errors_p) throw(AipsError("GenericL2Fit: no solution to get errors"));
  if (!ferrors_p) {
    ferrors_p = True;
    ferr_p.resize(pCount_p);
    ferr_p = 0;
    for (uInt i=0, k=0; i<pCount_p; ++i) {
      if (ptr_derive_p->mask(i)) ferr_p[i] = err_p[k++];
    }
  }
  return ferr_p;
}

template<class T>
Bool GenericL2Fit<T>::
errors(Vector<typename FunctionTraits<T>::BaseType> &err) const {
  if (errors_p) {
    if (!ferrors_p) {
      ferrors_p = True;
      ferr_p.resize(pCount_p);
      ferr_p = 0;
      for (uInt i=0, k=0; i<pCount_p; ++i) {
	if (ptr_derive_p->mask(i)) ferr_p[i] = err_p[k++];
      }
    }
    err.resize(ferr_p.nelements());
    err = ferr_p;
  }
  return errors_p;
}

template<class T>
Matrix<Double> GenericL2Fit<T>::compuCovariance() {
  Matrix<Double> tmp;
  compuCovariance(tmp);
  return tmp;
}

template<class T>
void GenericL2Fit<T>::compuCovariance(Matrix<Double> &cov) {
  Double *tmp = new Double[nUnknowns()*nUnknowns()];
  getCovariance(tmp);
  IPosition iw(2, pCount_p, pCount_p);
  if (!(cov.shape().conform(iw) && cov.shape() == iw)) {
    cov.resize();
    cov.resize(iw);
  }
  for (uInt i=0, l=0; i<pCount_p; i++) {
    if (ptr_derive_p->mask(i)) {
      for (uInt j=0, k=0; j<pCount_p; j++) {
	if (ptr_derive_p->mask(j)) cov(j, i) = tmp[nUnknowns()*k++ + l];
	else cov(j, i) = 0;
      }
      l++;
    } else for (uInt j=0; j<pCount_p; j++) cov(j, i) = 0;
  }
  delete [] tmp;
}

template<class T>
void GenericL2Fit<T>::
buildNormalMatrix(const Vector<typename FunctionTraits<T>::BaseType> &x, 
		  const Vector<typename FunctionTraits<T>::BaseType> &y,
		  const Vector<typename FunctionTraits<T>::BaseType> &sigma,
		  const Vector<Bool> *const mask) {
  buildMatrix(x, y, &sigma, mask); 
}

template<class T>
void GenericL2Fit<T>::
buildNormalMatrix(const Matrix<typename FunctionTraits<T>::BaseType> &x, 
		  const Vector<typename FunctionTraits<T>::BaseType> &y,
		  const Vector<typename FunctionTraits<T>::BaseType> &sigma,
		  const Vector<Bool> *const mask) {
  buildMatrix(x, y, &sigma, mask); 
}

template<class T>
void GenericL2Fit<T>::
buildNormalMatrix(const Vector<typename FunctionTraits<T>::BaseType> &x, 
		  const Vector<typename FunctionTraits<T>::BaseType> &y,
		  const Vector<Bool> *const mask) {
  buildMatrix(x, y,
	      static_cast<const Vector<typename FunctionTraits<T>::BaseType>
	      *const>(0), mask); 
}

template<class T>
void GenericL2Fit<T>::
buildNormalMatrix(const Matrix<typename FunctionTraits<T>::BaseType> &x, 
		  const Vector<typename FunctionTraits<T>::BaseType> &y,
		  const Vector<Bool> *const mask) {
  buildMatrix(x, y,
	      static_cast<const Vector<typename FunctionTraits<T>::BaseType>
	      *const>(0), mask); 
}

template<class T>
Bool GenericL2Fit<T>::
residual(Vector<typename FunctionTraits<T>::BaseType> &y,
	 const Array<typename FunctionTraits<T>::BaseType> &x,
	 const Vector<typename FunctionTraits<T>::BaseType> &sol,
	 const Bool model) {
  return buildResidual(y, x, &sol, model);
}

template<class T>
Bool GenericL2Fit<T>::
residual(Vector<typename FunctionTraits<T>::BaseType> &y,
	 const Array<typename FunctionTraits<T>::BaseType> &x,
	 const Bool model) {
  return buildResidual(y, x,
		       static_cast<const Vector
		       <typename FunctionTraits<T>::BaseType> *const>(0),
		       model);
}

template<class T>
void GenericL2Fit<T>::initfit_p(uInt parcnt) {
  if (needInit_p) {
    needInit_p = False;
    solved_p = False;
    errors_p = False;
    ferrors_p = False;
    set(parcnt, 
	typename LSQTraits<typename FunctionTraits<T>::BaseType>::num_type());
    condEq_p.resize(aCount_ai);
    fullEq_p.resize(pCount_p);
    arg_p.resize(ndim_p);
    sol_p.resize(aCount_ai);
    fsol_p.resize(pCount_p);
    err_p.resize(aCount_ai);
    ferr_p.resize(pCount_p);
    valder_p = typename FunctionTraits<T>::DiffType(0, pCount_p);
    if (ptr_derive_p) {
      for (uInt i=0; i<pCount_p; ++i) {
	(*ptr_derive_p)[i] = typename FunctionTraits<T>::DiffType
	  ((*ptr_derive_p)[i].value(), pCount_p, i);
      }
    }
    consvd_p.resize(0);
  }
}

template<class T>
uInt GenericL2Fit<T>::
testInput_p(const Array<typename FunctionTraits<T>::BaseType> &x,
	     const Vector<typename FunctionTraits<T>::BaseType> &y,
	     const Vector<typename FunctionTraits<T>::BaseType> *const sigma) {
  uInt xRows = (x.ndim() == 1 || x.ndim() == 2) ? x.shape()(0) : 0;
  if (xRows*ndim_p != y.nelements()*ndim_p ||
      (sigma && xRows != sigma->nelements())) {
    throw(AipsError("GenericL2Fit::buildNormalMatrix()"
		    " -- Illegal argument Array sizes"));
  }
  xRows = y.nelements();
  initfit_p(aCount_ai);
  return xRows;
}

template<class T>
void GenericL2Fit<T>::resetFunction() {
  delete ptr_derive_p; ptr_derive_p = 0;
  pCount_p = 0;
  ndim_p = 0;
  aCount_ai = 0;
  needInit_p = True;
  solved_p = False;
  errors_p = False;
  ferrors_p = False;
  for (uInt i=0; i<constrFun_p.nelements(); i++) {
    delete constrFun_p[i];  constrFun_p[i]  = 0;
    delete constrArg_p[i];  constrArg_p[i]  = 0;
    delete constrVal_p[i];  constrVal_p[i]  = 0;
  }
  constrFun_p.resize(0);
  constrArg_p.resize(0);
  constrVal_p.resize(0);
  consvd_p.resize(0);
}

template<class T>
typename FunctionTraits<T>::BaseType GenericL2Fit<T>::
getVal_p(const Array<typename FunctionTraits<T>::BaseType> &x,
	 uInt, uInt i) const {
  if (ptr_derive_p) {
    if (x.ndim() == 1) {
      valder_p =
	(*ptr_derive_p)(static_cast<const Vector
			<typename FunctionTraits<T>::BaseType> &>(x)[i]);
    } else {
      const Matrix<typename FunctionTraits<T>::BaseType> &xt =
	static_cast<const Matrix<typename FunctionTraits<T>::BaseType> &>(x);
      for (uInt k=0; k<ndim_p; k++) arg_p[k] = xt.row(i)[k];
      valder_p = (*ptr_derive_p)(arg_p);
    }
  }
  valder_p.derivatives(fullEq_p);
  return valder_p.value();
}

template<class T>
void GenericL2Fit<T>::
buildMatrix(const Array<typename FunctionTraits<T>::BaseType> &x, 
	    const Vector<typename FunctionTraits<T>::BaseType> &y,
	    const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
	    const Vector<Bool> *const mask) {
  if (!needInit_p) needInit_p = solved_p;
  uInt nrows = testInput_p(x, y, sigma);
  typename FunctionTraits<T>::BaseType b(0.0);
  typename FunctionTraits<T>::BaseType sig(1.0);
  VectorSTLIterator<typename FunctionTraits<T>::BaseType> ceqit(condEq_p);
  ptr_derive_p->lockParam(); // Parameters will not change during loop
  for (uInt i=0; i<nrows; i++) {
    if (mask && !((*mask)[i])) continue;
    if (sigma) {
      if ((*sigma)[i] == typename FunctionTraits<T>::BaseType(0) ||
	  (*sigma)[i] == typename FunctionTraits<T>::BaseType(-1)) continue;
      sig = (*sigma)[i];
      if (!asweight_p) {
	sig = abs(typename FunctionTraits<T>::BaseType(1.0)/sig); 
	sig *= sig;
      }
    }
    if (ptr_derive_p) {
      b = y(i) - getVal_p(x, 0, i);
      for (uInt j=0, k=0; j<pCount_p; j++) {
	if (ptr_derive_p->mask(j)) condEq_p[k++] = fullEq_p[j];
      }
    }
    makeNorm(ceqit, abs(sig), b);
  }
  ptr_derive_p->unlockParam();
}

template<class T>
void GenericL2Fit<T>::buildConstraint() {
  VectorSTLIterator<typename FunctionTraits<T>::BaseType> ceqit(condEq_p);
  for (uInt i=0; i<constrFun_p.nelements(); ++i) {	// all constraints
    // Copy parameters from function to be fitted
    for (uInt j=0; j<pCount_p; ++j) (*constrFun_p[i])[j].value() =
				      (*ptr_derive_p)[j].value();
    typename FunctionTraits<T>::BaseType b(*constrVal_p[i]); // known value
    // Get arguments
    carg_p.resize(constrArg_p[i]->nelements());
    for (uInt k=0; k<constrArg_p[i]->nelements(); ++k) carg_p[k] =
							 (*constrArg_p[i])[k];
    // calculate constraint equations
    valder_p = (*constrFun_p[i])(carg_p);
    valder_p.derivatives(fullEq_p);
    b -= valder_p.value();
    for (uInt j=0, k=0; j<pCount_p; ++j) {
      if (ptr_derive_p->mask(j)) condEq_p[k++] = fullEq_p[j];
    }
    if (i<nConstraints()) LSQFit::setConstraint(i, ceqit, b);
    else LSQFit::addConstraint(ceqit, b);
  }
}

template<class T>
void GenericL2Fit<T>::fillSVDConstraints() {
  uInt n=LSQFit::getDeficiency();
  consvd_p.resize(n);
  for (uInt i=0; i<n; ++i) {
    consvd_p[i].resize(aCount_ai);
    VectorSTLIterator<typename LSQTraits<typename FunctionTraits<T>::
      BaseType>::base> conit(consvd_p[i]);
    LSQFit::getConstraint(i, conit);
  }
}

template<class T>
Bool GenericL2Fit<T>::
buildResidual(Vector<typename FunctionTraits<T>::BaseType> &y, 
	      const Array<typename FunctionTraits<T>::BaseType> &x,
	      const Vector<typename FunctionTraits<T>::BaseType> *const sol,
	      const Bool model) {
  uInt nrows = testInput_p(x, y,
			   static_cast<const Vector
			   <typename FunctionTraits<T>::BaseType> *const>(0));
  if (sol && sol->nelements() != pCount_p) return False;
  for (uInt i=0; i<nrows; i++) {
    if (ptr_derive_p) {
      if (model) y[i] = typename FunctionTraits<T>::BaseType(0);
      y[i] -= getVal_p(x, 0, i);
      if (sol) {
	for (uInt j=0; j<pCount_p; j++) {
	  if (ptr_derive_p->mask(j)) y[i] -= sol->operator()(j) * fullEq_p[j];
	}
      }
    }
    if (model) y[i] = -y[i];
  }
  return True;
}

} //#End namesapce casa

#endif
