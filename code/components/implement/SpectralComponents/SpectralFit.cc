//# SpectralFit.cc: Least Squares fitting of spectral elements to spectrum
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

//# Includes
#include <trial/Wnbt/SpectralFit.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Functionals/SumFunction.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Functionals/Polynomial.h>
#include <trial/Wnbt/SpectralElement.h>
#include <trial/Fitting/NonLinearFitLM.h>
#include <trial/Functionals/FuncWithAutoDerivs.h>

//# Constructors
SpectralFit::SpectralFit() :
  n_p(0), cap_p(0), el_p(0) {}

SpectralFit::SpectralFit(uInt n) :
  n_p(n), cap_p(0), el_p(0) {
  capacity();
}

SpectralFit::SpectralFit(const SpectralFit &other) :
  n_p(other.n_p), cap_p(0), el_p(0) {
  capacity();
  for (uInt i=0; i<n_p; i++) el_p[i] = other.el_p[i];
}

SpectralFit::~SpectralFit() {
  delete [] el_p;
};

SpectralFit &SpectralFit::operator=(const SpectralFit &other) {
  if (this != &other) {
    n_p = other.n_p;
    cap_p = 0;
    el_p = 0;
    capacity();
    for (uInt i=0; i<n_p; i++) el_p[i] = other.el_p[i];
  };
  return *this;
}

void SpectralFit::setFitElement(uInt index, const SpectralElement &elem) {
  if (index >= n_p) throw(AipsError("setFitElement illegal index"));
  el_p[index] = elem;
}

void SpectralFit::addFitElement(const SpectralElement &elem) {
  capacity();
  el_p[n_p++] = elem;
}

const SpectralElement &SpectralFit::getElement(uInt index) const {
  if (index >= n_p) throw(AipsError("setFitElement illegal index"));
  return el_p[index];
}

Bool SpectralFit::fit(const Vector<Double> &x,
		      const Vector<Double> &y) {
  // The fitter
  NonLinearFitLM<Double> fitter;
  // The functions to fit
  const Gaussian1D<AutoDiff<Double> > gauss; 
  const Polynomial<AutoDiff<Double> > poly; 
  SumFunction<AutoDiff<Double>,AutoDiff<Double> > func;
  Int npar(0);
  for (uInt i=0; i<n_p; i++) {
    if (el_p[i].getType() == SpectralElement::GAUSSIAN) {
      func.addFunction(gauss);
      npar += 3;
    } else if (el_p[i].getType() == SpectralElement::POLYNOMIAL) {
      npar += el_p[i].getDegree()+1;
      const Polynomial<AutoDiff<Double> > poly(el_p[i].getDegree());
      func.addFunction(poly);
    };
  };
  FuncWithAutoDerivs<Double,Double> autoFunc(func);
  fitter.setFunction(autoFunc);
  // Initial guess
  Vector<Double> v(npar);
  uInt j(0);
  for (uInt i=0; i<n_p; i++) {
    if (el_p[i].getType() == SpectralElement::GAUSSIAN) {
      v(j++) = el_p[i].getAmpl();
      v(j++) = el_p[i].getCenter();
      v(j++) = el_p[i].getSigma();
    } else if (el_p[i].getType() == SpectralElement::POLYNOMIAL) {
      for (uInt k=0; k<el_p[i].getDegree()+1; k++) v(j++) = 0;
    };
  };
  fitter.setFittedFuncParams(v);
  // Max. number of iterations
  fitter.setMaxIter(50+ n_p*10);
  // Convergence criterium
  fitter.setCriteria(0.001);
  // Fit
  Vector<Double> sol;
  Vector<Double> sigma(x.nelements());
  sigma = 1.0;
  sol = fitter.fit(x, y, sigma);
  for (uInt i=0; i<n_p; i++) {
    el_p[i].setAmpl(  sol(3*i +0));
    el_p[i].setCenter(sol(3*i +1));
    el_p[i].setSigma( abs(sol(3*i +2)));
  };
  return fitter.converged();
}

Bool SpectralFit::fit(const Vector<Float> &x,
		      const Vector<Float> &y) {
  // The fitter
  NonLinearFitLM<Float> fitter;
  // The functions to fit
  const Gaussian1D<AutoDiff<Float> > gauss; 
  SumFunction<AutoDiff<Float>,AutoDiff<Float> > func;
  for (uInt i=0; i<n_p; i++) func.addFunction(gauss);
  FuncWithAutoDerivs<Float,Float> autoFunc(func);
  fitter.setFunction(autoFunc);
  // Initial guess
  Vector<Float> v(3*n_p);
  for (uInt i=0; i<n_p; i++) {
    v(3*i +0) = el_p[i].getAmpl();
    v(3*i +1) = el_p[i].getCenter();
    v(3*i +2) = el_p[i].getSigma();
  };
  fitter.setFittedFuncParams(v);
  // Max. number of iterations
  fitter.setMaxIter(50+ n_p*10);
  // Convergence criterium
  fitter.setCriteria(0.001);
  // Fit
  Vector<Float> sol;
  Vector<Float> sigma(x.nelements());
  sigma = 1.0;
  sol = fitter.fit(x, y, sigma);
  for (uInt i=0; i<n_p; i++) {
    el_p[i].setAmpl(  sol(3*i +0));
    el_p[i].setCenter(sol(3*i +1));
    el_p[i].setSigma( abs(sol(3*i +2)));
  };
  return fitter.converged();
}

void SpectralFit::capacity() {
  if (cap_p == 0) {
    cap_p = 10;
    el_p = new SpectralElement[cap_p];
  };
  if (cap_p-n_p < 1) {
    while (cap_p-n_p < 1) cap_p += 10;
    SpectralElement *tmp = new SpectralElement[cap_p];
    for (uInt i=0; i<n_p; i++) tmp[i] = el_p[i];
    delete [] el_p;
    el_p = tmp;
  };
}
