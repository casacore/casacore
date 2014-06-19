//# SpectralFit2.cc: Least Squares fitting of spectral elements: templated part
//# Copyright (C) 2001,2002,2004
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
#include <components/SpectralComponents/SpectralFit.h>

#include <casa/Utilities/PtrHolder.h>
#include <components/SpectralComponents/CompiledSpectralElement.h>
#include <components/SpectralComponents/GaussianSpectralElement.h>
#include <components/SpectralComponents/LogTransformedPolynomialSpectralElement.h>
#include <components/SpectralComponents/LorentzianSpectralElement.h>
#include <components/SpectralComponents/PolynomialSpectralElement.h>
#include <components/SpectralComponents/PowerLogPolynomialSpectralElement.h>
#include <scimath/Fitting/NonLinearFitLM.h>
#include <scimath/Functionals/CompiledFunction.h>
#include <scimath/Functionals/CompoundFunction.h>
#include <scimath/Functionals/CompoundParam.h>
#include <scimath/Functionals/Gaussian1D.h>
#include <scimath/Functionals/Lorentzian1D.h>
#include <scimath/Functionals/Polynomial.h>
#include <scimath/Functionals/PowerLogarithmicPolynomial.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Templated member functions

template <class MT>
Bool SpectralFit::fit(const Vector<MT> &y,
		      const Vector<MT> &x,
		      const Vector<Bool> *mask) {
  Vector<MT> sigma(x.nelements());
  sigma = 1.0;
  return fit(sigma, y, x, mask);
}

template <class MT>
Bool SpectralFit::fit(
	const Vector<MT> &sigma, const Vector<MT> &y,
	const Vector<MT> &x, const Vector<Bool> *mask
) {
	NonLinearFitLM<MT> fitter;
	iter_p = 0;
	// The functions to fit
	CompoundFunction<AutoDiff<MT> > func;
	uInt ncomps = slist_p.nelements();
	PtrHolder<Function<AutoDiff<MT> > > autodiff;
	for (uInt i=0; i<ncomps; i++) {
		SpectralElement *elem = slist_p[i];
		uInt nparms = elem->getOrder();
		SpectralElement::Types type = slist_p[i]->getType();
		switch(type) {
		case SpectralElement::GAUSSIAN: {
			autodiff.set(new Gaussian1D<AutoDiff<MT> >());
		}
		break;
		case SpectralElement::POLYNOMIAL: {
			PolynomialSpectralElement *x = dynamic_cast<PolynomialSpectralElement *>(elem);
			autodiff.set(new Polynomial<AutoDiff<MT> >(x->getDegree()));
		}
		break;
		case SpectralElement::COMPILED:
			// Allow fall through; these use the same code
		case SpectralElement::GMULTIPLET: {
			CompiledSpectralElement *x = dynamic_cast<CompiledSpectralElement *>(elem);
			autodiff.set(new CompiledFunction<AutoDiff<MT> >());
			dynamic_cast<CompiledFunction<AutoDiff<MT> > *>(
				autodiff.ptr()
			)->setFunction(x->getFunction());
		}
		break;
		case SpectralElement::LORENTZIAN: {
			autodiff.set(new Lorentzian1D<AutoDiff<MT> >());
		}
		break;
		case SpectralElement::POWERLOGPOLY: {
			Vector<Double> parms = elem->get();
			autodiff.set(new PowerLogarithmicPolynomial<AutoDiff<MT> > (nparms));
		}
		break;
		case SpectralElement::LOGTRANSPOLY: {
			LogTransformedPolynomialSpectralElement *x = dynamic_cast<
				LogTransformedPolynomialSpectralElement*
			>(elem);
			// treated as a polynomial for fitting purposes. The caller is responsible for passing the ln's of
			// the ordinate and obscissa values to the fitter.
			autodiff.set(new Polynomial<AutoDiff<MT> > (x->getDegree()));
		}
		break;
		default:
			throw AipsError("SpectralFit::fit(): Logic Error: Unhandled SpectralElement type");
		}
		Vector<Double> parms = elem->get();
		Vector<Bool> fixed = elem->fixed();
		for (uInt j=0; j<nparms; j++) {
			(*autodiff)[j] = AutoDiff<MT>(parms[j], nparms, j);
			if (j == PCFSpectralElement::WIDTH && type == SpectralElement::GAUSSIAN) {
				(*autodiff)[j] *= GaussianSpectralElement::SigmaToFWHM;
			}
			autodiff->mask(j) = ! fixed[j];
		}
		func.addFunction(*autodiff);
	}
	fitter.setFunction(func);
	// Max. number of iterations
	fitter.setMaxIter(50+ ncomps*10);
	// Convergence criterium
	fitter.setCriteria(0.001);
	// Fit
	Vector<MT> sol;
	Vector<MT> err;
	sol = fitter.fit(x, y, sigma, mask);
	err = fitter.errors();
	// Number of iterations
	iter_p = fitter.currentIteration();
	chiSq_p = fitter.getChi2();
	uInt j = 0;
	Vector<Double> tmp, terr;
	for (uInt i=0; i<ncomps; i++) {
		SpectralElement *element = slist_p[i];
		uInt nparms = element->getOrder();
		tmp.resize(nparms);
		terr.resize(nparms);
		SpectralElement::Types type = element->getType();
		for (uInt k=0; k<nparms; k++) {
			Bool convertGaussWidth = k==PCFSpectralElement::WIDTH && type == SpectralElement::GAUSSIAN;
			tmp(k) = convertGaussWidth
				? sol(j) / GaussianSpectralElement::SigmaToFWHM
				: sol(j);
			terr(k) = convertGaussWidth
				? err(j) / GaussianSpectralElement::SigmaToFWHM
				: err(j);
			j++;
		};
		element->set(tmp);
		element->setError(terr);
	}
	return fitter.converged();
}


} //# NAMESPACE CASA - END

