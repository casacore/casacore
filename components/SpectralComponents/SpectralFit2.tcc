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

#include <components/SpectralComponents/CompiledSpectralElement.h>
#include <components/SpectralComponents/GaussianSpectralElement.h>
#include <components/SpectralComponents/LorentzianSpectralElement.h>
#include <components/SpectralComponents/PolynomialSpectralElement.h>
#include <scimath/Fitting/NonLinearFitLM.h>
#include <scimath/Functionals/CompiledFunction.h>
#include <scimath/Functionals/CompoundFunction.h>
#include <scimath/Functionals/CompoundParam.h>
#include <scimath/Functionals/Gaussian1D.h>
#include <scimath/Functionals/Lorentzian1D.h>
#include <scimath/Functionals/Polynomial.h>

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
Bool SpectralFit::fit(const Vector<MT> &sigma,
		const Vector<MT> &y,
		const Vector<MT> &x,
		const Vector<Bool> *mask) {
	// The fitter
	NonLinearFitLM<MT> fitter;
	iter_p = 0;
	// The functions to fit
	CompoundFunction<AutoDiff<MT> > func;
	// Initial guess
	uInt npar(0);
	for (uInt i=0; i<slist_p.nelements(); i++) {
		switch(slist_p[i]->getType()) {
		case SpectralElement::GAUSSIAN: {
			Gaussian1D<AutoDiff<MT> > gauss;
			GaussianSpectralElement *x = dynamic_cast<GaussianSpectralElement *>(slist_p[i]);
			gauss[0] = AutoDiff<MT>(x->getAmpl(), gauss.nparameters(), 0);
			gauss[1] = AutoDiff<MT>(x->getCenter(), gauss.nparameters(), 1);
			gauss[2] = AutoDiff<MT>(x->getFWHM(), gauss.nparameters(), 2);
			gauss.mask(0) = ! x->fixedAmpl();
			gauss.mask(1) = ! x->fixedCenter();
			gauss.mask(2) = ! x->fixedFWHM();
			func.addFunction(gauss);
			npar += gauss.nparameters();
		}
		break;
		case SpectralElement::POLYNOMIAL: {
			PolynomialSpectralElement *x = dynamic_cast<PolynomialSpectralElement *>(slist_p[i]);

			npar += x->getDegree()+1;
			Polynomial<AutoDiff<MT> > poly(x->getDegree());
			for (uInt j=0; j<poly.nparameters(); ++j) {
				poly[j] = AutoDiff<MT>(0, poly.nparameters(), j);
				poly.mask(j) = ! x->fixed()(j);
			};
			func.addFunction(poly);
		}
		break;
		case SpectralElement::COMPILED:
			// Allow fall through; these use the same code
		case SpectralElement::GMULTIPLET: {
			CompiledSpectralElement *x = dynamic_cast<CompiledSpectralElement *>(slist_p[i]);
			CompiledFunction<AutoDiff<MT> > comp;
			comp.setFunction(x->getFunction());
			Vector<Double> param;
			x->get(param);
			for (uInt j=0; j<comp.nparameters(); ++j) {
				comp[j] = AutoDiff<MT>(param[j], comp.nparameters(), j);
				comp.mask(j) = ! x->fixed()(j);
			}
			func.addFunction(comp);
		}
		break;
		case SpectralElement::LORENTZIAN: {
			LorentzianSpectralElement *x = dynamic_cast<LorentzianSpectralElement *>(slist_p[i]);
			Lorentzian1D<AutoDiff<MT> > lor;
			lor[0] = AutoDiff<MT>(x->getAmpl(), lor.nparameters(), 0);
			lor[1] = AutoDiff<MT>(x->getCenter(), lor.nparameters(), 1);
			lor[2] = AutoDiff<MT>(x->getFWHM(), lor.nparameters(), 2);
			lor.mask(0) = ! x->fixedAmpl();
			lor.mask(1) = ! x->fixedCenter();
			lor.mask(2) = ! x->fixedFWHM();
			func.addFunction(lor);
			npar += lor.nparameters();
		}
		break;
		default:
			throw AipsError("Unhandled SpectralElement type");
		}
	}
	fitter.setFunction(func);
	// Max. number of iterations
	fitter.setMaxIter(50+ slist_p.nelements()*10);
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
	for (uInt i=0; i<slist_p.nelements(); i++) {
		tmp.resize(slist_p[i]->getOrder());
		terr.resize(slist_p[i]->getOrder());
		for (uInt k=0; k<slist_p[i]->getOrder(); k++) {
			if (k==2 && slist_p[i]->getType() == SpectralElement::GAUSSIAN) {
				tmp(k) = sol(j) / GaussianSpectralElement::SigmaToFWHM;
				terr(k) = err(j++)  / GaussianSpectralElement::SigmaToFWHM;
			} else { /// Get rid of / above after changing element contents
				tmp(k) = sol(j);
				terr(k) = err(j++);
			};
		};
		slist_p[i]->set(tmp);
		slist_p[i]->setError(terr);
	}
	return fitter.converged();
}


} //# NAMESPACE CASA - END

