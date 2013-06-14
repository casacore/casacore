//# SpectralElement.cc: Describes (a set of related) spectral lines
//# Copyright (C) 2001,2004
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

#include <components/SpectralComponents/GaussianMultipletSpectralElement.h>

#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <components/SpectralComponents/GaussianSpectralElement.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

#define _ORIGIN  String("GaussianMultipletSpectralElement::") + __FUNCTION__ + ":" + String::toString(__LINE__) + ": "

GaussianMultipletSpectralElement::GaussianMultipletSpectralElement(
	const vector<GaussianSpectralElement>& estimates,
	const Matrix<Double>& constraints
) : CompiledSpectralElement(SpectralElement::GMULTIPLET),
	_gaussians(estimates),_constraints(constraints),
	_paramIndices(estimates.size(), 3, 0) {
	if(estimates.size() != constraints.nrow()+1) {
		throw AipsError(
			_ORIGIN
			+  "Mismatch between size of estimates and constraints"
		);
	}
	if (constraints.ncolumn() != 3) {
		throw AipsError(_ORIGIN +  "constraints does not have 3 columns");
	}
	Matrix<Bool> fixed(LogicalArray(constraints != 0.0));
	for (uInt i=1; i<estimates.size(); i++) {
		if (! estimates[0].fixedAmpl()
			&& estimates[i].fixedAmpl()
			&& fixed(i-1, 0)
		) {
			throw AipsError(_ORIGIN + "You cannot fix the amplitude of a "
				"non-reference Gaussian if the reference Gaussian's "
				"amplitude is not fixed and there is a relationship "
				"between the two amplitudes."
			);
		}
		if (! estimates[0].fixedCenter()
			&& estimates[i].fixedCenter()
			&& fixed(i-1, 1)
		) {
			throw AipsError(_ORIGIN  +  "You cannot fix the center of a non-reference "
				"Gaussian if the reference Gaussian's center is not fixed and there "
				"is a relationship between the two centers."
			);
		}
		if (! estimates[0].fixedWidth()
			&& estimates[i].fixedWidth()
			&& fixed(i-1, 2)
		) {
			throw AipsError(_ORIGIN +  "You cannot fix the width of a non-reference "
				"Gaussian if the reference Gaussian's width is not fixed and there "
				"is a relationship between the two widths."
			);
		}
	}
	ostringstream myfunc;
	myfunc << "p0*exp(-0.5 * (x-p1)*(x-p1) / p2/p2)";
	Vector<Double> parm(3 + nfalse(fixed), 0);
	Vector<Double> errs = parm.copy();
	parm[0] = _gaussians[0].getAmpl();
	parm[1] = _gaussians[0].getCenter();
	parm[2] = _gaussians[0].getSigma();
	errs[0] = _gaussians[0].getAmplErr();
	errs[1] = _gaussians[0].getCenterErr();
	errs[2] = _gaussians[0].getSigmaErr();
	Vector<Bool> f(parm.size(), True);
	f[0] = _gaussians[0].fixedAmpl();
	f[1] = _gaussians[0].fixedCenter();
	f[2] = _gaussians[0].fixedSigma();
	_paramIndices(0, 0) = 0;
	_paramIndices(0, 1) = 1;
	_paramIndices(0, 2) = 2;
	uInt p = 3;
	for (uInt i=0; i<constraints.nrow(); i++) {
		String amp;
		if (constraints(i, 0) != 0) {
			amp = String::toString(constraints(i, 0)) + "*p0";
		}
		else {
			amp = "p" + String::toString(p);
			parm[p] = _gaussians[i+1].getAmpl();
			errs[p] = _gaussians[i+1].getAmplErr();
			f[p] = _gaussians[i+1].fixedAmpl();
			_paramIndices(i+1, 0) = p;
			p++;
		}
		String center;
		if (constraints(i, 1) != 0) {
			center = "x-(p1+(" + String::toString(constraints(i, 1)) + "))";
		}
		else {
			center = "x-p" + String::toString(p);
			parm[p] = _gaussians[i+1].getCenter();
			errs[p] = _gaussians[i+1].getCenterErr();
			f[p] = _gaussians[i+1].fixedCenter();
			_paramIndices(i+1, 1) = p;
			p++;

		}
		String sigma;
		if (constraints(i, 2) != 0) {
			sigma = String::toString(constraints(i, 2)) + "*p2";
		}
		else {
			sigma = "p" + String::toString(p);
			parm[p] = _gaussians[i+1].getSigma();
			errs[p] = _gaussians[i+1].getSigmaErr();
			f[p] = _gaussians[i+1].fixedSigma();
			_paramIndices(i+1, 2) = p;
			p++;
		}
		myfunc << " + " << amp << "*exp(-0.5 * (" << center << ")*("
			<< center << ") / (" << sigma << ") / (" << sigma << "))";
	}
	_setFunction(myfunc.str());
	// have to set the GaussianSpectralElement parameters
	set(parm);
	setError(errs);
	fix(f);
}

GaussianMultipletSpectralElement::GaussianMultipletSpectralElement(
	const GaussianMultipletSpectralElement& other
) : CompiledSpectralElement(other), _gaussians(other._gaussians),
	_constraints(other._constraints),
	_paramIndices(other._paramIndices) {}


GaussianMultipletSpectralElement::~GaussianMultipletSpectralElement() {}

SpectralElement* GaussianMultipletSpectralElement::clone() const {
	return new GaussianMultipletSpectralElement(*this);
}

GaussianMultipletSpectralElement& GaussianMultipletSpectralElement::operator=(
	const GaussianMultipletSpectralElement &other
) {
	if (this != &other) {
		CompiledSpectralElement::operator=(other);
		_gaussians = other._gaussians;
		_constraints.resize(other._constraints.shape());
		_constraints = other._constraints.copy();
		_paramIndices.resize(other._paramIndices.shape());
		_paramIndices = other._paramIndices.copy();
	}
	return *this;
}

Bool GaussianMultipletSpectralElement::operator==(
	const GaussianMultipletSpectralElement& other
) const {
	return(
		CompiledSpectralElement::operator==(other)
		&& allTrue(Vector<GaussianSpectralElement>(_gaussians) == Vector<GaussianSpectralElement>(other._gaussians))
		&& allTrue(_constraints == other._constraints)
	);
}

const vector<GaussianSpectralElement>&
GaussianMultipletSpectralElement::getGaussians() const {
	return _gaussians;
}

const Matrix<Double>& GaussianMultipletSpectralElement::getConstraints() const {
	return _constraints;
}

Bool GaussianMultipletSpectralElement::toRecord(RecordInterface& out) const {
	out.define(RecordFieldId("type"), fromType(getType()));
	Record gaussians;
	for (uInt i=0; i<_gaussians.size(); i++) {
		Record gaussian;
		_gaussians[i].toRecord(gaussian);
		gaussians.defineRecord(
			"*" + String::toString(i), gaussian
		);
	}
	out.defineRecord("gaussians", gaussians);
	out.define("fixedMatrix", _constraints);
	return True;
}

void GaussianMultipletSpectralElement::set(const Vector<Double>& param) {
	if (get().size() > 0 && param.size() != get().size()) {
		ostringstream x;
		x << _ORIGIN << "Inconsistent number of parameters. Got "
			<< param.size() << ". Must be " << get().size();
		throw AipsError(x.str());
	}
	SpectralElement::_set(param);
	Double amp0 = param[0];
	Double center0 = param[1];
	Double sigma0 = param[2];
	_gaussians[0].setAmpl(amp0);
	_gaussians[0].setCenter(center0);
	_gaussians[0].setSigma(sigma0);
	uInt p = 3;
	for (uInt i=3; i<_paramIndices.size(); i++) {
		uInt gNumber = i/3;
		uInt pNumber = i%3;
		uInt pIndex = _paramIndices(gNumber, pNumber);
		Double fRel = _constraints(gNumber-1, pNumber);
		if (pNumber == 0) {
			Double amp = pIndex == 0 ? fRel*amp0 : param[p];
			_gaussians[gNumber].setAmpl(amp);
		}
		else if (pNumber == 1) {
			Double center = pIndex == 0 ? fRel+center0 : param[p];
			_gaussians[gNumber].setCenter(center);
		}
		else if (pNumber == 2) {
			Double sigma = pIndex == 0 ? fRel*sigma0 : param[p];
			_gaussians[gNumber].setSigma(sigma);
		}
		if (pIndex > 0) {
			p++;
		}
	}
}

void GaussianMultipletSpectralElement::setError(const Vector<Double> &err) {
	SpectralElement::setError(err);
	Double amp0 = err[0];
	Double center0 = err[1];
	Double sigma0 = err[2];
	Vector<Double> errors(3);
	errors[0] = err[0];
	errors[1] = err[1];
	errors[2] = err[2];
	_gaussians[0].setError(errors);
	uInt p = 3;
	for (uInt i=3; i<_paramIndices.size(); i++) {
		uInt gNumber = i/3;
		uInt pNumber = i%3;
		uInt pIndex = _paramIndices(gNumber, pNumber);
		Double fRel = _constraints(gNumber-1, pNumber);
		if (pNumber == 0) {
			errors[0] = pIndex == 0 ? fRel*amp0 : err[p];
		}
		else if (pNumber == 1) {
			errors[1] = pIndex == 0 ? center0 : err[p];
		}
		else if (pNumber == 2) {
			errors[2] = pIndex == 0 ? fRel*sigma0 : err[p];
		}
		_gaussians[gNumber].setError(errors);

		if (pIndex > 0) {
			p++;
		}
	}
}

void GaussianMultipletSpectralElement::fix(const Vector<Bool>& fix) {
	SpectralElement::fix(fix);
	Bool amp0 = fix[0];
	Bool center0 = fix[1];
	Bool sigma0 = fix[2];
	Vector<Bool> fixed(3);
	fixed[0] = fix[0];
	fixed[1] = fix[1];
	fixed[2] = fix[2];
	_gaussians[0].fix(fixed);
	uInt p = 3;
	for (uInt i=3; i<_paramIndices.size(); i++) {
		uInt gNumber = i/3;
		uInt pNumber = i%3;
		uInt pIndex = _paramIndices(gNumber, pNumber);
		if (pNumber == 0) {
			fixed[0] = pIndex == 0 ? amp0 : fix[p];
		}
		else if (pNumber == 1) {
			fixed[1] = pIndex == 0 ? center0 : fix[p];
		}
		else if (pNumber == 2) {
			fixed[2] = pIndex == 0 ? sigma0 : fix[p];
		}
		_gaussians[gNumber].fix(fixed);
		if (pIndex > 0) {
			p++;
		}
	}
}

ostream &operator<<(ostream& os, const GaussianMultipletSpectralElement& elem) {
	os << SpectralElement::fromType((elem.getType())) << " element: " << endl;
	os << "  Function:    " << elem.getFunction() << endl;
	os << "  Gaussians:" << endl;
	Vector<GaussianSpectralElement> gaussians = elem.getGaussians();
	for (uInt i=0; i<gaussians.size(); i++) {
		os << "Gaussian " << i << ": " << gaussians[i] << endl;
	}
	Matrix<Double> r = elem.getConstraints();
	os << "Constraints: " << endl;
	for (uInt i=1; i<gaussians.size(); i++) {
		for (uInt j=0; j<3; j++) {
			Double v = r(i-1, j);
			if (v != 0) {
				switch (j) {
				case 0:
					os << "  Amplitude ratio of component " << i
						<< " to the reference component is fixed at " << v;
					break;
				case 1:
					os << "  Center offset of component " << i
						<< " to the reference component is fixed at " << v;
					break;
				case 2:
					os << "  FWHM ratio of component " << i
						<< " to the reference component is fixed at " << v;
					break;
				default:
					throw AipsError(_ORIGIN + "Logic Error");
				}
			}
		}
	}
	return os;
}

} //# NAMESPACE CASA - END


