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
//# $Id$

//# Includes
#include <components/SpectralComponents/LorentzianSpectralElement.h>

#include <casa/BasicSL/Constants.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

LorentzianSpectralElement::LorentzianSpectralElement()
: PCFSpectralElement() {
	Vector<Double> p(3);
	p[0] = 1;
	p[1] = 0;
	p[2] = 1;
	_construct(SpectralElement::LORENTZIAN, p);
}

LorentzianSpectralElement::LorentzianSpectralElement(
	const Double ampl,
	const Double center, const Double fwhm
) : PCFSpectralElement() {
	if (ampl == 0) {
		throw AipsError("Lorentzian amplitude cannot equal 0");
	}
	if (fwhm == 0) {
		throw AipsError("Lorentzian fwhm cannot equal 0");
	}
	Vector<Double> param(3);
	param(0) = ampl;
	param(1) = center;
	param(2) =  fwhm > 0 ? fwhm : -fwhm;
	_construct(SpectralElement::LORENTZIAN, param);
}

LorentzianSpectralElement::LorentzianSpectralElement(
	const Vector<Double>& param
) : PCFSpectralElement() {
    if (param.nelements() != 3) {
    	throw AipsError(
    		String(__FUNCTION__) +  ": LORENTZIAN must have "
    		"3 parameters"
    	);
    }
	if (param[0] == 0) {
		throw AipsError("Lorentzian amplitude cannot equal 0");
	}
	if (param[2] == 0) {
		throw AipsError("Lorentzian fwhm cannot equal 0");
	}
	Vector<Double> p = param.copy();
	if (p[2] < 0) {
		p[2] = -p[2];
	}
	_construct(SpectralElement::LORENTZIAN, p);
}

LorentzianSpectralElement::LorentzianSpectralElement(
	const LorentzianSpectralElement &other
) : PCFSpectralElement(other) {}

LorentzianSpectralElement::~LorentzianSpectralElement() {}

SpectralElement* LorentzianSpectralElement::clone() const {
	return new LorentzianSpectralElement(*this);
}

LorentzianSpectralElement& LorentzianSpectralElement::operator=(
	const LorentzianSpectralElement& other
) {
	if (this != &other) {
		SpectralElement::operator=(other);
	}
	return *this;
}

Double LorentzianSpectralElement::operator()(const Double x) const {
	Vector<Double> p = get();
	Double value = 2*(x - p[1])/p[2];
	return p[0] / ((1.0) + value*value);
}

Double LorentzianSpectralElement::getIntegral() const {
	return getAmpl()*getFWHM()*C::pi_2;
}

ostream &operator<<(ostream &os, const LorentzianSpectralElement &elem) {
	os << SpectralElement::fromType((elem.getType())) << " element: " << endl;
    os << "  Amplitude: " << elem.getAmpl() << ", " << elem.getAmplErr();
    if (elem.fixedAmpl()) os << " (fixed)";
    os << endl << "  Center:    " << elem.getCenter() << ", " << elem.getCenterErr();
    if (elem.fixedCenter()) os << " (fixed)";
    os << endl << "  FWHM:     " << elem.getFWHM() << ", " << elem.getFWHMErr();
    if (elem.fixedFWHM()) os << " (fixed)";
    os << endl;
    return os;
}

} //# NAMESPACE CASA - END

