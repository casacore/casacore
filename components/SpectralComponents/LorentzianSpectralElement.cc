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

#include <scimath/Functionals/Lorentzian1D.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

LorentzianSpectralElement::LorentzianSpectralElement()
: PCFSpectralElement(SpectralElement::LORENTZIAN, Vector<Double>(3)) {
	_setFunction(
		CountedPtr<Lorentzian1D<Double> >(
			new Lorentzian1D<Double>(1)
		)
	);
	setAmpl(1);
	setCenter(0);
	setFWHM(1);
}

LorentzianSpectralElement::LorentzianSpectralElement(
	const Double ampl,
	const Double center, const Double fwhm
) : PCFSpectralElement(SpectralElement::LORENTZIAN, ampl, center, fwhm) {
	if (fwhm == 0) {
		throw AipsError("Lorentzian fwhm cannot equal 0");
	}
	_setFunction(
		CountedPtr<Lorentzian1D<Double> >(
			new Lorentzian1D<Double>(ampl, center, fwhm)
		)
	);
}

LorentzianSpectralElement::LorentzianSpectralElement(
	const Vector<Double>& param
) : PCFSpectralElement(SpectralElement::LORENTZIAN, param) {
	if (param[2] == 0) {
		throw AipsError("Lorentzian fwhm cannot equal 0");
	}
	_setFunction(
		CountedPtr<Lorentzian1D<Double> >(
			new Lorentzian1D<Double>(param[AMP], param[CENTER], param[WIDTH])
		)
	);
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

