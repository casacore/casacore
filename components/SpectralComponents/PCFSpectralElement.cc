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
#include <components/SpectralComponents/PCFSpectralElement.h>

#include <casa/BasicSL/Constants.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN


//# Constructors
PCFSpectralElement::PCFSpectralElement()
: SpectralElement() {
}

PCFSpectralElement::PCFSpectralElement(const PCFSpectralElement& other)
: SpectralElement(other) {}

PCFSpectralElement::~PCFSpectralElement() {}


/*
SpectralElement* PCFSpectralElement::clone() const {
	return new PCFSpectralElement(*this);
}
*/

/*
PCFSpectralElement& PCFSpectralElement::operator=(
	const PCFSpectralElement &other
) {
	if (this != &other) {
		SpectralElement::operator=(other);
	}
	return *this;
}
*/

Double PCFSpectralElement::getAmpl() const {
  return get()[0];
}

Double PCFSpectralElement::getCenter() const {
  return get()[1];
}

Double PCFSpectralElement::getFWHM() const {
	return get()[2];
}

Double PCFSpectralElement::getAmplErr() const {
	return getError()[0];
}

Double PCFSpectralElement::getCenterErr() const {
	return getError()[1];
}

Double PCFSpectralElement::getFWHMErr() const {
	return getError()[2];
}

void PCFSpectralElement::setAmpl(const Double ampl) {
	if (ampl == 0) {
		throw AipsError("PCF amplitude cannot equal 0");
	}
	Vector<Double> p = get();
	p(0) = ampl;
	_set(p);
	Vector<Double> err = getError();
	err[0] = 0;
	setError(err);
} 

void PCFSpectralElement::setCenter(const Double center) {
	Vector<Double> p = get();
	p[1] = center;
	_set(p);
	Vector<Double> err = getError();
	err[1] = 0;
	setError(err);
}

void PCFSpectralElement::setFWHM(const Double fwhm) {

	Vector<Double> p = get();
	p[2] = fwhm > 0 ? fwhm : -fwhm;
	_set(p);
	Vector<Double> err = getError();
	err[2] = 0;
	setError(err);
}
void PCFSpectralElement::fixAmpl(const Bool isFixed) {
	Vector<Bool> myFixed = fixed();
	myFixed[0] = isFixed;
	fix(myFixed);
}

void PCFSpectralElement::fixCenter(const Bool isFixed) {
	Vector<Bool> myFixed = fixed();
	myFixed[1] = isFixed;
	fix(myFixed);
}

void PCFSpectralElement::fixFWHM(const Bool isFixed) {
	Vector<Bool> myFixed = fixed();
	myFixed[2] = isFixed;
	fix(myFixed);
}

void PCFSpectralElement::fixByString(const String& s) {
	String fix(s);
	fix.downcase();
	if (fix.contains("p")) {
		fixAmpl(True);
	}
	if (fix.contains("c")) {
		fixCenter(True);
	}
	if (fix.contains("f")) {
		fixFWHM(True);
	}
}

Bool PCFSpectralElement::fixedAmpl() const {
	return fixed()[0];
}

Bool PCFSpectralElement::fixedCenter() const {
	return fixed()[1];
}

Bool PCFSpectralElement::fixedFWHM() const {
	return fixed()[2];
}

void PCFSpectralElement::set(const Vector<Double>& params) {
	if (params.nelements() != 3) {
		throw AipsError(
			"PCFSpectralElement: PCF must have "
			"3 parameters"
		);
	}
	if (params[0] == 0) {
		throw AipsError("PCF amplitude cannot equal 0");
	}
	Vector<Double> p = params.copy();
	if (p[2] < 0) {
		p[2] = -p[2];
	}
 	_set(p);
}

Double PCFSpectralElement::getIntegralErr() const {
	Double amp = getAmpl();
	Double ampErr = getAmplErr();
	Double fwhm = getFWHM();
	Double fwhmErr = getFWHMErr();
	return sqrt(ampErr*ampErr/amp/amp + fwhmErr*fwhmErr/fwhm/fwhm) * getIntegral();
}


} //# NAMESPACE CASA - END

