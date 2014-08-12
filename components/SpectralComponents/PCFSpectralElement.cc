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
#include <scimath/Functionals/Gaussian1D.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN


//# Constructors
PCFSpectralElement::PCFSpectralElement()
: SpectralElement() {
	_initFunction();
}

PCFSpectralElement::PCFSpectralElement(
	SpectralElement::Types type
) : SpectralElement(type, Vector<Double>(3, 1)) {
	_initFunction();
	setCenter(0);
}

PCFSpectralElement::PCFSpectralElement(
	SpectralElement::Types type, const Vector<Double>& param
) : SpectralElement(type, param) {
	if (param.size() != 3) {
		throw AipsError(
			"PCFSpectralElement: PCF function must have "
			"3 parameters"
	    );
	}
	if (param[0] == 0) {
		throw AipsError(
			"PCFSpectralElement: PCF amplitude cannot equal 0"
		);
	}
	_initFunction();

}

PCFSpectralElement::PCFSpectralElement(
	SpectralElement::Types type, Double amp,
	Double center, Double width
) : SpectralElement(type, vector<Double>(3)) {
	_initFunction();
	setAmpl(amp);
	setCenter(center);
	setWidth(width);
}

PCFSpectralElement::PCFSpectralElement(const PCFSpectralElement& other)
: SpectralElement(other) {}

PCFSpectralElement::~PCFSpectralElement() {}

Double PCFSpectralElement::getAmpl() const {
  return get()[AMP];
}

Double PCFSpectralElement::getCenter() const {
  return get()[CENTER];
}

Double PCFSpectralElement::getWidth() const {
	return get()[WIDTH];
}

Double PCFSpectralElement::getAmplErr() const {
	return getError()[AMP];
}

Double PCFSpectralElement::getCenterErr() const {
	return getError()[CENTER];
}

Double PCFSpectralElement::getWidthErr() const {
	return getError()[WIDTH];
}

void PCFSpectralElement::setAmpl(const Double ampl) {
	if (ampl == 0) {
		throw AipsError("PCF amplitude cannot equal 0");
	}
	Vector<Double> p = get();
	p(0) = ampl;
	_set(p);
	Vector<Double> err = getError();
	err[AMP] = 0;
	setError(err);
} 

void PCFSpectralElement::setCenter(const Double center) {
	Vector<Double> p = get();
	p[1] = center;
	_set(p);
	Vector<Double> err = getError();
	err[CENTER] = 0;
	setError(err);
}

void PCFSpectralElement::setWidth(const Double width) {

	Vector<Double> p = get();
	p[2] = width > 0 ? width : -width;
	_set(p);
	Vector<Double> err = getError();
	err[WIDTH] = 0;
	setError(err);
}
void PCFSpectralElement::fixAmpl(const Bool isFixed) {
	Vector<Bool> myFixed = fixed();
	myFixed[AMP] = isFixed;
	fix(myFixed);
}

void PCFSpectralElement::fixCenter(const Bool isFixed) {
	Vector<Bool> myFixed = fixed();
	myFixed[CENTER] = isFixed;
	fix(myFixed);
}

void PCFSpectralElement::fixWidth(const Bool isFixed) {
	Vector<Bool> myFixed = fixed();
	myFixed[WIDTH] = isFixed;
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
		fixWidth(True);
	}
}

Bool PCFSpectralElement::fixedAmpl() const {
	return fixed()[AMP];
}

Bool PCFSpectralElement::fixedCenter() const {
	return fixed()[CENTER];
}

Bool PCFSpectralElement::fixedWidth() const {
	return fixed()[WIDTH];
}

void PCFSpectralElement::set(const Vector<Double>& params) {
	if (params.nelements() != 3) {
		throw AipsError(
			"PCFSpectralElement: PCF must have "
			"3 parameters"
		);
	}
	if (params[AMP] == 0) {
		throw AipsError("PCF amplitude cannot equal 0");
	}
	Vector<Double> p = params.copy();
	if (p[WIDTH] < 0) {
		p[WIDTH] *= -1;
	}
 	_set(p);
}

Double PCFSpectralElement::getIntegralErr() const {
	Double damp = getAmplErr()/getAmpl();
	Double dwidth = getWidthErr()/getWidth();
	return sqrt(damp*damp + dwidth*dwidth) * getIntegral();
}

void PCFSpectralElement::_initFunction() {
	// Just need something we can instantiate, subclasses should set their
	// own functions in their constructors
	_setFunction(
		CountedPtr<Function<Double> >(
			new Gaussian1D<Double>()
		)
	);
}



} //# NAMESPACE CASA - END

