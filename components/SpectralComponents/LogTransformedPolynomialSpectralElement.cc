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
//# $Id: SpectralElement.cc 21024 2011-03-01 11:46:18Z gervandiepen $

#include <components/SpectralComponents/LogTransformedPolynomialSpectralElement.h>

#include <casa/iostream.h>

#define _ORIGIN  String("LogTransformedPolynomialSpectralElement::") + __FUNCTION__ + ":" + String::toString(__LINE__) + ": "


namespace casa { //# NAMESPACE CASA - BEGIN


LogTransformedPolynomialSpectralElement::LogTransformedPolynomialSpectralElement(
	uInt order
) : PolynomialSpectralElement(order) {
	ThrowIf(
		order == 0,
		"order must be greater than zero."
	);
	_setType(SpectralElement::LOGTRANSPOLY);
}

LogTransformedPolynomialSpectralElement::LogTransformedPolynomialSpectralElement(
	const Vector<Double>& param
) : PolynomialSpectralElement(param) {
	set(param);
	_setType(SpectralElement::LOGTRANSPOLY);
}

LogTransformedPolynomialSpectralElement::LogTransformedPolynomialSpectralElement(
	const LogTransformedPolynomialSpectralElement &other
) : PolynomialSpectralElement(other) {}

LogTransformedPolynomialSpectralElement::~LogTransformedPolynomialSpectralElement() {}

LogTransformedPolynomialSpectralElement& LogTransformedPolynomialSpectralElement::operator=(
	const LogTransformedPolynomialSpectralElement& other
) {
	if (this != &other) {
		PolynomialSpectralElement::operator=(other);
	}
	return *this;
}

SpectralElement* LogTransformedPolynomialSpectralElement::clone() const {
	return new LogTransformedPolynomialSpectralElement(*this);
}

ostream &operator<<(
	ostream &os, const LogTransformedPolynomialSpectralElement &elem
) {
	os << SpectralElement::fromType((elem.getType())) << " element: " << endl;
	uInt degree = elem.getDegree();
    os << "  Degree:    " << degree << endl;
    os << "  Function: ln(y) = ln(c0) ";
    ostringstream ss;
    Vector<Double> c = elem.get();
    ss << "c0: " << exp(c[0]) << endl;
    for (uInt i=1; i<=degree; i++) {
    	os << " + c" << i << "*ln(x)";
    	if (i > 1) {
    		os << "**" << i;
    	}
    	ss << "c" << i << ": " << c[i] << endl;
    }
    os << endl;
    os << ss.str();
    return os;
}
}

