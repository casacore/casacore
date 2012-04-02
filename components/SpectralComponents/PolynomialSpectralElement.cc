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
#include <components/SpectralComponents/PolynomialSpectralElement.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN


PolynomialSpectralElement::PolynomialSpectralElement(const uInt n)
: SpectralElement() {
	Vector<Double> p(n+1, 0);
	_construct(SpectralElement::POLYNOMIAL, p);
}

PolynomialSpectralElement::PolynomialSpectralElement(const Vector<Double>& param)
: SpectralElement() {
	_construct(SpectralElement::POLYNOMIAL, param);
}

PolynomialSpectralElement::PolynomialSpectralElement(
	const PolynomialSpectralElement &other
) : SpectralElement(other) {}

PolynomialSpectralElement::~PolynomialSpectralElement() {}

SpectralElement* PolynomialSpectralElement::clone() const {
	return new PolynomialSpectralElement(*this);
}

PolynomialSpectralElement& PolynomialSpectralElement::operator=(
	const PolynomialSpectralElement &other
) {
	if (this != &other) {
		SpectralElement::operator=(other);
	}
	return *this;
}

Double PolynomialSpectralElement::operator()(const Double x) const {
	Double s = 0;
	Vector<Double> p = get();
    for (uInt i=0; i<p.size(); i++) {
    	Double prod = 1;
    	for (uInt j=0; j<i; j++) {
    		prod *= x;
    	}
    	s += p[i]*prod;
    }
    return s;
}

uInt PolynomialSpectralElement::getDegree() const {
	return get().size() - 1;
}

ostream &operator<<(ostream &os, const PolynomialSpectralElement &elem) {
	os << SpectralElement::fromType((elem.getType())) << " element: " << endl;
    os << "  Degree:    " << elem.getDegree() << endl;
    return os;
}

} //# NAMESPACE CASA - END

