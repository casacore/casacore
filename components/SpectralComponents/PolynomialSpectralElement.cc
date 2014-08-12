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

#include <scimath/Functionals/Polynomial.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

PolynomialSpectralElement::PolynomialSpectralElement()
: SpectralElement(SpectralElement::POLYNOMIAL, Vector<Double>(0)) {
	_setFunction(
		CountedPtr<Polynomial<Double> >(
			new Polynomial<Double>()
		)
	);
}

PolynomialSpectralElement::PolynomialSpectralElement(const uInt n)
  : SpectralElement(SpectralElement::POLYNOMIAL, Vector<Double>(n+1, 0.)) {
	_setFunction(
		CountedPtr<Polynomial<Double> >(
			new Polynomial<Double>(n)
		)
	);
}

PolynomialSpectralElement::PolynomialSpectralElement(const Vector<Double>& param)
: SpectralElement(SpectralElement::POLYNOMIAL, param) {
	_setFunction(
		CountedPtr<Polynomial<Double> >(
			new Polynomial<Double>(param.size())
		)
	);
	_set(param);
	fix(Vector<Bool>(param.size(), False));
}

PolynomialSpectralElement::PolynomialSpectralElement(
	const PolynomialSpectralElement &other
) : SpectralElement(other) {}

PolynomialSpectralElement::~PolynomialSpectralElement() {}

SpectralElement* PolynomialSpectralElement::clone() const {
	return new PolynomialSpectralElement(*this);
}

/*
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
*/

uInt PolynomialSpectralElement::getDegree() const {
	return get().size() - 1;
}

ostream &operator<<(ostream &os, const PolynomialSpectralElement &elem) {
	os << SpectralElement::fromType((elem.getType())) << " element: " << endl;
	uInt degree = elem.getDegree();
    os << "  Degree:    " << degree << endl;
    os << "  Function: c0 ";
    ostringstream ss;
    Vector<Double> c = elem.get();
    ss << "c0: " << c[0] << endl;
    for (uInt i=1; i<=degree; i++) {
    	os << " + c" << i << "*x";
    	if (i > 1) {
    		os << "**" << i;
    	}
    	ss << "c" << i << ": " << c[i] << endl;
    }
    os << endl;
    os << ss.str();


    return os;
}

} //# NAMESPACE CASA - END

