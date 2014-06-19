//# SpectralElement.h: Describes (a set of related) spectral lines
//# Copyright (C) 2001,2003,2004
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
//#
//# $Id: SpectralElement.h 20652 2009-07-06 05:04:32Z Malte.Marquarding $

#ifndef COMPONENTS_LOGTRANSFORMEDPOLYNOMIALSPECTRALELEMENT_H
#define COMPONENTS_LOGTRANSFORMEDPOLYNOMIALSPECTRALELEMENT_H

#include <components/SpectralComponents/PolynomialSpectralElement.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Describes the often used for determining spectral index plus higher order terms:
// y = log (S_x) = ln(c_0) + c_1*ln(x) + c_2*ln(x)**2 + c_3*ln(x)**3 + ...
// where c_1 is the traditional spectral index (alpha).
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=SpectralElement>SpectralElement</linkto> module
// </prerequisite>
//
// <etymology>
// From power law, logarithm, and polynomial and spectral line and element
// </etymology>
//
// <synopsis>
// Describes a function that can be used to fit for spectral index and higher order terms.
// The implementation simply subclasses PolynomialSpectralElement since that's all this function
// really is, whicht the exception the the lhs is ln(y) not y. This means it's the fitter
// configurator's responsibility to pass in the ln of the actual ordinate values and the ln
// of the abscissa values, not the ordinate and abscissa values themselves. Essentially, this
// class differs from PolynomialSpectralElement in its type and its stream operator.

// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a spectral element representing a spectral index function.
// </motivation>


class LogTransformedPolynomialSpectralElement: public PolynomialSpectralElement {
public:

	// Constructor. The n coefficients c_i to be solved for are
	// c_0 + c_1 * ln(x) + c_2 * ln(x)**2 + c_3 * ln(x)**3 + ... c_(n-1)*ln(x)**(n-1)
	// where x = nu/nu0. <src> order</src> is the polynomial, so the actual
	// function will have order+1 coefficients
	explicit LogTransformedPolynomialSpectralElement(uInt order);

	// Construct with the given parameters. See above constructor for
	// order in which the parameters should be supplied.
	LogTransformedPolynomialSpectralElement(const Vector<Double> &param);

	// Copy constructor (deep copy)
	LogTransformedPolynomialSpectralElement(const LogTransformedPolynomialSpectralElement &other);

	~LogTransformedPolynomialSpectralElement();

	LogTransformedPolynomialSpectralElement &operator=(
		const LogTransformedPolynomialSpectralElement& other
	);

	SpectralElement* clone() const;
};
ostream &operator<<(
	ostream &os, const LogTransformedPolynomialSpectralElement &elem
);


} //# NAMESPACE CASA - END

#endif

