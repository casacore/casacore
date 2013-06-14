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
//# $Id$

#ifndef COMPONENTS_SPECTRALELEMENT_H
#define COMPONENTS_SPECTRALELEMENT_H

#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/RecordInterface.h>
#include <tr1/memory>

namespace casa { //# NAMESPACE CASA - BEGIN

template <class T, class U> class Function;

// <summary>
// Describes (a set of related) spectral lines
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Functionals>Functionals</linkto> module
// </prerequisite>
//
// <etymology>
// From spectral line and element
// </etymology>
//
// <synopsis>
// The SpectralElement class is the abstract base class for classes
// describing spectral components (Gaussian, Polynonomial, etc).
//
// The element can be used in the
// <linkto class=SpectralFit>SpectralFit</linkto> class and in the
// <linkto class=SpectralEstimate>SpectralEstimate</linkto> class.
//
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a container for fitting of spectral profiles to an observed spectrum
// </motivation>
//
// <todo asof="2001/02/04">
//   <li> add more profile types
// </todo>

class SpectralElement {
public:

	//# Enumerations
	// Supported spectral components
	enum Types {
		// A gaussian profile
		GAUSSIAN,
		// A polynomial baseline
		POLYNOMIAL,
		// Any compiled string functional
		COMPILED,
		// Gaussian multiplet
		GMULTIPLET,
		// Lorentzian
		LORENTZIAN,
		// power log polynomial
		POWERLOGPOLY,
		// log transformed polynomial
		LOGTRANSPOLY,
		N_Types
	};

	virtual ~SpectralElement();

	virtual SpectralElement* clone() const = 0;

	// Evaluate the value of the element at x
	virtual Double operator()(const Double x) const;

	virtual Bool operator==(const SpectralElement& other) const;

	// Get parameter n
	// <thrown>
	//  <li> AipsError if illegal n
	// </thrown>
	virtual Double operator[](const uInt n) const;

	// Get all the types available as String and codes, and number available
	static const String* allTypes(Int &nall,
			const SpectralElement::Types *&typ);
	// Get a string from the type
	static const String &fromType(SpectralElement::Types tp);
	// Get a type from a (non-case sensitive; minimum match) String
	static Bool toType(SpectralElement::Types &tp,
			const String &typName);

	// Get type of this element
	SpectralElement::Types getType() const { return _type; }

	// Get all parameters
	void get(Vector<Double>& params) const;

	Vector<Double> get() const;

	// Get error estimates of parameters
	void getError(Vector<Double> &err) const;
	Vector<Double> getError() const;

	// Get the order (i.e. the number of parameters)
	uInt getOrder() const { return _params.size(); };

	// Set the error fields
	virtual void setError(const Vector<Double> &err);

	// Set fixed parameters (True) or unset them (False)
	// <thrown>
	//   <li> AipsError if incorrect number of parameters (e.g. not 3 for GAUSSIAN)
	// </thrown>

	// Fix/unfix all in one go
	virtual void fix(const Vector<Bool>& fix);

	// Get the fix state[s]
	const Vector<Bool> &fixed() const;

	// Save to a record.
	virtual Bool toRecord(RecordInterface& out) const;

	// set parameters
	virtual void set(const Vector<Double>& params);

protected:

	SpectralElement() {}

	SpectralElement(Types type, const Vector<Double>& parms=Vector<Double>(0));

	SpectralElement(const SpectralElement& other);

	SpectralElement &operator=(const SpectralElement& other);

	void _set(const Vector<Double>& params);

	void _setType(const Types type);

	void _setFunction(const std::tr1::shared_ptr<Function<Double, Double> >& f);

	virtual std::tr1::shared_ptr<Function<Double, Double> > _getFunction() const {
		return _function;
	}

private:
	//#Data
	// type of element
	Types _type;

	// The parameters of the function. I.e. the polynomial coefficients;
	// amplitude, center and sigma of a Gaussian.
	Vector<Double> _params;
	// The errors of the parameters
	Vector<Double> _errors;
	// The indication if the parameter has to be fixed (True) or solved (False).
	// Solved is the default.
	Vector<Bool> _fixed;

	std::tr1::shared_ptr<Function<Double, Double> > _function;

};

ostream &operator<<(ostream& os, const SpectralElement& elem);

Bool near(const SpectralElement& s1, const SpectralElement& s2, const Double tol);

Bool nearAbs(const SpectralElement& s1, const SpectralElement& s2, const Double tol);


} //# NAMESPACE CASA - END

#endif

