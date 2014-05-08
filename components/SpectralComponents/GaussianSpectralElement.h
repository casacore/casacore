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

#ifndef COMPONENTS_GAUSSIANSPECTRALELEMENT_H
#define COMPONENTS_GAUSSIANSPECTRALELEMENT_H

#include <components/SpectralComponents/PCFSpectralElement.h>

namespace casa {

// <summary>
// Describes a Gaussian spectral line
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=SpectralElement>SpectralElement</linkto> module
// </prerequisite>
//
// <etymology>
// From Gaussian and spectral line and element
// </etymology>
//
// <synopsis>
// The GaussianSpectralElement class describes a Gaussian spectral line.

// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a container for data descrbing a Gaussian spectral profile for fitting to an observed spectrum
// </motivation>

class GaussianSpectralElement : public PCFSpectralElement {

public:

	// Default constructor creates a default Gaussian element with an amplitude
	// of 1; an integral <src>(sigma=2sqrt(ln2)/pi)</src> of 1;
	// a central frequency of zero. It's necessary for this to be public because
	// Arrays of this class require access to the default constructor. It should never
	// be used in code developers write though.
	GaussianSpectralElement();


	//# Constants
	// Sigma to FWHM conversion factor
	static const Double SigmaToFWHM;

	//# Constructors

	// Construct with given type and values
	// <thrown>
	//   <li> AipsError if sigma == 0.0
	//   <li> AipsError if type not GAUSSIAN
	// </thrown>
	GaussianSpectralElement(
		const Double ampl, const Double center,
		const Double sigma
	);

	// Construct the given tp with the given param
	// <thrown>
	//   <li> AipsError if incorrect number of parameters (e.g. not 3 for GAUSSIAN)
	//   <li> AipsError if sigma == 0.0
	// </thrown>
	GaussianSpectralElement(const Vector<Double> &param);
	// Copy constructor (deep copy)
	// <thrown>
	//   <li> AipsError if sigma == 0.0
	// </thrown>
	GaussianSpectralElement(const GaussianSpectralElement &other);

	//#Destructor
	// Destructor
	~GaussianSpectralElement();

	SpectralElement* clone() const;

	// Assignment (copy semantics)
	// <thrown>
	//   <li> AipsError if sigma == 0.0
	// </thrown>
//	GaussianSpectralElement& operator=(const GaussianSpectralElement &other);
	// Evaluate the value of the element at x
	//Double operator()(const Double x) const;

	Double getSigma() const;
	Double getFWHM() const;

	Double getSigmaErr() const;
	Double getFWHMErr() const;


	void setSigma(Double sigma);
	void setFWHM(Double fwhm);

	void fixSigma(const Bool fix=True);

	Bool fixedSigma() const;

	Double getIntegral() const;

	// Save to a record.   For Gaussian elements,
	// the width is defined as a FWHM in the record interface.
	Bool toRecord(RecordInterface &out) const;

	// Sigma to FWHM
	// Convert from sigma to FWHM and vice versa
	// <group>
	static Double sigmaFromFWHM (const Double fwhm);

	static Double sigmaToFWHM (const Double sigma);
	// </group>

	void set(const Vector<Double>& v);

private:
	// need to overrride SpectralElement::_set() because _param[2] is sigma
	// but the second param of the corresponding Gaussian1D function is the
	// FWHM :(
	void _set(const Vector<Double>& v);

};

ostream &operator<<(ostream& os, const GaussianSpectralElement& elem);


} //# NAMESPACE CASA - END

#endif
