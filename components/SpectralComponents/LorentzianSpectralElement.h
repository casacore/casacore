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

#ifndef COMPONENTS_LORENTZIANSPECTRALELEMENT_H
#define COMPONENTS_LORENTZIANSPECTRALELEMENT_H

#include <components/SpectralComponents/PCFSpectralElement.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Describes a single Lorentzian spectral profile
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=SpectralElement>SpectralElement</linkto> module
// </prerequisite>
//
// <etymology>
// From Lorentzian and spectral profile and element
// </etymology>
//
// <synopsis>
// The LorentzianSpectralElement class describes a Lorentzian spectral profile.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a container for data descrbing a Lorentzian spectral profile for fitting to an observed spectrum
// </motivation>

class LorentzianSpectralElement : public PCFSpectralElement {
public:



	// Construct with given type and values
	// <thrown>
	//   <li> AipsError if fwhm == 0.0 or amp == 0.0
	// </thrown>
	LorentzianSpectralElement(
		const Double ampl, const Double center,
		const Double fwhm
	);

	LorentzianSpectralElement(const Vector<Double>& param);

	// Copy constructor (deep copy)
	LorentzianSpectralElement(const LorentzianSpectralElement& other);

	~LorentzianSpectralElement();

	SpectralElement* clone() const;

	// Assignment (copy semantics)
	LorentzianSpectralElement& operator=(const LorentzianSpectralElement& other);

	void setFWHM(Double fwhm) { setWidth(fwhm); }

	Double getFWHM() const { return getWidth(); }

	Double getFWHMErr() const { return getWidthErr(); }

	// get the integral of the function
	Double getIntegral() const;

private:
	// has amp = 1, center = 0, and fwhm = 1
		LorentzianSpectralElement();

};

ostream &operator<<(ostream& os, const LorentzianSpectralElement& elem);

} //# NAMESPACE CASA - END

#endif
