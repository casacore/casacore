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

#ifndef COMPONENTS_PCFSPECTRALELEMENT_H
#define COMPONENTS_PCFSPECTRALELEMENT_H

#include <components/SpectralComponents/SpectralElement.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Abstract base class that describes a spectral profile that can be parameterized
// by a peak value (amplitude), center, and FWHM.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=SpectralElement>SpectralElement</linkto> module
// </prerequisite>
//
// <etymology>
// From p(eak), c(enter), f(whm), and spectral line and element
// </etymology>
//
// <synopsis>
// Abstract base class that describes a spectral profile that can be parameterized
// by a peak value (amplitude), center, and FWHM.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a class containing common methods for things like Gaussian, Lorentzian,
// and Voigt profiles.
// </motivation>

class PCFSpectralElement : public SpectralElement {
public:





	//#Destructor
	// Destructor
	virtual ~PCFSpectralElement();

//	PCFSpectralElement& operator=(const PCFSpectralElement &other);

	// Get amplitude
	Double getAmpl() const;
	// Get center value
	Double getCenter() const;
	// Get the width
	// <group>
	virtual Double getFWHM() const;
	// get the integral from -inf to inf
	virtual Double getIntegral() const = 0;
	// </group>
	// Get amplitude error estimate
	Double getAmplErr() const;
	// Get center value error estimate
	Double getCenterErr() const;
	// Get the width error estimate
	// <group>
	virtual Double getFWHMErr() const;
	virtual Double getIntegralErr() const;
	// </group>
	// Get error estimates of parameters


	void set(const Vector<Double>& param);

	void setAmpl(const Double ampl);

	void setCenter(const Double center);

	virtual void setFWHM(const Double fwhm);


	void fixAmpl(const Bool fix=True);
	void fixCenter(const Bool fix=True);
	void fixFWHM(const Bool fix=True);
	// fix parameters via encoded string. If s contains a, fix amplitude. If s contains f, fix fwhm.
	// If s contains c, fix center.
	void fixByString(const String& s);
	// </group>


	Bool fixedAmpl() const;
	Bool fixedCenter() const;
	Bool fixedFWHM() const;

protected:
	PCFSpectralElement();
	PCFSpectralElement(const PCFSpectralElement& other);

};

} //# NAMESPACE CASA - END

#endif
