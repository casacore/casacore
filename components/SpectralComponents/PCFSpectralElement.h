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
// by a peak value (amplitude), center, and width.
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
// by a peak value (amplitude), center, and width.
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

	// to help avoid having to hard code parameter indices
	enum ParamType {
		AMP,
		CENTER,
		WIDTH
	};

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
	virtual Double getWidth() const;

	virtual Double getFWHM() const = 0;

	// get the integral from -inf to inf
	virtual Double getIntegral() const = 0;
	// </group>
	// Get amplitude error estimate
	Double getAmplErr() const;
	// Get center value error estimate
	Double getCenterErr() const;
	// Get the width error estimate
	// <group>
	virtual Double getWidthErr() const;
	virtual Double getFWHMErr() const = 0;


	virtual Double getIntegralErr() const;
	// </group>
	// Get error estimates of parameters


	void set(const Vector<Double>& param);

	void setAmpl(const Double ampl);

	void setCenter(const Double center);

	virtual void setWidth(const Double width);


	void fixAmpl(const Bool fix=True);
	void fixCenter(const Bool fix=True);
	void fixWidth(const Bool fix=True);
	void fixFWHM(const Bool fix=True) { fixWidth(fix); }

	// fix parameters via encoded string. If s contains a, fix amplitude. If s contains f, fix width.
	// If s contains c, fix center.
	void fixByString(const String& s);
	// </group>


	Bool fixedAmpl() const;
	Bool fixedCenter() const;
	Bool fixedWidth() const;

	Bool fixedFWHM() const { return fixedWidth(); }

protected:
	PCFSpectralElement(
		SpectralElement::Types type
	);

	// param should have three elements: amplitude, center, and width
	PCFSpectralElement(
		SpectralElement::Types type, const Vector<Double>& param
	);

	PCFSpectralElement(
		SpectralElement::Types type, Double amp,
		Double center, Double width
	);

	PCFSpectralElement(const PCFSpectralElement& other);


private:
	PCFSpectralElement();

	void _initFunction();

};

} //# NAMESPACE CASA - END

#endif
