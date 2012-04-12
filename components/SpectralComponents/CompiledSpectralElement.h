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

#ifndef COMPONENTS_COMPILEDSPECTRALELEMENT_H
#define COMPONENTS_COMPILEDSPECTRALELEMENT_H

#include <components/SpectralComponents/SpectralElement.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Describes a compiled function for describing a spectral profile
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=SpectralElement>SpectralElement</linkto> class
// </prerequisite>
//
// <etymology>
// From compiled and spectral line and element
// </etymology>
//
// <synopsis>
// The CompiledSpectralElement class describes a compiled function for
// describing spectral profile.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a container for data describing a compiled function for
// describing a spectral profile for fitting to an observed spectrum
// </motivation>

class CompiledSpectralElement: public SpectralElement {
public:

	// Construct a compiled string
	explicit CompiledSpectralElement(
		const String& function, const Vector<Double>& param
	);

	CompiledSpectralElement(const CompiledSpectralElement& other);

	virtual ~CompiledSpectralElement();

	SpectralElement* clone() const;

	CompiledSpectralElement &operator=(
		const CompiledSpectralElement& other
	);
	// Evaluate the value of the element at x
	virtual Double operator()(const Double x) const;


	// Get the string of a compiled functional
	const String& getFunction() const;
	// </group>

	// Save to a record.
	virtual Bool toRecord(RecordInterface& out) const;

protected:

	CompiledSpectralElement();

	virtual void _setFunction(const String& function);

private:
	// The string value for compiled functional
	String _function;
};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
ostream &operator<<(ostream &os, const CompiledSpectralElement &elem);
// </group>


} //# NAMESPACE CASA - END

#endif

