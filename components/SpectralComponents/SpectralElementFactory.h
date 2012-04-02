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

#ifndef COMPONENTS_SPECTRALELEMENTFACTORY_H
#define COMPONENTS_SPECTRALELEMENTFACTORY_H

//# Includes
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/RecordInterface.h>
#include <components/SpectralComponents/SpectralElement.h>

#include <memory>


namespace casa { //# NAMESPACE CASA - BEGIN

// FIXME fix documentation

// <summary>
// Class for creating spectral elements
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Functionals>SpectralElement</linkto> module
// </prerequisite>
//
// <etymology>
// From spectral and element and factory. As in to manufacture a SpectralElement
// </etymology>
//
// <synopsis>
// SpectralElementFactory contains methods for creating SpectralElements.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a factory class for producing types of SpectralElements.
// </motivation>
//
class SpectralElementFactory {
public:

	// Construct from record.  Must hold fields "type" (String) and
 	// "parameters" (Vector<Double>).  For type=GAUSSIAN, parameters
	// holds amplitude, center and sigma. For type=POLYNOMIAL,
	// parameters(0) holds the degree.
	static std::auto_ptr<SpectralElement> fromRecord(
		const RecordInterface &container
	);

};

} //# NAMESPACE CASA - END

#endif
