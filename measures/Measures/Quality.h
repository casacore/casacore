//# Quality.h: Quality parameter definitions for interface to table data
//# Copyright (C) 1994,1995,1996,1997,2000
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

#ifndef MEASURES_QUALITY_H
#define MEASURES_QUALITY_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Quality parameter definitions.
// </summary>

// <reviewed tests="tQuality">
// </reviewed>

// <synopsis>
// This enumerates the available Quality types. This class is a wrapper
// for the ENUM and conversion functions. All methods are static.
// </synopsis>

class Quality {
public:

	//# The enum comments below are placed in the position they are to make the
	//# extracted documentation look nice.
	enum QualityTypes {

		// undefined value = 0
	    Undefined=0,

	    // the data type
	    DATA,

	    // the error type
	    ERROR
	};

	// The number of QualityTypes.
	// <note role=warning>
	// <b>Update</b> <src>NumberOfTypes</src> when entries are added.
	// </note>
	enum {
		// The number of QualityTypes.
	    NumberOfTypes = 3
	    };
	
	// Convert Int to QualityTypes, returns Quality::Undefined if
	// it is an invalid type
	static QualityTypes type(Int qualityNumber);

	// Convert String to QualityTypes, returns Quality::Undefined if
	// it is an unrecognized string.  The valid strings are the
	// same as the characters used in the enum above (i.e.
	// "DATA" returns Quality::DATA, "ERROR" returns Quality::ERROR, etc).
	static QualityTypes type(const String & quality);

	// Convert QualityTypes to String, Quality::Undefined returns
	// "??".
	static String name(QualityTypes qualityType);

	// Get all recognized quality names in no guaranteed order.
	// The undefined type can be included.
	static Vector<String> allNames(Bool includeUndefined = False);

private:
};

} //# NAMESPACE CASACORE - END

#endif

