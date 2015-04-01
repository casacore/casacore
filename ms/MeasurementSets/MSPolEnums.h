//# MSPolarizationEnums.h: Definitions for the MeasurementSet POLARIZATION table
//# Copyright (C) 1999,2000
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

#ifndef MS_MSPOLENUMS_H
#define MS_MSPOLENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet POLARIZATION table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet POLARIZATION table
// </etymology>
// <synopsis>
// This class does nothing. It is merely a container for the enumerations
// used by the MSPolarization class.  These enumerations define the
// standard columns and keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/14">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSPolarizationEnums {
public:
    // The POLARIZATION table colums with predefined meaning.
    // Keys: POLARIZATION_ID is rownumber
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // A pair of integers for each correlation product specifying the receptors
    // from which the signal originated. E.g., (0,1) = receptor 0 on feed1 and
    // receptor 1 on feed2. This is unused for I,Q,U,V data.<BR>
    // Int(2, NUM_CORR)
    CORR_PRODUCT,
    // The polarization type for each correlation product, as a Stokes enum. <BR>
    // Int(NUM_CORR)
    CORR_TYPE,
    // Row flag <BR>
    // Bool
    FLAG_ROW,
    // Number of correlations. <BR>
    // Int
    NUM_CORR,
    // Number of required columns <BR>
    NUMBER_REQUIRED_COLUMNS=NUM_CORR,
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=NUMBER_REQUIRED_COLUMNS
    };
  
    // Keywords with a predefined meaning
    enum PredefinedKeywords {
    //
    // "True" keywords are defined. 
    UNDEFINED_KEYWORD=0,
    //
    // Not a keyword, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_KEYWORDS=0
    };
};

} //# NAMESPACE CASACORE - END

#endif
