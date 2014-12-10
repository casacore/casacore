//# MSFieldEnums.h: Definitions for the MeasurementSet FIELD table
//# Copyright (C) 1996,1999,2000
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

#ifndef MS_MSFIELDENUMS_H
#define MS_MSFIELDENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet FIELD table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet FIELD table
// </etymology>
// <synopsis>
// This class does nothing. It is merely a container for the enumerations
// used by the MSField class.  These enumerations define the
// standard columns and keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/13">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSFieldEnums {
public:
    // The FIELD table colums with predefined meaning.
    // Keys: FIELD_ID, SOURCE_ID
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Special characteristics of field, e.g. Bandpass calibrator.
    // We need to define a standard set that can be used for
    // automated data reduction.<BR>
    // String.
    CODE,  
    // Direction of delay center (e.g. RA, DEC) as polynomial in time.<BR>
    // Double(2,NUM_POLY+1) - rad - DIRECTION.
    DELAY_DIR,  
    // Flag for this row <BR>
    // Bool
    FLAG_ROW,
    // Field Name. <BR>
    // String
    NAME,
    // Polynomial order for *_DIR columns <BR>
    // Int
    NUM_POLY,
    // Direction of phase center (e.g. RA, DEC) as polynomial in time<BR>
    // Double(2,NUM_POLY+1) - rad - DIRECTION.
    PHASE_DIR,  
    // Direction of reference center (e.g. RA, DEC).<BR>
    // Double(2,NUM_POLY+1) - rad - DIRECTION.
    REFERENCE_DIR,  
    // Source id (index in SOURCE table) <BR>
    // Int 
    SOURCE_ID,
    // Time origin for the directions and rates. <BR>
    // Double - s - EPOCH
    TIME,
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=TIME,
    // Ephemeris id, pointer to EPHEMERIS table (for moving objects, with 
    // possible ephemeris updates) <BR>
    // Int
    EPHEMERIS_ID,
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=EPHEMERIS_ID
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
