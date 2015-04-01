//# MSPointingEnums.h: Definitions for the MeasurementSet POINTING table
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

#ifndef MS_MSPOINTINGENUMS_H
#define MS_MSPOINTINGENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet POINTING table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet POINTING table
// </etymology>
// <synopsis>
// This class does nothing. It is merely a container for the enumerations
// used by the MSPointing class.  These enumerations define the
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

class MSPointingEnums {
public:
    // The POINTING table colums with predefined meaning.
    // Keys: ANTENNA_ID, TIME, INTERVAL
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Antenna id <BR>
    // Int
    ANTENNA_ID,
    // Antenna pointing direction (e.g. RA, DEC) as polynomial in time.<BR>
    // Double(2,NUM_POLY+1) - rad - DIRECTION.
    DIRECTION,
    // Time interval <BR>
    // Double - s
    INTERVAL,
    // Pointing Name. <BR>
    // String
    NAME,
    // Polynomial order for *_DIR columns <BR>
    // Int
    NUM_POLY,
    // Target direction <BR>
    // Double(2,NUM_POLY+1) - rad - DIRECTION
    TARGET,
    // Time midpoint for interval. <BR>
    // Double - s - EPOCH
    TIME,
    // Time origin for the directions and rates. <BR>
    // Double - s - EPOCH
    TIME_ORIGIN,
    // Track flag - true if on position <BR>
    // Bool
    TRACKING,
    // Number of required columns <BR>
    NUMBER_REQUIRED_COLUMNS=TRACKING,
    // Encoder values <BR>
    // Double(2)
    ENCODER,
    // On source flag - true if on source <BR>
    // Bool
    ON_SOURCE,
    // Over the top flag - true if antenna has been driven over the top <BR>
    // Bool
    OVER_THE_TOP,
    // Pointing model id <BR>
    // Int 
    POINTING_MODEL_ID,
    // Pointing offset as polynomial in time<BR>
    // Double(2,NUM_POLY+1) - rad - DIRECTION.
    POINTING_OFFSET,  
    // Offset from source as polynomial in time<BR>
    // Double(2,NUM_POLY+1) - rad - DIRECTION.
    SOURCE_OFFSET,  
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=SOURCE_OFFSET
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
