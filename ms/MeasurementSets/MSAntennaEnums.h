//# MSAntennaEnums.h: Definitions for the MeasurementSet ANTENNA table
//# Copyright (C) 1996,2000
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

#ifndef MS_MSANTENNAENUMS_H
#define MS_MSANTENNAENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet ANTENNA table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet ANTENNA table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSAntenna class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1996/02/12">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSAntennaEnums {
public:
    // The ANTENNA table colums with predefined meaning.
    // Keys: ANTENNA_ID, ARRAY_ID
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Antenna type: "GROUND-BASED", "SPACE-BASED", "TRACKING-STATION" <BR>
    // String
    TYPE,
    // Physical diameter of dish (if appropriate)<BR>
    // Double - m
    DISH_DIAMETER,
    // Flag for this row <BR>
    // Bool
    FLAG_ROW,
    // Mount type: choose from "AZ-EL", "HA-DEC", "X-Y", "orbiting"
    // or "bizarre" (following VLBA FITS).<BR>
    // String.
    MOUNT,
    // Antenna name, e.g. VLA22, CA03.<BR>
    // String.
    NAME,
    // Axes offset of mount to FEED REFERENCE point. <BR>
    // Double(3) - m - POSITION
    OFFSET,
    // Antenna X,Y,Z phase reference positions in the IERS Terrestrial 
    // Reference Frame (ITRF);
    // right-handed, X towards the intersection of the ITRF equator and 
    // the Greenwich meridian, Z towards the adopted mean position of the pole.<BR>
    // Double(3) - m - POSITION
    POSITION,
    // Station (antenna pad) name.<BR>
    // String
    STATION,
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=STATION,
    // Mean Keplerian orbit elements <BR>
    // Double(6)
    MEAN_ORBIT,
    // Index into optional ORBIT table.<BR>
    // Int.
    ORBIT_ID,
    // Index into optional PHASED_ARRAY table.<BR>
    // Int.
    PHASED_ARRAY_ID,
    //
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=PHASED_ARRAY_ID
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
