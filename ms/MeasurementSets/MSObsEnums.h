//# MSObservationEnums.h: Definitions for the MeasurementSet OBSERVATION table
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

#ifndef MS_MSOBSENUMS_H
#define MS_MSOBSENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet OBSERVATION table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet OBSERVATION table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSObservation class.  These enumerations define the
// standard columns, keywords.
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

class MSObservationEnums {
public:
    // The OBSERVATION table colums with predefined meaning.
    // The OBSERVATION_ID is the row number .
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Row flag <BR>
    // Bool
    FLAG_ROW,
    // Observing log <BR>
    // String(*)
    LOG,
    // Name of observer(s) <BR>
    // String
    OBSERVER,
    // Project identification string<BR>
    // TpString
    PROJECT,
    // Release data, date when data may become public <BR>
    // Double - s - EPOCH
    RELEASE_DATE,
    // Observing schedule <BR>
    // String(*)
    SCHEDULE,
    // Observing schedule type <BR>
    // String
    SCHEDULE_TYPE,
    // Telescope name <BR>
    // TpString
    TELESCOPE_NAME,
    // Start and end times of observation <BR>
    // Double(2)
    TIME_RANGE,
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=TIME_RANGE,
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
    NUMBER_PREDEFINED_KEYWORDS=UNDEFINED_KEYWORD
    };
};

} //# NAMESPACE CASACORE - END

#endif
