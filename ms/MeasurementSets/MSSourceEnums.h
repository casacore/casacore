//# MSSourceEnums.h: Definitions for the MeasurementSet SOURCE table
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

#ifndef MS_MSSOURCEENUMS_H
#define MS_MSSOURCEENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet SOURCE table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet SOURCE table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSSource class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/15">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSSourceEnums {
public:
    // The Antenna table colums with predefined meaning.
    // Keys: SPECTRAL_WINDOW_ID, INTERVAL, TIME, SOURCE_ID
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Number of grouping for calibration purpose.<BR>
    // Int.
    CALIBRATION_GROUP,  
    // Special characteristics of source, e.g. Bandpass calibrator.
    // We need to define a standard set that can be used for
    // automated data reduction..<BR>
    // String.
    CODE,  
    // Direction (e.g. RA, DEC).<BR>
    // Double(2) - rad - DIRECTION.
    DIRECTION,  
    // Interval of time for which this set of parameters is accurate.<BR>
    // Double - s
    INTERVAL,  
    // Name of source as given during observations.<BR>
    // String.
    NAME,  
    // Number of spectral lines <BR>
    // Int
    NUM_LINES,
    // Proper motion.<BR>
    // Double(2) - rad/s - ?.
    PROPER_MOTION,  
    // Source id.<BR>
    // Int.
    SOURCE_ID,
    // Spectral window id.<BR>
    // Int.
    SPECTRAL_WINDOW_ID,  
    // Midpoint of time for which this set of parameters is accurate.<BR>
    // Double - s - EPOCH.
    TIME,  
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=TIME,
    // Position (e.g. for solar system objects.<BR>
    // Double(3) - m - POSITION.
    POSITION,  
    // Pulsar Id <BR>
    // Int
    PULSAR_ID,
    // Line rest frequency <BR>
    // Double(NUM_LINES) - Hz - Frequency
    REST_FREQUENCY,
    // Default Component Source Model <BR>
    // TableRecord
    SOURCE_MODEL,
    // Systemic velocity at reference.<BR>
    // Double(NUM_LINES) - m/s - RADIALVELOCITY.
    SYSVEL,  
    // Transition name <BR>
    // String(NUM_LINES)
    TRANSITION,
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=TRANSITION
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
