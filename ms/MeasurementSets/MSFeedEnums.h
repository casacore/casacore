//# MSFeedEnums.h: Definitions for the MeasurementSet FEED table
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

#ifndef MS_MSFEEDENUMS_H
#define MS_MSFEEDENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet FEED table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet FEED table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSFeed class.  These enumerations define the
// standard columns, keywords.
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

class MSFeedEnums {
public:
    // The FEED table colums with predefined meaning.
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Antenna Id.<BR>
    // Int.
    ANTENNA_ID,  
    // Index in BEAM model table. This is a specialized model 
    // e.g. NRAO_VLA_BEAM would have parameters for polynomial.<BR>
    // Int.
    BEAM_ID,
    // Beam position offset (on sky but in antenna reference frame).<BR>
    // Double(2,NUM_RECEPTORS) - rad - DIRECTION
    BEAM_OFFSET,
    // Feed id <BR>
    // Int
    FEED_ID,
    // Interval for which this set of parameters is accurate <BR>
    // Double - s
    INTERVAL,
    // Number of receptors on this feed (probably 1 or 2) <BR>
    // Int
    NUM_RECEPTORS,
    // D-matrix i.e. leakage between two receptors i.e. only makes
    // sense if NUM_RECEPTORS>1. Dimensionless coupling numbers. <BR>
    // Complex(NUM_RECEPTORS,NUM_RECEPTORS)
    POL_RESPONSE,
    // Type of polarization to which a given RECEPTOR responds. Probably
    // R, L or X, Y. <BR>
    // String(NUM_RECEPTORS)
    POLARIZATION_TYPE,
    // Position of feed relative to feed reference position for this antenna <BR>
    // Double(3) - m - POSITION
    POSITION,
    // The reference angle for polarization. Converts into
    //  Parallactic angle in the Sky domain. <BR>
    // Double(2) - rad 
    RECEPTOR_ANGLE,
    // Spectral Window id <BR>
    // Int
    SPECTRAL_WINDOW_ID,
    // Midpoint of time for which this set of parameters is accurate<BR>
    // Double - s - EPOCH
    TIME,
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=TIME,
    // Focus length <BR>
    // Double - m 
    FOCUS_LENGTH,
    // Phased feed id to index into PHASED_FEED table <BR>
    // Int
    PHASED_FEED_ID,
    //
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=PHASED_FEED_ID
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
