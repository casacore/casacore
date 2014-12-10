//# MSSysCalEnums.h: Class with definitions for the MSSysCal table
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

#ifndef MS_MSSYSCALENUMS_H
#define MS_MSSYSCALENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet SYSCAL table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet SYSCAL table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSSysCal class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/22">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSSysCalEnums {
public:
    // The SYSCAL table colums with predefined meaning.
    // Keys: ANTENNA_ID, ARRAY_ID, FEED_ID, SPECTRAL_WINDOW_ID, INTERVAL, TIME
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Antenna Id.<BR>
    // Int.
    ANTENNA_ID,  
    // Feed id <BR>
    // Int
    FEED_ID,
    // Interval for which this set of parameters is accurate <BR>
    // Double - s
    INTERVAL,
    // Spectral window id <BR>
    // Int
    SPECTRAL_WINDOW_ID,
    // Midpoint of time for which this set of parameters is accurate<BR>
    // Double - s - EPOCH
    TIME,
    // Enum specifying the number of required columns
    NUMBER_REQUIRED_COLUMNS=TIME,
    // Phase difference between receptor 2 and receptor 1. Not used
    // for single polarization feeds. <BR>
    // Float - rad 
    PHASE_DIFF,
    // Flag for PHASE_DIFF <BR>
    // Bool
    PHASE_DIFF_FLAG,
    // Antenna temperature <BR>
    // Float(NUM_RECEPTORS) - K
    TANT,
    // Flag for TANT <BR>
    // Bool
    TANT_FLAG,
    // Antenna temperature for each channel and receptor <BR>
    // Float(NUM_RECEPTORS,NUM_CHAN) - K
    TANT_SPECTRUM,
    // Ratio of antenna temperature and system temperature <BR>
    // Float(NUM_RECEPTORS) - K
    TANT_TSYS,
    // Flag for TANT_TSYS <BR>
    // Bool
    TANT_TSYS_FLAG,
    // Spectrum of Tant/Tsys ratio for each receptor <BR>
    // Float(NUM_RECEPTORS,NUM_CHAN) 
    TANT_TSYS_SPECTRUM,
    // Calibration temperature for each receptor <BR>
    // Float(NUM_RECEPTORS) - K
    TCAL,
    // Flag for TCAL <BR>
    // Bool
    TCAL_FLAG,
    // Calibration temp. for each channel and receptor <BR>
    // Float(NUM_RECEPTORS,NUM_CHAN) - K
    TCAL_SPECTRUM,
    // Receiver temperature for each of the two receptors. This is
    // a scalar quantity <BR>
    // Float(NUM_RECEPTORS) - K
    TRX,
    // Flag for TRX <BR>
    // Bool
    TRX_FLAG,
    // Receiver temp. for each channel and receptor <BR>
    // Float(NUM_RECEPTORS,NUM_CHAN) - K
    TRX_SPECTRUM,
    // Sky temperature for each of the two receptors. <BR>
    // Float(NUM_RECEPTORS) - K
    TSKY,
    // Flag for TSKY <BR>
    // Bool
    TSKY_FLAG,
    // Sky temp. for each channel and receptor <BR>
    // Float(NUM_RECEPTORS,NUM_CHAN) - K
    TSKY_SPECTRUM,
    // System temp. for each of the two receptors. <BR>
    // Float(NUM_RECEPTORS) - K
    TSYS,
    // Flag for TSYS <BR>
    // Bool
    TSYS_FLAG,
    // System temp. for each channel and receptor <BR>
    // Float(NUM_RECEPTORS,NUM_CHAN) - K
    TSYS_SPECTRUM,
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=TSYS_SPECTRUM
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

