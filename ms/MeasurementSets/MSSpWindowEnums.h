//# MSSpectralWindowEnums.h: Definitions for the MS SPECTRAL_WINDOW table
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

#ifndef MS_MSSPWINDOWENUMS_H
#define MS_MSSPWINDOWENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet SPECTRAL_WINDOW table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet SPECTRAL_WINDOW table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MeasurementSet class.  These enumerations define the
// standard columns and keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1996/01/16">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//


class MSSpectralWindowEnums {
public:
    // The SpectralWindow table colums with predefined meaning.
    // The SPECTRAL_WINDOW_ID is the row number in the table.
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Center frequencies for each channel in the data matrix.
    // Can therefore be non-linear to allow for e.g. AOS <BR>
    // Double(NUM_CHAN) - Hz - FREQUENCY
    CHAN_FREQ,
    // The channel width for each channel <BR>
    // Double(NUM_CHAN) - Hz
    CHAN_WIDTH,
    // The effective noise bandwidth of each channel <BR>
    // Double(NUM_CHAN) - Hz
    EFFECTIVE_BW,
    // Row flag <BR>
    // Bool
    FLAG_ROW,
    // The frequency group <BR>
    // Int
    FREQ_GROUP,
    // The frequency group name <BR>
    // String
    FREQ_GROUP_NAME,
    // The IF conversion chain (to distinguish the separate electronic paths for
    // simultaneous observations at multiple frequencies). E.g., VLA A-C and 
    // B-D should always be numbered 0 and 1 resp.<BR>
    // Int
    IF_CONV_CHAIN,
    // The frequency measure reference <BR>
    // Int
    MEAS_FREQ_REF,
    // Spectral window name <BR>
    // String
    NAME,
    // Net sideband for this spectral window (+/- 1) <BR>
    // Int
    NET_SIDEBAND,
    // Number of spectral channels <BR>
    // Int
    NUM_CHAN,
    // The reference frequency (as specified on-line). <BR>
    // Double - Hz - FREQUENCY
    REF_FREQUENCY,
    // The effective spectral resolution of each channel
    // The Vector nature allows for variable-width channels.<BR>
    // Double(NUM_CHAN) - Hz
    RESOLUTION,
    // The total bandwidth (as specified on-line). <BR>
    // Double - Hz
    TOTAL_BANDWIDTH,
    //
    // Not a column, but just an enum specifying the number of required columns.
    NUMBER_REQUIRED_COLUMNS=TOTAL_BANDWIDTH,
    // Nature of association with other spectral window id <BR>
    // String(*)
    ASSOC_NATURE,
    // Associated spectral window id's, e.g. averaged spectra
    // Int(*)
    ASSOC_SPW_ID,
    // Baseband converter number <BR>
    // Int
    BBC_NO,
    // Baseband converter sideband <BR>
    // Int
    BBC_SIDEBAND,
    // Doppler id, points to DOPPLER table <BR>
    // Int
    DOPPLER_ID,
    // Receiver id, identifies receiver used for this spectral window.
    // May point to optional RECEIVER table <BR>
    // Int
    RECEIVER_ID,
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=RECEIVER_ID
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
