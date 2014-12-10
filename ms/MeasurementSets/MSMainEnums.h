//# MSMainEnums.h: Class with definitions for the main MeasurementSet table
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

#ifndef MS_MSMAINENUMS_H
#define MS_MSMAINENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet main table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enum defininitions for the main MeasurementSet
// table.
// </etymology>

// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MeasurementSet class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/02/04">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSMainEnums {
public:
    // The Main table colums with predefined meaning.
    enum PredefinedColumns {
    // "True" columns are defined. <BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    //
    // ID of first antenna in antenna-pair. This is a key into the
    // ANTENNA table. Ranges from 0 to NUM_ANT-1.<BR>
    // Int
    ANTENNA1,    
    //
    // ID of second antenna in antenna-pair. For SD ANTENNA1==ANTENNA2<BR>
    // Int
    ANTENNA2,  
    //
    // ARRAY id.<BR>
    // Int.
    ARRAY_ID,
    //
    // Data description id <BR>
    // Int.
    DATA_DESC_ID,
    //
    // Effective integration time (i.e.<=INTERVAL)<BR>
    // Double - s.
    EXPOSURE,    
    //
    // Feed id on ANTENNA1.<BR>
    // Int.
    FEED1,
    //
    // Feed id on ANTENNA2.<BR>
    // Int.
    FEED2,
    //
    // Unique id for this pointing (or drift scan)<BR>
    // Int
    FIELD_ID,    
    //
    // The data flags, array of bools with same shape as data.
    // Data is flagged bad if FLAG is True.<BR>
    // Bool(Nc, Nf)
    FLAG,
    //
    // Flag category, allows for multiple categories of flagging, which can
    // selectively be reset. The cumulative effect is reflected in FLAG. 
    // This column should have an attached keyword CATEGORY which is a 
    // String (Ncat) of categories (e.g, ONLINE, FLAG_CMD, INTERACTIVE) <BR>
    // Bool (Nc, Nf, Ncat)
    FLAG_CATEGORY,
    //
    // Flag all data in this row if True.<BR>
    // Bool
    FLAG_ROW,
    //
    // The extent of this sample, sampling interval.<BR>
    // Double - s.
    INTERVAL, 
    //
    // Index into OBSERVATION table. <BR>
    // Int.
    OBSERVATION_ID,
    //
    // Processor Id, points to PROCESSOR table with information on the 
    // correlator or backend setup. <BR>
    // Int
    PROCESSOR_ID,
    //
    // Scan number.
    // Int.
    SCAN_NUMBER,
    //
    // Estimated rms noise for channel with unity bandpass response.<BR>
    // Float(Nc) - Same units as the DATA column.
    SIGMA,
    //
    // State Id, points to STATE table with info on current observing mode,
    // calibration and reference signals etc. (Mainly single dish) <BR>
    // Int
    STATE_ID,
    //
    // Modified Julian Day number (JD-2400000.5) for midpoint of integration.
    // For high precision timing, add the value from TIME_EXTRA_PREC.<BR>
    // Double - s - EPOCH.
    TIME,
    //
    // Modified Julian Day number (JD-2400000.5) for centroid of integration.
    // Double - s - EPOCH.
    TIME_CENTROID,
    //
    // UVW coordinates.<BR>
    // Double(3) - m - UVW.
    UVW,         
    //
    // Weight of spectrum. This is the weight assigned by the correlator and
    // does NOT get overwritten by e.g. imaging tasks that do weighting.<BR>
    // Float(Nc).
    WEIGHT,
    //
    // Not a column, but just an enum specifying the number of required columns.
    //# Note: first enum after this one should be assigned value of this enum.
    NUMBER_REQUIRED_COLUMNS=WEIGHT,
    //
    // Antenna3 - for triple correlations products. <BR>
    // Int
    ANTENNA3,
    //
    // Reference antenna for this baseline, True for ANTENNA1 <BR>
    // Bool
    BASELINE_REF,
    //
    // The Corrected complex visibility data (optional). <BR>
    // Complex(Nc, Nf)
    CORRECTED_DATA,
    //
    // Complex visibility matrix. The UNITS are unspecified to allow
    // for the calibrated data to show up as a DATA column as well but in
    // a calibrated MS.<BR>
    // Complex(Nc, Nf)
    DATA,
    //
    // Feed id on ANTENNA3 <BR>
    // Int
    FEED3,
    //
    // Floating point data column. For simple single dish work this can be used
    // instead of the complex DATA column. <BR>
    // Float(Nc, Nf)
    FLOAT_DATA,
    //
    // The imaging weights (optional). <BR>
    // Float(Nf)
    IMAGING_WEIGHT,
    //
    // Complex correlation function or lag spectrum for each correlation 
    // product <BR>
    // Complex(Nc, Nl)
    LAG_DATA,
    //
    // The model visibility data (optional). <BR>
    // Complex(Nc,Nf)
    MODEL_DATA,
    //
    // Switching phase Id <BR>
    // Int
    PHASE_ID,
    //
    // For a pulsar the correlations are assumed to be measured for a
    // limited number of pulse phase bins. This is the particular bin for
    // which this data was measured. (optional) <BR>
    // Int.
    PULSAR_BIN,
    //
    // Unique id for this pulsar gate. Index into PULSAR_GATE table.
    // (optional) <BR>
    // Int.
    PULSAR_GATE_ID,
    //
    // Estimated rms noise for each data point. To be used instead of
    // SIGMA if present. <BR>
    // Float(Nc,Nf) - Same units as the DATA column.
    SIGMA_SPECTRUM,
    //
    // Additional precision for TIME if required. Add this to TIME to obtain
    // the exact EPOCH.<BR>
    // Double - s.
    TIME_EXTRA_PREC, 
    //
    // UVW for second pair of triple correlation product. <BR>
    // Double(3) - m
    UVW2,
    //
    // Zero frequency point - needed for transform back to lag domain <BR>
    // Complex(Nc)
    VIDEO_POINT,
    //
    // Weight for each channel. To be used instead of WEIGHT if present.<BR>
    // Float(Nf).
    WEIGHT_SPECTRUM,
    // Corrected Weight for each channel.  If present can be used with corrected_data<BR>
    // Float(Nf).
    CORRECTED_WEIGHT_SPECTRUM,
    //
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=CORRECTED_WEIGHT_SPECTRUM
    };
  
    // Keywords with a predefined meaning
    enum PredefinedKeywords {
    //
    // "True" keywords are defined. 
    UNDEFINED_KEYWORD=0,
    //
    // Antenna subtable. Antenna positions, mount-types etc.
    ANTENNA,
    // Data Description subtable. Gives spectral window and polarization id.
    DATA_DESCRIPTION,
    // Feed subtable. Responses, offsets, beams etc.
    FEED,
    // Field subtable. Position etc. for each pointing.
    FIELD,
    // Flag command subtable. List of flag commands.
    FLAG_CMD,
    // History information subtable.
    HISTORY,
    // MS Version number. <BR>
    // Float.
    MS_VERSION,
    // Observation subtable. Project, observer, schedule.
    OBSERVATION,
    // Pointing information subtable.
    POINTING,
    // Polarization setup information subtable.
    POLARIZATION,
    // Back-end processor information subtable. Description of correlator etc.
    PROCESSOR,
    // Spectral window subtable. Frequencies, bandwidths, polarizations.
    SPECTRAL_WINDOW,
    // State subtable. Observing modes and states (cal, ref etc.)
    STATE,
    // Not a keyword, but an enum specifying the number of required keywords
    // The last required keyword should be set to this enum
    NUMBER_REQUIRED_KEYWORDS=STATE,
    // Calibration tables associated with this MS. <BR>
    // Table(NUM_CAL_TABLES)
    CAL_TABLES,
    // Doppler tracking information subtable.
    DOPPLER,
    // Frequency offset information subtable.
    FREQ_OFFSET,
    // Listing of sort columns for each sorted table. <BR>
    // String(NUM_SORTED_TABLES)
    SORT_COLUMNS,
    // Listing of sort orders for each sorted table. <BR>
    // String(NUM_SORTED_TABLES)
    SORT_ORDER,
    // Sorted reference tables of the main table. First one is main table.<BR>
    // Table(NUM_SORTED_TABLES)
    SORTED_TABLES,
    // Source subtable. Positions etc. for each source.
    SOURCE,
    // SysCal subtable. System calibration data (Tsys etc.)
    SYSCAL,
    // Weather subtable. Weather info for each antenna.
    WEATHER,
    // Not a keyword, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_KEYWORDS=WEATHER
    };
};

} //# NAMESPACE CASACORE - END

#endif

