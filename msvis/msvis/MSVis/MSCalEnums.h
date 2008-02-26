//# MSCalEnums.h: Field name dictionary for MS and CAL tables
//# Copyright (C) 1996,1997,1998,2000,2001,2002
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#ifndef MSVIS_MSCALENUMS_H
#define MSVIS_MSCALENUMS_H

#include <casa/aips.h>
#include <casa/Containers/SimOrdMap.h>
#include <casa/Containers/Block.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/DataType.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// MSCalEnums: Field name dictionary for MeasurementSet and calibration tables
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MeasurementSet">MeasurementSet</linkto> module
// </prerequisite>
//
// <etymology>
// From MeasurementSet and calibration enumerations.
// </etymology>
//
// <synopsis>
// MSCalEnums defines a global dictionary of field names and properties
// used by both the MeasurementSet and calibration tables. Field names
// and properties are accessed through enumerated constants, thus
// ensuring a consistent and uniform interface. Both column names and
// keywords are included here. All data are stored in static maps.
// </synopsis>
//
// <example>
// <srcblock>
//      // Determine the column name for the SPECTRAL_WINDOW_ID column
//      String name = MSC::fieldName (MSC::SPECTRAL_WINDOW_ID));
//
// </srcblock>
// </example>
//
// <motivation>
// Provide unified, consistent access to data fields used in the synthesis
// system, in both the user interface and individual applications.
// </motivation>
//
// <todo asof="98/01/05">
// Expand the column description information stored by this class.
// </todo>

// Re-define MSCalEnums to MSC for simplified access. Requires
// forward declaration of MSCalEnums

class MSCalEnums;
typedef MSCalEnums MSC;

class MSCalEnums
{
 public:
  // Enumerate all relevant data fields (columns and keywords)
  enum colDef {
  // First include all MeasurementSet MAIN columns which may be used
  // in calibration selection, interpolation or labelling.
  //
  // Antenna number
  ANTENNA1 = 0,
  ANTENNA2 = 1,
  // Feed identifier
  FEED1 = 2,
  FEED2 = 3,
  // Pulsar bin number
  PULSAR_BIN = 4,
  // Scan number
  SCAN_NUMBER = 5,
  // Time
  TIME = 6,
  // Extra time precision
  TIME_EXTRA_PREC = 7,
  // Interval
  INTERVAL = 8,
  // Array identifier
  ARRAY_ID = 9,
  // Processor identifier
  PROCESSOR_ID = 10,
  // Field identifier
  FIELD_ID = 11,
  // Observation identifier
  OBSERVATION_ID = 12,
  // Pulsar gate
  PULSAR_GATE_ID = 13,
  // Spectral window identifier
  SPECTRAL_WINDOW_ID = 14,
  // Phase identifier
  PHASE_ID = 15,
  // State identifier
  STATE_ID = 16,
  
  // Other secondary MeasurementSet columns
  //
  // Frequency group
  FREQ_GROUP = 100,
  // Frequency group name
  FREQ_GROUP_NAME = 101,
  // Field name
  FIELD_NAME = 102,
  // Field code
  FIELD_CODE = 103,
  // Source name
  SOURCE_NAME = 104,
  // Source code
  SOURCE_CODE = 105,
  // Source calibration group
  CALIBRATION_GROUP = 106,
  
  // Other calibration table columns
  //
  // Gain matrix values
  GAIN = 200,
  // Reference antenna
  REF_ANT = 201,
  // Reference feed
  REF_FEED = 202,
  // Reference receptor
  REF_RECEPTOR = 203,
  // Reference frequency
  REF_FREQUENCY = 204,
  // Reference frequency frame
  MEAS_FREQ_REF = 205,
  // Reference direction
  REF_DIRECTION = 206,
  // Reference direction frame
  MEAS_DIR_REF = 207,
  // Antenna pointing offsets
  POINTING_OFFSET = 208,
  // Pointing offset frame
  MEAS_POINTING = 209,
  // 
  // Calibration format descriptor
  CAL_DESC_ID = 300, 
  // Calibration history identifier
  CAL_HISTORY_ID = 301,

  // Solution statistics and weights
  //
  // Solution validity flag
  TOTAL_SOLUTION_OK = 400,
  // RMS of fit
  TOTAL_FIT = 401,
  // Sum of fit weights
  TOTAL_FIT_WEIGHT = 402,
  // Solution validity flag (per solution)
  SOLUTION_OK = 403,
  // RMS of fit (per solution)
  FIT = 404,
  // Sum of fit weights (per solution)
  FIT_WEIGHT = 405,
  // Solution FLAG (!SOLUTION_OK)
  FLAG = 406,
  // Solution SNR
  SNR = 407,
  
  // Calibration description columns
  //
  // Number of spectral windows
  NUM_SPW = 500,
  // Number of frequency channels
  NUM_CHAN = 501,
  // Number of receptors
  NUM_RECEPTORS = 502,
  // Jones matrix dimension
  N_JONES = 503,
  // Midpoint of frequency channels
  CHAN_FREQ = 504,
  // Frequency channel width
  CHAN_WIDTH = 505,
  // Channel range
  CHAN_RANGE = 506,
  // Jones matrix type
  JONES_TYPE = 507, 
  // Polarization enumerations
  POLARIZATION_TYPE = 508,
  // MeasurementSet name
  MS_NAME = 509,
  
  // Calibration history columns
  //
  // Application parameter values
  CAL_PARMS = 600,
  // Associated calibration tables
  CAL_TABLES = 601,
  // Calibration selection
  CAL_SELECT = 602,
  // Calibration notes
  CAL_NOTES = 603,

  // Keywords
  //
  // Cal Desc sub-table
  CAL_DESC = 700,
  // Cal History sub-table
  CAL_HISTORY = 701,
  
  // FJones additions
  ROT_MEASURE = 800,
  ROT_MEASURE_ERROR = 801,
  IONOSPH_TEC = 802,
  IONOSPH_TEC_ERROR = 803,

  // GJonesDelayRateSB additions
  PHASE_OFFSET = 900,
  SB_DELAY = 901,
  DELAY_RATE = 902,

  // General polynomial calibration additions
  POLY_TYPE = 1000,
  POLY_MODE = 1001,
  SCALE_FACTOR = 1002,
  VALID_DOMAIN = 1003,
  N_POLY_AMP = 1004,
  N_POLY_PHASE = 1005,
  POLY_COEFF_AMP = 1006,
  POLY_COEFF_PHASE = 1007,
  PHASE_UNITS = 1008,

  // BJonesPoly additions
  SIDEBAND_REF = 1100,

  // Spline polynomial calibration additions
  N_KNOTS_AMP = 1200,
  N_KNOTS_PHASE = 1201,
  SPLINE_KNOTS_AMP = 1202,
  SPLINE_KNOTS_PHASE = 1203

  };

  // Access functions for column/keyword description data
  static String fieldName (Int enumField);
  static Block<String> fieldNames (const Vector<Int>& enumFields);
  static DataType basicType (Int enumField);
 
 private:
  // Initialize the static maps
  static void initMaps();
  
  // Static ordered maps containing field descriptions
  // Enum => Field Name
  static SimpleOrderedMap <Int, String> theirFieldMap;
  // Enum => Basic data type
  static SimpleOrderedMap <Int, DataType> theirTypeMap;
};


} //# NAMESPACE CASA - END

#endif
   
  



