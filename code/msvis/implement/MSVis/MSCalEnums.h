//# MSCalEnums.h: Field name dictionary for MS and CAL tables
//# Copyright (C) 1996,1997,1998
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
//# $Id: 

#if !defined(TRIAL_MSCALENUMS_H)
#define TRIAL_MSCALENUMS_H

#include <aips/aips.h>
#include <aips/Containers/SimOrdMap.h>
#include <aips/Utilities/String.h>

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
  // Correlator identifier
  CORRELATOR_ID = 10,
  // Field identifier
  FIELD_ID = 11,
  // Observation identifier
  OBSERVATION_ID = 12,
  // Pulsar gate
  PULSAR_GATE_ID = 13,
  // Spectral window identifier
  SPECTRAL_WINDOW_ID = 14,
  
  // Other secondary MeasurementSet columns
  //
  // Frequency group
  FREQ_GROUP = 15,
  // Field name
  FIELD_NAME = 16,
  // Source name
  SOURCE_NAME = 17, 
  
  // Other calibration table columns
  //
  // Gain matrix values
  GAIN = 18,
  // Reference antenna
  REF_ANT = 19,
  // Reference feed
  REF_FEED = 20,
  // Reference receptor
  REF_RECEPTOR = 21,
  // Reference frequency
  REF_FREQUENCY = 22,
  // Reference direction
  REF_DIRECTION = 23,
  // Calibration format descriptor
  CAL_DESC_ID = 24, 
  // Calibration history identifier
  CAL_HISTORY_ID = 25,

  // Solution statistics and weights
  //
  // Solution validity flag
  TOTAL_SOLUTION_OK = 26,
  // RMS of fit
  TOTAL_FIT = 27,
  // Sum of fit weights
  TOTAL_FIT_WEIGHT = 28,
  // Solution validity flag (per solution)
  SOLUTION_OK = 29,
  // RMS of fit (per solution)
  FIT = 30,
  // Sum of fit weights (per solution)
  FIT_WEIGHT = 31,
  
  // Calibration description columns
  //
  // Number of spectral windows
  NUM_SPW = 32,
  // Number of frequency channels
  NUM_CHAN = 33,
  // Number of receptors
  NUM_RECEPTORS = 34,
  // Jones matrix dimension
  N_JONES = 35,
  // Midpoint of frequency channels
  CHAN_FREQ = 36,
  // Frequency channel width
  CHAN_WIDTH = 37,
  // Channel range
  CHAN_RANGE = 38,
  // Jones matrix type
  JONES_TYPE = 39, 
  // Polarization enumerations
  POLARIZATION_TYPE = 40,
  // MeasurementSet name
  MS_NAME = 41,
  
  // Calibration history columns
  //
  // Application parameter values
  CAL_PARMS = 42,
  // Associated calibration tables
  CAL_TABLES = 43,
  // Calibration selection
  CAL_SELECT = 44,
  // Calibration notes
  CAL_NOTES = 45,

  // Keywords
  //
  // Cal Desc sub-table
  CAL_DESC = 46,
  // Cal History sub-table
  CAL_HISTORY = 47};

  // Access functions for column/keyword description data
  static String fieldName (Int enumField);
 
 private:
  // Initialize static map
  static void initMap();
  
  // Static ordered maps containing field descriptions
  // Enum => Field Name
  static SimpleOrderedMap <Int, String> theirFieldMap;
};

#endif
   
  



