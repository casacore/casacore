//# MSCalEnums.h: Field name dictionary for MS and CAL tables
//# Copyright (C) 1996,1997
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
  // Feed identifier
  FEED1 = 1,
  // Pulsar bin number
  PULSAR_BIN = 2,
  // Scan number
  SCAN_NUMBER = 3,
  // Time
  TIME = 4,
  // Extra time precision
  TIME_EXTRA_PREC = 5,
  // Array identifier
  ARRAY_ID = 6,
  // Correlator identifier
  CORRELATOR_ID = 7,
  // Field identifier
  FIELD_ID = 8,
  // Observation identifier
  OBSERVATION_ID = 9,
  // Pulsar gate
  PULSAR_GATE_ID = 10,
  // Spectral window identifier
  SPECTRAL_WINDOW_ID = 11,
  
  // Other secondary MeasurementSet columns
  //
  // Frequency group
  FREQ_GROUP = 12,
  // Field name
  FIELD_NAME = 13,
  // Source name
  SOURCE_NAME = 14, 
  
  // Other calibration table columns
  //
  // Solution interval
  SOLINT = 15,
  // Gain matrix values
  GAIN = 16,
  // Reference antenna
  REF_ANT = 17,
  // Reference feed
  REF_FEED = 18,
  // Reference receptor
  REF_RECEPTOR = 19,
  // Reference frequency
  REF_FREQUENCY = 20,
  // Reference direction
  REF_DIRECTION = 21,
  // Calibration format descriptor
  CAL_DESC_ID = 22, 
  // Calibration history identifier
  CAL_HISTORY_ID = 23,

  // Solution statistics and weights
  //
  // Solution validity flag
  TOTAL_SOLUTION_OK = 24,
  // RMS of fit
  TOTAL_FIT = 25,
  // Sum of fit weights
  TOTAL_FIT_WEIGHT = 26,
  // Solution validity flag (per solution)
  SOLUTION_OK = 27,
  // RMS of fit (per solution)
  FIT = 28,
  // Sum of fit weights (per solution)
  FIT_WEIGHT = 29,
  
  // Calibration description columns
  //
  // Number of spectral windows
  NUM_SPW = 30,
  // Number of frequency channels
  NUM_CHAN = 31,
  // Number of receptors
  NUM_RECEPTORS = 32,
  // Midpoint of frequency channels
  CHAN_FREQ = 33,
  // Frequency channel width
  CHAN_WIDTH = 34,
  // Jones matrix type
  JONES_TYPE = 35, 
  // Polarization enumerations
  POLARIZATION_TYPE = 36,
  // MeasurementSet name
  MS_NAME = 37,
  
  // Calibration history columns
  //
  // Application parameter values
  CAL_PARMS = 38,
  // Associated calibration tables
  CAL_TABLES = 39,
  // Calibration selection
  CAL_SELECT = 40,
  // Calibration notes
  CAL_NOTES = 41,

  // Keywords
  //
  // Cal Desc sub-table
  CAL_DESC = 42,
  // Cal History sub-table
  CAL_HISTORY = 43};

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
   
  



