//# MSCalEnums.cc: Implementation of MSCalEnums.h
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
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: 
//----------------------------------------------------------------------------

#include <trial/MeasurementComponents/MSCalEnums.h>

//----------------------------------------------------------------------------

// Static data initialization
SimpleOrderedMap <Int, String> MSCalEnums::theirFieldMap ("");

//----------------------------------------------------------------------------

void MSCalEnums::initMap ()
{
// Initialize the static map containing the field names.
// Skip this step if already initialized.
//
  if (!theirFieldMap.ndefined()) {
    theirFieldMap.define (ANTENNA1, "ANTENNA1");
    theirFieldMap.define (ANTENNA2, "ANTENNA2");
    theirFieldMap.define (FEED1, "FEED1");
    theirFieldMap.define (FEED2, "FEED2");
    theirFieldMap.define (PULSAR_BIN, "PULSAR_BIN");
    theirFieldMap.define (SCAN_NUMBER, "SCAN_NUMBER");
    theirFieldMap.define (TIME, "TIME");
    theirFieldMap.define (TIME_EXTRA_PREC, "TIME_EXTRA_PREC");
    theirFieldMap.define (INTERVAL, "INTERVAL");
    theirFieldMap.define (ARRAY_ID, "ARRAY_ID");
    theirFieldMap.define (CORRELATOR_ID, "CORRELATOR_ID");
    theirFieldMap.define (FIELD_ID, "FIELD_ID");
    theirFieldMap.define (OBSERVATION_ID, "OBSERVATION_ID");
    theirFieldMap.define (PULSAR_GATE_ID, "PULSAR_GATE_ID ");
    theirFieldMap.define (SPECTRAL_WINDOW_ID, "SPECTRAL_WINDOW_ID");

    theirFieldMap.define (FREQ_GROUP, "FREQ_GROUP");
    theirFieldMap.define (FIELD_NAME, "FIELD_NAME");
    theirFieldMap.define (SOURCE_NAME, "SOURCE_NAME");

    theirFieldMap.define (GAIN, "GAIN");
    theirFieldMap.define (REF_ANT, "REF_ANT");
    theirFieldMap.define (REF_FEED, "REF_FEED"); 
    theirFieldMap.define (REF_RECEPTOR, "REF_RECEPTOR");
    theirFieldMap.define (REF_FREQUENCY, "REF_FREQUENCY");
    theirFieldMap.define (REF_DIRECTION, "REF_DIRECTION");
    theirFieldMap.define (CAL_DESC_ID, "CAL_DESC_ID");
    theirFieldMap.define (CAL_HISTORY_ID, "CAL_HISTORY_ID");
    
    theirFieldMap.define (TOTAL_SOLUTION_OK, "TOTAL_SOLUTION_OK");
    theirFieldMap.define (TOTAL_FIT, "TOTAL_FIT");
    theirFieldMap.define (TOTAL_FIT_WEIGHT, "TOTAL_FIT_WEIGHT");
    theirFieldMap.define (SOLUTION_OK, "SOLUTION_OK");
    theirFieldMap.define (FIT, "FIT");
    theirFieldMap.define (FIT_WEIGHT, "FIT_WEIGHT");
    
    theirFieldMap.define (NUM_SPW, "NUM_SPW");
    theirFieldMap.define (NUM_CHAN, "NUM_CHAN");
    theirFieldMap.define (NUM_RECEPTORS, "NUM_RECEPTORS");
    theirFieldMap.define (N_JONES, "N_JONES");
    theirFieldMap.define (CHAN_FREQ, "CHAN_FREQ");
    theirFieldMap.define (CHAN_WIDTH, "CHAN_WIDTH"); 
    theirFieldMap.define (CHAN_RANGE, "CHAN_RANGE");
    theirFieldMap.define (JONES_TYPE, "JONES_TYPE");
    theirFieldMap.define (POLARIZATION_TYPE, "POLARIZATION_TYPE");
    theirFieldMap.define (MS_NAME, "MS_NAME");
    
    theirFieldMap.define (CAL_PARMS, "CAL_PARMS");
    theirFieldMap.define (CAL_TABLES, "CAL_TABLES");
    theirFieldMap.define (CAL_SELECT, "CAL_SELECT");
    theirFieldMap.define (CAL_NOTES, "CAL_NOTES");
    
    theirFieldMap.define (CAL_DESC, "CAL_DESC");
    theirFieldMap.define (CAL_HISTORY, "CAL_HISTORY");
  };
};

//----------------------------------------------------------------------------

String MSCalEnums::fieldName (Int enumField)
{
// Static function to look up the field name:
// Inputs:
//    enumField   Int     Field enumeration.
// Outputs:
//    fieldName   String  Field name.
// Exceptions:
//    Exception if invalid field enumeration.
//
  // Initialize map if empty
  if (!theirFieldMap.ndefined()) initMap();
  
  // Return the column name
  return theirFieldMap (enumField);
};

//----------------------------------------------------------------------------







