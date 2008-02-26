//# MSCalEnums.cc: Implementation of MSCalEnums.h
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
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
//----------------------------------------------------------------------------

#include <msvis/MSVis/MSCalEnums.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//----------------------------------------------------------------------------

// Static data initialization
SimpleOrderedMap <Int, String> MSCalEnums::theirFieldMap ("");
SimpleOrderedMap <Int, DataType> MSCalEnums::theirTypeMap (TpOther);

//----------------------------------------------------------------------------

void MSCalEnums::initMaps ()
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
    theirFieldMap.define (PROCESSOR_ID, "PROCESSOR_ID");
    theirFieldMap.define (FIELD_ID, "FIELD_ID");
    theirFieldMap.define (OBSERVATION_ID, "OBSERVATION_ID");
    theirFieldMap.define (PULSAR_GATE_ID, "PULSAR_GATE_ID");
    theirFieldMap.define (SPECTRAL_WINDOW_ID, "SPECTRAL_WINDOW_ID");
    theirFieldMap.define (PHASE_ID, "PHASE_ID");
    theirFieldMap.define (STATE_ID, "STATE_ID");

    theirFieldMap.define (FREQ_GROUP, "FREQ_GROUP");
    theirFieldMap.define (FREQ_GROUP_NAME, "FREQ_GROUP_NAME");
    theirFieldMap.define (FIELD_NAME, "FIELD_NAME");
    theirFieldMap.define (FIELD_CODE, "FIELD_CODE");
    theirFieldMap.define (SOURCE_NAME, "SOURCE_NAME");
    theirFieldMap.define (SOURCE_CODE, "SOURCE_CODE");
    theirFieldMap.define (CALIBRATION_GROUP, "CALIBRATION_GROUP");

    theirFieldMap.define (GAIN, "GAIN");
    theirFieldMap.define (REF_ANT, "REF_ANT");
    theirFieldMap.define (REF_FEED, "REF_FEED"); 
    theirFieldMap.define (REF_RECEPTOR, "REF_RECEPTOR");
    theirFieldMap.define (REF_FREQUENCY, "REF_FREQUENCY");
    theirFieldMap.define (MEAS_FREQ_REF, "MEAS_FREQ_REF");
    theirFieldMap.define (REF_DIRECTION, "REF_DIRECTION");
    theirFieldMap.define (MEAS_DIR_REF, "MEAS_DIR_REF");
    theirFieldMap.define (POINTING_OFFSET, "POINTING_OFFSET");
    theirFieldMap.define (MEAS_POINTING, "MEAS_POINTING");
    theirFieldMap.define (CAL_DESC_ID, "CAL_DESC_ID");
    theirFieldMap.define (CAL_HISTORY_ID, "CAL_HISTORY_ID");
    
    theirFieldMap.define (TOTAL_SOLUTION_OK, "TOTAL_SOLUTION_OK");
    theirFieldMap.define (TOTAL_FIT, "TOTAL_FIT");
    theirFieldMap.define (TOTAL_FIT_WEIGHT, "TOTAL_FIT_WEIGHT");
    theirFieldMap.define (SOLUTION_OK, "SOLUTION_OK");
    theirFieldMap.define (FIT, "FIT");
    theirFieldMap.define (FIT_WEIGHT, "FIT_WEIGHT");
    theirFieldMap.define (FLAG, "FLAG");
    theirFieldMap.define (SNR, "SNR");
    
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
    
    theirFieldMap.define (ROT_MEASURE, "ROT_MEASURE");
    theirFieldMap.define (ROT_MEASURE_ERROR, "ROT_MEASURE_ERROR");
    theirFieldMap.define (IONOSPH_TEC, "IONOSPH_TEC");
    theirFieldMap.define (IONOSPH_TEC_ERROR, "IONOSPH_TEC_ERROR");

    theirFieldMap.define (PHASE_OFFSET, "PHASE_OFFSET");
    theirFieldMap.define (SB_DELAY, "SB_DELAY");
    theirFieldMap.define (DELAY_RATE, "DELAY_RATE");

    theirFieldMap.define (POLY_TYPE, "POLY_TYPE");
    theirFieldMap.define (POLY_MODE, "POLY_MODE");
    theirFieldMap.define (SCALE_FACTOR, "SCALE_FACTOR");
    theirFieldMap.define (VALID_DOMAIN, "VALID_DOMAIN");
    theirFieldMap.define (N_POLY_AMP, "N_POLY_AMP");
    theirFieldMap.define (N_POLY_PHASE, "N_POLY_PHASE");
    theirFieldMap.define (POLY_COEFF_AMP, "POLY_COEFF_AMP");
    theirFieldMap.define (POLY_COEFF_PHASE, "POLY_COEFF_PHASE");
    theirFieldMap.define (PHASE_UNITS, "PHASE_UNITS");

    theirFieldMap.define (SIDEBAND_REF, "SIDEBAND_REF");

    theirFieldMap.define (N_KNOTS_AMP, "N_KNOTS_AMP");
    theirFieldMap.define (N_KNOTS_PHASE, "N_KNOTS_PHASE");
    theirFieldMap.define (SPLINE_KNOTS_AMP, "SPLINE_KNOTS_AMP");
    theirFieldMap.define (SPLINE_KNOTS_PHASE, "SPLINE_KNOTS_PHASE");
  };

// Initialize the static map containing the basic field data types
// Skip this step if already initialized.
//
  if (!theirTypeMap.ndefined()) {
    theirTypeMap.define (ANTENNA1, TpInt);
    theirTypeMap.define (ANTENNA2, TpInt);
    theirTypeMap.define (FEED1, TpInt);
    theirTypeMap.define (FEED2, TpInt);
    theirTypeMap.define (PULSAR_BIN, TpInt);
    theirTypeMap.define (SCAN_NUMBER, TpInt);
    theirTypeMap.define (TIME, TpDouble);
    theirTypeMap.define (TIME_EXTRA_PREC, TpDouble);
    theirTypeMap.define (INTERVAL, TpDouble);
    theirTypeMap.define (ARRAY_ID, TpInt);
    theirTypeMap.define (PROCESSOR_ID, TpInt);
    theirTypeMap.define (FIELD_ID, TpInt);
    theirTypeMap.define (OBSERVATION_ID, TpInt);
    theirTypeMap.define (PULSAR_GATE_ID, TpInt);
    theirTypeMap.define (SPECTRAL_WINDOW_ID, TpInt);
    theirTypeMap.define (PHASE_ID, TpInt);
    theirTypeMap.define (STATE_ID, TpInt);

    theirTypeMap.define (FREQ_GROUP, TpInt);
    theirTypeMap.define (FREQ_GROUP_NAME, TpString);
    theirTypeMap.define (FIELD_NAME, TpString);
    theirTypeMap.define (FIELD_CODE, TpString);
    theirTypeMap.define (SOURCE_NAME, TpString);
    theirTypeMap.define (SOURCE_CODE, TpString);
    theirTypeMap.define (CALIBRATION_GROUP, TpInt);

    theirTypeMap.define (GAIN, TpComplex);
    theirTypeMap.define (REF_ANT, TpInt);
    theirTypeMap.define (REF_FEED, TpInt); 
    theirTypeMap.define (REF_RECEPTOR, TpInt);
    theirTypeMap.define (REF_FREQUENCY, TpDouble);
    theirTypeMap.define (MEAS_FREQ_REF, TpInt);
    theirTypeMap.define (REF_DIRECTION, TpDouble);
    theirTypeMap.define (MEAS_DIR_REF, TpInt);
    theirTypeMap.define (CAL_DESC_ID, TpInt);
    theirTypeMap.define (CAL_HISTORY_ID, TpInt);
    
    theirTypeMap.define (TOTAL_SOLUTION_OK, TpBool);
    theirTypeMap.define (TOTAL_FIT, TpFloat);
    theirTypeMap.define (TOTAL_FIT_WEIGHT, TpFloat);
    theirTypeMap.define (SOLUTION_OK, TpBool);
    theirTypeMap.define (FIT, TpFloat);
    theirTypeMap.define (FIT_WEIGHT, TpFloat);
    theirTypeMap.define (FLAG, TpBool);
    theirTypeMap.define (SNR, TpFloat);
    
    theirTypeMap.define (NUM_SPW, TpInt);
    theirTypeMap.define (NUM_CHAN, TpInt);
    theirTypeMap.define (NUM_RECEPTORS, TpInt);
    theirTypeMap.define (N_JONES, TpInt);
    theirTypeMap.define (CHAN_FREQ, TpDouble);
    theirTypeMap.define (CHAN_WIDTH, TpDouble); 
    theirTypeMap.define (CHAN_RANGE, TpInt);
    theirTypeMap.define (JONES_TYPE, TpString);
    theirTypeMap.define (POLARIZATION_TYPE, TpString);
    theirTypeMap.define (MS_NAME, TpString);
    
    theirTypeMap.define (CAL_PARMS, TpString);
    theirTypeMap.define (CAL_TABLES, TpString);
    theirTypeMap.define (CAL_SELECT, TpString);
    theirTypeMap.define (CAL_NOTES, TpString);
    
    theirTypeMap.define (CAL_DESC, TpTable);
    theirTypeMap.define (CAL_HISTORY, TpTable);
    
    theirTypeMap.define (ROT_MEASURE, TpFloat);
    theirTypeMap.define (ROT_MEASURE_ERROR, TpFloat);
    theirTypeMap.define (IONOSPH_TEC, TpFloat);
    theirTypeMap.define (IONOSPH_TEC_ERROR, TpFloat);

    theirTypeMap.define (PHASE_OFFSET, TpFloat);
    theirTypeMap.define (SB_DELAY, TpFloat);
    theirTypeMap.define (DELAY_RATE, TpFloat);

    theirTypeMap.define (POLY_TYPE, TpString);
    theirTypeMap.define (POLY_MODE, TpString);
    theirTypeMap.define (SCALE_FACTOR, TpComplex);
    theirTypeMap.define (VALID_DOMAIN, TpDouble);
    theirTypeMap.define (N_POLY_AMP, TpInt);
    theirTypeMap.define (N_POLY_PHASE, TpInt);
    theirTypeMap.define (POLY_COEFF_AMP, TpDouble);
    theirTypeMap.define (POLY_COEFF_PHASE, TpDouble);
    theirTypeMap.define (PHASE_UNITS, TpString);

    theirTypeMap.define (SIDEBAND_REF, TpComplex);

    theirTypeMap.define (N_KNOTS_AMP, TpInt);
    theirTypeMap.define (N_KNOTS_PHASE, TpInt);
    theirTypeMap.define (SPLINE_KNOTS_AMP, TpDouble);
    theirTypeMap.define (SPLINE_KNOTS_PHASE, TpDouble);
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
  if (!theirFieldMap.ndefined()) initMaps();
  
  // Return the column name
  return theirFieldMap (enumField);
};

//----------------------------------------------------------------------------

Block<String> MSCalEnums::fieldNames (const Vector<Int>& enumFields)
{
// Static function to look up a set of field names:
// Inputs:
//    enumFields  const Vector<Int>&     Field enumerations.
// Outputs:
//    fieldNames  Block<String>          Field names.
// Exceptions:
//    Exception if invalid field enumeration.
//
  // Return the column names
  uInt nFields = enumFields.nelements();
  Block<String> names(nFields);
  for (uInt i=0; i < nFields; i++) {
    names[i] = fieldName (enumFields(i));
  };
  return names;
};

//----------------------------------------------------------------------------

DataType MSCalEnums::basicType (Int enumField)
{
// Static function to look up the basic field data type:
// Inputs:
//    enumField   Int        Field enumeration.
// Outputs:
//    basicType   DataType   Basic data type
// Exceptions:
//    Exception if invalid field enumeration.
//
  // Initialize map if empty
  if (!theirTypeMap.ndefined()) initMaps();
  
  // Return the column name
  return theirTypeMap (enumField);
};

//----------------------------------------------------------------------------







} //# NAMESPACE CASA - END

