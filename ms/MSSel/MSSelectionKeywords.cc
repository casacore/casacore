//# MSSelectionKeywords.cc: selection keywords for the MS
//# Copyright (C) 1997,1998,1999,2000,2001
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
#include <casacore/ms/MSSel/MSSelectionKeywords.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSSelectionKeywords::Field MSSelectionKeywords::field(const String& itemName)
{
  // static map with enum to string mapping for fields
  static SimpleOrderedMap<String,Int>* fieldMap(0);
  static Block<Int>* reverseMap(0);

  if (!fieldMap) initMap(fieldMap,reverseMap);
 
  const Int* p=fieldMap->isDefined(itemName);
  return p ? Field(*p) : UNDEFINED;
}

const String& MSSelectionKeywords::keyword(Field fld) 
{
  static SimpleOrderedMap<String,Int>* fieldMap(0);
  static Block<Int>* reverseMap(0);

  if (!reverseMap) initMap(fieldMap,reverseMap);

  return fieldMap->getKey((*reverseMap)[fld]);
}


void MSSelectionKeywords::initMap(SimpleOrderedMap<String,Int>*& fieldMap,
				  Block<Int>*& reverseMap)
{
  static Bool initialized(False);
  static SimpleOrderedMap<String, Int> map(UNDEFINED,NUMBER_KEYWORDS);
  static Block<Int> revMap(NUMBER_KEYWORDS);

  if (!initialized) {
    map.define("undefined",UNDEFINED);
    map.define("amplitude",AMPLITUDE);
    map.define("corrected_amplitude",CORRECTED_AMPLITUDE);
    map.define("model_amplitude",MODEL_AMPLITUDE);
    map.define("ratio_amplitude",RATIO_AMPLITUDE);
    map.define("residual_amplitude",RESIDUAL_AMPLITUDE);
    map.define("obs_residual_amplitude",OBS_RESIDUAL_AMPLITUDE);
    map.define("antenna1",ANTENNA1);
    map.define("antenna2",ANTENNA2);
    map.define("antennas",ANTENNAS);
    map.define("array_id",ARRAY_ID);
    map.define("axis_info",AXIS_INFO);
    map.define("chan_freq",CHAN_FREQ);
    map.define("corr_names",CORR_NAMES);
    map.define("corr_types",CORR_TYPES);
    map.define("data",DATA);
    map.define("corrected_data",CORRECTED_DATA);
    map.define("model_data",MODEL_DATA);
    map.define("ratio_data",RATIO_DATA);
    map.define("residual_data",RESIDUAL_DATA);
    map.define("obs_residual_data",OBS_RESIDUAL_DATA);
    map.define("data_desc_id",DATA_DESC_ID);
    map.define("feed1",FEED1);
    map.define("feed2",FEED2);
    map.define("field_id",FIELD_ID);
    map.define("fields",FIELDS);
    map.define("flag",FLAG);
    map.define("flag_row",FLAG_ROW);
    map.define("flag_sum",FLAG_SUM);
    map.define("float_data",FLOAT_DATA);
    map.define("ha",HA);
    map.define("ifr_number",IFR_NUMBER);
    map.define("imaginary",IMAGINARY);
    map.define("corrected_imaginary",CORRECTED_IMAGINARY);
    map.define("model_imaginary",MODEL_IMAGINARY);
    map.define("ratio_imaginary",RATIO_IMAGINARY);
    map.define("residual_imaginary",RESIDUAL_IMAGINARY);
    map.define("obs_residual_imaginary",OBS_RESIDUAL_IMAGINARY);
    map.define("last",LAST);
    map.define("num_corr",NUM_CORR);
    map.define("num_chan",NUM_CHAN);
    map.define("phase",PHASE);
    map.define("corrected_phase",CORRECTED_PHASE);
    map.define("model_phase",MODEL_PHASE);
    map.define("ratio_phase",RATIO_PHASE);
    map.define("residual_phase",RESIDUAL_PHASE);
    map.define("obs_residual_phase",OBS_RESIDUAL_PHASE);
    map.define("phase_dir",PHASE_DIR);
    map.define("real",REAL);
    map.define("corrected_real",CORRECTED_REAL);
    map.define("model_real",MODEL_REAL);
    map.define("ratio_real",RATIO_REAL);
    map.define("residual_real",RESIDUAL_REAL);
    map.define("obs_residual_real",OBS_RESIDUAL_REAL);
    map.define("ref_frequency",REF_FREQUENCY);
    map.define("rows",ROWS);
    map.define("scan_number",SCAN_NUMBER);
    map.define("sigma",SIGMA);
    map.define("time",TIME);
    map.define("times",TIMES);
    map.define("u",U);
    map.define("v",V);
    map.define("w",W);
    map.define("ut",UT);
    map.define("uvw",UVW);
    map.define("uvdist",UVDIST);
    map.define("weight",WEIGHT);

    for (uInt i=0; i<NUMBER_KEYWORDS; i++) {
      revMap[map.getVal(i)]=i;
    }
    initialized=True;
  }
  fieldMap=&map;
  reverseMap=&revMap;
}

} //# NAMESPACE CASACORE - END

