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
#include <casacore/ms/MSSel/MSSelectionKeywords.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

std::map<String,int32_t>& MSSelectionKeywords::getMap()
{
  static std::map<String,int32_t> fieldMap(initMap());
  return fieldMap;
}

Block<String>& MSSelectionKeywords::getReverseMap()
{
  static Block<String> reverseMap(initReverseMap());
  return reverseMap;
}

MSSelectionKeywords::Field MSSelectionKeywords::field(const String& itemName)
{
  std::map<String,int32_t>& fieldMap = getMap();
  std::map<String,int32_t>::iterator iter = fieldMap.find(itemName);
  return iter==fieldMap.end() ?  UNDEFINED : Field(iter->second);
}

const String& MSSelectionKeywords::keyword(Field fld) 
{
  Block<String>& reverseMap = getReverseMap();
  return reverseMap[fld];
}


std::map<String,int32_t> MSSelectionKeywords::initMap()
{
  std::map<String,int32_t> fieldMap;
  fieldMap.insert (std::make_pair("undefined",UNDEFINED));
  fieldMap.insert (std::make_pair("amplitude",AMPLITUDE));
  fieldMap.insert (std::make_pair("corrected_amplitude",CORRECTED_AMPLITUDE));
  fieldMap.insert (std::make_pair("model_amplitude",MODEL_AMPLITUDE));
  fieldMap.insert (std::make_pair("ratio_amplitude",RATIO_AMPLITUDE));
  fieldMap.insert (std::make_pair("residual_amplitude",RESIDUAL_AMPLITUDE));
  fieldMap.insert (std::make_pair("obs_residual_amplitude",OBS_RESIDUAL_AMPLITUDE));
  fieldMap.insert (std::make_pair("antenna1",ANTENNA1));
  fieldMap.insert (std::make_pair("antenna2",ANTENNA2));
  fieldMap.insert (std::make_pair("antennas",ANTENNAS));
  fieldMap.insert (std::make_pair("array_id",ARRAY_ID));
  fieldMap.insert (std::make_pair("axis_info",AXIS_INFO));
  fieldMap.insert (std::make_pair("chan_freq",CHAN_FREQ));
  fieldMap.insert (std::make_pair("corr_names",CORR_NAMES));
  fieldMap.insert (std::make_pair("corr_types",CORR_TYPES));
  fieldMap.insert (std::make_pair("data",DATA));
  fieldMap.insert (std::make_pair("corrected_data",CORRECTED_DATA));
  fieldMap.insert (std::make_pair("model_data",MODEL_DATA));
  fieldMap.insert (std::make_pair("ratio_data",RATIO_DATA));
  fieldMap.insert (std::make_pair("residual_data",RESIDUAL_DATA));
  fieldMap.insert (std::make_pair("obs_residual_data",OBS_RESIDUAL_DATA));
  fieldMap.insert (std::make_pair("data_desc_id",DATA_DESC_ID));
  fieldMap.insert (std::make_pair("feed1",FEED1));
  fieldMap.insert (std::make_pair("feed2",FEED2));
  fieldMap.insert (std::make_pair("field_id",FIELD_ID));
  fieldMap.insert (std::make_pair("fields",FIELDS));
  fieldMap.insert (std::make_pair("flag",FLAG));
  fieldMap.insert (std::make_pair("flag_row",FLAG_ROW));
  fieldMap.insert (std::make_pair("flag_sum",FLAG_SUM));
  fieldMap.insert (std::make_pair("float_data",FLOAT_DATA));
  fieldMap.insert (std::make_pair("ha",HA));
  fieldMap.insert (std::make_pair("ifr_number",IFR_NUMBER));
  fieldMap.insert (std::make_pair("imaginary",IMAGINARY));
  fieldMap.insert (std::make_pair("corrected_imaginary",CORRECTED_IMAGINARY));
  fieldMap.insert (std::make_pair("model_imaginary",MODEL_IMAGINARY));
  fieldMap.insert (std::make_pair("ratio_imaginary",RATIO_IMAGINARY));
  fieldMap.insert (std::make_pair("residual_imaginary",RESIDUAL_IMAGINARY));
  fieldMap.insert (std::make_pair("obs_residual_imaginary",OBS_RESIDUAL_IMAGINARY));
  fieldMap.insert (std::make_pair("last",LAST));
  fieldMap.insert (std::make_pair("num_corr",NUM_CORR));
  fieldMap.insert (std::make_pair("num_chan",NUM_CHAN));
  fieldMap.insert (std::make_pair("phase",PHASE));
  fieldMap.insert (std::make_pair("corrected_phase",CORRECTED_PHASE));
  fieldMap.insert (std::make_pair("model_phase",MODEL_PHASE));
  fieldMap.insert (std::make_pair("ratio_phase",RATIO_PHASE));
  fieldMap.insert (std::make_pair("residual_phase",RESIDUAL_PHASE));
  fieldMap.insert (std::make_pair("obs_residual_phase",OBS_RESIDUAL_PHASE));
  fieldMap.insert (std::make_pair("phase_dir",PHASE_DIR));
  fieldMap.insert (std::make_pair("real",REAL));
  fieldMap.insert (std::make_pair("corrected_real",CORRECTED_REAL));
  fieldMap.insert (std::make_pair("model_real",MODEL_REAL));
  fieldMap.insert (std::make_pair("ratio_real",RATIO_REAL));
  fieldMap.insert (std::make_pair("residual_real",RESIDUAL_REAL));
  fieldMap.insert (std::make_pair("obs_residual_real",OBS_RESIDUAL_REAL));
  fieldMap.insert (std::make_pair("ref_frequency",REF_FREQUENCY));
  fieldMap.insert (std::make_pair("rows",ROWS));
  fieldMap.insert (std::make_pair("scan_number",SCAN_NUMBER));
  fieldMap.insert (std::make_pair("sigma",SIGMA));
  fieldMap.insert (std::make_pair("time",TIME));
  fieldMap.insert (std::make_pair("times",TIMES));
  fieldMap.insert (std::make_pair("u",U));
  fieldMap.insert (std::make_pair("v",V));
  fieldMap.insert (std::make_pair("w",W));
  fieldMap.insert (std::make_pair("ut",UT));
  fieldMap.insert (std::make_pair("uvw",UVW));
  fieldMap.insert (std::make_pair("uvdist",UVDIST));
  fieldMap.insert (std::make_pair("weight",WEIGHT));
  // Assure all fields are defined.
  AlwaysAssert (fieldMap.size() == NUMBER_KEYWORDS, AipsError);
  return fieldMap;
}

Block<String> MSSelectionKeywords::initReverseMap()
{
  std::map<String,int32_t>& fieldMap = getMap();
  Block<String> reverseMap(NUMBER_KEYWORDS);
  for (const auto& x : fieldMap) {
    AlwaysAssert (x.second < NUMBER_KEYWORDS, AipsError);
    reverseMap[x.second] = x.first;
  }
  return reverseMap;
}

} //# NAMESPACE CASACORE - END

