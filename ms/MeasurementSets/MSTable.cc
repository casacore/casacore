//# MSTable.cc:  the class that hold measurements from telescopes
//# Copyright (C) 1996,1997,2000,2001,2002
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

#include <casacore/ms/MeasurementSets/MSTable.tcc>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  Int MSTableMaps::mapType (const std::map<Int,String>& nameMap, const String& name) const
  {
    // find first occurrence of name in the map (should be only occurrence)
    Int type = 0; //# 0=UNDEFINED_COLUMN for all enums
    for (const auto& kv : nameMap) {
      if (kv.second == name) {
        type = kv.first;
        break;
      }
    }
    return type;
  }


  // Instantiate the templates.
  template class MSTable<MSMainEnums>;

  template class MSTable<MSAntennaEnums>;
  template class MSTable<MSDataDescriptionEnums>;
  template class MSTable<MSDopplerEnums>;
  template class MSTable<MSFeedEnums>;
  template class MSTable<MSFieldEnums>;
  template class MSTable<MSFlagCmdEnums>;
  template class MSTable<MSFreqOffsetEnums>;
  template class MSTable<MSHistoryEnums>;
  template class MSTable<MSObservationEnums>;
  template class MSTable<MSPointingEnums>;
  template class MSTable<MSPolarizationEnums>;
  template class MSTable<MSProcessorEnums>;
  template class MSTable<MSSourceEnums>;
  template class MSTable<MSSpectralWindowEnums>;
  template class MSTable<MSStateEnums>;
  template class MSTable<MSSysCalEnums>;
  template class MSTable<MSWeatherEnums>;

} //# NAMESPACE CASACORE - END

