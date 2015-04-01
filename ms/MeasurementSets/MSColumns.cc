//# Mscolumnsc.cc:  provides easy access to MeasurementSet columns
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
//# $Id$

#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROMSColumns::ROMSColumns(const MeasurementSet& ms):
  ROMSMainColumns(ms),
  antenna_p(ms.antenna()),
  dataDesc_p(ms.dataDescription()),
  doppler_p(ms.doppler()),
  feed_p(ms.feed()),
  field_p(ms.field()),
  flagCmd_p(ms.flagCmd()),
  freqOffset_p(ms.freqOffset()),
  history_p(ms.history()),
  observation_p(ms.observation()),
  pointing_p(ms.pointing()),
  polarization_p(ms.polarization()),
  processor_p(ms.processor()),
  source_p(ms.source()),
  spectralWindow_p(ms.spectralWindow()),
  state_p(ms.state()),
  sysCal_p(ms.sysCal()),
  weather_p(ms.weather())
{
}

ROMSColumns::~ROMSColumns() {}

MSColumns::MSColumns(MeasurementSet& ms):
  MSMainColumns(ms),
  antenna_p(ms.antenna()),
  dataDesc_p(ms.dataDescription()),
  doppler_p(ms.doppler()),
  feed_p(ms.feed()),
  field_p(ms.field()),
  flagCmd_p(ms.flagCmd()),
  freqOffset_p(ms.freqOffset()),
  history_p(ms.history()),
  observation_p(ms.observation()),
  pointing_p(ms.pointing()),
  polarization_p(ms.polarization()),
  processor_p(ms.processor()),
  source_p(ms.source()),
  spectralWindow_p(ms.spectralWindow()),
  state_p(ms.state()),
  sysCal_p(ms.sysCal()),
  weather_p(ms.weather())
{
}

MSColumns::~MSColumns() {}

void MSColumns::setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty)
{
  // Adjust the relevant columns in the main table
  MSMainColumns::setEpochRef(ref, tableMustBeEmpty);
  // Now the same for the subtables.
  feed().setEpochRef(ref, tableMustBeEmpty);
  field().setEpochRef(ref, tableMustBeEmpty);
  flagCmd().setEpochRef(ref, tableMustBeEmpty);
  history().setEpochRef(ref, tableMustBeEmpty);
  observation().setEpochRef(ref, tableMustBeEmpty);
  pointing().setEpochRef(ref, tableMustBeEmpty);
  if (!freqOffset_p.isNull()) {
    freqOffset_p.setEpochRef(ref, tableMustBeEmpty);
  }
  if (!source_p.isNull()) {
    source().setEpochRef(ref, tableMustBeEmpty);
  }
  if (!sysCal_p.isNull()) {
    sysCal_p.setEpochRef(ref, tableMustBeEmpty);
  }
  if (!weather_p.isNull()) {
    weather_p.setEpochRef(ref, tableMustBeEmpty);
  }
}

void MSColumns::setDirectionRef(MDirection::Types ref)
{
  field().setDirectionRef(ref);
  pointing().setDirectionRef(ref);
  if (!source_p.isNull()) {
    source().setDirectionRef(ref);
  }
}
// Local Variables: 
// compile-command: "gmake MSColumns"
// End: 

} //# NAMESPACE CASACORE - END

