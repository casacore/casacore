//# Mscolumnsc.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Utilities/String.h>

RONewMSColumns::RONewMSColumns(const NewMeasurementSet& ms):
  RONewMSMainColumns(ms),
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

RONewMSColumns::~RONewMSColumns() {}

NewMSColumns::NewMSColumns(NewMeasurementSet& ms):
  NewMSMainColumns(ms),
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

NewMSColumns::~NewMSColumns() {}

void NewMSColumns::setEpochRef(Int ref)
{
  // Adjust the relevant columns in the main table
  NewMSMainColumns::setEpochRef(ref);
  // Now the same for the subtables.
  const String timsys(MEpoch::showType(ref));
  const String k1("MEASINFO");
  const String k2("Type");
  const String fld("refer");
  feed_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  field_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  flagCmd_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  if (!freqOffset_p.isNull()) {
    freqOffset_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
      define(fld,timsys);
  }
  history_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  observation_p.timeRange().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  observation_p.releaseDate().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  pointing_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  pointing_p.timeOrigin().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  source_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
    define(fld,timsys);
  if (!sysCal_p.isNull()) {
    sysCal_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
      define(fld,timsys);
  }
  if (!weather_p.isNull()) {
    weather_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).
      define(fld,timsys);
  }
}

void NewMSColumns::setDirectionRef(Int ref)
{
  field_p.delayDir().rwKeywordSet().rwSubRecord("MEASINFO").
    rwSubRecord("Type").define("refer",MDirection::showType(ref));
  field_p.phaseDir().rwKeywordSet().rwSubRecord("MEASINFO").
    rwSubRecord("Type").define("refer",MDirection::showType(ref));
  field_p.referenceDir().rwKeywordSet().rwSubRecord("MEASINFO").
    rwSubRecord("Type").define("refer",MDirection::showType(ref));
  pointing_p.direction().rwKeywordSet().rwSubRecord("MEASINFO").
    rwSubRecord("Type").define("refer",MDirection::showType(ref));
  pointing_p.target().rwKeywordSet().rwSubRecord("MEASINFO").
    rwSubRecord("Type").define("refer",MDirection::showType(ref));
  if (!pointing_p.pointingOffset().isNull()) {
    pointing_p.pointingOffset().rwKeywordSet().rwSubRecord("MEASINFO").
      rwSubRecord("Type").define("refer",MDirection::showType(ref));
  }
  if (!pointing_p.sourceOffset().isNull()) {
    pointing_p.sourceOffset().rwKeywordSet().rwSubRecord("MEASINFO").
      rwSubRecord("Type").define("refer",MDirection::showType(ref));
  }
  source_p.direction().rwKeywordSet().rwSubRecord("MEASINFO").
    rwSubRecord("Type").define("refer",MDirection::showType(ref));
}
