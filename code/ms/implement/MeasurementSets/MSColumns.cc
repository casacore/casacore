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

//# Includes
#include <aips/MeasurementSets/NewMSColumns.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/Muvw.h>

NewMSColumns::NewMSColumns(NewMeasurementSet& ms):
  antenna1_p(ms,NewMS::columnName(NewMS::ANTENNA1)),
  antenna2_p(ms,NewMS::columnName(NewMS::ANTENNA2)),
  arrayId_p(ms,NewMS::columnName(NewMS::ARRAY_ID)),
  dataDescId_p(ms,NewMS::columnName(NewMS::DATA_DESC_ID)),
  exposure_p(ms,NewMS::columnName(NewMS::EXPOSURE)),
  feed1_p(ms,NewMS::columnName(NewMS::FEED1)),
  feed2_p(ms,NewMS::columnName(NewMS::FEED2)),
  fieldId_p(ms,NewMS::columnName(NewMS::FIELD_ID)),
  flag_p(ms,NewMS::columnName(NewMS::FLAG)),
  flagCategory_p(ms,NewMS::columnName(NewMS::FLAG_CATEGORY)),
  flagRow_p(ms,NewMS::columnName(NewMS::FLAG_ROW)),
  interval_p(ms,NewMS::columnName(NewMS::INTERVAL)),
  observationId_p(ms,NewMS::columnName(NewMS::OBSERVATION_ID)),
  processorId_p(ms,NewMS::columnName(NewMS::PROCESSOR_ID)),
  scanNumber_p(ms,NewMS::columnName(NewMS::SCAN_NUMBER)),
  sigma_p(ms,NewMS::columnName(NewMS::SIGMA)),
  stateId_p(ms,NewMS::columnName(NewMS::STATE_ID)),
  time_p(ms,NewMS::columnName(NewMS::TIME)),
  timeCentroid_p(ms,NewMS::columnName(NewMS::TIME_CENTROID)),
  uvw_p(ms,NewMS::columnName(NewMS::UVW)),
  weight_p(ms,NewMS::columnName(NewMS::WEIGHT)),
  timeMeas_p(ms,NewMS::columnName(NewMS::TIME)),
  timeCentroidMeas_p(ms,NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwMeas_p(ms,NewMS::columnName(NewMS::UVW)),
  exposureQuant_p(ms,NewMS::columnName(NewMS::EXPOSURE)),
  intervalQuant_p(ms,NewMS::columnName(NewMS::INTERVAL)),
  timeQuant_p(ms,NewMS::columnName(NewMS::TIME)),
  timeCentroidQuant_p(ms,NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwQuant_p(ms,NewMS::columnName(NewMS::UVW)),
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
  sysCal_p(ms.sysCal()),
  weather_p(ms.weather())
{
  const ColumnDescSet& cds=ms.tableDesc().columnDescSet();
  if (cds.isDefined(NewMS::columnName(NewMS::ANTENNA3)))
    antenna3_p.attach(ms,NewMS::columnName(NewMS::ANTENNA3));
  if (cds.isDefined(NewMS::columnName(NewMS::BASELINE_REF)))
    baselineRef_p.attach(ms,NewMS::columnName(NewMS::BASELINE_REF));
  if (cds.isDefined(NewMS::columnName(NewMS::CORRECTED_DATA)))
    correctedData_p.attach(ms,NewMS::columnName(NewMS::CORRECTED_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::DATA)))
    data_p.attach(ms,NewMS::columnName(NewMS::DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::FEED3)))
    feed3_p.attach(ms,NewMS::columnName(NewMS::FEED3));
  if (cds.isDefined(NewMS::columnName(NewMS::FLOAT_DATA))) 
    floatData_p.attach(ms,NewMS::columnName(NewMS::FLOAT_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::IMAGING_WEIGHT))) 
    imagingWeight_p.attach(ms,NewMS::columnName(NewMS::IMAGING_WEIGHT));
  if (cds.isDefined(NewMS::columnName(NewMS::LAG_DATA)))
    lagData_p.attach(ms,NewMS::columnName(NewMS::LAG_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::MODEL_DATA)))
    modelData_p.attach(ms,NewMS::columnName(NewMS::MODEL_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::PHASE_ID)))
    phaseId_p.attach(ms,NewMS::columnName(NewMS::PHASE_ID));
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_BIN)))
    pulsarBin_p.attach(ms,NewMS::columnName(NewMS::PULSAR_BIN));
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_GATE_ID)))
    pulsarGateId_p.attach(ms,NewMS::columnName(NewMS::PULSAR_GATE_ID));
  if (cds.isDefined(NewMS::columnName(NewMS::SIGMA_SPECTRUM))) 
    sigmaSpectrum_p.attach(ms,NewMS::columnName(NewMS::SIGMA_SPECTRUM));
  if (cds.isDefined(NewMS::columnName(NewMS::TIME_EXTRA_PREC))) {
    timeExtraPrec_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
    timeExtraPrecQuant_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::UVW2))) {
    uvw2_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Meas_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Quant_p.attach(ms,NewMS::columnName(NewMS::UVW2));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::VIDEO_POINT)))
    videoPoint_p.attach(ms,NewMS::columnName(NewMS::VIDEO_POINT));
  if (cds.isDefined(NewMS::columnName(NewMS::WEIGHT_SPECTRUM))) 
    weightSpectrum_p.attach(ms,NewMS::columnName(NewMS::WEIGHT_SPECTRUM));
}

NewMSColumns::~NewMSColumns() {}

void NewMSColumns::setEpochRef(Int ref)
{
    const String timsys(MEpoch::showType(ref));
    const String k1("MEASINFO");
    const String k2("Type");
    const String fld("refer");
    time_p.rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    timeCentroid_p.rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    feed_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    field_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    flagCmd_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    if (!freqOffset_p.isNull())
      freqOffset_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    history_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    observation_p.timeRange().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    observation_p.releaseDate().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    pointing_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    pointing_p.timeOrigin().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    source_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    if (!sysCal_p.isNull())
      sysCal_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
    if (!weather_p.isNull())
      weather_p.time().rwKeywordSet().rwSubRecord(k1).rwSubRecord(k2).define(fld,timsys);
}

void NewMSColumns::setUVWRef(Int ref)
{
  uvw_p.rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",Muvw::showType(ref));
  if (!uvw2_p.isNull()) uvw2_p.rwKeywordSet().rwSubRecord("MEASINFO").
    rwSubRecord("Type").define("refer",Muvw::showType(ref));
}

void NewMSColumns::setDirectionRef(Int ref)
{
  field_p.delayDir().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
  field_p.phaseDir().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
  field_p.referenceDir().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
  pointing_p.direction().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
  pointing_p.target().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
  if (!pointing_p.pointingOffset().isNull()) 
    pointing_p.pointingOffset().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
  if (!pointing_p.sourceOffset().isNull()) 
    pointing_p.sourceOffset().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
  source_p.direction().rwKeywordSet().rwSubRecord("MEASINFO").rwSubRecord("Type").
    define("refer",MDirection::showType(ref));
}

  
RONewMSColumns::RONewMSColumns(const NewMeasurementSet& ms):
  antenna1_p(ms,NewMS::columnName(NewMS::ANTENNA1)),
  antenna2_p(ms,NewMS::columnName(NewMS::ANTENNA2)),
  arrayId_p(ms,NewMS::columnName(NewMS::ARRAY_ID)),
  dataDescId_p(ms,NewMS::columnName(NewMS::DATA_DESC_ID)),
  exposure_p(ms,NewMS::columnName(NewMS::EXPOSURE)),
  feed1_p(ms,NewMS::columnName(NewMS::FEED1)),
  feed2_p(ms,NewMS::columnName(NewMS::FEED2)),
  fieldId_p(ms,NewMS::columnName(NewMS::FIELD_ID)),
  flag_p(ms,NewMS::columnName(NewMS::FLAG)),
  flagCategory_p(ms,NewMS::columnName(NewMS::FLAG_CATEGORY)),
  flagRow_p(ms,NewMS::columnName(NewMS::FLAG_ROW)),
  interval_p(ms,NewMS::columnName(NewMS::INTERVAL)),
  observationId_p(ms,NewMS::columnName(NewMS::OBSERVATION_ID)),
  processorId_p(ms,NewMS::columnName(NewMS::PROCESSOR_ID)),
  scanNumber_p(ms,NewMS::columnName(NewMS::SCAN_NUMBER)),
  sigma_p(ms,NewMS::columnName(NewMS::SIGMA)),
  stateId_p(ms,NewMS::columnName(NewMS::STATE_ID)),
  time_p(ms,NewMS::columnName(NewMS::TIME)),
  uvw_p(ms,NewMS::columnName(NewMS::UVW)),
  weight_p(ms,NewMS::columnName(NewMS::WEIGHT)),
  timeMeas_p(ms,NewMS::columnName(NewMS::TIME)),
  timeCentroidMeas_p(ms,NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwMeas_p(ms,NewMS::columnName(NewMS::UVW)),
  exposureQuant_p(ms,NewMS::columnName(NewMS::EXPOSURE)),
  intervalQuant_p(ms,NewMS::columnName(NewMS::INTERVAL)),
  timeQuant_p(ms,NewMS::columnName(NewMS::TIME)),
  timeCentroidQuant_p(ms,NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwQuant_p(ms,NewMS::columnName(NewMS::UVW)),
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
  sysCal_p(ms.sysCal()),
  weather_p(ms.weather())
{
  const ColumnDescSet& cds=ms.tableDesc().columnDescSet();
  if (cds.isDefined(NewMS::columnName(NewMS::ANTENNA3)))
    antenna3_p.attach(ms,NewMS::columnName(NewMS::ANTENNA3));
  if (cds.isDefined(NewMS::columnName(NewMS::BASELINE_REF)))
    baselineRef_p.attach(ms,NewMS::columnName(NewMS::BASELINE_REF));
  if (cds.isDefined(NewMS::columnName(NewMS::CORRECTED_DATA)))
    correctedData_p.attach(ms,NewMS::columnName(NewMS::CORRECTED_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::DATA)))
    data_p.attach(ms,NewMS::columnName(NewMS::DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::FEED3)))
    feed3_p.attach(ms,NewMS::columnName(NewMS::FEED3));
  if (cds.isDefined(NewMS::columnName(NewMS::FLOAT_DATA))) 
    floatData_p.attach(ms,NewMS::columnName(NewMS::FLOAT_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::IMAGING_WEIGHT))) 
    imagingWeight_p.attach(ms,NewMS::columnName(NewMS::IMAGING_WEIGHT));
  if (cds.isDefined(NewMS::columnName(NewMS::LAG_DATA)))
    lagData_p.attach(ms,NewMS::columnName(NewMS::LAG_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::MODEL_DATA)))
    modelData_p.attach(ms,NewMS::columnName(NewMS::MODEL_DATA));
  if (cds.isDefined(NewMS::columnName(NewMS::PHASE_ID)))
    phaseId_p.attach(ms,NewMS::columnName(NewMS::PHASE_ID));
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_BIN)))
    pulsarBin_p.attach(ms,NewMS::columnName(NewMS::PULSAR_BIN));
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_GATE_ID)))
    pulsarGateId_p.attach(ms,NewMS::columnName(NewMS::PULSAR_GATE_ID));
  if (cds.isDefined(NewMS::columnName(NewMS::SIGMA_SPECTRUM))) 
    sigmaSpectrum_p.attach(ms,NewMS::columnName(NewMS::SIGMA_SPECTRUM));
  if (cds.isDefined(NewMS::columnName(NewMS::TIME_EXTRA_PREC))) {
    timeExtraPrec_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
    timeExtraPrecQuant_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::UVW2))) {
    uvw2_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Meas_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Quant_p.attach(ms,NewMS::columnName(NewMS::UVW2));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::VIDEO_POINT)))
    videoPoint_p.attach(ms,NewMS::columnName(NewMS::VIDEO_POINT));
  if (cds.isDefined(NewMS::columnName(NewMS::WEIGHT_SPECTRUM))) 
    weightSpectrum_p.attach(ms,NewMS::columnName(NewMS::WEIGHT_SPECTRUM));
}

RONewMSColumns::~RONewMSColumns() {}
