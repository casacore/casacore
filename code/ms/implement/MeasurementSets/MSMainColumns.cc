//# NewMSMainColumns.cc: Easy access to NewMeasurementSet main table columns
//# Copyright (C) 2000
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

#include <aips/MeasurementSets/NewMSMainColumns.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Exceptions/Error.h>

RONewMSMainColumns::RONewMSMainColumns(const NewMeasurementSet& ms):
  antenna1_p(ms, NewMS::columnName(NewMS::ANTENNA1)),
  antenna2_p(ms, NewMS::columnName(NewMS::ANTENNA2)),
  arrayId_p(ms, NewMS::columnName(NewMS::ARRAY_ID)),
  dataDescId_p(ms, NewMS::columnName(NewMS::DATA_DESC_ID)),
  exposure_p(ms, NewMS::columnName(NewMS::EXPOSURE)),
  feed1_p(ms, NewMS::columnName(NewMS::FEED1)),
  feed2_p(ms, NewMS::columnName(NewMS::FEED2)),
  fieldId_p(ms, NewMS::columnName(NewMS::FIELD_ID)),
  flag_p(ms, NewMS::columnName(NewMS::FLAG)),
  flagCategory_p(ms, NewMS::columnName(NewMS::FLAG_CATEGORY)),
  flagRow_p(ms, NewMS::columnName(NewMS::FLAG_ROW)),
  interval_p(ms, NewMS::columnName(NewMS::INTERVAL)),
  observationId_p(ms, NewMS::columnName(NewMS::OBSERVATION_ID)),
  processorId_p(ms, NewMS::columnName(NewMS::PROCESSOR_ID)),
  scanNumber_p(ms, NewMS::columnName(NewMS::SCAN_NUMBER)),
  sigma_p(ms, NewMS::columnName(NewMS::SIGMA)),
  stateId_p(ms, NewMS::columnName(NewMS::STATE_ID)),
  time_p(ms, NewMS::columnName(NewMS::TIME)),
  timeCentroid_p(ms, NewMS::columnName(NewMS::TIME_CENTROID)),
  uvw_p(ms, NewMS::columnName(NewMS::UVW)),
  weight_p(ms, NewMS::columnName(NewMS::WEIGHT)),
  antenna3_p(),
  baselineRef_p(),
  data_p(),
  feed3_p(),
  floatData_p(),
  lagData_p(),
  phaseId_p(),
  pulsarBin_p(),
  pulsarGateId_p(),
  sigmaSpectrum_p(),
  timeExtraPrec_p(),
  uvw2_p(),
  videoPoint_p(),
  weightSpectrum_p(),
  correctedData_p(),
  imagingWeight_p(),
  modelData_p(),
  timeMeas_p(ms, NewMS::columnName(NewMS::TIME)),
  timeCentroidMeas_p(ms, NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwMeas_p(ms, NewMS::columnName(NewMS::UVW)),
  uvw2Meas_p(),
  exposureQuant_p(ms, NewMS::columnName(NewMS::EXPOSURE)),
  intervalQuant_p(ms, NewMS::columnName(NewMS::INTERVAL)),
  timeQuant_p(ms, NewMS::columnName(NewMS::TIME)),
  timeCentroidQuant_p(ms, NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwQuant_p(ms, NewMS::columnName(NewMS::UVW)),
  timeExtraPrecQuant_p(),
  uvw2Quant_p()
{
  attachOptionalCols(ms);
}

RONewMSMainColumns::~RONewMSMainColumns() {}

Vector<String> RONewMSMainColumns::flagCategories() const {
  const TableRecord& keywords = flagCategory().keywordSet();
  const RecordFieldId key("CATEGORY");
  DebugAssert(keywords.isDefined(key.fieldName()), AipsError);
  DebugAssert(keywords.dataType(key) == TpString, AipsError);
  DebugAssert(keywords.shape(key).nelements() == 1, AipsError);
  DebugAssert(nrow() == 0 || 
 	      keywords.shape(key)(0) == flagCategory().shape(0)(2), AipsError);
  return Vector<String>(keywords.asArrayString(key));
}

RONewMSMainColumns::RONewMSMainColumns():
  antenna1_p(),
  antenna2_p(),
  arrayId_p(),
  dataDescId_p(),
  exposure_p(),
  feed1_p(),
  feed2_p(),
  fieldId_p(),
  flag_p(),
  flagCategory_p(),
  flagRow_p(),
  interval_p(),
  observationId_p(),
  processorId_p(),
  scanNumber_p(),
  sigma_p(),
  stateId_p(),
  time_p(),
  timeCentroid_p(),
  uvw_p(),
  weight_p(),
  antenna3_p(),
  baselineRef_p(),
  data_p(),
  feed3_p(),
  floatData_p(),
  lagData_p(),
  phaseId_p(),
  pulsarBin_p(),
  pulsarGateId_p(),
  sigmaSpectrum_p(),
  timeExtraPrec_p(),
  uvw2_p(),
  videoPoint_p(),
  weightSpectrum_p(),
  correctedData_p(),
  imagingWeight_p(),
  modelData_p(),
  timeMeas_p(),
  timeCentroidMeas_p(),
  uvwMeas_p(),
  uvw2Meas_p(),
  exposureQuant_p(),
  intervalQuant_p(),
  timeQuant_p(),
  timeCentroidQuant_p(),
  uvwQuant_p(),
  timeExtraPrecQuant_p(),
  uvw2Quant_p()
{
}

void RONewMSMainColumns::attach(const NewMeasurementSet& ms)
{
  antenna1_p.attach(ms, NewMS::columnName(NewMS::ANTENNA1));
  antenna2_p.attach(ms, NewMS::columnName(NewMS::ANTENNA2));
  arrayId_p.attach(ms, NewMS::columnName(NewMS::ARRAY_ID));
  dataDescId_p.attach(ms, NewMS::columnName(NewMS::DATA_DESC_ID));
  exposure_p.attach(ms, NewMS::columnName(NewMS::EXPOSURE));
  feed1_p.attach(ms, NewMS::columnName(NewMS::FEED1));
  feed2_p.attach(ms, NewMS::columnName(NewMS::FEED2));
  fieldId_p.attach(ms, NewMS::columnName(NewMS::FIELD_ID));
  flag_p.attach(ms, NewMS::columnName(NewMS::FLAG));
  flagCategory_p.attach(ms, NewMS::columnName(NewMS::FLAG_CATEGORY));
  flagRow_p.attach(ms, NewMS::columnName(NewMS::FLAG_ROW));
  interval_p.attach(ms, NewMS::columnName(NewMS::INTERVAL));
  observationId_p.attach(ms, NewMS::columnName(NewMS::OBSERVATION_ID));
  processorId_p.attach(ms, NewMS::columnName(NewMS::PROCESSOR_ID));
  scanNumber_p.attach(ms, NewMS::columnName(NewMS::SCAN_NUMBER));
  sigma_p.attach(ms, NewMS::columnName(NewMS::SIGMA));
  stateId_p.attach(ms, NewMS::columnName(NewMS::STATE_ID));
  time_p.attach(ms, NewMS::columnName(NewMS::TIME));
  timeCentroid_p.attach(ms, NewMS::columnName(NewMS::TIME_CENTROID));
  uvw_p.attach(ms, NewMS::columnName(NewMS::UVW));
  weight_p.attach(ms, NewMS::columnName(NewMS::WEIGHT));
  attachOptionalCols(ms);
}

void RONewMSMainColumns::attachOptionalCols(const NewMeasurementSet& ms)
{
  const ColumnDescSet& cds=ms.tableDesc().columnDescSet();
  if (cds.isDefined(NewMS::columnName(NewMS::ANTENNA3))) {
    antenna3_p.attach(ms,NewMS::columnName(NewMS::ANTENNA3));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::BASELINE_REF))) {
    baselineRef_p.attach(ms,NewMS::columnName(NewMS::BASELINE_REF));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::DATA))) {
    data_p.attach(ms,NewMS::columnName(NewMS::DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::FEED3))) {
    feed3_p.attach(ms,NewMS::columnName(NewMS::FEED3));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::FLOAT_DATA))) {
    floatData_p.attach(ms,NewMS::columnName(NewMS::FLOAT_DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::LAG_DATA))) {
    lagData_p.attach(ms,NewMS::columnName(NewMS::LAG_DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::PHASE_ID))) {
    phaseId_p.attach(ms,NewMS::columnName(NewMS::PHASE_ID));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_BIN))) {
    pulsarBin_p.attach(ms,NewMS::columnName(NewMS::PULSAR_BIN));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_GATE_ID))) {
    pulsarGateId_p.attach(ms,NewMS::columnName(NewMS::PULSAR_GATE_ID));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::SIGMA_SPECTRUM))) {
    sigmaSpectrum_p.attach(ms,NewMS::columnName(NewMS::SIGMA_SPECTRUM));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::TIME_EXTRA_PREC))) {
    timeExtraPrec_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
    timeExtraPrecQuant_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::UVW2))) {
    uvw2_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Meas_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Quant_p.attach(ms,NewMS::columnName(NewMS::UVW2));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::VIDEO_POINT))) {
    videoPoint_p.attach(ms,NewMS::columnName(NewMS::VIDEO_POINT));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::WEIGHT_SPECTRUM))) {
    weightSpectrum_p.attach(ms,NewMS::columnName(NewMS::WEIGHT_SPECTRUM));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::CORRECTED_DATA))) {
    correctedData_p.attach(ms,NewMS::columnName(NewMS::CORRECTED_DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::IMAGING_WEIGHT))) {
    imagingWeight_p.attach(ms,NewMS::columnName(NewMS::IMAGING_WEIGHT));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::MODEL_DATA))) {
    modelData_p.attach(ms,NewMS::columnName(NewMS::MODEL_DATA));
  }
}

NewMSMainColumns::NewMSMainColumns(NewMeasurementSet& ms):
  RONewMSMainColumns(ms),
  antenna1_p(ms, NewMS::columnName(NewMS::ANTENNA1)),
  antenna2_p(ms, NewMS::columnName(NewMS::ANTENNA2)),
  arrayId_p(ms, NewMS::columnName(NewMS::ARRAY_ID)),
  dataDescId_p(ms, NewMS::columnName(NewMS::DATA_DESC_ID)),
  exposure_p(ms, NewMS::columnName(NewMS::EXPOSURE)),
  feed1_p(ms, NewMS::columnName(NewMS::FEED1)),
  feed2_p(ms, NewMS::columnName(NewMS::FEED2)),
  fieldId_p(ms, NewMS::columnName(NewMS::FIELD_ID)),
  flag_p(ms, NewMS::columnName(NewMS::FLAG)),
  flagCategory_p(ms, NewMS::columnName(NewMS::FLAG_CATEGORY)),
  flagRow_p(ms, NewMS::columnName(NewMS::FLAG_ROW)),
  interval_p(ms, NewMS::columnName(NewMS::INTERVAL)),
  observationId_p(ms, NewMS::columnName(NewMS::OBSERVATION_ID)),
  processorId_p(ms, NewMS::columnName(NewMS::PROCESSOR_ID)),
  scanNumber_p(ms, NewMS::columnName(NewMS::SCAN_NUMBER)),
  sigma_p(ms, NewMS::columnName(NewMS::SIGMA)),
  stateId_p(ms, NewMS::columnName(NewMS::STATE_ID)),
  time_p(ms, NewMS::columnName(NewMS::TIME)),
  timeCentroid_p(ms, NewMS::columnName(NewMS::TIME_CENTROID)),
  uvw_p(ms, NewMS::columnName(NewMS::UVW)),
  weight_p(ms, NewMS::columnName(NewMS::WEIGHT)),
  antenna3_p(),
  baselineRef_p(),
  data_p(),
  feed3_p(),
  floatData_p(),
  lagData_p(),
  phaseId_p(),
  pulsarBin_p(),
  pulsarGateId_p(),
  sigmaSpectrum_p(),
  timeExtraPrec_p(),
  uvw2_p(),
  videoPoint_p(),
  weightSpectrum_p(),
  correctedData_p(),
  imagingWeight_p(),
  modelData_p(),
  timeMeas_p(ms, NewMS::columnName(NewMS::TIME)),
  timeCentroidMeas_p(ms, NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwMeas_p(ms, NewMS::columnName(NewMS::UVW)),
  uvw2Meas_p(),
  exposureQuant_p(ms, NewMS::columnName(NewMS::EXPOSURE)),
  intervalQuant_p(ms, NewMS::columnName(NewMS::INTERVAL)),
  timeQuant_p(ms, NewMS::columnName(NewMS::TIME)),
  timeCentroidQuant_p(ms, NewMS::columnName(NewMS::TIME_CENTROID)),
  uvwQuant_p(ms, NewMS::columnName(NewMS::UVW)),
  timeExtraPrecQuant_p(),
  uvw2Quant_p()
{
  attachOptionalCols(ms);
}

NewMSMainColumns::~NewMSMainColumns() {}

void NewMSMainColumns::setFlagCategories(const Vector<String>& categories) {
  TableRecord& keywords = flagCategory().rwKeywordSet();
  const RecordFieldId key("CATEGORY");
  DebugAssert(nrow() == 0 || 
 	      categories.nelements() == 
	      static_cast<uInt>(flagCategory().shape(0)(2)), AipsError);
  keywords.define(key, categories);
}

void NewMSMainColumns::setEpochRef(MEpoch::Types ref) {
  timeMeas_p.setDescRefCode(ref);
  timeCentroidMeas_p.setDescRefCode(ref);
}

void NewMSMainColumns::setUVWRef(Muvw::Types ref)
{
  uvwMeas_p.setDescRefCode(ref);
  if (!uvw2_p.isNull()) {
    uvw2Meas_p.setDescRefCode(ref);
  }
}

NewMSMainColumns::NewMSMainColumns():
  RONewMSMainColumns(),
  antenna1_p(),
  antenna2_p(),
  arrayId_p(),
  dataDescId_p(),
  exposure_p(),
  feed1_p(),
  feed2_p(),
  fieldId_p(),
  flag_p(),
  flagCategory_p(),
  flagRow_p(),
  interval_p(),
  observationId_p(),
  processorId_p(),
  scanNumber_p(),
  sigma_p(),
  stateId_p(),
  time_p(),
  timeCentroid_p(),
  uvw_p(),
  weight_p(),
  antenna3_p(),
  baselineRef_p(),
  data_p(),
  feed3_p(),
  floatData_p(),
  lagData_p(),
  phaseId_p(),
  pulsarBin_p(),
  pulsarGateId_p(),
  sigmaSpectrum_p(),
  timeExtraPrec_p(),
  uvw2_p(),
  videoPoint_p(),
  weightSpectrum_p(),
  correctedData_p(),
  imagingWeight_p(),
  modelData_p(),
  timeMeas_p(),
  timeCentroidMeas_p(),
  uvwMeas_p(),
  uvw2Meas_p(),
  exposureQuant_p(),
  intervalQuant_p(),
  timeQuant_p(),
  timeCentroidQuant_p(),
  uvwQuant_p(),
  timeExtraPrecQuant_p(),
  uvw2Quant_p()
{
}

void NewMSMainColumns::attach(NewMeasurementSet& ms)
{
  RONewMSMainColumns::attach(ms);
  antenna1_p.attach(ms, NewMS::columnName(NewMS::ANTENNA1));
  antenna2_p.attach(ms, NewMS::columnName(NewMS::ANTENNA2));
  arrayId_p.attach(ms, NewMS::columnName(NewMS::ARRAY_ID));
  dataDescId_p.attach(ms, NewMS::columnName(NewMS::DATA_DESC_ID));
  exposure_p.attach(ms, NewMS::columnName(NewMS::EXPOSURE));
  feed1_p.attach(ms, NewMS::columnName(NewMS::FEED1));
  feed2_p.attach(ms, NewMS::columnName(NewMS::FEED2));
  fieldId_p.attach(ms, NewMS::columnName(NewMS::FIELD_ID));
  flag_p.attach(ms, NewMS::columnName(NewMS::FLAG));
  flagCategory_p.attach(ms, NewMS::columnName(NewMS::FLAG_CATEGORY));
  flagRow_p.attach(ms, NewMS::columnName(NewMS::FLAG_ROW));
  interval_p.attach(ms, NewMS::columnName(NewMS::INTERVAL));
  observationId_p.attach(ms, NewMS::columnName(NewMS::OBSERVATION_ID));
  processorId_p.attach(ms, NewMS::columnName(NewMS::PROCESSOR_ID));
  scanNumber_p.attach(ms, NewMS::columnName(NewMS::SCAN_NUMBER));
  sigma_p.attach(ms, NewMS::columnName(NewMS::SIGMA));
  stateId_p.attach(ms, NewMS::columnName(NewMS::STATE_ID));
  time_p.attach(ms, NewMS::columnName(NewMS::TIME));
  timeCentroid_p.attach(ms, NewMS::columnName(NewMS::TIME_CENTROID));
  uvw_p.attach(ms, NewMS::columnName(NewMS::UVW));
  weight_p.attach(ms, NewMS::columnName(NewMS::WEIGHT));
  attachOptionalCols(ms);
}

void NewMSMainColumns::attachOptionalCols(NewMeasurementSet& ms)
{
  const ColumnDescSet& cds=ms.tableDesc().columnDescSet();
  if (cds.isDefined(NewMS::columnName(NewMS::ANTENNA3))) {
    antenna3_p.attach(ms,NewMS::columnName(NewMS::ANTENNA3));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::BASELINE_REF))) {
    baselineRef_p.attach(ms,NewMS::columnName(NewMS::BASELINE_REF));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::DATA))) {
    data_p.attach(ms,NewMS::columnName(NewMS::DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::FEED3))) {
    feed3_p.attach(ms,NewMS::columnName(NewMS::FEED3));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::FLOAT_DATA))) {
    floatData_p.attach(ms,NewMS::columnName(NewMS::FLOAT_DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::LAG_DATA))) {
    lagData_p.attach(ms,NewMS::columnName(NewMS::LAG_DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::PHASE_ID))) {
    phaseId_p.attach(ms,NewMS::columnName(NewMS::PHASE_ID));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_BIN))) {
    pulsarBin_p.attach(ms,NewMS::columnName(NewMS::PULSAR_BIN));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::PULSAR_GATE_ID))) {
    pulsarGateId_p.attach(ms,NewMS::columnName(NewMS::PULSAR_GATE_ID));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::SIGMA_SPECTRUM))) {
    sigmaSpectrum_p.attach(ms,NewMS::columnName(NewMS::SIGMA_SPECTRUM));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::TIME_EXTRA_PREC))) {
    timeExtraPrec_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
    timeExtraPrecQuant_p.attach(ms,NewMS::columnName(NewMS::TIME_EXTRA_PREC));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::UVW2))) {
    uvw2_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Meas_p.attach(ms,NewMS::columnName(NewMS::UVW2));
    uvw2Quant_p.attach(ms,NewMS::columnName(NewMS::UVW2));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::VIDEO_POINT))) {
    videoPoint_p.attach(ms,NewMS::columnName(NewMS::VIDEO_POINT));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::WEIGHT_SPECTRUM))) {
    weightSpectrum_p.attach(ms,NewMS::columnName(NewMS::WEIGHT_SPECTRUM));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::CORRECTED_DATA))) {
    correctedData_p.attach(ms,NewMS::columnName(NewMS::CORRECTED_DATA));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::IMAGING_WEIGHT))) {
    imagingWeight_p.attach(ms,NewMS::columnName(NewMS::IMAGING_WEIGHT));
  }
  if (cds.isDefined(NewMS::columnName(NewMS::MODEL_DATA))) {
    modelData_p.attach(ms,NewMS::columnName(NewMS::MODEL_DATA));
  }
}
// Local Variables: 
// compile-command: "gmake NewMSMainColumns"
// End: 
