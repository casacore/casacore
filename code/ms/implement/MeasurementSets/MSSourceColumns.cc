//# NewMSSourceColumns.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSSourceColumns.h>
#include <aips/MeasurementSets/NewMSSource.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MRadialVelocity.h>

RONewMSSourceColumns::RONewMSSourceColumns(const NewMSSource& msSource):
  isNull_p(True),
  calibrationGroup_p(),
  code_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numLines_p(),
  properMotion_p(),
  spectralWindowId_p(),
  time_p(),
  position_p(),
  pulsarId_p(),
  restFrequency_p(),
  sourceModel_p(),
  sysvel_p(),
  transition_p(),
  directionMeas_p(),
  timeMeas_p(),
  positionMeas_p(),
  restFrequencyMeas_p(),
  sysvelMeas_p(),
  directionQuant_p(),
  intervalQuant_p(),
  properMotionQuant_p(),
  timeQuant_p(),
  positionQuant_p(),
  restFrequencyQuant_p(),
  sysvelQuant_p()
{ 
  attach(msSource);
}

RONewMSSourceColumns::~RONewMSSourceColumns() {}

RONewMSSourceColumns::RONewMSSourceColumns():
  isNull_p(True),
  calibrationGroup_p(),
  code_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numLines_p(),
  properMotion_p(),
  sourceId_p(),
  spectralWindowId_p(),
  time_p(),
  position_p(),
  pulsarId_p(),
  restFrequency_p(),
  sourceModel_p(),
  sysvel_p(),
  transition_p(),
  directionMeas_p(),
  timeMeas_p(),
  positionMeas_p(),
  restFrequencyMeas_p(),
  sysvelMeas_p(),
  directionQuant_p(),
  intervalQuant_p(),
  properMotionQuant_p(),
  timeQuant_p(),
  positionQuant_p(),
  restFrequencyQuant_p(),
  sysvelQuant_p()
{
}

void RONewMSSourceColumns::attach(const NewMSSource& msSource)
{
  isNull_p = msSource.isNull();
  if (!isNull()) {
    calibrationGroup_p.attach(msSource, NewMSSource::
			      columnName(NewMSSource::CALIBRATION_GROUP));
    code_p.attach(msSource, NewMSSource::columnName(NewMSSource::CODE));
    direction_p.attach(msSource, NewMSSource::
		       columnName(NewMSSource::DIRECTION));
    interval_p.attach(msSource, NewMSSource::
		      columnName(NewMSSource::INTERVAL));
    name_p.attach(msSource, NewMSSource::columnName(NewMSSource::NAME));
    numLines_p.attach(msSource, NewMSSource::
		      columnName(NewMSSource::NUM_LINES));
    properMotion_p.attach(msSource, NewMSSource::
			  columnName(NewMSSource::PROPER_MOTION));
    sourceId_p.attach(msSource, NewMSSource::
		      columnName(NewMSSource::SOURCE_ID));
    spectralWindowId_p.attach(msSource, NewMSSource::
			      columnName(NewMSSource::SPECTRAL_WINDOW_ID));
    time_p.attach(msSource, NewMSSource::columnName(NewMSSource::TIME));
    directionMeas_p.attach(msSource, NewMSSource::
			   columnName(NewMSSource::DIRECTION));
    timeMeas_p.attach(msSource, NewMSSource::columnName(NewMSSource::TIME));
    directionQuant_p.attach(msSource, NewMSSource::
			    columnName(NewMSSource::DIRECTION));
    intervalQuant_p.attach(msSource, NewMSSource::
			   columnName(NewMSSource::INTERVAL));
    properMotionQuant_p.attach(msSource, NewMSSource::
			       columnName(NewMSSource::PROPER_MOTION));
    timeQuant_p.attach(msSource, NewMSSource::columnName(NewMSSource::TIME));
    attachOptionalCols(msSource);
  }
}

void RONewMSSourceColumns::attachOptionalCols(const NewMSSource& msSource)
{
  const ColumnDescSet& cds = msSource.tableDesc().columnDescSet();
  const String& position = NewMSSource::columnName(NewMSSource::POSITION);
  if (cds.isDefined(position)) {
    position_p.attach(msSource, position);
    positionMeas_p.attach(msSource, position);
    positionQuant_p.attach(msSource, position);
  }
  const String& pulsarId = NewMSSource::columnName(NewMSSource::PULSAR_ID);
  if (cds.isDefined(pulsarId)) pulsarId_p.attach(msSource, pulsarId);
  const String& restFrequency = 
    NewMSSource::columnName(NewMSSource::REST_FREQUENCY);
  if (cds.isDefined(restFrequency)) {
    restFrequency_p.attach(msSource, restFrequency);
    restFrequencyMeas_p.attach(msSource, restFrequency);
    restFrequencyQuant_p.attach(msSource, restFrequency);
  }
  const String& sourceModel = 
    NewMSSource::columnName(NewMSSource::SOURCE_MODEL);
  if (cds.isDefined(sourceModel)) sourceModel_p.attach(msSource, sourceModel);
  const String& sysvel = NewMSSource::columnName(NewMSSource::SYSVEL);
  if (cds.isDefined(sysvel)) {
    sysvel_p.attach(msSource, sysvel);
    sysvelMeas_p.attach(msSource, sysvel);
    sysvelQuant_p.attach(msSource, sysvel);
  }
  const String& transition = NewMSSource::columnName(NewMSSource::TRANSITION);
  if (cds.isDefined(transition)) transition_p.attach(msSource, transition);
}

NewMSSourceColumns::NewMSSourceColumns(NewMSSource& msSource):
  RONewMSSourceColumns(),
  calibrationGroup_p(),
  code_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numLines_p(),
  properMotion_p(),
  spectralWindowId_p(),
  time_p(),
  position_p(),
  pulsarId_p(),
  restFrequency_p(),
  sourceModel_p(),
  sysvel_p(),
  transition_p(),
  directionMeas_p(),
  timeMeas_p(),
  positionMeas_p(),
  restFrequencyMeas_p(),
  sysvelMeas_p(),
  directionQuant_p(),
  intervalQuant_p(),
  properMotionQuant_p(),
  timeQuant_p(),
  positionQuant_p(),
  restFrequencyQuant_p(),
  sysvelQuant_p()
{ 
  attach(msSource);
}

NewMSSourceColumns::~NewMSSourceColumns() {}

NewMSSourceColumns::NewMSSourceColumns():
  RONewMSSourceColumns(),
  calibrationGroup_p(),
  code_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numLines_p(),
  properMotion_p(),
  sourceId_p(),
  spectralWindowId_p(),
  time_p(),
  position_p(),
  pulsarId_p(),
  restFrequency_p(),
  sourceModel_p(),
  sysvel_p(),
  transition_p(),
  directionMeas_p(),
  timeMeas_p(),
  positionMeas_p(),
  restFrequencyMeas_p(),
  sysvelMeas_p(),
  directionQuant_p(),
  intervalQuant_p(),
  properMotionQuant_p(),
  timeQuant_p(),
  positionQuant_p(),
  restFrequencyQuant_p(),
  sysvelQuant_p()
{
}

void NewMSSourceColumns::attach(NewMSSource& msSource)
{
  RONewMSSourceColumns::attach(msSource);
  if (!isNull()) {
    calibrationGroup_p.attach(msSource, NewMSSource::
			      columnName(NewMSSource::CALIBRATION_GROUP));
    code_p.attach(msSource, NewMSSource::columnName(NewMSSource::CODE));
    direction_p.attach(msSource, NewMSSource::
		       columnName(NewMSSource::DIRECTION));
    interval_p.attach(msSource, NewMSSource::
		      columnName(NewMSSource::INTERVAL));
    name_p.attach(msSource, NewMSSource::columnName(NewMSSource::NAME));
    numLines_p.attach(msSource, NewMSSource::
		      columnName(NewMSSource::NUM_LINES));
    properMotion_p.attach(msSource, NewMSSource::
			  columnName(NewMSSource::PROPER_MOTION));
    sourceId_p.attach(msSource, NewMSSource::
		      columnName(NewMSSource::SOURCE_ID));
    spectralWindowId_p.attach(msSource, NewMSSource::
			      columnName(NewMSSource::SPECTRAL_WINDOW_ID));
    time_p.attach(msSource, NewMSSource::columnName(NewMSSource::TIME));
    directionMeas_p.attach(msSource, NewMSSource::
			   columnName(NewMSSource::DIRECTION));
    timeMeas_p.attach(msSource, NewMSSource::columnName(NewMSSource::TIME));
    directionQuant_p.attach(msSource, NewMSSource::
			    columnName(NewMSSource::DIRECTION));
    intervalQuant_p.attach(msSource, NewMSSource::
			   columnName(NewMSSource::INTERVAL));
    properMotionQuant_p.attach(msSource, NewMSSource::
			       columnName(NewMSSource::PROPER_MOTION));
    timeQuant_p.attach(msSource, NewMSSource::columnName(NewMSSource::TIME));
    attachOptionalCols(msSource);
  }
}

void NewMSSourceColumns::attachOptionalCols(NewMSSource& msSource)
{
  const ColumnDescSet& cds = msSource.tableDesc().columnDescSet();
  const String& position = NewMSSource::columnName(NewMSSource::POSITION);
  if (cds.isDefined(position)) {
    position_p.attach(msSource, position);
    positionMeas_p.attach(msSource, position);
    positionQuant_p.attach(msSource, position);
  }
  const String& pulsarId = NewMSSource::columnName(NewMSSource::PULSAR_ID);
  if (cds.isDefined(pulsarId)) pulsarId_p.attach(msSource, pulsarId);
  const String& restFrequency =
    NewMSSource::columnName(NewMSSource::REST_FREQUENCY);
  if (cds.isDefined(restFrequency)) {
    restFrequency_p.attach(msSource, restFrequency);
    restFrequencyMeas_p.attach(msSource, restFrequency);
    restFrequencyQuant_p.attach(msSource, restFrequency);
  }
  const String& sourceModel =
    NewMSSource::columnName(NewMSSource::SOURCE_MODEL);
  if (cds.isDefined(sourceModel)) sourceModel_p.attach(msSource, sourceModel);
  const String& sysvel = NewMSSource::columnName(NewMSSource::SYSVEL);
  if (cds.isDefined(sysvel)) {
    sysvel_p.attach(msSource, sysvel);
    sysvelMeas_p.attach(msSource, sysvel);
    sysvelQuant_p.attach(msSource, sysvel);
  }
  const String& transition = NewMSSource::columnName(NewMSSource::TRANSITION);
  if (cds.isDefined(transition)) transition_p.attach(msSource, transition);
}

void NewMSSourceColumns::setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty) {
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

void NewMSSourceColumns::setDirectionRef(MDirection::Types ref) {
  directionMeas_p.setDescRefCode(ref);
}

void NewMSSourceColumns::setPositionRef(MPosition::Types ref)
{
  if (!positionMeas_p.isNull()) {
    positionMeas_p.setDescRefCode(ref);
  }
}

void NewMSSourceColumns::setFrequencyRef(MFrequency::Types ref)
{
  if (!restFrequencyMeas_p.isNull()) {
    restFrequencyMeas_p.setDescRefCode(ref);
  }
}

void NewMSSourceColumns::setRadialVelocityRef(MRadialVelocity::Types ref)
{
  if (!sysvelMeas_p.isNull()) {
    sysvelMeas_p.setDescRefCode(ref);
  }
}
// Local Variables: 
// compile-command: "gmake NewMSSourceColumns"
// End: 
