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

//# Includes
#include <aips/MeasurementSets/NewMSSourceColumns.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MRadialVelocity.h>

NewMSSourceColumns::NewMSSourceColumns(NewMSSource& msSource):
calibrationGroup_p(msSource,NewMSSource::columnName(NewMSSource::CALIBRATION_GROUP)),
code_p(msSource,NewMSSource::columnName(NewMSSource::CODE)),
direction_p(msSource,NewMSSource::columnName(NewMSSource::DIRECTION)),
interval_p(msSource,NewMSSource::columnName(NewMSSource::INTERVAL)),
name_p(msSource,NewMSSource::columnName(NewMSSource::NAME)),
numLines_p(msSource,NewMSSource::columnName(NewMSSource::NUM_LINES)),
position_p(msSource,NewMSSource::columnName(NewMSSource::POSITION)),
properMotion_p(msSource,NewMSSource::columnName(NewMSSource::PROPER_MOTION)),
sourceId_p(msSource,NewMSSource::columnName(NewMSSource::SOURCE_ID)),
spectralWindowId_p(msSource,NewMSSource::columnName(NewMSSource::SPECTRAL_WINDOW_ID)),
time_p(msSource,NewMSSource::columnName(NewMSSource::TIME)),
directionMeas_p(msSource,NewMSSource::columnName(NewMSSource::DIRECTION)),
positionMeas_p(msSource,NewMSSource::columnName(NewMSSource::POSITION)),
timeMeas_p(msSource,NewMSSource::columnName(NewMSSource::TIME)),
directionQuant_p(msSource,NewMSSource::columnName(NewMSSource::DIRECTION)),
intervalQuant_p(msSource,NewMSSource::columnName(NewMSSource::INTERVAL)),
positionQuant_p(msSource,NewMSSource::columnName(NewMSSource::POSITION)),
properMotionQuant_p(msSource,NewMSSource::columnName(NewMSSource::PROPER_MOTION)),
timeQuant_p(msSource,NewMSSource::columnName(NewMSSource::TIME))
{ 
  const ColumnDescSet& cds=msSource.tableDesc().columnDescSet();
  const String& pulsarId=NewMSSource::columnName(NewMSSource::PULSAR_ID);
  if (cds.isDefined(pulsarId)) pulsarId_p.attach(msSource,pulsarId);
  const String& restFrequency=NewMSSource::columnName(NewMSSource::REST_FREQUENCY);
  if (cds.isDefined(restFrequency)) {
    restFrequency_p.attach(msSource,restFrequency);
    restFrequencyMeas_p.attach(msSource,restFrequency);
    restFrequencyQuant_p.attach(msSource,restFrequency);
  }
  const String& sourceModel=NewMSSource::columnName(NewMSSource::SOURCE_MODEL);
  if (cds.isDefined(sourceModel)) sourceModel_p.attach(msSource,sourceModel);
  const String& sysvel=NewMSSource::columnName(NewMSSource::SYSVEL);
  if (cds.isDefined(sysvel)) {
    sysvel_p.attach(msSource,sysvel);
    sysvelMeas_p.attach(msSource,sysvel);
    sysvelQuant_p.attach(msSource,sysvel);
  }
  const String& transition=NewMSSource::columnName(NewMSSource::TRANSITION);
  if (cds.isDefined(transition)) transition_p.attach(msSource,transition);
}

NewMSSourceColumns::~NewMSSourceColumns() {}

void NewMSSourceColumns::setPositionRef(Int ref)
{
  position_p.rwKeywordSet().rwSubRecord("MEASINFO").
    define("Ref",MPosition::showType(ref));
}

void NewMSSourceColumns::setFrequencyRef(Int ref)
{
  if (!restFrequency_p.isNull()) 
    restFrequency_p.rwKeywordSet().rwSubRecord("MEASINFO").
    define("Ref",MFrequency::showType(ref));
}

void NewMSSourceColumns::setRadialVelocityRef(Int ref)
{
  if (!sysvel_p.isNull()) sysvel_p.rwKeywordSet().rwSubRecord("MEASINFO").
    define("Ref",MRadialVelocity::showType(ref));
}


RONewMSSourceColumns::RONewMSSourceColumns(const NewMSSource& msSource):
calibrationGroup_p(msSource,NewMSSource::columnName(NewMSSource::CALIBRATION_GROUP)),
code_p(msSource,NewMSSource::columnName(NewMSSource::CODE)),
direction_p(msSource,NewMSSource::columnName(NewMSSource::DIRECTION)),
interval_p(msSource,NewMSSource::columnName(NewMSSource::INTERVAL)),
name_p(msSource,NewMSSource::columnName(NewMSSource::NAME)),
numLines_p(msSource,NewMSSource::columnName(NewMSSource::NUM_LINES)),
position_p(msSource,NewMSSource::columnName(NewMSSource::POSITION)),
properMotion_p(msSource,NewMSSource::columnName(NewMSSource::PROPER_MOTION)),
sourceId_p(msSource,NewMSSource::columnName(NewMSSource::SOURCE_ID)),
spectralWindowId_p(msSource,NewMSSource::columnName(NewMSSource::SPECTRAL_WINDOW_ID)),
time_p(msSource,NewMSSource::columnName(NewMSSource::TIME)),
directionMeas_p(msSource,NewMSSource::columnName(NewMSSource::DIRECTION)),
positionMeas_p(msSource,NewMSSource::columnName(NewMSSource::POSITION)),
timeMeas_p(msSource,NewMSSource::columnName(NewMSSource::TIME)),
directionQuant_p(msSource,NewMSSource::columnName(NewMSSource::DIRECTION)),
intervalQuant_p(msSource,NewMSSource::columnName(NewMSSource::INTERVAL)),
positionQuant_p(msSource,NewMSSource::columnName(NewMSSource::POSITION)),
properMotionQuant_p(msSource,NewMSSource::columnName(NewMSSource::PROPER_MOTION)),
timeQuant_p(msSource,NewMSSource::columnName(NewMSSource::TIME))
{ 
  const ColumnDescSet& cds=msSource.tableDesc().columnDescSet();
  const String& pulsarId=NewMSSource::columnName(NewMSSource::PULSAR_ID);
  if (cds.isDefined(pulsarId)) pulsarId_p.attach(msSource,pulsarId);
  const String& restFrequency=NewMSSource::columnName(NewMSSource::REST_FREQUENCY);
  if (cds.isDefined(restFrequency)) {
    restFrequency_p.attach(msSource,restFrequency);
    restFrequencyMeas_p.attach(msSource,restFrequency);
    restFrequencyQuant_p.attach(msSource,restFrequency);
  }
  const String& sourceModel=NewMSSource::columnName(NewMSSource::SOURCE_MODEL);
  if (cds.isDefined(sourceModel)) sourceModel_p.attach(msSource,sourceModel);
  const String& sysvel=NewMSSource::columnName(NewMSSource::SYSVEL);
  if (cds.isDefined(sysvel)) {
    sysvel_p.attach(msSource,sysvel);
    sysvelMeas_p.attach(msSource,sysvel);
    sysvelQuant_p.attach(msSource,sysvel);
  }
  const String& transition=NewMSSource::columnName(NewMSSource::TRANSITION);
  if (cds.isDefined(transition)) transition_p.attach(msSource,transition);
}

RONewMSSourceColumns::~RONewMSSourceColumns() {}

