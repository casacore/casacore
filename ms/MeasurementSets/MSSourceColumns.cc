//# MSSourceColumns.cc:  provides easy access to MeasurementSet columns
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

#include <casacore/ms/MeasurementSets/MSSourceColumns.h>
#include <casacore/ms/MeasurementSets/MSSource.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MRadialVelocity.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSSourceColumns::MSSourceColumns():
  isNull_p(true)
{
}

MSSourceColumns::MSSourceColumns(const MSSource& msSource):
  isNull_p(true)
{ 
  attach(msSource);
}

MSSourceColumns::~MSSourceColumns() {}

void MSSourceColumns::attach(const MSSource& msSource)
{
  isNull_p = msSource.isNull();
  if (!isNull()) {
    calibrationGroup_p.attach(msSource, MSSource::
			      columnName(MSSource::CALIBRATION_GROUP));
    code_p.attach(msSource, MSSource::columnName(MSSource::CODE));
    direction_p.attach(msSource, MSSource::
		       columnName(MSSource::DIRECTION));
    interval_p.attach(msSource, MSSource::
		      columnName(MSSource::INTERVAL));
    name_p.attach(msSource, MSSource::columnName(MSSource::NAME));
    numLines_p.attach(msSource, MSSource::
		      columnName(MSSource::NUM_LINES));
    properMotion_p.attach(msSource, MSSource::
			  columnName(MSSource::PROPER_MOTION));
    sourceId_p.attach(msSource, MSSource::
		      columnName(MSSource::SOURCE_ID));
    spectralWindowId_p.attach(msSource, MSSource::
			      columnName(MSSource::SPECTRAL_WINDOW_ID));
    time_p.attach(msSource, MSSource::columnName(MSSource::TIME));
    directionMeas_p.attach(msSource, MSSource::
			   columnName(MSSource::DIRECTION));
    timeMeas_p.attach(msSource, MSSource::columnName(MSSource::TIME));
    directionQuant_p.attach(msSource, MSSource::
			    columnName(MSSource::DIRECTION));
    intervalQuant_p.attach(msSource, MSSource::
			   columnName(MSSource::INTERVAL));
    properMotionQuant_p.attach(msSource, MSSource::
			       columnName(MSSource::PROPER_MOTION));
    timeQuant_p.attach(msSource, MSSource::columnName(MSSource::TIME));
    attachOptionalCols(msSource);
  }
}

void MSSourceColumns::attachOptionalCols(const MSSource& msSource)
{
  const ColumnDescSet& cds = msSource.tableDesc().columnDescSet();
  const String& position = MSSource::columnName(MSSource::POSITION);
  if (cds.isDefined(position)) {
    position_p.attach(msSource, position);
    positionMeas_p.attach(msSource, position);
    positionQuant_p.attach(msSource, position);
  }
  const String& pulsarId = MSSource::columnName(MSSource::PULSAR_ID);
  if (cds.isDefined(pulsarId)) pulsarId_p.attach(msSource, pulsarId);
  const String& restFrequency =
    MSSource::columnName(MSSource::REST_FREQUENCY);
  if (cds.isDefined(restFrequency)) {
    restFrequency_p.attach(msSource, restFrequency);
    restFrequencyMeas_p.attach(msSource, restFrequency);
    restFrequencyQuant_p.attach(msSource, restFrequency);
  }
  const String& sourceModel =
    MSSource::columnName(MSSource::SOURCE_MODEL);
  if (cds.isDefined(sourceModel)) sourceModel_p.attach(msSource, sourceModel);
  const String& sysvel = MSSource::columnName(MSSource::SYSVEL);
  if (cds.isDefined(sysvel)) {
    sysvel_p.attach(msSource, sysvel);
    sysvelMeas_p.attach(msSource, sysvel);
    sysvelQuant_p.attach(msSource, sysvel);
  }
  const String& transition = MSSource::columnName(MSSource::TRANSITION);
  if (cds.isDefined(transition)) transition_p.attach(msSource, transition);
}

void MSSourceColumns::setEpochRef(MEpoch::Types ref, bool tableMustBeEmpty) {
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

void MSSourceColumns::setDirectionRef(MDirection::Types ref) {
  directionMeas_p.setDescRefCode(ref);
}

void MSSourceColumns::setPositionRef(MPosition::Types ref)
{
  if (!positionMeas_p.isNull()) {
    positionMeas_p.setDescRefCode(ref);
  }
}

void MSSourceColumns::setFrequencyRef(MFrequency::Types ref)
{
  if (!restFrequencyMeas_p.isNull()) {
    restFrequencyMeas_p.setDescRefCode(ref);
  }
}

void MSSourceColumns::setRadialVelocityRef(MRadialVelocity::Types ref)
{
  if (!sysvelMeas_p.isNull()) {
    sysvelMeas_p.setDescRefCode(ref);
  }
}

} //# NAMESPACE CASACORE - END

