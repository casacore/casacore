//# NewMSPointingColumns.cc:  provides easy access to NewMeasurementSet columns
//# Copyright (C) 1999,2000
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

#include <aips/MeasurementSets/NewMSPointingColumns.h>
#include <aips/MeasurementSets/NewMSFieldColumns.h>
#include <aips/MeasurementSets/NewMSPointing.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/String.h>

RONewMSPointingColumns::
RONewMSPointingColumns(const NewMSPointing& msPointing):
  antennaId_p(msPointing, NewMSPointing::
	      columnName(NewMSPointing::ANTENNA_ID)),
  direction_p(msPointing, NewMSPointing::columnName(NewMSPointing::DIRECTION)),
  interval_p(msPointing, NewMSPointing::columnName(NewMSPointing::INTERVAL)),
  name_p(msPointing, NewMSPointing::columnName(NewMSPointing::NAME)),
  numPoly_p(msPointing, NewMSPointing::columnName(NewMSPointing::NUM_POLY)),
  target_p(msPointing, NewMSPointing::columnName(NewMSPointing::TARGET)),
  time_p(msPointing, NewMSPointing::columnName(NewMSPointing::TIME)),
  timeOrigin_p(msPointing, NewMSPointing::
	       columnName(NewMSPointing::TIME_ORIGIN)),
  tracking_p(msPointing, NewMSPointing::columnName(NewMSPointing::TRACKING)),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(msPointing, NewMSPointing::
		  columnName(NewMSPointing::DIRECTION)),
  targetMeas_p(msPointing, NewMSPointing::columnName(NewMSPointing::TARGET)),
  timeMeas_p(msPointing, NewMSPointing::columnName(NewMSPointing::TIME)),
  timeOriginMeas_p(msPointing, NewMSPointing::
		   columnName(NewMSPointing::TIME_ORIGIN)),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(msPointing, NewMSPointing::
		  columnName(NewMSPointing::INTERVAL)),
  timeQuant_p(msPointing, NewMSPointing::columnName(NewMSPointing::TIME)),
  timeOriginQuant_p(msPointing, NewMSPointing::
		    columnName(NewMSPointing::TIME_ORIGIN))
{ 
  attachOptionalCols(msPointing);
}

RONewMSPointingColumns::~RONewMSPointingColumns() {}

MDirection RONewMSPointingColumns::directionMeas(Int row, 
						 Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(directionMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection RONewMSPointingColumns::targetMeas(Int row, Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(targetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection RONewMSPointingColumns::pointingOffsetMeas(Int row, 
						      Double interTime) const
{
  if (pointingOffsetMeasCol().isNull()) return MDirection();
  return NewMSFieldColumns::interpolateDirMeas(pointingOffsetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection RONewMSPointingColumns::sourceOffsetMeas(Int row,
						    Double interTime) const
{
  if (sourceOffsetMeasCol().isNull()) return MDirection();
  return NewMSFieldColumns::interpolateDirMeas(sourceOffsetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}


Int RONewMSPointingColumns::pointingIndex(Int antenna, Double ptime) const
{
  // return the first row matching the requirements
  const Int nrow = antennaId().nrow();
  for (Int i=0; i<nrow; i++) {
    if (antennaId()(i)==antenna) {
      Double halfInt = interval()(i)/2;
      if (halfInt>0) {
 	if (time()(i) >= ptime - halfInt && time()(i) <= ptime + halfInt) {
 	  return i;
 	}
      } else {
 	// valid for all times (we should also handle interval<0 -> timestamps)
 	return i;
      }
    }
  }
  return -1;
}

RONewMSPointingColumns::RONewMSPointingColumns():
  antennaId_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numPoly_p(),
  target_p(),
  time_p(),
  timeOrigin_p(),
  tracking_p(),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(),
  targetMeas_p(),
  timeMeas_p(),
  timeOriginMeas_p(),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  timeOriginQuant_p()
{ 
}

void RONewMSPointingColumns::attach(const NewMSPointing& msPointing)
{
  antennaId_p.attach(msPointing, NewMSPointing::
	      columnName(NewMSPointing::ANTENNA_ID));
  direction_p.attach(msPointing, NewMSPointing::
		     columnName(NewMSPointing::DIRECTION));
  interval_p.attach(msPointing, NewMSPointing::
		    columnName(NewMSPointing::INTERVAL));
  name_p.attach(msPointing, NewMSPointing::columnName(NewMSPointing::NAME));
  numPoly_p.attach(msPointing, NewMSPointing::
		   columnName(NewMSPointing::NUM_POLY));
  target_p.attach(msPointing, NewMSPointing::
		  columnName(NewMSPointing::TARGET));
  time_p.attach(msPointing, NewMSPointing::columnName(NewMSPointing::TIME));
  timeOrigin_p.attach(msPointing, NewMSPointing::
	       columnName(NewMSPointing::TIME_ORIGIN));
  tracking_p.attach(msPointing, NewMSPointing::
		    columnName(NewMSPointing::TRACKING));
  directionMeas_p.attach(msPointing, NewMSPointing::
		  columnName(NewMSPointing::DIRECTION));
  targetMeas_p.attach(msPointing, NewMSPointing::
		      columnName(NewMSPointing::TARGET));
  timeMeas_p.attach(msPointing, NewMSPointing::
		    columnName(NewMSPointing::TIME));
  timeOriginMeas_p.attach(msPointing, NewMSPointing::
		   columnName(NewMSPointing::TIME_ORIGIN));
  intervalQuant_p.attach(msPointing, NewMSPointing::
		  columnName(NewMSPointing::INTERVAL));
  timeQuant_p.attach(msPointing, NewMSPointing::
		     columnName(NewMSPointing::TIME));
  timeOriginQuant_p.attach(msPointing, NewMSPointing::
			   columnName(NewMSPointing::TIME_ORIGIN));
  attachOptionalCols(msPointing);
}

void RONewMSPointingColumns::
attachOptionalCols(const NewMSPointing& msPointing)
{
  const ColumnDescSet& cds = msPointing.tableDesc().columnDescSet();
  const String& encoder = NewMSPointing::columnName(NewMSPointing::ENCODER);
  if (cds.isDefined(encoder)) {
    encoder_p.attach(msPointing, encoder);
    encoderMeas_p.attach(msPointing, encoder);
  }
  const String& onSource = NewMSPointing::columnName(NewMSPointing::ON_SOURCE);
  if (cds.isDefined(onSource)) onSource_p.attach(msPointing, onSource);
  const String& pointingModelId = 
    NewMSPointing::columnName(NewMSPointing::POINTING_MODEL_ID);
  if (cds.isDefined(pointingModelId)) {
    pointingModelId_p.attach(msPointing, pointingModelId);
  }
  const String& pointingOffset = NewMSPointing::
    columnName(NewMSPointing::POINTING_OFFSET);
  if (cds.isDefined(pointingOffset)) {
    pointingOffset_p.attach(msPointing, pointingOffset);
    pointingOffsetMeas_p.attach(msPointing, pointingOffset);
  }
  const String& sourceOffset = NewMSPointing::
    columnName(NewMSPointing::SOURCE_OFFSET);
  if (cds.isDefined(sourceOffset)) {
    sourceOffset_p.attach(msPointing, sourceOffset);
    sourceOffsetMeas_p.attach(msPointing, sourceOffset);
  }
  const String& overTheTop = 
    NewMSPointing::columnName(NewMSPointing::OVER_THE_TOP);
  if (cds.isDefined(overTheTop)) overTheTop_p.attach(msPointing, overTheTop);
}

NewMSPointingColumns::NewMSPointingColumns(NewMSPointing& msPointing):
  RONewMSPointingColumns(msPointing),
  antennaId_p(msPointing, NewMSPointing::
	      columnName(NewMSPointing::ANTENNA_ID)),
  direction_p(msPointing, NewMSPointing::columnName(NewMSPointing::DIRECTION)),
  interval_p(msPointing, NewMSPointing::columnName(NewMSPointing::INTERVAL)),
  name_p(msPointing, NewMSPointing::columnName(NewMSPointing::NAME)),
  numPoly_p(msPointing, NewMSPointing::columnName(NewMSPointing::NUM_POLY)),
  target_p(msPointing, NewMSPointing::columnName(NewMSPointing::TARGET)),
  time_p(msPointing, NewMSPointing::columnName(NewMSPointing::TIME)),
  timeOrigin_p(msPointing, NewMSPointing::
	       columnName(NewMSPointing::TIME_ORIGIN)),
  tracking_p(msPointing, NewMSPointing::columnName(NewMSPointing::TRACKING)),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(msPointing, NewMSPointing::
		  columnName(NewMSPointing::DIRECTION)),
  targetMeas_p(msPointing, NewMSPointing::columnName(NewMSPointing::TARGET)),
  timeMeas_p(msPointing, NewMSPointing::columnName(NewMSPointing::TIME)),
  timeOriginMeas_p(msPointing, NewMSPointing::
		   columnName(NewMSPointing::TIME_ORIGIN)),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(msPointing, NewMSPointing::
		  columnName(NewMSPointing::INTERVAL)),
  timeQuant_p(msPointing, NewMSPointing::columnName(NewMSPointing::TIME)),
  timeOriginQuant_p(msPointing, NewMSPointing::
		    columnName(NewMSPointing::TIME_ORIGIN))
{ 
  attachOptionalCols(msPointing);
}

NewMSPointingColumns::~NewMSPointingColumns() {}

NewMSPointingColumns::NewMSPointingColumns():
  RONewMSPointingColumns(),
  antennaId_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numPoly_p(),
  target_p(),
  time_p(),
  timeOrigin_p(),
  tracking_p(),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(),
  targetMeas_p(),
  timeMeas_p(),
  timeOriginMeas_p(),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  timeOriginQuant_p()
{ 
}

void NewMSPointingColumns::attach(NewMSPointing& msPointing)
{
  RONewMSPointingColumns::attach(msPointing);
  antennaId_p.attach(msPointing, NewMSPointing::
		     columnName(NewMSPointing::ANTENNA_ID));
  direction_p.attach(msPointing, NewMSPointing::
		     columnName(NewMSPointing::DIRECTION));
  interval_p.attach(msPointing, NewMSPointing::
		    columnName(NewMSPointing::INTERVAL));
  name_p.attach(msPointing, NewMSPointing::columnName(NewMSPointing::NAME));
  numPoly_p.attach(msPointing, NewMSPointing::
		   columnName(NewMSPointing::NUM_POLY));
  target_p.attach(msPointing, NewMSPointing::
		  columnName(NewMSPointing::TARGET));
  time_p.attach(msPointing, NewMSPointing::columnName(NewMSPointing::TIME));
  timeOrigin_p.attach(msPointing, NewMSPointing::
	       columnName(NewMSPointing::TIME_ORIGIN));
  tracking_p.attach(msPointing, NewMSPointing::
		    columnName(NewMSPointing::TRACKING));
  directionMeas_p.attach(msPointing, NewMSPointing::
		  columnName(NewMSPointing::DIRECTION));
  targetMeas_p.attach(msPointing, NewMSPointing::
		      columnName(NewMSPointing::TARGET));
  timeMeas_p.attach(msPointing, NewMSPointing::
		    columnName(NewMSPointing::TIME));
  timeOriginMeas_p.attach(msPointing, NewMSPointing::
		   columnName(NewMSPointing::TIME_ORIGIN));
  intervalQuant_p.attach(msPointing, NewMSPointing::
		  columnName(NewMSPointing::INTERVAL));
  timeQuant_p.attach(msPointing, NewMSPointing::
		     columnName(NewMSPointing::TIME));
  timeOriginQuant_p.attach(msPointing, NewMSPointing::
			   columnName(NewMSPointing::TIME_ORIGIN));
  attachOptionalCols(msPointing);
}

void NewMSPointingColumns::attachOptionalCols(NewMSPointing& msPointing)
{
  const ColumnDescSet& cds = msPointing.tableDesc().columnDescSet();
  const String& encoder = NewMSPointing::columnName(NewMSPointing::ENCODER);
  if (cds.isDefined(encoder)) {
    encoder_p.attach(msPointing, encoder);
    encoderMeas_p.attach(msPointing, encoder);
  }
  const String& onSource = NewMSPointing::columnName(NewMSPointing::ON_SOURCE);
  if (cds.isDefined(onSource)) onSource_p.attach(msPointing, onSource);
  const String& pointingModelId = 
    NewMSPointing::columnName(NewMSPointing::POINTING_MODEL_ID);
  if (cds.isDefined(pointingModelId)) {
    pointingModelId_p.attach(msPointing, pointingModelId);
  }
  const String& pointingOffset = NewMSPointing::
    columnName(NewMSPointing::POINTING_OFFSET);
  if (cds.isDefined(pointingOffset)) {
    pointingOffset_p.attach(msPointing, pointingOffset);
    pointingOffsetMeas_p.attach(msPointing, pointingOffset);
  }
  const String& sourceOffset = NewMSPointing::
    columnName(NewMSPointing::SOURCE_OFFSET);
  if (cds.isDefined(sourceOffset)) {
    sourceOffset_p.attach(msPointing, sourceOffset);
    sourceOffsetMeas_p.attach(msPointing, sourceOffset);
  }
  const String& overTheTop = 
    NewMSPointing::columnName(NewMSPointing::OVER_THE_TOP);
  if (cds.isDefined(overTheTop)) overTheTop_p.attach(msPointing, overTheTop);
}

void NewMSPointingColumns::setEpochRef(MEpoch::Types ref) {
  timeMeas_p.setDescRefCode(ref);
  timeOriginMeas_p.setDescRefCode(ref);
}

void NewMSPointingColumns::setDirectionRef(MDirection::Types ref) {
  directionMeas_p.setDescRefCode(ref);
  targetMeas_p.setDescRefCode(ref);
  if (!pointingOffsetMeas_p.isNull()) {
    pointingOffsetMeas_p.setDescRefCode(ref);
  }
  if (!sourceOffsetMeas_p.isNull()) {
    sourceOffsetMeas_p.setDescRefCode(ref);
  }
}

void NewMSPointingColumns::setEncoderDirectionRef(MDirection::Types ref) 
{
  if (!encoderMeas_p.isNull()) {
    encoderMeas_p.setDescRefCode(ref);
  }
}
// Local Variables: 
// compile-command: "gmake NewMSPointingColumns"
// End: 
