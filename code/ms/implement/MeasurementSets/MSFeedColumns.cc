//# NewMSFeedColumns.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSFeedColumns.h>
#include <aips/MeasurementSets/NewMSFeed.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Utilities/String.h>

RONewMSFeedColumns::RONewMSFeedColumns(const NewMSFeed& msFeed):
  antennaId_p(msFeed, NewMSFeed::columnName(NewMSFeed::ANTENNA_ID)),
  beamId_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_ID)),
  beamOffset_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET)),
  feedId_p(msFeed, NewMSFeed::columnName(NewMSFeed::FEED_ID)),
  interval_p(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL)),
  numReceptors_p(msFeed, NewMSFeed::columnName(NewMSFeed::NUM_RECEPTORS)),
  polResponse_p(msFeed, NewMSFeed::columnName(NewMSFeed::POL_RESPONSE)),
  polarizationType_p(msFeed, NewMSFeed::
		     columnName(NewMSFeed::POLARIZATION_TYPE)),
  position_p(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION)),
  receptorAngle_p(msFeed, NewMSFeed::columnName(NewMSFeed::RECEPTOR_ANGLE)),
  spectralWindowId_p(msFeed, NewMSFeed::
		     columnName(NewMSFeed::SPECTRAL_WINDOW_ID)),
  time_p(msFeed, NewMSFeed::columnName(NewMSFeed::TIME)),
  focusLength_p(), 
  phasedFeedId_p(),
  beamOffsetMeas_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET)),
  positionMeas_p(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION)),
  timeMeas_p(msFeed, NewMSFeed::columnName(NewMSFeed::TIME)),
  beamOffsetQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET)),
  intervalQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL)),
  positionQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION)),
  receptorAngleQuant_p(msFeed, NewMSFeed::
		       columnName(NewMSFeed::RECEPTOR_ANGLE)),
  timeQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::TIME)),
  focusLengthQuant_p()
{
  attachOptionalCols(msFeed);
}

RONewMSFeedColumns::~RONewMSFeedColumns() {}

RONewMSFeedColumns::RONewMSFeedColumns():
  antennaId_p(),
  beamId_p(),
  beamOffset_p(),
  feedId_p(),
  interval_p(),
  numReceptors_p(),
  polResponse_p(),
  polarizationType_p(),
  position_p(),
  receptorAngle_p(),
  spectralWindowId_p(),
  time_p(),
  focusLength_p(), 
  phasedFeedId_p(),
  beamOffsetMeas_p(),
  positionMeas_p(),
  timeMeas_p(),
  beamOffsetQuant_p(),
  intervalQuant_p(),
  positionQuant_p(),
  receptorAngleQuant_p(),
  timeQuant_p(),
  focusLengthQuant_p()
{
}

void RONewMSFeedColumns::attach(const NewMSFeed& msFeed)
{
  antennaId_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::ANTENNA_ID));
  beamId_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_ID));
  beamOffset_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET));
  feedId_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::FEED_ID));
  interval_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL));
  numReceptors_p.attach(msFeed,
			NewMSFeed::columnName(NewMSFeed::NUM_RECEPTORS));
  polResponse_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POL_RESPONSE));
  polarizationType_p.attach(msFeed, NewMSFeed::
			    columnName(NewMSFeed::POLARIZATION_TYPE));
  position_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION));
  receptorAngle_p.attach(msFeed,
			 NewMSFeed::columnName(NewMSFeed::RECEPTOR_ANGLE));
  spectralWindowId_p.attach(msFeed, NewMSFeed::
			    columnName(NewMSFeed::SPECTRAL_WINDOW_ID));
  time_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::TIME));
  beamOffsetMeas_p.attach(msFeed,
			  NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET));
  positionMeas_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION));
  timeMeas_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::TIME));
  beamOffsetQuant_p.attach(msFeed,
			   NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET));
  intervalQuant_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL));
  positionQuant_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION));
  receptorAngleQuant_p.attach(msFeed, NewMSFeed::
			      columnName(NewMSFeed::RECEPTOR_ANGLE));
  timeQuant_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::TIME));
  attachOptionalCols(msFeed);
}

void RONewMSFeedColumns::attachOptionalCols(const NewMSFeed& msFeed)
{
  const ColumnDescSet& cds=msFeed.tableDesc().columnDescSet();
  const String& focusLength=NewMSFeed::columnName(NewMSFeed::FOCUS_LENGTH);
  if (cds.isDefined(focusLength)) {
    focusLength_p.attach(msFeed, focusLength);
    focusLengthQuant_p.attach(msFeed, focusLength);
  }
  const String& phasedFeedId=NewMSFeed::columnName(NewMSFeed::PHASED_FEED_ID);
  if (cds.isDefined(phasedFeedId)) phasedFeedId_p.attach(msFeed, phasedFeedId);
}

NewMSFeedColumns::NewMSFeedColumns(NewMSFeed& msFeed):
  RONewMSFeedColumns(msFeed),
  antennaId_p(msFeed, NewMSFeed::columnName(NewMSFeed::ANTENNA_ID)),
  beamId_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_ID)),
  beamOffset_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET)),
  feedId_p(msFeed, NewMSFeed::columnName(NewMSFeed::FEED_ID)),
  interval_p(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL)),
  numReceptors_p(msFeed, NewMSFeed::columnName(NewMSFeed::NUM_RECEPTORS)),
  polResponse_p(msFeed, NewMSFeed::columnName(NewMSFeed::POL_RESPONSE)),
  polarizationType_p(msFeed, NewMSFeed::
		     columnName(NewMSFeed::POLARIZATION_TYPE)),
  position_p(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION)),
  receptorAngle_p(msFeed, NewMSFeed::columnName(NewMSFeed::RECEPTOR_ANGLE)),
  spectralWindowId_p(msFeed, NewMSFeed::
		     columnName(NewMSFeed::SPECTRAL_WINDOW_ID)),
  time_p(msFeed, NewMSFeed::columnName(NewMSFeed::TIME)),
  focusLength_p(), 
  phasedFeedId_p(),
  beamOffsetMeas_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET)),
  positionMeas_p(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION)),
  timeMeas_p(msFeed, NewMSFeed::columnName(NewMSFeed::TIME)),
  beamOffsetQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET)),
  intervalQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL)),
  positionQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION)),
  receptorAngleQuant_p(msFeed, NewMSFeed::
		       columnName(NewMSFeed::RECEPTOR_ANGLE)),
  timeQuant_p(msFeed, NewMSFeed::columnName(NewMSFeed::TIME)),
  focusLengthQuant_p()
{
  attachOptionalCols(msFeed);
}

NewMSFeedColumns::~NewMSFeedColumns() {}

void NewMSFeedColumns::setDirectionRef(Int ref) 
{
  beamOffset_p.rwKeywordSet().rwSubRecord("MEASINFO").
    define("Ref", MDirection::showType(ref));
}

void NewMSFeedColumns::setPositionRef(Int ref) 
{
  position_p.rwKeywordSet().rwSubRecord("MEASINFO").
    define("Ref", MPosition::showType(ref));
}

NewMSFeedColumns::NewMSFeedColumns():
  RONewMSFeedColumns(),
  antennaId_p(),
  beamId_p(),
  beamOffset_p(),
  feedId_p(),
  interval_p(),
  numReceptors_p(),
  polResponse_p(),
  polarizationType_p(),
  position_p(),
  receptorAngle_p(),
  spectralWindowId_p(),
  time_p(),
  focusLength_p(), 
  phasedFeedId_p(),
  beamOffsetMeas_p(),
  positionMeas_p(),
  timeMeas_p(),
  beamOffsetQuant_p(),
  intervalQuant_p(),
  positionQuant_p(),
  receptorAngleQuant_p(),
  timeQuant_p(),
  focusLengthQuant_p()
{
}

void NewMSFeedColumns::attach(NewMSFeed& msFeed)
{
  RONewMSFeedColumns::attach(msFeed);
  antennaId_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::ANTENNA_ID));
  beamId_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_ID));
  beamOffset_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET));
  feedId_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::FEED_ID));
  interval_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL));
  numReceptors_p.attach(msFeed,
			NewMSFeed::columnName(NewMSFeed::NUM_RECEPTORS));
  polResponse_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POL_RESPONSE));
  polarizationType_p.attach(msFeed, NewMSFeed::
			    columnName(NewMSFeed::POLARIZATION_TYPE));
  position_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION));
  receptorAngle_p.attach(msFeed,
			 NewMSFeed::columnName(NewMSFeed::RECEPTOR_ANGLE));
  spectralWindowId_p.attach(msFeed, NewMSFeed::
			    columnName(NewMSFeed::SPECTRAL_WINDOW_ID));
  time_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::TIME));
  beamOffsetMeas_p.attach(msFeed,
			  NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET));
  positionMeas_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION));
  timeMeas_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::TIME));
  beamOffsetQuant_p.attach(msFeed,
			   NewMSFeed::columnName(NewMSFeed::BEAM_OFFSET));
  intervalQuant_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::INTERVAL));
  positionQuant_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::POSITION));
  receptorAngleQuant_p.attach(msFeed, NewMSFeed::
			      columnName(NewMSFeed::RECEPTOR_ANGLE));
  timeQuant_p.attach(msFeed, NewMSFeed::columnName(NewMSFeed::TIME));
  attachOptionalCols(msFeed);
}

void NewMSFeedColumns::attachOptionalCols(NewMSFeed& msFeed)
{
  const ColumnDescSet& cds = msFeed.tableDesc().columnDescSet();
  const String& focusLength = NewMSFeed::columnName(NewMSFeed::FOCUS_LENGTH);
  if (cds.isDefined(focusLength)) {
    focusLength_p.attach(msFeed, focusLength);
    focusLengthQuant_p.attach(msFeed, focusLength);
  }
  const String& phasedFeedId =NewMSFeed::columnName(NewMSFeed::PHASED_FEED_ID);
  if (cds.isDefined(phasedFeedId)) phasedFeedId_p.attach(msFeed, phasedFeedId);
}

