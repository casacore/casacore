//# MSFeedColumns.cc:  provides easy access to MeasurementSet columns
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

#include <ms/MeasurementSets/MSFeedColumns.h>
#include <ms/MeasurementSets/MSFeed.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ColDescSet.h>

namespace casa { //# NAMESPACE CASA - BEGIN

ROMSFeedColumns::ROMSFeedColumns(const MSFeed& msFeed):
  antennaId_p(msFeed, MSFeed::columnName(MSFeed::ANTENNA_ID)),
  beamId_p(msFeed, MSFeed::columnName(MSFeed::BEAM_ID)),
  beamOffset_p(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET)),
  feedId_p(msFeed, MSFeed::columnName(MSFeed::FEED_ID)),
  interval_p(msFeed, MSFeed::columnName(MSFeed::INTERVAL)),
  numReceptors_p(msFeed, MSFeed::columnName(MSFeed::NUM_RECEPTORS)),
  polResponse_p(msFeed, MSFeed::columnName(MSFeed::POL_RESPONSE)),
  polarizationType_p(msFeed, MSFeed::
		     columnName(MSFeed::POLARIZATION_TYPE)),
  position_p(msFeed, MSFeed::columnName(MSFeed::POSITION)),
  receptorAngle_p(msFeed, MSFeed::columnName(MSFeed::RECEPTOR_ANGLE)),
  spectralWindowId_p(msFeed, MSFeed::
		     columnName(MSFeed::SPECTRAL_WINDOW_ID)),
  time_p(msFeed, MSFeed::columnName(MSFeed::TIME)),
  focusLength_p(), 
  phasedFeedId_p(),
  beamOffsetMeas_p(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET)),
  positionMeas_p(msFeed, MSFeed::columnName(MSFeed::POSITION)),
  timeMeas_p(msFeed, MSFeed::columnName(MSFeed::TIME)),
  beamOffsetQuant_p(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET)),
  intervalQuant_p(msFeed, MSFeed::columnName(MSFeed::INTERVAL)),
  positionQuant_p(msFeed, MSFeed::columnName(MSFeed::POSITION)),
  receptorAngleQuant_p(msFeed, MSFeed::
		       columnName(MSFeed::RECEPTOR_ANGLE)),
  timeQuant_p(msFeed, MSFeed::columnName(MSFeed::TIME)),
  focusLengthQuant_p()
{
  attachOptionalCols(msFeed);
}

ROMSFeedColumns::~ROMSFeedColumns() {}

ROMSFeedColumns::ROMSFeedColumns():
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

void ROMSFeedColumns::attach(const MSFeed& msFeed)
{
  antennaId_p.attach(msFeed, MSFeed::columnName(MSFeed::ANTENNA_ID));
  beamId_p.attach(msFeed, MSFeed::columnName(MSFeed::BEAM_ID));
  beamOffset_p.attach(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET));
  feedId_p.attach(msFeed, MSFeed::columnName(MSFeed::FEED_ID));
  interval_p.attach(msFeed, MSFeed::columnName(MSFeed::INTERVAL));
  numReceptors_p.attach(msFeed,
			MSFeed::columnName(MSFeed::NUM_RECEPTORS));
  polResponse_p.attach(msFeed, MSFeed::columnName(MSFeed::POL_RESPONSE));
  polarizationType_p.attach(msFeed, MSFeed::
			    columnName(MSFeed::POLARIZATION_TYPE));
  position_p.attach(msFeed, MSFeed::columnName(MSFeed::POSITION));
  receptorAngle_p.attach(msFeed,
			 MSFeed::columnName(MSFeed::RECEPTOR_ANGLE));
  spectralWindowId_p.attach(msFeed, MSFeed::
			    columnName(MSFeed::SPECTRAL_WINDOW_ID));
  time_p.attach(msFeed, MSFeed::columnName(MSFeed::TIME));
  beamOffsetMeas_p.attach(msFeed,
			  MSFeed::columnName(MSFeed::BEAM_OFFSET));
  positionMeas_p.attach(msFeed, MSFeed::columnName(MSFeed::POSITION));
  timeMeas_p.attach(msFeed, MSFeed::columnName(MSFeed::TIME));
  beamOffsetQuant_p.attach(msFeed,
			   MSFeed::columnName(MSFeed::BEAM_OFFSET));
  intervalQuant_p.attach(msFeed, MSFeed::columnName(MSFeed::INTERVAL));
  positionQuant_p.attach(msFeed, MSFeed::columnName(MSFeed::POSITION));
  receptorAngleQuant_p.attach(msFeed, MSFeed::
			      columnName(MSFeed::RECEPTOR_ANGLE));
  timeQuant_p.attach(msFeed, MSFeed::columnName(MSFeed::TIME));
  attachOptionalCols(msFeed);
}

void ROMSFeedColumns::attachOptionalCols(const MSFeed& msFeed)
{
  const ColumnDescSet& cds=msFeed.tableDesc().columnDescSet();
  const String& focusLength=MSFeed::columnName(MSFeed::FOCUS_LENGTH);
  if (cds.isDefined(focusLength)) {
    focusLength_p.attach(msFeed, focusLength);
    focusLengthQuant_p.attach(msFeed, focusLength);
  }
  const String& phasedFeedId=MSFeed::columnName(MSFeed::PHASED_FEED_ID);
  if (cds.isDefined(phasedFeedId)) phasedFeedId_p.attach(msFeed, phasedFeedId);
}

MSFeedColumns::MSFeedColumns(MSFeed& msFeed):
  ROMSFeedColumns(msFeed),
  antennaId_p(msFeed, MSFeed::columnName(MSFeed::ANTENNA_ID)),
  beamId_p(msFeed, MSFeed::columnName(MSFeed::BEAM_ID)),
  beamOffset_p(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET)),
  feedId_p(msFeed, MSFeed::columnName(MSFeed::FEED_ID)),
  interval_p(msFeed, MSFeed::columnName(MSFeed::INTERVAL)),
  numReceptors_p(msFeed, MSFeed::columnName(MSFeed::NUM_RECEPTORS)),
  polResponse_p(msFeed, MSFeed::columnName(MSFeed::POL_RESPONSE)),
  polarizationType_p(msFeed, MSFeed::
		     columnName(MSFeed::POLARIZATION_TYPE)),
  position_p(msFeed, MSFeed::columnName(MSFeed::POSITION)),
  receptorAngle_p(msFeed, MSFeed::columnName(MSFeed::RECEPTOR_ANGLE)),
  spectralWindowId_p(msFeed, MSFeed::
		     columnName(MSFeed::SPECTRAL_WINDOW_ID)),
  time_p(msFeed, MSFeed::columnName(MSFeed::TIME)),
  focusLength_p(), 
  phasedFeedId_p(),
  beamOffsetMeas_p(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET)),
  positionMeas_p(msFeed, MSFeed::columnName(MSFeed::POSITION)),
  timeMeas_p(msFeed, MSFeed::columnName(MSFeed::TIME)),
  beamOffsetQuant_p(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET)),
  intervalQuant_p(msFeed, MSFeed::columnName(MSFeed::INTERVAL)),
  positionQuant_p(msFeed, MSFeed::columnName(MSFeed::POSITION)),
  receptorAngleQuant_p(msFeed, MSFeed::
		       columnName(MSFeed::RECEPTOR_ANGLE)),
  timeQuant_p(msFeed, MSFeed::columnName(MSFeed::TIME)),
  focusLengthQuant_p()
{
  attachOptionalCols(msFeed);
}

MSFeedColumns::~MSFeedColumns() {}

void MSFeedColumns::setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty) {
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

void MSFeedColumns::setDirectionRef(MDirection::Types ref) 
{
  beamOffsetMeas_p.setDescRefCode(ref);
}

void MSFeedColumns::setPositionRef(MPosition::Types ref) 
{
  positionMeas_p.setDescRefCode(ref);
}

MSFeedColumns::MSFeedColumns():
  ROMSFeedColumns(),
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

void MSFeedColumns::attach(MSFeed& msFeed)
{
  ROMSFeedColumns::attach(msFeed);
  antennaId_p.attach(msFeed, MSFeed::columnName(MSFeed::ANTENNA_ID));
  beamId_p.attach(msFeed, MSFeed::columnName(MSFeed::BEAM_ID));
  beamOffset_p.attach(msFeed, MSFeed::columnName(MSFeed::BEAM_OFFSET));
  feedId_p.attach(msFeed, MSFeed::columnName(MSFeed::FEED_ID));
  interval_p.attach(msFeed, MSFeed::columnName(MSFeed::INTERVAL));
  numReceptors_p.attach(msFeed,
			MSFeed::columnName(MSFeed::NUM_RECEPTORS));
  polResponse_p.attach(msFeed, MSFeed::columnName(MSFeed::POL_RESPONSE));
  polarizationType_p.attach(msFeed, MSFeed::
			    columnName(MSFeed::POLARIZATION_TYPE));
  position_p.attach(msFeed, MSFeed::columnName(MSFeed::POSITION));
  receptorAngle_p.attach(msFeed,
			 MSFeed::columnName(MSFeed::RECEPTOR_ANGLE));
  spectralWindowId_p.attach(msFeed, MSFeed::
			    columnName(MSFeed::SPECTRAL_WINDOW_ID));
  time_p.attach(msFeed, MSFeed::columnName(MSFeed::TIME));
  beamOffsetMeas_p.attach(msFeed,
			  MSFeed::columnName(MSFeed::BEAM_OFFSET));
  positionMeas_p.attach(msFeed, MSFeed::columnName(MSFeed::POSITION));
  timeMeas_p.attach(msFeed, MSFeed::columnName(MSFeed::TIME));
  beamOffsetQuant_p.attach(msFeed,
			   MSFeed::columnName(MSFeed::BEAM_OFFSET));
  intervalQuant_p.attach(msFeed, MSFeed::columnName(MSFeed::INTERVAL));
  positionQuant_p.attach(msFeed, MSFeed::columnName(MSFeed::POSITION));
  receptorAngleQuant_p.attach(msFeed, MSFeed::
			      columnName(MSFeed::RECEPTOR_ANGLE));
  timeQuant_p.attach(msFeed, MSFeed::columnName(MSFeed::TIME));
  attachOptionalCols(msFeed);
}

void MSFeedColumns::attachOptionalCols(MSFeed& msFeed)
{
  const ColumnDescSet& cds = msFeed.tableDesc().columnDescSet();
  const String& focusLength = MSFeed::columnName(MSFeed::FOCUS_LENGTH);
  if (cds.isDefined(focusLength)) {
    focusLength_p.attach(msFeed, focusLength);
    focusLengthQuant_p.attach(msFeed, focusLength);
  }
  const String& phasedFeedId =MSFeed::columnName(MSFeed::PHASED_FEED_ID);
  if (cds.isDefined(phasedFeedId)) phasedFeedId_p.attach(msFeed, phasedFeedId);
}

// Local Variables: 
// compile-command: "gmake MSFeedColumns"
// End: 

} //# NAMESPACE CASA - END

