//# NewMSFreqOffsetColumns.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSFreqOffColumns.h>
#include <aips/MeasurementSets/NewMSFreqOffset.h>

RONewMSFreqOffsetColumns::
RONewMSFreqOffsetColumns(const NewMSFreqOffset& msFreqOffset):
  isNull_p(True),
  antenna1_p(),
  antenna2_p(),
  feedId_p(),
  interval_p(),
  offset_p(),
  spectralWindowId_p(),
  time_p(),
  timeMeas_p(),
  intervalQuant_p(),
  offsetQuant_p(),
  timeQuant_p()
{
  attach(msFreqOffset);
}

RONewMSFreqOffsetColumns::~RONewMSFreqOffsetColumns() {}

RONewMSFreqOffsetColumns::RONewMSFreqOffsetColumns():
  isNull_p(True),
  antenna1_p(),
  antenna2_p(),
  feedId_p(),
  interval_p(),
  offset_p(),
  spectralWindowId_p(),
  time_p(),
  timeMeas_p(),
  intervalQuant_p(),
  offsetQuant_p(),
  timeQuant_p()
{
}

void RONewMSFreqOffsetColumns::attach(const NewMSFreqOffset& msFreqOffset)
{
  isNull_p = msFreqOffset.isNull();
  if (!isNull()) {
    antenna1_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::ANTENNA1));
    antenna2_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::ANTENNA2));
    feedId_p.attach(msFreqOffset, NewMSFreqOffset::
		    columnName(NewMSFreqOffset::FEED_ID));
    interval_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::INTERVAL));
    offset_p.attach(msFreqOffset, NewMSFreqOffset::
		    columnName(NewMSFreqOffset::OFFSET));
    spectralWindowId_p.attach(msFreqOffset, NewMSFreqOffset::
			      columnName(NewMSFreqOffset::SPECTRAL_WINDOW_ID));
    time_p.attach(msFreqOffset, NewMSFreqOffset::
		  columnName(NewMSFreqOffset::TIME));
    timeMeas_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::TIME));
    intervalQuant_p.attach(msFreqOffset, NewMSFreqOffset::
			   columnName(NewMSFreqOffset::INTERVAL));
    offsetQuant_p.attach(msFreqOffset, NewMSFreqOffset::
			 columnName(NewMSFreqOffset::OFFSET));
    timeQuant_p.attach(msFreqOffset, NewMSFreqOffset::
		       columnName(NewMSFreqOffset::TIME));
  }
}

NewMSFreqOffsetColumns::NewMSFreqOffsetColumns(NewMSFreqOffset& msFreqOffset):
  RONewMSFreqOffsetColumns(),
  antenna1_p(),
  antenna2_p(),
  feedId_p(),
  interval_p(),
  offset_p(),
  spectralWindowId_p(),
  time_p(),
  timeMeas_p(),
  intervalQuant_p(),
  offsetQuant_p(),
  timeQuant_p()
{
  attach(msFreqOffset);
}

NewMSFreqOffsetColumns::~NewMSFreqOffsetColumns() {}


void NewMSFreqOffsetColumns::setEpochRef(MEpoch::Types ref) {
  timeMeas_p.setDescRefCode(ref);
}

NewMSFreqOffsetColumns::NewMSFreqOffsetColumns():
  RONewMSFreqOffsetColumns(),
  antenna1_p(),
  antenna2_p(),
  feedId_p(),
  interval_p(),
  offset_p(),
  spectralWindowId_p(),
  time_p(),
  timeMeas_p(),
  intervalQuant_p(),
  offsetQuant_p(),
  timeQuant_p()
{
}

void NewMSFreqOffsetColumns::attach(NewMSFreqOffset& msFreqOffset) 
{
  RONewMSFreqOffsetColumns::attach(msFreqOffset);
  if (!isNull()) {
    antenna1_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::ANTENNA1));
    antenna2_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::ANTENNA2));
    feedId_p.attach(msFreqOffset, NewMSFreqOffset::
		    columnName(NewMSFreqOffset::FEED_ID));
    interval_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::INTERVAL));
    offset_p.attach(msFreqOffset, NewMSFreqOffset::
		    columnName(NewMSFreqOffset::OFFSET));
    spectralWindowId_p.attach(msFreqOffset, NewMSFreqOffset::
			      columnName(NewMSFreqOffset::SPECTRAL_WINDOW_ID));
    time_p.attach(msFreqOffset, NewMSFreqOffset::
		  columnName(NewMSFreqOffset::TIME));
    timeMeas_p.attach(msFreqOffset, NewMSFreqOffset::
		      columnName(NewMSFreqOffset::TIME));
    intervalQuant_p.attach(msFreqOffset, NewMSFreqOffset::
			   columnName(NewMSFreqOffset::INTERVAL));
    offsetQuant_p.attach(msFreqOffset, NewMSFreqOffset::
			 columnName(NewMSFreqOffset::OFFSET));
    timeQuant_p.attach(msFreqOffset, NewMSFreqOffset::
		       columnName(NewMSFreqOffset::TIME));
  }
}

// Local Variables: 
// compile-command: "gmake NewMSFreqOffColumns"
// End: 
