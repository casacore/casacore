//# NewMSObservationColumns.cc:  provides easy access to NewMeasurementSet columns
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
#include <aips/MeasurementSets/NewMSObsColumns.h>

NewMSObservationColumns::NewMSObservationColumns(NewMSObservation& msObservation):
flagRow_p(msObservation,NewMSObservation::columnName(NewMSObservation::FLAG_ROW)),
log_p(msObservation,NewMSObservation::columnName(NewMSObservation::LOG)),
observer_p(msObservation,NewMSObservation::columnName(NewMSObservation::OBSERVER)),
project_p(msObservation,NewMSObservation::columnName(NewMSObservation::PROJECT)),
releaseDate_p(msObservation,NewMSObservation::columnName(NewMSObservation::RELEASE_DATE)),
schedule_p(msObservation,NewMSObservation::columnName(NewMSObservation::SCHEDULE)),
scheduleType_p(msObservation,
		  NewMSObservation::columnName(NewMSObservation::SCHEDULE_TYPE)),
telescopeName_p(msObservation,
		NewMSObservation::columnName(NewMSObservation::TELESCOPE_NAME)),
timeRange_p(msObservation,
	    NewMSObservation::columnName(NewMSObservation::TIME_RANGE)),
releaseDateMeas_p(msObservation,NewMSObservation::columnName(NewMSObservation::RELEASE_DATE)),
timeRangeMeas_p(msObservation,
		NewMSObservation::columnName(NewMSObservation::TIME_RANGE)),
releaseDateQuant_p(msObservation,NewMSObservation::columnName(NewMSObservation::RELEASE_DATE)),
timeRangeQuant_p(msObservation,
		 NewMSObservation::columnName(NewMSObservation::TIME_RANGE))
{}

NewMSObservationColumns::~NewMSObservationColumns() {}

RONewMSObservationColumns::RONewMSObservationColumns(const NewMSObservation& msObservation):
flagRow_p(msObservation,NewMSObservation::columnName(NewMSObservation::FLAG_ROW)),
log_p(msObservation,NewMSObservation::columnName(NewMSObservation::LOG)),
observer_p(msObservation,NewMSObservation::columnName(NewMSObservation::OBSERVER)),
project_p(msObservation,NewMSObservation::columnName(NewMSObservation::PROJECT)),
releaseDate_p(msObservation,NewMSObservation::columnName(NewMSObservation::RELEASE_DATE)),
schedule_p(msObservation,NewMSObservation::columnName(NewMSObservation::SCHEDULE)),
scheduleType_p(msObservation,
		  NewMSObservation::columnName(NewMSObservation::SCHEDULE_TYPE)),
telescopeName_p(msObservation,
		NewMSObservation::columnName(NewMSObservation::TELESCOPE_NAME)),
timeRange_p(msObservation,
	    NewMSObservation::columnName(NewMSObservation::TIME_RANGE)),
releaseDateMeas_p(msObservation,NewMSObservation::columnName(NewMSObservation::RELEASE_DATE)),
timeRangeMeas_p(msObservation,
		NewMSObservation::columnName(NewMSObservation::TIME_RANGE)),
releaseDateQuant_p(msObservation,NewMSObservation::columnName(NewMSObservation::RELEASE_DATE)),
timeRangeQuant_p(msObservation,
		 NewMSObservation::columnName(NewMSObservation::TIME_RANGE))
{}

RONewMSObservationColumns::~RONewMSObservationColumns() {}

