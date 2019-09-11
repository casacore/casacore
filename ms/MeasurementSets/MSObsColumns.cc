//# MSObsColumns.cc:  provides easy access to MeasurementSet columns
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

#include <casacore/ms/MeasurementSets/MSObsColumns.h>
#include <casacore/ms/MeasurementSets/MSObservation.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSObservationColumns::MSObservationColumns()
{
}

MSObservationColumns::
MSObservationColumns(const MSObservation& msObservation)
{
  attach(msObservation);
}

MSObservationColumns::~MSObservationColumns() {}

void MSObservationColumns::attach(const MSObservation& msObservation)
{
  flagRow_p.attach(msObservation, MSObservation::
		   columnName(MSObservation::FLAG_ROW));
  log_p.attach(msObservation, 
	       MSObservation::columnName(MSObservation::LOG));
  observer_p.attach(msObservation, MSObservation::
		    columnName(MSObservation::OBSERVER));
  project_p.attach(msObservation, MSObservation::
		   columnName(MSObservation::PROJECT));
  releaseDate_p.attach(msObservation, MSObservation::
		       columnName(MSObservation::RELEASE_DATE));
  schedule_p.attach(msObservation, MSObservation::
		    columnName(MSObservation::SCHEDULE));
  scheduleType_p.attach(msObservation, MSObservation::
			columnName(MSObservation::SCHEDULE_TYPE));
  telescopeName_p.attach(msObservation, MSObservation::
			 columnName(MSObservation::TELESCOPE_NAME));
  timeRange_p.attach(msObservation, MSObservation::
		     columnName(MSObservation::TIME_RANGE));
  releaseDateMeas_p.attach(msObservation, MSObservation::
			   columnName(MSObservation::RELEASE_DATE));
  timeRangeMeas_p.attach(msObservation, MSObservation::
			 columnName(MSObservation::TIME_RANGE));
  releaseDateQuant_p.attach(msObservation, MSObservation::
			    columnName(MSObservation::RELEASE_DATE));
  timeRangeQuant_p.attach(msObservation, MSObservation::
			  columnName(MSObservation::TIME_RANGE));
}

void MSObservationColumns::
setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty) {
  timeRangeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
  releaseDateMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

} //# NAMESPACE CASACORE - END

