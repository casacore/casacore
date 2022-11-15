//# MSHistoryColumns.cc:  provides easy access to MeasurementSet columns
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

#include <casacore/ms/MeasurementSets/MSHistoryColumns.h>
#include <casacore/ms/MeasurementSets/MSHistory.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSHistoryColumns::MSHistoryColumns()
{
}

MSHistoryColumns::MSHistoryColumns(const MSHistory& msHistory)
{
  attach(msHistory);
}

MSHistoryColumns::~MSHistoryColumns() {}

void MSHistoryColumns::attach(const MSHistory& msHistory)
{
  application_p.attach(msHistory, MSHistory::
		       columnName(MSHistory::APPLICATION));
  appParams_p.attach(msHistory, MSHistory::
		     columnName(MSHistory::APP_PARAMS));
  cliCommand_p.attach(msHistory, MSHistory::
		      columnName(MSHistory::CLI_COMMAND));
  message_p.attach(msHistory, MSHistory::columnName(MSHistory::MESSAGE));
  objectId_p.attach(msHistory, MSHistory::
		    columnName(MSHistory::OBJECT_ID));
  observationId_p.attach(msHistory, MSHistory::
			 columnName(MSHistory::OBSERVATION_ID));
  origin_p.attach(msHistory, MSHistory::columnName(MSHistory::ORIGIN));
  priority_p.attach(msHistory, MSHistory::
		    columnName(MSHistory::PRIORITY));
  time_p.attach(msHistory, MSHistory::columnName(MSHistory::TIME));
  timeMeas_p.attach(msHistory, MSHistory::columnName(MSHistory::TIME));
  timeQuant_p.attach(msHistory, MSHistory::columnName(MSHistory::TIME));
}

void MSHistoryColumns::setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty)
{
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

} //# NAMESPACE CASACORE - END
