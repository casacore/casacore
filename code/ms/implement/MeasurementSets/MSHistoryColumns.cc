//# NewMSHistoryColumns.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSHistoryColumns.h>
#include <aips/MeasurementSets/NewMSHistory.h>

RONewMSHistoryColumns::RONewMSHistoryColumns(const NewMSHistory& msHistory):
  application_p(msHistory, NewMSHistory::
		columnName(NewMSHistory::APPLICATION)),
  appParams_p(msHistory, NewMSHistory::columnName(NewMSHistory::APP_PARAMS)),
  cliCommand_p(msHistory, NewMSHistory::columnName(NewMSHistory::CLI_COMMAND)),
  message_p(msHistory, NewMSHistory::columnName(NewMSHistory::MESSAGE)),
  objectId_p(msHistory, NewMSHistory::columnName(NewMSHistory::OBJECT_ID)),
  observationId_p(msHistory, NewMSHistory::
		  columnName(NewMSHistory::OBSERVATION_ID)),
  origin_p(msHistory, NewMSHistory::columnName(NewMSHistory::ORIGIN)),
  priority_p(msHistory, NewMSHistory::columnName(NewMSHistory::PRIORITY)),
  time_p(msHistory, NewMSHistory::columnName(NewMSHistory::TIME)),
  timeMeas_p(msHistory, NewMSHistory::columnName(NewMSHistory::TIME)),
  timeQuant_p(msHistory, NewMSHistory::columnName(NewMSHistory::TIME))
{}

RONewMSHistoryColumns::~RONewMSHistoryColumns() {}

RONewMSHistoryColumns::RONewMSHistoryColumns():
  application_p(),
  appParams_p(),
  cliCommand_p(),
  message_p(),
  objectId_p(),
  observationId_p(),
  origin_p(),
  priority_p(),
  time_p(),
  timeMeas_p(),
  timeQuant_p()
{}

void RONewMSHistoryColumns::attach(const NewMSHistory& msHistory)
{
  application_p.attach(msHistory, NewMSHistory::
		       columnName(NewMSHistory::APPLICATION));
  appParams_p.attach(msHistory, NewMSHistory::
		     columnName(NewMSHistory::APP_PARAMS));
  cliCommand_p.attach(msHistory, NewMSHistory::
		      columnName(NewMSHistory::CLI_COMMAND));
  message_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::MESSAGE));
  objectId_p.attach(msHistory, NewMSHistory::
		    columnName(NewMSHistory::OBJECT_ID));
  observationId_p.attach(msHistory, NewMSHistory::
			 columnName(NewMSHistory::OBSERVATION_ID));
  origin_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::ORIGIN));
  priority_p.attach(msHistory, NewMSHistory::
		    columnName(NewMSHistory::PRIORITY));
  time_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::TIME));
  timeMeas_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::TIME));
  timeQuant_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::TIME));
}

NewMSHistoryColumns::NewMSHistoryColumns(NewMSHistory& msHistory):
  RONewMSHistoryColumns(msHistory),
  application_p(msHistory, NewMSHistory::
		columnName(NewMSHistory::APPLICATION)),
  appParams_p(msHistory, NewMSHistory::columnName(NewMSHistory::APP_PARAMS)),
  cliCommand_p(msHistory, NewMSHistory::columnName(NewMSHistory::CLI_COMMAND)),
  message_p(msHistory, NewMSHistory::columnName(NewMSHistory::MESSAGE)),
  objectId_p(msHistory, NewMSHistory::columnName(NewMSHistory::OBJECT_ID)),
  observationId_p(msHistory, NewMSHistory::
		  columnName(NewMSHistory::OBSERVATION_ID)),
  origin_p(msHistory, NewMSHistory::columnName(NewMSHistory::ORIGIN)),
  priority_p(msHistory, NewMSHistory::columnName(NewMSHistory::PRIORITY)),
  time_p(msHistory, NewMSHistory::columnName(NewMSHistory::TIME)),
  timeMeas_p(msHistory, NewMSHistory::columnName(NewMSHistory::TIME)),
  timeQuant_p(msHistory, NewMSHistory::columnName(NewMSHistory::TIME))
{}

NewMSHistoryColumns::~NewMSHistoryColumns() {}

void NewMSHistoryColumns::setEpochRef(MEpoch::Types ref) {
  timeMeas_p.setDescRefCode(ref);
}

NewMSHistoryColumns::NewMSHistoryColumns():
  RONewMSHistoryColumns(),
  application_p(),
  appParams_p(),
  cliCommand_p(),
  message_p(),
  objectId_p(),
  observationId_p(),
  origin_p(),
  priority_p(),
  time_p(),
  timeMeas_p(),
  timeQuant_p()
{}

void NewMSHistoryColumns::attach(NewMSHistory& msHistory)
{
  RONewMSHistoryColumns::attach(msHistory);
  application_p.attach(msHistory, NewMSHistory::
		       columnName(NewMSHistory::APPLICATION));
  appParams_p.attach(msHistory, NewMSHistory::
		     columnName(NewMSHistory::APP_PARAMS));
  cliCommand_p.attach(msHistory, NewMSHistory::
		      columnName(NewMSHistory::CLI_COMMAND));
  message_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::MESSAGE));
  objectId_p.attach(msHistory, NewMSHistory::
		    columnName(NewMSHistory::OBJECT_ID));
  observationId_p.attach(msHistory, NewMSHistory::
			 columnName(NewMSHistory::OBSERVATION_ID));
  origin_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::ORIGIN));
  priority_p.attach(msHistory, NewMSHistory::
		    columnName(NewMSHistory::PRIORITY));
  time_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::TIME));
  timeMeas_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::TIME));
  timeQuant_p.attach(msHistory, NewMSHistory::columnName(NewMSHistory::TIME));
}
// Local Variables: 
// compile-command: "gmake NewMSHistoryColumns"
// End: 
