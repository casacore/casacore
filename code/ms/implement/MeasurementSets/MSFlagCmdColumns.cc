//# NewMSFlagCmdColumns.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSFlagCmdColumns.h>
#include <aips/MeasurementSets/NewMSFlagCmd.h>

RONewMSFlagCmdColumns::RONewMSFlagCmdColumns(const NewMSFlagCmd& msFlagCmd):
  applied_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::APPLIED)),
  command_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::COMMAND)),
  interval_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::INTERVAL)),
  level_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::LEVEL)),
  reason_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::REASON)),
  severity_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::SEVERITY)),
  time_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME)),
  type_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TYPE)),
  timeMeas_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME)),
  intervalQuant_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::INTERVAL)),
  timeQuant_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME))
{}

RONewMSFlagCmdColumns::~RONewMSFlagCmdColumns() {}

RONewMSFlagCmdColumns::RONewMSFlagCmdColumns():
  applied_p(),
  command_p(),
  interval_p(),
  level_p(),
  reason_p(),
  severity_p(),
  time_p(),
  type_p(),
  timeMeas_p(),
  intervalQuant_p(),
  timeQuant_p()
{}

void RONewMSFlagCmdColumns::attach(const NewMSFlagCmd& msFlagCmd)
{
  applied_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::APPLIED));
  command_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::COMMAND));
  interval_p.attach(msFlagCmd, NewMSFlagCmd::
		    columnName(NewMSFlagCmd::INTERVAL));
  level_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::LEVEL));
  reason_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::REASON));
  severity_p.attach(msFlagCmd, NewMSFlagCmd::
		    columnName(NewMSFlagCmd::SEVERITY));
  time_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME));
  type_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TYPE));
  timeMeas_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME));
  intervalQuant_p.attach(msFlagCmd, NewMSFlagCmd::
			 columnName(NewMSFlagCmd::INTERVAL));
  timeQuant_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME));
}

NewMSFlagCmdColumns::NewMSFlagCmdColumns(NewMSFlagCmd& msFlagCmd):
  RONewMSFlagCmdColumns(msFlagCmd),
  applied_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::APPLIED)),
  command_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::COMMAND)),
  interval_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::INTERVAL)),
  level_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::LEVEL)),
  reason_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::REASON)),
  severity_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::SEVERITY)),
  time_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME)),
  type_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TYPE)),
  timeMeas_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME)),
  intervalQuant_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::INTERVAL)),
  timeQuant_p(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME))
{}

NewMSFlagCmdColumns::~NewMSFlagCmdColumns() {}

NewMSFlagCmdColumns::NewMSFlagCmdColumns():
  RONewMSFlagCmdColumns(),
  applied_p(),
  command_p(),
  interval_p(),
  level_p(),
  reason_p(),
  severity_p(),
  time_p(),
  type_p(),
  timeMeas_p(),
  intervalQuant_p(),
  timeQuant_p()
{}

void NewMSFlagCmdColumns::attach(NewMSFlagCmd& msFlagCmd)
{
  RONewMSFlagCmdColumns::attach(msFlagCmd);
  applied_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::APPLIED));
  command_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::COMMAND));
  interval_p.attach(msFlagCmd, NewMSFlagCmd::
		    columnName(NewMSFlagCmd::INTERVAL));
  level_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::LEVEL));
  reason_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::REASON));
  severity_p.attach(msFlagCmd, NewMSFlagCmd::
		    columnName(NewMSFlagCmd::SEVERITY));
  time_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME));
  type_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TYPE));
  timeMeas_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME));
  intervalQuant_p.attach(msFlagCmd, NewMSFlagCmd::
			 columnName(NewMSFlagCmd::INTERVAL));
  timeQuant_p.attach(msFlagCmd, NewMSFlagCmd::columnName(NewMSFlagCmd::TIME));
}
// Local Variables: 
// compile-command: "gmake NewMSFlagCmdColumns"
// End: 
