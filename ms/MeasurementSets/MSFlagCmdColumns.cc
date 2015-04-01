//# MSFlagCmdColumns.cc:  provides easy access to MeasurementSet columns
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

#include <casacore/ms/MeasurementSets/MSFlagCmdColumns.h>
#include <casacore/ms/MeasurementSets/MSFlagCmd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROMSFlagCmdColumns::ROMSFlagCmdColumns(const MSFlagCmd& msFlagCmd):
  applied_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::APPLIED)),
  command_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::COMMAND)),
  interval_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::INTERVAL)),
  level_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::LEVEL)),
  reason_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::REASON)),
  severity_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::SEVERITY)),
  time_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME)),
  type_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TYPE)),
  timeMeas_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME)),
  intervalQuant_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::INTERVAL)),
  timeQuant_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME))
{}

ROMSFlagCmdColumns::~ROMSFlagCmdColumns() {}

ROMSFlagCmdColumns::ROMSFlagCmdColumns():
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

void ROMSFlagCmdColumns::attach(const MSFlagCmd& msFlagCmd)
{
  applied_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::APPLIED));
  command_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::COMMAND));
  interval_p.attach(msFlagCmd, MSFlagCmd::
		    columnName(MSFlagCmd::INTERVAL));
  level_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::LEVEL));
  reason_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::REASON));
  severity_p.attach(msFlagCmd, MSFlagCmd::
		    columnName(MSFlagCmd::SEVERITY));
  time_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME));
  type_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TYPE));
  timeMeas_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME));
  intervalQuant_p.attach(msFlagCmd, MSFlagCmd::
			 columnName(MSFlagCmd::INTERVAL));
  timeQuant_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME));
}

MSFlagCmdColumns::MSFlagCmdColumns(MSFlagCmd& msFlagCmd):
  ROMSFlagCmdColumns(msFlagCmd),
  applied_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::APPLIED)),
  command_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::COMMAND)),
  interval_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::INTERVAL)),
  level_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::LEVEL)),
  reason_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::REASON)),
  severity_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::SEVERITY)),
  time_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME)),
  type_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TYPE)),
  timeMeas_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME)),
  intervalQuant_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::INTERVAL)),
  timeQuant_p(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME))
{}

MSFlagCmdColumns::~MSFlagCmdColumns() {}


void MSFlagCmdColumns::setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty){
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

MSFlagCmdColumns::MSFlagCmdColumns():
  ROMSFlagCmdColumns(),
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

void MSFlagCmdColumns::attach(MSFlagCmd& msFlagCmd)
{
  ROMSFlagCmdColumns::attach(msFlagCmd);
  applied_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::APPLIED));
  command_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::COMMAND));
  interval_p.attach(msFlagCmd, MSFlagCmd::
		    columnName(MSFlagCmd::INTERVAL));
  level_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::LEVEL));
  reason_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::REASON));
  severity_p.attach(msFlagCmd, MSFlagCmd::
		    columnName(MSFlagCmd::SEVERITY));
  time_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME));
  type_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TYPE));
  timeMeas_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME));
  intervalQuant_p.attach(msFlagCmd, MSFlagCmd::
			 columnName(MSFlagCmd::INTERVAL));
  timeQuant_p.attach(msFlagCmd, MSFlagCmd::columnName(MSFlagCmd::TIME));
}
// Local Variables: 
// compile-command: "gmake MSFlagCmdColumns"
// End: 

} //# NAMESPACE CASACORE - END

