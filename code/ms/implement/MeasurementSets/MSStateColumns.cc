//# NewMSStateColumns.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSStateColumns.h>
#include <aips/MeasurementSets/NewMSState.h>

RONewMSStateColumns::RONewMSStateColumns(const NewMSState& msState):
  cal_p(msState, NewMSState::columnName(NewMSState::CAL)),
  flagRow_p(msState, NewMSState::columnName(NewMSState::FLAG_ROW)),
  load_p(msState, NewMSState::columnName(NewMSState::LOAD)),
  obsMode_p(msState, NewMSState::columnName(NewMSState::OBS_MODE)),
  ref_p(msState, NewMSState::columnName(NewMSState::REF)),
  sig_p(msState, NewMSState::columnName(NewMSState::SIG)),
  subScan_p(msState, NewMSState::columnName(NewMSState::SUB_SCAN)),
  calQuant_p(msState, NewMSState::columnName(NewMSState::CAL)),
  loadQuant_p(msState, NewMSState::columnName(NewMSState::LOAD))
{}

RONewMSStateColumns::~RONewMSStateColumns() {}

RONewMSStateColumns::RONewMSStateColumns():
  cal_p(),
  flagRow_p(),
  load_p(),
  obsMode_p(),
  ref_p(),
  sig_p(),
  subScan_p(),
  calQuant_p(),
  loadQuant_p()
{}

void RONewMSStateColumns::attach(const NewMSState& msState) 
{
  cal_p.attach(msState, NewMSState::columnName(NewMSState::CAL));
  flagRow_p.attach(msState, NewMSState::columnName(NewMSState::FLAG_ROW));
  load_p.attach(msState, NewMSState::columnName(NewMSState::LOAD));
  obsMode_p.attach(msState, NewMSState::columnName(NewMSState::OBS_MODE));
  ref_p.attach(msState, NewMSState::columnName(NewMSState::REF));
  sig_p.attach(msState, NewMSState::columnName(NewMSState::SIG));
  subScan_p.attach(msState, NewMSState::columnName(NewMSState::SUB_SCAN));
  calQuant_p.attach(msState, NewMSState::columnName(NewMSState::CAL));
  loadQuant_p.attach(msState, NewMSState::columnName(NewMSState::LOAD));
}

NewMSStateColumns::NewMSStateColumns(NewMSState& msState):
  cal_p(msState, NewMSState::columnName(NewMSState::CAL)),
  flagRow_p(msState, NewMSState::columnName(NewMSState::FLAG_ROW)),
  load_p(msState, NewMSState::columnName(NewMSState::LOAD)),
  obsMode_p(msState, NewMSState::columnName(NewMSState::OBS_MODE)),
  ref_p(msState, NewMSState::columnName(NewMSState::REF)),
  sig_p(msState, NewMSState::columnName(NewMSState::SIG)),
  subScan_p(msState, NewMSState::columnName(NewMSState::SUB_SCAN)),
  calQuant_p(msState, NewMSState::columnName(NewMSState::CAL)),
  loadQuant_p(msState, NewMSState::columnName(NewMSState::LOAD))
{}

NewMSStateColumns::~NewMSStateColumns() {}

NewMSStateColumns::NewMSStateColumns():
  RONewMSStateColumns(),
  cal_p(),
  flagRow_p(),
  load_p(),
  obsMode_p(),
  ref_p(),
  sig_p(),
  subScan_p(),
  calQuant_p(),
  loadQuant_p()
{}

void NewMSStateColumns::attach(NewMSState& msState) 
{
  RONewMSStateColumns::attach(msState);
  cal_p.attach(msState, NewMSState::columnName(NewMSState::CAL));
  flagRow_p.attach(msState, NewMSState::columnName(NewMSState::FLAG_ROW));
  load_p.attach(msState, NewMSState::columnName(NewMSState::LOAD));
  obsMode_p.attach(msState, NewMSState::columnName(NewMSState::OBS_MODE));
  ref_p.attach(msState, NewMSState::columnName(NewMSState::REF));
  sig_p.attach(msState, NewMSState::columnName(NewMSState::SIG));
  subScan_p.attach(msState, NewMSState::columnName(NewMSState::SUB_SCAN));
  calQuant_p.attach(msState, NewMSState::columnName(NewMSState::CAL));
  loadQuant_p.attach(msState, NewMSState::columnName(NewMSState::LOAD));
}
// Local Variables: 
// compile-command: "gmake NewMSStateColumns"
// End: 
