//# MSStateColumns.cc:  provides easy access to MeasurementSet columns
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

#include <aips/MeasurementSets/MSStateColumns.h>
#include <aips/MeasurementSets/MSState.h>

ROMSStateColumns::ROMSStateColumns(const MSState& msState):
  cal_p(msState, MSState::columnName(MSState::CAL)),
  flagRow_p(msState, MSState::columnName(MSState::FLAG_ROW)),
  load_p(msState, MSState::columnName(MSState::LOAD)),
  obsMode_p(msState, MSState::columnName(MSState::OBS_MODE)),
  ref_p(msState, MSState::columnName(MSState::REF)),
  sig_p(msState, MSState::columnName(MSState::SIG)),
  subScan_p(msState, MSState::columnName(MSState::SUB_SCAN)),
  calQuant_p(msState, MSState::columnName(MSState::CAL)),
  loadQuant_p(msState, MSState::columnName(MSState::LOAD))
{}

ROMSStateColumns::~ROMSStateColumns() {}

ROMSStateColumns::ROMSStateColumns():
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

void ROMSStateColumns::attach(const MSState& msState) 
{
  cal_p.attach(msState, MSState::columnName(MSState::CAL));
  flagRow_p.attach(msState, MSState::columnName(MSState::FLAG_ROW));
  load_p.attach(msState, MSState::columnName(MSState::LOAD));
  obsMode_p.attach(msState, MSState::columnName(MSState::OBS_MODE));
  ref_p.attach(msState, MSState::columnName(MSState::REF));
  sig_p.attach(msState, MSState::columnName(MSState::SIG));
  subScan_p.attach(msState, MSState::columnName(MSState::SUB_SCAN));
  calQuant_p.attach(msState, MSState::columnName(MSState::CAL));
  loadQuant_p.attach(msState, MSState::columnName(MSState::LOAD));
}

MSStateColumns::MSStateColumns(MSState& msState):
  ROMSStateColumns(msState),
  cal_p(msState, MSState::columnName(MSState::CAL)),
  flagRow_p(msState, MSState::columnName(MSState::FLAG_ROW)),
  load_p(msState, MSState::columnName(MSState::LOAD)),
  obsMode_p(msState, MSState::columnName(MSState::OBS_MODE)),
  ref_p(msState, MSState::columnName(MSState::REF)),
  sig_p(msState, MSState::columnName(MSState::SIG)),
  subScan_p(msState, MSState::columnName(MSState::SUB_SCAN)),
  calQuant_p(msState, MSState::columnName(MSState::CAL)),
  loadQuant_p(msState, MSState::columnName(MSState::LOAD))
{}

MSStateColumns::~MSStateColumns() {}

MSStateColumns::MSStateColumns():
  ROMSStateColumns(),
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

void MSStateColumns::attach(MSState& msState) 
{
  ROMSStateColumns::attach(msState);
  cal_p.attach(msState, MSState::columnName(MSState::CAL));
  flagRow_p.attach(msState, MSState::columnName(MSState::FLAG_ROW));
  load_p.attach(msState, MSState::columnName(MSState::LOAD));
  obsMode_p.attach(msState, MSState::columnName(MSState::OBS_MODE));
  ref_p.attach(msState, MSState::columnName(MSState::REF));
  sig_p.attach(msState, MSState::columnName(MSState::SIG));
  subScan_p.attach(msState, MSState::columnName(MSState::SUB_SCAN));
  calQuant_p.attach(msState, MSState::columnName(MSState::CAL));
  loadQuant_p.attach(msState, MSState::columnName(MSState::LOAD));
}
// Local Variables: 
// compile-command: "gmake MSStateColumns"
// End: 
