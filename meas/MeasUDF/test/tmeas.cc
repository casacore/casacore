//# tmeas.cc: Test program for the meas classes
//# Copyright (C) 2016
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/meas/MeasUDF/Register.h>
#include <iostream>

using namespace casacore;

int main()
{
  HelpMeasUDF::showFuncsPosition (std::cout, False);
  HelpMeasUDF::showFuncsEpoch (std::cout, False);
  HelpMeasUDF::showFuncsDirection (std::cout, False);
  HelpMeasUDF::showFuncsEarthMagnetic (std::cout, False);
  HelpMeasUDF::showFuncsFrequency (std::cout, False);
  HelpMeasUDF::showFuncsRadialVelocity (std::cout, False);
}
