//# AppInfo.cc: General information for applications
//# Copyright (C) 1996,1997
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
//#
//# $Id$

#include <aips/Tasking/AppInfo.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/Time.h>
#include <aips/Measures/Unit.h>

Bool AppInfo::need_init_p = True;
uInt AppInfo::memory_r = 0;
uInt AppInfo::nproc_r = 0;
uInt AppInfo::tz_r = 0;

void AppInfo::init() {
  need_init_p = False;
  
  // memory
  Int size = 64; // Default
  memory_r = AipsrcValue<Int>::registerRC("system.resources.memory", size);
  size = AppInfo::memoryInMB();
  
  // # CPU's
  Int numcpu = 1; // Default
  nproc_r = AipsrcValue<Int>::registerRC("system.resources.numcpu", numcpu);
  numcpu = AppInfo::nProcessors();
  
  // timezone
  Double tz;
  // Get System offset as default
  tz_r = AipsrcValue<Double>::
    registerRC("system.time.tzoffset", "h", "d", Time::timeZoneDays());
  tz = AppInfo::timeZone();
  
  // Do the asserts at the end so that all the variables are initialized as
  // well as possible before throwing an exception.
  AlwaysAssert(size > 0, AipsError);
  AlwaysAssert(numcpu > 0, AipsError);
  AlwaysAssert(tz >= -0.625 && tz <= 0.625, AipsError);
}
