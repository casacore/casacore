//# AppInfo.cc: General information for applications
//# Copyright (C) 1996,1997,1998
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

#include <aips/Arrays/Vector.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Tasking/AipsrcVector.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/Time.h>
#include <aips/OS/Memory.h>
#include <aips/Quanta/Unit.h>
#include <aips/Arrays/Vector.h>
#include <aips/OS/Directory.h>
#include <aips/Logging/LogIO.h>

Bool AppInfo::need_init_p = True;
uInt AppInfo::memory_r = 0;
uInt AppInfo::nproc_r = 0;
uInt AppInfo::tz_r = 0;

Vector<String> AppInfo::workDirectories(uInt minimumFreeSpaceInMB)
{
    static Bool init = False;
    static uInt workdir = 0;
    if (!init) {
	init = True;
	// Default is an empty vector
	Vector<String> empty;
	workdir = AipsrcVector<String>::registerRC("user.directories.work", 
						   empty);
    }

    Vector<String> workdirs(AipsrcVector<String>::get(workdir).copy());

    if (workdirs.nelements() == 0) {
	// We haven't been given a work directory list, so use a sensible
	// default. If "." exists and is writable use it, otherwise use "/tmp".
	Directory dir(".");
	if (!dir.exists() || !dir.isWritable()) {
	    dir = Directory("/tmp");
	}
	if (dir.exists() && dir.isWritable()) {
	    workdirs.resize(1);
	    workdirs(0) = dir.path().originalName();
	}
    }
    // OK, elmiinate candidates (if any).
    Vector<Bool> good(workdirs.nelements());
    good = True;
    for (uInt i=0; i<workdirs.nelements(); i++) {
	File dir(workdirs(i));
	if (!dir.exists() || !dir.isWritable() || !dir.isDirectory()) {
	    // Whinge if it's for an odd reason
	    LogIO os(LogOrigin("AppInfo", "workDirectories(uInt)", WHERE));
	    os << LogIO::WARN << "Work directory candidate '" <<
		dir.path().originalName() << "' does not exist or is not" <<
		" writable.\n" <<
		"Check aipsrc variable user.directories.work." << 
		LogIO::POST;
	    good(i) = False;
	} else {
	    Directory asdir = dir;
	    if (asdir.freeSpace()/(1024*1024) < uLong(minimumFreeSpaceInMB)) {
		good(i) = False;
	    }
	}
    }
    // Compress the array
    MaskedArray<String> masked(workdirs, good);
    workdirs.resize(0);
    workdirs = masked.getCompressedArray();
    return workdirs;
}

String AppInfo::workDirectory(uInt minimumFreeSpaceInMB)
{
    static uInt count = 0;
    count++;
    Vector<String> candidates = workDirectories(minimumFreeSpaceInMB);
    if (candidates.nelements() == 0) {
	LogIO os(LogOrigin("AppInfo", "workDirectory(uInt)", WHERE));
	os << LogIO::SEVERE << "No work directory with at least " <<
	    minimumFreeSpaceInMB << "MB free can be found." << endl <<
	    "Check aipsrc variable user.directories.work." << 
	    LogIO::EXCEPTION;
    }
    return candidates((count-1) % candidates.nelements());
}

String AppInfo::workFileName(uInt minimumFreeSpaceInMB,
			     const String &filenamePrefix)
{
    String dir = workDirectory(minimumFreeSpaceInMB);
    return File::newUniqueName(dir, filenamePrefix).originalName();
}

Int AppInfo::availableMemoryInMB()
{
  if (need_init_p) {
    init();
  }
  Int  used = Memory::allocatedMemoryInBytes()/1024/1024;
  Int allowed = memoryInMB();
  return allowed - used;
}

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
