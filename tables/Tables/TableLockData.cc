//# TableLockData.cc: Class to hold table lock data
//# Copyright (C) 1997,1998,1999,2000,2001
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


#include <casacore/tables/Tables/TableLockData.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Logging/LogIO.h>
#include <unistd.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableLockData::TableLockData (const TableLock& lockOptions,
			      TableLockData::ReleaseCallBack* releaseCallBack,
			      void* releaseParentObject)
: TableLock          (lockOptions),
  itsLock            (0),
  itsReleaseCallBack (releaseCallBack),
  itsReleaseParent   (releaseParentObject)
{}

TableLockData::~TableLockData()
{
    delete itsLock;
}


void TableLockData::makeLock (const String& name, Bool create,
			      FileLocker::LockType type, uInt locknr)
{
    //# Create lock file object only when not created yet.
    //# It is acceptable that no lock file exists for a readonly table
    //# (to be able to read older tables).
    if (itsLock == 0) {
	itsLock = new LockFile (name + "/table.lock", interval(), create,
				True, False, locknr, isPermanent(),
                                option() == NoLocking);
    }
    //# Acquire a lock when permanent locking is in use.
    if (isPermanent()) {
	uInt nattempts = 1;
	if (option() == PermanentLockingWait) {
	    nattempts = 0;                          // wait
	}
	if (! itsLock->acquire (type, nattempts)) {
	    throw (TableError ("Permanent lock on table " + name +
			       " could not be acquired (" +
			       itsLock->lastMessage() + ")"));
	}
    }
}

Bool TableLockData::acquire (MemoryIO* info,
			     FileLocker::LockType type, uInt nattempts)
{
    //# Try to acquire a lock.
    //# Show a message when we have to wait for a long time.
    //# Start with n attempts, show a message and continue thereafter.
    uInt n = 30;
    if (nattempts > 0  &&  nattempts < n) {
	n = nattempts;
    }
    Bool status = itsLock->acquire (info, type, n);
    if (!status  &&  n != nattempts) {
	String s = "read";
	if (type == FileLocker::Write) {
	    s = "write";
	}
	LogIO os;
	os << "Process " << uInt(getpid()) << ": waiting for "
	   << s << "-lock on file " << itsLock->name();
	os.post();
	if (nattempts > 0) {
	    nattempts -= n;
	}
	status = itsLock->acquire (info, type, nattempts);
	if (status) {
	    os << "Process " << uInt(getpid()) << ": acquired "
	       << s << "-lock on file " << itsLock->name();
	    os.post();
	}else{
	    if (nattempts > 0) {
		os << "Process " << uInt(getpid()) << ": gave up acquiring "
		   << s << "-lock on file " << itsLock->name()
		   << " after " << nattempts << " seconds";
		os.post();
	    }
	}
    }
    //# Throw exception when error while we had to wait forever.
    if (!status) {
	if (nattempts == 0) {
	    throw (TableError ("Error (" + itsLock->lastMessage() +
			       ") when acquiring lock on " + itsLock->name()));
	}
    }
    return status;
}

void TableLockData::release (Bool always)
{
    //# Only release if not permanently locked.
    if (always  ||  !isPermanent()) {
	MemoryIO* memIO = 0;
	if (hasLock (FileLocker::Write)) {
	    if (itsReleaseCallBack != 0) {
		memIO = itsReleaseCallBack (itsReleaseParent, always);
	    }
	}
	if (! itsLock->release (memIO)) {
	    throw (TableError ("Error (" + itsLock->lastMessage() +
			       ") when releasing lock on " + itsLock->name()));
	}
    }
}

} //# NAMESPACE CASACORE - END

