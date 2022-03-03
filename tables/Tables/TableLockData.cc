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
#include <sys/types.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN
//statics
ManagedObjectPool<String, LockFile> TableLockData::processLocks;

TableLockData::TableLockData (const TableLock& lockOptions,
			      TableLockData::ReleaseCallBack* releaseCallBack,
			      void* releaseParentObject)
: TableLock          (lockOptions),
  itsReleaseCallBack (releaseCallBack),
  itsReleaseParent   (releaseParentObject)
{}

TableLockData::~TableLockData()
{
	TableLockLockAllType lg(processLocks);
	// lock will automatically go out of scope -- closing 
	// this TableLockData is not allowed to close the lock
	// for all instances that point to the same filename
}


void TableLockData::makeLock (const String& name, Bool create,
			      FileLocker::LockType type, uInt locknr)
{
	TableLockLockAllType lg(processLocks);
    //# Create lock file object only when not created yet.
    //# It is acceptable that no lock file exists for a readonly table
    //# (to be able to read older tables).
    
	String lockName = name + "/table.lock";
	processLocks.checkConstructObject(std::ref(lockName),
									  std::ref(lockName),
									  interval(),
									  create,
									  True,
									  False,
									  locknr,
									  isPermanent(),
									  option() == NoLocking);
	lockFileName = lockName;
    //# Acquire a lock when permanent locking is in use.
    if (isPermanent()) {
	uInt nattempts = 1;
	if (option() == PermanentLockingWait) {
	    nattempts = 0;                          // wait
	}
	if (! processLocks[lockFileName].acquire (type, nattempts)) {
	    throw (TableError ("Permanent lock on table " + name +
			       " could not be acquired (" +
			       processLocks[lockFileName].lastMessage() + ")"));
	}
    }
}

Bool TableLockData::acquire (MemoryIO* info,
			     FileLocker::LockType type, uInt nattempts)
{
	TableLockLockAllType lg(processLocks);
    //# Try to acquire a lock.
    //# Show a message when we have to wait for a long time.
    //# Start with n attempts, show a message and continue thereafter.
    uInt n = 30;
    if (nattempts > 0  &&  nattempts < n) {
	n = nattempts;
    }
    Bool status = processLocks[lockFileName].acquire (info, type, n);
    if (!status  &&  n != nattempts) {
	String s = "read";
	if (type == FileLocker::Write) {
	    s = "write";
	}
	LogIO os;
	os << "Process " << uInt(getpid()) << ": waiting for "
	   << s << "-lock on file " << processLocks[lockFileName].name();
	os.post();
	if (nattempts > 0) {
	    nattempts -= n;
	}
	status = processLocks[lockFileName].acquire (info, type, nattempts);
	if (status) {
	    os << "Process " << uInt(getpid()) << ": acquired "
	       << s << "-lock on file " << processLocks[lockFileName].name();
	    os.post();
	}else{
	    if (nattempts > 0) {
		os << "Process " << uInt(getpid()) << ": gave up acquiring "
		   << s << "-lock on file " << processLocks[lockFileName].name()
		   << " after " << nattempts << " seconds";
		os.post();
	    }
	}
    }
    //# Throw exception when error while we had to wait forever.
    if (!status) {
	if (nattempts == 0) {
	    throw (TableError ("Error (" + processLocks[lockFileName].lastMessage() +
			       ") when acquiring lock on " + processLocks[lockFileName].name()));
	}
    }
    return status;
}

void TableLockData::release (Bool always)
{
	TableLockLockAllType lg(processLocks);
    //# Only release if not permanently locked.
    if (always  ||  !isPermanent()) {
	MemoryIO* memIO = 0;
	if (hasLock (FileLocker::Write)) {
	    if (itsReleaseCallBack != 0) {
		memIO = itsReleaseCallBack (itsReleaseParent, always);
	    }
	}
	if (! processLocks[lockFileName].release (memIO)) {
	    throw (TableError ("Error (" + processLocks[lockFileName].lastMessage() +
			       ") when releasing lock on " + processLocks[lockFileName].name()));
	}
    }
}

} //# NAMESPACE CASACORE - END

