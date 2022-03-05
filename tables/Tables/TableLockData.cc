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
// statics
ManagedObjectPool<TableLockData::CustodianThreadLockKeyType, 
                  TableLockData::CustodianThreadLockValueType> TableLockData::fileThreadLocks;

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
	TableLockLockAllType lg(fileThreadLocks);
    delete itsLock;
}


void TableLockData::makeLock (const String& name, Bool create,
			      FileLocker::LockType type, uInt locknr)
{
	{
		TableLockLockAllType lg(fileThreadLocks);
		//# Create lock file object only when not created yet.
		//# It is acceptable that no lock file exists for a readonly table
		//# (to be able to read older tables).
		if (itsLock == 0) {
			String lockFName = name + "/table.lock";
			itsLock = new LockFile (lockFName, interval(), create,
						True, False, locknr, isPermanent(),
										option() == NoLocking);
			fileThreadLocks.checkConstructObject(giveThreadLockKey());
			// create a new record if not existant (another plaintable may have already opened it)
			getPiDThreadCustodianRecord();
		}
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
	while (True) {
		{ // lg (see below) scope
			//# File lock unfortunately will have to block other
			//# lock files trying to be acquired -- process can only one such file
			//# at a time to ensure thread safety in modfying the thread state
			//# (avoid potential check then set race)
			TableLockLockAllType lg(fileThreadLocks);
			//# Try to acquire a lock.
			//# Start by acquiring for this PiD
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
				} else {
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
			//# Successfully acquired for this PiD, now check that it 
			//# can be acquired for the calling TiD (or the TiD has it already)
			if (acquireThreadLock(type)) return True;
		}// leave lg scope -- other Lock files or PiDs may now try to acquire
		// while this TiD goes to sleep

		// LogIO os;
		// os << "Process " << uInt(getpid()) << " thread " << pthread_self() << ": waiting for "
		// 			<< (type == FileLocker::Write ? "write" : "read") << "-lock on file " << itsLock->name();
		// os.post();
		sleep(1);
		// <--- from above: recheck if this TiD can now get the lock (PiD may have released
		// and lost custody to another PiD)
	} // While True on TiD acquisition
}

void TableLockData::release (Bool always)
{
	TableLockLockAllType lg(fileThreadLocks);
    //# Only release if not permanently locked.
	//# Note:: only one thread may take custody of a permanent locked table
    if (always  ||  !isPermanent()) {
		// At this point we control:
		// - PiD-wide lock through lockguard on class-level object
		// - file lock for the PiD (itsLock is not nullptr and acquired should have 
		//   been called already)
		//
		// LogIO os;
		// os << "Process " << uInt(getpid()) << " thread " << pthread_self() << ": trying to release custody of "
		// << "lock on file " << itsLock->name();
		// os.post();
		if (doTidHaveCustody(ThreadLockState::LockedRead) ||
		    doTidHaveCustody(ThreadLockState::LockedWrite)) {
			// LogIO os;
			// os << "Process " << uInt(getpid()) << " thread " << pthread_self() << ": released custody of "
			// << "lock on file " << itsLock->name();
			// os.post();
			// First relinquish custodian TiD's custody rights on table lock
			TableLockData::CustodianThreadLockValueType& vels = fileThreadLocks[giveThreadLockKey()];
			size_t it = getPiDThreadCustodianRecord();
			std::get<0>(vels[it]) = ThreadLockState::NoLock;
		} 
		// another TiD still has custody => we have no authority to release the PiD lock	
		// TiD does not have release authority on PiD lock
		// Another TiD must do so
		if (doAnyTidHaveCustody(ThreadLockState::LockedRead) ||
			doAnyTidHaveCustody(ThreadLockState::LockedWrite)){
			// LogIO os;
			// os << "Process " << uInt(getpid()) << " thread " << pthread_self() << ": no release authority on "
			// << "lock on file " << itsLock->name();
			// os.post();
			
			return;
		}
		
		// Now relinquish PiD's custody right on table lock
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

TableLockData::CustodianThreadLockKeyType TableLockData::giveThreadLockKey() const {
    TableLockLockAllType lg(fileThreadLocks);
    return itsLock == 0 ? String("") // no locks - Wild West access
                        : itsLock->name();
}

bool TableLockData::doTidHaveCustody(const ThreadLockState& state) const {
    TableLockLockAllType lg(fileThreadLocks);
    if (itsLock == 0) {
        return true; // no locks - Wild West access
    }
    pthread_t _meTid = pthread_self();
    pid_t _mePid = getpid();
    TableLockData::CustodianThreadLockValueType& vels = \
        fileThreadLocks[giveThreadLockKey()];
	// Note:: if thispid did not acquire the lock because we have forked after acquiring
    //        then the ThreadLockState is invald (we forked...)
	bool thisTidState = std::any_of(vels.begin(), vels.end(), 
			[&_mePid,&_meTid,&state](const CustodianThreadValueRecordType& vel)->bool {
		return ((std::get<0>(vel) == state || std::get<0>(vel) == ThreadLockState::LockedWrite) &&
				std::get<1>(vel) == _meTid && 
				std::get<2>(vel) == _mePid);
	});
	// many threads may share read state, as long as no other thread is in write state
	// this thread may write if no other thread is in write state
	return thisTidState;
}

bool TableLockData::doAnyTidHaveCustody(const ThreadLockState& state) const {
    TableLockLockAllType lg(fileThreadLocks);
    if (itsLock == 0) {
        return True; // no locks - Wild West access
    }
    pid_t _mePid = getpid();
    TableLockData::CustodianThreadLockValueType& vels = \
        fileThreadLocks[giveThreadLockKey()];
	bool res = std::any_of(vels.begin(), vels.end(), 
		[&_mePid, &state](const CustodianThreadValueRecordType& vel)->bool {
			return std::get<0>(vel) == state &&
				// thispid did acquire the lock... we did not fork after acquiring
				// ThreadLockState is valid
				std::get<2>(vel) == _mePid;
	});
	return res;
}

Bool TableLockData::hasLock (FileLocker::LockType type) const
{
    TableLockLockAllType lg(fileThreadLocks);
	Bool PiDLocked = itsLock == 0  ?  True  // no locks - Wild West access
								   :  itsLock->hasLock (type);
    return PiDLocked;
}

size_t TableLockData::getPiDThreadCustodianRecord() const {
	TableLockLockAllType lg(fileThreadLocks);
	TableLockData::CustodianThreadLockValueType& vels = fileThreadLocks[giveThreadLockKey()];
	for (size_t i = 0; i < vels.size(); ++i) {
		if (std::get<1>(vels[i]) == pthread_self() &&
		    std::get<2>(vels[i]) == getpid()) {
			return i;
		}
	} 
	// maybe we forked / no record exist yet - create new
	// this lock will not be in custody of the flock either then
	CustodianThreadValueRecordType vel;
	std::get<0>(vel) = ThreadLockState::NoLock;
	std::get<1>(vel) = pthread_self();
	std::get<2>(vel) = getpid();	
	vels.resize(vels.size()+1);
	vels.push_back(vel);
	return vels.size()-1;
}

bool TableLockData::acquireThreadLock(FileLocker::LockType type) {
	TableLockLockAllType lg(fileThreadLocks);
	ThreadLockState reqState = FileLocker::LockType::Read ? ThreadLockState::LockedRead
						   										  : ThreadLockState::LockedWrite;
	TableLockData::CustodianThreadLockValueType& vels = fileThreadLocks[giveThreadLockKey()];
	size_t it = getPiDThreadCustodianRecord();
	if (reqState == ThreadLockState::LockedWrite && doTidHaveCustody(ThreadLockState::LockedWrite)) {
		return True; // base case -> TiD already has write custody, lock level remains Write
	}
	if (!doAnyTidHaveCustody(ThreadLockState::LockedWrite)) {
		// We have successfully obtained the following resources thus far::
		// - then the PiD lock through lock guard lg, blocking all other filelocks trying to call acquire
		//   or change the lock statuses container
		// - the file lock for this PiD at the requested level
		// - we can add either a read or write lock for this TiD since no other TiD
		//   is holding a write lock, so it may just upgrade the lock
		std::get<0>(vels[it]) = reqState; 
		std::get<1>(vels[it]) = pthread_self();
		// may have forked inbetween making a lock and trying to acquire, we must now
		// ensure the ThreadLockState is validated for subsequent doTiDHaveCustody
		// and doAnyTidHaveCustody
		std::get<2>(vels[it]) = getpid();
		// LogIO os;
		// os << "Process " << uInt(getpid()) << " thread " << pthread_self() << ": took custody of "
		// 	<< (type == FileLocker::Write ? "write" : "read") << "-lock on file " << itsLock->name();
		// os.post();
		return true;
	} 
	if (reqState == ThreadLockState::LockedRead && doTidHaveCustody(ThreadLockState::LockedWrite)) {
		// TiD already have sole write custody... now requesting read-only custody
		// doTidHaveCustody guarrantees PiD not stale
		// => downgrade lock
		// LogIO os;
		// os << "Process " << uInt(getpid()) << " thread " << pthread_self() << ": took custody of "
		// 	<< (type == FileLocker::Write ? "write" : "read") << "-lock on file " << itsLock->name();
		// os.post();
		std::get<0>(vels[it]) = reqState;
		return true;
	}
	// another TiD busy in Write mode, may not upgrade to read or write lock, fall through
	return false; 
}

} //# NAMESPACE CASACORE - END

