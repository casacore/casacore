//# TableLockData.h: Class to hold table lock data
//# Copyright (C) 1997,1998
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

#if !defined(AIPS_TABLELOCKDATA_H)
#define AIPS_TABLELOCKDATA_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/TableLock.h>


// <summary> 
// Class to hold table lock data.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTable" demos="">
// </reviewed>

// <prerequisite> 
//    <li> class <linkto class=Table>TableLock</linkto>
// </prerequisite>

// <synopsis> 
// This class keeps the <src>LockFile</src> object used to do the
// actual locking/unlocking.
// It also keeps the synchronization information.
// </synopsis>

// <motivation> 
// Encapsulate Table locking data.
// </motivation>


class TableLockData : public TableLock
{
public: 
    // Define the signature of the callback function when a lock is released.
    // The flag <src>always</src> tells if the callback function should
    // always write its main data (meant for case that table gets closed).
    // The callback function has to write the synchronization data
    // (preferably in canonical format) in a MemoryIO object.
    // A pointer to this MemoryIO object has to be returned. A zero pointer
    // can be returned when no synchronization data is available.
    typedef MemoryIO* ReleaseCallBack (void* parentObject, Bool always);

    // Construct from the given TableLock object.
    TableLockData (const TableLock& lockOptions, ReleaseCallBack* = 0,
		   void* releaseParentObject = 0);

    ~TableLockData();

    // Create the <src>LockFile</src> object and acquire a read or write
    // lock when permanent locking is in effect.
    // It throws an exception when acquiring the lock failed.
    void makeLock (const String& name, Bool create, FileLocker::LockType);

    // Acquire a read or write lock.
    // It throws an exception when acquire failed while it had to wait.
    Bool acquire (MemoryIO* info, FileLocker::LockType, uInt nattempts);

    // Release the lock. When always==False, the lock is not released
    // when a permanent lock is used.
    // It does nothing when permanent locking is used.
    // It throws an exception when the release failed.
    // When the lock is released, the release callback function (if defined)
    // is called to write the synchronization data.
    void release (Bool always = False);

    // When the inspection interval has expired, inspect if another process
    // needs the lock. If so, release the lock.
    void autoRelease();

    // Has this process the read or write lock, thus can the table
    // be read or written safely?
    Bool hasLock (FileLocker::LockType) const;

    // Is the table in use (i.e. open) in another process?
    Bool isMultiUsed() const;

private:
    // Copy constructor is forbidden.
    TableLockData (const TableLockData& that);

    // Assignment is forbidden.
    TableLockData& operator= (const TableLockData& that);


    //# Define the lock file.
    LockFile*        itsLock;
    //# Define if the file is already read or write locked.
    Bool             itsReadLocked;
    Bool             itsWriteLocked;
    ReleaseCallBack* itsReleaseCallBack;
    void*            itsReleaseParent;
};


inline Bool TableLockData::hasLock (FileLocker::LockType type) const
{
    return (type == FileLocker::Write  ?  itsWriteLocked : itsReadLocked);
}
inline void TableLockData::autoRelease()
{
    if (option() == AutoLocking  &&  itsLock->inspect()) {
	release();
    }
}
inline Bool TableLockData::isMultiUsed() const
{
    return itsLock->isMultiUsed();
}


#endif
