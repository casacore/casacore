//# ExternalLockSync.h: Class to hold table lock data
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

#ifndef TABLES_EXTERNALLOCKSYNC_H
#define TABLES_EXTERNALLOCKSYNC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableLockData.h>
#include <casacore/tables/Tables/TableSyncData.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Class to hold table lock data.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tTable" demos="">
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


class ExternalLockSync
{
public: 
    // Construct from the given TableLock object.
    ExternalLockSync (const TableLock& lockOptions);

    ~ExternalLockSync();

    // Create the <src>LockFile</src> object and acquire a read or write
    // lock when permanent locking is in effect.
    // It throws an exception when acquiring the lock failed.
    void makeLock (const String& tableName, Bool create, FileLocker::LockType);

    // Acquire a read or write lock (when needed).
    // Nattempts==0 indicates that it has to wait until the lock is acquired.
    // Nattempts>0 indicates that it gives up acquiring the lock when
    // nattempts have been done (with 1 second intervals).
    // It throws an exception when acquire failed while it had to wait.
    // It returns a false status when acquiring the lock failed
    // while it does not have to wait.
    // <br>When a lock is successfully acquired, the number of rows
    // (see function nrrow() below) is reset as a result of
    // synchronizing the access to the table.
    Bool acquire (FileLocker::LockType = FileLocker::Write, uInt nattempts = 0);

    // Get the current number of rows in this object.
    uInt nrow() const;
    
    // Release the lock and synchronize the table access.
    // When autolocking is in effect, the lock is only released when
    // the inspection-interval (see class
    // <linkto class=TableLockData>TableLockData</linkto>) has expired.
    // It does nothing when permanent locking is used.
    // It throws an exception when the release failed.
    void release (uInt nrrow);

    // Check if the table has a read or write lock, thus if the table can
    // be read or written safely.
    Bool hasLock (FileLocker::LockType) const;

private:
    // Copy constructor is forbidden.
    ExternalLockSync (const ExternalLockSync& that);

    // Assignment is forbidden.
    ExternalLockSync& operator= (const ExternalLockSync& that);

    // The callback function when releasing a lock.
    static MemoryIO* releaseCallBack (void* lockSyncObject, Bool always);

    // The member function executing the callback functionality.
    MemoryIO* doReleaseCallBack (Bool always);


    //# Define the lock and sync data objects.
    TableLockData  itsLock;
    TableSyncData  itsSync;
    uInt           itsNrrow;
};


inline Bool ExternalLockSync::hasLock (FileLocker::LockType type) const
{
    return itsLock.hasLock (type);
}
inline void ExternalLockSync::release (uInt nrrow)
{
    itsNrrow = nrrow;
    itsLock.release();
}
inline MemoryIO* ExternalLockSync::doReleaseCallBack (Bool)
{
    itsSync.write (itsNrrow);
    return &(itsSync.memoryIO());
}
inline uInt ExternalLockSync::nrow() const
{
    return itsNrrow;
}



} //# NAMESPACE CASACORE - END

#endif
