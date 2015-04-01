//# FileLocker.h: Class to handle file locking
//# Copyright (C) 1997,1998,1999,2000
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

#ifndef CASA_FILELOCKER_H
#define CASA_FILELOCKER_H


//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;


// <summary> 
// Class to handle file locking.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLockFile" demos="">
// </reviewed>

// <prerequisite> 
//    <li> man page of fcntl
// </prerequisite>

// <synopsis> 
// This class handles file locking by means of the fcntl SETLK function.
// Locking of files on NFS-mounted file systems works correctly
// as long as the NFS lockd and statd deamons are configured correctly.
// Otherwise lock requests may be granted incorrectly.
// <p>
// Acquiring a lock can be done for a read or a write lock.
// Multiple locks on a file can exist as long as they are all
// read locks. When a write lock is involved, no other lock can exist.
// It is possible to acquire a lock in 2 ways:
// <ul>
// <li>Wait until the lock request is granted; i.e. until the processes
//     holding a lock on the file release their lock.
// <li>Do several attempts; between each attempt it sleeps 1 second.
//     Note that nattempts=1 means it returns immediately when the
//     lock request could not be granted.
// </ul>
// </synopsis>

// <example>
// <srcblock>
// int fd = open ("file.name");
// FileLocker lock(fd);
// if (lock.acquire()) {
//     ... do something with the file ...
//     lock.release();
// }else{
//     cout << lock.lastMessage() << endl;
// }
// </srcblock>
// </example>

// <motivation> 
// Make it possible to lock files in a standard way.
// </motivation>


class FileLocker
{
public:
    // Define the possible lock types.
    enum LockType {
	// Acquire a read lock.
	Read,
	// Acquire a write lock.
	Write
    };

    // Default constructor creates an invalid fd.
    FileLocker();

    // Construct the FileLocker object for the given file descriptor.
    // This can be used to lock a segment of the given file.
    // The segment is given by start and length. Length=0 means till the
    // end of the file.
    explicit FileLocker (int fd, uInt start=0, uInt length=0);

    ~FileLocker();

    // Acquire a write or read lock.
    // <src>nattempts</src> defines how often it tries to acquire the lock.
    // A zero value indicates an infinite number of times (i.e. wait until
    // the lock is acquired).
    // A positive value means it waits 1 second between each attempt.
    Bool acquire (LockType = Write, uInt nattempts = 0);

    // Release a lock.
    // The return status indicates if an error occurred.
    Bool release();

    // Test if the file can be locked for read or write.
    // Optionally the PID of the process holding the lock is returned.
    // <group>
    Bool canLock (LockType = Write);
    Bool canLock (uInt& pid, LockType = Write);
    // </group>

    // Test if the process has a lock for read or write on the file.
    Bool hasLock (LockType = Write) const;

    // Get the fd in use.
    int fd() const;

    // Get the last error.
    int lastError() const;

    // Get the message belonging to the last error.
    String lastMessage() const;

private:
    int    itsFD;
    int    itsError;
    int    itsStart;
    int    itsLength;
    Bool   itsMsgShown;           /// temporary for SUSE 6.1
    Bool   itsReadLocked;
    Bool   itsWriteLocked;
};


inline Bool FileLocker::hasLock (LockType type) const
{
    return (type == Write  ?  itsWriteLocked : itsReadLocked);
}
inline int FileLocker::fd() const
{
    return itsFD;
}
inline int FileLocker::lastError() const
{
    return itsError;
}



} //# NAMESPACE CASACORE - END

#endif

