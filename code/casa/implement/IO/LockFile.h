//# LockFile.h: Class to handle file locking and synchronization
//# Copyright (C) 1997
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

#if !defined(AIPS_LOCKFILE_H)
#define AIPS_LOCKFILE_H

#if defined (_AIX)
#pragma implementation ("LockFile.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/IO/FileLocker.h>
#include <aips/OS/Time.h>
#include <aips/Containers/Block.h>
#include <aips/Utilities/String.h>
#include <sys/types.h>

//# Forward declarations
class FilebufIO;
class MemoryIO;
class CanonicalIO;


// <summary> 
// Class to handle file locking and synchronization.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tLockFile" demos="">
// </reviewed>

// <prerequisite> 
//    <li> class <linkto class=FileLocker>FileLocker</linkto>
// </prerequisite>

// <synopsis> 
// This class handles file locking by means of a special lock file
// which serves as the locking mechanism for another file or
// group of files. It is for instance used to lock a table in
// the AIPS++ table system.
// <p>
// The lock file has in principle world read/write access, so every
// process accessing the main file can write information in it.
// The lock file contains the following information (in canonical format):
// <ul>
// <li> A request list indicating which processes wants access.
//      The locking process can inspect this list to decide if it
//      should release its lock. An interval can be defined to be sure
//      that the list is not inspected too often.
//      A user can choose not to add to this list, because it incurs some
//      overhead to write the list. However, that should only be done when
//      one is sure that another process cannot keep a lock forever.
// <li> Other information telling if the state of the main file has changed.
//      A table could store one or more counters in it, which can be used to
//      determine if the table has to refresh its caches.
// </ul>
// When the lock file cannot be opened as read/write, it is opened as
// readonly. It means that the request list cannot be stored in it,
// so the process has no way to tell the other processes it wants
// access to the file. It has to wait until the lock is released.
// <br> In principle a lock file should always be there. However, it
// is allowed (with a constructor option) there is no lock file. In
// that case each lock request succeeds without doing actual locking.
// This mode was needed to be able to handle readonly tables containing
// no lock file.
// <p>
// After each write the <src>fsync</src> function is called to make
// sure that the contents of the file are written to disk. This is
// necessary for correct file synchronization in NFS.

// <example>
// <srcblock>
// // Create/open the lock file (with 1 sec inspection interval).
// // Acquire the lock and get the synchronization info.
// LockFile lock ("file.name", 1);
// MemoryIO syncInfo;
// if (! lock.acquire (syncInfo)) {
//     throw (AipsError ("Locking failed: " + lock.message()));
// }
// while (...) {
//      ... do something with the table files ...
//      // Test if another process needs the files.
//      // If so, synchronize files and release lock.
//      if (lock.inspect()) {
//         do fsync for all other files
//         syncInfo.seek (0);
//         syncInfo.write (...);
//         lock.release (syncInfo);
//         // At this point another process can grab the lock.
//         // Reacquire the lock
//         lock.acquire (syncInfo);
//             throw (AipsError ("Locking failed: " + lock.message()));
//         }
//     }
// }
// </srcblock>
// </example>

// <motivation> 
// Make it possible to lock and synchronize tables in an easy and
// efficient way.
// </motivation>


class LockFile
{
public: 
    // Create or open the lock file with the given name.
    // It is created if create=True or if the file does not exist yet.
    // The interval (in seconds) defines how often function <src>inspect</src>
    // inspects the request list in the lock file.
    // An interval&gt;0 means that it is only inspected if the last inspect
    // was at least <src>inspectInterval</src> seconds ago.
    // An interval&lt;=0 means that <src>inspect</src> always inspects
    // the request list.
    // <br>When addToRequestList=False, function <src>acquire</src> does not
    // add the request to the lock file when a lock cannot be acquired.
    // This may result in better performance, but should be used with care.
    // <br> When mustExist=False, it is allowed that the LockFile
    // does not exist and cannot be created either.
    explicit LockFile (const String& fileName, double inspectInterval = 0,
		       Bool create = False, Bool addToRequestList = True,
		       Bool mustExist = True);

    // The destructor does not delete the file, because it is not known
    // when the last process using the lock file will stop.
    // For the table system this is no problem, because the lock file
    // is contained in the directory of the table, thus deleted when
    // the table gets deleted.
    ~LockFile();

    // Acquire a read or write lock..
    // It reads the information (if argument is given) from the lock file.
    // The user is responsible for interpreting the information (e.g.
    // converting from canonical to local format).
    // The seek pointer in the <src>MemoryIO</src> object is set to 0.
    // <group>
    Bool acquire (Bool write = True, uInt nattempts = 0);
    Bool acquire (MemoryIO& info, Bool write = True, uInt nattempts = 0);
    // </group>

    // Release a lock and write the information (if given) into the lock file.
    // The user is responsible for making the information machine-independent
    // (e.g. converting from local to canonical format).
    // <group>
    Bool release();
    Bool release (const MemoryIO& info);
    // </group>

    // Inspect if another process wants to access the file (i.e. if the
    // request list is not empty).
    // It only inspects if the time passed since the last inspection
    // exceeds the inspection interval as given in the constructor.
    // If the time passed is too short, a "no access needed" state is returned.
    Bool inspect();

    // Test if the file can be locked for read or write.
    Bool canLock (Bool write = True);

    // Get the last error.
    int lastError() const;

    // Get the message belonging to the last error.
    String lastMessage() const;

    // Get the name of the lock file.
    const String& name() const;

    // Acquire the lock (service function for the public functions).
    Bool doAcquire (MemoryIO* info, Bool write, uInt nattempts);

    // Release the lock (service function for the public functions).
    Bool doRelease (const MemoryIO* info);

    // Get the block of request id's.
    const Block<Int>& reqIds() const;

private:
    // The copy constructor cannot be used (its semantics are too difficult).
    LockFile (const LockFile&);

    // Assignment cannot be used (its semantics are too difficult).
    LockFile& operator= (const LockFile&);

    // Get the request id's and the info from the lock file.
    void getInfo (MemoryIO& info);

    // Get an Int from the buffer at the given offset and convert
    // it from canonical to local format.
    // If the buffer is too short (i.e. does not contain the value),
    // a zero value is returned.
    Int getInt (const uChar* buffer, uInt leng, uInt offset) const;

    // Add the request id of this process to the list.
    void addReqId();

    // Remove the request id of this process from the list
    // (and all the ones before it).
    void removeReqId();

    // Get the request list from the file.
    void getReqId();

    // Put the request list into the file.
    void putReqId (int fd) const;

    // Convert the request id from canonical to local format.
    void convReqId (const uChar* buffer, uInt leng);

    // Get the number of request id's.
    Int getNrReqId() const;

    // Put the info into the file (after the request id's).
    void putInfo (const MemoryIO& info) const;


    //# The member variables.
    FileLocker   itsLocker;
    FilebufIO*   itsFileIO;
    CanonicalIO* itsCanIO;
    Bool         itsWritable;         //# lock file is writable?
    Bool         itsAddToList;        //# Should acquire add to request list?
    double       itsInterval;         //# interval between inspections
    Time         itsLastTime;         //# time of last inspection
    String       itsName;             //# Name of lock file
    uInt         itsPid;
    uInt         itsHostId;
    Block<Int>   itsReqId;            //# Id's of processes requesting lock
                                      //# First value contains #req id's
                                      //# Thereafter pid, hostid
};


inline Bool LockFile::acquire (Bool write, uInt nattempts)
{
    return doAcquire (0, write, nattempts);
}
inline Bool LockFile::acquire (MemoryIO& info, Bool write, uInt nattempts)
{
    return doAcquire (&info, write, nattempts);
}
inline Bool LockFile::release()
{
    return doRelease (0);
}
inline Bool LockFile::release (const MemoryIO& info)
{
    return doRelease (&info);
}
inline Bool LockFile::canLock (Bool write)
{
    return (itsFileIO == 0  ?  True : itsLocker.canLock (write));
}
inline int LockFile::lastError() const
{
    return itsLocker.lastError();
}
inline String LockFile::lastMessage() const
{
    return itsLocker.lastMessage();
}
inline const String& LockFile::name() const
{
    return itsName;
}
inline const Block<Int>& LockFile::reqIds() const
{
    return itsReqId;
}


#endif
