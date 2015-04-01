//# LockFile.h: Class to handle file locking and synchronization
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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

#ifndef CASA_LOCKFILE_H
#define CASA_LOCKFILE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/FileLocker.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <sys/types.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class FiledesIO;
class MemoryIO;
class CanonicalIO;


// <summary> 
// Class to handle file locking and synchronization.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLockFile" demos="">
// </reviewed>

// <prerequisite> 
//    <li> class <linkto class=FileLocker>FileLocker</linkto>
//    <li> class <linkto class=MemoryIO>MemoryIO</linkto>
// </prerequisite>

// <synopsis> 
// This class handles file locking by means of a special lock file
// which serves as the locking mechanism for another file or
// group of files. It is for instance used to lock a table in
// the Casacore Table System.
// <p>
// The lock file has in principle world read/write access, so every
// process accessing the main file can write information in it.
// The lock file contains the following information (in canonical format):
// <ul>
// <li> A request list indicating which processes want to acquire a lock.
//      The process holding the lock can inspect this list to decide if it
//      should release its lock. An interval can be defined to be sure
//      that the list is not inspected too often.
//      A user can choose not to add to this list, because it incurs some
//      overhead to write the list. However, that should only be done when
//      one is sure that another process cannot keep a lock forever.
// <li> Some information telling if the state of the main file has changed.
//      The information can be used by a process to synchronize its
//      internal buffers with the new contents of the file(s).
//      E.g. a table could store one or more counters in it, which can be
//      used to determine if the table has to refresh its caches.
//      This information is passed as a MemoryIO object and is opaque
//      for the <src>LockFile</src> class. It is simply handled as a
//      stream of bytes.
// </ul>
// <p>
// Acquiring a lock works as follows:
// <ul>
//  <li> Class <linkto class=FileLocker>FileLocker</linkto> is used
//   to do one attempt to acquire a read or write lock.
//  <li> If it fails and multiple attempts have to be done, the
//   request is added to the request list in the lock file to tell
//   the process holding the lock that another process needs a lock.
//  <li> Other attempts (with 1 second intervals) will be done until the
//   lock is acquired or until the maximum number of attempts is reached.
//  <li> The lock request is removed from the request list.
//  <li> When the lock was acquired, the synchronization info is read
//   from the lock file.
// </ul>
// Releasing a lock writes the synchronization info into the lock file
// and tells <src>FileLocker</src> to release the lock.
// <p>
// When the lock file cannot be opened as read/write, it is opened as
// readonly. It means that the request list cannot be stored in it,
// so the process has no way to tell the other processes it wants
// access to the file. It has to wait until the lock is released.
// <br> In principle a lock file should always be there. However, it
// is possible (with a constructor option) that there is no lock file.
// In that case each lock request succeeds without doing actual locking.
// This mode is needed to be able to handle readonly tables containing
// no lock file.
// <p>
// After each write the <src>fsync</src> function is called to make
// sure that the contents of the file are written to disk. This is
// necessary for correct file synchronization in NFS.
// However, at the moment this feature is switched off, because it
// degraded performance severely.
// <p>
// Apart from the read/write lock handling, the <src>LockFile</src>
// also contains a mechanism to detect if a file is opened by another
// process. This can be used to test if a process can safely delete the file.
// For this purpose it sets another read lock when the file gets opened.
// The function <src>isMultiUsed</src> tests this lock to see if the file is
// used in other processes.
// <br> This lock is also used to tell if the file is permanently locked.
// If that is the case, the locked block is 2 bytes instead of 1.
// <p>
// When in the same process multiple LockFile objects are created for the same
// file, deleting one object releases all locks on the file, thus also the
// locks held by the other LockFile objects. This behaviour is due to the way
// file locking is working on UNIX machines (certainly on Solaris 2.6).
// One can use the test program tLockFile to test for this behaviour.
// </synopsis>

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
    // <br> If <src>create==True</src>, a new lock file will always be created.
    // Otherwise it will be created if it does not exist yet.
    // <br> If <src>mustExist==False</src>, it is allowed that the LockFile
    // does not exist and cannot be created either.
    // <br> The seqnr is used to set the offset where LockFile will use 2 bytes
    // to set the locks on. Only in special cases it should be other than 0.
    // At the moment the offset is 2*seqnr.
    // <br> The <src>permLocking</src> argument is used to indicate if
    // permanent locking will be used. If so, it'll indicate so. In that
    // way showLock() can find out if if table is permanently locked.
    // <br> The <src>noLocking</src> argument is used to indicate that
    // no locking is needed. It means that acquiring a lock always succeeds.
    explicit LockFile (const String& fileName, double inspectInterval = 0,
		       Bool create = False, Bool addToRequestList = True,
		       Bool mustExist = True, uInt seqnr = 0,
		       Bool permLocking = False, Bool noLocking = False);

    // The destructor does not delete the file, because it is not known
    // when the last process using the lock file will stop.
    // For the table system this is no problem, because the lock file
    // is contained in the directory of the table, thus deleted when
    // the table gets deleted.
    ~LockFile();

    // Is the file associated with the LockFile object in use in
    // another process?
    Bool isMultiUsed();

    // Acquire a read or write lock.
    // It reads the information (if the <src>info</src> argument is given)
    // from the lock file. The user is responsible for interpreting the
    // information (e.g. converting from canonical to local format).
    // The seek pointer in the <src>MemoryIO</src> object is set to 0,
    // so the user can simply start reading the pointer.
    // <br>The argument <src>nattempts</src> tells how often it is
    // attempted (with 1 second intervals) to acquire the lock if
    // it does not succeed.
    // 0 means forever, while 1 means do not retry.
    // <group>
    Bool acquire (FileLocker::LockType = FileLocker::Write, uInt nattempts = 0);
    Bool acquire (MemoryIO& info, FileLocker::LockType = FileLocker::Write,
		  uInt nattempts = 0);
    Bool acquire (MemoryIO* info, FileLocker::LockType type, uInt nattempts);
    // </group>

    // Release a lock and write the information (if given) into the lock file.
    // The user is responsible for making the information machine-independent
    // (e.g. converting from local to canonical format).
    // <group>
    Bool release();
    Bool release (const MemoryIO& info);
    Bool release (const MemoryIO* info);
    // </group>

    // Inspect if another process wants to access the file (i.e. if the
    // request list is not empty).
    // It only inspects if the time passed since the last inspection
    // exceeds the inspection interval as given in the constructor.
    // If the time passed is too short, False is returned (indicating
    // that no access is needed).
    // If <src>always==True</src>, no test on inspection interval is done,
    // so the inspect is always done.
    Bool inspect (Bool always=False);

    // Test if the file can be locked for read or write.
    Bool canLock (FileLocker::LockType = FileLocker::Write);

    // Test if the process has a lock for read or write on the file.
    Bool hasLock (FileLocker::LockType = FileLocker::Write) const;

    // Get the last error.
    int lastError() const;

    // Get the message belonging to the last error.
    String lastMessage() const;

    // Get the name of the lock file.
    const String& name() const;

    // Get the block of request id's.
    const Block<Int>& reqIds() const;

    // Get the request id's and the info from the lock file.
    void getInfo (MemoryIO& info);

    // Put the info into the file (after the request id's).
    void putInfo (const MemoryIO& info) const;

    // Tell if another process holds a read or write lock on the given file
    // or has the file opened. It returns:
    // <br> 3 if write-locked elsewhere.
    // <br> 2 if read-locked elsewhere.
    // <br> 1 if opened elsewhere.
    // <br> 0 if locked nor opened.
    // <br>It fills in the PID of the process having the file locked or opened.
    // <br>If locked, it also tells if it is permanently locked.
    // <br>An exception is thrown if the file does not exist or cannot
    // be opened.
    static uInt showLock (uInt& pid, Bool& permLocked, const String& fileName);

private:
    // The copy constructor cannot be used (its semantics are too difficult).
    LockFile (const LockFile&);

    // Assignment cannot be used (its semantics are too difficult).
    LockFile& operator= (const LockFile&);

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


    //# The member variables.
    FileLocker   itsLocker;
    FileLocker   itsUseLocker;
    FiledesIO*   itsFileIO;
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
    Int          itsInspectCount;     //# The number of times inspect() has
                                      //# been called since the last elapsed
                                      //# time check.
};


inline Bool LockFile::acquire (FileLocker::LockType type, uInt nattempts)
{
    return acquire (0, type, nattempts);
}
inline Bool LockFile::acquire (MemoryIO& info, FileLocker::LockType type,
			       uInt nattempts)
{
    return acquire (&info, type, nattempts);
}
inline Bool LockFile::release()
{
    return release (0);
}
inline Bool LockFile::release (const MemoryIO& info)
{
    return release (&info);
}
inline Bool LockFile::canLock (FileLocker::LockType type)
{
    return (itsFileIO == 0  ?  True : itsLocker.canLock (type));
}
inline Bool LockFile::hasLock (FileLocker::LockType type) const
{
    return (itsFileIO == 0  ?  True : itsLocker.hasLock (type));
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



} //# NAMESPACE CASACORE - END

#endif
