//# FileLocker.cc: Class to handle file locking
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

#include <casacore/casa/IO/FileLocker.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <casacore/casa/string.h>

//# Locking is not supported on Cray compute nodes.
#if defined(AIPS_CRAY_PGI)  &&  !defined(AIPS_NOFILELOCK)
# define AIPS_NOFILELOCK 1
#endif


namespace casacore { //# NAMESPACE CASACORE - BEGIN

FileLocker::FileLocker()
: itsFD          (-1),
  itsError       (0),
  itsStart       (0),
  itsLength      (0),
  itsMsgShown    (False),
  itsReadLocked  (False),
  itsWriteLocked (False)
{}

FileLocker::FileLocker (int fd, uInt start, uInt length)
: itsFD          (fd),
  itsError       (0),
  itsStart       (start),
  itsLength      (length),
  itsMsgShown    (False),
  itsReadLocked  (False),
  itsWriteLocked (False)
{}

FileLocker::~FileLocker()
{}

Bool FileLocker::acquire (LockType type, uInt nattempts)
{
    itsError = 0;
    // Always success if locking is not supported.
#if defined(AIPS_NOFILELOCK)
    itsReadLocked = True;
    if (!itsWriteLocked  &&  type == Write) {
        itsWriteLocked = True;
    }
    return True;
#else
    struct flock ls;
    ls.l_whence = SEEK_SET;
    ls.l_start  = itsStart;
    ls.l_len    = itsLength;
    ls.l_type   = F_WRLCK;
    // When a read-lock is acquired, it may release an existing write-lock.
    // We do not want that to happen, so when it is write-locked, test
    // if the write-lock is still valid.
    if (type == Read) {
        if (itsWriteLocked) {
  	    if (fcntl (itsFD, F_SETLK, &ls) != -1) {
///	    cout << "kept " << itsReadLocked << ' ' <<itsWriteLocked <<
///	      ' '<<itsStart<<' '<<itsLength<<endl;
	        return True;
	    }
	    itsWriteLocked = False;
	}
	ls.l_type = F_RDLCK;
    }
    if (nattempts == 0) {
	// Wait until lock succeeds.
	if (fcntl (itsFD, F_SETLKW, &ls) != -1) {
	    itsReadLocked = True;
	    if (type == Write) {
		itsWriteLocked = True;
	    }
///	    cout << "acquired " << itsReadLocked << ' ' <<itsWriteLocked <<
///	      ' '<<itsStart<<' '<<itsLength<<endl;
	    return True;
	}
	itsError = errno;
    }
    // Do finite number of attempts. Wait 1 second between each attempt.
    for (uInt i=0; i<nattempts; i++) {
	if (fcntl (itsFD, F_SETLK, &ls) != -1) {
	    itsError = 0;
	    itsReadLocked = True;
	    if (type == Write) {
		itsWriteLocked = True;
	    }
///	    cout << "acquired " << itsReadLocked << ' ' <<itsWriteLocked <<
///	      ' '<<itsStart<<' '<<itsLength<<endl;
	    return True;
	}
	// If locking fails, there is usually something wrong with locking
        // over NFS because the statd or lockd deamons are not running.
	// Hence locks on NFS files result in ENOLCK. Treat it as success.
	// Issue a message if hit for the first time.
#if defined(AIPS_LINUX) || defined(AIPS_DARWIN)
	if (errno == ENOLCK) {
	    itsError = 0;
	    itsReadLocked = True;
	    if (type == Write) {
		itsWriteLocked = True;
	    }
	    if (!itsMsgShown) {
	      itsMsgShown = True;
	      cerr << "*** The ENOLCK error was returned by the kernel." << endl;
              cerr << "*** It usually means that a lock for an NFS file could not be" << endl;
              cerr << "*** obtained, maybe because the statd or lockd daemon is not running." << endl;
	    }
	    return True;
	}
#endif
	itsError = errno;
	if (errno != EAGAIN  &&  errno != EACCES) {
	    i = nattempts;                             // exit the loop
	}
	if (i < nattempts-1) {
	    sleep (1);
	}
    }
    itsWriteLocked = False;
    // Note that the system keeps a lock per file and not per fd.
    // So if the same file is opened in the same process and unlocked
    // at the same place, the read lock for this fd is also released.
    // If we think we hold a read lock, determine if we still hold it.
    // We certainly do not if we asked for a read lock.
    // If asked for a write lock, we might still hold it.
    // One attempt is enough to see if we indeed can get a read lock.
    if (itsReadLocked) {
        itsReadLocked = False;
	if (type == Write) {
	    ls.l_type = F_RDLCK;
	    if (fcntl (itsFD, F_SETLK, &ls) != -1) {
	        itsReadLocked = True;
	    }
	}
    }
///    cout << "failed " << itsReadLocked << ' ' <<itsWriteLocked<<' '<<type<<
///	      ' '<<itsStart<<' '<<itsLength<<endl;
    return False;
#endif
}

// Release a lock.
Bool FileLocker::release()
{
///    cout << "released " << itsReadLocked << ' ' <<itsWriteLocked<<
///	      ' '<<itsStart<<' '<<itsLength<<endl;
    itsReadLocked  = False;
    itsWriteLocked = False;
    itsError = 0;
#if defined(AIPS_NOFILELOCK)
    return True;
#else
    struct flock ls;
    ls.l_type   = F_UNLCK;
    ls.l_whence = SEEK_SET;
    ls.l_start  = itsStart;
    ls.l_len    = itsLength;
    if (fcntl (itsFD, F_SETLK, &ls) != -1) {
	return True;
    }
#if defined(AIPS_LINUX) || defined(AIPS_DARWIN)
    if (errno == ENOLCK) {
      return True;
    }
#endif
    itsError = errno;
    return False;
#endif
}

Bool FileLocker::canLock (LockType type)
{
#if defined(AIPS_NOFILELOCK)
    return True;
#else
    uInt pid;
    return canLock (pid, type);
#endif
}

Bool FileLocker::canLock (uInt& pid, LockType type)
{
#if defined(AIPS_NOFILELOCK)
    return True;
#else
    pid = 0;
    itsError = 0;
    struct flock ls;
    if (type == Write) {
	ls.l_type = F_WRLCK;
    }else{
	ls.l_type = F_RDLCK;
    }
    ls.l_whence = SEEK_SET;
    ls.l_start  = itsStart;
    ls.l_len    = itsLength;
    if (fcntl (itsFD, F_GETLK, &ls) != -1) {
        pid = ls.l_pid;
	return  (ls.l_type == F_UNLCK);
    }
    itsError = errno;
    return False;
#endif
}

String FileLocker::lastMessage() const
{
    if (itsError == 0) {
	return "";
    }
    return strerror(itsError);
}

} //# NAMESPACE CASACORE - END

