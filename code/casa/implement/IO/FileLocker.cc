//# FileLocker.cc: Class to handle file locking
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

#include <aips/IO/FileLocker.h>
#include <aips/Utilities/String.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>


FileLocker::FileLocker()
: itsFD          (-1),
  itsError       (0),
  itsStart       (0),
  itsLength      (0),
  itsReadLocked  (False),
  itsWriteLocked (False)
{}

FileLocker::FileLocker (int fd, uInt start, uInt length)
: itsFD          (fd),
  itsError       (0),
  itsStart       (start),
  itsLength      (length),
  itsReadLocked  (False),
  itsWriteLocked (False)
{}

FileLocker::~FileLocker()
{}

Bool FileLocker::acquire (LockType type, uInt nattempts)
{
    itsReadLocked  = False;
    itsWriteLocked = False;
    itsError = 0;
    flock ls;
    if (type == Write) {
	ls.l_type = F_WRLCK;
    }else{
	ls.l_type = F_RDLCK;
    }
    ls.l_whence = SEEK_SET;
    ls.l_start  = itsStart;
    ls.l_len    = itsLength;
    if (nattempts == 0) {
	// Wait until lock succeeds.
	if (fcntl (itsFD, F_SETLKW, &ls) == -1) {
	    itsError = errno;
	    return False;
	}
	itsReadLocked = True;
	if (write) {
	    itsWriteLocked = True;
	}
	return True;
    }
    // Do finite number of attempts. Wait 1 second between each attempt.
    for (uInt i=0; i<nattempts; i++) {
	if (fcntl (itsFD, F_SETLK, &ls) != -1) {
	    itsReadLocked = True;
	    if (type == Write) {
		itsWriteLocked = True;
	    }
	    return True;
	}
	itsError = errno;
	if (errno != EAGAIN  &&  errno != EACCES) {
	    return False;               // other error than already locked
	}
	if (i < nattempts-1) {
	    sleep (1);
	}
    }
    return False;
}

// Release a lock.
Bool FileLocker::release()
{
    itsReadLocked  = False;
    itsWriteLocked = False;
    itsError = 0;
    flock ls;
    ls.l_type   = F_UNLCK;
    ls.l_whence = SEEK_SET;
    ls.l_start  = itsStart;
    ls.l_len    = itsLength;
    if (fcntl (itsFD, F_SETLK, &ls) != -1) {
	return True;
    }
    itsError = errno;
    return False;
}

Bool FileLocker::canLock (LockType type)
{
    itsError = 0;
    flock ls;
    if (type == Write) {
	ls.l_type = F_WRLCK;
    }else{
	ls.l_type = F_RDLCK;
    }
    ls.l_whence = SEEK_SET;
    ls.l_start  = itsStart;
    ls.l_len    = itsLength;
    if (fcntl (itsFD, F_GETLK, &ls) != -1) {
	return ToBool (ls.l_type == F_UNLCK);
    }
    itsError = errno;
    return False;
}

String FileLocker::lastMessage() const
{
    if (itsError == 0) {
	return "";
    }
    return strerror(itsError);
}
