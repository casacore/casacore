//# LockFile.cc: Class to handle file locking
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


#include <aips/IO/LockFile.h>
#include <aips/IO/FiledesIO.h>
#include <aips/IO/MemoryIO.h>
#include <aips/IO/CanonicalIO.h>
#include <aips/OS/RegularFile.h>
#include <aips/OS/Path.h>
#include <aips/OS/CanonicalConversion.h>
#include <aips/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>
#include <iostream.h>
#include <strstream.h>


static const uInt sizeInt   = CanonicalConversion::canonicalSize ((Int*)0);
static const uInt nrReqId   = 32;
// The ntv compiler cannot handle the expression below in a statement
// like:   uChar buffer[sizeReqId];
// Therefore the fixed value 4 is used instead.
//static const uInt sizeReqId = (1 + 2*nrReqId) * sizeInt;
static const uInt sizeReqId = (1 + 2*nrReqId) * 4;


LockFile::LockFile (const String& fileName, double inspectInterval,
		    Bool create, Bool setRequestFlag, Bool mustExist)
: itsFileIO    (0),
  itsCanIO     (0),
  itsWritable  (True),
  itsAddToList (setRequestFlag),
  itsInterval  (inspectInterval),
  itsPid       (getpid()),
//  itsHostId    (gethostid()),     gethostid is not declared in unistd.h
  itsHostId    (0),
  itsReqId     (1 + nrReqId*2, (Int)0)
{
    itsName = Path(fileName).expandedName();
    //# Create the file if it does not exist yet.
    //# When the flag is set, it is allowed that the file does not
    //# exist and cannot be created. In that case it is assumed that
    //# later on each locking request is successful (without doing actual
    //# locking).
    if (! create) {
	File f (itsName);
	if (! f.exists()) {
	    if (!f.canCreate()  &&  !mustExist) {
		return;
	    }
	    create = True;
	}
    }
    //# Open the lock file as read/write if it exists.
    //# If it did not succeed, open as readonly.
    int fd;
    if (!create) {
	fd = FiledesIO::open (itsName, True, False);
	if (fd == -1) {
	    fd = FiledesIO::open (itsName, False);
	    itsWritable  = False;
	    itsAddToList = False;
	}
    }else{
	//# Create a new file with world write access.
	//# Initialize the values in it.
	fd = FiledesIO::create (itsName, 0666);
	putReqId (fd);
    }
    //# Create FileLocker objects for this lock file.
    //# The first one is for read/write locks.
    //# The second one is to set the file to "in use".
    itsLocker    = FileLocker (fd, 0, 1);
    itsUseLocker = FileLocker (fd, 1, 1);
    itsFileIO = new FiledesIO (fd);
    itsCanIO  = new CanonicalIO (itsFileIO);
    // Set the file to in use by acquiring a read lock.
    itsUseLocker.acquire (False, 1);
}

LockFile::~LockFile()
{
    delete itsCanIO;
    delete itsFileIO;
    int fd = itsLocker.fd();
    if (fd >= 0) {
	FiledesIO::close (fd);
    }
}

Bool LockFile::isMultiUsed()
{
    //# If a write lock cannot be obtained, the file is in use.
    return (ToBool (itsUseLocker.fd() >= 0  &&  !itsUseLocker.canLock (True)));
}

Bool LockFile::doAcquire (MemoryIO* info, Bool write, uInt nattempts)
{
    //# When no lock file, lock requests always succeed,
    //# but we cannot return any info.
    if (itsFileIO == 0) {
	if (info != 0) {
	    info->clear();
	}
	return True;
    }
    //# Try to set a lock without waiting.
    Bool succ = itsLocker.acquire (write, 1);
    Bool added = False;
    //# When unsuccessful and multiple attempts have to be done,
    //# add the process to the request list (if needed) and try to acquire.
    if (!succ  &&  nattempts != 1) {
	if (itsAddToList) {
	    addReqId();
	    added = True;
	}
	succ = itsLocker.acquire (write, nattempts);
    }
    //# Do not read info if we did not acquire the lock.
    if (!succ) {
	info = 0;
    }
    //# Read the info when needed.
    //# This also reads the request id's.
    //# If no info is needed, read req id's only when needed.
    //# Note that each IO-operation is quite expensive, so do as few
    //# IO's as possible.
    if (info != 0) {
	getInfo (*info);
    } else if (added) {
	getReqId();
    }
    //# Remove this id from the request list (if added).
    if (added) {
	removeReqId();
    }
    //# Reset the inspection time.
    itsLastTime.now();
    return succ;
}

Bool LockFile::doRelease (const MemoryIO* info)
{
    //# When no lock file, lock requests are not really handled.
    if (itsFileIO == 0) {
	return True;
    }
    if (info != 0) {
	putInfo (*info);
    }
    return itsLocker.release();
}

Bool LockFile::inspect()
{
    //# When no lock file, lock requests are not really handled.
    if (itsFileIO == 0) {
	return False;
    }
    //# Only inspect if time interval has passed.
    if (itsInterval > 0  &&  itsLastTime.age() < itsInterval) {
	return False;
    }
    //# Get the number of request id's and reset the time.
    uInt nr = getNrReqId();
    itsLastTime.now();
    return ToBool (nr > 0);
}

void LockFile::getInfo (MemoryIO& info)
{
    uChar buffer[2048];
    lseek (itsLocker.fd(), 0, SEEK_SET);
    uInt leng = read (itsLocker.fd(), buffer, sizeof(buffer));
    convReqId (buffer, leng);
    uInt infoLeng = getInt (buffer, leng, sizeReqId);
    info.clear();
    if (infoLeng == 0) {
	return;
    }
    leng -= sizeReqId+sizeInt;
    if (leng > infoLeng) {
	leng = infoLeng;
    }
    info.seek (0);
    info.write (leng, buffer + sizeReqId + sizeInt);
    if (infoLeng > leng) {
	infoLeng -= leng;
	uChar* buf = new uChar[infoLeng];
	read (itsLocker.fd(), buf, infoLeng);
	info.write (infoLeng, buf);
	delete [] buf;
    }
    info.seek (0);
}

void LockFile::putInfo (const MemoryIO& info) const
{
    uInt infoLeng = ((MemoryIO&)info).length();
    if (!itsWritable  ||  infoLeng == 0) {
	return;
    }
    uChar buffer[1024];
    uInt leng = CanonicalConversion::fromLocal (buffer, infoLeng);
    lseek (itsLocker.fd(), sizeReqId, SEEK_SET);
    if (infoLeng > 1024 - leng) {
	write (itsLocker.fd(), buffer, leng);
	write (itsLocker.fd(), info.getBuffer(), infoLeng);
    }else{
	memcpy (buffer+leng, info.getBuffer(), infoLeng);
	write (itsLocker.fd(), buffer, leng+infoLeng);
    }
    fsync (itsLocker.fd());
}

Int LockFile::getNrReqId() const
{
    uChar buffer[8];
    lseek (itsLocker.fd(), 0, SEEK_SET);
    uInt leng = read (itsLocker.fd(), buffer, sizeInt);
    return getInt (buffer, leng, 0);
}

Int LockFile::getInt (const uChar* buffer, uInt leng, uInt offset) const
{
    if (leng < offset + sizeInt) {
	return 0;
    }
    Int value;
    CanonicalConversion::toLocal (value, buffer+offset);
    return value;
}

void LockFile::convReqId (const uChar* buffer, uInt leng)
{
    if (leng >= sizeReqId) {
	CanonicalConversion::toLocal (itsReqId.storage(), buffer,
				      sizeReqId/sizeInt);
    }
}

void LockFile::addReqId()
{
    //# Add the id at the last free place in the block.
    //# If full, use last element. This is better than ignoring it,
    //# because in this way the last request is always known.
    uInt inx = itsReqId[0];
    if (inx == itsReqId.nelements()) {
	inx--;
    }else{
	itsReqId[0]++;
    }
    itsReqId[2*inx+1] = itsPid;
    itsReqId[2*inx+2] = itsHostId;
    putReqId (itsLocker.fd());
}

void LockFile::removeReqId()
{
    Int i;
    //# Remove the id and all previous id's from the block.
    //# In principle previous id's should not occur, but it
    //# can happen when a process with an outstanding request died.
    Int nr = itsReqId[0];
    for (i=0; i<nr; i++) {
	if (Int(itsPid) == itsReqId[2*i+1]
        &&  Int(itsHostId) == itsReqId[2*i+2]) {
	    break;
	}
    }
    if (i < nr) {
	nr -= i+1;
	objcopy (&itsReqId[1], &itsReqId[2*i+1], 2*nr);
	itsReqId[0] = nr;
	putReqId (itsLocker.fd());
    }
}

void LockFile::putReqId (int fd) const
{
    if (itsAddToList) {
	uChar buffer[sizeReqId];
	uInt leng = CanonicalConversion::fromLocal (buffer,
						    itsReqId.storage(),
						    itsReqId.nelements());
	lseek (fd, 0, SEEK_SET);
	write (fd, buffer, leng);
	fsync (fd);
    }
}

void LockFile::getReqId()
{
    int fd = itsLocker.fd();
    uChar buffer[sizeReqId];
    lseek (fd, 0, SEEK_SET);
    if (read (fd, buffer, sizeReqId) > 0) {
	CanonicalConversion::fromLocal (buffer,
					itsReqId.storage(),
					itsReqId.nelements());
    }
}
