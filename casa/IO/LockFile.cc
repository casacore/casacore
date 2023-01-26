//# LockFile.cc: Class to handle file locking
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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


#include <casacore/casa/IO/LockFile.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

// PABLO_IO is used to profile the IO performance of the Casacore (in
// particular to help us locate bottlenecks associated with parallel
// processing)
//
// Please see http://www-pablo.cs.uiuc.edu if you need more details
// You'll need to set PABLO_IO variable in the makedefs as well as the
// -I flag to find the header file and -L to resolve with the library.
//
#ifdef PABLO_IO
#include "IOTrace.h"
#else
#define traceFCLOSE fclose
#define traceFSEEK fseek
#define traceFREAD fread
#define traceFWRITE fwrite
#define traceWRITE write
#define tracePWRITE pwrite
#define tracePREAD pread
#define trace2OPEN open
#define trace3OPEN open
#define traceLSEEK lseek
#define traceCLOSE close
#endif // PABLO_IO

//# canonical size of an int32_t (checked in constructor).
#define SIZEINT 4u
#define NRREQID 32u
#define SIZEREQID ((1 + 2*NRREQID) * SIZEINT)

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LockFile::LockFile (const String& fileName, double inspectInterval,
		    bool create, bool setRequestFlag, bool mustExist,
		    uint32_t seqnr, bool permLocking, bool noLocking)
: itsFileIO      (0),
  itsCanIO       (0),
  itsWritable    (true),
  itsAddToList   (setRequestFlag),
  itsInterval    (inspectInterval),
  itsPid         (getpid()),
///  itsHostId    (gethostid()),     gethostid is not declared in unistd.h
  itsHostId      (0),
  itsReqId       (SIZEREQID/SIZEINT, (int32_t)0),
  itsInspectCount(0)
{
    AlwaysAssert (SIZEINT == CanonicalConversion::canonicalSize (static_cast<int32_t*>(0)),
		  AipsError);
    itsName = Path(fileName).absoluteName();
    //# If needed, create the file if it does not exist yet.
    //# If the flag is set, it is allowed that the file does not
    //# exist and cannot be created. In that case it is assumed that
    //# later on each locking request is successful (without doing actual
    //# locking).
    if (!noLocking  &&  !create) {
      File f (itsName);
      if (! f.exists()) {
        if (!f.canCreate()  &&  !mustExist) {
          return;    // Acceptable that lock file does not exist
        }
        create = true;
      }
    }
    //# Open the lock file as read/write if it exists.
    //# If it did not succeed, open as readonly.
    //# For noLocking, it does not need to exist.
    int fd = -1;
    if (!create) {
      fd = FiledesIO::open (itsName.chars(), true, false);
      if (fd == -1) {
        fd = FiledesIO::open (itsName.chars(), false, !noLocking);
        itsWritable  = false;
        itsAddToList = false;
      }
    } else if (!noLocking) {
      //# Create a new file with world write access.
      //# Initialize the values in it.
      fd = FiledesIO::create (itsName.chars(), 0666);
      putReqId (fd);
    }
    if (fd >= 0) {
      //# Create FileLocker objects for this lock file.
      //# The first one is for read/write locks.
      //# The second one is to set the file to "in use" and to tell if
      //# permanent locking is used.
      itsLocker = FileLocker (fd, 4*seqnr, 1);
      if (permLocking) {
        itsUseLocker = FileLocker (fd, 4*seqnr+1, 2);
      } else {
        itsUseLocker = FileLocker (fd, 4*seqnr+1, 1);
      }
      if (!noLocking) {
        itsFileIO = new FiledesIO (fd, itsName);
        itsCanIO  = new CanonicalIO (itsFileIO);
        // Set the file to in use by acquiring a read lock.
        itsUseLocker.acquire (FileLocker::Read, 1);
      }
    }
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

bool LockFile::isMultiUsed()
{
    //# If a write lock cannot be obtained, the file is in use.
    return ( (itsUseLocker.fd() >= 0
                &&  !itsUseLocker.canLock (FileLocker::Write)));
}

bool LockFile::acquire (MemoryIO* info, FileLocker::LockType type,
			uint32_t nattempts)
{
    //# When no lock file, lock requests always succeed,
    //# but we cannot return any info.
    if (itsFileIO == 0) {
	if (info != 0) {
	    info->clear();
	}
	return true;
    }
    //# Try to set a lock without waiting.
    bool succ = itsLocker.acquire (type, 1);
    bool added = false;
    //# When unsuccessful and multiple attempts have to be done,
    //# add the process to the request list (if needed) and try to acquire.
    if (!succ  &&  nattempts != 1) {
	if (itsAddToList) {
	    addReqId();
	    added = true;
	}
	succ = itsLocker.acquire (type, nattempts);
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
    itsInspectCount = 0;
    return succ;
}

bool LockFile::release (const MemoryIO* info)
{
    //# When no lock file, lock requests are not really handled.
    if (itsFileIO == 0) {
	return true;
    }
    if (info != 0) {
	putInfo (*info);
    }
    return itsLocker.release();
}

bool LockFile::inspect (bool always)
{
    //# When no lock file, lock requests are not really handled.
    if (itsFileIO == 0) {
	return false;
    }

    if (!always) {
      //# Only check elapsed time every n-th request (where n=25 at present),
      //# as the elapsed time calculation is computationally expensive
      if (itsInterval > 0 && itsInspectCount++ < 25) {
	return false;
      }
      itsInspectCount = 0;

      //# Only inspect if time interval has passed.
      if (itsInterval > 0  &&  itsLastTime.age() < itsInterval) {
	return false;
      }
    }

    //# Get the number of request id's and reset the time.
    uint32_t nr = getNrReqId();
    itsLastTime.now();
    return  (nr > 0);
}

void LockFile::getInfo (MemoryIO& info)
{
    // Do nothing if no locking.
    if (itsLocker.fd() < 0) {
        return;
    }
    // The lock file contains:
    // - the fixed length request list in the first bytes
    // - thereafter the length of the info (as a uint32_t)
    // - thereafter the entire info
    unsigned char buffer[2048];
    // Read the first part of the file.
    traceLSEEK (itsLocker.fd(), 0, SEEK_SET);
    uint32_t leng = ::read (itsLocker.fd(), buffer, sizeof(buffer));
    // Extract the request list from it.
    convReqId (buffer, leng);
    // Get the length of the info.
    uint32_t infoLeng = getInt (buffer, leng, SIZEREQID);
    // Clear the MemoryIO object.
    info.clear();
    if (infoLeng == 0) {
	return;
    }
    // Get the length of the info in the part already read.
    // (subtract length of request list and the info-length uint32_t)
    leng -= SIZEREQID+SIZEINT;
    if (leng > infoLeng) {
	leng = infoLeng;
    }
    info.seek (int64_t(0));
    info.write (leng, buffer + SIZEREQID + SIZEINT);
    // Read the remaining info parts.
    if (infoLeng > leng) {
	infoLeng -= leng;
	unsigned char* buf = new unsigned char[infoLeng];
	AlwaysAssert (::read (itsLocker.fd(), buf, infoLeng) == int32_t(infoLeng),
                      AipsError);
	info.write (infoLeng, buf);
	delete [] buf;
    }
    info.seek (int64_t(0));
}

void LockFile::putInfo (const MemoryIO& info) const
{
    uint32_t infoLeng = const_cast<MemoryIO&>(info).length();
    if (itsLocker.fd() < 0  ||  !itsWritable  ||  infoLeng == 0) {
	return;
    }
    // Write the info into the lock file preceeded by its length.
    unsigned char buffer[1024];
    uint32_t leng = CanonicalConversion::fromLocal (buffer, infoLeng);
    if (infoLeng > 1024 - leng) {
      // Too large for the buffer, so write length and info separately.
      traceLSEEK (itsLocker.fd(), SIZEREQID, SEEK_SET);
      AlwaysAssert (traceWRITE (itsLocker.fd(), (char *)buffer, leng) ==
                    int32_t(leng), AipsError);
      AlwaysAssert (traceWRITE (itsLocker.fd(), (char *)info.getBuffer(),
                                infoLeng) == int32_t(infoLeng), AipsError);
    }else{
      // Info fits in the buffer, so copy and do a single write.
      memcpy (buffer+leng, info.getBuffer(), infoLeng);
      AlwaysAssert (tracePWRITE (itsLocker.fd(), (char *)buffer, leng+infoLeng,
                                 SIZEREQID) == int32_t(leng+infoLeng), AipsError);
    }
    // Do an fsync to achieve NFS synchronization.
    fsync (itsLocker.fd());
}

int32_t LockFile::getNrReqId() const
{
    unsigned char buffer[8];
    uint32_t leng = tracePREAD (itsLocker.fd(), buffer, SIZEINT, 0);
    return getInt (buffer, leng, 0);
}

int32_t LockFile::getInt (const unsigned char* buffer, uint32_t leng, uint32_t offset) const
{
    if (leng < offset + SIZEINT) {
	return 0;
    }
    int32_t value;
    CanonicalConversion::toLocal (value, buffer+offset);
    return value;
}

void LockFile::convReqId (const unsigned char* buffer, uint32_t leng)
{
    if (leng >= SIZEREQID) {
	CanonicalConversion::toLocal (itsReqId.storage(), buffer,
				      SIZEREQID/SIZEINT);
    }
}

void LockFile::addReqId()
{
    //# Add the id at the last free place in the block.
    //# If full, use last element. This is better than ignoring it,
    //# because in this way the last request is always known.
    uint32_t inx = itsReqId[0];
    if (inx >= NRREQID) {
	inx = NRREQID-1;
    }
    itsReqId[0] = inx+1;
    itsReqId[2*inx+1] = itsPid;
    itsReqId[2*inx+2] = itsHostId;
    putReqId (itsLocker.fd());
}

void LockFile::removeReqId()
{
    int32_t i;
    //# Remove the id and all previous id's from the block.
    //# In principle previous id's should not occur, but it
    //# can happen when a process with an outstanding request died.
    int32_t nr = itsReqId[0];
    for (i=0; i<nr; i++) {
	if (int32_t(itsPid) == itsReqId[2*i+1]
        &&  int32_t(itsHostId) == itsReqId[2*i+2]) {
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
	unsigned char buffer[SIZEREQID];
	uint32_t leng = CanonicalConversion::fromLocal (buffer,
						    itsReqId.storage(),
						    itsReqId.nelements());
        AlwaysAssert(tracePWRITE(fd, (char *)buffer, leng, 0) == int32_t(leng),
                     AipsError);
	fsync (fd);
    }
}

void LockFile::getReqId()
{
    int fd = itsLocker.fd();
    unsigned char buffer[SIZEREQID];
    if (tracePREAD(fd, buffer, SIZEREQID, 0) > 0) {
	CanonicalConversion::fromLocal (buffer,
					itsReqId.storage(),
					itsReqId.nelements());
    }
}


uint32_t LockFile::showLock (uint32_t& pid, bool& permLocked, const String& fileName)
{
    pid = 0;
    permLocked = false;
    String fullName = Path(fileName).absoluteName();
    File f (fullName);
    if (! f.exists()) {
        throw AipsError ("LockFile::showLock - File " + fileName +
			 " does not exist");
    }
    //# Open the lock file as readonly.
    int fd = FiledesIO::open (fullName.chars(), false);
    if (fd == -1) {
        throw AipsError ("LockFile::showLock - File " + fileName +
			 " could not be opened");
    }
    // The first byte is for read/write locking.
    // The second byte is to see if the file is used in another process.
    // The third one is to see if the file is permanently locked.
    FileLocker fileLocker (fd, 0, 1);
    FileLocker useLocker  (fd, 1, 1);
    FileLocker permLocker (fd, 2, 1);
    // Determine if the file is opened in another process.
    // If not, we can exit immediately.
    uint32_t usePid;
    if (useLocker.canLock (usePid, FileLocker::Write)) {
        return 0;
    }
    uint32_t result;
    // If we cannot readlock, the file is writelocked elsewhere.
    // If we cannot writelock, the file is readlocked elsewhere.
    // Otherwise the file is simply in use.
    if (! fileLocker.canLock (pid, FileLocker::Read)) {
        result = 3;
    } else if (! fileLocker.canLock (pid, FileLocker::Write)) {
        result = 2;
    } else {
        pid = usePid;
        return 1;
    }
    if (! permLocker.canLock (usePid, FileLocker::Write)) {
        permLocked = true;
    }
    return result;
}

} //# NAMESPACE CASACORE - END
