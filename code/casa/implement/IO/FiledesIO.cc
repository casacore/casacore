//# FiledesIO.cc: Class for IO on a file descriptor
//# Copyright (C) 1997,1999,2001
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

#include <casa/aips.h>
#include <casa/IO/FiledesIO.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>                // needed for errno
#include <casa/string.h>               // needed for strerror

#ifdef PABLO_IO
#include "IOTrace.h"
#else
#define traceFCLOSE fclose
#define traceFSEEK fseek
#define traceFREAD fread
#define traceFWRITE fwrite
#define traceREAD read
#define traceWRITE write
#define trace2OPEN open
#define traceLSEEK lseek
#define trace3OPEN open
#define traceCLOSE close
#endif // PABLO_IO

namespace casa { //# NAMESPACE CASA - BEGIN

FiledesIO::FiledesIO()
: itsSeekable (False),
  itsReadable (False),
  itsWritable (False),
  itsFile     (-1)
{}

FiledesIO::FiledesIO (int fd)
: itsFile  (-1)
{
    attach (fd);
}

FiledesIO::~FiledesIO()
{
    detach();
}


void FiledesIO::attach (int fd)
{
    AlwaysAssert (itsFile == -1, AipsError);
    itsFile = fd;
    fillRWFlags (fd);
    fillSeekable();
}

void FiledesIO::detach()
{
    itsFile = -1;
}

void FiledesIO::fillRWFlags (int fd)
{
    itsReadable = False;
    itsWritable = False;
    int flags = fcntl (fd, F_GETFL);
    if ((flags & O_RDWR)  ==  O_RDWR) {
	itsReadable = True;
	itsWritable = True;
    } else if ((flags & O_WRONLY)  ==  O_WRONLY) {
	itsWritable = True;
    } else {
	itsReadable = True;
    }
}

void FiledesIO::fillSeekable()
{
    itsSeekable = (seek (0, ByteIO::Current)  >= 0);
}


String FiledesIO::fileName() const
{
    return "";
}


void FiledesIO::write (uInt size, const void* buf)
{
    // Throw an exception if not writable.
    if (!itsWritable) {
	throw (AipsError ("FiledesIO object is not writable"));
    }
    if (::traceWRITE(itsFile, (Char *)buf, size) != Int(size)) {
	throw (AipsError (String("FiledesIO: write error: ")
			  + strerror(errno)));
    }
}

Int FiledesIO::read (uInt size, void* buf, Bool throwException)
{
  // Throw an exception if not readable.
  if (!itsReadable) {
    throw (AipsError ("FiledesIO::read - descriptor is not readable"));
  }
  Int bytesRead = ::traceREAD (itsFile, (Char *)buf, size);
  if (bytesRead > Int(size)) { // Should never be executed
    throw (AipsError ("FiledesIO::read - read returned a bad value"));
  }
  if (bytesRead != Int(size) && throwException == True) {
    if (bytesRead < 0) {
      throw (AipsError (String("FiledesIO::read - "
			       " error returned by system call: ") + 
			strerror(errno)));
    } else if (bytesRead < Int(size)) {
      throw (AipsError ("FiledesIO::read - incorrect number of bytes read"));
    }
  }
  return bytesRead;
}

Int64 FiledesIO::doSeek (Int64 offset, ByteIO::SeekOption dir)
{
    switch (dir) {
    case ByteIO::Begin:
	return ::traceLSEEK (itsFile, offset, SEEK_SET);
    case ByteIO::End:
        return ::traceLSEEK (itsFile, offset, SEEK_END);
    default:
	break;
    }
    return ::traceLSEEK (itsFile, offset, SEEK_CUR);
}

Int64 FiledesIO::length()
{
    // Get current position to be able to reposition.
    Int64 pos = seek (0, ByteIO::Current);
    // Seek to the end of the stream.
    // If it fails, we cannot seek and the current position is the length.
    Int64 len = seek (0, ByteIO::End);
    if (len < 0) {
	return pos;
    }
    // Reposition and return the length.
    seek (pos, ByteIO::Begin);
    return len;
}

   
Bool FiledesIO::isReadable() const
{
    return itsReadable;
}

Bool FiledesIO::isWritable() const
{
    return itsWritable;
}

Bool FiledesIO::isSeekable() const
{
    return itsSeekable;
}


int FiledesIO::create (const Char* name, int mode)
{
    int fd = ::trace3OPEN ((Char *)name, O_RDWR | O_CREAT | O_TRUNC, mode);
    if (fd == -1) {
	throw (AipsError ("FiledesIO: file " + String(name) +
			  " could not be created: " + strerror(errno)));
    }
    return fd;
}
int FiledesIO::open (const Char* name, Bool writable, Bool throwExcp)
{
    int fd;
    if (writable) {
	fd = ::trace2OPEN ((Char *)name, O_RDWR);
    }else{
	fd = ::trace2OPEN ((Char *)name, O_RDONLY);
    }
    if (throwExcp  &&  fd == -1) {
	throw (AipsError ("FiledesIO: file " + String(name) +
			  " could not be opened: " + strerror(errno)));
    }
    return fd;
}
void FiledesIO::close (int fd)
{
    if (::traceCLOSE (fd)  == -1) {
	throw (AipsError (String("FiledesIO: file could not be closed: ")
			  + strerror(errno)));
    }
}

} //# NAMESPACE CASA - END

