//# LargeFiledesIO.cc: Class for IO on a large file descriptor
//# Copyright (C) 2001,2002
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
#include <casa/IO/LargeIOFuncDef.h>
#include <casa/IO/LargeFiledesIO.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>                // needed for errno
#include <casa/string.h>               // needed for strerror


namespace casa { //# NAMESPACE CASA - BEGIN

LargeFiledesIO::LargeFiledesIO()
: itsSeekable (False),
  itsReadable (False),
  itsWritable (False),
  itsFile     (-1)
{}

LargeFiledesIO::LargeFiledesIO (int fd)
: itsFile  (-1)
{
    attach (fd);
}

LargeFiledesIO::~LargeFiledesIO()
{
    detach();
}


void LargeFiledesIO::attach (int fd)
{
    AlwaysAssert (itsFile == -1, AipsError);
    itsFile = fd;
    fillRWFlags (fd);
    fillSeekable();
}

void LargeFiledesIO::detach()
{
    itsFile = -1;
}

void LargeFiledesIO::fillRWFlags (int fd)
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

void LargeFiledesIO::fillSeekable()
{
    itsSeekable = (seek (0, ByteIO::Current)  >= 0);
}


String LargeFiledesIO::fileName() const
{
    return "";
}


void LargeFiledesIO::write (uInt size, const void* buf)
{
    // Throw an exception if not writable.
    if (!itsWritable) {
	throw (AipsError ("LargeFiledesIO object is not writable"));
    }
    if (::traceWRITE(itsFile, (Char *)buf, size) != Int(size)) {
	throw (AipsError (String("LargeFiledesIO: write error: ")
			  + strerror(errno)));
    }
}

Int LargeFiledesIO::read (uInt size, void* buf, Bool throwException)
{
  // Throw an exception if not readable.
  if (!itsReadable) {
    throw (AipsError ("LargeFiledesIO::read - descriptor is not readable"));
  }
  Int bytesRead = ::traceREAD (itsFile, (Char *)buf, size);
  if (bytesRead > Int(size)) { // Should never be executed
    throw (AipsError ("LargeFiledesIO::read - read returned a bad value"));
  }
  if (bytesRead != Int(size) && throwException == True) {
    if (bytesRead < 0) {
      throw (AipsError (String("LargeFiledesIO::read - "
			       " error returned by system call: ") + 
			strerror(errno)));
    } else if (bytesRead < Int(size)) {
      throw (AipsError ("LargeFiledesIO::read - "
			"incorrect number of bytes read"));
    }
  }
  return bytesRead;
}

Int64 LargeFiledesIO::doSeek (Int64 offset, ByteIO::SeekOption dir)
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

Int64 LargeFiledesIO::length()
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

   
Bool LargeFiledesIO::isReadable() const
{
    return itsReadable;
}

Bool LargeFiledesIO::isWritable() const
{
    return itsWritable;
}

Bool LargeFiledesIO::isSeekable() const
{
    return itsSeekable;
}


int LargeFiledesIO::create (const Char* name, int mode)
{
    int fd = ::trace3OPEN ((Char *)name, O_RDWR | O_CREAT | O_TRUNC, mode);
    if (fd == -1) {
	throw (AipsError ("LargeFiledesIO: file " + String(name) +
			  " could not be created: " + strerror(errno)));
    }
    return fd;
}
int LargeFiledesIO::open (const Char* name, Bool writable, Bool throwExcp)
{
    int fd;
    if (writable) {
	fd = ::trace2OPEN ((Char *)name, O_RDWR);
    }else{
	fd = ::trace2OPEN ((Char *)name, O_RDONLY);
    }
    if (throwExcp  &&  fd == -1) {
	throw (AipsError ("LargeFiledesIO: file " + String(name) +
			  " could not be opened: " + strerror(errno)));
    }
    return fd;
}
void LargeFiledesIO::close (int fd)
{
    if (::traceCLOSE (fd)  == -1) {
	throw (AipsError (String("LargeFiledesIO: file could not be closed: ")
			  + strerror(errno)));
    }
}

} //# NAMESPACE CASA - END

