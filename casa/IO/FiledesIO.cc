//# FiledesIO.cc: Class for IO on a  file descriptor
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

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/LargeIOFuncDef.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>                     // needed for errno
#include <casacore/casa/string.h>               // needed for strerror


namespace casacore { //# NAMESPACE CASACORE - BEGIN

FiledesIO::FiledesIO()
: itsSeekable (False),
  itsReadable (False),
  itsWritable (False),
  itsFile     (-1)
{}

FiledesIO::FiledesIO (int fd, const String& fileName)
: itsFile  (-1)
{
  attach (fd, fileName);
}

FiledesIO::~FiledesIO()
{
    detach();
}


void FiledesIO::attach (int fd, const String& fileName)
{
    AlwaysAssert (itsFile == -1, AipsError);
    itsFile     = fd;
    itsFileName = fileName;
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


void FiledesIO::write (Int64 size, const void* buf)
{
    // Throw an exception if not writable.
    if (!itsWritable) {
	throw AipsError ("FiledesIO " + itsFileName
                         + "is not writable");
    }
    if (::traceWRITE(itsFile, (Char *)buf, size) != size) {
        int error = errno;
	throw AipsError ("FiledesIO: write error in "
                         + itsFileName + ": " + strerror(error));
    }
}

Int64 FiledesIO::read (Int64 size, void* buf, Bool throwException)
{
  // Throw an exception if not readable.
  if (!itsReadable) {
    throw AipsError ("FiledesIO::read " + itsFileName
                     + " - is not readable");
  }
  Int64 bytesRead = ::traceREAD (itsFile, (Char *)buf, size);
  int error = errno;
  if (bytesRead > size) { // Should never be executed
    throw AipsError ("FiledesIO::read " + itsFileName
                     + " - read returned a bad value");
  }
  if (bytesRead != size  &&  throwException == True) {
    if (bytesRead < 0) {
      throw AipsError ("FiledesIO::read " + itsFileName +
                       " - error returned by system call: " + 
                       strerror(error));
    } else if (bytesRead < size) {
      throw AipsError ("FiledesIO::read - incorrect number of bytes ("
		       + String::toString(bytesRead) + " out of "
                       + String::toString(size) + ") read for file "
                       + itsFileName);
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


String FiledesIO::fileName() const
{
    return itsFileName;
}

void FiledesIO::fsync()
{
    ::fsync (itsFile);
}

int FiledesIO::create (const Char* name, int mode)
{
    int fd = ::trace3OPEN ((Char *)name, O_RDWR | O_CREAT | O_TRUNC, mode);
    int error = errno;
    if (fd == -1) {
      throw AipsError ("FiledesIO: file " + String(name) +
                       " could not be created: " + strerror(error));
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
    int error = errno;
    if (throwExcp  &&  fd == -1) {
	throw AipsError ("FiledesIO: file " + String(name) +
                         " could not be opened: " + strerror(error));
    }
    return fd;
}

void FiledesIO::close (int fd)
{
  if (fd >= 0) {
    if (::traceCLOSE (fd)  == -1) {
      int error = errno;
      throw AipsError (String("FiledesIO: file could not be closed: ")
                       + strerror(error));
    }
  }
}

} //# NAMESPACE CASACORE - END
