//# LargeFilebufIO.cc: Class for IO on a large file using a filebuf object.
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#include <aips/aips.h>
#include <aips/IO/LargeFilebufIO.h>
#include <aips/IO/LargeIOFuncDef.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <fcntl.h>
#include <errno.h>                // needed for errno
#include <string.h>               // needed for strerror


LargeFilebufIO::LargeFilebufIO()
: itsOwner     (False),
  itsSeekable  (False),
  itsReadable  (False),
  itsWritable  (False),
  itsFile      (0),
  itsBufSize   (0),
  itsBuffer    (0)
{}

LargeFilebufIO::LargeFilebufIO (FILE* file, uInt bufferSize)
: itsFile   (0),
  itsBuffer (0)
{
    attach (file, bufferSize);
}

LargeFilebufIO::LargeFilebufIO (FILE* file, Bool takeOver)
: itsFile   (0),
  itsBuffer (0)
{
    attach (file, takeOver);
}

LargeFilebufIO::LargeFilebufIO (int fd, uInt bufferSize)
: itsFile   (0),
  itsBuffer (0)
{
    attach (fd, bufferSize);
}

LargeFilebufIO::~LargeFilebufIO()
{
    detach();
}

void LargeFilebufIO::attach (FILE* file, uInt bufferSize,
			Bool readable, Bool writable)
{
    AlwaysAssert (itsFile == 0, AipsError);
    itsFile      = file;
    itsBufSize   = bufferSize;
    itsOwner     = True;
    itsReadable  = readable;
    itsWritable  = writable;
    itsSeekable  = False;
    itsReadDone  = True;
    itsWriteDone = True;
    // When needed create a buffer for the FILE.
    // Attach it to the file.
    if (bufferSize > 0) {
	itsBuffer = new char[bufferSize];
	AlwaysAssert (itsBuffer != 0, AipsError);
	setvbuf (itsFile, itsBuffer, _IOFBF, bufferSize);
    }
}

void LargeFilebufIO::attach (FILE* file, uInt bufferSize)
{
    attach (file, bufferSize, False, False);
    fillRWFlags (fileno (itsFile));
    fillSeekable();
}

void LargeFilebufIO::attach (FILE* file, Bool takeOver)
{
    attach (file, uInt(0));
    itsOwner = takeOver;
}

void LargeFilebufIO::attach (int fd, uInt bufferSize)
{
    fillRWFlags (fd);
    String opt = "rb";
    if (itsWritable) {
	opt = "wb";
	if (itsReadable) {
	    opt = "rb+";
	}
    }
    FILE* file = fdopen (fd, opt.chars());
    if (file == 0) {
       throw (AipsError ("LargeFilebufIO: error in fdopen: " +
                         String(strerror(errno))));
    }
    attach (file, bufferSize, itsReadable, itsWritable);
    fillSeekable();
}

void LargeFilebufIO::detach()
{
    if (itsOwner  &&  itsFile != 0) {
	traceFCLOSE (itsFile);
	delete [] itsBuffer;
	itsFile   = 0;
	itsBuffer = 0;
    }
}

void LargeFilebufIO::fillRWFlags (int fd)
{
    int flags = fcntl (fd, F_GETFL);
    if (flags & O_RDWR) {
	itsReadable = True;
	itsWritable = True;
    } else if (flags & O_WRONLY) {
	itsWritable = True;
    } else {
	itsReadable = True;
    }
}

void LargeFilebufIO::fillSeekable()
{
    itsSeekable = (seek (0, ByteIO::Current)  >= 0);
}


String LargeFilebufIO::fileName() const
{
    return "";
}


void LargeFilebufIO::write (uInt size, const void* buf)
{
    // Throw an exception if not writable.
    if (!itsWritable) {
	throw (AipsError ("LargeFilebufIO object is not writable"));
    }
    // After a read a seek is needed before a write can be done.
    // (requirement of stdio).
    if (itsReadDone) {
	traceFSEEK (itsFile, 0, SEEK_CUR);
	itsReadDone = False;
    }
    if (traceFWRITE ((char *)buf, 1, size, itsFile) != size) {
	throw (AipsError ("LargeFilebufIO: error while writing "
			  + fileName()));
    }
    itsWriteDone = True;
}

Int LargeFilebufIO::read (uInt size, void* buf, Bool throwException)
{
    // Throw an exception if not readable.
    if (!itsReadable) {
	throw (AipsError ("LargeFilebufIO::read - file is not readable"));
    }
    // After a write a seek is needed before a read can be done.
    // (requirement of stdio).
    if (itsWriteDone) {
	traceFSEEK (itsFile, 0, SEEK_CUR);
	itsWriteDone = False;
    }
    Int bytesRead = traceFREAD ((char *)buf, 1, size, itsFile);
    itsReadDone = True;
    if (bytesRead < 0  ||  bytesRead > Int(size)) {
      throw (AipsError ("LargeFilebufIO::read - fread returned a bad value"));
    }
    //# In case of a table reparation the remainder has to be filled with 0.
#if defined(TABLEREPAIR)
    if (bytesRead < Int(size)) {
      memset ((char*)buf + bytesRead, 0, size-bytesRead);
      bytesRead = size;
    }
#endif
    if (bytesRead != Int(size) && throwException) {
      String errmsg = "LargeFilebufIO::read - incorrect number of bytes read";
      errmsg += " from file " + fileName();
      throw (AipsError (errmsg));
    }
    return bytesRead;
}

Int64 LargeFilebufIO::doSeek (Int64 offset, ByteIO::SeekOption dir)
{
    // On the SUN a seek (even at the same place) slows down fread
    // tremendously. So only seek when needed.
    switch (dir) {
    case ByteIO::Begin:
	// On the SUN a seek (even at the same place) slows down fread
	// tremendously. So only seek when needed.
	if (offset != traceFTELL(itsFile)) {
	    traceFSEEK (itsFile, offset, SEEK_SET);
	    itsReadDone  = False;
	    itsWriteDone = False;
	}
	break;
    case ByteIO::End:
	traceFSEEK (itsFile, offset, SEEK_END);
	itsReadDone  = False;
	itsWriteDone = False;
	break;
    default:
	if (offset != 0) {
	    traceFSEEK (itsFile, offset, SEEK_CUR);
	    itsReadDone  = False;
	    itsWriteDone = False;
	}
	break;
    }
    return ftell (itsFile);
}

Int64 LargeFilebufIO::length()
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

   
Bool LargeFilebufIO::isReadable() const
{
    return itsReadable;
}

Bool LargeFilebufIO::isWritable() const
{
    return itsWritable;
}

Bool LargeFilebufIO::isSeekable() const
{
    return itsSeekable;
}
