//# FilebufIO.cc: Class for IO on a file using a filebuf object.
//# Copyright (C) 1996,1997,1998
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
#include <aips/IO/FilebufIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <fcntl.h>
#include <errno.h>                // needed for errno
#include <string.h>               // needed for strerror

//
// PABLO_IO is used to profile the IO performance of the AIPS++ (in particular
// to help us locate bottlenecks associated with parallel processing)
//
// Please see http://www-pablo.cs.uiuc.edu if you need more details
// You'll need to set PABLO_IO='yes' in the makedefs add addition -I flag to find
// the header file and -L to resolve with the library.
//

#ifdef PABLO_IO
#include "IOTrace.h"
#else
#define traceFCLOSE fclose
#define traceFSEEK fseek
#define traceFREAD fread
#define traceFWRITE fwrite
#endif PABLO_IO

FilebufIO::FilebufIO()
: itsOwner     (False),
  itsSeekable  (False),
  itsReadable  (False),
  itsWritable  (False),
  itsFile      (0),
  itsBufSize   (0),
  itsBuffer    (0)
{}

FilebufIO::FilebufIO (FILE* file, uInt bufferSize)
: itsFile   (0),
  itsBuffer (0)
{
    attach (file, bufferSize);
}

FilebufIO::FilebufIO (FILE* file, Bool takeOver)
: itsFile   (0),
  itsBuffer (0)
{
    attach (file, takeOver);
}

FilebufIO::FilebufIO (int fd, uInt bufferSize)
: itsFile   (0),
  itsBuffer (0)
{
    attach (fd, bufferSize);
}

FilebufIO::~FilebufIO()
{
    detach();
}

void FilebufIO::attach (FILE* file, uInt bufferSize,
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

void FilebufIO::attach (FILE* file, uInt bufferSize)
{
    attach (file, bufferSize, False, False);
    fillRWFlags (fileno (itsFile));
    fillSeekable();
}

void FilebufIO::attach (FILE* file, Bool takeOver)
{
    attach (file, uInt(0));
    itsOwner = takeOver;
}

void FilebufIO::attach (int fd, uInt bufferSize)
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
       throw (AipsError ("FilebufIO: error in fdopen: " +
                         String(strerror(errno))));
    }
    attach (file, bufferSize, itsReadable, itsWritable);
    fillSeekable();
}

void FilebufIO::detach()
{
    if (itsOwner  &&  itsFile != 0) {
	traceFCLOSE (itsFile);
	delete [] itsBuffer;
	itsFile   = 0;
	itsBuffer = 0;
    }
}

void FilebufIO::fillRWFlags (int fd)
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

void FilebufIO::fillSeekable()
{
    itsSeekable = ToBool (seek (0, ByteIO::Current)  >= 0);
}


String FilebufIO::fileName() const
{
    return "";
}


void FilebufIO::write (uInt size, const void* buf)
{
    // Throw an exception if not writable.
    if (!itsWritable) {
	throw (AipsError ("FilebufIO object is not writable"));
    }
    // After a read a seek is needed before a write can be done.
    // (requirement of stdio).
    if (itsReadDone) {
	traceFSEEK (itsFile, 0, SEEK_CUR);
	itsReadDone = False;
    }
    if (traceFWRITE (buf, 1, size, itsFile) != size) {
	throw (AipsError ("FilebufIO: error while writing " + fileName()));
    }
    itsWriteDone = True;
}

void FilebufIO::read (uInt size, void* buf)
{
    // Throw an exception if not readable.
    if (!itsReadable) {
	throw (AipsError ("FilebufIO object is not readable"));
    }
    // After a write a seek is needed before a read can be done.
    // (requirement of stdio).
    if (itsWriteDone) {
	traceFSEEK (itsFile, 0, SEEK_CUR);
	itsWriteDone = False;
    }
    if (traceFREAD (buf, 1, size, itsFile) != size) {
	throw (AipsError ("FilebufIO: error while reading " + fileName()));
    }
    itsReadDone = True;
}

Long FilebufIO::seek (Long offset, ByteIO::SeekOption dir)
{
    // On the SUN a seek (even at the same place) slows down fread
    // tremendously. So lonly seek when needed.
    switch (dir) {
    case ByteIO::Begin:
	// On the SUN a seek (even at the same place) slows down fread
	// tremendously. So only seek when needed.
	if (offset != ftell(itsFile)) {
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

Long FilebufIO::length()
{
    // Get current position to be able to reposition.
    Long pos = seek (0, ByteIO::Current);
    // Seek to the end of the stream.
    // If it fails, we cannot seek and the current position is the length.
    Long len = seek (0, ByteIO::End);
    if (len < 0) {
	return pos;
    }
    // Reposition and return the length.
    seek (pos, ByteIO::Begin);
    return len;
}

   
Bool FilebufIO::isReadable() const
{
    return itsReadable;
}

Bool FilebufIO::isWritable() const
{
    return itsWritable;
}

Bool FilebufIO::isSeekable() const
{
    return itsSeekable;
}
