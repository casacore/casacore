//# FilebufIO.cc: Class for IO on a file using a filebuf object.
//# Copyright (C) 1996
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
#include <iostream.h>
#include <fstream.h>
#include <fcntl.h>

FilebufIO::FilebufIO()
: itsOwner     (False),
  itsFilebuf   (0),
  itsBuffer    (0),
  itsSeekable  (False),
  itsReadable  (False),
  itsWritable  (False),
  itsReadDone  (False),
  itsWriteDone (False)
{}

FilebufIO::FilebufIO (int fd, uInt bufferSize)
: itsFilebuf (0)
{
    attach (fd, bufferSize);
}

FilebufIO::~FilebufIO()
{
    detach();
}

void FilebufIO::prepareAttach (uInt bufferSize, Bool readable, Bool writable)
{
    AlwaysAssert (itsFilebuf == 0, AipsError);
    itsOwner     = True;
    itsReadable  = readable;
    itsWritable  = writable;
    itsReadDone  = True;
    itsWriteDone = True;
    // Create a buffer for filebuf and attach it.
    itsBuffer  = new char[bufferSize];
    itsFilebuf = new filebuf;
    AlwaysAssert (itsBuffer != 0  &&  itsFilebuf != 0, AipsError);
    AlwaysAssert (itsFilebuf->setbuf(itsBuffer, bufferSize) != 0, AipsError);
}

void FilebufIO::attach (int fd, uInt bufferSize)
{
    prepareAttach (bufferSize, False, False);
    // Attach the file.
    AlwaysAssert (itsFilebuf->attach (fd) != 0, AipsError);
    int flags = fcntl (fd, F_GETFL);
    if (flags & O_RDWR) {
	itsReadable = True;
	itsWritable = True;
    } else if (flags & O_WRONLY) {
	itsWritable = True;
    } else {
	itsReadable = True;
    }
    fillSeekable();
}

void FilebufIO::detach()
{
    if (itsFilebuf != 0) {
	if (itsOwner) {
	    delete itsFilebuf;
	    delete [] itsBuffer;
	}
	itsFilebuf = 0;
	itsBuffer  = 0;
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
    // If a read has been done, adjust the write pointer.
    if (itsReadDone) {
	itsFilebuf->seekoff (0, ios::cur, ios::in|ios::out);
	itsReadDone = False;
    }
    if (itsFilebuf->sputn ((const char*)buf, size) != size) {
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
    // If a write has been done, adjust the read pointer.
    if (itsWriteDone) {
	itsFilebuf->seekoff (0, ios::cur, ios::in|ios::out);
	itsWriteDone = False;
    }
    if (itsFilebuf->sgetn ((char*)buf, size) != size) {
	throw (AipsError ("FilebufIO: error while reading " + fileName()));
    }
    itsReadDone = True;
}

Long FilebufIO::seek (Long offset, ByteIO::SeekOption dir)
{
    itsReadDone  = False;
    itsWriteDone = False;
    switch (dir) {
    case ByteIO::Begin:
	return itsFilebuf->seekoff (offset, ios::beg, ios::in|ios::out);
    case ByteIO::End:
	return itsFilebuf->seekoff (offset, ios::end, ios::in|ios::out);
    }
    return itsFilebuf->seekoff (offset, ios::cur, ios::in|ios::out);
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
