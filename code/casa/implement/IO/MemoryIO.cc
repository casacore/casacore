//# MemoryIO.cc: Class for IO in memory
//# Copyright (C) 1996,1999
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


#include <aips/IO/MemoryIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


MemoryIO::MemoryIO (uLong initialSize, uLong expandSize)
: itsBuffer     (0),
  itsAlloc      (initialSize),
  itsExpandSize (expandSize),
  itsUsed       (0),
  itsPosition   (0),
  itsReadable   (True),
  itsWritable   (True),
  itsCanDelete  (True)
{
    if (itsAlloc > 0) {
	itsBuffer = new uChar[itsAlloc];
	AlwaysAssert (itsBuffer != 0, AipsError);
    }
}

MemoryIO::MemoryIO (const void* buffer, uLong size)
: itsBuffer     ((uChar*)buffer),
  itsAlloc      (size),
  itsExpandSize (0),
  itsUsed       (size),
  itsPosition   (0),
  itsReadable   (True),
  itsWritable   (False),
  itsCanDelete  (False)
{}

MemoryIO::MemoryIO (void* buffer, uLong size, ByteIO::OpenOption option,
		    uLong expandSize, Bool canDelete)
: itsBuffer     ((uChar*)buffer),
  itsAlloc      (size),
  itsExpandSize (expandSize),
  itsUsed       (size),
  itsPosition   (0),
  itsReadable   (True),
  itsWritable   (True),
  itsCanDelete  (canDelete)
{
    // Make sure there is a buffer.
    if (itsAlloc > 0) {
	AlwaysAssert (itsBuffer != 0, AipsError);
    }
    // Adapt position, etc. from the option.
    switch (option) {
    case ByteIO::Old:
	itsWritable = False;
	break;
    case ByteIO::Append:
	itsPosition = itsUsed;
	break;
    default:
	itsUsed = 0;
	break;
    }
}


MemoryIO::~MemoryIO()
{
    if (itsCanDelete) {
	delete [] itsBuffer;
    }
}

void MemoryIO::write (uInt size, const void* buf)
{
    // Throw an exception if not writable.
    if (!itsWritable) {
	throw (AipsError ("MemoryIO object is not writable"));
    }
    // Expand the buffer when needed (and possible).
    uLong minSize = itsPosition + size;
    if (minSize > itsAlloc) {
	if (! expand (minSize)) {
	    throw (AipsError ("MemoryIO::write: buffer cannot be expanded"));
	}
    }
    // Copy the data and set new position and used.
    memcpy (itsBuffer + itsPosition, buf, size);
    itsPosition += size;
    if (itsPosition > itsUsed) {
	itsUsed = itsPosition;
    }
}

Int MemoryIO::read (uInt size, void* buf, Bool throwException) {
  // Throw an exception if not readable.
  if (!itsReadable) {
    throw (AipsError ("MemoryIO::read - buffer is not readable"));
  }
  const Int bytesLeft = itsUsed - itsPosition;
  Int bytesRead = 0;
  if (Int(size) <= bytesLeft) {
    memcpy (buf, itsBuffer + itsPosition, size);
    itsPosition += size;
    bytesRead = size;
  } else {
    if (bytesLeft > 0) { 
      memcpy (buf, itsBuffer + itsPosition, uInt(bytesLeft));
      itsPosition += bytesLeft;
      if (throwException) {
	throw (AipsError ("MemoryIO::read - incorrect number of bytes read"));
      }
      bytesRead = bytesLeft;
    } else {
      throw (AipsError ("MemoryIO::read - buffer position is invalid"));
    }
  }
  return bytesRead;
}

Long MemoryIO::seek (Long offset, ByteIO::SeekOption dir)
{
    // Determine the new position.
    // Exit with error status if negative.
    Long newPos;
    switch (dir) {
    case ByteIO::Begin:
	newPos = offset;
	break;
    case ByteIO::End:
	newPos = itsUsed + offset;
	break;
    default:
	newPos = itsPosition + offset;
	break;
    }
    if (newPos < 0) {
	throw (AipsError ("MemoryIO::seek before start of object"));
    }
    // It is possible to seek past the end of the buffer.
    // This means that the buffer usage increases.
    // Expand the buffer if needed.
    // Initialize the new buffer positions with zeroes.
    if (newPos > Long(itsUsed)) {
	// Throw an exception if not writable.
	if (!itsWritable) {
	    throw (AipsError ("MemoryIO::seek past end of readonly object"));
	}
	if (newPos > Long(itsAlloc)) {
	    if (! expand (newPos)) {
		throw (AipsError("MemoryIO::seek: buffer cannot be expanded"));
	    }
	}
	for (; Long(itsUsed)<newPos; itsUsed++) {
	    itsBuffer[itsUsed] = 0;
	}
    }
    itsPosition = newPos;
    return newPos;
}


Bool MemoryIO::expand (uLong minSize)
{
    // Check if expansion is really needed.
    if (minSize <= itsAlloc) {
	return True;
    }
    // Return with error status when expansion is not possible.
    if (itsExpandSize == 0) {
	return False;
    }
    // Expand with at least the expansion size.
    if (minSize < itsAlloc + itsExpandSize) {
	minSize = itsAlloc + itsExpandSize;
    }
    // Allocate new buffer, copy contents and delete old buffer (if possible).
    uChar* newBuffer = new uChar[minSize];
    AlwaysAssert (newBuffer != 0, AipsError);
    // Copy the old contents (if any).
    if (itsBuffer != 0) {
	memcpy (newBuffer, itsBuffer, itsUsed);
	if (itsCanDelete) {
	    delete [] itsBuffer;
	}
    }
    itsBuffer    = newBuffer;
    itsAlloc     = minSize;
    itsCanDelete = True;
    return True;
}


Long MemoryIO::length()
{
    return itsUsed;
}

Bool MemoryIO::isReadable() const
{
    return itsReadable;
}
Bool MemoryIO::isWritable() const
{
    return itsWritable;
}
Bool MemoryIO::isSeekable() const
{
    return True;
}
