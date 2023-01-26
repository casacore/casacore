//# MemoryIO.cc: Class for IO in memory
//# Copyright (C) 1996,1999,2001
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
//#        Intgernet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA


#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <iostream>
#include <sstream>
#include <cstring>                  //# for memcpy with gcc-4.3

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MemoryIO::MemoryIO (uint64_t initialSize, uint64_t expandSize)
: itsBuffer     (0),
  itsAlloc      (initialSize),
  itsExpandSize (expandSize),
  itsUsed       (0),
  itsPosition   (0),
  itsReadable   (true),
  itsWritable   (true),
  itsCanDelete  (true)
{
  if (itsAlloc > 0) {
    itsBuffer = new unsigned char[itsAlloc];
    AlwaysAssert (itsBuffer != 0, AipsError);
  }
}

MemoryIO::MemoryIO (const void* buffer, uint64_t size)
: itsBuffer     ((unsigned char*)buffer),
  itsAlloc      (size),
  itsExpandSize (0),
  itsUsed       (size),
  itsPosition   (0),
  itsReadable   (true),
  itsWritable   (false),
  itsCanDelete  (false)
{}

MemoryIO::MemoryIO (void* buffer, uint64_t size, ByteIO::OpenOption option,
		    uint64_t expandSize, bool canDelete)
: itsBuffer     ((unsigned char*)buffer),
  itsAlloc      (size),
  itsExpandSize (expandSize),
  itsUsed       (size),
  itsPosition   (0),
  itsReadable   (true),
  itsWritable   (true),
  itsCanDelete  (canDelete)
{
  // Make sure there is a buffer.
  if (itsAlloc > 0) {
    AlwaysAssert (itsBuffer != 0, AipsError);
  }
  // Adapt position, etc. from the option.
  switch (option) {
  case ByteIO::Old:
    itsWritable = false;
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

void MemoryIO::write (int64_t size, const void* buf)
{
  // Throw an exception if not writable.
  if (!itsWritable) {
    throw (AipsError ("MemoryIO::write - MemoryIO object is not writable"));
  }
  // Expand the buffer when needed (and possible).
  int64_t minSize = itsPosition + size;
  if (minSize > itsAlloc) {
    if (! expand (minSize)) {
      throw (AipsError ("MemoryIO::write - buffer cannot be expanded"));
    }
  }
  // Copy the data and set new position and used.
  memcpy (itsBuffer + itsPosition, buf, size);
  itsPosition += size;
  if (itsPosition > itsUsed) {
    itsUsed = itsPosition;
  }
}

int64_t MemoryIO::read (int64_t size, void* buf, bool throwException)
{
  // Throw an exception if not readable.
  if (!itsReadable) {
    throw (AipsError ("MemoryIO::read - buffer is not readable"));
  }
  const int64_t bytesLeft = itsUsed - itsPosition;
  int64_t bytesRead = 0;
  if (size <= bytesLeft) {
    memcpy (buf, itsBuffer + itsPosition, size);
    itsPosition += size;
    bytesRead = size;
  } else {
    if (bytesLeft >= 0) { 
      bytesRead = bytesLeft;
      memcpy (buf, itsBuffer + itsPosition, bytesRead);
      itsPosition += bytesLeft;
      if (throwException) {
        std::ostringstream oss;
        oss << "MemoryIO::read - incorrect number of bytes read:  "
            << std::endl
            << "  size=" << size << ", used=" << itsUsed
            << ", pos=" << itsPosition << ", left=" << bytesLeft;
	throw AipsError (oss.str());
      }
    } else {
      std::ostringstream oss;
      oss << "MemoryIO::read - buffer position is invalid:"
          << std::endl
          << "  size=" << size << ", used=" << itsUsed
          << ", pos=" << itsPosition << ", left=" << bytesLeft;
      throw AipsError (oss.str());
    }
  }
  return bytesRead;
}

int64_t MemoryIO::doSeek (int64_t offset, ByteIO::SeekOption dir)
{
  // Determine the new position.
  // Exit with error status if negative.
  int64_t newPos;
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
    throw (AipsError("MemoryIO::seek - cannot seek before start of object"));
  }
  // It is possible to seek past the end of the buffer.
  // This means that the buffer usage increases.
  // Expand the buffer if needed.
  // Initialize the new buffer positions with zeroes.
  if (newPos > itsUsed) {
    // Throw an exception if not writable.
    if (!itsWritable) {
      throw (AipsError ("MemoryIO::seek - cannot seek past the end "
			"of a readonly object"));
    }
    if (newPos > itsAlloc) {
      if (! expand (newPos)) {
	throw (AipsError("MemoryIO::seek - buffer cannot be expanded"));
      }
    }
    for (; itsUsed<newPos; itsUsed++) {
      itsBuffer[itsUsed] = 0;
    }
  }
  itsPosition = newPos;
  return newPos;
}


bool MemoryIO::expand (uint64_t minSize)
{
  int64_t minsz = minSize;
  // Check if expansion is really needed.
  if (minsz <= itsAlloc) {
    return true;
  }
  // Return with error status when expansion is not possible.
  if (itsExpandSize == 0) {
    return false;
  }
  // Expand with at least the expansion size.
  if (minsz < itsAlloc + itsExpandSize) {
    minsz = itsAlloc + itsExpandSize;
  }
  // Allocate new buffer, copy contents and delete old buffer (if possible).
  unsigned char* newBuffer = new unsigned char[minsz];
  AlwaysAssert (newBuffer != 0, AipsError);
  // Copy the old contents (if any).
  if (itsBuffer != 0) {
    memcpy (newBuffer, itsBuffer, itsUsed);
    if (itsCanDelete) {
      delete [] itsBuffer;
    }
  }
  itsBuffer    = newBuffer;
  itsAlloc     = minsz;
  itsCanDelete = true;
  return true;
}


int64_t MemoryIO::length()
{
  return itsUsed;
}

bool MemoryIO::isReadable() const
{
  return itsReadable;
}
bool MemoryIO::isWritable() const
{
  return itsWritable;
}
bool MemoryIO::isSeekable() const
{
  return true;
}

void MemoryIO::setUsed (uint64_t bytesUsed)
{
  if (!itsWritable) {
    throw (AipsError ("MemoryIO::setUsed - object is not writable"));
  }
  if (int64_t(bytesUsed) > itsAlloc) {
    throw(AipsError ("MemoryIO::setUsed - cannot use more than is allocated"));
  }
  itsUsed = bytesUsed;
}

unsigned char* MemoryIO::setBuffer (uint64_t length)
{
  if (!itsWritable) {
    throw (AipsError ("MemoryIO::setBuffer - object is not writable"));
  }
  if (!expand (length)) {
    throw(AipsError ("MemoryIO::setBuffer - buffer cannot be expanded"));
  }
  itsUsed = length;
  return itsBuffer;
}

} //# NAMESPACE CASACORE - END

