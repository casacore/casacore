//# FilebufIO.cc: Class for buffered IO on a file.
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>                     // needed for errno
#include <casacore/casa/string.h>               // needed for strerror


namespace casacore { //# NAMESPACE CASACORE - BEGIN

FilebufIO::FilebufIO()
: itsSeekable   (False),
  itsReadable   (False),
  itsWritable   (False),
  itsFile       (-1),
  itsBufSize    (0),
  itsBufLen     (0),
  itsBuffer     (0),
  itsBufOffset  (-1),
  itsOffset     (-1),
  itsSeekOffset (-1),
  itsDirty      (False)
{}

FilebufIO::FilebufIO (int fd, uInt bufferSize)
: itsFile       (-1),
  itsBufSize    (0),
  itsBufLen     (0),
  itsBuffer     (0),
  itsBufOffset  (-1),
  itsOffset     (-1),
  itsSeekOffset (-1),
  itsDirty      (False)
{
  attach (fd, bufferSize);
}

FilebufIO::~FilebufIO()
{
  detach();
}


void FilebufIO::attach (int fd, uInt bufSize)
{
  AlwaysAssert (itsFile == -1, AipsError);
  itsFile       = fd;
  itsOffset     = 0;
  itsSeekOffset = -1;
  itsDirty      = False;
  fillRWFlags (fd);
  fillSeekable();
  setBuffer (bufSize);
}

void FilebufIO::setBuffer (Int64 bufSize)
{
  if (itsBuffer) {
    flush();
    delete [] itsBuffer;
    itsBuffer    = 0;
    itsBufSize   = 0;
    itsBufLen    = 0;
    itsBufOffset = -1;
  }
  if (bufSize > 0) {
    itsBuffer  = new char[bufSize];
    itsBufSize = bufSize;
    itsBufOffset = -Int(itsBufSize+1);
  }
}

void FilebufIO::detach (Bool closeFile)
{
  setBuffer (0);
  if (closeFile  &&  itsFile >= 0) {
    ::traceCLOSE (itsFile);
  }
  itsFile = -1;
}

void FilebufIO::fillRWFlags (int fd)
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

void FilebufIO::fillSeekable()
{
  Int64 curOff = itsOffset;
  itsSeekable = (doSeek (0, ByteIO::End)  >= 0);
  itsOffset = curOff;
}


String FilebufIO::fileName() const
{
  return "";
}


void FilebufIO::flush()
{
  if (itsDirty) {
    writeBuffer (itsBufOffset, itsBuffer, itsBufLen);
    itsDirty = False;
  }
}

void FilebufIO::resync()
{
  AlwaysAssert (!itsDirty, AipsError);
  itsBufLen     = 0;
  itsBufOffset  = -Int(itsBufSize+1);
  itsOffset     = 0;
  itsSeekOffset = -1;
}


void FilebufIO::writeBuffer (Int64 offset, const char* buf, Int64 size)
{
  if (size > 0) {
    if (offset != itsSeekOffset) {
      ::traceLSEEK (itsFile, offset, SEEK_SET);
      itsSeekOffset = offset;
    }
    if (::traceWRITE (itsFile, const_cast<char*>(buf), size) != size) {
      int error = errno;
      itsSeekOffset = -1;
      throw AipsError (String("FilebufIO: write error for file ")
		       + fileName() + ": " + strerror(error));
    }
    itsSeekOffset += size;
  }
}

Int64 FilebufIO::readBuffer (Int64 offset, char* buf, Int64 size,
                                  Bool throwException)
{
  if (offset != itsSeekOffset) {
    ::traceLSEEK (itsFile, offset, SEEK_SET);
    itsSeekOffset = offset;
  }
  Int64 bytesRead = ::traceREAD (itsFile, buf, size);
  int error = errno;
  if (bytesRead > Int(size)) { // Should never be executed
    itsSeekOffset = -1;
    throw AipsError ("FilebufIO::read - read returned a bad value"
		     " for file " + fileName());
  }
  if (bytesRead != Int(size) && throwException == True) {
    //# In case of a table reparation the remainder has to be filled with 0.
#if defined(TABLEREPAIR)
    memset ((char*)buf + bytesRead, 0, size-bytesRead);
    bytesRead = size;
#endif
    if (bytesRead < 0) {
      itsSeekOffset = -1;
      throw AipsError (String("FilebufIO::read error for file ")
		       + fileName() + ": " + strerror(error));
    } else if (bytesRead < Int(size)) {
      itsSeekOffset = -1;
      throw AipsError ("FilebufIO::read - incorrect number of bytes ("
		       + String::toString(bytesRead) + " out of "
                       + String::toString(size) + ") read for file "
                       + fileName());
    }
  }
  itsSeekOffset += bytesRead;
  return bytesRead;
}

void FilebufIO::write (Int64 size, const void* buf)
{
  // Throw an exception if not writable.
  if (!itsWritable) {
    throw AipsError ("FilebufIO object (file " + fileName()
		     + ") is not writable");
  }
  const char* bufc = static_cast<const char*>(buf);
  // Determine blocknr of first and last full block.
  Int64 st = (itsOffset + itsBufSize - 1) / itsBufSize;
  Int64 end = (itsOffset + size) / itsBufSize;
  Int64 blkst = st * itsBufSize - itsOffset;
  Int64 sz = 0;
  if (st < end) {
    sz = (end-st) * itsBufSize;
    // There are one or more full blocks.
    // Write them all.
    writeBuffer (st*itsBufSize, bufc+blkst, sz);
    // Discard the current buffer if within these full blocks.
    if (st*itsBufSize <= itsBufOffset
    &&  end*itsBufSize >= itsBufOffset+itsBufSize) {
      itsDirty = False;
      itsBufOffset = -Int(itsBufSize+1);
      itsBufLen = 0;
    }
  }
  // Write the start of the user buffer (if needed).
  // Note that by doing this after the full block write, we avoid a
  // possible flush of the current buffer if it is within the full blocks.
  if (blkst > 0) {
    if (blkst > size) {
      blkst = size;
    }
    writeBlock (blkst, bufc);
  }
  // Write the remainder of the user buffer (if needed).
  // First update the offset in the stream.
  blkst += sz;
  itsOffset += blkst;
  if (blkst < size) {
    size -= blkst;
    writeBlock (size, bufc+blkst);
    itsOffset += size;
  }
}

Int64 FilebufIO::read (Int64 size, void* buf, Bool throwException)
{
  // Throw an exception if not readable.
  if (!itsReadable) {
    throw AipsError ("FilebufIO object (file " + fileName()
		     + ") is not readable");
  }
  char* bufc = static_cast<char*>(buf);
  // Determine blocknr of first and last full block.
  Int64 st = (itsOffset + itsBufSize - 1) / itsBufSize;
  Int64 end = (itsOffset + size) / itsBufSize;
  Int64 blkst = st * itsBufSize - itsOffset;
  Int64 sz = 0;
  if (st < end) {
    // There are one or more full blocks.
    // Read them all.
    // Handle the case that one of them is the current buffer.
    Int64 stoff = st*itsBufSize;
    Int64 endoff = end*itsBufSize;
    char* bufp = bufc+blkst;
    if (stoff <= itsBufOffset  &&  endoff >= itsBufOffset+itsBufSize) {
      if (stoff < itsBufOffset) {
	sz = itsBufOffset-stoff;
	AlwaysAssert (readBuffer (stoff, bufp, sz, throwException) == sz,
		      AipsError);
	bufp += sz;
      }
      // Do the get of the current block via readBlock to handle
      // the (hardly possible) case that itsBufSize!=itsBufLen.
      Int64 savoff = itsOffset;
      itsOffset = itsBufOffset;
      sz += readBlock (itsBufSize, bufp, throwException);
      itsOffset = savoff;
      stoff = itsBufOffset + itsBufSize;
    }
    // Read the remaining full blocks.
    sz += readBuffer (stoff, bufp, endoff-stoff, throwException);
  }
  // Read the start of the user buffer (if needed).
  Int64 total = sz;
  if (blkst > 0) {
    if (blkst > size) {
      blkst = size;
    }
    total += readBlock (blkst, bufc, throwException);
  }
  // Read the remainder of the user buffer (if needed).
  // First update the offset in the stream.
  blkst += sz;
  itsOffset += blkst;
  if (blkst < size) {
    sz = size - blkst;
    total += readBlock (sz, bufc+blkst, throwException);
    itsOffset += sz;
  }
  return total;
}

void FilebufIO::writeBlock (Int64 size, const char* buf)
{
  // Write a part of a block.
  // It is ensured that the buffer fits in a single block and that it
  // is not a full block.
  // Flush current buffer if needed.
  if (itsOffset < itsBufOffset || itsOffset >= itsBufOffset + itsBufSize) {
    if (itsDirty) {
      flush();
    }
    // Read the new buffer.
    itsBufOffset = itsOffset / itsBufSize * itsBufSize;
    itsBufLen = readBuffer (itsBufOffset, itsBuffer, itsBufSize, False);
  }
  Int64 st = itsOffset - itsBufOffset;
  memcpy (itsBuffer+st, buf, size);
  itsDirty = True;
  if (st+size > itsBufLen) {
    itsBufLen = st+size;
  }
}

Int64 FilebufIO::readBlock (Int64 size, char* buf, Bool throwException)
{
  // Read a part of a block.
  // It is ensured that the buffer fits in a single block and that it
  // is not a full block.
  // Read current buffer if needed. Flush if current block is dirty.
  if (itsOffset < itsBufOffset || itsOffset >= itsBufOffset + itsBufSize) {
    if (itsDirty) {
      flush();
    }
    // Read the new buffer.
    itsBufOffset = itsOffset / itsBufSize * itsBufSize;
    itsBufLen = readBuffer (itsBufOffset, itsBuffer, itsBufSize, False);
  }
  Int64 st = itsOffset - itsBufOffset;
#if defined(TABLEREPAIR)
  if (st+size > itsBufLen) {
    memset (itsBuffer+itsBufLen, 0, st+size-itsBufLen);
    itsBufLen = st+size;
  }
#endif
  if (st+size > itsBufLen) {
    if (throwException) {
      throw AipsError ("FilebufIO::readBlock - incorrect number of bytes"
		       " read for file " + fileName());
    }
    if (itsBufLen > st) {
      size = itsBufLen-st;
    } else {
      size = 0;
    }
  }
  memcpy (buf, itsBuffer+st, size);
  return size;
}

Int64 FilebufIO::doSeek (Int64 offset, ByteIO::SeekOption dir)
{
  switch (dir) {
  case ByteIO::Begin:
    itsOffset = offset;
    break;
  case ByteIO::End:
    itsSeekOffset = ::traceLSEEK (itsFile, offset, SEEK_END);
    itsOffset = itsSeekOffset;
    break;
  default:
    itsOffset += offset;
    break;
  }
  return itsOffset;
}


Int64 FilebufIO::length()
{
  itsSeekOffset = ::traceLSEEK (itsFile, 0, SEEK_END);
  Int64 len = itsBufOffset+itsBufLen;
  if (len < itsSeekOffset) {
    len = itsSeekOffset;
  }
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

} //# NAMESPACE CASACORE - END

