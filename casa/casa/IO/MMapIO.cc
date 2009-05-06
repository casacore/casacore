//# MMapIO.cc: Memory-mapped IO on a file
//#
//#  Copyright (C) 2009
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
//#  $Id$

#include <casa/IO/MMapIO.h>
#include <casa/IO/LargeRegularFileIO.h>
#include <casa/Exceptions/Error.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <cstring>

namespace casa
{
  
MMapIO::MMapIO (const RegularFile& regularFile, ByteIO::OpenOption option)
  : itsPtr      (0),
    itsFileName (regularFile.path().originalName())
{
  itsFD = LargeRegularFileIO::openCreate (regularFile, option);
  attach (itsFD);
  // Keep writable switch because it is used quite often.
  itsIsWritable = isWritable();
  itsFileSize = length();
  if (itsFileSize > 0) {
    mapFile();
  }
  if (option == ByteIO::Append) {
    doSeek (itsFileSize, ByteIO::Begin);
  }
}

MMapIO::~MMapIO()
{
  unmapFile();
  close (itsFD);
}

void MMapIO::mapFile()
{
  // Unmap file if still mapped.
  if (itsPtr != 0) {
    unmapFile();
  }
  int prot = PROT_READ;
  if (itsIsWritable) {
    prot = PROT_READ | PROT_WRITE;
  }
  // Do mmap of entire file.
  itsPtr = static_cast<char*>(::mmap (0, itsFileSize, prot, MAP_SHARED,
                                      itsFD, 0));
  if (itsPtr == MAP_FAILED) {
    throw AipsError ("MMapIO::MMapIO - mmap of " + itsFileName +
                     " failed: " + strerror(errno));
  }
  // Optimize for sequential access.
  ::madvise (itsPtr, itsFileSize, MADV_SEQUENTIAL);
}

void MMapIO::unmapFile()
{
  if (itsPtr != 0) {
    int res = ::munmap (itsPtr, itsFileSize);
    if (res != 0) {
      throw AipsError ("MMapIO::unmapFile - munmap of " + itsFileName +
                       " failed: " + strerror(errno));
    }
    itsPtr = 0;
  }
}

void MMapIO::flush()
{
  if (itsIsWritable  &&  itsPtr != 0) {
    int res = ::msync (0, itsFileSize, MS_SYNC);
    if (res != 0) {
      throw AipsError ("MMapIO::flush - msync of " + itsFileName +
                       " failed: " + strerror(errno));
    }
  }
}

void MMapIO::write (uInt size, const void* buf)
{
  if (!itsIsWritable) {
    throw AipsError ("MMapIO file " + itsFileName + " is not writable");
  }
  if (size > 0) {
    // If past end-of-file, write the last byte to extend the file.
    // Unmap and remap the file in that case.
    if (itsPosition + size > itsFileSize) {
      unmapFile();
      itsFileSize = itsPosition + size;
      LargeFiledesIO::doSeek (itsFileSize-1, ByteIO::Begin);
      char b=0;
      LargeFiledesIO::write (1, &b);
      mapFile();
    }
    memcpy (itsPtr+itsPosition, buf, size);
    itsPosition += size;
  }
}

Int MMapIO::read (uInt size, void* buf, Bool throwException)
{
  Int szrd = size;
  if (itsPosition >= itsFileSize) {
    szrd = 0;
  } else if (itsPosition+size > itsFileSize) {
    szrd = itsFileSize - itsPosition;
  }
  if (szrd > 0) {
    memcpy (buf, itsPtr+itsPosition, szrd);
    itsPosition += szrd;
    if (throwException  &&  szrd < size) {
      throw AipsError ("MMapIO::read - " + itsFileName +
                       " incorrect number of bytes read");
    }
  }
  return szrd;
}

Int64 MMapIO::doSeek (Int64 offset, ByteIO::SeekOption dir)
{
  itsPosition = LargeFiledesIO::doSeek (offset, dir);
  return itsPosition;
}

const void* MMapIO::getReadPointer (Int64 offset) const
{
  if (offset >= itsFileSize) {
    throw AipsError ("MMapIO::getReadPointer: beyond EOF of " + itsFileName);
  }
  return itsPtr+offset;
}

void* MMapIO::getWritePointer (Int64 offset)
{
  if (!itsIsWritable) {
    throw AipsError ("MMapIO file " + itsFileName + " is not writable");
  }
  if (offset >= itsFileSize) {
    throw AipsError ("MMapIO::getWritePointer: beyond EOF of " + itsFileName);
  }
  return itsPtr+offset;
}

} // end namespace
