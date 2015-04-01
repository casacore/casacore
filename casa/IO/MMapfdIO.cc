//# MMapfdIO.cc: Memory-mapped IO on a file
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

#include <casacore/casa/IO/MMapfdIO.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <cstring>

namespace casacore
{
  
  MMapfdIO::MMapfdIO()
    : itsFileSize   (0),
      itsPosition   (0),
      itsPtr        (0),
      itsIsWritable (False)
  {}

  MMapfdIO::MMapfdIO (int fd, const String& fileName)
    : itsPtr (0)
  {
    map (fd, fileName);
  }

  void MMapfdIO::map (int fd, const String& fileName)
  {
    attach (fd, fileName);
    // Keep writable switch because it is used quite often.
    itsIsWritable = isWritable();
    itsFileSize   = length();
    itsPosition   = 0;
    if (itsFileSize > 0) {
      mapFile();
    }
  }

  MMapfdIO::~MMapfdIO()
  {
    unmapFile();
  }

  void MMapfdIO::mapFile()
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
                                        fd(), 0));
    if (itsPtr == MAP_FAILED) {
      throw AipsError ("MMapfdIO::MMapfdIO - mmap of " + fileName() +
                       " failed: " + strerror(errno));
    }
    // Optimize for sequential access.
    ::madvise (itsPtr, itsFileSize, MADV_SEQUENTIAL);
  }

  void MMapfdIO::unmapFile()
  {
    if (itsPtr != 0) {
      int res = ::munmap (itsPtr, itsFileSize);
      if (res != 0) {
        throw AipsError ("MMapfdIO::unmapFile - munmap of " + fileName() +
                         " failed: " + strerror(errno));
      }
      itsPtr = 0;
    }
  }

  void MMapfdIO::flush()
  {
    if (itsIsWritable  &&  itsPtr != 0) {
      int res = ::msync (itsPtr, itsFileSize, MS_SYNC);
      if (res != 0) {
        throw AipsError ("MMapfdIO::flush - msync of " + fileName() +
                         " failed: " + strerror(errno));
      }
    }
  }

  void MMapfdIO::write (Int64 size, const void* buf)
  {
    if (!itsIsWritable) {
      throw AipsError ("MMapfdIO file " + fileName() + " is not writable");
    }
    if (size > 0) {
      // If past end-of-file, write the last byte to extend the file.
      // Unmap and remap the file in that case.
      if (itsPosition + size > itsFileSize) {
        unmapFile();
        itsFileSize = itsPosition + size;
        FiledesIO::doSeek (itsFileSize-1, ByteIO::Begin);
        char b=0;
        FiledesIO::write (1, &b);
        mapFile();
      }
      memcpy (itsPtr+itsPosition, buf, size);
      itsPosition += size;
    }
  }

  Int64 MMapfdIO::read (Int64 size, void* buf, Bool throwException)
  {
    Int64 szrd = size;
    if (itsPosition >= itsFileSize) {
      szrd = 0;
    } else if (itsPosition+size > itsFileSize) {
      szrd = itsFileSize - itsPosition;
    }
    if (szrd > 0) {
      memcpy (buf, itsPtr+itsPosition, szrd);
      itsPosition += szrd;
      if (throwException  &&  szrd < Int(size)) {
        throw AipsError ("MMapfdIO::read - " + fileName() +
                         " incorrect number of bytes read");
      }
    }
    return szrd;
  }

  Int64 MMapfdIO::doSeek (Int64 offset, ByteIO::SeekOption dir)
  {
    itsPosition = FiledesIO::doSeek (offset, dir);
    return itsPosition;
  }

  const void* MMapfdIO::getReadPointer (Int64 offset) const
  {
    if (offset >= itsFileSize) {
      throw AipsError ("MMapfdIO::getReadPointer: beyond EOF of "
                       + fileName());
    }
    return itsPtr+offset;
  }

  void* MMapfdIO::getWritePointer (Int64 offset)
  {
    if (!itsIsWritable) {
      throw AipsError ("MMapfdIO file " + fileName() + " is not writable");
    }
    if (offset >= itsFileSize) {
      throw AipsError ("MMapfdIO::getWritePointer: beyond EOF of "
                       + fileName());
    }
    return itsPtr+offset;
  }

} // end namespace
