//# MMapIO.h: Memory-mapped IO on a file
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

#ifndef CASA_MMAPIO_H
#define CASA_MMAPIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/MMapfdIO.h>
#include <casacore/casa/OS/RegularFile.h>

namespace casacore
{

// <summary>
// Memory-mapped IO on a file.
// </summary>

// <synopsis>
// Memory-mapped IO lets the OS take care of caching file segments.
// This is particularly useful for the Tiled Storage Manager which keeps a
// cache of tiles. When using memory-mapped IO it does not need to do that
// anymore.
//
// On 32-bit systems its use is limited because for large files the 4 GB
// memory space is insufficient. However, for 64-bit systems the memory
// space is large enough to make use of it.
//
// In the general case there is direct access to the mapped file space.
// The read and write methods copies the data into/from a buffer.
// However, to avoid the copying it is possible to get a direct pointer
// to the mapped data. This should be used with care, because writing to
// it will cause a segmentation if the file is readonly. If the file is
// writable, writing into the mapped data segment means changing the file
// contents.
// </synopsis>

class MMapIO: public MMapfdIO
{
public:
  // Open the given file and map it entirely into memory with read access.
  // The map has write access if the file is opened for write.
  explicit MMapIO (const RegularFile& regularFile,
                   ByteIO::OpenOption = ByteIO::Old);

  // Destructor.
  // It will flush and unmap the file.
  ~MMapIO();

private:
  // Forbid copy constructor and assignment
  // <group>
  MMapIO (const MMapIO&);
  MMapIO& operator= (const MMapIO&);
  // </group>
};

} // end namespace

#endif
