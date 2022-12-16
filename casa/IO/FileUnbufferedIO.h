//# FileUnbufferedIO.h: Class for unbuffered IO on a file
//# Copyright (C) 2022
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

#ifndef CASA_FILEUNBUFFEREDIO_H
#define CASA_FILEUNBUFFEREDIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/OS/RegularFile.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class for unbuffered IO on a file.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class
//    <li> file descriptors
// </prerequisite>

// <synopsis> 
// This class is a specialization of class
// <linkto class=ByteIO>FiledesIO</linkto> to make it easier to open a file.
// It reads/writes the data directly instead of using a buffer to reduce the
// number of IO-operations.
// It is meant for file access using large reads and writes as done by MultiFile.
//
// Optionally the file can be used with O_DIRECT to bypass the system's file cache
// for more predictable IO behaviour which can be of importance in
// real-time applications.
// </synopsis>

// <motivation> 
// Make it possible to use the Casacore IO functionality on any file.
// In this way any device can be hooked to the IO framework.
// </motivation>


class FileUnbufferedIO: public FiledesIO
{
public: 
  // The constructor opens or creates a file.
  // For option NewNoReplace it is checked if the file does not exist yet.
  // If useODirect=True and if supported by the OS, the file will be opened
  // with O_DIRECT which bypasses the kernel's file cache for more predictable
  // I/O behaviour. It requires the size and the alignment of the data read
  // or written to be a multiple of the the disk's logical block size.
  explicit FileUnbufferedIO (const RegularFile& fileName,
                             ByteIO::OpenOption option,
                             Bool useODirect=False);

  // The destructor closes the file.
  ~FileUnbufferedIO() override;

  // Copy constructor and assignment cannot be used.
  FileUnbufferedIO (const FileUnbufferedIO&) = delete;
  FileUnbufferedIO& operator= (const FileUnbufferedIO&) = delete;

  // Reopen the file for read/write.
  // Nothing is done if already opened for read/write.
  // An exception is thrown if the file is readonly.
  void reopenRW() override;

private:
  Bool itsUseODirect;
};


} //# NAMESPACE CASACORE - END

#endif
