//# FileUnbufferedIO.h: Class for unbuffered IO on a file
//# Copyright (C) 2019
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
// <linkto class=ByteIO>ByteIO</linkto>. It uses a file descriptor
// to read/write data.
// <p>
// The file associated with the file descriptor has to be opened
// before hand.
// The constructor will determine automatically if the file is
// readable, writable and seekable.
// Note that on destruction the file descriptor is NOT closed.
// </synopsis>

// <example>
// This example shows how FiledesIO can be used with an fd.
// It uses the fd for a regular file, which could be done in an easier
// way using class <linkto class=RegularFileIO>RegularFileIO</linkto>.
// However, when using pipes or sockets, this would be the only way.
// <srcblock>
//    // Get a file descriptor for the file.
//    int fd = open ("file.name");
//    // Use that as the source of AipsIO (which will also use CanonicalIO).
//    FiledesIO fio (fd);
//    AipsIO stream (&fio);
//    // Read the data.
//    Int vali;
//    Bool valb;
//    stream >> vali >> valb;
// </srcblock>
// </example>

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
  FileUnbufferedIO (const RegularFile& fileName, ByteIO::OpenOption option,
                    Bool useODirect=False);

  // The destructor closes the file.
  virtual ~FileUnbufferedIO();

  // Reopen the file for read/write.
  // Nothing is done if already opened for read/write.
  // An exception is thrown if the file is readonly.
  virtual void reopenRW();

private:
  Bool itsUseODirect;

  // Copy constructor, should not be used.
  FileUnbufferedIO (const FileUnbufferedIO& that);

  // Assignment, should not be used.
  FileUnbufferedIO& operator= (const FileUnbufferedIO& that);
};


} //# NAMESPACE CASACORE - END

#endif
