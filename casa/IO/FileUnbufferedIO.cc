//# FileUnbufferedIO.cc: Class for unbuffered IO on a file
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

//# Includes
#include <casacore/casa/IO/FileUnbufferedIO.h>
#include <casacore/casa/IO/RegularFileIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  FileUnbufferedIO::FileUnbufferedIO (const RegularFile& fileName,
                                      ByteIO::OpenOption option,
                                      Bool useODirect)
    : itsUseODirect (useODirect)
  {
    attach (RegularFileIO::openCreate (fileName, option, useODirect),
            fileName.path().originalName());
  }

  FileUnbufferedIO::~FileUnbufferedIO()
  {
    close (fd());
    detach();
  }

  void FileUnbufferedIO::reopenRW()
  {
    if (isWritable()) {
      return;
    }
    // First try if the file can be opened as read/write.
    // An exception is thrown if not possible.
    int newfd = RegularFileIO::openCreate (fileName(), ByteIO::Update,
                                           itsUseODirect);
    // Now close the readonly file and reset fd.
    close (fd());
    detach();
    attach (newfd, fileName());
    setWritable();
  }


} //# NAMESPACE CASACORE - END
