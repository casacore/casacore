//# LargeRegularFileIO.cc: Class for IO on a regular large file
//# Copyright (C) 1996,1997,1998,1999,2001,2002
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

#include <casa/aips.h>
#include <casa/IO/LargeIOFuncDef.h>
#include <casa/IO/LargeRegularFileIO.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <fcntl.h>
#include <errno.h>                    // needed for errno
#include <casa/string.h>               // needed for strerror


namespace casa { //# NAMESPACE CASA - BEGIN

LargeRegularFileIO::LargeRegularFileIO (const RegularFile& regularFile,
					ByteIO::OpenOption option,
					uInt bufferSize)
: itsOption      (option),
  itsRegularFile (regularFile)
{
    int file = openCreate (regularFile, option);
    attach (file, (bufferSize == 0 ? 16384 : bufferSize));
    // If appending, set the stream offset to the file length.
    if (option == ByteIO::Append) {
        seek (length());
    }
}

LargeRegularFileIO::~LargeRegularFileIO()
{
    detach (True);
    if (itsOption == ByteIO::Scratch  ||  itsOption == ByteIO::Delete) {
	itsRegularFile.remove();
    }
}

int LargeRegularFileIO::openCreate (const RegularFile& file,
                                    ByteIO::OpenOption option)
{
    const String& name = file.path().expandedName();
    Bool create = False;
    Int stropt;
    switch (option) {
    case ByteIO::Old:
	stropt = O_RDONLY;
	break;
    case ByteIO::NewNoReplace:
	if (file.exists()) {
	    throw (AipsError ("LargeRegularFileIO: new file " + name +
			      " already exists"));
	}
    case ByteIO::New:
    case ByteIO::Scratch:
        create = True;
	stropt = O_RDWR | O_CREAT | O_TRUNC;
	break;
    case ByteIO::Append:
	stropt = O_RDWR;
	break;
    case ByteIO::Update:
    case ByteIO::Delete:
	stropt = O_RDWR;
	break;
    default:
	throw (AipsError ("LargeRegularFileIO: unknown open option"));
    }
    // Open the file.
    int fd;
    if (create) {
      fd = trace3OPEN ((char*)name.chars(), stropt, 0644);
    } else {
      fd = trace2OPEN ((char*)name.chars(), stropt);
    }
    if (fd < 0) {
	throw (AipsError ("LargeRegularFileIO: error in open or create of file " +
			  name + ": " + strerror(errno)));
    }
    return fd;
}

void LargeRegularFileIO::reopenRW()
{
    if (isWritable()) {
	return;
    }
    // First try if the file can be opened as read/write.
    const String& name = itsRegularFile.path().expandedName();
    int file = trace2OPEN ((char *)name.chars(), O_RDWR);
    if (file < 0) {
	throw (AipsError ("LargeRegularFileIO::reopenRW "
			  "not possible for file " +
			  name + ": " + strerror(errno)));
    }
    uInt bufsize = bufferSize();
    detach (True);
    attach (file, bufsize);
    // It can be reopened, so close and reopen.
    itsOption = ByteIO::Update;
}


String LargeRegularFileIO::fileName() const
{
    return itsRegularFile.path().expandedName();
}

} //# NAMESPACE CASA - END

