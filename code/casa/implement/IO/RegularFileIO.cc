//# RegularFileIO.cc: Class for IO on a regular file
//# Copyright (C) 1996
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

#include <aips/aips.h>
#include <aips/IO/RegularFileIO.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>


RegularFileIO::RegularFileIO (const RegularFile& regularFile,
			      ByteIO::OpenOption option, uInt bufferSize)
: itsOption      (option),
  itsRegularFile (regularFile)
{
    // Open file as input and/or output.
    Bool writable = True;
    int stropt;
    switch (option) {
    case ByteIO::Old:
	writable = False;
	stropt = ios::in|ios::nocreate;
	break;
    case ByteIO::New:
    case ByteIO::Scratch:
	stropt = ios::in|ios::out|ios::trunc;
	break;
    case ByteIO::NewNoReplace:
	stropt = ios::in|ios::out|ios::noreplace|ios::trunc;
	break;
    case ByteIO::Append:
	stropt = ios::in|ios::out|ios::app;
	break;
    case ByteIO::Update:
    case ByteIO::Delete:
	stropt = ios::in|ios::out|ios::nocreate;
	break;
    default:
	throw (AipsError ("RegularFileIO: unknown open option"));
    }
    prepareAttach (bufferSize, True, writable);
    // Open the file.
    const String& name = itsRegularFile.path().expandedName();
    if (getFilebuf()->open (name.chars(), stropt) == 0) {
	throw (AipsError ("RegularFileIO: error in open or create of file " +
			  name));
    }
    fillSeekable();
}

RegularFileIO::~RegularFileIO()
{
    getFilebuf()->close();
    detach();
    if (itsOption == ByteIO::Scratch  ||  itsOption == ByteIO::Delete) {
	itsRegularFile.remove();
    }
}


void RegularFileIO::reopenRW()
{
    if (isWritable()) {
	return;
    }
    // First try if the file can be opened as read/write.
    const String& name = itsRegularFile.path().expandedName();
    int fd = ::open (name.chars(), O_RDWR);
    if (fd == -1) {
	throw (AipsError ("RegularFileIO: reopenRW not possible for file " +
			  name));
    }
    ::close (fd);
    // It can be reopened, so close and reopen.
    getFilebuf()->close();
    AlwaysAssert (getFilebuf()->open (name.chars(),
				      ios::in|ios::out|ios::nocreate) != 0,
		  AipsError);
    setWritable();
    itsOption = ByteIO::Update;
}


String RegularFileIO::fileName() const
{
    return itsRegularFile.path().expandedName();
}
