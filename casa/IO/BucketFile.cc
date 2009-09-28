//# BucketFile.cc: Tiled Hypercube Storage Manager for tables
//# Copyright (C) 1995,1996,1999,2001,2002
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


//# Includes
#include <casa/IO/LargeIOFuncDef.h>
#include <casa/IO/BucketFile.h>
#include <casa/OS/Path.h>
#include <casa/OS/DOos.h>
#include <casa/Exceptions/Error.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>                // needed for errno
#include <casa/string.h>          // needed for strerror

#if defined(AIPS_DARWIN)
#undef trace3OPEN
#define trace3OPEN open
#undef trace2OPEN
#define trace2OPEN open
#undef traceLSEEK
#define traceLSEEK lseek
#endif

namespace casa { //# NAMESPACE CASA - BEGIN

BucketFile::BucketFile (const String& fileName)
: name_p       (Path(fileName).expandedName()),
  isWritable_p (True),
  fd_p         (-1)
{
    // Create the file.
    fd_p = ::trace3OPEN ((Char *)name_p.chars(), O_RDWR | O_CREAT | O_TRUNC, 0644);
    if (fd_p < 0) {
	throw (AipsError ("BucketFile: create error on file " + name_p +
			  ": " + strerror(errno)));
    }
}

BucketFile::BucketFile (const String& fileName, Bool isWritable)
: name_p       (Path(fileName).expandedName()),
  isWritable_p (isWritable),
  fd_p         (-1)
{}

BucketFile::~BucketFile()
{
    close();
}


void BucketFile::close()
{
    if (fd_p >= 0) {
	::traceCLOSE (fd_p);
	fd_p = -1;
    }
}


void BucketFile::open()
{
    if (fd_p < 0) {
	if (isWritable_p) {
	    fd_p = ::trace2OPEN ((Char *)name_p.chars(), O_RDWR);
	}else{
	    fd_p = ::trace2OPEN ((Char *)name_p.chars(), O_RDONLY);
	}
	if (fd_p == -1) {
	    throw (AipsError ("BucketFile: open error on file " + name_p +
			      ": " + strerror(errno)));
	}
    }
}


void BucketFile::remove()
{
    close();
    DOos::remove (name_p, False, False);
}


void BucketFile::fsync()
{
    if (fd_p >= 0) {
	::fsync (fd_p);
    }
}


void BucketFile::setRW()
{
    // Exit if already writable.
    if (isWritable_p) {
	return;
    }
    // Try to reopen the file as read/write.
    // Throw an exception if it fails.
    if (fd_p >= 0) {
	int fd = ::trace2OPEN ((Char *)name_p.chars(), O_RDWR);
	if (fd == -1) {
	    throw (AipsError ("BucketFile: reopenRW error on file " + name_p +
			      ": " + strerror(errno)));
	}
	::traceCLOSE (fd_p);
	fd_p = fd;
    }
    isWritable_p = True;
}


uInt BucketFile::read (void* buffer, uInt length) const
{
    if (::traceREAD (fd_p, (Char *)buffer, length)  !=  Int(length)) {
	throw (AipsError ("BucketFile: read error on file " + name_p +
	                  ": " + strerror(errno)));
    }
    return length;
}

uInt BucketFile::write (const void* buffer, uInt length)
{
    if (::traceWRITE (fd_p, (Char *)buffer, length)  !=  Int(length)) {
	throw (AipsError ("BucketFile: write error on file " + name_p +
	                  ": " + strerror(errno)));
    }
    return length;
}

void BucketFile::seek (Int64 offset) const
    { ::traceLSEEK (fd_p, offset, SEEK_SET); }

Int64 BucketFile::fileSize () const
    { return ::traceLSEEK (fd_p, 0, SEEK_END); }

} //# NAMESPACE CASA - END

