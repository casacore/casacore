//# ByteIO.cc: Abstract base class for IO on a byte stream
//# Copyright (C) 1996,2001
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

#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ByteIO::~ByteIO()
{}


void ByteIO::reopenRW()
{
    if (! isWritable()) {
	throw (AipsError ("ByteIO: reopenRW is not possible"));
    }
}

void ByteIO::pwrite (int64_t size, int64_t offset, const void* buf)
{
    int64_t cur = doSeek(0, ByteIO::Current);
    doSeek(offset, ByteIO::Begin);
    try {
        write(size, buf);
    }
    catch (...) {
        doSeek(cur, ByteIO::Begin);
        throw;
    }
    doSeek(cur, ByteIO::Begin);
}

int64_t ByteIO::pread (int64_t size, int64_t offset, void* buf,
                     bool throwException)
{
    int64_t r = -1;
    int64_t cur = doSeek(0, ByteIO::Current);
    doSeek(offset, ByteIO::Begin);
    try {
        r = read(size, buf, throwException);
    }
    catch (...) {
        doSeek(cur, ByteIO::Begin);
        throw;
    }
    doSeek(cur, ByteIO::Begin);
    return r;
}

void ByteIO::flush()
{}

void ByteIO::fsync()
{}

void ByteIO::resync()
{}

void ByteIO::truncate (int64_t)
{}

String ByteIO::fileName() const
{
  return String();
}


} //# NAMESPACE CASACORE - END

