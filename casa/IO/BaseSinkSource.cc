//# BaseSinkSource.cc: Shared base class for ByteSink and ByteSource
//# Copyright (C) 1996,1999,2001
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

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BaseSinkSource.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

BaseSinkSource::BaseSinkSource()
: itsTypeIO ()
{}

BaseSinkSource::BaseSinkSource (TypeIO* typeIO, Bool takeOver)
: itsTypeIO (typeIO, takeOver)
{}

BaseSinkSource::BaseSinkSource (const BaseSinkSource& sinkSource)
: itsTypeIO (sinkSource.itsTypeIO)
{}

BaseSinkSource& BaseSinkSource::operator= (const BaseSinkSource& sinkSource)
{
    if (this != &sinkSource) {
	itsTypeIO = sinkSource.itsTypeIO;
    }
    return *this;
}

BaseSinkSource::~BaseSinkSource()
{}


TypeIO& BaseSinkSource::typeIO()
{
    return *itsTypeIO;
}

const TypeIO& BaseSinkSource::typeIO() const
{
    return *itsTypeIO;
}

Int64 BaseSinkSource::seek (Int64 offset, ByteIO::SeekOption option)
{
    return itsTypeIO->seek (offset, option);
}
Int64 BaseSinkSource::seek (Int offset, ByteIO::SeekOption option)
{
    return itsTypeIO->seek (offset, option);
}

Bool BaseSinkSource::isReadable() const
{
    return itsTypeIO->isReadable();
}

Bool BaseSinkSource::isWritable() const
{
    return itsTypeIO->isWritable();
}

Bool BaseSinkSource::isSeekable() const
{
    return itsTypeIO->isSeekable();
}

Bool BaseSinkSource::isNull() const
{
    return itsTypeIO.null();
}

} //# NAMESPACE CASACORE - END

