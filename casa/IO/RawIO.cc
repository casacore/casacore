//# RawIO.cc: Class for IO in local format
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

#include <casa/IO/RawIO.h>
#include <casa/IO/ByteIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

RawIO::RawIO (ByteIO* byteIO, Bool takeOver)
: TypeIO (byteIO, takeOver)
{}

RawIO::RawIO (const RawIO& that)
: TypeIO (that)
{}

RawIO& RawIO::operator= (const RawIO& that)
{
    if (this != &that) {
	TypeIO::operator= (that);
    }
    return *this;
}

RawIO::~RawIO()
{}


uInt RawIO::write (uInt nvalues, const Bool* value)
{
    return TypeIO::write (nvalues, value);
}

uInt RawIO::write (uInt nvalues, const Char* value)
{
    itsByteIO->write (nvalues * sizeof(Char), (void*) value);
    return nvalues * sizeof(Char);
}

uInt RawIO::write (uInt nvalues, const uChar* value)
{
    itsByteIO->write (nvalues * sizeof(uChar), (void*) value);
    return nvalues * sizeof(uChar);
}

uInt RawIO::write (uInt nvalues, const Short* value)
{
    itsByteIO->write (nvalues * sizeof(Short), (void*) value);
    return nvalues * sizeof(Short);
}

uInt RawIO::write (uInt nvalues, const uShort* value)
{
    itsByteIO->write (nvalues * sizeof(uShort), (void*) value);
    return nvalues * sizeof(uShort);
}

uInt RawIO::write (uInt nvalues, const Int* value)
{
    itsByteIO->write (nvalues * sizeof(Int), (void*) value);
    return nvalues * sizeof(Int);
}

uInt RawIO::write (uInt nvalues, const uInt* value)
{
    itsByteIO->write (nvalues * sizeof(uInt), (void*) value);
    return nvalues * sizeof(uInt);
}

uInt RawIO::write (uInt nvalues, const Int64* value)
{
    itsByteIO->write (nvalues * sizeof(Int64), (void*) value);
    return nvalues * sizeof(Int64);
}

uInt RawIO::write (uInt nvalues, const uInt64* value)
{
    itsByteIO->write (nvalues * sizeof(uInt64), (void*) value);
    return nvalues * sizeof(uInt64);
}

uInt RawIO::write (uInt nvalues, const Float* value)
{
    itsByteIO->write (nvalues * sizeof(Float), (void*) value);
    return nvalues * sizeof(Float);
}

uInt RawIO::write (uInt nvalues, const Double* value)
{
    itsByteIO->write (nvalues * sizeof(Double), (void*) value);
    return nvalues * sizeof(Double);
}

uInt RawIO::write (uInt nvalues, const Complex* value)
{
    return TypeIO::write (nvalues, value);
}

uInt RawIO::write (uInt nvalues, const DComplex* value)
{
    return TypeIO::write (nvalues, value);
}

uInt RawIO::write (uInt nvalues, const String* value)
{
    return TypeIO::write (nvalues, value);
}


uInt RawIO::read (uInt nvalues, Bool* value)
{
    return TypeIO::read (nvalues, value);
}

uInt RawIO::read (uInt nvalues, Char* value)
{
    itsByteIO->read (nvalues * sizeof(Char), value);
    return nvalues * sizeof(Char);
}

uInt RawIO::read (uInt nvalues, uChar* value)
{
    itsByteIO->read (nvalues * sizeof(uChar), value);
    return nvalues * sizeof(uChar);
}

uInt RawIO::read (uInt nvalues, Short* value)
{
    itsByteIO->read (nvalues * sizeof(Short), value);
    return nvalues * sizeof(Short);
}

uInt RawIO::read (uInt nvalues, uShort* value)
{
    itsByteIO->read (nvalues * sizeof(uShort), value);
    return nvalues * sizeof(uShort);
}

uInt RawIO::read (uInt nvalues, Int* value)
{
    itsByteIO->read (nvalues * sizeof(Int), value);
    return nvalues * sizeof(Int);
}

uInt RawIO::read (uInt nvalues, uInt* value)
{
    itsByteIO->read (nvalues * sizeof(uInt), value);
    return nvalues * sizeof(uInt);
}

uInt RawIO::read (uInt nvalues, Int64* value)
{
    itsByteIO->read (nvalues * sizeof(Int64), value);
    return nvalues * sizeof(Int64);
}

uInt RawIO::read (uInt nvalues, uInt64* value)
{
    itsByteIO->read (nvalues * sizeof(uInt64), value);
    return nvalues * sizeof(uInt64);
}

uInt RawIO::read (uInt nvalues, Float* value)
{
    itsByteIO->read (nvalues * sizeof(Float), value);
    return nvalues * sizeof(Float);
}

uInt RawIO::read (uInt nvalues, Double* value)
{
    itsByteIO->read (nvalues * sizeof(Double), value);
    return nvalues * sizeof(Double);
}

uInt RawIO::read (uInt nvalues, Complex* value)
{
    return TypeIO::read (nvalues, value);
}

uInt RawIO::read (uInt nvalues, DComplex* value)
{
    return TypeIO::read (nvalues, value);
}

uInt RawIO::read (uInt nvalues, String* value)
{
    return TypeIO::read (nvalues, value);
}

} //# NAMESPACE CASA - END

