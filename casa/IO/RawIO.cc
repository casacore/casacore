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

#include <casacore/casa/IO/RawIO.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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


size_t RawIO::write (size_t nvalues, const Bool* value)
{
    return TypeIO::write (nvalues, value);
}

size_t RawIO::write (size_t nvalues, const Char* value)
{
    itsByteIO->write (nvalues * sizeof(Char), (void*) value);
    return nvalues * sizeof(Char);
}

size_t RawIO::write (size_t nvalues, const uChar* value)
{
    itsByteIO->write (nvalues * sizeof(uChar), (void*) value);
    return nvalues * sizeof(uChar);
}

size_t RawIO::write (size_t nvalues, const Short* value)
{
    itsByteIO->write (nvalues * sizeof(Short), (void*) value);
    return nvalues * sizeof(Short);
}

size_t RawIO::write (size_t nvalues, const uShort* value)
{
    itsByteIO->write (nvalues * sizeof(uShort), (void*) value);
    return nvalues * sizeof(uShort);
}

size_t RawIO::write (size_t nvalues, const Int* value)
{
    itsByteIO->write (nvalues * sizeof(Int), (void*) value);
    return nvalues * sizeof(Int);
}

size_t RawIO::write (size_t nvalues, const uInt* value)
{
    itsByteIO->write (nvalues * sizeof(uInt), (void*) value);
    return nvalues * sizeof(uInt);
}

size_t RawIO::write (size_t nvalues, const Int64* value)
{
    itsByteIO->write (nvalues * sizeof(Int64), (void*) value);
    return nvalues * sizeof(Int64);
}

size_t RawIO::write (size_t nvalues, const uInt64* value)
{
    itsByteIO->write (nvalues * sizeof(uInt64), (void*) value);
    return nvalues * sizeof(uInt64);
}

size_t RawIO::write (size_t nvalues, const Float* value)
{
    itsByteIO->write (nvalues * sizeof(Float), (void*) value);
    return nvalues * sizeof(Float);
}

size_t RawIO::write (size_t nvalues, const Double* value)
{
    itsByteIO->write (nvalues * sizeof(Double), (void*) value);
    return nvalues * sizeof(Double);
}

size_t RawIO::write (size_t nvalues, const Complex* value)
{
    return TypeIO::write (nvalues, value);
}

size_t RawIO::write (size_t nvalues, const DComplex* value)
{
    return TypeIO::write (nvalues, value);
}

size_t RawIO::write (size_t nvalues, const String* value)
{
    return TypeIO::write (nvalues, value);
}


size_t RawIO::read (size_t nvalues, Bool* value)
{
    return TypeIO::read (nvalues, value);
}

size_t RawIO::read (size_t nvalues, Char* value)
{
    itsByteIO->read (nvalues * sizeof(Char), value);
    return nvalues * sizeof(Char);
}

size_t RawIO::read (size_t nvalues, uChar* value)
{
    itsByteIO->read (nvalues * sizeof(uChar), value);
    return nvalues * sizeof(uChar);
}

size_t RawIO::read (size_t nvalues, Short* value)
{
    itsByteIO->read (nvalues * sizeof(Short), value);
    return nvalues * sizeof(Short);
}

size_t RawIO::read (size_t nvalues, uShort* value)
{
    itsByteIO->read (nvalues * sizeof(uShort), value);
    return nvalues * sizeof(uShort);
}

size_t RawIO::read (size_t nvalues, Int* value)
{
    itsByteIO->read (nvalues * sizeof(Int), value);
    return nvalues * sizeof(Int);
}

size_t RawIO::read (size_t nvalues, uInt* value)
{
    itsByteIO->read (nvalues * sizeof(uInt), value);
    return nvalues * sizeof(uInt);
}

size_t RawIO::read (size_t nvalues, Int64* value)
{
    itsByteIO->read (nvalues * sizeof(Int64), value);
    return nvalues * sizeof(Int64);
}

size_t RawIO::read (size_t nvalues, uInt64* value)
{
    itsByteIO->read (nvalues * sizeof(uInt64), value);
    return nvalues * sizeof(uInt64);
}

size_t RawIO::read (size_t nvalues, Float* value)
{
    itsByteIO->read (nvalues * sizeof(Float), value);
    return nvalues * sizeof(Float);
}

size_t RawIO::read (size_t nvalues, Double* value)
{
    itsByteIO->read (nvalues * sizeof(Double), value);
    return nvalues * sizeof(Double);
}

size_t RawIO::read (size_t nvalues, Complex* value)
{
    return TypeIO::read (nvalues, value);
}

size_t RawIO::read (size_t nvalues, DComplex* value)
{
    return TypeIO::read (nvalues, value);
}

size_t RawIO::read (size_t nvalues, String* value)
{
    return TypeIO::read (nvalues, value);
}

} //# NAMESPACE CASACORE - END

