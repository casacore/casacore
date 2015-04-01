//# ByteSink.cc: Class for write-only access to data in a given format
//# Copyright (C) 1996,1998,1999,2001
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

#include <casacore/casa/IO/ByteSink.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ByteSink::ByteSink()
{}

ByteSink::ByteSink (TypeIO* typeIO, Bool takeOver)
: BaseSinkSource (typeIO, takeOver)
{
    if (!isWritable()) {
	throw (AipsError ("ByteSink is not writable"));
    }
}

ByteSink::ByteSink (const ByteSink& sink)
: BaseSinkSource (sink)
{}

ByteSink& ByteSink::operator= (const ByteSink& sink)
{
    BaseSinkSource::operator= (sink);
    return *this;
}

ByteSink::~ByteSink()
{}


ByteSink& ByteSink::operator<< (Bool value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (Char value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uChar value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (Short value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uShort value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (Int value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uInt value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (Int64 value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uInt64 value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (Float value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (Double value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (const Complex& value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (const DComplex& value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (const String& value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (const Char* value)
{
    String str(value);
    itsTypeIO->write (1, &str);
    return *this;
}


void ByteSink::write (size_t nvalues, const Bool* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const Char* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const uChar* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const Short* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const uShort* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const Int* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const uInt* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const Int64* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const uInt64* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const Float* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const Double* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const Complex* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const DComplex* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const String* value)
{
    itsTypeIO->write (nvalues, value);
}

} //# NAMESPACE CASACORE - END

