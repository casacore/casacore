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

#include <casacore/casa/IO/ByteSink.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ByteSink::ByteSink()
{}

ByteSink::ByteSink (TypeIO* typeIO, bool takeOver)
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


ByteSink& ByteSink::operator<< (bool value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (char value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (unsigned char value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (int16_t value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uint16_t value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (int32_t value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uint32_t value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (int64_t value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uint64_t value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (float value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (double value)
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

ByteSink& ByteSink::operator<< (const char* value)
{
    String str(value);
    itsTypeIO->write (1, &str);
    return *this;
}


void ByteSink::write (size_t nvalues, const bool* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const char* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const unsigned char* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const int16_t* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const uint16_t* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const int32_t* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const uint32_t* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const int64_t* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const uint64_t* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const float* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (size_t nvalues, const double* value)
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

