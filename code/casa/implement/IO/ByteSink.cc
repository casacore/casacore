//# ByteSink.cc: Class for write-only access to data in a given format
//# Copyright (C) 1996,1998
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
#include <aips/IO/ByteSink.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>


ByteSink::ByteSink()
{}

ByteSink::ByteSink (TypeIO* typeIO)
: BaseSinkSource (typeIO)
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

ByteSink& ByteSink::operator<< (Long value)
{
    itsTypeIO->write (1, &value);
    return *this;
}

ByteSink& ByteSink::operator<< (uLong value)
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


void ByteSink::write (uInt nvalues, const Bool* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const Char* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const uChar* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const Short* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const uShort* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const Int* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const uInt* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const Long* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const uLong* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const Float* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const Double* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const Complex* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const DComplex* value)
{
    itsTypeIO->write (nvalues, value);
}

void ByteSink::write (uInt nvalues, const String* value)
{
    itsTypeIO->write (nvalues, value);
}
