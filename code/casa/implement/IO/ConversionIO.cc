//# ConversionIO.cc: Class for IO in a converted format
//# Copyright (C) 1996,1999
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
#include <aips/IO/ConversionIO.h>
#include <aips/OS/DataConversion.h>
#include <aips/IO/ByteIO.h>


ConversionIO::ConversionIO (DataConversion* dataConversion,
			    ByteIO* byteIO, uInt bufferLength, Bool takeOver)
: TypeIO          (byteIO, takeOver),
  itsConversion   (dataConversion, takeOver),
  itsBuffer       (new char[bufferLength]),
  itsBufferLength (bufferLength)
{
    init();
}

ConversionIO::ConversionIO (const ConversionIO& that)
: TypeIO          (that), 
  itsConversion   (that.itsConversion),
  itsBuffer       (new char[that.itsBufferLength]),
  itsBufferLength (that.itsBufferLength)
{
    init();
}

ConversionIO& ConversionIO::operator= (const ConversionIO& that)
{
    if (this != &that) {
	TypeIO::operator= (that);
	itsConversion = that.itsConversion;
	if (itsBufferLength != that.itsBufferLength) {
	    delete [] itsBuffer;
	    itsBufferLength = that.itsBufferLength;
	    itsBuffer = new char[itsBufferLength];
	}
	init();
    }
    return *this;
}

ConversionIO::~ConversionIO()
{
    delete [] itsBuffer;
}


void ConversionIO::init()
{
    itsCopyChar   = itsConversion->canCopy (static_cast<Char*>(0));
    itsCopyuChar  = itsConversion->canCopy (static_cast<uChar*>(0));
    itsCopyShort  = itsConversion->canCopy (static_cast<Short*>(0));
    itsCopyuShort = itsConversion->canCopy (static_cast<uShort*>(0));
    itsCopyInt    = itsConversion->canCopy (static_cast<Int*>(0));
    itsCopyuInt   = itsConversion->canCopy (static_cast<uInt*>(0));
    itsCopyLong   = itsConversion->canCopy (static_cast<Long*>(0));
    itsCopyuLong  = itsConversion->canCopy (static_cast<uLong*>(0));
    itsCopyFloat  = itsConversion->canCopy (static_cast<Float*>(0));
    itsCopyDouble = itsConversion->canCopy (static_cast<Double*>(0));
    itsSizeChar   = itsConversion->externalSize (static_cast<Char*>(0));
    itsSizeuChar  = itsConversion->externalSize (static_cast<uChar*>(0));
    itsSizeShort  = itsConversion->externalSize (static_cast<Short*>(0));
    itsSizeuShort = itsConversion->externalSize (static_cast<uShort*>(0));
    itsSizeInt    = itsConversion->externalSize (static_cast<Int*>(0));
    itsSizeuInt   = itsConversion->externalSize (static_cast<uInt*>(0));
    itsSizeLong   = itsConversion->externalSize (static_cast<Long*>(0));
    itsSizeuLong  = itsConversion->externalSize (static_cast<uLong*>(0));
    itsSizeFloat  = itsConversion->externalSize (static_cast<Float*>(0));
    itsSizeDouble = itsConversion->externalSize (static_cast<Double*>(0));
}


uInt ConversionIO::write (uInt nvalues, const Bool* value)
{
    return TypeIO::write (nvalues, value);
}

uInt ConversionIO::write (uInt nvalues, const Complex* value)
{
    return TypeIO::write (nvalues, value);
}

uInt ConversionIO::write (uInt nvalues, const DComplex* value)
{
    return TypeIO::write (nvalues, value);
}

uInt ConversionIO::write (uInt nvalues, const String* value)
{
    return TypeIO::write (nvalues, value);
}

uInt ConversionIO::read (uInt nvalues, Bool* value)
{
    return TypeIO::read (nvalues, value);
}

uInt ConversionIO::read (uInt nvalues, Complex* value)
{
    return TypeIO::read (nvalues, value);
}

uInt ConversionIO::read (uInt nvalues, DComplex* value)
{
    return TypeIO::read (nvalues, value);
}

uInt ConversionIO::read (uInt nvalues, String* value)
{
    return TypeIO::read (nvalues, value);
}


#define CONVERSIONIO_DOIT(T) \
uInt ConversionIO::write (uInt nvalues, const T* value) \
{ \
    uInt size = nvalues * aips_name2(itsSize,T); \
    if (aips_name2(itsCopy,T)) { \
	itsByteIO->write (size, value); \
    } else { \
	if (size <= itsBufferLength) { \
	    itsConversion->fromLocal (itsBuffer, value, nvalues); \
	    itsByteIO->write (size, itsBuffer); \
    	} else { \
	    char* tempBuffer = new char [size]; \
	    itsConversion->fromLocal (tempBuffer, value, nvalues); \
	    itsByteIO->write (size, tempBuffer); \
	    delete [] tempBuffer; \
	} \
    } \
    return size; \
} \
uInt ConversionIO::read (uInt nvalues, T* value) \
{ \
    uInt size = nvalues * aips_name2(itsSize,T); \
    if (aips_name2(itsCopy,T)) { \
	itsByteIO->read (size, value); \
    } else { \
	if (size <= itsBufferLength) { \
	    itsByteIO->read (size, itsBuffer); \
	    itsConversion->toLocal (value, itsBuffer, nvalues); \
	} else { \
	    char* tempBuffer = new char[size]; \
	    itsByteIO->read (size, tempBuffer); \
	    itsConversion->toLocal (value, tempBuffer, nvalues); \
	    delete [] tempBuffer; \
	} \
    } \
    return size; \
}


CONVERSIONIO_DOIT(Char)
CONVERSIONIO_DOIT(uChar)
CONVERSIONIO_DOIT(Short)
CONVERSIONIO_DOIT(uShort)
CONVERSIONIO_DOIT(Int)
CONVERSIONIO_DOIT(uInt)
CONVERSIONIO_DOIT(Long)
CONVERSIONIO_DOIT(uLong)
CONVERSIONIO_DOIT(Float)
CONVERSIONIO_DOIT(Double)
