//# ConversionIO.cc: Class for IO in a converted format
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

//# Includes
#include <casacore/casa/IO/ConversionIO.h>
#include <casacore/casa/OS/DataConversion.h>
#include <casacore/casa/IO/ByteIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ConversionIO::ConversionIO (DataConversion* dataConversion,
			    ByteIO* byteIO, uint32_t bufferLength, bool takeOver)
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
    itsCopyChar   = itsConversion->canCopy (static_cast<char*>(0));
    itsCopyuChar  = itsConversion->canCopy (static_cast<unsigned char*>(0));
    itsCopyShort  = itsConversion->canCopy (static_cast<int16_t*>(0));
    itsCopyuShort = itsConversion->canCopy (static_cast<uint16_t*>(0));
    itsCopyInt    = itsConversion->canCopy (static_cast<int32_t*>(0));
    itsCopyuInt   = itsConversion->canCopy (static_cast<uint32_t*>(0));
    itsCopyInt64  = itsConversion->canCopy (static_cast<int64_t*>(0));
    itsCopyuInt64 = itsConversion->canCopy (static_cast<uint64_t*>(0));
    itsCopyFloat  = itsConversion->canCopy (static_cast<float*>(0));
    itsCopyDouble = itsConversion->canCopy (static_cast<double*>(0));
    itsSizeChar   = itsConversion->externalSize (static_cast<char*>(0));
    itsSizeuChar  = itsConversion->externalSize (static_cast<unsigned char*>(0));
    itsSizeShort  = itsConversion->externalSize (static_cast<int16_t*>(0));
    itsSizeuShort = itsConversion->externalSize (static_cast<uint16_t*>(0));
    itsSizeInt    = itsConversion->externalSize (static_cast<int32_t*>(0));
    itsSizeuInt   = itsConversion->externalSize (static_cast<uint32_t*>(0));
    itsSizeInt64  = itsConversion->externalSize (static_cast<int64_t*>(0));
    itsSizeuInt64 = itsConversion->externalSize (static_cast<uint64_t*>(0));
    itsSizeFloat  = itsConversion->externalSize (static_cast<float*>(0));
    itsSizeDouble = itsConversion->externalSize (static_cast<double*>(0));
}


size_t ConversionIO::write (size_t nvalues, const bool* value)
{
    return TypeIO::write (nvalues, value);
}

size_t ConversionIO::write (size_t nvalues, const Complex* value)
{
    return TypeIO::write (nvalues, value);
}

size_t ConversionIO::write (size_t nvalues, const DComplex* value)
{
    return TypeIO::write (nvalues, value);
}

size_t ConversionIO::write (size_t nvalues, const String* value)
{
    return TypeIO::write (nvalues, value);
}

size_t ConversionIO::read (size_t nvalues, bool* value)
{
    return TypeIO::read (nvalues, value);
}

size_t ConversionIO::read (size_t nvalues, Complex* value)
{
    return TypeIO::read (nvalues, value);
}

size_t ConversionIO::read (size_t nvalues, DComplex* value)
{
    return TypeIO::read (nvalues, value);
}

size_t ConversionIO::read (size_t nvalues, String* value)
{
    return TypeIO::read (nvalues, value);
}


#define CONVERSIONIO_DOIT(T,TP)                               \
size_t ConversionIO::write (size_t nvalues, const T* value) \
{ \
    size_t size = nvalues * aips_name2(itsSize,TP); \
    if (aips_name2(itsCopy,TP)) { \
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
size_t ConversionIO::read (size_t nvalues, T* value) \
{ \
    size_t size = nvalues * aips_name2(itsSize,TP); \
    if (aips_name2(itsCopy,TP)) { \
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


CONVERSIONIO_DOIT(char,Char)
CONVERSIONIO_DOIT(unsigned char,uChar)
CONVERSIONIO_DOIT(int16_t,Short)
CONVERSIONIO_DOIT(uint16_t,uShort)
CONVERSIONIO_DOIT(int32_t,Int)
CONVERSIONIO_DOIT(uint32_t,uInt)
CONVERSIONIO_DOIT(int64_t,Int64)
CONVERSIONIO_DOIT(uint64_t,uInt64)
CONVERSIONIO_DOIT(float,Float)
CONVERSIONIO_DOIT(double,Double)

} //# NAMESPACE CASACORE - END

