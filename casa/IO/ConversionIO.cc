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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/casa/IO/ConversionIO.h>
#include <casacore/casa/OS/DataConversion.h>
#include <casacore/casa/IO/ByteIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ConversionIO::ConversionIO (const std::shared_ptr<DataConversion>& dataConversion,
			    const std::shared_ptr<ByteIO>& byteIO,
                            uInt bufferLength)
: TypeIO          (byteIO),
  itsConversion   (dataConversion),
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

template<typename T>
void ConversionIO::initType(uInt& size, Bool& copy) const
{
  copy = itsConversion->canCopyGeneric<T>();
  size = itsConversion->externalSizeGeneric<T>();
}

void ConversionIO::init()
{
  initType<Char>   (itsSizeChar,   itsCopyChar);
  initType<uChar>  (itsSizeuChar,  itsCopyuChar);
  initType<Short>  (itsSizeShort,  itsCopyShort);
  initType<uShort> (itsSizeuShort, itsCopyuShort);
  initType<Int>    (itsSizeInt,    itsCopyInt);
  initType<uInt>   (itsSizeuInt,   itsCopyuInt);
  initType<Int64>  (itsSizeInt64,  itsCopyInt64);
  initType<uInt64> (itsSizeuInt64, itsCopyuInt64);
  initType<Float>  (itsSizeFloat,  itsCopyFloat);
  initType<Double> (itsSizeDouble, itsCopyDouble);
}


size_t ConversionIO::write (size_t nvalues, const Bool* value)
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

size_t ConversionIO::read (size_t nvalues, Bool* value)
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

template<typename T>
size_t ConversionIO::writeGeneric (size_t nvalues, const T* value, size_t type_size, bool copy)
{
    const size_t size = nvalues * type_size;
    if (copy) {
	itsByteIO->write (size, value);
    } else {
	if (size <= itsBufferLength) {
	    itsConversion->fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (size, itsBuffer);
    	} else {
	    char* tempBuffer = new char [size];
	    itsConversion->fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (size, tempBuffer);
	    delete [] tempBuffer;
	}
    }
    return size;
}

template<typename T>
size_t ConversionIO::readGeneric (size_t nvalues, T* value, size_t type_size, bool copy)
{
    const size_t size = nvalues * type_size;
    if (copy) {
	itsByteIO->read (size, value);
    } else {
	if (size <= itsBufferLength) {
	    itsByteIO->read (size, itsBuffer);
	    itsConversion->toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[size];
	    itsByteIO->read (size, tempBuffer);
	    itsConversion->toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    }
    return size;
}

size_t ConversionIO::write (size_t nvalues, const Char* data) { return writeGeneric<Char>(nvalues, data, itsSizeChar, itsCopyChar); }
size_t ConversionIO::write (size_t nvalues, const uChar* data) { return writeGeneric<uChar>(nvalues, data, itsSizeuChar, itsCopyuChar); }
size_t ConversionIO::write (size_t nvalues, const Short* data) { return writeGeneric<Short>(nvalues, data, itsSizeShort, itsCopyShort); }
size_t ConversionIO::write (size_t nvalues, const uShort* data) { return writeGeneric<uShort>(nvalues, data, itsSizeuShort, itsCopyuShort); }
size_t ConversionIO::write (size_t nvalues, const Int* data) { return writeGeneric<Int>(nvalues, data, itsSizeInt, itsCopyInt); }
size_t ConversionIO::write (size_t nvalues, const uInt* data) { return writeGeneric<uInt>(nvalues, data, itsSizeuInt, itsCopyuInt); }
size_t ConversionIO::write (size_t nvalues, const Int64* data) { return writeGeneric<Int64>(nvalues, data, itsSizeInt64, itsCopyInt64); }
size_t ConversionIO::write (size_t nvalues, const uInt64* data) { return writeGeneric<uInt64>(nvalues, data, itsSizeuInt64, itsCopyuInt64); }
size_t ConversionIO::write (size_t nvalues, const Float* data) { return writeGeneric<Float>(nvalues, data, itsSizeFloat, itsCopyFloat); }
size_t ConversionIO::write (size_t nvalues, const Double* data) { return writeGeneric<Double>(nvalues, data, itsSizeDouble, itsCopyDouble); }

size_t ConversionIO::read (size_t nvalues, Char* data) { return readGeneric<Char>(nvalues, data, itsSizeChar, itsCopyChar); }
size_t ConversionIO::read (size_t nvalues, uChar* data) { return readGeneric<uChar>(nvalues, data, itsSizeuChar, itsCopyuChar); }
size_t ConversionIO::read (size_t nvalues, Short* data) { return readGeneric<Short>(nvalues, data, itsSizeShort, itsCopyShort); }
size_t ConversionIO::read (size_t nvalues, uShort* data) { return readGeneric<uShort>(nvalues, data, itsSizeuShort, itsCopyuShort); }
size_t ConversionIO::read (size_t nvalues, Int* data) { return readGeneric<Int>(nvalues, data, itsSizeInt, itsCopyInt); }
size_t ConversionIO::read (size_t nvalues, uInt* data) { return readGeneric<uInt>(nvalues, data, itsSizeuInt, itsCopyuInt); }
size_t ConversionIO::read (size_t nvalues, Int64* data) { return readGeneric<Int64>(nvalues, data, itsSizeInt64, itsCopyInt64); }
size_t ConversionIO::read (size_t nvalues, uInt64* data) { return readGeneric<uInt64>(nvalues, data, itsSizeuInt64, itsCopyuInt64); }
size_t ConversionIO::read (size_t nvalues, Float* data) { return readGeneric<Float>(nvalues, data, itsSizeFloat, itsCopyFloat); }
size_t ConversionIO::read (size_t nvalues, Double* data) { return readGeneric<Double>(nvalues, data, itsSizeDouble, itsCopyDouble); }

} //# NAMESPACE CASACORE - END

