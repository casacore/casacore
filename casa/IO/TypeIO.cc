//# TypeIO.cc: Abstract base class for IO of data in a type-dependent format
//# Copyright (C) 1996,1999,2001,2002
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

#include <casa/IO/TypeIO.h>
#include <casa/OS/Conversion.h>
#include <casa/BasicSL/String.h>
#include <casa/BasicSL/Complex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TypeIO::TypeIO (ByteIO* byteIO, Bool takeOver)
: itsByteIO(byteIO, takeOver) 
{}

TypeIO::TypeIO (const TypeIO& that)
: itsByteIO (that.itsByteIO) 
{}

TypeIO::~TypeIO()
{}

TypeIO& TypeIO::operator= (const TypeIO& that)
{
    if (this != &that) {
	itsByteIO = that.itsByteIO;
    }
    return *this;
}

const ByteIO& TypeIO::byteIO() const {
  return *itsByteIO;
}

ByteIO& TypeIO::byteIO() {
  return *itsByteIO;
}

Int64 TypeIO::seek (Int64 offset, ByteIO::SeekOption option)   
{
    return itsByteIO->seek (offset, option);
}
Int64 TypeIO::seek (Int offset, ByteIO::SeekOption option)   
{
    return itsByteIO->seek (offset, option);
}

Bool TypeIO::isReadable() const
{
    return itsByteIO->isReadable();
}

Bool TypeIO::isWritable() const
{
    return itsByteIO->isWritable();
}

Bool TypeIO::isSeekable() const
{
    return itsByteIO->isSeekable();
}


uInt TypeIO::write (uInt nvalues, const Bool* value)
{
    uInt nb = (nvalues+7) / 8;
    uChar* buf = new uChar[nb];
    Conversion::boolToBit (buf, value, nvalues);
    write (nb, buf);
    delete [] buf;
    return nb;
}

uInt TypeIO::write (uInt nvalues, const Complex* value)
{
    if (sizeof(Complex) == 2*sizeof(float)) {
	return write (2*nvalues, (const float*)value);
    }
    uInt n = 0;
    for (uInt i=0; i<nvalues; i++) {
	float f1= value[i].real();
	float f2= value[i].imag();
	n += write (1, &f1);
	n += write (1, &f2);
    }
    return n;
}

uInt TypeIO::write (uInt nvalues, const DComplex* value)
{
    if (sizeof(DComplex) == 2*sizeof(double)) {
	return write (2*nvalues, (const double*)value);
    }
    uInt n = 0;
    for (uInt i=0; i<nvalues; i++) {
	double d1= value[i].real();
	double d2= value[i].imag();
	n += write (1, &d1);
	n += write (1, &d2);
    }
    return n;
}

uInt TypeIO::write (uInt nvalues, const String* value)
{
    uInt n = 0;
    for (uInt i=0; i<nvalues; i++) {
	uInt len = value[i].length();
	n += write (1, &len);
	n += write (len, value[i].chars());
    }
    return n;
}


uInt TypeIO::read (uInt nvalues, Bool* value)
{
    uInt nb = (nvalues+7) / 8;
    uChar* buf = new uChar[nb];
    read (nb, buf);
    Conversion::bitToBool (value, buf, nvalues);
    delete [] buf;
    return nb;
}

uInt TypeIO::read (uInt nvalues, Complex* value)
{
    if (sizeof(Complex) == 2*sizeof(float)) {
	return read (2*nvalues, (float*)value);
    }
    uInt n=0;
    for (uInt i=0; i<nvalues; i++) {
	float f1;
	float f2;
	n += read (1, &f1);
	n += read (1, &f2);
	value[i] = Complex(f1,f2);
    }
    return n;
}

uInt TypeIO::read (uInt nvalues, DComplex* value)
{
    if (sizeof(DComplex) == 2*sizeof(double)) {
	return read (2*nvalues, (double*)value);
    }
    uInt n=0;
    for (uInt i=0; i<nvalues; i++) {
	double d1;
	double d2;
	n += read (1, &d1);
	n += read (1, &d2);
	value[i] = DComplex(d1,d2);
    }
    return n;
}

uInt TypeIO::read (uInt nvalues, String* str) {
    uInt n=0;
    for (uInt i=0; i<nvalues; i++) {
      uInt len;
      n += read (1, &len);
      str[i].resize (len);              // resize storage which adds trailing 0
      Char* ptr = &(str[i][0]);         // get actual string
      n += read (len, ptr);             // read string
#ifdef USE_OLD_STRING
      ptr[len] = '\0';
#endif
    }
    return n;
}

} //# NAMESPACE CASA - END

