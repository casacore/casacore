//# LECanonicalConversion.cc: A class with static functions to convert little endian canonical format
//# Copyright (C) 2002
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


#include <casacore/casa/OS/LECanonicalConversion.h>
#include <assert.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

size_t LECanonicalConversion::toLocalChar (void* to, const void* from,
                                           size_t nr)
{
    assert (sizeof(char) == SIZE_LECAN_CHAR);
    memcpy (to, from, nr);
    return nr * SIZE_LECAN_CHAR;
}

size_t LECanonicalConversion::fromLocalChar (void* to, const void* from,
                                             size_t nr)
{
    assert (sizeof(char) == SIZE_LECAN_CHAR);
    memcpy (to, from, nr);
    return nr * SIZE_LECAN_CHAR;
}

void* LECanonicalConversion::byteToLocalChar (void* to, const void* from,
					      size_t nrbytes)
{
    assert (sizeof(char) == SIZE_LECAN_CHAR);
    memcpy (to, from, nrbytes);
    return to;
}

void* LECanonicalConversion::byteFromLocalChar (void* to, const void* from,
						size_t nrbytes)
{
    assert (sizeof(char) == SIZE_LECAN_CHAR);
    memcpy (to, from, nrbytes);
    return to;
}


Conversion::ByteFunction* LECanonicalConversion::getByteToLocal (const char*)
{
    assert (sizeof(char) == SIZE_LECAN_CHAR);
    return Conversion::getmemcpy();
}

Conversion::ByteFunction* LECanonicalConversion::getByteFromLocal (const char*)
{
    assert (sizeof(char) == SIZE_LECAN_CHAR);
    return Conversion::getmemcpy();
}


size_t LECanonicalConversion::toLocalUChar (void* to, const void* from,
                                            size_t nr)
{
    assert (sizeof(unsigned char) == SIZE_LECAN_UCHAR);
    memcpy (to, from, nr);
    return nr * SIZE_LECAN_UCHAR;
}

size_t LECanonicalConversion::fromLocalUChar (void* to, const void* from,
                                              size_t nr)
{
    assert (sizeof(unsigned char) == SIZE_LECAN_UCHAR);
    memcpy (to, from, nr);
    return nr * SIZE_LECAN_UCHAR;
}

void* LECanonicalConversion::byteToLocalUChar (void* to, const void* from,
					       size_t nrbytes)
{
    assert (sizeof(unsigned char) == SIZE_LECAN_UCHAR);
    memcpy (to, from, nrbytes);
    return to;
}

void* LECanonicalConversion::byteFromLocalUChar (void* to, const void* from,
						 size_t nrbytes)
{
    assert (sizeof(unsigned char) == SIZE_LECAN_UCHAR);
    memcpy (to, from, nrbytes);
    return to;
}

Conversion::ByteFunction* LECanonicalConversion::getByteToLocal
                                                         (const unsigned char*)
{
    assert (sizeof(unsigned char) == SIZE_LECAN_UCHAR);
    return Conversion::getmemcpy();
}

Conversion::ByteFunction* LECanonicalConversion::getByteFromLocal
                                                         (const unsigned char*)
{
    assert (sizeof(unsigned char) == SIZE_LECAN_UCHAR);
    return Conversion::getmemcpy();
}



#define LECANONICALCONVERSION_DO(CONVERT,SIZE,TOLOCAL,FROMLOCAL,BYTETO,BYTEFROM,T) \
size_t LECanonicalConversion::TOLOCAL (void* to, const void* from, \
                                       size_t nr)                  \
{ \
    /* Use memcpy if no conversion is needed. */ \
    if (CONVERT == 0) { \
	assert (sizeof(T) == SIZE); \
	memcpy (to, from, nr*SIZE); \
    }else{ \
	const char* data = (const char*)from; \
        T* dest = (T*)to; \
	T* last = dest + nr; \
	while (dest < last) { \
	    toLocal (*dest++, data); \
	    data += SIZE; \
	} \
    } \
    return nr*SIZE; \
} \
size_t LECanonicalConversion::FROMLOCAL (void* to, const void* from, \
                                         size_t nr)                  \
{ \
    /* Use memcpy if no conversion is needed. */ \
    if (CONVERT == 0) { \
	assert (sizeof(T) == SIZE); \
	memcpy (to, from, nr*SIZE); \
    }else{ \
	char* data = (char*)to; \
	const T* src = (const T*)from; \
	const T* last = src + nr; \
	while (src < last) { \
	    fromLocal (data, *src++); \
	    data += SIZE; \
	} \
    } \
    return nr*SIZE; \
} \
void* LECanonicalConversion::BYTETO (void* to, const void* from, \
                                     size_t nrbytes)             \
{ \
    TOLOCAL (to, from, nrbytes / sizeof(T)); \
    return to; \
} \
void* LECanonicalConversion::BYTEFROM (void* to, const void* from, \
                                       size_t nrbytes)             \
{ \
    FROMLOCAL (to, from, nrbytes / sizeof(T)); \
    return to; \
} \
Conversion::ByteFunction* LECanonicalConversion::getByteToLocal (const T*) \
{ \
    if (CONVERT == 0) { \
	assert (sizeof(T) == SIZE); \
        return Conversion::getmemcpy(); \
    } \
    return BYTETO; \
} \
Conversion::ByteFunction* LECanonicalConversion::getByteFromLocal (const T*) \
{ \
    if (CONVERT == 0) { \
	assert (sizeof(T) == SIZE); \
        return Conversion::getmemcpy(); \
    } \
    return BYTEFROM; \
}


LECANONICALCONVERSION_DO (CONVERT_LECAN_SHORT,  SIZE_LECAN_SHORT,
			  toLocalShort,  fromLocalShort,
			  byteToLocalShort,  byteFromLocalShort,  short)
LECANONICALCONVERSION_DO (CONVERT_LECAN_USHORT, SIZE_LECAN_USHORT,
			  toLocalUShort, fromLocalUShort,
			  byteToLocalUShort, byteFromLocalUShort,
			  unsigned short)
LECANONICALCONVERSION_DO (CONVERT_LECAN_INT,    SIZE_LECAN_INT,
			  toLocalInt,    fromLocalInt,
			  byteToLocalInt,    byteFromLocalInt,    int)
LECANONICALCONVERSION_DO (CONVERT_LECAN_UINT,   SIZE_LECAN_UINT,
			  toLocalUInt,   fromLocalUInt,
			  byteToLocalUInt,   byteFromLocalUInt,   unsigned int)
LECANONICALCONVERSION_DO (CONVERT_LECAN_INT64,  SIZE_LECAN_INT64,
			  toLocalInt64,  fromLocalInt64,
			  byteToLocalInt64,  byteFromLocalInt64,  Int64)
LECANONICALCONVERSION_DO (CONVERT_LECAN_UINT64, SIZE_LECAN_UINT64,
			  toLocalUInt64, fromLocalUInt64,
			  byteToLocalUInt64, byteFromLocalUInt64, uInt64)
LECANONICALCONVERSION_DO (CONVERT_LECAN_FLOAT,  SIZE_LECAN_FLOAT,
			  toLocalFloat,  fromLocalFloat,
			  byteToLocalFloat,  byteFromLocalFloat,  float)
LECANONICALCONVERSION_DO (CONVERT_LECAN_DOUBLE, SIZE_LECAN_DOUBLE,
			  toLocalDouble, fromLocalDouble,
			  byteToLocalDouble, byteFromLocalDouble, double)

} //# NAMESPACE CASACORE - END

