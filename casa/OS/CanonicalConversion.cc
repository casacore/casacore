//# CanonicalConversion.cc: A class with static functions to convert canonical format
//# Copyright (C) 1996,2000,2001
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


#include <casacore/casa/OS/CanonicalConversion.h>
#include <assert.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

size_t CanonicalConversion::toLocalChar (void* to, const void* from,
                                         size_t nr)
{
    assert (sizeof(char) == SIZE_CAN_CHAR);
    memcpy (to, from, nr);
    return nr * SIZE_CAN_CHAR;
}

size_t CanonicalConversion::fromLocalChar (void* to, const void* from,
                                           size_t nr)
{
    assert (sizeof(char) == SIZE_CAN_CHAR);
    memcpy (to, from, nr);
    return nr * SIZE_CAN_CHAR;
}

void* CanonicalConversion::byteToLocalChar (void* to, const void* from,
					    size_t nrbytes)
{
    assert (sizeof(char) == SIZE_CAN_CHAR);
    memcpy (to, from, nrbytes);
    return to;
}

void* CanonicalConversion::byteFromLocalChar (void* to, const void* from,
					      size_t nrbytes)
{
    assert (sizeof(char) == SIZE_CAN_CHAR);
    memcpy (to, from, nrbytes);
    return to;
}


Conversion::ByteFunction* CanonicalConversion::getByteToLocal (const char*)
{
    assert (sizeof(char) == SIZE_CAN_CHAR);
    return Conversion::getmemcpy();
}

Conversion::ByteFunction* CanonicalConversion::getByteFromLocal (const char*)
{
    assert (sizeof(char) == SIZE_CAN_CHAR);
    return Conversion::getmemcpy();
}


size_t CanonicalConversion::toLocalUChar (void* to, const void* from,
                                          size_t nr)
{
    assert (sizeof(unsigned char) == SIZE_CAN_UCHAR);
    memcpy (to, from, nr);
    return nr * SIZE_CAN_UCHAR;
}

size_t CanonicalConversion::fromLocalUChar (void* to, const void* from,
                                            size_t nr)
{
    assert (sizeof(unsigned char) == SIZE_CAN_UCHAR);
    memcpy (to, from, nr);
    return nr * SIZE_CAN_UCHAR;
}

void* CanonicalConversion::byteToLocalUChar (void* to, const void* from,
					     size_t nrbytes)
{
    assert (sizeof(unsigned char) == SIZE_CAN_UCHAR);
    memcpy (to, from, nrbytes);
    return to;
}

void* CanonicalConversion::byteFromLocalUChar (void* to, const void* from,
					       size_t nrbytes)
{
    assert (sizeof(unsigned char) == SIZE_CAN_UCHAR);
    memcpy (to, from, nrbytes);
    return to;
}

Conversion::ByteFunction* CanonicalConversion::getByteToLocal
                                                         (const unsigned char*)
{
    assert (sizeof(unsigned char) == SIZE_CAN_UCHAR);
    return Conversion::getmemcpy();
}

Conversion::ByteFunction* CanonicalConversion::getByteFromLocal
                                                         (const unsigned char*)
{
    assert (sizeof(unsigned char) == SIZE_CAN_UCHAR);
    return Conversion::getmemcpy();
}



#define CANONICALCONVERSION_DO(CONVERT,SIZE,TOLOCAL,FROMLOCAL,BYTETO,BYTEFROM,T) \
size_t CanonicalConversion::TOLOCAL (void* to, const void* from, \
				     size_t nr) \
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
size_t CanonicalConversion::FROMLOCAL (void* to, const void* from, \
				       size_t nr) \
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
void* CanonicalConversion::BYTETO (void* to, const void* from, \
				   size_t nrbytes) \
{ \
    TOLOCAL (to, from, nrbytes / sizeof(T)); \
    return to; \
} \
void* CanonicalConversion::BYTEFROM (void* to, const void* from, \
				     size_t nrbytes) \
{ \
    FROMLOCAL (to, from, nrbytes / sizeof(T)); \
    return to; \
} \
Conversion::ByteFunction* CanonicalConversion::getByteToLocal (const T*) \
{ \
    if (CONVERT == 0) { \
	assert (sizeof(T) == SIZE); \
        return Conversion::getmemcpy(); \
    } \
    return BYTETO; \
} \
Conversion::ByteFunction* CanonicalConversion::getByteFromLocal (const T*) \
{ \
    if (CONVERT == 0) { \
	assert (sizeof(T) == SIZE); \
        return Conversion::getmemcpy(); \
    } \
    return BYTEFROM; \
}


CANONICALCONVERSION_DO (CONVERT_CAN_SHORT,  SIZE_CAN_SHORT,
			toLocalShort,  fromLocalShort,
			byteToLocalShort,  byteFromLocalShort,  short)
CANONICALCONVERSION_DO (CONVERT_CAN_USHORT, SIZE_CAN_USHORT,
			toLocalUShort, fromLocalUShort,
			byteToLocalUShort, byteFromLocalUShort, unsigned short)
CANONICALCONVERSION_DO (CONVERT_CAN_INT,    SIZE_CAN_INT,
			toLocalInt,    fromLocalInt,
			byteToLocalInt,    byteFromLocalInt,    int)
CANONICALCONVERSION_DO (CONVERT_CAN_UINT,   SIZE_CAN_UINT,
			toLocalUInt,   fromLocalUInt,
			byteToLocalUInt,   byteFromLocalUInt,   unsigned int)
CANONICALCONVERSION_DO (CONVERT_CAN_INT64,  SIZE_CAN_INT64,
			toLocalInt64,  fromLocalInt64,
			byteToLocalInt64,  byteFromLocalInt64,  Int64)
CANONICALCONVERSION_DO (CONVERT_CAN_UINT64, SIZE_CAN_UINT64,
			toLocalUInt64, fromLocalUInt64,
			byteToLocalUInt64, byteFromLocalUInt64, uInt64)
CANONICALCONVERSION_DO (CONVERT_CAN_FLOAT,  SIZE_CAN_FLOAT,
			toLocalFloat,  fromLocalFloat,
			byteToLocalFloat,  byteFromLocalFloat,  float)
CANONICALCONVERSION_DO (CONVERT_CAN_DOUBLE, SIZE_CAN_DOUBLE,
			toLocalDouble, fromLocalDouble,
			byteToLocalDouble, byteFromLocalDouble, double)

} //# NAMESPACE CASACORE - END

