//# LittleEndianConversion.cc: A class with static functions to convert littleEndian format
//# Copyright (C) 1996,2001
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


#include <casacore/casa/OS/LittleEndianConversion.h>
#include <assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

void LittleEndianConversion::toLocal (char* to, const void* from,
				      size_t nr)
{
    assert (sizeof(char) == 1);
    memcpy (to, from, nr);
}

void LittleEndianConversion::toLocal (unsigned char* to, const void* from,
				      size_t nr)
{
    assert (sizeof(unsigned char) == 1);
    memcpy (to, from, nr);
}

void LittleEndianConversion::toLocal (short* to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    short* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 2;
    }
}


void LittleEndianConversion::toLocal (unsigned short* to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    unsigned short* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 2;
    }
}

void LittleEndianConversion::toLocal (int* to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    int* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}

void LittleEndianConversion::toLocal (unsigned int*  to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    unsigned int* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}

void LittleEndianConversion::toLocal (Int64* to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    Int64* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}

void LittleEndianConversion::toLocal (uInt64* to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    uInt64* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}

void LittleEndianConversion::toLocal (float* to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    float* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}

void LittleEndianConversion::toLocal (double* to, const void* from,
				      size_t nr)
{
    const char* data = (const char*)from;
    double* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}


void LittleEndianConversion::fromLocal (void* to, const char* from,
					size_t nr)
{
    assert (sizeof(char) == 1);
    memcpy (to, from, nr);
}

void LittleEndianConversion::fromLocal (void* to, const unsigned char* from,
					size_t nr)
{
    assert (sizeof(unsigned char) == 1);
    memcpy (to, from, nr);
}

void LittleEndianConversion::fromLocal (void* to, const short* from,
					size_t nr)
{
    char* data = (char*)to;
    const short* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 2;
    }
}

void LittleEndianConversion::fromLocal (void* to, const unsigned short* from,
					size_t nr)
{
    char* data = (char*)to;
    const unsigned short* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 2;
    }
}

void LittleEndianConversion::fromLocal (void* to, const int* from,
					size_t nr)
{
    char* data = (char*)to;
    const int* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 4;
    }
}

void LittleEndianConversion::fromLocal (void* to, const unsigned int* from,
					size_t nr)
{
    char* data = (char*)to;
    const unsigned int* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 4;
    }
}

void LittleEndianConversion::fromLocal (void* to, const Int64* from,
					size_t nr)
{
    char* data = (char*)to;
    const Int64* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 4;
    }
}

void LittleEndianConversion::fromLocal (void* to, const uInt64* from,
					size_t nr)
{
    char* data = (char*)to;
    const uInt64* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 4;
    }
}

void LittleEndianConversion::fromLocal (void* to, const float* from,
					size_t nr)
{
    char* data = (char*)to;
    const float* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 4;
    }
}

void LittleEndianConversion::fromLocal (void* to, const double* from,
					size_t nr)
{
    char* data = (char*)to;
    const double* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 8;
    }
}

} //# NAMESPACE CASACORE - END

