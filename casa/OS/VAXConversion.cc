//# VAXConversion.cc: A class with static functions to convert VAX format
//# Copyright (C) 1996,1997
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


#include <casacore/casa/OS/VAXConversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// VAX has almost identical format to IEEE (but no NaN)
// which is SEEEEEEE EFFFFFFF ...
// The VAX exponent is 2 higher.
// The VAX byte order is b1b0b3b2 (Big-endian IEEE has b0b1b2b3).
void VAXConversion::toLocal (float* to, const void* from,
			     size_t nr)
{
    assert (sizeof(unsigned int) == 4);
    assert (sizeof(float) == 4);
    const char* data = (const char*)from;
    float* last = to + nr;
    while (to < last) {
	unsigned int value;
	moveFloat (&value, data);
	data += 4;
	unsigned int exponent = (value & 0x7f800000) >> 23;
	if (exponent <= 2) {
	    *(unsigned int*)to = 0;
	}else{
	    exponent = (exponent-2) << 23;
	    *(unsigned int*)to = (value & 0x807fffff) | exponent;
	}
	to++;
    }
}

// VAX D-float has format SEEEEEEE EFFFFFFF ...
// IEEE double has format SEEEEEEE EEEEFFFF ...
// The VAX exponent is 2 higher.
// The VAX byte order is b1b0b3b2b5b4b7b6.
void VAXConversion::toLocal (double* to, const void* from,
			     size_t nr)
{
    assert (sizeof(unsigned int) == 4);
    assert (sizeof(double) == 8);
    const char* data = (const char*)from;
    double* last = to + nr;
    unsigned int value, rest;
    while (to < last) {
	moveFloat (&value, data);
	moveFloat (&rest, data+4);
	data += 8;
	unsigned int exponent = (value & 0x7f800000) >> 23;
	if (exponent == 0) {
	    value = 0;
	    rest  = 0;
	}else{
	    exponent = (exponent + (1022-128)) << 20;
	    rest  = (value << 29) | (rest >> 3);
	    value = (value & 0x80000000) | exponent
	            | ((value >> 3) & 0x000fffff);
	}
#if defined(AIPS_LITTLE_ENDIAN)
	((unsigned int*)to)[0] = rest;
	((unsigned int*)to)[1] = value;
#else
	((unsigned int*)to)[0] = value;
	((unsigned int*)to)[1] = rest;
#endif
	to++;
    }
}


void VAXConversion::fromLocal (void* to, const float* from,
			       size_t nr)
{
    assert (sizeof(unsigned int) == 4);
    assert (sizeof(float) == 4);
    char* data = (char*)to;
    const float* last = from + nr;
    while (from < last) {
	unsigned int value;
	value = *(unsigned int*)from;
	unsigned int exponent = (value & 0x7f800000) >> 23;
	if (exponent == 0) {
	    value = 0;
	}else{
	    exponent += 2;
	    if (exponent > 255) {
		value |= 0x7fffffff;
	    }else{
		value = (value & 0x807fffff) | (exponent << 23);
	    }
	}
	moveFloat (data, &value);
	data += 4;
	from++;
    }
}

void VAXConversion::fromLocal (void* to, const double* from,
			       size_t nr)
{
    assert (sizeof(unsigned int) == 4);
    assert (sizeof(double) == 8);
    char* data = (char*)to;
    const double* last = from + nr;
    while (from < last) {
	unsigned int value, rest;
#if defined(AIPS_LITTLE_ENDIAN)
	rest  = ((unsigned int*)from)[0];
	value = ((unsigned int*)from)[1];
#else
	value = ((unsigned int*)from)[0];
	rest  = ((unsigned int*)from)[1];
#endif
	unsigned int exponent = (value & 0x7ff00000) >> 20;
	exponent -= 1022-128;
	if (exponent <= 0) {
	    exponent = 0;
	    value = 0;
	    rest  = 0;
	}else{
	    if (exponent > 255) {
		value |= 0x7fffffff;
		rest  = 0xffffffff;
	    }else{
		value = (value & 0x80000000) | (exponent << 23)
		        | ((value << 3) & 0x007fffff) | (rest >> 29);
		rest <<= 3;
	    }
	}
	moveFloat (data, &value);
	moveFloat (data+4, &rest);
	data += 8;
	from++;
    }
}

} //# NAMESPACE CASACORE - END

