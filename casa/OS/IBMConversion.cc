//# IBMConversion.cc: A class with static functions to convert IBM format
//# Copyright (C) 1996,1997,2001
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


#include <casacore/casa/OS/IBMConversion.h>
#include <casacore/casa/Exceptions/Error.h>
#include <assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

static char fromEBCDIC[256] = {
    0  ,1  ,2  ,3  ,0  ,9  ,0  ,127,0  ,0  ,0  ,11,12,13,14,15,
    16 ,17 ,18 ,0  ,0  ,0  ,8  ,0  ,24 ,25 ,0  ,0 ,28,29,30,31,
    0  ,0  ,28 ,0  ,0  ,10 ,23 ,27 ,0  ,0  ,0  ,0 ,0 ,5 ,6 ,7 ,
    0  ,0  ,24 ,0  ,0  ,30 ,0  ,4  ,0  ,0  ,0  ,19,20,21,0 ,26,
    32 ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,46,60,40,43,124,
    38 ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,33 ,36,42,41,59,94,
    45 ,47 ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,124,44,37,95,62,63,
    0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,96 ,58 ,35,64,39,61,34,
    0  ,97 ,98 ,99 ,100,101,102,103,104,105,0  ,0 ,0 ,0 ,0 ,0 ,
    0  ,106,107,108,109,110,111,112,113,114,0  ,0 ,0 ,0 ,0 ,0 ,
    0  ,126,115,116,117,118,119,120,121,122,0  ,0 ,0 ,0 ,0 ,0 ,
    0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0 ,0 ,0 ,0 ,0 ,
    123,65 ,66 ,67 ,68 ,69 ,70 ,71 ,72 ,73 ,0  ,0 ,0 ,0 ,0 ,0 ,
    125,74 ,75 ,76 ,77 ,78 ,79 ,80 ,81 ,82 ,0  ,0 ,0 ,0 ,0 ,0 ,
    92 ,0  ,83 ,84 ,85 ,86 ,87 ,88 ,89 ,90 ,0  ,0 ,0 ,0 ,0 ,0 ,
    48 ,49 ,50 ,51 ,52 ,53 ,54 ,55 ,56 ,57 ,124,0 ,0 ,0 ,0 ,0
};

static char toEBCDIC[128] = {
       0,   1,   2,   3,  55,  45,  46,  47,
      22,   5,  37,  11,  12,  13,  14,  15,
      16,  17,  18,  59,  60,  61,  50,  38,
      24,  25,  63,  39,  28,  29,  30,  31,
      64,  90, 127, 123,  91, 108,  80, 125,
      77,  93,  92,  78, 107,  96,  75,  97,
     -26, -25, -24, -23, -22, -21, -20, -19,
     -18, -17, 122,  94,  76, 126, 110, 111,
     124, -63, -62, -61, -60, -59, -58, -57,
     -56, -55, -47, -46, -45, -44, -43, -42,
     -41, -40, -39, -30, -29, -28, -27, -26,
     -25, -24, -23, -64, -32, -48,  95, 109,
     121,-127,-126,-125,-124,-123,-122,-121,
    -120,-119,-111,-110,-109,-108,-107,-106,
    -105,-104,-103, -94, -93, -92, -91, -90,
     -89, -88, -87, -86,  -6, -48, -95,   7
    };


void IBMConversion::toLocal (char& to, const void* from)
{
    assert (sizeof(char) == 1);
    to = fromEBCDIC[*(const unsigned char*)from];
}

void IBMConversion::toLocal (char* to, const void* from,
			     size_t nr)
{
    assert (sizeof(char) == 1);
    const unsigned char* data = (const unsigned char*)from;
    char* last = to + nr;
    while (to < last) {
	*to++ = fromEBCDIC[*data++];
    }
}

void IBMConversion::fromLocal (void* to, char from)
{
    assert (sizeof(char) == 1);
    if (from < 0) {
	*(signed char*)to = 0;
    }else{
        *(signed char*)to = toEBCDIC[int(from)];
    }
}

void IBMConversion::fromLocal (void* to, const char* from,
			       size_t nr)
{
    assert (sizeof(char) == 1);
    signed char* data = (signed char*)to;
    const char* last = from + nr;
    while (from < last) {
	fromLocal (data++, *from++);
    }
}


void IBMConversion::toLocal (Int64* to, const void* from,
			     size_t nr)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(Int64) == SIZE_IBM_INT64) {
	memcpy (to, from, nr*sizeof(Int64));
	return;
    }
#endif
    const char* data = (const char*)from;
    Int64* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}

void IBMConversion::toLocal (uInt64* to, const void* from,
			     size_t nr)
{ 
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(uInt64) == SIZE_IBM_UINT64) {
	memcpy (to, from, nr*sizeof(uInt64));
	return;
    }
#endif
    const char* data = (const char*)from;
    uInt64* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += 4;
    }
}

void IBMConversion::fromLocal (void* to, const Int64* from,
			       size_t nr)
{
    char* data = (char*)to;
    const Int64* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 4;
    }
}

void IBMConversion::fromLocal (void* to, const uInt64* from,
			       size_t nr)
{ 
    char* data = (char*)to;
    const uInt64* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += 4;
    }
}


// IBM has format  SEEEEEEE FFFFFFFF ...
// The exponent has base 16. Fraction has no hidden bits.
// IEEE has format SEEEEEEE EFFFFFFF ...
// The exponent has base 2. Fraction has a hidden bit (i.e. a 1 before first F)
void IBMConversion::toLocal (float* to, const void* from,
			     size_t nr)
{
    assert (sizeof(unsigned int) == 4);
    assert (sizeof(float) == 4);
    const char* data = (const char*)from;
    float* last = to + nr;
    while (to < last) {
	unsigned int value;
#if defined(AIPS_LITTLE_ENDIAN)
	CanonicalConversion::reverse4 (&value, data);
#else
	CanonicalConversion::move4 (&value, data);
#endif
	if ((value & 0x00ffffff) == 0) {
	    *to = 0;
	}else{
	    // Get exponent by shifting 24 bits to right.
	    // IBM has base 16, IEEE has base 2.
	    // So multiply by 4, thus shift 2 left.
	    int exponent = ((value & 0x7f000000) >> (24-2));
	    exponent -= 256;
	    unsigned int sign = (value & 0x80000000);
	    // Shift to left until first bit of fraction is one.
	    while ((value & 0x00800000) == 0) {
		value <<= 1;
		exponent--;
	    }
	    // Test for over/underflow.
	    if (exponent > 128) {
		exponent = 128;
		value = 0xffffffff;
	    } else if (exponent <= -126) {
		exponent = -126;
		sign  = 0;
	        value = 0;
	    }
	    // Mask off first bit of fraction (is hidden bit).
	    *(unsigned int*)to = sign | ((exponent+126) << 23)
		                  | (value & 0x007fffff);
	}
	data += 4;
	to++;
    }
}

void IBMConversion::fromLocal (void* to, const float* from,
			       size_t nr)
{
    assert (sizeof(unsigned int) == 4);
    assert (sizeof(float) == 4);
    char* data = (char*)to;
    const float* last = from + nr;
    while (from < last) {
	unsigned int value;
	value = *(unsigned int*)from;
	int expo = (value & 0x7f800000) >> 23;
	if (expo == 0) {
	    value = 0;
	}else{
	    // Keep exponent positive, but correct for multiple of 4 test.
	    expo += (128-126);
	    unsigned int sign = (value & 0x80000000);
	    value &= 0x007fffff;                   // remove sign and exponent
	    value |= 0x00800000;                   // add hidden bit
	    // Make the exponent a multiple of 4 (by shifting 2 bits)
	    // while shifting mantissa to right.
	    int exponent = ((expo+3) >> 2);
	    value >>= ((exponent << 2) - expo);
	    exponent -= 32;
	    // Under/overflow of exponent is not possible.
	    value |= sign | ((exponent+64)<<24);
	}
#if defined(AIPS_LITTLE_ENDIAN)
	CanonicalConversion::reverse4 (data, &value);
#else
	CanonicalConversion::move4 (data, &value);
#endif
	data += 4;
	from++;
    }
}


// IBM has format  SEEEEEEE FFFFFFFF ...
// The exponent has base 16. Fraction has no hidden bits.
// IEEE has format SEEEEEEE EEEEFFFF ...
// The exponent has base 2. Fraction has a hidden bit (i.e. a 1 before first F)
void IBMConversion::toLocal (double* to, const void* from,
			     size_t nr)
{
    assert (sizeof(unsigned int) == 4);
    assert (sizeof(double) == 8);
    const char* data = (const char*)from;
    double* last = to + nr;
    while (to < last) {
	unsigned int value, rest;
#if defined(AIPS_LITTLE_ENDIAN)
	CanonicalConversion::reverse4 (&value, data);
	CanonicalConversion::reverse4 (&rest, data+4);
#else
	CanonicalConversion::move4 (&value, data);
	CanonicalConversion::move4 (&rest, data+4);
#endif
	if ((value & 0x00ffffff) == 0) {
	    *to = 0;
	}else{
	    // Get exponent by shifting 24 bits to right.
	    // IBM has base 16, IEEE has base 2.
	    // So multiply by 4, thus shift 2 left.
	    int exponent = ((value & 0x7f000000) >> (24-2));
	    exponent -= 256;
	    unsigned int sign = (value & 0x80000000);
	    // Shift to left until first bit of fraction is one.
	    while ((value & 0x00800000) == 0) {
		value <<= 1;
		if ((rest & 0x80000000) != 0) {
		    value++;                    // shift first bit of rest
		}
		rest <<= 1;
		exponent--;
	    }
	    
	    // Over/underflow is not possible.
	    // Shift fraction to the right (4 bits minus hidden bit).
	    // Mask off first bit of fraction (is hidden bit).
	    rest  = (value << 29) | (rest >> 3);
	    value = sign | ((exponent+1022) << 20)
		    | ((value >> 3) & 0x000fffff);
#if defined(AIPS_LITTLE_ENDIAN)
	    ((unsigned int*)to)[0] = rest;
	    ((unsigned int*)to)[1] = value;
#else
	    ((unsigned int*)to)[0] = value;
	    ((unsigned int*)to)[1] = rest;
#endif
	}
	to++;
	data += 8;
    }
}

void IBMConversion::fromLocal (void* to, const double* from,
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
	unsigned int expo = (value & 0x7ff00000) >> 20;
	if (expo == 0) {
	    value = 0;
	    rest  = 0;
	}else{
	    // Keep exponent positive, but correct for multiple of 4 test.
	    expo += (1024-1022);
	    unsigned int sign = (value & 0x80000000);
	    value &= 0x000fffff;                  // remove sign and exponent
	    value |= 0x00100000;                  // add hidden bit
	    // The mantissa has to be shifted left 3 bits.
	    // Make the exponent a multiple of 4 (by shifting 2 bits)
	    // while shifting mantissa to right (will not be more than 3 bits).
	    // So the net result is no shift or a shift left.
	    int exponent = ((expo+3) >> 2);
	    expo -= (exponent << 2) - 3;
	    if (expo > 0) {
		value <<= expo;
		value |= (rest >> (32 - expo));
		rest <<= expo;
	    }
	    exponent -= 256;
	    // Test for over/underflow.
	    if (exponent > 63) {
		value    = 0x00ffffff;
		rest     = 0xffffffff;
		exponent = 63;
	    } else if (exponent <= -64  ||  (value == 0  &&  rest == 0)) {
		value    = 0;
		rest     = 0;
		exponent = -64;
		sign     = 0;
	    }
	    value |= sign | ((exponent+64)<<24);
	}
#if defined(AIPS_LITTLE_ENDIAN)
	CanonicalConversion::reverse4 (data, &value);
	CanonicalConversion::reverse4 (data+4, &rest);
#else
	CanonicalConversion::move4 (data, &value);
	CanonicalConversion::move4 (data+4, &rest);
#endif
	data += 8;
	from++;
    }
}

} //# NAMESPACE CASACORE - END

