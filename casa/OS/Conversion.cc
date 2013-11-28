//# Conversion.cc: A class with static functions to convert canonical format
//# Copyright (C) 1996,2011
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


#include <stdint.h>
#include <assert.h>
#include <casa/aips.h>
#include <casa/OS/Conversion.h>
#include <casa/iostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

unsigned int Conversion::boolToBit (void* to, const void* from,
				    unsigned int nvalues)
{
    const Bool* data = (const Bool*)from;
    unsigned char* bits = (unsigned char*)to;
    //# Fill as many bytes as needed.
    unsigned int nbytes = (nvalues + 7) / 8;
    unsigned int i,j;
    unsigned int index = 0;
    for (i=0; i<nbytes; i++) {
	unsigned char& ch = bits[i];
	ch = 0;
	unsigned char mask = 1;
	//# Take care of correct number of bits in last byte.
	unsigned int nbits = (nvalues-index < 8  ?  nvalues-index : 8);
	for (j=0; j<nbits; j++) {
	    if (data[index++]) {
		ch |= mask;
	    }
	    mask <<= 1;
	}
    }
    return nbytes;
}

void Conversion::boolToBit (void* to, const void* from,
			    unsigned int startBit,
			    unsigned int nvalues)
{
    const Bool* data = (const Bool*)from;
    unsigned char* bits = (unsigned char*)to;
    //# Determine the first and last byte to be set
    //# and the first and last bit in the first and last byte.
    unsigned int startByte = startBit / 8;
    unsigned int startBit1 = startBit - 8 * startByte;
    unsigned int endByte   = (startBit + nvalues) / 8;
    unsigned int endBit1   = 8;
    unsigned int endBit2   = startBit + nvalues - 8 * endByte;
    //# Take care if only one byte has to be handled.
    if (startByte == endByte) {
	endBit1 = endBit2;
	endBit2 = 0;
    }
    unsigned int i, j, index;
    //# Set the bits in the last byte (if needed).
    if (endBit2 > 0) {
	index = endByte * 8 - startBit;
	unsigned char& ch = bits[endByte];
	unsigned char mask = 1;
	for (i=0; i<endBit2; i++) {
	    if (data[index++]) {
		ch |= mask;
	    }else{
		ch &= (~mask);
	    }
	    mask <<= 1;
	}
    }
    index = 0;
    //# Set the bits in the first byte (if needed).
    if (startBit1 > 0  ||  endBit1 < 8) {
	unsigned char& ch = bits[startByte++];
	unsigned char mask = (1 << startBit1);
	for (i=startBit1; i<endBit1; i++) {
	    if (data[index++]) {
		ch |= mask;
	    }else{
		ch &= (~mask);
	    }
	    mask <<= 1;
	}
    }
    //# Set the bits in all 'full' bytes.
    //# Note that the index in the Bool array is correct.
    for (i=startByte; i<endByte; i++) {
	unsigned char& ch = bits[i];
	ch = 0;
	unsigned char mask = 1;
	for (j=0; j<8; j++) {
	    if (data[index++]) {
		ch |= mask;
	    }
	    mask <<= 1;
	}
    }
}

unsigned int Conversion::bitToBool_ (void* to, const void* from,
                                     unsigned int nvalues)
{
    Bool* data = (Bool*)to;
    const unsigned char* bits = (const unsigned char*)from;
    //# Fill as many bytes as needed.
    unsigned int nbytes = (nvalues + 7) / 8;
    unsigned int i,j;
    unsigned int index = 0;
    for (i=0; i<nbytes; i++) {
	const unsigned int ch = bits[i];
	unsigned int mask = 1;
	//# Take care of correct number of bits in last byte.
	unsigned int nbits = (nvalues-index < 8  ?  nvalues-index : 8);
	for (j=0; j<nbits; j++) {
	    if (ch & mask) {
		data[index] = True;
	    }else{
		data[index] = False;
	    }
	    mask <<= 1;
	    index++;
	}
    }
    return nbytes;
}


//# July 2011: Optimization made by Kohji Nakamura
//# and slightly adapted by Ger van Diepen

// Define 
typedef union {
    // __m64d wide;
    Bool b[8];   // must be the first member for the initialization below.
    double d;
} m64d_t;

// Define all flag patterns for values 0 till 255.
static m64d_t conv_tab[256] __attribute__ ((aligned (8))) = {
  {{False, False, False, False, False, False, False, False}},
  {{True , False, False, False, False, False, False, False}},
  {{False, True , False, False, False, False, False, False}},
  {{True , True , False, False, False, False, False, False}},
  {{False, False, True , False, False, False, False, False}},
  {{True , False, True , False, False, False, False, False}},
  {{False, True , True , False, False, False, False, False}},
  {{True , True , True , False, False, False, False, False}},
  {{False, False, False, True , False, False, False, False}},
  {{True , False, False, True , False, False, False, False}},
  {{False, True , False, True , False, False, False, False}},
  {{True , True , False, True , False, False, False, False}},
  {{False, False, True , True , False, False, False, False}},
  {{True , False, True , True , False, False, False, False}},
  {{False, True , True , True , False, False, False, False}},
  {{True , True , True , True , False, False, False, False}},
  {{False, False, False, False, True , False, False, False}},
  {{True , False, False, False, True , False, False, False}},
  {{False, True , False, False, True , False, False, False}},
  {{True , True , False, False, True , False, False, False}},
  {{False, False, True , False, True , False, False, False}},
  {{True , False, True , False, True , False, False, False}},
  {{False, True , True , False, True , False, False, False}},
  {{True , True , True , False, True , False, False, False}},
  {{False, False, False, True , True , False, False, False}},
  {{True , False, False, True , True , False, False, False}},
  {{False, True , False, True , True , False, False, False}},
  {{True , True , False, True , True , False, False, False}},
  {{False, False, True , True , True , False, False, False}},
  {{True , False, True , True , True , False, False, False}},
  {{False, True , True , True , True , False, False, False}},
  {{True , True , True , True , True , False, False, False}},
  {{False, False, False, False, False, True , False, False}},
  {{True , False, False, False, False, True , False, False}},
  {{False, True , False, False, False, True , False, False}},
  {{True , True , False, False, False, True , False, False}},
  {{False, False, True , False, False, True , False, False}},
  {{True , False, True , False, False, True , False, False}},
  {{False, True , True , False, False, True , False, False}},
  {{True , True , True , False, False, True , False, False}},
  {{False, False, False, True , False, True , False, False}},
  {{True , False, False, True , False, True , False, False}},
  {{False, True , False, True , False, True , False, False}},
  {{True , True , False, True , False, True , False, False}},
  {{False, False, True , True , False, True , False, False}},
  {{True , False, True , True , False, True , False, False}},
  {{False, True , True , True , False, True , False, False}},
  {{True , True , True , True , False, True , False, False}},
  {{False, False, False, False, True , True , False, False}},
  {{True , False, False, False, True , True , False, False}},
  {{False, True , False, False, True , True , False, False}},
  {{True , True , False, False, True , True , False, False}},
  {{False, False, True , False, True , True , False, False}},
  {{True , False, True , False, True , True , False, False}},
  {{False, True , True , False, True , True , False, False}},
  {{True , True , True , False, True , True , False, False}},
  {{False, False, False, True , True , True , False, False}},
  {{True , False, False, True , True , True , False, False}},
  {{False, True , False, True , True , True , False, False}},
  {{True , True , False, True , True , True , False, False}},
  {{False, False, True , True , True , True , False, False}},
  {{True , False, True , True , True , True , False, False}},
  {{False, True , True , True , True , True , False, False}},
  {{True , True , True , True , True , True , False, False}},
  {{False, False, False, False, False, False, True , False}},
  {{True , False, False, False, False, False, True , False}},
  {{False, True , False, False, False, False, True , False}},
  {{True , True , False, False, False, False, True , False}},
  {{False, False, True , False, False, False, True , False}},
  {{True , False, True , False, False, False, True , False}},
  {{False, True , True , False, False, False, True , False}},
  {{True , True , True , False, False, False, True , False}},
  {{False, False, False, True , False, False, True , False}},
  {{True , False, False, True , False, False, True , False}},
  {{False, True , False, True , False, False, True , False}},
  {{True , True , False, True , False, False, True , False}},
  {{False, False, True , True , False, False, True , False}},
  {{True , False, True , True , False, False, True , False}},
  {{False, True , True , True , False, False, True , False}},
  {{True , True , True , True , False, False, True , False}},
  {{False, False, False, False, True , False, True , False}},
  {{True , False, False, False, True , False, True , False}},
  {{False, True , False, False, True , False, True , False}},
  {{True , True , False, False, True , False, True , False}},
  {{False, False, True , False, True , False, True , False}},
  {{True , False, True , False, True , False, True , False}},
  {{False, True , True , False, True , False, True , False}},
  {{True , True , True , False, True , False, True , False}},
  {{False, False, False, True , True , False, True , False}},
  {{True , False, False, True , True , False, True , False}},
  {{False, True , False, True , True , False, True , False}},
  {{True , True , False, True , True , False, True , False}},
  {{False, False, True , True , True , False, True , False}},
  {{True , False, True , True , True , False, True , False}},
  {{False, True , True , True , True , False, True , False}},
  {{True , True , True , True , True , False, True , False}},
  {{False, False, False, False, False, True , True , False}},
  {{True , False, False, False, False, True , True , False}},
  {{False, True , False, False, False, True , True , False}},
  {{True , True , False, False, False, True , True , False}},
  {{False, False, True , False, False, True , True , False}},
  {{True , False, True , False, False, True , True , False}},
  {{False, True , True , False, False, True , True , False}},
  {{True , True , True , False, False, True , True , False}},
  {{False, False, False, True , False, True , True , False}},
  {{True , False, False, True , False, True , True , False}},
  {{False, True , False, True , False, True , True , False}},
  {{True , True , False, True , False, True , True , False}},
  {{False, False, True , True , False, True , True , False}},
  {{True , False, True , True , False, True , True , False}},
  {{False, True , True , True , False, True , True , False}},
  {{True , True , True , True , False, True , True , False}},
  {{False, False, False, False, True , True , True , False}},
  {{True , False, False, False, True , True , True , False}},
  {{False, True , False, False, True , True , True , False}},
  {{True , True , False, False, True , True , True , False}},
  {{False, False, True , False, True , True , True , False}},
  {{True , False, True , False, True , True , True , False}},
  {{False, True , True , False, True , True , True , False}},
  {{True , True , True , False, True , True , True , False}},
  {{False, False, False, True , True , True , True , False}},
  {{True , False, False, True , True , True , True , False}},
  {{False, True , False, True , True , True , True , False}},
  {{True , True , False, True , True , True , True , False}},
  {{False, False, True , True , True , True , True , False}},
  {{True , False, True , True , True , True , True , False}},
  {{False, True , True , True , True , True , True , False}},
  {{True , True , True , True , True , True , True , False}},
  {{False, False, False, False, False, False, False, True }},
  {{True , False, False, False, False, False, False, True }},
  {{False, True , False, False, False, False, False, True }},
  {{True , True , False, False, False, False, False, True }},
  {{False, False, True , False, False, False, False, True }},
  {{True , False, True , False, False, False, False, True }},
  {{False, True , True , False, False, False, False, True }},
  {{True , True , True , False, False, False, False, True }},
  {{False, False, False, True , False, False, False, True }},
  {{True , False, False, True , False, False, False, True }},
  {{False, True , False, True , False, False, False, True }},
  {{True , True , False, True , False, False, False, True }},
  {{False, False, True , True , False, False, False, True }},
  {{True , False, True , True , False, False, False, True }},
  {{False, True , True , True , False, False, False, True }},
  {{True , True , True , True , False, False, False, True }},
  {{False, False, False, False, True , False, False, True }},
  {{True , False, False, False, True , False, False, True }},
  {{False, True , False, False, True , False, False, True }},
  {{True , True , False, False, True , False, False, True }},
  {{False, False, True , False, True , False, False, True }},
  {{True , False, True , False, True , False, False, True }},
  {{False, True , True , False, True , False, False, True }},
  {{True , True , True , False, True , False, False, True }},
  {{False, False, False, True , True , False, False, True }},
  {{True , False, False, True , True , False, False, True }},
  {{False, True , False, True , True , False, False, True }},
  {{True , True , False, True , True , False, False, True }},
  {{False, False, True , True , True , False, False, True }},
  {{True , False, True , True , True , False, False, True }},
  {{False, True , True , True , True , False, False, True }},
  {{True , True , True , True , True , False, False, True }},
  {{False, False, False, False, False, True , False, True }},
  {{True , False, False, False, False, True , False, True }},
  {{False, True , False, False, False, True , False, True }},
  {{True , True , False, False, False, True , False, True }},
  {{False, False, True , False, False, True , False, True }},
  {{True , False, True , False, False, True , False, True }},
  {{False, True , True , False, False, True , False, True }},
  {{True , True , True , False, False, True , False, True }},
  {{False, False, False, True , False, True , False, True }},
  {{True , False, False, True , False, True , False, True }},
  {{False, True , False, True , False, True , False, True }},
  {{True , True , False, True , False, True , False, True }},
  {{False, False, True , True , False, True , False, True }},
  {{True , False, True , True , False, True , False, True }},
  {{False, True , True , True , False, True , False, True }},
  {{True , True , True , True , False, True , False, True }},
  {{False, False, False, False, True , True , False, True }},
  {{True , False, False, False, True , True , False, True }},
  {{False, True , False, False, True , True , False, True }},
  {{True , True , False, False, True , True , False, True }},
  {{False, False, True , False, True , True , False, True }},
  {{True , False, True , False, True , True , False, True }},
  {{False, True , True , False, True , True , False, True }},
  {{True , True , True , False, True , True , False, True }},
  {{False, False, False, True , True , True , False, True }},
  {{True , False, False, True , True , True , False, True }},
  {{False, True , False, True , True , True , False, True }},
  {{True , True , False, True , True , True , False, True }},
  {{False, False, True , True , True , True , False, True }},
  {{True , False, True , True , True , True , False, True }},
  {{False, True , True , True , True , True , False, True }},
  {{True , True , True , True , True , True , False, True }},
  {{False, False, False, False, False, False, True , True }},
  {{True , False, False, False, False, False, True , True }},
  {{False, True , False, False, False, False, True , True }},
  {{True , True , False, False, False, False, True , True }},
  {{False, False, True , False, False, False, True , True }},
  {{True , False, True , False, False, False, True , True }},
  {{False, True , True , False, False, False, True , True }},
  {{True , True , True , False, False, False, True , True }},
  {{False, False, False, True , False, False, True , True }},
  {{True , False, False, True , False, False, True , True }},
  {{False, True , False, True , False, False, True , True }},
  {{True , True , False, True , False, False, True , True }},
  {{False, False, True , True , False, False, True , True }},
  {{True , False, True , True , False, False, True , True }},
  {{False, True , True , True , False, False, True , True }},
  {{True , True , True , True , False, False, True , True }},
  {{False, False, False, False, True , False, True , True }},
  {{True , False, False, False, True , False, True , True }},
  {{False, True , False, False, True , False, True , True }},
  {{True , True , False, False, True , False, True , True }},
  {{False, False, True , False, True , False, True , True }},
  {{True , False, True , False, True , False, True , True }},
  {{False, True , True , False, True , False, True , True }},
  {{True , True , True , False, True , False, True , True }},
  {{False, False, False, True , True , False, True , True }},
  {{True , False, False, True , True , False, True , True }},
  {{False, True , False, True , True , False, True , True }},
  {{True , True , False, True , True , False, True , True }},
  {{False, False, True , True , True , False, True , True }},
  {{True , False, True , True , True , False, True , True }},
  {{False, True , True , True , True , False, True , True }},
  {{True , True , True , True , True , False, True , True }},
  {{False, False, False, False, False, True , True , True }},
  {{True , False, False, False, False, True , True , True }},
  {{False, True , False, False, False, True , True , True }},
  {{True , True , False, False, False, True , True , True }},
  {{False, False, True , False, False, True , True , True }},
  {{True , False, True , False, False, True , True , True }},
  {{False, True , True , False, False, True , True , True }},
  {{True , True , True , False, False, True , True , True }},
  {{False, False, False, True , False, True , True , True }},
  {{True , False, False, True , False, True , True , True }},
  {{False, True , False, True , False, True , True , True }},
  {{True , True , False, True , False, True , True , True }},
  {{False, False, True , True , False, True , True , True }},
  {{True , False, True , True , False, True , True , True }},
  {{False, True , True , True , False, True , True , True }},
  {{True , True , True , True , False, True , True , True }},
  {{False, False, False, False, True , True , True , True }},
  {{True , False, False, False, True , True , True , True }},
  {{False, True , False, False, True , True , True , True }},
  {{True , True , False, False, True , True , True , True }},
  {{False, False, True , False, True , True , True , True }},
  {{True , False, True , False, True , True , True , True }},
  {{False, True , True , False, True , True , True , True }},
  {{True , True , True , False, True , True , True , True }},
  {{False, False, False, True , True , True , True , True }},
  {{True , False, False, True , True , True , True , True }},
  {{False, True , False, True , True , True , True , True }},
  {{True , True , False, True , True , True , True , True }},
  {{False, False, True , True , True , True , True , True }},
  {{True , False, True , True , True , True , True , True }},
  {{False, True , True , True , True , True , True , True }},
  {{True , True , True , True , True , True , True , True }}
};


unsigned int Conversion::bitToBool (void* to, const void* from,
				    unsigned int nvalues)
{
    if (sizeof(Bool) != sizeof(char)  ||  (7 & (unsigned long long)to)) {
	return bitToBool_ (to, from, nvalues);
    }
#ifdef __clang__
    double* __attribute__ ((aligned (8))) data = (double *)to;
#else
    double* __attribute__ ((aligned (8))) data =
 	(double __attribute__ ((aligned (8)))*)to;
#endif
    const uint8_t* bits = (const uint8_t*)from;
    const size_t bits_per_loop = 8;
    const size_t nwords = nvalues / bits_per_loop;
#ifdef _OPENMP
# pragma omp parallel if (nwords >= 1024*2)
#endif
    {
#ifdef _OPENMP
# pragma omp for
#endif
	for (size_t i = 0; i < nwords; i++) {
	    data[i] = conv_tab[bits[i]].d;
	}
    }
    return nwords * (bits_per_loop / 8)
	+ bitToBool_ (&data[nwords], &bits[nwords],
                      nvalues - (nwords * bits_per_loop));
}

void Conversion::bitToBool (void* to, const void* from,
			    unsigned int startBit,
			    unsigned int nvalues)
{
    Bool* data = (Bool*)to;
    const unsigned char* bits = (const unsigned char*)from;
    //# Determine the first and last byte to be read
    //# and the first and last bit in the first and last byte.
    unsigned int startByte = startBit / 8;
    unsigned int startBit1 = startBit - 8 * startByte;
    unsigned int endByte   = (startBit + nvalues) / 8;
    unsigned int endBit1   = 8;
    unsigned int endBit2   = startBit + nvalues - 8 * endByte;
    //# Take care if only one byte has to be handled.
    if (startByte == endByte) {
	endBit1 = endBit2;
	endBit2 = 0;
    }
    unsigned int i, j, index;
    //# Get the bits in the last byte (if needed).
    if (endBit2 > 0) {
	index = endByte * 8 - startBit;
	const unsigned char& ch = bits[endByte];
	unsigned char mask = 1;
	for (i=0; i<endBit2; i++) {
	    if (ch & mask) {
		data[index] = True;
	    }else{
		data[index] = False;
	    }
	    mask <<= 1;
	    index++;
	}
    }
    index = 0;
    //# Set the bits in the first byte (if needed).
    if (startBit1 > 0  ||  endBit1 < 8) {
	const unsigned char& ch = bits[startByte++];
	unsigned char mask = (1 << startBit1);
	for (i=startBit1; i<endBit1; i++) {
	    if (ch & mask) {
		data[index] = True;
	    }else{
		data[index] = False;
	    }
	    mask <<= 1;
	    index++;
	}
    }
    //# Set the bits in all 'full' bytes.
    //# Note that the index in the Bool array is correct.
    for (i=startByte; i<endByte; i++) {
	const unsigned char& ch = bits[i];
	unsigned char mask = 1;
	for (j=0; j<8; j++) {
	    if (ch & mask) {
		data[index] = True;
	    }else{
		data[index] = False;
	    }
	    mask <<= 1;
	    index++;
	}
    }
}


unsigned int Conversion::valueCopy (void* to, const void* from,
				    unsigned int nbytes)
{
    memcpy (to, from, nbytes);
    return nbytes;
}


void* Conversion::mymemcpy (void* to, const void* from, unsigned int nbytes)
{
    return memcpy (to, from, nbytes);
}

} //# NAMESPACE CASA - END

