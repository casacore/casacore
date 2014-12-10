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
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

size_t Conversion::boolToBit (void* to, const void* from,
                              size_t nvalues)
{
    const Bool* data = (const Bool*)from;
    unsigned char* bits = (unsigned char*)to;
    //# Fill as many full bytes as possible.
    //# Note: the compiler can optimize much better for j<8 than j<nbits.
    // Therefore use a separate loop for the full bytes.
    size_t nfbytes = nvalues / 8;
    for (size_t i=0; i<nfbytes; ++i) {
        unsigned char& ch = bits[i];
	ch = 0;
	for (size_t j=0; j<8; ++j) {
	    if (*data++) {
                ch |= (1<<j);
	    }
        }
    }
    // Do the last byte if needed.
    size_t nbits = nvalues - nfbytes*8;
    if (nbits > 0) {
        unsigned char& ch = bits[nfbytes];
	ch = 0;
	//# Take care of correct number of bits in last byte.
	for (size_t j=0; j<nbits; ++j) {
	    if (*data++) {
                ch |= (1<<j);
	    }
	}
        nfbytes++;
    }
    return nfbytes;
}

void Conversion::boolToBit (void* to, const void* from,
			    size_t startBit,
			    size_t nvalues)
{
    const Bool* data = (const Bool*)from;
    unsigned char* bits = (unsigned char*)to;
    //# Determine the first and last byte to be set
    //# and the first and last bit in the first and last byte.
    size_t startByte = startBit / 8;
    size_t startBit1 = startBit - 8 * startByte;
    size_t endByte   = (startBit + nvalues) / 8;
    size_t endBit1   = 8;
    size_t endBit2   = startBit + nvalues - 8 * endByte;
    //# Take care if only one byte has to be handled.
    if (startByte == endByte) {
	endBit1 = endBit2;
	endBit2 = 0;
    }
    //# Set the bits in the first byte (if needed).
    if (startBit1 > 0  ||  endBit1 < 8) {
	unsigned char& ch = bits[startByte++];
	unsigned char mask = (1 << startBit1);
	for (size_t j=startBit1; j<endBit1; ++j) {
	    if (*data++) {
		ch |= mask;
	    }else{
		ch &= (~mask);
	    }
	    mask <<= 1;
	}
    }
    //# Set the bits in all 'full' bytes.
    for (size_t i=startByte; i<endByte; ++i) {
	unsigned char& ch = bits[i];
	ch = 0;
	for (size_t j=0; j<8; ++j) {
	    if (*data++) {
                ch |= (1<<j);
	    }
	}
    }
    //# Set the bits in the last byte (if needed).
    if (endBit2 > 0) {
	unsigned char& ch = bits[endByte];
	unsigned char mask = 1;
	for (size_t j=0; j<endBit2; ++j) {
	    if (*data++) {
		ch |= mask;
	    }else{
		ch &= (~mask);
	    }
	    mask <<= 1;
	}
    }
}

size_t Conversion::bitToBool_ (void* to, const void* from,
                               size_t nvalues)
{
    Bool* data = (Bool*)to;
    const unsigned char* bits = (const unsigned char*)from;
    //# Use as many full bytes as possible.
    size_t nfbytes = nvalues / 8;
    for (size_t i=0; i<nfbytes; ++i) {
	int ch = bits[i];
	for (size_t j=0; j<8; ++j) {
            *data++ = (ch & (1<<j));
	}
    }
    //# Handle final byte if needed.
    size_t nbits = nvalues - nfbytes*8;
    if (nbits > 0) {
	int ch = bits[nfbytes];
	for (size_t j=0; j<nbits; ++j) {
            *data++ = (ch & (1<<j));
	}
        nfbytes++;
    }
    return nfbytes;
}


//# July 2011: Optimization made by Kohji Nakamura
//# and slightly adapted by Ger van Diepen

// Define 
typedef union {
    // __m64d wide;
    Bool  b[8];   // must be the first member for the initialization below.
    Int64 d;
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


size_t Conversion::bitToBool (void* to, const void* from,
                              size_t nvalues)
{
    if (sizeof(Bool) != sizeof(char)  ||  (7 & (unsigned long long)to)) {
	return bitToBool_ (to, from, nvalues);
    }
#ifdef __clang__
    uint64_t* __attribute__ ((aligned (8))) data = (uint64_t *)to;
#else
    uint64_t* __attribute__ ((aligned (8))) data =
 	(uint64_t __attribute__ ((aligned (8)))*)to;
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
	for (size_t i = 0; i < nwords; ++i) {
	    data[i] = conv_tab[bits[i]].d;
	}
    }
    return nwords * (bits_per_loop / 8)
	+ bitToBool_ (&data[nwords], &bits[nwords],
                      nvalues - (nwords * bits_per_loop));
}

void Conversion::bitToBool (void* to, const void* from,
			    size_t startBit,
			    size_t nvalues)
{
    Bool* data = (Bool*)to;
    const unsigned char* bits = (const unsigned char*)from;
    //# Determine the first and last byte to be read
    //# and the first and last bit in the first and last byte.
    size_t startByte = startBit / 8;
    size_t startBit1 = startBit - 8 * startByte;
    size_t endByte   = (startBit + nvalues) / 8;
    size_t endBit1   = 8;
    size_t endBit2   = startBit + nvalues - 8 * endByte;
    //# Take care if only one byte has to be handled.
    if (startByte == endByte) {
	endBit1 = endBit2;
	endBit2 = 0;
    }
    //# Set the bits in the first byte (if needed).
    if (startBit1 > 0  ||  endBit1 < 8) {
	int ch = bits[startByte++];
	for (size_t j=startBit1; j<endBit1; ++j) {
            *data++ = (ch & (1<<j));
	}
    }
    //# Set the bits in all 'full' bytes.
    for (size_t i=startByte; i<endByte; ++i) {
	int ch = bits[i];
	for (size_t j=0; j<8; ++j) {
            *data++ = (ch & (1<<j));
	}
    }
    //# Get the bits in the last byte (if needed).
    if (endBit2 > 0) {
	int ch = bits[endByte];
	for (size_t j=0; j<endBit2; ++j) {
            *data++ = (ch & (1<<j));
	}
    }
}


size_t Conversion::valueCopy (void* to, const void* from,
                              size_t nbytes)
{
    memcpy (to, from, nbytes);
    return nbytes;
}


} //# NAMESPACE CASACORE - END

