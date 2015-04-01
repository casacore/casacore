//# tConversionPerf.cc: performance test program for class Conversion
//# Copyright (C) 2014
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
//# $Id: tConversion.cc 21130 2011-10-18 07:39:05Z gervandiepen $


#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This program tests various ways to convert bits to Bools and vice-versa.

void bool2char (unsigned char* out, const Bool* in, size_t nr)
{
  // Define the union that equivalences an unsigned char to 8 bits.
  // This works slightly faster than the boolToBit implementation,
  // but is a bit more tricky and maybe less portable.
  struct {
    union {
      unsigned char val;
      struct {
#ifdef AIPS_LITTLE_ENDIAN
        unsigned h : 1;     //# uses 1 bit
        unsigned g : 1;
        unsigned f : 1;
        unsigned e : 1;
        unsigned d : 1;
        unsigned c : 1;
        unsigned b : 1;
        unsigned a : 1;
#else
        unsigned a : 1;     //# uses 1 bit
        unsigned b : 1;
        unsigned c : 1;
        unsigned d : 1;
        unsigned e : 1;
        unsigned f : 1;
        unsigned g : 1;
        unsigned h : 1;
#endif
      } y;
    } x;
  } o;
  for (size_t i=0; i<nr/8; ++i) {
    o.x.y.a = *in++;
    o.x.y.b = *in++;
    o.x.y.c = *in++;
    o.x.y.d = *in++;
    o.x.y.e = *in++;
    o.x.y.f = *in++;
    o.x.y.g = *in++;
    o.x.y.h = *in++;
    out[i] = o.x.val;
  }
}

size_t boolToBit (void* to, const void* from, size_t nvalues)
{
    const Bool* data = (const Bool*)from;
    unsigned char* bits = (unsigned char*)to;
    //# Fill as many full bytes as possible.
    size_t nfbytes = nvalues / 8;
    size_t i,j;
    for (i=0; i<nfbytes; ++i) {
        unsigned char& ch = bits[i];
	ch = 0;
	unsigned char mask = 1;
	for (j=0; j<8; ++j) {
	    if (*data++) {
              ch += mask;
              //              ch |= (1<<j);
	    }
            mask *= 2;  // this is probably a bit slower than the shift
	    //mask <<= 1;
        }
    }
    // Do the last byte if needed.
    size_t nbits = nvalues - nfbytes*8;
    if (nbits > 0) {
        unsigned char& ch = bits[nfbytes];
	ch = 0;
	unsigned char mask = 1;
	//# Take care of correct number of bits in last byte.
	for (j=0; j<nbits; ++j) {
	    if (*data++) {
		ch |= mask;
	    }
	    mask <<= 1;
	}
        nfbytes++;
    }
    return nfbytes;
}

void boolToBit (void* to, const void* from,
                size_t startBit,
                size_t nvalues)
{
    const Bool* data = (const Bool*)from;
    unsigned char* bits = (unsigned char*)to;
    //# Determine the first and last byte to be set
    //# and the first and last bit in the first and last byte.
    size_t startByte = startBit / 8;
    size_t startBit1 = startBit - 8 * startByte;
    size_t endByte = (startBit + nvalues) / 8;
    size_t endBit1 = 8;
    size_t endBit2 = startBit + nvalues - 8 * endByte;
    //# Take care if only one byte has to be handled.
    if (startByte == endByte) {
      endBit1 = endBit2;
      endBit2 = 0;
    }
    size_t i, j, index;
    //# Set the bits in the last byte (if needed).
    if (endBit2 > 0) {
      index = endByte * 8 - startBit;  // = nvalues - endBit2;
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
    if (startByte < endByte) {
      boolToBit (bits+startByte, data+index, 8*(endByte-startByte));
    }
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


// Check optimized conversions.
void checkPerf()
{
  cout << "checkAll ..." << endl;
  uChar bits[256];
  for (uInt i=0; i<256; ++i) {
    bits[i]= i;
  }
  Bool flagArr[8*260];
  // Make sure to use an aligned array.
  Bool* flags = flagArr;
  cout << "unaligned flag pointer " << flags << endl;
  flags = (Bool*)(8 * (((unsigned long long)flags-1)/8 + 1));
  cout << "  aligned flag pointer " << flags << endl;
  // Time difference between old and optimized version.
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      Conversion::bitToBool (flags+1, bits, 8*256);
    }
    timer.show("unaligned");
  }
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      Conversion::bitToBool (flags, bits, 8*256);
    }
    timer.show("aligned  ");
  }
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      Conversion::boolToBit (bits, flags, 8*256);
    }
    timer.show("convtobit  ");
  }
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      boolToBit (bits, flags, 8*256);
    }
    timer.show("toBit      ");
  }
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      bool2char (bits, flags, 8*256);
    }
    timer.show("tochar     ");
  }
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      boolToBit (bits, flags, 1, 8*256-2);
    }
    timer.show("parttobit  ");
  }
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      Conversion::boolToBit (bits, flags, 1, 8*256-2);
    }
    timer.show("cparttobit ");
  }
  {
    Timer timer;
    for (int i=0; i<100000; ++i) {
      Conversion::bitToBool (flags, bits, 1, 8*256-2);
    }
    timer.show("cparttobool");
  }
}

int main()
{
    checkPerf();
    cout << "OK" << endl;
    return 0;
}
