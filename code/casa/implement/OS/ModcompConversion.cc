//# ModcompConversion.cc: Static functions to convert Modcomp numeric formats
//# Copyright (C) 1998,1999
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

#include <trial/OS/ModcompConversion.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iomanip.h>

// Modcomp has one more bit in the exponent than IEEE and because it does not
// have an implicit bit two less bits in the Mantissa. It does not have any
// special numbers like NaN or Infinity. The Modcomp is big-endian (like Sun's)
void ModcompConversion::toLocal (Float* to, const void* from, uInt nr)
{
  DebugAssert(sizeof(Float) == SIZE_MODCOMP_FLOAT, AipsError);
  DebugAssert(sizeof(Short) >= 2, AipsError);
  union {
    Float asFloat;
    uChar asByte[SIZE_MODCOMP_FLOAT];
  };
  const Char* data = (const Char*) from;
  for (const Float* const last = to + nr; to < last; 
       data += SIZE_MODCOMP_FLOAT, to++) {
    // Copy the data to the union correcting for big/little ended machines
    CanonicalConversion::toLocal(asFloat, data);
    // If the number is negative then its positive value is the twos complement
    const Bool isNegative = 
      ((asByte[SIZE_MODCOMP_FLOAT-1] & 0x80) > 0) ? True : False;
    if (isNegative) { // This code takes the twos complement
      uShort i = SIZE_MODCOMP_FLOAT;
      while (i > 0) {
 	i--;
 	asByte[i] = ~asByte[i];
      }
      asByte[i]++;
      while (asByte[i] == 0 && i < SIZE_MODCOMP_FLOAT) {
 	i++;
 	asByte[i]++;
      }
    }

    Bool isZero = (asByte[SIZE_MODCOMP_FLOAT-2] & 0x3f) == 0x00;
    {
      uInt i = SIZE_MODCOMP_FLOAT-2;
      while (i > 0) {
 	i--;
 	isZero = (asByte[i] == 0x00) && isZero;
      }
    }
    if (isZero) { // Early exit if the number is zero. 
      // I am told this is common in the data so it should speed things up.
      for (uInt k = 0; k < SIZE_MODCOMP_FLOAT-1; k++) {
	asByte[k] = 0x00;
      }
      if (isNegative) { // Return a signed zero
	asByte[SIZE_MODCOMP_FLOAT-1] = 0x80;
      } else {
	asByte[SIZE_MODCOMP_FLOAT-1] = 0x00;
      }
    } else { // Number is not zero
      Short exponent = ( (asByte[SIZE_MODCOMP_FLOAT-1] & 0x7f) << 2 ) |
	               ( (asByte[SIZE_MODCOMP_FLOAT-2] & 0xc0) >> 6 );
      while ((asByte[2] & 0x20) == 0) { // See if the number is unnormalised
	{ // If so try to normalise it. 
	  // This code does a (painful) byte by byte shift by one bit.
	  uInt i = 0; 
	  Bool msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	  asByte[i] <<= 1;
	  i++;
	  while (i < SIZE_MODCOMP_FLOAT-2) {
	    Bool prevMsbIsSet = msbIsSet;
	    msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	    asByte[i] <<= 1;
	    if (prevMsbIsSet) asByte[i] |= 0x01;
	    i++;
	  }
	  asByte[i] = ((asByte[i] & 0x3f) << 1) | asByte[i] & 0xc0;
	  if (msbIsSet) asByte[i] |= 0x01;
	}
	exponent--; // Because the exponent can go negative it must be signed
      }
      if (exponent > 384) {// exponent is too big.
	for (uInt i = 0; i < SIZE_MODCOMP_FLOAT-2; i++) {
	  asByte[i] = 0x00;
	}
	asByte[SIZE_MODCOMP_FLOAT-2] = 0x80;
	if (isNegative) {
	  asByte[SIZE_MODCOMP_FLOAT-1] = 0xff; // Return a negative infinity
	} else {
	  asByte[SIZE_MODCOMP_FLOAT-1] = 0x7f;  // Return a positive infinity
	}
      } else if (exponent < 108) {// exponent is too small.
	for (uInt i = 0; i < SIZE_MODCOMP_FLOAT-1; i++) {
	  asByte[i] = 0x00;       
	}
	if (isNegative) {
	  asByte[SIZE_MODCOMP_FLOAT-1] = 0x80;  // Return a negative zero
	} else {
	  asByte[SIZE_MODCOMP_FLOAT-1] = 0x00;  // Return a positive zero
	}
      } else if (exponent > 130) { // A normal number
	// The next 2 lines assumes mantissa is normalised (as is done above) 
	exponent -= 130;
	{ // This code does a (painful) byte by byte shift by 2 bits
	  uInt i = 0; 
	  uChar msbits = asByte[i] >> 6;
	  asByte[i] <<= 2;
	  i++;
	  while (i < SIZE_MODCOMP_FLOAT-2) {
	    uChar prevMsbits = msbits;
	    msbits = asByte[i] >> 6;
	    asByte[i] <<= 2;
	    asByte[i] |= prevMsbits;
	    i++;
	  }
	  asByte[i] = (asByte[i] << 2) | msbits;
	}
	if ((exponent & 0x0001) == 0) {
	  asByte[SIZE_MODCOMP_FLOAT-2] &= 0x7f; 
	} else {
	  asByte[SIZE_MODCOMP_FLOAT-2] |= 0x80; 
	}
	asByte[SIZE_MODCOMP_FLOAT-1] = (uChar) exponent >> 1;
	if (isNegative) asByte[SIZE_MODCOMP_FLOAT-1] |= 0x80;
	// The IEEE format effectively has two more bits in the mantissa than
	// the Modcomp format. The least significant bits are always set to
	// zero. Numerically it would be better to set them randomly to zero or
	// one but I do not think the performance degradation warrents this.
      } else { // A subnormal number
	if (exponent < 129) { // need to shift mantissa to the right
	  Short shift = 129-exponent;
	  // Do a series of 8 or less bit right shifts (the painful way).
	  while (shift > 0) { // Exponent too small is already dealt with
	    const Short thisShift = shift > 8 ? 8 : shift;
	    const Short compShift = 8-thisShift;
	    uInt i = SIZE_MODCOMP_FLOAT-2;
	    asByte[i] &= 0x3f;
	    uChar lsbits = asByte[i] << compShift;
	    asByte[i] >>= thisShift;
	    i--;
	    while (i > 0) {
	      uChar prevLsbits = lsbits;
	      lsbits = asByte[i] << compShift;
	      asByte[i] >>= thisShift; asByte[i] |= prevLsbits;
	      i--;
	    }
	    asByte[i] >>= thisShift; asByte[i] |= lsbits;
	    shift -= 8;
	  }
	} else if (exponent == 130) { // need to shift mantissa to the left
	  // This code does a (painful) byte by byte left shift by one bit.
	  uInt i = 0; 
	  Bool msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	  asByte[i] <<= 1;
	  i++;
	  while (i < SIZE_MODCOMP_FLOAT-2) {
	    Bool prevMsbIsSet = msbIsSet;
	    msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	    asByte[i] <<= 1;
	    if (prevMsbIsSet) asByte[i] |= 0x01;
	    i++;
	  }
	  asByte[i] = (asByte[i] & 0x3f) << 1;
	  if (msbIsSet) asByte[i] |= 0x01;
	} else { // exponent == 129. No need to shift the mantissa
	  asByte[SIZE_MODCOMP_FLOAT-2] &= 0x3f;
	}
	if (isNegative) {// the exponent is always zero, so just fixup the sign
	  asByte[SIZE_MODCOMP_FLOAT-1] = 0x80;
	} else {
	  asByte[SIZE_MODCOMP_FLOAT-1] = 0x00;
	}
      }
    }
    *to = asFloat; 
  }
}

// Modcomp has one more bit in the exponent than IEEE and because it does not
// have an implicit bit two less bits in the Mantissa. It does not have any
// special numbers like NaN or Infinity. The Modcomp is big-endian (like Sun's)
void ModcompConversion::toLocal (Double* to, const void* from, uInt nr)
{
}
// // Modcomp D-float has format SEEEEEEE EFFFFFFF ...
// // IEEE double has format SEEEEEEE EEEEFFFF ...
// // The Modcomp exponent is 2 higher.
// // The Modcomp byte order is b1b0b3b2b5b4b7b6.
// void ModcompConversion::toLocal (double* to, const void* from,
// 			     uInt nr)
// {
//     assert (sizeof(uInt) == 4);
//     assert (sizeof(double) == 8);
//     const char* data = (const char*)from;
//     double* last = to + nr;
//     uInt value, rest;
//     while (to < last) {
// 	moveFloat (&value, data);
// 	moveFloat (&rest, data+4);
// 	data += 8;
// 	uInt exponent = (value & 0x7f800000) >> 23;
// 	if (exponent == 0) {
// 	    value = 0;
// 	    rest  = 0;
// 	}else{
// 	    exponent = (exponent + (1022-128)) << 20;
// 	    rest  = (value << 29) | (rest >> 3);
// 	    value = (value & 0x80000000) | exponent
// 	            | ((value >> 3) & 0x000fffff);
// 	}
// #if defined(AIPS_LITTLE_ENDIAN)
// 	((uInt*)to)[0] = rest;
// 	((uInt*)to)[1] = value;
// #else
// 	((uInt*)to)[0] = value;
// 	((uInt*)to)[1] = rest;
// #endif
// 	to++;
//     }
// }


// void ModcompConversion::fromLocal (void* to, const float* from,
// 			       uInt nr)
// {
//     assert (sizeof(uInt) == 4);
//     assert (sizeof(float) == 4);
//     char* data = (char*)to;
//     const float* last = from + nr;
//     while (from < last) {
// 	uInt value;
// 	value = *(uInt*)from;
// 	uInt exponent = (value & 0x7f800000) >> 23;
// 	if (exponent == 0) {
// 	    value = 0;
// 	}else{
// 	    exponent += 2;
// 	    if (exponent > 255) {
// 		value |= 0x7fffffff;
// 	    }else{
// 		value = (value & 0x807fffff) | (exponent << 23);
// 	    }
// 	}
// 	moveFloat (data, &value);
// 	data += 4;
// 	from++;
//     }
// }

// void ModcompConversion::fromLocal (void* to, const double* from,
// 			       uInt nr)
// {
//     assert (sizeof(uInt) == 4);
//     assert (sizeof(double) == 8);
//     char* data = (char*)to;
//     const double* last = from + nr;
//     while (from < last) {
// 	uInt value, rest;
// #if defined(AIPS_LITTLE_ENDIAN)
// 	rest  = ((uInt*)from)[0];
// 	value = ((uInt*)from)[1];
// #else
// 	value = ((uInt*)from)[0];
// 	rest  = ((uInt*)from)[1];
// #endif
// 	uInt exponent = (value & 0x7ff00000) >> 20;
// 	exponent -= 1022-128;
// 	if (exponent <= 0) {
// 	    exponent = 0;
// 	    value = 0;
// 	    rest  = 0;
// 	}else{
// 	    if (exponent > 255) {
// 		value |= 0x7fffffff;
// 		rest  = 0xffffffff;
// 	    }else{
// 		value = (value & 0x80000000) | (exponent << 23)
// 		        | ((value << 3) & 0x007fffff) | (rest >> 29);
// 		rest <<= 3;
// 	    }
// 	}
// 	moveFloat (data, &value);
// 	moveFloat (data+4, &rest);
// 	data += 8;
// 	from++;
//     }
// }
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ModcompConversion; cd test; gmake tModcompConversion"
// End: 
