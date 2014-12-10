//# ModcompConversion.cc: Static functions to convert Modcomp numeric formats
//# Copyright (C) 1998,1999,2001
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

#include <casacore/casa/OS/ModcompConversion.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

size_t ModcompConversion::toLocal (Int64* to, const void* from,
                                   size_t nr)
{
    const char* data = (const char*)from;
    Int64* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += SIZE_MODCOMP_INT64;
    }
    return nr*SIZE_MODCOMP_INT64;
}

size_t ModcompConversion::toLocal (uInt64* to, const void* from,
                                   size_t nr)
{ 
    const char* data = (const char*)from;
    uInt64* last = to + nr;
    while (to < last) {
	toLocal (*to++, data);
	data += SIZE_MODCOMP_UINT64;
    }
    return nr*SIZE_MODCOMP_UINT64;
}

size_t ModcompConversion::fromLocal (void* to, const Int64* from,
                                     size_t nr)
{
    char* data = (char*)to;
    const Int64* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += SIZE_MODCOMP_INT64;
    }
    return nr*SIZE_MODCOMP_INT64;
}

size_t ModcompConversion::fromLocal (void* to, const uInt64* from,
                                     size_t nr)
{ 
    char* data = (char*)to;
    const uInt64* last = from + nr;
    while (from < last) {
	fromLocal (data, *from++);
	data += SIZE_MODCOMP_UINT64;
    }
    return nr*SIZE_MODCOMP_UINT64;
}


// Modcomp has one more bit in the exponent than IEEE and because it does not
// have an implicit bit two less bits in the Mantissa. It does not have any
// special numbers like NaN or Infinity. The Modcomp is big-endian (like Sun's)
size_t ModcompConversion::toLocal (Float* to, const void* from, size_t nr) {
  DebugAssert(sizeof(Short) >= 2, AipsError);
  uChar asByte[SIZE_MODCOMP_FLOAT];
  size_t retval = 0;

  const uChar* data = (const uChar*) from;
  for (const Float* const last = to + nr; to < last; to++) {
    // Copy the data to the temporary buffer
    for (uShort i = 0; i < SIZE_MODCOMP_FLOAT; i++) {
      asByte[i] = *data;
      data++;
    }
    // If the number is negative then its positive value is the twos complement
    const Bool isNegative = ((asByte[0] & 0x80) > 0) ? True : False;
    if (isNegative) { // This code takes the twos complement
      uShort i = 0;
      while (i < SIZE_MODCOMP_FLOAT) {
 	asByte[i] = ~asByte[i];
 	i++;
      }
      i--;
      asByte[i]++;
      while (asByte[i] == 0 && i > 0) {
 	i--;
 	asByte[i]++;
      }
    }

    Bool isZero = (asByte[1] & 0x3f) == 0x00;
    for (uShort i = 2; i < SIZE_MODCOMP_FLOAT; i++) {
      isZero = (asByte[i] == 0x00) && isZero;
    }

    if (isZero) { // Early exit if the number is zero. 
      // I am told this is common in the data so it should speed things up.
      for (uShort k = 1; k < SIZE_MODCOMP_FLOAT; k++) {
	asByte[k] = 0x00;
      }
      if (isNegative) { // Return a signed zero
	asByte[0] = 0x80;
      } else {
	asByte[0] = 0x00;
      }
    } else { // Number is not zero
      Short exponent = ((asByte[0] & 0x7f) << 2) | ((asByte[1] & 0xc0) >> 6);
      while ((asByte[1] & 0x20) == 0) { // See if the number is unnormalised
	{ // If so try to normalise it. 
	  // This code does a byte by byte left shift by one bit (painful).
	  uShort i = SIZE_MODCOMP_FLOAT-1; 
	  Bool msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	  asByte[i] <<= 1;
	  i--;
	  while (i > 1) {
	    Bool prevMsbIsSet = msbIsSet;
	    msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	    asByte[i] <<= 1;
	    if (prevMsbIsSet) asByte[i] |= 0x01;
	    i--;
	  }
	  asByte[1] = ((asByte[1] & 0x3f) << 1) | (asByte[1] & 0xc0);
	  if (msbIsSet) asByte[1] |= 0x01;
	}
	exponent--; // Because the exponent can go negative it must be signed
      }
      if (exponent > 384) {// exponent is too big.
	for (uShort i = 2; i < SIZE_MODCOMP_FLOAT; i++) {
	  asByte[i] = 0x00;
	}
	asByte[1] = 0x80;
	if (isNegative) {
	  asByte[0] = 0xff; // Return a negative infinity
	} else {
	  asByte[0] = 0x7f; // Return a positive infinity
	}
      } else if (exponent < 108) {// exponent is too small.
	for (uShort i = 1; i < SIZE_MODCOMP_FLOAT; i++) {
	  asByte[i] = 0x00;       
	}
	if (isNegative) {
	  asByte[0] = 0x80;  // Return a negative zero
	} else {
	  asByte[0] = 0x00;  // Return a positive zero
	}
      } else if (exponent > 130) { // A normal number
	// The next 2 lines assumes mantissa is normalised (as is done above) 
	exponent -= 130;
	{ // This code does a byte by byte left shift by 2 bits (painful)
	  uShort i = SIZE_MODCOMP_FLOAT-1;
	  uChar msbits = asByte[i] >> 6;
	  asByte[i] <<= 2;
	  i--;
	  while (i > 1) {
	    uChar prevMsbits = msbits;
	    msbits = asByte[i] >> 6;
	    asByte[i] <<= 2;
	    asByte[i] |= prevMsbits;
	    i--;
	  }
	  asByte[1] = (asByte[1] << 2) | msbits;
	}
	if ((exponent & 0x0001) == 0) {
	  asByte[1] &= 0x7f; 
	} else {
	  asByte[1] |= 0x80; 
	}
	asByte[0] = (uChar) exponent >> 1;
	if (isNegative) asByte[0] |= 0x80;
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
	    asByte[1] &= 0x3f;
	    uChar lsbits = asByte[1] << compShift;
	    asByte[1] >>= thisShift;
	    uShort i = 2;
	    while (i < SIZE_MODCOMP_FLOAT-1) {
	      uChar prevLsbits = lsbits;
	      lsbits = asByte[i] << compShift;
	      asByte[i] >>= thisShift; 
	      asByte[i] |= prevLsbits;
	      i++;
	    }
	    asByte[SIZE_MODCOMP_FLOAT-1] >>= thisShift; 
	    asByte[SIZE_MODCOMP_FLOAT-1] |= lsbits;
	    shift -= 8;
	  }
	} else if (exponent == 130) { // need to shift mantissa to the left
	  // This code does a (painful) byte by byte left shift by one bit.
	  uShort i = SIZE_MODCOMP_FLOAT-1;
	  Bool msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	  asByte[i] <<= 1;
	  i--;
	  while (i > 1) {
	    Bool prevMsbIsSet = msbIsSet;
	    msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
	    asByte[i] <<= 1;
	    if (prevMsbIsSet) asByte[i] |= 0x01;
	    i--;
	  }
	  asByte[1] = (asByte[1] & 0x3f) << 1;
	  if (msbIsSet) asByte[1] |= 0x01;
	} else { // exponent == 129. No need to shift the mantissa
	  asByte[1] &= 0x3f;
	}
	if (isNegative) {// the exponent is always zero, so just fixup the sign
	  asByte[0] = 0x80;
	} else {
	  asByte[0] = 0x00;
	}
      }
    }
    retval += CanonicalConversion::toLocal(*to, asByte);
  }
  return retval;
}

// Modcomp has one more bit in the exponent than IEEE and because it does not
// have an implicit bit two less bits in the Mantissa. It does not have any
// special numbers like NaN or Infinity. The Modcomp is big-endian (like Sun's)
size_t ModcompConversion::toLocal (Double* to, const void* from, size_t nr) {
  DebugAssert(sizeof(Short) >= 2, AipsError);
  uChar asByte[SIZE_MODCOMP_DOUBLE];
  size_t retval = 0;
  const uChar* data = (const uChar*) from;
  for (const Double* const last = to + nr; to < last; to++) {
    // Copy the data to temporary buffer
    for (uShort i = 0; i < SIZE_MODCOMP_DOUBLE; i++) {
      asByte[i] = *data;
      data++;
    }
    // If the number is negative then its positive value is the twos complement
    const Bool isNegative = ((asByte[0] & 0x80) > 0) ? True : False;
    if (isNegative) { // This code takes the twos complement
      uShort i = 0;
      while (i < SIZE_MODCOMP_DOUBLE) {
 	asByte[i] = ~asByte[i];
 	i++;
      }
      i--;
      asByte[i]++;
      while (asByte[i] == 0 && i > 0) {
 	i--;
 	asByte[i]++;
      }
    }

    Bool isZero = (asByte[1] & 0x3f) == 0x00;
    for (uShort i = 2; i < SIZE_MODCOMP_DOUBLE; i++) {
      isZero = (asByte[i] == 0x00) && isZero;
    }

    if (isZero) { // Early exit if the number is zero. 
      // I am told this is common in the data so it should speed things up.
      for (uShort k = 1; k < SIZE_MODCOMP_DOUBLE; k++) {
	asByte[k] = 0x00;
      }
      if (isNegative) { // Return a signed zero
	asByte[0] = 0x80;
      } else {
	asByte[0] = 0x00;
      }
    } else { // Number is not zero
      Short exponent = ((asByte[0] & 0x7f) << 2) | ((asByte[1] & 0xc0) >> 6);
      while ((asByte[1] & 0x20) == 0) { // See if the number is unnormalised
 	{ // If so try to normalise it. 
 	  // This code does a byte by byte left shift by one bit (painful).
 	  uShort i = SIZE_MODCOMP_DOUBLE-1; 
 	  Bool msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
 	  asByte[i] <<= 1;
 	  i--;
 	  while (i > 1) {
 	    Bool prevMsbIsSet = msbIsSet;
 	    msbIsSet = (asByte[i] & (0x80)) > 0 ? True : False;
 	    asByte[i] <<= 1;
 	    if (prevMsbIsSet) asByte[i] |= 0x01;
 	    i--;
 	  }
 	  asByte[1] = ((asByte[1] & 0x3f) << 1) | (asByte[1] & 0xc0);
 	  if (msbIsSet) asByte[1] |= 0x01;
 	}
 	exponent--; // Because the exponent can go negative it must be signed
      }
      // In IEEE double precision the exponent is 11 bits, whereas the
      // Modcomp's is only 9 bits. So we never have to worry about:
      // * Numbers that are too big (overflow). Set to infinity.
      // * Numbers that are too small (underflow). Set to zero or generate
      //   a subnormal number.
      // Hence, unlike the single precision code, every Modcomp double
      // precision number can be represented as a 'normal' IEEE double
      // precision number.

      // The mantissa of the Modcomp double precision number is 54 bits,
      // whereas it is 52 bits in the IEEE format. But only one bit of
      // precision is lost in the conversion because the IEEE format has an
      // implicit one at the beginning of the mantissa whereas the Modcomp has
      // an explicit one (that wastes a bit).

      // The next few lines assumes mantissa is normalised (as is done above)
      exponent += 766;
      { // This code does a byte by byte right shift by 1-bit (painful)
	for (uShort i = SIZE_MODCOMP_DOUBLE-1; i > 1; i--) {
	  asByte[i] >>= 1;
	  asByte[i] |= (asByte[i-1] & 0x01) << 7;
	}
	asByte[1] >>= 1;
      }
      asByte[1] &= 0x0f;
      asByte[1] |= uChar(exponent & 0x000f) << 4;
      asByte[0] = uChar(exponent >> 4);
      if (isNegative) {
	asByte[0] |= 0x80;
      } else {
	asByte[0] &= 0x7f;
      }
      // The IEEE format effectively has two more bits in the mantissa than
      // the Modcomp format. The least significant bits are always set to
      // zero. Numerically it would be better to set them randomly to zero or
      // one but I do not think the performance degradation warrents this.
    }
    retval += CanonicalConversion::toLocal(*to, asByte);
  }
  return retval;
}

size_t ModcompConversion::fromLocal(void* to, const Float* from, size_t nr) {
  // Dummy statements to suppress compiler warnings about unused variables
  if (nr == 0) {}
  if (from == 0) {}
  if (to != 0) {}
  throw(AipsError("ModcompConversion::fromLocal(Float&, const void*) - "
		  "Cannot convert floating point numbers to Modcomp format"));
  return 0;
}

size_t ModcompConversion::fromLocal(void* to, const Double* from, size_t nr) {
  // Dummy statements to suppress compiler warnings about unused variables
  if (nr == 0) {}
  if (from == 0) {}
  if (to != 0) {}
  throw(AipsError("ModcompConversion::fromLocal(Double&, const void*) - "
		  "Cannot convert floating point numbers to Modcomp format"));
  return 0;
}

// size_t ModcompConversion::fromLocal (void* to, const float* from,
// 			       size_t nr)
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

// size_t ModcompConversion::fromLocal (void* to, const double* from,
// 			       size_t nr)
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

} //# NAMESPACE CASACORE - END

