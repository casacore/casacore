//# ModCompConversion.h: A class with static functions to convert ModComp format
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

#if !defined(AIPS_MODCOMPCONVERSION_H)
#define AIPS_MODCOMPCONVERSION_H

#include <aips/aips.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/CanonicalConversion.h>

// Define the canonical sizes of the built-in data types.
// These are the same for all machine architectures.

#define SIZE_MODCOMP_CHAR     1
#define SIZE_MODCOMP_UCHAR    1
#define SIZE_MODCOMP_SHORT    2
#define SIZE_MODCOMP_USHORT   2
#define SIZE_MODCOMP_INT      4
#define SIZE_MODCOMP_UINT     4
#define SIZE_MODCOMP_LONG     4
#define SIZE_MODCOMP_ULONG    4
#define SIZE_MODCOMP_FLOAT    4
#define SIZE_MODCOMP_DOUBLE   8

// Define for each data format if a conversion is needed from the ModComp
// format to the local format (or vice-versa).  This allows for optimizations
// in, for example, AipsIO.  

// The ModComp format is ASCII for strings, a proprietary floating point format
// and 2-complement for integers (all most significant bit first) with the
// lengths as shown above.

// Change the definitions below if new architectures (whith different lengths
// for these integer types) are being used.
#define CONVERT_MODCOMP_CHAR     0
#define CONVERT_MODCOMP_UCHAR    0
// Conversion is needed for little endian architectures (like DEC and Intel),
// because the bytes have to be swapped (thus not for data with length 1).
#if defined(AIPS_LITTLE_ENDIAN)
#define CONVERT_MODCOMP_SHORT    1
#define CONVERT_MODCOMP_USHORT   1
#define CONVERT_MODCOMP_INT      1
#define CONVERT_MODCOMP_UINT     1
#define CONVERT_MODCOMP_LONG     1
#define CONVERT_MODCOMP_ULONG    1
#else
// Conversion is not needed for integers if the local lengths for Shorts,Ints
// and Longs is 2,4,4 repectively. There is no support, at the moment, for
// machines with a Long of length 8.
#define CONVERT_MODCOMP_SHORT    0
#define CONVERT_MODCOMP_USHORT   0
#define CONVERT_MODCOMP_INT      0
#define CONVERT_MODCOMP_UINT     0
#define CONVERT_MODCOMP_LONG     0
#define CONVERT_MODCOMP_ULONG    0
#endif
// Conversion is always needed for floating point data.
#define CONVERT_MODCOMP_FLOAT    1
#define CONVERT_MODCOMP_DOUBLE   1

// <summary>Static functions to convert Modcomp numeric formats</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tModcompConversion" demos="">
// </reviewed>

// <synopsis>
// This class contains static toLocal functions to convert data from Modcomp
// format to local format and vice-versa. 
// 
// The functions work on both big-endian and little-endian machines. They
// convert between Modcomp 2-byte integers and Shorts (or uShorts) and Modcomp
// 4-byte integers and Ints (or uInts, Longs or uLongs).
//
// It is currently not possible to convert floating point numbers from local
// format to Modcomp. Attempting to do so will throw an exception (AipsError).
// </synopsis>

// <motivation>
// The VLA data is stored using Modcomp numeric data formats and needs to be
// converted to the local format to be manipulated.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support conversion of floating point data to Modcomp format
//  <li> Support data type long double.
// </todo>


class ModcompConversion
{
public:
  // Convert one value from Modcomp format to local format.
  // The from and to buffer should not overlap.
  // <group>
  static void toLocal(Char&   to, const void* from);
  static void toLocal(uChar&  to, const void* from);
  static void toLocal(Short&  to, const void* from);
  static void toLocal(uShort& to, const void* from);
  static void toLocal(Int&    to, const void* from);
  static void toLocal(uInt&   to, const void* from);
  static void toLocal(Long&   to, const void* from);
  static void toLocal(uLong&  to, const void* from);
  static void toLocal(Float&  to, const void* from);
  static void toLocal(Double& to, const void* from);
  // </group>
    
  // Convert nr values from Modcomp format to local format.
  // The from and to buffer should not overlap.
  // <group>
  static void toLocal(Char*   to, const void* from, uInt nr);
  static void toLocal(uChar*  to, const void* from, uInt nr);
  static void toLocal(Short*  to, const void* from, uInt nr);
  static void toLocal(uShort* to, const void* from, uInt nr);
  static void toLocal(Int*    to, const void* from, uInt nr);
  static void toLocal(uInt*   to, const void* from, uInt nr);
  static void toLocal(Long*   to, const void* from, uInt nr);
  static void toLocal(uLong*  to, const void* from, uInt nr);
  static void toLocal(Float*  to, const void* from, uInt nr);
  static void toLocal(Double* to, const void* from, uInt nr);
  // </group>

  // Convert one value from local format to Modcomp format.  The from and to
  // buffer should not overlap. The floating point functions will throw
  // exceptions as they are not implemented yet.
  // <group>
  static void fromLocal(void* to, Char   from);
  static void fromLocal(void* to, uChar  from);
  static void fromLocal(void* to, Short  from);
  static void fromLocal(void* to, uShort from);
  static void fromLocal(void* to, Int    from);
  static void fromLocal(void* to, uInt   from);
  static void fromLocal(void* to, Long   from);
  static void fromLocal(void* to, uLong  from);
  static void fromLocal(void* to, Float  from);
  static void fromLocal(void* to, Double from);
  // </group>
    
  // Convert nr values from local format to Modcomp format.  The from and to
  // buffer should not overlap. The floating point functions will throw
  // exceptions as they are not implemented yet.  
  // <group>
  static void fromLocal(void* to, const Char*   from, uInt nr);
  static void fromLocal(void* to, const uChar*  from, uInt nr);
  static void fromLocal(void* to, const Short*  from, uInt nr);
  static void fromLocal(void* to, const uShort* from, uInt nr);
  static void fromLocal(void* to, const Int*    from, uInt nr);
  static void fromLocal(void* to, const uInt*   from, uInt nr);
  static void fromLocal(void* to, const Long*   from, uInt nr);
  static void fromLocal(void* to, const uLong*  from, uInt nr);
  static void fromLocal(void* to, const Float*  from, uInt nr);
  static void fromLocal(void* to, const Double* from, uInt nr);
  // </group>
    
private:
  // This class should not be constructed
  // (so declare the constructor private).
  ModcompConversion();
};

inline void ModcompConversion::toLocal(Char& to, const void* from) {
  CanonicalConversion::toLocal(to, from);
}

inline void ModcompConversion::toLocal(uChar& to, const void* from) {
  CanonicalConversion::toLocal(to, from);
}

inline void ModcompConversion::toLocal(Short& to, const void* from) {
  CanonicalConversion::toLocal(to, from);
}

inline void ModcompConversion::toLocal(uShort& to, const void* from) {
  CanonicalConversion::toLocal(to, from);
}

inline void ModcompConversion::toLocal(Int& to, const void* from) {
  CanonicalConversion::toLocal(to, from);
}

inline void ModcompConversion::toLocal(uInt& to, const void* from) {
  CanonicalConversion::toLocal(to, from);
}

inline void ModcompConversion::toLocal(Long& to, const void* from) {
  CanonicalConversion::toLocal((Int &) to, from);
}

inline void ModcompConversion::toLocal(uLong& to, const void* from) {
  CanonicalConversion::toLocal((uInt &) to, from);
}

inline void ModcompConversion::toLocal(Char* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalChar(to, from, nr);
}

inline void ModcompConversion::toLocal(uChar* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalUChar(to, from, nr);
}

inline void ModcompConversion::toLocal(Short* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalShort(to, from, nr);
}

inline void ModcompConversion::toLocal(uShort* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalUShort(to, from, nr);
}

inline void ModcompConversion::toLocal(Int* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalInt(to, from, nr);
}

inline void ModcompConversion::toLocal(uInt* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalUInt(to, from, nr);
}

inline void ModcompConversion::toLocal(Long* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalInt(to, from, nr);
}

inline void ModcompConversion::toLocal(uLong* to, const void* from, uInt nr) {
  CanonicalConversion::toLocalUInt(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, Char from) {
  CanonicalConversion::fromLocal(to, from);
}

inline void ModcompConversion::fromLocal(void* to, uChar from) {
  CanonicalConversion::fromLocal(to, from);
}

inline void ModcompConversion::fromLocal(void* to, Short from) {
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal(void* to, uShort from) {
  CanonicalConversion::fromLocal(to, from);
}

inline void ModcompConversion::fromLocal(void* to, Int from) {
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal(void* to, uInt from) {
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal(void* to, Long from) {
  CanonicalConversion::fromLocal (to, (Int) from);
}

inline void ModcompConversion::fromLocal(void* to, uLong from) {
  CanonicalConversion::fromLocal (to, (uInt) from);
}

inline void ModcompConversion::fromLocal(void* to, const Char* from, uInt nr) {
  CanonicalConversion::fromLocalChar(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, const uChar* from, uInt nr){
  CanonicalConversion::fromLocalUChar(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, const Short* from, uInt nr){
  CanonicalConversion::fromLocalShort(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, const uShort* from,uInt nr){
  CanonicalConversion::fromLocalUShort(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, const Int* from, uInt nr) {
  CanonicalConversion::fromLocalInt(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, const uInt* from, uInt nr) {
  CanonicalConversion::fromLocalUInt(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, const Long* from, uInt nr) {
  CanonicalConversion::fromLocalInt(to, from, nr);
}

inline void ModcompConversion::fromLocal(void* to, const uLong* from,uInt nr){ 
  CanonicalConversion::fromLocalUInt(to, from, nr);
}


#endif
