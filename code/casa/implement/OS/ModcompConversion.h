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

//# Includes
#include <aips/aips.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/CanonicalConversion.h>

// Define the canonical sizes of the built-in data types.
// These are the same for all machine architectures.

#define SIZE_MODCOMP_CHAR     1
#define SIZE_MODCOMP_UCHAR    1
#define SIZE_MODCOMP_SHORT    2
#define SIZE_MODCOMP_USHORT   2
#define SIZE_MODCOMP_INT      2
#define SIZE_MODCOMP_UINT     2
#define SIZE_MODCOMP_LONG     4
#define SIZE_MODCOMP_ULONG    4
#define SIZE_MODCOMP_FLOAT    4
#define SIZE_MODCOMP_DOUBLE   8

// <summary>Static functions to convert Modcomp numeric formats</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tModcompConversion" demos="">
// </reviewed>

// <synopsis>
// This class contains static toLocal functions to convert data from Modcomp
// format to local format and vice-versa. It only handles Modcomp D-float format.
// Another class should be implemented to handle Modcomp G-float format.
// <p>
// The functions work well on big-endian as well as little-endian machines.
// </synopsis>
// <motivation>
// Archived WSRT data can be stored in the old Modcomp format
// (little-endian and Modcomp D-float floating point format).
// Conversion functions are needed to read these data.
// </motivation>

// <todo asof="$DATE$">
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
  static void toLocal (Char*   to, const void* from, uInt nr);
  static void toLocal (uChar*  to, const void* from, uInt nr);
  static void toLocal (Short*  to, const void* from, uInt nr);
  static void toLocal (uShort* to, const void* from, uInt nr);
  static void toLocal (Int*    to, const void* from, uInt nr);
  static void toLocal (uInt*   to, const void* from, uInt nr);
  static void toLocal (Long*   to, const void* from, uInt nr);
  static void toLocal (uLong*  to, const void* from, uInt nr);
  static void toLocal (Float*  to, const void* from, uInt nr);
  static void toLocal (Double* to, const void* from, uInt nr);
  // </group>

  // Convert one value from local format to Modcomp format.
  // The from and to buffer should not overlap.
  // <group>
  static void fromLocal (void* to, Char   from);
  static void fromLocal (void* to, uChar  from);
  static void fromLocal (void* to, Short  from);
  static void fromLocal (void* to, uShort from);
  static void fromLocal (void* to, Int    from);
  static void fromLocal (void* to, uInt   from);
  static void fromLocal (void* to, Long   from);
  static void fromLocal (void* to, uLong  from);
//     static void fromLocal (void* to, Float  from);
//     static void fromLocal (void* to, Double from);
  // </group>
    
  // Convert nr values from local format to Modcomp format.
  // The from and to buffer should not overlap.
  // <group>
  static void fromLocal (void* to, const Char*   from, uInt nr);
  static void fromLocal (void* to, const uChar*  from, uInt nr);
  static void fromLocal (void* to, const Short*  from, uInt nr);
  static void fromLocal (void* to, const uShort* from, uInt nr);
  static void fromLocal (void* to, const Int*    from, uInt nr);
  static void fromLocal (void* to, const uInt*   from, uInt nr);
  static void fromLocal (void* to, const Long*   from, uInt nr);
  static void fromLocal (void* to, const uLong*  from, uInt nr);
//     static void fromLocal (void* to, const Float*  from, uInt nr);
//     static void fromLocal (void* to, const Double* from, uInt nr);
  // </group>
    
private:
  // This class should not be constructed
  // (so declare the constructor private).
  ModcompConversion();
};

inline void ModcompConversion::toLocal (Char& to, const void* from)
{
  CanonicalConversion::toLocal (to, from);
}

inline void ModcompConversion::toLocal (uChar& to, const void* from)
{
  CanonicalConversion::toLocal (to, from);
}

inline void ModcompConversion::toLocal (Short& to, const void* from)
{
    CanonicalConversion::toLocal (to, from);
}

inline void ModcompConversion::toLocal (uShort& to, const void* from)
{
    CanonicalConversion::toLocal (to, from);
}

inline void ModcompConversion::toLocal (Int& to, const void* from)
{
  CanonicalConversion::toLocal (to, from);
}

inline void ModcompConversion::toLocal (uInt& to, const void* from)
{
  CanonicalConversion::toLocal (to, from);
}

inline void ModcompConversion::toLocal (Long& to, const void* from)
{
  Int to4;
  CanonicalConversion::toLocal (to4, from);
  to = to4;
}

inline void ModcompConversion::toLocal (uLong& to, const void* from)
{
  uInt to4;
  CanonicalConversion::toLocal (to4, from);
  to = to4;
}

// inline void ModcompConversion::toLocal (Float& to, const void* from)
// {
//     toLocal (&to, from, 1);
// }

// inline void ModcompConversion::toLocal (Double& to, const void* from)
// {
//     toLocal (&to, from, 1);
// }

inline void ModcompConversion::toLocal (Char* to, const void* from, uInt nr)
{
  CanonicalConversion::toLocal (to, from, nr);
}

inline void ModcompConversion::toLocal (uChar* to, const void* from, uInt nr)
{
  CanonicalConversion::toLocal (to, from, nr);
}

inline void ModcompConversion::toLocal (Short* to, const void* from, uInt nr)
{
  CanonicalConversion::toLocal (to, from, nr);
}

inline void ModcompConversion::toLocal (uShort* to, const void* from, uInt nr)
{
  CanonicalConversion::toLocal (to, from, nr);
}

inline void ModcompConversion::toLocal (Int* to, const void* from, uInt nr)
{
  CanonicalConversion::toLocal (to, from, nr);
}

inline void ModcompConversion::toLocal (uInt* to, const void* from, uInt nr)
{
  CanonicalConversion::toLocal (to, from, nr);
}

inline void ModcompConversion::toLocal (Long* to, const void* from, uInt nr)
{
  const Char * fromPtr = (const Char *) from;
  while (nr > 0) {
    ModcompConversion::toLocal (*to, fromPtr);
    fromPtr += 4;
    to++;
    nr--;
  }
}

inline void ModcompConversion::toLocal (uLong* to, const void* from, uInt nr)
{
  const Char * fromPtr = (const Char *) from;
  while (nr > 0) {
    ModcompConversion::toLocal (*to, fromPtr);
    fromPtr += 4;
    to++;
    nr--;
  }
}


inline void ModcompConversion::fromLocal (void* to, Char from)
{
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal (void* to, uChar from)
{
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal (void* to, Short from)
{
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal (void* to, uShort from)
{
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal (void* to, Int from)
{
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal (void* to, uInt from)
{
  CanonicalConversion::fromLocal (to, from);
}

inline void ModcompConversion::fromLocal (void* to, Long from)
{
  CanonicalConversion::fromLocal (to, (Int) from);
}

inline void ModcompConversion::fromLocal (void* to, uLong from)
{
  CanonicalConversion::fromLocal (to, (uInt) from);
}

// inline void ModcompConversion::fromLocal (void* to, Float from)
// {
//   ModcompConversion::fromLocal (to, &from, 1);
// }

// inline void ModcompConversion::fromLocal (void* to, Double from)
// {
//   ModcompConversion::fromLocal (to, &from, 1);
// }

inline void ModcompConversion::fromLocal (void* to, const Char* from, uInt nr)
{ 
  CanonicalConversion::fromLocal (to, from, nr);
}

inline void ModcompConversion::fromLocal (void* to, const uChar* from, uInt nr)
{ 
  CanonicalConversion::fromLocal (to, from, nr);
}

inline void ModcompConversion::fromLocal (void* to, const Short* from, uInt nr)
{ 
  CanonicalConversion::fromLocal (to, from, nr);
}

inline void ModcompConversion::fromLocal (void* to, const uShort* from, 
					  uInt nr)
{ 
  CanonicalConversion::fromLocal (to, from, nr);
}

inline void ModcompConversion::fromLocal (void* to, const Int* from, uInt nr)
{ 
  CanonicalConversion::fromLocal (to, from, nr);
}

inline void ModcompConversion::fromLocal (void* to, const uInt* from, uInt nr)
{ 
  CanonicalConversion::fromLocal (to, from, nr);
}

inline void ModcompConversion::fromLocal (void* to, const Long* from, uInt nr)
{ 
  Char * toPtr = (Char *) to;
  while (nr > 0) {
    ModcompConversion::fromLocal (toPtr, *from);
    from++;
    toPtr += 4;
    nr--;
  }
}

inline void ModcompConversion::fromLocal (void* to, const uLong* from, uInt nr)
{ 
  Char * toPtr = (Char *) to;
  while (nr > 0) {
    ModcompConversion::fromLocal (toPtr, *from);
    from++;
    toPtr += 4;
    nr--;
  }
}


#endif
