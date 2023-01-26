//# ModCompConversion.h: A class with static functions to convert ModComp format
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

#ifndef CASA_MODCOMPCONVERSION_H
#define CASA_MODCOMPCONVERSION_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/CanonicalConversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define the canonical sizes of the built-in data types.
// These are the same for all machine architectures.

#define SIZE_MODCOMP_CHAR     1
#define SIZE_MODCOMP_UCHAR    1
#define SIZE_MODCOMP_SHORT    2
#define SIZE_MODCOMP_USHORT   2
#define SIZE_MODCOMP_INT      4
#define SIZE_MODCOMP_UINT     4
#define SIZE_MODCOMP_INT64    4
#define SIZE_MODCOMP_UINT64   4
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
#define CONVERT_MODCOMP_INT64    1
#define CONVERT_MODCOMP_UINT64   1
#else
// Conversion is not needed for integers if the local lengths for Shorts and
// Ints is 2,4 respectively.
#define CONVERT_MODCOMP_SHORT    0
#define CONVERT_MODCOMP_USHORT   0
#define CONVERT_MODCOMP_INT      0
#define CONVERT_MODCOMP_UINT     0
#define CONVERT_MODCOMP_INT64    1
#define CONVERT_MODCOMP_UINT64   1
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
// 4-byte integers and Ints (or uInts, Int64s or uInt64s).
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
  static size_t toLocal(char&   to, const void* from);
  static size_t toLocal(unsigned char&  to, const void* from);
  static size_t toLocal(int16_t&  to, const void* from);
  static size_t toLocal(uint16_t& to, const void* from);
  static size_t toLocal(int32_t&    to, const void* from);
  static size_t toLocal(uint32_t&   to, const void* from);
  static size_t toLocal(int64_t&  to, const void* from);
  static size_t toLocal(uint64_t& to, const void* from);
  static size_t toLocal(float&  to, const void* from);
  static size_t toLocal(double& to, const void* from);
  // </group>
    
  // Convert nr values from Modcomp format to local format.
  // The from and to buffer should not overlap.
  // <group>
  static size_t toLocal(char*   to, const void* from, size_t nr);
  static size_t toLocal(unsigned char*  to, const void* from, size_t nr);
  static size_t toLocal(int16_t*  to, const void* from, size_t nr);
  static size_t toLocal(uint16_t* to, const void* from, size_t nr);
  static size_t toLocal(int32_t*    to, const void* from, size_t nr);
  static size_t toLocal(uint32_t*   to, const void* from, size_t nr);
  static size_t toLocal(int64_t*  to, const void* from, size_t nr);
  static size_t toLocal(uint64_t* to, const void* from, size_t nr);
  static size_t toLocal(float*  to, const void* from, size_t nr);
  static size_t toLocal(double* to, const void* from, size_t nr);
  // </group>

  // Convert one value from local format to Modcomp format.  The from and to
  // buffer should not overlap. The floating point functions will throw
  // exceptions as they are not implemented yet.
  // <group>
  static size_t fromLocal(void* to, char   from);
  static size_t fromLocal(void* to, unsigned char  from);
  static size_t fromLocal(void* to, int16_t  from);
  static size_t fromLocal(void* to, uint16_t from);
  static size_t fromLocal(void* to, int32_t    from);
  static size_t fromLocal(void* to, uint32_t   from);
  static size_t fromLocal(void* to, int64_t  from);
  static size_t fromLocal(void* to, uint64_t from);
  static size_t fromLocal(void* to, float  from);
  static size_t fromLocal(void* to, double from);
  // </group>
    
  // Convert nr values from local format to Modcomp format.  The from and to
  // buffer should not overlap. The floating point functions will throw
  // exceptions as they are not implemented yet.  
  // <group>
  static size_t fromLocal(void* to, const char*   from, size_t nr);
  static size_t fromLocal(void* to, const unsigned char*  from, size_t nr);
  static size_t fromLocal(void* to, const int16_t*  from, size_t nr);
  static size_t fromLocal(void* to, const uint16_t* from, size_t nr);
  static size_t fromLocal(void* to, const int32_t*    from, size_t nr);
  static size_t fromLocal(void* to, const uint32_t*   from, size_t nr);
  static size_t fromLocal(void* to, const int64_t*  from, size_t nr);
  static size_t fromLocal(void* to, const uint64_t* from, size_t nr);
  static size_t fromLocal(void* to, const float*  from, size_t nr);
  static size_t fromLocal(void* to, const double* from, size_t nr);
  // </group>
    
private:
  // This class should not be constructed
  // (so declare the constructor private).
  ModcompConversion();
};

inline size_t ModcompConversion::toLocal(char& to, const void* from) {
  return CanonicalConversion::toLocal(to, from);
}

inline size_t ModcompConversion::toLocal(unsigned char& to, const void* from) {
  return CanonicalConversion::toLocal(to, from);
}

inline size_t ModcompConversion::toLocal(int16_t& to, const void* from) {
  return CanonicalConversion::toLocal(to, from);
}

inline size_t ModcompConversion::toLocal(uint16_t& to, const void* from) {
  return CanonicalConversion::toLocal(to, from);
}

inline size_t ModcompConversion::toLocal(int32_t& to, const void* from) {
  return CanonicalConversion::toLocal(to, from);
}

inline size_t ModcompConversion::toLocal(uint32_t& to, const void* from) {
  return CanonicalConversion::toLocal(to, from);
}

inline size_t ModcompConversion::toLocal(int64_t& to, const void* from) {
    int32_t tmp;
    size_t res = toLocal (tmp, from);
    to = tmp;
    return res;
}

inline size_t ModcompConversion::toLocal(uint64_t& to, const void* from) {
    uint32_t tmp;
    size_t res = toLocal (tmp, from);
    to = tmp;
    return res;
}

inline size_t ModcompConversion::toLocal(float& to, const void* from) {
  return ModcompConversion::toLocal(&to, from, 1u);
}

inline size_t ModcompConversion::toLocal(double& to, const void* from) {
  return ModcompConversion::toLocal(&to, from, 1u);
}

inline size_t ModcompConversion::toLocal(char* to, const void* from, size_t nr) {
  return CanonicalConversion::toLocalChar(to, from, nr);
}

inline size_t ModcompConversion::toLocal(unsigned char* to, const void* from, size_t nr) {
  return CanonicalConversion::toLocalUChar(to, from, nr);
}

inline size_t ModcompConversion::toLocal(int16_t* to, const void* from, size_t nr) {
  return CanonicalConversion::toLocalShort(to, from, nr);
}

inline size_t ModcompConversion::toLocal(uint16_t* to, const void* from, size_t nr) {
  return CanonicalConversion::toLocalUShort(to, from, nr);
}

inline size_t ModcompConversion::toLocal(int32_t* to, const void* from, size_t nr) {
  return CanonicalConversion::toLocalInt(to, from, nr);
}

inline size_t ModcompConversion::toLocal(uint32_t* to, const void* from, size_t nr) {
  return CanonicalConversion::toLocalUInt(to, from, nr);
}

inline size_t ModcompConversion::fromLocal(void* to, char from) {
  return CanonicalConversion::fromLocal(to, from);
}

inline size_t ModcompConversion::fromLocal(void* to, unsigned char from) {
  return CanonicalConversion::fromLocal(to, from);
}

inline size_t ModcompConversion::fromLocal(void* to, int16_t from) {
  return CanonicalConversion::fromLocal (to, from);
}

inline size_t ModcompConversion::fromLocal(void* to, uint16_t from) {
  return CanonicalConversion::fromLocal(to, from);
}

inline size_t ModcompConversion::fromLocal(void* to, int32_t from) {
  return CanonicalConversion::fromLocal (to, from);
}

inline size_t ModcompConversion::fromLocal(void* to, uint32_t from) {
  return CanonicalConversion::fromLocal (to, from);
}

inline size_t ModcompConversion::fromLocal(void* to, int64_t from) {
  return CanonicalConversion::fromLocal (to, (int32_t) from);
}

inline size_t ModcompConversion::fromLocal(void* to, uint64_t from) {
  return CanonicalConversion::fromLocal (to, (uint32_t) from);
}

inline size_t ModcompConversion::fromLocal(void* to, float from) {
  return ModcompConversion::fromLocal(to, &from, 1u);
}

inline size_t ModcompConversion::fromLocal(void* to, double from) {
  return ModcompConversion::fromLocal(to, &from, 1u);
}

inline size_t ModcompConversion::fromLocal(void* to, const char* from, size_t nr) {
  return CanonicalConversion::fromLocalChar(to, from, nr);
}

inline size_t ModcompConversion::fromLocal(void* to, const unsigned char* from, size_t nr){
  return CanonicalConversion::fromLocalUChar(to, from, nr);
}

inline size_t ModcompConversion::fromLocal(void* to, const int16_t* from, size_t nr){
  return CanonicalConversion::fromLocalShort(to, from, nr);
}

inline size_t ModcompConversion::fromLocal(void* to, const uint16_t* from,size_t nr){
  return CanonicalConversion::fromLocalUShort(to, from, nr);
}

inline size_t ModcompConversion::fromLocal(void* to, const int32_t* from, size_t nr) {
  return CanonicalConversion::fromLocalInt(to, from, nr);
}

inline size_t ModcompConversion::fromLocal(void* to, const uint32_t* from, size_t nr) {
  return CanonicalConversion::fromLocalUInt(to, from, nr);
}



} //# NAMESPACE CASACORE - END

#endif
