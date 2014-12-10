//# CanonicalConversion.h: A class with static functions to convert canonical format
//# Copyright (C) 1996,1997,1999,2000,2001,2002
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

#ifndef CASA_CANONICALCONVERSION_H
#define CASA_CANONICALCONVERSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Conversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define the canonical sizes of the built-in data types.
// These are the same for all machine architectures.
// Also define the maximum size.

#define SIZE_CAN_CHAR     1
#define SIZE_CAN_UCHAR    1
#define SIZE_CAN_SHORT    2
#define SIZE_CAN_USHORT   2
#define SIZE_CAN_INT      4
#define SIZE_CAN_UINT     4
#define SIZE_CAN_INT64    8
#define SIZE_CAN_UINT64   8
#define SIZE_CAN_FLOAT    4
#define SIZE_CAN_DOUBLE   8
//#//define SIZE_CAN_LDOUBLE 16


// Define for each data format if a conversion is needed from the
// local format to the canonical format (or vice-versa).
// This allows for optimizations in, for example, AipsIO.
// The canonical format is ASCII for strings, IEEE for floating point
// and 2-complement for integers (all most significant bit first)
// with the lengths as shown above.
// The function checkConvert() can be used to check if the flags are
// set correctly.

// Conversion is needed for little endian architectures (like DEC and Intel),
// because the bytes have to be swapped (thus not for data with length 1).
#define CONVERT_CAN_CHAR     0
#define CONVERT_CAN_UCHAR    0

#if defined(AIPS_LITTLE_ENDIAN)
#  define CONVERT_CAN_SHORT    1
#  define CONVERT_CAN_USHORT   1
#  define CONVERT_CAN_INT      1
#  define CONVERT_CAN_UINT     1
#  define CONVERT_CAN_INT64    1
#  define CONVERT_CAN_UINT64   1
#  define CONVERT_CAN_FLOAT    1
#  define CONVERT_CAN_DOUBLE   1
//#//#  define CONVERT_CAN_LDOUBLE  1
#else

// Conversion is not needed for IEEE data.
// Change the definitions below if new architectures are being used.
#  define CONVERT_CAN_SHORT    0
#  define CONVERT_CAN_USHORT   0
#  define CONVERT_CAN_INT      0
#  define CONVERT_CAN_UINT     0
#  define CONVERT_CAN_INT64    0
#  define CONVERT_CAN_UINT64   0
#  define CONVERT_CAN_FLOAT    0
#  define CONVERT_CAN_DOUBLE   0
// LDOUBLE is 8 bytes on SUN, but 16 bytes canonical.
//#//#  define CONVERT_CAN_LDOUBLE  1
#endif



// <summary>
// A class with static functions to convert canonical format
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tCanonicalConversion" demos="">
// </reviewed>

// <synopsis>
// This class consists of several static functions to convert
// data from local (=native) format to a canonical format.
// The canonical length of each data type is:
// <br>- Bool: 1 bit
// <br>- char: 1 byte
// <br>- short: 2 bytes
// <br>- int: 4 bytes
// <br>- Int64: 8 bytes
// <br>- float: 4 bytes
// <br>- double: 8 bytes
// <br> The canonical format is big-endian IEEE format, so on many machines
// the conversion is only a copy operation. On Alpha- or Intel-based
// machines, however, it involves a byte swap to convert from little
// endian to big endian.
// <p>
// The class also contains conversion functions making it possible to
// specify the number of bytes (in local format) instead of the number
// of values. These functions are included to make it possible to have
// the same signature as memcpy.
// <p>
// The current implementation of this class works on big- and little-endian
// machines using IEEE format. When using on other machines (e.g. VAX)
// the toLocal and fromLocal functions have to be changed.
// <p>
// Note that no functions are provided to handle Bools. Instead class
// <linkto class=Conversion>Conversion</linkto> provides functions to
// convert Bools to/from bits.
// </synopsis>

// <example>
// <srcblock>
// void someFunction (const uInt* data, uInt nrval)
// {
//     char* buffer = new char[nrval*CanonicalConversion::canonicalSize(data)];
//     CanonicalConversion::fromLocal (buffer, data, nrval);
//     ....
//     delete [] buffer;
// }
// </srcblock>
// </example>

// <motivation>
// Casacore data will often be stored in a canonical format.
// To read these data conversion functions are needed.
// However, these functions do not use any other Casacore classes,
// so they can easily be used in any other software system.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support data type long double.
// </todo>


class CanonicalConversion
{
public:
    // Convert one value from canonical format to local format.
    // The from and to buffer should not overlap.
    // <group>
    static size_t toLocal (char&           to, const void* from);
    static size_t toLocal (unsigned char&  to, const void* from);
    static size_t toLocal (short&          to, const void* from);
    static size_t toLocal (unsigned short& to, const void* from);
    static size_t toLocal (int&            to, const void* from);
    static size_t toLocal (unsigned int&   to, const void* from);
    static size_t toLocal (Int64&          to, const void* from);
    static size_t toLocal (uInt64&         to, const void* from);
    static size_t toLocal (float&          to, const void* from);
    static size_t toLocal (double&         to, const void* from);
    // </group>
    
    // Convert one value from local format to canonical format.
    // The from and to buffer should not overlap.
    //# Note that the from value is passed by reference (and not by value),
    //# because the & operator applied to it is slowish if passed by value.
    // <group>
    static size_t fromLocal (void* to, const char&           from);
    static size_t fromLocal (void* to, const unsigned char&  from);
    static size_t fromLocal (void* to, const short&          from);
    static size_t fromLocal (void* to, const unsigned short& from);
    static size_t fromLocal (void* to, const int&            from);
    static size_t fromLocal (void* to, const unsigned int&   from);
    static size_t fromLocal (void* to, const Int64&          from);
    static size_t fromLocal (void* to, const uInt64&         from);
    static size_t fromLocal (void* to, const float&          from);
    static size_t fromLocal (void* to, const double&         from);
    // </group>
    
    // Convert nr values from canonical format to local format.
    // The from and to buffer should not overlap.
    // <group>
    static size_t toLocal (char*           to, const void* from,
                           size_t nr);
    static size_t toLocal (unsigned char*  to, const void* from,
                           size_t nr);
    static size_t toLocal (short*          to, const void* from,
                           size_t nr);
    static size_t toLocal (unsigned short* to, const void* from,
                           size_t nr);
    static size_t toLocal (int*            to, const void* from,
                           size_t nr);
    static size_t toLocal (unsigned int*   to, const void* from,
                           size_t nr);
    static size_t toLocal (Int64*          to, const void* from,
                           size_t nr);
    static size_t toLocal (uInt64*         to, const void* from,
                           size_t nr);
    static size_t toLocal (float*          to, const void* from,
                           size_t nr);
    static size_t toLocal (double*         to, const void* from,
                           size_t nr);
    // </group>

    // Convert nr values from local format to canonical format.
    // The from and to buffer should not overlap.
    // <group>
    static size_t fromLocal (void* to, const char*           from,
                             size_t nr);
    static size_t fromLocal (void* to, const unsigned char*  from,
                             size_t nr);
    static size_t fromLocal (void* to, const short*          from,
                             size_t nr);
    static size_t fromLocal (void* to, const unsigned short* from,
                             size_t nr);
    static size_t fromLocal (void* to, const int*            from,
                             size_t nr);
    static size_t fromLocal (void* to, const unsigned int*   from,
                             size_t nr);
    static size_t fromLocal (void* to, const Int64*          from,
                             size_t nr);
    static size_t fromLocal (void* to, const uInt64*         from,
                             size_t nr);
    static size_t fromLocal (void* to, const float*          from,
                             size_t nr);
    static size_t fromLocal (void* to, const double*         from,
                             size_t nr);
    // </group>

    // Convert nr values from canonical format to local format.
    // The from and to buffer should not overlap.
    // <group>
    static size_t toLocalChar     (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalUChar    (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalShort    (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalUShort   (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalInt      (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalUInt     (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalInt64    (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalUInt64   (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalFloat    (void* to, const void* from,
                                   size_t nr);
    static size_t toLocalDouble   (void* to, const void* from,
                                   size_t nr);
    // </group>

    // Convert nr values from local format to canonical format.
    // The from and to buffer should not overlap.
    // <group>
    static size_t fromLocalChar     (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalUChar    (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalShort    (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalUShort   (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalInt      (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalUInt     (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalInt64    (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalUInt64   (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalFloat    (void* to, const void* from,
                                     size_t nr);
    static size_t fromLocalDouble   (void* to, const void* from,
                                     size_t nr);
    // </group>
    
    // Convert values from canonical format to local format.
    // The from and to buffer should not overlap.
    // The number of values involved is determined from the argument
    // <src>nrbytes</src>, which gives the number of bytes in local format.
    // The signature of this function is the same as <src>memcpy</src>, so
    // that memcpy can directly be used if no conversion is needed.
    // <group>
    static void* byteToLocalChar     (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalUChar    (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalShort    (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalUShort   (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalInt      (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalUInt     (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalInt64    (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalUInt64   (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalFloat    (void* to, const void* from,
				      size_t nrbytes);
    static void* byteToLocalDouble   (void* to, const void* from,
				      size_t nrbytes);
    // </group>
    
    // Convert values from local format to canonical format.
    // The from and to buffer should not overlap.
    // The number of values involved is determined from the argument
    // <src>nrbytes</src>, which gives the number of bytes in local format.
    // The signature of this function is the same as <src>memcpy</src>, so
    // that memcpy can directly be used if no conversion is needed.
    // <group>
    static void* byteFromLocalChar     (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalUChar    (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalShort    (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalUShort   (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalInt      (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalUInt     (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalInt64    (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalUInt64   (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalFloat    (void* to, const void* from,
					size_t nrbytes);
    static void* byteFromLocalDouble   (void* to, const void* from,
					size_t nrbytes);
    // </group>
    
    // Get the value conversion function for the given type.
    // <group>
    static Conversion::ValueFunction* getToLocal (const char*);
    static Conversion::ValueFunction* getToLocal (const unsigned char*);
    static Conversion::ValueFunction* getToLocal (const short*);
    static Conversion::ValueFunction* getToLocal (const unsigned short*);
    static Conversion::ValueFunction* getToLocal (const int*);
    static Conversion::ValueFunction* getToLocal (const unsigned int*);
    static Conversion::ValueFunction* getToLocal (const Int64*);
    static Conversion::ValueFunction* getToLocal (const uInt64*);
    static Conversion::ValueFunction* getToLocal (const float*);
    static Conversion::ValueFunction* getToLocal (const double*);
    static Conversion::ValueFunction* getFromLocal (const char*);
    static Conversion::ValueFunction* getFromLocal (const unsigned char*);
    static Conversion::ValueFunction* getFromLocal (const short*);
    static Conversion::ValueFunction* getFromLocal (const unsigned short*);
    static Conversion::ValueFunction* getFromLocal (const int*);
    static Conversion::ValueFunction* getFromLocal (const unsigned int*);
    static Conversion::ValueFunction* getFromLocal (const Int64*);
    static Conversion::ValueFunction* getFromLocal (const uInt64*);
    static Conversion::ValueFunction* getFromLocal (const float*);
    static Conversion::ValueFunction* getFromLocal (const double*);
    // </group>

    // Get the byte conversion function for the given type.
    // The function <src>memcpy</src> is returned when a conversion
    // is not needed.
    // <group>
    static Conversion::ByteFunction* getByteToLocal (const char*);
    static Conversion::ByteFunction* getByteToLocal (const unsigned char*);
    static Conversion::ByteFunction* getByteToLocal (const short*);
    static Conversion::ByteFunction* getByteToLocal (const unsigned short*);
    static Conversion::ByteFunction* getByteToLocal (const int*);
    static Conversion::ByteFunction* getByteToLocal (const unsigned int*);
    static Conversion::ByteFunction* getByteToLocal (const Int64*);
    static Conversion::ByteFunction* getByteToLocal (const uInt64*);
    static Conversion::ByteFunction* getByteToLocal (const float*);
    static Conversion::ByteFunction* getByteToLocal (const double*);
    static Conversion::ByteFunction* getByteFromLocal (const char*);
    static Conversion::ByteFunction* getByteFromLocal (const unsigned char*);
    static Conversion::ByteFunction* getByteFromLocal (const short*);
    static Conversion::ByteFunction* getByteFromLocal (const unsigned short*);
    static Conversion::ByteFunction* getByteFromLocal (const int*);
    static Conversion::ByteFunction* getByteFromLocal (const unsigned int*);
    static Conversion::ByteFunction* getByteFromLocal (const Int64*);
    static Conversion::ByteFunction* getByteFromLocal (const uInt64*);
    static Conversion::ByteFunction* getByteFromLocal (const float*);
    static Conversion::ByteFunction* getByteFromLocal (const double*);
    // </group>

    // Return the canonical length for the various data types.
    // <group>
    static unsigned int canonicalSize (const char*);
    static unsigned int canonicalSize (const unsigned char*);
    static unsigned int canonicalSize (const short*);
    static unsigned int canonicalSize (const unsigned short*);
    static unsigned int canonicalSize (const int*);
    static unsigned int canonicalSize (const unsigned int*);
    static unsigned int canonicalSize (const Int64*);
    static unsigned int canonicalSize (const uInt64*);
    static unsigned int canonicalSize (const float*);
    static unsigned int canonicalSize (const double*);
    //#//static unsigned int canonicalSize (const long double*);
    // </group>

    // Reverse 2 bytes.
    static void reverse2 (void* to, const void* from);

    // Reverse 4 bytes.
    static void reverse4 (void* to, const void* from);

    // Reverse 8 bytes.
    static void reverse8 (void* to, const void* from);

    // Move 2 bytes.
    static void move2 (void* to, const void* from);

    // Move 4 bytes.
    static void move4 (void* to, const void* from);

    // Move 8 bytes.
    static void move8 (void* to, const void* from);

private:
    // This class should not be constructed
    // (so declare the constructor private).
    CanonicalConversion();
};



inline void CanonicalConversion::reverse2 (void* to, const void* from)
{
    ((char*)to)[0] = ((const char*)from)[1];
    ((char*)to)[1] = ((const char*)from)[0];
}
inline void CanonicalConversion::reverse4 (void* to, const void* from)
{
    ((char*)to)[0] = ((const char*)from)[3];
    ((char*)to)[1] = ((const char*)from)[2];
    ((char*)to)[2] = ((const char*)from)[1];
    ((char*)to)[3] = ((const char*)from)[0];
}
inline void CanonicalConversion::reverse8 (void* to, const void* from)
{
    ((char*)to)[0] = ((const char*)from)[7];
    ((char*)to)[1] = ((const char*)from)[6];
    ((char*)to)[2] = ((const char*)from)[5];
    ((char*)to)[3] = ((const char*)from)[4];
    ((char*)to)[4] = ((const char*)from)[3];
    ((char*)to)[5] = ((const char*)from)[2];
    ((char*)to)[6] = ((const char*)from)[1];
    ((char*)to)[7] = ((const char*)from)[0];
}

inline void CanonicalConversion::move2 (void* to, const void* from)
{
    ((char*)to)[0] = ((const char*)from)[0];
    ((char*)to)[1] = ((const char*)from)[1];
}
inline void CanonicalConversion::move4 (void* to, const void* from)
{
    ((char*)to)[0] = ((const char*)from)[0];
    ((char*)to)[1] = ((const char*)from)[1];
    ((char*)to)[2] = ((const char*)from)[2];
    ((char*)to)[3] = ((const char*)from)[3];
}
inline void CanonicalConversion::move8 (void* to, const void* from)
{
    ((char*)to)[0] = ((const char*)from)[0];
    ((char*)to)[1] = ((const char*)from)[1];
    ((char*)to)[2] = ((const char*)from)[2];
    ((char*)to)[3] = ((const char*)from)[3];
    ((char*)to)[4] = ((const char*)from)[4];
    ((char*)to)[5] = ((const char*)from)[5];
    ((char*)to)[6] = ((const char*)from)[6];
    ((char*)to)[7] = ((const char*)from)[7];
}



inline size_t CanonicalConversion::toLocal (char& to, const void* from)
{
    to = *(char*)from;
    return SIZE_CAN_CHAR;
}

inline size_t CanonicalConversion::toLocal (unsigned char& to,
                                            const void* from)
{
    to = *(unsigned char*)from;
    return SIZE_CAN_UCHAR;
}

inline size_t CanonicalConversion::toLocal (short& to, const void* from)
{
    if (sizeof(short) != 2) {
	if (((signed char*)from)[0] < 0) {
	    to = -1;
	}else{
	    to = 0;
	}
    }
#if defined(AIPS_LITTLE_ENDIAN)
    reverse2 (&to, from);
#else
    move2 (((char*)&to)+sizeof(short)-2, from);
#endif
    return SIZE_CAN_SHORT;
}

inline size_t CanonicalConversion::toLocal (unsigned short& to,
                                            const void* from)
{
    if (sizeof(unsigned short) != 2) {
	to = 0;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    reverse2 (&to, from);
#else
    move2 (((char*)&to)+sizeof(unsigned short)-2, from);
#endif
    return SIZE_CAN_USHORT;
}

inline size_t CanonicalConversion::toLocal (int& to, const void* from)
{
    if (sizeof(int) != 4) {
	if (((signed char*)from)[0] < 0) {
	    to = -1;
	}else{
	    to = 0;
	}
    }
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (&to, from);
#else
    move4 (((char*)&to)+sizeof(int)-4, from);
#endif
    return SIZE_CAN_INT;
}

inline size_t CanonicalConversion::toLocal (unsigned int& to,
                                            const void* from)
{
    if (sizeof(unsigned int) != 4) {
	to = 0;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (&to, from);
#else
    move4 (((char*)&to)+sizeof(unsigned int)-4, from);
#endif
    return SIZE_CAN_UINT;
}

inline size_t CanonicalConversion::toLocal (Int64& to, const void* from)
{
    if (sizeof(Int64) != 8) {
	if (((signed char*)from)[0] < 0) {
	    to = -1;
	}else{
	    to = 0;
	}
    }
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (&to, from);
#else
    move8 (((char*)&to)+sizeof(Int64)-8, from);
#endif
    return SIZE_CAN_INT64;
}

inline size_t CanonicalConversion::toLocal (uInt64& to, const void* from)
{
    if (sizeof(uInt64) != 8) {
	to = 0;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (&to, from);
#else
    move8 (((char*)&to)+sizeof(uInt64)-8, from);
#endif
    return SIZE_CAN_UINT64;
}

inline size_t CanonicalConversion::toLocal (float& to, const void* from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (((char*)&to)+sizeof(float)-4, from);
#else
    move4 (&to, from);
#endif
    return SIZE_CAN_FLOAT;
}

inline size_t CanonicalConversion::toLocal (double& to, const void* from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (((char*)&to)+sizeof(double)-8, from);
#else
    move8 (&to, from);
#endif
    return SIZE_CAN_DOUBLE;
}


inline size_t CanonicalConversion::fromLocal (void* to,
                                              const char& from)
{
    *(char*)to = from;
    return SIZE_CAN_CHAR;
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const unsigned char& from)
{
    *(unsigned char*)to = from;
    return SIZE_CAN_UCHAR;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const short& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse2 (to, &from);
#else
    move2 (to, ((char*)&from)+sizeof(short)-2);
#endif
    return SIZE_CAN_SHORT;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const unsigned short& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse2 (to, &from);
#else
    move2 (to, ((char*)&from)+sizeof(unsigned short)-2);
#endif
    return SIZE_CAN_USHORT;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const int& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (to, &from);
#else
    move4 (to, ((char*)&from)+sizeof(int)-4);
#endif
    return SIZE_CAN_INT;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const unsigned int& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (to, &from);
#else
    move4 (to, ((char*)&from)+sizeof(unsigned int)-4);
#endif
    return SIZE_CAN_UINT;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const Int64& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (to, &from);
#else
    move8 (to, ((char*)&from)+sizeof(Int64)-8);
#endif
    return SIZE_CAN_INT64;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const uInt64& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (to, &from);
#else
    move8 (to, ((char*)&from)+sizeof(uInt64)-8);
#endif
    return SIZE_CAN_UINT64;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const float& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (to, &from);
#else
    move4 (to, &from);
#endif
    return SIZE_CAN_FLOAT;
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const double& from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (to, &from);
#else
    move8 (to, &from);
#endif
    return SIZE_CAN_FLOAT;
}


inline size_t CanonicalConversion::toLocal (char*           to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalChar (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (unsigned char*  to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalUChar (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (short*          to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalShort (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (unsigned short* to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalUShort (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (int*            to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalInt (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (unsigned int*   to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalUInt (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (Int64*          to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalInt64 (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (uInt64*         to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalUInt64 (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (float*          to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalFloat (to, from, nr);
}
inline size_t CanonicalConversion::toLocal (double*         to,
                                            const void* from,
                                            size_t nr)
{
    return toLocalDouble (to, from, nr);
}

inline size_t CanonicalConversion::fromLocal (void* to,
                                              const char*           from,
                                              size_t nr)
{
    return fromLocalChar (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const unsigned char*  from,
                                              size_t nr)
{
    return fromLocalUChar (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const short*          from,
                                              size_t nr)
{
    return fromLocalShort (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const unsigned short* from,
                                              size_t nr)
{
    return fromLocalUShort (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const int*            from,
                                              size_t nr)
{
    return fromLocalInt (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const unsigned int*   from,
                                              size_t nr)
{
    return fromLocalUInt (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const Int64*          from,
                                              size_t nr)
{
    return fromLocalInt64 (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const uInt64*         from,
                                              size_t nr)
{
    return fromLocalUInt64 (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const float*          from,
                                              size_t nr)
{
    return fromLocalFloat (to, from, nr);
}
inline size_t CanonicalConversion::fromLocal (void* to,
                                              const double*         from,
                                              size_t nr)
{
    return fromLocalDouble (to, from, nr);
}


inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const char*)
{
    return toLocalChar;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const unsigned char*)
{
    return toLocalUChar;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const short*)
{
    return toLocalShort;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const unsigned short*)
{
    return toLocalUShort;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const int*)
{
    return toLocalInt;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const unsigned int*)
{
    return toLocalUInt;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const Int64*)
{
    return toLocalInt64;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const uInt64*)
{
    return toLocalUInt64;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const float*)
{
    return toLocalFloat;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const double*)
{
    return toLocalDouble;
}

inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const char*)
{
    return fromLocalChar;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const unsigned char*)
{
    return fromLocalUChar;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const short*)
{
    return fromLocalShort;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const unsigned short*)
{
    return fromLocalUShort;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const int*)
{
    return fromLocalInt;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const unsigned int*)
{
    return fromLocalUInt;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const Int64*)
{
    return fromLocalInt64;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const uInt64*)
{
    return fromLocalUInt64;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const float*)
{
    return fromLocalFloat;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const double*)
{
    return fromLocalDouble;
}


inline unsigned int CanonicalConversion::canonicalSize (const char*)
    {return SIZE_CAN_CHAR;}
inline unsigned int CanonicalConversion::canonicalSize (const unsigned char*)
    {return SIZE_CAN_UCHAR;} 
inline unsigned int CanonicalConversion::canonicalSize (const short*)
    {return SIZE_CAN_SHORT;}
inline unsigned int CanonicalConversion::canonicalSize (const unsigned short*)
    {return SIZE_CAN_USHORT;}
inline unsigned int CanonicalConversion::canonicalSize (const int*)
    {return SIZE_CAN_INT;}
inline unsigned int CanonicalConversion::canonicalSize (const unsigned int*)
    {return SIZE_CAN_UINT;}
inline unsigned int CanonicalConversion::canonicalSize (const Int64*)
    {return SIZE_CAN_INT64;}
inline unsigned int CanonicalConversion::canonicalSize (const uInt64*)
    {return SIZE_CAN_UINT64;}
inline unsigned int CanonicalConversion::canonicalSize (const float*)
    {return SIZE_CAN_FLOAT;}
inline unsigned int CanonicalConversion::canonicalSize (const double*)
    {return SIZE_CAN_DOUBLE;}
//#//inline unsigned int CanonicalConversion::canonicalSize (const long double*)
//#//    {return SIZE_CAN_LDOUBLE;}




} //# NAMESPACE CASACORE - END

#endif
