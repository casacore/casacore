//# CanonicalConversion.h: A class with static functions to convert canonical format
//# Copyright (C) 1996,1997,1999,2000
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

#if !defined(AIPS_CANONICALCONVERSION_H)
#define AIPS_CANONICALCONVERSION_H

//# Includes
#include <aips/OS/Conversion.h>



// Define the canonical sizes of the built-in data types.
// These are the same for all machine architectures.
// Also define the maximum size.

#define SIZE_CAN_CHAR     1
#define SIZE_CAN_UCHAR    1
#define SIZE_CAN_SHORT    2
#define SIZE_CAN_USHORT   2
#define SIZE_CAN_INT      4
#define SIZE_CAN_UINT     4
#define SIZE_CAN_LONG     8
#define SIZE_CAN_ULONG    8
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
#define CONVERT_CAN_SHORT    1
#define CONVERT_CAN_USHORT   1
#define CONVERT_CAN_INT      1
#define CONVERT_CAN_UINT     1
#define CONVERT_CAN_LONG     1
#define CONVERT_CAN_ULONG    1
#define CONVERT_CAN_FLOAT    1
#define CONVERT_CAN_DOUBLE   1
//#//#define CONVERT_CAN_LDOUBLE  1
#else

// Conversion is not needed for IEEE data, but it is for longs
// (unless the their local length is 8).
// Change the definitions below if new architectures are being used.
#define CONVERT_CAN_SHORT    0
#define CONVERT_CAN_USHORT   0
#define CONVERT_CAN_INT      0
#define CONVERT_CAN_UINT     0
#ifdef SGI64
#define CONVERT_CAN_LONG     0
#define CONVERT_CAN_ULONG    0
#else 
#define CONVERT_CAN_LONG     1
#define CONVERT_CAN_ULONG    1
#endif
#define CONVERT_CAN_FLOAT    0
#define CONVERT_CAN_DOUBLE   0
// LDOUBLE is 8 bytes on SUN, but 16 bytes canonical.
//#//#define CONVERT_CAN_LDOUBLE  1
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
// <br>- long: 8 bytes
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
// AIPS++ data will be stored in a canonical format.
// To read these data conversion functions are needed.
// However, these functions do not use any other AIPS++ classes,
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
    static unsigned int toLocal (char&           to, const void* from);
    static unsigned int toLocal (unsigned char&  to, const void* from);
    static unsigned int toLocal (short&          to, const void* from);
    static unsigned int toLocal (unsigned short& to, const void* from);
    static unsigned int toLocal (int&            to, const void* from);
    static unsigned int toLocal (unsigned int&   to, const void* from);
    static unsigned int toLocal (long&           to, const void* from);
    static unsigned int toLocal (unsigned long&  to, const void* from);
    static unsigned int toLocal (float&          to, const void* from);
    static unsigned int toLocal (double&         to, const void* from);
    // </group>
    
    // Convert one value from local format to canonical format.
    // The from and to buffer should not overlap.
    // <group>
    static unsigned int fromLocal (void* to, char           from);
    static unsigned int fromLocal (void* to, unsigned char  from);
    static unsigned int fromLocal (void* to, short          from);
    static unsigned int fromLocal (void* to, unsigned short from);
    static unsigned int fromLocal (void* to, int            from);
    static unsigned int fromLocal (void* to, unsigned int   from);
    static unsigned int fromLocal (void* to, long           from);
    static unsigned int fromLocal (void* to, unsigned long  from);
    static unsigned int fromLocal (void* to, float          from);
    static unsigned int fromLocal (void* to, double         from);
    // </group>
    
    // Convert nr values from canonical format to local format.
    // The from and to buffer should not overlap.
    // <group>
    static unsigned int toLocal (char*           to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (unsigned char*  to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (short*          to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (unsigned short* to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (int*            to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (unsigned int*   to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (long*           to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (unsigned long*  to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (float*          to, const void* from,
				 unsigned int nr);
    static unsigned int toLocal (double*         to, const void* from,
				 unsigned int nr);
    // </group>

    // Convert nr values from local format to canonical format.
    // The from and to buffer should not overlap.
    // <group>
    static unsigned int fromLocal (void* to, const char*           from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const unsigned char*  from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const short*          from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const unsigned short* from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const int*            from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const unsigned int*   from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const long*           from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const unsigned long*  from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const float*          from,
				   unsigned int nr);
    static unsigned int fromLocal (void* to, const double*         from,
				   unsigned int nr);
    // </group>

    // Convert nr values from canonical format to local format.
    // The from and to buffer should not overlap.
    // <group>
    static unsigned int toLocalChar     (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalUChar    (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalShort    (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalUShort   (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalInt      (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalUInt     (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalLong     (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalULong    (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalFloat    (void* to, const void* from,
					 unsigned int nr);
    static unsigned int toLocalDouble   (void* to, const void* from,
					 unsigned int nr);
    // </group>
    
    // Convert nr values from local format to canonical format.
    // The from and to buffer should not overlap.
    // <group>
    static unsigned int fromLocalChar     (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalUChar    (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalShort    (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalUShort   (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalInt      (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalUInt     (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalLong     (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalULong    (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalFloat    (void* to, const void* from,
					   unsigned int nr);
    static unsigned int fromLocalDouble   (void* to, const void* from,
					   unsigned int nr);
    // </group>
    
    // Convert values from canonical format to local format.
    // The from and to buffer should not overlap.
    // The number of values involved is determined from the argument
    // <src>nrbytes</src>, which gives the number of bytes in local format.
    // The signature of this function is the same as <src>memcpy</src>, so
    // that memcpy can directly be used if no conversion is needed.
    // <group>
    static void* byteToLocalChar     (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalUChar    (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalShort    (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalUShort   (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalInt      (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalUInt     (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalLong     (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalULong    (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalFloat    (void* to, const void* from,
				      unsigned int nrbytes);
    static void* byteToLocalDouble   (void* to, const void* from,
				      unsigned int nrbytes);
    // </group>
    
    // Convert values from local format to canonical format.
    // The from and to buffer should not overlap.
    // The number of values involved is determined from the argument
    // <src>nrbytes</src>, which gives the number of bytes in local format.
    // The signature of this function is the same as <src>memcpy</src>, so
    // that memcpy can directly be used if no conversion is needed.
    // <group>
    static void* byteFromLocalChar     (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalUChar    (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalShort    (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalUShort   (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalInt      (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalUInt     (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalLong     (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalULong    (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalFloat    (void* to, const void* from,
					unsigned int nrbytes);
    static void* byteFromLocalDouble   (void* to, const void* from,
					unsigned int nrbytes);
    // </group>
    
    // Get the value conversion function for the given type.
    // <group>
    static Conversion::ValueFunction* getToLocal (const char*);
    static Conversion::ValueFunction* getToLocal (const unsigned char*);
    static Conversion::ValueFunction* getToLocal (const short*);
    static Conversion::ValueFunction* getToLocal (const unsigned short*);
    static Conversion::ValueFunction* getToLocal (const int*);
    static Conversion::ValueFunction* getToLocal (const unsigned int*);
    static Conversion::ValueFunction* getToLocal (const long*);
    static Conversion::ValueFunction* getToLocal (const unsigned long*);
    static Conversion::ValueFunction* getToLocal (const float*);
    static Conversion::ValueFunction* getToLocal (const double*);
    static Conversion::ValueFunction* getFromLocal (const char*);
    static Conversion::ValueFunction* getFromLocal (const unsigned char*);
    static Conversion::ValueFunction* getFromLocal (const short*);
    static Conversion::ValueFunction* getFromLocal (const unsigned short*);
    static Conversion::ValueFunction* getFromLocal (const int*);
    static Conversion::ValueFunction* getFromLocal (const unsigned int*);
    static Conversion::ValueFunction* getFromLocal (const long*);
    static Conversion::ValueFunction* getFromLocal (const unsigned long*);
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
    static Conversion::ByteFunction* getByteToLocal (const long*);
    static Conversion::ByteFunction* getByteToLocal (const unsigned long*);
    static Conversion::ByteFunction* getByteToLocal (const float*);
    static Conversion::ByteFunction* getByteToLocal (const double*);
    static Conversion::ByteFunction* getByteFromLocal (const char*);
    static Conversion::ByteFunction* getByteFromLocal (const unsigned char*);
    static Conversion::ByteFunction* getByteFromLocal (const short*);
    static Conversion::ByteFunction* getByteFromLocal (const unsigned short*);
    static Conversion::ByteFunction* getByteFromLocal (const int*);
    static Conversion::ByteFunction* getByteFromLocal (const unsigned int*);
    static Conversion::ByteFunction* getByteFromLocal (const long*);
    static Conversion::ByteFunction* getByteFromLocal (const unsigned long*);
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
    static unsigned int canonicalSize (const long*);
    static unsigned int canonicalSize (const unsigned long*);
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



inline unsigned int CanonicalConversion::toLocal (char& to, const void* from)
{
    to = *(char*)from;
    return SIZE_CAN_CHAR;
}

inline unsigned int CanonicalConversion::toLocal (unsigned char& to,
						  const void* from)
{
    to = *(unsigned char*)from;
    return SIZE_CAN_UCHAR;
}

inline unsigned int CanonicalConversion::toLocal (short& to, const void* from)
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

inline unsigned int CanonicalConversion::toLocal (unsigned short& to,
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

inline unsigned int CanonicalConversion::toLocal (int& to, const void* from)
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

inline unsigned int CanonicalConversion::toLocal (unsigned int& to,
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

inline unsigned int CanonicalConversion::toLocal (long& to, const void* from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(long) == 4) {
	reverse4 (&to, ((const char*)from)+4);
    }else{
	reverse8 (&to, from);
    }
#else
    if (sizeof(long) == 4) {
	move4 (&to, ((const char*)from)+4);
    }else{
	move8 (&to, from);
    }
#endif
    return SIZE_CAN_LONG;
}

inline unsigned int CanonicalConversion::toLocal (unsigned long& to,
						  const void* from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(unsigned long) == 4) {
	reverse4 (&to, ((const char*)from)+4);
    }else{
	reverse8 (&to, from);
    }
#else
    if (sizeof(unsigned long) == 4) {
	move4 (&to, ((const char*)from)+4);
    }else{
	move8 (&to, from);
    }
#endif
    return SIZE_CAN_ULONG;
}


inline unsigned int CanonicalConversion::toLocal (float& to, const void* from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (((char*)&to)+sizeof(float)-4, from);
#else
    move4 (&to, from);
#endif
    return SIZE_CAN_FLOAT;
}

inline unsigned int CanonicalConversion::toLocal (double& to, const void* from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (((char*)&to)+sizeof(double)-8, from);
#else
    move8 (&to, from);
#endif
    return SIZE_CAN_DOUBLE;
}


inline unsigned int CanonicalConversion::fromLocal (void* to, char from)
{
    *(char*)to = from;
    return SIZE_CAN_CHAR;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    unsigned char from)
{
    *(unsigned char*)to = from;
    return SIZE_CAN_UCHAR;
}

inline unsigned int CanonicalConversion::fromLocal (void* to, short from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse2 (to, &from);
#else
    move2 (to, ((char*)&from)+sizeof(short)-2);
#endif
    return SIZE_CAN_SHORT;
}

inline unsigned int CanonicalConversion::fromLocal (void* to,
						    unsigned short from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse2 (to, &from);
#else
    move2 (to, ((char*)&from)+sizeof(unsigned short)-2);
#endif
    return SIZE_CAN_USHORT;
}

inline unsigned int CanonicalConversion::fromLocal (void* to, int from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (to, &from);
#else
    move4 (to, ((char*)&from)+sizeof(int)-4);
#endif
    return SIZE_CAN_INT;
}

inline unsigned int CanonicalConversion::fromLocal (void* to,
						    unsigned int from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (to, &from);
#else
    move4 (to, ((char*)&from)+sizeof(unsigned int)-4);
#endif
    return SIZE_CAN_UINT;
}

inline unsigned int CanonicalConversion::fromLocal (void* to, long from)
{
    if (sizeof(long) == 4) {
	char c = 0;
	if (from < 0) {
	    c = -1;
	}
	((char*)to)[0] = c;
	((char*)to)[1] = c;
	((char*)to)[2] = c;
	((char*)to)[3] = c;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(long) == 4) {
	reverse4 (((char*)to)+4, &from);
    }else{
	reverse8 (to, &from);
    }
#else
    if (sizeof(long) == 4) {
	move4 (((char*)to)+4, &from);
    }else{
	move8 (to, &from);
    }
#endif
    return SIZE_CAN_LONG;
}

inline unsigned int CanonicalConversion::fromLocal (void* to,
						    unsigned long from)
{
    if (sizeof(unsigned long) == 4) {
	((char*)to)[0] = 0;
	((char*)to)[1] = 0;
	((char*)to)[2] = 0;
	((char*)to)[3] = 0;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(unsigned long) == 4) {
	reverse4 (((char*)to)+4, &from);
    }else{
	reverse8 (to, &from);
    }
#else
    if (sizeof(unsigned long) == 4) {
	move4 (((char*)to)+4, &from);
    }else{
	move8 (to, &from);
    }
#endif
    return SIZE_CAN_ULONG;
}

inline unsigned int CanonicalConversion::fromLocal (void* to, float from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse4 (to, &from);
#else
    move4 (to, &from);
#endif
    return SIZE_CAN_FLOAT;
}

inline unsigned int CanonicalConversion::fromLocal (void* to, double from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    reverse8 (to, &from);
#else
    move8 (to, &from);
#endif
    return SIZE_CAN_FLOAT;
}


inline unsigned int CanonicalConversion::toLocal (char*           to,
						  const void* from,
						  unsigned int nr)
{
    toLocalChar (to, from, nr);
    return nr * SIZE_CAN_CHAR;
}
inline unsigned int CanonicalConversion::toLocal (unsigned char*  to,
						  const void* from,
						  unsigned int nr)
{
    toLocalUChar (to, from, nr);
    return nr * SIZE_CAN_UCHAR;
}
inline unsigned int CanonicalConversion::toLocal (short*          to,
						  const void* from,
						  unsigned int nr)
{
    toLocalShort (to, from, nr);
    return nr * SIZE_CAN_SHORT;
}
inline unsigned int CanonicalConversion::toLocal (unsigned short* to,
						  const void* from,
						  unsigned int nr)
{
    toLocalUShort (to, from, nr);
    return nr * SIZE_CAN_USHORT;
}
inline unsigned int CanonicalConversion::toLocal (int*            to,
						  const void* from,
						  unsigned int nr)
{
    toLocalInt (to, from, nr);
    return nr * SIZE_CAN_INT;
}
inline unsigned int CanonicalConversion::toLocal (unsigned int*   to,
						  const void* from,
						  unsigned int nr)
{
    toLocalUInt (to, from, nr);
    return nr * SIZE_CAN_UINT;
}
inline unsigned int CanonicalConversion::toLocal (long*           to,
						  const void* from,
						  unsigned int nr)
{
    toLocalLong (to, from, nr);
    return nr * SIZE_CAN_LONG;
}
inline unsigned int CanonicalConversion::toLocal (unsigned long*  to,
						  const void* from,
						  unsigned int nr)
{
    toLocalULong (to, from, nr);
    return nr * SIZE_CAN_ULONG;
}
inline unsigned int CanonicalConversion::toLocal (float*          to,
						  const void* from,
						  unsigned int nr)
{
    toLocalFloat (to, from, nr);
    return nr * SIZE_CAN_FLOAT;
}
inline unsigned int CanonicalConversion::toLocal (double*         to,
						  const void* from,
						  unsigned int nr)
{
    toLocalDouble (to, from, nr);
    return nr * SIZE_CAN_DOUBLE;
}

inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const char*           from,
						    unsigned int nr)
{
    fromLocalChar (to, from, nr);
    return nr * SIZE_CAN_CHAR;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const unsigned char*  from,
						    unsigned int nr)
{
    fromLocalUChar (to, from, nr);
    return nr * SIZE_CAN_UCHAR;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const short*          from,
						    unsigned int nr)
{
    fromLocalShort (to, from, nr);
    return nr * SIZE_CAN_SHORT;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const unsigned short* from,
						    unsigned int nr)
{
    fromLocalUShort (to, from, nr);
    return nr * SIZE_CAN_USHORT;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const int*            from,
						    unsigned int nr)
{
    fromLocalInt (to, from, nr);
    return nr * SIZE_CAN_INT;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const unsigned int*   from,
						    unsigned int nr)
{
    fromLocalUInt (to, from, nr);
    return nr * SIZE_CAN_UINT;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const long*           from,
						    unsigned int nr)
{
    fromLocalLong (to, from, nr);
    return nr * SIZE_CAN_LONG;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const unsigned long*  from,
						    unsigned int nr)
{
    fromLocalULong (to, from, nr);
    return nr * SIZE_CAN_ULONG;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const float*          from,
						    unsigned int nr)
{
    fromLocalFloat (to, from, nr);
    return nr * SIZE_CAN_FLOAT;
}
inline unsigned int CanonicalConversion::fromLocal (void* to,
						    const double*         from,
						    unsigned int nr)
{
    fromLocalDouble (to, from, nr);
    return nr * SIZE_CAN_DOUBLE;
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
                                                      (const long*)
{
    return toLocalLong;
}
inline Conversion::ValueFunction* CanonicalConversion::getToLocal
                                                      (const unsigned long*)
{
    return toLocalULong;
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
                                                      (const long*)
{
    return fromLocalLong;
}
inline Conversion::ValueFunction* CanonicalConversion::getFromLocal
                                                      (const unsigned long*)
{
    return fromLocalULong;
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
inline unsigned int CanonicalConversion::canonicalSize (const long*)
    {return SIZE_CAN_LONG;}
inline unsigned int CanonicalConversion::canonicalSize (const unsigned long*)
    {return SIZE_CAN_ULONG;}
inline unsigned int CanonicalConversion::canonicalSize (const float*)
    {return SIZE_CAN_FLOAT;}
inline unsigned int CanonicalConversion::canonicalSize (const double*)
    {return SIZE_CAN_DOUBLE;}
//#//inline unsigned int CanonicalConversion::canonicalSize (const long double*)
//#//    {return SIZE_CAN_LDOUBLE;}



#endif
