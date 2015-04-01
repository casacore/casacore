//# IBMConversion.h: A class with static functions to convert IBM format
//# Copyright (C) 1996,1997,1999,2001
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

#ifndef CASA_IBMCONVERSION_H
#define CASA_IBMCONVERSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/CanonicalConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define the IBM sizes of the built-in data types.

#define SIZE_IBM_CHAR     1
#define SIZE_IBM_UCHAR    1
#define SIZE_IBM_SHORT    2
#define SIZE_IBM_USHORT   2
#define SIZE_IBM_INT      4
#define SIZE_IBM_UINT     4
#define SIZE_IBM_INT64    4
#define SIZE_IBM_UINT64   4
#define SIZE_IBM_FLOAT    4
#define SIZE_IBM_DOUBLE   8


// <summary>
// A class with static functions to convert IBM format
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tIBMConversion" demos="">
// </reviewed>

// <synopsis>
// This class contains static toLocal functions to convert data from IBM-360
// format to local format and vice-versa. It also handles the conversion
// of the IBM EBCDIC characters to ASCII characters (for data type char).
// <p>
// The functions work well on big-endian as well as little-endian machines.
// </synopsis>

// <motivation>
// Archived WSRT data can be stored in the old IBM format
// (EBCDIC characters and floats with base 16).
// Conversion functions are needed to read these data.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support data type long double.
// </todo>


class IBMConversion
{
public:
    // Convert one value from IBM format to local format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version converts from EBCDIC to ASCII, while the
    // unsigned char version is a simple copy.
    // </note>
    // <group>
    static void toLocal (char&           to, const void* from);
    static void toLocal (unsigned char&  to, const void* from);
    static void toLocal (short&          to, const void* from);
    static void toLocal (unsigned short& to, const void* from);
    static void toLocal (int&            to, const void* from);
    static void toLocal (unsigned int&   to, const void* from);
    static void toLocal (Int64&          to, const void* from);
    static void toLocal (uInt64&         to, const void* from);
    static void toLocal (float&          to, const void* from);
    static void toLocal (double&         to, const void* from);
    // </group>
    
    // Convert nr values from IBM format to local format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version converts from EBCDIC to ASCII, while the
    // unsigned char version is a simple copy.
    // </note>
    // <group>
    static void toLocal (char*           to, const void* from,
			 size_t nr);
    static void toLocal (unsigned char*  to, const void* from,
			 size_t nr);
    static void toLocal (short*          to, const void* from,
			 size_t nr);
    static void toLocal (unsigned short* to, const void* from,
			 size_t nr);
    static void toLocal (int*            to, const void* from,
			 size_t nr);
    static void toLocal (unsigned int*   to, const void* from,
			 size_t nr);
    static void toLocal (Int64*          to, const void* from,
			 size_t nr);
    static void toLocal (uInt64*         to, const void* from,
			 size_t nr);
    static void toLocal (float*          to, const void* from,
			 size_t nr);
    static void toLocal (double*         to, const void* from,
			 size_t nr);
    // </group>

    // Convert one value from local format to IBM format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version converts from ASCII to EBCDIC, while the
    // unsigned char version is a simple copy.
    // </note>
    // <group>
    static void fromLocal (void* to, char           from);
    static void fromLocal (void* to, unsigned char  from);
    static void fromLocal (void* to, short          from);
    static void fromLocal (void* to, unsigned short from);
    static void fromLocal (void* to, int            from);
    static void fromLocal (void* to, unsigned int   from);
    static void fromLocal (void* to, Int64          from);
    static void fromLocal (void* to, uInt64         from);
    static void fromLocal (void* to, float          from);
    static void fromLocal (void* to, double         from);
    // </group>
    
    // Convert nr values from local format to IBM format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version converts from ASCII to EBCDIC, while the
    // unsigned char version is a simple copy.
    // </note>
    // <group>
    static void fromLocal (void* to, const char*           from,
			   size_t nr);
    static void fromLocal (void* to, const unsigned char*  from,
			   size_t nr);
    static void fromLocal (void* to, const short*          from,
			   size_t nr);
    static void fromLocal (void* to, const unsigned short* from,
			   size_t nr);
    static void fromLocal (void* to, const int*            from,
			   size_t nr);
    static void fromLocal (void* to, const unsigned int*   from,
			   size_t nr);
    static void fromLocal (void* to, const Int64*          from,
			   size_t nr);
    static void fromLocal (void* to, const uInt64*         from,
			   size_t nr);
    static void fromLocal (void* to, const float*          from,
			   size_t nr);
    static void fromLocal (void* to, const double*         from,
			   size_t nr);
    // </group>
    
private:
    // This class should not be constructed
    // (so declare the constructor private).
    IBMConversion();
};



inline void IBMConversion::toLocal (unsigned char& to, const void* from)
{
    CanonicalConversion::toLocal (to, from);
}

inline void IBMConversion::toLocal (short& to, const void* from)
{
    CanonicalConversion::toLocal (to, from);
}

inline void IBMConversion::toLocal (unsigned short& to, const void* from)
{
    CanonicalConversion::toLocal (to, from);
}

inline void IBMConversion::toLocal (int& to, const void* from)
{
    CanonicalConversion::toLocal (to, from);
}

inline void IBMConversion::toLocal (unsigned int& to, const void* from)
{
    CanonicalConversion::toLocal (to, from);
}

inline void IBMConversion::toLocal (Int64& to, const void* from)
{
    if (sizeof(Int64) != 4) {
	if (((signed char*)from)[0] < 0) {
	    to = -1;
	}else{
	    to = 0;
	}
    }
#if defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (&to, from);
#else
    CanonicalConversion::move4 (((char*)&to)+sizeof(Int64)-4, from);
#endif
}

inline void IBMConversion::toLocal (uInt64& to, const void* from)
{
    if (sizeof(uInt64) != 4) {
	to = 0;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (&to, from);
#else
    CanonicalConversion::move4 (((char*)&to)+sizeof(uInt64)-4, from);
#endif
}

inline void IBMConversion::toLocal (float& to, const void* from)
{
    toLocal (&to, from, 1);
}

inline void IBMConversion::toLocal (double& to, const void* from)
{
    toLocal (&to, from, 1);
}

inline void IBMConversion::toLocal (unsigned char* to, const void* from,
				    size_t nr)
{
    CanonicalConversion::toLocal (to, from, nr);
}

inline void IBMConversion::toLocal (short* to, const void* from,
				    size_t nr)
{
    CanonicalConversion::toLocal (to, from, nr);
}

inline void IBMConversion::toLocal (unsigned short* to, const void* from,
				    size_t nr)
{
    CanonicalConversion::toLocal (to, from, nr);
}

inline void IBMConversion::toLocal (int* to, const void* from,
				    size_t nr)
{
    CanonicalConversion::toLocal (to, from, nr);
}

inline void IBMConversion::toLocal (unsigned int* to, const void* from,
				    size_t nr)
{
    CanonicalConversion::toLocal (to, from, nr);
}


inline void IBMConversion::fromLocal (void* to, unsigned char from)
{
    CanonicalConversion::fromLocal (to, from);
}

inline void IBMConversion::fromLocal (void* to, short from)
{
    CanonicalConversion::fromLocal (to, from);
}

inline void IBMConversion::fromLocal (void* to, unsigned short from)
{
    CanonicalConversion::fromLocal (to, from);
}

inline void IBMConversion::fromLocal (void* to, int from)
{
    CanonicalConversion::fromLocal (to, from);
}

inline void IBMConversion::fromLocal (void* to, unsigned int from)
{
    CanonicalConversion::fromLocal (to, from);
}

inline void IBMConversion::fromLocal (void* to, Int64 from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (to, &from);
#else
    CanonicalConversion::move4 (to, ((char*)&from)+sizeof(Int64)-4);
#endif
}

inline void IBMConversion::fromLocal (void* to, uInt64 from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (to, &from);
#else
    CanonicalConversion::move4 (to,((char*)&from)+sizeof(uInt64)-4);
#endif
}

inline void IBMConversion::fromLocal (void* to, float from)
{
    fromLocal (to, &from, 1);
}

inline void IBMConversion::fromLocal (void* to, double from)
{
    fromLocal (to, &from, 1);
}


inline void IBMConversion::fromLocal (void* to, const unsigned char* from,
				      size_t nr)
{ 
    CanonicalConversion::fromLocal (to, from, nr);
}

inline void IBMConversion::fromLocal (void* to, const short* from,
				      size_t nr)
{ 
    CanonicalConversion::fromLocal (to, from, nr);
}

inline void IBMConversion::fromLocal (void* to, const unsigned short* from,
				      size_t nr)
{ 
    CanonicalConversion::fromLocal (to, from, nr);
}

inline void IBMConversion::fromLocal (void* to, const int* from,
				      size_t nr)
{ 
    CanonicalConversion::fromLocal (to, from, nr);
}

inline void IBMConversion::fromLocal (void* to, const unsigned int* from,
				      size_t nr)
{ 
    CanonicalConversion::fromLocal (to, from, nr);
}




} //# NAMESPACE CASACORE - END

#endif
