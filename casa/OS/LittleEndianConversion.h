//# LittleEndianConversion.h: A class with static functions to convert littleEndian format
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

#ifndef CASA_LITTLEENDIANCONVERSION_H
#define CASA_LITTLEENDIANCONVERSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/CanonicalConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A class with static functions to convert littleEndian format
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tVAXConversion" demos="">
// </reviewed>

// <synopsis>
// This class is intended to be used as a common class for all
// classes converting data to/from little-endian format.
// </synopsis>

// <motivation>
// Sometimes data are stored in little-endian format (e.g. old VAX-data).
// Instead of putting all these conversion functions in all such classes,
// it is better to keep them separate to be able to use them elsewhere.
// However, note that this version handles a long as 4 bytes.
// On several little-endian machines (e.g. DEC-alpha) a long is 8 bytes,
// so a special function is needed for them.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support data type long double.
//  <li> Support a long as 4 or as 8 bytes.
// </todo>


class LittleEndianConversion
{
public:
    // Convert one value from littleEndian format to local format.
    // The from and to buffer should not overlap.
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
    
    // Convert nr values from littleEndian format to local format.
    // The from and to buffer should not overlap.
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

    // Convert one value from local format to littleEndian format.
    // The from and to buffer should not overlap.
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
    
    // Convert nr values from local format to littleEndian format.
    // The from and to buffer should not overlap.
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
    LittleEndianConversion();
};



inline void LittleEndianConversion::toLocal (char& to, const void* from)
{
    to = *(char*)from;
}

inline void LittleEndianConversion::toLocal (unsigned char& to,
					     const void* from)
{
    to = *(unsigned char*)from;
}

inline void LittleEndianConversion::toLocal (short& to, const void* from)
{
    if (sizeof(short) != 2) {
	if (((signed char*)from)[2] < 0) {
	    to = -1;
	}else{
	    to = 0;
	}
    }
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse2 (((char*)&to)+sizeof(short)-2, from);
#else
    CanonicalConversion::move2 (&to, from);
#endif
}

inline void LittleEndianConversion::toLocal (unsigned short& to,
					     const void* from)
{
    if (sizeof(unsigned short) != 2) {
	to = 0;
    }
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse2 (((char*)&to)+sizeof(unsigned short)-2,from);
#else
    CanonicalConversion::move2 (&to, from);
#endif
}

inline void LittleEndianConversion::toLocal (int& to, const void* from)
{
    if (sizeof(int) != 4) {
	if (((signed char*)from)[3] < 0) {
	    to = -1;
	}else{
	    to = 0;
	}
    }
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (((char*)&to)+sizeof(int)-4, from);
#else
    CanonicalConversion::move4 (&to, from);
#endif
}

inline void LittleEndianConversion::toLocal (unsigned int& to,
					     const void* from)
{
    if (sizeof(unsigned int) != 4) {
	to = 0;
    }
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (((char*)&to)+sizeof(unsigned int)-4, from);
#else
    CanonicalConversion::move4 (&to, from);
#endif
}

inline void LittleEndianConversion::toLocal (Int64& to, const void* from)
{
    int tmp;
    LittleEndianConversion::toLocal (tmp, from);
    to = tmp;
}

inline void LittleEndianConversion::toLocal (uInt64& to,
					     const void* from)
{
    unsigned int tmp;
    LittleEndianConversion::toLocal (tmp, from);
    to = tmp;
}

inline void LittleEndianConversion::toLocal (float& to, const void* from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (&to, from);
#else
    CanonicalConversion::move4 (&to, from);
#endif
}

inline void LittleEndianConversion::toLocal (double& to, const void* from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse8 (&to, from);
#else
    CanonicalConversion::move8 (&to, from);
#endif
}


inline void LittleEndianConversion::fromLocal (void* to, char from)
{
    *(char*)to = from;
}
inline void LittleEndianConversion::fromLocal (void* to, unsigned char from)
{
    *(unsigned char*)to = from;
}

inline void LittleEndianConversion::fromLocal (void* to, short from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse2 (to, ((char*)&from)+sizeof(short)-2);
#else
    CanonicalConversion::move2 (to, &from);
#endif
}

inline void LittleEndianConversion::fromLocal (void* to, unsigned short from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse2 (to,((char*)&from)+sizeof(unsigned short)-2);
#else
    CanonicalConversion::move2 (to, &from);
#endif
}

inline void LittleEndianConversion::fromLocal (void* to, int from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (to, ((char*)&from)+sizeof(int)-4);
#else
    CanonicalConversion::move4 (to, &from);
#endif
}

inline void LittleEndianConversion::fromLocal (void* to, unsigned int from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (to, ((char*)&from)+sizeof(unsigned int)-4);
#else
    CanonicalConversion::move4 (to, &from);
#endif
}

inline void LittleEndianConversion::fromLocal (void* to, Int64 from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (to, ((char*)&from)+sizeof(Int64)-4);
#else
    CanonicalConversion::move4 (to, &from);
#endif
}

inline void LittleEndianConversion::fromLocal (void* to, uInt64 from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (to, ((char*)&from)+sizeof(uInt64)-4);
#else
    CanonicalConversion::move4 (to, &from);
#endif
}

inline void LittleEndianConversion::fromLocal (void* to, float from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse4 (to, ((char*)&from)+sizeof(float)-4);
#else
    CanonicalConversion::move4 (to, &from);
#endif
}

inline void LittleEndianConversion::fromLocal (void* to, double from)
{
#if !defined(AIPS_LITTLE_ENDIAN)
    CanonicalConversion::reverse8 (to, ((char*)&from)+sizeof(double)-8);
#else
    CanonicalConversion::move8 (to, &from);
#endif
}




} //# NAMESPACE CASACORE - END

#endif
