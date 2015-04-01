//# VAXConversion.h: A class with static functions to convert VAX format
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

#ifndef CASA_VAXCONVERSION_H
#define CASA_VAXCONVERSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <assert.h>
#include <casacore/casa/OS/LittleEndianConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define the canonical sizes of the built-in data types.
// These are the same for all machine architectures.

#define SIZE_VAX_CHAR     1
#define SIZE_VAX_UCHAR    1
#define SIZE_VAX_SHORT    2
#define SIZE_VAX_USHORT   2
#define SIZE_VAX_INT      4
#define SIZE_VAX_UINT     4
#define SIZE_VAX_INT64    4
#define SIZE_VAX_UINT64   4
#define SIZE_VAX_FLOAT    4
#define SIZE_VAX_DOUBLE   8


// <summary>
// A class with static functions to convert VAX format
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tVAXConversion" demos="">
// </reviewed>

// <synopsis>
// This class contains static toLocal functions to convert data from VAX
// format to local format and vice-versa. It only handles VAX D-float format.
// Another class should be implemented to handle VAX G-float format.
// <p>
// The functions work well on big-endian as well as little-endian machines.
// </synopsis>

// <motivation>
// Archived WSRT data can be stored in the old VAX format
// (little-endian and VAX D-float floating point format).
// Conversion functions are needed to read these data.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support data type long double.
// </todo>


class VAXConversion
{
public:
    // Convert one value from VAX format to local format.
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
    
    // Convert nr values from VAX format to local format.
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

    // Convert one value from local format to VAX format.
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
    
    // Convert nr values from local format to VAX format.
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
    
    // Move a float value (by swapping bytes correctly).
    static void moveFloat (void* to, const void* from);

private:
    // This class should not be constructed
    // (so declare the constructor private).
    VAXConversion();
};



inline void VAXConversion::toLocal (char& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (unsigned char& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (short& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (unsigned short& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (int& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (unsigned int& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (Int64& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (uInt64& to, const void* from)
{
    LittleEndianConversion::toLocal (to, from);
}

inline void VAXConversion::toLocal (float& to, const void* from)
{
    toLocal (&to, from, 1);
}

inline void VAXConversion::toLocal (double& to, const void* from)
{
    toLocal (&to, from, 1);
}

inline void VAXConversion::toLocal (char* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}

inline void VAXConversion::toLocal (unsigned char* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}

inline void VAXConversion::toLocal (short* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}

inline void VAXConversion::toLocal (unsigned short* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}

inline void VAXConversion::toLocal (int* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}

inline void VAXConversion::toLocal (unsigned int* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}

inline void VAXConversion::toLocal (Int64* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}

inline void VAXConversion::toLocal (uInt64* to, const void* from,
				    size_t nr)
{
    LittleEndianConversion::toLocal (to, from, nr);
}


inline void VAXConversion::fromLocal (void* to, char from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, unsigned char from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, short from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, unsigned short from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, int from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, unsigned int from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, Int64 from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, uInt64 from)
{
    LittleEndianConversion::fromLocal (to, from);
}

inline void VAXConversion::fromLocal (void* to, float from)
{
    fromLocal (to, &from, 1);
}

inline void VAXConversion::fromLocal (void* to, double from)
{
    fromLocal (to, &from, 1);
}

inline void VAXConversion::fromLocal (void* to, const char* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}

inline void VAXConversion::fromLocal (void* to, const unsigned char* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}

inline void VAXConversion::fromLocal (void* to, const short* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}

inline void VAXConversion::fromLocal (void* to, const unsigned short* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}

inline void VAXConversion::fromLocal (void* to, const int* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}

inline void VAXConversion::fromLocal (void* to, const unsigned int* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}

inline void VAXConversion::fromLocal (void* to, const Int64* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}

inline void VAXConversion::fromLocal (void* to, const uInt64* from,
				      size_t nr)
{ 
    LittleEndianConversion::fromLocal (to, from, nr);
}



inline void VAXConversion::moveFloat (void* to, const void* from)
{
#if defined(AIPS_LITTLE_ENDIAN)
    ((char*)to)[0] = ((const char*)from)[2];
    ((char*)to)[1] = ((const char*)from)[3];
    ((char*)to)[2] = ((const char*)from)[0];
    ((char*)to)[3] = ((const char*)from)[1];
#else
    ((char*)to)[0] = ((const char*)from)[1];
    ((char*)to)[1] = ((const char*)from)[0];
    ((char*)to)[2] = ((const char*)from)[3];
    ((char*)to)[3] = ((const char*)from)[2];
#endif
}



} //# NAMESPACE CASACORE - END

#endif
