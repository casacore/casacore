//# DataConversion.h: Abstract base class with functions to convert any format
//# Copyright (C) 1996,1999,2001
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

#ifndef CASA_DATACONVERSION_H
#define CASA_DATACONVERSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Conversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Abstract base class with functions to convert any format
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tDataConversion" demos="">
// </reviewed>

// <synopsis>
// This abstract base class contains pure virtual functions to convert
// from any foreign data format to local format and vice-versa.
// Classes derived from it implement the functions for a specific
// foreign format (e.g.
// <linkto class=CanonicalDataConversion:description>CanonicalDataConversion
// </linkto>,
// <linkto class=VAXDataConversion:description>VAXDataConversion</linkto>,
// <linkto class=IBMDataConversion:description>IBMDataConversion</linkto>).
// <linkto class=RawDataConversion:description>RawDataConversion</linkto>).
// </synopsis>

// <example>
// <srcblock>
// // Construct the correct conversion object.
// DataConversion* conv = new IBMDataConversion();
// // Say that you read a block of 256 floats (in IBM-format).
// char buffer[1024];
// read (fd, buffer, 1024);
// // Convert the float to local format.
// float values[256];
// conv->toLocal (values, buffer, 256);
// </srcblock>
// </example>

// <motivation>
// The abstract base class allows one to construct the correct conversion
// object at the beginning. Thereafter this base class can be used and
// polymorphism takes care of picking the correct functions.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support data type long double.
// </todo>


class DataConversion
{
public:
    // Construct the object.
    DataConversion();

    virtual ~DataConversion();

    // Convert one value from foreign format to local format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version handles characters (thus may involve conversion
    // EBCDIC to ASCII), while the unsigned chars are simply bytes.
    // </note>
    // <group>
    virtual size_t toLocal (char&           to,
				  const void* from) const = 0;
    virtual size_t toLocal (unsigned char&  to,
				  const void* from) const = 0;
    virtual size_t toLocal (short&          to,
				  const void* from) const = 0;
    virtual size_t toLocal (unsigned short& to,
				  const void* from) const = 0;
    virtual size_t toLocal (int&            to,
				  const void* from) const = 0;
    virtual size_t toLocal (unsigned int&   to,
				  const void* from) const = 0;
    virtual size_t toLocal (Int64&          to,
				  const void* from) const = 0;
    virtual size_t toLocal (uInt64&         to,
				  const void* from) const = 0;
    virtual size_t toLocal (float&          to,
				  const void* from) const = 0;
    virtual size_t toLocal (double&         to,
				  const void* from) const = 0;
    // </group>
    
    // Convert nr values from foreign format to local format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version handles characters (thus may involve conversion
    // EBCDIC to ASCII), while the unsigned chars are simply bytes.
    // </note>
    // <group>
    virtual size_t toLocal (char*           to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (unsigned char*  to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (short*          to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (unsigned short* to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (int*            to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (unsigned int*   to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (Int64*          to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (uInt64*         to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (float*          to, const void* from,
                            size_t nr) const = 0;
    virtual size_t toLocal (double*         to, const void* from,
                            size_t nr) const = 0;
    // </group>

    // Convert one value from local format to foreign format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version handles characters (thus may involve conversion
    // ASCII to EBCDIC), while the unsigned chars are simply bytes.
    // </note>
    // <group>
    virtual size_t fromLocal (void* to, char           from) const = 0;
    virtual size_t fromLocal (void* to, unsigned char  from) const = 0;
    virtual size_t fromLocal (void* to, short          from) const = 0;
    virtual size_t fromLocal (void* to, unsigned short from) const = 0;
    virtual size_t fromLocal (void* to, int            from) const = 0;
    virtual size_t fromLocal (void* to, unsigned int   from) const = 0;
    virtual size_t fromLocal (void* to, Int64          from) const = 0;
    virtual size_t fromLocal (void* to, uInt64         from) const = 0;
    virtual size_t fromLocal (void* to, float          from) const = 0;
    virtual size_t fromLocal (void* to, double         from) const = 0;
    // </group>
    
    // Convert nr values from local format to foreign format.
    // The from and to buffer should not overlap.
    // <note>
    // The char version handles characters (thus may involve conversion
    // ASCII to EBCDIC), while the unsigned chars are simply bytes.
    // </note>
    // <group>
    virtual size_t fromLocal (void* to, const char*           from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const unsigned char*  from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const short*          from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const unsigned short* from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const int*            from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const unsigned int*   from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const Int64*          from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const uInt64*         from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const float*          from,
                              size_t nr) const = 0;
    virtual size_t fromLocal (void* to, const double*         from,
                              size_t nr) const = 0;
    // </group>

    // Determine if the data for a data type can be simply copied, thus
    // if no conversion is needed.
    // <group>
    virtual Bool canCopy (const char*) const = 0;
    virtual Bool canCopy (const unsigned char*) const = 0;
    virtual Bool canCopy (const short*) const = 0;
    virtual Bool canCopy (const unsigned short*) const = 0;
    virtual Bool canCopy (const int*) const = 0;
    virtual Bool canCopy (const unsigned int*) const = 0;
    virtual Bool canCopy (const Int64*) const = 0;
    virtual Bool canCopy (const uInt64*) const = 0;
    virtual Bool canCopy (const float*) const = 0;
    virtual Bool canCopy (const double*) const = 0;
    // </group>

    // Get the external size of the data type.
    // <group>
    virtual unsigned int externalSize (const char*) const = 0;
    virtual unsigned int externalSize (const unsigned char*) const = 0;
    virtual unsigned int externalSize (const short*) const = 0;
    virtual unsigned int externalSize (const unsigned short*) const = 0;
    virtual unsigned int externalSize (const int*) const = 0;
    virtual unsigned int externalSize (const unsigned int*) const = 0;
    virtual unsigned int externalSize (const Int64*) const = 0;
    virtual unsigned int externalSize (const uInt64*) const = 0;
    virtual unsigned int externalSize (const float*) const = 0;
    virtual unsigned int externalSize (const double*) const = 0;
    // </group>
};


inline DataConversion::DataConversion()
{}



} //# NAMESPACE CASACORE - END

#endif
