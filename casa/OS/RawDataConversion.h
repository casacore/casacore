//# RawDataConversion.h: A class with virtual functions to copy without conversion
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

#ifndef CASA_RAWDATACONVERSION_H
#define CASA_RAWDATACONVERSION_H

//# Includes
#include <casa/OS/DataConversion.h>


namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// A class with virtual functions to copy without conversion
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tDataConversion" demos="">
// </reviewed>

// <synopsis>
// This class is a specialization of the abstract base class
// <linkto class=DataConversion>DataConversion</linkto>.
// It contains functions to copy data without conversion.
// <p>
// This class exists to make it possible to use a <src>DataConversion</src>
// object when no conversion is actually needed.
// </synopsis>

// <example>
// See example in class <linkto class=DataConversion>DataConversion</linkto>.
// </example>

// <motivation>
// This class makes it possible to use the conversion framework
// for operations where a conversion is only a simple copy.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support data type long double.
// </todo>


class RawDataConversion : public DataConversion
{
public:
    // Construct the object.
    RawDataConversion();

    virtual ~RawDataConversion();

    // Copy one value from external to local (is a simple memcpy).
    // The from and to buffer should not overlap.
    // <group>
    virtual unsigned int toLocal (char&           to, const void* from) const;
    virtual unsigned int toLocal (unsigned char&  to, const void* from) const;
    virtual unsigned int toLocal (short&          to, const void* from) const;
    virtual unsigned int toLocal (unsigned short& to, const void* from) const;
    virtual unsigned int toLocal (int&            to, const void* from) const;
    virtual unsigned int toLocal (unsigned int&   to, const void* from) const;
    virtual unsigned int toLocal (Int64&          to, const void* from) const;
    virtual unsigned int toLocal (uInt64&         to, const void* from) const;
    virtual unsigned int toLocal (float&          to, const void* from) const;
    virtual unsigned int toLocal (double&         to, const void* from) const;
    // </group>
    
    // Copy multiple values from external to local (is a simple memcpy).
    // The from and to buffer should not overlap.
    // <group>
    virtual unsigned int toLocal (char*           to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (unsigned char*  to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (short*          to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (unsigned short* to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (int*            to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (unsigned int*   to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (Int64*          to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (uInt64*         to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (float*          to, const void* from,
				  unsigned int nr) const;
    virtual unsigned int toLocal (double*         to, const void* from,
				  unsigned int nr) const;
    // </group>

    // Copy one value from local to external (is a simple memcpy).
    // The from and to buffer should not overlap.
    // <group>
    virtual unsigned int fromLocal (void* to, char           from) const;
    virtual unsigned int fromLocal (void* to, unsigned char  from) const;
    virtual unsigned int fromLocal (void* to, short          from) const;
    virtual unsigned int fromLocal (void* to, unsigned short from) const;
    virtual unsigned int fromLocal (void* to, int            from) const;
    virtual unsigned int fromLocal (void* to, unsigned int   from) const;
    virtual unsigned int fromLocal (void* to, Int64          from) const;
    virtual unsigned int fromLocal (void* to, uInt64         from) const;
    virtual unsigned int fromLocal (void* to, float          from) const;
    virtual unsigned int fromLocal (void* to, double         from) const;
    // </group>
    
    // Copy multiple values from local to external (is a simple memcpy).
    // The from and to buffer should not overlap.
    // <group>
    virtual unsigned int fromLocal (void* to, const char*           from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const unsigned char*  from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const short*          from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const unsigned short* from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const int*            from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const unsigned int*   from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const Int64*          from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const uInt64*         from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const float*          from,
				    unsigned int nr) const;
    virtual unsigned int fromLocal (void* to, const double*         from,
				    unsigned int nr) const;
    // </group>

    // Determine if the data for a data type can be simply copied, thus
    // if no conversion is needed. This is always True.
    // <group>
    virtual Bool canCopy (const char*) const;
    virtual Bool canCopy (const unsigned char*) const;
    virtual Bool canCopy (const short*) const;
    virtual Bool canCopy (const unsigned short*) const;
    virtual Bool canCopy (const int*) const;
    virtual Bool canCopy (const unsigned int*) const;
    virtual Bool canCopy (const Int64*) const;
    virtual Bool canCopy (const uInt64*) const;
    virtual Bool canCopy (const float*) const;
    virtual Bool canCopy (const double*) const;
    // </group>

    // Get the external size of the data type.
    // This returns the <src>sizeof</src>.
    // <group>
    virtual unsigned int externalSize (const char*) const;
    virtual unsigned int externalSize (const unsigned char*) const;
    virtual unsigned int externalSize (const short*) const;
    virtual unsigned int externalSize (const unsigned short*) const;
    virtual unsigned int externalSize (const int*) const;
    virtual unsigned int externalSize (const unsigned int*) const;
    virtual unsigned int externalSize (const Int64*) const;
    virtual unsigned int externalSize (const uInt64*) const;
    virtual unsigned int externalSize (const float*) const;
    virtual unsigned int externalSize (const double*) const;
    // </group>
};


inline RawDataConversion::RawDataConversion()
{}



} //# NAMESPACE CASA - END

#endif
