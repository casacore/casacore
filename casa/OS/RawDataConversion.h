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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_RAWDATACONVERSION_H
#define CASA_RAWDATACONVERSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/DataConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
    size_t toLocal (char&           to, const void* from) const override;
    size_t toLocal (unsigned char&  to, const void* from) const override;
    size_t toLocal (short&          to, const void* from) const override;
    size_t toLocal (unsigned short& to, const void* from) const override;
    size_t toLocal (int&            to, const void* from) const override;
    size_t toLocal (unsigned int&   to, const void* from) const override;
    size_t toLocal (Int64&          to, const void* from) const override;
    size_t toLocal (uInt64&         to, const void* from) const override;
    size_t toLocal (float&          to, const void* from) const override;
    size_t toLocal (double&         to, const void* from) const override;
    // </group>

    // Copy multiple values from external to local (is a simple memcpy).
    // The from and to buffer should not overlap.
    // <group>
    size_t toLocal (char*           to, const void* from,
                    size_t nr) const override;
    size_t toLocal (unsigned char*  to, const void* from,
                    size_t nr) const override;
    size_t toLocal (short*          to, const void* from,
                    size_t nr) const override;
    size_t toLocal (unsigned short* to, const void* from,
                    size_t nr) const override;
    size_t toLocal (int*            to, const void* from,
                    size_t nr) const override;
    size_t toLocal (unsigned int*   to, const void* from,
                    size_t nr) const override;
    size_t toLocal (Int64*          to, const void* from,
                    size_t nr) const override;
    size_t toLocal (uInt64*         to, const void* from,
                    size_t nr) const override;
    size_t toLocal (float*          to, const void* from,
                    size_t nr) const override;
    size_t toLocal (double*         to, const void* from,
                    size_t nr) const override;
    // </group>

    // Copy one value from local to external (is a simple memcpy).
    // The from and to buffer should not overlap.
    // <group>
    size_t fromLocal (void* to, char           from) const override;
    size_t fromLocal (void* to, unsigned char  from) const override;
    size_t fromLocal (void* to, short          from) const override;
    size_t fromLocal (void* to, unsigned short from) const override;
    size_t fromLocal (void* to, int            from) const override;
    size_t fromLocal (void* to, unsigned int   from) const override;
    size_t fromLocal (void* to, Int64          from) const override;
    size_t fromLocal (void* to, uInt64         from) const override;
    size_t fromLocal (void* to, float          from) const override;
    size_t fromLocal (void* to, double         from) const override;
    // </group>

    // Copy multiple values from local to external (is a simple memcpy).
    // The from and to buffer should not overlap.
    // <group>
    size_t fromLocal (void* to, const char*           from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const unsigned char*  from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const short*          from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const unsigned short* from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const int*            from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const unsigned int*   from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const Int64*          from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const uInt64*         from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const float*          from,
                      size_t nr) const override;
    size_t fromLocal (void* to, const double*         from,
                      size_t nr) const override;
    // </group>

    // Determine if the data for a data type can be simply copied, thus
    // if no conversion is needed. This is always True.
    // <group>
    Bool canCopy (const char*) const override;
    Bool canCopy (const unsigned char*) const override;
    Bool canCopy (const short*) const override;
    Bool canCopy (const unsigned short*) const override;
    Bool canCopy (const int*) const override;
    Bool canCopy (const unsigned int*) const override;
    Bool canCopy (const Int64*) const override;
    Bool canCopy (const uInt64*) const override;
    Bool canCopy (const float*) const override;
    Bool canCopy (const double*) const override;
    // </group>

    // Get the external size of the data type.
    // This returns the <src>sizeof</src>.
    // <group>
    unsigned int externalSize (const char*) const override;
    unsigned int externalSize (const unsigned char*) const override;
    unsigned int externalSize (const short*) const override;
    unsigned int externalSize (const unsigned short*) const override;
    unsigned int externalSize (const int*) const override;
    unsigned int externalSize (const unsigned int*) const override;
    unsigned int externalSize (const Int64*) const override;
    unsigned int externalSize (const uInt64*) const override;
    unsigned int externalSize (const float*) const override;
    unsigned int externalSize (const double*) const override;
    // </group>
};


inline RawDataConversion::RawDataConversion()
{}



} //# NAMESPACE CASACORE - END

#endif
