//# ModcompDataConversion.h: A DataConversion class to convert Modcomp format.
//# Copyright (C) 1999,2001
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

#ifndef CASA_MODCOMPDATACONVERSION_H
#define CASA_MODCOMPDATACONVERSION_H

#include <casacore/casa/aips.h>
#include <casacore/casa/OS/DataConversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>A DataConversion class to convert between Modcomp format.</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This class is a specialization of the abstract base class
// <linkto class=DataConversion>DataConversion</linkto>.
// It contains functions to convert data from/to the Modcomp format
// using the static functions in class <linkto class=ModcompConversion>
// ModcompConversion</linkto>.
// </synopsis>

// <example>
// See example in class <linkto class=DataConversion>DataConversion</linkto>.
// </example>

// <motivation>
// This class is an addition to <linkto class=ModcompConversion>
// ModcompConversion</linkto>
// to be able to use the conversion functions in a polymorphic way.
// </motivation>

// <todo asof="$DATE$">
//  <li> Support data type long double.
// </todo>


class ModcompDataConversion :public DataConversion
{
public:
    // Construct the object.
    ModcompDataConversion();

    virtual ~ModcompDataConversion();

    // Convert one value from Modcomp format to local format.
    // The from and to buffer should not overlap.
    // <group>
    virtual size_t toLocal (char&   to, const void* from) const;
    virtual size_t toLocal (unsigned char&  to, const void* from) const;
    virtual size_t toLocal (int16_t&  to, const void* from) const;
    virtual size_t toLocal (uint16_t& to, const void* from) const;
    virtual size_t toLocal (int32_t&    to, const void* from) const;
    virtual size_t toLocal (uint32_t&   to, const void* from) const;
    virtual size_t toLocal (int64_t&  to, const void* from) const;
    virtual size_t toLocal (uint64_t& to, const void* from) const;
    virtual size_t toLocal (float&  to, const void* from) const;
    virtual size_t toLocal (double& to, const void* from) const;
    // </group>
    
    // Convert nr values from Modcomp format to local format.
    // The from and to buffer should not overlap.
    // <group>
    virtual size_t toLocal (char*   to, const void* from, size_t nr) const;
    virtual size_t toLocal (unsigned char*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (int16_t*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (uint16_t* to, const void* from, size_t nr) const;
    virtual size_t toLocal (int32_t*    to, const void* from, size_t nr) const;
    virtual size_t toLocal (uint32_t*   to, const void* from, size_t nr) const;
    virtual size_t toLocal (int64_t*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (uint64_t* to, const void* from, size_t nr) const;
    virtual size_t toLocal (float*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (double* to, const void* from, size_t nr) const;
    // </group>

    // Convert one value from local format to Modcomp format.
    // The from and to buffer should not overlap.
    // <group>
    virtual size_t fromLocal (void* to, char   from) const;
    virtual size_t fromLocal (void* to, unsigned char  from) const;
    virtual size_t fromLocal (void* to, int16_t  from) const;
    virtual size_t fromLocal (void* to, uint16_t from) const;
    virtual size_t fromLocal (void* to, int32_t    from) const;
    virtual size_t fromLocal (void* to, uint32_t   from) const;
    virtual size_t fromLocal (void* to, int64_t  from) const;
    virtual size_t fromLocal (void* to, uint64_t from) const;
    virtual size_t fromLocal (void* to, float  from) const;
    virtual size_t fromLocal (void* to, double from) const;
    // </group>
    
    // Convert nr values from local format to ModComp format.
    // The from and to buffer should not overlap.
    // <group>
    virtual size_t fromLocal (void* to, const char*   from, size_t nr) const;
    virtual size_t fromLocal (void* to, const unsigned char*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const int16_t*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const uint16_t* from, size_t nr) const;
    virtual size_t fromLocal (void* to, const int32_t*    from, size_t nr) const;
    virtual size_t fromLocal (void* to, const uint32_t*   from, size_t nr) const;
    virtual size_t fromLocal (void* to, const int64_t*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const uint64_t* from, size_t nr) const;
    virtual size_t fromLocal (void* to, const float*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const double* from, size_t nr) const;
    // </group>

    // Determine if the data for a data type can be simply copied, thus
    // if no conversion is needed.
    // <group>
    virtual bool canCopy (const char*) const;
    virtual bool canCopy (const unsigned char*) const;
    virtual bool canCopy (const int16_t*) const;
    virtual bool canCopy (const uint16_t*) const;
    virtual bool canCopy (const int32_t*) const;
    virtual bool canCopy (const uint32_t*) const;
    virtual bool canCopy (const int64_t*) const;
    virtual bool canCopy (const uint64_t*) const;
    virtual bool canCopy (const float*) const;
    virtual bool canCopy (const double*) const;
    // </group>

    // Get the external size of the data type.
    // <group>
    virtual uint32_t externalSize (const char*) const;
    virtual uint32_t externalSize (const unsigned char*) const;
    virtual uint32_t externalSize (const int16_t*) const;
    virtual uint32_t externalSize (const uint16_t*) const;
    virtual uint32_t externalSize (const int32_t*) const;
    virtual uint32_t externalSize (const uint32_t*) const;
    virtual uint32_t externalSize (const int64_t*) const;
    virtual uint32_t externalSize (const uint64_t*) const;
    virtual uint32_t externalSize (const float*) const;
    virtual uint32_t externalSize (const double*) const;
    // </group>
};


inline ModcompDataConversion::ModcompDataConversion() {
}

} //# NAMESPACE CASACORE - END

#endif
