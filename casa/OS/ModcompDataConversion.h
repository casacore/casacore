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
//#
//# $Id$

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
//  <li> Support data type long Double.
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
    virtual size_t toLocal (Char&   to, const void* from) const;
    virtual size_t toLocal (uChar&  to, const void* from) const;
    virtual size_t toLocal (Short&  to, const void* from) const;
    virtual size_t toLocal (uShort& to, const void* from) const;
    virtual size_t toLocal (Int&    to, const void* from) const;
    virtual size_t toLocal (uInt&   to, const void* from) const;
    virtual size_t toLocal (Int64&  to, const void* from) const;
    virtual size_t toLocal (uInt64& to, const void* from) const;
    virtual size_t toLocal (Float&  to, const void* from) const;
    virtual size_t toLocal (Double& to, const void* from) const;
    // </group>
    
    // Convert nr values from Modcomp format to local format.
    // The from and to buffer should not overlap.
    // <group>
    virtual size_t toLocal (Char*   to, const void* from, size_t nr) const;
    virtual size_t toLocal (uChar*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (Short*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (uShort* to, const void* from, size_t nr) const;
    virtual size_t toLocal (Int*    to, const void* from, size_t nr) const;
    virtual size_t toLocal (uInt*   to, const void* from, size_t nr) const;
    virtual size_t toLocal (Int64*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (uInt64* to, const void* from, size_t nr) const;
    virtual size_t toLocal (Float*  to, const void* from, size_t nr) const;
    virtual size_t toLocal (Double* to, const void* from, size_t nr) const;
    // </group>

    // Convert one value from local format to Modcomp format.
    // The from and to buffer should not overlap.
    // <group>
    virtual size_t fromLocal (void* to, Char   from) const;
    virtual size_t fromLocal (void* to, uChar  from) const;
    virtual size_t fromLocal (void* to, Short  from) const;
    virtual size_t fromLocal (void* to, uShort from) const;
    virtual size_t fromLocal (void* to, Int    from) const;
    virtual size_t fromLocal (void* to, uInt   from) const;
    virtual size_t fromLocal (void* to, Int64  from) const;
    virtual size_t fromLocal (void* to, uInt64 from) const;
    virtual size_t fromLocal (void* to, Float  from) const;
    virtual size_t fromLocal (void* to, Double from) const;
    // </group>
    
    // Convert nr values from local format to ModComp format.
    // The from and to buffer should not overlap.
    // <group>
    virtual size_t fromLocal (void* to, const Char*   from, size_t nr) const;
    virtual size_t fromLocal (void* to, const uChar*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const Short*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const uShort* from, size_t nr) const;
    virtual size_t fromLocal (void* to, const Int*    from, size_t nr) const;
    virtual size_t fromLocal (void* to, const uInt*   from, size_t nr) const;
    virtual size_t fromLocal (void* to, const Int64*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const uInt64* from, size_t nr) const;
    virtual size_t fromLocal (void* to, const Float*  from, size_t nr) const;
    virtual size_t fromLocal (void* to, const Double* from, size_t nr) const;
    // </group>

    // Determine if the data for a data type can be simply copied, thus
    // if no conversion is needed.
    // <group>
    virtual Bool canCopy (const Char*) const;
    virtual Bool canCopy (const uChar*) const;
    virtual Bool canCopy (const Short*) const;
    virtual Bool canCopy (const uShort*) const;
    virtual Bool canCopy (const Int*) const;
    virtual Bool canCopy (const uInt*) const;
    virtual Bool canCopy (const Int64*) const;
    virtual Bool canCopy (const uInt64*) const;
    virtual Bool canCopy (const Float*) const;
    virtual Bool canCopy (const Double*) const;
    // </group>

    // Get the external size of the data type.
    // <group>
    virtual uInt externalSize (const Char*) const;
    virtual uInt externalSize (const uChar*) const;
    virtual uInt externalSize (const Short*) const;
    virtual uInt externalSize (const uShort*) const;
    virtual uInt externalSize (const Int*) const;
    virtual uInt externalSize (const uInt*) const;
    virtual uInt externalSize (const Int64*) const;
    virtual uInt externalSize (const uInt64*) const;
    virtual uInt externalSize (const Float*) const;
    virtual uInt externalSize (const Double*) const;
    // </group>
};


inline ModcompDataConversion::ModcompDataConversion() {
}

} //# NAMESPACE CASACORE - END

#endif
