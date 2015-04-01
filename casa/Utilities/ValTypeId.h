//# ValType.h: The id-string of a value type
//# Copyright (C) 1993,1994,1995,1996,1998,2000
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

#ifndef CASA_VALTYPEID_H
#define CASA_VALTYPEID_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/ValType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// The id-string of a value type
// </summary>

// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/20" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> class <linkto class=ValType">ValType</linkto>
// </prerequisite>

// <synopsis>
// The templated global functions valDataTypeId get the data type id string
// of the given type.
// For standard types this is the ValType data type string.
// For other types (i.e. classes) it is the result of the class dataTypeId
// function.
// </synopsis>

// <group name=typeid>

template<class T> inline String valDataTypeId (const T*)
    { return T::dataTypeId(); }
inline String valDataTypeId (const Bool* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const Char* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const uChar* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const Short* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const uShort* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const Int* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const uInt* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const float* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const double* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const Complex* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const DComplex* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const String* obj)
    { return ValType::getTypeStr (obj); }
inline String valDataTypeId (const TableRecord* obj)
    { return ValType::getTypeStr (obj); }

// </group>

} //# NAMESPACE CASACORE - END

#endif



