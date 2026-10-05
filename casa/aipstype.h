// # aipstype.h: Global initialization for standard Casacore types
// # Copyright (C) 2000,2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_AIPSTYPE_H
#define CASA_AIPSTYPE_H

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// Define the standard types used by Casacore

[[deprecated("Use bool")]]
typedef bool Bool;
[[deprecated("Use true")]]
const bool True = true;
[[deprecated("Use false")]]
const bool False = false;
[[deprecated("Use char")]]
typedef char Char;
[[deprecated("Use unsigned char")]]
typedef unsigned char uChar;
[[deprecated("Use short")]]
typedef short Short;
[[deprecated("Use unsigned short")]]
typedef unsigned short uShort;
[[deprecated("Use int")]]
typedef int Int;
[[deprecated("Use unsigned int")]]
typedef unsigned int uInt;
[[deprecated("Use long")]]
typedef long Long;
[[deprecated("Use unsigned long")]]
typedef unsigned long uLong;
[[deprecated("Use float")]]
typedef float Float;
[[deprecated("Use double")]]
typedef double Double;
[[deprecated("Use long double")]]
typedef long double lDouble;

}  // namespace casacore

#endif
