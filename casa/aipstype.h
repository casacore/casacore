//# aipstype.h: Global initialization for standard Casacore types
//# Copyright (C) 2000,2001,2002
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

#ifndef CASA_AIPSTYPE_H
#define CASA_AIPSTYPE_H

// For temporary backward namespace compatibility, use casa as alias for casacore.
//# Note: namespace casa = casacore; does not work for forward declarations.
#define casacore casa

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Define the standard types used by Casacore

typedef bool Bool;
const Bool True = true;
const Bool False = false;

typedef char Char;
typedef unsigned char uChar;
typedef short Short;
typedef unsigned short uShort;
typedef int Int;
typedef unsigned int uInt;
typedef long Long;
typedef unsigned long uLong;
typedef float Float;
typedef double Double;
typedef long double lDouble;

} //# NAMESPACE CASACORE - END

#endif
