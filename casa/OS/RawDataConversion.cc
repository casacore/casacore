//# RawDataConversion.cc: A class with virtual functions to copy without conversion
//# Copyright (C) 1996,2001
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


#include <casacore/casa/OS/RawDataConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

RawDataConversion::~RawDataConversion()
{}


#define RAWDATACONVERSION_DOIT(T) \
size_t RawDataConversion::toLocal (T& to, \
					 const void* from) const \
{ \
    memcpy (&to, from, sizeof(T)); \
    return sizeof(T); \
} \
size_t RawDataConversion::toLocal (T* to, const void* from, \
				   size_t nr) const \
{ \
    memcpy (to, from, nr * sizeof(T)); \
    return nr * sizeof(T); \
} \
size_t RawDataConversion::fromLocal (void* to, T from) const \
{ \
    memcpy (to, &from, sizeof(T)); \
    return sizeof(T); \
} \
size_t RawDataConversion::fromLocal (void* to, const T* from, \
				     size_t nr) const \
{ \
    memcpy (to, from, nr * sizeof(T)); \
    return nr * sizeof(T); \
} \
Bool RawDataConversion::canCopy (const T*) const \
{ \
    return True; \
} \
unsigned int RawDataConversion::externalSize (const T*) const \
{ \
    return sizeof(T); \
}


RAWDATACONVERSION_DOIT(char)
RAWDATACONVERSION_DOIT(unsigned char)
RAWDATACONVERSION_DOIT(short)
RAWDATACONVERSION_DOIT(unsigned short)
RAWDATACONVERSION_DOIT(int)
RAWDATACONVERSION_DOIT(unsigned int)
RAWDATACONVERSION_DOIT(Int64)
RAWDATACONVERSION_DOIT(uInt64)
RAWDATACONVERSION_DOIT(float)
RAWDATACONVERSION_DOIT(double)

} //# NAMESPACE CASACORE - END

