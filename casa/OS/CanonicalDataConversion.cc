//# CanonicalDataConversion.cc: A class with virtual functions to convert canonical format
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


#include <casacore/casa/OS/CanonicalDataConversion.h>
#include <casacore/casa/OS/CanonicalConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

CanonicalDataConversion::~CanonicalDataConversion()
{}


size_t CanonicalDataConversion::toLocal (char& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (unsigned char& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (short& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (unsigned short& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (int& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (unsigned int& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (Int64& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (uInt64& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (float& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}
size_t CanonicalDataConversion::toLocal (double& to,
                                         const void* from) const
{
    return CanonicalConversion::toLocal (to, from);
}

size_t CanonicalDataConversion::toLocal (char* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (unsigned char* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (short* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (unsigned short* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (int* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (unsigned int* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (Int64* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (uInt64* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (float* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}
size_t CanonicalDataConversion::toLocal (double* to,
                                         const void* from,
                                         size_t nr) const
{
    return CanonicalConversion::toLocal (to, from, nr);
}

size_t CanonicalDataConversion::fromLocal (void* to,
                                           char from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           unsigned char from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           short from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           unsigned short from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           int from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           unsigned int from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           Int64 from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           uInt64 from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           float from) const
{
    return CanonicalConversion::fromLocal (to, from);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           double from) const
{
    return CanonicalConversion::fromLocal (to, from);
}

size_t CanonicalDataConversion::fromLocal (void* to,
                                           const char* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const unsigned char* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const short* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const unsigned short* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const int* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const unsigned int* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const Int64* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const uInt64* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const float* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}
size_t CanonicalDataConversion::fromLocal (void* to,
                                           const double* from,
                                           size_t nr) const
{
    return CanonicalConversion::fromLocal (to, from, nr);
}


Bool CanonicalDataConversion::canCopy (const char*) const
{
    return (CONVERT_CAN_CHAR == 0);
}
Bool CanonicalDataConversion::canCopy (const unsigned char*) const
{
    return (CONVERT_CAN_UCHAR == 0);
}
Bool CanonicalDataConversion::canCopy (const short*) const
{
    return (CONVERT_CAN_SHORT == 0);
}
Bool CanonicalDataConversion::canCopy (const unsigned short*) const
{
    return (CONVERT_CAN_USHORT == 0);
}
Bool CanonicalDataConversion::canCopy (const int*) const
{
    return (CONVERT_CAN_INT == 0);
}
Bool CanonicalDataConversion::canCopy (const unsigned int*) const
{
    return (CONVERT_CAN_UINT == 0);
}
Bool CanonicalDataConversion::canCopy (const Int64*) const
{
    return (CONVERT_CAN_INT64 == 0);
}
Bool CanonicalDataConversion::canCopy (const uInt64*) const
{
    return (CONVERT_CAN_UINT64 == 0);
}
Bool CanonicalDataConversion::canCopy (const float*) const
{
    return (CONVERT_CAN_FLOAT == 0);
}
Bool CanonicalDataConversion::canCopy (const double*) const
{
    return (CONVERT_CAN_DOUBLE == 0);
}


unsigned int CanonicalDataConversion::externalSize (const char*) const
{
    return SIZE_CAN_CHAR;
}
unsigned int CanonicalDataConversion::externalSize (const unsigned char*) const
{
    return SIZE_CAN_UCHAR;
}
unsigned int CanonicalDataConversion::externalSize (const short*) const
{
    return SIZE_CAN_SHORT;
}
unsigned int CanonicalDataConversion::externalSize (const unsigned short*) const
{
    return SIZE_CAN_USHORT;
}
unsigned int CanonicalDataConversion::externalSize (const int*) const
{
    return SIZE_CAN_INT;
}
unsigned int CanonicalDataConversion::externalSize (const unsigned int*) const
{
    return SIZE_CAN_UINT;
}
unsigned int CanonicalDataConversion::externalSize (const Int64*) const
{
    return SIZE_CAN_INT64;
}
unsigned int CanonicalDataConversion::externalSize (const uInt64*) const
{
    return SIZE_CAN_UINT64;
}
unsigned int CanonicalDataConversion::externalSize (const float*) const
{
    return SIZE_CAN_FLOAT;
}
unsigned int CanonicalDataConversion::externalSize (const double*) const
{
    return SIZE_CAN_DOUBLE;
}

} //# NAMESPACE CASACORE - END

