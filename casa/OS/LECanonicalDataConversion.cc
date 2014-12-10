//# LECanonicalDataConversion.cc: A class with virtual functions to convert little endian canonical format
//# Copyright (C) 2002
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


#include <casacore/casa/OS/LECanonicalDataConversion.h>
#include <casacore/casa/OS/LECanonicalConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LECanonicalDataConversion::~LECanonicalDataConversion()
{}


size_t LECanonicalDataConversion::toLocal (char& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (unsigned char& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (short& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (unsigned short& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (int& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (unsigned int& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (Int64& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (uInt64& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (float& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}
size_t LECanonicalDataConversion::toLocal (double& to,
                                           const void* from) const
{
    return LECanonicalConversion::toLocal (to, from);
}

size_t LECanonicalDataConversion::toLocal (char* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (unsigned char* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (short* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (unsigned short* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (int* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (unsigned int* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (Int64* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (uInt64* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (float* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}
size_t LECanonicalDataConversion::toLocal (double* to,
                                           const void* from,
                                           size_t nr) const
{
    return LECanonicalConversion::toLocal (to, from, nr);
}

size_t LECanonicalDataConversion::fromLocal (void* to,
                                             char from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             unsigned char from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             short from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             unsigned short from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             int from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             unsigned int from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             Int64 from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             uInt64 from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             float from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             double from) const
{
    return LECanonicalConversion::fromLocal (to, from);
}

size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const char* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const unsigned char* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const short* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const unsigned short* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const int* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const unsigned int* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const Int64* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const uInt64* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const float* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}
size_t LECanonicalDataConversion::fromLocal (void* to,
                                             const double* from,
                                             size_t nr) const
{
    return LECanonicalConversion::fromLocal (to, from, nr);
}


Bool LECanonicalDataConversion::canCopy (const char*) const
{
    return (CONVERT_LECAN_CHAR == 0);
}
Bool LECanonicalDataConversion::canCopy (const unsigned char*) const
{
    return (CONVERT_LECAN_UCHAR == 0);
}
Bool LECanonicalDataConversion::canCopy (const short*) const
{
    return (CONVERT_LECAN_SHORT == 0);
}
Bool LECanonicalDataConversion::canCopy (const unsigned short*) const
{
    return (CONVERT_LECAN_USHORT == 0);
}
Bool LECanonicalDataConversion::canCopy (const int*) const
{
    return (CONVERT_LECAN_INT == 0);
}
Bool LECanonicalDataConversion::canCopy (const unsigned int*) const
{
    return (CONVERT_LECAN_UINT == 0);
}
Bool LECanonicalDataConversion::canCopy (const Int64*) const
{
    return (CONVERT_LECAN_INT64 == 0);
}
Bool LECanonicalDataConversion::canCopy (const uInt64*) const
{
    return (CONVERT_LECAN_UINT64 == 0);
}
Bool LECanonicalDataConversion::canCopy (const float*) const
{
    return (CONVERT_LECAN_FLOAT == 0);
}
Bool LECanonicalDataConversion::canCopy (const double*) const
{
    return (CONVERT_LECAN_DOUBLE == 0);
}


unsigned int LECanonicalDataConversion::externalSize (const char*) const
{
    return SIZE_LECAN_CHAR;
}
unsigned int LECanonicalDataConversion::externalSize (const unsigned char*) const
{
    return SIZE_LECAN_UCHAR;
}
unsigned int LECanonicalDataConversion::externalSize (const short*) const
{
    return SIZE_LECAN_SHORT;
}
unsigned int LECanonicalDataConversion::externalSize (const unsigned short*) const
{
    return SIZE_LECAN_USHORT;
}
unsigned int LECanonicalDataConversion::externalSize (const int*) const
{
    return SIZE_LECAN_INT;
}
unsigned int LECanonicalDataConversion::externalSize (const unsigned int*) const
{
    return SIZE_LECAN_UINT;
}
unsigned int LECanonicalDataConversion::externalSize (const Int64*) const
{
    return SIZE_LECAN_INT64;
}
unsigned int LECanonicalDataConversion::externalSize (const uInt64*) const
{
    return SIZE_LECAN_UINT64;
}
unsigned int LECanonicalDataConversion::externalSize (const float*) const
{
    return SIZE_LECAN_FLOAT;
}
unsigned int LECanonicalDataConversion::externalSize (const double*) const
{
    return SIZE_LECAN_DOUBLE;
}

} //# NAMESPACE CASACORE - END

