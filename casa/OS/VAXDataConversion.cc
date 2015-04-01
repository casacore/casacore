//# VAXDataConversion.cc: A class with virtual functions to convert VAX format
//# Copyright (C) 1996,1997,2001
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


#include <casacore/casa/OS/VAXDataConversion.h>
#include <casacore/casa/OS/VAXConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

VAXDataConversion::~VAXDataConversion()
{}


size_t VAXDataConversion::toLocal (char&           to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_CHAR;
}
size_t VAXDataConversion::toLocal (unsigned char&  to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_UCHAR;
}
size_t VAXDataConversion::toLocal (short&          to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_SHORT;
}
size_t VAXDataConversion::toLocal (unsigned short& to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_USHORT;
}
size_t VAXDataConversion::toLocal (int&            to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_INT;
}
size_t VAXDataConversion::toLocal (unsigned int&   to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_UINT;
}
size_t VAXDataConversion::toLocal (Int64&          to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_INT64;
}
size_t VAXDataConversion::toLocal (uInt64&         to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_UINT64;
}
size_t VAXDataConversion::toLocal (float&          to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_FLOAT;
}
size_t VAXDataConversion::toLocal (double&         to,
                                   const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_DOUBLE;
}


size_t VAXDataConversion::toLocal (char*           to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_CHAR;
}
size_t VAXDataConversion::toLocal (unsigned char*  to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_UCHAR;
}
size_t VAXDataConversion::toLocal (short*          to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_SHORT;
}
size_t VAXDataConversion::toLocal (unsigned short* to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_USHORT;
}
size_t VAXDataConversion::toLocal (int*            to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_INT;
}
size_t VAXDataConversion::toLocal (unsigned int*   to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_UINT;
}
size_t VAXDataConversion::toLocal (Int64*          to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_INT64;
}
size_t VAXDataConversion::toLocal (uInt64*         to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_UINT64;
}
size_t VAXDataConversion::toLocal (float*          to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_FLOAT;
}
size_t VAXDataConversion::toLocal (double*         to, const void* from,
                                   size_t nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_DOUBLE;
}

size_t VAXDataConversion::fromLocal (void* to, char           from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_CHAR;
}
size_t VAXDataConversion::fromLocal (void* to, unsigned char  from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_UCHAR;
}
size_t VAXDataConversion::fromLocal (void* to, short          from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_SHORT;
}
size_t VAXDataConversion::fromLocal (void* to, unsigned short from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_USHORT;
}
size_t VAXDataConversion::fromLocal (void* to, int            from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_INT;
}
size_t VAXDataConversion::fromLocal (void* to, unsigned int   from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_UINT;
}
size_t VAXDataConversion::fromLocal (void* to, Int64          from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_INT64;
}
size_t VAXDataConversion::fromLocal (void* to, uInt64         from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_UINT64;
}
size_t VAXDataConversion::fromLocal (void* to, float          from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_FLOAT;
}
size_t VAXDataConversion::fromLocal (void* to, double         from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_DOUBLE;
}

size_t VAXDataConversion::fromLocal (void* to, const char* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_CHAR;
}
size_t VAXDataConversion::fromLocal (void* to, const unsigned char* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_UCHAR;
}
size_t VAXDataConversion::fromLocal (void* to, const short* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_SHORT;
}
size_t VAXDataConversion::fromLocal (void* to, const unsigned short* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_USHORT;
}
size_t VAXDataConversion::fromLocal (void* to, const int* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_INT;
}
size_t VAXDataConversion::fromLocal (void* to, const unsigned int* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_UINT;
}
size_t VAXDataConversion::fromLocal (void* to, const Int64* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_INT64;
}
size_t VAXDataConversion::fromLocal (void* to, const uInt64* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_UINT64;
}
size_t VAXDataConversion::fromLocal (void* to, const float* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_FLOAT;
}
size_t VAXDataConversion::fromLocal (void* to, const double* from,
                                     size_t nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_DOUBLE;
}


Bool VAXDataConversion::canCopy (const char*) const
{
    if (sizeof(char) == SIZE_VAX_CHAR) {
	return True;
    }
    return False;
}

Bool VAXDataConversion::canCopy (const unsigned char*) const
{
    if (sizeof(unsigned char) == SIZE_VAX_UCHAR) {
	return True;
    }
    return False;
}

Bool VAXDataConversion::canCopy (const short*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(short) == SIZE_VAX_SHORT) {
	return True;
    }
#endif
    return False;
}

Bool VAXDataConversion::canCopy (const unsigned short*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(unsigned short) == SIZE_VAX_USHORT) {
	return True;
    }
#endif
    return False;
}

Bool VAXDataConversion::canCopy (const int*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(int) == SIZE_VAX_INT) {
	return True;
    }
#endif
    return False;
}

Bool VAXDataConversion::canCopy (const unsigned int*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(unsigned int) == SIZE_VAX_UINT) {
	return True;
    }
#endif
    return False;
}

Bool VAXDataConversion::canCopy (const Int64*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(Int64) == SIZE_VAX_INT64) {
	return True;
    }
#endif
    return False;
}

Bool VAXDataConversion::canCopy (const uInt64*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(uInt64) == SIZE_VAX_UINT64) {
	return True;
    }
#endif
    return False;
}

Bool VAXDataConversion::canCopy (const float*) const
{
    return False;
}

Bool VAXDataConversion::canCopy (const double*) const
{
    return False;
}


unsigned int VAXDataConversion::externalSize (const char*) const
{
    return SIZE_VAX_CHAR;
}
unsigned int VAXDataConversion::externalSize (const unsigned char*) const
{
    return SIZE_VAX_UCHAR;
}
unsigned int VAXDataConversion::externalSize (const short*) const
{
    return SIZE_VAX_SHORT;
}
unsigned int VAXDataConversion::externalSize (const unsigned short*) const
{
    return SIZE_VAX_USHORT;
}
unsigned int VAXDataConversion::externalSize (const int*) const
{
    return SIZE_VAX_INT;
}
unsigned int VAXDataConversion::externalSize (const unsigned int*) const
{
    return SIZE_VAX_UINT;
}
unsigned int VAXDataConversion::externalSize (const Int64*) const
{
    return SIZE_VAX_INT64;
}
unsigned int VAXDataConversion::externalSize (const uInt64*) const
{
    return SIZE_VAX_UINT64;
}
unsigned int VAXDataConversion::externalSize (const float*) const
{
    return SIZE_VAX_FLOAT;
}
unsigned int VAXDataConversion::externalSize (const double*) const
{
    return SIZE_VAX_DOUBLE;
}

} //# NAMESPACE CASACORE - END

