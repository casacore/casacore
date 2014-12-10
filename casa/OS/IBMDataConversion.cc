//# IBMDataConversion.cc: A class with virtual functions to convert IBM format
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


#include <casacore/casa/OS/IBMDataConversion.h>
#include <casacore/casa/OS/IBMConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

IBMDataConversion::~IBMDataConversion()
{}


size_t IBMDataConversion::toLocal (char&           to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_CHAR;
}
size_t IBMDataConversion::toLocal (unsigned char&  to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_UCHAR;
}
size_t IBMDataConversion::toLocal (short&          to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_SHORT;
}
size_t IBMDataConversion::toLocal (unsigned short& to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_USHORT;
}
size_t IBMDataConversion::toLocal (int&            to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_INT;
}
size_t IBMDataConversion::toLocal (unsigned int&   to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_UINT;
}
size_t IBMDataConversion::toLocal (Int64&          to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_INT64;
}
size_t IBMDataConversion::toLocal (uInt64&         to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_UINT64;
}
size_t IBMDataConversion::toLocal (float&          to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_FLOAT;
}
size_t IBMDataConversion::toLocal (double&         to,
					 const void* from) const
{
    IBMConversion::toLocal (to, from);
    return SIZE_IBM_DOUBLE;
}


size_t IBMDataConversion::toLocal (char*           to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_CHAR;
}
size_t IBMDataConversion::toLocal (unsigned char*  to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_UCHAR;
}
size_t IBMDataConversion::toLocal (short*          to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_SHORT;
}
size_t IBMDataConversion::toLocal (unsigned short* to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_USHORT;
}
size_t IBMDataConversion::toLocal (int*            to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_INT;
}
size_t IBMDataConversion::toLocal (unsigned int*   to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_UINT;
}
size_t IBMDataConversion::toLocal (Int64*          to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_INT64;
}
size_t IBMDataConversion::toLocal (uInt64*         to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_UINT64;
}
size_t IBMDataConversion::toLocal (float*          to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_FLOAT;
}
size_t IBMDataConversion::toLocal (double*         to, const void* from,
					 size_t nr) const
{
    IBMConversion::toLocal (to, from, nr);
    return nr*SIZE_IBM_DOUBLE;
}

size_t IBMDataConversion::fromLocal (void* to, char           from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_CHAR;
}
size_t IBMDataConversion::fromLocal (void* to, unsigned char  from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_UCHAR;
}
size_t IBMDataConversion::fromLocal (void* to, short          from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_SHORT;
}
size_t IBMDataConversion::fromLocal (void* to, unsigned short from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_USHORT;
}
size_t IBMDataConversion::fromLocal (void* to, int            from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_INT;
}
size_t IBMDataConversion::fromLocal (void* to, unsigned int   from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_UINT;
}
size_t IBMDataConversion::fromLocal (void* to, Int64          from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_INT64;
}
size_t IBMDataConversion::fromLocal (void* to, uInt64         from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_UINT64;
}
size_t IBMDataConversion::fromLocal (void* to, float          from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_FLOAT;
}
size_t IBMDataConversion::fromLocal (void* to, double         from) const
{
    IBMConversion::fromLocal (to, from);
    return SIZE_IBM_DOUBLE;
}

size_t IBMDataConversion::fromLocal (void* to, const char* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_CHAR;
}
size_t IBMDataConversion::fromLocal (void* to, const unsigned char* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_UCHAR;
}
size_t IBMDataConversion::fromLocal (void* to, const short* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_SHORT;
}
size_t IBMDataConversion::fromLocal (void* to,
					   const unsigned short* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_USHORT;
}
size_t IBMDataConversion::fromLocal (void* to, const int* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_INT;
}
size_t IBMDataConversion::fromLocal (void* to, const unsigned int* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_UINT;
}
size_t IBMDataConversion::fromLocal (void* to, const Int64* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_INT64;
}
size_t IBMDataConversion::fromLocal (void* to, const uInt64* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_UINT64;
}
size_t IBMDataConversion::fromLocal (void* to, const float* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_FLOAT;
}
size_t IBMDataConversion::fromLocal (void* to, const double* from,
					   size_t nr) const
{
    IBMConversion::fromLocal (to, from, nr);
    return nr*SIZE_IBM_DOUBLE;
}


Bool IBMDataConversion::canCopy (const char*) const
{
    return False;
}

Bool IBMDataConversion::canCopy (const unsigned char*) const
{
    if (sizeof(unsigned char) == SIZE_IBM_UCHAR) {
	return True;
    }
    return False;
}

Bool IBMDataConversion::canCopy (const short*) const
{
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(short) == SIZE_IBM_SHORT) {
	return True;
    }
#endif
    return False;
}

Bool IBMDataConversion::canCopy (const unsigned short*) const
{
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(unsigned short) == SIZE_IBM_USHORT) {
	return True;
    }
#endif
    return False;
}

Bool IBMDataConversion::canCopy (const int*) const
{
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(int) == SIZE_IBM_INT) {
	return True;
    }
#endif
    return False;
}

Bool IBMDataConversion::canCopy (const unsigned int*) const
{
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(unsigned int) == SIZE_IBM_UINT) {
	return True;
    }
#endif
    return False;
}

Bool IBMDataConversion::canCopy (const Int64*) const
{
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(Int64) == SIZE_IBM_INT64) {
	return True;
    }
#endif
    return False;
}

Bool IBMDataConversion::canCopy (const uInt64*) const
{
#if !defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(uInt64) == SIZE_IBM_UINT64) {
	return True;
    }
#endif
    return False;
}

Bool IBMDataConversion::canCopy (const float*) const
{
    return False;
}

Bool IBMDataConversion::canCopy (const double*) const
{
    return False;
}


unsigned int IBMDataConversion::externalSize (const char*) const
{
    return SIZE_IBM_CHAR;
}
unsigned int IBMDataConversion::externalSize (const unsigned char*) const
{
    return SIZE_IBM_UCHAR;
}
unsigned int IBMDataConversion::externalSize (const short*) const
{
    return SIZE_IBM_SHORT;
}
unsigned int IBMDataConversion::externalSize (const unsigned short*) const
{
    return SIZE_IBM_USHORT;
}
unsigned int IBMDataConversion::externalSize (const int*) const
{
    return SIZE_IBM_INT;
}
unsigned int IBMDataConversion::externalSize (const unsigned int*) const
{
    return SIZE_IBM_UINT;
}
unsigned int IBMDataConversion::externalSize (const Int64*) const
{
    return SIZE_IBM_INT64;
}
unsigned int IBMDataConversion::externalSize (const uInt64*) const
{
    return SIZE_IBM_UINT64;
}
unsigned int IBMDataConversion::externalSize (const float*) const
{
    return SIZE_IBM_FLOAT;
}
unsigned int IBMDataConversion::externalSize (const double*) const
{
    return SIZE_IBM_DOUBLE;
}

} //# NAMESPACE CASACORE - END

