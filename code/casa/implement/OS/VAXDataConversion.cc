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


#include <aips/OS/VAXDataConversion.h>
#include <aips/OS/VAXConversion.h>


VAXDataConversion::~VAXDataConversion()
{}


unsigned int VAXDataConversion::toLocal (char&           to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_CHAR;
}
unsigned int VAXDataConversion::toLocal (unsigned char&  to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_UCHAR;
}
unsigned int VAXDataConversion::toLocal (short&          to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_SHORT;
}
unsigned int VAXDataConversion::toLocal (unsigned short& to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_USHORT;
}
unsigned int VAXDataConversion::toLocal (int&            to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_INT;
}
unsigned int VAXDataConversion::toLocal (unsigned int&   to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_UINT;
}
unsigned int VAXDataConversion::toLocal (long&           to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_LONG;
}
unsigned int VAXDataConversion::toLocal (unsigned long&  to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_ULONG;
}
unsigned int VAXDataConversion::toLocal (float&          to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_FLOAT;
}
unsigned int VAXDataConversion::toLocal (double&         to,
					 const void* from) const
{
    VAXConversion::toLocal (to, from);
    return SIZE_VAX_DOUBLE;
}


unsigned int VAXDataConversion::toLocal (char*           to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_CHAR;
}
unsigned int VAXDataConversion::toLocal (unsigned char*  to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_UCHAR;
}
unsigned int VAXDataConversion::toLocal (short*          to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_SHORT;
}
unsigned int VAXDataConversion::toLocal (unsigned short* to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_USHORT;
}
unsigned int VAXDataConversion::toLocal (int*            to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_INT;
}
unsigned int VAXDataConversion::toLocal (unsigned int*   to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_UINT;
}
unsigned int VAXDataConversion::toLocal (long*           to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_LONG;
}
unsigned int VAXDataConversion::toLocal (unsigned long*  to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_ULONG;
}
unsigned int VAXDataConversion::toLocal (float*          to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_FLOAT;
}
unsigned int VAXDataConversion::toLocal (double*         to, const void* from,
					 unsigned int nr) const
{
    VAXConversion::toLocal (to, from, nr);
    return nr*SIZE_VAX_DOUBLE;
}

unsigned int VAXDataConversion::fromLocal (void* to, char           from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_CHAR;
}
unsigned int VAXDataConversion::fromLocal (void* to, unsigned char  from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_UCHAR;
}
unsigned int VAXDataConversion::fromLocal (void* to, short          from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_SHORT;
}
unsigned int VAXDataConversion::fromLocal (void* to, unsigned short from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_USHORT;
}
unsigned int VAXDataConversion::fromLocal (void* to, int            from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_INT;
}
unsigned int VAXDataConversion::fromLocal (void* to, unsigned int   from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_UINT;
}
unsigned int VAXDataConversion::fromLocal (void* to, long           from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_LONG;
}
unsigned int VAXDataConversion::fromLocal (void* to, unsigned long  from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_ULONG;
}
unsigned int VAXDataConversion::fromLocal (void* to, float          from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_FLOAT;
}
unsigned int VAXDataConversion::fromLocal (void* to, double         from) const
{
    VAXConversion::fromLocal (to, from);
    return SIZE_VAX_DOUBLE;
}

unsigned int VAXDataConversion::fromLocal (void* to, const char* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_CHAR;
}
unsigned int VAXDataConversion::fromLocal (void* to, const unsigned char* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_UCHAR;
}
unsigned int VAXDataConversion::fromLocal (void* to, const short* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_SHORT;
}
unsigned int VAXDataConversion::fromLocal (void* to,
					   const unsigned short* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_USHORT;
}
unsigned int VAXDataConversion::fromLocal (void* to, const int* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_INT;
}
unsigned int VAXDataConversion::fromLocal (void* to, const unsigned int* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_UINT;
}
unsigned int VAXDataConversion::fromLocal (void* to, const long* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_LONG;
}
unsigned int VAXDataConversion::fromLocal (void* to, const unsigned long* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_ULONG;
}
unsigned int VAXDataConversion::fromLocal (void* to, const float* from,
					   unsigned int nr) const
{
    VAXConversion::fromLocal (to, from, nr);
    return nr*SIZE_VAX_FLOAT;
}
unsigned int VAXDataConversion::fromLocal (void* to, const double* from,
					   unsigned int nr) const
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
    if (sizeof(int) == SIZE_VAX_UINT) {
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

Bool VAXDataConversion::canCopy (const long*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(long) == SIZE_VAX_ULONG) {
	return True;
    }
#endif
    return False;
}

Bool VAXDataConversion::canCopy (const unsigned long*) const
{
#if defined(AIPS_LITTLE_ENDIAN)
    if (sizeof(unsigned long) == SIZE_VAX_ULONG) {
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
unsigned int VAXDataConversion::externalSize (const long*) const
{
    return SIZE_VAX_LONG;
}
unsigned int VAXDataConversion::externalSize (const unsigned long*) const
{
    return SIZE_VAX_ULONG;
}
unsigned int VAXDataConversion::externalSize (const float*) const
{
    return SIZE_VAX_FLOAT;
}
unsigned int VAXDataConversion::externalSize (const double*) const
{
    return SIZE_VAX_DOUBLE;
}
