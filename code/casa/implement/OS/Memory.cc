//# Memory.cc: Memory related information and utilities.
//# Copyright (C) 1997
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
//#
//# $Id$

#include <aips/OS/Memory.h>
#include <malloc.h>

size_t Memory::allocatedMemoryInBytes()
{
    size_t total = 0;

#if defined(AIPS_LINUX)
    struct mallinfo m = mallinfo();
    total = m.hblkhd + m.usmblks + m.uordblks;
#endif
    return total;
}

size_t Memory::assignedMemoryInBytes()
{
    size_t total = 0;

#if defined(AIPS_LINUX)
    struct mallinfo m = mallinfo();
    total = m.arena;
#endif
    return total;
}

void Memory::releaseMemory()
{
#if defined(AIPS_RELEASEMEM)
    AIPS_RELEASEMEM;
#elif defined(AIPS_LINUX)
    malloc_trim(0);
#endif
}
