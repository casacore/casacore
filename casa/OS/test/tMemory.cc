//# tMemory.cc: Test tMemory class.
//# Copyright (C) 1997,1998,2001
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

#include <casacore/casa/iostream.h>
#include <casacore/casa/OS/Memory.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/namespace.h>
int main()
{
    size_t base = Memory::allocatedMemoryInBytes();

    // Not much testing is possible.
    char *ptrs[1000];
    size_t alloc = 0;
    uInt i;
    for (i=0; i<sizeof(ptrs)/sizeof(char*); i++) {
	ptrs[i] = new char[100];
	alloc += 100;
    }
    for (i=0; i<sizeof(ptrs)/sizeof(char*)/2; i++) { // Delete 1/2
	delete [] ptrs[i];
	alloc -= 100;
    }

#ifndef AIPS_DARWIN
    AlwaysAssertExit(Memory::allocatedMemoryInBytes()-base >= alloc);
    size_t assigned = Memory::assignedMemoryInBytes(); 
    AlwaysAssertExit( assigned >= Memory::allocatedMemoryInBytes());
    Memory::releaseMemory();
    AlwaysAssertExit(assigned >= Memory::assignedMemoryInBytes());
#endif

    // Add a big allocation
    base = Memory::allocatedMemoryInBytes();
    char *cp = new char[10*1024*1024];
#ifndef AIPS_DARWIN
    AlwaysAssertExit(Memory::allocatedMemoryInBytes()-base > 10*1024*1024);
    assigned = Memory::assignedMemoryInBytes(); 
    AlwaysAssertExit( assigned >= Memory::allocatedMemoryInBytes());
#endif

    // Cleanup
    delete [] cp;
    for (i=sizeof(ptrs)/sizeof(char*)/2; i<sizeof(ptrs)/sizeof(char*); i++) {
	delete [] ptrs[i];
    }

    return 0;
}
