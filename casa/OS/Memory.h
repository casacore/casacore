//# Memory.h: Memory related information and utilities.
//# Copyright (C) 1997,2001
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

#ifndef CASA_MEMORY_H
#define CASA_MEMORY_H

#include <casacore/casa/aips.h>
//# The following is used to get size_t.
#include <casacore/casa/stdlib.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>Memory related information and utilities.</summary>

// use visibility=export>

// <reviewed reviewer="rmarson" date="1997/11/12" tests="tMemory" demos="">
// </reviewed>

// <prerequisite>
//   <li> General knowledge of C/C++ memory allocation issues.
// </prerequisite>
//
// <synopsis>
// This class should generally not be used by general application programmers.
// Instead you should use the memory information available in the 
// <linkto class=AppInfo>AppInfo</linkto> class.
//
// This class reports on the dynamic ("heap") memory used by this process, and
// it can attempt to release some of that memory back to the operating
// system. The class reports on <src>allocated</src> memory, which is memory
// that the process has actually requested, typically via <src>new</src>, but
// also via <src>malloc</src>. The number might be somewhat larger than actually
// requested by the programmer to account for overhead and alignment
// considerations. The class also reports <src>assigned</src> memory, which is
// the total memory that the process has been given, not all of which has been
// allocated by the programmer. Typically this results from memory which has
// been <src>delete</src>d by the programmer, but has not been released to the
// OS on the assumption that it might be needed again. (Getting and releasing
// memory to the OS can be expensive).
//
// At present, the values for allocated and assigned memory are obtained via the
// <src>mallinfo()</src> call, usually defined in <src>malloc.h</src>. This call
// seems to be adequately portable for Unix. Another alternative would be to
// replace global operators <src>new</src> and <src>delete</src> for systems
// that do not have <src>mallinfo()</src>.
//
// The member function <src>releaseMemory()</src> on some system will attempt
// to return assigned but unallocated memory to the OS. If the compilation
// system cannot automatically determine how to do this (at present it only
// recognizes Linux systems), you can you this function by setting the
// preprocessor symbol <src>AIPS_RELEASEMEM</src> to a C++ statement which
// will release memory to the OS. For example, if you are using GNU malloc
// you could set it to <src>malloc_trim(0)</src>. Note that
// <src>releaseMemory()</src> might be a no-op on many systems, and that
// calling it might be expensive so it should not be called in tight-loops.
//
// Note that this class does not use any Casacore facilities and does not cause
// any Casacore code to be linked in to your executable.
// </synopsis>
//
// <example>
// We could attempt to return memory to the OS when we are wasting a lot
// of memory as follows. 
// <srcBlock>
// if (Memory::assignedMemoryInBytes() - Memory::allocatedMemoryInBytes() >
//     1024*1024) {
//     Memory::releaseMemory(); // Attempt to release if more than 1M "wasted"
// }
// </srcBlock>
// </example>
//
// <motivation>
// At run time we need to be able to make decisions about whether we should
// keep things in memory or about whether we should do I/O (e.g. use an
// Array or a PagedArray).
// </motivation>
//
// <todo asof="1997/11/10">
//   <li> We might some day want to put actual allocation/deallocation
//        functions in here if the mallinfo() interface turns out to not
//        be portable.
// </todo>

class Memory
{
public:
    // How much memory has been allocated by the programmer (via either
    // <src>new</src> or <src>malloc</src>. This can include some extra
    // overhead bytes, e.g. for alignment reasons.
    static size_t allocatedMemoryInBytes();
    // How much memory in in the memory "pool" of this process. This includes
    // not only the allocated memory, but some memory which has been
    // <src>delete</src>d but not returned to the OS on the assumption that
    // it might be needed again.
    static size_t assignedMemoryInBytes();
    // Attempt to release memory which has been assigned but not allocated.
    // On many systems this will be a no-op, and even on systems in which it
    // does something the amount of reclaimed memory cannot be specified.
    // Since this function may be somewhat expensive to call it should not
    // be called too often.
    static void releaseMemory();

    // setMemoryOptions and setMemoryOption are typically front ends for mallopt
    // which lets the user control some memory allocation parameters.  setMemoryOptions
    // is intended to be called only once at the start of a program while setMemoryOption
    // could be called where desired (but see mallopt man page for possible side effects).
    // Note: these two functions were added to address in a general way a memory
    // fragmentation problem encountered on by the MIPSpro C++ compiler on the SGI.
    static void setMemoryOptions();
    static int  setMemoryOption(int, int);
};


} //# NAMESPACE CASACORE - END

#endif


