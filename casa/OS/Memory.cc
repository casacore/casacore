//# Memory.cc: Memory related information and utilities.
//# Copyright (C) 1997,1999,2001
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

//# MH 97/11/24 Stop attempt to use mallinfo from stdlib.h for HPUX
#include <casacore/casa/aips.h>

#if defined(__hpux__) && !defined(AIPS_NO_LEA_MALLOC)
#define _STRUCT_MALLINFO
#endif

#include <casacore/casa/OS/Memory.h>
#include <casacore/casa/OS/malloc.h>

#if defined(AIPS_NO_LEA_MALLOC)
#if defined(AIPS_DARWIN) || defined(AIPS_CRAY_PGI)
#include <sys/time.h>
#include <sys/resource.h>
#if defined(AIPS_CRAY_PGI)
#include <unistd.h>
#endif
#else
#include <malloc.h>
#endif
#endif

namespace casacore  { //#Begin namespace casa

size_t Memory::allocatedMemoryInBytes()
{
    size_t total = 0;

#if defined(AIPS_DARWIN) || defined(AIPS_CRAY_PGI)
// Use getrusage to get the RSS
   struct rusage rus;
   getrusage(0, &rus);
   total = rus.ru_maxrss;
#else

   // Ger van Diepen   25-May-2004
   // For IntelCC (with -cxxlib-gcc) mallinfo hangs if called before any
   // malloc is done. So do a new to prevent that from happening.
   // Ger van Diepen   6-Oct-2004
   // Hang also occurs with gcc on Linux. So always do a new.
   static char* ptr = 0;
   if (ptr == 0) {
     char* ptr = new char[4];
     delete [] ptr;
   }

   struct mallinfo m = mallinfo();
   total = m.hblkhd + m.usmblks + m.uordblks;

#endif
   return total;
}

size_t Memory::assignedMemoryInBytes()
{
    size_t total = 0;
#if defined(AIPS_DARWIN) || defined(AIPS_CRAY_PGI)
// Use getrusage to get the other memory segments
   struct rusage rus;
   getrusage(0, &rus);
   total = rus.ru_idrss + rus.ru_isrss;
#else

    struct mallinfo m = mallinfo();
    total = m.arena + m.hblkhd;
#endif

    return total;
}

void Memory::releaseMemory()
{
#if defined(AIPS_RELEASEMEM)
    AIPS_RELEASEMEM;
#elif !defined(AIPS_NO_LEA_MALLOC)
    malloc_trim(0);
#endif
}

   //setMemoryOptions uses compiler defines to set the
   // memory options if needed.  It is intended to be
   // only called at the start of a program.  Use 
   // setMemoryOption to tweak the memory options elsewhere.
void Memory::setMemoryOptions(){
   #ifdef AIPS_MALLOC_M_MXFAST
      mallopt(M_MXFAST, AIPS_MALLOC_M_MXFAST);
   #endif
   #ifdef AIPS_MALLOC_M_NLBLKS
      mallopt(M_NLBLKS, AIPS_MALLOC_M_NLBLKS);
   #endif
   #ifdef AIPS_MALLOC_M_GRAIN
      mallopt(M_GRAIN, AIPS_MALLOC_M_GRAIN);
   #endif
   #ifdef AIPS_MALLOC_M_KEEP
      mallopt(M_KEEP, 1);
   #endif
   #ifdef AIPS_MALLOC_M_DEBUG
      mallopt(M_DEBUG, 1);
   #endif
     // Following options are from the SGI mallopt
   #ifdef AIPS_MALLOC_M_BLKSZ
      mallopt(M_BLKSZ, AIPS_MALLOC_M_BLKSZ);
   #endif
   #ifdef AIPS_MALLOC_M_MXCHK
      mallopt(M_MXCHK, AIPS_MALLOC_M_MXCHK);
   #endif
   #ifdef AIPS_MALLOC_M_FREEHD
      mallopt(M_FREEHD, 1);
   #endif
   #ifdef AIPS_MALLOC_M_CLRONFREE
      mallopt(M_CLRONFREE, AIPS_MALLOC_M_CLRONFREE);
   #endif
}

#if defined(AIPS_DARWIN) || defined(AIPS_CRAY_PGI)
int Memory::setMemoryOption(int, int) {
   return 0;
#else
int Memory::setMemoryOption(int cmd, int value) {
   return(mallopt(cmd, value));
#endif
}

} //#End namespace casacore
