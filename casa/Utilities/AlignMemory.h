//# AlignMemory.h: Class to specify and calculate memory alignment
//# Copyright (C) 2014
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_ALIGNMEMORY_H
#define CASA_ALIGNMEMORY_H

#include <casacore/casa/aips.h>
#include <stddef.h>


namespace casacore { //#Begin casa namespace
  
  class AlignMemory
  {

  public:
    // Default alignment is none.
    explicit AlignMemory (uInt alignment=0)
      : itsAlign(alignment)
    {}

    // Get the alignment.
    uInt alignment() const
      { return itsAlign; }

    // Allocate the given amount of memory with the correct alignment.
    // If alignment < sizeof(void*), malloc will be used, otherwise posix_memalign.
    // The alignment must be a power of 2 for posix_memalign to succeed.
    // It can be freed with the normal free.
    void* alloc (size_t size) const;

  private:
    uInt itsAlign;
  };

} //#End casa namespace

#endif
