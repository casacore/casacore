//# AlignMemory.cc: Class to specify and calculate memory alignment
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: CountedPtr.h 21469 2014-08-12 11:25:55Z gervandiepen $

#include <casacore/casa/Utilities/AlignMemory.h>
#include <casacore/casa/Exceptions/Error.h>
#include <stdlib.h>

namespace casacore { //#Begin casa namespace

  void* AlignMemory::alloc (size_t size) const
  {
    void* ptr = 0;
    if (size > 0) {
      // posix_memalign alignment must be at least sizeof(void*).
      if (itsAlign >= sizeof(void*)) {
        int sts = posix_memalign (&ptr, itsAlign, size);
        if (sts != 0) {
          throw AllocError("posix_memalign (aligned alloc) failed for " +
                           String::toString(size) + " bytes", size);
        }
      } else {
        ptr = malloc(size);
        if (ptr == 0) {
          throw AllocError("malloc failed for " +
                           String::toString(size) + " bytes", size);
        }
      }
    }
    return ptr;
  }

} //#End casa namespace
