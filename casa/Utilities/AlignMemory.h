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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: CountedPtr.h 21469 2014-08-12 11:25:55Z gervandiepen $

#ifndef CASA_ALIGNMEMORY_H
#define CASA_ALIGNMEMORY_H

#include <casacore/casa/aips.h>
#include <stddef.h>


namespace casacore { //#Begin casa namespace

// <summary>Referenced counted pointer for constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <etymology>
// This class is <em>Counted</em> because it is reference counted.
// </etymology>

// <synopsis>
// This class implements a reference counting mechanism. It
// allows <src>CountedPtr</src>s to be passed around freely,
// incrementing or decrementing the reference count as needed when one
// <src>CountedPtr</src> is assigned to another. When the
// reference count reaches zero the internal storage is deleted by
// default, but this behavior can be overridden.
//
// Internally the class uses std::shared_ptr to be thread-safe. Note that
// tr1 is used if the compiler does not support C++11 yet.
// </synopsis>

// <motivation>
// Reference counting
// </motivation>

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
