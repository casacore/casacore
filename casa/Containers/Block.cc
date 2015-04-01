//# Block.cc: Simple templated array classes
//# Copyright (C) 1993-1997,2000,2002,2005
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
//# $Id: Block.h 21120 2011-09-01 13:51:56Z gervandiepen $

#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/OS/MemoryTrace.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Initialize static members.
  size_t BlockTrace::itsTraceSize = 0;


  void BlockTrace::setTraceSize (size_t sz)
  {
    itsTraceSize = sz;
    if (sz > 0) {
      MemoryTrace::open();
    }
  }
  void BlockTrace::doTraceAlloc (const void* addr, size_t nelem,
                                 DataType type, size_t sz)
  {
    traceMemoryAlloc (addr, nelem*sz, "Block " << type << ' ' << sz);
  }
  
  void BlockTrace::doTraceFree (const void* addr, size_t nelem,
                                DataType type, size_t sz)
  {
    traceMemoryFree (addr, "Block " << type << ' ' << nelem*sz);
  }

} //# NAMESPACE CASACORE - END
