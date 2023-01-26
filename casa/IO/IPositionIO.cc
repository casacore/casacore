//# IPosition2.cc: A vector of integers, used to index into arrays (for Array<int>)
//# Copyright (C) 1993,1994,1995,1996,1999,2001
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

//# This source file is not needed if you aren't interested in converting
//# to and from Array<int>, i.e. if you don't want IPosition's to depend
//# on arrays.

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>

#include <casacore/casa/IO/AipsIO.h>

#include <cassert>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

AipsIO& operator<< (AipsIO& aio, const IPosition& ip)
{
  bool use32 = true;
  if (sizeof(ssize_t) > 4) {
    for (size_t i=0; i<ip.nelements(); ++i) {
      if (ip[i] > 2147483647) {
        use32 = false;
        break;
      }
    }
  }
  if (use32) {
    // Write values as int.
    aio.putstart("IPosition", 1);
    aio << (uint32_t) ip.nelements();
    for (size_t i=0; i<ip.nelements(); ++i) {
      aio << int(ip[i]);
    }
  } else {
    // Write values as long long.
    aio.putstart("IPosition", 2);
    aio << (uint32_t) ip.nelements();
    for (size_t i=0; i<ip.nelements(); ++i) {
      aio << (long long) (ip[i]);
    }
  }
  aio.putend();
  return aio;
}

// <thrown>
//    <item> ArrayError
// </thrown>
AipsIO& operator>> (AipsIO& aio, IPosition& ip)
{
  int vers = aio.getstart("IPosition");
  uint32_t nel;
  aio >> nel;
  ip.resize (nel, false);
  if (vers == 1) {
    int v;
    for (size_t i=0; i<nel; ++i) {
      aio >> v;
      ip[i] = v;
    }
  } else if (vers == 2) {
    long long v;
    if (sizeof(ssize_t) <= 4) {
      throw ArrayError ("AipsIO& operator>>(AipsIO& aio, IPosition& ip) - "
                       "cannot read back in an ssize_t of 4 bytes");
    }
    for (size_t i=0; i<nel; ++i) {
      aio >> v;
      ip[i] = v;
    }
  } else {
    throw(ArrayError("AipsIO& operator>>(AipsIO& aio, IPosition& ip) - "
                    "version on disk and in class do not match"));
  }
  aio.getend();
  assert (ip.ok());
  return aio;
}

} //# NAMESPACE CASACORE - END
