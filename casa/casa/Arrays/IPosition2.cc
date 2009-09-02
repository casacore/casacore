//# IPosition2.cc: A vector of integers, used to index into arrays (for Array<Int>)
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
//#
//# $Id$

//# This source file is not needed if you aren't interested in converting
//# to and from Array<Int>, i.e. if you don't want IPosition's to depend
//# on arrays.

#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Vector.h>
#include <casa/IO/AipsIO.h>
#include <casa/Logging/LogIO.h>
#include <casa/Utilities/Copy.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>


namespace casa { //# NAMESPACE CASA - BEGIN

IPosition::IPosition (const Array<Int> &other)
: size_p (other.nelements()),
  data_p (0)
{
    if (size_p == 0) {
	return;        // Be slightly loose about conformance checking
    }
    if (other.ndim() != 1) {
	throw(AipsError("IPosition::IPosition(const Array<Int> &other) - "
			"other is not one-dimensional"));
    }
    allocateBuffer();
    Bool del;
    const Int *storage = other.getStorage(del);
    for (uInt i=0; i<size_p; ++i) {
      data_p[i] = storage[i];
    }
    other.freeStorage (storage, del);
    DebugAssert(ok(), AipsError);
}

Vector<Int> IPosition::asVector() const
{
    DebugAssert(ok(), AipsError);
    // Make an array which is the correct size.
    Vector<Int> retval(nelements());
    for (uInt i=0; i<nelements(); i++) {
        AlwaysAssert (data_p[i] <= 2147483647, AipsError);
	retval[i] = data_p[i];
    }
    return retval;
}

LogIO& operator<< (LogIO& os, const IPosition& ip)
{
    os.output() << ip;
    return os;
}

AipsIO& operator<< (AipsIO& aio, const IPosition& ip)
{
  Bool use32 = True;
  if (sizeof(ssize_t) > 4) {
    for (uInt i=0; i<ip.size_p; ++i) {
      if (ip[i] >= 32768U*65536U) {
        use32 = False;
        break;
      }
    }
  }
  if (use32) {
    // Write values as Int.
    aio.putstart("IPosition", 1);
    aio << ip.size_p;
    for (uInt i=0; i<ip.size_p; ++i) {
      aio << Int(ip[i]);
    }
  } else {
    // Write values as Int64.
    aio.putstart("IPosition", 2);
    aio << ip.size_p;
    for (uInt i=0; i<ip.size_p; ++i) {
      aio << Int64(ip[i]);
    }
  }
  aio.putend();
  return aio;
}

// <thrown>
//    <item> AipsError
// </thrown>
AipsIO& operator>> (AipsIO& aio, IPosition& ip)
{
  Int vers = aio.getstart("IPosition");
  uInt nel;
  aio >> nel;
  ip.resize (nel, False);
  if (vers == 1) {
    Int v;
    for (uInt i=0; i<nel; ++i) {
      aio >> v;
      ip[i] = v;
    }
  } else if (vers == 2) {
    Int64 v;
    if (sizeof(ssize_t) <= 4) {
      throw AipsError ("AipsIO& operator>>(AipsIO& aio, IPosition& ip) - "
                       "cannot read back in an ssize_t of 4 bytes");
    }
    for (uInt i=0; i<nel; ++i) {
      aio >> v;
      ip[i] = v;
    }
  } else {
    throw(AipsError("AipsIO& operator>>(AipsIO& aio, IPosition& ip) - "
                    "version on disk and in class do not match"));
  }
  aio.getend();
  DebugAssert (ip.ok(), AipsError);
  return aio;
}

} //# NAMESPACE CASA - END
