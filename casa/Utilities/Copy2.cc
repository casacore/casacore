//# Copy2.cc: Non-templated parts (tests/exceptions) for object copies
//# Copyright (C) 2005
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

#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  void objthrowmv1(const void *to, const void *from, const size_t n) {
    if (n > 0 && (!from || !to)) 
      throw(AipsError("objmove(T* to, const T* from, size_t n)"
		      " - illegal argument"));		
  }	
  
  void objthrowmv2(const void *to, const void *from, const size_t n,
		   const size_t toStride, const size_t fromStride) {
    if (n > 0 && (!from || !to || !toStride || !fromStride))
      throw(AipsError("objmove(T* to, const T* from, size_t n, size_t toStride, "
		      "size_t fromStride) - illegal argument"));
  }

  void objthrowcp1(const void *to, const void *from, const size_t n) {
    if (n > 0 && (!from || !to))
      throw(AipsError("objcopy(T* to, const T* from, size_t n)"
		      " - illegal argument"));
  }

  void objthrowcp2(const void *to, const void *from, const size_t n,
		   const size_t toStride, const size_t fromStride) {
    if (n > 0 && (!from || !to || !toStride || !fromStride))
      throw(AipsError("objcopy(T* to, const T* from, size_t n, size_t toStride, "
		      "size_t fromStride) - illegal argument"));
  }

  void objthrowfl1(const void *to, const size_t n) {
    if (n > 0 && !to)
      throw(AipsError("objset(T* to, const T fillValue, size_t n)"
                      " - illegal argument"));
  }

  void objthrowfl2(const void *to, const size_t n,
                   const size_t toStride) {
    if (n > 0 && (!to || !toStride))
      throw(AipsError("objset(T* to, const T fillValue, size_t n, "
                      "size_t toStride) - illegal argument"));
  }

  void objtestmv(size_t &nLeft, size_t &startLeft, size_t &startRight,
		 const void *to, const void *from, const size_t n, 
		 const size_t toStride, const size_t fromStride,
		 const void *toPn, const void *fromPn,
		 const size_t fromMto, const size_t toMfrom) {
    // It's not a simple block move.
    // The to and from interval may overlap, so determine
    // if we have to start moving from the left or right.
    nLeft = n;
    startLeft = 0;
    startRight = n;
    // First test if the to and from intervals are disjoint.
    // If so, we can move everything from the left.
    if (toPn <= from  ||  to >= fromPn);
    else {
      // When the strides are equal, we can also move everything
      // from the left when to starts before from.
      // Otherwise everything has to be moved from the right.
      if (toStride == fromStride) {
	if (to <= from);
	else nLeft = 0;
      } else {
	// Hmm, it's getting more complex.
	// First consider the case toStride > fromStride.
	// When to starts after from, move everything from the right.
	// When to ends before the end of from, move from the left.
	if (toStride > fromStride) {
	  if (to >= from) nLeft = 0;
	  else if (toPn <= fromPn);
	  else {
	    // The intervals overlap in a way that part has to be
	    // moved from the left, part from the right.
	    // Determine the crosspoint.
	    nLeft = fromMto;
	    if (nLeft > n) nLeft = n;
	  }
	} else {
	  // This case is the opposite from the previous one.
	  // However, the first part has to be moved from the right
	  // and the last part from the left.
	  if (from >= to);
	  else if (fromPn <= toPn) nLeft = 0;
	  else {
	    startRight = toMfrom;
	    if (startRight > n) startRight = n;
	    startLeft = startRight;
	    nLeft = n - startRight;
	  }
	}
      }
    }
  }
 
} //# NAMESPACE CASACORE - END
