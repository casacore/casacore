//# Slice.c: Define a (start,length,increment) along an axis
//# Copyright (C) 2008
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

#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  IPosition Slice::checkSlices (Vector<Vector<Slice> >& slices, Slicer& first,
                                const IPosition& shape)
  {
    // In principle the shape defines the dimensionality, but it can be empty.
    // In that case use the slices definition.
    uInt ndim = shape.size();
    if (ndim == 0) {
      ndim = slices.size();
    }
    if (slices.size() > ndim) {
      throw AipsError ("Slice::checkSlices - "
                       "slices size exceeds dimensionality");
    }
    // Extend slices if needed.
    slices.resize (ndim, True);
    // Initialize the result shape and first Slicer start,end,incr.
    IPosition result(ndim, 0);
    IPosition start(ndim);
    IPosition length(ndim);
    IPosition incr(ndim);
    for (uInt i=0; i<ndim; ++i) {
      Vector<Slice>& sliceVec = slices[i];
      if (sliceVec.size() == 0) {
        sliceVec.resize (1);
        sliceVec[0] = Slice(0, shape[i]);
      }
      // Get total slices length if slices are given.
      // Check if slice does not exceed shape.
      for (uInt j=0; j<sliceVec.size(); ++j) {
        if (sliceVec[j].end() >= size_t(shape[i])) {
          throw AipsError("Slice::checkSlices - "
                          "slice exceeds array shape");
        }
        result[i] += sliceVec[j].length();
      }
      start[i]  = sliceVec[0].start();
      length[i] = sliceVec[0].length();
      incr[i]   = sliceVec[0].inc();
    }
    first = Slicer(start, length, incr);
    return result;
  }

} //# NAMESPACE CASACORE - END
