//# ArrayPartMath.h: mathematics done on an array parts.
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2001,2003
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

#include "ArrayPartMath.h"

namespace casacore {

  void fillBoxedShape (const IPosition& shape, const IPosition& boxSize,
                       IPosition& fullBoxSize, IPosition& resultShape)
  {
    size_t ndim = shape.size();
    // Set missing axes to 1.
    fullBoxSize.resize (ndim);
    fullBoxSize = 1;
    for (size_t i=0; i<std::min(ndim,boxSize.size()); ++i) {
      // Set unspecified axes to full length.
      if (boxSize[i] <= 0  ||  boxSize[i] > shape[i]) {
        fullBoxSize[i] = shape[i];
      } else {
        fullBoxSize[i] = boxSize[i];
      }
    }
    // Determine the output shape.
    resultShape.resize (ndim);
    for (size_t i=0; i<ndim; ++i) {
      resultShape[i] = (shape[i] + fullBoxSize[i] - 1) / fullBoxSize[i];
    }
  }

  bool fillSlidingShape (const IPosition& shape, const IPosition& halfBoxSize,
                         IPosition& boxEnd, IPosition& resultShape)
  {
    size_t ndim = shape.size();
    // Set full box end (is size-1) and resize/fill as needed.
    boxEnd.resize (halfBoxSize.size());
    boxEnd = 2*halfBoxSize;
    if (boxEnd.size() != ndim) {
      size_t sz = boxEnd.size();
      boxEnd.resize (ndim);
      for (size_t i=sz; i<boxEnd.size(); ++i) {
        boxEnd[i] = 0;
      }
    }
    // Determine the output shape. See if anything has to be done.
    bool empty = false;
    resultShape.resize (shape.size());
    for (size_t i=0; i<ndim; ++i) {
      resultShape[i] = shape[i] - boxEnd[i];
      if (resultShape[i] <= 0) {
        resultShape[i] = 0;
        empty = true;
      }
    }
    return empty;
  }

} //# end namespace
