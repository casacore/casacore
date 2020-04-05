//# ArrayUtil.cc: Utility functions for arrays (templated)
//# Copyright (C) 1995,2001
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

#ifndef CASA_ARRAYUTIL_2_TCC
#define CASA_ARRAYUTIL_2_TCC

#include "ArrayUtil.h"
#include "ArrayError.h"

#include <algorithm>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
Array<T> concatenateArray (const Array<T>& left, const Array<T>& right)
{
    if (left.nelements() == 0) {
	return right.copy();
    }
    if (right.nelements() == 0) {
	return left.copy();
    }
    IPosition shape = right.shape();
    IPosition leftShape = left.shape();
    std::size_t ndim = shape.nelements();
    if (! shape.isEqual (leftShape, ndim-1)) {
	throw (ArrayConformanceError ("concatenateArray(left,right)"));
    }
    shape(ndim-1) += leftShape(ndim-1);
    Array<T> result (shape);
    IPosition start(ndim, 0);
    result (start, leftShape-1).assign_conforming( left );
    start(ndim-1) = leftShape(ndim-1);
    result (start, shape-1).assign_conforming( right );
    return result;
}


template<class T>
Array<T> reorderArray (const Array<T>& array,
		       const IPosition& newAxisOrder,
		       bool alwaysCopy)
{
  const IPosition& shape = array.shape();
  IPosition newShape, incr;
  size_t contAxes = reorderArrayHelper (newShape, incr,
				      shape, newAxisOrder);
  // If not reordered, we can simply return the array (or a copy if needed).
  size_t ndim = shape.nelements();
  if (contAxes == ndim) {
    if (alwaysCopy) {
      return array.copy();
    }
    return array;
  }
  Array<T> result(newShape);
  bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out the nr of contiguous elements.
  size_t nrcont = 1;
  if (contAxes == 0) {
    contAxes = 1;
  } else {
    for (size_t i=0; i<contAxes; i++) {
      nrcont *= shape(i);
    }
  }
  size_t incr0 = incr(0);
  size_t n0 = shape(0);
  // Loop through all data and copy as needed.
  IPosition pos(ndim, 0);
  while (true) {
    if (nrcont > 1) {
      std::copy_n(data, nrcont, res);
      data += nrcont;
      res += nrcont;
    } else {
      for (size_t i=0; i<n0; i++) {
	*res = *data++;
	res += incr0;
      }
    }
    size_t ax;
    for (ax=contAxes; ax<ndim; ax++) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}



template<class T>
Array<T> reverseArray (const Array<T>& array, size_t axis, bool alwaysCopy)
{
  const IPosition& shape = array.shape();
  if (axis >= shape.size()) {
    throw ArrayError(
                    std::string(__FUNCTION__)
                    + ": axis number is higher than number of axes in the array"
                    );
  }
  bool nothingToDo = shape[axis] == 1;
  if (nothingToDo) {
    if (alwaysCopy) {
      return array.copy();
    }
    return array;
  }
  bool deletein, deleteout;
  const T *indata = array.getStorage(deletein);
  Array<T> result(shape);
  T *outdata = result.getStorage(deleteout);
  size_t outerProduct = 1;
  size_t innerProduct = 1;
  for (size_t i=0; i<shape.size(); i++) {
    if (i<axis) {
      innerProduct *= shape[i];
    } else if (i>axis) {
      outerProduct *= shape[i];
    }
  }
  for (size_t j=0; j<outerProduct; ++j) {
    size_t idx = shape[axis]*innerProduct*j;
    for (int k=0; k<shape[axis]; ++k) {
      std::copy_n (
        indata + idx + innerProduct*k,
        innerProduct,
        outdata + idx + innerProduct*(shape[axis] - k - 1));
    }
  }
  array.freeStorage(indata, deletein);
  result.putStorage(outdata, deleteout);
  return result;
}

template<class T>
Array<T> reverseArray (const Array<T>& array, const IPosition& reversedAxes,
                       bool alwaysCopy)
{
  const IPosition& shape = array.shape();
  bool nothingToDo = true;
  for (size_t i=0; i<reversedAxes.size(); i++) {
    if (reversedAxes[i] >= int(shape.size())) {
      throw ArrayError(std::string(__FUNCTION__)
                      + ": axis number "
                      + std::to_string(reversedAxes[i])
                      + " is higher than number of axes in the array"
                      );
    }
    if (shape[reversedAxes[i]] > 1) {
      nothingToDo = false;
      break;
    }
  }
  if (nothingToDo) {
    if (alwaysCopy) {
      return array.copy();
    }
    return array;
  }
  Array<T> result = array.copy();
  for (size_t i=0; i<reversedAxes.size(); ++i) {
    result = reverseArray(result, reversedAxes[i], alwaysCopy);
  }
  return result;
}

} //# NAMESPACE CASACORE - END

#endif
