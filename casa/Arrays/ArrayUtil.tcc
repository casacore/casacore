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

#ifndef CASA_ARRAYUTIL_TCC
#define CASA_ARRAYUTIL_TCC

#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Utilities/Copy.h>


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
    uInt ndim = shape.nelements();
    if (! shape.isEqual (leftShape, ndim-1)) {
	throw (ArrayConformanceError ("concatenateArray(left,right)"));
    }
    shape(ndim-1) += leftShape(ndim-1);
    Array<T> result (shape);
    IPosition start(ndim, 0);
    result (start, leftShape-1) = left;
    start(ndim-1) = leftShape(ndim-1);
    result (start, shape-1) = right;
    return result;
}


template<class T>
Array<T> reorderArray (const Array<T>& array,
		       const IPosition& newAxisOrder,
		       Bool alwaysCopy)
{
  const IPosition& shape = array.shape();
  IPosition newShape, incr;
  uInt contAxes = reorderArrayHelper (newShape, incr,
				      shape, newAxisOrder);
  // If not reordered, we can simply return the array (or a copy if needed).
  uInt ndim = shape.nelements();
  if (contAxes == ndim) {
    if (alwaysCopy) {
      return array.copy();
    }
    return array;
  }
  Array<T> result(newShape);
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out the nr of contiguous elements.
  uInt nrcont = 1;
  if (contAxes == 0) {
    contAxes = 1;
  } else {
    for (uInt i=0; i<contAxes; i++) {
      nrcont *= shape(i);
    }
  }
  uInt incr0 = incr(0);
  uInt n0 = shape(0);
  // Loop through all data and copy as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (nrcont > 1) {
      objcopy (res, data, nrcont);
      data += nrcont;
      res += nrcont;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res = *data++;
	res += incr0;
      }
    }
    uInt ax;
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
Array<T> reverseArray (const Array<T>& array, uInt axis, Bool alwaysCopy)
{
  const IPosition& shape = array.shape();
  if (axis >= shape.size()) {
    throw AipsError(
                    String(__FUNCTION__)
                    + ": axis number is higher than number of axes in the array"
                    );
  }
  Bool nothingToDo = shape[axis] == 1;
  if (nothingToDo) {
    if (alwaysCopy) {
      return array.copy();
    }
    return array;
  }
  Bool deletein, deleteout;
  const T *indata = array.getStorage(deletein);
  Array<T> result(shape);
  T *outdata = result.getStorage(deleteout);
  uInt outerProduct = 1;
  uInt innerProduct = 1;
  for (uInt i=0; i<shape.size(); i++) {
    if (i<axis) {
      innerProduct *= shape[i];
    } else if (i>axis) {
      outerProduct *= shape[i];
    }
  }
  for (uInt j=0; j<outerProduct; ++j) {
    uInt idx = shape[axis]*innerProduct*j;
    for (Int k=0; k<shape[axis]; ++k) {
      objcopy (outdata + idx + innerProduct*(shape[axis] - k - 1),
               indata + idx + innerProduct*k,
               innerProduct);
    }
  }
  array.freeStorage(indata, deletein);
  result.putStorage(outdata, deleteout);
  return result;
}

template<class T>
Array<T> reverseArray (const Array<T>& array, const IPosition& reversedAxes,
                       Bool alwaysCopy)
{
  const IPosition& shape = array.shape();
  Bool nothingToDo = True;
  for (uInt i=0; i<reversedAxes.size(); i++) {
    if (reversedAxes[i] >= Int(shape.size())) {
      throw AipsError(String(__FUNCTION__)
                      + ": axis number "
                      + String::toString(reversedAxes[i])
                      + " is higher than number of axes in the array"
                      );
    }
    if (shape[reversedAxes[i]] > 1) {
      nothingToDo = False;
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
  for (uInt i=0; i<reversedAxes.size(); ++i) {
    result = reverseArray(result, reversedAxes[i], alwaysCopy);
  }
  return result;
}

} //# NAMESPACE CASACORE - END

#endif
