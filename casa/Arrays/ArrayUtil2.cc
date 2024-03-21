//# ArrayUtil.cc: Utility functions for arrays (non-templated)
//# Copyright (C) 1995,1999,2001
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# The include of Array.h before ArrayUtil.h is needed, otherwise
//# gcc-4 complains (rightfully) about partialFuncHelper not being declared.
//# Because include-chain is:
//#  Array.h, Array.tcc, MaskedArray.h, MaskedArray.tcc, ArrayLogical.h,
//#  ArrayLogical.tcc, ArrayUtil.h

#include "Array.h"
#include "ArrayUtil.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
Vector<std::string> strToVector (const std::string& str, char delim)
{
  if (str.empty()) {
    return Vector<std::string>(0);
  }
  size_t nr = std::count(str.begin(), str.end(), delim);
  Vector<std::string> vec(nr+1);
  size_t st = 0;
  nr = 0;
  size_t i;
  for (i=0; i<str.length(); i++) {
    if (str[i] == delim) {
      vec(nr++) = str.substr(st,i-st);
      st = i+1;
    }
  }
  vec(nr++) = str.substr(st,i-st);
  return vec;
}

Vector<std::string> strToVector (const std::string& string, const std::regex& delim)
{
  std::vector<std::string> stdvec;
  const char* s = string.c_str();
  int sl = string.length();
  if (sl != 0)
  {
    const char* pos = s;
    std::cmatch results;
    bool match = std::regex_search (s, s+sl, results, delim);
    while (match) {
      stdvec.emplace_back(std::string (pos, results[0].first));
      pos = results[0].first + results[0].length();
      match = std::regex_search (pos, s+sl, results, delim);
    }
    stdvec.emplace_back( std::string (pos, s + sl - pos) );
  }
  return Vector<std::string>(stdvec);
}

size_t partialFuncHelper (int& nelemCont,
			IPosition& resultShape, IPosition& incr,
			const IPosition& sourceShape,
			const IPosition& collapseAxes)
{
  int ndim = sourceShape.nelements();
  // Get the remaining axes.
  // It also checks if axes are specified correctly.
  IPosition resultAxes = IPosition::otherAxes (ndim, collapseAxes);
  size_t nres = resultAxes.nelements();
  // Create an array which determines how to increment in the result
  // when incrementing an axis of the input array.
  incr.resize (ndim);
  incr = 0;
  // Find out how many contiguous elements are available.
  size_t stax = ndim;
  nelemCont = 1;
  // Create the resulting shape and array.
  if (nres == 0) {
    resultShape.resize (1);
    resultShape = 1;
    nelemCont = sourceShape.product();
  } else {
    resultShape.resize (nres);
    int nr = 1;
    int lastAxis = -2;
    for (size_t i=0; i<nres; i++) {
      int axis = resultAxes(i);
      resultShape(i) = sourceShape(axis);
      if (axis != lastAxis+1) {
	incr(axis) = nr;
      } else {
	incr(axis) = 0;
      }
      lastAxis = axis;
      nr *= sourceShape(axis);
      if (++axis < ndim) {
	incr(axis) = -nr;
      }
    }
    stax = resultAxes(0);
    for (size_t i=0; i<stax; i++) {
      nelemCont *= sourceShape(i);
    }
  }
  return stax;
}

size_t reorderArrayHelper (IPosition& newShape, IPosition& incr,
			 const IPosition& shape, const IPosition& newAxisOrder)
{
  size_t ndim = shape.nelements();
  // Get the remaining axes forming mapping of new to old axes.
  // It also checks if axes are specified correctly.
  IPosition toOld = IPosition::makeAxisPath (ndim, newAxisOrder);
  // Create the mapping from old to new axes.
  // Create the new shape and the volume for each dimension.
  // Check if axes are really reordered.
  IPosition toNew(ndim);
  newShape.resize (ndim);
  IPosition volume(ndim+1, 1);
  size_t contAxes = ndim;
  for (size_t i=0; i<ndim; i++) {
    size_t oldAxis = toOld(i);
    toNew(oldAxis) = i;
    newShape(i) = shape(oldAxis);
    volume(i+1) = volume(i) * newShape(i);
    if (contAxes == ndim  &&  oldAxis != i) {
      contAxes = i;
    }
  }
  // Create an array which determines how to increment in the result
  // when incrementing an axis of the input array.
  incr.resize (ndim);
  // The increment in the first dimension is the volume of the axes
  // that get in front of the originally first axis.
  incr(0) = volume(toNew(0));
  // The increment in the other axes is the difference between the
  // last item in the previous dimension and the first one in the next.
  for (size_t i=1; i<ndim; i++) {
    size_t prevAxis = toNew(i-1);
    size_t newAxis = toNew(i);
    incr(i) = volume(newAxis) - volume(prevAxis+1);
  }
  return contAxes;
}

} //# NAMESPACE CASACORE - END

