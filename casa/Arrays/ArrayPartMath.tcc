//# ArrayMath.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2001,2003
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

#include <casa/iostream.h>

#include <casa/Arrays/ArrayPartMath.h>
#include <casa/Arrays/ArrayIter.h>
#include <casa/Arrays/ArrayError.h>
#include <casa/BasicMath/Math.h>
#include <casa/Utilities/Assert.h>

namespace casa { //# NAMESPACE CASA - BEGIN


template<class T> Array<T> partialSums (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	tmp += *data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res += *data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
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

template<class T> Array<T> partialSumSqrs (const Array<T>& array,
                                           const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	tmp += *data * *data;
        data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res += *data * *data;
        data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
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

template<class T> Array<T> partialProducts (const Array<T>& array,
					    const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	tmp *= *data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res *= *data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
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

template<class T> Array<T> partialMins (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Initialize the minima with the first value of collapsed axes.
  IPosition end(shape-1);
  for (uInt i=0; i<collapseAxes.nelements(); i++) {
    uInt axis = collapseAxes(i);
    end(axis) = 0;
  }
  Array<T> tmp(array);           // to get a non-const array for operator()
  result = tmp(IPosition(ndim,0), end).reform (resShape);
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	if (*data < tmp) {
	  tmp = *data;
	}
	data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	if (*data < *res) {
	  *res = *data;
	}
	data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
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

template<class T> Array<T> partialMaxs (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Initialize the maxima with the first value of collapsed axes.
  IPosition end(shape-1);
  for (uInt i=0; i<collapseAxes.nelements(); i++) {
    uInt axis = collapseAxes(i);
    end(axis) = 0;
  }
  Array<T> tmp(array);           // to get a non-const array for operator()
  result = tmp(IPosition(ndim,0), end).reform (resShape);
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	if (*data > tmp) {
	  tmp = *data;
	}
	data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	if (*data > *res) {
	  *res = *data;
	}
	data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
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

template<class T> Array<T> partialMeans (const Array<T>& array,
					 const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  Array<T> result = partialSums (array, collapseAxes);
  uInt nr = result.nelements();
  if (nr > 0) {
    uInt factor = array.nelements() / nr;
    Bool deleteRes;
    T* res = result.getStorage (deleteRes);
    for (uInt i=0; i<nr; i++) {
      res[i] /= 1.0 * factor;
    }
    result.putStorage (res, deleteRes);
  }
  return result;
}

template<class T> Array<T> partialVariances (const Array<T>& array,
					     const IPosition& collapseAxes,
					     const Array<T>& means)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  if (! resShape.isEqual (means.shape())) {
    throw AipsError ("partialVariances: shape of means array mismatches "
		     "shape of result array");
  }
  Array<T> result (resShape);
  result = 0;
  uInt nr = result.nelements();
  uInt factor = array.nelements() / nr - 1;
  if (factor == 0) {
    return result;
  }
  Bool deleteData, deleteRes, deleteMean;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  const T* meanData = means.getStorage (deleteMean);
  const T* mean = meanData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      T tmpm = *mean;
      for (uInt i=0; i<n0; i++) {
	T var = *data++ - tmpm;
	tmp += var*var;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	T var = *data++ - *mean;
	*res += var*var;
	res += incr0;
	mean += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      mean += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  res = resData;
  for (uInt i=0; i<nr; i++) {
    res[i] /= 1.0 * factor;
  }
  array.freeStorage (arrData, deleteData);
  means.freeStorage (meanData, deleteMean);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialAvdevs (const Array<T>& array,
					  const IPosition& collapseAxes,
					  const Array<T>& means)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  if (! resShape.isEqual (means.shape())) {
    throw AipsError ("partialAvdevs: shape of means array mismatches "
		     "shape of result array");
  }
  Array<T> result (resShape);
  result = 0;
  uInt nr = result.nelements();
  uInt factor = array.nelements() / nr;
  Bool deleteData, deleteRes, deleteMean;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  const T* meanData = means.getStorage (deleteMean);
  const T* mean = meanData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      T tmpm = *mean;
      for (uInt i=0; i<n0; i++) {
	tmp += fabs(*data++ - tmpm);
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res += fabs(*data++ - *mean);
	res += incr0;
	mean += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      mean += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  res = resData;
  for (uInt i=0; i<nr; i++) {
    res[i] /= 1.0 * factor;
  }
  array.freeStorage (arrData, deleteData);
  means.freeStorage (meanData, deleteMean);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialRmss (const Array<T>& array,
					const IPosition& collapseAxes)
{
  if (collapseAxes.nelements() == 0) {
    return array.copy();
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<T> result (resShape);
  result = 0;
  uInt nr = result.nelements();
  uInt factor = array.nelements() / nr;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      T tmp = *res;
      for (uInt i=0; i<n0; i++) {
	tmp += *data * *data;
	data++;
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	*res += *data * *data;
	data++;
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
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
  res = resData;
  for (uInt i=0; i<nr; i++) {
    res[i] = T(sqrt (res[i] / factor));
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialMedians (const Array<T>& array,
					   const IPosition& collapseAxes,
					   Bool takeEvenMean,
					   Bool inPlace)
{
  // Need to make shallow copy because operator() is non-const.
  Array<T> arr = array;
  // Is there anything to collapse?
  if (collapseAxes.nelements() == 0) {
    return (inPlace  ?  array : array.copy());
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  // Get the remaining axes.
  // It also checks if axes are specified correctly.
  IPosition resAxes = IPosition::otherAxes (ndim, collapseAxes);
  uInt ndimRes = resAxes.nelements();
  // Create the result shape.
  // Create blc and trc to step through the input array.
  IPosition resShape(ndimRes);
  IPosition blc(ndim, 0);
  IPosition trc(shape-1);
  for (uInt i=0; i<ndimRes; ++i) {
    resShape[i] = shape[resAxes[i]];
    trc[resAxes[i]] = 0;
  }
  if (ndimRes == 0) {
    resShape.resize(1);
    resShape[0] = 1;
  }
  Array<T> result (resShape);
  Bool deleteRes;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  Block<T> tmp;
  // Loop through all data and assemble as needed.
  IPosition pos(ndimRes, 0);
  while (True) {
    *res++ = median(arr(blc,trc), tmp, False, takeEvenMean, inPlace);
    uInt ax;
    for (ax=0; ax<ndimRes; ax++) {
      if (++pos(ax) < resShape(ax)) {
	blc[resAxes[ax]]++;
	trc[resAxes[ax]]++;
	break;
      }
      pos(ax) = 0;
      blc[resAxes[ax]] = 0;
      trc[resAxes[ax]] = 0;
    }
    if (ax == ndimRes) {
      break;
    }
  }
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<T> partialFractiles (const Array<T>& array,
					     const IPosition& collapseAxes,
					     Float fraction,
					     Bool inPlace)
{
  if (fraction < 0  ||  fraction > 1) {
    throw(ArrayError("::fractile(const Array<T>&) - fraction <0 or >1 "));
  }    
  // Need to make shallow copy because operator() is non-const.
  Array<T> arr = array;
  // Is there anything to collapse?
  if (collapseAxes.nelements() == 0) {
    return (inPlace  ?  array : array.copy());
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<T>();
  }
  // Get the remaining axes.
  // It also checks if axes are specified correctly.
  IPosition resAxes = IPosition::otherAxes (ndim, collapseAxes);
  uInt ndimRes = resAxes.nelements();
  // Create the result shape.
  // Create blc and trc to step through the input array.
  IPosition resShape(ndimRes);
  IPosition blc(ndim, 0);
  IPosition trc(shape-1);
  for (uInt i=0; i<ndimRes; ++i) {
    resShape[i] = shape[resAxes[i]];
    trc[resAxes[i]] = 0;
  }
  if (ndimRes == 0) {
    resShape.resize(1);
    resShape[0] = 1;
  }
  Array<T> result (resShape);
  Bool deleteRes;
  T* resData = result.getStorage (deleteRes);
  T* res = resData;
  Block<T> tmp;
  // Loop through all data and assemble as needed.
  IPosition pos(ndimRes, 0);
  while (True) {
    *res++ = fractile(arr(blc,trc), tmp, fraction, False, inPlace);
    uInt ax;
    for (ax=0; ax<ndimRes; ax++) {
      if (++pos(ax) < resShape(ax)) {
	blc[resAxes[ax]]++;
	trc[resAxes[ax]]++;
	break;
      }
      pos(ax) = 0;
      blc[resAxes[ax]] = 0;
      trc[resAxes[ax]] = 0;
    }
    if (ax == ndimRes) {
      break;
    }
  }
  result.putStorage (resData, deleteRes);
  return result;
}


template<typename T, typename RES>
void partialArrayMath (Array<RES>& res,
                       const Array<T>& a,
                       const IPosition& collapseAxes,
                       const ArrayFunctorBase<T,RES>& funcObj)
{
  ReadOnlyArrayIterator<T> aiter(a, collapseAxes);
  IPosition shape(a.shape().removeAxes (collapseAxes));
  res.resize (shape);
  RES* data = res.data();
  while (!aiter.pastEnd()) {
    *data++ = funcObj(aiter.array());
    aiter.next();
  }
}


template <typename T, typename RES>
void boxedArrayMath (Array<RES>& result,
                     const Array<T>& array,
                     const IPosition& boxShape,
                     const ArrayFunctorBase<T,RES>& funcObj)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.size();
  IPosition fullBoxShape, resShape;
  fillBoxedShape (shape, boxShape, fullBoxShape, resShape);
  result.resize (resShape);
  DebugAssert (result.contiguousStorage(), AipsError);
  T* res = result.data();
  // Loop through all data and assemble as needed.
  IPosition blc(ndim, 0);
  IPosition trc(fullBoxShape-1);
  while (True) {
    *res++ = funcObj (array(blc,trc));
    uInt ax;
    for (ax=0; ax<ndim; ++ax) {
      blc[ax] += fullBoxShape[ax];
      if (blc[ax] < shape[ax]) {
	trc[ax] += fullBoxShape[ax];
	if (trc[ax] >= shape[ax]) {
	  trc[ax] = shape[ax]-1;
	}
	break;
      }
      blc[ax] = 0;
      trc[ax] = fullBoxShape[ax]-1;
    }
    if (ax == ndim) {
      break;
    }
  }
}

template <typename T, typename RES>
void slidingArrayMath (Array<RES>& result,
                       const Array<T>& array,
                       const IPosition& halfBoxShape,
                       const ArrayFunctorBase<T,RES>& funcObj,
                       Bool fillEdge)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.size();
  IPosition boxEnd, resShape;
  Bool empty = fillSlidingShape (shape, halfBoxShape, boxEnd, resShape);
  if (fillEdge) {
    result.resize (shape);
    result = RES();
  } else {
    result.resize (resShape);
  }
  if (!empty) {
    Array<RES>  resa (result);
    if (fillEdge) {
      IPosition boxEnd2 (boxEnd/2);
      resa.reference (resa(boxEnd2, resShape+boxEnd2-1));
    }
    typename Array<RES>::iterator iterarr(resa.begin());
    // Loop through all data and assemble as needed.
    IPosition blc(ndim, 0);
    IPosition trc(boxEnd);
    IPosition pos(ndim, 0);
    while (True) {
      *iterarr = funcObj (array(blc,trc));
      ++iterarr;
      uInt ax;
      for (ax=0; ax<ndim; ++ax) {
        if (++pos[ax] < resShape[ax]) {
          blc[ax]++;
          trc[ax]++;
          break;
        }
        pos(ax) = 0;
        blc[ax] = 0;
        trc[ax] = boxEnd[ax];
      }
      if (ax == ndim) {
        break;
      }
    }
  }
}


} //# NAMESPACE CASA - END
