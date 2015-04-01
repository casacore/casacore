//# ArrayLogical.cc: Element by element logical operations on arrays.
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

#ifndef CASA_ARRAYLOGICAL_TCC
#define CASA_ARRAYLOGICAL_TCC

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayError.h>
//# For scalar near() functions.
#include <casacore/casa/BasicMath/Functors.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<typename T, typename CompareOperator>
bool arrayCompareAll (const Array<T>& left, const Array<T>& right,
                      CompareOperator op)
{
  if (! left.conform(right)) return false;
  if (left.contiguousStorage()  &&  right.contiguousStorage()) {
    return compareAll (left.cbegin(), left.cend(), right.cbegin(), op);
  } else {
    return compareAll (left.begin(),  left.end(),  right.begin(),  op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAll (const Array<T>& left, T right,
                      CompareOperator op)
{
  if (left.contiguousStorage()) {
    return compareAllRight (left.cbegin(), left.cend(), right, op);
  } else {
    return compareAllRight (left.begin(), left.end(), right, op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAll (T left, const Array<T>& right,
                      CompareOperator op)
{
  if (right.contiguousStorage()) {
    return compareAllLeft (right.cbegin(), right.cend(), left, op);
  } else {
    return compareAllLeft (right.begin(), right.end(), left, op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAny (const Array<T>& left, const Array<T>& right,
                      CompareOperator op)
{
  if (! left.conform(right)) return false;
  if (left.contiguousStorage()  &&  right.contiguousStorage()) {
    return compareAny (left.cbegin(), left.cend(), right.cbegin(), op);
  } else {
    return compareAny (left.begin(),  left.end(),  right.begin(),  op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAny (const Array<T>& left, T right,
                      CompareOperator op)
{
  if (left.contiguousStorage()) {
    return compareAnyRight (left.cbegin(), left.cend(), right, op);
  } else {
    return compareAnyRight (left.begin(), left.end(), right, op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAny (T left, const Array<T>& right,
                      CompareOperator op)
{
  if (right.contiguousStorage()) {
    return compareAnyLeft (right.cbegin(), right.cend(), left, op);
  } else {
    return compareAnyLeft (right.begin(), right.end(), left, op);
  }
}


template<class T>
Bool allEQ (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::equal_to<T>());
}
template<class T>
Bool allNE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::not_equal_to<T>());
}
template<class T>
Bool allLT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less<T>());
}
template<class T>
Bool allLE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less_equal<T>());
}
template<class T>
Bool allGT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater<T>());
}
template<class T>
Bool allGE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater_equal<T>());
}
template<class T>
Bool allOR (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_or<T>());
}
template<class T>
Bool allAND (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_and<T>());
}

template<class T>
Bool allEQ (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::equal_to<T>());
}
template<class T>
Bool allNE (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::not_equal_to<T>());
}
template<class T>
Bool allLT (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::less<T>());
}
template<class T>
Bool allLE (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::less_equal<T>());
}
template<class T>
Bool allGT (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::greater<T>());
}
template<class T>
Bool allGE (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::greater_equal<T>());
}
template<class T>
Bool allOR (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::logical_or<T>());
}
template<class T>
Bool allAND (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::logical_and<T>());
}

template<class T>
Bool allEQ (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::equal_to<T>());
}
template<class T>
Bool allNE (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::not_equal_to<T>());
}
template<class T>
Bool allLT (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less<T>());
}
template<class T>
Bool allLE (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less_equal<T>());
}
template<class T>
Bool allGT (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater<T>());
}
template<class T>
Bool allGE (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater_equal<T>());
}
template<class T>
Bool allOR (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_or<T>());
}
template<class T>
Bool allAND (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_and<T>());
}


template<class T>
Bool anyEQ (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::equal_to<T>());
}
template<class T>
Bool anyNE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::not_equal_to<T>());
}
template<class T>
Bool anyLT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less<T>());
}
template<class T>
Bool anyLE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less_equal<T>());
}
template<class T>
Bool anyGT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater<T>());
}
template<class T>
Bool anyGE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater_equal<T>());
}
template<class T>
Bool anyOR (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::logical_or<T>());
}
template<class T>
Bool anyAND (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::logical_and<T>());
}

template<class T>
Bool anyEQ (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::equal_to<T>());
}
template<class T>
Bool anyNE (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::not_equal_to<T>());
}
template<class T>
Bool anyLT (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::less<T>());
}
template<class T>
Bool anyLE (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::less_equal<T>());
}
template<class T>
Bool anyGT (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::greater<T>());
}
template<class T>
Bool anyGE (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::greater_equal<T>());
}
template<class T>
Bool anyOR (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::logical_or<T>());
}
template<class T>
Bool anyAND (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::logical_and<T>());
}

template<class T>
Bool anyEQ (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::equal_to<T>());
}
template<class T>
Bool anyNE (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::not_equal_to<T>());
}
template<class T>
Bool anyLT (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less<T>());
}
template<class T>
Bool anyLE (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less_equal<T>());
}
template<class T>
Bool anyGT (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater<T>());
}
template<class T>
Bool anyGE (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater_equal<T>());
}
template<class T>
Bool anyOR (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::logical_or<T>());
}
template<class T>
Bool anyAND (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::logical_and<T>());
}



template<class T>
LogicalArray operator== (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, "==");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::equal_to<T>());
  return result;
}

template<class T>
LogicalArray operator!= (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, "!=");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::not_equal_to<T>());
  return result;
}

template<class T>
LogicalArray operator< (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, "<");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::less<T>());
  return result;
}

template<class T>
LogicalArray operator<= (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, "<=");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::less_equal<T>());
  return result;
}

template<class T>
LogicalArray operator> (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, ">");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::greater<T>());
  return result;
}

template<class T>
LogicalArray operator>= (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, ">=");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::greater_equal<T>());
  return result;
}

template<class T>
LogicalArray operator|| (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, "||");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::logical_or<T>());
  return result;
}

template<class T>
LogicalArray operator&& (const Array<T> &l, const Array<T> &r)
{
  checkArrayShapes (l, r, "&&");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::logical_and<T>());
  return result;
}

template<class T>
LogicalArray operator== (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::equal_to<T>());
  return result;
}

template<class T>
LogicalArray operator!= (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::not_equal_to<T>());
  return result;
}

template<class T>
LogicalArray operator< (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::less<T>());
  return result;
}

template<class T>
LogicalArray operator<= (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::less_equal<T>());
  return result;
}

template<class T>
LogicalArray operator> (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::greater<T>());
  return result;
}

template<class T>
LogicalArray operator>= (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::greater_equal<T>());
  return result;
}

template<class T>
LogicalArray operator|| (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::logical_or<T>());
  return result;
}

template<class T>
LogicalArray operator&& (const Array<T> &l, const T &r)
{
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, std::logical_and<T>());
  return result;
}

template<class T>
LogicalArray operator== (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::equal_to<T>());
  return result;
}

template<class T>
LogicalArray operator!= (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::not_equal_to<T>());
  return result;
}

template<class T>
LogicalArray operator< (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::less<T>());
  return result;
}

template<class T>
LogicalArray operator<= (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::less_equal<T>());
  return result;
}

template<class T>
LogicalArray operator> (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::greater<T>());
  return result;
}

template<class T>
LogicalArray operator>= (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::greater_equal<T>());
  return result;
}

template<class T>
LogicalArray operator|| (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::logical_or<T>());
  return result;
}

template<class T>
LogicalArray operator&& (const T &l, const Array<T> &r)
{
  LogicalArray result(r.shape());
  arrayContTransform (l, r, result, std::logical_and<T>());
  return result;
}



template<class T>
LogicalArray operator! (const Array<T> &array)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, result, std::logical_not<T>());
  return result;
}


template<class T>
LogicalArray isNaN (const Array<T> &array)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, result, casacore::IsNaN<T>());
  return result;
}

template<class T>
LogicalArray isInf (const Array<T> &array)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, result, casacore::IsInf<T>());
  return result;
}

template<class T>
LogicalArray isFinite (const Array<T> &array)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, result, casacore::IsFinite<T>());
  return result;
}

template<class T>
LogicalArray near (const Array<T> &l, const Array<T>& r, Double tol)
{
  checkArrayShapes (l, r, "near");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, casacore::Near<T>(tol));
  return result;
}


template<class T> LogicalArray nearAbs(const Array<T> &l, const Array<T> &r,
              				    Double tol)
{
  checkArrayShapes (l, r, "nearAbs");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, casacore::NearAbs<T>(tol));
  return result;
}

template<class T> LogicalArray near (const Array<T> &array, const T &val,
				     Double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, val, result, casacore::Near<T>(tol));
  return result;
}

template<class T> LogicalArray near (const T &val, const Array<T> &array,
				      Double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (val, array, result, casacore::Near<T>(tol));
  return result;
}

template<class T> LogicalArray nearAbs (const Array<T> &array, const T &val,
				     Double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, val, result, casacore::NearAbs<T>(tol));
  return result;
}

template<class T> LogicalArray nearAbs (const T &val, const Array<T> &array,
				      Double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (val, array, result, casacore::NearAbs<T>(tol));
  return result;
}


template<class T> Bool allNear (const Array<T> &l, const Array<T> &r,
				Double tol)
{
  return arrayCompareAll (l, r, casacore::Near<T>(tol));
}

template<class T> Bool allNear (const Array<T> &array, const T &val, Double tol)
{
  return arrayCompareAll (array, val, casacore::Near<T>(tol));
}

template<class T> Bool allNear (const T &val, const Array<T> &array, Double tol)
{
  return arrayCompareAll (val, array, casacore::Near<T>(tol));
}

template<class T> Bool allNearAbs (const Array<T> &l, const Array<T> &r,
				   Double tol)
{
  return arrayCompareAll (l, r, casacore::NearAbs<T>(tol));
}

template<class T> Bool allNearAbs (const Array<T> &array, const T &val,
				   Double tol)
{
  return arrayCompareAll (array, val, casacore::NearAbs<T>(tol));
}

template<class T> Bool allNearAbs (const T &val, const Array<T> &array,
				   Double tol)
{
  return arrayCompareAll (val, array, casacore::NearAbs<T>(tol));
}


template<class T> Bool anyNear (const Array<T> &l, const Array<T> &r,
				Double tol)
{
  return arrayCompareAny (l, r, casacore::Near<T>(tol));
}

template<class T> Bool anyNear (const Array<T> &array, const T &val, Double tol)
{
  return arrayCompareAny (array, val, casacore::Near<T>(tol));
}

template<class T> Bool anyNear (const T &val, const Array<T> &array, Double tol)
{
  return arrayCompareAny (val, array, casacore::Near<T>(tol));
}

template<class T> Bool anyNearAbs (const Array<T> &l, const Array<T> &r,
				   Double tol)
{
  return arrayCompareAny (l, r, casacore::NearAbs<T>(tol));
}

template<class T> Bool anyNearAbs (const Array<T> &array, const T &val,
				   Double tol)
{
  return arrayCompareAny (array, val, casacore::NearAbs<T>(tol));
}

template<class T> Bool anyNearAbs (const T &val, const Array<T> &array,
				   Double tol)
{
  return arrayCompareAny (val, array, casacore::NearAbs<T>(tol));
}


template<class T> size_t nfalse (const Array<T> &array)
{
  return (array.contiguousStorage() ?
          std::count (array.cbegin(), array.cend(), T()) :
          std::count (array.begin(),  array.end(),  T()));
}

template<class T> Array<uInt> partialNTrue (const Array<T>& array,
					    const IPosition& collapseAxes)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<uInt>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<uInt> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  uInt* resData = result.getStorage (deleteRes);
  uInt* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // const tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // no gives the number of contiguous elements.
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
      uInt tmp = *res;
      for (uInt i=0; i<n0; i++) {
	if (*data++) {
	  tmp++;
	}
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	if (*data++) {
	  (*res)++;
	}
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

template<class T> Array<uInt> partialNFalse (const Array<T>& array,
					     const IPosition& collapseAxes)
{
  Array<uInt> result = partialNTrue (array, collapseAxes);
  uInt nr = result.nelements();
  if (nr > 0) {
    uInt factor = array.nelements() / nr;
    Bool deleteRes;
    uInt* res = result.getStorage (deleteRes);
    for (uInt i=0; i<nr; i++) {
      res[i] = factor - res[i];
    }
    result.putStorage (res, deleteRes);
  }
  return result;
}

} //# NAMESPACE CASACORE - END


#endif
