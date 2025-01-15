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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_ARRAYLOGICAL_2_TCC
#define CASA_ARRAYLOGICAL_2_TCC

#include "ArrayLogical.h"
#include "ArrayMath.h"
#include "ArrayUtil.h"
#include "ArrayError.h"
#include "ElementFunctions.h"

#include <algorithm>
#include <cmath>
#include <complex>
#include <limits>

#include "ElementFunctions.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<typename T, typename CompareOperator>
bool arrayCompareAll (const Array<T>& left, const Array<T>& right,
                      CompareOperator op)
{
  if (! left.conform(right)) return false;
  if (left.contiguousStorage()  &&  right.contiguousStorage()) {
    return arrays_internal::compareAll (left.cbegin(), left.cend(), right.cbegin(), op);
  } else {
    return arrays_internal::compareAll (left.begin(),  left.end(),  right.begin(),  op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAll (const Array<T>& left, T right,
                      CompareOperator op)
{
  if (left.contiguousStorage()) {
    return arrays_internal::compareAllRight (left.cbegin(), left.cend(), right, op);
  } else {
    return arrays_internal::compareAllRight (left.begin(), left.end(), right, op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAll (T left, const Array<T>& right,
                      CompareOperator op)
{
  if (right.contiguousStorage()) {
    return arrays_internal::compareAllLeft (right.cbegin(), right.cend(), left, op);
  } else {
    return arrays_internal::compareAllLeft (right.begin(), right.end(), left, op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAny (const Array<T>& left, const Array<T>& right,
                      CompareOperator op)
{
  if (! left.conform(right)) return false;
  if (left.contiguousStorage()  &&  right.contiguousStorage()) {
    return arrays_internal::compareAny (left.cbegin(), left.cend(), right.cbegin(), op);
  } else {
    return arrays_internal::compareAny (left.begin(),  left.end(),  right.begin(),  op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAny (const Array<T>& left, T right,
                      CompareOperator op)
{
  if (left.contiguousStorage()) {
    return arrays_internal::compareAnyRight (left.cbegin(), left.cend(), right, op);
  } else {
    return arrays_internal::compareAnyRight (left.begin(), left.end(), right, op);
  }
}

template<typename T, typename CompareOperator>
bool arrayCompareAny (T left, const Array<T>& right,
                      CompareOperator op)
{
  if (right.contiguousStorage()) {
    return arrays_internal::compareAnyLeft (right.cbegin(), right.cend(), left, op);
  } else {
    return arrays_internal::compareAnyLeft (right.begin(), right.end(), left, op);
  }
}


template<class T>
bool allEQ (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::equal_to<T>());
}
template<class T>
bool allNE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::not_equal_to<T>());
}
template<class T>
bool allLT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less<T>());
}
template<class T>
bool allLE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less_equal<T>());
}
template<class T>
bool allGT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater<T>());
}
template<class T>
bool allGE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater_equal<T>());
}
template<class T>
bool allOR (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_or<T>());
}
template<class T>
bool allAND (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_and<T>());
}

template<class T>
bool allEQ (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::equal_to<T>());
}
template<class T>
bool allNE (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::not_equal_to<T>());
}
template<class T>
bool allLT (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::less<T>());
}
template<class T>
bool allLE (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::less_equal<T>());
}
template<class T>
bool allGT (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::greater<T>());
}
template<class T>
bool allGE (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::greater_equal<T>());
}
template<class T>
bool allOR (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::logical_or<T>());
}
template<class T>
bool allAND (const Array<T> &l, const T &r)
{
  return arrayCompareAll (l, r, std::logical_and<T>());
}

template<class T>
bool allEQ (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::equal_to<T>());
}
template<class T>
bool allNE (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::not_equal_to<T>());
}
template<class T>
bool allLT (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less<T>());
}
template<class T>
bool allLE (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::less_equal<T>());
}
template<class T>
bool allGT (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater<T>());
}
template<class T>
bool allGE (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::greater_equal<T>());
}
template<class T>
bool allOR (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_or<T>());
}
template<class T>
bool allAND (const T &l, const Array<T> &r)
{
  return arrayCompareAll (l, r, std::logical_and<T>());
}


template<class T>
bool anyEQ (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::equal_to<T>());
}
template<class T>
bool anyNE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::not_equal_to<T>());
}
template<class T>
bool anyLT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less<T>());
}
template<class T>
bool anyLE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less_equal<T>());
}
template<class T>
bool anyGT (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater<T>());
}
template<class T>
bool anyGE (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater_equal<T>());
}
template<class T>
bool anyOR (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::logical_or<T>());
}
template<class T>
bool anyAND (const Array<T> &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::logical_and<T>());
}

template<class T>
bool anyEQ (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::equal_to<T>());
}
template<class T>
bool anyNE (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::not_equal_to<T>());
}
template<class T>
bool anyLT (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::less<T>());
}
template<class T>
bool anyLE (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::less_equal<T>());
}
template<class T>
bool anyGT (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::greater<T>());
}
template<class T>
bool anyGE (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::greater_equal<T>());
}
template<class T>
bool anyOR (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::logical_or<T>());
}
template<class T>
bool anyAND (const Array<T> &l, const T &r)
{
  return arrayCompareAny (l, r, std::logical_and<T>());
}

template<class T>
bool anyEQ (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::equal_to<T>());
}
template<class T>
bool anyNE (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::not_equal_to<T>());
}
template<class T>
bool anyLT (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less<T>());
}
template<class T>
bool anyLE (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::less_equal<T>());
}
template<class T>
bool anyGT (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater<T>());
}
template<class T>
bool anyGE (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::greater_equal<T>());
}
template<class T>
bool anyOR (const T &l, const Array<T> &r)
{
  return arrayCompareAny (l, r, std::logical_or<T>());
}
template<class T>
bool anyAND (const T &l, const Array<T> &r)
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
  using std::isnan;
  using arrays_internal::isnan;
  arrayContTransform (array, result, [](T val){ return isnan(val);} );
  return result;
}

template<class T>
LogicalArray isInf (const Array<T> &array)
{
  LogicalArray result(array.shape());
  using std::isinf;
  using arrays_internal::isinf;
  arrayContTransform (array, result, [](T val){ return isinf(val);} );
  return result;
}

template<class T>
LogicalArray isFinite (const Array<T> &array)
{
  LogicalArray result(array.shape());
  using std::isfinite;
  using arrays_internal::isfinite;
  arrayContTransform (array, result, [](T val){ return isfinite(val);} );
  return result;
}

template<class T>
LogicalArray near (const Array<T> &l, const Array<T>& r, double tol)
{
  checkArrayShapes (l, r, "near");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
  return result;
}


template<class T> LogicalArray nearAbs(const Array<T> &l, const Array<T> &r,
              				    double tol)
{
  checkArrayShapes (l, r, "nearAbs");
  LogicalArray result(l.shape());
  arrayContTransform (l, r, result, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
  return result;
}

template<class T> LogicalArray near (const Array<T> &array, const T &val,
				     double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, val, result, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
  return result;
}

template<class T> LogicalArray near (const T &val, const Array<T> &array,
				      double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (val, array, result, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
  return result;
}

template<class T> LogicalArray nearAbs (const Array<T> &array, const T &val,
				     double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (array, val, result, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
  return result;
}

template<class T> LogicalArray nearAbs (const T &val, const Array<T> &array,
				      double tol)
{
  LogicalArray result(array.shape());
  arrayContTransform (val, array, result, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
  return result;
}


template<class T> bool allNear (const Array<T> &l, const Array<T> &r,
				double tol)
{
  return arrayCompareAll (l, r, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
}

template<class T> bool allNear (const Array<T> &array, const T &val, double tol)
{
  return arrayCompareAll (array, val, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
}

template<class T> bool allNear (const T &val, const Array<T> &array, double tol)
{
  return arrayCompareAll (val, array, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
}

template<class T> bool allNearAbs (const Array<T> &l, const Array<T> &r,
				   double tol)
{
  return arrayCompareAll (l, r, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
}

template<class T> bool allNearAbs (const Array<T> &array, const T &val,
				   double tol)
{
  return arrayCompareAll (array, val, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
}

template<class T> bool allNearAbs (const T &val, const Array<T> &array,
				   double tol)
{
  return arrayCompareAll (val, array, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
}


template<class T> bool anyNear (const Array<T> &l, const Array<T> &r,
				double tol)
{
  return arrayCompareAny (l, r, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
}

template<class T> bool anyNear (const Array<T> &array, const T &val, double tol)
{
  return arrayCompareAny (array, val, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
}

template<class T> bool anyNear (const T &val, const Array<T> &array, double tol)
{
  return arrayCompareAny (val, array, [tol](T left, T right){ return arrays_internal::near(left, right, tol); });
}

template<class T> bool anyNearAbs (const Array<T> &l, const Array<T> &r,
				   double tol)
{
  return arrayCompareAny (l, r, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
}

template<class T> bool anyNearAbs (const Array<T> &array, const T &val,
				   double tol)
{
  return arrayCompareAny (array, val, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
}

template<class T> bool anyNearAbs (const T &val, const Array<T> &array,
				   double tol)
{
  return arrayCompareAny (val, array, [tol](T left, T right){ return arrays_internal::nearAbs(left, right, tol); });
}


template<class T> size_t nfalse (const Array<T> &array)
{
  return (array.contiguousStorage() ?
          std::count (array.cbegin(), array.cend(), T()) :
          std::count (array.begin(),  array.end(),  T()));
}

template<class T> Array<size_t> partialNTrue (const Array<T>& array,
					    const IPosition& collapseAxes)
{
  const IPosition& shape = array.shape();
  size_t ndim = shape.nelements();
  if (ndim == 0) {
    return Array<size_t>();
  }
  IPosition resShape, incr;
  int nelemCont = 0;
  size_t stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<size_t> result (resShape);
  result = 0;
  bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  size_t* resData = result.getStorage (deleteRes);
  size_t* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // const tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // no gives the number of contiguous elements.
  bool cont = true;
  unsigned n0 = nelemCont;
  int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = false;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (true) {
    if (cont) {
      size_t tmp = *res;
      for (size_t i=0; i<n0; i++) {
	if (*data++) {
	  tmp++;
	}
      }
      *res = tmp;
    } else {
      for (size_t i=0; i<n0; i++) {
	if (*data++) {
	  (*res)++;
	}
	res += incr0;
      }
    }
    size_t ax;
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

template<class T> Array<size_t> partialNFalse (const Array<T>& array,
					     const IPosition& collapseAxes)
{
  Array<size_t> result = partialNTrue (array, collapseAxes);
  size_t nr = result.nelements();
  if (nr > 0) {
    size_t factor = array.nelements() / nr;
    bool deleteRes;
    size_t* res = result.getStorage (deleteRes);
    for (size_t i=0; i<nr; i++) {
      res[i] = factor - res[i];
    }
    result.putStorage (res, deleteRes);
  }
  return result;
}

} //# NAMESPACE CASACORE - END


#endif
