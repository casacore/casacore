//# ExprFuncNodeArray.cc: Class representing an array function in table select expression
//# Copyright (C) 2001,2003
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

#include <casacore/tables/TaQL/ExprFuncNodeArray.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/TaQL/MArrayMath.h>
#include <casacore/tables/TaQL/MArrayLogical.h>
#include <casacore/tables/TaQL/MArrayUtil.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Helper function to fill an array from another one.
  template <typename T>
  void TEFNAFillArray (Array<T>& res, Array<T> arr)
  {
    bool delRes, delArr;
    T* resd = res.getStorage (delRes);
    const T* arrd = arr.getStorage (delArr);
    size_t j=0;
    size_t arrsz = arr.size();
    size_t n = res.size();
    for (size_t i=0; i<n; i++) {
      resd[i] = arrd[j++];
      if (j >= arrsz) {
        j = 0;
      }
    }
    res.putStorage (resd, delRes);
    arr.freeStorage (arrd, delArr);
  }

  template <typename T>
  void TEFNAiifExec (const bool* cond,
                     const T* left, int incrLeft,
                     const T* right, int incrRight,
                     T* result, size_t nr)
  {
    size_t p1 = 0;
    size_t p2 = 0;
    for (size_t i=0; i<nr; ++i) {
      if (cond[i]) {
        result[i] = left[p1];
      } else {
        result[i] = right[p2];
      }
      p1 += incrLeft;
      p2 += incrRight;
    }
  }

  // Handle the case where the condition and one of the arguments is a scalar,
  // and the other is an array.
  // The result is always an array.
  template <typename T>
  MArray<T> TEFNAiifAS (bool useArray, const MArray<T>& arr,
                        const TENShPtr& node, const TableExprId& id)
  {
    if (useArray  ||  arr.isNull()) return arr;
    // Use the scalar by expanding it to an (unmasked) array.
    Array<T> res(arr.shape());
    T val;
    node->get(id, val);
    res = val;
    return MArray<T>(res);
  }
  
  // Result mask is true if cond.mask=true, otherwise mask1 or mask2.
  // Result is null if one of the operands is null. Only if condition
  // is scalar and operands are arrays, the result might be non-null.
  template <typename T>
  MArray<T> TEFNAiif (const vector<TENShPtr>& operands,
                      const TableExprId& id)
  {
    // If the condition is a scalar, one or both operands is an array.
    if (operands[0]->valueType() == TableExprNodeRep::VTScalar) {
      bool valc = operands[0]->getBool(id);
      MArray<T> values;
      // If an operand is a scalar, return array or scalar expanded to array.
      // Note that the evaluation of the scalar operand is only done if needed.
      if (operands[1]->valueType() == TableExprNodeRep::VTScalar) {
        operands[2]->get(id, values);
        return TEFNAiifAS (!valc, values, operands[1], id);
      } else if (operands[2]->valueType() == TableExprNodeRep::VTScalar) {
        operands[1]->get(id, values);
        return TEFNAiifAS (valc, values, operands[2], id);
      } else if (valc) {
        operands[1]->get(id, values);
        return values;
      } else {
        operands[2]->get(id, values);
        return values;
      }
    }
    // The condition is an array. The operands can be scalar or array.
    // Arrays can have masks.
    // First get the condition array and a pointer to its data.
    MArray<bool> arrc (operands[0]->getArrayBool(id));
    if (arrc.isNull()) {
      return MArray<T>();
    }
    bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
    const bool* datac = arrc.array().getStorage (deleteArrc);
    IPosition shp (arrc.shape());
    size_t nr = arrc.size();
    // The operands can be array or scalar.
    // So use a pointer and increment that can deal with either of them.
    MArray<T> arr1, arr2;
    T val1, val2;
    const T* data1 = &val1;
    const T* data2 = &val2;
    size_t incr1 = 1;
    size_t incr2 = 1;
    // Set initially that the operands do not have a mask.
    bool hasMask = false;
    bool isNull  = false;
    // Get the values. If array, check if shape matches.
    if (operands[1]->valueType() == TableExprNodeRep::VTScalar) {
      operands[1]->get(id, val1);
      incr1 = 0;
    } else {
      operands[1]->get(id, arr1);
      if (arr1.isNull()) isNull = true;
      if (! shp.isEqual (arr1.shape())) {
        throw TableInvExpr ("TableExprFuncNodeArray::get<T>, "
                            "array shapes mismatch in function IIF");
      }
      data1   = arr1.array().getStorage (deleteArr1);
      hasMask = hasMask || arr1.hasMask();
    }
    if (operands[2]->valueType() == TableExprNodeRep::VTScalar) {
      operands[2]->get(id, val2);
      incr2 = 0;
    } else {
      operands[2]->get(id, arr2);
      if (arr2.isNull()) isNull = true;
      if (! shp.isEqual (arr2.shape())) {
        throw TableInvExpr ("TableExprFuncNodeArray::get<T>, "
                            "array shapes mismatch in function IIF");
      }
      data2   = arr2.array().getStorage (deleteArr2);
      hasMask = hasMask || arr2.hasMask();
    }
    if (isNull) {
      return MArray<T>();
    }
    Array<T> result(shp);
    T* res = result.getStorage (deleteRes);
    TEFNAiifExec (datac, data1, incr1, data2, incr2, res, nr);
    arrc.array().freeStorage (datac, deleteArrc);
    if (data1 != &val1) arr1.array().freeStorage (data1, deleteArr1);
    if (data2 != &val2) arr2.array().freeStorage (data2, deleteArr2);
    result.putStorage (res, deleteRes);
    if (!hasMask) {
      return MArray<T>(result, arrc.mask());
    }
    // The operands have a mask, so combine them in the same way as the values.
    bool valMask1 = false;
    bool valMask2 = false;
    const bool* valm1 = &valMask1;
    const bool* valm2 = &valMask2;
    incr1 = 0;
    incr2 = 0;
    if (arr1.hasMask()) {
      valm1 = arr1.mask().getStorage (deleteArr1);
      incr1 = 1;
    }
    if (arr2.hasMask()) {
      valm2 = arr2.mask().getStorage (deleteArr2);
      incr2 = 1;
    }
    Array<bool> resMask(shp);
    bool* resm = resMask.getStorage (deleteRes);
    TEFNAiifExec (datac, valm1, incr1, valm2, incr2, resm, arrc.size());
    if (valm1 != &valMask1) arr1.mask().freeStorage (valm1, deleteArr1);
    if (valm2 != &valMask2) arr2.mask().freeStorage (valm2, deleteArr2);
    result.putStorage (res, deleteRes);
    MArray<T> mares (result, resMask);
    return MArray<T> (result, mares.combineMask (arrc));
  }

  template<typename T>
  MArray<T> TEFMASKneg (const MArray<T>& arr)
  {
    if (arr.isNull()) {
      return arr;
    } else if (!arr.hasMask()) {
      return MArray<T> (arr.array(), Array<bool>(arr.shape(), true));
    }
    return MArray<T> (arr.array(), !arr.mask());
  }

  template<typename T>
  MArray<T> TEFMASKrepl (const MArray<T>& arr, const TENShPtr& operand2,
                         const TableExprId& id, bool maskValue)
  {
    MArray<T> res(arr);
    MArray<T> arr2;
    T val2;
    T* data1;
    const T* data2 = &val2;
    const bool* mask1;
    size_t incr2 = 0;
    bool del1, del2, delm;
    if (operand2->valueType() == TableExprNodeRep::VTScalar) {
      operand2->get(id, val2);
    } else {
      operand2->get(id, arr2);
      if (arr2.isNull()) {
        return MArray<T>();
      }
      if (! arr.shape().isEqual (arr2.shape())) {
        throw TableInvExpr ("TableExprFuncNodeArray::get<T>, array shapes "
                            "mismatch in function REPLACE(UN)MASKED");
      }
      data2 = arr2.array().getStorage (del2);
      incr2 = 1;
    }
    if (!arr.hasMask()) {
      if (!maskValue) {
        if (incr2 == 0) {
          res.array() = val2;
        } else {
          res.array() = arr2.array();
        }
      }
    } else {
      data1 = res.array().getStorage (del1);
      mask1 = arr.mask().getStorage (delm);
      for (size_t i=0; i<arr.size(); ++i, data2+=incr2) {
        if (mask1[i] == maskValue) {
          data1[i] = *data2;
        }
      }
      res.array().putStorage (data1, del1);
      arr.mask().freeStorage (mask1, delm);
      if (incr2 > 0) {
        arr2.array().freeStorage (data2, del2);
      }
    }
    return res;
  }

  // Helper function to resize an array.
  template<typename T>
  MArray<T> TableExprFuncNodeArray::TEFResize (const MArray<T>& arr,
                                               const TableExprId& id)
  {
    IPosition shp = adjustShape (getArrayShape(id), arr.shape());
    const IPosition& alt = getAlternate(id);
    if (alt.empty()) {
      Array<T> res(shp, T());
      res.copyMatchingPart (arr.array());
      if (arr.hasMask()) {
        Array<bool> resm(shp, false);
        resm.copyMatchingPart (arr.mask());
        return MArray<T>(res, resm);
      }
      return MArray<T>(res);
    }
    Array<T> res(shp);
    expandArray (res, arr.array(), alt);
    if (arr.hasMask()) {
      Array<bool> resm(shp);
      expandArray (resm, arr.mask(), alt);
      return MArray<T>(res, resm);
    }
    return MArray<T>(res);
  }


TableExprFuncNodeArray::TableExprFuncNodeArray
                             (TableExprFuncNode::FunctionType ftype,
                              NodeDataType dtype, ValueType vtype,
                              const TableExprNodeSet& source,
                              const vector<TENShPtr>& nodes,
                              const Block<int32_t>& dtypeOper,
                              const TaQLStyle& style)
: TableExprNodeArray (dtype, OtFunc),
  node_p      (ftype, dtype, vtype, source, nodes, dtypeOper),
  origin_p    (style.origin()),
  isCOrder_p  (style.isCOrder()),
  constAxes_p (false),
  constAlt_p  (false)
{
  exprtype_p = node_p.exprType();
  unit_p     = node_p.unit();
  tryToConst();
}

TableExprFuncNodeArray::~TableExprFuncNodeArray()
{}

void TableExprFuncNodeArray::flattenTree (std::vector<TableExprNodeRep*>& nodes)
{
  nodes.push_back (this);
  node_p.flattenTree (nodes);
}

void TableExprFuncNodeArray::tryToConst()
{
    int32_t axarg = 1;
    switch (funcType()) {
    case TableExprFuncNode::shapeFUNC:
        if (operands()[0]->ndim() == 0
        ||  operands()[0]->shape().size() > 0  ) {
            exprtype_p = Constant;
        }
        break;
    case TableExprFuncNode::arrfractilesFUNC:
        axarg = 2;
        CASACORE_FALLTHROUGH;
    case TableExprFuncNode::arrsumsFUNC:
    case TableExprFuncNode::arrproductsFUNC:
    case TableExprFuncNode::arrsumsqrsFUNC:
    case TableExprFuncNode::arrminsFUNC:
    case TableExprFuncNode::arrmaxsFUNC:
    case TableExprFuncNode::arrmeansFUNC:
    case TableExprFuncNode::arrvariances0FUNC:
    case TableExprFuncNode::arrvariances1FUNC:
    case TableExprFuncNode::arrstddevs0FUNC:
    case TableExprFuncNode::arrstddevs1FUNC:
    case TableExprFuncNode::arravdevsFUNC:
    case TableExprFuncNode::arrrmssFUNC:
    case TableExprFuncNode::arrmediansFUNC:
    case TableExprFuncNode::arranysFUNC:
    case TableExprFuncNode::arrallsFUNC:
    case TableExprFuncNode::arrntruesFUNC:
    case TableExprFuncNode::arrnfalsesFUNC:
    case TableExprFuncNode::areverseFUNC:
        if (operands()[axarg]->isConstant()) {
            ipos_p = getAxes (0, -1, axarg);
            constAxes_p = true;
        }
        break;
    case TableExprFuncNode::diagonalFUNC:
        if (operands()[axarg]->isConstant()) {
            getDiagonalArg (0, IPosition());
            constAxes_p = true;
        }
        break;
    case TableExprFuncNode::resizeFUNC:
        if (operands().size() < 3  ||  operands()[2]->isConstant()) {
            getAlternate (0);
            constAlt_p = true;
        }
        // fall through
    case TableExprFuncNode::arrayFUNC:
        if (operands()[axarg]->isConstant()) {
          getArrayShape (0, axarg);            // fills ipos_p
            constAxes_p = true;
        }
        break;
    case TableExprFuncNode::transposeFUNC:
        if (operands()[axarg]->isConstant()) {
            ipos_p = getAxes (0, -1, axarg, false);
            constAxes_p = true;
        }
        break;
    case TableExprFuncNode::nullarrayFUNC:
        exprtype_p = Constant;
        break;
    case TableExprFuncNode::marrayFUNC:
    case TableExprFuncNode::arrdataFUNC:
    case TableExprFuncNode::arrmaskFUNC:
    case TableExprFuncNode::negatemaskFUNC:
    case TableExprFuncNode::replmaskedFUNC:
    case TableExprFuncNode::replunmaskedFUNC:
    case TableExprFuncNode::arrflatFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            ipos_p = IPosition(1,1);
            constAxes_p = true;
        }
        break;
    default:
        break;
    }
}


IPosition TableExprFuncNodeArray::getAxes (const TableExprId& id,
                                           int32_t ndim, uint32_t axarg,
                                           bool swapRemove)
{
  // Get the axes if not constant (or not known).
  if (!constAxes_p) {
    Array<int64_t> ax(operands()[axarg]->getArrayInt(id).array());
    AlwaysAssert (ax.ndim() == 1, AipsError);
    AlwaysAssert (ax.contiguousStorage(), AipsError);
    ipos_p.resize (ax.size());
    for (uint32_t i=0; i<ax.size(); i++) {
      ipos_p(i) = ax.data()[i] - origin_p;
    }
    iposN_p = ipos_p;
  }
  // Check if an axis exceeds the dimensionality.
  uint32_t nr = 0;
  for (uint32_t i=0; i<ipos_p.size(); i++) {
    if (ipos_p(i) < 0) {
        throw TableInvExpr ("axis < 0 used in xxxs function");
    }
    if (ndim < 0) {
      nr = ipos_p.size();
    } else {
      if (ipos_p(i) < ndim) {
        // Correct for possible specification in C-order.
        // Note that for collapse the axes order is not important,
        // but it is for transpose.
        if (isCOrder_p && swapRemove) {
          ipos_p(i) = ndim - iposN_p(i) - 1;
        }
        nr++;
      }
    }
  }
  if (nr == ipos_p.size()  ||  !swapRemove) {
    return ipos_p;
  }
  // Remove axes exceeding dimensionality.
  return removeAxes (ipos_p, ndim);
}

IPosition TableExprFuncNodeArray::removeAxes (const IPosition& axes,
                                              int32_t ndim) const
{
  // Count axes not exceeding ndim.
  uint32_t nr=0;
  for (uint32_t i=0; i<axes.size(); ++i) {
    if (axes[i] < ndim) {
      nr++;
    }
  }
  if (nr == axes.size()) {
    return axes;
  }
  // Remove the too high axes.
  IPosition newAxes(nr);
  uint32_t j=0;
  for (uint32_t i=0; i<axes.size(); ++i) {
    if (ipos_p[i] < ndim) {
      newAxes[j++] = ipos_p[i];
    }
  }
  return newAxes;
}
                           
const IPosition& TableExprFuncNodeArray::getArrayShape (const TableExprId& id,
                                                        uint32_t axarg)
{
  // Get the shape if not constant.
  if (!constAxes_p) {
    Array<int64_t> ax(operands()[axarg]->getArrayInt(id).array());
    AlwaysAssert (ax.ndim() == 1, AipsError);
    AlwaysAssert (ax.contiguousStorage(), AipsError);
    uint32_t ndim = ax.size();
    ipos_p.resize (ndim);
    if (isCOrder_p) {
      for (uint32_t i=0; i<ndim; i++) {
        ipos_p(i) = ax.data()[ndim-i-1];
      }
    } else {
      for (uint32_t i=0; i<ndim; i++) {
        ipos_p(i) = ax.data()[i];
      }
    }
  }
  return ipos_p;
}

IPosition TableExprFuncNodeArray::getOrder (const TableExprId& id, int32_t ndim)
{
  IPosition order = getAxes(id, ndim, 1, false);
  if (order.empty()) {
    // Default is to transpose the full array.
    order.resize (ndim);
    for (int32_t i=0; i<ndim; ++i) {
      order[i] = ndim-i-1;
    }
    return order;
  }
  // Remove possibly too high axes.
  IPosition ord = removeAxes (order, ndim);
  if (!isCOrder_p) {
    return ord;
  }
  // Take care of correct mapping for Python style.
  // Unspecified axes have to be added first.
  IPosition ordf = IPosition::makeAxisPath (ndim, ord);
  IPosition nord(ordf.size());
  for (uint32_t i=0; i<ordf.size(); ++i) {
    nord[i] = ndim - ordf[ordf.size()-i-1] - 1;
  }
  return nord;
}

IPosition TableExprFuncNodeArray::getReverseAxes (const TableExprId& id, uint32_t ndim)
{
  IPosition axes = getAxes(id, ndim);
  if (axes.empty()) {
    // Default is to reverse the full array.
    axes.resize (ndim);
    for (uint32_t i=0; i<ndim; ++i) {
      axes[i] = i;
    }
  }
  return axes;
}

const IPosition& TableExprFuncNodeArray::getDiagonalArg (const TableExprId& id,
                                                         const IPosition& shp)
{
  // Get the arguments if not constant (or not known).
  if (!constAxes_p) {
    Array<int64_t> ax(operands()[1]->getArrayInt(id).array());
    AlwaysAssert (ax.ndim() == 1, AipsError);
    AlwaysAssert (ax.contiguousStorage(), AipsError);
    if (ax.size() > 0) {
      ipos_p.resize (2);
      ipos_p[0] = ax.data()[0] - origin_p;     // firstAxis
      ipos_p[1] = 0;
      if (ax.size() > 1) {
        ipos_p[1] = ax.data()[1];              // diag
      }
      iposN_p = ipos_p;
    }
  }
  // If there is a real array, check the arguments.
  if (shp.size() > 0) {
    // Check the arguments and reverse C-order if needed.
    // The diagonals are taken for two consecutive axes.
    // If the axes are given in C-order, the user has given the last axis,
    // so we have to subtract one extra.
    // Use defaults if no arguments given.
    if (iposN_p.empty()) {
      ipos_p.resize (2);
      ipos_p[0] = ipos_p[1] = 0;
    } else if (isCOrder_p) {
      ipos_p[0] = shp.size() - iposN_p[0] - 2;
    }
    if (ipos_p[0] < 0  ||  ipos_p[0] >= int32_t(shp.size())-1) {
      throw TableInvExpr ("Diagonals axes outside array with ndim=" +
                          String::toString(shp.size()));
    }
    if (shp[ipos_p[0]] != shp[ipos_p[0]+1]) {
      throw TableInvExpr ("Diagonals axis " + String::toString(ipos_p[0]) +
                          " and " + String::toString(ipos_p[0]+1) +
                          " should have equal length");
    }
    // Set offset to last one if exceeding.
    if (abs(ipos_p[1]) > shp[ipos_p[0]] - 1) {
      ipos_p[1] = shp[ipos_p[0]] - 1;
      if (iposN_p[1] < 0) {
        ipos_p[1] = -ipos_p[1];
      }
    }
  }
  return ipos_p;
}

const IPosition& TableExprFuncNodeArray::getAlternate (const TableExprId& id)
{
  // Only do it if not constant or known.
  if (! constAlt_p) {
    if (operands().size() < 3) {
      expandAlt_p = IPosition();    // normal resize
    } else {
      if (operands()[2]->valueType() == VTScalar) {
        // A scalar is true for all axes.
        // The dimensionality is unknown, so make it very large to cover all.
        expandAlt_p = IPosition(20, operands()[2]->getInt(id));
      } else {
        Array<int64_t> arr(operands()[2]->getArrayInt(id).array());
        expandAlt_p.resize (arr.size());
        if (isCOrder_p) {
          for (uint32_t i=0; i<arr.size(); ++i) {
            expandAlt_p[i] = arr.data()[arr.size() - i - 1];
          }
        } else {
          for (uint32_t i=0; i<arr.size(); ++i) {
            expandAlt_p[i] = arr.data()[i];
          }
        }
      }
    }
  }
  return expandAlt_p;
}

IPosition TableExprFuncNodeArray::adjustShape (const IPosition& shape,
                                               const IPosition& origShape) const
{
  // Set axis < 0 to original shape (if present) or 1.
  IPosition shp(shape);
  for (uint32_t i=0; i<shp.size(); ++i) {
    if (shp[i] < 0) {
      if (i < origShape.size()) {
        shp[i] = origShape[i];
      } else {
        shp[i] = 1;
      }
    }
  }
  return shp;
}

MArray<double> TableExprFuncNodeArray::angdistx (const MArray<double>& a1,
                                                 const MArray<double>& a2) const
{
  Array<double>::const_iterator end1 = a1.array().end();
  Array<double>::const_iterator end2 = a2.array().end();
  Array<double> result(IPosition(2, a1.size()/2, a2.size()/2));
  double* res = result.data();
  for (Array<double>::const_iterator p2 = a2.array().begin();
       p2!=end2; ++p2) {
    double ra2     = *p2;
    ++p2;
    double sindec2 = sin(*p2);
    double cosdec2 = cos(*p2);
    for (Array<double>::const_iterator p1 = a1.array().begin();
         p1!=end1; ++p1) {
      double ra1 = *p1;
      ++p1;
      *res++ = acos (sin(*p1)*sindec2 + cos(*p1)*cosdec2*cos(ra1-ra2));
    }
  }
  /// deal with possible mask
  return MArray<double>(result);
}


MArray<bool> TableExprFuncNodeArray::getArrayBool (const TableExprId& id)
{
    switch (funcType()) {
    case TableExprFuncNode::boolFUNC:
      if (operands()[0]->dataType() == NTBool) {
        return operands()[0]->getArrayBool(id);
      } else if (operands()[0]->dataType() == NTInt) {
        return (operands()[0]->getArrayInt(id) != int64_t(0));
      } else if (operands()[0]->dataType() == NTDouble) {
        return (operands()[0]->getArrayDouble(id) != 0.);
      } else if (operands()[0]->dataType() == NTComplex) {
        return (operands()[0]->getArrayDComplex(id) != DComplex());
      } else if (operands()[0]->dataType() == NTDate) {
        return (operands()[0]->getArrayDouble(id) != 0.);
      } else {
        MArray<String> values = operands()[0]->getArrayString(id);
        Array<bool> res(values.shape());
        Array<String>::const_iterator in = values.array().begin();
        for (Array<bool>::contiter out=res.cbegin();
             out!=res.cend(); ++out, ++in) {
          *out = TableExprFuncNode::string2Bool (*in);
        }
        return MArray<bool> (res, values);
      }
    case TableExprFuncNode::near2FUNC:
        if (argDataType() == NTDouble) {
            if (operands()[0]->valueType() == VTScalar) {
                return near (operands()[0]->getDouble(id),
                             operands()[1]->getArrayDouble(id),
                             1.0e-13);
            } else if (operands()[1]->valueType() == VTScalar) {
                return near (operands()[0]->getArrayDouble(id),
                             operands()[1]->getDouble(id),
                             1.0e-13);
            } else {
                return near (operands()[0]->getArrayDouble(id),
                             operands()[1]->getArrayDouble(id),
                             1.0e-13);
            }
        }
        if (operands()[0]->valueType() == VTScalar) {
            return near (operands()[0]->getDComplex(id),
                         operands()[1]->getArrayDComplex(id),
                         1.0e-13);
        } else if (operands()[1]->valueType() == VTScalar) {
            return near (operands()[0]->getArrayDComplex(id),
                         operands()[1]->getDComplex(id),
                         1.0e-13);
        } else {
            return near (operands()[0]->getArrayDComplex(id),
                         operands()[1]->getArrayDComplex(id),
                         1.0e-13);
        }
    case TableExprFuncNode::near3FUNC:
        if (argDataType() == NTDouble) {
            if (operands()[0]->valueType() == VTScalar) {
                return near (operands()[0]->getDouble(id),
                             operands()[1]->getArrayDouble(id),
                             operands()[2]->getDouble(id));
            } else if (operands()[1]->valueType() == VTScalar) {
                return near (operands()[0]->getArrayDouble(id),
                             operands()[1]->getDouble(id),
                             operands()[2]->getDouble(id));
            } else {
                return near (operands()[0]->getArrayDouble(id),
                             operands()[1]->getArrayDouble(id),
                             operands()[2]->getDouble(id));
            }
        }
        if (operands()[0]->valueType() == VTScalar) {
            return near (operands()[0]->getDComplex(id),
                         operands()[1]->getArrayDComplex(id),
                         operands()[2]->getDouble(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return near (operands()[0]->getArrayDComplex(id),
                         operands()[1]->getDComplex(id),
                         operands()[2]->getDouble(id));
        } else {
            return near (operands()[0]->getArrayDComplex(id),
                         operands()[1]->getArrayDComplex(id),
                         operands()[2]->getDouble(id));
        }
    case TableExprFuncNode::nearabs2FUNC:
        if (argDataType() == NTDouble) {
            if (operands()[0]->valueType() == VTScalar) {
                return nearAbs (operands()[0]->getDouble(id),
                                operands()[1]->getArrayDouble(id),
                                1.0e-13);
            } else if (operands()[1]->valueType() == VTScalar) {
                return nearAbs (operands()[0]->getArrayDouble(id),
                                operands()[1]->getDouble(id),
                                1.0e-13);
            } else {
                return nearAbs (operands()[0]->getArrayDouble(id),
                                operands()[1]->getArrayDouble(id),
                                1.0e-13);
            }
        }
        if (operands()[0]->valueType() == VTScalar) {
            return nearAbs (operands()[0]->getDComplex(id),
                            operands()[1]->getArrayDComplex(id),
                            1.0e-13);
        } else if (operands()[1]->valueType() == VTScalar) {
            return nearAbs (operands()[0]->getArrayDComplex(id),
                            operands()[1]->getDComplex(id),
                            1.0e-13);
        } else {
            return nearAbs (operands()[0]->getArrayDComplex(id),
                            operands()[1]->getArrayDComplex(id),
                            1.0e-13);
        }
    case TableExprFuncNode::nearabs3FUNC:
        if (argDataType() == NTDouble) {
            if (operands()[0]->valueType() == VTScalar) {
                return nearAbs (operands()[0]->getDouble(id),
                                operands()[1]->getArrayDouble(id),
                                operands()[2]->getDouble(id));
            } else if (operands()[1]->valueType() == VTScalar) {
                return nearAbs (operands()[0]->getArrayDouble(id),
                                operands()[1]->getDouble(id),
                                operands()[2]->getDouble(id));
            } else {
                return nearAbs (operands()[0]->getArrayDouble(id),
                                operands()[1]->getArrayDouble(id),
                                operands()[2]->getDouble(id));
            }
        }
        if (operands()[0]->valueType() == VTScalar) {
            return nearAbs (operands()[0]->getDComplex(id),
                            operands()[1]->getArrayDComplex(id),
                            operands()[2]->getDouble(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return nearAbs (operands()[0]->getArrayDComplex(id),
                            operands()[1]->getDComplex(id),
                            operands()[2]->getDouble(id));
        } else {
            return nearAbs (operands()[0]->getArrayDComplex(id),
                            operands()[1]->getArrayDComplex(id),
                            operands()[2]->getDouble(id));
        }
    case TableExprFuncNode::arranysFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return partialAnys (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrallsFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return partialAlls (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runallFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return slidingAlls (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runanyFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return slidingAnys (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxallFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return boxedAlls (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxanyFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return boxedAnys (arr, getArrayShape(id));
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
        Array<bool> res(shp);
        Array<bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
          res = operands()[0]->getBool(id);
        } else {
          MArray<bool> arr (operands()[0]->getArrayBool(id));
          if (arr.isNull()) {
            return arr;
          }
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
        }
        return MArray<bool> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return reorderArray (arr, getOrder(id, arr.ndim()), false);
      }
    case TableExprFuncNode::areverseFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        return reverseArray (arr, getReverseAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::diagonalFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        if (arr.isNull()) {
          return arr;
        }
        const IPosition parms = getDiagonalArg (id, arr.shape());
        if (arr.hasMask()) {
          return MArray<bool>(arr.array().diagonals(parms[0], parms[1]),
                              arr.mask().diagonals(parms[0], parms[1]));
        }
        return MArray<bool>(arr.array().diagonals(parms[0], parms[1]));
      }
    case TableExprFuncNode::resizeFUNC:
        return TEFResize (operands()[0]->getArrayBool(id), id);
    case TableExprFuncNode::isnanFUNC:
        if (argDataType() == NTComplex) {
            return isNaN (operands()[0]->getArrayDComplex(id));
        } else {
            return isNaN (operands()[0]->getArrayDouble(id));
        }
    case TableExprFuncNode::isinfFUNC:
        if (argDataType() == NTComplex) {
            return isInf (operands()[0]->getArrayDComplex(id));
        } else {
            return isInf (operands()[0]->getArrayDouble(id));
        }
    case TableExprFuncNode::isfiniteFUNC:
        if (argDataType() == NTComplex) {
            return isFinite (operands()[0]->getArrayDComplex(id));
        } else {
            return isFinite (operands()[0]->getArrayDouble(id));
        }
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<bool> (operands(), id);
    case TableExprFuncNode::nullarrayFUNC:
        return MArray<bool>();
    case TableExprFuncNode::marrayFUNC:
        return MArray<bool> (operands()[0]->getBoolAS(id),
                             operands()[1]->getBoolAS(id));
    case TableExprFuncNode::arrdataFUNC:
      {
        MArray<bool> arr(operands()[0]->getBoolAS(id).array());
        return arr.isNull()  ?  arr : MArray<bool> (arr.array());
      }
    case TableExprFuncNode::negatemaskFUNC:
        return TEFMASKneg (operands()[0]->getBoolAS(id));
    case TableExprFuncNode::replmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getBoolAS(id), operands()[1],
                            id, true);
    case TableExprFuncNode::replunmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getBoolAS(id), operands()[1],
                            id, false);
    case TableExprFuncNode::arrflatFUNC:
        return MArray<bool> (operands()[0]->getBoolAS(id).flatten());
    case TableExprFuncNode::arrmaskFUNC:
      {
        IPosition shp;
        bool isNull = false;
        switch (operands()[0]->dataType()) {
        case NTBool:
          {
            MArray<bool> arr (operands()[0]->getBoolAS(id).mask());
            if (arr.hasMask()) {
              return MArray<bool> (arr.mask());
            }
            shp = arr.shape();
            isNull = arr.isNull();
            break;
          }
        case NTInt:
          {
            MArray<int64_t> arr (operands()[0]->getIntAS(id));
            if (arr.hasMask()) {
              return MArray<bool> (arr.mask());
            }
            shp = arr.shape();
            isNull = arr.isNull();
            break;
          }
        case NTDouble:
          {
            MArray<double> arr (operands()[0]->getDoubleAS(id));
            if (arr.hasMask()) {
              return MArray<bool> (arr.mask());
            }
            shp = arr.shape();
            isNull = arr.isNull();
            break;
          }
        case NTComplex:
          {
            MArray<DComplex> arr (operands()[0]->getDComplexAS(id));
            if (arr.hasMask()) {
              return MArray<bool> (arr.mask());
            }
            shp = arr.shape();
            isNull = arr.isNull();
            break;
          }
        case NTString:
          {
            MArray<String> arr (operands()[0]->getStringAS(id));
            if (arr.hasMask()) {
              return MArray<bool> (arr.mask());
            }
            shp = arr.shape();
            isNull = arr.isNull();
            break;
          }
        case NTDate:
          {
            MArray<MVTime> arr (operands()[0]->getDateAS(id));
            if (arr.hasMask()) {
              return MArray<bool> (arr.mask());
            }
            shp = arr.shape();
            isNull = arr.isNull();
            break;
          }
        default:
            throw TableInvExpr ("TableExprFuncNodeArray::getArrayBool, "
                                "unknown datatype in mask function");
        }
        if (isNull) {
          return MArray<bool>();
        }
        Array<bool> mask(shp);
        mask = false;
        return MArray<bool> (mask);
      }
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayBool, "
                        "unknown function " +
                        String::toString(funcType()));
}

MArray<int64_t> TableExprFuncNodeArray::getArrayInt (const TableExprId& id)
{
    switch (funcType()) {
    case TableExprFuncNode::squareFUNC:
    case TableExprFuncNode::normFUNC:
        return square (operands()[0]->getArrayInt(id));
    case TableExprFuncNode::cubeFUNC:
        return cube (operands()[0]->getArrayInt(id));
    case TableExprFuncNode::absFUNC:
        return abs (operands()[0]->getArrayInt(id));
    case TableExprFuncNode::intFUNC:
        if (operands()[0]->dataType() == NTString) {
            MArray<String> values (operands()[0]->getArrayString(id));
            Array<int64_t> res(values.shape());
            Array<String>::const_iterator in = values.array().begin();
            for (Array<int64_t>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = TableExprFuncNode::string2Int (*in);
            }
            return MArray<int64_t> (res, values);
        } else if (operands()[0]->dataType() == NTBool) {
            MArray<bool> values (operands()[0]->getArrayBool(id));
            Array<int64_t> res(values.shape());
            Array<bool>::const_iterator in = values.array().begin();
            for (Array<int64_t>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = *in ? 1:0;
            }
            return MArray<int64_t> (res, values);
        } else if (argDataType() == NTDouble) {
            MArray<double> val (operands()[0]->getArrayDouble(id));
            Array<int64_t> arr(val.shape());
            convertArray (arr, val.array());
            return MArray<int64_t> (arr, val);
        }
        return operands()[0]->getArrayInt(id);
    case TableExprFuncNode::signFUNC:
        return sign(operands()[0]->getArrayInt(id));
    case TableExprFuncNode::roundFUNC:
    case TableExprFuncNode::floorFUNC:
    case TableExprFuncNode::ceilFUNC:
        return operands()[0]->getArrayInt(id);
    case TableExprFuncNode::shapeFUNC:
      {
        IPosition shp (operands()[0]->shape(id));
        int32_t n = shp.size();
        Array<int64_t> result(IPosition(1,n));
        int64_t* res = result.data();
        if (isCOrder_p) {
            for (int32_t i=0; i<n; ++i) {
                res[i] = shp[n-i-1];
            }
        } else {
            for (int32_t i=0; i<n; ++i) {
                res[i] = shp[i];
            }
        }
        return MArray<int64_t>(result);
      }
    case TableExprFuncNode::strlengthFUNC:
      {
        MArray<String> values (operands()[0]->getArrayString(id));
        Array<int64_t> res(values.shape());
        bool deleteVal, deleteRes;
        const String* val = values.array().getStorage (deleteVal);
        int64_t* resp = res.getStorage (deleteRes);
        size_t n = values.size();
        for (size_t i=0; i<n; i++) {
            resp[i] = val[i].length();
        }
        values.array().freeStorage (val, deleteVal);
        res.putStorage (resp, deleteRes);
        return MArray<int64_t> (res, values);
      }
    case TableExprFuncNode::yearFUNC:
    case TableExprFuncNode::monthFUNC:
    case TableExprFuncNode::dayFUNC:
    case TableExprFuncNode::weekdayFUNC:
    case TableExprFuncNode::weekFUNC:
      {
        MArray<MVTime> values (operands()[0]->getArrayDate(id));
        Array<int64_t> res(values.shape());
        bool deleteVal, deleteRes;
        const MVTime* val = values.array().getStorage (deleteVal);
        int64_t* resp = res.getStorage (deleteRes);
        size_t n = values.size();
        switch (funcType()) {
        case TableExprFuncNode::yearFUNC:
            for (size_t i=0; i<n; i++) {
                resp[i] = val[i].year();
            }
            break;
        case TableExprFuncNode::monthFUNC:
            for (size_t i=0; i<n; i++) {
                resp[i] = val[i].month();
            }
            break;
        case TableExprFuncNode::dayFUNC:
            for (size_t i=0; i<n; i++) {
                resp[i] = val[i].monthday();
            }
            break;
        case TableExprFuncNode::weekdayFUNC:
            for (size_t i=0; i<n; i++) {
                resp[i] = val[i].weekday();
            }
            break;
        case TableExprFuncNode::weekFUNC:
            for (size_t i=0; i<n; i++) {
                resp[i] = val[i].yearweek();
            }
            break;
        default:
            throw TableInvExpr ("TableExprFuncNodeArray::getArrayInt, "
                                "unhandled date/time function " +
                                String::toString(funcType()));
        }
        values.array().freeStorage (val, deleteVal);
        res.putStorage (resp, deleteRes);
        return MArray<int64_t> (res, values);
      }
    case TableExprFuncNode::minFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return min (operands()[1]->getArrayInt(id),
                        operands()[0]->getInt(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return min (operands()[0]->getArrayInt(id),
                        operands()[1]->getInt(id));
        } else {
            return min (operands()[0]->getArrayInt(id),
                        operands()[1]->getArrayInt(id));
        }
    case TableExprFuncNode::maxFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return max (operands()[1]->getArrayInt(id),
                        operands()[0]->getInt(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return max (operands()[0]->getArrayInt(id),
                        operands()[1]->getInt(id));
        } else {
            return max (operands()[0]->getArrayInt(id),
                        operands()[1]->getArrayInt(id));
        }
    case TableExprFuncNode::fmodFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return operands()[0]->getInt(id) %
                   operands()[1]->getArrayInt(id);
        } else if (operands()[1]->valueType() == VTScalar) {
            return operands()[0]->getArrayInt(id) %
                   operands()[1]->getInt(id);
        } else {
            return operands()[0]->getArrayInt(id) %
                   operands()[1]->getArrayInt(id);
        }
    case TableExprFuncNode::arrsumsFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrminsFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return partialMins (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmaxsFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return partialMaxs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runsumFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return slidingSums (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runproductFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return slidingProducts (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runsumsqrFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return slidingSumSqrs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runminFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return slidingMins (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmaxFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return slidingMaxs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxsumFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return boxedSums (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxproductFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return boxedProducts (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxsumsqrFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return boxedSumSqrs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxminFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return boxedMins (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmaxFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return boxedMaxs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::arrntruesFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        MArray<size_t> res(partialNTrue (arr, getAxes(id, arr.ndim())));
        Array<int64_t> resd(res.shape());
        convertArray (resd, res.array());
        return MArray<int64_t> (resd, res);
      }
    case TableExprFuncNode::runntrueFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        MArray<uint32_t> res(slidingNTrue (arr, getArrayShape(id)));
        Array<int64_t> resd(res.shape());
        convertArray (resd, res.array());
        return MArray<int64_t> (resd, res);
      }
    case TableExprFuncNode::boxntrueFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        MArray<uint32_t> res(boxedNTrue (arr, getArrayShape(id)));
        Array<int64_t> resd(res.shape());
        convertArray (resd, res.array());
        return MArray<int64_t> (resd, res);
      }
    case TableExprFuncNode::arrnfalsesFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        MArray<size_t> res(partialNFalse (arr, getAxes(id, arr.ndim())));
        Array<int64_t> resd(res.shape());
        convertArray (resd, res.array());
        return MArray<int64_t> (resd, res);
      }
    case TableExprFuncNode::runnfalseFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        MArray<uint32_t> res(slidingNFalse (arr, getArrayShape(id)));
        Array<int64_t> resd(res.shape());
        convertArray (resd, res.array());
        return MArray<int64_t> (resd, res);
      }
    case TableExprFuncNode::boxnfalseFUNC:
      {
        MArray<bool> arr (operands()[0]->getArrayBool(id));
        MArray<uint32_t> res(boxedNFalse (arr, getArrayShape(id)));
        Array<int64_t> resd(res.shape());
        convertArray (resd, res.array());
        return MArray<int64_t> (resd, res);
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
        Array<int64_t> res(shp);
        Array<bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
          res = operands()[0]->getInt(id);
        } else {
          MArray<int64_t> arr (operands()[0]->getArrayInt(id));
          if (arr.isNull()) {
            return arr;
          }
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
        }
        return MArray<int64_t> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return reorderArray (arr, getOrder(id, arr.ndim()), false);
      }
    case TableExprFuncNode::areverseFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        return reverseArray (arr, getReverseAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::diagonalFUNC:
      {
        MArray<int64_t> arr (operands()[0]->getArrayInt(id));
        if (arr.isNull()) {
          return arr;
        }
        const IPosition parms = getDiagonalArg (id, arr.shape());
        if (arr.hasMask()) {
          return MArray<int64_t>(arr.array().diagonals(parms[0], parms[1]),
                               arr.mask().diagonals(parms[0], parms[1]));
        }
        return MArray<int64_t>(arr.array().diagonals(parms[0], parms[1]));
      }
    case TableExprFuncNode::resizeFUNC:
        return TEFResize (operands()[0]->getArrayInt(id), id);
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<int64_t> (operands(), id);
    case TableExprFuncNode::nullarrayFUNC:
        return MArray<int64_t>();
    case TableExprFuncNode::marrayFUNC:
        return MArray<int64_t> (operands()[0]->getIntAS(id),
                              operands()[1]->getBoolAS(id));
    case TableExprFuncNode::arrdataFUNC:
      {
        MArray<int64_t> arr(operands()[0]->getIntAS(id).array());
        return arr.isNull()  ?  arr : MArray<int64_t> (arr.array());
      }
    case TableExprFuncNode::negatemaskFUNC:
        return TEFMASKneg (operands()[0]->getIntAS(id));
    case TableExprFuncNode::replmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getIntAS(id), operands()[1],
                            id, true);
    case TableExprFuncNode::replunmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getIntAS(id), operands()[1],
                            id, false);
    case TableExprFuncNode::arrflatFUNC:
        return MArray<int64_t> (operands()[0]->getIntAS(id).flatten());
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayInt, "
                        "unknown function " +
                        String::toString(funcType()));
}

MArray<double> TableExprFuncNodeArray::getArrayDouble (const TableExprId& id)
{
    if (dataType() == NTInt) {
        return TableExprNodeArray::getArrayDouble (id);
    }
    // Delta degrees of freedom for variance/stddev.
    uint32_t ddof = 1;
    switch (funcType()) {
    case TableExprFuncNode::sinFUNC:
        return sin      (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::sinhFUNC:
        return sinh     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::cosFUNC:
        return cos      (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::coshFUNC:
        return cosh     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::expFUNC:
        return exp      (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::logFUNC:
        return log      (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::log10FUNC:
        return log10    (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::squareFUNC:
        return square   (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::cubeFUNC:
        return cube     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::sqrtFUNC:
      {
        MArray<double> res = sqrt (operands()[0]->getArrayDouble(id));
        if (node_p.getScale() != 1.) {
          // Note: in this way arr references the array in res.
          Array<double> arr(res.array());
          arrayTransformInPlace (arr, node_p.getScale(),
                                 casacore::Multiplies<double,double>());
        }
        return res;
      }
    case TableExprFuncNode::conjFUNC:
        return           operands()[0]->getArrayDouble(id);
    case TableExprFuncNode::normFUNC:
        if (argDataType() == NTDouble) {
            return square (operands()[0]->getArrayDouble(id));
        } else {
            MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
            Array<double> result(arr.shape());
            bool deleteArr, deleteRes;
            const DComplex* data = arr.array().getStorage (deleteArr);
            double* res = result.getStorage (deleteRes);
            size_t nr = arr.size();
            for (size_t i=0; i<nr; i++) {
                res[i] = norm(data[i]);
            }
            arr.array().freeStorage (data, deleteArr);
            result.putStorage (res, deleteRes);
            return MArray<double> (result, arr);
        }
    case TableExprFuncNode::absFUNC:
        if (argDataType() == NTDouble) {
            return abs (operands()[0]->getArrayDouble(id));
        }
        return amplitude (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::argFUNC:
        if (argDataType() == NTDouble) {
            MArray<double> marr (operands()[0]->getArrayDouble(id));
            Array<double> arr(marr.array().copy());
            bool deleteIt;
            double* data = arr.getStorage (deleteIt);
            size_t nr = arr.size();
            for (size_t i=0; i<nr; i++) {
                if (data[i] >= 0) {
                    data[i] = 0;
                } else {
                    data[i] = C::pi;
                }
            }
            arr.putStorage (data, deleteIt);
            return MArray<double> (arr, marr);
        }
        return phase (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::realFUNC:
        if (operands()[0]->dataType() == NTString) {
            MArray<String> values (operands()[0]->getArrayString(id));
            Array<double> res(values.shape());
            Array<String>::const_iterator in = values.array().begin();
            for (Array<double>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = TableExprFuncNode::string2Real (*in);
            }
            return MArray<double> (res, values);
        } else if (operands()[0]->dataType() == NTBool) {
            MArray<bool> values (operands()[0]->getArrayBool(id));
            Array<double> res(values.shape());
            Array<bool>::const_iterator in = values.array().begin();
            for (Array<double>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = *in ? 1:0;
            }
            return MArray<double> (res, values);
        } else if (argDataType() == NTDouble) {
            return operands()[0]->getArrayDouble(id);
        }
        return real (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::imagFUNC:
        if (argDataType() == NTDouble) {
            MArray<double> arr (operands()[0]->getArrayDouble(id));
            Array<double> result(arr.shape());
            result = 0.;
            return MArray<double> (result, arr);
        }
        return imag (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::asinFUNC:
        return asin     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::acosFUNC:
        return acos     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::atanFUNC:
        return atan     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::tanFUNC:
        return tan      (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::tanhFUNC:
        return tanh     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::signFUNC:
        return sign     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::roundFUNC:
        return round    (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::floorFUNC:
        return floor    (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::ceilFUNC:
        return ceil     (operands()[0]->getArrayDouble(id));
    case TableExprFuncNode::mjdFUNC:
    case TableExprFuncNode::timeFUNC:
      {
        MArray<MVTime> values (operands()[0]->getArrayDate(id));
        Array<double> doubles(values.shape());
        bool deleteVal, deleteDoub;
        const MVTime* val = values.array().getStorage (deleteVal);
        double* doub = doubles.getStorage (deleteDoub);
        size_t n = values.size();
        if (funcType() == TableExprFuncNode::mjdFUNC) {
            for (size_t i=0; i<n; i++) {
                doub[i] = val[i].day();
            }
        } else {
            for (size_t i=0; i<n; i++) {
                doub[i] = fmod (double(val[i]), 1.) * C::_2pi;   // in radians
            }
        }
        values.array().freeStorage (val, deleteVal);
        doubles.putStorage (doub, deleteDoub);
        return MArray<double> (doubles, values);
      }
    case TableExprFuncNode::powFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return pow (operands()[0]->getDouble(id),
                        operands()[1]->getArrayDouble(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return pow (operands()[0]->getArrayDouble(id),
                        operands()[1]->getDouble(id));
        } else {
            return pow (operands()[0]->getArrayDouble(id),
                        operands()[1]->getArrayDouble(id));
        }
    case TableExprFuncNode::minFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return min (operands()[1]->getArrayDouble(id),
                        operands()[0]->getDouble(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return min (operands()[0]->getArrayDouble(id),
                        operands()[1]->getDouble(id));
        } else {
            return min (operands()[0]->getArrayDouble(id),
                        operands()[1]->getArrayDouble(id));
        }
    case TableExprFuncNode::maxFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return max (operands()[1]->getArrayDouble(id),
                        operands()[0]->getDouble(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return max (operands()[0]->getArrayDouble(id),
                        operands()[1]->getDouble(id));
        } else {
            return max (operands()[0]->getArrayDouble(id),
                        operands()[1]->getArrayDouble(id));
        }
    case TableExprFuncNode::atan2FUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return atan2 (operands()[0]->getDouble(id),
                          operands()[1]->getArrayDouble(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return atan2 (operands()[0]->getArrayDouble(id),
                          operands()[1]->getDouble(id));
        } else {
            return atan2 (operands()[0]->getArrayDouble(id),
                          operands()[1]->getArrayDouble(id));
        }
    case TableExprFuncNode::fmodFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return fmod (operands()[0]->getDouble(id),
                         operands()[1]->getArrayDouble(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return fmod (operands()[0]->getArrayDouble(id),
                         operands()[1]->getDouble(id));
        } else {
            return fmod (operands()[0]->getArrayDouble(id),
                         operands()[1]->getArrayDouble(id));
        }
    case TableExprFuncNode::arrsumsFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrminsFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialMins (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmaxsFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialMaxs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmeansFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialMeans (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrvariances0FUNC:
      ddof = 0;    // fall through
    case TableExprFuncNode::arrvariances1FUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(partialVariances (arr, getAxes(id, arr.ndim()), ddof));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialVariances (arr, getAxes(id, arr.ndim()), ddof);
      }
    case TableExprFuncNode::arrstddevs0FUNC:
      ddof = 0;    // fall through
    case TableExprFuncNode::arrstddevs1FUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(partialStddevs (arr, getAxes(id, arr.ndim()), ddof));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialStddevs (arr, getAxes(id, arr.ndim()), ddof);
      }
    case TableExprFuncNode::arravdevsFUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(partialAvdevs (arr, getAxes(id, arr.ndim())));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialAvdevs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrrmssFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialRmss (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmediansFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialMedians (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrfractilesFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return partialFractiles (arr,
                                 getAxes(id, arr.ndim(), 2),
                                 operands()[1]->getDouble(id));
      }
    case TableExprFuncNode::runsumFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingSums (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runproductFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingProducts (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runsumsqrFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingSumSqrs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runminFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingMins (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmaxFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingMaxs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmeanFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingMeans (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runvariance0FUNC:
      ddof = 0;    // fall through
    case TableExprFuncNode::runvariance1FUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(slidingVariances (arr, getArrayShape(id), ddof));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingVariances (arr, getArrayShape(id), ddof);
      }
    case TableExprFuncNode::runstddev0FUNC:
      ddof = 0;    // fall through
    case TableExprFuncNode::runstddev1FUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(slidingStddevs (arr, getArrayShape(id), ddof));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingStddevs (arr, getArrayShape(id), ddof);
      }
    case TableExprFuncNode::runavdevFUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(slidingAvdevs (arr, getArrayShape(id)));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingAvdevs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runrmsFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingRmss (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmedianFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingMedians (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runfractileFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return slidingFractiles (arr, getArrayShape(id, 2),
                                 operands()[1]->getDouble(id));
      }
    case TableExprFuncNode::boxsumFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedSums (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxproductFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedProducts (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxsumsqrFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedSumSqrs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxminFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedMins (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmaxFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedMaxs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmeanFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedMeans (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxvariance0FUNC:
      ddof = 0;    // fall through
    case TableExprFuncNode::boxvariance1FUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(boxedVariances (arr, getArrayShape(id), ddof));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedVariances (arr, getArrayShape(id), ddof);
      }
    case TableExprFuncNode::boxstddev0FUNC:
      ddof = 0;    // fall through
    case TableExprFuncNode::boxstddev1FUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(boxedStddevs (arr, getArrayShape(id), ddof));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedStddevs (arr, getArrayShape(id), ddof);
      }
    case TableExprFuncNode::boxavdevFUNC:
      {
        if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          return real(boxedAvdevs (arr, getArrayShape(id)));
        }
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedAvdevs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxrmsFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedRmss (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmedianFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedMedians (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxfractileFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return boxedFractiles (arr, getArrayShape(id, 2),
                               operands()[1]->getDouble(id));
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
        Array<double> res(shp);
        Array<bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
          res = operands()[0]->getDouble(id);
        } else {
          MArray<double> arr (operands()[0]->getArrayDouble(id));
          if (arr.isNull()) {
            return arr;
          }
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
        }
        return MArray<double> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return reorderArray (arr, getOrder(id, arr.ndim()), false);
      }
    case TableExprFuncNode::areverseFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        return reverseArray (arr, getReverseAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::diagonalFUNC:
      {
        MArray<double> arr (operands()[0]->getArrayDouble(id));
        if (arr.isNull()) {
          return arr;
        }
        const IPosition parms = getDiagonalArg (id, arr.shape());
        if (arr.hasMask()) {
          return MArray<double>(arr.array().diagonals(parms[0], parms[1]),
                                arr.mask().diagonals(parms[0], parms[1]));
        }
        return MArray<double>(arr.array().diagonals(parms[0], parms[1]));
      }
    case TableExprFuncNode::resizeFUNC:
        return TEFResize (operands()[0]->getArrayDouble(id), id);
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<double> (operands(), id);
    case TableExprFuncNode::nullarrayFUNC:
        return MArray<double>();
    case TableExprFuncNode::marrayFUNC:
        return MArray<double> (operands()[0]->getDoubleAS(id),
                               operands()[1]->getBoolAS(id));
    case TableExprFuncNode::arrdataFUNC:
      {
        MArray<double> arr(operands()[0]->getDoubleAS(id).array());
        return arr.isNull()  ?  arr : MArray<double> (arr.array());
      }
    case TableExprFuncNode::negatemaskFUNC:
        return TEFMASKneg (operands()[0]->getDoubleAS(id));
    case TableExprFuncNode::replmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getDoubleAS(id), operands()[1],
                            id, true);
    case TableExprFuncNode::replunmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getDoubleAS(id), operands()[1],
                            id, false);
    case TableExprFuncNode::arrflatFUNC:
        return MArray<double> (operands()[0]->getDoubleAS(id).flatten());
    case TableExprFuncNode::angdistFUNC:
      {
        MArray<double> a1 = operands()[0]->getArrayDouble(id);
        MArray<double> a2 = operands()[1]->getArrayDouble(id);
        if (!(a1.size() %2 == 0  &&  a2.size() %2 == 0)) {
          throw TableInvExpr ("Arguments of angdist function must have a "
                              "multiple of 2 values");
        }
        // Treat an array of size 2 as scalar, so allow scalar-array operations
        // which is handled by angdistxFUNC.
        if (a1.size() == 2  ||  a2.size() == 2) {
          return angdistx (a1, a2);
        }
        if (a1.size() != a2.size()) {
          throw TableInvExpr ("Arguments of angdist function must have "
                              "equal length");
        }
        Array<double> result(IPosition(1, a1.size()/2));
        double* res = result.data();
        Array<double>::const_iterator p2   = a2.array().begin();
        Array<double>::const_iterator end1 = a1.array().end();
        for (Array<double>::const_iterator p1 = a1.array().begin();
             p1!=end1; ++p1) {
          double ra1 = *p1;
          ++p1;
          double ra2 = *p2;
          ++p2;
          *res++ = acos (sin(*p1)*sin(*p2) + cos(*p1)*cos(*p2)*cos(ra1-ra2));
          ++p2;
        }
        // Reduce possible masks by combining every 2 values.
        Array<bool> mask;
        if (a1.hasMask()) {
          partialArrayMath (mask,
                            a1.mask().reform(IPosition(2, 2, a1.size()/2)),
                            IPosition(1,0),
                            AnyFunc<bool>());
        }
        if (a2.hasMask()) {
          Array<bool> mask2;
          partialArrayMath (mask2,
                            a2.mask().reform(IPosition(2, 2, a2.size()/2)),
                            IPosition(1,0),
                            AnyFunc<bool>());
          if (mask.empty()) {
            mask.reference (mask2);
          } else {
            mask.reference (mask || mask2);
          }
        }
        return MArray<double> (result, mask);
      }
    case TableExprFuncNode::angdistxFUNC:
      {
        MArray<double> a1 = operands()[0]->getArrayDouble(id);
        MArray<double> a2 = operands()[1]->getArrayDouble(id);
        if (!(a1.size() %2 == 0  &&  a2.size() %2 == 0)) {
          throw TableInvExpr ("Arguments of angdistx function must have a "
                              "multiple of 2 values");
        }
        return angdistx (a1, a2);

      }
    case TableExprFuncNode::normangleFUNC:
      {
        MArray<double> values (operands()[0]->getArrayDouble(id));
        Array<double> res(values.shape());
        bool deleteVal, deleteRes;
        const double* val = values.array().getStorage (deleteVal);
        double* resp = res.getStorage (deleteRes);
        size_t n = values.size();
        for (size_t i=0; i<n; i++) {
          double v = fmod(val[i], C::_2pi);
          if (v < -C::pi) v += C::_2pi;
          res[i] = (v <= C::pi  ?  v : v-C::_2pi);
        }
        values.array().freeStorage (val, deleteVal);
        res.putStorage (resp, deleteRes);
        return MArray<double> (res, values);
      }
    case TableExprFuncNode::datetimeFUNC:
    case TableExprFuncNode::mjdtodateFUNC:
    case TableExprFuncNode::dateFUNC:
      {
        MArray<MVTime> arr (getArrayDate(id));
        Array<double> res(arr.shape());
        convertArray (res, arr.array());
        return MArray<double> (res, arr);
      }
    default:
      {
        // Functions like YEAR are implemented as int32_t only.
        MArray<int64_t> arr (getArrayInt(id));
        Array<double> res(arr.shape());
        convertArray (res, arr.array());
        return MArray<double> (res, arr);
      }
    }
    return MArray<double>();
}

MArray<DComplex> TableExprFuncNodeArray::getArrayDComplex
                                                     (const TableExprId& id)
{
    if (dataType() == NTDouble) {
        return TableExprNodeArray::getArrayDComplex (id);
    }
    switch (funcType()) {
    case TableExprFuncNode::sinFUNC:
        return sin      (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::sinhFUNC:
        return sinh     (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::cosFUNC:
        return cos      (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::coshFUNC:
        return cosh     (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::expFUNC:
        return exp      (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::logFUNC:
        return log      (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::log10FUNC:
        return log10    (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::squareFUNC:
        return square   ( operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::cubeFUNC:
        return cube     ( operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::sqrtFUNC:
      {
        MArray<DComplex> res = sqrt (operands()[0]->getArrayDComplex(id));
        if (node_p.getScale() != 1.) {
          arrayTransformInPlace (res.array(), node_p.getScale(),
                                 casacore::Multiplies<DComplex,double>());
        }
        return res;
      }
    case TableExprFuncNode::conjFUNC:
        return conj     (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::powFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return casacore::pow (operands()[0]->getDComplex(id),
                                  operands()[1]->getArrayDComplex(id));
        } else if (operands()[1]->valueType() == VTScalar) {
          if (operands()[1]->dataType() == NTDouble) {
            return casacore::pow (operands()[0]->getArrayDComplex(id),
                                  operands()[1]->getDouble(id));
          } else {
            return casacore::pow (operands()[0]->getArrayDComplex(id),
                                  operands()[1]->getDComplex(id));
          }
        } else {
            return pow (operands()[0]->getArrayDComplex(id),
                        operands()[1]->getArrayDComplex(id));
        }
    case TableExprFuncNode::minFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return min (operands()[0]->getDComplex(id),
                        operands()[1]->getArrayDComplex(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return min (operands()[0]->getArrayDComplex(id),
                        operands()[1]->getDComplex(id));
        } else {
            return min (operands()[0]->getArrayDComplex(id),
                        operands()[1]->getArrayDComplex(id));
        }
    case TableExprFuncNode::maxFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            return max (operands()[0]->getDComplex(id),
                        operands()[1]->getArrayDComplex(id));
        } else if (operands()[1]->valueType() == VTScalar) {
            return max (operands()[0]->getArrayDComplex(id),
                        operands()[1]->getDComplex(id));
        } else {
            return max (operands()[0]->getArrayDComplex(id),
                        operands()[1]->getArrayDComplex(id));
        }
    case TableExprFuncNode::asinFUNC:
        return asin  (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::acosFUNC:
        return acos  (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::atanFUNC:
        return atan  (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::tanFUNC:
        return tan   (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::tanhFUNC:
        return tanh  (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::arrsumsFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runsumFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return slidingSums (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxsumFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return boxedSums (arr, getArrayShape(id));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runproductFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return slidingProducts (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxproductFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return boxedProducts (arr, getArrayShape(id));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runsumsqrFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return slidingSumSqrs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxsumsqrFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return boxedSumSqrs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::arrmeansFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return partialMeans (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runmeanFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return slidingMeans (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmeanFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return boxedMeans (arr, getArrayShape(id));
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
        Array<DComplex> res(shp);
        Array<bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
          res = operands()[0]->getDComplex(id);
        } else {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          if (arr.isNull()) {
            return arr;
          }
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
        }
        return MArray<DComplex> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return reorderArray (arr, getOrder(id, arr.ndim()), false);
      }
    case TableExprFuncNode::areverseFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        return reverseArray (arr, getReverseAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::diagonalFUNC:
      {
        MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
        if (arr.isNull()) {
          return arr;
        }
        const IPosition parms = getDiagonalArg (id, arr.shape());
        if (arr.hasMask()) {
          return MArray<DComplex>(arr.array().diagonals(parms[0], parms[1]),
                                  arr.mask().diagonals(parms[0], parms[1]));
        }
        return MArray<DComplex>(arr.array().diagonals(parms[0], parms[1]));
      }
    case TableExprFuncNode::resizeFUNC:
        return TEFResize (operands()[0]->getArrayDComplex(id), id);
    case TableExprFuncNode::complexFUNC:
      {
        if (operands().size() == 1) {
            MArray<String> values (operands()[0]->getArrayString(id));
            Array<DComplex> res(values.shape());
            Array<String>::const_iterator in = values.array().begin();
            for (Array<DComplex>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = TableExprFuncNode::string2Complex (*in);
            }
            return MArray<DComplex> (res, values);
        }
        if (operands()[0]->valueType() == VTScalar) {
          double val = operands()[0]->getDouble(id);
          MArray<double> arr (operands()[1]->getArrayDouble(id));
          return MArray<DComplex> (makeComplex(val,arr.array()), arr);
        } else if (operands()[1]->valueType() == VTScalar) {
          MArray<double> arr (operands()[0]->getArrayDouble(id));
          double val = operands()[1]->getDouble(id);
          return MArray<DComplex> (makeComplex(arr.array(), val), arr);
        }
        MArray<double> arr1 (operands()[0]->getArrayDouble(id));
        MArray<double> arr2 (operands()[1]->getArrayDouble(id));
        if (arr1.isNull()  ||  arr2.isNull()) {
          return MArray<DComplex>();
        }
        return MArray<DComplex> (makeComplex(arr1.array(), arr2.array()),
                                 arr1.combineMask(arr2));
      }
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<DComplex> (operands(), id);
    case TableExprFuncNode::nullarrayFUNC:
        return MArray<DComplex>();
    case TableExprFuncNode::marrayFUNC:
        return MArray<DComplex> (operands()[0]->getDComplexAS(id),
                                 operands()[1]->getBoolAS(id));
    case TableExprFuncNode::arrdataFUNC:
      {
        MArray<DComplex> arr(operands()[0]->getDComplexAS(id).array());
        return arr.isNull()  ?  arr : MArray<DComplex> (arr.array());
      }
    case TableExprFuncNode::negatemaskFUNC:
        return TEFMASKneg (operands()[0]->getDComplexAS(id));
    case TableExprFuncNode::replmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getDComplexAS(id), operands()[1],
                            id, true);
    case TableExprFuncNode::replunmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getDComplexAS(id), operands()[1],
                            id, false);
    case TableExprFuncNode::arrflatFUNC:
        return MArray<DComplex> (operands()[0]->getDComplexAS(id).flatten());
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayDComplex, "
                        "unknown function " +
                        String::toString(funcType()));
}

MArray<String> TableExprFuncNodeArray::getArrayString (const TableExprId& id)
{
    switch (funcType()) {
    case TableExprFuncNode::upcaseFUNC:
    case TableExprFuncNode::downcaseFUNC:
    case TableExprFuncNode::capitalizeFUNC:
    case TableExprFuncNode::sreverseFUNC:
    case TableExprFuncNode::trimFUNC:
    case TableExprFuncNode::ltrimFUNC:
    case TableExprFuncNode::rtrimFUNC:
    case TableExprFuncNode::substrFUNC:
    case TableExprFuncNode::replaceFUNC:
      {
        static Regex leadingWS("^[ \t]*");
        static Regex trailingWS("[ \t]*$");
        MArray<String> mstrings (operands()[0]->getArrayString(id));
        Array<String> strings (mstrings.array().copy());
        bool deleteStr;
        String* str = strings.getStorage (deleteStr);
        size_t n = strings.size();
        switch (funcType()) {
        case TableExprFuncNode::upcaseFUNC:
            for (size_t i=0; i<n; i++) {
                str[i].upcase();
            }
            break;
        case TableExprFuncNode::downcaseFUNC:
            for (size_t i=0; i<n; i++) {
                str[i].downcase();
            }
            break;
        case TableExprFuncNode::capitalizeFUNC:
            for (size_t i=0; i<n; i++) {
                str[i].capitalize();
            }
            break;
        case TableExprFuncNode::sreverseFUNC:
            for (size_t i=0; i<n; i++) {
                str[i].reverse();
            }
            break;
        case TableExprFuncNode::trimFUNC:
            for (size_t i=0; i<n; i++) {
                str[i].trim();
            }
            break;
        case TableExprFuncNode::ltrimFUNC:
            for (size_t i=0; i<n; i++) {
                str[i].gsub (leadingWS, String());
            }
            break;
        case TableExprFuncNode::rtrimFUNC:
            for (size_t i=0; i<n; i++) {
                str[i].gsub (trailingWS, String());
            }
            break;
        case TableExprFuncNode::substrFUNC:
            {
              int64_t stv = operands()[1]->getInt (id);
              int64_t sz  = String::npos;
              if (operands().size() > 2) {
                sz = std::max (int64_t(0), operands()[2]->getInt (id));
              }
              for (size_t i=0; i<n; i++) {
                int64_t st = stv;
                if (st < 0) st += str[i].size();
                if (st < 0) st = 0;
                str[i] = str[i].substr (st, sz);
              }
            }
            break;
        case TableExprFuncNode::replaceFUNC:
            {
              String repl;
              if (operands().size() > 2) {
                repl = operands()[2]->getString (id);
              }
              if (operands()[1]->dataType() == TableExprNodeRep::NTString) {
                String patt = operands()[1]->getString(id);
                for (size_t i=0; i<n; i++) {
                  str[i].gsub (patt, repl);
                }
              } else {
                Regex patt = operands()[1]->getRegex(id).regex();
                for (size_t i=0; i<n; i++) {
                  str[i].gsub (patt, repl);
                }
              }
            }
            break;
        default:
            throw TableInvExpr ("TableExprFuncNodeArray::getArrayString, "
                                "unhandled string function " +
                                String::toString(funcType()));
        }
        strings.putStorage (str, deleteStr);
        return MArray<String> (strings, mstrings);
        break;
      }
    case TableExprFuncNode::cmonthFUNC:
    case TableExprFuncNode::cdowFUNC:
    case TableExprFuncNode::ctodFUNC:
    case TableExprFuncNode::cdateFUNC:
    case TableExprFuncNode::ctimeFUNC:
      {
        MArray<MVTime> values (operands()[0]->getArrayDate(id));
        Array<String> strings(values.shape());
        bool deleteVal, deleteStr;
        const MVTime* val = values.array().getStorage (deleteVal);
        String* str = strings.getStorage (deleteStr);
        size_t n = values.size();
        switch (funcType()) {
        case TableExprFuncNode::cmonthFUNC:
            for (size_t i=0; i<n; i++) {
                str[i] = val[i].monthName();
            }
            break;
        case TableExprFuncNode::cdowFUNC:
            for (size_t i=0; i<n; i++) {
                str[i] = val[i].dayName();
            }
            break;
        case TableExprFuncNode::ctodFUNC:
            for (size_t i=0; i<n; i++) {
                str[i] = TableExprFuncNode::stringDateTime (val[i], 9);
            }
            break;
        case TableExprFuncNode::cdateFUNC:
            for (size_t i=0; i<n; i++) {
                str[i] = TableExprFuncNode::stringDate (val[i]);
            }
            break;
        case TableExprFuncNode::ctimeFUNC:
            for (size_t i=0; i<n; i++) {
                str[i] = TableExprFuncNode::stringTime (val[i], 9);
            }
            break;
        default:
            throw TableInvExpr ("TableExprFuncNodeArray::getArrayString, "
                                "unhandled date-string function " +
                                String::toString(funcType()));
        }
        values.array().freeStorage (val, deleteVal);
        strings.putStorage (str, deleteStr);
        return MArray<String> (strings, values);
        break;
      }
    case TableExprFuncNode::stringFUNC:
      {
        String fmt;
        int32_t width, prec;
        TableExprFuncNode::getPrintFormat (fmt, width, prec, operands(), id);
        Array<String> res;
        if (operands()[0]->dataType() == NTBool) {
          MArray<bool> arr (operands()[0]->getArrayBool(id));
          res.resize (arr.shape());
          Array<bool>::const_iterator arrIter = arr.array().begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt, width);
          }
          return MArray<String>(res, arr);
        } else if (operands()[0]->dataType() == NTInt) {
          MArray<int64_t> arr (operands()[0]->getArrayInt(id));
          res.resize (arr.shape());
          Array<int64_t>::const_iterator arrIter = arr.array().begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt, width);
          }
          return MArray<String>(res, arr);
        } else if (operands()[0]->dataType() == NTDouble) {
          std::pair<int,int> mvFormat = TableExprFuncNode::getMVFormat(fmt);
          MArray<double> arr (operands()[0]->getArrayDouble(id));
          res.resize (arr.shape());
          Array<double>::const_iterator arrIter = arr.array().begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
           *resIter = TableExprFuncNode::stringValue (*arrIter, fmt,
                                                      width, prec, mvFormat,
                                                      operands()[0]->unit());
          }
          return MArray<String>(res, arr);
        } else if (operands()[0]->dataType() == NTComplex) {
          MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
          res.resize (arr.shape());
          Array<DComplex>::const_iterator arrIter = arr.array().begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
           *resIter = TableExprFuncNode::stringValue (*arrIter, fmt,
                                                      width, prec);
          }
          return MArray<String>(res, arr);
        } else if (operands()[0]->dataType() == NTDate) {
          std::pair<int,int> mvFormat = TableExprFuncNode::getMVFormat(fmt);
          MArray<MVTime> arr (operands()[0]->getArrayDate(id));
          res.resize (arr.shape());
          Array<MVTime>::const_iterator arrIter = arr.array().begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt,
                                                       width, mvFormat);
          }
          return MArray<String>(res, arr);
        } else {
          MArray<String> arr (operands()[0]->getArrayString(id));
          Array<String> res(arr.shape());
          Array<String>::const_iterator arrIter = arr.array().begin();
          Array<String>::iterator iterEnd = res.end();
         for (Array<String>::iterator resIter = res.begin();
              resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt, width);
          }
         return MArray<String>(res, arr);
        }
      }
    case TableExprFuncNode::hmsFUNC:
    case TableExprFuncNode::dmsFUNC:
    case TableExprFuncNode::hdmsFUNC:
      {
        MArray<double> values (operands()[0]->getArrayDouble(id));
        Array<String> strings(values.shape());
        bool deleteVal, deleteStr;
        const double* val = values.array().getStorage (deleteVal);
        String* str = strings.getStorage (deleteStr);
        size_t n = values.size();
        switch (funcType()) {
        case TableExprFuncNode::hmsFUNC:
          for (size_t i=0; i<n; i++) {
            str[i] = TableExprFuncNode::stringHMS (val[i], 9);
          }
          break;
        case TableExprFuncNode::dmsFUNC:
          for (size_t i=0; i<n; i++) {
            str[i] = TableExprFuncNode::stringDMS (val[i], 9);
          }
          break;
        case TableExprFuncNode::hdmsFUNC:
          for (size_t i=0; i<n; i++) {
            if (i%2 == 0) {
              str[i] = TableExprFuncNode::stringHMS (val[i], 9);
            } else {
              str[i] = TableExprFuncNode::stringDMS (val[i], 9);
            }
          }
          break;
        default:
            throw TableInvExpr ("TableExprFuncNodeArray::getArrayString, "
                                "unhandled angle-string function " +
                                String::toString(funcType()));
        }
        values.array().freeStorage (val, deleteVal);
        strings.putStorage (str, deleteStr);
        return MArray<String> (strings, values);
        break;
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
        Array<String> res(shp);
        Array<bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
          res = operands()[0]->getString(id);
        } else {
          MArray<String> arr (operands()[0]->getArrayString(id));
          if (arr.isNull()) {
            return arr;
          }
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
        }
        return MArray<String> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
        MArray<String> arr (operands()[0]->getArrayString(id));
        return reorderArray (arr, getOrder(id, arr.ndim()), false);
      }
    case TableExprFuncNode::areverseFUNC:
      {
        MArray<String> arr (operands()[0]->getArrayString(id));
        return reverseArray (arr, getReverseAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::diagonalFUNC:
      {
        MArray<String> arr (operands()[0]->getArrayString(id));
        if (arr.isNull()) {
          return arr;
        }
        const IPosition parms = getDiagonalArg (id, arr.shape());
        if (arr.hasMask()) {
          return MArray<String>(arr.array().diagonals(parms[0], parms[1]),
                                arr.mask().diagonals(parms[0], parms[1]));
        }
        return MArray<String>(arr.array().diagonals(parms[0], parms[1]));
      }
    case TableExprFuncNode::resizeFUNC:
        return TEFResize (operands()[0]->getArrayString(id), id);
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<String> (operands(), id);
    case TableExprFuncNode::nullarrayFUNC:
        return MArray<String>();
    case TableExprFuncNode::marrayFUNC:
        return MArray<String> (operands()[0]->getStringAS(id),
                               operands()[1]->getBoolAS(id));
    case TableExprFuncNode::arrdataFUNC:
      {
        MArray<String> arr(operands()[0]->getStringAS(id).array());
        return arr.isNull()  ?  arr : MArray<String> (arr.array());
      }
    case TableExprFuncNode::negatemaskFUNC:
        return TEFMASKneg (operands()[0]->getStringAS(id));
    case TableExprFuncNode::replmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getStringAS(id), operands()[1],
                            id, true);
    case TableExprFuncNode::replunmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getStringAS(id), operands()[1],
                            id, false);
   case TableExprFuncNode::arrflatFUNC:
        return MArray<String> (operands()[0]->getStringAS(id).flatten());
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayString, "
                        "unknown function " +
                        String::toString(funcType()));
}

MArray<MVTime> TableExprFuncNodeArray::getArrayDate (const TableExprId& id)
{
    switch (funcType()) {
    case TableExprFuncNode::datetimeFUNC:
      {
        MArray<String> values (operands()[0]->getArrayString(id));
        Array<MVTime> dates(values.shape());
        bool deleteVal, deleteDat;
        const String* val = values.array().getStorage (deleteVal);
        MVTime* dat = dates.getStorage (deleteDat);
        Quantity quant;
        size_t n = values.size();
        for (size_t i=0; i<n; i++) {
            if (MVTime::read (quant, val[i])) {
                dat[i] = quant;
            }
            throw (TableInvExpr ("invalid date string " + val[i]));
        }
        values.array().freeStorage (val, deleteVal);
        dates.putStorage (dat, deleteDat);
        return MArray<MVTime> (dates, values);
      }
    case TableExprFuncNode::mjdtodateFUNC:
      {
        MArray<double> values (operands()[0]->getArrayDouble(id));
        Array<MVTime> dates(values.shape());
        bool deleteVal, deleteDat;
        const double* val = values.array().getStorage (deleteVal);
        MVTime* dat = dates.getStorage (deleteDat);
        size_t n = values.size();
        for (size_t i=0; i<n; i++) {
            dat[i] = MVTime (val[i]);
        }
        values.array().freeStorage (val, deleteVal);
        dates.putStorage (dat, deleteDat);
        return MArray<MVTime> (dates, values);
      }
    case TableExprFuncNode::dateFUNC:
      {
        MArray<MVTime> values (operands()[0]->getArrayDate(id));
        Array<MVTime> dates(values.shape());
        bool deleteVal, deleteDat;
        const MVTime* val = values.array().getStorage (deleteVal);
        MVTime* dat = dates.getStorage (deleteDat);
        size_t n = values.size();
        for (size_t i=0; i<n; i++) {
            dat[i] = MVTime (floor (double (val[i])));
        }
        values.array().freeStorage (val, deleteVal);
        dates.putStorage (dat, deleteDat);
        return MArray<MVTime> (dates, values);
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
        Array<MVTime> res(shp);
        Array<bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
          res = operands()[0]->getDate(id);
        } else {
          MArray<MVTime> arr (operands()[0]->getArrayDate(id));
          if (arr.isNull()) {
            return arr;
          }
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
        }
        return MArray<MVTime> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
        MArray<MVTime> arr (operands()[0]->getArrayDate(id));
        return reorderArray (arr, getOrder(id, arr.ndim()), false);
      }
    case TableExprFuncNode::areverseFUNC:
      {
        MArray<MVTime> arr (operands()[0]->getArrayDate(id));
        return reverseArray (arr, getReverseAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::diagonalFUNC:
      {
        MArray<MVTime> arr (operands()[0]->getArrayDate(id));
        if (arr.isNull()) {
          return arr;
        }
        const IPosition parms = getDiagonalArg (id, arr.shape());
        if (arr.hasMask()) {
          return MArray<MVTime>(arr.array().diagonals(parms[0], parms[1]),
                                arr.mask().diagonals(parms[0], parms[1]));
        }
        return MArray<MVTime>(arr.array().diagonals(parms[0], parms[1]));
      }
    case TableExprFuncNode::resizeFUNC:
        return TEFResize (operands()[0]->getArrayDate(id), id);
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<MVTime> (operands(), id);
    case TableExprFuncNode::nullarrayFUNC:
        return MArray<MVTime>();
    case TableExprFuncNode::marrayFUNC:
        return MArray<MVTime> (operands()[0]->getDateAS(id),
                               operands()[1]->getBoolAS(id));
    case TableExprFuncNode::arrdataFUNC:
      {
        MArray<MVTime> arr(operands()[0]->getDateAS(id).array());
        return arr.isNull()  ?  arr : MArray<MVTime> (arr.array());
      }
    case TableExprFuncNode::negatemaskFUNC:
        return TEFMASKneg (operands()[0]->getDateAS(id));
    case TableExprFuncNode::replmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getDateAS(id), operands()[1],
                            id, true);
    case TableExprFuncNode::replunmaskedFUNC:
        return TEFMASKrepl (operands()[0]->getDateAS(id), operands()[1],
                            id, false);
    case TableExprFuncNode::arrflatFUNC:
        return MArray<MVTime> (operands()[0]->getDateAS(id).flatten());
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayDate, "
                        "unknown function " +
                        String::toString(funcType()));
}

} //# NAMESPACE CASACORE - END
