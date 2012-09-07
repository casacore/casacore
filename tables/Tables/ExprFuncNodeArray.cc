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
//#
//# $Id$

#include <tables/Tables/ExprFuncNodeArray.h>
#include <tables/Tables/TableError.h>
#include <tables/Tables/ExprNode.h>
#include <tables/Tables/ExprDerNode.h>
#include <tables/Tables/ExprNodeSet.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/MArrayMath.h>
#include <casa/Arrays/MArrayLogical.h>
#include <casa/Arrays/MArrayUtil.h>
#include <casa/Quanta/MVTime.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <casa/Utilities/Assert.h>


namespace casa { //# NAMESPACE CASA - BEGIN

  // Helper function to fill an array from another one.
  template <typename T>
  void TEFNAFillArray (Array<T>& res, Array<T> arr)
  {
    Bool delRes, delArr;
    T* resd = res.getStorage (delRes);
    const T* arrd = arr.getStorage (delArr);
    size_t j=0;
    size_t arrsz = arr.nelements();
    size_t n = res.nelements();
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
  void TEFNAiifExec (const Bool* cond,
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
  MArray<T> TEFNAiifAS (Bool useArray, const MArray<T>& arr,
                        TableExprNodeRep* node, const TableExprId& id)
  {
    if (useArray) return arr;
    // Use the scalar by expanding it to an (unmasked) array.
    Array<T> res(arr.shape());
    res = node->get(id, T());
    return MArray<T>(res);
  }
  
  // result mask is True if cond.mask=True, otherwise mask1 or mask2
  template <typename T>
  MArray<T> TEFNAiif (const PtrBlock<TableExprNodeRep*>& operands,
                      const TableExprId& id)
  {
    // If the condition is a scalar, one or both operands is an array.
    if (operands[0]->valueType() == TableExprNodeRep::VTScalar) {
      Bool valc = operands[0]->getBool(id);
      // If an operand is a scalar, return array or scalar expanded to array.
      // Note that the evaluation of the scalar operand is only done if needed.
      if (operands[1]->valueType() == TableExprNodeRep::VTScalar) {
        return TEFNAiifAS (!valc, operands[2]->getArray(id, T()),
                           operands[1], id);
      } else if (operands[2]->valueType() == TableExprNodeRep::VTScalar) {
        return TEFNAiifAS (valc, operands[1]->getArray(id, T()),
                           operands[2], id);
      } else if (valc) {
        return operands[1]->getArray(id, T());
      } else {
        return operands[2]->getArray(id, T());
      }
    }
    // The condition is an array. The operands can be scalar or array.
    // Arrays can have masks.
    // First get the condition array and a pointer to its data.
    Bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
    MArray<Bool> arrc (operands[0]->getArrayBool(id));
    const Bool* datac = arrc.array().getStorage (deleteArrc);
    IPosition shp (arrc.shape());
    size_t nr = arrc.nelements();
    // The operands can be array or scalar.
    // So use a pointer and increment that can deal with either of them.
    MArray<T> arr1, arr2;
    T val1, val2;
    const T* data1 = &val1;
    const T* data2 = &val2;
    size_t incr1 = 1;
    size_t incr2 = 1;
    // Set initially that the operands do not have a mask.
    Bool hasMask = False;
    // Get the values. If array, check if shape matches.
    if (operands[1]->valueType() == TableExprNodeRep::VTScalar) {
      val1  = operands[1]->get(id, T());
      incr1 = 0;
    } else {
      arr1.reference (operands[1]->getArray(id, T()));
      if (! shp.isEqual (arr1.shape())) {
        throw TableInvExpr ("TableExprFuncNodeArray::get<T>, "
                            "array shapes mismatch in function IIF");
      }
      data1   = arr1.array().getStorage (deleteArr1);
      hasMask = hasMask || arr1.hasMask();
    }
    if (operands[2]->valueType() == TableExprNodeRep::VTScalar) {
      val2  = operands[2]->get(id, T());
      incr2 = 0;
    } else {
      arr2.reference (operands[2]->getArray(id, T()));
      if (! shp.isEqual (arr2.shape())) {
        throw TableInvExpr ("TableExprFuncNodeArray::get<T>, "
                            "array shapes mismatch in function IIF");
      }
      data2   = arr2.array().getStorage (deleteArr2);
      hasMask = hasMask || arr2.hasMask();
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
    Bool valMask1 = False;
    Bool valMask2 = False;
    const Bool* valm1 = &valMask1;
    const Bool* valm2 = &valMask2;
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
    Array<Bool> resMask(shp);
    Bool* resm = resMask.getStorage (deleteRes);
    TEFNAiifExec (datac, valm1, incr1, valm2, incr2, resm, arrc.size());
    if (valm1 != &valMask1) arr1.mask().freeStorage (valm1, deleteArr1);
    if (valm2 != &valMask2) arr2.mask().freeStorage (valm2, deleteArr2);
    result.putStorage (res, deleteRes);
    MArray<T> mares (result, resMask);
    return MArray<T> (result, mares.combineMask (arrc));
  }


TableExprFuncNodeArray::TableExprFuncNodeArray
                             (TableExprFuncNode::FunctionType ftype,
			      NodeDataType dtype, ValueType vtype,
			      const TableExprNodeSet& source,
			      const TaQLStyle& style)
: TableExprNodeArray (dtype, OtFunc),
  node_p      (ftype, dtype, vtype, source),
  origin_p    (style.origin()),
  isCOrder_p  (style.isCOrder()),
  constAxes_p (False)
{
    table_p = source.table();
    exprtype_p = Variable;
}

TableExprFuncNodeArray::~TableExprFuncNodeArray()
{}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
// When one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprFuncNodeArray::fillNode
                                   (TableExprFuncNodeArray* thisNode,
				    PtrBlock<TableExprNodeRep*>& nodes,
				    const Block<Int>& dtypeOper)
{
    // Fill child nodes as needed.
    TableExprFuncNode::fillChildNodes (&(thisNode->node_p), nodes, dtypeOper);
    // Set the resulting unit.
    Double scale = TableExprFuncNode::fillUnits (thisNode, thisNode->rwOperands(),
                                                 thisNode->funcType());
    thisNode->setScale (scale);
    // Some functions on a variable can already give a constant result.
    thisNode->tryToConst();
    if (thisNode->operands().nelements() > 0) {
	return convertNode (thisNode, True);
    }
    return thisNode;
}

void TableExprFuncNodeArray::tryToConst()
{
    Int axarg = 1;
    switch (funcType()) {
    case TableExprFuncNode::shapeFUNC:
	if (operands()[0]->ndim() == 0
        ||  operands()[0]->shape().nelements() > 0  ) {
	    exprtype_p = Constant;
	}
	break;
    case TableExprFuncNode::arrfractilesFUNC:
        axarg = 2;
    case TableExprFuncNode::arrsumsFUNC:
    case TableExprFuncNode::arrproductsFUNC:
    case TableExprFuncNode::arrsumsqrsFUNC:
    case TableExprFuncNode::arrminsFUNC:
    case TableExprFuncNode::arrmaxsFUNC:
    case TableExprFuncNode::arrmeansFUNC:
    case TableExprFuncNode::arrvariancesFUNC:
    case TableExprFuncNode::arrstddevsFUNC:
    case TableExprFuncNode::arravdevsFUNC:
    case TableExprFuncNode::arrrmssFUNC:
    case TableExprFuncNode::arrmediansFUNC:
    case TableExprFuncNode::anysFUNC:
    case TableExprFuncNode::allsFUNC:
    case TableExprFuncNode::ntruesFUNC:
    case TableExprFuncNode::nfalsesFUNC:
        if (operands()[axarg]->isConstant()) {
	    ipos_p = getAxes (0, -1, axarg);
	    constAxes_p = True;
	}
        break;
    case TableExprFuncNode::arrayFUNC:
        if (operands()[axarg]->isConstant()) {
	    ipos_p = getArrayShape (0, axarg);
	    constAxes_p = True;
	}
        break;
    case TableExprFuncNode::transposeFUNC:
        if (operands()[axarg]->isConstant()) {
            ipos_p = getAxes (0, -1, axarg, False);
	    constAxes_p = True;
	}
        break;
    case TableExprFuncNode::marrayFUNC:
    case TableExprFuncNode::arrdataFUNC:
    case TableExprFuncNode::arrmaskFUNC:
    case TableExprFuncNode::arrflatFUNC:
        if (operands()[0]->valueType() == VTScalar) {
            ipos_p = IPosition(1,1);
	    constAxes_p = True;
	}
        break;
    default:
	break;
    }
}


IPosition TableExprFuncNodeArray::getAxes (const TableExprId& id,
                                           Int ndim, uInt axarg,
                                           bool swapRemove)
{
  // Get the axes if not constant (or not known).
  if (!constAxes_p) {
    Array<Int64> ax(operands()[axarg]->getArrayInt(id).array());
    AlwaysAssert (ax.ndim() == 1, AipsError);
    AlwaysAssert (ax.contiguousStorage(), AipsError);
    ipos_p.resize (ax.nelements());
    for (uInt i=0; i<ax.nelements(); i++) {
      ipos_p(i) = ax.data()[i] - origin_p;
    }
    iposN_p = ipos_p;
  }
  // Check if an axis exceeds the dimensionality.
  uInt nr = 0;
  for (uInt i=0; i<ipos_p.nelements(); i++) {
    if (ipos_p(i) < 0) {
        throw TableInvExpr ("axis < 0 used in xxxs function");
    }
    if (ndim < 0) {
      nr = ipos_p.nelements();
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
  if (nr == ipos_p.nelements()  ||  !swapRemove) {
    return ipos_p;
  }
  // Remove axes exceeding dimensionality.
  return removeAxes (ipos_p, ndim);
}

IPosition TableExprFuncNodeArray::removeAxes (const IPosition& axes, 
                                              Int ndim) const
{
  // Count axes not exceeding ndim.
  uInt nr=0;
  for (uInt i=0; i<axes.size(); ++i) {
    if (axes[i] < ndim) {
      nr++;
    }
  }
  if (nr == axes.size()) {
    return axes;
  }
  // Remove the too high axes.
  IPosition newAxes(nr);
  uInt j=0;
  for (uInt i=0; i<axes.size(); ++i) {
    if (ipos_p[i] < ndim) {
      newAxes[j++] = ipos_p[i];
    }
  }
  return newAxes;
}
			   
const IPosition& TableExprFuncNodeArray::getArrayShape(const TableExprId& id,
						       uInt axarg)
{
  // Get the shape if not constant.
  if (!constAxes_p) {
    Array<Int64> ax(operands()[axarg]->getArrayInt(id).array());
    AlwaysAssert (ax.ndim() == 1, AipsError);
    AlwaysAssert (ax.contiguousStorage(), AipsError);
    uInt ndim = ax.nelements();
    ipos_p.resize (ndim);
    if (isCOrder_p) {
      for (uInt i=0; i<ndim; i++) {
	ipos_p(i) = ax.data()[ndim-i-1];
      }
    } else {
      for (uInt i=0; i<ndim; i++) {
	ipos_p(i) = ax.data()[i];
      }
    }
  }
  return ipos_p;
}

IPosition TableExprFuncNodeArray::getOrder (const TableExprId& id, Int ndim)
{
  IPosition order = getAxes(id, ndim, 1, False);
  cout <<"order="<< order<<endl;
  if (order.empty()) {
    // Default is to transpose the full array.
    order.resize (ndim);
    for (int i=0; i<ndim; ++i) {
      order[i] = ndim-i-1;
    }
    return order;
  }
  // Remove possibly too high axes.
  return removeAxes (order, ndim);
}


MArray<Bool> TableExprFuncNodeArray::getArrayBool (const TableExprId& id)
{
    switch (funcType()) {
    case TableExprFuncNode::boolFUNC:
      if (operands()[0]->dataType() == NTBool) {
        return operands()[0]->getArrayBool(id);
      } else if (operands()[0]->dataType() == NTInt) {
        return (operands()[0]->getArrayInt(id) != Int64(0));
      } else if (operands()[0]->dataType() == NTDouble) {
        return (operands()[0]->getArrayDouble(id) != 0.);
      } else if (operands()[0]->dataType() == NTComplex) {
        return (operands()[0]->getArrayDComplex(id) != DComplex());
      } else if (operands()[0]->dataType() == NTDate) {
        return (operands()[0]->getArrayDouble(id) != 0.);
      } else {
        MArray<String> values = operands()[0]->getArrayString(id);
        Array<Bool> res(values.shape());
        Array<String>::const_iterator in = values.array().begin();
        for (Array<Bool>::contiter out=res.cbegin();
             out!=res.cend(); ++out, ++in) {
          *out = TableExprFuncNode::string2Bool (*in);
        }
        return MArray<Bool> (res, values.mask());
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
    case TableExprFuncNode::anysFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	return partialAnys (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::allsFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	return partialAlls (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runallFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	return slidingAlls (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runanyFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	return slidingAnys (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxallFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	return boxedAlls (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxanyFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	return boxedAnys (arr, getArrayShape(id));
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
	Array<Bool> res(shp);
        Array<Bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getBool(id);
	} else {
	  MArray<Bool> arr (operands()[0]->getArrayBool(id));
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
	}
	return MArray<Bool> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
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
        return TEFNAiif<Bool> (operands(), id);
    case TableExprFuncNode::marrayFUNC:
        return MArray<Bool> (operands()[0]->getBoolAS(id).array(),
                             operands()[1]->getBoolAS(id).array());
    case TableExprFuncNode::arrdataFUNC:
        return MArray<Bool> (operands()[0]->getBoolAS(id).array());
    case TableExprFuncNode::arrflatFUNC:
        return MArray<Bool> (operands()[0]->getBoolAS(id).flatten());
    case TableExprFuncNode::arrmaskFUNC:
      {
        IPosition shp;
        switch (operands()[0]->dataType()) {
        case NTBool:
          {
            MArray<Bool> arr (operands()[0]->getBoolAS(id).mask());
            if (arr.hasMask()) {
              return MArray<Bool> (arr.mask());
            }
            shp = arr.shape();
            break;
          }
        case NTInt:
          {
            MArray<Int64> arr (operands()[0]->getIntAS(id));
            if (arr.hasMask()) {
              return MArray<Bool> (arr.mask());
            }
            shp = arr.shape();
            break;
          }
        case NTDouble:
          {
            MArray<Double> arr (operands()[0]->getDoubleAS(id));
            if (arr.hasMask()) {
              return MArray<Bool> (arr.mask());
            }
            shp = arr.shape();
            break;
          }
        case NTComplex:
          {
            MArray<DComplex> arr (operands()[0]->getDComplexAS(id));
            if (arr.hasMask()) {
              return MArray<Bool> (arr.mask());
            }
            shp = arr.shape();
            break;
          }
        case NTString:
          {
            MArray<String> arr (operands()[0]->getStringAS(id));
            if (arr.hasMask()) {
              return MArray<Bool> (arr.mask());
            }
            shp = arr.shape();
            break;
          }
        case NTDate:
          {
            MArray<MVTime> arr (operands()[0]->getDateAS(id));
            if (arr.hasMask()) {
              return MArray<Bool> (arr.mask());
            }
            shp = arr.shape();
            break;
          }
        default:
            throw TableInvExpr ("TableExprFuncNodeArray::getArrayBool, "
                                "unknown datatype in mask function");
        }
        Array<Bool> mask(shp);
        mask = False;
        return MArray<Bool> (mask);
      }
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayBool, "
                        "unknown function " +
                        String::toString(funcType()));
}

MArray<Int64> TableExprFuncNodeArray::getArrayInt (const TableExprId& id)
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
            Array<Int64> res(values.shape());
            Array<String>::const_iterator in = values.array().begin();
            for (Array<Int64>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = TableExprFuncNode::string2Int (*in);
            }
	    return MArray<Int64> (res, values.mask());
        } else if (operands()[0]->dataType() == NTBool) {
            MArray<Bool> values (operands()[0]->getArrayBool(id));
            Array<Int64> res(values.shape());
            Array<Bool>::const_iterator in = values.array().begin();
            for (Array<Int64>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = *in ? 1:0;
            }
	    return MArray<Int64> (res, values.mask());
        } else if (argDataType() == NTDouble) {
            MArray<Double> val (operands()[0]->getArrayDouble(id));
            Array<Int64> arr(val.shape());
            convertArray (arr, val.array());
            return MArray<Int64> (arr, val.mask());
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
	Int n = shp.nelements();
	Array<Int64> result(IPosition(1,n));
	Int64* res = result.data();
	if (isCOrder_p) {
	    for (Int i=0; i<n; ++i) {
	        res[i] = shp[n-i-1];
	    }
	} else {
	    for (Int i=0; i<n; ++i) {
	        res[i] = shp[i];
	    }
	}
	return MArray<Int64>(result);
      }
    case TableExprFuncNode::strlengthFUNC:
      {
	MArray<String> values (operands()[0]->getArrayString(id));
	Array<Int64> res(values.shape());
	Bool deleteVal, deleteRes;
	const String* val = values.array().getStorage (deleteVal);
	Int64* resp = res.getStorage (deleteRes);
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    resp[i] = val[i].length();
	}
	values.array().freeStorage (val, deleteVal);
	res.putStorage (resp, deleteRes);
	return MArray<Int64> (res, values.mask());
      }
    case TableExprFuncNode::yearFUNC:
    case TableExprFuncNode::monthFUNC:
    case TableExprFuncNode::dayFUNC:
    case TableExprFuncNode::weekdayFUNC:
    case TableExprFuncNode::weekFUNC:
      {
	MArray<MVTime> values (operands()[0]->getArrayDate(id));
	Array<Int64> res(values.shape());
	Bool deleteVal, deleteRes;
	const MVTime* val = values.array().getStorage (deleteVal);
	Int64* resp = res.getStorage (deleteRes);
	size_t n = values.nelements();
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
	return MArray<Int64> (res, values.mask());
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
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrminsFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return partialMins (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmaxsFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return partialMaxs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runminFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return slidingMins (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmaxFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return slidingMaxs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxminFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return boxedMins (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::boxmaxFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return boxedMaxs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::ntruesFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	MArray<uInt> res(partialNTrue (arr, getAxes(id, arr.ndim())));
	Array<Int64> resd(res.shape());
	convertArray (resd, res.array());
	return MArray<Int64> (resd, res.mask());
      }
    case TableExprFuncNode::nfalsesFUNC:
      {
	MArray<Bool> arr (operands()[0]->getArrayBool(id));
	MArray<uInt> res(partialNFalse (arr, getAxes(id, arr.ndim())));
	Array<Int64> resd(res.shape());
	convertArray (resd, res.array());
	return MArray<Int64> (resd, res.mask());
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
	Array<Int64> res(shp);
        Array<Bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getInt(id);
	} else {
	  MArray<Int64> arr (operands()[0]->getArrayInt(id));
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
	}
	return MArray<Int64> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
	MArray<Int64> arr (operands()[0]->getArrayInt(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<Int64> (operands(), id);
    case TableExprFuncNode::marrayFUNC:
        return MArray<Int64> (operands()[0]->getIntAS(id).array(),
                              operands()[1]->getBoolAS(id).array());
    case TableExprFuncNode::arrdataFUNC:
        return MArray<Int64> (operands()[0]->getIntAS(id).array());
    case TableExprFuncNode::arrflatFUNC:
        return MArray<Int64> (operands()[0]->getIntAS(id).flatten());
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayInt, "
                        "unknown function " +
                        String::toString(funcType()));
}

MArray<Double> TableExprFuncNodeArray::getArrayDouble (const TableExprId& id)
{
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
	MArray<Double> res = sqrt (operands()[0]->getArrayDouble(id));
        if (node_p.getScale() != 1.) {
          // Note: in this way arr references the array in res.
          Array<Double> arr(res.array());
          arrayTransformInPlace (arr, node_p.getScale(),
                                 casa::Multiplies<Double,Double>());
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
	    Array<Double> result(arr.shape());
	    Bool deleteArr, deleteRes;
	    const DComplex* data = arr.array().getStorage (deleteArr);
	    Double* res = result.getStorage (deleteRes);
	    size_t nr = arr.nelements();
	    for (size_t i=0; i<nr; i++) {
	        res[i] = norm(data[i]);
	    }
	    arr.array().freeStorage (data, deleteArr);
	    result.putStorage (res, deleteRes);
	    return MArray<Double> (result, arr.mask());
	}
    case TableExprFuncNode::absFUNC:
	if (argDataType() == NTDouble) {
	    return abs (operands()[0]->getArrayDouble(id));
	}
	return amplitude (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::argFUNC:
	if (argDataType() == NTDouble) {
	    MArray<Double> marr (operands()[0]->getArrayDouble(id));
            Array<Double> arr(marr.array().copy());
	    Bool deleteIt;
	    Double* data = arr.getStorage (deleteIt);
	    size_t nr = arr.nelements();
	    for (size_t i=0; i<nr; i++) {
	        if (data[i] >= 0) {
		    data[i] = 0;
		} else {
                    data[i] = C::pi;
		}
	    }
	    arr.putStorage (data, deleteIt);
	    return MArray<Double> (arr, marr.mask());
	}
	return phase (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::realFUNC:
        if (operands()[0]->dataType() == NTString) {
            MArray<String> values (operands()[0]->getArrayString(id));
            Array<Double> res(values.shape()); 
            Array<String>::const_iterator in = values.array().begin();
            for (Array<Double>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = TableExprFuncNode::string2Real (*in);
            }
	    return MArray<Double> (res, values.mask());
        } else if (operands()[0]->dataType() == NTBool) {
            MArray<Bool> values (operands()[0]->getArrayBool(id));
            Array<Double> res(values.shape());
            Array<Bool>::const_iterator in = values.array().begin();
            for (Array<Double>::contiter out=res.cbegin();
                 out!=res.cend(); ++out, ++in) {
              *out = *in ? 1:0;
            }
	    return MArray<Double> (res, values.mask());
        } else if (argDataType() == NTDouble) {
	    return operands()[0]->getArrayDouble(id);
	}
	return real (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::imagFUNC:
	if (argDataType() == NTDouble) {
            MArray<Double> arr (operands()[0]->getArrayDouble(id));
            Array<Double> result(arr.shape());
	    result = 0.;
	    return MArray<Double> (result, arr.mask());
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
	Array<Double> doubles(values.shape());
	Bool deleteVal, deleteDoub;
	const MVTime* val = values.array().getStorage (deleteVal);
	Double* doub = doubles.getStorage (deleteDoub);
	size_t n = values.nelements();
        if (funcType() == TableExprFuncNode::mjdFUNC) {
	    for (size_t i=0; i<n; i++) {
		doub[i] = val[i].day();
	    }
        } else {
	    for (size_t i=0; i<n; i++) {
                doub[i] = fmod (Double(val[i]), 1.) * C::_2pi;   // in radians
            }
        }
	values.array().freeStorage (val, deleteVal);
	doubles.putStorage (doub, deleteDoub);
	return MArray<Double> (doubles, values.mask());
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
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrminsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMins (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmaxsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMaxs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmeansFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMeans (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrvariancesFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialVariances (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrstddevsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialStddevs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arravdevsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialAvdevs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrrmssFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialRmss (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmediansFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMedians (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrfractilesFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return partialFractiles (arr,
				 getAxes(id, arr.ndim(), 2),
				 operands()[1]->getDouble(id));
      }
    case TableExprFuncNode::runminFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingMins (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmaxFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingMaxs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmeanFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingMeans (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runvarianceFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingVariances (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runstddevFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingStddevs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runavdevFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingAvdevs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runrmsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingRmss (arr, getArrayShape(id));
      }
    case TableExprFuncNode::runmedianFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingMedians (arr, getArrayShape(id));
    }
    case TableExprFuncNode::boxminFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedMins (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmaxFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedMaxs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmeanFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedMeans (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxvarianceFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedVariances (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxstddevFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedStddevs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxavdevFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedAvdevs (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxrmsFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedRmss (arr, getArrayShape(id));
      }
    case TableExprFuncNode::boxmedianFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedMedians (arr, getArrayShape(id));
    }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
	Array<Double> res(shp);
        Array<Bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getDouble(id);
	} else {
	  MArray<Double> arr (operands()[0]->getArrayDouble(id));
          TEFNAFillArray (res, arr.array());
          if (arr.hasMask()) {
            mask.resize (shp);
            TEFNAFillArray (mask, arr.mask());
          }
	}
	return MArray<Double> (res, mask);
      }
    case TableExprFuncNode::transposeFUNC:
      {
	MArray<Double> arr (operands()[0]->getArrayDouble(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<Double> (operands(), id);
    case TableExprFuncNode::marrayFUNC:
        return MArray<Double> (operands()[0]->getDoubleAS(id).array(),
                               operands()[1]->getBoolAS(id).array());
    case TableExprFuncNode::arrdataFUNC:
        return MArray<Double> (operands()[0]->getDoubleAS(id).array());
    case TableExprFuncNode::arrflatFUNC:
        return MArray<Double> (operands()[0]->getDoubleAS(id).flatten());
    case TableExprFuncNode::angdistFUNC:
      {
        MArray<Double> a1 = operands()[0]->getArrayDouble(id);
        MArray<Double> a2 = operands()[1]->getArrayDouble(id);
        // Treat an array of size 2 as scalar, so allow scalar-array operations
        // which is handled by angdistxFUNC.
        if (a1.size() != 2  &&  a2.size() != 2) {
          if (a1.size() != a2.size()) {
            throw TableInvExpr ("Arguments of angdist function must have "
                                "equal length");
          }
          if (a1.size() %2 != 0) {
            throw TableInvExpr ("Arguments of angdist function must have a "
                                "multiple of 2 values");
          }
          Array<Double> result(IPosition(1, a1.size()/2));
          Double* res = result.data();
          Array<Double>::const_iterator p2   = a2.array().begin();
          Array<Double>::const_iterator end1 = a1.array().end();
          for (Array<Double>::const_iterator p1 = a1.array().begin();
               p1!=end1; ++p1) {
            Double ra1 = *p1;
            ++p1;
            Double ra2 = *p2;
            ++p2;
            *res++ = acos (sin(*p1)*sin(*p2) + cos(*p1)*cos(*p2)*cos(ra1-ra2));
            ++p2;
          }
          // Reduce possible masks by combining every 2 values.
          Array<Bool> mask;
          if (a1.hasMask()) {
            partialArrayMath (mask,
                              a1.mask().reform(IPosition(2, 2, a1.size()/2)),
                              IPosition(1,0),
                              AnyFunc<Bool>());
          }
          if (a2.hasMask()) {
            Array<Bool> mask2;
            partialArrayMath (mask2,
                              a2.mask().reform(IPosition(2, 2, a2.size()/2)),
                              IPosition(1,0),
                              AnyFunc<Bool>());
            if (mask.empty()) {
              mask.reference (mask2);
            } else {
              mask.reference (mask || mask2);
            }
          }
          return MArray<Double> (result, mask);
        }  // fall through if either array has size 2
      }
    case TableExprFuncNode::angdistxFUNC:
      {
        MArray<Double> a1 = operands()[0]->getArrayDouble(id);
        MArray<Double> a2 = operands()[1]->getArrayDouble(id);
        if (!(a1.size() %2 == 0  &&  a2.size() %2 == 0)) {
          throw TableInvExpr ("Arguments of angdistx function must have a "
                              "multiple of 2 values");
        }
        Array<Double>::const_iterator end1 = a1.array().end();
        Array<Double>::const_iterator end2 = a2.array().end();
        Array<Double> result(IPosition(2, a1.size()/2, a2.size()/2));
        Double* res = result.data();
        for (Array<Double>::const_iterator p2 = a2.array().begin();
             p2!=end2; ++p2) {
          Double ra2     = *p2;
          ++p2;
          Double sindec2 = sin(*p2);
          Double cosdec2 = cos(*p2);
          for (Array<Double>::const_iterator p1 = a1.array().begin();
               p1!=end1; ++p1) {
            Double ra1 = *p1;
            ++p1;
            *res++ = acos (sin(*p1)*sindec2 + cos(*p1)*cosdec2*cos(ra1-ra2));
          }
        }
        /// deal with possible mask
        return MArray<Double>(result);
      }
    case TableExprFuncNode::datetimeFUNC:
    case TableExprFuncNode::mjdtodateFUNC:
    case TableExprFuncNode::dateFUNC:
      {
        MArray<MVTime> arr (getArrayDate(id));
        Array<Double> res(arr.shape());
        convertArray (res, arr.array());
        return MArray<Double> (res, arr.mask());
      }
    default:
      {
        // Functions like YEAR are implemented as Int only.
        MArray<Int64> arr (getArrayInt(id));
        Array<Double> res(arr.shape());
        convertArray (res, arr.array());
        return MArray<Double> (res, arr.mask());
      }
    }
    return MArray<Double>();
}

MArray<DComplex> TableExprFuncNodeArray::getArrayDComplex
                                                     (const TableExprId& id)
{
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
                                 casa::Multiplies<DComplex,Double>());
        }
        return res;
      }
    case TableExprFuncNode::conjFUNC:
	return conj     (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::powFUNC:
        if (operands()[0]->valueType() == VTScalar) {
	    return pow (operands()[0]->getDComplex(id),
			operands()[1]->getArrayDComplex(id));
	} else if (operands()[1]->valueType() == VTScalar) {
            MArray<DComplex> arr1 (operands()[0]->getArrayDComplex(id));
	    Array<DComplex> arr2 (arr1.shape());
	    arr2 = operands()[1]->getDComplex(id);
            /// Make pow of array,scalar possible
	    return MArray<DComplex> (pow(arr1.array(), arr2), arr1.mask());
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
    case TableExprFuncNode::arrsumsFUNC:
      {
	MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
	MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
	MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
	Array<DComplex> res(shp);
        Array<Bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getDComplex(id);
	} else {
	  MArray<DComplex> arr (operands()[0]->getArrayDComplex(id));
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
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
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
	    return MArray<DComplex> (res, values.mask());
        }
        if (operands()[0]->valueType() == VTScalar) {
          Double val = operands()[0]->getDouble(id);
          MArray<Double> arr (operands()[1]->getArrayDouble(id));
          return MArray<DComplex> (makeComplex(val,arr.array()), arr.mask());
	} else if (operands()[1]->valueType() == VTScalar) {
          MArray<Double> arr (operands()[0]->getArrayDouble(id));
          Double val = operands()[1]->getDouble(id);
          return MArray<DComplex> (makeComplex(arr.array(), val), arr.mask());
	}
        MArray<Double> arr1 (operands()[0]->getArrayDouble(id));
        MArray<Double> arr2 (operands()[1]->getArrayDouble(id));
	return MArray<DComplex> (makeComplex(arr1.array(), arr2.array()),
                                 arr1.combineMask(arr2));
      }
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<DComplex> (operands(), id);
    case TableExprFuncNode::marrayFUNC:
        return MArray<DComplex> (operands()[0]->getDComplexAS(id).array(),
                                 operands()[1]->getBoolAS(id).array());
    case TableExprFuncNode::arrdataFUNC:
        return MArray<DComplex> (operands()[0]->getDComplexAS(id).array());
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
	Bool deleteStr;
	String* str = strings.getStorage (deleteStr);
	size_t n = strings.nelements();
	size_t i;
	switch (funcType()) {
	case TableExprFuncNode::upcaseFUNC:
	    for (i=0; i<n; i++) {
		str[i].upcase();
	    }
	    break;
	case TableExprFuncNode::downcaseFUNC:
	    for (i=0; i<n; i++) {
		str[i].downcase();
	    }
	    break;
	case TableExprFuncNode::trimFUNC:
	    for (i=0; i<n; i++) {
                str[i].gsub (leadingWS, string());
                str[i].gsub (trailingWS, string());
	    }
	    break;
	case TableExprFuncNode::ltrimFUNC:
	    for (i=0; i<n; i++) {
                str[i].gsub (leadingWS, string());
	    }
	    break;
	case TableExprFuncNode::rtrimFUNC:
	    for (i=0; i<n; i++) {
                str[i].gsub (trailingWS, string());
	    }
	    break;
        case TableExprFuncNode::substrFUNC:
            {
              size_t st = std::max (Int64(0), operands()[1]->getInt (id));
              size_t sz = String::npos;
              if (operands().size() > 2) {
                sz = std::max (Int64(0), operands()[2]->getInt (id));
              }
              for (i=0; i<n; i++) {
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
                for (i=0; i<n; i++) {
                  str[i].gsub (patt, repl);
                }
              } else {
                Regex patt = operands()[1]->getRegex(id).regex();
                for (i=0; i<n; i++) {
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
	return MArray<String> (strings, mstrings.mask());
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
	Bool deleteVal, deleteStr;
	const MVTime* val = values.array().getStorage (deleteVal);
	String* str = strings.getStorage (deleteStr);
	size_t n = values.size();
	size_t i;
	switch (funcType()) {
	case TableExprFuncNode::cmonthFUNC:
	    for (i=0; i<n; i++) {
		str[i] = val[i].monthName();
	    }
	    break;
	case TableExprFuncNode::cdowFUNC:	
	    for (i=0; i<n; i++) {
		str[i] = val[i].dayName();
	    }
	    break;
	case TableExprFuncNode::ctodFUNC:	
	    for (i=0; i<n; i++) {
                str[i] = TableExprFuncNode::stringDateTime (val[i], 9);
	    }
	    break;
	case TableExprFuncNode::cdateFUNC:	
	    for (i=0; i<n; i++) {
                str[i] = TableExprFuncNode::stringDate (val[i]);
	    }
	    break;
	case TableExprFuncNode::ctimeFUNC:	
	    for (i=0; i<n; i++) {
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
	return MArray<String> (strings, values.mask());
        break;
      }
    case TableExprFuncNode::hmsFUNC:
    case TableExprFuncNode::dmsFUNC:
    case TableExprFuncNode::hdmsFUNC:
      {
	MArray<Double> values (operands()[0]->getArrayDouble(id));
	Array<String> strings(values.shape());
	Bool deleteVal, deleteStr;
	const Double* val = values.array().getStorage (deleteVal);
	String* str = strings.getStorage (deleteStr);
	size_t n = values.nelements();
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
        return MArray<String> (strings, values.mask());
        break;
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
	Array<String> res(shp);
        Array<Bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getString(id);
	} else {
	  MArray<String> arr (operands()[0]->getArrayString(id));
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
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<String> (operands(), id);
    case TableExprFuncNode::marrayFUNC:
        return MArray<String> (operands()[0]->getStringAS(id).array(),
                               operands()[1]->getBoolAS(id).array());
    case TableExprFuncNode::arrdataFUNC:
        return MArray<String> (operands()[0]->getStringAS(id).array());
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
	Bool deleteVal, deleteDat;
	const String* val = values.array().getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	Quantity quant;
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    if (MVTime::read (quant, val[i])) {
		dat[i] = quant;
	    }
	    throw (TableInvExpr ("invalid date string " + val[i]));
	}
	values.array().freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return MArray<MVTime> (dates, values.mask());
      }
    case TableExprFuncNode::mjdtodateFUNC:
      {
	MArray<Double> values (operands()[0]->getArrayDouble(id));
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const Double* val = values.array().getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    dat[i] = MVTime (val[i]);
	}
	values.array().freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return MArray<MVTime> (dates, values.mask());
      }
    case TableExprFuncNode::dateFUNC:
      {
	MArray<MVTime> values (operands()[0]->getArrayDate(id));
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const MVTime* val = values.array().getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    dat[i] = MVTime (floor (Double (val[i])));
	}
	values.array().freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return MArray<MVTime> (dates, values.mask());
      }
    case TableExprFuncNode::arrayFUNC:
      {
        IPosition shp (getArrayShape(id));
	Array<MVTime> res(shp);
        Array<Bool> mask;
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getDate(id);
	} else {
	  MArray<MVTime> arr (operands()[0]->getArrayDate(id));
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
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
        return TEFNAiif<MVTime> (operands(), id);
    case TableExprFuncNode::marrayFUNC:
        return MArray<MVTime> (operands()[0]->getDateAS(id).array(),
                               operands()[1]->getBoolAS(id).array());
    case TableExprFuncNode::arrdataFUNC:
        return MArray<MVTime> (operands()[0]->getDateAS(id).array());
    case TableExprFuncNode::arrflatFUNC:
        return MArray<MVTime> (operands()[0]->getDateAS(id).flatten());
    default:
      break;
    }
    throw TableInvExpr ("TableExprFuncNodeArray::getArrayDate, "
                        "unknown function " +
                        String::toString(funcType()));
}

} //# NAMESPACE CASA - END
