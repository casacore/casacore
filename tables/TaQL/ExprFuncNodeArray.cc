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

#include <casacore/tables/TaQL/ExprFuncNodeArray.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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

void TableExprFuncNodeArray::getAggrNodes (vector<TableExprNodeRep*>& aggr)
{
    node_p.getAggrNodes (aggr);
}

void TableExprFuncNodeArray::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    node_p.getColumnNodes (cols);
}

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
  TableExprFuncNode::fillChildNodes (thisNode->getChild(), nodes, dtypeOper);
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
    Array<Int64> ax(operands()[axarg]->getArrayInt(id));
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
    Array<Int64> ax(operands()[axarg]->getArrayInt(id));
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


Array<Bool> TableExprFuncNodeArray::getArrayBool (const TableExprId& id)
{
    switch (funcType()) {
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
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	Array<uInt> res (partialNTrue (arr, getAxes(id, arr.ndim())));
	return res > 0u;
      }
    case TableExprFuncNode::allsFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	Array<uInt> res (partialNFalse (arr, getAxes(id, arr.ndim())));
	return res == 0u;
      }
    case TableExprFuncNode::runallFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	return slidingArrayMath (arr, getArrayShape(id), AllFunc());
      }
    case TableExprFuncNode::runanyFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	return slidingArrayMath (arr, getArrayShape(id), AnyFunc());
      }
    case TableExprFuncNode::boxallFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	return boxedArrayMath (arr, getArrayShape(id), AllFunc());
      }
    case TableExprFuncNode::boxanyFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	return boxedArrayMath (arr, getArrayShape(id), AnyFunc());
      }
    case TableExprFuncNode::arrayFUNC:
      {
	Array<Bool> res(getArrayShape(id));
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getBool(id);
	} else {
	  Array<Bool> arr (operands()[0]->getArrayBool(id));
	  Bool delRes, delArr;
	  Bool* resd = res.getStorage (delRes);
	  const Bool* arrd = arr.getStorage (delArr);
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
	return res;
      }
    case TableExprFuncNode::transposeFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
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
      {
	Array<Bool> arrc;
	Array<Bool> arr1, arr2;
	Bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
	Bool valc;
	Bool val1, val2;
	const Bool* datac = &valc;
	const Bool* data1 = &val1;
	const Bool* data2 = &val2;
	size_t incrc = 1;
	size_t incr1 = 1;
	size_t incr2 = 1;
	IPosition shp;
        if (operands()[0]->valueType() == VTScalar) {
	    valc = operands()[0]->getBool(id);
	    incrc = 0;
	} else {
	    arrc.reference (operands()[0]->getArrayBool(id));
	    shp = arrc.shape();
	    datac = arrc.getStorage (deleteArrc);
	}
        if (operands()[1]->valueType() == VTScalar) {
	    val1 = operands()[1]->getBool(id);
	    incr1 = 0;
	} else {
	    arr1.reference (operands()[1]->getArrayBool(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr1.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getBool, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr1.shape();
	    data1 = arr1.getStorage (deleteArr1);
	}
        if (operands()[2]->valueType() == VTScalar) {
	    val2 = operands()[2]->getBool(id);
	    incr2 = 0;
	} else {
	    arr2.reference (operands()[2]->getArrayBool(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr2.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getBool, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr2.shape();
	    data2 = arr2.getStorage (deleteArr2);
	}
	Array<Bool> result(shp);
	Bool* res = result.getStorage (deleteRes);
	size_t nr = result.nelements();
	size_t pc = 0;
	size_t p1 = 0;
	size_t p2 = 0;
	for (size_t i=0; i<nr; i++) {
	    if (datac[pc]) {
	        res[i] = data1[p1];
	    } else {
	        res[i] = data2[p2];
	    }
	    pc += incrc;
	    p1 += incr1;
	    p2 += incr2;
	}
	if (datac != &valc) {
	    arrc.freeStorage (datac, deleteArrc);
	}
	if (data1 != &val1) {
	    arr1.freeStorage (data1, deleteArr1);
	}
	if (data2 != &val2) {
	    arr2.freeStorage (data2, deleteArr2);
	}
	result.putStorage (res, deleteRes);
	return result;
      }
    default:
	throw TableInvExpr ("TableExprFuncNodeArray::getArrayBool, "
                            "unknown function " +
                            String::toString(funcType()));
    }
    return Array<Bool>();
}

Array<Int64> TableExprFuncNodeArray::getArrayInt (const TableExprId& id)
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
        if (argDataType() == NTDouble) {
            Array<Double> val (operands()[0]->getArrayDouble(id));
            Array<Int64> arr(val.shape());
            convertArray (arr, val);
            return arr;
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
	return result;
      }
    case TableExprFuncNode::strlengthFUNC:
      {
	Array<String> values (operands()[0]->getArrayString(id));
	Array<Int64> res(values.shape());
	Bool deleteVal, deleteRes;
	const String* val = values.getStorage (deleteVal);
	Int64* resp = res.getStorage (deleteRes);
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    resp[i] = val[i].length();
	}
	values.freeStorage (val, deleteVal);
	res.putStorage (resp, deleteRes);
	return res;
      }
    case TableExprFuncNode::yearFUNC:
    case TableExprFuncNode::monthFUNC:
    case TableExprFuncNode::dayFUNC:
    case TableExprFuncNode::weekdayFUNC:
    case TableExprFuncNode::weekFUNC:
      {
	Array<MVTime> values (operands()[0]->getArrayDate(id));
	Array<Int64> res(values.shape());
	Bool deleteVal, deleteRes;
	const MVTime* val = values.getStorage (deleteVal);
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
	values.freeStorage (val, deleteVal);
	res.putStorage (resp, deleteRes);
	return res;
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
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrminsFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return partialMins (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmaxsFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return partialMaxs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runminFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return slidingArrayMath (arr, getArrayShape(id), MinFunc<Int64>());
      }
    case TableExprFuncNode::runmaxFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return slidingArrayMath (arr, getArrayShape(id), MaxFunc<Int64>());
      }
    case TableExprFuncNode::boxminFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return boxedArrayMath (arr, getArrayShape(id), MinFunc<Int64>());
      }
    case TableExprFuncNode::boxmaxFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return boxedArrayMath (arr, getArrayShape(id), MaxFunc<Int64>());
      }
    case TableExprFuncNode::ntruesFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	Array<uInt> res(partialNTrue (arr, getAxes(id, arr.ndim())));
	Array<Int64> resd(res.shape());
	convertArray (resd, res);
	return resd;
      }
    case TableExprFuncNode::nfalsesFUNC:
      {
	Array<Bool> arr (operands()[0]->getArrayBool(id));
	Array<uInt> res(partialNFalse (arr, getAxes(id, arr.ndim())));
	Array<Int64> resd(res.shape());
	convertArray (resd, res);
	return resd;
      }
    case TableExprFuncNode::arrayFUNC:
      {
	Array<Int64> res(getArrayShape(id));
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getInt(id);
	} else {
	  Array<Int64> arr (operands()[0]->getArrayInt(id));
	  Bool delRes, delArr;
	  Int64* resd = res.getStorage (delRes);
	  const Int64* arrd = arr.getStorage (delArr);
	  size_t j=0;
          size_t arrsz = arr.nelements();
	  size_t n = res.nelements();
	  for (size_t i=0; i<n; i++) {
	    resd[i] = arrd[j++];
            // Start at beginning again if at the end.
	    if (j >= arrsz) {
	      j = 0;
	    }
	  }
	  res.putStorage (resd, delRes);
	  arr.freeStorage (arrd, delArr);
	}
	return res;
      }
    case TableExprFuncNode::transposeFUNC:
      {
	Array<Int64> arr (operands()[0]->getArrayInt(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
      {
	Array<Bool> arrc;
	Array<Int64> arr1, arr2;
	Bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
	Bool valc;
	Int64 val1, val2;
	const Bool* datac = &valc;
	const Int64* data1 = &val1;
	const Int64* data2 = &val2;
	size_t incrc = 1;
	size_t incr1 = 1;
	size_t incr2 = 1;
	IPosition shp;
        if (operands()[0]->valueType() == VTScalar) {
	    valc = operands()[0]->getBool(id);
	    incrc = 0;
	} else {
	    arrc.reference (operands()[0]->getArrayBool(id));
	    shp = arrc.shape();
	    datac = arrc.getStorage (deleteArrc);
	}
        if (operands()[1]->valueType() == VTScalar) {
	    val1 = operands()[1]->getInt(id);
	    incr1 = 0;
	} else {
	    arr1.reference (operands()[1]->getArrayInt(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr1.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getInt, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr1.shape();
	    data1 = arr1.getStorage (deleteArr1);
	}
        if (operands()[2]->valueType() == VTScalar) {
	    val2 = operands()[2]->getInt(id);
	    incr2 = 0;
	} else {
	    arr2.reference (operands()[2]->getArrayInt(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr2.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getInt, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr2.shape();
	    data2 = arr2.getStorage (deleteArr2);
	}
	Array<Int64> result(shp);
	Int64* res = result.getStorage (deleteRes);
	size_t nr = result.nelements();
	size_t pc = 0;
	size_t p1 = 0;
	size_t p2 = 0;
	for (size_t i=0; i<nr; i++) {
	    if (datac[pc]) {
	        res[i] = data1[p1];
	    } else {
	        res[i] = data2[p2];
	    }
	    pc += incrc;
	    p1 += incr1;
	    p2 += incr2;
	}
	if (datac != &valc) {
	    arrc.freeStorage (datac, deleteArrc);
	}
	if (data1 != &val1) {
	    arr1.freeStorage (data1, deleteArr1);
	}
	if (data2 != &val2) {
	    arr2.freeStorage (data2, deleteArr2);
	}
	result.putStorage (res, deleteRes);
	return result;
      }
    default:
	throw TableInvExpr ("TableExprFuncNodeArray::getArrayInt, "
                            "unknown function " +
                            String::toString(funcType()));
    }
    return Array<Int64>();
}

Array<Double> TableExprFuncNodeArray::getArrayDouble (const TableExprId& id)
{
    if (dataType() == NTInt) {
	return TableExprNodeArray::getArrayDouble (id);
    }
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
	Array<Double> res = sqrt (operands()[0]->getArrayDouble(id));
        if (node_p.getScale() != 1.) {
          arrayTransformInPlace (res, node_p.getScale(),
                                 casacore::Multiplies<Double,Double>());
        }
        return res;
      }
    case TableExprFuncNode::conjFUNC:
	return           operands()[0]->getArrayDouble(id);
    case TableExprFuncNode::normFUNC:
        if (argDataType() == NTDouble) {
            return square (operands()[0]->getArrayDouble(id));
	} else {
            Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	    Array<Double> result(arr.shape());
	    Bool deleteArr, deleteRes;
	    const DComplex* data = arr.getStorage (deleteArr);
	    Double* res = result.getStorage (deleteRes);
	    size_t nr = arr.nelements();
	    for (size_t i=0; i<nr; i++) {
	        res[i] = norm(data[i]);
	    }
	    arr.freeStorage (data, deleteArr);
	    result.putStorage (res, deleteRes);
	    return result;
	}
    case TableExprFuncNode::absFUNC:
	if (argDataType() == NTDouble) {
	    return abs (operands()[0]->getArrayDouble(id));
	}
	return amplitude (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::argFUNC:
	if (argDataType() == NTDouble) {
	    Double pival = atan2 (Double(0), Double(-1));  // results in pi
	    Array<Double> arr (operands()[0]->getArrayDouble(id).copy());
	    Bool deleteIt;
	    Double* data = arr.getStorage (deleteIt);
	    size_t nr = arr.nelements();
	    for (size_t i=0; i<nr; i++) {
	        if (data[i] >= 0) {
		    data[i] = 0;
		} else {
		    data[i] = pival;
		}
	    }
	    arr.putStorage (data, deleteIt);
	    return arr;
	}
	return phase (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::realFUNC:
	if (argDataType() == NTDouble) {
	    return operands()[0]->getArrayDouble(id);
	}
	return real (operands()[0]->getArrayDComplex(id));
    case TableExprFuncNode::imagFUNC:
	if (argDataType() == NTDouble) {
            IPosition shp (operands()[0]->shape(id));
	    Array<Double> result(shp);
	    result = 0;
	    return result;
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
	Array<MVTime> values (operands()[0]->getArrayDate(id));
	Array<Double> doubles(values.shape());
	Bool deleteVal, deleteDoub;
	const MVTime* val = values.getStorage (deleteVal);
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
	values.freeStorage (val, deleteVal);
	doubles.putStorage (doub, deleteDoub);
	return doubles;
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
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrminsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMins (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmaxsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMaxs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmeansFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMeans (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrvariancesFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialVariances (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrstddevsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialStddevs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arravdevsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialAvdevs (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrrmssFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialRmss (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmediansFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialMedians (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrfractilesFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return partialFractiles (arr,
				 getAxes(id, arr.ndim(), 2),
				 operands()[1]->getDouble(id));
      }
    case TableExprFuncNode::runminFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), MinFunc<Double>());
      }
    case TableExprFuncNode::runmaxFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), MaxFunc<Double>());
      }
    case TableExprFuncNode::runmeanFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), MeanFunc<Double>());
      }
    case TableExprFuncNode::runvarianceFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), VarianceFunc<Double>());
      }
    case TableExprFuncNode::runstddevFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), StddevFunc<Double>());
      }
    case TableExprFuncNode::runavdevFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), AvdevFunc<Double>());
      }
    case TableExprFuncNode::runrmsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), RmsFunc<Double>());
      }
    case TableExprFuncNode::runmedianFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return slidingArrayMath (arr, getArrayShape(id), MedianFunc<Double>());
    }
    case TableExprFuncNode::boxminFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), MinFunc<Double>());
      }
    case TableExprFuncNode::boxmaxFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), MaxFunc<Double>());
      }
    case TableExprFuncNode::boxmeanFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), MeanFunc<Double>());
      }
    case TableExprFuncNode::boxvarianceFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), VarianceFunc<Double>());
      }
    case TableExprFuncNode::boxstddevFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), StddevFunc<Double>());
      }
    case TableExprFuncNode::boxavdevFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), AvdevFunc<Double>());
      }
    case TableExprFuncNode::boxrmsFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), RmsFunc<Double>());
      }
    case TableExprFuncNode::boxmedianFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return boxedArrayMath (arr, getArrayShape(id), MedianFunc<Double>());
    }
    case TableExprFuncNode::arrayFUNC:
      {
	Array<Double> res(getArrayShape(id));
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getDouble(id);
	} else {
	  Array<Double> arr (operands()[0]->getArrayDouble(id));
	  Bool delRes, delArr;
	  Double* resd = res.getStorage (delRes);
	  const Double* arrd = arr.getStorage (delArr);
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
	return res;
      }
    case TableExprFuncNode::transposeFUNC:
      {
	Array<Double> arr (operands()[0]->getArrayDouble(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
      {
	Array<Bool> arrc;
	Array<Double> arr1, arr2;
	Bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
	Bool valc;
	Double val1, val2;
	const Bool* datac = &valc;
	const Double* data1 = &val1;
	const Double* data2 = &val2;
	size_t incrc = 1;
	size_t incr1 = 1;
	size_t incr2 = 1;
	IPosition shp;
        if (operands()[0]->valueType() == VTScalar) {
	    valc = operands()[0]->getBool(id);
	    incrc = 0;
	} else {
	    arrc.reference (operands()[0]->getArrayBool(id));
	    shp = arrc.shape();
	    datac = arrc.getStorage (deleteArrc);
	}
        if (operands()[1]->valueType() == VTScalar) {
	    val1 = operands()[1]->getDouble(id);
	    incr1 = 0;
	} else {
	    arr1.reference (operands()[1]->getArrayDouble(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr1.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getDouble, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr1.shape();
	    data1 = arr1.getStorage (deleteArr1);
	}
        if (operands()[2]->valueType() == VTScalar) {
	    val2 = operands()[2]->getDouble(id);
	    incr2 = 0;
	} else {
	    arr2.reference (operands()[2]->getArrayDouble(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr2.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getDouble, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr2.shape();
	    data2 = arr2.getStorage (deleteArr2);
	}
	Array<Double> result(shp);
	Double* res = result.getStorage (deleteRes);
	size_t nr = result.nelements();
	size_t pc = 0;
	size_t p1 = 0;
	size_t p2 = 0;
	for (size_t i=0; i<nr; i++) {
	    if (datac[pc]) {
	        res[i] = data1[p1];
	    } else {
	        res[i] = data2[p2];
	    }
	    pc += incrc;
	    p1 += incr1;
	    p2 += incr2;
	}
	if (datac != &valc) {
	    arrc.freeStorage (datac, deleteArrc);
	}
	if (data1 != &val1) {
	    arr1.freeStorage (data1, deleteArr1);
	}
	if (data2 != &val2) {
	    arr2.freeStorage (data2, deleteArr2);
	}
	result.putStorage (res, deleteRes);
	return result;
      }
    case TableExprFuncNode::angdistFUNC:
      {
        Array<double> a1 = operands()[0]->getArrayDouble(id);
        Array<double> a2 = operands()[1]->getArrayDouble(id);
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
          Array<double> result(IPosition(1, a1.size()/2));
          double* res = result.data();
          Array<double>::const_iterator p2   = a2.begin();
          Array<double>::const_iterator end1 = a1.end();
          for (Array<double>::const_iterator p1 = a1.begin(); p1!=end1; ++p1) {
            double ra1 = *p1;
            ++p1;
            double ra2 = *p2;
            ++p2;
            *res++ = acos (sin(*p1)*sin(*p2) + cos(*p1)*cos(*p2)*cos(ra1-ra2));
            ++p2;
          }
          return result;
        }  // fall through if either arrays have size 2
      }
    case TableExprFuncNode::angdistxFUNC:
      {
        Array<double> a1 = operands()[0]->getArrayDouble(id);
        Array<double> a2 = operands()[1]->getArrayDouble(id);
        if (!(a1.size() %2 == 0  &&  a2.size() %2 == 0)) {
          throw TableInvExpr ("Arguments of angdistx function must have a "
                              "multiple of 2 values");
        }
        Array<double>::const_iterator end1 = a1.end();
        Array<double>::const_iterator end2 = a2.end();
        Array<double> result(IPosition(2, a1.size()/2, a2.size()/2));
        double* res = result.data();
        for (Array<double>::const_iterator p2 = a2.begin(); p2!=end2; ++p2) {
          double ra2     = *p2;
          ++p2;
          double sindec2 = sin(*p2);
          double cosdec2 = cos(*p2);
          for (Array<double>::const_iterator p1 = a1.begin(); p1!=end1; ++p1) {
            double ra1 = *p1;
            ++p1;
            *res++ = acos (sin(*p1)*sindec2 + cos(*p1)*cosdec2*cos(ra1-ra2));
          }
        }
        return result;
      }
    case TableExprFuncNode::datetimeFUNC:
    case TableExprFuncNode::mjdtodateFUNC:
    case TableExprFuncNode::dateFUNC:
      {
        Array<MVTime> arr (getArrayDate(id));
        Array<Double> res(arr.shape());
        convertArray (res, arr);
        return res;
      }
    default:
      {
        // Functions like YEAR are implemented as Int only.
        Array<Int64> arr (getArrayInt(id));
        Array<Double> res(arr.shape());
        convertArray (res, arr);
        return res;
      }
    }
    return Array<Double>();
}

Array<DComplex> TableExprFuncNodeArray::getArrayDComplex
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
	Array<DComplex> res = sqrt (operands()[0]->getArrayDComplex(id));
        if (node_p.getScale() != 1.) {
          arrayTransformInPlace (res, node_p.getScale(),
                                 casacore::Multiplies<DComplex,Double>());
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
            Array<DComplex> arr1 (operands()[0]->getArrayDComplex(id));
	    Array<DComplex> arr2 (arr1.shape());
	    arr2 = operands()[1]->getDComplex(id);
	    return pow (arr1, arr2);
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
	Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return partialSums (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrproductsFUNC:
      {
	Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return partialProducts (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrsumsqrsFUNC:
      {
	Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return partialSums (arr*arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::arrmeansFUNC:
      {
	Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return partialMeans (arr, getAxes(id, arr.ndim()));
      }
    case TableExprFuncNode::runmeanFUNC:
      {
	Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return slidingArrayMath (arr, getArrayShape(id), MeanFunc<DComplex>());
      }
    case TableExprFuncNode::boxmeanFUNC:
      {
	Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return boxedArrayMath (arr, getArrayShape(id), MeanFunc<DComplex>());
      }
    case TableExprFuncNode::arrayFUNC:
      {
	Array<DComplex> res(getArrayShape(id));
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getDComplex(id);
	} else {
	  Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	  Bool delRes, delArr;
	  DComplex* resd = res.getStorage (delRes);
	  const DComplex* arrd = arr.getStorage (delArr);
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
	return res;
      }
    case TableExprFuncNode::transposeFUNC:
      {
	Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::complexFUNC:
      {
        Array<DComplex> result;
	Array<Double> arr;
	Bool deleteArr, deleteArr2, deleteRes;
	DComplex* res;
	const Double* data;
        if (operands()[0]->valueType() == VTScalar) {
	    Double val = operands()[0]->getDouble(id);
	    arr.reference (operands()[1]->getArrayDouble(id));
	    result.resize (arr.shape());
	    data = arr.getStorage (deleteArr);
	    res = result.getStorage (deleteRes);
	    size_t nr = arr.nelements();
	    for (size_t i=0; i<nr; i++) {
	        res[i] = DComplex(val, data[i]);
	    }
	} else if (operands()[1]->valueType() == VTScalar) {
	    Double val = operands()[1]->getDouble(id);
	    arr.reference (operands()[0]->getArrayDouble(id));
	    result.resize (arr.shape());
	    data = arr.getStorage (deleteArr);
	    res = result.getStorage (deleteRes);
	    size_t nr = arr.nelements();
	    for (size_t i=0; i<nr; i++) {
	        res[i] = DComplex(data[i], val);
	    }
	} else {
	    arr.reference (operands()[0]->getArrayDouble(id));
	    Array<Double> arr2 (operands()[1]->getArrayDouble(id));
	    if (! arr2.shape().isEqual (arr.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getDComplex, "
				  "array shapes mismatch in function COMPLEX");
	    }
	    result.resize (arr.shape());
	    data = arr.getStorage (deleteArr);
	    const Double* data2 = arr2.getStorage (deleteArr2);
	    res = result.getStorage (deleteRes);
	    size_t nr = arr.nelements();
	    for (size_t i=0; i<nr; i++) {
	        res[i] = DComplex(data[i], data2[i]);
	    }
	    arr2.freeStorage (data2, deleteArr2);
	}
	arr.freeStorage (data, deleteArr);
	result.putStorage (res, deleteRes);
	return result;
      }
    case TableExprFuncNode::iifFUNC:
      {
	Array<Bool> arrc;
	Array<DComplex> arr1, arr2;
	Bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
	Bool valc;
	DComplex val1, val2;
	const Bool* datac = &valc;
	const DComplex* data1 = &val1;
	const DComplex* data2 = &val2;
	size_t incrc = 1;
	size_t incr1 = 1;
	size_t incr2 = 1;
	IPosition shp;
        if (operands()[0]->valueType() == VTScalar) {
	    valc = operands()[0]->getBool(id);
	    incrc = 0;
	} else {
	    arrc.reference (operands()[0]->getArrayBool(id));
	    shp = arrc.shape();
	    datac = arrc.getStorage (deleteArrc);
	}
        if (operands()[1]->valueType() == VTScalar) {
	    val1 = operands()[1]->getDComplex(id);
	    incr1 = 0;
	} else {
	    arr1.reference (operands()[1]->getArrayDComplex(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr1.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getDComplex, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr1.shape();
	    data1 = arr1.getStorage (deleteArr1);
	}
        if (operands()[2]->valueType() == VTScalar) {
	    val2 = operands()[2]->getDComplex(id);
	    incr2 = 0;
	} else {
	    arr2.reference (operands()[2]->getArrayDComplex(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr2.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getDComplex, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr2.shape();
	    data2 = arr2.getStorage (deleteArr2);
	}
	Array<DComplex> result(shp);
	DComplex* res = result.getStorage (deleteRes);
	size_t nr = result.nelements();
	size_t pc = 0;
	size_t p1 = 0;
	size_t p2 = 0;
	for (size_t i=0; i<nr; i++) {
	    if (datac[pc]) {
	        res[i] = data1[p1];
	    } else {
	        res[i] = data2[p2];
	    }
	    pc += incrc;
	    p1 += incr1;
	    p2 += incr2;
	}
	if (datac != &valc) {
	    arrc.freeStorage (datac, deleteArrc);
	}
	if (data1 != &val1) {
	    arr1.freeStorage (data1, deleteArr1);
	}
	if (data2 != &val2) {
	    arr2.freeStorage (data2, deleteArr2);
	}
	result.putStorage (res, deleteRes);
	return result;
      }
    default:
	throw TableInvExpr ("TableExprFuncNodeArray::getArrayDComplex, "
                            "unknown function " +
                            String::toString(funcType()));
    }
    return Array<DComplex>();
}

Array<String> TableExprFuncNodeArray::getArrayString (const TableExprId& id)
{
    switch (funcType()) {
    case TableExprFuncNode::upcaseFUNC:
    case TableExprFuncNode::downcaseFUNC:
    case TableExprFuncNode::capitalizeFUNC:
    case TableExprFuncNode::trimFUNC:
    case TableExprFuncNode::ltrimFUNC:
    case TableExprFuncNode::rtrimFUNC:
    case TableExprFuncNode::substrFUNC:
    case TableExprFuncNode::replaceFUNC:
      {
        static Regex leadingWS("^[ \t]*");
        static Regex trailingWS("[ \t]*$");
	Array<String> strings (operands()[0]->getArrayString(id).copy());
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
	case TableExprFuncNode::capitalizeFUNC:
	    for (i=0; i<n; i++) {
		str[i].capitalize();
	    }
	    break;
	case TableExprFuncNode::trimFUNC:
	    for (i=0; i<n; i++) {
                str[i].trim();
	    }
	    break;
	case TableExprFuncNode::ltrimFUNC:
	    for (i=0; i<n; i++) {
                str[i].gsub (leadingWS, String());
	    }
	    break;
	case TableExprFuncNode::rtrimFUNC:
	    for (i=0; i<n; i++) {
                str[i].gsub (trailingWS, String());
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
	return strings;
	break;
      }
    case TableExprFuncNode::cmonthFUNC:
    case TableExprFuncNode::cdowFUNC:	
    case TableExprFuncNode::ctodFUNC:	
    case TableExprFuncNode::cdateFUNC:	
    case TableExprFuncNode::ctimeFUNC:	
      {
	Array<MVTime> values (operands()[0]->getArrayDate(id));
	Array<String> strings(values.shape());
	Bool deleteVal, deleteStr;
	const MVTime* val = values.getStorage (deleteVal);
	String* str = strings.getStorage (deleteStr);
	size_t n = values.nelements();
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
	values.freeStorage (val, deleteVal);
	strings.putStorage (str, deleteStr);
	return strings;
        break;
      }
    case TableExprFuncNode::stringFUNC:
      {
        String fmt;
        Int width, prec;
        TableExprFuncNode::getPrintFormat (fmt, width, prec, operands(), id);
        Array<String> res;
        if (operands()[0]->dataType() == NTBool) {
          Array<Bool> arr (operands()[0]->getArrayBool(id));
          res.resize (arr.shape());
          Array<Bool>::const_iterator arrIter = arr.begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt, width);
          }
        } else if (operands()[0]->dataType() == NTInt) {
          Array<Int64> arr (operands()[0]->getArrayInt(id));
          res.resize (arr.shape());
          Array<Int64>::const_iterator arrIter = arr.begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt, width);
          }
        } else if (operands()[0]->dataType() == NTDouble) {
          std::pair<int,int> mvFormat = TableExprFuncNode::getMVFormat(fmt);
          Array<Double> arr (operands()[0]->getArrayDouble(id));
          res.resize (arr.shape());
          Array<Double>::const_iterator arrIter = arr.begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
           *resIter = TableExprFuncNode::stringValue (*arrIter, fmt,
                                                      width, prec, mvFormat,
                                                      operands()[0]->unit());
          }
        } else if (operands()[0]->dataType() == NTComplex) {
          Array<DComplex> arr (operands()[0]->getArrayDComplex(id));
          res.resize (arr.shape());
          Array<DComplex>::const_iterator arrIter = arr.begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
           *resIter = TableExprFuncNode::stringValue (*arrIter, fmt,
                                                      width, prec);
          }
        } else if (operands()[0]->dataType() == NTDate) {
          std::pair<int,int> mvFormat = TableExprFuncNode::getMVFormat(fmt);
          Array<MVTime> arr (operands()[0]->getArrayDate(id));
          res.resize (arr.shape());
          Array<MVTime>::const_iterator arrIter = arr.begin();
          Array<String>::iterator iterEnd = res.end();
          for (Array<String>::iterator resIter = res.begin();
               resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt,
                                                       width, mvFormat);
          }
        } else {
          Array<String> arr (operands()[0]->getArrayString(id));
          Array<String> res(arr.shape());
          Array<String>::const_iterator arrIter = arr.begin();
          Array<String>::iterator iterEnd = res.end();
         for (Array<String>::iterator resIter = res.begin();
              resIter != iterEnd; ++resIter, ++arrIter) {
            *resIter = TableExprFuncNode::stringValue (*arrIter, fmt, width);
          }
        }
        return res;
      }
    case TableExprFuncNode::hmsFUNC:
    case TableExprFuncNode::dmsFUNC:
    case TableExprFuncNode::hdmsFUNC:
      {
	Array<Double> values (operands()[0]->getArrayDouble(id));
	Array<String> strings(values.shape());
	Bool deleteVal, deleteStr;
	const Double* val = values.getStorage (deleteVal);
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
	values.freeStorage (val, deleteVal);
	strings.putStorage (str, deleteStr);
        return strings;
        break;
      }
    case TableExprFuncNode::arrayFUNC:
      {
	Array<String> res(getArrayShape(id));
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getString(id);
	} else {
	  Array<String> arr (operands()[0]->getArrayString(id));
	  Bool delRes, delArr;
	  String* resd = res.getStorage (delRes);
	  const String* arrd = arr.getStorage (delArr);
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
	return res;
      }
    case TableExprFuncNode::transposeFUNC:
      {
	Array<String> arr (operands()[0]->getArrayString(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
      {
	Array<Bool> arrc;
	Array<String> arr1, arr2;
	Bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
	Bool valc;
	String val1, val2;
	const Bool* datac = &valc;
	const String* data1 = &val1;
	const String* data2 = &val2;
	size_t incrc = 1;
	size_t incr1 = 1;
	size_t incr2 = 1;
	IPosition shp;
        if (operands()[0]->valueType() == VTScalar) {
	    valc = operands()[0]->getBool(id);
	    incrc = 0;
	} else {
	    arrc.reference (operands()[0]->getArrayBool(id));
	    shp = arrc.shape();
	    datac = arrc.getStorage (deleteArrc);
	}
        if (operands()[1]->valueType() == VTScalar) {
	    val1 = operands()[1]->getString(id);
	    incr1 = 0;
	} else {
	    arr1.reference (operands()[1]->getArrayString(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr1.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getString, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr1.shape();
	    data1 = arr1.getStorage (deleteArr1);
	}
        if (operands()[2]->valueType() == VTScalar) {
	    val2 = operands()[2]->getString(id);
	    incr2 = 0;
	} else {
	    arr2.reference (operands()[2]->getArrayString(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr2.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getString, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr2.shape();
	    data2 = arr2.getStorage (deleteArr2);
	}
	Array<String> result(shp);
	String* res = result.getStorage (deleteRes);
	size_t nr = result.nelements();
	size_t pc = 0;
	size_t p1 = 0;
	size_t p2 = 0;
	for (size_t i=0; i<nr; i++) {
	    if (datac[pc]) {
	        res[i] = data1[p1];
	    } else {
	        res[i] = data2[p2];
	    }
	    pc += incrc;
	    p1 += incr1;
	    p2 += incr2;
	}
	if (datac != &valc) {
	    arrc.freeStorage (datac, deleteArrc);
	}
	if (data1 != &val1) {
	    arr1.freeStorage (data1, deleteArr1);
	}
	if (data2 != &val2) {
	    arr2.freeStorage (data2, deleteArr2);
	}
	result.putStorage (res, deleteRes);
	return result;
      }
    default:
	throw TableInvExpr ("TableExprFuncNodeArray::getArrayString, "
                            "unknown function " +
                            String::toString(funcType()));
    }
    return Array<String>();
}

Array<MVTime> TableExprFuncNodeArray::getArrayDate (const TableExprId& id)
{
    switch (funcType()) {
    case TableExprFuncNode::datetimeFUNC:
      {
	Array<String> values (operands()[0]->getArrayString(id));
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const String* val = values.getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	Quantity quant;
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    if (MVTime::read (quant, val[i])) {
		dat[i] = quant;
	    }
	    throw (TableInvExpr ("invalid date string " + val[i]));
	}
	values.freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return dates;
      }
    case TableExprFuncNode::mjdtodateFUNC:
      {
	Array<Double> values (operands()[0]->getArrayDouble(id));
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const Double* val = values.getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    dat[i] = MVTime (val[i]);
	}
	values.freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return dates;
      }
    case TableExprFuncNode::dateFUNC:
      {
	Array<MVTime> values (operands()[0]->getArrayDate(id));
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const MVTime* val = values.getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	size_t n = values.nelements();
	for (size_t i=0; i<n; i++) {
	    dat[i] = MVTime (floor (Double (val[i])));
	}
	values.freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return dates;
      }
    case TableExprFuncNode::arrayFUNC:
      {
	Array<MVTime> res(getArrayShape(id));
        if (operands()[0]->valueType() == VTScalar) {
	  res = operands()[0]->getDate(id);
	} else {
	  Array<MVTime> arr (operands()[0]->getArrayDate(id));
	  Bool delRes, delArr;
	  MVTime* resd = res.getStorage (delRes);
	  const MVTime* arrd = arr.getStorage (delArr);
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
	return res;
      }
    case TableExprFuncNode::transposeFUNC:
      {
	Array<MVTime> arr (operands()[0]->getArrayDate(id));
	return reorderArray (arr, getOrder(id, arr.ndim()), False);
      }
    case TableExprFuncNode::iifFUNC:
      {
	Array<Bool> arrc;
	Array<MVTime> arr1, arr2;
	Bool deleteArrc, deleteArr1, deleteArr2, deleteRes;
	Bool valc;
	MVTime val1, val2;
	const Bool* datac = &valc;
	const MVTime* data1 = &val1;
	const MVTime* data2 = &val2;
	size_t incrc = 1;
	size_t incr1 = 1;
	size_t incr2 = 1;
	IPosition shp;
        if (operands()[0]->valueType() == VTScalar) {
	    valc = operands()[0]->getBool(id);
	    incrc = 0;
	} else {
	    arrc.reference (operands()[0]->getArrayBool(id));
	    shp = arrc.shape();
	    datac = arrc.getStorage (deleteArrc);
	}
        if (operands()[1]->valueType() == VTScalar) {
	    val1 = operands()[1]->getDate(id);
	    incr1 = 0;
	} else {
	    arr1.reference (operands()[1]->getArrayDate(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr1.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getDate, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr1.shape();
	    data1 = arr1.getStorage (deleteArr1);
	}
        if (operands()[2]->valueType() == VTScalar) {
	    val2 = operands()[2]->getDate(id);
	    incr2 = 0;
	} else {
	    arr2.reference (operands()[2]->getArrayDate(id));
	    if (shp.nelements() > 0  &&  ! shp.isEqual (arr2.shape())) {
	        throw TableInvExpr ("TableExprFuncNodeArray::getDate, "
				    "array shapes mismatch in function IIF");
	    }
	    shp = arr2.shape();
	    data2 = arr2.getStorage (deleteArr2);
	}
	Array<MVTime> result(shp);
	MVTime* res = result.getStorage (deleteRes);
	size_t nr = result.nelements();
	size_t pc = 0;
	size_t p1 = 0;
	size_t p2 = 0;
	for (size_t i=0; i<nr; i++) {
	    if (datac[pc]) {
	        res[i] = data1[p1];
	    } else {
	        res[i] = data2[p2];
	    }
	    pc += incrc;
	    p1 += incr1;
	    p2 += incr2;
	}
	if (datac != &valc) {
	    arrc.freeStorage (datac, deleteArrc);
	}
	if (data1 != &val1) {
	    arr1.freeStorage (data1, deleteArr1);
	}
	if (data2 != &val2) {
	    arr2.freeStorage (data2, deleteArr2);
	}
	result.putStorage (res, deleteRes);
	return result;
      }
    default:
	throw TableInvExpr ("TableExprFuncNodeArray::getArrayDate, "
                            "unknown function " +
                            String::toString(funcType()));
    }
    return Array<MVTime>();
}

} //# NAMESPACE CASACORE - END
