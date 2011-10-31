//# ExprFuncNode.cc: Class representing a function in table select expression
//# Copyright (C) 1994,1995,1996,1997,1998,2000,2001,2003
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

#include <tables/Tables/ExprFuncNode.h>
#include <tables/Tables/TableError.h>
#include <tables/Tables/ExprNode.h>
#include <tables/Tables/ExprNodeSet.h>
#include <tables/Tables/ExprDerNode.h>
#include <tables/Tables/ExprUnitNode.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QMath.h>
#include <casa/OS/Time.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <casa/Utilities/Assert.h>


namespace casa { //# NAMESPACE CASA - BEGIN

TableExprFuncNode::TableExprFuncNode (FunctionType ftype, NodeDataType dtype,
				      ValueType vtype,
				      const TableExprNodeSet& source)
: TableExprNodeMulti (dtype, vtype, OtFunc, source),
  funcType_p         (ftype),
  argDataType_p      (dtype),
  scale_p            (1)
{}

TableExprFuncNode::~TableExprFuncNode()
{}

Bool TableExprFuncNode::isSingleValue() const
{
  // Constant and group reduction functions result in a single value.
  // Note that in the column-list something like max(column) is ambiguous
  // because for arrays it could mean take the max of each array (resulting in
  // a scalar per row) or take the max of the corresponding elements of all
  // arrays in the column (resulting in a single array).
  // For now max(column) means the first for arrays, but for scalars it is the
  // max of the entire column (to be SQL compliant).
  // If needed, we can later introduce maxc for the meaning max(column),
  // thus a result as an array with the meaning max(row1, row2, ...).
  // This is true for all reduction functions below.
  switch (funcType_p) {
  case piFUNC:
  case eFUNC:
  case randFUNC:
    return True;
  case arrsumFUNC:
  case arrproductFUNC:
  case arrsumsqrFUNC:
  case arrminFUNC:
  case arrmaxFUNC:
  case arrmeanFUNC:
  case arrvarianceFUNC:
  case arrstddevFUNC:
  case arravdevFUNC:
  case arrrmsFUNC:
  case arrmedianFUNC:
  case arrfractileFUNC:
  case anyFUNC:
  case allFUNC:
  case ntrueFUNC:
  case nfalseFUNC:
    return operands_p[0]->valueType() == VTScalar;
  default:
    break;
  }
  // Otherwise all children have to be such.
  for (uInt i=0; i<operands_p.size(); ++i) {
    if (! operands_p[0]->isSingleValue()) {
      return False;
    }
  }
  return True;
}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
// If one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprFuncNode::fillNode
                                   (TableExprFuncNode* thisNode,
				    PtrBlock<TableExprNodeRep*>& nodes,
				    const Block<Int>& dtypeOper)
{
    // Fill child nodes as needed. It also fills ooperands_p.
    fillChildNodes (thisNode, nodes, dtypeOper);
    // Set the unit for some functions.
    Double scale = fillUnits (thisNode, thisNode->operands_p,
                              thisNode->funcType());
    thisNode->setScale (scale);
    // Some functions on a variable can already give a constant result.
    thisNode->tryToConst();
    if (thisNode->operands_p.nelements() > 0) {
	return convertNode (thisNode, True);
    }
    return thisNode;
}

Double TableExprFuncNode::fillUnits (TableExprNodeRep* node,
                                     PtrBlock<TableExprNodeRep*>& nodes,
                                     FunctionType func)
{
  Double scale = 1;
  if (nodes.nelements() > 0) {
    const Unit& childUnit = nodes[0]->unit();
    switch (func) {
    case asinFUNC:
    case acosFUNC:
    case atanFUNC:
    case atan2FUNC:
    case timeFUNC:
    case argFUNC:
      // These functions return radians.
      node->setUnit ("rad");
      break;
    case mjdFUNC:
      // These functions return days.
      node->setUnit ("d");
      break;
    case mjdtodateFUNC:
      // These functions require days.
      if (! childUnit.empty()) {
	TableExprNodeUnit::adaptUnit (nodes[0], "d");
      }
      break;
    case absFUNC:
    case realFUNC:
    case imagFUNC:
    case conjFUNC:
    case roundFUNC:
    case floorFUNC:
    case ceilFUNC:
    case fmodFUNC:
    case arrsumFUNC:
    case arrminFUNC:
    case arrmaxFUNC:
    case arrmeanFUNC:
    case arrstddevFUNC:
    case arravdevFUNC:
    case arrrmsFUNC:
    case arrmedianFUNC:
    case arrfractileFUNC:
    case arrsumsFUNC:
    case arrminsFUNC:
    case arrmaxsFUNC:
    case arrmeansFUNC:
    case arrstddevsFUNC:
    case arravdevsFUNC:
    case arrrmssFUNC:
    case arrmediansFUNC:
    case arrfractilesFUNC:
    case runminFUNC:
    case runmaxFUNC:
    case runmeanFUNC:
    case runstddevFUNC:
    case runavdevFUNC:
    case runrmsFUNC:
    case runmedianFUNC:
    case boxminFUNC:
    case boxmaxFUNC:
    case boxmeanFUNC:
    case boxstddevFUNC:
    case boxavdevFUNC:
    case boxrmsFUNC:
    case boxmedianFUNC:
      // These functions return the same unit as their child.
      node->setUnit (childUnit);
      break;
    case normFUNC:
    case squareFUNC:
    case arrsumsqrFUNC:
    case arrsumsqrsFUNC:
    case arrvarianceFUNC:
    case arrvariancesFUNC:
    case runvarianceFUNC:
    case boxvarianceFUNC:
      // These functions return the square of their child.
     if (! childUnit.empty()) {
       Quantity q(1., childUnit);
       node->setUnit (pow(q,2).getFullUnit());
     }
     break;
    case cubeFUNC:
     if (! childUnit.empty()) {
       Quantity q(1., childUnit);
       node->setUnit (pow(q,3).getFullUnit());
     }
     break;
    case sqrtFUNC:
      // These functions return the sqrt of their child.
      if (! childUnit.empty()) {
	Quantity q(1., childUnit);
        Quantity qs(sqrt(q));
        // sqrt result is always in SI units, so scaling might be involved.
        scale = qs.getValue();
	node->setUnit (qs.getFullUnit());
      }
      break;
    case sinFUNC:
    case cosFUNC:
    case tanFUNC:
    case hmsFUNC:
    case dmsFUNC:
    case hdmsFUNC:
      // These functions return no unit, but their child must be in radians.
      if (! childUnit.empty()) {
	TableExprNodeUnit::adaptUnit (nodes[0], "rad");
      }
      break;
    case iifFUNC:
      node->setUnit (makeEqualUnits (nodes, 1, nodes.nelements()));
      break;
    case complexFUNC:
    case minFUNC:
    case maxFUNC:
      node->setUnit (makeEqualUnits (nodes, 0, nodes.nelements()));
      break;
    case near2FUNC:
    case nearabs2FUNC:
    case near3FUNC:
      makeEqualUnits (nodes, 0, 2);
      break;
    case nearabs3FUNC:
      makeEqualUnits (nodes, 0, 3);
      break;
    case conesFUNC:
    case cones3FUNC:
    case anyconeFUNC:
    case anycone3FUNC:
    case findconeFUNC:
    case findcone3FUNC:
    case angdistFUNC:
      for (uInt i=0; i<nodes.nelements(); ++i) {
	TableExprNodeUnit::adaptUnit (nodes[i], "rad");
      }
      if (func == angdistFUNC) {
        node->setUnit ("rad");
      }
      break;
    default:
      // Several functions (e.g. intFUNC, powFUNC) do not set units
      break;
    }
  }
  return scale;
}

const Unit& TableExprFuncNode::makeEqualUnits
                             (PtrBlock<TableExprNodeRep*>& nodes,
			      uInt starg, uInt endarg)
{
  // These functions have multiple children, which must have the same unit.
  // The first real unit is chosen as the result unit.
  const Unit* unit = &(nodes[starg]->unit());
  for (uInt i=starg; i<endarg; ++i) {
    if (! nodes[i]->unit().empty()) {
      unit = &(nodes[i]->unit());
      break;
    }
  }
  if (! unit->empty()) {
    for (uInt i=starg; i<endarg; ++i) {
      TableExprNodeUnit::adaptUnit (nodes[i], *unit);
    }
  }
  return *unit;
}

// Fill the children pointers of a node.
void TableExprFuncNode::fillChildNodes (TableExprFuncNode* thisNode,
					PtrBlock<TableExprNodeRep*>& nodes,
					const Block<Int>& dtypeOper)
{
    uInt i;
    // Copy block of children.
    // Determine if common argument type is Int, Double or Complex.
    // (this is used by some functions like near and norm).
    thisNode->operands_p.resize (nodes.nelements());
    thisNode->argDataType_p = NTInt;
    for (i=0; i<nodes.nelements(); i++) {
	thisNode->operands_p[i] = nodes[i]->link();
	if (nodes[i]->dataType() == NTDouble
          &&  thisNode->argDataType_p != NTComplex) {
	    thisNode->argDataType_p = NTDouble;
	} else if (nodes[i]->dataType() == NTComplex) {
	    thisNode->argDataType_p = NTComplex;
	}
    }
    // Convert String to Date if needed
    for (i=0; i<nodes.nelements(); i++) {
	if (dtypeOper[i] == NTDate) {
            if (nodes[i]->dataType() == NTString) {
                TableExprNode dNode = datetime (thisNode->operands_p[i]);
                unlink (thisNode->operands_p[i]);
                thisNode->operands_p[i] = getRep (dNode)->link();
            } else if (nodes[i]->dataType() == NTDouble) {
                TableExprNode dNode = mjdtodate (thisNode->operands_p[i]);
                unlink (thisNode->operands_p[i]);
                thisNode->operands_p[i] = getRep (dNode)->link();
            }
        }
    }
}

void TableExprFuncNode::tryToConst()
{
    switch (funcType_p) {
    case ndimFUNC:
	if (operands_p[0]->ndim() >= 0) {
	    exprtype_p = Constant;
	}
	break;
    case nelemFUNC:
    case isdefFUNC:
	if (operands_p[0]->ndim() == 0
        ||  operands_p[0]->shape().nelements() > 0  ) {
	    exprtype_p = Constant;
	}
	break;
    default:
	break;
    }
}

Bool TableExprFuncNode::getBool (const TableExprId& id)
{
    switch (funcType_p) {
    case anyFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return anyTrue (operands_p[0]->getArrayBool(id));
	}
	return operands_p[0]->getBool (id);
    case allFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return allTrue (operands_p[0]->getArrayBool(id));
	}
	return operands_p[0]->getBool (id);
    case isnanFUNC:
	if (argDataType_p == NTComplex) {
            return isNaN(operands_p[0]->getDComplex(id));
	}
        return isNaN(operands_p[0]->getDouble(id));
    case isinfFUNC:
	if (argDataType_p == NTComplex) {
            return isInf(operands_p[0]->getDComplex(id));
	}
        return isInf(operands_p[0]->getDouble(id));
    case isfiniteFUNC:
        if (argDataType_p == NTComplex) {
            return isFinite(operands_p[0]->getDComplex(id));
	}
        return isFinite(operands_p[0]->getDouble(id));
    case isdefFUNC:
	return operands_p[0]->isDefined (id);
    case near2FUNC:
	if (argDataType_p == NTDouble) {
	    return near (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id),
			 1.0e-13);
	}
	return near (operands_p[0]->getDComplex(id),
		     operands_p[1]->getDComplex(id),
		     1.0e-13);
    case near3FUNC:
	if (argDataType_p == NTDouble) {
	    return near (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id),
			 operands_p[2]->getDouble(id));
	}
	return near (operands_p[0]->getDComplex(id),
		     operands_p[1]->getDComplex(id),
		     operands_p[2]->getDouble(id));
    case nearabs2FUNC:
	if (argDataType_p == NTDouble) {
	    return nearAbs (operands_p[0]->getDouble(id),
			    operands_p[1]->getDouble(id),
			    1.0e-13);
	}
	return nearAbs (operands_p[0]->getDComplex(id),
			operands_p[1]->getDComplex(id),
			1.0e-13);
    case nearabs3FUNC:
	if (argDataType_p == NTDouble) {
	    return nearAbs (operands_p[0]->getDouble(id),
			    operands_p[1]->getDouble(id),
			    operands_p[2]->getDouble(id));
	}
	return nearAbs (operands_p[0]->getDComplex(id),
			operands_p[1]->getDComplex(id),
			operands_p[2]->getDouble(id));
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getBool(id) : operands_p[2]->getBool(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getBool, "
			     "unknown function " +
                             String::toString(funcType_p)));
    }
    return True;
}

Int64 TableExprFuncNode::getInt (const TableExprId& id)
{
    switch(funcType_p) {
    case powFUNC:
      {
        Double val = pow (operands_p[0]->getDouble(id),
                          operands_p[1]->getDouble(id));
	return Int64(val<0 ? ceil(val-0.5) : floor(val+0.5));
      }
    case squareFUNC:
      {
	Int64 val = operands_p[0]->getInt(id);
	return val * val;
      }
    case cubeFUNC:
      {
	Int64 val = operands_p[0]->getInt(id);
	return val * val * val;
      }
    case minFUNC:
        return std::min (operands_p[0]->getInt(id),
                         operands_p[1]->getInt(id));
    case maxFUNC:
        return std::max (operands_p[0]->getInt(id),
                         operands_p[1]->getInt(id));
    case normFUNC:
        {
	    Int64 val = operands_p[0]->getInt(id);
	    return val * val;
        }
    case absFUNC:
        return abs (operands_p[0]->getInt(id));
    case intFUNC:
	if (argDataType_p == NTDouble) {
            return Int64 (operands_p[0]->getDouble(id));
        }
        return operands_p[0]->getInt(id);
    case signFUNC:
      {
	Int64 val = operands_p[0]->getInt(id);
	if (val > 0) {
	    return 1;
	}
	if (val < 0) {
	    return -1;
	}
	return 0;
      }
    case roundFUNC:
	return operands_p[0]->getInt(id);
    case floorFUNC:
	return operands_p[0]->getInt(id);
    case ceilFUNC:
	return operands_p[0]->getInt(id);
    case fmodFUNC:
	return operands_p[0]->getInt(id) % operands_p[1]->getInt(id);
    case strlengthFUNC:
	return operands_p[0]->getString (id).length();
    case yearFUNC:
	return operands_p[0]->getDate(id).year();
    case monthFUNC:
	return operands_p[0]->getDate(id).month();
    case dayFUNC:
	return operands_p[0]->getDate(id).monthday();
    case weekdayFUNC:
	return operands_p[0]->getDate(id).weekday();
    case weekFUNC:
	return operands_p[0]->getDate(id).yearweek();
    case arrminFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return min (operands_p[0]->getArrayInt (id));
	}
	return operands_p[0]->getInt (id);
    case arrmaxFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return max (operands_p[0]->getArrayInt (id));
	}
	return operands_p[0]->getInt (id);
    case arrsumFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return sum (operands_p[0]->getArrayInt (id));
	}
	return operands_p[0]->getInt (id);
    case arrproductFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return product (operands_p[0]->getArrayInt (id));
	}
	return operands_p[0]->getInt (id);
    case arrsumsqrFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<Int64> arr = operands_p[0]->getArrayInt (id);
            AlwaysAssert (arr.contiguousStorage(), AipsError);
            return std::accumulate(arr.cbegin(), arr.cend(), Int64(0),
                                   casa::SumSqr<Int64>());
	} else {
	    Int64 val = operands_p[0]->getInt(id);
	    return val * val;
	}
    case ntrueFUNC:
        if (operands_p[0]->valueType() == VTArray) {
            return ntrue (operands_p[0]->getArrayBool (id));
	}
	return (operands_p[0]->getBool(id)  ?  1 : 0);
    case nfalseFUNC:
        if (operands_p[0]->valueType() == VTArray) {
            return nfalse (operands_p[0]->getArrayBool (id));
	}
	return (operands_p[0]->getBool(id)  ?  0 : 1);
    case ndimFUNC:
      {
        // Return fixed dimensionality if available.
        Int64 nrdim = operands_p[0]->ndim();
	return (nrdim >= 0  ?  nrdim : operands_p[0]->shape(id).nelements());
      }
    case nelemFUNC:
	return (operands_p[0]->valueType() == VTScalar  ?
                                   1 : operands_p[0]->shape(id).product());
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getInt(id) : operands_p[2]->getInt(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getInt, "
			     "unknown function " +
                             String::toString(funcType_p)));
    }
    return 0;
}

Double TableExprFuncNode::getDouble (const TableExprId& id)
{
    switch(funcType_p) {
    case piFUNC:
	return C::pi;
    case eFUNC:
	return C::e;
    case sinFUNC:
	return sin      (operands_p[0]->getDouble(id));
    case sinhFUNC:
	return sinh     (operands_p[0]->getDouble(id));
    case cosFUNC:
	return cos      (operands_p[0]->getDouble(id));
    case coshFUNC:
	return cosh     (operands_p[0]->getDouble(id));
    case expFUNC:
	return exp      (operands_p[0]->getDouble(id));
    case logFUNC:
	return log      (operands_p[0]->getDouble(id));
    case log10FUNC:
	return log10    (operands_p[0]->getDouble(id));
    case powFUNC:
	return pow      (operands_p[0]->getDouble(id),
                         operands_p[1]->getDouble(id));
    case squareFUNC:
      {
	Double val = operands_p[0]->getDouble(id);
	return val * val;
      }
    case cubeFUNC:
      {
	Double val = operands_p[0]->getDouble(id);
	return val * val * val;
      }
    case sqrtFUNC:
	return sqrt     (operands_p[0]->getDouble(id)) * scale_p;
    case conjFUNC:
	return           operands_p[0]->getDouble(id);
    case minFUNC:
	return min (operands_p[0]->getDouble(id),
		    operands_p[1]->getDouble(id));
    case maxFUNC:
	return max (operands_p[0]->getDouble(id),
		    operands_p[1]->getDouble(id));
    case normFUNC:
	if (argDataType_p == NTDouble) {
	    Double val = operands_p[0]->getDouble(id);
	    return val * val;
	}
	return norm (operands_p[0]->getDComplex(id));
    case absFUNC:
	if (argDataType_p == NTDouble) {
	    return abs (operands_p[0]->getDouble(id));
	}
	return abs (operands_p[0]->getDComplex(id));
    case argFUNC:
	if (argDataType_p == NTDouble) {
	    if (operands_p[0]->getDouble(id) >= 0) {
		return 0;
	    }
	    return atan2 (Double(0), Double(-1));  // results in pi
	}
	return arg (operands_p[0]->getDComplex(id));
    case realFUNC:
	if (argDataType_p == NTInt) {
	    return operands_p[0]->getInt(id);
	} else if (argDataType_p == NTDouble) {
	    return operands_p[0]->getDouble(id);
	}
	return operands_p[0]->getDComplex(id).real();
    case imagFUNC:
	if (argDataType_p == NTDouble) {
	    return 0;
	}
	return operands_p[0]->getDComplex(id).imag();
    case asinFUNC:
	return asin     (operands_p[0]->getDouble(id));
    case acosFUNC:
	return acos     (operands_p[0]->getDouble(id));
    case atanFUNC:
	return atan     (operands_p[0]->getDouble(id));
    case tanFUNC:
	return tan      (operands_p[0]->getDouble(id));
    case tanhFUNC:
	return tanh     (operands_p[0]->getDouble(id));
    case atan2FUNC:
	return atan2    (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id));
    case signFUNC:
      {
	Double val = operands_p[0]->getDouble(id);
	if (val > 0) {
	    return 1;
	}
	if (val < 0) {
	    return -1;
	}
	return 0;
      }
    case roundFUNC:
      {
	Double val = operands_p[0]->getDouble(id);
	if (val < 0) {
	    return ceil (val - 0.5);
	}
	return floor (val + 0.5);
      }
    case floorFUNC:
	return floor    (operands_p[0]->getDouble(id));
    case ceilFUNC:
	return ceil     (operands_p[0]->getDouble(id));
    case fmodFUNC:
	return fmod     (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id));
    case mjdFUNC:
	return operands_p[0]->getDate(id).day();
    case timeFUNC:                                       //# return in radians
	return fmod (Double(operands_p[0]->getDate(id)), 1.) * C::_2pi;
    case arrminFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return min (operands_p[0]->getArrayDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case arrmaxFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return max (operands_p[0]->getArrayDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case arrsumFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return sum (operands_p[0]->getArrayDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case arrproductFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return product (operands_p[0]->getArrayDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case arrsumsqrFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<Double> arr = operands_p[0]->getArrayDouble (id);
            AlwaysAssert (arr.contiguousStorage(), AipsError);
            return std::accumulate(arr.cbegin(), arr.cend(), Double(0),
                                   casa::SumSqr<Double>());
	} else {
	    Double val = operands_p[0]->getDouble(id);
	    return val * val;
	}
    case arrmeanFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return mean (operands_p[0]->getArrayDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case arrvarianceFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<Double> arr = operands_p[0]->getArrayDouble (id);
	    if (arr.nelements() < 2) {
	        return 0;
	    }
	    return variance (arr);
	}
	return 0;
    case arrstddevFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<Double> arr = operands_p[0]->getArrayDouble (id);
	    if (arr.nelements() < 2) {
	        return 0;
	    }
	    return stddev (arr);
	}
	return 0;
    case arravdevFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<Double> arr = operands_p[0]->getArrayDouble (id);
	    if (arr.empty()) {
	        return 0;
	    }
	    return avdev (operands_p[0]->getArrayDouble (id));
	}
	return 0;
    case arrrmsFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return rms (operands_p[0]->getArrayDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case arrmedianFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return median (operands_p[0]->getArrayDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case arrfractileFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return fractile (operands_p[0]->getArrayDouble (id),
			     operands_p[1]->getDouble (id));
	}
	return operands_p[0]->getDouble (id);
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getDouble(id) : operands_p[2]->getDouble(id);
    case angdistFUNC:
      {
        Array<double> a1 = operands_p[0]->getArrayDouble(id);
        Array<double> a2 = operands_p[1]->getArrayDouble(id);
        if (!(a1.size() == 2  &&  a1.contiguousStorage()  &&
              a2.size() == 2  &&  a2.contiguousStorage())) {
          throw TableInvExpr ("Arguments of angdist function must have a "
                              "multiple of 2 values");
        }
        const double* d1 = a1.data();
        const double* d2 = a2.data();
        return angdist (d1[0], d1[1], d2[0], d2[1]);
      }
    default:
        // Functions like MJD are implemented as Int only.
        return getInt(id);
    }
    return 0;
}

DComplex TableExprFuncNode::getDComplex (const TableExprId& id)
{
    if (dataType() == NTDouble) {
	return TableExprFuncNode::getDouble (id);
    }
    switch (funcType_p) {
    case sinFUNC:
	return sin      (operands_p[0]->getDComplex(id));
    case sinhFUNC:
	return sinh     (operands_p[0]->getDComplex(id));
    case cosFUNC:
	return cos      (operands_p[0]->getDComplex(id));
    case coshFUNC:
	return cosh     (operands_p[0]->getDComplex(id));
    case expFUNC:
	return exp      (operands_p[0]->getDComplex(id));
    case logFUNC:
	return log      (operands_p[0]->getDComplex(id));
    case log10FUNC:
	return log10    (operands_p[0]->getDComplex(id));
    case powFUNC:
	return pow      (operands_p[0]->getDComplex(id),
		         operands_p[1]->getDComplex(id));
    case squareFUNC:
      {
	DComplex val = operands_p[0]->getDComplex(id);
	return val * val;
      }
    case cubeFUNC:
      {
	DComplex val = operands_p[0]->getDComplex(id);
	return val * val * val;
      }
    case sqrtFUNC:
        return sqrt     (operands_p[0]->getDComplex(id)) * scale_p;
    case conjFUNC:
	return conj     (operands_p[0]->getDComplex(id));
    case minFUNC:
      {
	DComplex val0(operands_p[0]->getDComplex (id));
	DComplex val1(operands_p[1]->getDComplex (id));
	if (val0 > val1) {
	    return val1;
	}
	return val0;
      }
    case maxFUNC:
      {
	DComplex val0(operands_p[0]->getDComplex (id));
	DComplex val1(operands_p[1]->getDComplex (id));
	if (val0 < val1) {
	    return val1;
	}
	return val0;
      }
    case complexFUNC:
	return DComplex (operands_p[0]->getDouble (id),
			 operands_p[1]->getDouble (id));
    case arrsumFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return sum (operands_p[0]->getArrayDComplex (id));
	}
	return operands_p[0]->getDComplex (id);
    case arrproductFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return product (operands_p[0]->getArrayDComplex (id));
	}
	return operands_p[0]->getDComplex (id);
    case arrsumsqrFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<DComplex> arr = operands_p[0]->getArrayDComplex (id);
	    Bool deleteIt;
	    const DComplex* data = arr.getStorage (deleteIt);
	    uInt nr = arr.nelements();
	    DComplex result = 0;   
	    for (uInt i=0; i < nr; i++) {
	        result += data[i] * data[i];
	    }
	    arr.freeStorage (data, deleteIt);
	    return result;
	} else {
	    DComplex val = operands_p[0]->getDComplex (id);
	    return val * val;
	}
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getDComplex(id) : operands_p[2]->getDComplex(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDComplex, "
			     "unknown function " +
                             String::toString(funcType_p)));
    }
    return DComplex(0., 0.);
}

String TableExprFuncNode::getString (const TableExprId& id)
{
    static Regex leadingWS("^[ \t]*");
    static Regex trailingWS("[ \t]*$");
    switch (funcType_p) {
    case upcaseFUNC:
      {
	String str = operands_p[0]->getString (id);
	str.upcase();
	return str;
      }
    case downcaseFUNC:
      {
	String str = operands_p[0]->getString (id);
	str.downcase();
	return str;
      }
    case trimFUNC:
      {
	String str = operands_p[0]->getString (id);
        str.gsub (leadingWS, string());
        str.gsub (trailingWS, string());
	return str;
      }
    case ltrimFUNC:
      {
	String str = operands_p[0]->getString (id);
        str.gsub (leadingWS, string());
	return str;
      }
    case rtrimFUNC:
      {
	String str = operands_p[0]->getString (id);
        str.gsub (trailingWS, string());
	return str;
      }
    case cmonthFUNC:
	return operands_p[0]->getDate(id).monthName();
    case cdowFUNC:
        return operands_p[0]->getDate(id).dayName();
    case ctodFUNC:
        return stringDateTime (operands_p[0]->getDate(id), 9);
    case cdateFUNC:
        return stringDate (operands_p[0]->getDate(id));
    case ctimeFUNC:
        return stringTime (operands_p[0]->getDate(id), 9);
    case hmsFUNC:
        return stringHMS (operands_p[0]->getDouble(id), 9);
    case dmsFUNC:
        return stringDMS (operands_p[0]->getDouble(id), 9);
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getString(id) : operands_p[2]->getString(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getString, "
			     "unknown function " +
                             String::toString(funcType_p)));
    }
    return "";
}

TaqlRegex TableExprFuncNode::getRegex (const TableExprId& id)
{
    switch (funcType_p) {
    case regexFUNC:
      return TaqlRegex(Regex(operands_p[0]->getString (id)));
    case patternFUNC:
      return TaqlRegex(Regex(Regex::fromPattern(operands_p[0]->getString (id))));
    case sqlpatternFUNC:
      return TaqlRegex(Regex(Regex::fromSQLPattern(operands_p[0]->getString (id))));
    case iifFUNC:
      return operands_p[0]->getBool(id)  ?
        operands_p[1]->getRegex(id) : operands_p[2]->getRegex(id);
    default:
      break;
    }
    throw (TableInvExpr ("TableExprFuncNode::getRegex, "
                         "unknown function " +
                         String::toString(funcType_p)));
}

MVTime TableExprFuncNode::getDate (const TableExprId& id)
{
    switch (funcType_p) {
    case datetimeFUNC:
      {
	Quantity quant;
	if (MVTime::read (quant, operands_p[0]->getString(id))) {
	    return quant;
	}
	throw (TableInvExpr ("invalid date string " +
			     operands_p[0]->getString(id)));
      }
    case mjdtodateFUNC:
	return MVTime (operands_p[0]->getDouble(id));
    case dateFUNC:
	return MVTime (floor (Double (operands_p[0]->getDate(id))));
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getDate(id) : operands_p[2]->getDate(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDate, "
			     "unknown function " +
                             String::toString(funcType_p)));
    }
    return MVTime();
}

String TableExprFuncNode::stringDT (const MVTime& dt, Int prec,
                                    MVTime::formatTypes type)
{
  MVTime::setFormat (type, prec);
  ostringstream ostr;
  ostr << dt;
  return ostr.str();
}
String TableExprFuncNode::stringAngle (double val, Int prec,
                                       MVAngle::formatTypes type)
{
  MVAngle::setFormat (type, prec);
  ostringstream ostr;
  ostr << MVAngle(val);
  return ostr.str();
}
String TableExprFuncNode::stringDateTime (const MVTime& dt, Int prec)
{
  return stringDT (dt, prec, MVTime::YMD);
}
String TableExprFuncNode::stringDate (const MVTime& dt)
{
  return stringDT (dt, 0, MVTime::formatTypes(MVTime::DMY+MVTime::NO_TIME));
}
String TableExprFuncNode::stringTime (const MVTime& dt, Int prec)
{
  return stringDT (dt, prec, MVTime::TIME);
}
String TableExprFuncNode::stringHMS (double val, Int prec)
{
  // Replace : by h and m.
  String s = stringAngle (val, prec, MVAngle::TIME);
  char r = 'h';
  for (uInt i=0; i<s.size(); ++i) {
    if (s[i] == ':') {
      s[i] = r;
      r    = 'm';
    }
  }
  return s;
}
String TableExprFuncNode::stringDMS (double val, Int prec)
{
  String s = stringAngle (val, prec, MVAngle::ANGLE);
  char r = 'd';
  for (uInt i=0; i<s.size(); ++i) {
    if (s[i] == '.') {
      s[i] = r;
      if (r == 'm') {
        break;
      }
      r = 'm';
    }
  }
  return s;
}


TableExprNodeRep::NodeDataType TableExprFuncNode::checkOperands
                                 (Block<Int>& dtypeOper,
				  ValueType& resVT, Block<Int>&,
				  FunctionType fType,
				  PtrBlock<TableExprNodeRep*>& nodes)
{
    uInt i;
    // The default returned value type is a scalar.
    resVT = VTScalar;
    // The default datatype is NTDouble.
    NodeDataType dtin = NTDouble;
    NodeDataType dtout = NTDouble;
    // The following functions accept a single scalar or array argument.
    // They result in a scalar.
    switch (fType) {
    case arrminFUNC:
    case arrmaxFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTReal, NTReal, nodes);
    case arrmeanFUNC:
    case arrvarianceFUNC:
    case arrstddevFUNC:
    case arravdevFUNC:
    case arrrmsFUNC:
    case arrmedianFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTReal, NTDouble, nodes);
    case arrfractileFUNC:
	checkNumOfArg (2, 2, nodes);
	if (nodes[1]->valueType() != VTScalar) {
	    throw TableInvExpr ("2nd argument of FRACTILE function "
				"has to be a scalar");
	}
	return checkDT (dtypeOper, NTReal, NTDouble, nodes);
    case arrsumFUNC:
    case arrproductFUNC:
    case arrsumsqrFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case anyFUNC:
    case allFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTBool, NTBool, nodes);
    case ntrueFUNC:
    case nfalseFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTBool, NTInt, nodes);
    case nelemFUNC:
    case ndimFUNC:
    case shapeFUNC:
	checkNumOfArg (1, 1, nodes);
	if (fType == shapeFUNC) {
	    resVT = VTArray;
	}
	return checkDT (dtypeOper, NTAny, NTInt, nodes);
    case isdefFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTAny, NTBool, nodes);
    case angdistFUNC:
        checkNumOfArg (2, 2, nodes);
        if (nodes[0]->valueType() != VTArray  ||
            nodes[1]->valueType() != VTArray) {
          throw TableInvExpr ("Arguments of angdist function "
                              "have to be arrays");
        }
        if (nodes[0]->shape().product() != 2  ||
            nodes[1]->shape().product() != 2) {
          resVT = VTArray;    // result is scalar if both arg have 2 values
        }
        return checkDT (dtypeOper, NTReal, NTDouble, nodes);
    default:
	break;
    }
    // The following functions accept one array or scalar and a set of
    // one or more scalars.
    // They return an array.
    switch (fType) {
    case arrsumsFUNC:
    case arrproductsFUNC:
    case arrsumsqrsFUNC:
    case arrminsFUNC:
    case arrmaxsFUNC:
    case arrmeansFUNC:
    case arrvariancesFUNC:
    case arrstddevsFUNC:
    case arravdevsFUNC:
    case arrrmssFUNC:
    case arrmediansFUNC:
    case arrfractilesFUNC:
    case anysFUNC:
    case allsFUNC:
    case ntruesFUNC:
    case nfalsesFUNC:
    case runminFUNC:
    case runmaxFUNC:
    case runmeanFUNC:
    case runvarianceFUNC:
    case runstddevFUNC:
    case runavdevFUNC:
    case runrmsFUNC:
    case runmedianFUNC:
    case runanyFUNC:
    case runallFUNC:
    case boxminFUNC:
    case boxmaxFUNC:
    case boxmeanFUNC:
    case boxvarianceFUNC:
    case boxstddevFUNC:
    case boxavdevFUNC:
    case boxrmsFUNC:
    case boxmedianFUNC:
    case boxanyFUNC:
    case boxallFUNC:
    case arrayFUNC:
      {
        // Most functions can have Int or Double in and result in Double.
        dtin = NTReal;
        dtout = NTDouble;
	uInt axarg = 1;
        switch (fType) {
	case arrsumsFUNC:
	case arrproductsFUNC:
	case arrsumsqrsFUNC:
	    dtin = dtout = NTNumeric;
	    break;
	case arrfractilesFUNC:
	    axarg = 2;
	    break;
	case anysFUNC:
	case allsFUNC:
	case runanyFUNC:
	case runallFUNC:
	case boxanyFUNC:
	case boxallFUNC:
	    dtin = dtout = NTBool;
	    break;
	case ntruesFUNC:
	case nfalsesFUNC:
	    dtin = NTBool;
            dtout = NTInt;
	    break;
	case arrayFUNC:
	    dtin = dtout = NTAny;
	    break;
	default:
	    break;
	}
	// The result is an array.
	// All arguments (except possibly first) must be integers.
        resVT = VTArray;
        checkNumOfArg (axarg+1, axarg+1, nodes);
	dtypeOper.resize(axarg+1);
	dtypeOper = NTInt;
	// Check for XXXs and run/boxXXX functions if first argument is array.
	if (fType != arrayFUNC) {
	    if (nodes[0]->valueType() != VTArray) {
	        throw TableInvExpr ("1st argument of xxxS function "
				    "has to be an array");
	    }
	}
	// Check if first argument has correct type.
	PtrBlock<TableExprNodeRep*> nodeTmp(1);
	nodeTmp[0] = nodes[0];
	Block<Int> dtypeTmp;    // Gets filled in by checkDT
	dtout = checkDT (dtypeTmp, dtin, dtout, nodeTmp);
	dtypeOper[0] = dtypeTmp[0];
	// If more arguments are needed, they have to be Int scalars.
	if (axarg > 1) {
	  for (uInt i=1; i<axarg; i++) {
	    if (nodes[i]->valueType() != VTScalar
	    &&  nodes[i]->dataType() != NTInt) {
	      throw TableInvExpr ("2nd argument of runningXXX, boxedXXX, or "
				  "XXXs function has to be an integer scalar");
	    }
	  }
	}
	// The last argument forms the axes as an array object.
	AlwaysAssert (nodes[axarg]->valueType() == VTArray, AipsError);
	return dtout;
      }
    default:
        break;
    }
    // The following functions accept scalars and arrays.
    // They return an array if one of the input arguments is an array.
    // If a function has no arguments, it results in a scalar.
    for (i=0; i< nodes.nelements(); i++) {
        ValueType vt = nodes[i]->valueType();
	if (vt == VTArray) {
	    resVT = vt;
	} else if (vt != VTScalar) {
	    throw (TableInvExpr
                          ("Function has to have a scalar or array argument"));
	}
    }
    switch (fType) {
    case strlengthFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTInt, nodes);
    case upcaseFUNC:
    case downcaseFUNC:
    case trimFUNC:
    case ltrimFUNC:
    case rtrimFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTString, nodes);
    case datetimeFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTString, NTDate, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTString;
	nodes.resize(1);
	nodes[0] = new TableExprNodeConstString ("today");
	return NTDate;
    case mjdtodateFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTReal, NTDate, nodes);
    case dateFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTDate, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTDate;
    case yearFUNC:
    case monthFUNC:
    case dayFUNC:
    case weekdayFUNC:
    case weekFUNC:
        dtout = NTInt;
    case mjdFUNC:
    case timeFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, dtout, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return dtout;
    case cmonthFUNC:
    case cdowFUNC:
    case ctodFUNC:
    case cdateFUNC:
    case ctimeFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTString, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTString;
    case hmsFUNC:
    case dmsFUNC:
    case hdmsFUNC:
        checkNumOfArg (1, 1, nodes);
        if (fType == hdmsFUNC  &&  nodes[0]->valueType() != VTArray) {
            throw TableInvExpr("Argument of function HDMS has to be an array");
        }
        return checkDT (dtypeOper, NTReal, NTString, nodes);
    case sinFUNC:
    case sinhFUNC:
    case cosFUNC:
    case coshFUNC:
    case expFUNC:
    case logFUNC:
    case log10FUNC:
    case sqrtFUNC:
    case conjFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTDouCom, nodes);
    case squareFUNC:
    case cubeFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case normFUNC:
    case absFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTReal, nodes);
    case argFUNC:
    case realFUNC:
    case imagFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTDouble, nodes);
    case asinFUNC:
    case acosFUNC:
    case atanFUNC:
    case tanFUNC:
    case tanhFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTReal, NTDouble, nodes);
    case signFUNC:
    case roundFUNC:
    case floorFUNC:
    case ceilFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTReal, NTReal, nodes);
    case intFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTReal, NTInt, nodes);
    case near2FUNC:
    case nearabs2FUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTNumeric, NTBool, nodes);
    case near3FUNC:
    case nearabs3FUNC:
    {
	checkNumOfArg (3, 3, nodes);
	// Check if tolerance has a Real value.
	PtrBlock<TableExprNodeRep*> nodeTol(1);
	nodeTol[0] = nodes[2];
	checkDT (dtypeOper, NTReal, NTBool, nodeTol);
	return checkDT (dtypeOper, NTNumeric, NTBool, nodes);
    }
    case powFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTNumeric, NTDouCom, nodes);
    case minFUNC:
    case maxFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case atan2FUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTReal, NTDouble, nodes);
    case fmodFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTReal, NTReal, nodes);
    case complexFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTReal, NTComplex, nodes);
    case isnanFUNC:
    case isinfFUNC:
    case isfiniteFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTBool, nodes);
    case iifFUNC:
      {
	checkNumOfArg (3, 3, nodes);
	PtrBlock<TableExprNodeRep*> nodeCond(1);
	nodeCond[0] = nodes[0];
	PtrBlock<TableExprNodeRep*> nodeArg(2);
	nodeArg[0] = nodes[1];
	nodeArg[1] = nodes[2];
	Block<Int> dtypeTmp;
	checkDT (dtypeTmp, NTBool, NTBool, nodeCond);
	dtypeOper.resize(3);
	dtypeOper[0] = dtypeTmp[0];
	NodeDataType dt = checkDT (dtypeTmp, NTAny, NTAny, nodeArg);
	dtypeOper[1] = dtypeTmp[0];
	dtypeOper[2] = dtypeTmp[1];
	return dt;
      }
    default:
	break;
    }
    // The following functions accept scalars only (or no arguments).
    for (i=0; i< nodes.nelements(); i++) {
	if (nodes[i]->valueType() != VTScalar) {
	    throw (TableInvExpr ("Function has to have a scalar argument"));
	}
    }
    switch (fType) {
    case rownrFUNC:
    case rowidFUNC:
	checkNumOfArg (0, 0, nodes);
	return NTInt;
    case randFUNC:
    case piFUNC:
    case eFUNC:
	checkNumOfArg (0, 0, nodes);
	return NTDouble;
    case regexFUNC:
    case patternFUNC:
    case sqlpatternFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTRegex, nodes);
    default:
	throw (TableInvExpr ("TableExprFuncNode::checkOperands, "
			     "function not contained in switch statement"));
    }
    return NTNumeric;
}

} //# NAMESPACE CASA - END
