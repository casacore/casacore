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
  argDataType_p      (dtype)
{}

TableExprFuncNode::~TableExprFuncNode()
{}

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
    fillUnits (thisNode, thisNode->operands_p, thisNode->funcType());
    // Some functions on a variable can already give a constant result.
    thisNode->tryToConst();
    if (thisNode->operands_p.nelements() > 0) {
	return convertNode (thisNode, True);
    }
    return thisNode;
}

void TableExprFuncNode::fillUnits (TableExprNodeRep* node,
				   PtrBlock<TableExprNodeRep*>& nodes,
				   FunctionType func)
{
  if (nodes.nelements() > 0) {
    const Unit& childUnit = nodes[0]->unit();
    switch (func) {
    case asinFUNC:
    case acosFUNC:
    case atanFUNC:
    case atan2FUNC:
    case timeFUNC:
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
    case normFUNC:
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
    case sqrtFUNC:
      // These functions return the sqrt of their child.
      if (! childUnit.empty()) {
	Quantity q(1., childUnit);
	node->setUnit (sqrt(q).getFullUnit());
      }
      break;
    case sinFUNC:
    case cosFUNC:
    case tanFUNC:
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
      for (uInt i=0; i<nodes.nelements(); ++i) {
	TableExprNodeUnit::adaptUnit (nodes[i], "rad");
      }
      break;
    default:
      break;
    }
  }
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
    // Determine if common argument type is Double or Complex.
    // (this is used by some functions like near and norm).
    thisNode->operands_p.resize (nodes.nelements());
    thisNode->argDataType_p = NTDouble;
    for (i=0; i<nodes.nelements(); i++) {
	thisNode->operands_p[i] = nodes[i]->link();
	if (nodes[i]->dataType() == NTComplex) {
	    thisNode->argDataType_p = NTComplex;
	}
    }
    // Convert String to Date if needed
    for (i=0; i<nodes.nelements(); i++) {
	if (nodes[i]->dataType() == NTString  &&  dtypeOper[i] == NTDate) {
	    TableExprNode dNode = datetime (thisNode->operands_p[i]);
	    unlink (thisNode->operands_p[i]);
	    thisNode->operands_p[i] = getRep (dNode)->link();
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
	if (argDataType_p == NTDouble) {
	    return isNaN(operands_p[0]->getDouble(id));
	}
	return isNaN(operands_p[0]->getDComplex(id));
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
			     "unknown function"));
    }
    return True;
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
    case sqrtFUNC:
	return sqrt     (operands_p[0]->getDouble(id));
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
	if (argDataType_p == NTDouble) {
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
    case strlengthFUNC:
	return operands_p[0]->getString (id).length();
    case datetimeFUNC:
    case mjdtodateFUNC:
    case dateFUNC:
        return Double (getDate(id));
    case mjdFUNC:
	return operands_p[0]->getDate(id).day();
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
	    Bool deleteIt;
	    const Double* data = arr.getStorage (deleteIt);
	    uInt nr = arr.nelements();
	    Double result = 0;   
	    for (uInt i=0; i < nr; i++) {
	      result += data[i] * data[i];
	    }
	    arr.freeStorage (data, deleteIt);
	    return result;
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
	    return avdev (operands_p[0]->getArrayDouble (id));
	}
	return 0;
    case arrrmsFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    return rms (operands_p[0]->getArrayDouble (id));
	}
	return 0;
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
    case ntrueFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<Bool> arr = operands_p[0]->getArrayBool (id);
	    Bool deleteIt;
	    const Bool* data = arr.getStorage (deleteIt);
	    uInt nr = arr.nelements();
	    uInt n = 0;
	    for (uInt i=0; i<nr; i++) {
	        if (data[i]) {
		    n++;
		}
	    }
	    arr.freeStorage (data, deleteIt);
	    return n;
	}
	return (operands_p[0]->getBool(id)  ?  1 : 0);
    case nfalseFUNC:
        if (operands_p[0]->valueType() == VTArray) {
	    Array<Bool> arr = operands_p[0]->getArrayBool (id);
	    Bool deleteIt;
	    const Bool* data = arr.getStorage (deleteIt);
	    uInt nr = arr.nelements();
	    uInt n = 0;
	    for (uInt i=0; i<nr; i++) {
	        if (! data[i]) {
		    n++;
		}
	    }
	    arr.freeStorage (data, deleteIt);
	    return n;
	}
	return (operands_p[0]->getBool(id)  ?  0 : 1);
    case ndimFUNC:
      {
        // Return fixed dimensionality if available.
        Int nrdim = operands_p[0]->ndim();
	return (nrdim >= 0  ?  nrdim : operands_p[0]->shape(id).nelements());
      }
    case nelemFUNC:
	return (operands_p[0]->valueType() == VTScalar  ?
                                   1 : operands_p[0]->shape(id).product());
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getDouble(id) : operands_p[2]->getDouble(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDouble, "
			     "unknown function"));
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
    case sqrtFUNC:
	return sqrt     (operands_p[0]->getDComplex(id));
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
			     "unknown function"));
    }
    return DComplex(0., 0.);
}

String TableExprFuncNode::getString (const TableExprId& id)
{
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
	Int pos = str.length();
	while (--pos >= 0  &&  str[pos] == ' ' ) ;
	if (pos < 0) {
	    return "";
	} else if (pos+1 < Int(str.length())) {
	    return str.through(pos);
	}
	return str;
      }
    case cmonthFUNC:
	return operands_p[0]->getDate(id).monthName();
    case cdowFUNC:
	return operands_p[0]->getDate(id).dayName();
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getString(id) : operands_p[2]->getString(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getString, "
			     "unknown function"));
    }
    return "";
}

Regex TableExprFuncNode::getRegex (const TableExprId& id)
{
    switch (funcType_p) {
    case regexFUNC:
	return Regex(operands_p[0]->getString (id));
    case patternFUNC:
	return Regex(Regex::fromPattern(operands_p[0]->getString (id)));
    case sqlpatternFUNC:
	return Regex(Regex::fromSQLPattern(operands_p[0]->getString (id)));
    case iifFUNC:
        return operands_p[0]->getBool(id)  ?
	       operands_p[1]->getRegex(id) : operands_p[2]->getRegex(id);
    default:
	throw (TableInvExpr ("TableExprFuncNode::getRegex, "
			     "unknown function"));
    }
    return Regex("");
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
			     "unknown function"));
    }
    return MVTime();
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
    // The following functions accept a single scalar or array argument.
    // They result in a scalar.
    switch (fType) {
    case arrminFUNC:
    case arrmaxFUNC:
    case arrmeanFUNC:
    case arrvarianceFUNC:
    case arrstddevFUNC:
    case arravdevFUNC:
    case arrrmsFUNC:
    case arrmedianFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTDouble, NTDouble, nodes);
    case arrfractileFUNC:
	checkNumOfArg (2, 2, nodes);
	if (nodes[1]->valueType() != VTScalar) {
	    throw TableInvExpr ("2nd argument of FRACTILE function "
				"has to be a scalar");
	}
	return checkDT (dtypeOper, NTDouble, NTDouble, nodes);
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
	return checkDT (dtypeOper, NTBool, NTDouble, nodes);
    case nelemFUNC:
    case ndimFUNC:
    case shapeFUNC:
	checkNumOfArg (1, 1, nodes);
	if (fType == shapeFUNC) {
	    resVT = VTArray;
	}
	return checkDT (dtypeOper, NTAny, NTDouble, nodes);
    case isdefFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTAny, NTBool, nodes);
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
        NodeDataType dtin = NTDouble;
        NodeDataType dtout = NTDouble;
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
	    break;
	case arrayFUNC:
	    dtin = dtout = NTAny;
	    break;
	default:
	    break;
	}
	// The result is an array.
	// All arguments (except possibly first) are doubles.
        resVT = VTArray;
        checkNumOfArg (axarg+1, axarg+1, nodes);
	dtypeOper.resize(axarg+1);
	dtypeOper = NTDouble;
	PtrBlock<TableExprNodeRep*> nodeArr(1);
	// Check for XXXs and run/boxXXX functions if first argument is an array.
	nodeArr[0] = nodes[0];
	if (fType != arrayFUNC) {
	    if (nodes[0]->valueType() != VTArray) {
	        throw TableInvExpr ("1st argument of xxxS function "
				    "has to be an array");
	    }
	}
	// Check if first argument has correct type.
	Block<Int> dtypeTmp;
	dtout = checkDT (dtypeTmp, dtin, dtout, nodeArr);
	dtypeOper[0] = dtypeTmp[0];
	// If more arguments are needed, they have to be double scalars.
	if (axarg > 1) {
	  for (uInt i=1; i<axarg; i++) {
	    if (nodes[i]->valueType() != VTScalar
	    &&  nodes[i]->dataType() != NTDouble) {
	      throw TableInvExpr ("2nd argument of runningXXX, boxedXXX, or "
				  "XXXs function has to be a double scalar");
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
    // They return a array if one of the input arguments is an array.
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
	return checkDT (dtypeOper, NTString, NTDouble, nodes);
    case upcaseFUNC:
    case downcaseFUNC:
    case trimFUNC:
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
	return checkDT (dtypeOper, NTDouble, NTDate, nodes);
    case dateFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTDate, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTDate;
    case mjdFUNC:
    case yearFUNC:
    case monthFUNC:
    case dayFUNC:
    case weekdayFUNC:
    case weekFUNC:
    case timeFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTDouble, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTDouble;
    case cmonthFUNC:
    case cdowFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTString, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTString;
    case sinFUNC:
    case sinhFUNC:
    case cosFUNC:
    case coshFUNC:
    case expFUNC:
    case logFUNC:
    case log10FUNC:
    case squareFUNC:
    case sqrtFUNC:
    case conjFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case normFUNC:
    case absFUNC:
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
    case signFUNC:
    case roundFUNC:
    case floorFUNC:
    case ceilFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTDouble, NTDouble, nodes);
    case near2FUNC:
    case nearabs2FUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTNumeric, NTBool, nodes);
    case near3FUNC:
    case nearabs3FUNC:
    {
	checkNumOfArg (3, 3, nodes);
	// Check if tolerance has a Double value.
	PtrBlock<TableExprNodeRep*> nodeTol(1);
	nodeTol[0] = nodes[2];
	checkDT (dtypeOper, NTDouble, NTBool, nodeTol);
	return checkDT (dtypeOper, NTNumeric, NTBool, nodes);
    }
    case powFUNC:
    case minFUNC:
    case maxFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case atan2FUNC:
    case fmodFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTDouble, NTDouble, nodes);
    case complexFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTDouble, NTComplex, nodes);
    case isnanFUNC:
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
    case randFUNC:
    case rownrFUNC:
    case rowidFUNC:
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
