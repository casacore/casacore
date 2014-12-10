//# ExprNodeRep.cc: Representation class for a table column expression tree
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001
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

#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprRange.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/iostream.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

// The constructor to be used by the derived classes.
TableExprNodeRep::TableExprNodeRep (NodeDataType dtype, ValueType vtype,
				    OperType optype,
				    const Table& table)
: count_p    (0),
  table_p    (table),
  dtype_p    (dtype),
  vtype_p    (vtype),
  optype_p   (optype),
  argtype_p  (NoArr),
  exprtype_p (Variable),
  ndim_p     (0)
{
    if (table.isNull()) {
	exprtype_p = Constant;
    }
}

TableExprNodeRep::TableExprNodeRep (NodeDataType dtype, ValueType vtype,
				    OperType optype, ArgType argtype,
				    ExprType exprtype,
				    Int ndim, const IPosition& shape,
				    const Table& table)
: count_p    (0),
  table_p    (table),
  dtype_p    (dtype),
  vtype_p    (vtype),
  optype_p   (optype),
  argtype_p  (argtype),
  exprtype_p (exprtype),
  ndim_p     (ndim),
  shape_p    (shape)
{}

TableExprNodeRep::TableExprNodeRep (const TableExprNodeRep& that)
: count_p    (0),
  table_p    (that.table_p),
  dtype_p    (that.dtype_p),
  vtype_p    (that.vtype_p),
  optype_p   (that.optype_p),
  argtype_p  (that.argtype_p),
  exprtype_p (that.exprtype_p),
  ndim_p     (that.ndim_p),
  shape_p    (that.shape_p),
  unit_p     (that.unit_p)
{}

TableExprNodeRep::~TableExprNodeRep ()
{}


void TableExprNodeRep::unlink (TableExprNodeRep* node)
{
    if (node != 0) {
	if (--node->count_p == 0) {
	    delete node;
	}
    }
}

void TableExprNodeRep::show (ostream& os, uInt indent) const
{
    for (uInt i=0; i<indent; i++) {
	os << ' ';
    }
    os << Int(dtype_p) << ' ' << Int(vtype_p) << ' ' << Int(optype_p)
       << ' ' << Int(exprtype_p) << ' '<< Int(argtype_p) << ' '
       << ndim_p << ' ' << shape_p << ' ' << table_p.baseTablePtr() << endl;
}

void TableExprNodeRep::disableApplySelection()
{}

void TableExprNodeRep::applySelection (const Vector<uInt>&)
{}

void TableExprNodeRep::getAggrNodes (vector<TableExprNodeRep*>&)
{}

void TableExprNodeRep::getColumnNodes (vector<TableExprNodeRep*>&)
{}

void TableExprNodeRep::checkAggrFuncs (const TableExprNodeRep* node)
{
  vector<TableExprNodeRep*> aggr;
  if (node) {
    const_cast<TableExprNodeRep*>(node)->getAggrNodes (aggr);
    if (! aggr.empty()) {
      throw TableInvExpr("Invalid use of an aggregate function "
                         "(only use in SELECT or HAVING clause)");
    }
  }
}

void TableExprNodeRep::setUnit (const Unit& unit)
{
    unit_p = unit;
    if (!unit.empty()  &&  dtype_p == NTInt) {
        dtype_p = NTDouble;
    }
}

Double TableExprNodeRep::getUnitFactor() const
{
    return 1.;
}

void TableExprNodeRep::adaptSetUnits (const Unit&)
{}

//# Determine the number of rows in the table used in the expression.
uInt TableExprNodeRep::nrow() const
{
    if (exprtype_p == Constant) {
        return 1;
    }
    if (table_p.isNull()) {
      return 1;                  // for calc expressions
    }
    return table_p.nrow();
}

void TableExprNodeRep::convertConstChild()
{}

void TableExprNodeRep::checkTablePtr (Table& table,
				      const TableExprNodeRep* node)
{
    if (node != 0) {
      if (table.isNull()  ||  table.nrow() == 0) {
	    table = node->table();
	}else{
        if (!(node->table().isNull()  ||  node->table().nrow() == 0)
            &&  node->table().nrow() != table.nrow()) {
		throw (TableInvExpr
                       ("expression uses differently sized tables"));
	    }
	}
    }
}
void TableExprNodeRep::fillExprType (ExprType& type,
				     const TableExprNodeRep* node)
{
    if (node != 0  &&  !node->isConstant()) {
	type = Variable;
    }
}

// The getColumn data type is unknown.
Bool TableExprNodeRep::getColumnDataType (DataType&) const
    { return False; }

// Convert the tree to a number of range vectors which at least
// select the same things.
// By default a not possible is returned (an empty block).
void TableExprNodeRep::ranges (Block<TableExprRange>& blrange)
{
    blrange.resize (0, True);
}

// Create a range.
void TableExprNodeRep::createRange (Block<TableExprRange>& blrange)
{
    blrange.resize (0, True);
}

void TableExprNodeRep::createRange (Block<TableExprRange>& blrange,
				    TableExprNodeColumn* tsn,
				    Double st, Double end)
{
    if (tsn == 0) {
	blrange.resize (0, True);
    }else{
	blrange.resize (1, True);
	blrange[0] = TableExprRange (tsn->getColumn(), st, end);
    }
}

const IPosition& TableExprNodeRep::shape (const TableExprId& id)
{
    if (ndim_p == 0  ||  shape_p.nelements() != 0) {
	return shape_p;
    }
    return getShape (id);
}
const IPosition& TableExprNodeRep::getShape (const TableExprId&)
{
    throw (TableInvExpr ("getShape not implemented"));
    return shape_p;
}

Bool TableExprNodeRep::isDefined (const TableExprId&)
{
    return True;
}

//# Supply the default functions for the get functions.
Bool TableExprNodeRep::getBool (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getBool not implemented)");
    return False;
}
Int64 TableExprNodeRep::getInt (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getInt not implemented)");
    return 0;
}
Double TableExprNodeRep::getDouble (const TableExprId& id)
{
    return getInt (id);
}
DComplex TableExprNodeRep::getDComplex (const TableExprId& id)
{
    return getDouble (id);
}
String TableExprNodeRep::getString (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getString not implemented)");
    return "";
}
TaqlRegex TableExprNodeRep::getRegex (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getRegex not implemented)");
    return TaqlRegex(Regex(String()));
}
MVTime TableExprNodeRep::getDate (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getDate not implemented)");
    return MVTime(0.);
}
Array<Bool> TableExprNodeRep::getArrayBool (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getArrayBool not implemented)");
    return Array<Bool>();
}
Array<Int64> TableExprNodeRep::getArrayInt (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getArrayInt not implemented)");
    return Array<Int64>();
}
Array<Double> TableExprNodeRep::getArrayDouble (const TableExprId& id)
{
    Array<Int64> tmp(getArrayInt(id));
    Array<Double> res(tmp.shape());
    convertArray (res, tmp);
    return res;
}
Array<DComplex> TableExprNodeRep::getArrayDComplex (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getArrayDComplex not implemented)");
    return Array<DComplex>();
}
Array<String> TableExprNodeRep::getArrayString (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getArrayString not implemented)");
    return Array<String>();
}
Array<MVTime> TableExprNodeRep::getArrayDate (const TableExprId&)
{
    TableExprNode::throwInvDT ("(getArrayDate not implemented)");
    return Array<MVTime>();
}

Array<Bool> TableExprNodeRep::getBoolAS (const TableExprId& id)
{
  if (valueType() == VTArray) {
    return getArrayBool(id);
  }
  Vector<Bool> res(1);
  res[0] = getBool(id);
  return res;
}
Array<Int64> TableExprNodeRep::getIntAS (const TableExprId& id)
{
  if (valueType() == VTArray) {
    return getArrayInt(id);
  }
  Vector<Int64> res(1);
  res[0] = getInt(id);
  return res;
}
Array<Double> TableExprNodeRep::getDoubleAS (const TableExprId& id)
{
  if (valueType() == VTArray) {
    return getArrayDouble(id);
  }
  Vector<Double> res(1);
  res[0] = getDouble(id);
  return res;
}
Array<DComplex> TableExprNodeRep::getDComplexAS (const TableExprId& id)
{
  if (valueType() == VTArray) {
    return getArrayDComplex(id);
  }
  Vector<DComplex> res(1);
  res[0] = getDComplex(id);
  return res;
}
Array<String> TableExprNodeRep::getStringAS (const TableExprId& id)
{
  if (valueType() == VTArray) {
    return getArrayString(id);
  }
  Vector<String> res(1);
  res[0] = getString(id);
  return res;
}
Array<MVTime> TableExprNodeRep::getDateAS (const TableExprId& id)
{
  if (valueType() == VTArray) {
    return getArrayDate(id);
  }
  Vector<MVTime> res(1);
  res[0] = getDate(id);
  return res;
}

Bool TableExprNodeRep::hasBool     (const TableExprId& id, Bool value)
{
    return (value == getBool(id));
}
Bool TableExprNodeRep::hasInt      (const TableExprId& id, Int64 value)
{
    return (value == getInt(id));
}
Bool TableExprNodeRep::hasDouble   (const TableExprId& id, Double value)
{
    return (value == getDouble(id));
}
Bool TableExprNodeRep::hasDComplex (const TableExprId& id,
				    const DComplex& value)
{
    return (value == getDComplex(id));
}
Bool TableExprNodeRep::hasString   (const TableExprId& id,
				    const String& value)
{
    return (value == getString(id));
}
Bool TableExprNodeRep::hasDate     (const TableExprId& id,
				    const MVTime& value)
{
    return (value == getDate(id));
}
Array<Bool> TableExprNodeRep::hasArrayBool (const TableExprId& id,
					    const Array<Bool>& value)
{
    return (getBool(id) == value);
}
Array<Bool> TableExprNodeRep::hasArrayInt (const TableExprId& id,
                                           const Array<Int64>& value)
{
    return (getInt(id) == value);
}
Array<Bool> TableExprNodeRep::hasArrayDouble (const TableExprId& id,
					      const Array<Double>& value)
{
    return (getDouble(id) == value);
}
Array<Bool> TableExprNodeRep::hasArrayDComplex (const TableExprId& id,
						const Array<DComplex>& value)
{
    return (getDComplex(id) == value);
}
Array<Bool> TableExprNodeRep::hasArrayString (const TableExprId& id,
					      const Array<String>& value)
{
    return (getString(id) == value);
}
Array<Bool> TableExprNodeRep::hasArrayDate (const TableExprId& id,
					    const Array<MVTime>& value)
{
    return (getDate(id) == value);
}


Array<Bool>     TableExprNodeRep::getColumnBool
(const Vector<uInt>& rownrs)
{
    TableExprId id;
    uInt nrrow = rownrs.size();
    Vector<Bool> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
      id.setRownr   (rownrs[i]);
      vec[i] = getBool (id);
    }
    return vec;
}
Array<uChar>    TableExprNodeRep::getColumnuChar
(const Vector<uInt>&)
{
    TableExprNode::throwInvDT ("(getColumnuChar not implemented)");
    return Array<uChar>();
}
Array<Short>    TableExprNodeRep::getColumnShort
(const Vector<uInt>&)
{
    TableExprNode::throwInvDT ("(getColumnShort not implemented)");
    return Array<Short>();
}
Array<uShort>   TableExprNodeRep::getColumnuShort
(const Vector<uInt>&)
{
    TableExprNode::throwInvDT ("(getColumnuShort not implemented)");
    return Array<uShort>();
}
Array<Int>      TableExprNodeRep::getColumnInt
(const Vector<uInt>& rownrs)
{
    TableExprId id;
    uInt nrrow = rownrs.size();
    Vector<Int> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
      id.setRownr   (rownrs[i]);
      vec[i] = getInt (id);
    }
    return vec;
}
Array<uInt>     TableExprNodeRep::getColumnuInt
(const Vector<uInt>&)
{
    TableExprNode::throwInvDT ("(getColumnuInt not implemented)");
    return Array<uInt>();
}
// Array<Int64>    TableExprNodeRep::getColumnInt64
// (const Vector<uInt>& rownrs)
// {
//     TableExprId id;
//     uInt nrrow = rownrs.size();
//     Vector<Int64> vec (nrrow);
//     for (uInt i=0; i<nrrow; i++) {
//       id.setRownr   (rownrs[i]);
//       vec[i] = getInt (id);
//     }
//     return vec;
// }
Array<Float>    TableExprNodeRep::getColumnFloat
(const Vector<uInt>&)
{
    TableExprNode::throwInvDT ("(getColumnFloat not implemented)");
    return Array<Float>();
}
Array<Double>   TableExprNodeRep::getColumnDouble
(const Vector<uInt>& rownrs)
{
    TableExprId id;
    uInt nrrow = rownrs.size();
    Vector<Double> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
      id.setRownr   (rownrs[i]);
      vec[i] = getDouble (id);
    }
    return vec;
}
Array<Complex>  TableExprNodeRep::getColumnComplex
(const Vector<uInt>&)
{
    TableExprNode::throwInvDT ("(getColumnComplex not implemented)");
    return Array<Complex>();
}
Array<DComplex> TableExprNodeRep::getColumnDComplex
(const Vector<uInt>& rownrs)
{
    TableExprId id;
    uInt nrrow = rownrs.size();
    Vector<DComplex> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
      id.setRownr   (rownrs[i]);
      vec[i] = getDComplex (id);
    }
    return vec;
}
Array<String>   TableExprNodeRep::getColumnString
(const Vector<uInt>& rownrs)
{
    TableExprId id;
    uInt nrrow = rownrs.size();
    Vector<String> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
      id.setRownr   (rownrs[i]);
      vec[i] = getString (id);
    }
    return vec;
}

// The following can be implemented one time.
// It is a optimization to remove an OR or AND when one branch is constant.
// It should be done in TableExprNodeBinary.
#if defined(TABLEXPRNODEREP_NEVER_TRUE)
TableExprNodeRep* TableExprNodeBinary::shortcutOrAnd ()
{
    if (thisNode->operType() != OtOR  &&  thisNode->operType() != OtAND) {
	return this;
    }
    // Determine if and which node is a constant.
    // Exit if no constant.
    TableExprNodeRep** constNode = &lnode_p;
    TableExprNodeRep** otherNode = &rnode_p;
    if (! lnode_p->isConstant()) {
	if (! rnode_p->isConstant()) {
	    return this;
	}
	constNode = &rnode_p;
	otherNode = &lnode_p;
    }
    // Only a constant Scalar can be handled, since arrays can be varying
    // in size and the result can be important.
    if ((**constNode).valueType() != VTScalar) {
	return this;
    }
    Bool value = (**constNode)->getBool (0);
    // For an AND a true constant means the other node determines the result.
    // So we can replace the AND by that node.
    // A false results in a constant false when the other operand is a scalar.
    // So in that case the constant is the result.
    // For OR the same can be done.
	    (**otherNode).count_p++;
	    delete thisNode;
	    (**otherNode).count_p--;
    if (thisNode->operType() == OtAND) {
	if (value) {
	    return *otherNode;
	}else{
	    if ((**otherNode).valueType() != VTScalar) {
		return *constNode;
	    }
	}
    }else{
	if (value) {
	    if ((**otherNode).valueType() != VTScalar) {
		return this;
	    }
	}else{
	    return *otherNode;
	}
    }
    return *constNode;

    // In the calling routine something like the following has to be done.
    TableExprNodeRep* node = shortcutAndOr();
    if (node != thisNode) {
	node->count_p++; // prevent child from being deleted by delete thisNode
	delete thisNode;
	node->count_p--;
    }
}
#endif

TableExprNodeRep* TableExprNodeRep::convertNode (TableExprNodeRep* thisNode,
						 Bool convertConstType)
{
    // If the expression is not constant, try to convert the type
    // of a constant child to the other child's type.
    if (! thisNode->isConstant()) {
	if (convertConstType) {
	    thisNode->convertConstChild();
	}
	return thisNode;
    }
    // Evaluate the constant subexpression and replace the node.
    TableExprNodeRep* newNode = 0;
    if (thisNode->valueType() == VTScalar) {
	switch (thisNode->dataType()) {
	case NTBool:
	    newNode = new TableExprNodeConstBool (thisNode->getBool (0));
	    break;
	case NTInt:
	    newNode = new TableExprNodeConstInt (thisNode->getInt (0));
	    break;
	case NTDouble:
	    newNode = new TableExprNodeConstDouble (thisNode->getDouble (0));
	    break;
	case NTComplex:
	    newNode = new TableExprNodeConstDComplex (thisNode->getDComplex(0));
	    break;
	case NTString:
	    newNode = new TableExprNodeConstString (thisNode->getString (0));
	    break;
	case NTRegex:
	    newNode = new TableExprNodeConstRegex (thisNode->getRegex (0));
	    break;
	case NTDate:
	    newNode = new TableExprNodeConstDate (thisNode->getDate (0));
	    break;
	default:
	    TableExprNode::throwInvDT ("in convertNode"); // should never occur
	}
    }else{
	switch (thisNode->dataType()) {
	case NTBool:
	    newNode = new TableExprNodeArrayConstBool
                                         (thisNode->getArrayBool (0));
	    break;
	case NTInt:
	    newNode = new TableExprNodeArrayConstInt
                                         (thisNode->getArrayInt (0));
	    break;
	case NTDouble:
	    newNode = new TableExprNodeArrayConstDouble
                                         (thisNode->getArrayDouble (0));
	    break;
	case NTComplex:
	    newNode = new TableExprNodeArrayConstDComplex
                                         (thisNode->getArrayDComplex (0));
	    break;
	case NTString:
	    newNode = new TableExprNodeArrayConstString
                                         (thisNode->getArrayString (0));
	    break;
	case NTDate:
	    newNode = new TableExprNodeArrayConstDate
                                         (thisNode->getArrayDate (0));
	    break;
	default:
	    TableExprNode::throwInvDT ("in convertNode"); // should never occur
	}
    }
    newNode->setUnit (thisNode->unit());
    delete thisNode;
    return newNode;
}    


TableExprNodeRep* TableExprNodeRep::getRep (TableExprNode& node)
{
    return node.getRep();
}




// ------------------------------
// TableExprNodeBinary functions
// ------------------------------

TableExprNodeBinary::TableExprNodeBinary (NodeDataType tp,
					  ValueType vtype,
					  OperType oper,
					  const Table& table)
: TableExprNodeRep (tp, vtype, oper, table),
  lnode_p          (0),
  rnode_p          (0)
{}
    
TableExprNodeBinary::TableExprNodeBinary (NodeDataType tp,
					  const TableExprNodeRep& that,
					  OperType oper)
: TableExprNodeRep (that),
  lnode_p          (0),
  rnode_p          (0)
{
    dtype_p  = tp;
    optype_p = oper;
}
    
TableExprNodeBinary::~TableExprNodeBinary()
{
    unlink (lnode_p);
    unlink (rnode_p);
}

void TableExprNodeBinary::show (ostream& os, uInt indent) const
{
    TableExprNodeRep::show (os, indent);
    if (lnode_p != 0) {
	lnode_p->show (os, indent+2);
    }
    if (rnode_p != 0) {
	rnode_p->show (os, indent+2);
    }
}

void TableExprNodeBinary::getAggrNodes (vector<TableExprNodeRep*>& aggr)
{
  if (lnode_p) {
    lnode_p->getAggrNodes (aggr);
  }
  if (rnode_p) {
    rnode_p->getAggrNodes (aggr);
  }
}

void TableExprNodeBinary::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
  if (lnode_p) {
    lnode_p->getColumnNodes (cols);
  }
  if (rnode_p) {
    rnode_p->getColumnNodes (cols);
  }
}

// Check the datatypes and get the common one.
// For use with operands.
TableExprNodeRep::NodeDataType TableExprNodeBinary::getDT
                                             (NodeDataType leftDtype,
					      NodeDataType rightDtype,
					      OperType opt)
{
    // Equal types is mostly fine.
    if (leftDtype == rightDtype) {
        if (leftDtype==NTBool || leftDtype==NTDouble ||
            leftDtype==NTComplex || leftDtype==NTString ||
            (leftDtype==NTInt && opt!=OtDivide)) {
          return leftDtype;
        }
    }
    // If one is an Int, try as Double.
    if (leftDtype  == NTInt) leftDtype = NTDouble;
    if (rightDtype == NTInt) rightDtype = NTDouble;
    // Double matches Int and Double.
    if (leftDtype  == NTDouble  &&  rightDtype == NTDouble) {
      return NTDouble;
    }
    // Complex matches Double and Complex.
    if ((leftDtype == NTComplex  &&  rightDtype == NTDouble)
    ||  (leftDtype == NTDouble   &&  rightDtype == NTComplex)) {
	return NTComplex;
    }
    // String and Regex will get Regex
    if ((leftDtype == NTString  &&  rightDtype == NTRegex)
    ||  (leftDtype == NTRegex  &&  rightDtype == NTString)) {
	return NTRegex;
    }
    // A String will be promoted to Date when used with a Date.
    if (leftDtype == NTDate  &&  rightDtype == NTString) {
	rightDtype = NTDate;
    }
    if (leftDtype == NTString  &&  rightDtype == NTDate) {
	leftDtype = NTDate;
    }
    // A double will be promoted to Date when used with a Date in a comparison.
    if (opt >= OtEQ  &&  opt <= OtIN) {
        if (leftDtype == NTDate  &&  rightDtype == NTDouble) {
            rightDtype = NTDate;
        }
        if (leftDtype == NTDouble  &&  rightDtype == NTDate) {
            leftDtype = NTDate;
        }
    }      
    // Date - Date will get Double
    if (leftDtype == NTDate  &&  rightDtype == NTDate  &&  opt == OtMinus) {
	return NTDouble;
    }
    // Date matches Date; Date+Date is not allowed
    // Note that date/date or date*date has been catched earlier.
    if (leftDtype == NTDate  &&  rightDtype == NTDate  &&  opt != OtPlus) {
	return NTDate;
    }
    // Date+Double and Date-Double is allowed
    if (opt == OtPlus  ||  opt == OtMinus) {
	if (leftDtype == NTDate  &&  rightDtype == NTDouble) {
	    return NTDate;
	}
    }
    // Double+Date is allowed
    if (opt == OtPlus) {
	if (leftDtype == NTDouble  &&  rightDtype == NTDate) {
	    return NTDate;
	}
    }
    TableExprNode::throwInvDT("TableExprNodeBinary::getDT cannot combine "
                              "arguments with data type " +
                              typeString(leftDtype) + " and " +
                              typeString(rightDtype));
    return NTComplex; // compiler satisfaction
}

TableExprNodeRep TableExprNodeBinary::getTypes (const TableExprNodeRep& left,
						const TableExprNodeRep& right,
						OperType opt)
{
    ValueType leftVtype = left.valueType();
    ValueType rightVtype = right.valueType();
    // Check that the value type is VTScalar and/or VTArray.
    if ((leftVtype  != VTArray  &&  leftVtype  != VTScalar)
    ||  (rightVtype != VTArray  &&  rightVtype != VTScalar)) {
	throw (TableInvExpr ("Operand has to be a scalar or an array"));
    }
    // The resulting value type is Array if one of the operands is array.
    // Otherwise it is scalar.
    ValueType vtype;
    if (leftVtype == VTArray  ||  rightVtype == VTArray) {
	vtype = VTArray;
    }else{
	vtype = VTScalar;
    }
    NodeDataType leftDtype = left.dataType();
    NodeDataType rightDtype = right.dataType();
    NodeDataType dtype = getDT (leftDtype, rightDtype, opt);
    ArgType atype = ArrArr;
    // Set the argument type in case arrays are involved.
    // Its setting is not important if 2 scalars are involved.
    if (leftVtype == VTScalar) {
	atype = ScaArr;
    }
    if (rightVtype == VTScalar) {
	atype = ArrSca;
    }
    // Get dimensionality and shape of result.
    IPosition shape;
    Int ndim = -1;
    if (leftVtype == VTScalar  &&  rightVtype == VTScalar) {
	ndim = 0;
    }else{
	// Check if the 2 operands have matching dimensionality and shape.
	// This can only be done if they are fixed.
	// Also determine the resulting dimensionality and shape.
	Int leftNdim = left.ndim();
	Int rightNdim = right.ndim();
	if (leftNdim > 0) {
	    ndim = leftNdim;
	    if (rightNdim > 0  &&  leftNdim != rightNdim) {
		throw (TableInvExpr ("Mismatching dimensionality of operands"));
	    }
	} else if (rightNdim > 0) {
	    ndim = rightNdim;
	}
	IPosition leftShape = left.shape();
	IPosition rightShape = right.shape();
	leftNdim = leftShape.nelements();
	rightNdim = rightShape.nelements();
	if (leftNdim > 0) {
	    shape = leftShape;
	    if (rightNdim > 0  &&  !leftShape.isEqual (rightShape)) {
		throw (TableInvExpr ("Mismatching shape of operands"));
	    }
	} else if (rightNdim > 0) {
	    shape = rightShape;
	}
    }
    // The result is constant when both operands are constant.
    ExprType extype = Variable;
    if (left.isConstant()  &&  right.isConstant()) {
	extype = Constant;
    }
    // Determine from which table the expression is coming
    // and whether the tables match.
    Table table = left.table();
    checkTablePtr (table, &right);
    return TableExprNodeRep (dtype, vtype, opt, atype, extype, ndim, shape,
			     table);
}

// Fill the child pointers of a node.
// Also reduce the tree if possible by combining constants.
// When only one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprNodeBinary::fillNode (TableExprNodeBinary* thisNode,
						 TableExprNodeRep* left,
						 TableExprNodeRep* right,
						 Bool convertConstType,
                                                 Bool adaptDataType)
{
    // Fill the children and link to them.
    // If needed, change the children to get matching data types.
    thisNode->lnode_p = left->link();
    if (right != 0) {
      thisNode->rnode_p = right->link();
      if (adaptDataType) {
        // Adapt data types as needed.

	// NTRegex will always be placed in the right node 
	if (left->dataType() == NTRegex) {
	    thisNode->lnode_p = right;
	    thisNode->rnode_p = left;
	}

	// If expression with date and double or string, convert to date
	if (left->dataType() == NTDate) {
            if (right->dataType() == NTString) {
                TableExprNode dNode = datetime (right);
	        unlink (right);
	        thisNode->rnode_p = getRep(dNode)->link();
            } else if (right->dataType() == NTDouble  ||
                       right->dataType() == NTInt) {
                TableExprNode dNode = mjdtodate (right);
	        unlink (right);
	        thisNode->rnode_p = getRep(dNode)->link();
            }
        }
	if (right->dataType() == NTDate) {
            if (left->dataType() == NTString) {
                TableExprNode dNode = datetime (left);
                unlink (left);
                thisNode->lnode_p = getRep(dNode)->link();
            } else if (left->dataType() == NTDouble  ||
                       left->dataType() == NTInt) {
                TableExprNode dNode = mjdtodate (left);
                unlink (left);
                thisNode->lnode_p = getRep(dNode)->link();
            }
        }
        // date-date results in double, so convert if needed.
        if (thisNode->dataType() == NTDouble) {
            if (left->dataType() == NTDate) {
                TableExprNode dNode = mjd (left);
                unlink (left);
                thisNode->lnode_p = getRep(dNode)->link();
            }
            if (right->dataType() == NTDate) {
                TableExprNode dNode = mjd (right);
                unlink (right);
                thisNode->rnode_p = getRep(dNode)->link();
            }
        }
      }
    }
    // Check and adapt units.
    thisNode->handleUnits();
    return convertNode (thisNode, convertConstType);
}

const Unit& TableExprNodeBinary::makeEqualUnits (TableExprNodeRep* left,
						 TableExprNodeRep*& right)
{
    // The first real unit is chosen as the result unit.
    const Unit* unit = &(left->unit());
    if (right != 0) {
        if (unit->empty()) {
	    unit = &(right->unit());
	} else if (! right->unit().empty()) {
	    TableExprNodeUnit::adaptUnit (right, *unit);
	}
    }
    return *unit;
}

void TableExprNodeBinary::handleUnits()
{
    const Unit& resUnit = makeEqualUnits (lnode_p, rnode_p);
    // A comparison has no units, so only set if not bool.
    if (dataType() != NTBool) {
      setUnit (resUnit);
    }
}

void TableExprNodeBinary::convertConstChild()
{
    // Convert data type of a constant 
    // from Double to DComplex if:
    //   - there are 2 nodes
    //   - data types are not equal
    //   - conversion from Int or Double can be done
    if (rnode_p == 0  ||  lnode_p->dataType() == rnode_p->dataType()) {
	return;
    }
    // Determine if and which node is a constant.
    TableExprNodeRep** constNode = &lnode_p;
    TableExprNodeRep** otherNode = &rnode_p;
    if (! lnode_p->isConstant()) {
	if (! rnode_p->isConstant()) {
	    return;
	}
	constNode = &rnode_p;
	otherNode = &lnode_p;
    }
    // Only scalars and arrays can be converted.
    ValueType vtype = (**constNode).valueType();
    if (vtype != VTScalar  &&  vtype != VTArray) {
	return;
    }
    // The only possible conversion is from Int or Double to Double or DComplex.
    NodeDataType newType = NTDouble;
    if ((**otherNode).dataType() == NTDouble) {
        if ((**constNode).dataType() != NTInt) {
            return;
        }
    } else if ((**otherNode).dataType() == NTComplex) {
        newType = NTComplex;
        if (((**constNode).dataType() != NTInt)
        &&  ((**constNode).dataType() != NTDouble)) {
            return;
        }
    } else {
        return;
    }
    // Yeah, we have something to convert.
#if defined(AIPS_TRACE)
    cout << "constant converted" << endl;
#endif
    TableExprNodeRep* newNode;
    if (vtype == VTScalar) {
      if (newType == NTDouble) {
	newNode = new TableExprNodeConstDouble ((**constNode).getDouble(0));
      } else {
	newNode = new TableExprNodeConstDComplex ((**constNode).getDouble(0));
      }
    }else{
      if (newType == NTDouble) {
	newNode = new TableExprNodeArrayConstDouble
	                                    ((**constNode).getArrayDouble(0));
      } else {
	newNode = new TableExprNodeArrayConstDComplex
	                                    ((**constNode).getArrayDouble(0));
      }
    }
    newNode->setUnit ((**constNode).unit());
    unlink (*constNode);
    *constNode = newNode->link();
}





// ----------------------------
// TableExprNodeMulti functions
// ----------------------------

TableExprNodeMulti::TableExprNodeMulti (NodeDataType tp, ValueType vtype,
					OperType oper,
					const TableExprNodeRep& source)
: TableExprNodeRep (tp, vtype, oper, source.table()),
  operands_p       (0)
{
    exprtype_p = source.exprType();
}

TableExprNodeMulti::~TableExprNodeMulti()
{
    for (uInt i=0; i<operands_p.nelements(); i++) {
	unlink (operands_p[i]);
    }
}

void TableExprNodeMulti::show (ostream& os, uInt indent) const
{
    TableExprNodeRep::show (os, indent);
    for (uInt j=0; j<operands_p.nelements(); j++) {
	if (operands_p[j] != 0) {
	    operands_p[j]->show (os, indent+2);
	}
    }
}

void TableExprNodeMulti::getAggrNodes (vector<TableExprNodeRep*>& aggr)
{
    for (uInt j=0; j<operands_p.nelements(); j++) {
	if (operands_p[j] != 0) {
            operands_p[j]->getAggrNodes (aggr);
	}
    }
}

void TableExprNodeMulti::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    for (uInt j=0; j<operands_p.nelements(); j++) {
	if (operands_p[j] != 0) {
            operands_p[j]->getColumnNodes (cols);
	}
    }
}

CountedPtr<TableExprGroupFuncBase> TableExprNodeRep::makeGroupAggrFunc()
{
  throw AipsError ("TableExprNodeRep::makeGroupAggrFunc should not be called");
}

Bool TableExprNodeRep::isLazyAggregate() const
{
  return True;
}



uInt TableExprNodeMulti::checkNumOfArg
                                    (uInt low, uInt high,
				     const PtrBlock<TableExprNodeRep*>& nodes)
{
    if (nodes.nelements() < low) {
	throw (TableInvExpr("too few function arguments"));
    } else if (nodes.nelements() > high) {
	throw (TableInvExpr("too many function arguments"));
    }
    return nodes.nelements();
}

TableExprNodeRep::NodeDataType TableExprNodeMulti::checkDT
				    (Block<Int>& dtypeOper,
				     NodeDataType dtIn,
				     NodeDataType dtOut,
				     const PtrBlock<TableExprNodeRep*>& nodes)
{
    uInt nelem = nodes.nelements();
    dtypeOper.resize (nelem);
    dtypeOper.set (dtIn);
    // NTAny means that it can be any type.
    // An output of NTAny means that the types have to match.
    if (dtIn == NTAny) {
        if (dtOut != NTAny) {
          // Make sure output type is not generic.
          AlwaysAssert (dtOut!=NTNumeric && dtOut!=NTReal && dtOut!=NTDouCom,
                        AipsError);
          return dtOut;
	}
        // Input data type is first one. Set to NTNumeric if numeric, so
        // numeric data types can be mixed.
	dtIn = nodes[0]->dataType();
	if (dtIn == NTInt  ||  dtIn == NTDouble  ||  dtIn == NTComplex) {
	    dtIn = NTNumeric;
	}
    }
    uInt i;
    NodeDataType resultType = dtIn;
    if (dtIn == NTNumeric) {
        // NTNumeric -> dtIn must be NTComplex or NTDouble or NTInt
        //              and set resultType to the highest type of dtIn
        resultType = (dtOut==NTDouCom ? NTDouble : NTInt);
	for (i=0; i<nelem; i++) {
	    if (nodes[i]->dataType() == NTComplex) {
		resultType = NTComplex;
	    } else if (nodes[i]->dataType() == NTDouble) {
                if (resultType != NTComplex) {
                    resultType = NTDouble;
                }
            } else if (nodes[i]->dataType() != NTInt) {
		TableExprNode::throwInvDT("function argument is not numeric");
	    }
	}
    } else if (dtIn == NTReal) {
        // NTReal -> dtIn must be NTDouble or NTInt
        //           and set resultType to the highest type of dtIn
        resultType = (dtOut==NTDouCom ? NTDouble : NTInt);
	for (i=0; i<nelem; i++) {
            if (nodes[i]->dataType() == NTDouble) {
                resultType = NTDouble;
            } else if (nodes[i]->dataType() != NTInt) {
		TableExprNode::throwInvDT("function argument is not real");
	    }
	}
    } else {
	// Data types of the nodes must match dtIn
	for (i=0; i<nelem; i++) {
	    // Double or String to Date conversion is possible.
	    if (nodes[i]->dataType() != dtIn) {
                if (dtIn == NTDate) {
                    if (nodes[i]->dataType() != NTString  &&
                        nodes[i]->dataType() != NTDouble) {
                      TableExprNode::throwInvDT("function argument is not "
                                                "date, string or real");
                    }
                } else {
                    TableExprNode::throwInvDT("function argument is not " +
                                              typeString(dtIn));
                }
	    }
	}
    }
    if (dtOut == NTReal) {
        if (resultType == NTComplex) {
            resultType = NTDouble;
        }
    } else if (dtOut == NTDouCom) {
        if (resultType == NTInt) {
            resultType = NTDouble;
        }
    } else if (dtOut != NTNumeric  &&  dtOut != NTAny) {
        resultType = dtOut;
    }
    return resultType;
}

String TableExprNodeRep::typeString (NodeDataType type)
{
  switch (type) {
  case NTBool:
    return "Bool";
  case NTInt:
    return "Integer";
  case NTDouble:
    return "Double";
  case NTComplex:
    return "Complex";
  case NTString:
    return "String";
  case NTRegex:
    return "Regex";
  case NTDate:
    return "DateTime";
  case NTReal:
    return "Real";
  case NTDouCom:
    return "Double/Complex";
  case NTNumeric:
    return "Numeric";
  case NTAny:
    return "Any";
  }
  throw AipsError("TableExprNodeRep::typeString NodeDataType");
}

String TableExprNodeRep::typeString (ValueType type)
{
  switch (type) {
  case VTScalar:
    return "Scalar";
  case VTArray:
    return "Array";
  case VTRecord:
    return "Record";
  case VTSetElem:
    return "SetElement";
  case VTSet:
    return "Set";
  case VTIndex:
    return "Index";
  }
  throw AipsError("TableExprNodeRep::typeString ValueType");
}

} //# NAMESPACE CASACORE - END
