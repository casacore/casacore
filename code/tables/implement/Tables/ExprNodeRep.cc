//# ExprNodeRep.cc: Representation class for a table column expression tree
//# Copyright (C) 1994,1995,1996,1997
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

#include <aips/Tables/ExprNodeRep.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/ExprRange.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Containers/Block.h>


// The constructor to be used by the derived classes.
TableExprNodeRep::TableExprNodeRep (NodeDataType dtype, OperType optype)
: count_p     (0),
  baseTabPtr_p(0),
  dtype_p     (dtype),
  optype_p    (optype),
  isNull_p    (False),
  exprtype_p  (Variable)
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

uInt TableExprNodeRep::ndim() const
    { return 0; }
IPosition TableExprNodeRep::shape() const
    { return IPosition(); }

//# Determine the number of rows in the table used in the expression.
uInt TableExprNodeRep::nrow() const
{
    if (baseTabPtr_p == 0) {
	return 0;
    }
    return baseTabPtr_p->nrow();
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
				    double st, double end)
{
    if (tsn == 0) {
	blrange.resize (0, True);
    }else{
	blrange.resize (1, True);
	blrange[0] = TableExprRange (tsn->getColumn(), st, end);
    }
}

//# Supply the default functions for the get functions.
Bool TableExprNodeRep::getBool (uInt)
{
    TableExprNode::throwInvDT ("(getBool not implemented)");
    return False;
}
double TableExprNodeRep::getDouble (uInt)
{
    TableExprNode::throwInvDT ("(getDouble not implemented)");
    return 0;
}
DComplex TableExprNodeRep::getDComplex (uInt)
{
    TableExprNode::throwInvDT ("(getDComplex not implemented)");
    return 0;
}
String TableExprNodeRep::getString (uInt)
{
    TableExprNode::throwInvDT ("(getString not implemented)");
    return "";
}
Regex TableExprNodeRep::getRegex (uInt)
{
    TableExprNode::throwInvDT ("(getRegex not implemented)");
    return Regex("");
}
MVTime TableExprNodeRep::getDate (uInt)
{
    TableExprNode::throwInvDT ("(getDate not implemented)");
    return MVTime(0.);
}



Array<Bool>     TableExprNodeRep::getColumnBool()
{
    uInt nrrow = nrow();
    Vector<Bool> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
	vec(i) = getBool (i);
    }
    return vec;
}
Array<uChar>    TableExprNodeRep::getColumnuChar()
{
    TableExprNode::throwInvDT ("(getColumnuChar not implemented)");
    return Array<uChar>();
}
Array<Short>    TableExprNodeRep::getColumnShort()
{
    TableExprNode::throwInvDT ("(getColumnShort not implemented)");
    return Array<Short>();
}
Array<uShort>   TableExprNodeRep::getColumnuShort()
{
    TableExprNode::throwInvDT ("(getColumnuShort not implemented)");
    return Array<uShort>();
}
Array<Int>      TableExprNodeRep::getColumnInt()
{
    TableExprNode::throwInvDT ("(getColumnInt not implemented)");
    return Array<Int>();
}
Array<uInt>     TableExprNodeRep::getColumnuInt()
{
    TableExprNode::throwInvDT ("(getColumnuInt not implemented)");
    return Array<uInt>();
}
Array<Float>    TableExprNodeRep::getColumnFloat()
{
    TableExprNode::throwInvDT ("(getColumnFloat not implemented)");
    return Array<Float>();
}
Array<Double>   TableExprNodeRep::getColumnDouble()
{
    uInt nrrow = nrow();
    Vector<Double> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
	vec(i) = getDouble (i);
    }
    return vec;
}
Array<Complex>  TableExprNodeRep::getColumnComplex()
{
    TableExprNode::throwInvDT ("(getColumnComplex not implemented)");
    return Array<Complex>();
}
Array<DComplex> TableExprNodeRep::getColumnDComplex()
{
    uInt nrrow = nrow();
    Vector<DComplex> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
	vec(i) = getDComplex (i);
    }
    return vec;
}
Array<String>   TableExprNodeRep::getColumnString()
{
    uInt nrrow = nrow();
    Vector<String> vec (nrrow);
    for (uInt i=0; i<nrrow; i++) {
	vec(i) = getString (i);
    }
    return vec;
}

TableExprNodeRep* TableExprNodeRep::convertNode (TableExprNodeRep* thisNode)
{
    // When no BaseTable involved, we have a constant subexpression.
    // Evaluate it and replace the node.
    // Otherwise replace thisNode.
    if (thisNode->baseTabPtr_p != 0) {
	thisNode->convertConst();
	return thisNode;
    }
    TableExprNodeRep* newNode;
    switch (thisNode->dataType()) {
    case NTBool:
	newNode = new TableExprNodeConstBool (thisNode->getBool (0));
	break;
    case NTDouble:
	newNode = new TableExprNodeConstDouble (thisNode->getDouble (0));
	break;
    case NTComplex:
	newNode = new TableExprNodeConstDComplex (thisNode->getDComplex (0));
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
	TableExprNode::throwInvDT ("in convertNode");    // should never occur
    }
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

TableExprNodeBinary::TableExprNodeBinary (NodeDataType tp, OperType oper)
: TableExprNodeRep (tp, oper),
  rnode_p          (0),
  lnode_p          (0)
{}
    
TableExprNodeBinary::~TableExprNodeBinary()
{
    unlink (lnode_p);
    unlink (rnode_p);
}

// Check the datatypes and get the common one.
// For use with operands.
TableExprNodeRep::NodeDataType TableExprNodeBinary::getDT
                                             (NodeDataType left_dt,
					      NodeDataType right_dt,
					      OperType opt)
{
    // Bool only matches itself.
    if (left_dt == NTBool  &&  right_dt == NTBool) {
	return NTBool;
    }
    // String matches String.
    if (left_dt == NTString  &&  right_dt == NTString) {
	return NTString;
    }
    // String and Regex will get Regex
    if ((left_dt == NTString  &&  right_dt == NTRegex)
    ||  (left_dt == NTRegex  &&  right_dt == NTString)) {
	return NTRegex;
    }
    // Date - Date will get Double
    if (left_dt == NTDate  &&  right_dt == NTDate  &&  opt == OtMinus) {
	return NTDouble;
    }
    // Date matches Date; Date+Date is not allowed
    if ((left_dt == NTDate  && right_dt == NTDate  &&  opt != OtPlus)) {
	return NTDate;
    }
    if ((left_dt == NTDate  &&  right_dt == NTDouble)
    ||  (left_dt == NTDouble  &&  right_dt == NTDate  &&  opt != OtMinus)) {
	// Double - Date not allowed
	return NTDate;
    }
    if ((left_dt == NTDate  &&  right_dt == NTString)
    ||  (left_dt == NTString  &&  right_dt == NTDate)) {
	return NTDate;
    }
    if (left_dt == NTDouble  &&  right_dt == NTDouble) {
	return NTDouble;
    }
    if ((left_dt == NTComplex  &&  right_dt == NTDouble)
    ||  (left_dt == NTDouble   &&  right_dt == NTComplex)
    ||  (left_dt == NTComplex  &&  right_dt == NTComplex)) {
	return NTComplex;
    }
    TableExprNode::throwInvDT();
    return NTComplex;                  // compiler satisfaction
}

// Fill the child pointers of a node.
// Also reduce the tree if possible by combining constants.
// When only one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprNodeBinary::fillNode (TableExprNodeBinary* thisNode,
						 TableExprNodeRep* left,
						 TableExprNodeRep* right)
{
    // Fill the children and link to them.
    thisNode->lnode_p = left->link();
    if (right != 0) {
	thisNode->rnode_p = right->link();
    }
    // Check and fill the BaseTable pointer.
    thisNode->checkTable();

    if (right != 0) {
	// NTRegex will always be placed in the right node 
	if (left->dataType() == NTRegex) {
	    thisNode->lnode_p = right;
	    thisNode->rnode_p = left;
	}

	// If expression with date and string, convert string to date
	if (left->dataType() == NTDate  &&  right->dataType() == NTString) {
	    TableExprNode dNode = datetime (right);
	    unlink (right);
	    thisNode->rnode_p = getRep(dNode)->link();
	}
	if (left->dataType() == NTString  &&  right->dataType() == NTDate) {
	    TableExprNode dNode = datetime (left);
	    unlink (left);
	    thisNode->lnode_p = getRep(dNode)->link();
	}
    }
    return convertNode (thisNode);
}

void TableExprNodeBinary::convertConst()
{
    // Convert data type of a constant 
    // from double to DComplex if:
    //   - there are 2 nodes
    //   - data types are not equal
    //   - data of constant is double
    if (rnode_p == 0  ||  lnode_p->dataType() == rnode_p->dataType()) {
	return;
    }
    // Determine if and which node is a constant.
    TableExprNodeRep** constNode = &lnode_p;
    if (lnode_p->operType() != OtConst) {
	if (rnode_p->operType() != OtConst) {
	    return;
	}
	constNode = &rnode_p;
    }
    if ((**constNode).dataType() != NTDouble) {
	return;
    }
    // Yeah, we have something to convert (from double to DComplex).
    TableExprNodeRep* newNode = new TableExprNodeConstDComplex
	                                   ((**constNode).getDouble(0));
    unlink (*constNode);
    *constNode = newNode->link();
}

// Check if the children are derived from the same table.
// Also fill in that table for this node.
// Note that for constants or keywords the baseTabPtr's will be zero.
void TableExprNodeBinary::checkTable ()
{
    baseTabPtr_p = lnode_p->baseTablePtr();
    if (rnode_p == 0) {
	return;                       // unary operator
    }
    if (baseTabPtr_p == 0) {
	baseTabPtr_p = rnode_p->baseTablePtr();
    }else{
	if (rnode_p->baseTablePtr() != baseTabPtr_p
        &&  rnode_p->baseTablePtr() != 0) {
	    throw (TableInvExpr ("subexpressions use different tables"));
	}
    }
}




// ----------------------------
// TableExprNodeMulti functions
// ----------------------------

TableExprNodeMulti::TableExprNodeMulti (NodeDataType tp, OperType oper)
: TableExprNodeRep (tp, oper),
  operands_p       (0)
{}

TableExprNodeMulti::~TableExprNodeMulti()
{
    for (uInt i=0; i<operands_p.nelements(); i++) {
	unlink (operands_p[i]);
    }
}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
// When one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprNodeMulti::fillNode
                                   (TableExprNodeMulti* thisNode,
				    PtrBlock<TableExprNodeRep*>& nodes,
				    const Block<Int>& dtypeOper)
{
    uInt i;
    // Copy block of children.
    thisNode->operands_p.resize (nodes.nelements());
    for (i=0; i<nodes.nelements(); i++) {
	thisNode->operands_p[i] = nodes[i]->link();
    }
    // Check and fill the BaseTable pointer.
    thisNode->checkTable();
    // Convert String to Date if needed
    for (i=0; i<nodes.nelements(); i++) {
	if (nodes[i]->dataType() == NTString  &&  dtypeOper[i] == NTDate) {
	    TableExprNode dNode = datetime (thisNode->operands_p[i]);
	    unlink (thisNode->operands_p[i]);
	    thisNode->operands_p[i] = getRep (dNode)->link();
	}
    }
    if (thisNode->operands_p.nelements() > 0) {
	return convertNode (thisNode);
    }
    return thisNode;
}

void TableExprNodeMulti::convertConst()
{}

// Check if the children are derived from the same table.
// Also fill in that table for this node.
// Note that for constants or keywords the baseTabPtr's will be zero.
void TableExprNodeMulti::checkTable()
{
    baseTabPtr_p = 0;
    for (uInt i=0; i<operands_p.nelements(); i++) {
	if (operands_p[i]->baseTablePtr() != baseTabPtr_p 
	&&  operands_p[i]->baseTablePtr() != 0) {
	    if (baseTabPtr_p == 0) {
		baseTabPtr_p = operands_p[i]->baseTablePtr();
	    }else{
		throw (TableInvExpr ("subexpressions use different tables"));
	    }
	}
    }
}

uInt TableExprNodeMulti::checkNumOfArg
                                    (uInt low, uInt high,
				     const PtrBlock<TableExprNodeRep*>& nodes)
{
    if (nodes.nelements() < low) {
	throw (TableInvExpr("too few function arguments"));
    } else if (nodes.nelements() > high) {
	throw (TableInvExpr("", "too many function arguments"));
    }
    return nodes.nelements();
}

TableExprNodeRep::NodeDataType TableExprNodeMulti::checkDT
				    (Block<Int>& dtypeOper,
				     NodeDataType dtIn,
				     NodeDataType dtOut,
				     const PtrBlock<TableExprNodeRep*>& nodes)
{
    dtypeOper.resize (nodes.nelements());
    dtypeOper.set (dtIn);
    Int i;
    NodeDataType resultType = dtIn;
    // NTNumeric -> dtIn must be NTComplex or NTDouble
    //              and set resultType to the highest type of dtIn
    if (dtIn == NTNumeric) {
	resultType = NTDouble;
	for (i=0; i<nodes.nelements(); i++) {
	    if (nodes[i]->dataType() == NTComplex) {
		resultType = NTComplex;
	    } else if (nodes[i]->dataType() != NTDouble) {
		TableExprNode::throwInvDT();
	    }
	}
    } else {
	// Data types of the nodes must match dtIn
	for (i=0; i<nodes.nelements(); i++) {
	    // String to Date conversion is possible.
	    if (nodes[i]->dataType() != dtIn) {
		if (nodes[i]->dataType() != NTString  ||  dtIn != NTDate) {
		    TableExprNode::throwInvDT();
		}
	    }
	}
    }
    if (dtOut == NTNumeric) {
	return resultType;
    }
    return dtOut;
}
