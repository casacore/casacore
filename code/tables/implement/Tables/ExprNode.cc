//# ExprNode.cc: Handle class for a table column expression tree
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

#include <aips/Tables/ExprFuncNode.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/ExprRange.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Containers/Block.h>
#include <aips/Utilities/DataType.h>
#include <aips/Tables/ExprNodeArray.h>

TableExprNode::TableExprNode() : node_p(0)
{}

//# Constructors for the various constants.
//# These objects are created as temporaries by the compiler.
TableExprNode::TableExprNode (const Int& val)
{
    node_p = new TableExprNodeConstDouble (double (val));
    node_p->link();
}
TableExprNode::TableExprNode (const double& val)
{
    node_p = new TableExprNodeConstDouble (double (val));
    node_p->link();
}
TableExprNode::TableExprNode (const DComplex& val)
{
    node_p = new TableExprNodeConstDComplex (val);
    node_p->link();
}
TableExprNode::TableExprNode (const String& val)
{
    node_p = new TableExprNodeConstString (val);
    node_p->link();
}
TableExprNode::TableExprNode (const char* val)
{
    node_p = new TableExprNodeConstString (String(val));
    node_p->link();
}
TableExprNode::TableExprNode (const Regex& val)
{
    node_p = new TableExprNodeConstRegex (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Bool& val)
{
    node_p = new TableExprNodeConstBool (val);
    node_p->link();
}
TableExprNode::TableExprNode (const MVTime& val)
{
    node_p = new TableExprNodeConstDate (val);
    node_p->link();
}

TableExprNode::TableExprNode (TableExprNodeRep* node)
: node_p (node->link())
{}

TableExprNode::TableExprNode (const TableExprNode& node)
: node_p (node.node_p->link())
{}

TableExprNode& TableExprNode::operator= (const TableExprNode& that)
{
    if (this != &that) {
	TableExprNodeRep::unlink (node_p);
	node_p = that.node_p->link();
    }
    return *this;
}

//# Destructor.
TableExprNode::~TableExprNode ()
{
    TableExprNodeRep::unlink (node_p);
}


DataType TableExprNode::getColumnDataType() const
{
    DataType dt;
    if (node_p->getColumnDataType (dt)) {
	return dt;
    }
    return dataType();
}


Bool TableExprNode::checkTable (const Table& table) const
{
    return ToBool (table.baseTablePtr() == node_p->baseTablePtr());
}


//# Throw the invalid datatype exception.
// <thrown>
//   <li> TableInvDT
// </thrown>
void TableExprNode::throwInvDT()
    { throw (TableInvExpr ("mismatch in operand data types")); }
void TableExprNode::throwInvDT (const String& message)
    { throw (TableInvExpr ("invalid data type, " + message)); }



TableExprNodeRep* TableExprNode::newPlus (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtPlus);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodePlusDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodePlusDComplex();
        break;
    case TableExprNodeRep::NTString:
        tsnptr = new TableExprNodePlusString();
        break;
    case TableExprNodeRep::NTDate:
	tsnptr = new TableExprNodePlusDate();
	break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newMinus (TableExprNodeRep* right) const
{
    // Double - Date is forbidden
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtMinus);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeMinusDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodeMinusDComplex();
        break;
    case TableExprNodeRep::NTDate:
	tsnptr = new TableExprNodeMinusDate();
	break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newTimes (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtTimes);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeTimesDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodeTimesDComplex();
        break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newDivide (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtDivide);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeDivideDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodeDivideDComplex();
        break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newModulo (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtModulo);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeModuloDouble();
        break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newEQ (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtEQ);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTBool:
        tsnptr = new TableExprNodeEQBool();
        break;
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeEQDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodeEQDComplex();
        break;
    case TableExprNodeRep::NTString:
        tsnptr = new TableExprNodeEQString();
        break;
    case TableExprNodeRep::NTRegex:
	tsnptr = new TableExprNodeEQRegex();
	break;
    case TableExprNodeRep::NTDate:
	tsnptr = new TableExprNodeEQDate();
	break;
    default:
        TableExprNode::throwInvDT();
    }
    
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newNE (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtNE);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTBool:
        tsnptr = new TableExprNodeNEBool();
        break;
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeNEDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodeNEDComplex();
        break;
    case TableExprNodeRep::NTString:
        tsnptr = new TableExprNodeNEString();
        break;
    case TableExprNodeRep::NTRegex:
	tsnptr = new TableExprNodeNERegex();
	break;
    case TableExprNodeRep::NTDate:
	tsnptr = new TableExprNodeNEDate();
	break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newGT (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtGT);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeGTDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodeGTDComplex();
        break;
    case TableExprNodeRep::NTString:
        tsnptr = new TableExprNodeGTString();
        break;
    case TableExprNodeRep::NTDate:
	tsnptr = new TableExprNodeGTDate();
	break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newGE (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtGE);
    TableExprNodeBinary* tsnptr;
    switch (dt) {
    case TableExprNodeRep::NTDouble:
        tsnptr = new TableExprNodeGEDouble();
        break;
    case TableExprNodeRep::NTComplex:
        tsnptr = new TableExprNodeGEDComplex();
        break;
    case TableExprNodeRep::NTString:
        tsnptr = new TableExprNodeGEString();
        break;
    case TableExprNodeRep::NTDate:
	tsnptr = new TableExprNodeGEDate();
	break;
    default:
        TableExprNode::throwInvDT();
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newOR (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtOR);
    if (dt != TableExprNodeRep::NTBool) {
	TableExprNode::throwInvDT ();
    }
    TableExprNodeBinary* tsnptr = new TableExprNodeOR();
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}

TableExprNodeRep* TableExprNode::newAND (TableExprNodeRep* right) const
{
    TableExprNodeRep::NodeDataType dt = TableExprNodeBinary::getDT
	                                         (node_p->dataType(),
						  right->dataType(),
						  TableExprNodeRep::OtAND);
    if (dt != TableExprNodeRep::NTBool) {
	TableExprNode::throwInvDT ();
    }
    TableExprNodeBinary* tsnptr = new TableExprNodeAND();
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right);
}


TableExprNode TableExprNode::operator+ ()
    { return *this; }

TableExprNode TableExprNode::operator- ()
{
    if (node_p->dataType() != TableExprNodeRep::NTDouble
    &&  node_p->dataType() != TableExprNodeRep::NTComplex) {
	throwInvDT ();
    }
    TableExprNodeBinary* tsnptr = new TableExprNodeMIN (node_p->dataType());
    return TableExprNodeBinary::fillNode (tsnptr, node_p, 0);
}

TableExprNode TableExprNode::operator! ()
{
    if (node_p->dataType() != TableExprNodeRep::NTBool) {
	throwInvDT ();
    }
    TableExprNodeBinary* tsnptr = new TableExprNodeNOT();
    return TableExprNodeBinary::fillNode (tsnptr, node_p, 0);
}


//# Create a column node on behalf of the Table class.
//# For builtin data types another type of node is created than
//# for other data types.
TableExprNode TableExprNode::newColumnNode (const Table& tab,
					    const BaseTable* tabptr,
					    const String& name,
					    Bool isArray)
{
    //# Get the column description. This throws an exception if
    //# the name is not a column.
    TableExprNodeRep* tsnptr;
    const ColumnDesc& coldes = tab.tableDesc().columnDesc (name);
    ROTableColumn col(tab, name);
    if (isArray  &&  coldes.isArray()) {
	switch(coldes.dataType()) {
	case TpBool:
	    tsnptr = new TableExprNodeArrayBool (col, tabptr);
	    break;
	case TpComplex:
	    tsnptr = new TableExprNodeArrayComplex (col, tabptr);
	    break;
	case TpDComplex:
	    tsnptr = new TableExprNodeArrayDComplex (col, tabptr);
	    break;
	case TpString:
	    tsnptr = new TableExprNodeArrayString (col, tabptr);
	    break;
	case TpUChar:
	    tsnptr = new TableExprNodeArrayuChar (col, tabptr);
	    break;
	case TpShort:
	    tsnptr = new TableExprNodeArrayShort(col, tabptr);
	    break;
	case TpUShort:
	    tsnptr = new TableExprNodeArrayuShort (col, tabptr);
	    break;
	case TpInt:
	    tsnptr = new TableExprNodeArrayInt (col, tabptr);
	    break;
	case TpUInt:
	    tsnptr = new TableExprNodeArrayuInt (col, tabptr);
	    break;
	case TpFloat:
	    tsnptr = new TableExprNodeArrayFloat (col, tabptr);
	    break;
	case TpDouble:
	    tsnptr = new TableExprNodeArrayDouble (col, tabptr);
	    break;
	default:
	    throw (TableInvExpr (name, "unknown data type"));
	}
    } else if (!isArray  &&  coldes.isScalar()) {
	tsnptr = new TableExprNodeColumn (tab, tabptr, name);
    } else {
	if (isArray) {
	    throw (TableInvExpr (name, " must be an Array column"));
	}else{
	    throw (TableInvExpr (name, " must be a Scalar column"));
	}
    }
    if (tsnptr == 0) {
	throw (AllocError ("TableExprNode::newColumnNode", 1));
    }
    return tsnptr;
}


//# Create a constant node for a keyword on behalf of the Table class.
//# The constructor reads in the value and stores it as a constant.
// <throw>
//   <li> AllocError
// </thrown>
TableExprNode TableExprNode::newKeyConst (const TableRecord& keyset,
					  const String& name,
					  Bool isArray)
{
    if (isArray) {
	throw (TableInvExpr ("array keyword " + name + " not supported yet"));
    }	
    TableExprNodeRep* tsnptr;
    switch (keyset.dataType (name)) {
    case TpBool:
	tsnptr = new TableExprNodeConstBool (keyset.asBool (name));
	break;
    case TpString:
	tsnptr = new TableExprNodeConstString (keyset.asString (name));
	break;
    case TpComplex:
    case TpDComplex:
	tsnptr = new TableExprNodeConstDComplex (keyset.asDComplex (name));
	break;
    case TpChar:
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
    case TpFloat:
    case TpDouble:
	tsnptr = new TableExprNodeConstDouble (keyset.asdouble (name));
	break;
    default:
	throw (TableInvExpr ("keyword " + name + " has unknown data type"));
    }
    return tsnptr;
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& node)
{
    Block<TableExprNode> bl(1);
    bl[0] = node;
    return newFunctionNode (ftype, bl, Table());
}
TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& node1,
			          const TableExprNode& node2)
{
    Block<TableExprNode> bl(2);
    bl[0] = node1;
    bl[1] = node2;
    return newFunctionNode (ftype, bl, Table());
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
				  Block<TableExprNode>& nodes,
				  const Table& table)
{
    // rownrFUNC and randomFUNC are special
    if (ftype == TableExprFuncNode::rownrFUNC) {
	return table.nodeRownr (1);           // first rownr is 1 in TaQL
    }                                         // (in C++ first rownr is 0)
    if (ftype == TableExprFuncNode::randFUNC) {
	return table.nodeRandom();
    }
    // Convert Block<TableExprNode> to PtrBlock<TableExprNodeRep*>.
    PtrBlock<TableExprNodeRep*> par = convertBlockTEN (nodes);
    // Check all the operands and get the resulting datatype of the function.
    // It also fills the expected data type of the operands.
    Block<Int> dtypeOper;
    TableExprNodeRep::NodeDataType result_dt = 
                        TableExprFuncNode::checkOperands (dtypeOper,
							  ftype, par);
    // Create new function node and fill it.
    TableExprFuncNode* fnode = new TableExprFuncNode (ftype, result_dt); 
    return TableExprNodeMulti::fillNode (fnode, par, dtypeOper);
}

TableExprNode TableExprNode::newArrayElementNode (TableExprNode& arrayNode,
						  Block<TableExprNode>& index)
{
    // Convert Block<TableExprNode> to PtrBlock<TableExprNodeRep*>.
    PtrBlock<TableExprNodeRep*> indices = convertBlockTEN (index);
    // Check all the indices, all indices must be double
    TableExprNodeIndex::checkIndices (indices);
    // Create new Index node (with origin 1) and fill it.
    TableExprNodeIndex*  inode = new TableExprNodeIndex (1);
    inode->fillNode (indices);
    inode->checkIndexValues (arrayNode.node_p);
    TableExprNodeBinary* anode = new TableExprNodeArrayElement
                                    (inode, arrayNode.node_p->dataType());
    return TableExprNodeBinary::fillNode (anode, arrayNode.node_p, inode);
}

TableExprNode TableExprNode::newRownrNode (const BaseTable* tabptr,
					   uInt origin)
{
    TableExprNodeRep* tsnptr = new TableExprNodeRownr (tabptr, origin);
    return tsnptr;
}

TableExprNode TableExprNode::newRandomNode (const BaseTable* tabptr)
{
    TableExprNodeRep* tsnptr = new TableExprNodeRandom (tabptr);
    return tsnptr;
}

DataType TableExprNode::dataType() const
{
    switch(node_p->dataType()) {
    case TableExprNodeRep::NTBool:
	return TpBool;
    case TableExprNodeRep::NTDouble:
	return TpDouble;
    case TableExprNodeRep::NTComplex:
	return TpDComplex;
    case TableExprNodeRep::NTString:
	return TpString;
    default:
	throwInvDT ("(of expression result)");
    }
    return TpOther;   // just here to get rid of that warning
}

PtrBlock<TableExprNodeRep*> TableExprNode::convertBlockTEN
                                          (Block<TableExprNode>& nodes)
{
    PtrBlock<TableExprNodeRep*> resultblock(nodes.nelements());
    for (uInt i=0; i<nodes.nelements(); i++) {
	resultblock[i] = nodes[i].node_p;
    }
    return resultblock;
}
