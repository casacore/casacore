//# ExprNode.cc: Handle class for a table column expression tree
//# Copyright (C) 1994,1995,1996,1997,1998
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
#include <aips/Tables/ExprNodeSet.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/ExprDerNodeArray.h>
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
TableExprNode::TableExprNode (const Bool& val)
{
    node_p = new TableExprNodeConstBool (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Int& val)
{
    node_p = new TableExprNodeConstDouble (Double (val));
    node_p->link();
}
TableExprNode::TableExprNode (const Double& val)
{
    node_p = new TableExprNodeConstDouble (Double (val));
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
TableExprNode::TableExprNode (const MVTime& val)
{
    node_p = new TableExprNodeConstDate (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Bool>& val)
{
    node_p = new TableExprNodeArrayConstBool (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<uChar>& val)
{
    node_p = new TableExprNodeArrayConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Short>& val)
{
    node_p = new TableExprNodeArrayConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<uShort>& val)
{
    node_p = new TableExprNodeArrayConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Int>& val)
{
    node_p = new TableExprNodeArrayConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<uInt>& val)
{
    node_p = new TableExprNodeArrayConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Float>& val)
{
    node_p = new TableExprNodeArrayConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Double>& val)
{
    node_p = new TableExprNodeArrayConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Complex>& val)
{
    node_p = new TableExprNodeArrayConstDComplex (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<DComplex>& val)
{
    node_p = new TableExprNodeArrayConstDComplex (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<String>& val)
{
    node_p = new TableExprNodeArrayConstString (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<MVTime>& val)
{
    node_p = new TableExprNodeArrayConstDate (val);
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


TableExprNode TableExprNode::in (const TableExprNodeSet& set) const
{
    TableExprNodeSet setcp = set;
    return newIN (setcp.setOrArray());
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

Bool TableExprNode::checkReplaceTable (const Table& table) const
{
    if (table.baseTablePtr() == node_p->baseTablePtr()) {
	return True;
    }
    if (node_p->baseTablePtr() == 0) {
	return False;
    }
    Bool equalDataTypes;
    if (! table.tableDesc().columnDescSet().isEqual
	                 (node_p->baseTablePtr()->tableDesc().columnDescSet(),
			  equalDataTypes)
    ||  !equalDataTypes) {
	return False;
    }
    node_p->replaceTablePtr (table, table.baseTablePtr());
    return True;
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
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtPlus);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodePlusDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodePlusDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodePlusString (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodePlusDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayPlusDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayPlusDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeArrayPlusString (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newMinus (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtMinus);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeMinusDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeMinusDComplex (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeMinusDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayMinusDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayMinusDComplex (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newTimes (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtTimes);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeTimesDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeTimesDComplex (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayTimesDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayTimesDComplex (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newDivide (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtDivide);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeDivideDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeDivideDComplex (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayDivideDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayDivideDComplex (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newModulo (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtModulo);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeModuloDouble (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayModuloDouble (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newEQ (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtEQ);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeEQBool (node);
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeEQDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeEQDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeEQString (node);
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr = new TableExprNodeEQRegex (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeEQDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeArrayEQBool (node);
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayEQDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayEQDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeArrayEQString (node);
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr = new TableExprNodeArrayEQRegex (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeArrayEQDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newNE (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtNE);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeNEBool (node);
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeNEDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeNEDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeNEString (node);
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr = new TableExprNodeNERegex (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeNEDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeArrayNEBool (node);
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayNEDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayNEDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeArrayNEString (node);
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr = new TableExprNodeArrayNERegex (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeArrayNEDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newGT (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtGT);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeGTDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeGTDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeGTString (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeGTDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayGTDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayGTDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeArrayGTString (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeArrayGTDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newGE (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtGE);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeGEDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeGEDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeGEString (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeGEDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayGEDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayGEDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeArrayGEString (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeArrayGEDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}


TableExprNodeRep* TableExprNode::newIN (TableExprNodeRep* right) const
{
    TableExprNodeRep::ValueType vtRight = right->valueType();
    if (vtRight != TableExprNodeRep::VTArray
    &&  vtRight != TableExprNodeRep::VTSet) {
	throw (TableInvExpr ("Right operand of IN has to be an array or set"));
    }
    if (node_p->dataType() != right->dataType()) {
	throwInvDT ("operands for IN-operator");
    }
    TableExprNodeRep::ExprType extype = TableExprNodeRep::Variable;
    if (node_p->isConstant()  &&  right->isConstant()) {
	extype = TableExprNodeRep::Constant;
    }
    TableExprNodeRep node (node_p->dataType(), node_p->valueType(),
			   TableExprNodeRep::OtIN,
			   TableExprNodeRep::NoArr, extype,
			   node_p->ndim(), node_p->shape(),
			   node_p->baseTablePtr());
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeINDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeINDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeINString (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeINDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr = new TableExprNodeArrayINDouble (node);
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr = new TableExprNodeArrayINDComplex (node);
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr = new TableExprNodeArrayINString (node);
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr = new TableExprNodeArrayINDate (node);
	    break;
	default:
	    TableExprNode::throwInvDT();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, False);
}

TableExprNodeRep* TableExprNode::newOR (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtOR);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeOR (node);
	    break;
	default:
	    TableExprNode::throwInvDT ();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeArrayOR (node);
	    break;
	default:
	    TableExprNode::throwInvDT ();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}

TableExprNodeRep* TableExprNode::newAND (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtAND);
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeAND (node);
	    break;
	default:
	    TableExprNode::throwInvDT ();
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeArrayAND (node);
	    break;
	default:
	    TableExprNode::throwInvDT ();
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}


TableExprNode TableExprNode::operator+ () const
    { return *this; }

TableExprNode TableExprNode::operator- () const
{
    if (node_p->dataType() != TableExprNodeRep::NTDouble
    &&  node_p->dataType() != TableExprNodeRep::NTComplex) {
	throwInvDT ();
    }
    TableExprNodeBinary* tsnptr;
    if (node_p->valueType() == TableExprNodeRep::VTScalar) {
	tsnptr = new TableExprNodeMIN (*node_p);
    }else{
	tsnptr = new TableExprNodeArrayMIN (*node_p);
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, 0, True);
}

TableExprNode TableExprNode::operator! () const
{
    if (node_p->dataType() != TableExprNodeRep::NTBool) {
	throwInvDT ();
    }
    TableExprNodeBinary* tsnptr;
    if (node_p->valueType() == TableExprNodeRep::VTScalar) {
	tsnptr = new TableExprNodeNOT (*node_p);
    }else{
	tsnptr = new TableExprNodeArrayNOT (*node_p);
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, 0, True);
}


//# Create a column node on behalf of the Table class.
//# For builtin data types another type of node is created than
//# for other data types.
TableExprNode TableExprNode::newColumnNode (const Table& tab,
					    const BaseTable* tabptr,
					    const String& name,
					    const Vector<String>& fieldNames)
{
    //# Get the column description. This throws an exception if
    //# the name is not a column.
    TableExprNodeRep* tsnptr = 0;
    const ColumnDesc& coldes = tab.tableDesc().columnDesc (name);
    ROTableColumn col(tab, name);
    if (fieldNames.nelements() > 0  &&  coldes.dataType() != TpRecord) {
	throw (TableInvExpr ("Column " + name + " does not contain records, "
			     "so no subfields can be given for it"));
    }
    if (coldes.isArray()) {
	switch(coldes.dataType()) {
	case TpBool:
	    tsnptr = new TableExprNodeArrayColumnBool (col, tabptr);
	    break;
	case TpUChar:
	    tsnptr = new TableExprNodeArrayColumnuChar (col, tabptr);
	    break;
	case TpShort:
	    tsnptr = new TableExprNodeArrayColumnShort(col, tabptr);
	    break;
	case TpUShort:
	    tsnptr = new TableExprNodeArrayColumnuShort (col, tabptr);
	    break;
	case TpInt:
	    tsnptr = new TableExprNodeArrayColumnInt (col, tabptr);
	    break;
	case TpUInt:
	    tsnptr = new TableExprNodeArrayColumnuInt (col, tabptr);
	    break;
	case TpFloat:
	    tsnptr = new TableExprNodeArrayColumnFloat (col, tabptr);
	    break;
	case TpDouble:
	    tsnptr = new TableExprNodeArrayColumnDouble (col, tabptr);
	    break;
	case TpComplex:
	    tsnptr = new TableExprNodeArrayColumnComplex (col, tabptr);
	    break;
	case TpDComplex:
	    tsnptr = new TableExprNodeArrayColumnDComplex (col, tabptr);
	    break;
	case TpString:
	    tsnptr = new TableExprNodeArrayColumnString (col, tabptr);
	    break;
	default:
	    throw (TableInvExpr (name, "unknown data type"));
	}
    } else if (coldes.isScalar()) {
	if (coldes.dataType() == TpRecord  &&  fieldNames.nelements() == 0) {
	    throw (TableInvExpr ("Column " + name + " contains records, "
			     "so subfields have to be given for it"));
	}
	if (coldes.dataType() == TpRecord) {
	    throw (TableInvExpr ("Sorry, column " + name + " contains records, "
				 "which is not supported yet"));
	}
	tsnptr = new TableExprNodeColumn (tab, tabptr, name);
    } else {
	throw (TableInvExpr (name, " must be a Scalar or Array column"));
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
					  const Vector<String>& fieldNames)
{
    TableExprNodeRep* tsnptr = 0;
    const TableRecord* ksPtr = &keyset;
    // All field names, except last one, should be records.
    uInt last = fieldNames.nelements() - 1;
    String keyword;
    Int fieldnr;
    for (uInt i=0; i<=last; i++) {
	if (i > 0) {
	    keyword += '.';
	}
	keyword += fieldNames(i);
	fieldnr = ksPtr->fieldNumber (fieldNames(i));
	if (fieldnr < 0) {
	    throw (TableInvExpr ("Keyword " + keyword + " does not exist"));
	}
	if (i < last) {
	    if (ksPtr->dataType(fieldnr) != TpRecord) {
		throw (TableInvExpr ("Keyword " + keyword + " is no record, "
				     "so no subfields can be given for it"));
	    }
	    ksPtr = &(ksPtr->subRecord(fieldnr));
	}
    }
    const String& name = fieldNames(last);
    switch (ksPtr->dataType (fieldnr)) {
    case TpBool:
	tsnptr = new TableExprNodeConstBool (ksPtr->asBool (name));
	break;
    case TpString:
	tsnptr = new TableExprNodeConstString (ksPtr->asString (name));
	break;
    case TpComplex:
    case TpDComplex:
	tsnptr = new TableExprNodeConstDComplex (ksPtr->asDComplex (name));
	break;
    case TpChar:
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
    case TpFloat:
    case TpDouble:
	tsnptr = new TableExprNodeConstDouble (ksPtr->asDouble (name));
	break;
    case TpArrayBool:
	tsnptr = new TableExprNodeArrayConstBool (ksPtr->asArrayBool (name));
	break;
    case TpArrayString:
	tsnptr = new TableExprNodeArrayConstString
                                               (ksPtr->asArrayString (name));
	break;
    case TpArrayComplex:
	tsnptr = new TableExprNodeArrayConstDComplex
                                               (ksPtr->asArrayComplex (name));
	break;
    case TpArrayDComplex:
	tsnptr = new TableExprNodeArrayConstDComplex
                                               (ksPtr->asArrayDComplex (name));
	break;
    case TpArrayUChar:
	tsnptr = new TableExprNodeArrayConstDouble
                                               (ksPtr->asArrayuChar (name));
	break;
    case TpArrayShort:
	tsnptr = new TableExprNodeArrayConstDouble
                                               (ksPtr->asArrayShort (name));
	break;
    case TpArrayInt:
	tsnptr = new TableExprNodeArrayConstDouble
                                               (ksPtr->asArrayInt (name));
	break;
    case TpArrayUInt:
	tsnptr = new TableExprNodeArrayConstDouble
                                               (ksPtr->asArrayuInt (name));
	break;
    case TpArrayFloat:
	tsnptr = new TableExprNodeArrayConstDouble
                                               (ksPtr->asArrayFloat (name));
	break;
    case TpArrayDouble:
	tsnptr = new TableExprNodeArrayConstDouble
                                               (ksPtr->asArrayDouble (name));
	break;
    case TpRecord:
	throw (TableInvExpr ("Keyword " + keyword + " contains records, "
			     "so subfields have to be given for it"));
	break;
    case TpTable:
	throw (TableInvExpr ("Keyword " + name + " is a table"));
	break;
    default:
	throw (TableInvExpr ("keyword " + keyword + " has unknown data type"));
    }
    return tsnptr;
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& node)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node));
    return newFunctionNode (ftype, set, Table());
}
TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& node1,
			          const TableExprNode& node2)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node1));
    set.add (TableExprNodeSetElem(node2));
    return newFunctionNode (ftype, set, Table());
}
TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& node1,
			          const TableExprNode& node2,
			          const TableExprNode& node3)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node1));
    set.add (TableExprNodeSetElem(node2));
    set.add (TableExprNodeSetElem(node3));
    return newFunctionNode (ftype, set, Table());
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
				  const TableExprNodeSet& set,
				  const Table& table)
{
    // Convert the set to a PtrBlock of the values in the set elements.
    // This requires that the set has single values.
    if (! set.isSingle()) {
	throw (TableInvExpr ("A function parameter cannot be an interval"));
    }
    uInt npar = set.nelements();
    PtrBlock<TableExprNodeRep*> par(npar);
    for (uInt i=0; i<npar; i++) {
	par[i] = (TableExprNodeRep*)(set[i].start());
    }
    // rownrFUNC and randomFUNC are special, because they need their
    // own objects and the table.
    if (ftype == TableExprFuncNode::rownrFUNC) {
	TableExprNodeMulti::checkNumOfArg (0, 0, par);
	return table.nodeRownr (1);           // first rownr is 1 in TaQL
    }                                         // (in C++ first rownr is 0)
    if (ftype == TableExprFuncNode::randFUNC) {
	TableExprNodeMulti::checkNumOfArg (0, 0, par);
	return table.nodeRandom();
    }
    // Check all the operands and get the resulting data type and value type
    // of the function.
    // It also fills the expected data and value type of the operands.
    Block<Int> dtypeOper;
    Block<Int> vtypeOper;
    TableExprNodeRep::ValueType resVT;
    TableExprNodeRep::NodeDataType resDT;
    resDT = TableExprFuncNode::checkOperands (dtypeOper, resVT, vtypeOper,
					      ftype, par);
    // Create new function node and fill it.
    TableExprFuncNode* fnode = new TableExprFuncNode (ftype, resDT,
						      resVT, set); 
    return TableExprFuncNode::fillNode (fnode, par, dtypeOper);
}

TableExprNode TableExprNode::newArrayPartNode (const TableExprNode& arrayNode,
					       const TableExprNodeSet& indices,
					       uInt origin)
{
    // Check if the node is an array.
    if (arrayNode.node_p->valueType() != TableExprNodeRep::VTArray) {
	throw (TableInvExpr ("Indexing can only be done on arrays"));
    }
    // Create new Index node and fill it.
    // Check the index bounds as far as possible.
    TableExprNodeIndex* inode = new TableExprNodeIndex (indices, origin);
    inode->checkIndexValues (arrayNode.node_p);
    TableExprNodeBinary* anode = new TableExprNodeArrayPart (arrayNode.node_p,
							     inode);
    return TableExprNodeBinary::fillNode (anode, arrayNode.node_p,
					  inode, False);
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
    if (node_p->valueType() == TableExprNodeRep::VTScalar) {
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
	    break;
	}
    }
    throwInvDT ("(of expression result)");
    return TpOther;   // just here to get rid of that warning
}
