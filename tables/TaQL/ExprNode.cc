//# ExprNode.cc: Handle class for a table column expression tree
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2003
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

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprMathNode.h>
#include <casacore/tables/TaQL/ExprLogicNode.h>
#include <casacore/tables/TaQL/ExprFuncNode.h>
#include <casacore/tables/TaQL/ExprAggrNode.h>
#include <casacore/tables/TaQL/ExprUDFNode.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/TaQL/ExprMathNodeArray.h>
#include <casacore/tables/TaQL/ExprLogicNodeArray.h>
#include <casacore/tables/TaQL/ExprFuncNodeArray.h>
#include <casacore/tables/TaQL/ExprAggrNodeArray.h>
#include <casacore/tables/TaQL/ExprUDFNodeArray.h>
#include <casacore/tables/TaQL/ExprRange.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNode::TableExprNode() : node_p(0)
{}

//# Constructors for the various constants.
//# These objects are created as temporaries by the compiler.
TableExprNode::TableExprNode (const Bool& val)
{
    node_p = new TableExprNodeConstBool (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Int64& val)
{
    node_p = new TableExprNodeConstInt (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Int& val)
{
    node_p = new TableExprNodeConstInt (val);
    node_p->link();
}
TableExprNode::TableExprNode (const uInt& val)
{
    node_p = new TableExprNodeConstInt (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Float& val)
{
    node_p = new TableExprNodeConstDouble (Double (val));
    node_p->link();
}
TableExprNode::TableExprNode (const Double& val)
{
    node_p = new TableExprNodeConstDouble (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Complex& val)
{
    node_p = new TableExprNodeConstDComplex (DComplex(val));
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
TableExprNode::TableExprNode (const std::string& val)
{
    node_p = new TableExprNodeConstString (String(val));
    node_p->link();
}
TableExprNode::TableExprNode (const char* val)
{
    node_p = new TableExprNodeConstString (String(val));
    node_p->link();
}
TableExprNode::TableExprNode (const Regex& val)
{
    node_p = new TableExprNodeConstRegex (TaqlRegex(val));
    node_p->link();
}
TableExprNode::TableExprNode (const StringDistance& val)
{
    node_p = new TableExprNodeConstRegex (TaqlRegex(val));
    node_p->link();
}
TableExprNode::TableExprNode (const TaqlRegex& val)
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
    node_p = new TableExprNodeArrayConstInt (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Short>& val)
{
    node_p = new TableExprNodeArrayConstInt (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<uShort>& val)
{
    node_p = new TableExprNodeArrayConstInt (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<Int>& val)
{
    node_p = new TableExprNodeArrayConstInt (val);
    node_p->link();
}
TableExprNode::TableExprNode (const Array<uInt>& val)
{
    node_p = new TableExprNodeArrayConstInt (val);
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
: node_p (node.node_p)
{
    if (node_p) {
        node_p->link();
    }
}

TableExprNode& TableExprNode::operator= (const TableExprNode& that)
{
    if (this != &that) {
	TableExprNodeRep::unlink (node_p);
	node_p = that.node_p;
	if (node_p) {
	    node_p->link();
	}
    }
    return *this;
}

//# Destructor.
TableExprNode::~TableExprNode ()
{
    TableExprNodeRep::unlink (node_p);
}

TableExprNode operator&& (const TableExprNode& left,
			  const TableExprNode& right)
{
    if (left.isNull()) return right;
    if (right.isNull()) return left;
    return left.newAND (right.node_p);
}

TableExprNode operator|| (const TableExprNode& left,
			  const TableExprNode& right)
{
    if (left.isNull()) return right;
    if (right.isNull()) return left;
    return left.newOR (right.node_p);
}

TableExprNode TableExprNode::in (const TableExprNodeSet& set,
                                 const TaQLStyle& style) const
{
    // An empty set never matches.
    // Note it makes it possible to use an empty set that has
    // no data type yet.
    if (set.nelements() == 0) {
        return TableExprNode(False);
    }
    set.checkEqualDataTypes();
    TableExprNodeSet setcp = set;
    return newIN (setcp.setOrArray(), style);
}

TableExprNode TableExprNode::useUnit (const Unit& unit) const
{
    if (node_p->dataType() != TableExprNodeRep::NTInt
    &&  node_p->dataType() != TableExprNodeRep::NTDouble
    &&  node_p->dataType() != TableExprNodeRep::NTComplex) {
        throwInvDT("units can only be used with numeric values");
    }
    return TableExprNodeUnit::useUnit (node_p, unit);
}

DataType TableExprNode::getColumnDataType() const
{
    DataType dt;
    if (node_p->getColumnDataType (dt)) {
	return dt;
    }
    return dataType();
}

Bool TableExprNode::checkTableSize (const Table& table, Bool canBeConst) const
{
    // Always correct if no original table.
    if (table.isNull()) {
        return True;
    }
    if (node_p->table().isNull()) {
	return canBeConst;
    }
    return (table.nrow() == node_p->nrow());
}

void TableExprNode::throwInvDT (const String& message)
    { throw (TableInvExpr ("invalid operand data type; " + message)); }


TableExprNodeRep* TableExprNode::newPlus (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtPlus);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodePlusInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodePlusDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodePlusDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodePlusString (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodePlusDate (node));
	    break;
	default:
	    throwInvDT("in scalar operator+");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayPlusInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayPlusDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeArrayPlusDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeArrayPlusString (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeArrayPlusDate (node));
	    break;
	default:
	    throwInvDT("in array operator+");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newMinus (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtMinus);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
          tsnptr.reset (new TableExprNodeMinusInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
          tsnptr.reset (new TableExprNodeMinusDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
          tsnptr.reset (new TableExprNodeMinusDComplex (node));
	    break;
	case TableExprNodeRep::NTDate:
          tsnptr.reset (new TableExprNodeMinusDate (node));
	    break;
	default:
	    throwInvDT("in scalar operator-");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
            tsnptr.reset (new TableExprNodeArrayMinusInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
            tsnptr.reset (new TableExprNodeArrayMinusDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
            tsnptr.reset (new TableExprNodeArrayMinusDComplex (node));
	    break;
	case TableExprNodeRep::NTDate:
            tsnptr.reset (new TableExprNodeArrayMinusDate (node));
	    break;
	default:
	    throwInvDT("in array operator-");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newTimes (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtTimes);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeTimesInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeTimesDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeTimesDComplex (node));
	    break;
	default:
	    throwInvDT("in scalar operator*");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayTimesInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayTimesDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeArrayTimesDComplex (node));
	    break;
	default:
	    throwInvDT("in array operator*");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newDivide (TableExprNodeRep* right) const
{
    // Note that (as in python3) integer division is exact and results in
    // a double.
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtDivide);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeDivideDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeDivideDComplex (node));
	    break;
	default:
	    throwInvDT("in scalar operator/");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayDivideDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
            tsnptr.reset (new TableExprNodeArrayDivideDComplex (node));
	    break;
	default:
	    throwInvDT("in array operator/");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newModulo (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtModulo);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeModuloInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeModuloDouble (node));
	    break;
	default:
	    throwInvDT("no real operands in modulo (%)");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayModuloInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayModuloDouble (node));
	    break;
	default:
	    throwInvDT("no real operands in modulo (%)");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newBitAnd (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtBitAnd);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeBitAndInt (node));
	    break;
	default:
	    throwInvDT("no integer operands in bitand (&)");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayBitAndInt (node));
	    break;
	default:
	    throwInvDT("no integer operands in bitand (&)");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newBitOr (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtBitOr);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeBitOrInt (node));
	    break;
	default:
	    throwInvDT("no integer operands in bitor (|)");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayBitOrInt (node));
	    break;
	default:
	    throwInvDT("no integer operands in bitor (|)");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newBitXor (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtBitXor);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeBitXorInt (node));
	    break;
	default:
	    throwInvDT("no integer operands in bitxor (^)");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayBitXorInt (node));
	    break;
	default:
	    throwInvDT("no integer operands in bitxor (^)");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newEQ (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtEQ);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr.reset (new TableExprNodeEQBool (node));
	    break;
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeEQInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeEQDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeEQDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeEQString (node));
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr.reset (new TableExprNodeEQRegex (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeEQDate (node));
	    break;
	default:
	    throwInvDT("in scalar operator==");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr.reset (new TableExprNodeArrayEQBool (node));
	    break;
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayEQInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayEQDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeArrayEQDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeArrayEQString (node));
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr.reset (new TableExprNodeArrayEQRegex (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeArrayEQDate (node));
	    break;
	default:
	    throwInvDT("in array operator==");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newNE (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtNE);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr.reset (new TableExprNodeNEBool (node));
	    break;
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeNEInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeNEDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeNEDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeNEString (node));
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr.reset (new TableExprNodeNERegex (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeNEDate (node));
	    break;
	default:
	    throwInvDT("in scalar operator<> (!=)");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr.reset (new TableExprNodeArrayNEBool (node));
	    break;
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayNEInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayNEDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeArrayNEDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeArrayNEString (node));
	    break;
	case TableExprNodeRep::NTRegex:
	    tsnptr.reset (new TableExprNodeArrayNERegex (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeArrayNEDate (node));
	    break;
	default:
	    throwInvDT("in array operator<> (!=)");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newGT (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtGT);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeGTInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeGTDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeGTDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeGTString (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeGTDate (node));
	    break;
	default:
	    throwInvDT("in scalar operator>");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
            tsnptr.reset (new TableExprNodeArrayGTInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayGTDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeArrayGTDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeArrayGTString (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeArrayGTDate (node));
	    break;
	default:
	    throwInvDT("in array operator>");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}

TableExprNodeRep* TableExprNode::newGE (TableExprNodeRep* right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getTypes
                                (*node_p, *right, TableExprNodeRep::OtGE);
    SPtrHolder<TableExprNodeBinary> tsnptr;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeGEInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeGEDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeGEDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeGEString (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeGEDate (node));
	    break;
	default:
	    throwInvDT("in scalar operator>=");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr.reset (new TableExprNodeArrayGEInt (node));
	    break;
	case TableExprNodeRep::NTDouble:
	    tsnptr.reset (new TableExprNodeArrayGEDouble (node));
	    break;
	case TableExprNodeRep::NTComplex:
	    tsnptr.reset (new TableExprNodeArrayGEDComplex (node));
	    break;
	case TableExprNodeRep::NTString:
	    tsnptr.reset (new TableExprNodeArrayGEString (node));
	    break;
	case TableExprNodeRep::NTDate:
	    tsnptr.reset (new TableExprNodeArrayGEDate (node));
	    break;
	default:
	    throwInvDT("in array operator>=");
	}
    }
    TableExprNodeRep* nptr = TableExprNodeBinary::fillNode
      (tsnptr.ptr(), node_p, right, True);
    tsnptr.release();
    return nptr;
}


TableExprNodeRep* TableExprNode::newIN (TableExprNodeRep* right,
                                        const TaQLStyle& style) const
{
    // Use EQ if a single value is used (scalar or single set element).
    TableExprNodeRep::ValueType vtRight = right->valueType();
    if (vtRight == TableExprNodeRep::VTScalar) {
      return newEQ (right);
    } else if (vtRight == TableExprNodeRep::VTArray) {
      TableExprNodeSet* set = dynamic_cast<TableExprNodeSet*>(right);
      if (set) {
        if (set->isSingle()  &&  set->nelements() == 1  &&
            ! set->hasArrays()) {
          TableExprNodeRep* snode = (*set)[0].start();
          return newEQ (snode);
        }
      } else {
        TableExprNodeArray* arr = dynamic_cast<TableExprNodeArray*>(right);
        if (arr) {
          TableExprNodeRep* sca = arr->makeConstantScalar();
          if (sca) {
            return newEQ (sca);
          }
        }
      }
    } else if (vtRight != TableExprNodeRep::VTSet) {
      throw (TableInvExpr
             ("Right operand of IN has to be a scalar, array or set"));
    }
    TableExprNodeRep::NodeDataType dtype = node_p->dataType();
    TableExprNodeRep::NodeDataType rdtype = right->dataType();
    if (dtype != rdtype) {
        if ((dtype==TableExprNodeRep::NTInt &&
             rdtype==TableExprNodeRep::NTDouble) ||
            (dtype==TableExprNodeRep::NTDouble &&
             rdtype==TableExprNodeRep::NTInt)) {
          dtype = TableExprNodeRep::NTDouble;
        } else {
          throwInvDT ("mismatching operand types for IN-operator");
        }
    }
    TableExprNodeRep::ExprType extype = TableExprNodeRep::Variable;
    if (node_p->isConstant()  &&  right->isConstant()) {
	extype = TableExprNodeRep::Constant;
    }
    TableExprNodeRep node (dtype, node_p->valueType(),
			   TableExprNodeRep::OtIN,
			   TableExprNodeRep::NoArr, extype,
			   node_p->ndim(), node_p->shape(),
			   node_p->table());
    TableExprNodeBinary* tsnptr = 0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeINInt (node, style.doTracing());
	    break;
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
	    throwInvDT("in scalar IN-operator");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTInt:
	    tsnptr = new TableExprNodeArrayINInt (node);
	    break;
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
	    throwInvDT("in array IN-operator");
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
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
	    throwInvDT("no Bool operands in logical OR (||)");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeArrayOR (node);
	    break;
	default:
	    throwInvDT("no Bool operands in logical OR (||)");
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
	    throwInvDT("no Bool operators in logical AND (&&)");
	}
    }else{
	switch (node.dataType()) {
	case TableExprNodeRep::NTBool:
	    tsnptr = new TableExprNodeArrayAND (node);
	    break;
	default:
	    throwInvDT("no Bool operators in logical AND (&&)");
	}
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, right, True);
}


TableExprNode TableExprNode::operator+ () const
    { return *this; }

TableExprNode TableExprNode::operator- () const
{
    if (node_p->dataType() != TableExprNodeRep::NTInt
    &&  node_p->dataType() != TableExprNodeRep::NTDouble
    &&  node_p->dataType() != TableExprNodeRep::NTComplex) {
	throwInvDT("no numeric operand in unary -");
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
	throwInvDT("no numeric operand in unary NOT (!)");
    }
    TableExprNodeBinary* tsnptr;
    if (node_p->valueType() == TableExprNodeRep::VTScalar) {
	tsnptr = new TableExprNodeNOT (*node_p);
    }else{
	tsnptr = new TableExprNodeArrayNOT (*node_p);
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, 0, True);
}

TableExprNode TableExprNode::operator~ () const
{
    if (node_p->dataType() != TableExprNodeRep::NTInt) {
	throwInvDT ("no integer operand in unary bitnegate (~)");
    }
    TableExprNodeBinary* tsnptr;
    if (node_p->valueType() == TableExprNodeRep::VTScalar) {
	tsnptr = new TableExprNodeBitNegate (*node_p);
    }else{
	tsnptr = new TableExprNodeArrayBitNegate (*node_p);
    }
    return TableExprNodeBinary::fillNode (tsnptr, node_p, 0, True);
}


//# Create a column node on behalf of the Table class.
//# For builtin data types another type of node is created than
//# for other data types.
TableExprNode TableExprNode::newColumnNode (const Table& table,
					    const String& name,
					    const Vector<String>& fieldNames)
{
    //# Get the column description. This throws an exception if
    //# the name is not a column.
    TableExprNodeRep* tsnptr = 0;
    const ColumnDesc& coldes = table.tableDesc().columnDesc (name);
    TableColumn col(table, name);
    if (fieldNames.nelements() > 0  &&  coldes.dataType() != TpRecord) {
	throw (TableInvExpr ("Column " + name + " does not contain records, "
			     "so no subfields can be given for it"));
    }
    if (coldes.isArray()) {
	switch(coldes.dataType()) {
	case TpBool:
	    tsnptr = new TableExprNodeArrayColumnBool (col, table);
	    break;
	case TpUChar:
	    tsnptr = new TableExprNodeArrayColumnuChar (col, table);
	    break;
	case TpShort:
	    tsnptr = new TableExprNodeArrayColumnShort(col, table);
	    break;
	case TpUShort:
	    tsnptr = new TableExprNodeArrayColumnuShort (col, table);
	    break;
	case TpInt:
	    tsnptr = new TableExprNodeArrayColumnInt (col, table);
	    break;
	case TpUInt:
	    tsnptr = new TableExprNodeArrayColumnuInt (col, table);
	    break;
	case TpFloat:
	    tsnptr = new TableExprNodeArrayColumnFloat (col, table);
	    break;
	case TpDouble:
	    tsnptr = new TableExprNodeArrayColumnDouble (col, table);
	    break;
	case TpComplex:
	    tsnptr = new TableExprNodeArrayColumnComplex (col, table);
	    break;
	case TpDComplex:
	    tsnptr = new TableExprNodeArrayColumnDComplex (col, table);
	    break;
	case TpString:
	    tsnptr = new TableExprNodeArrayColumnString (col, table);
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
	tsnptr = new TableExprNodeColumn (table, name);
    } else {
	throw (TableInvExpr (name, " must be a Scalar or Array column"));
    }
    return tsnptr;
}


//# Create a constant node for a keyword on behalf of the Table class.
//# The constructor reads in the value and stores it as a constant.
TableExprNode TableExprNode::newKeyConst (const TableRecord& keyset,
					  const Vector<String>& fieldNames)
{
    TableExprNodeRep* tsnptr = 0;
    const TableRecord* ksPtr = &keyset;
    // All field names, except last one, should be records.
    uInt last = fieldNames.nelements() - 1;
    String keyword;
    Int fieldnr = 0;
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
    case TpFloat:
    case TpDouble:
	tsnptr = new TableExprNodeConstDouble (ksPtr->asDouble (name));
	break;
    case TpChar:
    case TpShort:
    case TpInt:
	tsnptr = new TableExprNodeConstInt (ksPtr->asInt (name));
	break;
    case TpUChar:
    case TpUShort:
    case TpUInt:
	tsnptr = new TableExprNodeConstInt (ksPtr->asuInt (name));
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
	tsnptr = new TableExprNodeArrayConstInt
                                               (ksPtr->asArrayuChar (name));
	break;
    case TpArrayShort:
	tsnptr = new TableExprNodeArrayConstInt
                                               (ksPtr->asArrayShort (name));
	break;
    case TpArrayInt:
	tsnptr = new TableExprNodeArrayConstInt
                                               (ksPtr->asArrayInt (name));
	break;
    case TpArrayUInt:
	tsnptr = new TableExprNodeArrayConstInt
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
			          const TableExprNode& array,
			          const TableExprNodeSet& axes)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(array));
    // Turn the axes set into an array.
    set.add (TableExprNodeSetElem(axes.setOrArray()));
    return newFunctionNode (ftype, set, Table());
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& array,
			          const TableExprNode& node,
			          const TableExprNodeSet& axes)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(array));
    set.add (TableExprNodeSetElem(node));
    // Turn the axes set into an array.
    set.add (TableExprNodeSetElem(axes.setOrArray()));
    return newFunctionNode (ftype, set, Table());
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
				  const TableExprNodeSet& set,
				  const Table& table,
				  const TaQLStyle& style)
{
    // Convert the set to a PtrBlock of the values in the set elements.
    // This requires that the set has single values.
    if (! set.isSingle()) {
	throw (TableInvExpr ("A function parameter cannot be an interval"));
    }
    uInt npar = set.nelements();
    PtrBlock<TableExprNodeRep*> par(npar);
    for (uInt i=0; i<npar; i++) {
	par[i] = const_cast<TableExprNodeRep*>(set[i].start());
    }
    // rownrFUNC, rowidFUNC and randomFUNC are special, because they
    // need their own objects and the table.
    if (ftype == TableExprFuncNode::rownrFUNC) {
	TableExprNodeMulti::checkNumOfArg (0, 0, par);
	return newRownrNode (table, style.origin());  // first rownr is 0 or 1
    }
    if (ftype == TableExprFuncNode::rowidFUNC) {
	TableExprNodeMulti::checkNumOfArg (0, 0, par);
	return newRowidNode (table);
    }
    if (ftype == TableExprFuncNode::randFUNC) {
	TableExprNodeMulti::checkNumOfArg (0, 0, par);
	return newRandomNode (table);
    }
    // Check all the operands and get the resulting data type and value type
    // of the function.
    // It also fills the expected data and value type of the operands.
    TableExprNodeRep::ValueType resVT;
    TableExprNodeRep::NodeDataType resDT;
    Block<Int> dtypeOper;
    Block<Int> vtypeOper;
    if (ftype >= TableExprFuncNode::FirstAggrFunc) {
      resDT = TableExprAggrNode::checkOperands (dtypeOper, resVT, ftype, par);
      // Create new function node and fill it.
      if (resVT == TableExprNodeRep::VTScalar) {
        TableExprFuncNode* fnode = new TableExprAggrNode (ftype, resDT,
                                                          resVT, set);
	return TableExprFuncNode::fillNode (fnode, par, dtypeOper);
      }
      TableExprFuncNodeArray* fnode = new TableExprAggrNodeArray
        (ftype, resDT, resVT, set, style);
      return TableExprFuncNodeArray::fillNode (fnode, par, dtypeOper);
    }
    resDT = TableExprFuncNode::checkOperands (dtypeOper, resVT, vtypeOper,
					      ftype, par);
    // Create new function node and fill it.
    // Keep it temporarily in SPtrHolder for destruction on exception.
    TableExprNode res;
    if (resVT == TableExprNodeRep::VTScalar) {
        TableExprFuncNode* fnode = new TableExprFuncNode (ftype, resDT,
							  resVT, set);
        SPtrHolder<TableExprFuncNode> fnodeHold(fnode);
	res = TableExprFuncNode::fillNode (fnode, par, dtypeOper);
        fnodeHold.release();
    } else {
        TableExprFuncNodeArray* fnode = new TableExprFuncNodeArray
          (ftype, resDT, resVT, set, style);
        SPtrHolder<TableExprFuncNodeArray> fnodeHold(fnode);
        res = TableExprFuncNodeArray::fillNode (fnode, par, dtypeOper);
        fnodeHold.release();
    }
    return res;
}

TableExprNode TableExprNode::newUDFNode (const String& name,
                                         const TableExprNodeSet& set,
                                         const Table& table,
                                         const TaQLStyle& style)
{
    // Create the correct UDF object. An exception is thrown if unknown.
    SPtrHolder<UDFBase> udf(UDFBase::createUDF (name, style));
    // Convert the set to a PtrBlock of the values in the set elements.
    // This requires that the set has single values.
    if (! set.isSingle()) {
	throw (TableInvExpr ("A function parameter cannot be an interval"));
    }
    uInt npar = set.nelements();
    PtrBlock<TableExprNodeRep*> par(npar);
    for (uInt i=0; i<npar; i++) {
        par[i] = const_cast<TableExprNodeRep*>(set[i].start());
    }
    udf->init (par, table, style);
    if (udf->ndim() == 0) {
        return new TableExprUDFNode (udf.transfer(), table, set);
    }
    return new TableExprUDFNodeArray (udf.transfer(), table, set);
}

TableExprNode TableExprNode::newConeNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& node1,
			          const TableExprNode& node2)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node1));
    set.add (TableExprNodeSetElem(node2));
    return newConeNode (ftype, set);
}
TableExprNode TableExprNode::newConeNode
                                 (TableExprFuncNode::FunctionType ftype,
			          const TableExprNode& node1,
			          const TableExprNode& node2,
			          const TableExprNode& node3)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node1));
    set.add (TableExprNodeSetElem(node2));
    set.add (TableExprNodeSetElem(node3));
    return newConeNode (ftype, set);
}
TableExprNode TableExprNode::newConeNode
                                 (TableExprFuncNode::FunctionType ftype,
				  const TableExprNodeSet& set,
				  uInt origin)
{
    // Convert the set to a PtrBlock of the values in the set elements.
    // This requires that the set has single values.
    if (! set.isSingle()) {
	throw (TableInvExpr ("A function parameter cannot be an interval"));
    }
    uInt npar = set.nelements();
    PtrBlock<TableExprNodeRep*> par(npar);
    for (uInt i=0; i<npar; i++) {
	par[i] = const_cast<TableExprNodeRep*>(set[i].start());
    }
    // Check all the operands and get the resulting data type and value type
    // of the function.
    // It also fills the expected data and value type of the operands.
    Block<Int> dtypeOper;
    Block<Int> vtypeOper;
    TableExprNodeRep::ValueType resVT;
    TableExprNodeRep::NodeDataType resDT;
    resDT = TableExprConeNode::checkOperands (dtypeOper, resVT, vtypeOper,
					      ftype, par);
    // Create new function node and fill it.
    if (resVT == TableExprNodeRep::VTScalar) {
      TableExprConeNode* fnode = new TableExprConeNode (ftype, resDT,
                                                        set, origin);
      return TableExprConeNode::fillNode (fnode, par, dtypeOper);
    }
    TableExprConeNodeArray* fnode = new TableExprConeNodeArray (ftype, resDT,
                                                                set, origin);
    return TableExprConeNodeArray::fillNode (fnode, par, dtypeOper);
}

TableExprNode TableExprNode::newArrayPartNode (const TableExprNode& arrayNode,
					       const TableExprNodeSet& indices,
					       const TaQLStyle& style)
{
    // Check if the node is an array.
    if (arrayNode.node_p->valueType() != TableExprNodeRep::VTArray) {
	throw (TableInvExpr ("Indexing can only be done on arrays"));
    }
    // Create new Index node and fill it.
    // Check the index bounds as far as possible.
    SPtrHolder<TableExprNodeIndex>
                     inodep (new TableExprNodeIndex (indices, style));
    inodep->checkIndexValues (arrayNode.node_p);
    TableExprNodeIndex* inode = inodep.transfer();
    TableExprNodeBinary* anode = new TableExprNodeArrayPart (arrayNode.node_p,
							     inode);
    return TableExprNodeBinary::fillNode (anode, arrayNode.node_p,
					  inode, False, False);
}

void TableExprNode::adaptUnit (const Unit& unit)
{
    TableExprNodeUnit::adaptUnit (node_p, unit);
}

TableExprNode TableExprNode::newRownrNode (const Table& table,
					   uInt origin)
{
    TableExprNodeRep* tsnptr = new TableExprNodeRownr (table, origin);
    return tsnptr;
}

TableExprNode TableExprNode::newRowidNode (const Table& table)
{
    TableExprNodeRep* tsnptr = new TableExprNodeRowid (table);
    return tsnptr;
}

TableExprNode TableExprNode::newRandomNode (const Table& table)
{
    TableExprNodeRep* tsnptr = new TableExprNodeRandom (table);
    return tsnptr;
}

DataType TableExprNode::dataType() const
{
    if (node_p->valueType() == TableExprNodeRep::VTScalar
    ||  node_p->valueType() == TableExprNodeRep::VTArray) {
	switch(node_p->dataType()) {
	case TableExprNodeRep::NTBool:
	    return TpBool;
	case TableExprNodeRep::NTInt:
	    return TpInt;
	case TableExprNodeRep::NTDouble:
	    return TpDouble;
	case TableExprNodeRep::NTComplex:
	    return TpDComplex;
	case TableExprNodeRep::NTString:
	    return TpString;
	default:
            return TpOther;
	}
    }
    return TpOther;
}

} //# NAMESPACE CASACORE - END

