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

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprMathNode.h>
#include <casacore/tables/TaQL/ExprLogicNode.h>
#include <casacore/tables/TaQL/ExprConeNode.h>
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
#include <casacore/tables/TaQL/ExprNodeArray.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNode::TableExprNode() : node_p(0)
{}

//# Constructors for the various constants.
//# These objects are created as temporaries by the compiler.
TableExprNode::TableExprNode (const Bool& val)
{
    node_p = std::make_shared<TableExprNodeConstBool>(val);
}
TableExprNode::TableExprNode (const Int& val)
{
    node_p = std::make_shared<TableExprNodeConstInt>(val);
}
TableExprNode::TableExprNode (const uInt& val)
{
    node_p = std::make_shared<TableExprNodeConstInt>(val);
}
TableExprNode::TableExprNode (const Int64& val)
{
    node_p = std::make_shared<TableExprNodeConstInt>(val);
}
TableExprNode::TableExprNode (const uInt64& val)
{
    node_p = std::make_shared<TableExprNodeConstInt>(val);
}
TableExprNode::TableExprNode (const Float& val)
{
    node_p = std::make_shared<TableExprNodeConstDouble>(Double(val));
}
TableExprNode::TableExprNode (const Double& val)
{
    node_p = std::make_shared<TableExprNodeConstDouble>(val);
}
TableExprNode::TableExprNode (const Complex& val)
{
    node_p = std::make_shared<TableExprNodeConstDComplex>(DComplex(val));
}
TableExprNode::TableExprNode (const DComplex& val)
{
    node_p = std::make_shared<TableExprNodeConstDComplex>(val);
}
TableExprNode::TableExprNode (const String& val)
{
    node_p = std::make_shared<TableExprNodeConstString>(val);
}
TableExprNode::TableExprNode (const std::string& val)
{
    node_p = std::make_shared<TableExprNodeConstString>(String(val));
}
TableExprNode::TableExprNode (const char* val)
{
    node_p = std::make_shared<TableExprNodeConstString>(String(val));
}
TableExprNode::TableExprNode (const Regex& val)
{
    node_p = std::make_shared<TableExprNodeConstRegex>(TaqlRegex(val));
}
TableExprNode::TableExprNode (const StringDistance& val)
{
    node_p = std::make_shared<TableExprNodeConstRegex>(TaqlRegex(val));
}
TableExprNode::TableExprNode (const TaqlRegex& val)
{
    node_p = std::make_shared<TableExprNodeConstRegex>(val);
}
TableExprNode::TableExprNode (const MVTime& val)
{
    node_p = std::make_shared<TableExprNodeConstDate>(val);
}
TableExprNode::TableExprNode (const Array<Bool>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstBool>(val);
}
TableExprNode::TableExprNode (const Array<uChar>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const Array<Short>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const Array<uShort>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const Array<Int>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const Array<uInt>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const Array<Int64>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const Array<uInt64>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const Array<Float>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDouble>(val);
}
TableExprNode::TableExprNode (const Array<Double>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDouble>(val);
}
TableExprNode::TableExprNode (const Array<Complex>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDComplex>(val);
}
TableExprNode::TableExprNode (const Array<DComplex>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDComplex>(val);
}
TableExprNode::TableExprNode (const Array<String>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstString>(val);
}
TableExprNode::TableExprNode (const Array<MVTime>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDate>(val);
}

TableExprNode::TableExprNode (const MArray<Bool>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstBool>(val);
}
TableExprNode::TableExprNode (const MArray<uChar>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const MArray<Short>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const MArray<uShort>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const MArray<Int>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const MArray<uInt>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const MArray<Int64>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const MArray<uInt64>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstInt>(val);
}
TableExprNode::TableExprNode (const MArray<Float>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDouble>(val);
}
TableExprNode::TableExprNode (const MArray<Double>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDouble>(val);
}
TableExprNode::TableExprNode (const MArray<Complex>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDComplex>(val);
}
TableExprNode::TableExprNode (const MArray<DComplex>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDComplex>(val);
}
TableExprNode::TableExprNode (const MArray<String>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstString>(val);
}
TableExprNode::TableExprNode (const MArray<MVTime>& val)
{
    node_p = std::make_shared<TableExprNodeArrayConstDate>(val);
}

TableExprNode::TableExprNode (const TENShPtr& node)
: node_p (node)
{}

TableExprNode::TableExprNode (const TableExprNode& node)
: node_p (node.node_p)
{}

TableExprNode& TableExprNode::operator= (const TableExprNode& that)
{
    node_p = that.node_p;
    return *this;
}

//# Destructor.
TableExprNode::~TableExprNode ()
{}

TableExprNode operator&& (const TableExprNode& left,
                          const TableExprNode& right)
{
    if (left.isNull()) return right;
    if (right.isNull()) return left;
    return left.newAND (right.getRep());
}

TableExprNode operator|| (const TableExprNode& left,
                          const TableExprNode& right)
{
    if (left.isNull()) return right;
    if (right.isNull()) return left;
    return left.newOR (right.getRep());
}

TableExprNode TableExprNode::in (const TableExprNodeSet& set,
                                 const TaQLStyle& style) const
{
    // An empty set never matches.
    // Note it makes it possible to use an empty set that has
    // no data type yet.
    if (set.size() == 0) {
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
    &&  node_p->dataType() != TableExprNodeRep::NTComplex
    &&  node_p->dataType() != TableExprNodeRep::NTDate) {
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
    std::vector<Table> tables(TableExprNodeUtil::getNodeTables (node_p.get(), True));
    if (tables.empty()) {
        return canBeConst;
    }
    return (table.nrow() == TableExprNodeUtil::getCheckNRow (tables));
}

void TableExprNode::throwInvDT (const String& message)
    { throw (TableInvExpr ("invalid operand data type; " + message)); }


TENShPtr TableExprNode::setBinaryNodeInfo (TableExprNodeBinary* tsnptr,
                                           const TENShPtr& right) const
{
  TENShPtr shPtr(tsnptr);
  tsnptr->setChildren (node_p, right);
  return TableExprNodeRep::replaceConstNode (shPtr);
}

TENShPtr TableExprNode::newPlus (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtPlus);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodePlusInt (node);
            break;
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
            throwInvDT("in scalar operator+");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayPlusInt (node);
            break;
        case TableExprNodeRep::NTDouble:
            tsnptr = new TableExprNodeArrayPlusDouble (node);
            break;
        case TableExprNodeRep::NTComplex:
            tsnptr = new TableExprNodeArrayPlusDComplex (node);
            break;
        case TableExprNodeRep::NTString:
            tsnptr = new TableExprNodeArrayPlusString (node);
            break;
        case TableExprNodeRep::NTDate:
            tsnptr = new TableExprNodeArrayPlusDate (node);
            break;
        default:
            throwInvDT("in array operator+");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newMinus (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtMinus);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
          tsnptr = new TableExprNodeMinusInt (node);
            break;
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
            throwInvDT("in scalar operator-");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayMinusInt (node);
            break;
        case TableExprNodeRep::NTDouble:
            tsnptr = new TableExprNodeArrayMinusDouble (node);
            break;
        case TableExprNodeRep::NTComplex:
            tsnptr = new TableExprNodeArrayMinusDComplex (node);
            break;
        case TableExprNodeRep::NTDate:
            tsnptr = new TableExprNodeArrayMinusDate (node);
            break;
        default:
            throwInvDT("in array operator-");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newTimes (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtTimes);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeTimesInt (node);
            break;
        case TableExprNodeRep::NTDouble:
            tsnptr = new TableExprNodeTimesDouble (node);
            break;
        case TableExprNodeRep::NTComplex:
            tsnptr = new TableExprNodeTimesDComplex (node);
            break;
        default:
            throwInvDT("in scalar operator*");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayTimesInt (node);
            break;
        case TableExprNodeRep::NTDouble:
            tsnptr = new TableExprNodeArrayTimesDouble (node);
            break;
        case TableExprNodeRep::NTComplex:
            tsnptr = new TableExprNodeArrayTimesDComplex (node);
            break;
        default:
            throwInvDT("in array operator*");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newDivide (const TENShPtr& right) const
{
    // Note that (as in python3) integer division is exact and results in
    // a double.
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtDivide);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTDouble:
            tsnptr = new TableExprNodeDivideDouble (node);
            break;
        case TableExprNodeRep::NTComplex:
            tsnptr = new TableExprNodeDivideDComplex (node);
            break;
        default:
            throwInvDT("in scalar operator/");
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
            throwInvDT("in array operator/");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newModulo (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtModulo);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeModuloInt (node);
            break;
        case TableExprNodeRep::NTDouble:
            tsnptr = new TableExprNodeModuloDouble (node);
            break;
        default:
            throwInvDT("no real operands in modulo (%)");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayModuloInt (node);
            break;
        case TableExprNodeRep::NTDouble:
            tsnptr = new TableExprNodeArrayModuloDouble (node);
            break;
        default:
            throwInvDT("no real operands in modulo (%)");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newBitAnd (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtBitAnd);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeBitAndInt (node);
            break;
        default:
            throwInvDT("no integer operands in bitand (&)");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayBitAndInt (node);
            break;
        default:
            throwInvDT("no integer operands in bitand (&)");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newBitOr (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtBitOr);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeBitOrInt (node);
            break;
        default:
            throwInvDT("no integer operands in bitor (|)");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayBitOrInt (node);
            break;
        default:
            throwInvDT("no integer operands in bitor (|)");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newBitXor (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtBitXor);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeBitXorInt (node);
            break;
        default:
            throwInvDT("no integer operands in bitxor (^)");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayBitXorInt (node);
            break;
        default:
            throwInvDT("no integer operands in bitxor (^)");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newEQ (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtEQ);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTBool:
            tsnptr = new TableExprNodeEQBool (node);
            break;
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeEQInt (node);
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
            throwInvDT("in scalar operator==");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTBool:
            tsnptr = new TableExprNodeArrayEQBool (node);
            break;
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayEQInt (node);
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
            throwInvDT("in array operator==");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newNE (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtNE);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTBool:
            tsnptr = new TableExprNodeNEBool (node);
            break;
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeNEInt (node);
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
            throwInvDT("in scalar operator<> (!=)");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTBool:
            tsnptr = new TableExprNodeArrayNEBool (node);
            break;
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayNEInt (node);
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
            throwInvDT("in array operator<> (!=)");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newGT (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtGT);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeGTInt (node);
            break;
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
            throwInvDT("in scalar operator>");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayGTInt (node);
            break;
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
            throwInvDT("in array operator>");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newGE (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtGE);
    TableExprNodeBinary* tsnptr=0;
    if (node.valueType() == TableExprNodeRep::VTScalar) {
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeGEInt (node);
            break;
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
            throwInvDT("in scalar operator>=");
        }
    }else{
        switch (node.dataType()) {
        case TableExprNodeRep::NTInt:
            tsnptr = new TableExprNodeArrayGEInt (node);
            break;
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
            throwInvDT("in array operator>=");
        }
    }
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newIN (const TENShPtr& right,
                               const TaQLStyle& style) const
{
    // Is the right operand a scalar or an array with a single element?
    // If so, the IN operator can be replaced by the EQ operator.
    // Note that an array can also be represented by a set with single values.
    TableExprNodeRep::ValueType vtRight = right->valueType();
    if (vtRight == TableExprNodeRep::VTScalar) {
      return newEQ (right);
    } else if (vtRight == TableExprNodeRep::VTArray) {
      TableExprNodeSet* set =
        dynamic_cast<TableExprNodeSet*>(right.get());
      if (set) {
        if (set->isSingle()  &&  set->size() == 1  &&
            ! set->hasArrays()) {
          TENShPtr snode = (*set)[0]->start();
          return newEQ (snode);
        }
      } else {
        TableExprNodeArray* arr =
          dynamic_cast<TableExprNodeArray*>(right.get());
        if (arr) {
          TENShPtr sca = arr->makeConstantScalar();
          if (sca) {
            return newEQ (sca);
          }
        }
      }
    } else if (vtRight != TableExprNodeRep::VTSet) {
      throw (TableInvExpr
             ("Right operand of IN has to be a scalar, array or set"));
    }
    // A mix of Int and Double operands means Double.
    // Otherwise the operands should have equal data types.
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
    // If both operands are constant, the result is constant as well.
    TableExprNodeRep::ExprType extype = TableExprNodeRep::Variable;
    if (right->isConstant()  &&  node_p->isConstant()) {
        extype = TableExprNodeRep::Constant;
    }
    TableExprNodeRep node (dtype, node_p->valueType(),
                           TableExprNodeRep::OtIN,
                           TableExprNodeRep::NoArr, extype,
                           node_p->ndim(), node_p->shape());
    // Create the correct IN object depending on data type
    // and if the left hand operand is scalar or array.
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
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newOR (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtOR);
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
    return setBinaryNodeInfo (tsnptr, right);
}

TENShPtr TableExprNode::newAND (const TENShPtr& right) const
{
    TableExprNodeRep node = TableExprNodeBinary::getCommonTypes
                                (node_p, right, TableExprNodeRep::OtAND);
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
    return setBinaryNodeInfo (tsnptr, right);
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
    return setBinaryNodeInfo (tsnptr);
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
    return setBinaryNodeInfo (tsnptr);
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
    return setBinaryNodeInfo (tsnptr);
}


//# Create an expression node for either a keyword or column.
TableExprNode TableExprNode::keyCol (const TableExprInfo& tabInfo,
                                     const String& name,
                                     const Vector<String>& fieldNames)
{
    if (tabInfo.table().tableDesc().isColumn (name)) {
        return newColumnNode (tabInfo, name, fieldNames);
    } else {
	uInt nr = fieldNames.nelements();
	Vector<String> names (nr + 1);
	names (Slice(1,nr)) = fieldNames;
	names(0) = name;
	return newKeyConst (tabInfo.table().keywordSet(), names);
    }
}

//# Create a column node on behalf of the Table class.
//# For builtin data types another type of node is created than
//# for other data types.
TableExprNode TableExprNode::newColumnNode (const TableExprInfo& tableInfo,
                                            const String& name,
                                            const Vector<String>& fieldNames)
{
    //# Get the column description. This throws an exception if
    //# the name is not a column.
    const Table& table = tableInfo.table();
    TENShPtr tsnptr;
    const ColumnDesc& coldes = table.tableDesc().columnDesc (name);
    TableColumn col(table, name);
    if (fieldNames.size() > 0  &&  coldes.dataType() != TpRecord) {
        throw (TableInvExpr ("Column " + name + " does not contain records, "
                             "so no subfields can be given for it"));
    }
    if (coldes.isArray()) {
        switch(coldes.dataType()) {
        case TpBool:
            tsnptr = std::make_shared<TableExprNodeArrayColumnBool>(col, tableInfo);
            break;
        case TpUChar:
            tsnptr = std::make_shared<TableExprNodeArrayColumnuChar>(col, tableInfo);
            break;
        case TpShort:
            tsnptr = std::make_shared<TableExprNodeArrayColumnShort>(col, tableInfo);
            break;
        case TpUShort:
            tsnptr = std::make_shared<TableExprNodeArrayColumnuShort>(col, tableInfo);
            break;
        case TpInt:
            tsnptr = std::make_shared<TableExprNodeArrayColumnInt>(col, tableInfo);
            break;
        case TpUInt:
            tsnptr = std::make_shared<TableExprNodeArrayColumnuInt>(col, tableInfo);
            break;
        case TpInt64:
            tsnptr = std::make_shared<TableExprNodeArrayColumnInt64>(col, tableInfo);
            break;
        case TpFloat:
            tsnptr = std::make_shared<TableExprNodeArrayColumnFloat>(col, tableInfo);
            break;
        case TpDouble:
            tsnptr = std::make_shared<TableExprNodeArrayColumnDouble>(col, tableInfo);
            break;
        case TpComplex:
            tsnptr = std::make_shared<TableExprNodeArrayColumnComplex>(col, tableInfo);
            break;
        case TpDComplex:
            tsnptr = std::make_shared<TableExprNodeArrayColumnDComplex>(col, tableInfo);
            break;
        case TpString:
            tsnptr = std::make_shared<TableExprNodeArrayColumnString>(col, tableInfo);
            break;
        default:
            throw (TableInvExpr (name, "unknown data type"));
        }
    } else if (coldes.isScalar()) {
        if (coldes.dataType() == TpRecord  &&  fieldNames.size() == 0) {
            throw (TableInvExpr ("Column " + name + " contains records, "
                             "so subfields have to be given for it"));
        }
        if (coldes.dataType() == TpRecord) {
            throw (TableInvExpr ("Sorry, column " + name + " contains records, "
                                 "which is not supported yet"));
        }
        tsnptr = std::make_shared<TableExprNodeColumn>(tableInfo, name);
    } else {
        throw (TableInvExpr (name, " must be a Scalar or Array column"));
    }
    return tsnptr;
}

// Find the last TableRecord of the field names of a keyword.
TableRecord* TableExprNode::findLastKeyRec (const TableRecord& keyset,
                                            const Vector<String>& fieldNames,
                                            String& fullName)
{
    const TableRecord* ksPtr = &keyset;
    // All field names, except last one, should be records.
    uInt last = fieldNames.size() - 1;
    fullName.clear();
    Int fieldnr = 0;
    for (uInt i=0; i<last; i++) {
        if (i > 0) {
            fullName += '.';
        }
        fullName += fieldNames(i);
        fieldnr = ksPtr->fieldNumber (fieldNames(i));
        if (fieldnr < 0) {
            throw (TableInvExpr ("Keyword " + fullName + " does not exist"));
        }
        if (ksPtr->dataType(fieldnr) != TpRecord) {
          throw (TableInvExpr ("Keyword " + fullName + " is no record, "
                               "so no subfields can be given for it"));
        }
        ksPtr = &(ksPtr->subRecord(fieldnr));
    }
    return const_cast<TableRecord*>(ksPtr);
}

//# Create a constant node for a keyword on behalf of the Table class.
//# The constructor reads in the value and stores it as a constant.
TableExprNode TableExprNode::newKeyConst (const TableRecord& keyset,
                                          const Vector<String>& fieldNames)
{
    TENShPtr tsnptr;
    String fullName;
    const TableRecord* ks = findLastKeyRec (keyset, fieldNames, fullName);
    String name = fieldNames[fieldNames.size() - 1];
    fullName += '.' + name;
    Int fieldnr = ks->fieldNumber (name);
    if (fieldnr < 0) {
      throw (TableInvExpr ("Keyword " + fullName + " does not exist"));
    }
    switch (ks->dataType (fieldnr)) {
    case TpBool:
        tsnptr = std::make_shared<TableExprNodeConstBool>(ks->asBool (name));
        break;
    case TpString:
        tsnptr = std::make_shared<TableExprNodeConstString>(ks->asString (name));
        break;
    case TpComplex:
    case TpDComplex:
        tsnptr = std::make_shared<TableExprNodeConstDComplex>(ks->asDComplex (name));
        break;
    case TpFloat:
    case TpDouble:
        tsnptr = std::make_shared<TableExprNodeConstDouble>(ks->asDouble (name));
        break;
    case TpChar:
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
    case TpInt64:
        tsnptr = std::make_shared<TableExprNodeConstInt>(ks->asInt64 (name));
        break;
    case TpArrayBool:
        tsnptr = std::make_shared<TableExprNodeArrayConstBool>(ks->asArrayBool (name));
        break;
    case TpArrayString:
        tsnptr = std::make_shared<TableExprNodeArrayConstString>
                                               (ks->asArrayString (name));
        break;
    case TpArrayComplex:
        tsnptr = std::make_shared<TableExprNodeArrayConstDComplex>
                                               (ks->asArrayComplex (name));
        break;
    case TpArrayDComplex:
        tsnptr = std::make_shared<TableExprNodeArrayConstDComplex>
                                               (ks->asArrayDComplex (name));
        break;
    case TpArrayUChar:
        tsnptr = std::make_shared<TableExprNodeArrayConstInt>
                                               (ks->asArrayuChar (name));
        break;
    case TpArrayShort:
        tsnptr = std::make_shared<TableExprNodeArrayConstInt>
                                               (ks->asArrayShort (name));
        break;
    case TpArrayInt:
        tsnptr = std::make_shared<TableExprNodeArrayConstInt>
                                               (ks->asArrayInt (name));
        break;
    case TpArrayUInt:
        tsnptr = std::make_shared<TableExprNodeArrayConstInt>
                                               (ks->asArrayuInt (name));
        break;
    case TpArrayInt64:
        tsnptr = std::make_shared<TableExprNodeArrayConstInt>
                                               (ks->asArrayInt64 (name));
        break;
    case TpArrayFloat:
        tsnptr = std::make_shared<TableExprNodeArrayConstDouble>
                                               (ks->asArrayFloat (name));
        break;
    case TpArrayDouble:
        tsnptr = std::make_shared<TableExprNodeArrayConstDouble>
                                               (ks->asArrayDouble (name));
        break;
    case TpRecord:
        throw (TableInvExpr ("Keyword " + fullName + " contains records, "
                             "so subfields have to be given for it"));
        break;
    case TpTable:
        throw (TableInvExpr ("Keyword " + name + " is a table"));
        break;
    default:
        throw (TableInvExpr ("keyword " + fullName +
                             " has unknown data type"));
    }
    return tsnptr;
}

TableExprNode diagonal (const TableExprNode& array,
                        const TableExprNode& firstAxis)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(firstAxis));
    return TableExprNode::newFunctionNode (TableExprFuncNode::diagonalFUNC,
                                           array, set);
}
TableExprNode diagonal (const TableExprNode& array,
                        const TableExprNode& firstAxis,
                        const TableExprNode& diag)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(firstAxis));
    set.add (TableExprNodeSetElem(diag));
    return TableExprNode::newFunctionNode (TableExprFuncNode::diagonalFUNC,
                                           array, set);
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
                                  const TableExprNode& node)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node));
    return newFunctionNode (ftype, set, TableExprInfo());
}
TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
                                  const TableExprNode& node1,
                                  const TableExprNode& node2)
{
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node1));
    set.add (TableExprNodeSetElem(node2));
    return newFunctionNode (ftype, set, TableExprInfo());
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
    return newFunctionNode (ftype, set, TableExprInfo());
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
    return newFunctionNode (ftype, set, TableExprInfo());
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
    return newFunctionNode (ftype, set, TableExprInfo());
}

TableExprNode TableExprNode::newFunctionNode
                                 (TableExprFuncNode::FunctionType ftype,
                                  const TableExprNodeSet& set,
                                  const TableExprInfo& tabInfo,
                                  const TaQLStyle& style)
{
    // Convert the set to a vector of the values in the set elements.
    // This requires that the set has single values.
    if (! set.isSingle()) {
        throw (TableInvExpr ("A function parameter cannot be an interval"));
    }
    uInt npar = set.size();
    vector<TENShPtr> par(npar);
    for (uInt i=0; i<npar; i++) {
        par[i] = set[i]->start();
    }
    // rownrFUNC, rowidFUNC and randomFUNC are special, because they
    // need their own objects and the table.
    if (ftype == TableExprFuncNode::rownrFUNC) {
        TableExprNodeMulti::checkNumOfArg (0, 0, par);
        // First rownr is 0 or 1.
        return newRownrNode (tabInfo, style.origin());
    }
    if (ftype == TableExprFuncNode::rowidFUNC) {
        TableExprNodeMulti::checkNumOfArg (0, 0, par);
        return newRowidNode (tabInfo);
    }
    if (ftype == TableExprFuncNode::randFUNC) {
        TableExprNodeMulti::checkNumOfArg (0, 0, par);
        return newRandomNode (tabInfo);
    }
    // Check all the operands and get the resulting data type and value type
    // of the function.
    // It also fills the expected data and value type of the operands.
    TableExprNodeRep::ValueType resVT;
    TableExprNodeRep::NodeDataType resDT;
    Block<Int> dtypeOper;
    Block<Int> vtypeOper;
    TENShPtr fnode;
    // Create new function node depending on the type.
    if (ftype >= TableExprFuncNode::FirstAggrFunc) {
      resDT = TableExprAggrNode::checkOperands (dtypeOper, resVT, ftype, par);
      // Create new aggregate function node and fill it.
      if (resVT == TableExprNodeRep::VTScalar) {
        fnode = std::make_shared<TableExprAggrNode>(ftype, resDT, resVT, set,
                                                    par, dtypeOper);
      } else {
        fnode = std::make_shared<TableExprAggrNodeArray>(ftype, resDT, resVT, set,
                                                         par, dtypeOper, style);
      }
    } else {
      resDT = TableExprFuncNode::checkOperands (dtypeOper, resVT, vtypeOper,
                                                ftype, par);
      if (resVT == TableExprNodeRep::VTScalar) {
        auto node = std::make_shared<TableExprFuncNode>(ftype, resDT, resVT, set,
                                                        par, dtypeOper, tabInfo);
        fnode = node;
        // If the condition of IIF is a constant scalar, evaluate and replace.
        // Do it only if the data type matches.
        // Take the operands from the function node created,
        // because the units might be adapted.
        if (ftype == TableExprFuncNode::iifFUNC) {
          const std::vector<TENShPtr>& oper = node->operands();
          if (oper[0]->isConstant()) {
            if (oper[0]->getBool(TableExprId(0))) {
              if (resDT == oper[1]->dataType()) {
                fnode = oper[1];
              }
            } else {
              if (resDT == oper[2]->dataType()) {
                fnode = oper[2];
              }
            }
          }
        }
      } else {
        fnode = std::make_shared<TableExprFuncNodeArray>(ftype, resDT, resVT, set,
                                                         par, dtypeOper, style);
      }
    }
    return TableExprNodeRep::replaceConstNode (fnode);
}

TableExprNode TableExprNode::newUDFNode (const String& name,
                                         const TableExprNodeSet& set,
                                         const TableExprInfo& tableInfo,
                                         const TaQLStyle& style)
{
    // Create the correct UDF object. An exception is thrown if unknown.
    std::shared_ptr<UDFBase> udf(UDFBase::createUDF (name, style));
    // Convert the set to a vector of the values in the set elements.
    // This requires that the set has single values.
    if (! set.isSingle()) {
        throw (TableInvExpr ("A function parameter cannot be an interval"));
    }
    uInt npar = set.size();
    vector<TENShPtr> par(npar);
    for (uInt i=0; i<npar; i++) {
        par[i] = set[i]->start();
    }
    udf->init (par, tableInfo, style);
    if (udf->ndim() == 0) {
        return new TableExprUDFNode (udf, tableInfo, set);
    }
    return new TableExprUDFNodeArray (udf, tableInfo, set);
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
    // Convert the set to a vector of the values in the set elements.
    // This requires that the set has single values.
    if (! set.isSingle()) {
        throw (TableInvExpr ("A function parameter cannot be an interval"));
    }
    uInt npar = set.size();
    vector<TENShPtr> par(npar);
    for (uInt i=0; i<npar; i++) {
        par[i] = set[i]->start();
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
    TENShPtr fnode;
    if (resVT == TableExprNodeRep::VTScalar) {
      fnode = std::make_shared<TableExprConeNode>(ftype, resDT, set,
                                                  par, dtypeOper, origin);
    } else {
      fnode = std::make_shared<TableExprConeNodeArray>(ftype, resDT, set,
                                                       par, dtypeOper, origin);
    }
    return TableExprNodeRep::replaceConstNode (fnode);
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
    TENShPtr inode(new TableExprNodeIndex (indices, style));
    TENShPtr anode(new TableExprNodeArrayPart (arrayNode.node_p, inode));
    return anode;
}

void TableExprNode::adaptUnit (const Unit& unit)
{
    TableExprNodeUnit::adaptUnit (node_p, unit);
}

TableExprNode TableExprNode::newRownrNode (const TableExprInfo& tableInfo,
                                           uInt origin)
{
    return TENShPtr(new TableExprNodeRownr (tableInfo, origin));
}

TableExprNode TableExprNode::newRowidNode (const TableExprInfo& tableInfo)
{
    return TENShPtr(new TableExprNodeRowid (tableInfo));
}

TableExprNode TableExprNode::newRandomNode (const TableExprInfo& tableInfo)
{
    return TENShPtr(new TableExprNodeRandom (tableInfo));
}

DataType TableExprNode::dataType() const
{
    if (node_p->valueType() == TableExprNodeRep::VTScalar
    ||  node_p->valueType() == TableExprNodeRep::VTArray) {
        switch(node_p->dataType()) {
        case TableExprNodeRep::NTBool:
            return TpBool;
        case TableExprNodeRep::NTInt:
            return TpInt64;
        case TableExprNodeRep::NTDouble:
            return TpDouble;
        case TableExprNodeRep::NTComplex:
            return TpDComplex;
        case TableExprNodeRep::NTString:
            return TpString;
        case TableExprNodeRep::NTDate:
            return TpQuantity;
        default:
            return TpOther;
        }
    }
    return TpOther;
}

} //# NAMESPACE CASACORE - END

