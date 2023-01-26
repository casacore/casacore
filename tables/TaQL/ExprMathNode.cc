//# ExprMathNode.cc: Nodes representing scalar mathematical operators in table select expression tree
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000
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

#include <casacore/tables/TaQL/ExprMathNode.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Implement the arithmetic operators for each data type.

TableExprNodePlus::TableExprNodePlus (NodeDataType dt,
                                      const TableExprNodeRep& node)
: TableExprNodeBinary (dt, node, OtPlus)
{}
TableExprNodePlus::~TableExprNodePlus()
{}

TableExprNodePlusInt::TableExprNodePlusInt (const TableExprNodeRep& node)
: TableExprNodePlus (NTInt, node)
{}
TableExprNodePlusInt::~TableExprNodePlusInt()
{}
int64_t TableExprNodePlusInt::getInt (const TableExprId& id)
    { return lnode_p->getInt(id) + rnode_p->getInt(id); }
double TableExprNodePlusInt::getDouble (const TableExprId& id)
    { return lnode_p->getInt(id) + rnode_p->getInt(id); }
DComplex TableExprNodePlusInt::getDComplex (const TableExprId& id)
    { return double(lnode_p->getInt(id) + rnode_p->getInt(id)); }

TableExprNodePlusDouble::TableExprNodePlusDouble (const TableExprNodeRep& node)
: TableExprNodePlus (NTDouble, node)
{}
TableExprNodePlusDouble::~TableExprNodePlusDouble()
{}
double TableExprNodePlusDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) + rnode_p->getDouble(id); }
DComplex TableExprNodePlusDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) + rnode_p->getDouble(id); }

TableExprNodePlusDComplex::TableExprNodePlusDComplex (const TableExprNodeRep& node)
: TableExprNodePlus (NTComplex, node)
{}
TableExprNodePlusDComplex::~TableExprNodePlusDComplex()
{}
DComplex TableExprNodePlusDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) + rnode_p->getDComplex(id); }

TableExprNodePlusString::TableExprNodePlusString (const TableExprNodeRep& node)
: TableExprNodePlus (NTString, node)
{}
TableExprNodePlusString::~TableExprNodePlusString()
{}
String TableExprNodePlusString::getString (const TableExprId& id)
    { return lnode_p->getString(id) + rnode_p->getString(id); }

TableExprNodePlusDate::TableExprNodePlusDate (const TableExprNodeRep& node)
: TableExprNodePlus (NTDate, node)
{}
TableExprNodePlusDate::~TableExprNodePlusDate()
{}
void TableExprNodePlusDate::handleUnits()
{
    if (lnode_p->dataType() == NTDouble) {
        TableExprNodeUnit::adaptUnit (lnode_p, "d");
    } else if (rnode_p->dataType() == NTDouble) {
        TableExprNodeUnit::adaptUnit (rnode_p, "d");
    }
}
MVTime TableExprNodePlusDate::getDate(const TableExprId& id)
    { return lnode_p->getDouble(id) + rnode_p->getDouble(id); }
double TableExprNodePlusDate::getDouble(const TableExprId& id)
    { return lnode_p->getDouble(id) + rnode_p->getDouble(id); }


TableExprNodeMinus::TableExprNodeMinus (NodeDataType dt,
                                        const TableExprNodeRep& node)
: TableExprNodeBinary (dt, node, OtMinus)
{}
TableExprNodeMinus::~TableExprNodeMinus()
{}

TableExprNodeMinusInt::TableExprNodeMinusInt (const TableExprNodeRep& node)
: TableExprNodeMinus (NTInt, node)
{}
TableExprNodeMinusInt::~TableExprNodeMinusInt()
{}
void TableExprNodeMinusInt::handleUnits()
{
    if (lnode_p->dataType() == NTDate  &&  rnode_p->dataType() == NTDate) {
        setUnit("d");                     //# date-date results in days
    } else {
        TableExprNodeBinary::handleUnits();
    }
}
int64_t TableExprNodeMinusInt::getInt (const TableExprId& id)
    { return lnode_p->getInt(id) - rnode_p->getInt(id); }
double TableExprNodeMinusInt::getDouble (const TableExprId& id)
    { return lnode_p->getInt(id) - rnode_p->getInt(id); }
DComplex TableExprNodeMinusInt::getDComplex (const TableExprId& id)
    { return double(lnode_p->getInt(id) - rnode_p->getInt(id)); }

TableExprNodeMinusDouble::TableExprNodeMinusDouble (const TableExprNodeRep& node)
: TableExprNodeMinus (NTDouble, node)
{}
TableExprNodeMinusDouble::~TableExprNodeMinusDouble()
{}
void TableExprNodeMinusDouble::handleUnits()
{
    if (lnode_p->dataType() == NTDate  &&  rnode_p->dataType() == NTDate) {
        setUnit("d");                     //# date-date results in days
    } else {
        TableExprNodeBinary::handleUnits();
    }
}
double TableExprNodeMinusDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }
DComplex TableExprNodeMinusDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }

TableExprNodeMinusDComplex::TableExprNodeMinusDComplex (const TableExprNodeRep& node)
: TableExprNodeMinus (NTComplex, node)
{}
TableExprNodeMinusDComplex::~TableExprNodeMinusDComplex()
{}
DComplex TableExprNodeMinusDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) - rnode_p->getDComplex(id); }

TableExprNodeMinusDate::TableExprNodeMinusDate (const TableExprNodeRep& node)
: TableExprNodeMinus (NTDate, node)
{}
TableExprNodeMinusDate::~TableExprNodeMinusDate()
{}
void TableExprNodeMinusDate::handleUnits()
{
    // Right hand side must be in days.
    TableExprNodeUnit::adaptUnit (rnode_p, "d");
}
MVTime TableExprNodeMinusDate::getDate(const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }
double TableExprNodeMinusDate::getDouble(const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }


TableExprNodeTimes::TableExprNodeTimes (NodeDataType dt,
                                        const TableExprNodeRep& node)
: TableExprNodeBinary (dt, node, OtTimes)
{}
TableExprNodeTimes::~TableExprNodeTimes()
{}
void TableExprNodeTimes::handleUnits()
{
    if (lnode_p->unit().empty()) {
        setUnit (rnode_p->unit());
    } else if (rnode_p->unit().empty()) {
        setUnit (lnode_p->unit());
    } else {
        Quantity q1 (1, lnode_p->unit());
        Quantity q2 (1, rnode_p->unit());
        setUnit ((q1*q2).getFullUnit());
    }
}

TableExprNodeTimesInt::TableExprNodeTimesInt (const TableExprNodeRep& node)
: TableExprNodeTimes (NTInt, node)
{}
TableExprNodeTimesInt::~TableExprNodeTimesInt()
{}
int64_t TableExprNodeTimesInt::getInt (const TableExprId& id)
    { return lnode_p->getInt(id) * rnode_p->getInt(id); }
double TableExprNodeTimesInt::getDouble (const TableExprId& id)
    { return lnode_p->getInt(id) * rnode_p->getInt(id); }
DComplex TableExprNodeTimesInt::getDComplex (const TableExprId& id)
    { return double(lnode_p->getInt(id) * rnode_p->getInt(id)); }

TableExprNodeTimesDouble::TableExprNodeTimesDouble (const TableExprNodeRep& node)
: TableExprNodeTimes (NTDouble, node)
{}
TableExprNodeTimesDouble::~TableExprNodeTimesDouble()
{}
double TableExprNodeTimesDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) * rnode_p->getDouble(id); }
DComplex TableExprNodeTimesDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) * rnode_p->getDouble(id); }

TableExprNodeTimesDComplex::TableExprNodeTimesDComplex (const TableExprNodeRep& node)
: TableExprNodeTimes (NTComplex, node)
{}
TableExprNodeTimesDComplex::~TableExprNodeTimesDComplex()
{}
DComplex TableExprNodeTimesDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) * rnode_p->getDComplex(id); }


TableExprNodeDivide::TableExprNodeDivide (NodeDataType dt,
                                          const TableExprNodeRep& node)
: TableExprNodeBinary (dt, node, OtDivide)
{}
TableExprNodeDivide::~TableExprNodeDivide()
{}
void TableExprNodeDivide::handleUnits()
{
    if (lnode_p->unit().empty()) {
        if (! rnode_p->unit().empty()) {
            Quantity q1 (1);
            Quantity q2 (1, rnode_p->unit());
            setUnit ((q1/q2).getFullUnit());
        }
    } else if (rnode_p->unit().empty()) {
        // For backward compatibility dividing seconds by 86400 is a
        // conversion to days.
        if (rnode_p->isConstant()
        &&  (rnode_p->dataType() == NTDouble  ||  rnode_p->dataType() == NTInt)
        &&  rnode_p->getDouble(0) == 86400.
        &&  lnode_p->unit().getName() == "s") {
            setUnit ("d");
        } else {
            setUnit (lnode_p->unit());
        }
    } else {
        Quantity q1 (1, lnode_p->unit());
        Quantity q2 (1, rnode_p->unit());
        // If same unit kinds, result is no unit.
        if (q1.isConform (q2)) {
            makeEqualUnits (lnode_p, rnode_p);
        } else {
            setUnit ((q1/q2).getFullUnit());
        }
    }
}

TableExprNodeDivideDouble::TableExprNodeDivideDouble (const TableExprNodeRep& node)
: TableExprNodeDivide (NTDouble, node)
{}
TableExprNodeDivideDouble::~TableExprNodeDivideDouble()
{}
double TableExprNodeDivideDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) / rnode_p->getDouble(id); }
DComplex TableExprNodeDivideDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) / rnode_p->getDouble(id); }

TableExprNodeDivideDComplex::TableExprNodeDivideDComplex (const TableExprNodeRep& node)
: TableExprNodeDivide (NTComplex, node)
{}
TableExprNodeDivideDComplex::~TableExprNodeDivideDComplex()
{}
DComplex TableExprNodeDivideDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) / rnode_p->getDComplex(id); }


TableExprNodeModulo::TableExprNodeModulo (NodeDataType dt,
                                          const TableExprNodeRep& node)
: TableExprNodeBinary (dt, node, OtModulo)
{}
TableExprNodeModulo::~TableExprNodeModulo()
{}
void TableExprNodeModulo::handleUnits()
{
    TableExprNodeBinary::handleUnits();
}

TableExprNodeModuloInt::TableExprNodeModuloInt (const TableExprNodeRep& node)
: TableExprNodeModulo (NTInt, node)
{}
TableExprNodeModuloInt::~TableExprNodeModuloInt()
{}
int64_t TableExprNodeModuloInt::getInt (const TableExprId& id)
    { return floormod (lnode_p->getInt(id), rnode_p->getInt(id)); }
double TableExprNodeModuloInt::getDouble (const TableExprId& id)
    { return getInt (id); }
DComplex TableExprNodeModuloInt::getDComplex (const TableExprId& id)
    { return getInt (id); }

TableExprNodeModuloDouble::TableExprNodeModuloDouble (const TableExprNodeRep& node)
: TableExprNodeModulo (NTDouble, node)
{}
TableExprNodeModuloDouble::~TableExprNodeModuloDouble()
{}
double TableExprNodeModuloDouble::getDouble (const TableExprId& id)
    { return floormod (lnode_p->getDouble(id), rnode_p->getDouble(id)); }
DComplex TableExprNodeModuloDouble::getDComplex (const TableExprId& id)
    { return getDouble (id); }


TableExprNodeBitAndInt::TableExprNodeBitAndInt (const TableExprNodeRep& node)
: TableExprNodeBinary (NTInt, node, OtBitAnd)
{}
TableExprNodeBitAndInt::~TableExprNodeBitAndInt()
{}
int64_t TableExprNodeBitAndInt::getInt (const TableExprId& id)
    { return lnode_p->getInt(id) & rnode_p->getInt(id); }
double TableExprNodeBitAndInt::getDouble (const TableExprId& id)
    { return lnode_p->getInt(id) & rnode_p->getInt(id); }
DComplex TableExprNodeBitAndInt::getDComplex (const TableExprId& id)
    { return double(lnode_p->getInt(id) & rnode_p->getInt(id)); }

TableExprNodeBitOrInt::TableExprNodeBitOrInt (const TableExprNodeRep& node)
: TableExprNodeBinary (NTInt, node, OtBitOr)
{}
TableExprNodeBitOrInt::~TableExprNodeBitOrInt()
{}
int64_t TableExprNodeBitOrInt::getInt (const TableExprId& id)
    { return lnode_p->getInt(id) | rnode_p->getInt(id); }
double TableExprNodeBitOrInt::getDouble (const TableExprId& id)
    { return lnode_p->getInt(id) | rnode_p->getInt(id); }
DComplex TableExprNodeBitOrInt::getDComplex (const TableExprId& id)
    { return double(lnode_p->getInt(id) | rnode_p->getInt(id)); }

TableExprNodeBitXorInt::TableExprNodeBitXorInt (const TableExprNodeRep& node)
: TableExprNodeBinary (NTInt, node, OtBitXor)
{}
TableExprNodeBitXorInt::~TableExprNodeBitXorInt()
{}
int64_t TableExprNodeBitXorInt::getInt (const TableExprId& id)
    { return lnode_p->getInt(id) ^ rnode_p->getInt(id); }
double TableExprNodeBitXorInt::getDouble (const TableExprId& id)
    { return lnode_p->getInt(id) ^ rnode_p->getInt(id); }
DComplex TableExprNodeBitXorInt::getDComplex (const TableExprId& id)
    { return double(lnode_p->getInt(id) ^ rnode_p->getInt(id)); }


TableExprNodeMIN::TableExprNodeMIN (const TableExprNodeRep& node)
: TableExprNodeBinary (node.dataType(), node, OtMIN)
{}
TableExprNodeMIN::~TableExprNodeMIN()
{}
int64_t TableExprNodeMIN::getInt (const TableExprId& id)
    { return -(lnode_p->getInt(id)); }
double TableExprNodeMIN::getDouble (const TableExprId& id)
    { return -(lnode_p->getDouble(id)); }
DComplex TableExprNodeMIN::getDComplex (const TableExprId& id)
    { return -(lnode_p->getDComplex(id)); }


TableExprNodeBitNegate::TableExprNodeBitNegate (const TableExprNodeRep& node)
: TableExprNodeBinary (node.dataType(), node, OtBitNegate)
{}
TableExprNodeBitNegate::~TableExprNodeBitNegate()
{}
int64_t TableExprNodeBitNegate::getInt (const TableExprId& id)
    { return ~(lnode_p->getInt(id)); }
double TableExprNodeBitNegate::getDouble (const TableExprId& id)
    { return double(~(lnode_p->getInt(id))); }
DComplex TableExprNodeBitNegate::getDComplex (const TableExprId& id)
    { return double(~(lnode_p->getInt(id))); }


} //# NAMESPACE CASACORE - END
