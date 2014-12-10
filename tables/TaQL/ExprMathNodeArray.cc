//# ExprMathNodeArray.cc: Nodes representing mathematical array operators in table select expression tree
//# Copyright (C) 1997,1998,1999,2000
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

#include <casacore/tables/TaQL/ExprMathNodeArray.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Quanta/MVTime.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Implement the arithmetic operators for each data type.

TableExprNodeArrayPlus::TableExprNodeArrayPlus (NodeDataType dt,
						const TableExprNodeRep& node)
: TableExprNodeArray (node, dt, OtPlus)
{}
TableExprNodeArrayPlus::~TableExprNodeArrayPlus()
{}

TableExprNodeArrayPlusInt::TableExprNodeArrayPlusInt
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayPlus (NTInt, node)
{}
TableExprNodeArrayPlusInt::~TableExprNodeArrayPlusInt()
{}
Array<Int64> TableExprNodeArrayPlusInt::getArrayInt
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayInt (id) + rnode_p->getInt (id);
    case ScaArr:
	return lnode_p->getInt (id) + rnode_p->getArrayInt (id);
    default:
	break;
    }
    return lnode_p->getArrayInt (id) + rnode_p->getArrayInt (id);
}

TableExprNodeArrayPlusDouble::TableExprNodeArrayPlusDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayPlus (NTDouble, node)
{}
TableExprNodeArrayPlusDouble::~TableExprNodeArrayPlusDouble()
{}
Array<Double> TableExprNodeArrayPlusDouble::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (id) + rnode_p->getDouble (id);
    case ScaArr:
	return lnode_p->getDouble (id) + rnode_p->getArrayDouble (id);
    default:
	break;
    }
    return lnode_p->getArrayDouble (id) + rnode_p->getArrayDouble (id);
}

TableExprNodeArrayPlusDComplex::TableExprNodeArrayPlusDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayPlus (NTComplex, node)
{}
TableExprNodeArrayPlusDComplex::~TableExprNodeArrayPlusDComplex()
{}
Array<DComplex> TableExprNodeArrayPlusDComplex::getArrayDComplex
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (id) + rnode_p->getDComplex (id);
    case ScaArr:
	return lnode_p->getDComplex (id) + rnode_p->getArrayDComplex (id);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (id) + rnode_p->getArrayDComplex (id);
}

TableExprNodeArrayPlusString::TableExprNodeArrayPlusString
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayPlus (NTString, node)
{}
TableExprNodeArrayPlusString::~TableExprNodeArrayPlusString()
{}
Array<String> TableExprNodeArrayPlusString::getArrayString
                                            (const TableExprId& id)
{
    IPosition shape;
    Array<String> left;
    Array<String> right;
    Int incrLeft = 1;
    Int incrRight = 1;
    switch (argtype_p) {
    case ArrSca:
	left = lnode_p->getArrayString (id);
	shape = left.shape();
	right.resize (IPosition(1,1));
	right.set (rnode_p->getString (id));
	incrRight = 0;
	break;
    case ScaArr:
	left.resize (IPosition(1,1));
	left.set (lnode_p->getString (id));
	incrLeft = 0;
	right = rnode_p->getArrayString (id);
	break;
    default:
	left = lnode_p->getArrayString (id);
	shape = left.shape();
	right = rnode_p->getArrayString (id);
	if (!shape.isEqual (right.shape())) {
	    throw (TableInvExpr ("TableExprNodeArrayPlusString: "
				 " mismatching array shapes"));
	}
    }
    Array<String> result(shape);
    Bool deleteLeft, deleteRight, deleteTo;
    const String* l = left.getStorage (deleteLeft);
    const String* r = right.getStorage (deleteRight);
    String* to = result.getStorage (deleteTo);
    concString (to, l, incrLeft, r, incrRight, shape.product());
    left.freeStorage (l, deleteLeft);
    right.freeStorage (r, deleteRight);
    result.putStorage (to, deleteTo);
    return result;
}
void TableExprNodeArrayPlusString::concString (String* to,
					       const String* left,
					       Int incrLeft,
					       const String* right,
					       Int incrRight,
					       uInt nr) const
{
    String* end = to + nr;
    while (to < end) {
	*to++ = *left + *right;
	left  += incrLeft;
	right += incrRight;
    }
}

TableExprNodeArrayPlusDate::TableExprNodeArrayPlusDate
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayPlus (NTDate, node)
{}
TableExprNodeArrayPlusDate::~TableExprNodeArrayPlusDate()
{}
void TableExprNodeArrayPlusDate::handleUnits()
{
    if (lnode_p->dataType() == NTDouble) {
        TableExprNodeUnit::adaptUnit (lnode_p, "d");
    } else if (rnode_p->dataType() == NTDouble) {
        TableExprNodeUnit::adaptUnit (rnode_p, "d");
    }
}
Array<Double> TableExprNodeArrayPlusDate::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
        return lnode_p->getArrayDouble(id) + rnode_p->getDouble(id);
    case ScaArr:
        return lnode_p->getDouble(id) + rnode_p->getArrayDouble(id);
    default:
	break;
    }
    return lnode_p->getArrayDouble(id) + rnode_p->getArrayDouble(id);
}
Array<MVTime> TableExprNodeArrayPlusDate::getArrayDate
                                            (const TableExprId& id)
{
    Array<Double> tmp(getArrayDouble(id));
    Array<MVTime> res(tmp.shape());
    convertArray (res, tmp);
    return res;
}


TableExprNodeArrayMinus::TableExprNodeArrayMinus (NodeDataType dt,
						  const TableExprNodeRep& node)
: TableExprNodeArray (node, dt, OtMinus)
{}
TableExprNodeArrayMinus::~TableExprNodeArrayMinus()
{}

TableExprNodeArrayMinusInt::TableExprNodeArrayMinusInt
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayMinus (NTInt, node)
{}
TableExprNodeArrayMinusInt::~TableExprNodeArrayMinusInt()
{}
Array<Int64> TableExprNodeArrayMinusInt::getArrayInt
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayInt (id) - rnode_p->getInt (id);
    case ScaArr:
	return lnode_p->getInt (id) - rnode_p->getArrayInt (id);
    default:
	break;
    }
    return lnode_p->getArrayInt (id) - rnode_p->getArrayInt (id);
}

TableExprNodeArrayMinusDouble::TableExprNodeArrayMinusDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayMinus (NTDouble, node)
{}
TableExprNodeArrayMinusDouble::~TableExprNodeArrayMinusDouble()
{}
Array<Double> TableExprNodeArrayMinusDouble::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (id) - rnode_p->getDouble (id);
    case ScaArr:
	return lnode_p->getDouble (id) - rnode_p->getArrayDouble (id);
    default:
	break;
    }
    return lnode_p->getArrayDouble (id) - rnode_p->getArrayDouble (id);
}

TableExprNodeArrayMinusDComplex::TableExprNodeArrayMinusDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayMinus (NTComplex, node)
{}
TableExprNodeArrayMinusDComplex::~TableExprNodeArrayMinusDComplex()
{}
Array<DComplex> TableExprNodeArrayMinusDComplex::getArrayDComplex
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (id) - rnode_p->getDComplex (id);
    case ScaArr:
	return lnode_p->getDComplex (id) - rnode_p->getArrayDComplex (id);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (id) - rnode_p->getArrayDComplex (id);
}

TableExprNodeArrayMinusDate::TableExprNodeArrayMinusDate
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayMinus (NTDate, node)
{}
TableExprNodeArrayMinusDate::~TableExprNodeArrayMinusDate()
{}
void TableExprNodeArrayMinusDate::handleUnits()
{
    // Right hand side must be in days.
    TableExprNodeUnit::adaptUnit (rnode_p, "d");
}
Array<Double> TableExprNodeArrayMinusDate::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
        return lnode_p->getArrayDouble(id) - rnode_p->getDouble(id);
    case ScaArr:
        return lnode_p->getDouble(id) - rnode_p->getArrayDouble(id);
    default:
	break;
    }
    return lnode_p->getArrayDouble(id) - rnode_p->getArrayDouble(id);
}
Array<MVTime> TableExprNodeArrayMinusDate::getArrayDate
                                            (const TableExprId& id)
{
    Array<Double> tmp(getArrayDouble(id));
    Array<MVTime> res(tmp.shape());
    convertArray (res, tmp);
    return res;
}


TableExprNodeArrayTimes::TableExprNodeArrayTimes (NodeDataType dt,
						  const TableExprNodeRep& node)
: TableExprNodeArray (node, dt, OtTimes)
{}
TableExprNodeArrayTimes::~TableExprNodeArrayTimes()
{}
void TableExprNodeArrayTimes::handleUnits()
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

TableExprNodeArrayTimesInt::TableExprNodeArrayTimesInt
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayTimes (NTInt, node)
{}
TableExprNodeArrayTimesInt::~TableExprNodeArrayTimesInt()
{}
Array<Int64> TableExprNodeArrayTimesInt::getArrayInt
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayInt (id) * rnode_p->getInt (id);
    case ScaArr:
	return lnode_p->getInt (id) * rnode_p->getArrayInt (id);
    default:
	break;
    }
    return lnode_p->getArrayInt (id) * rnode_p->getArrayInt (id);
}

TableExprNodeArrayTimesDouble::TableExprNodeArrayTimesDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayTimes (NTDouble, node)
{}
TableExprNodeArrayTimesDouble::~TableExprNodeArrayTimesDouble()
{}
Array<Double> TableExprNodeArrayTimesDouble::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (id) * rnode_p->getDouble (id);
    case ScaArr:
	return lnode_p->getDouble (id) * rnode_p->getArrayDouble (id);
    default:
	break;
    }
    return lnode_p->getArrayDouble (id) * rnode_p->getArrayDouble (id);
}

TableExprNodeArrayTimesDComplex::TableExprNodeArrayTimesDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayTimes (NTComplex, node)
{}
TableExprNodeArrayTimesDComplex::~TableExprNodeArrayTimesDComplex()
{}
Array<DComplex> TableExprNodeArrayTimesDComplex::getArrayDComplex
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (id) * rnode_p->getDComplex (id);
    case ScaArr:
	return lnode_p->getDComplex (id) * rnode_p->getArrayDComplex (id);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (id) * rnode_p->getArrayDComplex (id);
}


TableExprNodeArrayDivide::TableExprNodeArrayDivide (NodeDataType dt,
						const TableExprNodeRep& node)
: TableExprNodeArray (node, dt, OtDivide)
{}
TableExprNodeArrayDivide::~TableExprNodeArrayDivide()
{}
void TableExprNodeArrayDivide::handleUnits()
{
    if (lnode_p->unit().empty()) {
        setUnit (rnode_p->unit());
    } else if (rnode_p->unit().empty()) {
        setUnit (lnode_p->unit());
    } else {
        Quantity q1 (1, lnode_p->unit());
	Quantity q2 (1, rnode_p->unit());
	setUnit ((q1/q2).getFullUnit());
    }
}

TableExprNodeArrayDivideDouble::TableExprNodeArrayDivideDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayDivide (NTDouble, node)
{}
TableExprNodeArrayDivideDouble::~TableExprNodeArrayDivideDouble()
{}
Array<Double> TableExprNodeArrayDivideDouble::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (id) / rnode_p->getDouble (id);
    case ScaArr:
	return lnode_p->getDouble (id) / rnode_p->getArrayDouble (id);
    default:
	break;
    }
    return lnode_p->getArrayDouble (id) / rnode_p->getArrayDouble (id);
}

TableExprNodeArrayDivideDComplex::TableExprNodeArrayDivideDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayDivide (NTComplex, node)
{}
TableExprNodeArrayDivideDComplex::~TableExprNodeArrayDivideDComplex()
{}
Array<DComplex> TableExprNodeArrayDivideDComplex::getArrayDComplex
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (id) / rnode_p->getDComplex (id);
    case ScaArr:
	return lnode_p->getDComplex (id) / rnode_p->getArrayDComplex (id);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (id) / rnode_p->getArrayDComplex (id);
}


TableExprNodeArrayModulo::TableExprNodeArrayModulo (NodeDataType dt,
						const TableExprNodeRep& node)
: TableExprNodeArray (node, dt, OtModulo)
{}
TableExprNodeArrayModulo::~TableExprNodeArrayModulo()
{}
void TableExprNodeArrayModulo::handleUnits()
{
    setUnit (lnode_p->unit());
}

TableExprNodeArrayModuloInt::TableExprNodeArrayModuloInt
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayModulo (NTInt, node)
{}
TableExprNodeArrayModuloInt::~TableExprNodeArrayModuloInt()
{}
Array<Int64> TableExprNodeArrayModuloInt::getArrayInt
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayInt(id) % rnode_p->getInt(id);
    case ScaArr:
	return lnode_p->getInt(id) % rnode_p->getArrayInt(id);
    default:
	break;
    }
    return lnode_p->getArrayInt(id) % rnode_p->getArrayInt(id);
}


TableExprNodeArrayModuloDouble::TableExprNodeArrayModuloDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArrayModulo (NTDouble, node)
{}
TableExprNodeArrayModuloDouble::~TableExprNodeArrayModuloDouble()
{}
Array<Double> TableExprNodeArrayModuloDouble::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
      return fmod (lnode_p->getArrayDouble(id), rnode_p->getDouble(id));
    case ScaArr:
      return fmod (lnode_p->getDouble(id), rnode_p->getArrayDouble(id));
    default:
	break;
    }
    return fmod (lnode_p->getArrayDouble(id), rnode_p->getArrayDouble(id));
}


TableExprNodeArrayBitAndInt::TableExprNodeArrayBitAndInt
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTInt, OtBitAnd)
{}
TableExprNodeArrayBitAndInt::~TableExprNodeArrayBitAndInt()
{}
Array<Int64> TableExprNodeArrayBitAndInt::getArrayInt
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayInt(id) & rnode_p->getInt(id);
    case ScaArr:
	return lnode_p->getInt(id) & rnode_p->getArrayInt(id);
    default:
	break;
    }
    return lnode_p->getArrayInt(id) & rnode_p->getArrayInt(id);
}


TableExprNodeArrayBitOrInt::TableExprNodeArrayBitOrInt
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTInt, OtBitOr)
{}
TableExprNodeArrayBitOrInt::~TableExprNodeArrayBitOrInt()
{}
Array<Int64> TableExprNodeArrayBitOrInt::getArrayInt
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayInt(id) | rnode_p->getInt(id);
    case ScaArr:
	return lnode_p->getInt(id) | rnode_p->getArrayInt(id);
    default:
	break;
    }
    return lnode_p->getArrayInt(id) | rnode_p->getArrayInt(id);
}


TableExprNodeArrayBitXorInt::TableExprNodeArrayBitXorInt
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTInt, OtBitXor)
{}
TableExprNodeArrayBitXorInt::~TableExprNodeArrayBitXorInt()
{}
Array<Int64> TableExprNodeArrayBitXorInt::getArrayInt
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayInt(id) ^ rnode_p->getInt(id);
    case ScaArr:
	return lnode_p->getInt(id) ^ rnode_p->getArrayInt(id);
    default:
	break;
    }
    return lnode_p->getArrayInt(id) ^ rnode_p->getArrayInt(id);
}


TableExprNodeArrayMIN::TableExprNodeArrayMIN (const TableExprNodeRep& node)
: TableExprNodeArray (node, node.dataType(), OtMIN)
{}
TableExprNodeArrayMIN::~TableExprNodeArrayMIN()
{}
Array<Int64> TableExprNodeArrayMIN::getArrayInt (const TableExprId& id)
    { return -(lnode_p->getArrayInt(id)); }
Array<Double> TableExprNodeArrayMIN::getArrayDouble (const TableExprId& id)
    { return -(lnode_p->getArrayDouble(id)); }
Array<DComplex> TableExprNodeArrayMIN::getArrayDComplex (const TableExprId& id)
    { return -(lnode_p->getArrayDComplex(id)); }


TableExprNodeArrayBitNegate::TableExprNodeArrayBitNegate
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, node.dataType(), OtBitNegate)
{}
TableExprNodeArrayBitNegate::~TableExprNodeArrayBitNegate()
{}
Array<Int64> TableExprNodeArrayBitNegate::getArrayInt (const TableExprId& id)
    { return ~(lnode_p->getArrayInt(id)); }


} //# NAMESPACE CASACORE - END

