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

#include <aips/Tables/ExprMathNodeArray.h>
#include <aips/Tables/TableError.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Quanta/MVTime.h>


// Implement the arithmetic operators for each data type.

TableExprNodeArrayPlusDouble::TableExprNodeArrayPlusDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtPlus)
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
: TableExprNodeArray (node, NTComplex, OtPlus)
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
: TableExprNodeArray (node, NTComplex, OtPlus)
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


TableExprNodeArrayMinusDouble::TableExprNodeArrayMinusDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtMinus)
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
: TableExprNodeArray (node, NTComplex, OtMinus)
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


TableExprNodeArrayTimesDouble::TableExprNodeArrayTimesDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtTimes)
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
: TableExprNodeArray (node, NTComplex, OtTimes)
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


TableExprNodeArrayDivideDouble::TableExprNodeArrayDivideDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtDivide)
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
: TableExprNodeArray (node, NTComplex, OtDivide)
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


TableExprNodeArrayModuloDouble::TableExprNodeArrayModuloDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtModulo)
{}
TableExprNodeArrayModuloDouble::~TableExprNodeArrayModuloDouble()
{}
Array<Double> TableExprNodeArrayModuloDouble::getArrayDouble
                                            (const TableExprId& id)
{
    switch (argtype_p) {
    case ArrSca:
    {
	Array<Double> arr = lnode_p->getArrayDouble (id);
	return fmod (arr,
		     makeArray (arr.shape(), rnode_p->getDouble (id)));
    }
    case ScaArr:
    {
	Array<Double> arr = rnode_p->getArrayDouble (id);
	return fmod (makeArray (arr.shape(), lnode_p->getDouble (id)),
		     arr);
    }
    default:
	break;
    }
    return fmod (lnode_p->getArrayDouble (id),
		 rnode_p->getArrayDouble (id));
}


TableExprNodeArrayMIN::TableExprNodeArrayMIN (const TableExprNodeRep& node)
: TableExprNodeArray (node, node.dataType(), OtMIN)
{}
TableExprNodeArrayMIN::~TableExprNodeArrayMIN()
{}
Array<Double> TableExprNodeArrayMIN::getArrayDouble (const TableExprId& id)
    { return -(lnode_p->getArrayDouble(id)); }
Array<DComplex> TableExprNodeArrayMIN::getArrayDComplex (const TableExprId& id)
    { return -(lnode_p->getArrayDComplex(id)); }
