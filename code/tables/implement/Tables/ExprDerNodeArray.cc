//# ExprDerNodeArray.cc: Nodes representing array operators in table select expression tree
//# Copyright (C) 1997,1998
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

#include <aips/Tables/ExprDerNodeArray.h>
#include <aips/Tables/TableError.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Quanta/MVTime.h>


// Implement the arithmetic operators for each data type.

TableExprNodeArrayPlusDouble::TableExprNodeArrayPlusDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtPlus)
{}
TableExprNodeArrayPlusDouble::~TableExprNodeArrayPlusDouble()
{}
Array<Double> TableExprNodeArrayPlusDouble::getArrayDouble (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (rownr) + rnode_p->getDouble (rownr);
    case ScaArr:
	return lnode_p->getDouble (rownr) + rnode_p->getArrayDouble (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble (rownr) + rnode_p->getArrayDouble (rownr);
}

TableExprNodeArrayPlusDComplex::TableExprNodeArrayPlusDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTComplex, OtPlus)
{}
TableExprNodeArrayPlusDComplex::~TableExprNodeArrayPlusDComplex()
{}
Array<DComplex> TableExprNodeArrayPlusDComplex::getArrayDComplex (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (rownr) + rnode_p->getDComplex (rownr);
    case ScaArr:
	return lnode_p->getDComplex (rownr) + rnode_p->getArrayDComplex (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (rownr) + rnode_p->getArrayDComplex (rownr);
}

TableExprNodeArrayPlusString::TableExprNodeArrayPlusString
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTComplex, OtPlus)
{}
TableExprNodeArrayPlusString::~TableExprNodeArrayPlusString()
{}
Array<String> TableExprNodeArrayPlusString::getArrayString (uInt rownr)
{
    IPosition shape;
    Array<String> left;
    Array<String> right;
    Int incrLeft = 1;
    Int incrRight = 1;
    switch (argtype_p) {
    case ArrSca:
	left = lnode_p->getArrayString (rownr);
	shape = left.shape();
	right.resize (IPosition(1,1));
	right.set (rnode_p->getString (rownr));
	incrRight = 0;
	break;
    case ScaArr:
	left.resize (IPosition(1,1));
	left.set (lnode_p->getString (rownr));
	incrLeft = 0;
	right = rnode_p->getArrayString (rownr);
	break;
    default:
	left = lnode_p->getArrayString (rownr);
	shape = left.shape();
	right = rnode_p->getArrayString (rownr);
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
Array<Double> TableExprNodeArrayMinusDouble::getArrayDouble (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (rownr) - rnode_p->getDouble (rownr);
    case ScaArr:
	return lnode_p->getDouble (rownr) - rnode_p->getArrayDouble (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble (rownr) - rnode_p->getArrayDouble (rownr);
}

TableExprNodeArrayMinusDComplex::TableExprNodeArrayMinusDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTComplex, OtMinus)
{}
TableExprNodeArrayMinusDComplex::~TableExprNodeArrayMinusDComplex()
{}
Array<DComplex> TableExprNodeArrayMinusDComplex::getArrayDComplex (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (rownr) - rnode_p->getDComplex (rownr);
    case ScaArr:
	return lnode_p->getDComplex (rownr) - rnode_p->getArrayDComplex (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (rownr) - rnode_p->getArrayDComplex (rownr);
}


TableExprNodeArrayTimesDouble::TableExprNodeArrayTimesDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtTimes)
{}
TableExprNodeArrayTimesDouble::~TableExprNodeArrayTimesDouble()
{}
Array<Double> TableExprNodeArrayTimesDouble::getArrayDouble (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (rownr) * rnode_p->getDouble (rownr);
    case ScaArr:
	return lnode_p->getDouble (rownr) * rnode_p->getArrayDouble (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble (rownr) * rnode_p->getArrayDouble (rownr);
}

TableExprNodeArrayTimesDComplex::TableExprNodeArrayTimesDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTComplex, OtTimes)
{}
TableExprNodeArrayTimesDComplex::~TableExprNodeArrayTimesDComplex()
{}
Array<DComplex> TableExprNodeArrayTimesDComplex::getArrayDComplex (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (rownr) * rnode_p->getDComplex (rownr);
    case ScaArr:
	return lnode_p->getDComplex (rownr) * rnode_p->getArrayDComplex (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (rownr) * rnode_p->getArrayDComplex (rownr);
}


TableExprNodeArrayDivideDouble::TableExprNodeArrayDivideDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtDivide)
{}
TableExprNodeArrayDivideDouble::~TableExprNodeArrayDivideDouble()
{}
Array<Double> TableExprNodeArrayDivideDouble::getArrayDouble (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (rownr) / rnode_p->getDouble (rownr);
    case ScaArr:
	return lnode_p->getDouble (rownr) / rnode_p->getArrayDouble (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble (rownr) / rnode_p->getArrayDouble (rownr);
}

TableExprNodeArrayDivideDComplex::TableExprNodeArrayDivideDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTComplex, OtDivide)
{}
TableExprNodeArrayDivideDComplex::~TableExprNodeArrayDivideDComplex()
{}
Array<DComplex> TableExprNodeArrayDivideDComplex::getArrayDComplex (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex (rownr) / rnode_p->getDComplex (rownr);
    case ScaArr:
	return lnode_p->getDComplex (rownr) / rnode_p->getArrayDComplex (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex (rownr) / rnode_p->getArrayDComplex (rownr);
}


TableExprNodeArrayModuloDouble::TableExprNodeArrayModuloDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTDouble, OtModulo)
{}
TableExprNodeArrayModuloDouble::~TableExprNodeArrayModuloDouble()
{}
Array<Double> TableExprNodeArrayModuloDouble::getArrayDouble (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
    {
	Array<Double> arr = lnode_p->getArrayDouble (rownr);
	return fmod (arr,
		     makeArray (arr.shape(), rnode_p->getDouble (rownr)));
    }
    case ScaArr:
    {
	Array<Double> arr = rnode_p->getArrayDouble (rownr);
	return fmod (makeArray (arr.shape(), lnode_p->getDouble (rownr)),
		     arr);
    }
    default:
	break;
    }
    return fmod (lnode_p->getArrayDouble (rownr),
		 rnode_p->getArrayDouble (rownr));
}



TableExprNodeArrayEQBool::TableExprNodeArrayEQBool
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtEQ)
{}
TableExprNodeArrayEQBool::~TableExprNodeArrayEQBool()
{}
Array<Bool> TableExprNodeArrayEQBool::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayBool (rownr) == rnode_p->getBool (rownr);
    case ScaArr:
	return lnode_p->getBool (rownr) == rnode_p->getArrayBool (rownr);
    default:
	break;
    }
    return lnode_p->getArrayBool (rownr) == rnode_p->getArrayBool (rownr);
}

TableExprNodeArrayEQDouble::TableExprNodeArrayEQDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtEQ)
{}
TableExprNodeArrayEQDouble::~TableExprNodeArrayEQDouble()
{}
Array<Bool> TableExprNodeArrayEQDouble::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (rownr) == rnode_p->getDouble (rownr);
    case ScaArr:
	return lnode_p->getDouble (rownr) == rnode_p->getArrayDouble (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble (rownr) == rnode_p->getArrayDouble (rownr);
}

TableExprNodeArrayEQDComplex::TableExprNodeArrayEQDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtEQ)
{}
TableExprNodeArrayEQDComplex::~TableExprNodeArrayEQDComplex()
{}
Array<Bool> TableExprNodeArrayEQDComplex::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex(rownr) == rnode_p->getDComplex(rownr);
    case ScaArr:
	return lnode_p->getDComplex(rownr) == rnode_p->getArrayDComplex(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex(rownr) == rnode_p->getArrayDComplex(rownr);
}

TableExprNodeArrayEQString::TableExprNodeArrayEQString
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtEQ)
{}
TableExprNodeArrayEQString::~TableExprNodeArrayEQString()
{}
Array<Bool> TableExprNodeArrayEQString::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayString (rownr) == rnode_p->getString (rownr);
    case ScaArr:
	return lnode_p->getString (rownr) == rnode_p->getArrayString (rownr);
    default:
	break;
    }
    return lnode_p->getArrayString (rownr) == rnode_p->getArrayString (rownr);
}

TableExprNodeArrayEQRegex::TableExprNodeArrayEQRegex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtEQ)
{}
TableExprNodeArrayEQRegex::~TableExprNodeArrayEQRegex()
{}
Array<Bool> TableExprNodeArrayEQRegex::getArrayBool (uInt rownr)
{
    Bool deleteArr, deleteRes;
    Array<String> left = lnode_p->getArrayString(rownr);
    const String* arr = left.getStorage (deleteArr);
    Array<Bool> result(left.shape());
    Bool* res = result.getStorage (deleteRes);
    Regex regex = rnode_p->getRegex(rownr);
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	res[i] = arr[i].matches(regex)  ?  True : False;
    }
    left.freeStorage (arr, deleteArr);
    result.putStorage (res, deleteRes);
    return result;
}

TableExprNodeArrayEQDate::TableExprNodeArrayEQDate
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtEQ)
{}
TableExprNodeArrayEQDate::~TableExprNodeArrayEQDate()
{}
Array<Bool> TableExprNodeArrayEQDate::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDate(rownr) != rnode_p->getDate(rownr);
    case ScaArr:
	return lnode_p->getDate(rownr) != rnode_p->getArrayDate(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDate(rownr) != rnode_p->getArrayDate(rownr);
}


TableExprNodeArrayNEBool::TableExprNodeArrayNEBool
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtNE)
{}
TableExprNodeArrayNEBool::~TableExprNodeArrayNEBool()
{}
Array<Bool> TableExprNodeArrayNEBool::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayBool (rownr) != rnode_p->getBool (rownr);
    case ScaArr:
	return lnode_p->getBool (rownr) != rnode_p->getArrayBool (rownr);
    default:
	break;
    }
    return lnode_p->getArrayBool (rownr) != rnode_p->getArrayBool (rownr);
}

TableExprNodeArrayNEDouble::TableExprNodeArrayNEDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtNE)
{}
TableExprNodeArrayNEDouble::~TableExprNodeArrayNEDouble()
{}
Array<Bool> TableExprNodeArrayNEDouble::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble (rownr) != rnode_p->getDouble (rownr);
    case ScaArr:
	return lnode_p->getDouble (rownr) != rnode_p->getArrayDouble (rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble (rownr) != rnode_p->getArrayDouble (rownr);
}

TableExprNodeArrayNEDComplex::TableExprNodeArrayNEDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtNE)
{}
TableExprNodeArrayNEDComplex::~TableExprNodeArrayNEDComplex()
{}
Array<Bool> TableExprNodeArrayNEDComplex::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex(rownr) != rnode_p->getDComplex(rownr);
    case ScaArr:
	return lnode_p->getDComplex(rownr) != rnode_p->getArrayDComplex(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex(rownr) != rnode_p->getArrayDComplex(rownr);
}

TableExprNodeArrayNEString::TableExprNodeArrayNEString
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtNE)
{}
TableExprNodeArrayNEString::~TableExprNodeArrayNEString()
{}
Array<Bool> TableExprNodeArrayNEString::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayString (rownr) != rnode_p->getString (rownr);
    case ScaArr:
	return lnode_p->getString (rownr) != rnode_p->getArrayString (rownr);
    default:
	break;
    }
    return lnode_p->getArrayString (rownr) != rnode_p->getArrayString (rownr);
}

TableExprNodeArrayNERegex::TableExprNodeArrayNERegex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtNE)
{}
TableExprNodeArrayNERegex::~TableExprNodeArrayNERegex()
{}
Array<Bool> TableExprNodeArrayNERegex::getArrayBool (uInt rownr)
{
    Bool deleteArr, deleteRes;
    Array<String> left = lnode_p->getArrayString(rownr);
    const String* arr = left.getStorage (deleteArr);
    Array<Bool> result(left.shape());
    Bool* res = result.getStorage (deleteRes);
    Regex regex = rnode_p->getRegex(rownr);
    uInt n = left.nelements();
    for (uInt i=0; i<n; i++) {
	res[i] = arr[i].matches(regex)  ?  False : True;
    }
    left.freeStorage (arr, deleteArr);
    result.putStorage (res, deleteRes);
    return result;
}

TableExprNodeArrayNEDate::TableExprNodeArrayNEDate
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtNE)
{}
TableExprNodeArrayNEDate::~TableExprNodeArrayNEDate()
{}
Array<Bool> TableExprNodeArrayNEDate::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDate(rownr) != rnode_p->getDate(rownr);
    case ScaArr:
	return lnode_p->getDate(rownr) != rnode_p->getArrayDate(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDate(rownr) != rnode_p->getArrayDate(rownr);
}


TableExprNodeArrayGTDouble::TableExprNodeArrayGTDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGT)
{}
TableExprNodeArrayGTDouble::~TableExprNodeArrayGTDouble()
{}
Array<Bool> TableExprNodeArrayGTDouble::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble(rownr) > rnode_p->getDouble(rownr);
    case ScaArr:
	return lnode_p->getDouble(rownr) > rnode_p->getArrayDouble(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble(rownr) > rnode_p->getArrayDouble(rownr);
}

TableExprNodeArrayGTDComplex::TableExprNodeArrayGTDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGT)
{}
TableExprNodeArrayGTDComplex::~TableExprNodeArrayGTDComplex()
{}
Array<Bool> TableExprNodeArrayGTDComplex::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex(rownr) > rnode_p->getDComplex(rownr);
    case ScaArr:
	return lnode_p->getDComplex(rownr) > rnode_p->getArrayDComplex(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex(rownr) > rnode_p->getArrayDComplex(rownr);
}

TableExprNodeArrayGTString::TableExprNodeArrayGTString
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGT)
{}
TableExprNodeArrayGTString::~TableExprNodeArrayGTString()
{}
Array<Bool> TableExprNodeArrayGTString::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayString(rownr) > rnode_p->getString(rownr);
    case ScaArr:
	return lnode_p->getString(rownr) > rnode_p->getArrayString(rownr);
    default:
	break;
    }
    return lnode_p->getArrayString(rownr) > rnode_p->getArrayString(rownr);
}

TableExprNodeArrayGTDate::TableExprNodeArrayGTDate
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGT)
{}
TableExprNodeArrayGTDate::~TableExprNodeArrayGTDate()
{}
Array<Bool> TableExprNodeArrayGTDate::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDate(rownr) > rnode_p->getDate(rownr);
    case ScaArr:
	return lnode_p->getDate(rownr) > rnode_p->getArrayDate(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDate(rownr) > rnode_p->getArrayDate(rownr);
}


TableExprNodeArrayGEDouble::TableExprNodeArrayGEDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGE)
{}
TableExprNodeArrayGEDouble::~TableExprNodeArrayGEDouble()
{}
Array<Bool> TableExprNodeArrayGEDouble::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDouble(rownr) >= rnode_p->getDouble(rownr);
    case ScaArr:
	return lnode_p->getDouble(rownr) >= rnode_p->getArrayDouble(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDouble(rownr) >= rnode_p->getArrayDouble(rownr);
}

TableExprNodeArrayGEDComplex::TableExprNodeArrayGEDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGE)
{}
TableExprNodeArrayGEDComplex::~TableExprNodeArrayGEDComplex()
{}
Array<Bool> TableExprNodeArrayGEDComplex::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDComplex(rownr) >= rnode_p->getDComplex(rownr);
    case ScaArr:
	return lnode_p->getDComplex(rownr) >= rnode_p->getArrayDComplex(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDComplex(rownr) >= rnode_p->getArrayDComplex(rownr);
}

TableExprNodeArrayGEString::TableExprNodeArrayGEString
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGE)
{}
TableExprNodeArrayGEString::~TableExprNodeArrayGEString()
{}
Array<Bool> TableExprNodeArrayGEString::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayString(rownr) >= rnode_p->getString(rownr);
    case ScaArr:
	return lnode_p->getString(rownr) >= rnode_p->getArrayString(rownr);
    default:
	break;
    }
    return lnode_p->getArrayString(rownr) >= rnode_p->getArrayString(rownr);
}

TableExprNodeArrayGEDate::TableExprNodeArrayGEDate
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtGE)
{}
TableExprNodeArrayGEDate::~TableExprNodeArrayGEDate()
{}
Array<Bool> TableExprNodeArrayGEDate::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayDate(rownr) >= rnode_p->getDate(rownr);
    case ScaArr:
	return lnode_p->getDate(rownr) >= rnode_p->getArrayDate(rownr);
    default:
	break;
    }
    return lnode_p->getArrayDate(rownr) >= rnode_p->getArrayDate(rownr);
}


TableExprNodeArrayINDouble::TableExprNodeArrayINDouble
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtIN)
{}
TableExprNodeArrayINDouble::~TableExprNodeArrayINDouble()
{}
Array<Bool> TableExprNodeArrayINDouble::getArrayBool (uInt rownr)
{
    return rnode_p->hasArrayDouble (rownr, lnode_p->getArrayDouble (rownr));
}

TableExprNodeArrayINDComplex::TableExprNodeArrayINDComplex
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtIN)
{}
TableExprNodeArrayINDComplex::~TableExprNodeArrayINDComplex()
{}
Array<Bool> TableExprNodeArrayINDComplex::getArrayBool (uInt rownr)
{
    return rnode_p->hasArrayDComplex (rownr, lnode_p->getArrayDComplex (rownr));
}

TableExprNodeArrayINString::TableExprNodeArrayINString
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtIN)
{}
TableExprNodeArrayINString::~TableExprNodeArrayINString()
{}
Array<Bool> TableExprNodeArrayINString::getArrayBool (uInt rownr)
{
    return rnode_p->hasArrayString (rownr, lnode_p->getArrayString (rownr));
}

TableExprNodeArrayINDate::TableExprNodeArrayINDate
                                            (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtIN)
{}
TableExprNodeArrayINDate::~TableExprNodeArrayINDate()
{}
Array<Bool> TableExprNodeArrayINDate::getArrayBool (uInt rownr)
{
    return rnode_p->hasArrayDate (rownr, lnode_p->getArrayDate (rownr));
}


TableExprNodeArrayConstBool::TableExprNodeArrayConstBool
                                                 (const Array<Bool>& val)
: TableExprNodeArray (NTBool, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstBool::~TableExprNodeArrayConstBool()
{}
Array<Bool> TableExprNodeArrayConstBool::getArrayBool (uInt)
    { return value_p; }


TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<Double>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<Float>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<uInt>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<Int>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<uShort>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<Short>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<uChar>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDouble::~TableExprNodeArrayConstDouble()
{}
Array<Double> TableExprNodeArrayConstDouble::getArrayDouble (uInt)
    { return value_p; }
Array<DComplex> TableExprNodeArrayConstDouble::getArrayDComplex (uInt)
{
    Array<DComplex> arr(value_p.shape());
    convertArray (arr, value_p);
    return arr;
}

TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const Array<DComplex>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const Array<Complex>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const Array<Double>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape()),
  value_p            (val.shape())
{
    convertArray (value_p, val);
}
TableExprNodeArrayConstDComplex::~TableExprNodeArrayConstDComplex()
{}
Array<DComplex> TableExprNodeArrayConstDComplex::getArrayDComplex (uInt)
    { return value_p; }

TableExprNodeArrayConstString::TableExprNodeArrayConstString
                                                 (const Array<String>& val)
: TableExprNodeArray (NTString, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstString::~TableExprNodeArrayConstString()
{}
Array<String> TableExprNodeArrayConstString::getArrayString (uInt)
    { return value_p; }

TableExprNodeArrayConstDate::TableExprNodeArrayConstDate
                                                 (const Array<MVTime>& val)
: TableExprNodeArray (NTDate, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDate::~TableExprNodeArrayConstDate()
{}
Array<Double> TableExprNodeArrayConstDate::getArrayDouble (uInt)
{
    Array<Double> arr(value_p.shape());
    convertArray (arr, value_p);
    return arr;
}
Array<MVTime> TableExprNodeArrayConstDate::getArrayDate (uInt)
    { return value_p; }


TableExprNodeArrayOR::TableExprNodeArrayOR (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtOR)
{}
TableExprNodeArrayOR::~TableExprNodeArrayOR()
{}
Array<Bool> TableExprNodeArrayOR::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayBool(rownr) || rnode_p->getBool(rownr);
    case ScaArr:
	return lnode_p->getBool(rownr) || rnode_p->getArrayBool(rownr);
    default:
	break;
    }
    return lnode_p->getArrayBool(rownr) || rnode_p->getArrayBool(rownr);
}


TableExprNodeArrayAND::TableExprNodeArrayAND (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtAND)
{}
TableExprNodeArrayAND::~TableExprNodeArrayAND()
{}
Array<Bool> TableExprNodeArrayAND::getArrayBool (uInt rownr)
{
    switch (argtype_p) {
    case ArrSca:
	return lnode_p->getArrayBool(rownr) && rnode_p->getBool(rownr);
    case ScaArr:
	return lnode_p->getBool(rownr) && rnode_p->getArrayBool(rownr);
    default:
	break;
    }
    return lnode_p->getArrayBool(rownr) && rnode_p->getArrayBool(rownr);
}


TableExprNodeArrayNOT::TableExprNodeArrayNOT (const TableExprNodeRep& node)
: TableExprNodeArray (node, NTBool, OtNOT)
{}
TableExprNodeArrayNOT::~TableExprNodeArrayNOT()
{}
Array<Bool> TableExprNodeArrayNOT::getArrayBool (uInt rownr)
{
    return !(lnode_p->getArrayBool(rownr));
}


TableExprNodeArrayMIN::TableExprNodeArrayMIN (const TableExprNodeRep& node)
: TableExprNodeArray (node, node.dataType(), OtMIN)
{}
TableExprNodeArrayMIN::~TableExprNodeArrayMIN()
{}
Array<Double> TableExprNodeArrayMIN::getArrayDouble (uInt rownr)
    { return -(lnode_p->getArrayDouble(rownr)); }
Array<DComplex> TableExprNodeArrayMIN::getArrayDComplex (uInt rownr)
    { return -(lnode_p->getArrayDComplex(rownr)); }
