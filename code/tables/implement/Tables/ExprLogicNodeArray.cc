//# ExprLogicNodeArray.cc: Nodes representing logical array operators in table select expression tree
//# Copyright (C) 1997,1998,1999
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

#include <aips/Tables/ExprLogicNodeArray.h>
#include <aips/Tables/TableError.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Quanta/MVTime.h>



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
