//# ExprFuncNode.cc: Class representing a function in table select expression
//# Copyright (C) 1994,1995,1996,1997,1998,2000
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
#include <aips/Tables/TableError.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprNodeSet.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Quanta/MVTime.h>
#include <aips/OS/Time.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>

typedef Quantum<Double> ExprFuncNode_gpp_bug1;
typedef Vector<Int> ExprFuncNode_gpp_bug2;


TableExprFuncNode::TableExprFuncNode (FunctionType ftype, NodeDataType dtype,
				      ValueType vtype,
				      const TableExprNodeSet& source)
: TableExprNodeMulti (dtype, vtype, OtFunc, source),
  funcType_p         (ftype),
  argDataType_p      (dtype)
{}

TableExprFuncNode::~TableExprFuncNode()
{}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
// When one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprFuncNode::fillNode
                                   (TableExprFuncNode* thisNode,
				    PtrBlock<TableExprNodeRep*>& nodes,
				    const Block<Int>& dtypeOper)
{
    uInt i;
    // Copy block of children.
    // Determine if common argument type is Double or Complex.
    // (this is used by some functions like near and norm).
    thisNode->operands_p.resize (nodes.nelements());
    thisNode->argDataType_p = NTDouble;
    for (i=0; i<nodes.nelements(); i++) {
	thisNode->operands_p[i] = nodes[i]->link();
	if (nodes[i]->dataType() == NTComplex) {
	    thisNode->argDataType_p = NTComplex;
	}
    }
    // Convert String to Date if needed
    for (i=0; i<nodes.nelements(); i++) {
	if (nodes[i]->dataType() == NTString  &&  dtypeOper[i] == NTDate) {
	    TableExprNode dNode = datetime (thisNode->operands_p[i]);
	    unlink (thisNode->operands_p[i]);
	    thisNode->operands_p[i] = getRep (dNode)->link();
	}
    }
    // Some functions on a variable can already give a constant result.
    thisNode->tryToConst();
    if (thisNode->operands_p.nelements() > 0) {
	return convertNode (thisNode, True);
    }
    return thisNode;
}

void TableExprFuncNode::tryToConst()
{
    switch (funcType_p) {
    case ndimFUNC:
	if (operands_p[0]->ndim() >= 0) {
	    exprtype_p = Constant;
	}
	break;
    case shapeFUNC:
    case nelemFUNC:
    case isdefFUNC:
	if (operands_p[0]->ndim() == 0
        ||  operands_p[0]->shape().nelements() > 0  ) {
	    exprtype_p = Constant;
	}
	break;
    default:
	break;
    }
}

Bool TableExprFuncNode::getBool (const TableExprId& id)
{
    switch (funcType_p) {
    case anyFUNC:
	return anyEQ (operands_p[0]->getArrayBool(id), True);
    case allFUNC:
	return allEQ (operands_p[0]->getArrayBool(id), True);
    case isdefFUNC:
	return operands_p[0]->isDefined (id);
    case near2FUNC:
	if (argDataType_p == NTDouble) {
	    return near (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id),
			 1.0e-13);
	}
	return near (operands_p[0]->getDComplex(id),
		     operands_p[1]->getDComplex(id),
		     1.0e-13);
    case near3FUNC:
	if (argDataType_p == NTDouble) {
	    return near (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id),
			 operands_p[2]->getDouble(id));
	}
	return near (operands_p[0]->getDComplex(id),
		     operands_p[1]->getDComplex(id),
		     operands_p[2]->getDouble(id));
    case nearabs2FUNC:
	if (argDataType_p == NTDouble) {
	    return nearAbs (operands_p[0]->getDouble(id),
			    operands_p[1]->getDouble(id),
			    1.0e-13);
	}
	return nearAbs (operands_p[0]->getDComplex(id),
			operands_p[1]->getDComplex(id),
			1.0e-13);
    case nearabs3FUNC:
	if (argDataType_p == NTDouble) {
	    return nearAbs (operands_p[0]->getDouble(id),
			    operands_p[1]->getDouble(id),
			    operands_p[2]->getDouble(id));
	}
	return nearAbs (operands_p[0]->getDComplex(id),
			operands_p[1]->getDComplex(id),
			operands_p[2]->getDouble(id));
    default:
	throw (TableInvExpr ("TableExprFuncNode::getBool, "
			     "unknown function"));
    }
    return True;
}

Double TableExprFuncNode::getDouble (const TableExprId& id)
{
    switch(funcType_p) {
    case piFUNC:
	return C::pi;
    case eFUNC:
	return C::e;
    case sinFUNC:
	return sin      (operands_p[0]->getDouble(id));
    case sinhFUNC:
	return sinh     (operands_p[0]->getDouble(id));
    case cosFUNC:
	return cos      (operands_p[0]->getDouble(id));
    case coshFUNC:
	return cosh     (operands_p[0]->getDouble(id));
    case expFUNC:
	return exp      (operands_p[0]->getDouble(id));
    case logFUNC:
	return log      (operands_p[0]->getDouble(id));
    case log10FUNC:
	return log10    (operands_p[0]->getDouble(id));
    case powFUNC:
	return pow      (operands_p[0]->getDouble(id),
		         operands_p[1]->getDouble(id));
    case squareFUNC:
	{
	    Double val = operands_p[0]->getDouble(id);
	    return val * val;
	}
    case sqrtFUNC:
	return sqrt     (operands_p[0]->getDouble(id));
    case conjFUNC:
	return           operands_p[0]->getDouble(id);
    case minFUNC:
	return min (operands_p[0]->getDouble(id),
		    operands_p[1]->getDouble(id));
    case maxFUNC:
	return max (operands_p[0]->getDouble(id),
		    operands_p[1]->getDouble(id));
    case normFUNC:
	if (argDataType_p == NTDouble) {
	    Double val = operands_p[0]->getDouble(id);
	    return val*val;
	}
	return norm (operands_p[0]->getDComplex(id));
    case absFUNC:
	if (argDataType_p == NTDouble) {
	    return abs (operands_p[0]->getDouble(id));
	}
	return abs (operands_p[0]->getDComplex(id));
    case argFUNC:
	if (argDataType_p == NTDouble) {
	    if (operands_p[0]->getDouble(id) >= 0) {
		return 0;
	    }
	    return atan2 (Double(0), Double(-1));  // results in pi
	}
	return arg (operands_p[0]->getDComplex(id));
    case realFUNC:
	if (argDataType_p == NTDouble) {
	    return operands_p[0]->getDouble(id);
	}
	return operands_p[0]->getDComplex(id).real();
    case imagFUNC:
	if (argDataType_p == NTDouble) {
	    return 0;
	}
	return operands_p[0]->getDComplex(id).imag();
    case asinFUNC:
	return asin     (operands_p[0]->getDouble(id));
    case acosFUNC:
	return acos     (operands_p[0]->getDouble(id));
    case atanFUNC:
	return atan     (operands_p[0]->getDouble(id));
    case tanFUNC:
	return tan      (operands_p[0]->getDouble(id));
    case tanhFUNC:
	return tanh     (operands_p[0]->getDouble(id));
    case atan2FUNC:
	return atan2    (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id));
    case signFUNC:
	{
	    Double val = operands_p[0]->getDouble(id);
	    if (val > 0) {
		return 1;
	    }
	    if (val < 0) {
		return -1;
	    }
	    return 0;
	}
    case roundFUNC:
	{
	    Double val = operands_p[0]->getDouble(id);
	    if (val < 0) {
		return ceil (val - 0.5);
	    }
	    return floor (val + 0.5);
	}
    case floorFUNC:
	return floor    (operands_p[0]->getDouble(id));
    case ceilFUNC:
	return ceil     (operands_p[0]->getDouble(id));
    case fmodFUNC:
	return fmod     (operands_p[0]->getDouble(id),
			 operands_p[1]->getDouble(id));
    case strlengthFUNC:
	return operands_p[0]->getString (id).length();
    case datetimeFUNC:
    case mjdtodateFUNC:
    case dateFUNC:
        return Double (getDate(id));
    case mjdFUNC:
	return operands_p[0]->getDate(id).day();
    case yearFUNC:
	return operands_p[0]->getDate(id).year();
    case monthFUNC:
	return operands_p[0]->getDate(id).month();
    case dayFUNC:
	return operands_p[0]->getDate(id).monthday();
    case weekdayFUNC:
	return operands_p[0]->getDate(id).weekday();
    case weekFUNC:
	return operands_p[0]->getDate(id).yearweek();
    case timeFUNC:                                       //# return in radians
	return fmod (Double(operands_p[0]->getDate(id)), 1.) * C::_2pi;
    case arrminFUNC:
	return min (operands_p[0]->getArrayDouble (id));
    case arrmaxFUNC:
	return max (operands_p[0]->getArrayDouble (id));
    case arrsumFUNC:
	return sum (operands_p[0]->getArrayDouble (id));
    case arrproductFUNC:
	return product (operands_p[0]->getArrayDouble (id));
    case arrmeanFUNC:
	return mean (operands_p[0]->getArrayDouble (id));
    case arrvarianceFUNC:
	return variance (operands_p[0]->getArrayDouble (id));
    case arrstddevFUNC:
	return stddev (operands_p[0]->getArrayDouble (id));
    case arravdevFUNC:
	return avdev (operands_p[0]->getArrayDouble (id));
    case arrmedianFUNC:
	return median (operands_p[0]->getArrayDouble (id));
    case ntrueFUNC:
    {
	Array<Bool> arr = operands_p[0]->getArrayBool (id);
	Bool deleteIt;
	const Bool* data = arr.getStorage (deleteIt);
	const Bool* p = data;
	const Bool* end = p + arr.nelements();
	uInt n = 0;
	while (p < end) {
	    if (*p++) {
		n++;
	    }
	}
	arr.freeStorage (data, deleteIt);
	return n;
    }
    case nfalseFUNC:
    {
	Array<Bool> arr = operands_p[0]->getArrayBool (id);
	Bool deleteIt;
	const Bool* data = arr.getStorage (deleteIt);
	const Bool* p = data;
	const Bool* end = p + arr.nelements();
	uInt n = 0;
	while (p < end) {
	    if (! *p++) {
		n++;
	    }
	}
	arr.freeStorage (data, deleteIt);
	return n;
    }
    case ndimFUNC:
    {
	// Return fixed dimensionality if available.
	Int nrdim = operands_p[0]->ndim();
	return (nrdim >= 0  ?  nrdim : operands_p[0]->shape(id).nelements());
    }
    case nelemFUNC:
	return (operands_p[0]->valueType() == VTScalar  ?
                                   1 : operands_p[0]->shape(id).product());
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDouble, "
			     "unknown function"));
    }
    return 0;
}

DComplex TableExprFuncNode::getDComplex (const TableExprId& id)
{
    if (dataType() == NTDouble) {
	return TableExprFuncNode::getDouble (id);
    }
    switch (funcType_p) {
    case sinFUNC:
	return sin      (operands_p[0]->getDComplex(id));
    case sinhFUNC:
	return sinh     (operands_p[0]->getDComplex(id));
    case cosFUNC:
	return cos      (operands_p[0]->getDComplex(id));
    case coshFUNC:
	return cosh     (operands_p[0]->getDComplex(id));
    case expFUNC:
	return exp      (operands_p[0]->getDComplex(id));
    case logFUNC:
	return log      (operands_p[0]->getDComplex(id));
    case log10FUNC:
	return log10    (operands_p[0]->getDComplex(id));
    case powFUNC:
	return pow      (operands_p[0]->getDComplex(id),
		         operands_p[1]->getDComplex(id));
    case squareFUNC:
	{
	    DComplex val = operands_p[0]->getDComplex(id);
	    return val * val;
	}
    case sqrtFUNC:
	return sqrt     (operands_p[0]->getDComplex(id));
    case conjFUNC:
	return conj     (operands_p[0]->getDComplex(id));
    case minFUNC:
	{
	    DComplex val0(operands_p[0]->getDComplex (id));
	    DComplex val1(operands_p[1]->getDComplex (id));
	    if (val0 > val1) {
		return val1;
	    }
	    return val0;
	}
    case maxFUNC:
	{
	    DComplex val0(operands_p[0]->getDComplex (id));
	    DComplex val1(operands_p[1]->getDComplex (id));
	    if (val0 < val1) {
		return val1;
	    }
	    return val0;
	}
    case complexFUNC:
	return DComplex (operands_p[0]->getDouble (id),
			 operands_p[1]->getDouble (id));
    case arrsumFUNC:
	return sum (operands_p[0]->getArrayDComplex (id));
    case arrproductFUNC:
	return product (operands_p[0]->getArrayDComplex (id));
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDComplex, "
			     "unknown function"));
    }
    return DComplex(0., 0.);
}

String TableExprFuncNode::getString (const TableExprId& id)
{
    switch (funcType_p) {
    case upcaseFUNC:
	{
	    String str = operands_p[0]->getString (id);
	    str.upcase();
	    return str;
	}
    case downcaseFUNC:
	{
	    String str = operands_p[0]->getString (id);
	    str.downcase();
	    return str;
	}
    case trimFUNC:
	{
	    String str = operands_p[0]->getString (id);
	    Int pos = str.length();
	    while (--pos >= 0  &&  str[pos] == ' ' ) ;
	    if (pos < 0) {
		return "";
	    } else if (pos+1 < Int(str.length())) {
		return str.through(pos);
	    }
	    return str;
	}
    case cmonthFUNC:
	return operands_p[0]->getDate(id).monthName();
    case cdowFUNC:
	return operands_p[0]->getDate(id).dayName();
    default:
	throw (TableInvExpr ("TableExprFuncNode::getString, "
			     "unknown function"));
    }
    return "";
}

Regex TableExprFuncNode::getRegex (const TableExprId& id)
{
    switch (funcType_p) {
    case regexFUNC:
	return Regex(operands_p[0]->getString (id));
    case patternFUNC:
	return Regex(Regex::fromPattern(operands_p[0]->getString (id)));
    default:
	throw (TableInvExpr ("TableExprFuncNode::getRegex, "
			     "unknown function"));
    }
    return Regex("");
}

MVTime TableExprFuncNode::getDate (const TableExprId& id)
{
    switch (funcType_p) {
    case datetimeFUNC:
	{
	    Quantity quant;
	    if (MVTime::read (quant, operands_p[0]->getString(id))) {
		return quant;
	    }
	    throw (TableInvExpr ("invalid date string " +
				 operands_p[0]->getString(id)));
	}
    case mjdtodateFUNC:
	return MVTime (operands_p[0]->getDouble(id));
    case dateFUNC:
	return MVTime (floor (Double (operands_p[0]->getDate(id))));
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDate, "
			     "unknown function"));
    }
    return MVTime();
}

Array<Double> TableExprFuncNode::getArrayDouble (const TableExprId& id)
{
    switch (funcType_p) {
    case shapeFUNC:
    {
	Array<Int> shp = operands_p[0]->shape(id).asVector();
	Array<Double> result(shp.shape());
	convertArray (result, shp);
	return result;
    }
    case strlengthFUNC:
    {
	Array<String> values = operands_p[0]->getArrayString(id);
	Array<Double> doubles(values.shape());
	Bool deleteVal, deleteDoub;
	const String* val = values.getStorage (deleteVal);
	Double* doub = doubles.getStorage (deleteDoub);
	uInt n = values.nelements();
	for (uInt i=0; i<n; i++) {
	    doub[i] = val[i].length();
	}
	values.freeStorage (val, deleteVal);
	doubles.putStorage (doub, deleteDoub);
	return doubles;
    }
    case mjdFUNC:
    case yearFUNC:
    case monthFUNC:
    case dayFUNC:
    case weekdayFUNC:
    case weekFUNC:
    case timeFUNC:
    {
	Array<MVTime> values = operands_p[0]->getArrayDate(id);
	Array<Double> doubles(values.shape());
	Bool deleteVal, deleteDoub;
	const MVTime* val = values.getStorage (deleteVal);
	Double* doub = doubles.getStorage (deleteDoub);
	uInt n = values.nelements();
	uInt i;
	switch (funcType_p) {
	case mjdFUNC:
	    for (i=0; i<n; i++) {
		doub[i] = val[i].day();
	    }
	    break;
	case yearFUNC:
	    for (i=0; i<n; i++) {
		doub[i] = val[i].year();
	    }
	    break;
	case monthFUNC:
	    for (i=0; i<n; i++) {
		doub[i] = val[i].month();
	    }
	    break;
	case dayFUNC:
	    for (i=0; i<n; i++) {
		doub[i] = val[i].monthday();
	    }
	    break;
	case weekdayFUNC:
	    for (i=0; i<n; i++) {
		doub[i] = val[i].weekday();
	    }
	    break;
	case weekFUNC:
	    for (i=0; i<n; i++) {
		doub[i] = val[i].yearweek();
	    }
	    break;
	case timeFUNC:                                   //# return in radians
	    for (i=0; i<n; i++) {
		doub[i] = fmod (Double(val[i]), 1.) * C::_2pi;
	    }
	    break;
	default:
	    break;
	}
	values.freeStorage (val, deleteVal);
	doubles.putStorage (doub, deleteDoub);
	return doubles;
    }
    default:
	throw (TableInvExpr ("TableExprFuncNode::getArrayDouble, "
			     "unknown function"));
    }
    return Array<Double>();
}

Array<String> TableExprFuncNode::getArrayString (const TableExprId& id)
{
    switch (funcType_p) {
    case upcaseFUNC:
    case downcaseFUNC:
    case trimFUNC:
    {
	Array<String> strings = operands_p[0]->getArrayString(id);
	Bool deleteStr;
	String* str = strings.getStorage (deleteStr);
	uInt n = strings.nelements();
	uInt i;
	switch (funcType_p) {
	case upcaseFUNC:
	    for (i=0; i<n; i++) {
		str[i].upcase();
	    }
	    break;
	case downcaseFUNC:
	    for (i=0; i<n; i++) {
		str[i].downcase();
	    }
	    break;
	case trimFUNC:
	    for (i=0; i<n; i++) {
		String& s = str[i];
		Int pos = s.length();
		while (--pos >= 0  &&  s[pos] == ' ' ) ;
		if (pos < 0) {
		    s = "";
		} else if (pos+1 < Int(s.length())) {
		    s = s.through(pos);
		}
	    }
	    break;
	default:
	    break;
	}
	strings.putStorage (str, deleteStr);
	return strings;
	break;
    }
    case cmonthFUNC:
    case cdowFUNC:	
    {
	Array<MVTime> values = operands_p[0]->getArrayDate(id);
	Array<String> strings(values.shape());
	Bool deleteVal, deleteStr;
	const MVTime* val = values.getStorage (deleteVal);
	String* str = strings.getStorage (deleteStr);
	uInt n = values.nelements();
	uInt i;
	switch (funcType_p) {
	case cmonthFUNC:
	    for (i=0; i<n; i++) {
		str[i] = val[i].monthName();
	    }
	    break;
	case cdowFUNC:	
	    for (i=0; i<n; i++) {
		str[i] = val[i].dayName();
	    }
	    break;
	default:
	    break;
	}
	values.freeStorage (val, deleteVal);
	strings.putStorage (str, deleteStr);
	return strings;
    }
    default:
	throw (TableInvExpr ("TableExprFuncNode::getArrayDouble, "
			     "unknown function"));
    }
    return Array<String>();
}

Array<MVTime> TableExprFuncNode::getArrayDate (const TableExprId& id)
{
    switch (funcType_p) {
    case datetimeFUNC:
    {
	Array<String> values = operands_p[0]->getArrayString(id);
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const String* val = values.getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	Quantity quant;
	uInt n = values.nelements();
	for (uInt i=0; i<n; i++) {
	    if (MVTime::read (quant, val[i])) {
		dat[i] = quant;
	    }
	    throw (TableInvExpr ("invalid date string " + val[i]));
	}
	values.freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return dates;
    }
    case mjdtodateFUNC:
    {
	Array<Double> values = operands_p[0]->getArrayDouble(id);
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const Double* val = values.getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	uInt n = values.nelements();
	for (uInt i=0; i<n; i++) {
	    dat[i] = MVTime (val[i]);
	}
	values.freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return dates;
    }
    case dateFUNC:
    {
	Array<MVTime> values = operands_p[0]->getArrayDate(id);
	Array<MVTime> dates(values.shape());
	Bool deleteVal, deleteDat;
	const MVTime* val = values.getStorage (deleteVal);
	MVTime* dat = dates.getStorage (deleteDat);
	uInt n = values.nelements();
	for (uInt i=0; i<n; i++) {
	    dat[i] = MVTime (floor (Double (val[i])));
	}
	values.freeStorage (val, deleteVal);
	dates.putStorage (dat, deleteDat);
	return dates;
    }
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDate, "
			     "unknown function"));
    }
    return Array<MVTime>();
}


TableExprNodeRep::NodeDataType TableExprFuncNode::checkOperands
                                 (Block<Int>& dtypeOper,
				  ValueType& resVT, Block<Int>&,
				  FunctionType fType,
				  PtrBlock<TableExprNodeRep*>& nodes)
{
    uInt i;
    // The default returned value type is a scalar.
    resVT = VTScalar;
    // The following functions accept an array argument only.
    switch (fType) {
    case arrminFUNC:
    case arrmaxFUNC:
    case arrmeanFUNC:
    case arrvarianceFUNC:
    case arrstddevFUNC:
    case arravdevFUNC:
    case arrmedianFUNC:
	checkNumOfArg (1, 1, nodes);
	if (nodes[0]->valueType() != VTArray) {
	    throw (TableInvExpr ("Function has to have an array argument"));
	}
	return checkDT (dtypeOper, NTDouble, NTDouble, nodes);
    case arrsumFUNC:
    case arrproductFUNC:
	checkNumOfArg (1, 1, nodes);
	if (nodes[0]->valueType() != VTArray) {
	    throw (TableInvExpr ("Function has to have an array argument"));
	}
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case anyFUNC:
    case allFUNC:
	checkNumOfArg (1, 1, nodes);
	if (nodes[0]->valueType() != VTArray) {
	    throw (TableInvExpr ("Function has to have an array argument"));
	}
	return checkDT (dtypeOper, NTBool, NTBool, nodes);
    case ntrueFUNC:
    case nfalseFUNC:
	checkNumOfArg (1, 1, nodes);
	if (nodes[0]->valueType() != VTArray) {
	    throw (TableInvExpr ("Function has to have an array argument"));
	}
	return checkDT (dtypeOper, NTBool, NTDouble, nodes);
    case nelemFUNC:
    case ndimFUNC:
    case shapeFUNC:
	checkNumOfArg (1, 1, nodes);
	if (fType == shapeFUNC) {
	    resVT = VTArray;
	}
	return checkDT (dtypeOper, NTAny, NTDouble, nodes);
    case isdefFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTAny, NTBool, nodes);
    default:
	break;
    }
    // The following functions accept scalars and arrays.
    // The return a scalar or array (same as the input argument).
    // When a function has no argument, it results in a scalar.
    for (i=0; i< nodes.nelements(); i++) {
	resVT = nodes[i]->valueType();
	if (resVT != VTScalar  &&  resVT != VTArray) {
	    throw (TableInvExpr
                          ("Function has to have a scalar or array argument"));
	}
    }
    switch (fType) {
    case strlengthFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTDouble, nodes);
    case upcaseFUNC:
    case downcaseFUNC:
    case trimFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTString, nodes);
    case datetimeFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTString, NTDate, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTString;
	nodes.resize(1);
	nodes[0] = new TableExprNodeConstString ("today");
	return NTDate;
    case mjdtodateFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTDouble, NTDate, nodes);
    case dateFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTDate, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTDate;
    case mjdFUNC:
    case yearFUNC:
    case monthFUNC:
    case dayFUNC:
    case weekdayFUNC:
    case weekFUNC:
    case timeFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTDouble, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTDouble;
    case cmonthFUNC:
    case cdowFUNC:
	if (checkNumOfArg (0, 1, nodes) == 1) {
	    return checkDT (dtypeOper, NTDate, NTString, nodes);
	}
	dtypeOper.resize (1);
	dtypeOper[0] = NTDate;
	nodes.resize (1);
	nodes[0] = new TableExprNodeConstDate (MVTime(Time()));
	return NTString;
    default:
	break;
    }
    // The following functions accept scalars only.
    // In the future they may also need to support arrays.
    for (i=0; i< nodes.nelements(); i++) {
	if (nodes[i]->valueType() != VTScalar) {
	    throw (TableInvExpr ("Function has to have a scalar argument"));
	}
    }
    switch (fType) {
    case randFUNC:
    case rownrFUNC:
    case piFUNC:
    case eFUNC:
	checkNumOfArg (0, 0, nodes);
	return NTDouble;
    case near2FUNC:
    case nearabs2FUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTNumeric, NTBool, nodes);
    case near3FUNC:
    case nearabs3FUNC:
	checkNumOfArg (3, 3, nodes);
	return checkDT (dtypeOper, NTNumeric, NTBool, nodes);
    case sinFUNC:
    case sinhFUNC:
    case cosFUNC:
    case coshFUNC:
    case expFUNC:
    case logFUNC:
    case log10FUNC:
    case squareFUNC:
    case sqrtFUNC:
    case conjFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case powFUNC:
    case minFUNC:
    case maxFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case normFUNC:
    case absFUNC:
    case argFUNC:
    case realFUNC:
    case imagFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTNumeric, NTDouble, nodes);
    case asinFUNC:
    case acosFUNC:
    case atanFUNC:
    case tanFUNC:
    case tanhFUNC:
    case signFUNC:
    case roundFUNC:
    case floorFUNC:
    case ceilFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTDouble, NTDouble, nodes);
    case atan2FUNC:
    case fmodFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTDouble, NTDouble, nodes);
    case complexFUNC:
	checkNumOfArg (2, 2, nodes);
	return checkDT (dtypeOper, NTDouble, NTComplex, nodes);
    case regexFUNC:
    case patternFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTRegex, nodes);
    default:
	throw (TableInvExpr ("TableExprFuncNode::checkOperands, "
			     "function not contained in switch statement"));
    }
    return NTNumeric;
}
