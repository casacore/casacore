//# ExprFuncNode.cc: Class representing a function in table select expression
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
#include <aips/Tables/TableError.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Measures/MVTime.h>
#include <aips/OS/Time.h>
#include <aips/Mathematics/Constants.h>

typedef Quantum<double> gpp_bug1;

TableExprFuncNode::TableExprFuncNode (FunctionType ftype, NodeDataType dtype)
: TableExprNodeMulti (dtype, OtFunc),
  funcType_p         (ftype)
{}

TableExprFuncNode::~TableExprFuncNode()
{}

Bool TableExprFuncNode::getBool (uInt rownr)
{
    switch (funcType_p) {
    default:
	throw (TableInvExpr ("TableExprFuncNode::getBool, "
			     "unknown function"));
    }
    return True;
}

Double TableExprFuncNode::getDouble (uInt rownr)
{
    switch(funcType_p) {
    case piFUNC:
	return C::pi;
    case eFUNC:
	return C::e;
    case sinFUNC:
	return sin      (operands_p[0]->getDouble(rownr));
    case sinhFUNC:
	return sinh     (operands_p[0]->getDouble(rownr));
    case cosFUNC:
	return cos      (operands_p[0]->getDouble(rownr));
    case coshFUNC:
	return cosh     (operands_p[0]->getDouble(rownr));
    case expFUNC:
	return exp      (operands_p[0]->getDouble(rownr));
    case logFUNC:
	return log      (operands_p[0]->getDouble(rownr));
    case log10FUNC:
	return log10    (operands_p[0]->getDouble(rownr));
    case powFUNC:
	return pow      (operands_p[0]->getDouble(rownr),
		         operands_p[1]->getDouble(rownr));
    case squareFUNC:
	{
	    double val = operands_p[0]->getDouble(rownr);
	    return val * val;
	}
    case sqrtFUNC:
	return sqrt     (operands_p[0]->getDouble(rownr));
    case conjFUNC:
	return           operands_p[0]->getDouble(rownr);
    case minFUNC:
	return min (operands_p[0]->getDouble(rownr),
		    operands_p[1]->getDouble(rownr));
    case maxFUNC:
	return max (operands_p[0]->getDouble(rownr),
		    operands_p[1]->getDouble(rownr));
    case normFUNC:
	if (operands_p[0]->dataType() == NTDouble) {
	    double val = operands_p[0]->getDouble(rownr);
	    return val*val;
	}
	return norm (operands_p[0]->getDComplex(rownr));
    case absFUNC:
	if (operands_p[0]->dataType() == NTDouble) {
	    return abs (operands_p[0]->getDouble(rownr));
	}
	return abs (operands_p[0]->getDComplex(rownr));
    case argFUNC:
	if (operands_p[0]->dataType() == NTDouble) {
	    if (operands_p[0]->getDouble(rownr) >= 0) {
		return 0;
	    }
	    return atan2 (double(0), double(-1));  // results in pi
	}
	return arg (operands_p[0]->getDComplex(rownr));
    case realFUNC:
	if (operands_p[0]->dataType() == NTDouble) {
	    return operands_p[0]->getDouble(rownr);
	}
	return operands_p[0]->getDComplex(rownr).real();
    case imagFUNC:
	if (operands_p[0]->dataType() == NTDouble) {
	    return 0;
	}
	return operands_p[0]->getDComplex(rownr).imag();
    case asinFUNC:
	return asin     (operands_p[0]->getDouble(rownr));
    case acosFUNC:
	return acos     (operands_p[0]->getDouble(rownr));
    case atanFUNC:
	return atan     (operands_p[0]->getDouble(rownr));
    case tanFUNC:
	return tan      (operands_p[0]->getDouble(rownr));
    case tanhFUNC:
	return tanh     (operands_p[0]->getDouble(rownr));
    case atan2FUNC:
	return atan2    (operands_p[0]->getDouble(rownr),
			 operands_p[1]->getDouble(rownr));
    case signFUNC:
	{
	    double val = operands_p[0]->getDouble(rownr);
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
	    double val = operands_p[0]->getDouble(rownr);
	    if (val < 0) {
		return ceil (val - 0.5);
	    }
	    return floor (val + 0.5);
	}
    case floorFUNC:
	return floor    (operands_p[0]->getDouble(rownr));
    case ceilFUNC:
	return ceil     (operands_p[0]->getDouble(rownr));
    case fmodFUNC:
	return fmod     (operands_p[0]->getDouble(rownr),
			 operands_p[1]->getDouble(rownr));
    case strlengthFUNC:
	return operands_p[0]->getString (rownr).length();
    case mjdFUNC:
	return operands_p[0]->getDate(rownr).day();
    case yearFUNC:
	return operands_p[0]->getDate(rownr).year();
    case monthFUNC:
	return operands_p[0]->getDate(rownr).month();
    case dayFUNC:
	return operands_p[0]->getDate(rownr).monthday();
    case weekdayFUNC:
	return operands_p[0]->getDate(rownr).weekday();
    case weekFUNC:
	return operands_p[0]->getDate(rownr).yearweek();
    case timeFUNC:                                       //# return in radians
	return fmod (Double(operands_p[0]->getDate(rownr)), 1.) * C::_2pi;
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDouble, "
			     "unknown function"));
    }
    return 0;
}

DComplex TableExprFuncNode::getDComplex (uInt rownr)
{
    if (dataType() == NTDouble) {
	return TableExprFuncNode::getDouble (rownr);
    }
    switch (funcType_p) {
    case sinFUNC:
	return sin      (operands_p[0]->getDComplex(rownr));
    case sinhFUNC:
	return sinh     (operands_p[0]->getDComplex(rownr));
    case cosFUNC:
	return cos      (operands_p[0]->getDComplex(rownr));
    case coshFUNC:
	return cosh     (operands_p[0]->getDComplex(rownr));
    case expFUNC:
	return exp      (operands_p[0]->getDComplex(rownr));
    case logFUNC:
	return log      (operands_p[0]->getDComplex(rownr));
    case log10FUNC:
	return log10    (operands_p[0]->getDComplex(rownr));
    case powFUNC:
	return pow      (operands_p[0]->getDComplex(rownr),
		         operands_p[1]->getDComplex(rownr));
    case squareFUNC:
	{
	    DComplex val = operands_p[0]->getDComplex(rownr);
	    return val * val;
	}
    case sqrtFUNC:
	return sqrt     (operands_p[0]->getDComplex(rownr));
    case conjFUNC:
	return conj     (operands_p[0]->getDComplex(rownr));
    case minFUNC:
	{
	    DComplex val0(operands_p[0]->getDComplex (rownr));
	    DComplex val1(operands_p[1]->getDComplex (rownr));
	    if (val0 > val1) {
		return val1;
	    }
	    return val0;
	}
    case maxFUNC:
	{
	    DComplex val0(operands_p[0]->getDComplex (rownr));
	    DComplex val1(operands_p[1]->getDComplex (rownr));
	    if (val0 < val1) {
		return val1;
	    }
	    return val0;
	}
    case complexFUNC:
	return DComplex (operands_p[0]->getDouble (rownr),
			 operands_p[1]->getDouble (rownr));
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDComplex, "
			     "unknown function"));
    }
    return DComplex(0., 0.);
}

String TableExprFuncNode::getString (uInt rownr)
{
    switch (funcType_p) {
    case upcaseFUNC:
	{
	    String str = operands_p[0]->getString (rownr);
	    str.upcase();
	    return str;
	}
    case downcaseFUNC:
	{
	    String str = operands_p[0]->getString (rownr);
	    str.downcase();
	    return str;
	}
    case trimFUNC:
	{
	    String str = operands_p[0]->getString (rownr);
	    int pos = str.length();
	    while (--pos >= 0 && str[pos] == ' ' ) ;
	    if (pos < 0) {
		return "";
	    }
	    return str.through(pos);
	}
    case cmonthFUNC:
	return operands_p[0]->getDate(rownr).monthName();
    case cdowFUNC:
	return operands_p[0]->getDate(rownr).dayName();
    default:
	throw (TableInvExpr ("TableExprFuncNode::getString, "
			     "unknown function"));
    }
    return "";
}

Regex TableExprFuncNode::getRegex (uInt rownr)
{
    switch (funcType_p) {
    case regexFUNC:
	return Regex(operands_p[0]->getString (rownr));
    case patternFUNC:
	return Regex(Regex::fromPattern(operands_p[0]->getString (rownr)));
    default:
	throw (TableInvExpr ("TableExprFuncNode::getRegex, "
			     "unknown function"));
    }
    return Regex("");
}

MVTime TableExprFuncNode::getDate (uInt rownr)
{
    switch (funcType_p) {
    case datetimeFUNC:
	{
	    Quantity quant;
	    if (MVTime::read (quant, operands_p[0]->getString(rownr))) {
		return quant;
	    }
	    throw (TableInvExpr ("invalid date string " +
				 operands_p[0]->getString(rownr)));
	}
    case mjdtodateFUNC:
	return MVTime (operands_p[0]->getDouble(rownr));
    case dateFUNC:
	return MVTime (floor (Double (operands_p[0]->getDate(rownr))));
    default:
	throw (TableInvExpr ("TableExprFuncNode::getDate, "
			     "unknown function"));
    }
    return MVTime();
}


TableExprFuncNode::NodeDataType TableExprFuncNode::checkOperands
				    (Block<Int>& dtypeOper,
				     FunctionType fType,
				     PtrBlock<TableExprNodeRep*>& nodes)
{
    switch (fType) {
    case randFUNC:
    case rownrFUNC:
    case piFUNC:
    case eFUNC:
	checkNumOfArg (0, 0, nodes);
	return NTDouble;
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
    case strlengthFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTDouble, nodes);
    case upcaseFUNC:
    case downcaseFUNC:
    case trimFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTString, nodes);
    case regexFUNC:
    case patternFUNC:
	checkNumOfArg (1, 1, nodes);
	return checkDT (dtypeOper, NTString, NTRegex, nodes);
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
	throw (TableInvExpr ("TableExprFuncNode::checkOperands, "
			     "function not contained in switch statement"));
    }
    return NTNumeric;
}
