//# ExprDerNode.cc: Column in table select expression tree (templated code)
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

#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/DataType.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MVTime.h>
#include <aips/OS/Time.h>


// Implement the arithmetic operators for each data type.

TableExprNodePlusDouble::TableExprNodePlusDouble()
: TableExprNodeBinary (NTDouble, OtPlus)
{}
TableExprNodePlusDouble::~TableExprNodePlusDouble()
{}
double TableExprNodePlusDouble::getDouble (uInt rownr)
    { return lnode_p->getDouble(rownr) + rnode_p->getDouble(rownr); }
DComplex TableExprNodePlusDouble::getDComplex (uInt rownr)
    { return lnode_p->getDouble(rownr) + rnode_p->getDouble(rownr); }

TableExprNodePlusDComplex::TableExprNodePlusDComplex()
: TableExprNodeBinary (NTComplex, OtPlus)
{}
TableExprNodePlusDComplex::~TableExprNodePlusDComplex()
{}
DComplex TableExprNodePlusDComplex::getDComplex (uInt rownr)
    { return lnode_p->getDComplex(rownr) + rnode_p->getDComplex(rownr); }

TableExprNodePlusString::TableExprNodePlusString()
: TableExprNodeBinary (NTString, OtPlus)
{}
TableExprNodePlusString::~TableExprNodePlusString()
{}
String TableExprNodePlusString::getString (uInt rownr)
    { return lnode_p->getString(rownr) + rnode_p->getString(rownr); }

TableExprNodePlusDate::TableExprNodePlusDate()
: TableExprNodeBinary (NTDate, OtPlus)
{}
TableExprNodePlusDate::~TableExprNodePlusDate()
{}
MVTime TableExprNodePlusDate::getDate(uInt rownr)
{ return lnode_p->getDouble(rownr) + rnode_p->getDouble(rownr); }
Double TableExprNodePlusDate::getDouble(uInt rownr)
{ return lnode_p->getDouble(rownr) + rnode_p->getDouble(rownr); }

TableExprNodeMinusDouble::TableExprNodeMinusDouble()
: TableExprNodeBinary (NTDouble, OtMinus)
{}
TableExprNodeMinusDouble::~TableExprNodeMinusDouble()
{}
double TableExprNodeMinusDouble::getDouble (uInt rownr)
    { return lnode_p->getDouble(rownr) - rnode_p->getDouble(rownr); }
DComplex TableExprNodeMinusDouble::getDComplex (uInt rownr)
    { return lnode_p->getDouble(rownr) - rnode_p->getDouble(rownr); }

TableExprNodeMinusDComplex::TableExprNodeMinusDComplex()
: TableExprNodeBinary (NTComplex, OtMinus)
{}
TableExprNodeMinusDComplex::~TableExprNodeMinusDComplex()
{}
DComplex TableExprNodeMinusDComplex::getDComplex (uInt rownr)
    { return lnode_p->getDComplex(rownr) - rnode_p->getDComplex(rownr); }

TableExprNodeMinusDate::TableExprNodeMinusDate()
: TableExprNodeBinary (NTDate, OtMinus)
{}
TableExprNodeMinusDate::~TableExprNodeMinusDate()
{}
MVTime TableExprNodeMinusDate::getDate(uInt rownr)
    { return lnode_p->getDouble(rownr) - rnode_p->getDouble(rownr); }
Double TableExprNodeMinusDate::getDouble(uInt rownr)
    { return lnode_p->getDouble(rownr) - rnode_p->getDouble(rownr); }

TableExprNodeTimesDouble::TableExprNodeTimesDouble()
: TableExprNodeBinary (NTDouble, OtTimes)
{}
TableExprNodeTimesDouble::~TableExprNodeTimesDouble()
{}
double TableExprNodeTimesDouble::getDouble (uInt rownr)
    { return lnode_p->getDouble(rownr) * rnode_p->getDouble(rownr); }
DComplex TableExprNodeTimesDouble::getDComplex (uInt rownr)
    { return lnode_p->getDouble(rownr) * rnode_p->getDouble(rownr); }

TableExprNodeTimesDComplex::TableExprNodeTimesDComplex()
: TableExprNodeBinary (NTComplex, OtTimes)
{}
TableExprNodeTimesDComplex::~TableExprNodeTimesDComplex()
{}
DComplex TableExprNodeTimesDComplex::getDComplex (uInt rownr)
    { return lnode_p->getDComplex(rownr) * rnode_p->getDComplex(rownr); }


TableExprNodeDivideDouble::TableExprNodeDivideDouble()
: TableExprNodeBinary (NTDouble, OtDivide)
{}
TableExprNodeDivideDouble::~TableExprNodeDivideDouble()
{}
double TableExprNodeDivideDouble::getDouble (uInt rownr)
    { return lnode_p->getDouble(rownr) / rnode_p->getDouble(rownr); }
DComplex TableExprNodeDivideDouble::getDComplex (uInt rownr)
    { return lnode_p->getDouble(rownr) / rnode_p->getDouble(rownr); }

TableExprNodeDivideDComplex::TableExprNodeDivideDComplex()
: TableExprNodeBinary (NTComplex, OtDivide)
{}
TableExprNodeDivideDComplex::~TableExprNodeDivideDComplex()
{}
DComplex TableExprNodeDivideDComplex::getDComplex (uInt rownr)
    { return lnode_p->getDComplex(rownr) / rnode_p->getDComplex(rownr); }


TableExprNodeModuloDouble::TableExprNodeModuloDouble()
: TableExprNodeBinary (NTDouble, OtModulo)
{}
TableExprNodeModuloDouble::~TableExprNodeModuloDouble()
{}
double TableExprNodeModuloDouble::getDouble (uInt rownr)
    { return fmod (lnode_p->getDouble(rownr), rnode_p->getDouble(rownr)); }
DComplex TableExprNodeModuloDouble::getDComplex (uInt rownr)
    { return fmod (lnode_p->getDouble(rownr), rnode_p->getDouble(rownr)); }



// Implement the comparison operators for each data type.

TableExprNodeEQBool::TableExprNodeEQBool()
: TableExprNodeBinary (NTBool, OtEQ
)
{}
TableExprNodeEQBool::~TableExprNodeEQBool()
{}
Bool TableExprNodeEQBool::getBool (uInt rownr)
    { return lnode_p->getBool(rownr) == rnode_p->getBool(rownr) ?
	                                                   True : False; }
TableExprNodeEQDouble::TableExprNodeEQDouble()
: TableExprNodeBinary (NTBool, OtEQ)
{}
TableExprNodeEQDouble::~TableExprNodeEQDouble()
{}
Bool TableExprNodeEQDouble::getBool (uInt rownr)
    { return lnode_p->getDouble(rownr) == rnode_p->getDouble(rownr) ?
	                                                   True : False; }
TableExprNodeEQDComplex::TableExprNodeEQDComplex()
: TableExprNodeBinary (NTBool, OtEQ)
{}
TableExprNodeEQDComplex::~TableExprNodeEQDComplex()
{}
Bool TableExprNodeEQDComplex::getBool (uInt rownr)
    { return lnode_p->getDComplex(rownr) == rnode_p->getDComplex(rownr) ?
	                                                   True : False; }
TableExprNodeEQString::TableExprNodeEQString()
: TableExprNodeBinary (NTBool, OtEQ)
{}
TableExprNodeEQString::~TableExprNodeEQString()
{}
Bool TableExprNodeEQString::getBool (uInt rownr)
    { return lnode_p->getString(rownr) == rnode_p->getString(rownr) ?
	                                                   True : False; }

TableExprNodeEQRegex::TableExprNodeEQRegex()
: TableExprNodeBinary (NTBool, OtEQ)
{}
TableExprNodeEQRegex::~TableExprNodeEQRegex()
{}
Bool TableExprNodeEQRegex::getBool (uInt rownr)
{ return lnode_p->getString(rownr).matches(rnode_p->getRegex(rownr)) ?
	                                                   True : False; }

TableExprNodeEQDate::TableExprNodeEQDate()
: TableExprNodeBinary (NTBool, OtEQ)
{}
TableExprNodeEQDate::~TableExprNodeEQDate()
{}
Bool TableExprNodeEQDate::getBool (uInt rownr)
{ return lnode_p->getDate(rownr) == rnode_p->getDate(rownr) ?
	                                                   True : False; }
TableExprNodeNEBool::TableExprNodeNEBool()
: TableExprNodeBinary (NTBool, OtNE)
{}
TableExprNodeNEBool::~TableExprNodeNEBool()
{}
Bool TableExprNodeNEBool::getBool (uInt rownr)
    { return lnode_p->getBool(rownr) != rnode_p->getBool(rownr) ?
	                                                   True : False; }
TableExprNodeNEDouble::TableExprNodeNEDouble()
: TableExprNodeBinary (NTBool, OtNE)
{}
TableExprNodeNEDouble::~TableExprNodeNEDouble()
{}
Bool TableExprNodeNEDouble::getBool (uInt rownr)
    { return lnode_p->getDouble(rownr) != rnode_p->getDouble(rownr) ?
	                                                   True : False; }
TableExprNodeNEDComplex::TableExprNodeNEDComplex()
: TableExprNodeBinary (NTBool, OtNE)
{}
TableExprNodeNEDComplex::~TableExprNodeNEDComplex()
{}
Bool TableExprNodeNEDComplex::getBool (uInt rownr)
    { return lnode_p->getDComplex(rownr) != rnode_p->getDComplex(rownr) ?
	                                                   True : False; }
TableExprNodeNEString::TableExprNodeNEString()
: TableExprNodeBinary (NTBool, OtNE)
{}
TableExprNodeNEString::~TableExprNodeNEString()
{}
Bool TableExprNodeNEString::getBool (uInt rownr)
    { return lnode_p->getString(rownr) != rnode_p->getString(rownr) ?
	                                                   True : False; }

TableExprNodeNERegex::TableExprNodeNERegex()
: TableExprNodeBinary (NTBool, OtNE)
{}
TableExprNodeNERegex::~TableExprNodeNERegex()
{}
Bool TableExprNodeNERegex::getBool (uInt rownr)
    { return lnode_p->getString(rownr).matches(rnode_p->getRegex(rownr)) ?
    	                                                   False : True; }
TableExprNodeNEDate::TableExprNodeNEDate()
: TableExprNodeBinary (NTBool, OtNE)
{}
TableExprNodeNEDate::~TableExprNodeNEDate()
{}
Bool TableExprNodeNEDate::getBool (uInt rownr)
    { return lnode_p->getDate(rownr) != rnode_p->getDate(rownr)  ?
    	                                                   True : False; }
TableExprNodeGTDouble::TableExprNodeGTDouble()
: TableExprNodeBinary (NTBool, OtGT)
{}
TableExprNodeGTDouble::~TableExprNodeGTDouble()
{}
Bool TableExprNodeGTDouble::getBool (uInt rownr)
    { return lnode_p->getDouble(rownr) > rnode_p->getDouble(rownr) ?
	                                                   True : False; }
TableExprNodeGTDComplex::TableExprNodeGTDComplex()
: TableExprNodeBinary (NTBool, OtGT)
{}
TableExprNodeGTDComplex::~TableExprNodeGTDComplex()
{}
Bool TableExprNodeGTDComplex::getBool (uInt rownr)
    { return lnode_p->getDComplex(rownr) > rnode_p->getDComplex(rownr) ?
	                                                   True : False; }
TableExprNodeGTString::TableExprNodeGTString()
: TableExprNodeBinary (NTBool, OtGT)
{}
TableExprNodeGTString::~TableExprNodeGTString()
{}
Bool TableExprNodeGTString::getBool (uInt rownr)
    { return lnode_p->getString(rownr) > rnode_p->getString(rownr) ?
	                                                   True : False; }
TableExprNodeGTDate::TableExprNodeGTDate()
: TableExprNodeBinary (NTBool, OtGT)
{}
TableExprNodeGTDate::~TableExprNodeGTDate()
{}
Bool TableExprNodeGTDate::getBool (uInt rownr)
    { return lnode_p->getDate(rownr) > rnode_p->getDate(rownr) ?
	                                                   True : False; }

TableExprNodeGEDouble::TableExprNodeGEDouble()
: TableExprNodeBinary (NTBool, OtGE)
{}
TableExprNodeGEDouble::~TableExprNodeGEDouble()
{}
Bool TableExprNodeGEDouble::getBool (uInt rownr)
    { return lnode_p->getDouble(rownr) >= rnode_p->getDouble(rownr) ?
	                                                   True : False; }
TableExprNodeGEDComplex::TableExprNodeGEDComplex()
: TableExprNodeBinary (NTBool, OtGE)
{}
TableExprNodeGEDComplex::~TableExprNodeGEDComplex()
{}
Bool TableExprNodeGEDComplex::getBool (uInt rownr)
    { return lnode_p->getDComplex(rownr) >= rnode_p->getDComplex(rownr) ?
	                                                   True : False; }
TableExprNodeGEString::TableExprNodeGEString()
: TableExprNodeBinary (NTBool, OtGE)
{}
TableExprNodeGEString::~TableExprNodeGEString()
{}
Bool TableExprNodeGEString::getBool (uInt rownr)
    { return lnode_p->getString(rownr) >= rnode_p->getString(rownr) ?
	                                                   True : False; }
TableExprNodeGEDate::TableExprNodeGEDate()
: TableExprNodeBinary (NTBool, OtGE)
{}
TableExprNodeGEDate::~TableExprNodeGEDate()
{}
Bool TableExprNodeGEDate::getBool (uInt rownr)
    { return lnode_p->getDate(rownr) >= rnode_p->getDate(rownr) ?
	                                                   True : False; }


// Implement the constants for each data type.

TableExprNodeConstBool::TableExprNodeConstBool (const Bool& val)
: TableExprNodeBinary (NTBool, OtConst),
  value_p             (val)
{}
TableExprNodeConstBool::~TableExprNodeConstBool()
{}
Bool TableExprNodeConstBool::getBool (uInt)
    { return value_p; }

TableExprNodeConstDouble::TableExprNodeConstDouble (const double& val)
: TableExprNodeBinary (NTDouble, OtConst),
  value_p             (val)
{}
TableExprNodeConstDouble::~TableExprNodeConstDouble()
{}
double TableExprNodeConstDouble::getDouble (uInt)
    { return value_p; }
DComplex TableExprNodeConstDouble::getDComplex (uInt)
    { return value_p; }

TableExprNodeConstDComplex::TableExprNodeConstDComplex (const DComplex& val)
: TableExprNodeBinary (NTComplex, OtConst),
  value_p             (val)
{}
TableExprNodeConstDComplex::~TableExprNodeConstDComplex()
{}
DComplex TableExprNodeConstDComplex::getDComplex (uInt)
    { return value_p; }

TableExprNodeConstString::TableExprNodeConstString (const String& val)
: TableExprNodeBinary (NTString, OtConst),
  value_p             (val)
{}
TableExprNodeConstString::~TableExprNodeConstString()
{}
String TableExprNodeConstString::getString (uInt)
    { return value_p; }

TableExprNodeConstRegex::TableExprNodeConstRegex (const Regex& val)
: TableExprNodeBinary (NTRegex, OtConst),
  value_p             (val)
{}
TableExprNodeConstRegex::~TableExprNodeConstRegex()
{}
Regex TableExprNodeConstRegex::getRegex (uInt)
    { return value_p; }

TableExprNodeConstDate::TableExprNodeConstDate (const MVTime& val)
: TableExprNodeBinary (NTDate, OtConst),
  value_p             (val)
{}
TableExprNodeConstDate::~TableExprNodeConstDate()
{}
double TableExprNodeConstDate::getDouble (uInt)
    { return value_p; }
MVTime TableExprNodeConstDate::getDate (uInt)
    { return value_p; }

TableExprNodeOR::TableExprNodeOR()
: TableExprNodeBinary (NTBool, OtOR)
{}
TableExprNodeOR::~TableExprNodeOR()
{}
Bool TableExprNodeOR::getBool (uInt rownr)
    { return lnode_p->getBool(rownr) || rnode_p->getBool(rownr)  ?
	                                                    True : False;}

TableExprNodeAND::TableExprNodeAND()
: TableExprNodeBinary (NTBool, OtAND)
{}
TableExprNodeAND::~TableExprNodeAND()
{}
Bool TableExprNodeAND::getBool (uInt rownr)
    { return lnode_p->getBool(rownr) && rnode_p->getBool(rownr)  ?
	                                                    True : False;}

TableExprNodeNOT::TableExprNodeNOT()
: TableExprNodeBinary (NTBool, OtNOT)
{}
TableExprNodeNOT::~TableExprNodeNOT()
{}
Bool TableExprNodeNOT::getBool (uInt rownr)
    { return lnode_p->getBool(rownr)  ?  False : True;}


TableExprNodeMIN::TableExprNodeMIN (NodeDataType type)
: TableExprNodeBinary (type, OtMIN)
{}
TableExprNodeMIN::~TableExprNodeMIN()
{}
double TableExprNodeMIN::getDouble (uInt rownr)
    { return -(lnode_p->getDouble(rownr)); }
DComplex TableExprNodeMIN::getDComplex (uInt rownr)
    { return -(lnode_p->getDComplex(rownr)); }


// <thrown>
//   <li> TableInvExpr
// </thrown>
//# Create a table expression node for a column.
//# First use a "dummy" data type and fill it in later.
TableExprNodeColumn::TableExprNodeColumn (const Table& table,
					  const BaseTable* tabptr,
					  const String& name)
: TableExprNodeBinary (NTNumeric, OtScaCol)
{
    //# Create a table column object and check if the column is a scalar.
    tabColPtr_p = new ROTableColumn (table, name);
    if (tabColPtr_p == 0) {
	throw (AllocError ("TableExprNodeColumn",1));
    }
    if (! tabColPtr_p->columnDesc().isScalar()) {
	throw (TableInvExpr (name, " is no scalar column"));
    }
    //# Fill in the real data type and the base table pointer.
    switch (tabColPtr_p->columnDesc().dataType()) {
    case TpBool:
	dtype_p = NTBool;
	break;
    case TpString:
	dtype_p = NTString;
	break;
    case TpComplex:
    case TpDComplex:
	dtype_p = NTComplex;
	break;
    default:
	dtype_p = NTDouble;
    }
    baseTabPtr_p = tabptr;
}

TableExprNodeColumn::~TableExprNodeColumn()
    { delete tabColPtr_p; }

//# Return the ROTableColumn.
const ROTableColumn& TableExprNodeColumn::getColumn() const
    { return *tabColPtr_p; }

Bool TableExprNodeColumn::getBool (uInt rownr)
{
    Bool val;
    tabColPtr_p->getScalar (rownr, val);
    return val;
}
double TableExprNodeColumn::getDouble (uInt rownr)
{
    double val;
    tabColPtr_p->getScalar (rownr, val);
    return val;
}
DComplex TableExprNodeColumn::getDComplex (uInt rownr)
{
    DComplex val;
    tabColPtr_p->getScalar (rownr, val);
    return val;
}
String TableExprNodeColumn::getString (uInt rownr)
{
    String val;
    tabColPtr_p->getScalar (rownr, val);
    return val;
}

Bool TableExprNodeColumn::getColumnDataType (DataType& dt) const
{
    dt = tabColPtr_p->columnDesc().dataType();
    return True;
}

Array<Bool>     TableExprNodeColumn::getColumnBool()
{
    ROScalarColumn<Bool> col (*tabColPtr_p);
    return col.getColumn();
}
Array<uChar>    TableExprNodeColumn::getColumnuChar()
{
    ROScalarColumn<uChar> col (*tabColPtr_p);
    return col.getColumn();
}
Array<Short>    TableExprNodeColumn::getColumnShort()
{
    ROScalarColumn<Short> col (*tabColPtr_p);
    return col.getColumn();
}
Array<uShort>   TableExprNodeColumn::getColumnuShort()
{
    ROScalarColumn<uShort> col (*tabColPtr_p);
    return col.getColumn();
}
Array<Int>      TableExprNodeColumn::getColumnInt()
{
    ROScalarColumn<Int> col (*tabColPtr_p);
    return col.getColumn();
}
Array<uInt>     TableExprNodeColumn::getColumnuInt()
{
    ROScalarColumn<uInt> col (*tabColPtr_p);
    return col.getColumn();
}
Array<Float>    TableExprNodeColumn::getColumnFloat()
{
    ROScalarColumn<Float> col (*tabColPtr_p);
    return col.getColumn();
}
Array<Double>   TableExprNodeColumn::getColumnDouble()
{
    ROScalarColumn<Double> col (*tabColPtr_p);
    return col.getColumn();
}
Array<Complex>  TableExprNodeColumn::getColumnComplex()
{
    ROScalarColumn<Complex> col (*tabColPtr_p);
    return col.getColumn();
}
Array<DComplex> TableExprNodeColumn::getColumnDComplex()
{
    ROScalarColumn<DComplex> col (*tabColPtr_p);
    return col.getColumn();
}
Array<String>   TableExprNodeColumn::getColumnString()
{
    ROScalarColumn<String> col (*tabColPtr_p);
    return col.getColumn();
}



TableExprNodeRownr::TableExprNodeRownr (const BaseTable* tabptr, uInt origin)
: TableExprNodeBinary (NTDouble, OtRownr),
  origin_p            (origin)
{
    baseTabPtr_p = tabptr;
}
TableExprNodeRownr::~TableExprNodeRownr ()
{}
double TableExprNodeRownr::getDouble (uInt rownr)
{
    return rownr + origin_p;
}



//# Take the seed from the current time and date.
TableExprNodeRandom::TableExprNodeRandom (const BaseTable* tabptr)
: TableExprNodeBinary (NTDouble, OtRandom),
  generator_p         (Int (fmod (Time().modifiedJulianDay(), 1.) * 86400000),
		       Int (Time().modifiedJulianDay())),
  random_p            (0, 1, &generator_p)
{
    baseTabPtr_p = tabptr;
}
TableExprNodeRandom::~TableExprNodeRandom ()
{}
double TableExprNodeRandom::getDouble (uInt rownr)
{
    return random_p();
}



void TableExprNodeEQDouble::ranges (Block<TableExprRange>& blrange)
{
    double dval;
    TableExprNodeRep* tsncol = 0;
    //# We can store a range if there is a column and constant (left or right).
    if (lnode_p->operType() == TableExprNodeRep::OtScaCol
    &&  rnode_p->operType() == TableExprNodeRep::OtConst) {
	tsncol = lnode_p;
	dval = rnode_p->getDouble (0);
    }else{
	if (rnode_p->operType() == TableExprNodeRep::OtScaCol
        &&  lnode_p->operType() == TableExprNodeRep::OtConst) {
	    tsncol = rnode_p;
	    dval = lnode_p->getDouble (0);
	}
    }
    //# Now create a range (if possible).
    //# The cast is harmless, since it is surely that object type.
    TableExprNodeRep::createRange (blrange, (TableExprNodeColumn*)tsncol,
				   dval, dval);
}

void TableExprNodeGEDouble::ranges (Block<TableExprRange>& blrange)
{
    double st,end;
    TableExprNodeRep* tsncol = 0;
    //# We can store a range if there is a column and constant (left or right).
    if (lnode_p->operType() == TableExprNodeRep::OtScaCol
    &&  rnode_p->operType() == TableExprNodeRep::OtConst) {
	tsncol = lnode_p;
	st = rnode_p->getDouble (0);
	end = MAXDOUBLE;
    }else{
	if (rnode_p->operType() == TableExprNodeRep::OtScaCol
        &&  lnode_p->operType() == TableExprNodeRep::OtConst) {
	    tsncol = rnode_p;
	    end = lnode_p->getDouble (0);
	    st = -MAXDOUBLE;
	}
    }
    //# Now create a range (if possible).
    //# The cast is harmless, since it is surely that object type.
    TableExprNodeRep::createRange (blrange, (TableExprNodeColumn*)tsncol,
				   st, end);
}

void TableExprNodeGTDouble::ranges (Block<TableExprRange>& blrange)
{
    double st,end;
    TableExprNodeRep* tsncol = 0;
    //# We can store a range if there is a column and constant (left or right).
    if (lnode_p->operType() == TableExprNodeRep::OtScaCol
    &&  rnode_p->operType() == TableExprNodeRep::OtConst) {
	tsncol = lnode_p;
	st = rnode_p->getDouble (0);
	end = MAXDOUBLE;
    }else{
	if (rnode_p->operType() == TableExprNodeRep::OtScaCol
        &&  lnode_p->operType() == TableExprNodeRep::OtConst) {
	    tsncol = rnode_p;
	    end = lnode_p->getDouble (0);
	    st = -MAXDOUBLE;
	}
    }
    //# Now create a range (if possible).
    //# The cast is harmless, since it is surely that object type.
    TableExprNodeRep::createRange (blrange, (TableExprNodeColumn*)tsncol,
				   st, end);
}


//# Or two blocks of ranges.
void TableExprNodeOR::ranges (Block<TableExprRange>& blrange)
{
    //# Get the ranges of the children.
    Block<TableExprRange> left,right;
    lnode_p->ranges (left);
    rnode_p->ranges (right);
    //# Now or the ranges.
    //# If a column appears in one, but not in the other it needs
    //# to be removed. Only equal columns can be combined and what
    //# gets created is a superset of the original blocks.
    blrange.resize(0,True);
    uInt nr=0;
    Int i,j;
    for (i=0; i<left.nelements(); i++) {
	for (j=0; j<right.nelements(); j++) {
	    if (right[j].getColumn().columnDesc().name() ==
		                left[i].getColumn().columnDesc().name()) {
		blrange.resize(nr+1, True);
		blrange[nr] = left[i];
		blrange[nr].mixOr (right[j]);
		nr++;
	    }
	}
    }
}


//# And two blocks of ranges.
void TableExprNodeAND::ranges (Block<TableExprRange>& blrange)
{
    //# Get the ranges of the children.
    Block<TableExprRange> other;
    lnode_p->ranges (blrange);
    rnode_p->ranges (other);
    //# If one of them is empty (which means all), return the other.
    if (other.nelements() == 0) {
	return;
    }
    if (blrange.nelements() == 0) {
	blrange = other;
	return;
    }
    //# Now and the ranges.
    //# First handle one and intersect its ranges with matching
    //# column names in the other one.
    //# Keep a vector with flags for non-processed other ones.
    Vector<Int> vec(other.nelements());
    vec = 0;
    Int i,j;
    for (i=0; i<blrange.nelements(); i++) {
	for (j=0; j<other.nelements(); j++) {
	    if (other[j].getColumn().columnDesc().name() ==
		                blrange[i].getColumn().columnDesc().name()) {
		blrange[i].mixAnd (other[j]);
		vec(j) = 1;
	    }
	}
    }
    //# Now add the non-processed other ones to the result.
    uInt nr = blrange.nelements();
    for (i=0; i<other.nelements(); i++) {
	if (vec(i) == 0) {
	    blrange.resize(nr+1, True);
	    blrange[nr++] = other[i];
	}
    }
}
