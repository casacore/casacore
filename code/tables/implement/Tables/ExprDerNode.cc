//# ExprDerNode.cc: Nodes representing scalar operators in table select expression tree
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
//#
//# $Id$

#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Utilities/DataType.h>
#include <aips/Mathematics/Math.h>
#include <aips/Quanta/MVTime.h>
#include <aips/OS/Time.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>



// Implement the constants for each data type.

TableExprNodeConstBool::TableExprNodeConstBool (const Bool& val)
: TableExprNodeBinary (NTBool, VTScalar, OtLiteral, 0),
  value_p             (val)
{}
TableExprNodeConstBool::~TableExprNodeConstBool()
{}
Bool TableExprNodeConstBool::getBool (const TableExprId&)
    { return value_p; }

TableExprNodeConstDouble::TableExprNodeConstDouble (const Double& val)
: TableExprNodeBinary (NTDouble, VTScalar, OtLiteral, 0),
  value_p             (val)
{}
TableExprNodeConstDouble::~TableExprNodeConstDouble()
{}
Double TableExprNodeConstDouble::getDouble (const TableExprId&)
    { return value_p; }
DComplex TableExprNodeConstDouble::getDComplex (const TableExprId&)
    { return value_p; }

TableExprNodeConstDComplex::TableExprNodeConstDComplex (const DComplex& val)
: TableExprNodeBinary (NTComplex, VTScalar, OtLiteral, 0),
  value_p             (val)
{}
TableExprNodeConstDComplex::~TableExprNodeConstDComplex()
{}
DComplex TableExprNodeConstDComplex::getDComplex (const TableExprId&)
    { return value_p; }

TableExprNodeConstString::TableExprNodeConstString (const String& val)
: TableExprNodeBinary (NTString, VTScalar, OtLiteral, 0),
  value_p             (val)
{}
TableExprNodeConstString::~TableExprNodeConstString()
{}
String TableExprNodeConstString::getString (const TableExprId&)
    { return value_p; }

TableExprNodeConstRegex::TableExprNodeConstRegex (const Regex& val)
: TableExprNodeBinary (NTRegex, VTScalar, OtLiteral, 0),
  value_p             (val)
{}
TableExprNodeConstRegex::~TableExprNodeConstRegex()
{}
Regex TableExprNodeConstRegex::getRegex (const TableExprId&)
    { return value_p; }

TableExprNodeConstDate::TableExprNodeConstDate (const MVTime& val)
: TableExprNodeBinary (NTDate, VTScalar, OtLiteral, 0),
  value_p             (val)
{}
TableExprNodeConstDate::~TableExprNodeConstDate()
{}
Double TableExprNodeConstDate::getDouble (const TableExprId&)
    { return value_p; }
MVTime TableExprNodeConstDate::getDate (const TableExprId&)
    { return value_p; }


// <thrown>
//   <li> TableInvExpr
// </thrown>
//# Create a table expression node for a column.
//# First use a "dummy" data type and fill it in later.
//# Similarly for the value type.
TableExprNodeColumn::TableExprNodeColumn (const Table& table,
					  const BaseTable* tabptr,
					  const String& name)
: TableExprNodeBinary (NTNumeric, VTScalar, OtColumn, tabptr)
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
}

void TableExprNodeColumn::replaceTablePtr (const Table& table,
					   const BaseTable* baseTablePtr)
{
    String name = tabColPtr_p->columnDesc().name();
    delete tabColPtr_p;
    tabColPtr_p = new ROTableColumn (table, name);
    baseTabPtr_p = baseTablePtr;
}

TableExprNodeColumn::~TableExprNodeColumn()
    { delete tabColPtr_p; }

//# Return the ROTableColumn.
const ROTableColumn& TableExprNodeColumn::getColumn() const
    { return *tabColPtr_p; }

Bool TableExprNodeColumn::getBool (const TableExprId& id)
{
    Bool val;
    tabColPtr_p->getScalar (id.rownr(), val);
    return val;
}
Double TableExprNodeColumn::getDouble (const TableExprId& id)
{
    Double val;
    tabColPtr_p->getScalar (id.rownr(), val);
    return val;
}
DComplex TableExprNodeColumn::getDComplex (const TableExprId& id)
{
    DComplex val;
    tabColPtr_p->getScalar (id.rownr(), val);
    return val;
}
String TableExprNodeColumn::getString (const TableExprId& id)
{
    String val;
    tabColPtr_p->getScalar (id.rownr(), val);
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
: TableExprNodeBinary (NTDouble, VTScalar, OtRownr, tabptr),
  origin_p            (origin)
{}
TableExprNodeRownr::~TableExprNodeRownr ()
{}
Double TableExprNodeRownr::getDouble (const TableExprId& id)
{
    AlwaysAssert (id.byRow(), AipsError);
    return id.rownr() + origin_p;
}



//# Take the seed from the current time and date.
TableExprNodeRandom::TableExprNodeRandom (const BaseTable* tabptr)
: TableExprNodeBinary (NTDouble, VTScalar, OtRandom, tabptr),
  generator_p         (Int (fmod (Time().modifiedJulianDay(), 1.) * 86400000),
		       Int (Time().modifiedJulianDay())),
  random_p            (&generator_p, 0, 1)
{}
TableExprNodeRandom::~TableExprNodeRandom ()
{}
Double TableExprNodeRandom::getDouble (const TableExprId&)
{
    return random_p();
}
