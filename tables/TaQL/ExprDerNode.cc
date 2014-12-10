//# ExprDerNode.cc: Nodes representing scalar operators in table select expression tree
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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

#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Implement the constants for each data type.

TableExprNodeConstBool::TableExprNodeConstBool (const Bool& val)
: TableExprNodeBinary (NTBool, VTScalar, OtLiteral, Table()),
  value_p             (val)
{}
TableExprNodeConstBool::~TableExprNodeConstBool()
{}
Bool TableExprNodeConstBool::getBool (const TableExprId&)
    { return value_p; }

TableExprNodeConstInt::TableExprNodeConstInt (const Int64& val)
: TableExprNodeBinary (NTInt, VTScalar, OtLiteral, Table()),
  value_p             (val)
{}
TableExprNodeConstInt::~TableExprNodeConstInt()
{}
Int64 TableExprNodeConstInt::getInt (const TableExprId&)
    { return value_p; }
Double TableExprNodeConstInt::getDouble (const TableExprId&)
    { return value_p; }
DComplex TableExprNodeConstInt::getDComplex (const TableExprId&)
    { return double(value_p); }

TableExprNodeConstDouble::TableExprNodeConstDouble (const Double& val)
: TableExprNodeBinary (NTDouble, VTScalar, OtLiteral, Table()),
  value_p             (val)
{}
TableExprNodeConstDouble::~TableExprNodeConstDouble()
{}
Double TableExprNodeConstDouble::getDouble (const TableExprId&)
    { return value_p; }
DComplex TableExprNodeConstDouble::getDComplex (const TableExprId&)
    { return value_p; }

TableExprNodeConstDComplex::TableExprNodeConstDComplex (const DComplex& val)
: TableExprNodeBinary (NTComplex, VTScalar, OtLiteral, Table()),
  value_p             (val)
{}
TableExprNodeConstDComplex::~TableExprNodeConstDComplex()
{}
DComplex TableExprNodeConstDComplex::getDComplex (const TableExprId&)
    { return value_p; }

TableExprNodeConstString::TableExprNodeConstString (const String& val)
: TableExprNodeBinary (NTString, VTScalar, OtLiteral, Table()),
  value_p             (val)
{}
TableExprNodeConstString::~TableExprNodeConstString()
{}
String TableExprNodeConstString::getString (const TableExprId&)
    { return value_p; }

TableExprNodeConstRegex::TableExprNodeConstRegex (const TaqlRegex& val)
: TableExprNodeBinary (NTRegex, VTScalar, OtLiteral, Table()),
  value_p             (val)
{}
TableExprNodeConstRegex::~TableExprNodeConstRegex()
{}
TaqlRegex TableExprNodeConstRegex::getRegex (const TableExprId&)
    { return value_p; }

TableExprNodeConstDate::TableExprNodeConstDate (const MVTime& val)
: TableExprNodeBinary (NTDate, VTScalar, OtLiteral, Table()),
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
					  const String& name)
  : TableExprNodeBinary (NTNumeric, VTScalar, OtColumn, table),
    selTable_p       (table),
    tabCol_p         (table, name),
    applySelection_p (True)
{
    //# Check if the column is a scalar.
    if (! tabCol_p.columnDesc().isScalar()) {
	throw (TableInvExpr (name, " is no scalar column"));
    }
    //# Fill in the real data type and the base table pointer.
    switch (tabCol_p.columnDesc().dataType()) {
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
    case TpFloat:
    case TpDouble:
	dtype_p = NTDouble;
	break;
    default:
	dtype_p = NTInt;
    }
    setUnit (getColumnUnit(tabCol_p));
}

Unit TableExprNodeColumn::getColumnUnit (const TableColumn& tabcol)
{
    Unit unit;
    //# Get the unit (if defined).
    const TableRecord& keyset = tabcol.keywordSet();
    if (keyset.isDefined ("QuantumUnits")) {
        const Array<String>& units = keyset.asArrayString("QuantumUnits");
	if (units.size() > 0) {
	    unit = *(units.data());
	}
    } else if (keyset.isDefined ("UNIT")) {
        unit = keyset.asString("UNIT");
    }
    return unit;
}

TableExprNodeColumn::~TableExprNodeColumn()
{}

void TableExprNodeColumn::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    cols.push_back (this);
}

void TableExprNodeColumn::disableApplySelection()
{
    applySelection_p = False;
}
  
void TableExprNodeColumn::applySelection (const Vector<uInt>& rownrs)
{
    if (applySelection_p) {
        // Attach the column to the selection of the table.
        // Get column name before doing selection!!!!
        String name = tabCol_p.columnDesc().name();
        selTable_p = selTable_p(rownrs);
        tabCol_p = TableColumn(selTable_p, name);
        // Reset switch, because the column object can be used multiple times.
        // when a select expression is used as e.g. sort key.
        applySelection_p = False;
    }
}

//# Return the TableColumn.
const TableColumn& TableExprNodeColumn::getColumn() const
    { return tabCol_p; }

Bool TableExprNodeColumn::getBool (const TableExprId& id)
{
    Bool val;
    tabCol_p.getScalar (id.rownr(), val);
    return val;
}
Int64 TableExprNodeColumn::getInt (const TableExprId& id)
{
    Int64 val;
    tabCol_p.getScalar (id.rownr(), val);
    return val;
}
Double TableExprNodeColumn::getDouble (const TableExprId& id)
{
    Double val;
    tabCol_p.getScalar (id.rownr(), val);
    return val;
}
DComplex TableExprNodeColumn::getDComplex (const TableExprId& id)
{
    DComplex val;
    tabCol_p.getScalar (id.rownr(), val);
    return val;
}
String TableExprNodeColumn::getString (const TableExprId& id)
{
    String val;
    tabCol_p.getScalar (id.rownr(), val);
    return val;
}

Bool TableExprNodeColumn::getColumnDataType (DataType& dt) const
{
    dt = tabCol_p.columnDesc().dataType();
    return True;
}

Array<Bool>     TableExprNodeColumn::getColumnBool (const Vector<uInt>& rownrs)
{
    ScalarColumn<Bool> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<uChar>    TableExprNodeColumn::getColumnuChar (const Vector<uInt>& rownrs)
{
    ScalarColumn<uChar> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<Short>    TableExprNodeColumn::getColumnShort (const Vector<uInt>& rownrs)
{
    ScalarColumn<Short> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<uShort>   TableExprNodeColumn::getColumnuShort (const Vector<uInt>& rownrs)
{
    ScalarColumn<uShort> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<Int>      TableExprNodeColumn::getColumnInt (const Vector<uInt>& rownrs)
{
    ScalarColumn<Int> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<uInt>     TableExprNodeColumn::getColumnuInt (const Vector<uInt>& rownrs)
{
    ScalarColumn<uInt> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<Float>    TableExprNodeColumn::getColumnFloat (const Vector<uInt>& rownrs)
{
    ScalarColumn<Float> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<Double>   TableExprNodeColumn::getColumnDouble (const Vector<uInt>& rownrs)
{
    ScalarColumn<Double> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<Complex>  TableExprNodeColumn::getColumnComplex (const Vector<uInt>& rownrs)
{
    ScalarColumn<Complex> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<DComplex> TableExprNodeColumn::getColumnDComplex (const Vector<uInt>& rownrs)
{
    ScalarColumn<DComplex> col (tabCol_p);
    return col.getColumnCells (rownrs);
}
Array<String>   TableExprNodeColumn::getColumnString (const Vector<uInt>& rownrs)
{
    ScalarColumn<String> col (tabCol_p);
    return col.getColumnCells (rownrs);
}



TableExprNodeRownr::TableExprNodeRownr (const Table& table, uInt origin)
: TableExprNodeBinary (NTInt, VTScalar, OtRownr, table),
  origin_p            (origin)
{}
TableExprNodeRownr::~TableExprNodeRownr ()
{}
Int64 TableExprNodeRownr::getInt (const TableExprId& id)
{
    AlwaysAssert (id.byRow(), AipsError);
    return id.rownr() + origin_p;
}



TableExprNodeRowid::TableExprNodeRowid (const Table& table)
  : TableExprNodeBinary (NTInt, VTScalar, OtRownr, table),
    rownrs_p (table.nrow())
{
  indgen (rownrs_p);
}
TableExprNodeRowid::~TableExprNodeRowid ()
{}
void TableExprNodeRowid::applySelection (const Vector<uInt>& rownrs)
{
    // Select the rowid-s of the given rows.
    Vector<uInt> newRows(rownrs.size());
    for (uInt i=0; i<rownrs.size(); ++i) {
        newRows[i] = rownrs_p[rownrs[i]];
    }
    rownrs_p.reference (newRows);
}
Int64 TableExprNodeRowid::getInt (const TableExprId& id)
{
    AlwaysAssert (id.byRow(), AipsError);
    return rownrs_p[id.rownr()];
}



//# Take the seed from the current time and date.
TableExprNodeRandom::TableExprNodeRandom (const Table& table)
: TableExprNodeBinary (NTDouble, VTScalar, OtRandom, table),
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

} //# NAMESPACE CASACORE - END

