//# TableQuantumDesc.cc: Definition of a Quantum in a Table.
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

//# Includes
#include <aips/Exceptions.h>
#include <aips/Quanta/Unit.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <trial/TableMeasures/TableQuantumDesc.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>

TableQuantumDesc::TableQuantumDesc(const TableDesc& td, const String& column)
: itsColName(column)
{
    checkColumn(td);
}

TableQuantumDesc::TableQuantumDesc(const TableDesc& td, const String& column,
	    	    	    	   const Unit& u)
: itsColName(column),
  itsUnitsName(u.getName())
{
    checkColumn(td);
}

TableQuantumDesc::TableQuantumDesc(const TableDesc& td, const String& column, 
	    	    	    	   const String& unitsCol)
: itsColName(column),
  itsUnitsColName(unitsCol)
{
    checkColumn(td);
    checkUnitsColumn(td);
}

TableQuantumDesc::TableQuantumDesc(const TableDesc& td, const String& column, 
	    	    	    	   const Char* unitsCol)
: itsColName(column),
  itsUnitsColName(unitsCol)
{
    checkColumn(td);
    checkUnitsColumn(td);
}

TableQuantumDesc::TableQuantumDesc(const TableQuantumDesc& that)
: itsColName(that.itsColName),
  itsUnitsName(that.itsUnitsName),
  itsUnitsColName(that.itsUnitsColName)
{}

TableQuantumDesc::~TableQuantumDesc()
{}

TableQuantumDesc& TableQuantumDesc::operator= (const TableQuantumDesc& that)
{
    if (this != &that) {
	itsColName = that.itsColName;
	itsUnitsName = that.itsUnitsName;
    	itsUnitsColName = that.itsUnitsColName;
    }
    return *this;
}

TableQuantumDesc* TableQuantumDesc::reconstruct(const TableDesc& td,
    	    	    	    	    	    	const String& columnName)
{
    TableQuantumDesc* P = 0;
    const TableRecord& columnKeyset = td[columnName].keywordSet();
    String refString;    
    Int fnr;
    
    fnr = columnKeyset.fieldNumber("VariableUnits");
    if (fnr >= 0) {
	refString = columnKeyset.asString(fnr);
    	P = new TableQuantumDesc(td, columnName, refString);
    } else {
    	fnr = columnKeyset.fieldNumber("QuantumUnit");
	
	if (fnr >= 0) {
	    refString = columnKeyset.asString(fnr);
    	    P = new TableQuantumDesc(td, columnName, Unit(refString));
	} else {
            throw(AipsError("TableQuantum::reconstruct; QuantumUnit field not"
            	            " found for column " + columnName));
    	}
    }

    return P;
}

void TableQuantumDesc::write(TableDesc& td)
{
    TableRecord& columnKeyset = td.rwColumnDesc(itsColName).rwKeywordSet();
    if (isUnitVariable()) {
    	columnKeyset.define("VariableUnits", itsUnitsColName);
    } else {
	columnKeyset.define("QuantumUnit", itsUnitsName);
    }
}

void TableQuantumDesc::checkColumn(const TableDesc& td) const
{
    if (td.isColumn(itsColName) == False) {
    	throw(AipsError("TableQuantum::checkColumn; No such column: "
            	    	 + itsColName));
    } 
}

void TableQuantumDesc::checkUnitsColumn(const TableDesc& td) const
{
    if (td.isColumn(itsUnitsColName) == False) {
    	throw(AipsError("TableQuantum::checkUnitsColumn; No such column: "
            	    	 + itsUnitsColName));
    } else if (td.columnDesc(itsUnitsColName).dataType() != TpString) {
    	throw(AipsError("TableQuantum::checkUnitsColumn; Type of column "
		    	"should be String: " + itsUnitsColName));
    }
}


