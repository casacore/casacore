//# TableQuantumDesc.cc: Definition of a Quantum in a Table.
//# Copyright (C) 1997,1998,1999,2000,2001
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
#include <casacore/measures/TableMeasures/TableQuantumDesc.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Exceptions.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableQuantumDesc::TableQuantumDesc (const TableDesc& td, const String& column)
: itsColName(column)
{
  checkColumn(td);
}

TableQuantumDesc::TableQuantumDesc (const TableDesc& td, const String& column,
				    const Unit& u)
: itsColName(column),
  itsUnitsName(1)
{
  checkColumn(td);
  itsUnitsName(0) = u.getName();
}

TableQuantumDesc::TableQuantumDesc (const TableDesc& td, const String& column,
				    const Vector<String>& unitNames)
: itsColName(column),
  itsUnitsName(unitNames)
{
  checkColumn(td);
}

TableQuantumDesc::TableQuantumDesc (const TableDesc& td, const String& column,
				    const Vector<Unit>& u)
: itsColName(column),
  itsUnitsName(u.nelements())
{
  checkColumn(td);
  for (uInt i=0; i<u.nelements(); i++) {
    itsUnitsName(i) = u(i).getName();
  }
}

TableQuantumDesc::TableQuantumDesc (const TableDesc& td, const String& column, 
				    const String& unitsCol)
: itsColName(column),
  itsUnitsColName(unitsCol)
{
  checkColumn(td);
  checkUnitsColumn(td);
}

TableQuantumDesc::TableQuantumDesc (const TableDesc& td, const String& column, 
				    const Char* unitsCol)
: itsColName(column),
  itsUnitsColName(unitsCol)
{
  checkColumn(td);
  checkUnitsColumn(td);
}

TableQuantumDesc::TableQuantumDesc (const TableQuantumDesc& that)
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
    itsUnitsName.resize (that.itsUnitsName.nelements());
    itsUnitsName = that.itsUnitsName;
    itsUnitsColName = that.itsUnitsColName;
  }
  return *this;
}

TableQuantumDesc* TableQuantumDesc::reconstruct (const TableDesc& td,
						 const String& columnName)
{
  TableQuantumDesc* p = 0;
  const TableRecord& columnKeyset = td[columnName].keywordSet();
  String refString;
  Int fnr = columnKeyset.fieldNumber("VariableUnits");
  if (fnr >= 0) {
    String unitColName = columnKeyset.asString(fnr);
    p = new TableQuantumDesc(td, columnName, unitColName);
  } else {
    fnr = columnKeyset.fieldNumber("QuantumUnits");
    if (fnr >= 0) {
      Vector<String> unitNames = columnKeyset.asArrayString(fnr);
      p = new TableQuantumDesc(td, columnName, unitNames);
    } else {
      throw(AipsError("TableQuantum::reconstruct; Not a Quantum"
		      " column: " + columnName));
    }
  }
  return p;
}

void TableQuantumDesc::write (TableDesc& td)
{
  writeKeys (td.rwColumnDesc(itsColName).rwKeywordSet());
}

void TableQuantumDesc::write (Table& tab)
{
  TableColumn tabcol (tab, itsColName);
  writeKeys (tabcol.rwKeywordSet());
}

void TableQuantumDesc::writeKeys (TableRecord& columnKeyset)
{
  if (isUnitVariable()) {
    columnKeyset.define("VariableUnits", itsUnitsColName);
  } else {
    columnKeyset.define("QuantumUnits", itsUnitsName);
  }
}

void TableQuantumDesc::checkColumn (const TableDesc& td) const
{
  if (! td.isColumn(itsColName)) {
    throw (AipsError ("TableQuantum::checkColumn; No such column: "
		      + itsColName));
  } 
}

void TableQuantumDesc::checkUnitsColumn (const TableDesc& td) const
{
  if (! td.isColumn(itsUnitsColName)) {
    throw (AipsError ("TableQuantum::checkUnitsColumn; No such column: "
		      + itsUnitsColName));
  } else if (td.columnDesc(itsUnitsColName).dataType() != TpString) {
    throw (AipsError ("TableQuantum::checkUnitsColumn; Type of column "
		      "should be String: " + itsUnitsColName));
  }
}

Bool TableQuantumDesc::hasQuanta (const TableColumn& column)
{
  return ( column.keywordSet().isDefined ("QuantumUnits")  ||
	   column.keywordSet().isDefined ("VariableUnits"));
}

} //# NAMESPACE CASACORE - END

