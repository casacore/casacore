//# TableMeasValueDesc.cc: Definition of a MeasValue in a Table.
//# Copyright (C) 1997,1999,2000,2001
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
#include <casacore/measures/TableMeasures/TableMeasValueDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableMeasValueDesc::TableMeasValueDesc()
{}

TableMeasValueDesc::TableMeasValueDesc (const TableDesc& td, 
					const String& columnName)
: itsColumn(columnName)
{
  checkColumn(td);
}

TableMeasValueDesc::TableMeasValueDesc (const TableMeasValueDesc& that)
: itsColumn(that.itsColumn)
{}

TableMeasValueDesc::~TableMeasValueDesc()
{}

TableMeasValueDesc& TableMeasValueDesc::operator=
                                            (const TableMeasValueDesc& that)
{
  if (this != &that) {
    itsColumn = that.itsColumn;
  }
  return *this;
}

void TableMeasValueDesc::write (TableDesc& td, const TableRecord& measInfo)
{
  checkColumn (td);
  writeKeys (td.rwColumnDesc(itsColumn).rwKeywordSet(), measInfo);
}

void TableMeasValueDesc::write (Table& tab, const TableRecord& measInfo)
{
  checkColumn (tab.tableDesc());
  TableColumn tabcol(tab, itsColumn);
  writeKeys (tabcol.rwKeywordSet(), measInfo);
}

void TableMeasValueDesc::writeKeys (TableRecord& columnKeyset,
				    const TableRecord& measInfo)
{
  if (measInfo.nfields() > 0) {
    columnKeyset.defineRecord ("MEASINFO", measInfo);
  }
}

void TableMeasValueDesc::checkColumn (const TableDesc& td) const
{
  if (! td.isColumn(itsColumn)) {
    throw (AipsError ("TableMeasValueDesc::checkColumn; No such column: "
		      + itsColumn));
  } else if (td.columnDesc(itsColumn).dataType() != TpDouble) {
    throw (AipsError ("TableMeasValueDesc::checkColumn; Column's type "
		      "must be Double: " + itsColumn));
  }
}

} //# NAMESPACE CASACORE - END

