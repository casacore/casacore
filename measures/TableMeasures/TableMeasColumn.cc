//# TableMeasColumn.cc: Access to Measure Columns in Tables.
//# Copyright (C) 1999,2000
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
#include <casacore/measures/TableMeasures/TableMeasColumn.h>
#include <casacore/measures/TableMeasures/TableMeasDescBase.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableMeasColumn::TableMeasColumn()
: itsNvals      (0),
  itsVarRefFlag (False),
  itsVarOffFlag (False)
{}

TableMeasColumn::TableMeasColumn (const Table& tab,
                                  const String& columnName)
: itsNvals      (0),
  itsTabDataCol (tab, columnName)
{
  itsDescPtr = TableMeasDescBase::reconstruct (tab, columnName);
  itsVarRefFlag = itsDescPtr->isRefCodeVariable();
  itsVarOffFlag = itsDescPtr->isOffsetVariable();
}

TableMeasColumn::TableMeasColumn (const TableMeasColumn& that)
: itsNvals      (that.itsNvals),
  itsDescPtr    (that.itsDescPtr),
  itsTabDataCol (that.itsTabDataCol),
  itsVarRefFlag (that.itsVarRefFlag),
  itsVarOffFlag (that.itsVarOffFlag)
{}

TableMeasColumn::~TableMeasColumn()
{}
 
void TableMeasColumn::reference (const TableMeasColumn& that)
{
  itsNvals   = that.itsNvals;
  itsDescPtr = that.itsDescPtr;
  itsTabDataCol.reference (that.itsTabDataCol);
  itsVarRefFlag = that.itsVarRefFlag;
  itsVarOffFlag = that.itsVarOffFlag;
}

void TableMeasColumn::attach (const Table& tab, const String& columnName)
{
  reference (TableMeasColumn (tab, columnName));
}
 
const String& TableMeasColumn::columnName() const
{
  return itsDescPtr->columnName();
}

Bool TableMeasColumn::isDefined (uInt rownr) const
{
  return itsTabDataCol.isDefined (rownr);
}

void TableMeasColumn::throwIfNull() const
{
  if (isNull()) {
    throw (TableInvOper("MeasTableColumn object is null"));
  }
}

Table TableMeasColumn::table() const
{
  return itsTabDataCol.table();
}

Bool TableMeasColumn::isScalar() const
{
  if (itsTabDataCol.columnDesc().isScalar()) {
    return True;
  }
  IPosition shape = itsTabDataCol.shapeColumn();
  if (shape.nelements() == 1) {
    if (itsNvals == 0  ||  Int(itsNvals) == shape(0)) {
      return True;
    }
  }
  return False;
}

} //# NAMESPACE CASACORE - END
