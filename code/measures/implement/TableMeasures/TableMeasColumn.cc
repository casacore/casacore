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
#include <aips/TableMeasures/TableMeasColumn.h>
#include <aips/TableMeasures/TableMeasDescBase.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/String.h>


ROTableMeasColumn::ROTableMeasColumn()
: itsNvals      (0),
  itsVarRefFlag (False),
  itsVarOffFlag (False)
{}

ROTableMeasColumn::ROTableMeasColumn (const Table& tab,
				      const String& columnName)
: itsNvals      (0),
  itsTabDataCol (tab, columnName)
{
  itsDescPtr = TableMeasDescBase::reconstruct (tab, columnName);
  itsVarRefFlag = itsDescPtr->isRefCodeVariable();
  itsVarOffFlag = itsDescPtr->isOffsetVariable();
}

ROTableMeasColumn::ROTableMeasColumn (const ROTableMeasColumn& that)
: itsNvals      (that.itsNvals),
  itsDescPtr    (that.itsDescPtr),
  itsTabDataCol (that.itsTabDataCol),
  itsVarRefFlag (that.itsVarRefFlag),
  itsVarOffFlag (that.itsVarOffFlag)
{}

ROTableMeasColumn::~ROTableMeasColumn()
{}
 
void ROTableMeasColumn::reference (const ROTableMeasColumn& that)
{
  itsNvals   = that.itsNvals;
  itsDescPtr = that.itsDescPtr;
  itsTabDataCol.reference (that.itsTabDataCol);
  itsVarRefFlag = that.itsVarRefFlag;
  itsVarOffFlag = that.itsVarOffFlag;
}

void ROTableMeasColumn::attach (const Table& tab, const String& columnName)
{
  reference (ROTableMeasColumn (tab, columnName));
}
 
const String& ROTableMeasColumn::columnName() const
{
  return itsDescPtr->columnName();
}

Bool ROTableMeasColumn::isDefined (uInt rownr) const
{
  return itsTabDataCol.isDefined (rownr);
}

void ROTableMeasColumn::throwIfNull() const
{
  if (isNull()) {
    throw (TableInvOper("ROMeasTableColumn object is null"));
  }
}

Table ROTableMeasColumn::table() const
{
  return itsTabDataCol.table();
}
