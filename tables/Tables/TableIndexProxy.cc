//# TableIndexProxy.cc: Holder of table index for the table glish client
//# Copyright (C) 2002
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

#include <casacore/tables/Tables/TableIndexProxy.h>
#include <casacore/tables/Tables/TableProxy.h>
#include <casacore/casa/Arrays/ArrayMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableIndexProxy::TableIndexProxy (const TableProxy& tablep,
				  const Vector<String>& columnNames,
				  Bool noSort)
: scaIndex_p (0),
  arrIndex_p (0)
{
  if (columnNames.nelements() == 1) {
    const String& colName = columnNames(0);
    const TableDesc& td = tablep.table().tableDesc();
    if (td.isColumn(colName)  &&  td[colName].isArray()) {
      arrIndex_p = new ColumnsIndexArray (tablep.table(), colName);
      return;
    }
  }
  scaIndex_p = new ColumnsIndex (tablep.table(), columnNames, 0, noSort);
}

TableIndexProxy::TableIndexProxy (const TableIndexProxy& that)
: scaIndex_p (0),
  arrIndex_p (0)
{
  if (that.scaIndex_p != 0) {
    scaIndex_p = new ColumnsIndex (*that.scaIndex_p);
  }
  if (that.arrIndex_p != 0) {
    arrIndex_p = new ColumnsIndexArray (*that.arrIndex_p);
  }
}

TableIndexProxy::~TableIndexProxy()
{
  delete scaIndex_p;
  delete arrIndex_p;
}

Bool TableIndexProxy::isUnique() const
{
  if (scaIndex_p != 0) {
    return scaIndex_p->isUnique();
  }
  return arrIndex_p->isUnique();
}

Vector<String> TableIndexProxy::columnNames() const
{
  if (scaIndex_p != 0) {
    return scaIndex_p->columnNames();
  }
  Vector<String> names(1);
  names(0) = arrIndex_p->columnName();
  return names;
}

void TableIndexProxy::setChanged (const Vector<String>& columnNames)
{
  if (columnNames.nelements() == 0) {
    if (scaIndex_p != 0) {
      scaIndex_p->setChanged();
    } else {
      arrIndex_p->setChanged();
    }
  } else {
    for (uInt i=0; i<columnNames.nelements(); i++) {
      if (scaIndex_p != 0) {
	scaIndex_p->setChanged (columnNames(i));
      } else {
	arrIndex_p->setChanged (columnNames(i));
      }
    }
  }
}

Int TableIndexProxy::getRowNumber (const Record& key)
{
  Bool found;
  Int rownr;
  if (scaIndex_p != 0) {
    rownr = scaIndex_p->getRowNumber (found, key);
  } else {
    rownr = arrIndex_p->getRowNumber (found, key);
  }
  if (!found) {
    rownr = -1;
  }
  return rownr;
}

Vector<Int> TableIndexProxy::getRowNumbers (const Record& key)
{
  Vector<uInt> rows;
  if (scaIndex_p != 0) {
    rows = scaIndex_p->getRowNumbers (key);
  } else {
    rows = arrIndex_p->getRowNumbers (key);
  }
  Vector<Int> rownrs(rows.shape());
  convertArray (rownrs, rows);
  return rownrs;
}

Vector<Int> TableIndexProxy::getRowNumbersRange (const Record& lower,
						 const Record& upper,
						 Bool lowerInclusive,
						 Bool upperInclusive)
{
  Vector<uInt> rows;
  if (scaIndex_p != 0) {
    rows = scaIndex_p->getRowNumbers (lower, upper, lowerInclusive,
				      upperInclusive);
  } else {
  rows = arrIndex_p->getRowNumbers (lower, upper, lowerInclusive,
				    upperInclusive);
  }
  Vector<Int> rownrs(rows.shape());
  convertArray (rownrs, rows);
  return rownrs;
}

} //# NAMESPACE CASACORE - END
