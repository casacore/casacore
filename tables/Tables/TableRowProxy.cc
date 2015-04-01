//# TableRowProxy.cc: Holder of table rows for the table glish client.
//# Copyright (C) 1994,1995,1996,2005
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


#include <casacore/tables/Tables/TableRowProxy.h>
#include <casacore/tables/Tables/TableProxy.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableRowProxy::TableRowProxy()
: isWritable_p (False)
{}

TableRowProxy::TableRowProxy (const TableProxy& tablep,
			      const Vector<String>& columnNames,
			      Bool exclude)
: isWritable_p (tablep.isWritable())
{
  if (columnNames.nelements() == 0) {
    rorow_p = ROTableRow (tablep.table(), False);
    if (isWritable_p) {
      rwrow_p = TableRow (tablep.table(), False);
    }
  } else {
    rorow_p = ROTableRow (tablep.table(), columnNames, exclude);
    if (isWritable_p) {
      rwrow_p = TableRow (tablep.table(), columnNames, exclude);
    }
  }
}

TableRowProxy::TableRowProxy (const TableRowProxy& that)
: isWritable_p (that.isWritable_p),
  rorow_p      (that.rorow_p),
  rwrow_p      (that.rwrow_p)
{}

TableRowProxy::~TableRowProxy()
{}

TableRowProxy& TableRowProxy::operator= (const TableRowProxy& that)
{
  if (this != &that) {
    isWritable_p = that.isWritable_p;
    rorow_p      = that.rorow_p;
    rwrow_p      = that.rwrow_p;
  }
  return *this;
}

Bool TableRowProxy::isNull() const
{
  return (rorow_p.isAttached()  ?  False : True);
}

Record TableRowProxy::get (uInt rownr) const
{
  return TableProxy::getKeyValues (rorow_p.get (rownr, True));
}

void TableRowProxy::put (uInt rownr, const Record& record,
			 Bool matchingFields)
{
  if (!isWritable_p) {
    throw TableError ("TableRowProxy: the given TableRow is not writable");
  }
  TableRecord trec;
  TableProxy::putKeyValues (trec, record);
  if (matchingFields) {
    rwrow_p.putMatchingFields (rownr, trec);
  } else {
    rwrow_p.put (rownr, trec);
  }
}

} //# NAMESPACE CASACORE - END
