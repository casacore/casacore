//# TableAttr.cc: Some attributes of a table
//# Copyright (C) 2001
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
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/Table.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableAttr::TableAttr()
: openWritable_p (False)
{}

TableAttr::TableAttr (const Table& table)
: name_p         (table.tableName()),
  openWritable_p (table.isWritable()),
  lockOptions_p  (table.lockOptions())
{}

TableAttr::TableAttr (const String& name, Bool openWritable)
: name_p         (name),
  openWritable_p (openWritable)
{}

TableAttr::TableAttr (const String& name, Bool openWritable,
		      const TableLock& lockOptions)
: name_p         (name),
  openWritable_p (openWritable),
  lockOptions_p  (lockOptions)
{}

TableAttr::TableAttr (const TableAttr& that)
: name_p         (that.name_p),
  openWritable_p (that.openWritable_p),
  lockOptions_p  (that.lockOptions_p)
{}

TableAttr& TableAttr::operator= (const TableAttr& that)
{
    if (this != &that) {
      name_p         = that.name_p;
      openWritable_p = that.openWritable_p;
      lockOptions_p  = that.lockOptions_p;
    }
    return *this;
}

TableAttr::~TableAttr()
{}

void TableAttr::set (const Table& table)
{
    name_p         = table.tableName();
    openWritable_p = table.isWritable();
    lockOptions_p  = table.lockOptions();
}

} //# NAMESPACE CASACORE - END

