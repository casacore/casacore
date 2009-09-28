//# DataManAccessor.cc: Base class for the Data Manager Accessor classes
//# Copyright (C) 1996
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
#include <tables/Tables/DataManAccessor.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/DataManError.h>
#include <casa/BasicSL/String.h>


namespace casa { //# NAMESPACE CASA - BEGIN

DataManager* RODataManAccessor::findDataManager (const Table& table,
					  const String& dataManagerName) const
{
    DataManager* dmptr = table.findDataManager (dataManagerName);
    if (dmptr == 0) {
	throw (DataManError ("Data manager name " + dataManagerName +
			     " does not exist in table " + table.tableName()));
    }
    return dmptr;
}

} //# NAMESPACE CASA - END

