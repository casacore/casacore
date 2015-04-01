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
#include <casacore/tables/DataMan/DataManAccessor.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  RODataManAccessor::RODataManAccessor (const Table& table,
                                        const String& name,
                                        Bool byColumn)
    : itsDataManager(0)
  {
    itsDataManager = table.findDataManager (name, byColumn);
  }

  RODataManAccessor::~RODataManAccessor()
  {}

  void RODataManAccessor::setProperties (const Record& prop) const
  {
    if (itsDataManager) {
      itsDataManager->setProperties (prop);
    } else {
      throw DataManError ("setProperties cannot be used on a default "
                          "RODataManAccessor object");
    }
  }

  Record RODataManAccessor::getProperties() const
  {
    if (itsDataManager) {
      return itsDataManager->getProperties();
    }
    throw DataManError ("getProperties cannot be used on a default "
                        "RODataManAccessor object");
  }

} //# NAMESPACE CASACORE - END

