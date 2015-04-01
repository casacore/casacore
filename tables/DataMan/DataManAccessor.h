//# DataManAccessor.h: Base class for the Data Manager Accessor classes
//# Copyright (C) 1996,1999
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

#ifndef TABLES_DATAMANACCESSOR_H
#define TABLES_DATAMANACCESSOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <iosfwd>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Base class for the Data Manager Accessor classes.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// <li> <linkto class=DataManager>DataManager</linkto>
// </prerequisite>

// <synopsis>
// The Table system has one or more data managers underneath.
// Once a table is constructed, these data managers are invisible
// and there is no way to get access to them.
// However, sometimes limited access to them is needed (e.g. to
// set the size of an internal cache).
// <p>
// This class should be used as the base class for specialized
// Data Manager Accessor classes (e.g. class
// <linkto class=ROIncrementalStManAccessor:description>
// ROIncrementalStManAccessor</linkto>.
// This base class provides the functionality to get the
// <src>DataManager</src> object for a given column.
// </synopsis> 

// <motivation>
// This base class makes it possible that every derived class can get the
// data manager, because RODataManAccessor is a friend of class Table.
// Otherwise all accessor classes needed to be friend of Table.
// </motivation>

//# <todo asof="$DATE:$">
//# </todo>


class RODataManAccessor
{
public:
    // Construct an empty object.
    RODataManAccessor()
      : itsDataManager(0)
    {}

    // Construct the accessor object for a data manager in the table.
    // An exception is thrown if the name of the data manager or column is
    // unknown.
    RODataManAccessor (const Table& table, const String& name,
                       Bool byColumn);

    virtual ~RODataManAccessor();

    // Set data manager properties using the fields in the record.
    // Each data manager has its specific set of properties.
    void setProperties (const Record&) const;

    // Get data manager properties as a record.
    Record getProperties() const;

    // Get the data manager type.
    String dataManagerType() const
      { return itsDataManager->dataManagerType(); } 

    // Get the data manager name.
    String dataManagerName() const
      { return itsDataManager->dataManagerName(); } 

    // Get the data manager sequence nr.
    uInt dataManagerSeqNr() const
      { return itsDataManager->sequenceNr(); } 

    // Show IO statistics.
    void showCacheStatistics (ostream& os) const
      { itsDataManager->showCacheStatistics (os); }

protected:
    // Get the data manager for the given data manager or column name.
    DataManager* baseDataManager() const
      { return itsDataManager; }

private:
    DataManager* itsDataManager;
};


} //# NAMESPACE CASACORE - END

#endif
