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
#include <casa/aips.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class DataManager;
class Table;
class String;


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
protected:
    // Construct the object.
    RODataManAccessor();

    ~RODataManAccessor();

    // Copy constructor (copy semantics).
    RODataManAccessor (const RODataManAccessor& that);

    // Assignment (copy semantics).
    RODataManAccessor& operator= (const RODataManAccessor& that);

    // Get the data manager for the given data manager name.
    DataManager* findDataManager (const Table& table,
				  const String& dataManagerName) const;
};



inline RODataManAccessor::RODataManAccessor()
{}
inline RODataManAccessor::~RODataManAccessor()
{}
inline RODataManAccessor::RODataManAccessor (const RODataManAccessor&)
{}
inline RODataManAccessor& RODataManAccessor::operator=
                                            (const RODataManAccessor&)
{ return *this; }




} //# NAMESPACE CASA - END

#endif
