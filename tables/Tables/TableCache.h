//# TableCache.h: Cache of open tables
//# Copyright (C) 1994,1995,1997,1999
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

#ifndef TABLES_TABLECACHE_H
#define TABLES_TABLECACHE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class PlainTable;


// <summary>
// Cache of open tables
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <etymology>
// TableCache represents a cache of open tables.
// </etymology>

// <synopsis> 
// A TableCache object keeps track of the tables which have already
// been opened in a program. It maps the name of a table to its
// PlainTable object.
// In principle only one TableCache object (statically defined in
// class PlainTable) exists in a process.
// The cache is used to prevent a table from being opened more than
// once, which is not only a waste of space, but more importantly,
// may give rise to synchronization problems.
// Synchronization between the same table in multiple processes must
// be done by a locking mechanism.
//
// TableCache is used by class Table and PlainTable.
// Before opening a table, Table will first look in the cache.
// Newly opened or created tables will be added to the cache.
// When a table is actually closed, it will be removed from the cache.
// </synopsis> 

// <motivation>
// When a RefTable is read back, it will also read back the table it
// references. However, that table may have been opened before and
// it is bad to have a table open more than once in the same program.
// The TableCache class catches this and will not reopen the table.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Currently only PlainTables are taken into account.
//          Maybe RefTables should be too.
// </todo>


class TableCache
{
public:

    // Construct an empty cache of open tables.
    TableCache();

    ~TableCache();

    // Try to find a table with the given name in the cache.
    // Return a pointer to a table if found (thus if already open).
    // Return a zero pointer if not found.
    PlainTable* operator() (const String& tableName) const;

    // Try to find a table at the given index in the cache.
    // Return a pointer to a table if found (thus if already open).
    // Return a zero pointer if not found.
    PlainTable* operator() (uInt index) const;

    // Return the number of open tables in the cache.
    uInt ntable() const;

    // Add an open table to the cache.
    void define (const String& tableName, PlainTable*);

    // Remove an open table.
    void remove (const String& tableName);

    // Rename an open table.
    // If oldName is not in the cache, nothing will be done.
    void rename (const String& newName, const String& oldName);

private:
    // The copy constructor is forbidden.
    TableCache (const TableCache&);
    // The assignment operator is forbidden.
    TableCache& operator= (const TableCache&);

    //# void* iso. PlainTable* is used in the map declaration
    //# to reduce the number of template instantiations.
    //# The .cc file will use (fully safe) casts.
    SimpleOrderedMap<String,void*> tableMap_p;
    //# A mutex to synchronize access to the cache.
    mutable Mutex itsMutex;
};



} //# NAMESPACE CASACORE - END

#endif
