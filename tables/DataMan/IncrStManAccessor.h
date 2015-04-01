//# IncrStManAccessor.h: Gives access to some IncrementalStMan functions
//# Copyright (C) 1996,1999,2000,2001
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

#ifndef TABLES_INCRSTMANACCESSOR_H
#define TABLES_INCRSTMANACCESSOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManAccessor.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ISMBase;
class DataManager;
class Table;
class String;

// <summary>
// Give access to some IncrementalStMan functions
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// <li> <linkto class=IncrementalStMan>IncrementalStMan</linkto>
// </prerequisite>

// <synopsis>
// The Table system has one or more storage managers underneath.
// One of these possible storage managers is the
// <linkto class=IncrementalStMan>IncrementalStMan</linkto>.
// This storage manager uses a cache of buckets. The default
// cache size is defined when the IncrementalStMan object was
// constructed at the time the table was created.
// <p>
// Sometimes it can be useful to change the cache size. E.g. when
// the table is accessed in a random way, the hit rate may drop
// when the cache is too small. The class ROIncrStManAccessor makes
// it possible to change the cache size in a temporary way.
// <br>
// Furthermore this class makes it possible to show the cache size
// and to show the cache statistics.

// <motivation>
// In principle a pointer to IncrementalStMan could be used.
// However, that would give access to all public functions.
// Furthermore it could not distinguish between read/write and readonly
// tables. 
// </motivation>

// <example>
// This example shows how to set the cache size for
// the incremental storage manager with the name "ISMExample". The cache
// size is not persistent, i.e. when the same table is reopened
// at a later time, this cache size is not remembered.
// <srcblock>
//  // Open a table.
//  Table table("someName.data");
//  // Set the cache size of its incremental storage manager ISMExample
//  // to 5 buckets.
//  ROIncrementalStManAccessor accessor(table, "ISMExample");
//  accessor.setCacheSize (5);
// </srcblock>
// </example>

//# <todo asof="$DATE:$">
//# </todo>


class ROIncrementalStManAccessor : public RODataManAccessor
{
public:

    // Construct the object for a data manager in the table given the name
    // of the data manager or the column.
    // An exception is thrown if the data manager type is not the incremental
    // storage manager.
    ROIncrementalStManAccessor (const Table& table, const String& name,
                                Bool byColumn=False);

    virtual ~ROIncrementalStManAccessor();

    // Copy constructor (reference semantics).
    ROIncrementalStManAccessor (const ROIncrementalStManAccessor& that);

    // Assignment (reference semantics).
    ROIncrementalStManAccessor& operator=
                                 (const ROIncrementalStManAccessor& that);

    // Set the cache size (in buckets) to be used by the
    // storage manager.
    // The cache size given in this way is not persistent.
    // Only the cache size given to the constructors of the incremental
    // storage managers, is persistent.
    // If <src>canExceedNrBuckets=True</src>, the given cache size can be
    // larger than the nr of buckets in the file. In this way the cache can
    // be made large enough for a future file extnsion.
    // Otherwise, it is limited to the actual number of buckets. This is useful
    // if one wants the entire file to be cached.
    void setCacheSize (uInt aSize, Bool canExceedNrBuckets=True);

    // Get the cache size (in buckets).
    uInt cacheSize() const;

    // Clear the caches used by the hypercubes in this storage manager.
    // It will flush the caches as needed and remove all buckets from them
    // resulting in a possibly large drop in memory used.
    void clearCache();

    // Show the index used by this storage manager.
    void showIndexStatistics (ostream& os) const;

    // Show the layout of the buckets used by this storage manager.
    void showBucketLayout (ostream& os) const;

    // Check that there are no repeated rowIds in the buckets comprising this ISM
    Bool checkBucketLayout (uInt &offendingCursor,
                            uInt &offendingBucketStartRow,
                            uInt &offendingBucketNrow,
                            uInt &offendingBucketNr,
                            uInt &offendingCol,
                            uInt &offendingIndex,
                            uInt &offendingRow,
                            uInt &offendingPrevRow) const;

private:
    //# Declare the data members.
    ISMBase* dataManPtr_p;
};



} //# NAMESPACE CASACORE - END

#endif
