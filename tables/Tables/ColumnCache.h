//# ColumnCache.h: A caching object for a table column
//# Copyright (C) 1997
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

#ifndef TABLES_COLUMNCACHE_H
#define TABLES_COLUMNCACHE_H


//# Includes
#include <casacore/casa/aips.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A caching object for a table column.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=ScalarColumn>ScalarColumn</linkto>
// </prerequisite>

// <synopsis>
// ColumnCache acts as a cache for a table column.
// It contains a pointer to data and the start and end row number
// for which these data are valid. An increment is part of the object
// and is usually 0 or 1. The value 0 is used for data which is
// valid for multiple rows (as used in
// <linkto class=IncrementalStMan>IncrementalStMan</linkto>).
// The value 1 is used for data stored consecutevily in a buffer for
// each row (as used in <linkto class=StManAipsIO>StManAipsIO</linkto>).
// <p>
// The ColumnCache object is created and updated by the data manager.
// The top level <linkto class=ScalarColumn>ScalarColumn</linkto> object
// contains a pointer to the cache object. In this way the
// <src>ScalarColumn::get</src> can often be executed by a few inlined
// statements which improves performance considerably.
// <p>
// The <src>invalidate</src> function can be used to invalidate the
// cache. This is for instance needed when a table lock is acquired
// or released to be sure that the cache gets refreshed.
// </synopsis> 

// <motivation>
// This class was developed to improve the performance for getting a scalar.
// </motivation>

// <todo asof="$DATE:$">
//  <li>For ConcatColumn add the ability to have other ColumnCache objects
//      using this one and invalidate them as well.
// </todo>


class ColumnCache
{
public:
    // Constructor.
    // It sets the increment to 1 and calls invalidate.
    ColumnCache();

    // Set the increment to the given value.
    void setIncrement (uInt increment);

    // Set the start and end row number for which the given data pointer
    // is valid.
    void set (uInt startRow, uInt endRow, const void* dataPtr);

    // Invalidate the cache.
    // This clears the data pointer and sets startRow>endRow.
    void invalidate();

    // Calculate the offset in the cached data for the given row.
    // -1 is returned if the row is not within the cached rows.
    Int offset (uInt rownr) const;

    // Give a pointer to the data.
    // The calling function has to do a proper cast after which the
    // calculated offset can be added to get the proper data.
    const void* dataPtr() const;

    // Give the start, end (including), and increment row number
    // of the cached column values.
    uInt start() const {return itsStart;}
    uInt end() const {return itsEnd;}
    uInt incr() const {return itsIncr;}

private:
    uInt  itsStart;
    uInt  itsEnd;
    uInt  itsIncr;
    const void* itsData;
};


inline void ColumnCache::setIncrement (uInt increment)
{
    itsIncr = increment;
}

inline void ColumnCache::invalidate()
{
    set (1, 0, 0);
}

inline Int ColumnCache::offset (uInt rownr) const
{
    return rownr<itsStart || rownr>itsEnd  ?  -1 :
	                                      Int((rownr-itsStart)*itsIncr);
}

inline const void* ColumnCache::dataPtr() const
{
    return itsData;
}

/*
inline uInt ColumnCache::start() const
{
    return itsStart;
}
inline uInt ColumnCache::end() const
{
    return itsEnd;
}
inline uInt ColumnCache::incr() const
{
    return itsIncr;
}
*/


} //# NAMESPACE CASACORE - END

#endif
