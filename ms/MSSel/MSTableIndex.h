 //# MSTableIndex: index into a MeasurementSet sub-table
//# Copyright (C) 2000,2001,2002
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
//#
//# $Id$

#ifndef MS_MSTABLEINDEX_H
#define MS_MSTABLEINDEX_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ColumnsIndex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class Record;
// class ColumnsIndex;
class String;

// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> ColumnsIndex
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo>
//    <li> Make the searches smarter - for TIME sorted tables, if the time to search is
//         past the last seach, there's no need to search any earlier times.
//    <li> Need to handle the INTERVAL=-1 case fully
// </todo>
class MSTableIndex
{
public:
    // no index attached, use the attach function or assignment operator to change that
    MSTableIndex();

    // construct one using the indicated subtable which is part of the parent MS
    // using the indicated index columns.  All index columns must be scalar integer
    // columns.  TIME and INTERVAL will be used when present.  A compare function
    // can be provided to over-ride literal matching of column values.
    MSTableIndex(const Table &subTable, const Vector<String> &indexCols, 
                 ColumnsIndex::Compare *compareFunction = 0);

    // construct one from another
    MSTableIndex(const MSTableIndex &other);

    virtual ~MSTableIndex();

    // assignment operator, refernce semantics
    MSTableIndex &operator=(const MSTableIndex &other);

    // attach this to a subtable using indexCols
    void attach(const Table &subTable, const Vector<String> &indexCols,
                        ColumnsIndex::Compare *compareFunction = 0);

    // Call this when an index in an existing row has changed.  There is no need to
    // call this when new rows are added to the table
    virtual void setChanged();

    // access the record of index (integer) keys
    virtual Record &accessKey() {return *key_p;}

    // access the TIME to use in the search (seconds)
    virtual Double &time() {return time_p;}

    // access the INTERVAL to use in the search (seconds), must be >= 0
    virtual Double &interval() {return interval_p;}

    // get all of the rows in the subTable which have data during the indicated time and
    // interval values.  For now, this code will miss the case where the subtable has
    // interval = -1 and the start time is outside of the time range implied by the time
    // and interval.  If the table has changed and the time is > 
    virtual Vector<uInt> getRowNumbers();

    // get the row number which falls in the interval and has the time nearest to the
    // center of the interval (time()).  This also has the same problem as the previous function.
    virtual uInt getNearestRow(Bool &found);

    // is this attached to a null table
    virtual Bool isNull() { return tab_p.isNull();}

    // return the subtable being indexed
    virtual Table &table() {return tab_p;}
private:
    // the subtable
    Table tab_p;

    ROScalarColumn<Double> timeColumn_p, intervalColumn_p;
    Vector<Double> timeVec_p, intervalVec_p;
    const Double *timeVals_p, *intervalVals_p;
    Bool deleteItTime_p, deleteItInterval_p;

    // Internal keys - set by user
    Record *key_p;
    Block<RecordFieldPtr<Int> > intKeys_p;
    Double time_p, interval_p;

    // last known integer key values
    Vector<Int> lastKeys_p;
    // last known time and interval
    Double lastTime_p, lastInterval_p;

    // last search result - matching integer keys
    Vector<uInt> lastSearch_p;

    // last nearest
    Int lastNearest_p;
    Bool nearestFound_p, nearestReady_p;

    // last known sub-table size
    uInt nrows_p;

    Bool hasChanged_p;

    ColumnsIndex *index_p;
    Block<RecordFieldPtr<Int> > indexKeys_p;
    Bool hasTime_p, hasInterval_p;

    void clear();
    void makeKeys();
    Bool keysChanged();
    void getInternals();
    void nearestTime();
};


} //# NAMESPACE CASACORE - END

#endif
    
