 //# NewMSTableIndex: index into a MeasurementSet sub-table
//# Copyright (C) 2000
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

#if !defined(AIPS_NEWMSTABLEINDEX_H)
#define AIPS_NEWMSTABLEINDEX_H

#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/Containers/RecordField.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ScalarColumn.h>

//# Forward declarations
class Record;
class ColumnsIndex;
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

class NewMSTableIndex
{
public:
    // no index attached, use the attach function or assignment operator to change that
    NewMSTableIndex();

    // construct one using the indicated subtable which is part of the parent MS
    // using the indicated index columns.  All index columns must be scalar integer
    // columns.  TIME and INTERVAL will be used when present. 
    NewMSTableIndex(const Table &subTable, const Vector<String> &indexCols);

    // construct one from another
    NewMSTableIndex(const NewMSTableIndex &other);

    virtual ~NewMSTableIndex();

    // assignment operator, refernce semantics
    virtual NewMSTableIndex &operator=(const NewMSTableIndex &other);

    // attach this to a subtable using indexCols
    virtual void attach(const Table &subTable, const Vector<String> &indexCols);

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
private:
    // the subtable
    Table tab_p;

    ROScalarColumn<Double> timeColumn_p;

    // Internal keys
    Record *key_p;
    Block<RecordFieldPtr<Int> > intKeys_p;

    Double time_p, interval_p;

    // last known integer key values
    Vector<Int> lastKeys_p;

    // last known time and interval
    Double lastTime_p, lastInterval_p;

    // last search
    Vector<uInt> lastSearch_p;

    // last nearest
    uInt lastNearest_p;
    Bool nearestFound_p, nearestReady_p;

    // last known sub-table size
    uInt nrows_p;

    Bool hasChanged_p;

    ColumnsIndex *index_p;
    Block<RecordFieldPtr<Int> > lowerIndexKeys_p;
    Block<RecordFieldPtr<Int> > upperIndexKeys_p;
    RecordFieldPtr<Double> lowerTimeKey_p, upperTimeKey_p;
    Bool hasTime_p, hasInterval_p;

    void clear();
    void makeKeys();
    Bool keysChanged();
    void getInternals();

    // comparison function
    static Int compare(const Block<void *>& fieldPtrs,
		       const Block<void *>& dataPtrs,
		       const Block<Int> &dataTypes,
		       Int index);
   
    // test function, only used in a DebugAssert in compare
    static Bool okDataTypes(const Block<Int> &dataTypes);
};

#endif
    
