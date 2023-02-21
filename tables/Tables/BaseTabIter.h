//# BaseTabIter.h: Base class for table iterator
//# Copyright (C) 1994,1995,1996,1997,1999
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

#ifndef TABLES_BASETABITER_H
#define TABLES_BASETABITER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableColumn;
class RefTable;
class String;


// <summary>
// Base class for table iterator
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> BaseTable
//   <li> TableIterator
// </prerequisite>

// <etymology>
// BaseTableIterator is the base class for the classes doing
// the actual iterating through a table.
// </etymology>

// <synopsis> 
// BaseTableIterator is the base class for the table iterators.
// It is a letter class of the envelope TableIterator.
// Currently there are no classes derived from BaseTableIterator,
// since it can do all the work itself. However, in the future
// this need not to be true anymore.
//
// BaseTableIterator iterates by sorting the table in the required
// order and then creating a RefTable for each step containing the
// rows for that iteration step. Each iteration step assembles the
// rows with equal key values.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class BaseTableIterator
{
public:

    // Create the table iterator to iterate through the given
    // columns in the given order. The given compare objects
    // will be used for the sort and to compare if values are equal.
    // If a compare object in cmpObjs is null, the default ObjCompare<T> 
    // will be used.
    // If cacheIterationBoundaries is set to true then the iteration
    // boundaries computed at construction time while sorting the table
    // are used when advancing with next(). Otherwise, for each next()
    // call the comparison functions are reevaluated again to get the
    // iteration boundary. This improves performance in general but will
    // break existing applications that change the comparison objects
    // (cmpObjs) between iterations.
    BaseTableIterator (const std::shared_ptr<BaseTable>&,
                       const Block<String>& columnNames,
                       const Block<std::shared_ptr<BaseCompare>>& cmpObjs,
                       const Block<Int>& orders,
                       int option,
                       bool cacheIterationBoundaries = false);

    // Clone this iterator.
    BaseTableIterator* clone() const;

    virtual ~BaseTableIterator();

    // Assignment is not needed, because the assignment operator in
    // the envelope class TableIterator has reference semantics.
    BaseTableIterator& operator= (const BaseTableIterator&) = delete;

    // Reset the iterator (i.e. restart iteration).
    virtual void reset();

    // Return the next group.
    virtual std::shared_ptr<BaseTable> next();

    virtual void copyState(const BaseTableIterator &);

    // Report Name of slowest sort column that changed (according to the
    // comparison function) to terminate the most recent call to next()
    // Enables clients to sense iteration boundary properties
    // and organize associated iterations
    inline const String& keyChangeAtLastNext() const
      { return keyChangeAtLastNext_p; }

protected:
    std::shared_ptr<BaseTable> sortTab_p; //# Table sorted in iteration order
    rownr_t                lastRow_p;     //# last row used from reftab
    uInt                   nrkeys_p;      //# nr of columns in group
    String                 keyChangeAtLastNext_p;  //# name of column that terminated most recent next()
    PtrBlock<BaseColumn*>  colPtr_p;      //# pointer to column objects
    Block<std::shared_ptr<BaseCompare>> cmpObj_p;  //# comparison object per column

    // Copy constructor (to be used by clone)
    BaseTableIterator (const BaseTableIterator&);

    std::shared_ptr<BaseTable> noCachedIterBoundariesNext();

private:
    Block<void*>           lastVal_p;     //# last value per column
    Block<void*>           curVal_p;      //# current value per column

    std::shared_ptr<Vector<rownr_t>> sortIterBoundaries_p;
    std::shared_ptr<Vector<size_t>> sortIterKeyIdxChange_p;
    Vector<rownr_t>::iterator sortIterBoundariesIt_p;
    Vector<size_t>::iterator  sortIterKeyIdxChangeIt_p;
    RefTable* aRefTable_p;      //# RefTable returned in each iteration
    std::shared_ptr<BaseTable> aBaseTable_p; //# Same as above for automatic deletion
};



} //# NAMESPACE CASACORE - END

#endif
