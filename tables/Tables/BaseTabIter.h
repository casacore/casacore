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
//#
//# $Id$

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
    // If a comare object is null, the default ObjCompare<T> will be used.
    BaseTableIterator (BaseTable*, const Block<String>& columnNames,
		       const Block<CountedPtr<BaseCompare> >&,
		       const Block<Int>& orders,
		       int option);

    // Clone this iterator.
    BaseTableIterator* clone() const;

    virtual ~BaseTableIterator();

    // Reset the iterator (i.e. restart iteration).
    virtual void reset();

    // Return the next group.
    virtual BaseTable* next();

protected:
    BaseTable*             sortTab_p;     //# Table sorted in iteration order
    uInt                   lastRow_p;     //# last row used from reftab
    uInt                   nrkeys_p;      //# nr of columns in group
    Block<void*>           lastVal_p;     //# last value per column
    Block<void*>           curVal_p;      //# current value per column
    PtrBlock<BaseColumn*>  colPtr_p;      //# pointer to column objects
    Block<CountedPtr<BaseCompare> > cmpObj_p;  //# comparison object per column

    // Copy constructor (to be used by clone)
    BaseTableIterator (const BaseTableIterator&);

private:
    // Assignment is not needed, because the assignment operator in
    // the envelope class TableIterator has reference semantics.
    // Declaring it private, makes it unusable.
    BaseTableIterator& operator= (const BaseTableIterator&);
};



} //# NAMESPACE CASACORE - END

#endif
