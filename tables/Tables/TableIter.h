//# TableIter.h: Iterate through a Table
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#ifndef TABLES_TABLEITER_H
#define TABLES_TABLEITER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/Compare.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class BaseTableIterator;
class String;
template<class T> class Block;


// <summary>
// Iterate through a Table
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Table
//   <li> Sort
// </prerequisite>

// <synopsis> 
// TableIterator is a class allowing one to iterate in an arbitrary
// way through a table. Each iteration step returns a Table
// containing the result of the iteration step.
// It is possible to have more than one iterator on a table.
//
// An iteration is defined by giving the columns over which to iterate.
// For example, take a UV data set with "axes" frequency, baseline and
// time. Getting all frequencies per time and baseline can be done
// by iterating over columns time and baseline (as shown in the example).
// The main iteration column must be given first.
// It is possible to define an iteration order per column.
// <br>It is also possible to define a compare object per column.
// For example, CompareIntervalReal can be used to iterate in intervals
// over, say, the TIME column by treating a range of values as equal
// (e.g. iterate in 60 seconds time intervals).
//
// The table is sorted before doing the iteration unless TableIterator::NoSort
// is given.
// </synopsis> 

// <example>
// <srcblock>
//    // Iterate over time and baseline (by default in ascending order).
//    // Time is the main iteration order.
//    Table t;
//    Table tab ("UV_Table.data");
//    Block<String> iv0(2);
//    iv0[0] = "time";
//    iv0[1] = "baseline";
//    // Create the iterator. This will prepare the first subtable.
//    TableIterator iter(tab, iv0);
//    Int nr = 0;
//    while (!iter.pastEnd()) {
//      // Get the first subtable.
//      // This will contain rows with equal time and baseline.
//	t = iter.table();
//	cout << t.nrow() << " ";
//	nr++;
//      // Prepare the next subtable with the next time,baseline value.
//	iter.next();
//    }
//    cout << endl << nr << " iteration steps" << endl;
// </srcblock>
// </example>

// <motivation>
// It is sometimes needed to access all data in a table in a grouped
// way; for example, all frequencies per time and baseline.
// This can perfectly be done with an iterator.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableIterator
{
public:

    // Define the possible iteration orders.
    enum Order {Ascending=Sort::Ascending, Descending=Sort::Descending};
    // Define the possible sorts.
    enum Option {QuickSort= Sort::QuickSort,
                 HeapSort = Sort::HeapSort,
                 InsSort  = Sort::InsSort,
                 ParSort  = Sort::ParSort,
                 NoSort   = 64};

    // Create a null TableIterator object (i.e. no iterator is attached yet).
    // The sole purpose of this constructor is to allow construction
    // of an array of TableIterator objects.
    // The assignment operator can be used to make a null object
    // reference a column.
    // Note that sort functions, etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    TableIterator();

    // Create a table iterator for the given table.
    // Each iteration step results in a Table containing all
    // rows in which the values in each given column is equal.
    // An iteration order can be given; it defaults to Ascending.
    // Per column a compare object can be given to use other compare
    // functions than the standard ones defined in Compare.h.
    // The compare functions are used for both the sort and the iteration.
    // The option argument makes it possible to choose from various
    // sorting algorithms. Usually ParSort is the fastest, but for
    // a single core machine QuickSort usually performs better.
    // InsSort (insertion sort) should only be used if the input
    // is almost in order.
    // If it is known that the table is already in order, the sort step can be
    // bypassed by giving the option TableIterator::NoSort.
    // The default option is ParSort.
    // <group>
    TableIterator (const Table&, const String& columnName,
		   Order = Ascending, Option = ParSort);
    TableIterator (const Table&, const Block<String>& columnNames,
		   Order = Ascending, Option = ParSort);
    // Give the iteration order per column.
    // <note>If an interval comparison object like CompareIntervalReal
    // is used, the data are sorted on the interval, not on the value.
    // One should consider to do an explicitsort on value and no iteration sort.
    // </note>
    TableIterator (const Table&, const Block<String>& columnNames,
		   const Block<Int>& orders, Option = ParSort);
    // Give the iteration order per column.
    // Give an optional compare object per column.
    // A zero pointer means that the default compare function will be used.
    TableIterator (const Table&, const Block<String>& columnNames,
		   const Block<CountedPtr<BaseCompare> >&,
		   const Block<Int>& orders, Option = ParSort);
    // </group>

    // Copy constructor (copy semantics).
    TableIterator (const TableIterator&);

    ~TableIterator();

    // Assignment (copy semantics).
    TableIterator& operator= (const TableIterator&);

    // Test if the object is null, i.e. does not reference a table yet.
    // This is the case if the default constructor is used.
    Bool isNull() const
	{ return (tabIterPtr_p == 0  ?  True : False); }

    // Throw an exception if the object is null, i.e.
    // if function isNull() is True.
    void throwIfNull() const;

    // Reset the iterator (i.e. restart iteration).
    void reset();

    // Test if at the end.
    Bool pastEnd() const;

    // Go to the next group.
    // <group>
    void next();
    void operator++();
    void operator++(int);
    // </group>

    // Get the current group.
    Table table() const;

protected:
    BaseTableIterator* tabIterPtr_p;
    Table              subTable_p;
};



//# Iterator is at the end if the subtable is empty.
inline Bool TableIterator::pastEnd() const
    { return (subTable_p.nrow() == 0  ?  True : False); }

inline Table TableIterator::table() const
    { return subTable_p; }

inline void TableIterator::operator++()
    { next(); }

inline void TableIterator::operator++(int)
    { next(); }





} //# NAMESPACE CASACORE - END

#endif
