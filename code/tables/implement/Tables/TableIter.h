//# TableIter.h: Iterate through a Table
//# Copyright (C) 1994,1995,1996,1997
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

#if !defined(AIPS_TABLEITER_H)
#define AIPS_TABLEITER_H

#if defined(_AIX)
#pragma implementation ("TableIter.cc")
#endif 

//# Includes
#include <aips/aips.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/Compare.h>
#include <aips/Exceptions/Excp.h>

//# Forward Declarations
class BaseTableIterator;
class String;
template<class T> class Block;


// <summary>
// Iterate through a Table
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
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
// It is also possible to define a compare function per column.
// This can, for example, be used to iterate in intervals by treating
// a range of values as equal (e.g. iterate in 60 seconds time intervals).
//
// Currently the table is sorted before doing the iteration.
// This may change in the future when, for example, indices are supported.
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

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> maybe add the possibility to specify an object which
//          determines if a value is equal. This allows for iterating
//          in intervals in a more flexible way than can be achieved
//          with the compare functions.
// </todo>


class TableIterator : public Cleanup
{
public:

    // Define the possible iteration orders.
    enum Order {Ascending=-1, DontCare=0, Descending=1};
    // Define the possible sorts.
    enum Option {QuickSort, HeapSort, InsSort, NoSort};

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
    // An iteration order can be given; it default to DontCare.
    // Per column a compare function can be given to use other compare
    // functions than the standard ones defined in Compare.h.
    // The compare functions are used for both the sort and the iteration.
    // The option argument makes it possible to choose from various
    // sorting algorithms. Usually QuickSort is the fastest, but for
    // some ill-conditioned input HeapSort performs much better.
    // InsSort (insertion sort) should only be used when the input
    // is almost in order.
    // When the table is already in order, the sort step can be bypassed
    // by giving the option TableIterator::NoSort.
    // The default option is HeapSort.
    // <group>
    TableIterator (const Table&, const String& columnName,
		   Order = DontCare, Option = HeapSort);
    TableIterator (const Table&, const Block<String>& columnNames,
		   Order = DontCare, Option = HeapSort);
    // Give the iteration order per column.
    TableIterator (const Table&, const Block<String>& columnNames,
		   const Block<Int>& orders, Option = HeapSort);
    // Give the iteration order per column.
    // Give an optional compare function per column.
    // A zero pointer means that the default compare function will be used.
    TableIterator (const Table&, const Block<String>& columnNames,
		   const PtrBlock<ObjCompareFunc*>&,
		   const Block<Int>& orders, Option = HeapSort);
    // </group>

    // Copy constructor (copy semantics).
    TableIterator (const TableIterator&);

    ~TableIterator();

    //*display 8
    // Needed for Cleanup.
    void cleanup();

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



#endif
