//# RefRows.h: Class holding the row numbers in a RefTable
//# Copyright (C) 1998
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

#if !defined(AIPS_REFROWS_H)
#define AIPS_REFROWS_H

//# Includes
#include <aips/Arrays/Vector.h>

//# Forward Declarations
class Slicer;


// <summary>
// Class holding the row numbers in a RefTable
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// RefRows is used to make a view on another table.
// Usually it is a view on a subset of the table, either in vertical
// or horizontal direction. Thus a subset of rows and/or columns.
// It will be the result of a select, sort, project or iterate function.
//
// It acts to the user as a normal table. All gets and puts are
// handled by RefColumn which directs them to the referenced column
// while (if needed) converting the given row number to the row number
// in the referenced table. For that purpose RefRows maintains a
// Vector of the row numbers in the referenced table.
//
// The RefRows constructor acts in a way that it will always reference
// the original table. This means that if a select is done on a RefRows,
// the resulting RefRows will also reference the original PlainTable.
// This is done to avoid long chains of RefRowss.
// However, if ever some other kind of table views are introduced
// (like a join or a concatenation of similar tables), this cannot be
// used anymore. Most software already anticipates on that. The only
// exception is the code anding, oring tables (refAnd, etc.).
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class RefRows
{
public:

    // Create the object from a Vector containing the row numbers.
    // When <src>isSliced==False</src>, the vector is treated as
    // containing individual row numbers, otherwise as containing
    // slices in the form start,end,incr.
    // When <src>collapse==True</src>, it will try to collapse the
    // individual row numbers to the slice form.
    RefRows (const Vector<uInt>& rowNumbers, Bool isSliced = False,
	     Bool collapse = False);

    // Create the object from a single start,end,incr slice.
    RefRows (uInt start, uInt end, uInt incr=1);

    // Copy constructor (reference semantics).
    RefRows (const RefRows& other);

    // Assignment (copy semantics).
    RefRows& operator= (const RefRows& other);

    ~RefRows();

    // Do this and the other object reference the same rows?
    Bool operator== (const RefRows& other) const;

    // Convert this object to a Vector<uInt> by applying the given row numbers.
    // It is used to convert the RefRows object with row numbers in a
    // RefTable to row numbers in the original root table.
    Vector<uInt> convert (const Vector<uInt>& rootRownrs) const;

    // Return the number of rows given by this object.
    // <group>
    uInt nrows() const
        { return (itsNrows == 0  ?  fillNrows() : itsNrows); }
    uInt nrow() const
        { return (itsNrows == 0  ?  fillNrows() : itsNrows); }
    // </group>

    // Return the first row in the object.
    uInt firstRow() const
        { return itsRows(0); }

    // Represents the vector a slice?
    Bool isSliced() const
        { return itsSliced; }

    // Get the true row vector.
    const Vector<uInt>& rowVector() const
        { return itsRows; }

private:
    // Fill the itsNrows variable.
    uInt fillNrows() const;

    Vector<uInt> itsRows;
    uInt         itsNrows;            //# 0 = still unknown
    Bool         itsSliced;           //# True = vector contains slices
};



class RefRowsSliceIter
{
public:
    RefRowsSliceIter (const RefRows&);

    // Reset the iterator.
    void reset();

    // Is the iterator past the end?
    Bool pastEnd() const
        { return itsPastEnd; }

    // Go the next slice.
    // <group>
    void operator++()
        { next(); }
    void operator++(Int)
        { next(); }
    void next();
    // </group>

    // Get the current slice.
    // <group>
    uInt sliceStart() const
        { return itsStart; }
    uInt sliceEnd() const
        { return itsEnd; }
    uInt sliceIncr() const
        { return itsIncr; }
    // </group>

private:
    Vector<uInt> itsRows;
    Bool         itsSliced;
    uInt         itsStart;
    uInt         itsEnd;
    uInt         itsIncr;
    uInt         itsPos;
    Bool         itsPastEnd;
};



#endif
