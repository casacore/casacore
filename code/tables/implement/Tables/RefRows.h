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
friend class RefRowsSliceIter;
friend class RefRowsRowIter;

public:

    // Create the object from a Vector containing the row numbers.
    // When <src>collapse==True</src>, it will collapse the vector
    // by combining adjacent row numbers.
    // If False, it is assumed that the vector is already collapsed.
    RefRows (const Vector<Int>& rowNumbers, Bool collapse = True);

    // Create the object from a Vector containing the row numbers.
    // It will collapse the vector by combining adjacent row numbers.
    RefRows (const Vector<uInt>& rowNumbers);

    // Create the object from the start,end,incr.
    RefRows (uInt start, uInt end, uInt incr=1);

    // Copy constructor (reference semantics).
    RefRows (const RefRows& other);

    // Assignment (copy semantics).
    RefRows& operator= (const RefRows& other);

    ~RefRows();

    // Do this and the other object reference the same rows?
    Bool operator== (const RefRows& other) const;

    // Return the number of rows given by this object.
    uInt nrow() const
        { return (itsNrows == 0  ?  fillNrows() : 0); }

    // Return the first row in the object.
    uInt firstRow() const
        { return itsRows(0); }

private:
    // Fill the itsNrows variable.
    uInt fillNrows() const;

    // Get the true row vector.
    const Vector<Int>& rowVector() const
        { return itsRows; }

    Vector<Int> itsRows;
    uInt        itsNrows;            //# 0 = still unknown
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
    Vector<Int> itsRows;
    uInt        itsStart;
    uInt        itsEnd;
    uInt        itsIncr;
    uInt        itsPos;
    Bool        itsPastEnd;
};



class RefRowsRowIter
{
public:
    RefRowsRowIter (const RefRows&);

    // Reset the iterator.
    void reset();

    // Is the iterator past the end?
    Bool pastEnd() const
        { return itsPastEnd; }

    // Go to the next row.
    // <group>
    void operator++()
        { next(); }
    void operator++(Int)
        { next(); }
    void next()
        { itsRow += itsIncr; if (itsRow > itsEnd) nextSlice(); }
    // </group>

    // Get the current row.
    uInt row() const
        { return itsRow; }

private:
    // Go to the next row slice.
    void nextSlice();

    Vector<Int> itsRows;
    uInt        itsRow;
    uInt        itsEnd;
    uInt        itsIncr;
    uInt        itsPos;
    Bool        itsPastEnd;
};



#endif
