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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_REFROWS_H
#define TABLES_REFROWS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/RowNumbers.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Slicer;


// <summary>
// Class holding the row numbers in a RefTable
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tRefRows.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Vector>Vector</linkto>
// </prerequisite>

// <synopsis> 
// RefRows is used to hold the row numbers forming a view on another
// table. It contains a vector which can hold the row numbers in 2 ways:
// <ol>
// <li> As a normal series of row numbers. This is used by e.g. class
//  <linkto class=RefTable>RefTable</linkto> 
// <li> As a series of Slices. In this case 3 subsequent entries
//  in the vector are used to represent start, end, and increment.
//  This is used by a function like <src>ScalarColumn::getColumnRange</src>.
// </ol>
// Class <linkto class=RefRowsSliceIter>RefRowsSliceIter</linkto> can be
// used to iterate through a RefRows object. Each step in the iteration
// goes to the next a slice. If the RefRows objct contains a simple series
// of row numbers, each slice contains only one row number.
// This can degrade performance, so it is possible to use shortcuts by
// testing if the object contains slices (using <src>isSliced()</src>)
// and getting the row number vector directly (using <src>rowVector()</src>).
// </synopsis>

// <motivation>
// RefRows is meant to have one class representing the various ways
// of picking row numbers. This simplifies the interface of the table
// and data manager classes dealing with getting/putting the data.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class RefRows
{
public:

    // Create the object from a Vector containing the row numbers.
    // When <src>isSliced==False</src>, the vector is treated as
    // containing individual row numbers, otherwise as containing
    // (possibly multiple) slices in the form start,end,incr.
    // When <src>collapse==True</src>, it will try to collapse the
    // individual row numbers to the slice form (to save memory).
    RefRows (const Vector<rownr_t>& rowNumbers, Bool isSliced = False,
             Bool collapse = False);
#ifdef IMPLICIT_CTDS_32BIT
    RefRows (const Vector<uInt>& rowNumbers, Bool isSliced = False,
             Bool collapse = False);
#else
    explicit RefRows (const Vector<uInt>& rowNumbers, Bool isSliced = False,
                      Bool collapse = False);
#endif

    // Create the object from a single start,end,incr slice.
    RefRows (rownr_t start, rownr_t end, rownr_t incr=1);

    // Copy constructor (reference semantics).
    RefRows (const RefRows& other);

    // Assignment (copy semantics).
    RefRows& operator= (const RefRows& other);

    ~RefRows();

    // Do this and the other object reference the same rows?
    Bool operator== (const RefRows& other) const;

    // Convert this object to a RowNumbers object by applying the given row numbers.
    // It is used to convert the RefRows object with row numbers in a
    // RefTable to row numbers in the original root table.
    RowNumbers convert (const RowNumbers& rootRownrs) const;

    // Convert this object to a RowNumbers object by de-slicing it.
    // I.e. it linearizes the row numbers.
    RowNumbers convert() const;

    // Return the number of rows given by this object.
    // If the object contains slices, it counts the number of rows
    // represented by each slice. // <group>
    rownr_t nrows() const
        { return (itsNrows == 0  ?  fillNrows() : itsNrows); }
    rownr_t nrow() const
        { return (itsNrows == 0  ?  fillNrows() : itsNrows); }
    // </group>

    // Return the first row in the object.
    rownr_t firstRow() const
        { return itsRows(0); }

    // Represents the vector a slice?
    Bool isSliced() const
        { return itsSliced; }

    // Get the row vector as is (thus sliced if the object contains slices).
    // It is mainly useful to get all row numbers when the object does not
    // contain slices.
    const Vector<rownr_t>& rowVector() const
        { return itsRows; }

private:
    // Initialize the object.
    void init (const Vector<rownr_t>& rowNumbers, Bool isSliced,
               Bool collapse);

    // Fill the itsNrows variable.
    rownr_t fillNrows() const;

    Vector<rownr_t> itsRows;
    rownr_t         itsNrows;            //# 0 = still unknown
    Bool         itsSliced;           //# True = vector contains slices
};



// <summary>
// Class to iterate through a RefRows object.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tRefRows.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=RefRows>RefRows</linkto>
// </prerequisite>

// <synopsis> 
// RefRowsSliceIter is useful to iterate through a
// <linkto class=RefRows>RefRows</linkto> object,
// especially if the RefRows object contains slices.
// Each step in the iteration returns a Slice object containing
// the next slice in the RefRows object.
// <br>
// It is used in Table and data manager classes (e.g. StManColumn).
// </synopsis>

// <example>
// This example shows how to iterate through a RefRows object
// (giving a slice) and through each of the slices.
// <srcblock>
// void somefunc (const RefRows& rownrs)
//   // Iterate through all slices.
//   RefRowsSliceIter rowiter(rownrs);
//   while (! rowiter.pastEnd()) {
//     // Get start, end, and increment for this slice.
//     rownr_t rownr = rowiter.sliceStart();
//     rownr_t end = rowiter.sliceEnd();
//     rownr_t incr = rowiter.sliceIncr();
//     // Iterate through the row numbers in the slice.
//     while (rownr <= end) {
//       rownr += incr;
//     }
//     // Go to next slice.
//     rowiter++;
//   }
// }
// </srcblock>
// </example>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class RefRowsSliceIter
{
public:
    // Construct the iterator on a RefRows object.
    // It is set to the beginning.
    RefRowsSliceIter (const RefRows&);

    // Reset the iterator to the beginning.
    void reset();

    // Is the iterator past the end?
    Bool pastEnd() const
        { return itsPastEnd; }

    // Go the next slice.
    // <group>
    void operator++()
        { next(); }
    void operator++(int)
        { next(); }
    void next();
    // </group>

    // Get the current slice start, end, or increment.
    // <group>
    rownr_t sliceStart() const
        { return itsStart; }
    rownr_t sliceEnd() const
        { return itsEnd; }
    rownr_t sliceIncr() const
        { return itsIncr; }
    // </group>

private:
    Vector<rownr_t> itsRows;
    Bool            itsSliced;
    rownr_t         itsStart;
    rownr_t         itsEnd;
    rownr_t         itsIncr;
    rownr_t         itsPos;
    Bool            itsPastEnd;
};




} //# NAMESPACE CASACORE - END

#endif
