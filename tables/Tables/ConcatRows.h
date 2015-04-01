//# ConcatRows.h: Class holding the row numbers in a ConcatTable
//# Copyright (C) 2008
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

#ifndef TABLES_CONCATROWS_H
#define TABLES_CONCATROWS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Class holding the row numbers in a ConcatTable
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="" tests="tConcatRows.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=Block>Block</linkto>
  // </prerequisite>

  // <synopsis>
  // ConcatRows is used to hold the row numbers forming the concatenation
  // of oher tables.
  // table. It contains a vector which can hold the row numbers in 2 ways:
  // <ol>
  // <li> As a normal series of row numbers. This is used by e.g. class
  //  <linkto class=ConcatTable>ConcatTable</linkto> 
  // <li> As a series of Slices. In this case 3 subsequent entries
  //  in the vector are used to represent start, end, and increment.
  //  This is used by a function like <src>ScalarColumn::getColumnRange</src>.
  // </ol>
  // Class <linkto class=ConcatRowsIter>ConcatRowsIter</linkto> can be
  // used to iterate through a ConcatRows object. Each step in the iteration
  // goes to the next slice. If the ConcatRows object contains a simple series
  // of row numbers, each slice contains only one row number.
  // This can degrade performance, so it is possible to use shortcuts by
  // testing if the object contains slices (using <src>isSliced()</src>)
  // and getting the row number vector directly (using <src>rowVector()</src>).
  // </synopsis>

  // <motivation>
  // ConcatRows is meant to have one class representing the various ways
  // of picking row numbers. This simplifies the interface of the table
  // and data manager classes dealing with getting/putting the data.
  // </motivation>

  //# <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned concatinements.
  //# </todo>


  class ConcatRows
  {
  public:
    // Construct an empty block.
    ConcatRows()
      : itsRows       (1,0),
	itsNTable     (0),
	itsLastStRow  (1),
	itsLastEndRow (0)
    {}

    // Reserve the block for the given nr of tables.
    void reserve (uInt ntable)
      { itsRows.resize (ntable+1); }

    // Add a table with the given nr of rows.
    void add (uInt nrow);

    // Give the nr of tables.
    uInt ntable() const
      { return itsNTable; }

    // Get the total nr of rows.
    uInt nrow() const
      { return itsRows[itsNTable]; }

    // Give the nr of rows for the i-th table.
    uInt operator[] (uInt i) const
      { return itsRows[i+1]; }

    // Give the offset for the i-th table.
    uInt offset (uInt i) const
      { return itsRows[i]; }

    // Map an overall row number to a table and row number.
    void mapRownr (uInt& tableNr, uInt& tabRownr, uInt rownr) const
    {
      if (rownr < itsLastStRow  ||  rownr >= itsLastEndRow) {
	findRownr (rownr);
      }
      tableNr = itsLastTableNr;
      tabRownr = rownr - itsLastStRow;
    }

  private:
    // Find the row number and fill in the lastXX_p values.
    void findRownr (uInt rownr) const;

    //# Data members.
    Block<uInt>  itsRows;
    uInt         itsNTable;
    mutable uInt itsLastStRow;         //# Cached variables to spped up
    mutable uInt itsLastEndRow;        //# function mapRownr().
    mutable uInt itsLastTableNr;
  };



  // <summary>
  // Class to iterate through a ConcatRows object.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="" tests="tConcatRows.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=ConcatRows>ConcatRows</linkto>
  // </prerequisite>

  // <synopsis>
  // ConcatRowsSliceIter is useful to iterate through a
  // <linkto class=ConcatRows>ConcatRows</linkto> object.
  // It is possible to define for which row
// especially if the ConcatRows object contains slices.
// Each step in the iteration returns a Slice object containing
// the next slice in the ConcatRows object.
// <br>
// It is used in Table and data manager classes (e.g. StManColumn).
// </synopsis>

// <example>
// This example shows how to iterate through a ConcatRows object
// (giving a slice) and through each of the slices.
// <srcblock>
// void somefunc (const ConcatRows& rownrs)
//   // Iterate through all slices.
//   ConcatRowsSliceIter rowiter(rownrs);
//   while (! rowiter.pastEnd()) {
//     // Get start, end, and increment for this slice.
//     uInt rownr = rowiter.sliceStart();
//     uInt end = rowiter.sliceEnd();
//     uInt incr = rowiter.sliceIncr();
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
//# A List of bugs, limitations, extensions or planned concatinements.
//# </todo>


  class ConcatRowsIter
  {
  public:
    // Construct the iterator on a ConcatRows object.
    // It is set to the full range.
    explicit ConcatRowsIter (const ConcatRows&);

    // Construct the iterator on a ConcatRows object for the given row range.
    ConcatRowsIter (const ConcatRows&, uInt start, uInt end, uInt incr=1);

    // Is the iterator past the end?
    Bool pastEnd() const
      { return itsPastEnd; }

    // Go the next chunk.
    // <group>
    void operator++()
      { next(); }
    void operator++(int)
      { next(); }
    void next();
    // </group>

    // Get the current chunk.
    RefRows getChunk() const
      { return RefRows(itsChunk, True); }

    // Get the nr of the table the current chunk is in.
    uInt tableNr() const
      { return itsPos; }

  private:
    const ConcatRows* itsRows;
    Vector<uInt>      itsChunk;
    uInt              itsStart;
    uInt              itsEnd;
    uInt              itsIncr;
    uInt              itsPos;
    Bool              itsPastEnd;
  };


} //# NAMESPACE CASACORE - END

#endif
