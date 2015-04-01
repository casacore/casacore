//# RowCopier.h: RowCopier copies part or all of a row from one table to another.
//# Copyright (C) 1995,2000
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


#ifndef TABLES_ROWCOPIER_H
#define TABLES_ROWCOPIER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Table;
template<class T> class Vector;
class String;
class ColumnHolder; //# Only in the .cc file


// <summary> 
// RowCopier copies all or part of a row from one table to another.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Mark Wieringa" date="21Nov94" tests="tRowCopier">
// </reviewed>

// <prerequisite>
//  <li> Table
// </prerequisite>

// <etymology>
// RowCopier is a class that copies rows from one table to another, hence
// the name.
// </etymology>

// <synopsis>
// The primary organization in a Table is the TableColumn.  However, data
// is often organized primarily by rows, at least initially (e.g. for an
// on-line system, the data will arrive in chunks that are likely to be
// individual rows rather than individual columns).  RowCopier is used 
// to copy values in a row from all or some of the columns of one table to
// another table.
//
// Some things to keep in mind:
// <ol>
// <li> For each column to be copied, the data types and dimensionality 
// must match.
// <li> The input row number need not be the same as the output row number.
// <li> The output row number must already exist (i.e. no new rows are created).
// <li> The output column name need not be the same as the input column name.
// <li> The output column name and input column name, when specified, must 
// already exist.
// <li> The output table and each output column must be writable.
// </ol>
// </synopsis>

// <example>
// In the FITS Binary Table extension to Table conversion class, BinTable,
// the input FITS file is a stream that must be read sequentially, so the
// input arrives row-by-row.  Internally, there is a single row table that
// is used to hold the values for the current row.  To fill a Casacore table
// with the data from each row, one creates the output table using the
// table descriptor from the input, single-row table and uses RowCopier to
// copy the single-row table to the appropriate row of the full table, 
// refilling the single-row table at each step. This is how that looks
// (leaving out some details not important to this example):
// 
// Background:
//   singleRowTab is a table constisting of a single row.  It is filled
//   from the input FITS classes using the fillRow() member function.
//   The nrows() member function returns the total number of FITS binary
//   table rows and currrow() returns the current row number.
//
// <srcblock>
// //   Create an empty Table able to hold all remaining FITS rows, including
// //   the current one and having the same descriptor as singleRowTab
//    SetupNewTable newTab("FullTable", singleRowTab.getDescriptor(), 
//                         Table:New);
//    Table full(newTab, (nrows() - currrow() + 1));
// //   create the copier to copy all columns
//    RowCopier copier(full, singleRowTab);
//    // loop over all remaining rows
//    // since full was just created, we start filling it at row 0.
//    for (uInt outRow = 0, fitsRow = currrow(); fitsRow < nrows();
//         outRow++, fitsRow++) {
//        // copy the only row from currRowTab (row 0) to the outRow of full
//        copier.copy(outRow, 0);
//        // fill the next row of currRowTab
//        fillRow();
//    }
// </srcblock>
//
// This example shows how to copy some of the values from one table to
// another.  This is a contrived example.  The input table 
// has columns named "HSource" and "VSource" along with other columns.
// This example places the values from these columns to columns named
// "RA (1950)" and "DEC (1950)" in the output table (which also has other
// columns).  Note that each input column must have the same type and
// dimensionality as the corresponding output column.  
//  
// <srcblock>
//  // construct a vector of the input column names to copy and the
//  // associated output column names
//  Vector<String> inColNames(2), outColNames(2);
//  inColNames(0) = "HSource"; outColNames(0) = "RA (1950)"
//  inColNames(1) = "VSource"; outColNames(1) = "DEC (1950)"
//
//  // construct the copier
//  RowCopier copier(inTable, outTable, inColNames, outColNames);
//
//  // Copy a row from in to out, obviously a typical use would do
//  // more than just one row.
//  copier.copy(outRownr, outRownr-1);
// </srcblock>
// </example>

// <motivation>
// See the comments in the synopsis.
// </motivation>

// <todo asof=$DATE:$">
//  <li> resize should probably happen in powers of 2
//  <li> is throwing exceptions really what we want to happen?
// </todo>


class RowCopier {
public:
    // This constructs a copier which will copy all columns which have the 
    // same name in both tables from in to out.
    // An exception is thrown if the columns having the same name in both
    // tables are not conformant (not the same type and not both scalar of both
    // array columns)
    // <thrown>
    //  <li> TableError
    // </thrown>
    RowCopier (Table &out, const Table &in);

    // This constructs a copier which will copy innames columns to outnames 
    // columns, outnames and innames must be conformant.  Columns are 
    // matched up element-by-element in innames and outnames.
    // An exception is thrown if an element of innames or outnames is not
    // present in the corresponding table, if innames and outnames are
    // not conformant and if the corresponding columns are not conformant
    // (not the same type and not both scalar or both array columns)
    // <thrown>
    //  <li> TableError
    // </thrown>
    RowCopier (Table &out, const Table &in, const Vector<String>& outNames,
	       const Vector<String>& inNames);
   
    // The things that actually do the copying when requested.
    // <group>
    // Copy different row numbers.
    Bool copy (uInt toRow, uInt fromRow);
    // Copy to and from the same row number
    Bool copy (uInt rownr);
    // </group>

    ~RowCopier();

private:
    //# The following constructors and operator don't seem to be useful
    // <group>
    RowCopier();
    RowCopier(const RowCopier &other);
    RowCopier &operator=(const RowCopier &other);
    // </group>

    // The ColumnHolder class exists only in the .cc file, it is what
    // ultimately does the work.
    CountedPtr<ColumnHolder> columns_p;
};


inline Bool RowCopier::copy (uInt rownr)
    { return copy (rownr, rownr); }



} //# NAMESPACE CASACORE - END

#endif
