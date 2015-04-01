//# IncrementalStMan.h: The Incremental Storage Manager
//# Copyright (C) 1996,1997,1999
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

#ifndef TABLES_INCREMENTALSTMAN_H
#define TABLES_INCREMENTALSTMAN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/ISMBase.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// The Incremental Storage Manager
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tIncrementalStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> The Table Data Managers concept as described in module file
//        <linkto module="Tables:Data Managers">Tables.h</linkto>
//   <li> <linkto class=ROIncrementalStManAccessor>
//        ROIncrementalStManAccessor</linkto>
//        for a discussion of the cache size
// </prerequisite>

// <etymology>
// IncrementalStMan is the data manager storing values in an incremental way
// (similar to an incremental backup). A value is only stored when it
// differs from the previous value.
// </etymology>

// <synopsis>
// IncrementalStMan stores the data in a way that a value is only stored
// when it is different from the value in the previous row. This storage
// manager is very well suited for columns with slowly changing values,
// because the resulting file can be much smaller. It is not suited at
// all for columns with continuously changing data.
// <p>
// In general it can be advantageous to use this storage manager when
// a value changes at most every 4 rows (although it depends on the length
// of the data values themselves). The following simple example 
// shows the approximate savings that can be achieved when storing a column
// with double values changing every CH rows.
// <srcblock>
//   #rows    CH     normal length      ISM length      compress ratio
//   50000     5        4000000          1606000               2.5
//   50000    50        4000000           164000              24.5
//   50000   500        4000000            32800             122
// </srcblock>
// There is a special test program <src>nISMBucket</src> in the Tables module
// doing a simple, but usually adequate, simulation of the amount of
// storage needed for a scenario.
// <p>
// IncrementalStMan stores the values (and associated indices) in
// fixed-length buckets. A <linkto class=BucketCache>BucketCache</linkto>
// object is used to read/write
// the buckets. The default cache size is 1 bucket (which is fine for
// sequential access), but for random access it can make sense to
// increase the size of the cache. This can be done using
// the class <linkto class=ROIncrementalStManAccessor>
// ROIncrementalStManAccessor</linkto>.
// <p>
// The IncrementalStMan can hold values of any standard data type (thus
// from Bool to String). It can handle scalars, direct and indirect
// arrays. It can support an arbitrary number of columns. The values in
// each of them can vary at its own speed.
// <br>
// A bucket contains the values of several consecutive rows.
// At the beginning of a bucket the values of the starting row of all
// columns for this storage manager are repeated. In this way the value
// of a cell can always be found in the bucket and no references
// to previous buckets are needed.
// <br>A bucket should be big enough to hold all starting values and
// a reasonable number of other values. As a rule of thumb it should be
// big enough to hold at least 100 values of each column. In general the
// default bucket size will do. Only in special cases (e.g. when storing
// large variable length strings) the bucket size should be set explicitly.
// Giving a zero bucket size means that a suitale default bucket size
// will be calculated.
// <br>
// When a table is filled sequentially each bucket can be filled as
// much as possible. When writing in a random way, buckets can contain
// some unused space, because a bucket in the middle of the file
// has to be split when a new value has to be put in it.
// <p>
// Each column in the IncrementalStMan has the following properties to
// achieve the "store-different-values-only" behaviour.
// <ul>
// <li> When a row is not explicitly put, it has the same value as the
//      previous row.
//      The first row gets the standard undefined values when not put.
//      The order of put's and addRow's is not important.
//      <br>E.g. when a table has N rows and row N and the following M rows
//      have the same value, the following schematic code has the same effect:
//      <br><src>  add 1 row; put value in row N; add M rows;</src>
//      <br><src>  add M+1 rows; put value in row N;</src>
// <li> When putting a scalar or direct array, it is tested if it matches
//      the previous row. If so, it is not stored again.
//      This test is not done for indirect arrays, because those can
//      be (very) big and it would be too time-consuming. So the only
//      way to save space for indirect arrays is by not putting them
//      as explained in the previous item.
// <li> For indirect arrays the buckets contain a pointer only. The
//      arrays themselves are stored in a separate file.
// <li> When a value of an existing row is updated, only that one row is
//      updated. The next row(s) keep their value, even if it was
//      shared with the row being updated. 
//      <br>For scalars and direct arrays it will be tested if the
//      new value matches the value in the previous and/or next row.
//      If so, those rows will be combined to save storage.  
// <li> The IncrementalStMan is optimized for sequential access to a table.
//      <br>- A bucket is accessed only once, because a bucket contains
//            consecutive rows.
//      <br>- For each column a copy is kept of the last value read.
//            So the value for the next rows (with that same value)
//            is immediately available.
//      <br>For random access the performance can be improved by setting
//          the cache size using class
//          <linkto class=ROIncrementalStManAccessor>
//          ROIncrementalStManAccessor</linkto>.
// </ul>
//
// <note>This class contains many public functions which are only used
// by other ISM classes. The only useful function for the user is the
// constructor.
// </note>

// <motivation>
// IncrementalStMan can save a lot of storage space.
// Unlike the old StManMirAIO it stores the values directly in the
// file to save on memory usage.
// </motivation>

// <example>
// This example shows how to create a table and how to attach
// the storage manager to some columns.
// <srcblock>
//   SetupNewTable newtab("name.data", tableDesc, Table::New);
//   IncrementalStMan stman;                  // define storage manager
//   newtab.bindColumn ("column1", stman);    // bind column to st.man.
//   newtab.bindColumn ("column2", stman);    // bind column to st.man.
//   Table tab(newtab);                       // actually create table
// </srcblock>
// </example>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class IncrementalStMan : public ISMBase
{
public:
    // Create an incremental storage manager with the given name.
    // If no name is used, it is set to an empty string.
    // The name can be used to construct a
    // <linkto class=ROIncrementalStManAccessor>ROIncrementalStManAccessor
    // </linkto> object (e.g. to set the cache size).
    // <br>
    // The bucket size has to be given in bytes and the cache size in buckets.
    // Bucket size 0 means that the storage manager will set the bucket
    // size such that it can contain about 100 rows
    // (with a minimum size of 32768 bytes). However, if that results
    // in a very large bucket size (>327680) it'll make it smaller.
    // Note it uses 32 bytes for the size of variable length strings,
    // so this heuristic may fail when a column contains large strings.
    // When <src>checkBucketSize</src> is set and Bucket size > 0
    // the storage manager throws an exception
    // when the size is too small to hold the values of at least 2 rows.
    // For this check it uses 0 for the length of variable length strings.
    // <group>
    explicit IncrementalStMan (uInt bucketSize = 0,
			       Bool checkBucketSize = True,
			       uInt cacheSize = 1);
    explicit IncrementalStMan (const String& dataManagerName,
			       uInt bucketSize = 0,
			       Bool checkBucketSize = True,
			       uInt cacheSize = 1);
    // </group>

    ~IncrementalStMan();

private:
    // Copy constructor cannot be used.
    IncrementalStMan (const IncrementalStMan& that);

    // Assignment cannot be used.
    IncrementalStMan& operator= (const IncrementalStMan& that);
};



} //# NAMESPACE CASACORE - END

#endif
