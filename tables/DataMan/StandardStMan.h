//# StandardStMan.h: The Standard Storage Manager
//# Copyright (C) 2000,2002
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

#ifndef TABLES_STANDARDSTMAN_H
#define TABLES_STANDARDSTMAN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/SSMBase.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// The Standard Storage Manager
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tStandardStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> The Table Data Managers concept as described in module file
//        <linkto module="Tables:Data Managers">Tables.h</linkto>
//   <li> <linkto class=ROStandardStManAccessor>
//        ROStandardStManAccessor</linkto>
//        for a discussion of the cache size
// </prerequisite>

// <etymology>
// StandardStMan is the data manager which stores the data in a
// standard way. I.e. it does not use special techniques like
// other storage managers do.
// </etymology>

// <synopsis>
// StandardStMan is meant as the storage manager to be used standardly.
// Other storage managers like
// <linkto class=IncrementalStMan>IncrementalStMan</linkto> and the
// <linkto class=TiledStMan>TiledStMan</linkto> derivatives should
// only be used when appropriate.
// <br>
// Like the other storage managers StandardStMan uses
// <linkto class=BucketCache>bucket-based</linkto> access to its data.
// where a bucket contains the number of columns and rows that fit best.
// Variable length strings are stored in separate buckets because they do
// not fit in the fixed bucket layout used for the other columns.
// Only fixed length strings and strings <= 8 characters are stored directly.
// Note that, in fact, fixed length string means maximum length strings.
// It can be set using the <src>setMaxLength</src> function in
// class <linkto class=ColumnDesc>ColumnDesc</linkto> or
// class <linkto class=BaseColumnDesc>BaseColumnDesc</linkto>.
// <p>
// The file size is at least the size of a bucket, even if only the table
// contains only a few rows, thus uses only a fraction of a bucket.
// The default bucketsize is 32 rows. This means that if it is known
// in advance that the table will contain many more rows, it might make
// sense to construct the StandardStMan with a larger bucketsize.
// <p>
// StandardStMan is a robust storage manager. Care has been taken
// that its index cannot be corrupted in case of exceptions like
// device full or crash.
// <p>
// StandardStMan supports the following functionality:
// <ol>
//  <li> Removal of rows. This leaves some empty space in a bucket.
//       An empty bucket will be reused.
//  <li> Addition of rows. This is always done in the last bucket
//       and a new bucket is added when needed.
//  <li> Removal of a column. This also leaves empty space, which will
//       be reused when a newly added column fits in it.
//  <li> Addition of a column. If available, empty column space is used.
//       Otherwise it creates as many new buckets as needed.
// </ol>
// All direct data (scalars and direct arrays) is stored in the main file.
// Indirect arrays (except strings) are stored in a second file.
// Indirect string arrays are also stored in the main file, because in
// that way frequently rewriting indirect strings arrays wastes far
// less space.
// <p>
// As said above all string arrays and variable length scalar strings
// are stored in separate string buckets. 
// </synopsis>

// <motivation>
// StManAipsIO is the standard storage manager used so far.
// Its major drawback is that it is memory based which makes it
// not usable for large tables. Furthermore it is not a very robust
// storage manager. When a system crashes, tables might get corrupted.
// <br>
// These drawbacks have been adressed in this new StandardStman.
// It uses a bucket-based access scheme and makes sure that its
// indices are stored in a way that they can hardly get corrupted.
// </motivation>

// <example>
// The following example shows how to create a table and how to attach
// the storage manager to some columns.
// <srcblock>
//   SetupNewTable newtab("name.data", tableDesc, Table::New);
//   StandardStMan stman;                     // define storage manager
//   newtab.bindColumn ("column1", stman);    // bind column to st.man.
//   newtab.bindColumn ("column2", stman);    // bind column to st.man.
//   Table tab(newtab);                       // actually create table
// </srcblock>
//
// The following example shows how to create a StandardStMan storage
// manager for a table with 16 rows. By giving the (expected) nr of rows
// to the storage manager, it can optimize its bucket size.
// <srcblock>
//   SetupNewTable newtab("name.data", tableDesc, Table::New);
//   StandardStMan stman(-16);
//   newtab.bindAll ("column1", stman);       // bind all columns to st.man.
//   Table tab(newtab);                       // actually create table
// </srcblock>
// </example>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class StandardStMan : public SSMBase
{
public:
    // Create a Standard storage manager with the given name.
    // If no name is used, it is set to "SSM"
    // The name can be used to construct a
    // <linkto class=ROStandardStManAccessor>ROStandardStManAccessor
    // </linkto> object (e.g. to set the cache size).
    // <br>
    // The cache size has to be given in buckets.
    // <br>
    // The bucket size can be given in 2 ways:
    // <br>- A positive number gives the bucket size in bytes.
    // The number of rows per bucket will be calculated from it.
    // <br>- A negative number gives the number of rows per bucket.
    // The bucket size in bytes will be calculated from it.
    // Note that in this way the maximum bucketsize is 32768 (minimum is 128).
    // <br>- The default 0 means that 32 rows will be stored in a bucket.
    // <br>Note that the default is only suitable for small tables.
    // In general it makes sense to give the expected number of table rows.
    // In that way the buckets will be small enough for small tables
    // and not too small for large tables.
    // <group>
    explicit StandardStMan (Int bucketSize = 0,
			    uInt cacheSize = 1);
    explicit StandardStMan (const String& dataManagerName,
			    Int bucketSize = 0,
			    uInt cacheSize = 1);
    // </group>

    ~StandardStMan();

private:
    // Copy constructor cannot be used.
    StandardStMan (const StandardStMan& that);

    // Assignment cannot be used.
    StandardStMan& operator= (const StandardStMan& that);
};



} //# NAMESPACE CASACORE - END

#endif
