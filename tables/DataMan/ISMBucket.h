//# ISMBucket.h: A bucket in the Incremental Storage Manager
//# Copyright (C) 1996,1999,2000,2001
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

#ifndef TABLES_ISMBUCKET_H
#define TABLES_ISMBUCKET_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class ISMBase;

// <summary>
// A bucket in the Incremental Storage Manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=IncrementalStMan>IncrementalStMan</linkto>
//   <li> <linkto class=BucketCache>BucketCache</linkto>
// </prerequisite>

// <etymology>
// ISMBucket represents a bucket in the Incremental Storage Manager.
// </etymology>

// <synopsis>
// The Incremental Storage Manager uses a <linkto class=BucketCache>
// BucketCache</linkto> object to read/write/cache the buckets
// containing the data. An <src>ISMBucket</src> object is the
// internal representation of the contents of a bucket. <src>ISMBucket</src>
// contains static callback functions which are called by
// <src>BucketCache</src> when reading/writing a bucket. These callback
// functions do the mapping of bucket data to <src>ISMBucket</src> object
// and vice-versa.
// <p>
// A bucket contains the values of several rows
// of all columns bound to this Incremental Storage Manager.
// A bucket is split into a data part and an index part.
// Each part has an arbitrary length but together they do not exceed
// the fixed bucket length.
// <p>
// The beginning of the data part contains the values of all columns
// bound. The remainder of the data part contains the values of
// the rows/columns with a changed value.
// <br>
// The index part contains an index per column. Each index contains the
// row number and an offset for a row with a stored value. The row numbers
// are relative to the beginning of the bucket, so the bucket has
// no knowledge about the absolute row numbers. In this way deletion of
// rows is much simpler.
// <p>
// The contents of a bucket looks like:
// <srcblock>
//    -------------------------------------------------------------------
//    | index offset   | data part     | index part              | free |
//    -------------------------------------------------------------------
//     0                4               4+length(data part)
//    <--------------------------bucketsize----------------------------->
// </srcblock>
// The data part contains all data value belonging to the bucket.
// The index part contains for each column the following data:
// <srcblock>
//    -----------------------------------------------------------------------
//    | #values stored | row numbers of values | offset in data part of     |
//    | for column i   | stored for column i   | values stored for column i |
//    -----------------------------------------------------------------------
//     0                4                       4+4*nrval
// </srcblock>
// Note that the row numbers in the bucket start at 0, thus are relative
// to the beginning of the bucket. The main index kept in
// <linkto class=ISMIndex>ISMIndex</linkto> knows the starting row of
// each bucket. In this way bucket splitting and especially row removal
// is much easier.
// <p>
// The bucket can be stored in canonical or local (i.e. native) data format.
// When a bucket is read into memory, its data are read, converted, and
// stored in the ISMBucket object. When flushed, the contents are
// written. ISMBucket takes care that the values stored in its object
// do not exceed the size of the bucket. When full, the user can call
// a function to split it into a left and right bucket. When the new
// value has to be written at the end, the split merely consist of
// creating a new bucket. In any case, care is taken that a row is
// not split. Thus a row is always entirely contained in one bucket.
// <p>
// Class <linkto class=ISMColumn>ISMColumn</linkto> does the actual
// writing of data in a bucket and uses the relevant ISMBucket functions.

// <motivation>
// ISMBucket encapsulates the data of a bucket.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class ISMBucket
{
public:

    // Create a bucket with the given parent.
    // When <src>bucketStorage</src> is non-zero, reconstruct the
    // object from it.
    // It keeps the pointer to its parent (but does not own it).
    ISMBucket (ISMBase* parent, const char* bucketStorage);

    ~ISMBucket();

    // Get the row-interval for given column and row.
    // It sets the start and end of the interval to which the row belongs
    // and the offset of its current value.
    // It returns the index where the row number can be put in the
    // bucket index.
    uInt getInterval (uInt colnr, uInt rownr, uInt bucketNrrow,
		      uInt& start, uInt& end, uInt& offset) const;

    // Is the bucket large enough to add a value?
    Bool canAddData (uInt leng) const;

    // Add the data to the data part.
    // It updates the bucket index at the given index.
    // An exception is thrown if the bucket is too small.
    void addData (uInt colnr, uInt rownr, uInt index,
		  const char* data, uInt leng);

    // Is the bucket large enough to replace a value?
    Bool canReplaceData (uInt newLeng, uInt oldLeng) const;

    // Replace a data item.
    // When its length is variable (indicated by fixedLength=0), the old
    // value will be removed and the new one appended at the end.
    // An exception is thrown if the bucket is too small.
    void replaceData (uInt& offset, const char* data, uInt newLeng,
		      uInt fixedLength);

    // Get a pointer to the data for the given offset.
    const char* get (uInt offset) const;

    // Get the length of the data value.
    // It is <src>fixedLength</src> when non-zero,
    // otherwise read it from the data value.
    uInt getLength (uInt fixedLength, const char* data) const;

    // Get access to the offset of the data for given column and row.
    // It allows to change it (used for example by replaceData).
    uInt& getOffset (uInt colnr, uInt rownr);

    // Get access to the index information for the given column.
    // This is used by ISMColumn when putting the data.
    // <group>
    // Return the row numbers with a stored value.
    Block<uInt>& rowIndex (uInt colnr);
    // Return the offsets of the values stored in the data part.
    Block<uInt>& offIndex (uInt colnr);
    // Return the number of values stored.
    uInt& indexUsed (uInt colnr);
    // </group>

    // Split the bucket in the middle.
    // It returns the row number where the bucket was split and the
    // new left and right bucket. The caller is responsible for
    // deleting the newly created buckets.
    // When possible a simple split is done.
    // <br>
    // The starting values in the right bucket may be copies of the
    // values in the left bucket. The duplicated Block contains a switch
    // per column indicating if the value is copied.
    uInt split (ISMBucket*& left, ISMBucket*& right, Block<Bool>& duplicated,
		uInt bucketStartRow, uInt bucketNrrow,
		uInt colnr, uInt rownr, uInt lengToAdd);

    // Determine whether a simple split is possible. If so, do it.
    // This is possible if the new row is at the end of the last bucket,
    // which will often be the case.
    // <br>A simple split means adding a new bucket for the new row.
    // If the old bucket already contains values for that row, those
    // values are moved to the new bucket.
    // <br>This fuction is only called by split, which created the
    // left and right bucket.
    Bool simpleSplit (ISMBucket* left, ISMBucket* right,
		      Block<Bool>& duplicated,
		      uInt& splitRownr, uInt rownr);

    // Return the index where the bucket should be split to get
    // two parts with almost identical length.
    uInt getSplit (uInt totLeng, const Block<uInt>& rowLeng,
		   const Block<uInt>& cumLeng);

    // Remove <src>nr</src> items from data and index part by shifting
    // to the left. The <src>rowIndex</src>, <src>offIndex</src>, and
    // <src>nused</src> get updated. The caller is responsible for
    // removing data when needed (e.g. <src>ISMIndColumn</src> removes
    // the indirect arrays from its file).
    void shiftLeft (uInt index, uInt nr, Block<uInt>& rowIndex,
		    Block<uInt>& offIndex, uInt& nused, uInt leng);

    // Copy the contents of that bucket to this bucket.
    // This is used after a split operation.
    void copy (const ISMBucket& that);

    // Callback function when BucketCache reads a bucket.
    // It creates an ISMBucket object and converts the raw bucketStorage
    // to that object.
    // It returns the pointer to ISMBucket object which gets part of the cache.
    // The object gets deleted by the deleteCallBack function.
    static char* readCallBack (void* owner, const char* bucketStorage);

    // Callback function when BucketCache writes a bucket.
    // It converts the ISMBucket bucket object to the raw bucketStorage.
    static void writeCallBack (void* owner, char* bucketStorage,
			       const char* bucket);

    // Callback function when BucketCache adds a new bucket to the data file.
    // This function creates an empty ISMBucket object.
    // It returns the pointer to ISMBucket object which gets part of the cache.
    // The object gets deleted by the deleteCallBack function.
    static char* initCallBack (void* owner);

    // Callback function when BucketCache removes a bucket from the cache.
    // This function dletes the ISMBucket bucket object.
    static void deleteCallBack (void*, char* bucket);

    // Show the layout of the bucket.
    void show (ostream& os) const;

    // Check that there are no repeated rowIds in the bucket
    Bool check (uInt &offendingCol, uInt &offendingIndex,
                uInt &offendingRow, uInt &offendingPrevRow) const;

private:
    // Forbid copy constructor.
    ISMBucket (const ISMBucket&);

    // Forbid assignment.
    ISMBucket& operator= (const ISMBucket&);

    // Remove a data item with the given length.
    // If the length is zero, its variable length is read first.
    void removeData (uInt offset, uInt leng);

    // Insert a data value by appending it to the end.
    // It returns the offset of the data value.
    uInt insertData (const char* data, uInt leng);

    // Copy a data item from this bucket to the other bucket.
    uInt copyData (ISMBucket& other, uInt colnr, uInt toRownr,
		   uInt fromIndex, uInt toIndex) const;

    // Read the data from the storage into this bucket.
    void read (const char* bucketStorage);

    // Write the bucket into the storage.
    void write (char* bucketStorage) const;


    //# Declare member variables.
    // Pointer to the parent storage manager.
    ISMBase*          stmanPtr_p;
    // The size (in bytes) of an uInt (used in index, etc.).
    uInt              uIntSize_p;
    // The size (in bytes) of the data.
    uInt              dataLeng_p;
    // The size (in bytes) of the index.
    uInt              indexLeng_p;
    // The row index per column; each index contains the row number
    // of each value stored in the bucket (for that column).
    PtrBlock<Block<uInt>*> rowIndex_p;
    // The offset index per column; each index contains the offset (in bytes)
    // of each value stored in the bucket (for that column).
    PtrBlock<Block<uInt>*> offIndex_p;
    // Nr of used elements in each index; i.e. the number of stored values
    // per column.
    Block<uInt>       indexUsed_p;
    // The data space (in external (e.g. canonical) format).
    char*             data_p;
};


inline const char* ISMBucket::get (uInt offset) const
{
    return data_p + offset;
}
inline Block<uInt>& ISMBucket::rowIndex (uInt colnr)
{
    return *(rowIndex_p[colnr]);
}
inline Block<uInt>& ISMBucket::offIndex (uInt colnr)
{
    return *(offIndex_p[colnr]);
}
inline uInt& ISMBucket::indexUsed (uInt colnr)
{
    return indexUsed_p[colnr];
}



} //# NAMESPACE CASACORE - END

#endif
