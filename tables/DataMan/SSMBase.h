//# SSMBase.h: Base class of the Standard Storage Manager
//# Copyright (C) 2000,2001,2002
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

#ifndef TABLES_SSMBASE_H
#define TABLES_SSMBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class BucketCache;
class BucketFile;
class StManArrayFile;
class SSMIndex;
class SSMColumn;
class SSMStringHandler;

// <summary>
// Base class of the Standard Storage Manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tStandardStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=StandardStMan>StandardStMan</linkto>
//   <li> <linkto class=SSMColumn>SSMColumn</linkto>
// </prerequisite>

// <etymology>
// SSMBase is the base class of the Standard Storage Manager.
// </etymology>

// <synopsis>
// The global principles of this class are described in
// <linkto class="StandardStMan:description">StandardStMan</linkto>.
// <p>
// The Standard Storage Manager divides the data file in equally sized
// chunks called buckets. There are 3 types of buckets:
// <ul>
//  <li> Data buckets containing the fixed length data (scalars and
//       direct arrays of data type Int, Float, Bool, etc.).
//       For variable shaped data (strings and indirect arrays) they
//       contain references to the actual data position in the
//       string buckets or in an external file.
//  <li> String buckets containing strings and array of strings.
//  <li> Index buckets containing the index info for the data buckets.
// </ul>
// Bucket access is handled by class
// <linkto class=BucketCache>BucketCache</linkto>.
// It also keeps a list of free buckets. A bucket is freed when it is
// not needed anymore (e.g. all data from it are deleted).
// <p>
// Data buckets form the main part of the SSM. The data can be viewed as
// a few streams of buckets, where each stream contains the data of
// a given number of columns. Each stream has an
// <linkto class=SSMIndex>SSMIndex</linkto> object describing the
// number of rows stored in each data bucket of the stream.
// The SSM starts with a single bucket stream (holding all columns),
// but when columns are added, new bucket streams might be created.
// <p>
// For example, we have an SSM with a bucket size of 100 bytes.
// There are 5 Int columns (A,B,C,D,E) each taking 4 bytes per row.
// Column A, B, C, and D are stored in bucket stream 1, while column
// E is stored in bucket stream 2. So in stream 1 each bucket can hold
// 6 rows, while in stream 2 each bucket can hold 25 rows.
// For a 100 row table it will result in 17+4 data buckets.
// <p>
// A few classes collaborate to make it work:
// <ul>
//  <li> Each bucket stream has an <linkto class=SSMIndex>SSMIndex</linkto>
//       object to map row number to bucket number.
//       Note that in principle each bucket in a stream contains the same
//       number of rows. However, when a row is deleted it is removed
//       from its bucket shifting the remainder to the left. Data in the
//       next buckets is not shifted, so that bucket has now one row less.
//  <li> For each column SSMBase knows to which bucket stream it belongs
//       and at which offset the column starts in a bucket.
//       Note that column data in a bucket are adjacent, which is done
//       to make it easier to use the
//       <linkto class=ColumnCache>ColumnCache</linkto> object in SSMColumn
//       and to be able to efficiently store Bool values as bits.
//  <li> Each column has an <linkto class=SSMColumn>SSMColumn</linkto>
//       object knowing how many bits each data cell takes in a bucket.
//       The SSMColumn objects handle all access to data in the columns
//       (using SSMBase and SSMIndex).
// </ul>
// <p>
// String buckets are used by class
// <linkto class=SSMStringHandler>SSMStringHandler</linkto> to
// store scalar strings and fixed and variable shaped arrays of strings.
// The bucketnr, offset, and length of such string (arrays) are stored
// in the data buckets.
// <br>
// Indirect arrays of other data types are also stored indirectly
// and their offset is stored in the data buckets. Such arrays are
// handled by class <linkto class=StIndArray>StIndArray</linkto>
// which uses an extra file to store the arrays.
// <p>
// Index buckets are used by SSMBase to make the SSMIndex data persistent.
// It uses alternately 2 sets of index buckets. In that way there is
// always an index availanle in case the system crashes.
// If possible 2 halfs of a single bucket are used alternately, otherwise 
// separate buckets are used.
// </synopsis>

// <motivation>
// The public interface of SSMBase is quite large, because the other
// internal SSM classes need these functions. To have a class with a
// minimal interface for the normal user, class <src>StandardStMan</src>
// is derived from it.
// <br>StandardStMan needs an isA- instead of hasA-relation to be
// able to bind columns to it in class <linkto class=SetupNewTable>
// SetupNewTable</linkto>.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Remove AipsIO argument from open and close.
//   <li> When only 1 bucket in use addcolumn can check if there's enough
//        room to fit the new column (so rearange the bucket) in the free
//        row space.
// </todo>


class SSMBase: public DataManager
{
public:
  // Create a Standard storage manager with default name SSM.
  explicit SSMBase (Int aBucketSize=0,
		    uInt aCacheSize=1);
  
  // Create a Standard storage manager with the given name.
  explicit SSMBase (const String& aDataManName,
		    Int aBucketSize=0,
		    uInt aCacheSize=1);
  
  // Create a Standard storage manager with the given name.
  // The specifications are part of the record (as created by dataManagerSpec).
  SSMBase (const String& aDataManName,
	   const Record& spec);
  
  ~SSMBase();
  
  // Clone this object.
  // It does not clone SSMColumn objects possibly used.
  // The caller has to delete the newly created object.
  virtual DataManager* clone() const;
  
  // Get the type name of the data manager (i.e. StandardStMan).
  virtual String dataManagerType() const;
  
  // Get the name given to the storage manager (in the constructor).
  virtual String dataManagerName() const;
  
  // Record a record containing data manager specifications.
  virtual Record dataManagerSpec() const;

  // Get data manager properties that can be modified.
  // It is only ActualCacheSize (the actual cache size in buckets).
  // It is a subset of the data manager specification.
  virtual Record getProperties() const;

  // Modify data manager properties.
  // Only ActualCacheSize can be used. It is similar to function setCacheSize
  // with <src>canExceedNrBuckets=False</src>.
  virtual void setProperties (const Record& spec);

  // Get the version of the class.
  uInt getVersion() const;
  
  // Set the cache size (in buckets).
  // If <src>canExceedNrBuckets=True</src>, the given cache size can be
  // larger than the nr of buckets in the file. In this way the cache can
  // be made large enough for a future file extension.
  // Otherwise, it is limited to the actual number of buckets. This is useful
  // if one wants the entire file to be cached.
  void setCacheSize (uInt aCacheSize, Bool canExceedNrBuckets=True);

  // Get the current cache size (in buckets).
  uInt getCacheSize() const;
  
  // Clear the cache used by this storage manager.
  // It will flush the cache as needed and remove all buckets from it.
  void clearCache();

  // Show the statistics of all caches used.
  virtual void showCacheStatistics (ostream& anOs) const;

  // Show statistics of all indices used.
  void showIndexStatistics (ostream & anOs) const;

  // Show statistics of the Base offsets/index etc.
  void showBaseStatistics (ostream & anOs) const;

  // Get the bucket size.
  uInt getBucketSize() const;
  
  // Get the number of rows in this storage manager.
  uInt getNRow() const;
  
  // The storage manager can add rows.
  virtual Bool canAddRow() const;
  
  // The storage manager can delete rows.
  virtual Bool canRemoveRow() const;
  
  // The storage manager can add columns.
  virtual Bool canAddColumn() const;
  
  // The storage manager can delete columns.
  virtual Bool canRemoveColumn() const;
  
  // Make the object from the type name string.
  // This function gets registered in the DataManager "constructor" map.
  // The caller has to delete the object.
  static DataManager* makeObject (const String& aDataManType,
				  const Record& spec);
  
  // Get access to the given column.
  SSMColumn& getColumn (uInt aColNr);
  
  // Get access to the given Index.
  SSMIndex& getIndex (uInt anIdxNr);
  
  // Make the current bucket in the cache dirty (i.e. something has been
  // changed in it and it needs to be written when removed from the cache).
  // (used by SSMColumn::putValue).
  void setBucketDirty();
  
  // Open (if needed) the file for indirect arrays with the given mode.
  // Return a pointer to the object.
  StManArrayFile* openArrayFile (ByteIO::OpenOption anOpt);

  // Find the bucket containing the column and row and return the pointer
  // to the beginning of the column data in that bucket.
  // It also fills in the start and end row for the column data.
  char* find (uInt aRowNr,     uInt aColNr, 
	      uInt& aStartRow, uInt& anEndRow);

  // Add a new bucket and get its bucket number.
  uInt getNewBucket();

  // Read the bucket (if needed) and return the pointer to it.
  char* getBucket (uInt aBucketNr);

  // Remove a bucket from the bucket cache.
  void removeBucket (uInt aBucketNr);

  // Get rows per bucket for the given column.
  uInt getRowsPerBucket (uInt aColumn) const;

  // Return a pointer to the (one and only) StringHandler object.
  SSMStringHandler* getStringHandler();

  // <group>
  // Callbacks for BucketCache access.
  static char* readCallBack (void* anOwner, const char* aBucketStorage);
  static void writeCallBack (void* anOwner, char* aBucketStorage,
                             const char* aBucket);
  static void deleteCallBack (void*, char* aBucket);
  static char* initCallBack (void* anOwner);
  // </group>

private:
  // Copy constructor (only meant for clone function).
  SSMBase (const SSMBase& that);
  
  // Assignment cannot be used.
  SSMBase& operator= (const SSMBase& that);
  
  // (Re)create the index, file, and cache object.
  // It is used when all rows are deleted from the table.
  void recreate();
  
  // The data manager supports use of MultiFile.
  virtual Bool hasMultiFileSupport() const;

  // Flush and optionally fsync the data.
  // It returns a True status if it had to flush (i.e. if data have changed).
  virtual Bool flush (AipsIO&, Bool doFsync);
  
  // Let the storage manager create files as needed for a new table.
  // This allows a column with an indirect array to create its file.
  virtual void create (uInt aNrRows);
  
  // Open the storage manager file for an existing table, read in
  // the data, and let the SSMColumn objects read their data.
  virtual void open (uInt aRowNr, AipsIO&);
  
  // Resync the storage manager with the new file contents.
  // This is done by clearing the cache.
  virtual void resync (uInt aRowNr);
  
  // Reopen the storage manager files for read/write.
  virtual void reopenRW();
  
  // The data manager will be deleted (because all its columns are
  // requested to be deleted).
  // So clean up the things needed (e.g. delete files).
  virtual void deleteManager();

  // Let the storage manager initialize itself (upon creation).
  // It determines the bucket size and fills the index.
  void init();

  // Determine and set the bucket size.
  // It returns the number of rows per bucket.
  uInt setBucketSize();
  
  // Get the number of indices in use.
  uInt getNrIndices() const;
  
  // Add rows to the storage manager.
  // Per column it extends number of rows.
  virtual void addRow (uInt aNrRows);
  
  // Delete a row from all columns.
  virtual void removeRow (uInt aRowNr);
  
  // Do the final addition of a column.
  virtual void addColumn (DataManagerColumn*);
  
  // Remove a column from the data file.
  virtual void removeColumn (DataManagerColumn*);
  
  // Create a column in the storage manager on behalf of a table column.
  // The caller has to delete the newly created object.
  // <group>
  // Create a scalar column.
  virtual DataManagerColumn* makeScalarColumn (const String& aName,
					       int aDataType,
					       const String& aDataTypeID);
  // Create a direct array column.
  virtual DataManagerColumn* makeDirArrColumn (const String& aName,
					       int aDataType,
					       const String& aDataTypeID);
  // Create an indirect array column.
  virtual DataManagerColumn* makeIndArrColumn (const String& aName,
					       int aDataType,
					       const String& aDataTypeID);
  // </group>
  
  // Get the cache object.
  // This will construct the cache object if not present yet.
  // The cache object will be deleted by the destructor.
  BucketCache& getCache();
  
  // Construct the cache object (if not constructed yet).
  void makeCache();
  
  // Read the header.
  void readHeader();
  
  // Read the index from its buckets.
  void readIndexBuckets();

  // Write the header and the indices.
  void writeIndex();


  //# Declare member variables.
  // Name of data manager.
  String       itsDataManName;
  
  // The file containing the indirect arrays.
  StManArrayFile* itsIosFile;
  
  // The number of rows in the columns.
  uInt         itsNrRows;
  
  // Column offset
  Block<uInt> itsColumnOffset;

  // Row Index ID containing all the columns in a bucket
  Block<uInt> itsColIndexMap;

  // Will contain all indices
  PtrBlock<SSMIndex*>  itsPtrIndex;
  
  // The cache with the SSM buckets.
  BucketCache* itsCache;
  
  // The file containing all data.
  BucketFile*  itsFile;
  
  // String handler class
  SSMStringHandler* itsStringHandler;

  // The persistent cache size.
  uInt itsPersCacheSize;
  
  // The actual cache size.
  uInt itsCacheSize;
  
  // The initial number of buckets in the cache.
  uInt itsNrBuckets;

  // Nr of buckets needed for index.
  uInt itsNrIdxBuckets;

  // Number of the first index bucket
  Int itsFirstIdxBucket;

  // Offset of index in first bucket.
  // If >0, the index fits in a single bucket.
  uInt itsIdxBucketOffset;

  // Number of the first String Bucket
  Int itsLastStringBucket;

  // length of index memoryblock
  uInt itsIndexLength;

  // The nr of free buckets.
  uInt itsFreeBucketsNr;
  
  // The first free bucket.
  Int itsFirstFreeBucket;
  
  // The bucket size.
  uInt itsBucketSize;
  uInt itsBucketRows;
  
  // The assembly of all columns.
  PtrBlock<SSMColumn*> itsPtrColumn;
  
  // Has the data changed since the last flush?
  Bool isDataChanged;
};


inline uInt SSMBase::getNrIndices() const
{
  return itsPtrIndex.nelements();
}

inline uInt SSMBase::getCacheSize() const
{
  return itsCacheSize;
}

inline uInt SSMBase::getNRow() const
{
  return itsNrRows;
}

inline uInt SSMBase::getBucketSize() const
{
  return itsBucketSize;
}

inline BucketCache& SSMBase::getCache()
{
  if (itsCache == 0) {
    makeCache();
  }
  return *itsCache;
}

inline SSMColumn& SSMBase::getColumn (uInt aColNr)
{
  return *(itsPtrColumn[aColNr]);
}

inline SSMIndex& SSMBase::getIndex (uInt anIdxNr)
{
  return *(itsPtrIndex[anIdxNr]);
}

inline SSMStringHandler* SSMBase::getStringHandler()
{
  return itsStringHandler;
}



} //# NAMESPACE CASACORE - END

#endif
