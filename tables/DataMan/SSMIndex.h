//# SSMIndex.h: The bucket index for a group of columns in the SSM
//# Copyright (C) 2000
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

#ifndef TABLES_SSMINDEX_H
#define TABLES_SSMINDEX_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Vector.h>
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class SSMBase;


// <summary>
// The bucket index for a group of columns in the Standard Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tStandardStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMBase>SSMBase</linkto>
// </prerequisite>

// <etymology>
// SSMIndex represent the bucket index in the Standard Storage Manager.
// </etymology>

// <synopsis>
// In <linkto class=SSMBase>SSMBase</linkto> it is described that an index
// is used to map row number to data bucket in a bucket stream.
// This class implements this index. It serves 2 purposes:
// <ol>
//  <li> It keeps a block of row numbers giving the last row number
//       stored in each data bucket.
//       <br>Note that each bucket does not need to contain the same number
//       of rows, because rows might be deleted from it.
//       When all rows are deleted from a bucket, the bucket is removed
//       from the index and added to the free bucket list.
//  <li> When a column is deleted, the bucket will have a hole.
//       SSMIndex maintains a map to know the size and offset of each hole.
//       Adjacent holes are combined.
//       When a new column is added <linkto class=SSMBase>SSMBase</linkto>
//       will scan the SSMIndex objects to find the hole fitting best.
// </ol>
// </synopsis>
  
// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> recreate should recreate the itsLastRow && itsBucketNr as well
//        (moving them to the front and rearrange the freespace as one
//        concatenated block) 
// </todo>

class SSMIndex 
{
public:
  // Create the object with the given number of rows per bucket.
  // Note that the default is needed to create the object for existing
  // tables.
  explicit SSMIndex (SSMBase* aPtrSSM, uint32_t rowsPerBucket=0);

  ~SSMIndex();

  // Read the bucket index from the AipsIO object.
  void get (AipsIO& anOs);

  // Write the bucket index into the AipsIO object.
  void put (AipsIO& anOs) const;

  // Recreate the object in case all rows are deleted from the table.
  void recreate();
  
  // Return all the bucketnrs used in this index.
  Vector<uint32_t> getBuckets() const;

  // Return the nr of buckets used.
  uint32_t getNrBuckets() const;

  // Set nr of columns use this index.
  void setNrColumns (int32_t aNrColumns, uint32_t aSizeUsed);

  // Add some rows.
  void addRow (rownr_t aNrRows);

  // Show Statistics of index.
  void showStatistics (ostream& anOs) const;

  // A column is removed.
  // Set the free space at offset for a field with the given nr of bits.
  // It returns the nr of columns still used in this index.
  int32_t removeColumn (int32_t anOffset, uint32_t nbits);

  // Try to find free space for a field with a given length (best fit).
  // -1 is returned if no fit is found.
  // Otherwise it returns the nr of bytes left unused.
  int32_t getFree (int32_t& anOffset, uint32_t nbits) const;

  // reuse the space at offset for a field with the given nr of bits.
  // This is used when column has been added to this bucket.
  void addColumn (int32_t anOffset, uint32_t nbits);

  // Delete the given row.
  // It returns the bucket nr if it gets empty, otherwise -1.
  int32_t deleteRow (rownr_t aRowNumber);

  // Get the number of rows that fits in ach bucket.
  uint32_t getRowsPerBucket() const;

  // Find the bucket containing the given row.
  // An exception is thrown if not found.
  // It also sets the first and last row number fitting in that bucket.
  void find (rownr_t aRowNumber, uint32_t& aBucketNr, rownr_t& aStartRow,
	     rownr_t& anEndRow, const String& colName) const;

private:
  // Get the index of the bucket containing the given row.
  uint32_t getIndex (rownr_t aRowNr, const String& colName) const;


  //# Pointer to specific Storage Manager.    
  SSMBase* itsSSMPtr;
    
  //# Nr of entries used in blocks.
  uint32_t itsNUsed;

  //# Last row nr indexed together with itsBucketNumber
  Block<rownr_t> itsLastRow;

  //# Bucketnumbers indexed together with itsLastRow.
  //# So itsLastRow[0] contains the last rownumber of the bucket
  //# in itsBucketNumber[0]
  Block<uint32_t> itsBucketNumber;

  //# Map that contains length/offset pairs for free size (size in bytes).
  std::map<int32_t,int32_t> itsFreeSpace;

  //# How many rows fit in a bucket?
  uint32_t itsRowsPerBucket;

  //# Nr of columns using this index.
  int32_t itsNrColumns;
};


inline uint32_t SSMIndex::getRowsPerBucket() const
{
  return itsRowsPerBucket;
}



} //# NAMESPACE CASACORE - END

#endif
