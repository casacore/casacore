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
//# $Id$   

#if !defined(AIPS_SSMINDEX_H)
#define AIPS_SSMINDEX_H

//# Includes
#include <aips/aips.h>
#include <aips/Containers/Block.h>
#include <aips/Containers/SimOrdMap.h>
#include <aips/Arrays/Vector.h>

//# Forward Declarations
class SSMBase;

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> recreate should recreate the itsLastRow && itsBucketNr as well
//        (moving them to the front and rearrange the freespace as one
//        concattenated block) 
// </todo>

class SSMIndex 
{
public:

  explicit SSMIndex( SSMBase* aPtrSSM, uInt rowsPerBucket=0);
  ~SSMIndex();

  // Read the bucket index from the AipsIO object.
  void get (AipsIO& anOs);

  // Write the bucket index into the AipsIO object.
  void put (AipsIO& anOs);

  void recreate();
  
  // return all the bucketnrs used in this index
  Vector<uInt> getBuckets() const;


  uInt getNrBuckets();

  // set nr of Columns use this index
  void setNrColumns(const Int aNrColumns,const uInt aSizeUsed);

  void addRow( uInt aNrRows );

  // Show Statistics of index.
  void showStatistics (ostream& anOs) const;

  // Set free space space at offset for a field with the given nr of bits.
  // This is used when column has been deleted.
  // return nr of columns still use this index
  Int removeColumn(const Int anOffset, const uInt nbits);

  // Try to find free space for a field with a given length (best fit).
  // -1 is returned if no fit is found.
  // Otherwise it returns the nr of bytes left unused.
  Int getFree(Int& anOffset, const uInt nbits);

  // reuse the space at offset for a field with the given nr of bits.
  // This is used when column has been added to this bucket.
  void addColumn(const Int anOffset, const uInt nbits);

  // Delete the given row.
  Int deleteRow(const uInt aRowNumber);

  // Get the number of rows that fits in ach bucket.
  uInt getRowsPerBucket();

  // Find the bucket containing the given row.
  // An exception is thrown if not found.
  // It also sets the first and last row number fitting in that bucket.
  void find( const uInt aRowNumber, 
	     uInt& aBucketNr, 
	     uInt& aStartRow,
	     uInt& anEndRow);

private:
  // get the Index of a given row
  uInt getIndex (uInt aRowNr) const;


  // Pointer to specific Storage Manager.    
  SSMBase* itsSSMPtr;
    
  // Nr of entries used
  uInt itsNUsed;

  // Last row nr indexed together with itsBucketNumber
  Block <uInt> itsLastRow;


  // Bucketnumbers indexed together with itsLastRow.
  // So itsLastRow[0] contains the lastrownumber of the bucket
  // in itsBucketNumber[0]
  Block <uInt> itsBucketNumber;

  // Map that contains length/offset pairs for free size (size in bytes)
  SimpleOrderedMap<Int,Int> itsFreeSpace;

  // How many rows will fit in a Bucket ?
  uInt itsRowsPerBucket;

  // Nr of Columns that use this index
  Int itsNrColumns;

};


inline uInt SSMIndex::getRowsPerBucket()
{
  return itsRowsPerBucket;
}

#endif
