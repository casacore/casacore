//# SSMIndex.cc: The bucket index for a group of columns in the SSM
//# Copyright (C) 2000,2001,2002,2005
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

#include <casacore/tables/DataMan/SSMIndex.h>
#include <casacore/tables/DataMan/SSMBase.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Containers/SimOrdMapIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

SSMIndex::SSMIndex (SSMBase* aSSMPtr, uInt rowsPerBucket) 
: itsSSMPtr           (aSSMPtr),
  itsNUsed            (0),
  itsFreeSpace        (0),
  itsRowsPerBucket    (rowsPerBucket),
  itsNrColumns        (0)
{  
}

SSMIndex::~SSMIndex()
{
}


void SSMIndex::get (AipsIO& anOs)
{
  anOs.getstart("SSMIndex");
  anOs >> itsNUsed;
  anOs >> itsRowsPerBucket;
  anOs >> itsNrColumns;
  anOs >> itsFreeSpace;
  getBlock (anOs, itsLastRow);
  getBlock (anOs, itsBucketNumber);
  anOs.getend();
}

void SSMIndex::put (AipsIO& anOs) const
{
  anOs.putstart("SSMIndex", 1);
  anOs << itsNUsed;
  anOs << itsRowsPerBucket;
  anOs << itsNrColumns;
  anOs << itsFreeSpace;
  putBlock (anOs, itsLastRow, itsNUsed);
  putBlock (anOs, itsBucketNumber, itsNUsed);
  anOs.putend();
}

void SSMIndex::showStatistics (ostream& anOs) const
{
  anOs << "Index statistics: " << endl;
  anOs << "Entries used       : " << itsNUsed << endl;
  anOs << "Rows Per bucket    : " << itsRowsPerBucket << endl;
  anOs << "Nr of Columns      : " << itsNrColumns << endl;   
  if (itsNrColumns > 0 ) {
    for (uInt i=0;i < itsNUsed; i++) {
      anOs << "BucketNr["<<i<<"]  : " << itsBucketNumber[i]
	   << " - LastRow["<<i<<"]   : " << itsLastRow[i] << endl;
    }

    anOs << "Freespace entries: " << itsFreeSpace.ndefined() << endl;
    for (uInt i=0;i < itsFreeSpace.ndefined(); i++) {
      anOs << "Offset["<<i<<"]: " << itsFreeSpace.getKey(i) <<
	"  -  nrBytes["<<i<<"]: " << itsFreeSpace.getVal(i) << endl;
    }
  }
  anOs << endl;
}

void SSMIndex::setNrColumns (Int aNrColumns, uInt aSizeUsed)
{
  itsNrColumns = aNrColumns;
  // Determine if there is some free space left at the end of the bucket.
  Int nfree = itsSSMPtr->getBucketSize()-aSizeUsed;
  if (nfree > 0) {
    itsFreeSpace.define (aSizeUsed, nfree);
  }
}

void SSMIndex::addRow (uInt aNrRows)
{
  uInt lastRow=0;
  if (aNrRows == 0 ) {
    return;
  }

  if (itsNUsed > 0 ) {
    lastRow = itsLastRow[itsNUsed-1]+1;
    uInt usedLast = lastRow;
    if (itsNUsed > 1) {
      usedLast -= itsLastRow[itsNUsed-2]+1;
    }
    uInt fitLast = itsRowsPerBucket-usedLast;
    uInt toAdd = min(fitLast, aNrRows);
    
    itsLastRow[itsNUsed-1] += toAdd;
    aNrRows -= toAdd;
    lastRow += toAdd;
  }
 
  if (aNrRows == 0) {
    return;
  }
   
  // add extra row. If nrrows > itsLastRow.nelements reserve extra rows
  // which is cheaper.
  // Take in account that NrRows can be bigger then itsRowsPerBucket !
  
  uInt aNr = (aNrRows+itsRowsPerBucket-1) / itsRowsPerBucket;
  
  uInt aNewNr = itsNUsed+aNr;
  uInt aOldNr = itsLastRow.nelements();
  if (aNewNr > aOldNr) { 
    uInt newSize = aOldNr*2;
    if (aNewNr < newSize) {
      aNewNr = newSize;
    }
    itsLastRow.resize (aNewNr);
    itsBucketNumber.resize(aNewNr);
  }
  
  // first time bucket is made and filled, last bucket was filled, so if
  // still rowsLeft, there must be a new entry/bucket
  
  while (aNrRows > 0) {
    itsBucketNumber[itsNUsed] =itsSSMPtr->getNewBucket();
    uInt toAdd = min (aNrRows, itsRowsPerBucket);
    lastRow += toAdd;
    aNrRows -= toAdd;
    itsLastRow[itsNUsed] = lastRow-1;
    itsNUsed += 1;
  }
}

Int SSMIndex::deleteRow (uInt aRowNr)
{
  // Decrement the rowNrs of all the intervals after the row to be removed
  uInt anIndex = getIndex(aRowNr);
  Bool isEmpty=False;

  for (uInt i = anIndex ; i< itsNUsed; i++) {
    if (itsLastRow[i] > 0) {
      itsLastRow[i]--;
    } else {
      isEmpty = True;
    }
  }

  // If this bucket is empty, add to free list and remove from itsLastRow
  // and itsBucketNumber.

  Int anEmptyBucket = -1;

  Int last = -1;
  if (anIndex > 0) {
    last = itsLastRow[anIndex-1];
  }
  if (static_cast<Int>(itsLastRow[anIndex]) == last || isEmpty) {
    anEmptyBucket = itsBucketNumber[anIndex];
    if (anIndex+1 < itsNUsed) {
      objmove (&itsLastRow[anIndex],
	       &itsLastRow[anIndex+1],
	       itsNUsed-anIndex-1);
      objmove (&itsBucketNumber[anIndex],
	       &itsBucketNumber[anIndex+1],
	       itsNUsed-anIndex-1);
    }
    itsNUsed--;
    itsLastRow[itsNUsed]=0;
    itsBucketNumber[itsNUsed]=0;
  }
  return anEmptyBucket;
}


void SSMIndex::recreate()
{
  itsNUsed=0;
}


uInt SSMIndex::getIndex (uInt aRowNumber) const
{
  Bool isFound;
  uInt anIndex = binarySearchBrackets( isFound, itsLastRow, aRowNumber, 
				       itsNUsed );
  if (anIndex >= itsNUsed) {
    throw TableError ("SSMIndex::getIndex - access to non-existing row "
		      + String::toString(aRowNumber) + " in " +
		      itsSSMPtr->table().tableName());
  }
  return anIndex;
}

Int SSMIndex::removeColumn (Int anOffset, uInt nbits)
{
  // set freespace (total in bytes).
  uInt aLength = (itsRowsPerBucket * nbits + 7) / 8;
  itsFreeSpace.define(anOffset,aLength);
 
  itsNrColumns--;
  AlwaysAssert (itsNrColumns > -1, AipsError);
  
  uInt i=0;
  while (i < itsFreeSpace.ndefined()-1) {
    // See if space can be combined
    // That is possible if this and the next entry are adjacent.
    Int aK = itsFreeSpace.getKey(i);
    Int aV = itsFreeSpace.getVal(i);
    if (aK+aV == itsFreeSpace.getKey(i+1) ) {
      aV += itsFreeSpace.getVal(i+1);
      itsFreeSpace.remove(itsFreeSpace.getKey(i+1));
      itsFreeSpace.define(aK,aV);
    } else {
      i++;
    }
  }
  return (itsNrColumns);
}

Vector<uInt> SSMIndex::getBuckets() const
{
  Vector<uInt> aBucketList(itsNUsed);
  for (uInt i=0; i< itsNUsed; i++) {
    aBucketList(i) = itsBucketNumber[i];
  }
  return aBucketList;
}

Int SSMIndex::getFree (Int& anOffset, uInt nbits) const
{
  Int aLength = (itsRowsPerBucket * nbits + 7) / 8;
  // returnvalue: -1  :  No fit
  //               0  :  Best fit at anOffset
  //            other :  Nr of bytes left after fit

  Int bestLength = -1;
  
  // try to find if there's a place to (best) fit data with a given length
  for (uInt i=0; i < itsFreeSpace.ndefined(); i++) {
    
    Int aV = itsFreeSpace.getVal(i);
    if (aV == aLength) {
      anOffset = itsFreeSpace.getKey(i);
      //Perfect fit found, don't need to continue
      return(0);
    }
    if (aV >= aLength  &&  (aV < bestLength  ||  bestLength == -1)) {
      bestLength = aV;
      anOffset = itsFreeSpace.getKey(i);
    }
  }
  if (bestLength == -1) {
    return (bestLength);
  } else {
    return (bestLength-aLength);
  }
}

void SSMIndex::addColumn (Int anOffset, uInt nbits)
{
  Int aLength = (itsRowsPerBucket * nbits + 7) / 8;
  Int aV = itsFreeSpace(anOffset);
  itsNrColumns++;
  itsFreeSpace.remove(anOffset);
  if (aLength != aV) {
    DebugAssert (aLength < aV, AipsError);
    // more space then needed, remap extra space
    Int anO = anOffset + aLength;
    aV -= aLength;
    itsFreeSpace.define(anO, aV);
  }
}

void SSMIndex::find (uInt aRowNumber, uInt& aBucketNr, 
		     uInt& aStartRow, uInt& anEndRow) const
{
  uInt anIndex = getIndex(aRowNumber);
  aBucketNr = itsBucketNumber[anIndex];
  anEndRow = itsLastRow[anIndex];
  aStartRow = 0;
  if (anIndex > 0) {
    aStartRow = itsLastRow[anIndex-1]+1;
  }
}

} //# NAMESPACE CASACORE - END

