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
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

SSMIndex::SSMIndex (SSMBase* aSSMPtr, uInt rowsPerBucket) 
: itsSSMPtr           (aSSMPtr),
  itsNUsed            (0),
  itsRowsPerBucket    (rowsPerBucket),
  itsNrColumns        (0)
{  
}

SSMIndex::~SSMIndex()
{
}


void SSMIndex::get (AipsIO& anOs)
{
  uInt version = anOs.getstart("SSMIndex");
  anOs >> itsNUsed;
  anOs >> itsRowsPerBucket;
  anOs >> itsNrColumns;
  anOs >> itsFreeSpace;
  if (version == 1) {
    Block<uInt> tmp;
    getBlock (anOs, tmp);
    itsLastRow.resize (tmp.size());
    for (size_t i=0; i<tmp.size(); ++i) {
      itsLastRow[i] = tmp[i];
    }
  } else {
    getBlock (anOs, itsLastRow);
  }
  getBlock (anOs, itsBucketNumber);
  anOs.getend();
}

void SSMIndex::put (AipsIO& anOs) const
{
  // Try to be forward compatible by trying to write the row numbers as uInt.
  uInt version = 1;
  if (itsNUsed > 0  &&  itsLastRow[itsNUsed-1] > DataManager::MAXROWNR32) {
    version = 2;
  }
  anOs.putstart("SSMIndex", version);
  anOs << itsNUsed;
  anOs << itsRowsPerBucket;
  anOs << itsNrColumns;
  anOs << itsFreeSpace;
  if (version == 1) {
    Block<uInt> tmp(itsNUsed);
    for (uInt i=0; i<itsNUsed; ++i) {
      tmp[i] = itsLastRow[i];
    }
    putBlock (anOs, tmp);
  } else {
    putBlock (anOs, itsLastRow, itsNUsed);
  }
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
    for (uInt i=0; i<itsNUsed; i++) {
      anOs << "BucketNr["<<i<<"]  : " << itsBucketNumber[i]
	   << " - LastRow["<<i<<"]   : " << itsLastRow[i] << endl;
    }
    anOs << "Freespace entries: " << itsFreeSpace.size() << endl;
    Int i=0;
    for (const auto& x : itsFreeSpace) {
      anOs << "Offset["<<i<<"]: " << x.first <<
	"  -  nrBytes["<<i<<"]: " << x.second << endl;
      i++;
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
    itsFreeSpace.insert (std::make_pair(aSizeUsed, nfree));
  }
}

void SSMIndex::addRow (rownr_t aNrRows)
{
  rownr_t lastRow=0;
  if (aNrRows == 0 ) {
    return;
  }

  if (itsNUsed > 0 ) {
    lastRow = itsLastRow[itsNUsed-1]+1;
    rownr_t usedLast = lastRow;
    if (itsNUsed > 1) {
      usedLast -= itsLastRow[itsNUsed-2]+1;
    }
    uInt64 fitLast = itsRowsPerBucket-usedLast;
    uInt64 toAdd = std::min(fitLast, aNrRows);
    
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
    uInt toAdd = std::min (aNrRows, rownr_t(itsRowsPerBucket));
    lastRow += toAdd;
    aNrRows -= toAdd;
    itsLastRow[itsNUsed] = lastRow-1;
    itsNUsed += 1;
  }
}

Int SSMIndex::deleteRow (rownr_t aRowNr)
{
  // Decrement the rowNrs of all the intervals after the row to be removed
  uInt anIndex = getIndex(aRowNr, String());
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


uInt SSMIndex::getIndex (rownr_t aRowNumber, const String& colName) const
{
  Bool isFound;
  uInt anIndex = binarySearchBrackets( isFound, itsLastRow, aRowNumber, 
				       itsNUsed );
  if (anIndex >= itsNUsed) {
    throw DataManError ("SSMIndex::getIndex - access to non-existing row "
                        + String::toString(aRowNumber) +
                        " in column " + colName + " of table " + 
                        itsSSMPtr->table().tableName());
  }
  return anIndex;
}

Int SSMIndex::removeColumn (Int anOffset, uInt nbits)
{
  // set freespace (total in bytes).
  uInt aLength = (itsRowsPerBucket * nbits + 7) / 8;
  itsFreeSpace.insert (std::make_pair(anOffset,aLength));
 
  itsNrColumns--;
  AlwaysAssert (itsNrColumns > -1, AipsError);
  
  // See if space can be combined
  // That is possible if two or more entries are adjacent.
  std::map<Int,Int>::iterator next = itsFreeSpace.begin();
  std::map<Int,Int>::iterator curr = next;
  next++;
  while (next != itsFreeSpace.end()) {
    Int aK = curr->first;
    Int aV = curr->second;
    if (aK+aV == next->first) {
      // Adjacent; combine current with next by adding its free space.
      curr->second += next->second;
      // Remove next such that the iterator is still valid.
      // Thus first increment, then erase.
      std::map<Int,Int>::iterator nextsav = next;
      ++next;
      itsFreeSpace.erase (nextsav);
    } else {
      curr = next;
      ++next;
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
  for (const auto& x : itsFreeSpace) {
    Int aV = x.second;
    if (aV == aLength) {
      anOffset = x.first;
      //Perfect fit found, don't need to continue
      return(0);
    }
    if (aV >= aLength  &&  (aV < bestLength  ||  bestLength == -1)) {
      bestLength = aV;
      anOffset = x.first;
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
  Int aV = itsFreeSpace.at(anOffset);
  itsNrColumns++;
  itsFreeSpace.erase(anOffset);
  if (aLength != aV) {
    DebugAssert (aLength < aV, AipsError);
    // more space then needed, remap extra space
    Int anO = anOffset + aLength;
    aV -= aLength;
    itsFreeSpace.insert (std::make_pair(anO, aV));
  }
}

void SSMIndex::find (rownr_t aRowNumber, uInt& aBucketNr, 
		     rownr_t& aStartRow, rownr_t& anEndRow,
                     const String& colName) const
{
  uInt anIndex = getIndex(aRowNumber, colName);
  aBucketNr = itsBucketNumber[anIndex];
  anEndRow = itsLastRow[anIndex];
  aStartRow = 0;
  if (anIndex > 0) {
    aStartRow = itsLastRow[anIndex-1]+1;
  }
}

void SSMIndex::checkSeqIndex() const
{
  rownr_t rownr = 0;
  for (uInt bnr=0; bnr<itsNUsed; ++bnr) {
    rownr += itsRowsPerBucket;
    if (itsBucketNumber[bnr] != bnr) {
      throw DataManError("StandardStMan Failover mode needs sequential bucket numbers - "
                         "expected " + String::toString(bnr) +
                         " but found " + String::toString(itsBucketNumber[bnr]));
    }
    // The last bucket may contain less rows, but others must be full.
    if (bnr < itsNUsed-1) {
      if (itsLastRow[bnr]+1 != rownr) {
        throw DataManError("StandardStMan Failover mode needs full buckets - "
                           "expected " + String::toString(itsRowsPerBucket) +
                           " but found " + String::toString(itsLastRow[bnr] + 1 - rownr));
      }
    }
  }
}

void SSMIndex::generate (rownr_t nrows, uInt ncolumns)
{
  itsNUsed = (nrows +itsRowsPerBucket - 1) / itsRowsPerBucket;
  itsLastRow.resize (itsNUsed);
  rownr_t rownr = 0;
  itsBucketNumber.resize (itsNUsed);
  for (uInt i=0; i<itsNUsed; ++i) {
    rownr += itsRowsPerBucket;
    itsLastRow[i] = std::min(nrows,rownr) - 1;
    itsBucketNumber[i] = i;
  }
  itsNrColumns = ncolumns;
}

} //# NAMESPACE CASACORE - END

