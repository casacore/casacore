//# SSMBase.cc: Base class of the Standard Storage Manager
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
//# Inc., 675 Massachusettes Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <aips/Tables/SSMBase.h>
#include <aips/Tables/SSMColumn.h>
#include <aips/Tables/SSMDirColumn.h>
#include <aips/Tables/SSMIndColumn.h>
#include <aips/Tables/SSMIndStringColumn.h>
#include <aips/Tables/SSMIndex.h>
#include <aips/Tables/SSMStringHandler.h>
#include <aips/Tables/BucketCache.h>
#include <aips/Tables/BucketFile.h>
#include <aips/Tables/StArrayFile.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/ValType.h>
#include <aips/Utilities/Assert.h>
#include <aips/IO/AipsIO.h>
#include <aips/IO/RawIO.h>
#include <aips/IO/MemoryIO.h>
#include <aips/IO/CanonicalIO.h>
#include <aips/OS/CanonicalConversion.h>
#include <aips/IO/FiledesIO.h>
#include <aips/Tables/DataManError.h>
#include <iostream.h>


SSMBase::SSMBase (uInt aBucketSize,uInt aCacheSize)
  : DataManager          (),
    itsVersion           (1),
    itsIosFile           (0),
    itsNrRows            (0),
    itsCache             (0),
    itsFile              (0),
    itsStringHandler     (0),
    itsPersCacheSize     (max(aCacheSize,2u)),
    itsCacheSize         (0),
    itsNrBuckets         (0), 
    itsNrIdxBuckets      (0),
    itsFirstIdxBucket    (-1),
    itsLastStringBucket  (-1),
    itsIndexLength       (0),
    itsFreeBucketsNr     (0),
    itsFirstFreeBucket   (-1),
    itsBucketSize        (aBucketSize),
    isDataChanged        (False)
{
  // Determine the data format (local or canonical).
  // For the moment it is always canonical (until Table supports it).
  isCanonical = True;
}

SSMBase::SSMBase (const String& aDataManName,
		  uInt aBucketSize,
                  uInt aCacheSize)
  : DataManager          (),
    itsDataManName       (aDataManName),
    itsVersion           (1),
    itsIosFile           (0),
    itsNrRows            (0),
    itsCache             (0),
    itsFile              (0),
    itsStringHandler     (0),
    itsPersCacheSize     (max(aCacheSize,2u)),
    itsCacheSize         (0),
    itsNrBuckets         (0),
    itsNrIdxBuckets      (0),
    itsFirstIdxBucket    (-1),
    itsLastStringBucket  (-1),
    itsIndexLength       (0),
    itsFreeBucketsNr     (0),
    itsFirstFreeBucket   (-1),
    itsBucketSize        (aBucketSize),
    isDataChanged        (False)
{
  // Determine the data format (local or canonical).
  // For the moment it is always canonical (until Table supports it).
  isCanonical = True;
}

SSMBase::SSMBase (const SSMBase& that)
  : DataManager          (),
    itsDataManName       (that.itsDataManName),
    itsVersion           (that.itsVersion),
    itsIosFile           (0),
    itsNrRows            (that.itsNrRows),
    itsCache             (0),
    itsFile              (0),
    itsStringHandler     (0),
    itsPersCacheSize     (that.itsPersCacheSize),
    itsCacheSize         (that.itsCacheSize),
    itsNrBuckets         (0),
    itsNrIdxBuckets      (that.itsNrIdxBuckets),
    itsFirstIdxBucket    (that.itsFirstIdxBucket),
    itsLastStringBucket  (that.itsLastStringBucket),
    itsIndexLength       (that.itsIndexLength),
    itsFreeBucketsNr     (0),
    itsFirstFreeBucket   (-1),
    itsBucketSize        (that.itsBucketSize),
    isDataChanged        (False)
{
  // Determine the data format (local or canonical).
  // For the moment it is always canonical (until Table supports it).
  isCanonical = True;
}

SSMBase::~SSMBase()
{
  for (uInt i=0; i<ncolumn(); i++) {
    delete itsPtrColumn[i];
  }
  for (uInt i=0; i<itsPtrIndex.nelements(); i++) {
    delete itsPtrIndex[i];
  }
  delete itsCache;
  delete itsFile;
  delete itsIosFile;
  delete itsStringHandler;
}

DataManager* SSMBase::clone() const
{
  return new SSMBase (*this);
}

String SSMBase::dataManagerType() const
{
  return "StandardStMan";
}

String SSMBase::dataManagerName() const
{
  return itsDataManName;
}

void SSMBase::clearCache()
{
  if (itsCache != 0) {
    itsCache->clear();
  }
}

void SSMBase::showBaseStatistics (ostream& anOs) const
{
  anOs << "StandardStMan Base statistics:" << endl;
  anOs << "Nr of columns               : " << ncolumn() << endl ; 
  anOs << "Nr of rows in the columns   : " << itsNrRows << endl;
  for (uInt i=0;i<ncolumn();i++) {
    anOs << " ColIndex["<<i<<"]           : " << itsColIndexMap[i];
    anOs << " ColOffset["<<i<<"]          : " << itsColumnOffset[i] << endl;
  }
  anOs << "CacheSize                   : " << itsCacheSize << endl ; 
  anOs << "Size of buckets             : " << itsBucketSize << endl ; 
  anOs << "Total buckets               : " << itsNrBuckets << endl ; 
  anOs << "Total Index buckets         : " << itsNrIdxBuckets << endl ; 
  anOs << "1st Index bucket            : " << itsFirstIdxBucket << endl ; 
  anOs << "last String bucket used     : " << itsLastStringBucket << endl ; 
  anOs << "Total free buckets          : " << itsFreeBucketsNr << endl ; 
  anOs << "1st free bucket             : " << itsFirstFreeBucket << endl ; 
  anOs << endl;

}

void SSMBase::showCacheStatistics (ostream& anOs) const
{
  if (itsCache != 0) {
    anOs << "StandardStMan cache statistics:" << endl;
    itsCache->showStatistics (anOs);
    anOs << endl;
  }
}

void SSMBase::showIndexStatistics (ostream & anOs) const
{
  uInt aNrIdx=itsPtrIndex.nelements();
  for (uInt i=0; i < aNrIdx; i++) {
    anOs << "StandardStMan index: " << i << " statistics:" << endl;
    itsPtrIndex[i]->showStatistics (anOs);
    anOs << endl;
  }
}

DataManagerColumn* SSMBase::makeScalarColumn (const String&,
					      int aDataType,
					      const String&)
{

  //# Extend itsPtrColumn block if needed.
  if (ncolumn() >= itsPtrColumn.nelements()) {
    itsPtrColumn.resize (itsPtrColumn.nelements() + 32);
  }
  SSMColumn* aColumn = new SSMColumn (this, aDataType, ncolumn());
  itsPtrColumn[ncolumn()] = aColumn;
  return aColumn;
}

DataManagerColumn* SSMBase::makeDirArrColumn (const String& aName,
					      int aDataType,
					      const String& aDataTypeId)
{
  //# Extend itsPtrColumn block if needed.
  if (ncolumn() >= itsPtrColumn.nelements()) {
    itsPtrColumn.resize (itsPtrColumn.nelements() + 32);
  }
  SSMColumn* aColumn = new SSMDirColumn (this, aDataType, ncolumn());
  itsPtrColumn[ncolumn()] = aColumn;
  return aColumn;
}

DataManagerColumn* SSMBase::makeIndArrColumn (const String&,
					      int aDataType,
					      const String&)
{
  //# Extend itsPtrColumn block if needed.
  if (ncolumn() >= itsPtrColumn.nelements()) {
    itsPtrColumn.resize (itsPtrColumn.nelements() + 32);
  }
  SSMColumn* aColumn;
  if (aDataType == TpString) {
    aColumn = new SSMIndStringColumn (this, aDataType, ncolumn());
  } else {
    aColumn = new SSMIndColumn (this, aDataType, ncolumn());
  }
  itsPtrColumn[ncolumn()] = aColumn;
  return aColumn;
}

DataManager* SSMBase::makeObject (const String&)
{
  // This function is called when reading a table back.
  // Construct it with the default bucket size and cache size.
  return new SSMBase ();
}

void SSMBase::setCacheSize (uInt aCacheSize)
{
  itsCacheSize = max(aCacheSize,2u);
  if (itsCache != 0) {
    itsCache->resize (itsCacheSize);
  }
}

void SSMBase::makeCache()
{
  if (itsCache == 0) {
    Bool forceFill= False;
    
    if (itsPtrIndex.nelements() == 0) {
      itsFile->open();
      readIndex();
      forceFill=True;
    }
    
    // Set cache size to persistent cache size if not set explicitly yet.
    if (itsCacheSize == 0) {
      itsCacheSize = itsPersCacheSize;
    }
    itsCache = new BucketCache (itsFile, 512, itsBucketSize,
				itsNrBuckets, itsCacheSize,
				this,
				SSMBase::readCallBack, 
				SSMBase::writeCallBack,
				SSMBase::initCallBack,
				SSMBase::deleteCallBack);
    itsCache->resync (itsNrBuckets, itsFreeBucketsNr, 
		      itsFirstFreeBucket);

    if (forceFill) {
      fillIndexBuckets();
    }
  }
}

uInt SSMBase::getRowsPerBucket(uInt aColumn) const
{
  return itsPtrIndex[itsColIndexMap[aColumn]]->getRowsPerBucket();
}

uInt SSMBase::getNewBucket()
{
  char* aBucketPtr = new char[itsBucketSize];
  memset (aBucketPtr,0,itsBucketSize);
  // Get a new bucket number from bucketcache
  return getCache().addBucket(aBucketPtr);
}

void SSMBase::readIndex()
{
  itsFile->seek(0);
  
  // Use the file indicated by the fd from the BucketFile object
  FiledesIO aFio (itsFile->fd());
  TypeIO*   aTio;
  
  // It is stored in canonical or local format.
  if (asCanonical()) {
    aTio = new CanonicalIO (&aFio);
  } else {
    aTio = new RawIO (&aFio);
  }
  AipsIO anOs (aTio);
  itsVersion = anOs.getstart("StandardStMan");
  anOs >> itsBucketSize;                // Size of the bucket
  anOs >> itsNrBuckets;                 // Initial Nr of Buckets
  anOs >> itsPersCacheSize;             // Size of Persistent cache
  anOs >> itsFreeBucketsNr;             // Nr of Free Buckets
  anOs >> itsFirstFreeBucket;           // First Free Bucket nr
  anOs >> itsNrIdxBuckets;              // Nr of Buckets needed 4 Index
  anOs >> itsFirstIdxBucket;            // First indexBucket Number
  anOs >> itsLastStringBucket;          // Last StringBucket in use
  anOs >> itsIndexLength;               // length of index
  uInt nrinx;
  anOs >> nrinx;                        // Nr of indices

  if (itsStringHandler == 0) {
    itsStringHandler = new SSMStringHandler(this);
    itsStringHandler->init();
  }
  itsStringHandler->setLastStringBucket(itsLastStringBucket);

  anOs.getend();
  anOs.close();
  delete aTio;

  for (uInt i=0; i<itsPtrIndex.nelements(); i++) {
    delete itsPtrIndex[i];
  }
  itsPtrIndex.resize (nrinx, True, False);
  itsPtrIndex = 0;
}

void SSMBase::fillIndexBuckets()
{
  TypeIO*   aMio;
  MemoryIO  aMemBuf(itsIndexLength);

  uInt aCLength = 2*CanonicalConversion::canonicalSize(&itsFirstIdxBucket);

  getCache();
  // It is stored in canonical or local format.
  if (asCanonical()) {
    aMio = new CanonicalIO (&aMemBuf);
  } else {
    aMio = new RawIO (&aMemBuf);
  }
  
  AipsIO anMOs (aMio);

  Int aBucket= itsFirstIdxBucket;
  uInt idxBucketSize = itsBucketSize-aCLength; 
  Int aNr=itsIndexLength;
  char* aBucketPtr;
  for (uint j=0; j< itsNrIdxBuckets; j++) {
    aBucketPtr = getBucket(aBucket);

    // 1st aCLength/2 bytes should be identical to next aCLength/2 bytes
    // if not it might be an indicator that something went wrong
    // This can be used in the future

    Int aCheckNr;
    CanonicalConversion::toLocal(aCheckNr,aBucketPtr);
    CanonicalConversion::toLocal(aBucket,aBucketPtr+aCLength/2);

    if (aCheckNr != aBucket) {
      // Not used for now
    }

    if (aNr < static_cast<Int>(idxBucketSize)) {
      aMemBuf.write(aNr,static_cast<void*>(aBucketPtr+aCLength));
    } else {
      aMemBuf.write(idxBucketSize,static_cast<void*>(aBucketPtr+aCLength));
    }
    aNr-=idxBucketSize;
  }
  
  aMemBuf.seek(0);

  uInt aNrIdx=itsPtrIndex.nelements();
  for (uInt i=0; i < aNrIdx; i++) {
    itsPtrIndex[i] = new SSMIndex(this);
    itsPtrIndex[i]->get(anMOs);
  }
  
  anMOs.close();
  delete aMio;
}

void SSMBase::writeIndex()
{
  TypeIO*   aTio;
  TypeIO*   aMio;
  MemoryIO  aMemBuf;

  // Use the file indicated by the fd from the BucketFile object
  FiledesIO aFio (itsFile->fd());
  uInt aCLength = 2*CanonicalConversion::canonicalSize(&itsFirstIdxBucket);

  // store it in canonical or local format.
  if (asCanonical()) {
    aMio = new CanonicalIO (&aMemBuf);
    aTio = new CanonicalIO (&aFio);
  } else {
    aMio = new RawIO (&aMemBuf);
    aTio = new RawIO (&aFio);
  }
  
  AipsIO anMOs (aMio);

  uInt aNrIdx = itsPtrIndex.nelements();
  for (uInt i=0;i<aNrIdx; i++ ){
    itsPtrIndex[i]->put(anMOs);
  }
  
  anMOs.close();

  // Write total Mio in NEW indexBuckets
  
  //leave space for next bucket nr
  uInt idxBucketSize = itsBucketSize-aCLength; 
  uInt aNrBuckets = aMemBuf.length()/idxBucketSize;
  uInt aRestSize  = aMemBuf.length()%idxBucketSize;

  if (aRestSize != 0) {
    aNrBuckets++;
  }

  Int anOldBucket=-1;
  uInt aNewBucket=0;
  const uChar* aMemPtr = aMemBuf.getBuffer();
  char* aBucketPtr;

  for (uInt i=aNrBuckets;i>0;i--) {
    aNewBucket = getNewBucket();
    aBucketPtr = getBucket(aNewBucket);

    CanonicalConversion::fromLocal(aBucketPtr,anOldBucket);
    CanonicalConversion::fromLocal(aBucketPtr+aCLength/2,anOldBucket);

    // write rest of Index as far as it fits
    if (i==aNrBuckets && aRestSize !=0) {
      memcpy(aBucketPtr+aCLength,aMemPtr+((i-1)*idxBucketSize),aRestSize);
    } else {
      memcpy(aBucketPtr+aCLength,aMemPtr+((i-1)*idxBucketSize),idxBucketSize);
    }
    setBucketDirty();
    anOldBucket=aNewBucket;
  }
 
  uInt aMioLength = aMemBuf.length();
  itsNrIdxBuckets=aNrBuckets;
  delete aMio;

  // New Index is written, give old indexbuckets free, and save firstBucketNr

  Int aBucket= itsFirstIdxBucket;
  while (aBucket != -1) {
    aBucketPtr=getBucket(aBucket);
    CanonicalConversion::toLocal(aBucket,aBucketPtr+aCLength/2);
    itsCache->removeBucket();
  }
  
  itsFirstIdxBucket=aNewBucket;
  AlwaysAssert ( itsStringHandler != 0, AipsError);
  itsLastStringBucket = itsStringHandler->lastStringBucket();

  itsStringHandler->flush();
  itsCache->flush();

  aNrBuckets = getCache().nBucket();

  itsFile->seek (0);
  AipsIO anOs (aTio);
  
  // write a few items at the beginning of the file  AipsIO anOs (aTio);

  anOs.putstart("StandardStMan",itsVersion);
  anOs << itsBucketSize;                // Size of the bucket
  anOs << aNrBuckets;                   // Present number of buckets
  anOs << itsPersCacheSize;             // Size of Persistent cache
  anOs << getCache().nFreeBucket();     // Nr of Free Buckets
  anOs << getCache().firstFreeBucket(); // First Free Bucket nr
  anOs << itsNrIdxBuckets;              // Nr buckets needed for index
  anOs << itsFirstIdxBucket;            // First Index bucket number
  anOs << itsLastStringBucket;          // Last String bucket in use
  anOs << aMioLength;                   // length of index
  anOs << itsPtrIndex.nelements();      // Nr of indices
  
  anOs.putend();  
  anOs.close();
  delete aTio;
}

void SSMBase::setBucketDirty()
{
  itsCache->setDirty();
  isDataChanged = True;
}

//# The storage manager can add rows.
Bool SSMBase::canAddRow() const
{
  return True;
}
//# The storage manager can delete rows.
Bool SSMBase::canRemoveRow() const
{
  return True;
}
//# The storage manager cannot add columns (not yet).
Bool SSMBase::canAddColumn() const
{
  return True;
}
//# The storage manager cannot delete columns (not yet).
Bool SSMBase::canRemoveColumn() const
{
  return True;
}


void SSMBase::addRow (uInt aNrRows)
{
  //make sure cache is available and filled (I need itsPtrIndex)
  getCache();

  uInt aNrIdx = itsPtrIndex.nelements();

  for (uInt i=0; i< aNrIdx; i++) {
    itsPtrIndex[i]->addRow(aNrRows);
  }

  uInt aNrCol = ncolumn();
  for (uInt j=0; j< aNrCol; j++) {
    itsPtrColumn[j]->addRow(itsNrRows+aNrRows,itsNrRows,False);
  }

  itsNrRows+=aNrRows;
  isDataChanged = True;
}

void SSMBase::removeRow (uInt aRowNr)
{
  uInt aNrCol = ncolumn();
  for (uInt j=0; j< aNrCol; j++) {
    itsPtrColumn[j]->deleteRow(aRowNr);
  }
  
  uInt aNrIdx=itsPtrIndex.nelements();
  for (uInt i=0; i< aNrIdx; i++) {
    Int anEmptyBucket=itsPtrIndex[i]->deleteRow(aRowNr);
    // remove bucket if empty;
    if (anEmptyBucket >= 0) {
      removeBucket(anEmptyBucket);
    }
  }
  itsNrRows--;
  if (itsNrRows == 0) {
    for (uInt i=0; i<itsPtrIndex.nelements(); i++) {
      delete itsPtrIndex[i];
    }
    Int aBucket= itsFirstIdxBucket;
    uInt aCLength = 2*CanonicalConversion::canonicalSize(&itsFirstIdxBucket);
    while (aBucket != -1) {
      char* aBucketPtr=getBucket(aBucket);
      CanonicalConversion::toLocal(aBucket,aBucketPtr+aCLength/2);
      itsCache->removeBucket();
    }
    itsFirstIdxBucket=-1;
    itsNrIdxBuckets=0;
    create(itsNrRows);
    //    recreate();
  }
  isDataChanged = True;
}

void SSMBase::addColumn (DataManagerColumn* aColumn)
{

  // Be sure cache is filled
  getCache();

  SSMColumn* aSSMC = dynamic_cast<SSMColumn*> (aColumn);
  AlwaysAssert ( aSSMC != 0, AipsError);

  aSSMC->doCreate(0);

  uInt aSearchLength = aSSMC->getExternalSizeBits();

  Int  anOffset=-1;
  Int  aBestFit=-1;
  uInt saveIndex=0;
  Int  saveOffset=-1; 

  // Try if there is  freespace available where this column can fit (best fit)
  // For now we assume that a best fit will be :
  //                                             1) exact fit
  //                                             2) any fit

  for (uInt i=0; i<itsPtrIndex.nelements() && 
	aBestFit != static_cast<Int>(aSearchLength) ; i++) {
   
    Int aFoundFit =itsPtrIndex[i]->getFree(anOffset,aSearchLength);
    if (aFoundFit == 0) {
      // Perfect Fit Found
      aBestFit = aSearchLength;
      saveIndex=i;
      saveOffset=anOffset;
    } else if (aFoundFit > 0) {
      if (aFoundFit < aBestFit || aBestFit == -1) {
	aBestFit = aFoundFit;
	saveIndex=i;
	saveOffset=anOffset;
      }
    }    
  }

  // If fit found use this space, else make new column
  uInt nCol = aSSMC->getColNr();
  itsColumnOffset.resize(ncolumn(),True);                            
  itsColIndexMap.resize(ncolumn(),True);
  if (aBestFit != -1) {
    itsPtrIndex[saveIndex]->addColumn(saveOffset,aSearchLength);
    itsColIndexMap[nCol]=saveIndex;
    itsColumnOffset[nCol]=saveOffset;
  } else {

    // calculate rowsperbuket for new index
    AlwaysAssert (aSearchLength !=0, AipsError);
    uInt rowsPerBucket = itsBucketSize*8 / aSearchLength;

    if (rowsPerBucket < 1) {
      // The BucketSize is too small to contain data.
      throw (DataManError ("StandardStMan::addColumn  bucketsize too small"));
    }

    uInt nrIdx=itsPtrIndex.nelements();
    itsPtrIndex.resize(nrIdx+1,True);
    
    itsPtrIndex[nrIdx] = new SSMIndex(this,rowsPerBucket);
    uInt aSize =(rowsPerBucket*aSSMC->getExternalSizeBits() + 7) / 8;
    itsPtrIndex[nrIdx]->setNrColumns(1,aSize);
    itsPtrIndex[nrIdx]->addRow(itsNrRows);

    itsColIndexMap[nCol]=nrIdx;
    itsColumnOffset[nCol]=0;                            
  

  }

  aSSMC->addRow(itsNrRows,0,aBestFit != -1);
  isDataChanged = True;
}

void SSMBase::removeBucket(const uInt aBucketNr)
{
  getCache().getBucket(aBucketNr);
  getCache().removeBucket();
}  

char*  SSMBase::getBucket(const uInt aBucketNr)
{
  return itsCache->getBucket(aBucketNr);
}
  

void SSMBase::removeColumn (DataManagerColumn* aColumn)
{
  getCache();

  SSMColumn* aSSMC = dynamic_cast<SSMColumn*> (aColumn);
  AlwaysAssert ( aSSMC != 0, AipsError);
  
  uInt aNrCol = ncolumn();
  uInt aColNr = aSSMC->getColNr();
  Bool isFound=False;
  
  for (uInt i=0; i<aNrCol && !isFound ; i++) {
    if (itsPtrColumn[i]->getColNr() == aColNr) {
      isFound=True;

      itsPtrColumn[i]->removeColumn();

      // free up space
      Int aNrColumns = itsPtrIndex[itsColIndexMap[i]]->removeColumn
                  (itsColumnOffset[i], itsPtrColumn[i]->getExternalSizeBits());

      // if no columns left,buckets can be released
      if (aNrColumns == 0) {
	Vector<uInt> aBucketList=itsPtrIndex[itsColIndexMap[i]]->getBuckets();
	for (uInt k=0; k<aBucketList.nelements(); k++) {
	  removeBucket(aBucketList(k));
	}
	delete itsPtrIndex[itsColIndexMap[i]];
	itsPtrIndex.remove(itsColIndexMap[i],True);
	// because there's one ptrindex less, the colindexmap ptr's with
	// a value > then i should be 1 less.
        for (uInt k=0; k<aNrCol; k++) {
	  if (itsColIndexMap[k] > itsColIndexMap[i]) {
	    itsColIndexMap[k] = itsColIndexMap[k]-1;
	  }
	}
      }
      delete itsPtrColumn[i];
      for (uInt j=i;j<aNrCol-1;j++) {
        // move columns right of removed on to the left in the PtrList
	itsPtrColumn[j] = itsPtrColumn[j+1];
        // decrement the columnnumber by 1
	itsPtrColumn[j]->setColNr(itsPtrColumn[j]->getColNr()-1);
        // move the offsets of the remaining columns to the left also
	itsColumnOffset[j] = itsColumnOffset[j+1];	
	itsColIndexMap[j] = itsColIndexMap[j+1];
	itsPtrColumn[j] = itsPtrColumn[j+1];
      }
      decrementNcolumn();
      isDataChanged = True;
    }
  }
}


char* SSMBase::readCallBack (void* anOwner, const char* aBucketStorage)
{
  uInt aSize = static_cast<SSMBase*>(anOwner)->getBucketSize();
  char* aBucket = new char [aSize];
  memcpy (aBucket, aBucketStorage, aSize);
  return aBucket;
}

void SSMBase::writeCallBack (void* anOwner, char* aBucketStorage,
                             const char* aBucket)
{
  uInt aSize = static_cast<SSMBase*>(anOwner)->getBucketSize();
  memcpy (aBucketStorage, aBucket, aSize);
}

void SSMBase::deleteCallBack (void*, char* aBucket)
{
  delete [] aBucket;
}

char* SSMBase::initCallBack (void* anOwner)
{
  uInt aSize = static_cast<SSMBase*>(anOwner)->getBucketSize();
  char* aBucket = new char [aSize];
  memset (aBucket,0,aSize);
  return aBucket;
}

char* SSMBase::find(uInt aRowNr,     uInt aColNr, 
		    uInt& aStartRow, uInt& anEndRow)
{
 
  // Make sure that cache is available and filled.
  getCache();
  SSMIndex* anIndexPtr = itsPtrIndex[itsColIndexMap[aColNr]];
  uInt aBucketNr;
  anIndexPtr->find(aRowNr,aBucketNr,aStartRow,anEndRow);
  char* aPtr = getBucket(aBucketNr);
  return aPtr + itsColumnOffset[aColNr];
}



void SSMBase::recreate()
{
  delete itsCache;
  itsCache = 0;
  delete itsFile;
  itsFile = 0;
  delete itsIosFile;
  itsIosFile = 0;
  delete itsStringHandler;
  itsStringHandler = 0;
  itsNrBuckets = 0;
  itsFirstIdxBucket = -1;
  itsFreeBucketsNr = 0;
  itsFirstFreeBucket   = -1;
  itsFile = new BucketFile (fileName());
  makeCache();
  // Let the Index recreate itself when needed
  uInt aNrIdx=itsPtrIndex.nelements();
  for (uInt i=0; i<aNrIdx; i++) {
    itsPtrIndex[i]->recreate();
  }
  
  itsStringHandler = new SSMStringHandler(this);
  itsStringHandler->init();

  // Let the column objects create something (if needed)
  uInt aNrCol = ncolumn();
  for (uInt i=0; i<aNrCol; i++) {
    itsPtrColumn[i]->doCreate(itsNrRows);
  }
}

Bool SSMBase::flush (AipsIO& ios, Bool doFsync)
{
  //# Check if anything has changed.
  Bool changed = False;

  if (itsStringHandler) {
    itsStringHandler->flush();
  }

  if (itsCache != 0) {
    itsCache->flush();
  }

  if (isDataChanged) {
    writeIndex();
    if (doFsync) {
      itsFile->fsync();
    }
    changed = True;
    isDataChanged = False;
  }

  if (itsIosFile != 0) {
    itsIosFile->flush(doFsync);
  }
  
  ios.putstart ("SSM", itsVersion);
  ios << itsDataManName;
  putBlock (ios, itsColumnOffset, itsColumnOffset.nelements());
  putBlock (ios, itsColIndexMap,  itsColIndexMap.nelements());
  ios.putend();
  return changed;
}

void SSMBase::resync (uInt aNrRows)
{
  itsNrRows = aNrRows;
  if (itsPtrIndex.nelements() != 0) {
    readIndex();
  }
  if (itsCache != 0) {
    itsCache->resync (itsNrBuckets, itsFreeBucketsNr, 
		      itsFirstFreeBucket);
  }
  if (itsPtrIndex.nelements() != 0) {
    fillIndexBuckets();
  }  
  if (itsStringHandler != 0) {
    itsStringHandler->resync();
  }

  uInt aNrCol = ncolumn();
  for (uInt i=0; i<aNrCol; i++) {
    itsPtrColumn[i]->resync (itsNrRows);
  }
}

void SSMBase::create (uInt aNrRows)
{
  init(True);
  recreate();
  itsNrRows = 0;
  addRow (aNrRows);
}

void SSMBase::open (uInt aRowNr, AipsIO& ios)
{
  itsNrRows = aRowNr;
  itsVersion = ios.getstart ("SSM");
  ios >> itsDataManName;
  getBlock (ios,itsColumnOffset);
  getBlock (ios,itsColIndexMap);
  ios.getend();
  init(False);
  
  itsFile = new BucketFile (fileName(), table().isWritable());
  AlwaysAssert (itsFile != 0, AipsError);

  // Let the column object initialize themselves (if needed)
  uInt aNrCol = ncolumn();
  for (uInt i=0; i<aNrCol; i++) {
    itsPtrColumn[i]->getFile(itsNrRows);
  }
  
}

StManArrayFile* SSMBase::openArrayFile (ByteIO::OpenOption anOpt)
{
  if (itsIosFile == 0) {
    itsIosFile = new StManArrayFile (fileName() + 'i', anOpt,
				     0, asCanonical());
  }
  return itsIosFile;
}

void SSMBase::reopenRW()
{
  if (itsFile != 0) {
    itsFile->setRW();
  }
  if (itsIosFile != 0) {
    itsIosFile->reopenRW();
  }
}

void SSMBase::init (Bool doMakeIndex)
{
  // Determine the size of a uInt in external format.
  if (isCanonical) {
    itsUIntSize = ValType::getCanonicalSize (TpUInt);
  }else{
    itsUIntSize = ValType::getTypeSize (TpUInt);
  }
  if (doMakeIndex) {
    uInt aNrCol = ncolumn();
    itsColumnOffset.resize(aNrCol,True);                            
    itsColIndexMap.resize(aNrCol,True);                            
    itsColIndexMap = 0;
    // Finding the nr of rows fitting in the bucket is a bit hard, because
    // Bool values are stored as bits. Therefore we have to iterate.
    // First find the nr of full bytes needed (ignoring possible remainders).
    uInt aTotalSize = 0;
    for (uInt i=0; i<aNrCol; i++) {
      aTotalSize += itsPtrColumn[i]->getExternalSizeBytes();
    }
    uInt rowsPerBucket = itsBucketSize/aTotalSize;
    while (True) {
      uInt bucketRest = itsBucketSize - rowsPerBucket*aTotalSize;
      uInt aRestSize = 0;
      for (uInt i=0; i<aNrCol; i++) {
	uInt rest = itsPtrColumn[i]->getExternalSizeBits() % 8;
	// Add total nr of bytes needed for the remainder.
	aRestSize += (rowsPerBucket * rest + 7) / 8;
      }
      if (aRestSize <= bucketRest) {
	break;
      }
      rowsPerBucket--;
    }

    if (itsBucketSize < 16 || rowsPerBucket < 1) {
      // The BucketSize is too small to contain data.
      throw (DataManError ("StandardStMan::init  bucketsize too small"));
    }
    // Determine the offset of each column.
    // Note that the data of a column are consecutive per bucket.
    aTotalSize = 0;
    for (uInt i=0; i<aNrCol; i++) {
      itsColumnOffset[i] = aTotalSize;
      aTotalSize +=
                (rowsPerBucket*itsPtrColumn[i]->getExternalSizeBits() + 7) / 8;
    }

    // All columns are in the same bucket list, thus only one SSMIndex needed.
    itsPtrIndex.resize(1,True);
    itsPtrIndex[0] = new SSMIndex(this, rowsPerBucket);
    itsPtrIndex[0]->setNrColumns(aNrCol,aTotalSize);
  }
}
