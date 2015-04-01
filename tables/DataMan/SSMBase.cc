//# SSMBase.cc: Base class of the Standard Storage Manager
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

#include <casacore/tables/DataMan/SSMBase.h>
#include <casacore/tables/DataMan/SSMColumn.h>
#include <casacore/tables/DataMan/SSMDirColumn.h>
#include <casacore/tables/DataMan/SSMIndColumn.h>
#include <casacore/tables/DataMan/SSMIndStringColumn.h>
#include <casacore/tables/DataMan/SSMIndex.h>
#include <casacore/tables/DataMan/SSMStringHandler.h>
#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/IO/BucketCache.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/LECanonicalIO.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/DOos.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

SSMBase::SSMBase (Int aBucketSize, uInt aCacheSize)
: DataManager          (),
  itsDataManName       ("SSM"),
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
  itsIdxBucketOffset   (0),
  itsLastStringBucket  (-1),
  itsIndexLength       (0),
  itsFreeBucketsNr     (0),
  itsFirstFreeBucket   (-1),
  itsBucketSize        (0),
  itsBucketRows        (0),
  isDataChanged        (False)
{ 
  if (aBucketSize < 0) {
    itsBucketRows = -aBucketSize;
  } else if (aBucketSize == 0) {
    itsBucketRows = 32;
  } else {
    itsBucketSize = aBucketSize;
  }
}

SSMBase::SSMBase (const String& aDataManName,
		  Int aBucketSize, uInt aCacheSize)
: DataManager          (),
  itsDataManName       (aDataManName),
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
  itsIdxBucketOffset   (0),
  itsLastStringBucket  (-1),
  itsIndexLength       (0),
  itsFreeBucketsNr     (0),
  itsFirstFreeBucket   (-1),
  itsBucketSize        (0),
  itsBucketRows        (0),
  isDataChanged        (False)
{ 
  if (aBucketSize < 0) {
    itsBucketRows = -aBucketSize;
  } else if (aBucketSize == 0) {
    itsBucketRows = 32;
  } else {
    itsBucketSize = aBucketSize;
  }
}

SSMBase::SSMBase (const String& aDataManName,
		  const Record& spec)
: DataManager          (),
  itsDataManName       (aDataManName),
  itsIosFile           (0),
  itsNrRows            (0),
  itsCache             (0),
  itsFile              (0),
  itsStringHandler     (0),
  itsPersCacheSize     (2),
  itsCacheSize         (0),
  itsNrBuckets         (0), 
  itsNrIdxBuckets      (0),
  itsFirstIdxBucket    (-1),
  itsIdxBucketOffset   (0),
  itsLastStringBucket  (-1),
  itsIndexLength       (0),
  itsFreeBucketsNr     (0),
  itsFirstFreeBucket   (-1),
  itsBucketSize        (0),
  itsBucketRows        (0),
  isDataChanged        (False)
{ 
  // Get bucketrows if defined.
  if (spec.isDefined ("BUCKETROWS")) {
    itsBucketRows = spec.asInt ("BUCKETROWS");
  }
  // If no bucketrows, get bucketsize if defined.
  // Otherwise set bucketrows to default value.
  if (itsBucketRows == 0) {
    if (spec.isDefined ("BUCKETSIZE")) {
      itsBucketSize = spec.asInt ("BUCKETSIZE");
    }
    if (itsBucketSize == 0) {
      itsBucketRows = 32;
    }
  }
  if (spec.isDefined ("PERSCACHESIZE")) {
    itsPersCacheSize = max(2, spec.asInt ("PERSCACHESIZE"));
  }
}

SSMBase::SSMBase (const SSMBase& that)
: DataManager          (),
  itsDataManName       (that.itsDataManName),
  itsIosFile           (0),
  itsNrRows            (0),
  itsCache             (0),
  itsFile              (0),
  itsStringHandler     (0),
  itsPersCacheSize     (that.itsPersCacheSize),
  itsCacheSize         (0),
  itsNrBuckets         (0),
  itsNrIdxBuckets      (0),
  itsFirstIdxBucket    (-1),
  itsIdxBucketOffset   (0),
  itsLastStringBucket  (-1),
  itsIndexLength       (0),
  itsFreeBucketsNr     (0),
  itsFirstFreeBucket   (-1),
  itsBucketSize        (that.itsBucketSize),
  itsBucketRows        (that.itsBucketRows),
  isDataChanged        (False)
{}

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

Record SSMBase::dataManagerSpec() const
{
  Record rec = getProperties();
  rec.define ("BUCKETSIZE", Int(itsBucketSize));
  rec.define ("PERSCACHESIZE", Int(itsPersCacheSize));
  rec.define ("IndexLength", Int(itsIndexLength));
  return rec;
}

Record SSMBase::getProperties() const
{
  // Make sure the cache is initialized, so the header has certainly been read.
  const_cast<SSMBase*>(this)->getCache();
  Record rec;
  rec.define ("ActualCacheSize", Int(itsCacheSize));
  return rec;
}

void SSMBase::setProperties (const Record& rec)
{
  if (rec.isDefined("ActualCacheSize")) {
    setCacheSize (rec.asInt("ActualCacheSize"), False);
  }
}

void SSMBase::clearCache()
{
  if (itsCache != 0) {
    itsStringHandler->flush();
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
  anOs << "Index bucket offset         : " << itsIdxBucketOffset << endl ; 
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

DataManagerColumn* SSMBase::makeDirArrColumn (const String&,
					      int aDataType,
					      const String&)
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

DataManager* SSMBase::makeObject (const String& group, const Record& spec)
{
  // This function is called when reading a table back.
  // Construct it with the default bucket size and cache size.
  return new SSMBase (group, spec);
}

void SSMBase::setCacheSize (uInt aCacheSize, Bool canExceedNrBuckets)
{
  itsCacheSize = max(aCacheSize,2u);
  // Limit the cache size if needed.
  if (!canExceedNrBuckets  &&  itsCacheSize > getCache().nBucket()) {
    itsCacheSize = itsCache->nBucket();
  }
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
      readHeader();
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
      readIndexBuckets();
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

void SSMBase::readHeader()
{
  // Set at start of file.
  itsFile->seek(0);
  // Use the file given by the BucketFile object
  // Use a buffer size (512) equal to start of buckets in the file,
  // so the IO buffers in the different objects do not overlap.
  CountedPtr<ByteIO> aFio = itsFile->makeFilebufIO (512);
  // It is stored in big or little endian canonical format.
  TypeIO* aTio;
  if (asBigEndian()) {
    aTio = new CanonicalIO (aFio.get());
  } else {
    aTio = new LECanonicalIO (aFio.get());
  }
  AipsIO anOs (aTio);
  uInt version = anOs.getstart("StandardStMan");
  itsBucketRows = 0;
  itsIdxBucketOffset = 0;
  Bool bigEndian = True;
  if (version >= 3) {
    anOs >> bigEndian;
  }
  if (bigEndian != asBigEndian()) {
    throw DataManError("Endian flag in SSM mismatches the table flag");
  }
  anOs >> itsBucketSize;                // Size of the bucket
  anOs >> itsNrBuckets;                 // Initial Nr of Buckets
  anOs >> itsPersCacheSize;             // Size of Persistent cache
  anOs >> itsFreeBucketsNr;             // Nr of Free Buckets
  anOs >> itsFirstFreeBucket;           // First Free Bucket nr
  anOs >> itsNrIdxBuckets;              // Nr of Buckets needed 4 Index
  anOs >> itsFirstIdxBucket;            // First indexBucket Number
  if (version >= 2) {
    anOs >> itsIdxBucketOffset;
  }
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

void SSMBase::readIndexBuckets()
{
  TypeIO*   aMio;
  MemoryIO  aMemBuf(itsIndexLength);

  uInt aCLength = 2*CanonicalConversion::canonicalSize (&itsFirstIdxBucket);
  getCache();
  // It is stored in big or little endian canonical format.
  if (asBigEndian()) {
    aMio = new CanonicalIO (&aMemBuf);
  } else {
    aMio = new LECanonicalIO (&aMemBuf);
  }
  AipsIO anMOs (aMio);

  Int aBucket = itsFirstIdxBucket;
  Int idxBucketSize = itsBucketSize - aCLength;
  Int aNr = itsIndexLength;
  char* aBucketPtr;
  for (uInt j=0; j< itsNrIdxBuckets; j++) {
    aBucketPtr = getBucket(aBucket);

    // First aCLength/2 bytes should be identical to next aCLength/2 bytes.
    // If not it might be an indicator that something went wrong
    // This can be used in the future
    Int aCheckNr;
    CanonicalConversion::toLocal (aCheckNr,aBucketPtr);
    CanonicalConversion::toLocal (aBucket,aBucketPtr+aCLength/2);
    if (aCheckNr != aBucket) {
      // Not used for now
    }

    // If offset is given, index fits in this single bucket from offset on.
    if (itsIdxBucketOffset > 0) {
      AlwaysAssert (itsIdxBucketOffset+itsIndexLength <= itsBucketSize
		    &&  itsNrIdxBuckets == 1,
		    AipsError);
      aMemBuf.write (aNr, aBucketPtr+itsIdxBucketOffset);
    } else if (aNr < idxBucketSize) {
      aMemBuf.write (aNr, aBucketPtr+aCLength);
    } else {
      aMemBuf.write (idxBucketSize, aBucketPtr+aCLength);
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

  // Use the file given by the BucketFile object..
  // Use a buffer size (512) equal to start of buckets in the file,
  // so the IO buffers in the different objects do not overlap.
  CountedPtr<ByteIO> aFio = itsFile->makeFilebufIO (512);
  uInt aCLength = 2*CanonicalConversion::canonicalSize(&itsFirstIdxBucket);

  // Store it in big or little endian canonical format.
  if (asBigEndian()) {
    aMio = new CanonicalIO (&aMemBuf);
    aTio = new CanonicalIO (aFio.get());
  } else {
    aMio = new LECanonicalIO (&aMemBuf);
    aTio = new LECanonicalIO (aFio.get());
  }
  AipsIO anMOs (aMio);

  uInt aNrIdx = itsPtrIndex.nelements();
  for (uInt i=0;i<aNrIdx; i++ ){
    itsPtrIndex[i]->put(anMOs);
  }
  anMOs.close();

  // Write total Mio in buckets.
  // Leave space for next bucket nr.
  const uChar* aMemPtr = aMemBuf.getBuffer();
  uInt idxLength = aMemBuf.length();
  uInt idxBucketSize = itsBucketSize-aCLength; 
  uInt aNrBuckets = idxLength / idxBucketSize;
  uInt aRestSize  = idxLength % idxBucketSize;
  if (aRestSize != 0) {
    aNrBuckets++;
  } else {
    aRestSize = idxBucketSize;
  }


  // If index is currently written in a single half of a bucket,
  // see if this fits in the other half.
  if (itsIdxBucketOffset > 0  &&  idxLength <= idxBucketSize/2) {
    if (itsIdxBucketOffset == aCLength) {
      itsIdxBucketOffset += idxBucketSize/2;
    } else {
      itsIdxBucketOffset = aCLength;
    }
    char* aBucketPtr = getBucket (itsFirstIdxBucket);
    memcpy (aBucketPtr+itsIdxBucketOffset, aMemPtr, idxLength);
    setBucketDirty();
  } else {

    // One or more new buckets are needed to store the index.
    Int aNewBucket = -1;
    Int anOldBucket = -1;
    for (uInt i=aNrBuckets; i>0; i--) {
      aNewBucket = getNewBucket();
      char* aBucketPtr = getBucket(aNewBucket);

      // Writing is done from the end to be able to fill in immediately
      // the nr of the next bucket (held in anOldBucket).
      CanonicalConversion::fromLocal (aBucketPtr, anOldBucket);
      CanonicalConversion::fromLocal (aBucketPtr+aCLength/2, anOldBucket);

      // Write rest of index as far as it fits.
      memcpy (aBucketPtr+aCLength, aMemPtr+((i-1)*idxBucketSize), aRestSize);
      setBucketDirty();
      aRestSize = idxBucketSize;
      anOldBucket = aNewBucket;
    }
 
    // New Index is written, give old indexbuckets free, and save firstBucketNr
    Int aBucket = itsFirstIdxBucket;
    while (aBucket != -1) {
      char* aBucketPtr = getBucket(aBucket);
      CanonicalConversion::toLocal (aBucket, aBucketPtr+aCLength/2);
      itsCache->removeBucket();
    }    
    itsFirstIdxBucket = aNewBucket;
    // If the index fits in half a bucket, we might be able to use the other
    // half when writying the index the next time.
    // Set the index offset variable accordingly.
    if (idxLength <= idxBucketSize/2) {
      itsIdxBucketOffset = aCLength;
    } else {
      itsIdxBucketOffset = 0;
    }
  }

  itsNrIdxBuckets = aNrBuckets;
  delete aMio;

  AlwaysAssert ( itsStringHandler != 0, AipsError);
  itsLastStringBucket = itsStringHandler->lastStringBucket();

  itsStringHandler->flush();
  itsCache->flush();

  aNrBuckets = getCache().nBucket();

  itsFile->seek (0);
  AipsIO anOs (aTio);
  
  // write a few items at the beginning of the file  AipsIO anOs (aTio);
  // The endian switch is a new feature. So only put it if little endian
  // is used. In that way older software can read newer tables.
  if (asBigEndian()) {
    anOs.putstart("StandardStMan", 2);
  } else {
    anOs.putstart("StandardStMan", 3);
    anOs << asBigEndian();
  }
  anOs << itsBucketSize;                // Size of the bucket
  anOs << aNrBuckets;                   // Present number of buckets
  anOs << itsPersCacheSize;             // Size of Persistent cache
  anOs << getCache().nFreeBucket();     // Nr of Free Buckets
  anOs << getCache().firstFreeBucket(); // First Free Bucket nr
  anOs << itsNrIdxBuckets;              // Nr buckets needed for index
  anOs << itsFirstIdxBucket;            // First Index bucket number
  anOs << itsIdxBucketOffset;           // Offset of bucket if fitting
  anOs << itsLastStringBucket;          // Last String bucket in use
  anOs << idxLength;                    // length of index
  anOs << uInt(itsPtrIndex.nelements());// Nr of indices
  
  anOs.putend();  
  anOs.close();
  delete aTio;
  aFio->flush();
  // Synchronize to make sure it gets written to disk.
  // This is needed for NFS-files under Linux (to resolve defect 2752).
  itsFile->fsync();
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
    Int aBucket = itsFirstIdxBucket;
    uInt aCLength = 2*CanonicalConversion::canonicalSize(&itsFirstIdxBucket);
    while (aBucket != -1) {
      char* aBucketPtr=getBucket(aBucket);
      CanonicalConversion::toLocal (aBucket, aBucketPtr+aCLength/2);
      itsCache->removeBucket();
    }
    itsFirstIdxBucket  = -1;
    itsIdxBucketOffset = 0;
    itsNrIdxBuckets    = 0;
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

  Int aSearchLength = aSSMC->getExternalSizeBits();

  Int  anOffset=-1;
  Int  aBestFit=-1;
  uInt saveIndex=0;
  Int  saveOffset=-1; 

  // Try if there is  freespace available where this column can fit (best fit)
  // For now we assume that a best fit will be :
  //                                             1) exact fit
  //                                             2) any fit

  for (uInt i=0; i<itsPtrIndex.nelements() && 
	         aBestFit != aSearchLength; i++) {
   
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

    // calculate rowsperbucket for new index
    AlwaysAssert (aSearchLength != 0, AipsError);
    uInt rowsPerBucket = itsBucketSize*8 / aSearchLength;

    if (rowsPerBucket < 1) {
      // The BucketSize is too small to contain data.
      throw (DataManError ("StandardStMan::addColumn  bucketsize too small"
                           " for adding column " + aColumn->columnName()));
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

void SSMBase::removeBucket (uInt aBucketNr)
{
  getCache().getBucket(aBucketNr);
  getCache().removeBucket();
}  

char*  SSMBase::getBucket (uInt aBucketNr)
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
  itsFile = new BucketFile (fileName(), 0, False, multiFile());
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
  isDataChanged = True;
}

Bool SSMBase::hasMultiFileSupport() const
  { return True; }

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
  
  ios.putstart ("SSM", 2);
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
    readHeader();
  }
  if (itsCache != 0) {
    itsCache->resync (itsNrBuckets, itsFreeBucketsNr, 
		      itsFirstFreeBucket);
  }
  if (itsPtrIndex.nelements() != 0) {
    readIndexBuckets();
  }  
  if (itsStringHandler != 0) {
    itsStringHandler->resync();
  }

  uInt aNrCol = ncolumn();
  if (itsIosFile != 0) {
    itsIosFile->resync();
  }
  for (uInt i=0; i<aNrCol; i++) {
    itsPtrColumn[i]->resync (itsNrRows);
  }
}

void SSMBase::create (uInt aNrRows)
{
  init();
  recreate();
  itsNrRows = 0;
  addRow (aNrRows);
}

void SSMBase::open (uInt aRowNr, AipsIO& ios)
{
  itsNrRows = aRowNr;
  ios.getstart ("SSM");
  ios >> itsDataManName;
  getBlock (ios,itsColumnOffset);
  getBlock (ios,itsColIndexMap);
  ios.getend();
  
  itsFile = new BucketFile (fileName(), table().isWritable(),
                            0, False, multiFile());
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
				     0, asBigEndian(), 0, multiFile());
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

void SSMBase::deleteManager()
{
  delete itsIosFile;
  itsIosFile = 0;
  // Clear cache without flushing.
  if (itsCache != 0) {
    itsCache->clear (0, False);
  }
  if (itsFile != 0) {
    itsFile->remove();
    delete itsFile;
    itsFile = 0;
  }
}

void SSMBase::init()
{
  // Size the blocks as needed.
  uInt nrCol = ncolumn();
  itsColumnOffset.resize (nrCol, True);                            
  itsColIndexMap.resize (nrCol, True);                            
  itsColIndexMap = 0;
  // Set the bucket size and get nr of rows per bucket.
  // If an advised nr of rows per bucket was given and the actual
  // nr is smaller, adjust it to fill up the last bucket.
  uInt rowsPerBucket = setBucketSize();
  if (itsBucketRows > 0  &&  itsBucketRows > rowsPerBucket) {
    uInt nbuckets = (itsBucketRows + rowsPerBucket - 1) / rowsPerBucket;
    itsBucketRows = (itsBucketRows + nbuckets - 1) / nbuckets;
    rowsPerBucket = setBucketSize();
  }
  // Determine the offset of each column.
  // Note that the data of a column are consecutive per bucket.
  uInt aTotalSize = 0;
  for (uInt i=0; i<nrCol; i++) {
    itsColumnOffset[i] = aTotalSize;
    aTotalSize += (rowsPerBucket *
		   itsPtrColumn[i]->getExternalSizeBits() + 7) / 8;
  }
  
  // All columns are in the same bucket list, thus only one SSMIndex needed.
  itsPtrIndex.resize (1, True);
  itsPtrIndex[0] = new SSMIndex(this, rowsPerBucket);
  itsPtrIndex[0]->setNrColumns (nrCol, aTotalSize);
}


uInt SSMBase::setBucketSize()
{
  // Find nr of columns and possibly advised nr of rows per bucket.
  uInt nrCol = ncolumn();
  uInt advBucketRows = itsBucketRows;
  // Finding the nr of rows fitting in the bucket is a bit hard, because
  // Bool values are stored as bits. Therefore we have to iterate.
  // First find the nr of full bytes needed (ignoring possible remainders).
  uInt aTotalSize = 0;
  for (uInt i=0; i<nrCol; i++) {
    aTotalSize += itsPtrColumn[i]->getExternalSizeBytes();
  }
  // Get first guess for nr of rows per bucket.
  if (itsBucketSize < 128) {
    itsBucketSize = 128;
  }
  uInt rowsPerBucket = advBucketRows;
  if (advBucketRows == 0) {
    if (itsBucketSize < 128) {
      itsBucketSize = 128;
    }
    rowsPerBucket = itsBucketSize/aTotalSize;
  }
  // Now refine it by determining how big bucket is when using one more row.
  while (True) {
    uInt aThisSize = 0;
    uInt aNextSize = 0;
    for (uInt i=0; i<nrCol; i++) {
      aThisSize += (rowsPerBucket *
		    itsPtrColumn[i]->getExternalSizeBits() + 7) / 8;
      aNextSize += ((rowsPerBucket+1) *
		    itsPtrColumn[i]->getExternalSizeBits() + 7) / 8;
    }
    // If advised #rows/bucket given, get bucket size.
    if (advBucketRows > 0) {
      itsBucketSize = min (32768u, max(128u, aThisSize));
      if (itsBucketSize == aThisSize) {
	break;
      }
      // Exceeding minimum or maximum, so calculate #rows/bucket.
      rowsPerBucket = itsBucketSize/aTotalSize;
      advBucketRows = 0;
    } else {
      // Stop if one more row does not fit.
      if (aNextSize > itsBucketSize) {
	break;
      }
      rowsPerBucket++;
    }
  }
  if (rowsPerBucket < 1) {
    // The bucket size is too small to contain all columns, so adjust it.
    itsBucketSize = aTotalSize;
    rowsPerBucket = 1;
  }
  AlwaysAssert (itsBucketSize >= 128, AipsError);
  return rowsPerBucket;
}

} //# NAMESPACE CASACORE - END
