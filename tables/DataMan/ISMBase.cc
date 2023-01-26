//# ISMBase.cc: Base class of the Incremental Storage Manager
//# Copyright (C) 1996,1997,1999,2000,2001,2002
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


#include <casacore/tables/DataMan/ISMBase.h>
#include <casacore/tables/DataMan/ISMBucket.h>
#include <casacore/tables/DataMan/ISMColumn.h>
#include <casacore/tables/DataMan/ISMIndColumn.h>
#include <casacore/tables/DataMan/ISMIndex.h>
#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/IO/BucketCache.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/LECanonicalIO.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/OS/DOos.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/ostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ISMBase::ISMBase (uint32_t bucketSize, bool checkBucketSize, uint32_t cacheSize)
: DataManager       (),
///  dataManName_p     ("ISM0"),
  version_p         (3),
  iosfile_p         (0),
  uniqnr_p          (0),
  cache_p           (0),
  file_p            (0),
  index_p           (0),
  persCacheSize_p   (cacheSize),
  cacheSize_p       (0),
  nbucketInit_p     (1),
  nFreeBucket_p     (0),
  firstFree_p       (-1),
  bucketSize_p      (bucketSize),
  checkBucketSize_p (checkBucketSize),
  dataChanged_p     (false),
  tempBuffer_p      (0)
{}

ISMBase::ISMBase (const String& dataManagerName,
		  uint32_t bucketSize, bool checkBucketSize, uint32_t cacheSize)
: DataManager       (),
  dataManName_p     (dataManagerName),
  version_p         (3),
  iosfile_p         (0),
  uniqnr_p          (0),
  cache_p           (0),
  file_p            (0),
  index_p           (0),
  persCacheSize_p   (cacheSize),
  cacheSize_p       (0),
  nbucketInit_p     (1),
  nFreeBucket_p     (0),
  firstFree_p       (-1),
  bucketSize_p      (bucketSize),
  checkBucketSize_p (checkBucketSize),
  dataChanged_p     (false),
  tempBuffer_p      (0)
{}

ISMBase::ISMBase (const String& dataManagerName, const Record& spec)
: DataManager       (),
  dataManName_p     (dataManagerName),
  version_p         (3),
  iosfile_p         (0),
  uniqnr_p          (0),
  cache_p           (0),
  file_p            (0),
  index_p           (0),
  persCacheSize_p   (1),
  cacheSize_p       (0),
  nbucketInit_p     (1),
  nFreeBucket_p     (0),
  firstFree_p       (-1),
  bucketSize_p      (32768),
  checkBucketSize_p (false),
  dataChanged_p     (false),
  tempBuffer_p      (0)
{
    if (spec.isDefined ("BUCKETSIZE")) {
        bucketSize_p = spec.asInt ("BUCKETSIZE");
    }
    if (spec.isDefined ("CHECKBUCKETSIZE")) {
        checkBucketSize_p = spec.asBool ("CHECKBUCKETSIZE");
    }
    if (spec.isDefined ("PERSCACHESIZE")) {
        persCacheSize_p = spec.asuInt ("PERSCACHESIZE");
    }
}

ISMBase::ISMBase (const ISMBase& that)
: DataManager       (),
  dataManName_p     (that.dataManName_p),
  version_p         (that.version_p),
  iosfile_p         (0),
  uniqnr_p          (0),
  cache_p           (0),
  file_p            (0),
  index_p           (0),
  persCacheSize_p   (that.persCacheSize_p),
  cacheSize_p       (that.cacheSize_p),
  nbucketInit_p     (1),
  nFreeBucket_p     (0),
  firstFree_p       (-1),
  bucketSize_p      (that.bucketSize_p),
  checkBucketSize_p (that.checkBucketSize_p),
  dataChanged_p     (false),
  tempBuffer_p      (0)
{}

ISMBase::~ISMBase()
{
    for (uint32_t i=0; i<ncolumn(); i++) {
	delete colSet_p[i];
    }
    delete index_p;
    delete cache_p;
    delete file_p;
    delete [] tempBuffer_p;
    delete iosfile_p;
}

DataManager* ISMBase::clone() const
{
    return new ISMBase (*this);
}

String ISMBase::dataManagerType() const
{
    return "IncrementalStMan";
}
String ISMBase::dataManagerName() const
{
    return dataManName_p;
}

Record ISMBase::dataManagerSpec() const
{
  Record rec = getProperties();
  rec.define ("BUCKETSIZE", int32_t(bucketSize_p));
  rec.define ("PERSCACHESIZE", persCacheSize_p);
  return rec;
}

Record ISMBase::getProperties() const
{
  // Make sure the cache is initialized, so the header has certainly been read.
  const_cast<ISMBase*>(this)->getCache();
  Record rec;
  rec.define ("MaxCacheSize", int32_t(cacheSize_p));
  return rec;
}

void ISMBase::setProperties (const Record& rec)
{
  if (rec.isDefined("MaxCacheSize")) {
    setCacheSize (rec.asInt("MaxCacheSize"), false);
  }
}

void ISMBase::clearCache()
{
    if (cache_p != 0) {
	cache_p->clear();
    }
}

void ISMBase::showCacheStatistics (ostream& os) const
{
    if (cache_p != 0) {
	os << ">>> IncrementalStMan cache statistics:" << endl;
	cache_p->showStatistics (os);
	os << "<<<" << endl;
    }
}

void ISMBase::showIndexStatistics (ostream& os)
{
    if (index_p != 0) {
        index_p->show (os);
    }
}

void ISMBase::showBucketLayout (ostream& os)
{
  uint32_t cursor=0;
  rownr_t bstrow=0;
  rownr_t bnrow;
  uint32_t bucketNr;
  while (getIndex().nextBucketNr (cursor, bstrow, bnrow, bucketNr)) {
    os << " bucket strow=" << bstrow << " bucketnr=" << bucketNr << endl;
    ((ISMBucket*) (getCache().getBucket (bucketNr)))->show (os);
  }
}

DataManagerColumn* ISMBase::makeScalarColumn (const String&,
					      int dataType,
					      const String&)
{
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    ISMColumn* colp = new ISMColumn (this, dataType, ncolumn());
    colSet_p[ncolumn()] = colp;
    return colp;
}
DataManagerColumn* ISMBase::makeDirArrColumn (const String& name,
					      int dataType,
					      const String& dataTypeId)
{
    return makeScalarColumn (name, dataType, dataTypeId);
}
DataManagerColumn* ISMBase::makeIndArrColumn (const String&,
					      int dataType,
					      const String&)
{
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    ISMColumn* colp = new ISMIndColumn (this, dataType, ncolumn());
    colSet_p[ncolumn()] = colp;
    return colp;
}

DataManager* ISMBase::makeObject (const String& group, const Record& spec)
{
    // This function is called when reading a table back.
    // Construct it with the default bucket size and cache size.
    return new ISMBase (group, spec);
}

void ISMBase::setCacheSize (uint32_t cacheSize, bool canExceedNrBuckets)
{
    cacheSize_p = cacheSize;
    // Limit the cache size if needed.
    if (!canExceedNrBuckets  &&  cacheSize_p > getCache().nBucket()) {
        cacheSize_p = cache_p->nBucket();
    }
    if (cache_p != 0) {
	cache_p->resize (cacheSize_p);
    }
}

void ISMBase::makeCache()
{
    if (cache_p == 0) {
	makeIndex();
	// Set cache size to persistent cache size if not set explicitly yet.
	if (cacheSize_p == 0) {
	    cacheSize_p = persCacheSize_p;
	}
	cache_p = new BucketCache (file_p, 512, bucketSize_p,
				   nbucketInit_p, cacheSize_p,
				   this,
				   ISMBucket::readCallBack, 
				   ISMBucket::writeCallBack,
				   ISMBucket::initCallBack,
				   ISMBucket::deleteCallBack);
	cache_p->resync (nbucketInit_p, nFreeBucket_p, firstFree_p);
	AlwaysAssert (cache_p != 0, AipsError);
	// Allocate a buffer for temporary storage by all ISM classes.
	if (tempBuffer_p == 0) {
	    tempBuffer_p = new char [bucketSize_p];
	    AlwaysAssert (tempBuffer_p != 0, AipsError);
	}
    }
}

void ISMBase::makeIndex()
{
    if (index_p != 0) {
	return;
    }
    index_p = new ISMIndex (this);
    AlwaysAssert (index_p != 0, AipsError);
    file_p->open();
    readIndex();
}

void ISMBase::readIndex()
{
    file_p->seek (0);
    // Use the file given by the BucketFile object.
    CountedPtr<ByteIO> fio = file_p->makeFilebufIO (1024);
    TypeIO* tio;
    // It is stored in canonical or local format.
    if (asBigEndian()) {
      tio = new CanonicalIO (fio.get());
    }else{
      tio = new LECanonicalIO (fio.get());
    }
    AipsIO os (tio);
    uint32_t version = os.getstart ("IncrementalStMan");
    //# ISMBase.cc version 11.1 contained a little error.
    //# It used the version of putstart("IncrementalStMan") instead of
    //# the version from putstart("ISM").
    //# The incorrect one used "IncrementalStMan",3 and "ISM",1.
    //# The fixed one uses     "IncrementalStMan",2 and "ISM",3.
    //# This newest one uses   "IncrementalStMan",4 and "ISM",3.
    //# It was fixed immediately after a weekly inhale, but unfortunately
    //# the TMS system in Westerbork used that version for a while
    //# without applying the fix.
    //# Therefore this hack (together with one in function open) is needed
    //# to make these MSs accessible.
    if (version == 3) {
        version_p = 3;
    }
    bool bigEndian = true;
    if (version >= 5) {
      os >> bigEndian;
    }
    if (bigEndian != asBigEndian()) {
      throw DataManError("Endian flag in ISM mismatches the table flag");
    }
    os >> bucketSize_p;
    os >> nbucketInit_p;
    os >> persCacheSize_p;
    os >> uniqnr_p;
    if (version > 1) {
	os >> nFreeBucket_p;
	os >> firstFree_p;
    }
    os.getend();
    int64_t off = nbucketInit_p;
    os.setpos (512 + off * bucketSize_p);
    index_p->get (os);
    os.close();
    delete tio;
}

void ISMBase::writeIndex()
{
    if (index_p == 0) {
	return;
    }
    uint32_t nbuckets = getCache().nBucket();
    // Write a few items at the beginning of the file.
    file_p->seek (0);
    // Use the file given by the BucketFile object.
    CountedPtr<ByteIO> fio = file_p->makeFilebufIO (1024);
    TypeIO* tio;
    // Store it in canonical or local format.
    if (asBigEndian()) {
      tio = new CanonicalIO (fio.get());
    }else{
      tio = new LECanonicalIO (fio.get());
    }
    AipsIO os (tio);
    // The endian switch is a new feature. So only put it if little endian
    // is used. In that way older software can read newer tables.
    if (asBigEndian()) {
      os.putstart ("IncrementalStMan", 4);
    } else {
      os.putstart ("IncrementalStMan", 5);
      os << asBigEndian();
    }
    os << bucketSize_p;
    os << nbuckets;
    os << persCacheSize_p;
    os << uniqnr_p;
    os << getCache().nFreeBucket();
    os << getCache().firstFreeBucket();
    os.putend();
    // Write the index itself at the very end of the file.
    int64_t off = nbuckets;
    os.setpos (512 + off * bucketSize_p);
    index_p->put (os);
    os.close();
    delete tio;
}
    

ISMBucket* ISMBase::getBucket (rownr_t rownr, rownr_t& bucketStartRow,
			       rownr_t& bucketNrrow)
{
    uint32_t bucketNr = getIndex().getBucketNr (rownr, bucketStartRow,
                                            bucketNrrow);
    return (ISMBucket*) (getCache().getBucket (bucketNr));
}

ISMBucket* ISMBase::nextBucket (uint32_t& cursor, rownr_t& bucketStartRow,
				rownr_t& bucketNrrow)
{
    uint32_t bucketNr;
    if (getIndex().nextBucketNr (cursor, bucketStartRow,
				  bucketNrrow, bucketNr)) {
	return (ISMBucket*) (getCache().getBucket (bucketNr));
    }
    return 0;
}

void ISMBase::setBucketDirty()
{
    cache_p->setDirty();
    dataChanged_p = true;
}

void ISMBase::addBucket (rownr_t rownr, ISMBucket* bucket)
{
    // Add the bucket to the cache and the index.
    // It's the last bucket in the cache.
    uint32_t bucketNr = getCache().addBucket ((char*)bucket);
    getIndex().addBucketNr (rownr, bucketNr);
}

//# The storage manager can add rows.
bool ISMBase::canAddRow() const
{
    return true;
}
//# The storage manager can delete rows.
bool ISMBase::canRemoveRow() const
{
    return true;
}
//# The storage manager cannot add columns (not yet).
bool ISMBase::canAddColumn() const
{
    return false;
}
//# The storage manager cannot delete columns (not yet).
bool ISMBase::canRemoveColumn() const
{
    return false;
}


void ISMBase::addRow64 (rownr_t nrrow)
{
    getIndex().addRow (nrrow);
    uint32_t nrcol = ncolumn();
    for (uint32_t i=0; i<nrcol; i++) {
	colSet_p[i]->addRow (nrrow_p + nrrow, nrrow_p);
    }
    nrrow_p += nrrow;
    dataChanged_p = true;
}

void ISMBase::removeRow64 (rownr_t rownr)
{
    // Get the bucket and interval to which the row belongs.
    uint32_t i;
    rownr_t bucketStartRow;
    rownr_t bucketNrrow;
    ISMBucket* bucket = getBucket (rownr, bucketStartRow, bucketNrrow);
    uint32_t bucketRownr = rownr - bucketStartRow;
    // Remove that row from the bucket for all columns.
    uint32_t nrcol = ncolumn();
    for (i=0; i<nrcol; i++) {
	colSet_p[i]->remove (bucketRownr, bucket, bucketNrrow, nrrow_p-1);
    }
    // Remove the row from the index.
    int32_t emptyBucket = getIndex().removeRow (rownr);
    nrrow_p--;
    // When no more rows left, recreate index and cache.
    if (nrrow_p == 0) {
	recreate();
    }else{
	// Remove the bucket if it is empty now.
	if (emptyBucket >= 0) {
	    getCache().getBucket (emptyBucket);
	    getCache().removeBucket();
	}
    }
    dataChanged_p = true;
}


// Note that the column has already been added by makeXXColumn.
// This function is merely for initializing the added column.
void ISMBase::addColumn (DataManagerColumn* colp)
{
    // AddColumn is not possible yet.
    throw (DataManInvOper ("IncrementalStMan::addColumn not possible yet"));
    for (uint32_t i=0; i<ncolumn(); i++) {
	if (colp == colSet_p[i]) {
	    colSet_p[i]->doCreate ((ISMBucket*)(getCache().getBucket (0)));
	    dataChanged_p = true;
	    return;
	}
    }
    throw (DataManInternalError ("ISMBase::addColumn"));
}

void ISMBase::removeColumn (DataManagerColumn* colp)
{
    // RemoveColumn is not possible yet.
    throw (DataManInvOper ("IncrementalStMan::removeColumn not possible yet"));
    for (uint32_t i=0; i<ncolumn(); i++) {
	if (colSet_p[i] == colp) {
	    delete colSet_p[i];
	    decrementNcolumn();
	    for (; i<ncolumn(); i++) {
		colSet_p[i-1] = colSet_p[i];
	    }
	    dataChanged_p = true;
	    return;
	}
    }
    throw (DataManInternalError ("ISMBase::removeColumn"));
}


void ISMBase::recreate()
{
    delete index_p;
    index_p = 0;
    delete cache_p;
    cache_p = 0;
    delete file_p;
    file_p = 0;
    delete iosfile_p;
    iosfile_p = 0;
    nbucketInit_p = 1;
    nFreeBucket_p = 0;
    firstFree_p   = -1;
    file_p = new BucketFile (fileName(), 0, false, multiFile());
    AlwaysAssert (file_p != 0, AipsError);
    index_p = new ISMIndex (this);
    AlwaysAssert (index_p != 0, AipsError);
    makeCache();
    //# Let the column objects create something if needed.
    for (uint32_t i=0; i<ncolumn(); i++) {
	colSet_p[i]->doCreate ((ISMBucket*)(getCache().getBucket (0)));
    }
    setBucketDirty();
}

bool ISMBase::hasMultiFileSupport() const
  { return true; }

bool ISMBase::flush (AipsIO& ios, bool fsync)
{
    //# Let the column objects flush themselves (if needed).
    //# Check if anything has changed.
    bool changed = false;
    uint32_t nrcol = ncolumn();
    for (uint32_t i=0; i<nrcol; i++) {
	if (colSet_p[i]->flush (nrrow_p, fsync)) {
	    changed = true;
	}
    }
    if (cache_p != 0) {
	cache_p->flush();
    }
    if (dataChanged_p) {
	writeIndex();
	if (fsync) {
	    file_p->fsync();
	}
	changed = true;
	dataChanged_p = false;
    }
    ios.putstart ("ISM", version_p);
    ios << dataManName_p;
    ios.putend();
    return changed;
}

rownr_t ISMBase::resync64 (rownr_t nrrow)
{
    nrrow_p = nrrow;
    if (index_p != 0) {
	readIndex();
    }
    if (cache_p != 0) {
	cache_p->resync (nbucketInit_p, nFreeBucket_p, firstFree_p);
    }
    uint32_t nrcol = ncolumn();
    for (uint32_t i=0; i<nrcol; i++) {
	colSet_p[i]->resync (nrrow_p);
    }
    if (iosfile_p != 0) {
        iosfile_p->resync();
    }
    return nrrow_p;
}

void ISMBase::create64 (rownr_t nrrow)
{
    init();
    recreate();
    nrrow_p = 0;
    addRow64 (nrrow);
}

rownr_t ISMBase::open64 (rownr_t tabNrrow, AipsIO& ios)
{
    nrrow_p = tabNrrow;
    // Do not check the bucketsize for an existing table.
    checkBucketSize_p = false;
    version_p = ios.getstart ("ISM");
    ios >> dataManName_p;
    ios.getend();
    init();
    file_p = new BucketFile (fileName(), table().isWritable(),
                             0, false, multiFile());
    AlwaysAssert (file_p != 0, AipsError);
    //# Westerbork MSs have a problem, because TMS used for a while
    //# the erroneous version of ISMBase.cc.
    //# So if we have an old ISM version, do a makeIndex to get
    //# the version from the index. That was 3.
    if (version_p == 1) {
        makeIndex();
    }
    //# Let the column objects initialize themselves (if needed).
    uint32_t nrcol = ncolumn();
    for (uint32_t i=0; i<nrcol; i++) {
	colSet_p[i]->getFile (nrrow_p);
    }
    return nrrow_p;
}

StManArrayFile* ISMBase::openArrayFile (ByteIO::OpenOption opt)
{
    if (iosfile_p == 0) {
	iosfile_p = new StManArrayFile (fileName() + 'i', opt,
					1, asBigEndian(), 0, multiFile());
    }
    return iosfile_p;
}

void ISMBase::reopenRW()
{
    file_p->setRW();
    uint32_t nrcol = ncolumn();
    for (uint32_t i=0; i<nrcol; i++) {
	colSet_p[i]->reopenRW();
    }
}

void ISMBase::deleteManager()
{
    delete iosfile_p;
    iosfile_p = 0;
    // Clear cache without flushing.
    if (cache_p != 0) {
      cache_p->clear (0, false);
    }
    if (file_p != 0) {
      file_p->remove();
      delete file_p;
      file_p = 0;
    }
}


void ISMBase::init()
{
    // Determine the size of a uint32_t in external format.
    uIntSize_p  = ValType::getCanonicalSize (TpUInt, asBigEndian());
    rownrSize_p = sizeof(rownr_t);
    // Get the total length for all columns.
    // Use 32 for each variable length element.
    // On top of that each variable length element requires uIntSize_p bytes
    // and uIntSize_p for all elements together (representing total length
    // and length per element).
    uint32_t fixedSize = 0;
    uint32_t varSize = 0;
    uint32_t nrcol = ncolumn();
    uint32_t headerSize = uIntSize_p * (nrcol + 1);        // needed per column
    for (uint32_t i=0; i<nrcol; i++) {
	uint32_t leng = colSet_p[i]->getFixedLength();
	fixedSize += 2 * uIntSize_p;                   // indices per column
	if (leng == 0) {
	    uint32_t nr = colSet_p[i]->nelements();
	    fixedSize += uIntSize_p * (nr + 1);        // length values
	    varSize   += 32 * nr;
	}else{
	    fixedSize += leng;
	}
    }
    if (checkBucketSize_p  &&  bucketSize_p > 0) {
	// The bucket size is defined. Check if at least 2
	// fixed-length items for each row fit in it.
	// When the bucket is smaller than 32768 bytes, check
	// if can hold at least 10 rows (since it makes no sense to
	// have very small buckets).
	if (bucketSize_p < headerSize + 2*fixedSize) {
	    throw (DataManError ("IncrementalStMan: bucket too small "
				 "to hold 2 rows"));
	}else if (bucketSize_p < 32768) {
	    if (bucketSize_p < headerSize + 10*fixedSize) {
		throw (DataManError ("IncrementalStMan: bucket < 32768 and "
				     "too small to hold 10 rows"));
	    }
	}
    }
    if (bucketSize_p == 0) {
	// Calculate the bucket size.
	// Try to fit 100 rows (with a minimum of 32768 bytes).
	// If that results in a very large size (> 327680) try to fit
	// 10 rows. If that still results in a large size, use 327680
	// but at least 2 rows have to fit in it.
	bucketSize_p = headerSize + 100 * (fixedSize + varSize);
	if (bucketSize_p < 32768) {
	    bucketSize_p = 32768;
	} else if (bucketSize_p > 327680) {
	    bucketSize_p = headerSize + 10 * (fixedSize + varSize);
	    if (bucketSize_p > 327680) {
		bucketSize_p = headerSize + 2 * (fixedSize + varSize);
		if (bucketSize_p < 327680) {
		    bucketSize_p = 327680;
		}
	    }
	}
    }
    // Only 28 bits can be used for the offset, so the bucketsize should
    // not be larger.
    if (bucketSize_p >= 1<<28) {
        throw DataManError("IncrementalStMan: bucketSize exceeds 28 bits"
                           " (>= 268435456)");
    }
}

bool ISMBase::checkBucketLayout (uint32_t& offendingCursor,
                                 rownr_t& offendingBucketStartRow,
                                 uint32_t& offendingBucketNrow,
                                 uint32_t& offendingBucketNr,
                                 uint32_t& offendingCol,
                                 uint32_t& offendingIndex,
                                 rownr_t& offendingRow,
                                 rownr_t& offendingPrevRow)
{
  bool ok = false;
  uint32_t cursor = 0;
  rownr_t bucketStartRow = 0;
  rownr_t bucketNrow = 0;
  uint32_t bucketNr = 0;
  while (getIndex().nextBucketNr(cursor, bucketStartRow, bucketNrow, bucketNr)) {
    ok = ((ISMBucket*) (getCache().getBucket(bucketNr)))->check(offendingCol,
                                                                offendingIndex,
                                                                offendingRow,
                                                                offendingPrevRow);
    if (not ok) {
      offendingCursor = cursor;
      offendingBucketStartRow = bucketStartRow;
      offendingBucketNrow = bucketNrow;
      offendingBucketNr = bucketNr;
      return false;
    }
  }
  return true;
}

} //# NAMESPACE CASACORE - END

