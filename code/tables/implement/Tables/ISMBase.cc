//# ISMBase.cc: Base class of the Incremental Storage Manager
//# Copyright (C) 1996,1997
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


#include <aips/Tables/ISMBase.h>
#include <aips/Tables/ISMBucket.h>
#include <aips/Tables/ISMColumn.h>
#include <aips/Tables/ISMIndColumn.h>
#include <aips/Tables/ISMIndex.h>
#include <aips/Tables/BucketCache.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/ValType.h>
#include <aips/Utilities/Assert.h>
#include <aips/IO/AipsIO.h>
#include <aips/IO/RawIO.h>
#include <aips/IO/CanonicalIO.h>
#include <aips/IO/FilebufIO.h>
#include <aips/Tables/DataManError.h>
#include <iostream.h>


ISMBase::ISMBase (uInt bucketSize, Bool checkBucketSize, uInt cacheSize)
: DataManager       (),
  bucketSize_p      (bucketSize),
  checkBucketSize_p (checkBucketSize),
  cacheSize_p       (0),
  persCacheSize_p   (cacheSize),
  uniqnr_p          (0),
  index_p           (0),
  file_p            (0),
  cache_p           (0),
  tempBuffer_p      (0)
{}

ISMBase::ISMBase (const String& dataManagerName,
		  uInt bucketSize, Bool checkBucketSize, uInt cacheSize)
: DataManager       (),
  dataManName_p     (dataManagerName),
  bucketSize_p      (bucketSize),
  checkBucketSize_p (checkBucketSize),
  cacheSize_p       (0),
  persCacheSize_p   (cacheSize),
  uniqnr_p          (0),
  index_p           (0),
  file_p            (0),
  cache_p           (0),
  tempBuffer_p      (0)
{}

ISMBase::ISMBase (const ISMBase& that)
: DataManager       (),
  dataManName_p     (that.dataManName_p),
  bucketSize_p      (that.bucketSize_p),
  checkBucketSize_p (that.checkBucketSize_p),
  cacheSize_p       (that.cacheSize_p),
  persCacheSize_p   (that.persCacheSize_p),
  uniqnr_p          (0),
  index_p           (0),
  file_p            (0),
  cache_p           (0),
  tempBuffer_p      (0)
{}

ISMBase::~ISMBase()
{
    for (uInt i=0; i<ncolumn(); i++) {
	delete colSet_p[i];
    }
    delete index_p;
    delete cache_p;
    delete file_p;
    delete [] tempBuffer_p;
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

void ISMBase::clearCache()
{
    if (cache_p != 0) {
	cache_p->resize (0);
	cache_p->clear();
	cache_p->resize (cacheSize_p);
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


DataManagerColumn* ISMBase::makeScalarColumn (const String&,
					      int dataType,
					      const String&)
{
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    ISMColumn* colp = new ISMColumn (this, dataType, ncolumn());
    if (colp == 0) {
	throw (AllocError ("ISMBase::makeScalarColumn", 1));
    }
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
    if (colp == 0) {
	throw (AllocError ("ISMBase::makeIndArrColumn", 1));
    }
    colSet_p[ncolumn()] = colp;
    return colp;
}

DataManager* ISMBase::makeObject (const String&)
{
    // This function is called when reading a table back.
    // Construct it with the default bucket size and cache size.
    return new ISMBase (0, False);
}

void ISMBase::setCacheSize (uInt cacheSize)
{
    cacheSize_p = cacheSize;
    if (cache_p != 0) {
	cache_p->resize (cacheSize);
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
				   getIndex().nbuckets(), cacheSize_p,
				   this,
				   ISMBucket::readCallBack, 
				   ISMBucket::writeCallBack,
				   ISMBucket::initCallBack,
				   ISMBucket::deleteCallBack);
	AlwaysAssert (cache_p != 0, AipsError);
	// Allocate a buffer for temporary storage by all ISM classes.
	tempBuffer_p = new char [bucketSize_p];
	AlwaysAssert (tempBuffer_p != 0, AipsError);
    }
}

void ISMBase::makeIndex()
{
    if (index_p != 0) {
	return;
    }
    file_p->open();
    file_p->seek (0);
    uInt nbuckets;
    // Use the file indicated by the fd from the BucketFile object.
    FilebufIO fio (file_p->fd());
    TypeIO* tio;
    // Store it in canonical or local format.
    if (asCanonical()) {
	tio = new CanonicalIO (&fio);
    }else{
	tio = new RawIO (&fio);
    }
    AipsIO os (tio);
    os.getstart ("IncrementalStMan");
    os >> bucketSize_p;
    os >> nbuckets;
    os >> persCacheSize_p;
    os >> uniqnr_p;
    os.getend();
    Long off = nbuckets;
    os.setpos (512 + off * bucketSize_p);
    index_p = new ISMIndex (this, os);
    AlwaysAssert (index_p != 0, AipsError);
    os.close();
    delete tio;
}

void ISMBase::writeIndex()
{
    if (index_p == 0) {
	return;
    }
    uInt nbuckets = index_p->nbuckets();
    // Write a few items at the beginning of the file.
    file_p->seek (0);
    // Use the file indicated by the fd from the BucketFile object.
    FilebufIO fio (file_p->fd());
    TypeIO* tio;
    // Store it in canonical or local format.
    if (asCanonical()) {
	tio = new CanonicalIO (&fio);
    }else{
	tio = new RawIO (&fio);
    }
    AipsIO os (tio);
    os.putstart ("IncrementalStMan", 1);
    os << bucketSize_p;
    os << nbuckets;
    os << persCacheSize_p;
    os << uniqnr_p;
    os.putend();
    // Write the index itself at the very end of the file.
    Long off = nbuckets;
    os.setpos (512 + off * bucketSize_p);
    index_p->put (os);
    os.close();
    delete tio;
}
    

ISMBucket* ISMBase::getBucket (uInt rownr, uInt& bucketStartRow,
			       uInt& bucketNrrow)
{
    uInt bucketNr = getIndex().getBucketNr (rownr, bucketStartRow,
					     bucketNrrow);
    return (ISMBucket*) (getCache().getBucket (bucketNr));
}

ISMBucket* ISMBase::nextBucket (uInt& cursor, uInt& bucketStartRow,
				uInt& bucketNrrow)
{
    uInt bucketNr;
    if (getIndex().nextBucketNr (cursor, bucketStartRow,
				  bucketNrrow, bucketNr)) {
	return (ISMBucket*) (getCache().getBucket (bucketNr));
    }
    return 0;
}

void ISMBase::setBucketDirty()
{
    cache_p->setDirty();
}

void ISMBase::addBucket (uInt rownr, ISMBucket* bucket)
{
    // Add the bucket to the cache and the index.
    // It's the last bucket in the cache.
    getCache().addBucket ((char*)bucket);
    getIndex().addBucketNr (rownr, getCache().nBucket() - 1);
}

//# The storage manager can add rows.
Bool ISMBase::canAddRow() const
{
    return True;
}
//# The storage manager can delete rows.
Bool ISMBase::canRemoveRow() const
{
    return True;
}
//# The storage manager cannot add columns (not yet).
Bool ISMBase::canAddColumn() const
{
    return False;
}
//# The storage manager cannot delete columns (not yet).
Bool ISMBase::canRemoveColumn() const
{
    return False;
}


void ISMBase::addRow (uInt nrrow)
{
    getIndex().addRow (nrrow);
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	colSet_p[i]->addRow (nrrow_p + nrrow, nrrow_p);
    }
    nrrow_p += nrrow;
}

void ISMBase::removeRow (uInt rownr)
{
    // Get the bucket and interval to which the row belongs.
    uInt bucketStartRow, bucketNrrow;
    ISMBucket* bucket = getBucket (rownr, bucketStartRow, bucketNrrow);
    uInt bucketRownr = rownr - bucketStartRow;
    // Remove that row from the bucket for all columns.
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	colSet_p[i]->remove (bucketRownr, bucket, bucketNrrow);
    }
    // Remove the row from the index.
    getIndex().removeRow (rownr);
    nrrow_p--;
}


// Note that the column has already been added by makeXXColumn.
// This function is merely for initializing the added column.
void ISMBase::addColumn (DataManagerColumn* colp)
{
    for (uInt i=0; i<ncolumn(); i++) {
	if (colp == colSet_p[i]) {
	    colSet_p[i]->doCreate ((ISMBucket*)(getCache().getBucket (0)));
	    return;
	}
    }
    throw (DataManInternalError ("ISMBase::addColumn"));
}

void ISMBase::removeColumn (DataManagerColumn* colp)
{
    for (uInt i=0; i<ncolumn(); i++) {
	if (colSet_p[i] == colp) {
	    delete colSet_p[i];
	    decrementNcolumn();
	    for (; i<ncolumn(); i++) {
		colSet_p[i-1] = colSet_p[i];
	    }
	    return;
	}
    }
    throw (DataManInternalError ("ISMBase::removeColumn"));
}


void ISMBase::close (AipsIO& ios)
{
    //# Let the column objects flush themselves (if needed).
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	colSet_p[i]->flush (nrrow_p);
    }
    if (cache_p != 0) {
	cache_p->flush();
    }
    writeIndex();
    ios.putstart ("ISM", 1);
    ios << dataManName_p;
    ios.putend();
}

void ISMBase::create (uInt nrrow)
{
    init();
    file_p = new BucketFile (fileName());
    AlwaysAssert (file_p != 0, AipsError);
    index_p = new ISMIndex (this);
    AlwaysAssert (index_p != 0, AipsError);
    makeCache();
    //# Let the column objects create something if needed.
    for (uInt i=0; i<ncolumn(); i++) {
	colSet_p[i]->doCreate ((ISMBucket*)(getCache().getBucket (0)));
    }
    nrrow_p = 0;
    addRow (nrrow);
}

void ISMBase::open (uInt tabNrrow, AipsIO& ios)
{
    nrrow_p = tabNrrow;
    // Do not check the bucketsize for an existing table.
    checkBucketSize_p = False;
    ios.getstart ("ISM");
    ios >> dataManName_p;
    ios.getend();
    init();
    file_p = new BucketFile (fileName(), table().isWritable());
    AlwaysAssert (file_p != 0, AipsError);
    //# Let the column objects initialize themselves (if needed).
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	colSet_p[i]->getFile (nrrow_p);
    }
}

void ISMBase::reopenRW()
{
    file_p->setRW();
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	colSet_p[i]->reopenRW();
    }
}


void ISMBase::init()
{
    // Determine the data format (local or canonical).
    // For the moment it is always canonical (until Table supports it).
    asCanonical_p = True;
    // Determine the size of a uInt in external format.
    if (asCanonical_p) {
	uIntSize_p = ValType::getCanonicalSize (TpUInt);
    }else{
	uIntSize_p = ValType::getTypeSize (TpUInt);
    }
    // Get the total length for all columns.
    // Use 32 for each variable length element.
    // On top of that each variable length element requires uIntSize_p bytes
    // and uIntSize_p for all elements together (representing total length
    // and length per element).
    uInt fixedSize = 0;
    uInt varSize = 0;
    uInt nrcol = ncolumn();
    uInt headerSize = uIntSize_p * (nrcol + 1);        // needed per column
    for (uInt i=0; i<nrcol; i++) {
	uInt leng = colSet_p[i]->getFixedLength();
	fixedSize += 2 * uIntSize_p;                   // indices per column
	if (leng == 0) {
	    uInt nr = colSet_p[i]->nelements();
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
}
