//# BucketCache.cc: Cache for buckets in a file
//# Copyright (C) 1994,1995,1996,1997
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


//# Includes
#include <aips/Tables/BucketCache.h>
#include <aips/Tables/BucketFile.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


BucketCache::BucketCache (BucketFile* file, uInt startOffset,
			  uInt bucketSize, uInt nrOfBuckets,
			  uInt cacheSize, void* ownerObject,
			  BucketCacheToLocal readCallBack,
			  BucketCacheFromLocal writeCallBack,
			  BucketCacheAddBuffer initCallBack,
			  BucketCacheDeleteBuffer deleteCallBack)
: its_file          (file),
  its_Owner         (ownerObject),
  its_ReadCallBack  (readCallBack),
  its_WriteCallBack (writeCallBack),
  its_DeleteCallBack(deleteCallBack),
  its_InitCallBack  (initCallBack),
  its_StartOffset   (startOffset),
  its_BucketSize    (bucketSize),
  its_CacheSize     (cacheSize),
  its_CurNrOfBuckets(0),
  its_NewNrOfBuckets(nrOfBuckets),
  its_CacheSizeUsed (0),
  its_ActualSlot    (0),
  its_SlotNr        (nrOfBuckets, Int(-1)),
  its_BucketNr      (cacheSize, uInt(0)),
  its_Dirty         (cacheSize, uInt(0)),
  its_LRU           (cacheSize, uInt(0)),
  its_LRUCounter    (0),
  its_Cache         (cacheSize, (char*)0),
  its_Buffer        (0)
{
    initStatistics();
    // The bucketsize must be set.
    if (bucketSize == 0) {
	throw (AipsError ("BucketCache::BucketCache; bucketsize=0"));
    }
    // A cache without slots is not possible; so give it a slot.
    if (its_CacheSize == 0) {
	resize (1);
    }
    // Allocate a buffer (for data in external format).
    // Initialize it to prevent "uninitialized memory errors" when writing.
    its_Buffer = new char[bucketSize];
    if (its_Buffer == 0) {
	throw (AllocError ("BucketCache::BucketCache", bucketSize));
    }
    for (uInt i=0; i<bucketSize; i++) {
	its_Buffer[i] = 0;
    }
    // Open the file if not open yet and get its physical size.
    // Use that to determine the number of buckets in the file.
    its_file->open();
    uInt size = its_file->fileSize();
    if (size > startOffset) {
	its_CurNrOfBuckets = (size - startOffset) / bucketSize;
	if (its_CurNrOfBuckets > its_NewNrOfBuckets) {
	    its_CurNrOfBuckets = its_NewNrOfBuckets;
	}
    }
}

BucketCache::~BucketCache()
{
    // Clear the entire cache.
    clear();
    delete [] its_Buffer;
}

void BucketCache::clear (uInt fromSlot)
{
    flush (fromSlot);
    for (uInt i=fromSlot; i<its_CacheSizeUsed; i++) {
	its_DeleteCallBack (its_Owner, its_Cache[i]);
	its_Cache[i] = 0;
	its_SlotNr[its_BucketNr[i]] = -1;
    }
    if (fromSlot == 0) {
	its_LRUCounter = 0;
	initStatistics();
    }
    if (fromSlot < its_CacheSizeUsed) {
	its_CacheSizeUsed = fromSlot;
    }
}

void BucketCache::flush (uInt fromSlot)
{
    // Initialize remaining buckets when everything is flushed.
    if (fromSlot == 0  &&  its_NewNrOfBuckets > 0) {
	initializeBuckets (its_NewNrOfBuckets - 1);
    }
    for (uInt i=fromSlot; i<its_CacheSizeUsed; i++) {
	if (its_Dirty[i]) {
	    writeBucket (i);
	}
    }
}

void BucketCache::resize (uInt cacheSize)
{
    // The cache must contain at least one slot.
    if (cacheSize == 0) {
	cacheSize = 1;
    }
    // Exit if the cache size does not change.
    if (cacheSize == its_CacheSize) {
	return;
    }
    // Clear the part of the cache to be deleted.
    // Then resize the cache.
    clear (cacheSize);
    its_Cache.resize    (cacheSize);
    its_BucketNr.resize (cacheSize);
    its_LRU.resize      (cacheSize);
    its_Dirty.resize    (cacheSize);
    // Initialize the new part of the cache.
    for (uInt i=its_CacheSize; i<cacheSize; i++) {
	its_Cache[i]    = 0;
	its_BucketNr[i] = 0;
	its_LRU[i]      = 0;
	its_Dirty[i]    = 0;
    }
    its_CacheSize = cacheSize;
    if (its_CacheSizeUsed > cacheSize) {
	its_CacheSizeUsed = cacheSize;
    }
    its_ActualSlot = 0;
}


uInt BucketCache::nBucket() const
{
    return its_NewNrOfBuckets;
}

void BucketCache::setDirty()
{
    its_Dirty[its_ActualSlot] = 1;
}


void BucketCache::setLRU()
{
    // When the LRU counter would wrap, clear all LRU info in the cache.
    if (its_LRUCounter == 4294967295) {
	its_LRUCounter = 0;
	for (uInt i=0; i<its_CacheSizeUsed; i++) {
	    its_LRU[i] = 0;
	}
    }
    its_LRU[its_ActualSlot] = ++its_LRUCounter;
}

char* BucketCache::getBucket (uInt bucketNr)
{
    if (bucketNr >= its_NewNrOfBuckets) {
	throw (indexError<Int> (Int(bucketNr)));
    }
    naccess_p++;
    // Test if it is already in the cache.
    if (its_SlotNr[bucketNr] >= 0) {
	its_ActualSlot = its_SlotNr[bucketNr];
	setLRU();
	return its_Cache[its_ActualSlot];
    }
    // Not in cache, so get a slot.
    // Read the bucket when it is already in the file.
    // Otherwise get a new initialized bucket.
    if (bucketNr < its_CurNrOfBuckets) {
	getSlot (bucketNr);
	readBucket (its_ActualSlot);
    }else{
	initializeBuckets (bucketNr);
    }
    return its_Cache[its_ActualSlot];
}

void BucketCache::extend (uInt nrBucket)
{
    its_NewNrOfBuckets += nrBucket;
    uInt oldSize = its_SlotNr.nelements();
    if (oldSize < its_NewNrOfBuckets) {
        uInt newSize = oldSize + 256;
	if (newSize < its_NewNrOfBuckets) {
	    newSize = its_NewNrOfBuckets;
	}
	its_SlotNr.resize (newSize);
	for (uInt i=oldSize; i<newSize; i++) {
	    its_SlotNr[i] = -1;
	}
    }
}
    
void BucketCache::addBucket (char* data)
{
    // Initialize all uninitialized buckets before the newly added bucket.
    if (its_CurNrOfBuckets < its_NewNrOfBuckets) {
	initializeBuckets (its_NewNrOfBuckets - 1);
    }
    extend (1);
    its_CurNrOfBuckets++;
    getSlot (its_NewNrOfBuckets - 1);
    its_Cache[its_ActualSlot] = data;
    its_Dirty[its_ActualSlot] = 1;
}


void BucketCache::get (char* buf, uInt length, uInt offset)
{
    checkOffset (length, offset);
    its_file->seek (offset);
    its_file->read (buf, length);
}
void BucketCache::put (const char* buf, uInt length, uInt offset)
{
    checkOffset (length, offset);
    its_file->seek (offset);
    its_file->write (buf, length);
}
void BucketCache::checkOffset (uInt length, uInt offset) const
{
    // Check if not before or after cached area.
    if (offset + length > its_StartOffset
    &&  offset < its_StartOffset + its_CurNrOfBuckets * its_BucketSize) {
	throw (indexError<Int> (Int(offset)));
    }
}


void BucketCache::getSlot (uInt bucketNr)
{
    if (its_CacheSizeUsed < its_CacheSize) {
	its_ActualSlot = its_CacheSizeUsed++;
    }else{
	its_ActualSlot = 0;
	uInt least = its_LRU[0];
	for (uInt i=1; i<its_CacheSizeUsed; i++) {
	    if (its_LRU[i] < least) {
		least = its_LRU[i];
		its_ActualSlot = i;
	    }
	}
	if (its_Dirty[its_ActualSlot]) {
	    writeBucket (its_ActualSlot);
	}
	its_DeleteCallBack (its_Owner, its_Cache[its_ActualSlot]);
	its_Cache[its_ActualSlot] = 0;
	its_SlotNr[its_BucketNr[its_ActualSlot]] = -1;
    }
    setLRU();
    its_BucketNr[its_ActualSlot] = bucketNr;
    its_SlotNr[bucketNr] = its_ActualSlot;
}


void BucketCache::writeBucket (uInt slotNr)
{
///    cout << "write " << its_BucketNr[slotNr] << " " << slotNr;
    its_WriteCallBack (its_Owner, its_Buffer, its_Cache[slotNr]);
    its_file->seek (its_StartOffset + its_BucketNr[slotNr] * its_BucketSize);
    its_file->write (its_Buffer, its_BucketSize);
    its_Dirty[slotNr] = 0;
    nwrite_p++;
}
void BucketCache::readBucket (uInt slotNr)
{
///    cout << "read " << its_BucketNr[slotNr] << " " << slotNr;
    its_file->seek (its_StartOffset + its_BucketNr[slotNr] * its_BucketSize);
    its_file->read (its_Buffer, its_BucketSize);
    its_Cache[slotNr] = its_ReadCallBack (its_Owner, its_Buffer);
    nread_p++;
}
void BucketCache::initializeBuckets (uInt bucketNr)
{
    // Initialize this bucket and all uninitialized ones before it.
    while (its_CurNrOfBuckets <= bucketNr) {
	getSlot (its_CurNrOfBuckets);
///	cout << "init " << its_CurNrOfBuckets << " " << its_ActualSlot;
	its_Cache[its_ActualSlot] = its_InitCallBack (its_Owner);
	its_Dirty[its_ActualSlot] = 1;
	its_CurNrOfBuckets++;
	ninit_p++;
    }
}


void BucketCache::showStatistics (ostream& os) const
{
    os << "cacheSize: " << its_CacheSize << " (*" << its_BucketSize
       << ")" << endl;
    os << "#buckets:  " << its_CurNrOfBuckets;
    if (nread_p+nwrite_p > its_CurNrOfBuckets) {
	os << "         (<  #reads + #writes!)";
    }
    os << endl;
    if (nread_p > 0) {
	os << "#reads:    " << nread_p << endl;
    }
    if (ninit_p > 0) {
	os << "#inits:    " << ninit_p << endl;
    }
    if (nwrite_p > 0) {
	os << "#writes:   " << nwrite_p << endl;
    }
    os << "#accesses: " << naccess_p;
    if (naccess_p > 0) {
	os << "        hit-rate:  "
	   << 100 * float(naccess_p - nread_p - ninit_p) /
	                               float(naccess_p) << "%" << endl;
    }
}

void BucketCache::initStatistics()
{
    naccess_p = 0;
    nread_p   = 0;
    ninit_p   = 0;
    nwrite_p  = 0;
}
