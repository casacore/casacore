//# BucketCache.cc: Cache for buckets in a file
//# Copyright (C) 1994,1995,1996,1997,1999,2001
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


//# Includes
#include <casacore/casa/IO/BucketCache.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

BucketCache::BucketCache (BucketFile* file, int64_t startOffset,
			  uint32_t bucketSize, uint32_t nrOfBuckets,
			  uint32_t cacheSize, void* ownerObject,
			  BucketCacheToLocal readCallBack,
			  BucketCacheFromLocal writeCallBack,
			  BucketCacheAddBuffer initCallBack,
			  BucketCacheDeleteBuffer deleteCallBack)
: its_file          (file),
  its_Owner         (ownerObject),
  its_ReadCallBack  (readCallBack),
  its_WriteCallBack (writeCallBack),
  its_InitCallBack  (initCallBack),
  its_DeleteCallBack(deleteCallBack),
  its_StartOffset   (startOffset),
  its_BucketSize    (bucketSize),
  its_CurNrOfBuckets(0),
  its_NewNrOfBuckets(nrOfBuckets),
  its_CacheSize     (cacheSize),
  its_CacheSizeUsed (0),
  its_Cache         (cacheSize, static_cast<char*>(0)),
  its_ActualSlot    (0),
  its_SlotNr        (nrOfBuckets, int32_t(-1)),
  its_BucketNr      (cacheSize, uint32_t(0)),
  its_Dirty         (cacheSize, uint32_t(0)),
  its_LRU           (cacheSize, uint32_t(0)),
  its_LRUCounter    (0),
  its_Buffer        (0),
  its_NrOfFree      (0),
  its_FirstFree     (-1)
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
    for (uint32_t i=0; i<bucketSize; i++) {
	its_Buffer[i] = 0;
    }
    // Open the file if not open yet and get its physical size.
    // Use that to determine the number of buckets in the file.
    its_file->open();
    int64_t size = its_file->fileSize();
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
    // It is not flushed (that should have been done before).
    // In that way no needless flushes are done for a temporary table.
    clear (0, false);
    delete [] its_Buffer;
}

void BucketCache::clear (uint32_t fromSlot, bool doFlush)
{
    if (doFlush) {
        flush (fromSlot);
    }
    for (uint32_t i=fromSlot; i<its_CacheSizeUsed; i++) {
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

bool BucketCache::flush (uint32_t fromSlot)
{
    // Initialize remaining buckets when everything has to be flushed.
    if (fromSlot == 0  &&  its_NewNrOfBuckets > 0) {
	initializeBuckets (its_NewNrOfBuckets - 1);
    }
    bool hasWritten = false;
    for (uint32_t i=fromSlot; i<its_CacheSizeUsed; i++) {
	if (its_Dirty[i]) {
	    writeBucket (i);
	    hasWritten = true;
	}
    }
    return hasWritten;
}

void BucketCache::resize (uint32_t cacheSize)
{
    // Clear the part of the cache to be deleted.
    clear (cacheSize);
    // The cache must contain at least one slot.
    if (cacheSize == 0) {
	cacheSize = 1;
    }
    // Exit if the cache size does not change.
    if (cacheSize == its_CacheSize) {
	return;
    }
    // Resize the cache.
    its_Cache.resize    (cacheSize);
    its_BucketNr.resize (cacheSize);
    its_LRU.resize      (cacheSize);
    its_Dirty.resize    (cacheSize);
    // Initialize the new part of the cache.
    for (uint32_t i=its_CacheSize; i<cacheSize; i++) {
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


void BucketCache::resync (uint32_t nrBucket, uint32_t nrOfFreeBucket,
			  int32_t firstFreeBucket)
{
    // Clear the entire cache, so data will be reread.
    // Set it to the new size.
    clear();
    if (nrBucket > its_NewNrOfBuckets) {
	extend (nrBucket - its_NewNrOfBuckets);
    }
    its_CurNrOfBuckets = nrBucket;
    its_NrOfFree       = nrOfFreeBucket;
    its_FirstFree      = firstFreeBucket;
}


uint32_t BucketCache::nBucket() const
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
    if (its_LRUCounter == 4294967295u) {
	its_LRUCounter = 0;
	for (uint32_t i=0; i<its_CacheSizeUsed; i++) {
	    its_LRU[i] = 0;
	}
    }
    its_LRU[its_ActualSlot] = ++its_LRUCounter;
}

char* BucketCache::getBucket (uint32_t bucketNr)
{
    if (bucketNr >= its_NewNrOfBuckets) {
	throw (indexError<int32_t> (bucketNr));
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
        if (! its_file->isWritable()) {
            throw AipsError ("BucketCache::getBucket: bucket " +
                             String::toString(bucketNr) +
                             " exceeds nr of buckets");
        }
	initializeBuckets (bucketNr);
    }
    return its_Cache[its_ActualSlot];
}

void BucketCache::extend (uint32_t nrBucket)
{
    its_NewNrOfBuckets += nrBucket;
    uint32_t oldSize = its_SlotNr.nelements();
    if (oldSize < its_NewNrOfBuckets) {
        uint32_t newSize = oldSize*2;
	if (newSize < its_NewNrOfBuckets) {
	    newSize = its_NewNrOfBuckets;
	}
	its_SlotNr.resize (newSize);
	for (uint32_t i=oldSize; i<newSize; i++) {
	    its_SlotNr[i] = -1;
	}
    }
}
    
uint32_t BucketCache::addBucket (char* data)
{
    uint32_t bucketNr;
    if (its_FirstFree >= 0) {
	// There is a free list, so get the first bucket from it.
	bucketNr = its_FirstFree;
	its_file->seek (its_StartOffset + int64_t(bucketNr) * its_BucketSize);
	its_file->read (its_Buffer,
		   CanonicalConversion::canonicalSize (static_cast<int32_t*>(0)));
	CanonicalConversion::toLocal (its_FirstFree, its_Buffer);
	its_NrOfFree--;
    }else{
	// No free buckets, so extend the file.
	// Initialize all uninitialized buckets before the newly added bucket.
	if (its_CurNrOfBuckets < its_NewNrOfBuckets) {
	    initializeBuckets (its_NewNrOfBuckets - 1);
	}
	extend (1);
	its_CurNrOfBuckets++;
	bucketNr = its_NewNrOfBuckets - 1;
    }
    getSlot (bucketNr);
    its_Cache[its_ActualSlot] = data;
    its_Dirty[its_ActualSlot] = 1;
    return bucketNr;
}

void BucketCache::removeBucket()
{
    // Removing a bucket means adding it to the beginning of the free list.
    // Thus store the bucket nr of the first free in this bucket
    // and make this bucket the first free.
    uint32_t bucketNr = its_BucketNr[its_ActualSlot];
    CanonicalConversion::fromLocal (its_Buffer, its_FirstFree);
    its_file->seek (its_StartOffset + int64_t(bucketNr) * its_BucketSize);
    its_file->write (its_Buffer, its_BucketSize);
    its_Dirty[its_ActualSlot] = 0;
    its_FirstFree = bucketNr;
    its_NrOfFree++;
    // Delete the stuff for this bucket.
    // Set the LRU to zero, so it will be reused first.
    its_DeleteCallBack (its_Owner, its_Cache[its_ActualSlot]);
    its_Cache[its_ActualSlot] = 0;
    its_SlotNr[bucketNr] = -1;
    its_LRU[its_ActualSlot] = 0;
    its_ActualSlot = 0;
}


void BucketCache::get (char* buf, uint32_t length, int64_t offset)
{
    checkOffset (length, offset);
    its_file->seek (offset);
    its_file->read (buf, length);
}
void BucketCache::put (const char* buf, uint32_t length, int64_t offset)
{
    checkOffset (length, offset);
    its_file->seek (offset);
    its_file->write (buf, length);
}
void BucketCache::checkOffset (uint32_t length, int64_t offset) const
{
    // Check if not before or after cached area.
    if (offset + length > its_StartOffset
    &&  offset < its_StartOffset + int64_t(its_CurNrOfBuckets)*its_BucketSize) {
	throw (indexError<int32_t> (offset));
    }
}


void BucketCache::getSlot (uint32_t bucketNr)
{
    if (its_CacheSizeUsed < its_CacheSize) {
	its_ActualSlot = its_CacheSizeUsed++;
    }else{
	its_ActualSlot = 0;
	uint32_t least = its_LRU[0];
	for (uint32_t i=1; i<its_CacheSizeUsed; i++) {
	    if (its_LRU[i] < least) {
		least = its_LRU[i];
		its_ActualSlot = i;
	    }
	}
	if (its_Dirty[its_ActualSlot]) {
	    writeBucket (its_ActualSlot);
	}
	if (its_Cache[its_ActualSlot] != 0) {
	    its_DeleteCallBack (its_Owner, its_Cache[its_ActualSlot]);
	    its_Cache[its_ActualSlot] = 0;
	    its_SlotNr[its_BucketNr[its_ActualSlot]] = -1;
	}
    }
    setLRU();
    its_BucketNr[its_ActualSlot] = bucketNr;
    its_SlotNr[bucketNr] = its_ActualSlot;
}


void BucketCache::writeBucket (uint32_t slotNr)
{
///    cout << "write " << its_BucketNr[slotNr] << " " << slotNr;
    its_WriteCallBack (its_Owner, its_Buffer, its_Cache[slotNr]);
    its_file->seek (its_StartOffset +
		    int64_t(its_BucketNr[slotNr]) * its_BucketSize);
    its_file->write (its_Buffer, its_BucketSize);
    its_Dirty[slotNr] = 0;
    nwrite_p++;
}
void BucketCache::readBucket (uint32_t slotNr)
{
///    cout << "read " << its_BucketNr[slotNr] << " " << slotNr;
    its_file->seek (its_StartOffset +
		    int64_t(its_BucketNr[slotNr]) * its_BucketSize);
    its_file->read (its_Buffer, its_BucketSize);
    its_Cache[slotNr] = its_ReadCallBack (its_Owner, its_Buffer);
    nread_p++;
}
void BucketCache::initializeBuckets (uint32_t bucketNr)
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
    if (its_NrOfFree > 0) {
	os << "#deleted:  " << its_NrOfFree << endl;
    }
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
	                               float(naccess_p) << "%";
    }
    cout << endl;
}

void BucketCache::initStatistics()
{
    naccess_p = 0;
    nread_p   = 0;
    ninit_p   = 0;
    nwrite_p  = 0;
}

} //# NAMESPACE CASACORE - END

