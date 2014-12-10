//# BucketCache.h: Cache for buckets in a part of a file
//# Copyright (C) 1994,1995,1996,1999,2000,2001
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

#ifndef CASA_BUCKETCACHE_H
#define CASA_BUCKETCACHE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/OS/CanonicalConversion.h>

//# Forward clarations
#include <casacore/casa/iosfwd.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Define the type of the static read and write function.
// </summary>
// <use visibility=export>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <synopsis>
// The BucketCache class needs a way to convert its data from local
// to canonical format and vice-versa. This is done by callback
// functions defined at construction time.
// <p>
// The ToLocal callback function has to allocate a buffer of the correct
// size and to copy/convert the canonical data in the input buffer to
// this buffer. The pointer this newly allocated buffer has to be returned.
// The BucketCache class keeps this pointer in the cache block.
// <p>
// The FromLocal callback function has to copy/convert the data from the
// buffer in local format to the buffer in canonical format. It should
// NOT delete the buffer; that has to be done by the DeleteBuffer function.
// <p>
// The AddBuffer callback function has to create (and initialize) a
// buffer to be added to the file and cache.
// When the file gets extended, BucketCache only registers the new size,
// but does not werite anything. When a bucket is read between the
// actual file size and the new file size, the AddBuffer callback function
// is called to create a buffer and possibly initialize it.
// <p>
// The DeleteBuffer callback function has to delete the buffer
// allocated by the ToLocal function.
// <p>
// The functions get a pointer to the owner object, which was provided
// at construction time. The callback function has to cast this to the
// correct type and can use it thereafter.
// <br>
// C++ supports pointers to members, but it is a bit hard. Therefore pointers
// to static members are used (which are simple pointers to functions).
// A pointer to the owner object is also passed to let the static function
// call the correct member function (when needed).
// </synopsis>
//
// <example>
// See class <linkto class=BucketCache>BucketCache</linkto>.
// </example>

// <group name=BucketCache_CallBack>
typedef char* (*BucketCacheToLocal) (void* ownerObject, const char* canonical);
typedef void (*BucketCacheFromLocal) (void* ownerObject, char* canonical,
				      const char* local);
typedef char* (*BucketCacheAddBuffer) (void* ownerObject);
typedef void (*BucketCacheDeleteBuffer) (void* ownerObject, char* buffer);
// </group>



// <summary>
// Cache for buckets in a part of a file
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=BucketFile>BucketFile</linkto>
// </prerequisite>

// <etymology>
// BucketCache implements a cache for buckets in (a part of) a file.
// </etymology>

// <synopsis> 
// A cache may allow more efficient quasi-random IO.
// It can, for instance, be used when a limited number of blocks
// in a file have to be accessed again and again.
// <p>
// The class BucketCache provides such a cache. It can be used on a
// consecutive part of a file as long as that part is not simultaneously
// accessed in another way (including another BucketCache object).
// <p>
// BucketCache stores the data as given.
// It uses <linkto group=BucketCache_CallBack>callback functions</linkto>
// to allocate/delete buffers and to convert the data to/from local format.
// <p>
// When a new bucket is needed and all slots in the cache are used,
// BucketCache will remove the least recently used bucket from the
// cache. When the dirty flag is set, it will first be written.
// <p>
// BucketCache maintains a list of free buckets. Initially this list is
// empty. When a bucket is removed, it is added to the free list.
// AddBucket will take buckets from the free list before extending the file.
// <p>
// Since it is possible to handle only a part of a file by a BucketCache
// object, it is also possible to have multiple BucketCache objects on
// the same file (as long as they access disjoint parts of the file).
// Each BucketCache object can have its own bucket size. This can,
// for example, be used to have tiled arrays with different tile shapes
// in the same file.
// <p>
// Statistics are kept to know how efficient the cache is working.
// It is possible to initialize and show the statistics.
// </synopsis> 

// <motivation>
// A cache may reduce IO traffix considerably.
// Furthermore it is more efficient to keep a cache in local format.
// In that way conversion to/from local only have to be done when
// data gets read/written. It also allows for precalculations.
// </motivation>

// <example>
// <srcblock>
//  // Define the callback function for reading a bucket.
//  char* bToLocal (void*, const char* data)
//  {
//    char* ptr = new char[32768];
//    memcpy (ptr, data, 32768);
//    return ptr;
//  }
//  // Define the callback function for writing a bucket.
//  void bFromLocal (void*, char* data, const char* local)
//  {
//    memcpy (data, local, 32768);
//  }
//  // Define the callback function for initializing a new bucket.
//  char* bAddBuffer (void*)
//  {
//    char* ptr = new char[32768];
//    for (uInt i=0; i++; i<32768) {
//	ptr[i] = 0;
//    }
//    return ptr;
//  }
//  // Define the callback function for deleting a bucket.
//  void bDeleteBuffer (void*, char* buffer)
//  {
//    delete [] buffer;
//  }
//
//  void someFunc()
//  {
//    // Open the filebuf.
//    BucketFile file(...);
//    file.open();
//    uInt i;
//    // Create a cache for the part of the file starting at offset 512
//    // consisting of 1000 buckets. The cache consists of 10 buckets.
//    // Each bucket is 32768 bytes.
//    BucketCache cache (&file, 512, 32768, 1000, 10, 0,
//                       bToLocal, bFromLocal, bAddBuffer, bDeleteBuffer);
//    // Write all buckets into the file.
//    for (i=0; i<100; i++) {
//      char* buf = new char[32768];
//      cache.addBucket (buf);
//    }
//    Flush the cache to write all buckets in it.
//    cache.flush();
//    // Read all buckets from the file.
//    for (i=0; i<1000; i++) {
//      char* buf = cache.getBucket(i);
//      ...
//    }
//    cout << cache.nBucket() << endl;
//  }
// </srcblock>
// </example>

// <todo asof="$DATE:$">
//   <li> When ready, use HashMap for the internal maps.
// </todo>


class BucketCache
{
public:

    // Create the cache for (a part of) a file.
    // The file part used starts at startOffset. Its length is
    // bucketSize*nrOfBuckets bytes.
    // When the file is smaller, the remainder is indicated as an extension
    // similarly to the behaviour of function extend.
    BucketCache (BucketFile* file, Int64 startOffset, uInt bucketSize,
		 uInt nrOfBuckets, uInt cacheSize,
		 void* ownerObject,
		 BucketCacheToLocal readCallBack,
		 BucketCacheFromLocal writeCallBack,
		 BucketCacheAddBuffer addCallBack,
		 BucketCacheDeleteBuffer deleteCallBack);

    ~BucketCache();

    // Flush the cache from the given slot on.
    // By default the entire cache is flushed.
    // When the entire cache is flushed, possible remaining uninitialized
    // buckets will be initialized first.
    // A True status is returned when buckets had to be written.
    Bool flush (uInt fromSlot = 0);

    // Clear the cache from the given slot on.
    // By default the entire cache is cleared.
    // It will remove the buckets in the cleared part.
    // If wanted and needed, the buckets are flushed to the file
    // before removing them.
    // It can be used to enforce rereading buckets from the file.
    void clear (uInt fromSlot = 0, Bool doFlush = True);

    // Resize the cache.
    // When the cache gets smaller, the latter buckets are cached out.
    // It does not take "least recently used" into account.
    void resize (uInt cacheSize);

    // Resynchronize the object (after another process updated the file).
    // It clears the cache (so all data will be reread) and sets
    // the new sizes.
    void resync (uInt nrBucket, uInt nrOfFreeBucket, Int firstFreeBucket);

    // Get the current nr of buckets in the file.
    uInt nBucket() const;

    // Get the current cache size (in buckets).
    uInt cacheSize() const;

    // Set the dirty bit for the current bucket.
    void setDirty();

    // Make another bucket current.
    // When no more cache slots are available, the one least recently
    // used is flushed.
    // The data in the bucket is converted using the ToLocal callback
    // function. When the bucket does not exist yet in the file, it
    // gets added and initialized using the AddBuffer callback function.
    // A pointer to the data in converted format is returned.
    char* getBucket (uInt bucketNr);

    // Extend the file with the given number of buckets.
    // The buckets get initialized when they are acquired
    // (using getBucket) for the first time.
    void extend (uInt nrBucket);

    // Add a bucket to the file and make it the current one.
    // When no more cache slots are available, the one least recently
    // used is flushed.
    // <br> When no free buckets are available, the file will be
    // extended with one bucket. It returns the new bucket number.
    // The buffer must have been allocated on the heap.
    // It will get part of the cache; its contents are not copied.
    // Thus the buffer should hereafter NOT be used for other purposes.
    // It will be deleted later via the DeleteBuffer callback function.
    // The data is copied into the bucket. A pointer to the data in
    // local format is returned.
    uInt addBucket (char* data);

    // Remove the current bucket; i.e. add it to the beginning of the
    // free bucket list.
    void removeBucket();

    // Get a part from the file outside the cached area.
    // It is checked if that part is indeed outside the cached file area.
    void get (char* buf, uInt length, Int64 offset);

    // Put a part from the file outside the cached area.
    // It is checked if that part is indeed outside the cached file area.
    void put (const char* buf, uInt length, Int64 offset);

    // Get the bucket number of the first free bucket.
    // -1 = no free buckets.
    Int firstFreeBucket() const;

    // Get the number of free buckets.
    uInt nFreeBucket() const;

    // (Re)initialize the cache statistics.
    void initStatistics();

    // Show the statistics.
    void showStatistics (ostream& os) const;

private:
    // The file used.
    BucketFile* its_file;
    // The owner object.
    void*    its_Owner;
    // The read callback function.
    BucketCacheToLocal   its_ReadCallBack;
    // The write callback function.
    BucketCacheFromLocal its_WriteCallBack;
    // The add bucket callback function.
    BucketCacheAddBuffer its_InitCallBack;
    // The delete callback function.
    BucketCacheDeleteBuffer its_DeleteCallBack;
    // The starting offsets of the buckets in the file.
    Int64    its_StartOffset;
    // The bucket size.
    uInt     its_BucketSize;
    // The current nr of buckets in the file.
    uInt     its_CurNrOfBuckets;
    // The new nr of buckets in the file (after extension).
    uInt     its_NewNrOfBuckets;
    // The size of the cache (i.e. #buckets fitting in it).
    uInt     its_CacheSize;
    // The nr of slots used in the cache.
    uInt     its_CacheSizeUsed;
    // The cache itself.
    PtrBlock<char*> its_Cache; 
    // The cache slot actually used.
    uInt         its_ActualSlot;
    // The slot numbers of the buckets in the cache (-1 = not in cache).
    Block<Int>   its_SlotNr;
    // The buckets in the cache.
    Block<uInt>  its_BucketNr;
    // Determine if a block is dirty (i.e. changed) (1=dirty).
    Block<uInt>  its_Dirty;
    // Determine when a block is used for the last time.
    Block<uInt>  its_LRU;
    // The Least Recently Used counter.
    uInt         its_LRUCounter;
    // The internal buffer.
    char*        its_Buffer;
    // The number of free buckets.
    uInt its_NrOfFree;
    // The first free bucket (-1 = no free buckets).
    Int  its_FirstFree;
    // The statistics.
    uInt naccess_p;
    uInt nread_p;
    uInt ninit_p;
    uInt nwrite_p;


    // Copy constructor is not possible.
    BucketCache (const BucketCache&);

    // Assignment is not possible.
    BucketCache& operator= (const BucketCache&);

    // Set the LRU information for the current slot.
    void setLRU();

    // Get a cache slot for the bucket.
    void getSlot (uInt bucketNr);

    // Write a bucket.
    void writeBucket (uInt slotNr);

    // Read a bucket.
    void readBucket (uInt slotNr);

    // Initialize the bucket buffer.
    // The uninitialized buckets before this bucket are also initialized.
    // It returns a pointer to the buffer.
    void initializeBuckets (uInt bucketNr);

    // Check if the offset of a non-cached part is correct.
    void checkOffset (uInt length, Int64 offset) const;
};



inline uInt BucketCache::cacheSize() const
    { return its_CacheSize; }

inline Int BucketCache::firstFreeBucket() const
    { return its_FirstFree; }

inline uInt BucketCache::nFreeBucket() const
    { return its_NrOfFree; }




} //# NAMESPACE CASACORE - END

#endif
