//# BucketMapped.h: File buckets by means of file mapping
//# Copyright (C) 2009
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

#ifndef CASA_BUCKETMAPPED_H
#define CASA_BUCKETMAPPED_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BucketBase.h>
#include <casacore/casa/IO/MMapfdIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Use file mapping for buckets in a part of a file
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=BucketFile>BucketFile</linkto>
  // </prerequisite>

  // <etymology>
  // BucketMapped uses memory-mapped files for bucket access.
  // </etymology>

  // <synopsis>
  // BucketMapped is similar to class
  // <linkto class=BucketCache>BucketCache</linkto> and is meant to be used by
  // the storage managers of the Table System.
  //
  // It gives access to buckets in a file by means of memory-mapped files.
  // However, its functionality is a subset of BucketCache and is only meant
  // to be used by the Tiled Storage Managers. If The Standard and Incremental
  // Storage Manager also want to use it, functions like <src>extend</src>
  // needs to be added to this class. Also support for a free bucket list needs
  // to be added.
  // </synopsis> 

  // <motivation>
  // Use of BucketCache is sub-optimal when having large buckets and more or
  // less random IO. Memory-mapping behaves much better.
  // </motivation>


  class BucketMapped: public BucketBase
  {
  public:
    // Create the cache for (part of) a file.
    // The file part mapped into memory starts at startOffset. Its length is
    // bucketSize*nrOfBuckets bytes.
    // If the file is smaller, the remainder is indicated as an extension
    // similarly to the behaviour of function extend.
    BucketMapped (BucketFile* file, Int64 startOffset, uInt bucketSize,
                  uInt nrOfBuckets);

    // Unmap the file
    ~BucketMapped();

    // Get a readonly pointer to the given bucket in memory.
    const char* getBucket (uInt bucketNr);

    // Get a writable pointer to the given bucket in memory.
    // It sets the hasWritten flag.
    char* getrwBucket (uInt bucketNr)
    {
      itsHasWritten = True;
      return const_cast<char*>(getBucket(bucketNr));
    }

private:
    // Copy constructor is not possible.
    BucketMapped (const BucketMapped&);

    // Assignment is not possible.
    BucketMapped& operator= (const BucketMapped&);

    // Flush the file.
    virtual void doFlush();

    // Do the actual resync-ing.
    virtual void doResync();

    // Extend the file with the given number of buckets.
    virtual void doExtend (uInt nrBucket);

    // Initialize the bucket buffer.
    // The uninitialized buckets before this bucket are also initialized.
    virtual void initializeBuckets (uInt bucketNr);
  };


} //# NAMESPACE CASACORE - END

#endif
