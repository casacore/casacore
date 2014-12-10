//# BucketBuffered.h: Use buffered file IO for buckets in a part of a file
//# Copyright (C) 2010
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

#ifndef CASA_BUCKETBUFFERED_H
#define CASA_BUCKETBUFFERED_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BucketBase.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations.
  class FilebufIO;


  // <summary>
  // Use buffered file IO for buckets in a part of a file
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=BucketFile>BucketFile</linkto>
  // </prerequisite>

  // <synopsis>
  // BucketBuffered is similar to class
  // <linkto class=BucketCache>BucketCache</linkto> and is meant to be used by
  // the storage managers of the Table System.
  //
  // It gives access to buckets in a file by means of memory-buffered files.
  // However, its functionality is a subset of BucketCache and is only meant
  // to be used by the Tiled Storage Managers. If The Standard and Incremental
  // Storage Manager also want to use it, functions like <src>extend</src>
  // needs to be added to this class. Also support for a free bucket list needs
  // to be added.
  // </synopsis> 

  // <motivation>
  // Use of BucketCache is sub-optimal when having large buckets and more or
  // less random IO. Memory-buffering behaves much better.
  // </motivation>


  class BucketBuffered: public BucketBase
  {
  public:
    // Create the object for (part of) a file.
    // The file part buffered into memory starts at startOffset. Its length is
    // bucketSize*nrOfBuckets bytes.
    // If the file is smaller, the remainder is indicated as an extension
    // similarly to the behaviour of function extend.
    BucketBuffered (BucketFile* file, Int64 startOffset, uInt bucketSize,
                    uInt nrOfBuckets);

    virtual ~BucketBuffered();

    // Get a pointer to the buffer.
    char* getBuffer()
      { return itsBuffer; }

    // Read the given part into the internal buffer at the given offset.
    void read (uInt bucketNr, uInt bucketOffset, uInt nbytes,
               uInt bufferOffset=0);

    // Write the given part from the internal buffer.
    void write (uInt bucketNr, uInt bucketOffset, uInt nbytes);

private:
    // Copy constructor is not possible.
    BucketBuffered (const BucketBuffered&);

    // Assignment is not possible.
    BucketBuffered& operator= (const BucketBuffered&);

    // Flush the file.
    virtual void doFlush();

    // Do the actual resync-ing.
    virtual void doResync();

    // Extend the file with the given number of buckets.
    virtual void doExtend (uInt nrBucket);

    // Initialize the bucket buffer.
    // The uninitialized buckets before this bucket are also initialized.
    virtual void initializeBuckets (uInt bucketNr);


    // Data buffer.
    char* itsBuffer;
};


} //# NAMESPACE CASACORE - END

#endif
