//# BucketBase.h: Abstract base class for Bucket classes
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

#ifndef CASA_BUCKETBASE_H
#define CASA_BUCKETBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BucketFile.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Abstract base class for Bucket classes.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=BucketFile>BucketFile</linkto>
  // </prerequisite>

  // <synopsis>
  // BucketBase is the abstract baseclass for the various Bucket classes
  // like BucketMapped and BucketBuffered.
  // It is used by TSMCube to do the IO in the required way.
  // </synopsis> 


  class BucketBase
  {
  public:
    // Create the bucket access for (part of) a file.
    // The file part starts at startOffset. Its length is
    // bucketSize*nrOfBuckets bytes.
    // If the file is smaller, the remainder is indicated as an extension
    // similarly to the behaviour of function extend.
    BucketBase (BucketFile* file, Int64 startOffset, uInt bucketSize,
                uInt nrOfBuckets);

    // Detach the file. The BucketFile is not closed.
    virtual ~BucketBase();

    // Flush the cached buckets.
    // Possibly remaining uninitialized buckets will be initialized first.
    // A True status is returned if buckets had to be written.
    // The actual flushing is done using <src>doFlush</src> in the derived
    // class.
    Bool flush();

    // Resynchronize the object (after another process updated the file).
    // It remaps the file if the nr of buckets has changed.
    // the new sizes.
    virtual void resync (uInt nrBucket);

    // Get the current nr of buckets in the file.
    uInt nBucket() const
      { return itsCurNrOfBuckets; }

    // Extend the file with the given number of buckets.
    // The buckets get initialized when they are acquired
    // (using getBucket) for the first time.
    void extend (uInt nrBucket);

    // Set that data has been written.
    void setWritten()
      { itsHasWritten = True; }

protected:
    // Copy constructor is not possible.
    BucketBase (const BucketBase&);

    // Assignment is not possible.
    BucketBase& operator= (const BucketBase&);

    // Do the actual flushing.
    virtual void doFlush() = 0;

    // Do the actual resync-ing.
    virtual void doResync() = 0;

    // Do the actual extension of the file.
    // Note that itsNewNrOfBuckets has been increased before doExtend is called.
    virtual void doExtend (uInt nrBucket) = 0;

    // Initialize the bucket buffer.
    // The uninitialized buckets before this bucket are also initialized.
    virtual void initializeBuckets (uInt bucketNr) = 0;


    // The file used.
    BucketFile* itsFile;
    // The starting offsets of the buckets in the file.
    Int64   itsStartOffset;
    // The bucket size.
    uInt    itsBucketSize;
    // The current nr of buckets in the file.
    uInt    itsCurNrOfBuckets;
    // The new nr of buckets in the file (after extension).
    uInt    itsNewNrOfBuckets;
    // Have data been written?
    Bool    itsHasWritten;
};


} //# NAMESPACE CASACORE - END

#endif
