//# BucketBase.cc: Abstract base class for Bucket classes
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

//# Includes
#include <casacore/casa/IO/BucketBase.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  BucketBase::BucketBase (BucketFile* file, Int64 startOffset,
                          uInt bucketSize, uInt nrOfBuckets)
    : itsFile           (file),
      itsStartOffset    (startOffset),
      itsBucketSize     (bucketSize),
      itsCurNrOfBuckets (0),
      itsNewNrOfBuckets (nrOfBuckets),
      itsHasWritten     (False)
  {
    // The bucketsize must be set.
    if (bucketSize == 0) {
      throw AipsError ("BucketBase::BucketBase; bucketsize=0");
    }
    // Open the file if not open yet and get its physical size.
    // Use that to determine the number of buckets in the file.
    itsFile->open();
    Int64 size = itsFile->fileSize();
    if (size > startOffset) {
      itsCurNrOfBuckets = (size - startOffset) / bucketSize;
      if (itsCurNrOfBuckets > itsNewNrOfBuckets) {
        itsCurNrOfBuckets = itsNewNrOfBuckets;
      }
    }
  }

  BucketBase::~BucketBase()
  {}

  Bool BucketBase::flush()
  {
    if (itsNewNrOfBuckets > 0) {
      initializeBuckets (itsNewNrOfBuckets - 1);
    }
    if (itsHasWritten) {
      doFlush();
      itsHasWritten = False;
      return True;
    }
    return False;
  }

  void BucketBase::resync (uInt nrBucket)
  {
    // Remap the file (if extended).
    if (nrBucket > itsNewNrOfBuckets) {
      doResync();
      itsNewNrOfBuckets = nrBucket;
    }
    itsCurNrOfBuckets = nrBucket;
  }

  void BucketBase::extend (uInt nrBucket)
  {
    // Extend the file by writing the last byte.
    if (nrBucket > 0) {
      itsNewNrOfBuckets += nrBucket;
      doExtend (nrBucket);
      itsHasWritten = True;
    }
  }


} //# NAMESPACE CASACORE - END
