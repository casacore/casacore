//# BucketCache.cc: File buckets by means of file mapping
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

//# Includes
#include <casacore/casa/IO/BucketMapped.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <cstring>   //# for memset

namespace casacore { //# NAMESPACE CASACORE - BEGIN


  BucketMapped::BucketMapped (BucketFile* file, Int64 startOffset,
                              uInt bucketSize, uInt nrOfBuckets)
    : BucketBase (file, startOffset, bucketSize, nrOfBuckets)
  {
    AlwaysAssert (itsFile->mappedFile() != 0, AipsError);
  }

  BucketMapped::~BucketMapped()
  {}

  void BucketMapped::doResync()
  {
    ////itsFile->mappedFile()->resync();
  }

  void BucketMapped::doFlush()
  {
    itsFile->mappedFile()->flush();
  }

  void BucketMapped::doExtend (uInt)
  {
    // Extend the file by writing the last byte.
    char ch=0;
    itsFile->mappedFile()->seek (itsStartOffset + 
                                 Int64(itsNewNrOfBuckets)*itsBucketSize - 1);
    itsFile->mappedFile()->write (1, &ch);
  }

  const char* BucketMapped::getBucket (uInt bucketNr)
  {
    if (bucketNr >= itsCurNrOfBuckets) {
      if (bucketNr >= itsNewNrOfBuckets) {
	throw (indexError<Int> (bucketNr));
      }
      initializeBuckets (bucketNr);
    }
    return static_cast<const char*>(itsFile->mappedFile()->getReadPointer
      (itsStartOffset + Int64(bucketNr)*itsBucketSize));
  }

  void BucketMapped::initializeBuckets (uInt bucketNr)
  {
    if (itsCurNrOfBuckets <= bucketNr) {
      doExtend (0);
      // Initialize this bucket and all uninitialized ones before it.
      while (itsCurNrOfBuckets <= bucketNr) {
        char* data = static_cast<char*>(itsFile->mappedFile()->getWritePointer
                   (itsStartOffset + Int64(itsCurNrOfBuckets)*itsBucketSize));
        memset (data, 0, itsBucketSize);
        itsCurNrOfBuckets++;
        setWritten();
      }
    }
  }


} //# NAMESPACE CASACORE - END
