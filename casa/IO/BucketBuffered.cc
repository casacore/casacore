//# BucketBuffered.cc: Use buffered file IO for buckets in a part of a file
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

//# Includes
#include <casacore/casa/IO/BucketBuffered.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <cstring>   //# for memset


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  BucketBuffered::BucketBuffered (BucketFile* file, int64_t startOffset,
                                  uint32_t bucketSize, uint32_t nrOfBuckets)
    : BucketBase   (file, startOffset, bucketSize, nrOfBuckets),
      itsBuffer    (0)
  {
    AlwaysAssert (itsFile->bufferedFile() != 0, AipsError);
    // Allocate a buffer that can hold a bucket.
    itsBuffer = new char[bucketSize];
  }

  BucketBuffered::~BucketBuffered()
  {
    delete [] itsBuffer;
  }

  void BucketBuffered::read (uint32_t bucketNr, uint32_t bucketOffset, uint32_t nbytes,
                             uint32_t bufferOffset)
  {
    if (bucketNr >= itsNewNrOfBuckets) {
      throw (indexError<int32_t> (bucketNr));
    }
    itsFile->bufferedFile()->seek
      (itsStartOffset + int64_t(bucketNr)*itsBucketSize + bucketOffset);
    // When doing read/write, it can happen that not all bytes are written yet.
    // So accept it if not all bytes could be read.
    uint32_t nread = itsFile->bufferedFile()->read (nbytes, itsBuffer+bufferOffset,
                                                false);
    if (nread < nbytes) {
      memset (itsBuffer+bufferOffset+nread, 0, nbytes-nread);
    }
  }

  void BucketBuffered::write (uint32_t bucketNr, uint32_t bucketOffset, uint32_t nbytes)
  {
    if (bucketNr >= itsCurNrOfBuckets) {
      if (bucketNr >= itsNewNrOfBuckets) {
	throw (indexError<int32_t> (bucketNr));
      }
      itsCurNrOfBuckets = bucketNr+1;
    }
    itsFile->bufferedFile()->seek
      (itsStartOffset + int64_t(bucketNr)*itsBucketSize + bucketOffset);
    itsFile->bufferedFile()->write (nbytes, itsBuffer);
    setWritten();
  }

  void BucketBuffered::doFlush()
  {
    // Make sure the length is an integer nr of tiles.
    int64_t cubeLen = itsFile->bufferedFile()->length() - itsStartOffset;
    int64_t expLen = itsNewNrOfBuckets * itsBucketSize;
    if (expLen > cubeLen) {
      doExtend(0);
    }
    itsFile->bufferedFile()->flush();
  }

  void BucketBuffered::doResync()
  {}

  void BucketBuffered::doExtend (uint32_t)
  {
    // Extend the file by writing the last byte.
    itsBuffer[0] = 0;
    write (itsNewNrOfBuckets-1, itsBucketSize-1, 1);
  }

  void BucketBuffered::initializeBuckets (uint32_t bucketNr)
  {
    // Initialize this bucket and all uninitialized ones before it.
    if (itsCurNrOfBuckets <= bucketNr) {
      memset (itsBuffer, 0, itsBucketSize);
      // Writing is sequentially, so seek needs to be done only once.
      itsFile->bufferedFile()->seek
        (itsStartOffset + int64_t(itsCurNrOfBuckets)*itsBucketSize);
      while (itsCurNrOfBuckets <= bucketNr) {
        itsFile->bufferedFile()->write (itsBucketSize, itsBuffer);
        itsCurNrOfBuckets++;
      }
      setWritten();
    }
  }


} //# NAMESPACE CASACORE - END
