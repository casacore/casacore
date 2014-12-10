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
//#
//# $Id$

//# Includes
#include <casacore/casa/IO/BucketBuffered.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <cstring>   //# for memset


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  BucketBuffered::BucketBuffered (BucketFile* file, Int64 startOffset,
                                  uInt bucketSize, uInt nrOfBuckets)
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

  void BucketBuffered::read (uInt bucketNr, uInt bucketOffset, uInt nbytes,
                             uInt bufferOffset)
  {
    if (bucketNr >= itsNewNrOfBuckets) {
      throw (indexError<Int> (bucketNr));
    }
    itsFile->bufferedFile()->seek
      (itsStartOffset + Int64(bucketNr)*itsBucketSize + bucketOffset);
    // When doing read/write, it can happen that not all bytes are written yet.
    // So accept it if not all bytes could be read.
    uInt nread = itsFile->bufferedFile()->read (nbytes, itsBuffer+bufferOffset,
                                                False);
    if (nread < nbytes) {
      memset (itsBuffer+bufferOffset+nread, 0, nbytes-nread);
    }
  }

  void BucketBuffered::write (uInt bucketNr, uInt bucketOffset, uInt nbytes)
  {
    if (bucketNr >= itsCurNrOfBuckets) {
      if (bucketNr >= itsNewNrOfBuckets) {
	throw (indexError<Int> (bucketNr));
      }
      itsCurNrOfBuckets = bucketNr+1;
    }
    itsFile->bufferedFile()->seek
      (itsStartOffset + Int64(bucketNr)*itsBucketSize + bucketOffset);
    itsFile->bufferedFile()->write (nbytes, itsBuffer);
    setWritten();
  }

  void BucketBuffered::doFlush()
  {
    // Make sure the length is an integer nr of tiles.
    Int64 cubeLen = itsFile->bufferedFile()->length() - itsStartOffset;
    Int64 expLen = itsNewNrOfBuckets * itsBucketSize;
    if (expLen > cubeLen) {
      doExtend(0);
    }
    itsFile->bufferedFile()->flush();
  }

  void BucketBuffered::doResync()
  {}

  void BucketBuffered::doExtend (uInt)
  {
    // Extend the file by writing the last byte.
    itsBuffer[0] = 0;
    write (itsNewNrOfBuckets-1, itsBucketSize-1, 1);
  }

  void BucketBuffered::initializeBuckets (uInt bucketNr)
  {
    // Initialize this bucket and all uninitialized ones before it.
    if (itsCurNrOfBuckets <= bucketNr) {
      memset (itsBuffer, 0, itsBucketSize);
      // Writing is sequentially, so seek needs to be done only once.
      itsFile->bufferedFile()->seek
        (itsStartOffset + Int64(itsCurNrOfBuckets)*itsBucketSize);
      while (itsCurNrOfBuckets <= bucketNr) {
        itsFile->bufferedFile()->write (itsBucketSize, itsBuffer);
        itsCurNrOfBuckets++;
      }
      setWritten();
    }
  }


} //# NAMESPACE CASACORE - END
