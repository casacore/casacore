//# SSMStringHandler.cc: Store Strings in the Standard Storage Manager
//# Copyright (C) 2000,2001,2002
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
 

#include <casacore/tables/DataMan/SSMStringHandler.h>
#include <casacore/tables/DataMan/SSMBase.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SSMStringHandler::SSMStringHandler (SSMBase* aBase):
  itsSSMPtr          (aBase),
  itsCurrentBucket   (-1),
  itsLength          (0),
  itsNDeleted        (0),
  itsUsedLength      (0),
  itsNextBucket      (-1),
  itsData            (0),
  itsIntBuf          (0),
  isChanged          (false),
  itsLastBucket      (-1)
{
}

SSMStringHandler::~SSMStringHandler()
{
  delete [] itsData;
  delete [] itsIntBuf;
}

void SSMStringHandler::init()
{
  delete [] itsData;
  itsData = 0;
  delete [] itsIntBuf;
  itsIntBuf = 0;

  itsIntSize = CanonicalConversion::canonicalSize(&itsUsedLength);
  itsStart   = 4*itsIntSize;
  itsLength  = itsSSMPtr->getBucketSize()-itsStart;
  itsData    = new char[itsLength];
  itsIntBuf  = new char[itsIntSize];

  memset(itsData,0,itsLength);
  memset(itsIntBuf,0,itsIntSize);
}

void SSMStringHandler::replace(int32_t bucketNr, int32_t offset, int32_t length, 
			       const String& string)
{
  // Check if current bucket is wanted bucket, else get wanted bucket.
  if (bucketNr != itsCurrentBucket) {
    getBucket(bucketNr);
  }

  replaceData(offset,string.length(),string.chars());

  // remove additional space left (if any)
  if (length - string.length() > 0) {
    remove (itsCurrentBucket, offset, length-string.length());
  }
}

void SSMStringHandler::replace(int32_t bucketNr, int32_t offset, int32_t length, 
			       int32_t totalLength, const Array<String>& string,
			       bool handleShape)
{
  const IPosition& aShape = string.shape();

  // Check if current bucket is wanted bucket, else get wanted bucket.
  if (bucketNr != itsCurrentBucket) {
    getBucket(bucketNr);
  }

  bool deleteIt;
  const String *aString = string.getStorage(deleteIt);

  if (handleShape) {
    CanonicalConversion::fromLocal (itsIntBuf, (uint32_t) aShape.nelements());
    replaceData (offset,itsIntSize, itsIntBuf);

    for (uint32_t i=0; i< aShape.nelements();i++) {
      CanonicalConversion::fromLocal (itsIntBuf, int32_t(aShape(i)));
      replaceData (offset,itsIntSize, itsIntBuf);
    }
    CanonicalConversion::fromLocal (itsIntBuf,1);
    replaceData (offset,itsIntSize, itsIntBuf);
  }

  for (uint32_t i=0;i<string.nelements();i++) {
      //
      // Made it a uint32_t so the SGI compiler could figure out which overloaded
      // function to use, since it seemed confused by string::size_t -> size_t
      //
    CanonicalConversion::fromLocal (itsIntBuf, uint32_t(aString[i].length()));
    replaceData (offset,itsIntSize, itsIntBuf);
    replaceData (offset,aString[i].length(), aString[i].chars());
  }

  string.freeStorage(aString,deleteIt);

  // remove additional space left (if any)
  if (length - totalLength > 0) {
    remove (itsCurrentBucket, offset, length-totalLength);
  }
}

void SSMStringHandler::replace(int32_t bucketNr, int32_t offset, int32_t length, 
			       int32_t totalLength, const IPosition& aShape)
{
  // Check if current bucket is wanted bucket, else get wanted bucket.
  if (bucketNr != itsCurrentBucket) {
    getBucket(bucketNr);
  }

  CanonicalConversion::fromLocal (itsIntBuf, uint32_t(aShape.nelements()));
  replaceData (offset,itsIntSize, itsIntBuf);

  for (uint32_t i=0; i< aShape.nelements();i++) {
    CanonicalConversion::fromLocal (itsIntBuf, int32_t(aShape(i)));
    replaceData (offset,itsIntSize, itsIntBuf);
  }

  // write 'empty' flag
  CanonicalConversion::fromLocal (itsIntBuf,0);
  replaceData (offset,itsIntSize, itsIntBuf);
  

  // remove additional space left (if any)
  if (length - totalLength > 0) {
    remove (itsCurrentBucket, offset, length-totalLength);
  }
}

void SSMStringHandler::replaceData (int32_t& offset,int32_t length, 
				    const char* data)
{
  while (length > 0) {
    int32_t nCopy = length;
    if (length > itsLength-offset) {
      nCopy = itsLength-offset;
    }
    length -= nCopy;
    memcpy (itsData+offset, data, nCopy);
    data += nCopy;
    offset += nCopy;
    isChanged=true;
    if (length > 0) {
      offset = 0;
      getBucket (itsNextBucket);
    }
  }
}


void SSMStringHandler::put (int32_t& bucketNr, int32_t& offset, int32_t& length, 
			    const String& string)
{
  if (length > 0) {
    if (static_cast<int32_t>(string.length()) > length  ||  string.length() == 0) {
      remove (bucketNr, offset, length);
      bucketNr = 0;
      offset = 0;
      length = 0;
    }
  }

  if (string.length() == 0) {
    return;
  }
  
  if (length > 0) {
    if (itsCurrentBucket != bucketNr) {
      getBucket(bucketNr);
    }
    replace(bucketNr, offset, length, string);
    length   = string.length();
    return;
  }

  if (itsLastBucket == -1) {
    getNewBucket(false);
  } else if (itsCurrentBucket != itsLastBucket) {
    getBucket(itsLastBucket);
  }

  
  // if Bucket available but string doesn't fit and space < 50 get 
  // a new bucket anyway.

  if (static_cast<int32_t>(string.length()) > itsLength-itsUsedLength &&
      itsLength-itsUsedLength < 50 ) { 
    getNewBucket(false);
  }

  offset   = itsUsedLength;
  bucketNr = itsCurrentBucket;
  length   = string.length();
  putData (length, string.chars());
}

void SSMStringHandler::put (int32_t& bucketNr, int32_t& offset, int32_t& length, 
			    const Array<String>& string, bool handleShape)
{
  const IPosition& aShape = string.shape();
  int32_t totalLength=0;
  bool deleteIt;
  const String *aString = string.getStorage(deleteIt);
  
  for (uint32_t i=0;i<string.nelements();i++) {
    totalLength += aString[i].length()+itsIntSize;
  }

  if (handleShape) {
    totalLength += (string.ndim()+2)*ValType::getCanonicalSize (TpInt);
  }

  if (length > 0) {
    if (totalLength > length  ||  totalLength == 0) {
      remove (bucketNr, offset, length);
      bucketNr = 0;
      offset = 0;
      length = 0;
    }
  }

  if (totalLength == 0) {
    string.freeStorage(aString,deleteIt);
    return;
  }
  
  if (length > 0) {
    if (itsCurrentBucket != bucketNr) {
      getBucket(bucketNr);
    }
    replace(bucketNr, offset, length, totalLength,string,handleShape);
    length = totalLength;
    string.freeStorage(aString,deleteIt);
    return;
  }

  if (itsLastBucket == -1) {
    getNewBucket(false);
  } else if (itsCurrentBucket != itsLastBucket) {
    getBucket(itsLastBucket);
  }

  

  // if Bucket available but string doesn't fit and space < 50 get 
  // a new bucket anyway.

  if (totalLength > itsLength-itsUsedLength &&
      itsLength-itsUsedLength < 50 ) { 
    getNewBucket(false);
  }
  
  bucketNr=itsCurrentBucket;
  offset= itsUsedLength;
  length= totalLength;

  if (handleShape) {
    CanonicalConversion::fromLocal (itsIntBuf, uint32_t(aShape.nelements()));
    putData (itsIntSize, itsIntBuf);

    for (uint32_t i=0; i< string.ndim();i++) {
      CanonicalConversion::fromLocal (itsIntBuf, int32_t(aShape(i)));
      putData (itsIntSize, itsIntBuf);
    }
    CanonicalConversion::fromLocal (itsIntBuf,1);
    putData (itsIntSize, itsIntBuf);
  }

  for (uint32_t i=0; i< string.nelements();i++) {
      //
      // Made it a uint32_t so the SGI compiler could figure out which overloaded
      // function to use, since it seemed confused by string::size_t -> size_t
      //
    CanonicalConversion::fromLocal (itsIntBuf, uint32_t(aString[i].length()));
    putData (itsIntSize, itsIntBuf);
    putData (aString[i].length(), aString[i].chars());
  }
  string.freeStorage(aString,deleteIt);
}

void SSMStringHandler::putData (int32_t length, const char* data)
{
  while (length > 0) {
    int32_t toDo = length;
    if (toDo > itsLength-itsUsedLength ) {
      toDo = itsLength-itsUsedLength;
    }
    length-=toDo;
    memcpy (itsData+itsUsedLength, data, toDo);
    data += toDo;
    itsNDeleted -= toDo;
    itsUsedLength += toDo;
    isChanged=true;
    if (length > 0) {
      getNewBucket(true);
    }
  }
}
  
void SSMStringHandler::getData (int32_t length, char* data,int32_t& offset)
{
  while (length > 0) {
    int32_t nCopy = itsUsedLength-offset;
    if (length < nCopy) {
      nCopy = length;
    }
    length -= nCopy;
    memcpy (data,itsData+offset, nCopy);
    data += nCopy;
    offset += nCopy;
    if (length > 0) {
      getBucket(itsNextBucket);
      offset=0;
    }
  }
}
  
void SSMStringHandler::remove (int32_t bucketNr, int32_t offset, int32_t length)
{
  if (itsCurrentBucket != bucketNr) {
    getBucket(bucketNr);
  }

  int32_t n = itsLength-offset;
  if (length < n) {
    n = length;
  }
  itsNDeleted+=n;
  // If space was at the end of the bucket, reset used length.
  if (offset+n == itsUsedLength) {
    itsUsedLength = offset;
  }
  isChanged = true;

  if (itsNDeleted == itsLength) {
    itsSSMPtr->removeBucket(itsCurrentBucket);
    if (itsCurrentBucket == itsLastBucket) {
      itsLastBucket=-1;
    }
    itsCurrentBucket=-1;
    isChanged = false;
  }
  
  // Check if continuation in next bucket
  length -= n;
  if (length > 0) {
    int32_t next=itsNextBucket;
    // We are deleting this concatenated string 
    itsNextBucket=-1;
    offset=0;
    remove(next,offset,length);
  }  
}


void SSMStringHandler::get (String& string, int32_t bucket, int32_t offset, 
			    int32_t length)
{
  if (itsCurrentBucket != static_cast<int32_t>(bucket)) {
    getBucket(bucket);
  }
  string.resize (length);          // resize storage which adds trailing 0
  char* data = &(string[0]);       // get actual string
  getData(length,data,offset);
  // terminate string for old strings
#ifdef USE_OLD_STRING
  data[length] = '\0';  
#endif
}

void SSMStringHandler::get (Array<String>& string, int32_t bucket, int32_t offset,
			    int32_t length, bool handleShape)
{

  IPosition aShape;
  uint32_t aFilledFlag=0;
  String emptyString;

  if (length >0) {
    if (itsCurrentBucket != static_cast<int32_t>(bucket)) {
      getBucket(bucket);
    }
    aFilledFlag=1;

    if (handleShape) {
      getShape(aShape,bucket,offset,length);
      DebugAssert (aShape.isEqual (string.shape()),AipsError);
      // get the 'filled' flag
      getData (itsIntSize, itsIntBuf,offset);
      CanonicalConversion::toLocal(aFilledFlag,itsIntBuf);
    }
  }


  bool deleteIt;
  String* aString = string.getStorage(deleteIt);


  for (uint32_t i=0; i< string.nelements();i++) {

    if (aFilledFlag == 0) {
      aString[i] = emptyString;
    } else {
      // get length of next string Beware, offset resetting will be done in
      // getdata, so you don't need to do it here again...
      getData (itsIntSize, itsIntBuf,offset);
      
      int32_t aL=0;
      CanonicalConversion::toLocal(aL,itsIntBuf);
      aString[i].resize (aL);       // resize storage which adds trailing 0
      char* aS = &(aString[i][0]);  // get actual string
      // get next string. Beware, offset resetting will be done in
      // getdata, so you don't need to do it here again...
      getData (aL, aS, offset);
      // terminate string
#ifdef USE_OLD_STRING
      aS[aL] = '\0';  
#endif
    }
  }
  string.putStorage(aString,deleteIt);
}

void SSMStringHandler::putShape (int32_t& bucketNr, int32_t& offset, int32_t& length, 
				 const IPosition& aShape)
{
  int32_t totalLength=0;

  if (itsLastBucket == -1) {
    getNewBucket(false);
  } else if (itsCurrentBucket != itsLastBucket) {
    getBucket(itsLastBucket);
  }
  
  totalLength = (aShape.nelements()+2)*ValType::getCanonicalSize (TpInt);

  if (length > 0) {
    if (totalLength > length  ||  totalLength == 0) {
      remove (bucketNr, offset, length);
      bucketNr = 0;
      offset = 0;
      length = 0;
    }
  }

  if (length > 0) {
    if (itsCurrentBucket != bucketNr) {
      getBucket(bucketNr);
    }
    replace(bucketNr, offset, length, totalLength,aShape);
    length = totalLength;
    return;
  }

  if (itsLastBucket == -1) {
    getNewBucket(false);
  } else if (itsCurrentBucket != itsLastBucket) {
    getBucket(itsLastBucket);
  }

  

  // if Bucket available but shape doesn't fit and space < 50 get 
  // a new bucket anyway.

  if (totalLength > itsLength-itsUsedLength &&
      itsLength-itsUsedLength < 50 ) { 
    getNewBucket(false);
  }

  bucketNr=itsCurrentBucket;
  offset= itsUsedLength;
  length= totalLength;

  CanonicalConversion::fromLocal (itsIntBuf, uint32_t(aShape.nelements()));
  putData (itsIntSize, itsIntBuf);

  for (uint32_t i=0; i< aShape.nelements();i++) {
    CanonicalConversion::fromLocal (itsIntBuf, int32_t(aShape(i)));
    putData (itsIntSize, itsIntBuf);
  }

  // write 'empty' flag
  CanonicalConversion::fromLocal (itsIntBuf,0);
  putData (itsIntSize, itsIntBuf);
}

void SSMStringHandler::getShape (IPosition& aShape, int32_t bucket, int32_t& offset, 
				 int32_t)
{
  if (itsCurrentBucket != static_cast<int32_t>(bucket)) {
    getBucket(bucket);
  }

  getData (itsIntSize, itsIntBuf,offset);
  
  int32_t nDim=0;
  CanonicalConversion::toLocal(nDim,itsIntBuf);
  aShape.resize(nDim);

  int32_t tmp;
  for (int32_t i=0; i< nDim; i++) {
    getData (itsIntSize, itsIntBuf,offset);
    CanonicalConversion::toLocal(tmp, itsIntBuf);
    aShape(i) = tmp;
  }
}


void SSMStringHandler::flush()
{
  if (isChanged) {
    AlwaysAssert (itsCurrentBucket != -1, AipsError);
    //save old bucket
    char* aPtr = itsSSMPtr->getBucket(itsCurrentBucket);
    CanonicalConversion::fromLocal (aPtr+itsIntSize,   itsUsedLength);
    CanonicalConversion::fromLocal (aPtr+itsIntSize*2, itsNDeleted);
    CanonicalConversion::fromLocal (aPtr+itsIntSize*3, itsNextBucket);
    memcpy (aPtr+itsStart, itsData, itsLength);
    itsSSMPtr->setBucketDirty();
    isChanged = false;
  }
}

void SSMStringHandler::getBucket (uint32_t bucketNr,bool isNew)
{
  // check if itsCurrentBuffer is in use, if so save this one first
  flush();
  itsCurrentBucket = bucketNr;
  if (! isNew) {
    char* aPtr = itsSSMPtr->getBucket(itsCurrentBucket);
    memcpy (itsData, aPtr+itsStart, itsLength);
    CanonicalConversion::toLocal (itsUsedLength, aPtr+itsIntSize);
    CanonicalConversion::toLocal (itsNDeleted,   aPtr+2*itsIntSize);
    CanonicalConversion::toLocal (itsNextBucket, aPtr+3*itsIntSize);
  }
}

void SSMStringHandler::getNewBucket(bool doConcat)
{
  int32_t bucketNr = itsSSMPtr->getNewBucket();
  if (doConcat) {
    itsNextBucket = bucketNr;

    // save nextbucket
    isChanged=true;
  }
  getBucket(bucketNr,true);

  // zero dataspace
  itsUsedLength = 0;
  itsNDeleted = itsLength;
  itsNextBucket=-1;
  itsLastBucket=itsCurrentBucket;
}

void SSMStringHandler::resync()
{
  AlwaysAssert (!isChanged,AipsError);
  itsCurrentBucket = -1;
}

} //# NAMESPACE CASACORE - END

