//# SSMStringHandler.h: Store strings in the Standard Storage Manager
//# Copyright (C) 2000
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
 
#if !defined(AIPS_SSMSTRINGHANDLER_H)
#define AIPS_SSMSTRINGHANDLER_H
 
 
//# Includes
#include <aips/aips.h>
#include <aips/OS/Conversion.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/Array.h>

class SSMBase;

class SSMStringHandler
{
public:
  // Default constructor initializes last string bucket to -1.
  SSMStringHandler (SSMBase* aBase);

  ~SSMStringHandler();

  // Set or get last string bucketnr.
  // Setting is needed when an existing table is opened.
  // <group>
  void setLastStringBucket (Int lastStringBucket);
  Int lastStringBucket() const;
  // </group>


  // set or get rownrs available in this StringHandler
  // setting is needed when an existing table is opened
  // <group>
  void setNrRows(const uInt aNrRows);
  uInt getNrRows() const;
  // </group>

  // Add a single string or an array of strings to a bucket.
  // If too long, continue in next bucket(s).
  // It fills the offset and bucketnr where stored and returns the
  // length occupied in the buckets.
  // An array of string is flattened first (a la SSMColumn::writeString).
  // <group>
  void put (Int& bucketNr, Int& offset, Int& length, 
	    const String& string);
  void put (Int& bucketNr, Int& offset, Int& length, 
	    const Array<String>& string, Bool handleShape);
  // </group>

  void putShape (Int& bucketNr, Int& offset, Int& length, 
		 const IPosition& aShape);
  void getShape (IPosition& aShape, Int bucket, Int& offset, 
		 Int length);

  // Remove data with the given length from a bucket.
  // If the data is continued in next bucket(s), they will be
  // removed there as well.
  void remove (Int bucketNr, Int offset, Int length);

  // Get a string or an array of strings.
  // The array must have the correct shape.
  // <group>
  void get (String& string, Int bucket, Int offset, Int length);
  void get (Array<String>& string, Int bucket, Int offset, 
	    Int length, Bool handleShape);
  // </group>

  // write pointers in bucket (if used). Be awaer to flush this
  // before the buckets are flushed, because there's something written
  // in the buckets here.
  void flush();
  
  // Init the StringHandler
  void init();

  void resync();
  
private:
  // Forbid copy constructor and assignment
  // <group>
  SSMStringHandler (const SSMStringHandler&);
  SSMStringHandler& operator= (const SSMStringHandler&);
  // </group>

  // Get the given bucket and make it current.
  // It first writes the current bucket if it has changed.
  void getBucket (uInt bucketNr,Bool isNew=False);

  void getNewBucket(Bool doConcat);
  
  void putData (Int length, const Char* data);
  void getData (Int length, Char* data,Int& offset);

  // <group>
  void replace (Int bucketNr, Int offset, Int length, 
		const String& string);
  void replace (Int bucketNr, Int offset, Int length, Int totalLength, 
		const IPosition& aShape);
  void replace (Int bucketNr, Int offset, Int length, Int totalLength,
		const Array<String>& string, Bool handleShape);
  void replaceData (Int& offset,Int length, const Char* data);
  // </group>

  SSMBase* itsSSMPtr;      // Pointer to SSMBase stucture
  Int   itsCurrentBucket;  // bucketnr of current string bucket (-1 is none)
  Int   itsLength;         // length of bucket in use (only the string part)
  Int   itsNDeleted;       // #bytes deleted from the string part of the bucket
  Int   itsUsedLength;     // #bytes used from the string part of the bucket
  Int   itsNextBucket;     // next bucket for long strings
  char* itsData;           // bucket string data
  char* itsIntBuf;         // buffer for initialisation params
  Bool  isChanged;         // has current bucket been changed?
  uInt  itsIntSize;        // size of integers in this system
  Int   itsLastBucket;     // last string bucket used
  uInt  itsStart;          // Start position of actual data in bucket
};


inline void SSMStringHandler::setLastStringBucket (Int lastStringBucket)
{ 
  itsLastBucket = lastStringBucket;
}  

inline Int SSMStringHandler::lastStringBucket() const
{ 
  return itsLastBucket; 
}  

#endif
