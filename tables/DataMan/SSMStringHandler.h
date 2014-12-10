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
 
#ifndef TABLES_SSMSTRINGHANDLER_H
#define TABLES_SSMSTRINGHANDLER_H
 
 
//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations.
class SSMBase;


// <summary>
// Store strings in the Standard Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tSSMStringHandler.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMBase>SSMBase</linkto>
// </prerequisite>

// <etymology>
// SSMStringHandler handles strings for the Standard Storage Manager.
// </etymology>

// <synopsis>
// Variable length strings cannot be stored in the data bucket.
// Only short (<8 characters) strings can be stored directly.
// Class SSMStringhandler is used by the SSM to store strings in
// so-called string buckets.
// A string bucket has the following layout:
// <ul>
//  <li> The first Int is reserved to be used for the free bucket list.
//  <li> <src>itsUsedLength</src> tells how many bytes have been used.
//       Thus it tells the next free byte in the string part.
//       In principle it always increases. Only if data are removed
//       from the last part of the string part, it is decreased, thus
//       the deleted part can be reused again.
//  <li> <src>itsNDeleted</src> tells how many bytes of the string part
//       are deleted (i.e. not used). Initially it is the length of the
//       string part of the bucket (i.e. bucketsize minus 4 Ints).
//       When a string is stored, its length is subtracted from itsNDeleted.
//       When a string is removed, its length is added again.
//       When the string part is deleted, the bucket is added to the
//       free bucket list.
//  <li> <src>itsNextBucket</src> tells the next bucket if the last
//       entry in the bucket is continued in another bucket.
//       Normally this field is -1 (meaning not continued), but long
//       strings or string arrays might be continued in another bucket
//       (and continued from there again).
//  <li> The string part is a sequence of bytes containing the string
//       data. When a value is to be stored, it will replace the current
//       value if the new value is not longer. Otherwise the current
//       value (if any) is deleted and the new value is appended to
//       the end of the string part in the last bucket used.
//       <p>
//       For a scalar string only its characters are stored. Its length
//       (and bucketnr and offset in string bucket) are stored in the data
//       bucket.
//       <br>
//       A fixed length array is stored as an array of bytes. That byte
//       array contains length-value pairs for each element of the array.
//       The total length (and bucketnr and offset) are stored in the data
//       bucket.
//       <br>
//       A variable length array is stored as the shape, a flag, optionally
//       followed by the string array as length-value pairs (as above).
//       The shape consists of the nr of dimensions followed by the
//       length of each dimension. The flag indicates if a string array
//       is actually stored. It is not if only the shape of the array
//       is set, but no data put yet.
// </ul>
// SSMStringHandler keeps a copy of the current bucket in use to reduce
// the number of accesses to the bucket cache.
// <p>
// It also keeps the bucket number of the last bucket where data were
// added to. It tells which bucket to use when new data has to be stored.
// </synopsis>
  
//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

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

  // Put a single string or an array of strings into a bucket.
  // If its length does not exceed the given length, it reuses
  // the currently used space (given by bucketnr and offset).
  // Otherwise it adds the data to the last string bucket.
  // It fills the offset and bucketnr where the data are stored and the
  // length occupied in the buckets.
  // An array of strings is flattened first (a la SSMColumn::writeString).
  // <br>
  // If <src>handleShape</src> is True (for variable shaped arrays), the
  // shape will be put first.
  // <group>
  void put (Int& bucketNr, Int& offset, Int& length, 
	    const String& string);
  void put (Int& bucketNr, Int& offset, Int& length, 
	    const Array<String>& string, Bool handleShape);
  // </group>

  // Put a single string or an array of strings into a bucket.
  // If its length does not exceed the given length, it reuses
  // the currently used space (given by bucketnr and offset).
  // Otherwise it adds the data to the last string bucket.
  // It fills the offset and bucketnr where stored and the
  // length occupied in the buckets.
  void putShape (Int& bucketNr, Int& offset, Int& length, 
		 const IPosition& aShape);

  // Get the shape in the given bucket and offset.
  // It sets the offset to the data right after the shape.
  // The IPosition object is resized as needed.
  void getShape (IPosition& aShape, Int bucket, Int& offset, Int length);

  // Remove data with the given length from a bucket.
  // If the data are continued in next bucket(s), they will be
  // removed there as well.
  void remove (Int bucketNr, Int offset, Int length);

  // Get a string or an array of strings.
  // The array must have the correct shape.
  // <src>handleShape</src> will be True for variable shaped arrays
  // indicating that the data are preceeded by the shape.
  // <group>
  void get (String& string, Int bucket, Int offset, Int length);
  void get (Array<String>& string, Int bucket, Int offset, 
	    Int length, Bool handleShape);
  // </group>

  // Flush the currently used string bucket.
  void flush();
  
  // Initialize the StringHandler
  void init();

  // Resynchronize (after a table lock was acquired).
  // It clears the itsCurrentBucket variable to assure that buckets
  // are reread.
  void resync();
  
private:
  // Forbid copy constructor and assignment.
  // <group>
  SSMStringHandler (const SSMStringHandler&);
  SSMStringHandler& operator= (const SSMStringHandler&);
  // </group>

  // Get the given bucket and make it current.
  // It first writes the current bucket if it has changed.
  // <br>
  // If <src>isNew</src> is True the bucket is new,
  // so the Ints at its beginning do not have to be interpreted.
  void getBucket (uInt bucketNr, Bool isNew=False);

  // Get a new bucket and make it current.
  // If <src>doConcat</src> is True, the new bucket is a continuation,
  // so <src>itsNextBucket</src> in the currently used bucket is filled
  // with the new bucket number.
  void getNewBucket (Bool doConcat);

  // Put the data with the given length at the end of the current bucket.
  // If they do not fit, they are continued in a new bucket.
  void putData (Int length, const Char* data);

  // Get the data with the given length from the curent bucket at the
  // given offset. If sets the offset to the byte after the data read.
  // Continuation buckets are followed (and made current).
  void getData (Int length, Char* data, Int& offset);

  // Replace the current data with the new data.
  // It is used by <src>put</src> after having assured that the
  // new length does not exceed the current one.
  // It follows continuation buckets as needed.
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



} //# NAMESPACE CASACORE - END

#endif
