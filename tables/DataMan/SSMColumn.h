//# SSMColumn.h: A Column in the Standard Storage Manager
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

#ifndef TABLES_SSMCOLUMN_H
#define TABLES_SSMCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManColumn.h>
#include <casacore/tables/DataMan/SSMBase.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/OS/Conversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations


// <summary>
// A Column in the Standard Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tStandardStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMBase>SSMBase</linkto>
//   <li> <linkto class=SSMStringHandler>SSMStringHandler</linkto>
// </prerequisite>

// <etymology>
// SSMColumn represents a Column in the Standard Storage Manager.
// </etymology>

// <synopsis>
// SSMColumn is the base class for access to a column stored with
// the Standard Storage manager. It provides some basic functionality
// for the derived classes handling direct and indirect arrays.
// <p>
// The main task of SSMColumn is handling the access to a column
// containing scalars of the various data types. The data is stored
// in buckets. The classes <linkto class=SSMBase>SSMBase</linkto>
// and <linkto class=SSMIndex>SSMIndex</linkto> keep track in which data
// bucket a given row is stored and at which offset the column starts.
// Using that information SSMColumn can access its data easily.
// <p>
// Almost all data types have a fixed length and can be handled easily.
// However, strings are a special case.
// <br>If the string is fixed length (which means it has a maximum length),
// the string is stored directly in the data bucket. If the string is
// shorter than the maximum length, its length is indicated by a
// trailing 0.
// <br>Variable strings are in principle stored in a special string bucket.
// The data bucket contains 3 integers telling the bucketnr, offset, and
// length of the string. However, it the string is short enough (ie. <=
// 8 characters), the string is stored directly in data bucket using
// the space for bucketnr and offset.
// <p>
// The class maintains a cache of the data in the bucket last read.
// This cache is used by the higher level table classes to get faster
// read access to the data.
// The cache is not used for strings, because they are stored differently.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class SSMColumn : public StManColumn
{
public:
  // Create a SSMColumn object with the given parent.
  // It initializes the various variables.
  // It keeps the pointer to its parent (but does not own it).
  SSMColumn (SSMBase* aParent, int aDataType, uInt aColNr);
  
  virtual ~SSMColumn();
  
  // Set the shape of an array in the column.
  // It is only called (right after the constructor) if the array has
  // a fixed shape.
  virtual void setShapeColumn (const IPosition& aShape);

  // Set the maximum length of a 'fixed length' string.
  // It is only called (right after the constructor) if the string has
  // a fixed length
  virtual void setMaxLength (uInt maxLength);

  // Get the dimensionality of the item in the given row.
  virtual uInt ndim (uInt aRowNr);
  
  // Get the shape of the array in the given row.
  virtual IPosition shape (uInt aRowNr);
  
  // Let the object initialize itself for a newly created table.
  // It is meant for a derived class.
  virtual void doCreate (uInt aNrRows);

  // Let the column object initialize itself for an existing table
  virtual void getFile (uInt aNrRows);

  // Resync the storage manager with the new file contents.
  // It resets the last rownr put.
  void resync (uInt aNrRow);
  
  // Get the scalar value in the given row.
  // <group>
  virtual void getBoolV     (uInt aRowNr, Bool* aDataPtr);
  virtual void getuCharV    (uInt aRowNr, uChar* aDataPtr);
  virtual void getShortV    (uInt aRowNr, Short* aDataPtr);
  virtual void getuShortV   (uInt aRowNr, uShort* aDataPtr);
  virtual void getIntV      (uInt aRowNr, Int* aDataPtr);
  virtual void getuIntV     (uInt aRowNr, uInt* aDataPtr);
  virtual void getfloatV    (uInt aRowNr, float* aDataPtr);
  virtual void getdoubleV   (uInt aRowNr, double* aDataPtr);
  virtual void getComplexV  (uInt aRowNr, Complex* aDataPtr);
  virtual void getDComplexV (uInt aRowNr, DComplex* aDataPtr);
  virtual void getStringV   (uInt aRowNr, String* aDataPtr);
  // </group>
  
  // Put the scalar value in the given row.
  // It updates the cache if the row is contained in the cache.
  // <group>
  virtual void putBoolV     (uInt aRowNr, const Bool* aDataPtr);
  virtual void putuCharV    (uInt aRowNr, const uChar* aDataPtr);
  virtual void putShortV    (uInt aRowNr, const Short* aDataPtr);
  virtual void putuShortV   (uInt aRowNr, const uShort* aDataPtr);
  virtual void putIntV      (uInt aRowNr, const Int* aDataPtr);
  virtual void putuIntV     (uInt aRowNr, const uInt* aDataPtr);
  virtual void putfloatV    (uInt aRowNr, const float* aDataPtr);
  virtual void putdoubleV   (uInt aRowNr, const double* aDataPtr);
  virtual void putComplexV  (uInt aRowNr, const Complex* aDataPtr);
  virtual void putDComplexV (uInt aRowNr, const DComplex* aDataPtr);
  virtual void putStringV   (uInt aRowNr, const String* aDataPtr);
  // </group>
  
  // Get the scalar values of the entire column.
  // <group>
  virtual void getScalarColumnBoolV     (Vector<Bool>* aDataPtr);
  virtual void getScalarColumnuCharV    (Vector<uChar>* aDataPtr);
  virtual void getScalarColumnShortV    (Vector<Short>* aDataPtr);
  virtual void getScalarColumnuShortV   (Vector<uShort>* aDataPtr);
  virtual void getScalarColumnIntV      (Vector<Int>* aDataPtr);
  virtual void getScalarColumnuIntV     (Vector<uInt>* aDataPtr);
  virtual void getScalarColumnfloatV    (Vector<float>* aDataPtr);
  virtual void getScalarColumndoubleV   (Vector<double>* aDataPtr);
  virtual void getScalarColumnComplexV  (Vector<Complex>* aDataPtr);
  virtual void getScalarColumnDComplexV (Vector<DComplex>* aDataPtr);
  virtual void getScalarColumnStringV   (Vector<String>* aDataPtr);
  // </group>
  
  // Put the scalar values of the entire column.
  // It invalidates the cache.
  // <group>
  virtual void putScalarColumnBoolV     (const Vector<Bool>* aDataPtr);
  virtual void putScalarColumnuCharV    (const Vector<uChar>* aDataPtr);
  virtual void putScalarColumnShortV    (const Vector<Short>* aDataPtr);
  virtual void putScalarColumnuShortV   (const Vector<uShort>* aDataPtr);
  virtual void putScalarColumnIntV      (const Vector<Int>* aDataPtr);
  virtual void putScalarColumnuIntV     (const Vector<uInt>* aDataPtr);
  virtual void putScalarColumnfloatV    (const Vector<float>* aDataPtr);
  virtual void putScalarColumndoubleV   (const Vector<double>* aDataPtr);
  virtual void putScalarColumnComplexV  (const Vector<Complex>* aDataPtr);
  virtual void putScalarColumnDComplexV (const Vector<DComplex>* aDataPtr);
  virtual void putScalarColumnStringV   (const Vector<String>* aDataPtr);
  // </group>
  
  // Add (NewNrRows-OldNrRows) rows to the Column and initialize
  // the new rows when needed.
  virtual void addRow (uInt aNewNrRows, uInt anOldNrRows, Bool doInit);

  // Remove the given row from the data bucket and possibly string bucket.
  // If needed, it also removes it from the cache.
  virtual void deleteRow (uInt aRowNr);

  // Get the size of the dataType in bytes!!
  uInt getExternalSizeBytes() const;

  // Get the size of the dataType in bits!!
  uInt getExternalSizeBits() const;

  // get the sequence number of this column.
  uInt getColNr();

  // set the sequence number of this column.
  void setColNr (uInt aColNr);

  // If something special has to be done before removing the Column,
  // as is the case with Strings, it can be done here.
  void removeColumn();

protected:
  // Shift the rows in the bucket one to the left when removing the given row.
  void shiftRows (char* aValue, uInt rowNr, uInt startRow, uInt endRow);

  // Fill the cache with data of the bucket containing the given row.
  void getValue (uInt aRowNr);
  
  // Get the bucketnr, offset, and length of a variable length string.
  // <src>data</src> must have 3 Ints to hold the values.
  // It returns a pointer to the data in the bucket, which can be used
  // for the case that the data bucket contains the (short) string.
  Char* getRowValue (Int* data, uInt aRowNr);
    
  // Put the given value for the row into the correct data bucket.
  void putValue (uInt aRowNr, const void* aValue);

  // Put the given string for the row into the correct data bucket.
  // The argument <src>aValue></src> must be 3 Ints (for bucketnr, offset,
  // and length). Only the length is actually used.
  void putValueShortString (uInt aRowNr, const void* aValue,
			    const String& string);
  
  // Get the values for the entire column.
  // The data from all buckets is copied to the array.
  void getColumnValue (void* anArray, uInt aNrRows);
  
  // Put the values from the array in the entire column.
  // Each data bucket is filled with the the appropriate part of the array.
  void putColumnValue (const void* anArray, uInt aNrRows);


  // Pointer to the parent storage manager.
  SSMBase*          itsSSMPtr;
  // Length of column cell value in storage format (0 = variable length).
  uInt              itsExternalSizeBytes;
  uInt              itsExternalSizeBits;
  // Column sequence number of this column.
  uInt              itsColNr;
  // The shape of the column.
  IPosition         itsShape;
  // The maximum length of a 'fixed length' string.
  uInt              itsMaxLen;
  // Number of elements in a value for this column.
  uInt              itsNrElem;
  // Number of values to be copied.
  // Normally this is itsNrElem, but for complex types it is 2*itsNrElem.
  // When local format is used, it is the number of bytes.
  uInt              itsNrCopy;
  // The sizeof the datatype in local format
  uInt              itsLocalSize;
  // The data in local format.
  void*             itsData;
  // Pointer to a convert function for writing.
  Conversion::ValueFunction* itsWriteFunc;
  // Pointer to a convert function for reading.
  Conversion::ValueFunction* itsReadFunc;
  
private:
  // Forbid copy constructor.
  SSMColumn (const SSMColumn&);
  
  // Forbid assignment.
  SSMColumn& operator= (const SSMColumn&);
  
  // Initialize part of the object.
  // It determines the nr of elements, the function to use to convert
  // from local to file format, etc..
  void init();

  // Get the pointer to the cache. It is created if not done yet.
  char* getDataPtr();
};


inline uInt SSMColumn::getExternalSizeBytes() const
{
  return itsExternalSizeBytes;
}

inline uInt SSMColumn::getExternalSizeBits() const
{
  return itsExternalSizeBits;
}

inline char* SSMColumn::getDataPtr()
{
  if (itsData == 0) {
    itsData = new char[itsSSMPtr->getRowsPerBucket(itsColNr) * itsLocalSize];
  }
  return static_cast<char*>(itsData);
}

inline uInt SSMColumn::getColNr()
{
  return itsColNr;
}

inline void SSMColumn::setColNr (uInt aColNr)
{
  itsColNr = aColNr;
}



} //# NAMESPACE CASACORE - END

#endif
