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

#if !defined(AIPS_SSMCOLUMN_H)
#define AIPS_SSMCOLUMN_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/StManColumn.h>
#include <aips/Tables/SSMBase.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Containers/Block.h>
#include <aips/Utilities/Compare.h>
#include <aips/OS/Conversion.h>

//# Forward declarations


// <summary>
// A Column in the Standard Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMBase>SSMBase</linkto>
// </prerequisite>

// <etymology>
// SSMColumn represents a Column in the Standard Storage Manager.
// </etymology>

// <synopsis>
// SSMColumn handles the access to a column containing scalars of the 
// various data types. 
// </synopsis> 

// <motivation>
// SSMColumn encapsulates all operations on an SSM Column.
// </motivation>

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
  virtual void setShapeColumn (const IPosition& aShape);

  // Set the maximum length of a 'fixed length' string.
  virtual void setMaxLength (uInt maxLength);

  // Get the dimensionality of the item in the given row.
  // This is the same for all rows.
  virtual uInt ndim (uInt aRowNr);
  
  // Get the shape of the array in the given row.
  // This is the same for all rows.
  virtual IPosition shape (uInt aRowNr);
  
  // Let the Column object initialize itself for a newly created table
  // This is meant for a derived class
  virtual void doCreate(uInt aNrRows);

  // Let the Column object initialize itself for an existing table
  virtual void getFile(uInt aNrRows);

  // Resync the storage manager with the new file contents.
  // It resets the last rownr put.
  void resync (uInt aNrRow);
  
  // Get a scalar value in the given row.
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
  
  // Put a scalar value in the given row.
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
  
  // Get the scalar values in the entire column
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
  
  // put the scalar values in the entire column
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
  
  virtual void deleteRow(uInt aRowNr);

  // Get the function needed to read/write a uInt from/to external format.
  // This is used by other classes to read the length of a variable
  // data value.
  // <group>
  static Conversion::ValueFunction* getReadUInt  (Bool asCanonical);
  static Conversion::ValueFunction* getWriteUInt (Bool asCanonical);
  // </group>
  
  // get the size of the dataType in bytes!!
  uInt getExternalSizeBytes() const;

  // get the size of the dataType in bits!!
  uInt getExternalSizeBits() const;

  // get the SequenceNr of this Column
  uInt getColNr();

  // set the SequenceNr of this Column
  void setColNr(const uInt aColNr);

  // if something special has to be done before removing the Column,
  // as is the case by Strings, it can be done here
  void removeColumn();

protected:
  //# Declare member variables.

  // Shiftrows after removing a row
  void shiftRows(char* aValue, uInt aRowNr, uInt aSRow, uInt anERow);

  //set the cache && itsData for this row
  void getValue(uInt aRowNr);
  
  // In case strings are used.
  Char* getRowValue(Int* data, uInt aRowNr);
    
  // set the value for this row
  // <group>
  void putValue(uInt aRowNr, const void* aValue);
  void putValueShortString(uInt aRowNr, const void* aValue,
			   const String& string);
  // </group>
  
  //get the values from the entire column
  void getColumnValue(void* anArray,uInt aNrRows);
  
  //put the values from the array in the entire column
  void putColumnValue(const void* anArray,uInt aNrRows);
  
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
  
  // Copy uInt values.
  // This function is used to write the lengths, etc. when the
  // data is kept in local format.
  static uInt copyUInt (void* anOut, const void* anIn, uInt aNValues);
  
  // Initialize part of the object.
  // It is used by doCreate and getFile.
  void init();

  // Create local cache if not available
  char* getDataPtr();

};


inline uInt SSMColumn::getExternalSizeBytes() const
{
  return (itsExternalSizeBytes);
}

inline uInt SSMColumn::getExternalSizeBits() const
{
  return (itsExternalSizeBits);
}

inline char* SSMColumn::getDataPtr()
{
  if (itsData == 0) {
    itsData = new char[itsSSMPtr->getRowsPerBucket(itsColNr) 
		      * itsLocalSize];
  }
  return static_cast<char*>(itsData);
}

inline uInt SSMColumn::getColNr()
{
  return (itsColNr);
}

inline  void SSMColumn::setColNr(const uInt aColNr)
{
  itsColNr = aColNr;
}

#endif
