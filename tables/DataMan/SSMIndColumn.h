//# SSMIndColumn.h: A column in Standard storage manager for indirect arrays
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

#ifndef TABLES_SSMINDCOLUMN_H
#define TABLES_SSMINDCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/SSMColumn.h>
#include <casacore/tables/DataMan/StIndArray.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class StManArrayFile;
class AipsIO;


// <summary>
// A column of Standard storage manager for indirect arrays.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tStandardStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SSMColumn>SSMColumn</linkto>
//   <li> <linkto class=StIndArray>StIndArray</linkto>
// </prerequisite>

// <etymology>
// SSMIndColumn represents a Column in the Standard Storage Manager
// containing Indirect arrays.
// </etymology>

// <synopsis> 
// SSMIndColumn is the implementation of an
// <linkto class=SSMColumn>SSMColumn</linkto> class
// to handle indirect arrays. The arrays (shape and data) are stored in
// a separate file using class <linkto class=StIndArray>StIndArray</linkto>.
// The file offset of the beginning of the array in stored in the
// appropriate data bucket using the standard SSMColumn functions.
// <p>
// Note that an indirect array can have a fixed shape. In that case
// adding a row results in reserving space for the array in the StIndArray
// file, so for each row an array is present.
// On the other hand adding a row does nothing for variable shaped arrays.
// So when no data is put or shape is set, a row may contain no array at all.
// In that case the function <src>isShapeDefined</src> returns False for
// that row.
// <p>
// Indirect arrays containing strings are not handled by this class, but
// by <linkto class=SSMIndStringColumn>SSMIndStringColumn</linkto>.
// That class stores those string arrays in the special string buckets
// instead of using StIndArray. The reason is that the string buckets
// are more disk space efficient when string arrays are frequently updated.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class SSMIndColumn : public SSMColumn
{
public:
  // Create a column of the given data type.
  // It keeps the pointer to its parent (but does not own it).
  SSMIndColumn (SSMBase* aParent, int aDataType, uInt aColNr);
  
  // Frees up the storage.
  ~SSMIndColumn();
  
  // An array of 'fixed length' strings is not handled specially,
  // thus this function is ignored.
  // It is needed to override the bahviour of the base class.
  virtual void setMaxLength (uInt maxLength);

  // It can handle access to a slice in a cell.
  virtual Bool canAccessSlice (Bool& reask) const;
  
  // Add (newNrrow-oldNrrow) rows to the column.
  virtual void addRow (uInt aNewNrRows, uInt anOldNrRows, Bool doInit);
  
  // Set the (fixed) shape of the arrays in the entire column.
  virtual void setShapeColumn (const IPosition& aShape);
  
  // Get the dimensionality of the item in the given row.
  virtual uInt ndim (uInt aRowNr);
  
  // Set the shape of the array in the given row and allocate the array
  // in the file.
  void setShape (uInt aRowNr, const IPosition& aShape);
  
  // Is the shape defined (i.e. is there an array) in this row?
  virtual Bool isShapeDefined (uInt aRowNr);
  
  // Get the shape of the array in the given row.
  virtual IPosition shape (uInt aRowNr);
  
  // This storage manager can handle changing array shapes.
  Bool canChangeShape() const;
  
  // Get an array value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn get function).
  // <group>
  virtual void getArrayBoolV     (uInt aRowNr, Array<Bool>* aDataPtr);
  virtual void getArrayuCharV    (uInt aRowNr, Array<uChar>* aDataPtr);
  virtual void getArrayShortV    (uInt aRowNr, Array<Short>* aDataPtr);
  virtual void getArrayuShortV   (uInt aRowNr, Array<uShort>* aDataPtr);
  virtual void getArrayIntV      (uInt aRowNr, Array<Int>* aDataPtr);
  virtual void getArrayuIntV     (uInt aRowNr, Array<uInt>* aDataPtr);
  virtual void getArrayfloatV    (uInt aRowNr, Array<float>* aDataPtr);
  virtual void getArraydoubleV   (uInt aRowNr, Array<double>* aDataPtr);
  virtual void getArrayComplexV  (uInt aRowNr, Array<Complex>* aDataPtr);
  virtual void getArrayDComplexV (uInt aRowNr, Array<DComplex>* aDataPtr);
  virtual void getArrayStringV   (uInt aRowNr, Array<String>* aDataPtr);
  // </group>
  
  // Put an array value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn put function).
  // <group>
  virtual void putArrayBoolV     (uInt aRowNr,
				  const Array<Bool>* aDataPtr);
  virtual void putArrayuCharV    (uInt aRowNr,
				  const Array<uChar>* aDataPtr);
  virtual void putArrayShortV    (uInt aRowNr,
				  const Array<Short>* aDataPtr);
  virtual void putArrayuShortV   (uInt aRowNr,
				  const Array<uShort>* aDataPtr);
  virtual void putArrayIntV      (uInt aRowNr,
				  const Array<Int>* aDataPtr);
  virtual void putArrayuIntV     (uInt aRowNr,
				  const Array<uInt>* aDataPtr);
  virtual void putArrayfloatV    (uInt aRowNr,
				  const Array<float>* aDataPtr);
  virtual void putArraydoubleV   (uInt aRowNr,
				  const Array<double>* aDataPtr);
  virtual void putArrayComplexV  (uInt aRowNr,
				  const Array<Complex>* aDataPtr);
  virtual void putArrayDComplexV (uInt aRowNr,
				  const Array<DComplex>* aDataPtr);
  virtual void putArrayStringV   (uInt aRowNr,
				  const Array<String>* aDataPtr);
  // </group>
  
  // Get a section of the array in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getSlice function).
  // <group>
  virtual void getSliceBoolV     (uInt aRowNr, const Slicer&,
				  Array<Bool>* aDataPtr);
  virtual void getSliceuCharV    (uInt aRowNr, const Slicer&,
				  Array<uChar>* aDataPtr);
  virtual void getSliceShortV    (uInt aRowNr, const Slicer&,
				  Array<Short>* aDataPtr);
  virtual void getSliceuShortV   (uInt aRowNr, const Slicer&,
				  Array<uShort>* aDataPtr);
  virtual void getSliceIntV      (uInt aRowNr, const Slicer&,
				  Array<Int>* aDataPtr);
  virtual void getSliceuIntV     (uInt aRowNr, const Slicer&,
				  Array<uInt>* aDataPtr);
  virtual void getSlicefloatV    (uInt aRowNr, const Slicer&,
				  Array<float>* aDataPtr);
  virtual void getSlicedoubleV   (uInt aRowNr, const Slicer&,
				  Array<double>* aDataPtr);
  virtual void getSliceComplexV  (uInt aRowNr, const Slicer&,
				  Array<Complex>* aDataPtr);
  virtual void getSliceDComplexV (uInt aRowNr, const Slicer&,
				  Array<DComplex>* aDataPtr);
  virtual void getSliceStringV   (uInt aRowNr, const Slicer&,
				  Array<String>* aDataPtr);
  // </group>
  
  // Put into a section of the array in the given row.
  // The buffer pointed to by aDataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn putSlice function).
  // <group>
  virtual void putSliceBoolV     (uInt aRowNr, const Slicer&,
				  const Array<Bool>* aDataPtr);
  virtual void putSliceuCharV    (uInt aRowNr, const Slicer&,
				  const Array<uChar>* aDataPtr);
  virtual void putSliceShortV    (uInt aRowNr, const Slicer&,
				  const Array<Short>* aDataPtr);
  virtual void putSliceuShortV   (uInt aRowNr, const Slicer&,
				  const Array<uShort>* aDataPtr);
  virtual void putSliceIntV      (uInt aRowNr, const Slicer&,
				  const Array<Int>* aDataPtr);
  virtual void putSliceuIntV     (uInt aRowNr, const Slicer&,
				  const Array<uInt>* aDataPtr);
  virtual void putSlicefloatV    (uInt aRowNr, const Slicer&,
				  const Array<float>* aDataPtr);
  virtual void putSlicedoubleV   (uInt aRowNr, const Slicer&,
				  const Array<double>* aDataPtr);
  virtual void putSliceComplexV  (uInt aRowNr, const Slicer&,
				  const Array<Complex>* aDataPtr);
  virtual void putSliceDComplexV (uInt aRowNr, const Slicer&,
				  const Array<DComplex>* aDataPtr);
  virtual void putSliceStringV   (uInt aRowNr, const Slicer&,
				  const Array<String>* aDataPtr);
  // </group>
  
  // Let the column object create its array file.
  virtual void doCreate (uInt aNrRows);

  // Open an existing file.
  virtual void getFile (uInt aNrRows);

  // Remove the given row from the data bucket and possibly string bucket.
  virtual void deleteRow(uInt aRowNr);


private:
  // Forbid copy constructor.
  SSMIndColumn (const SSMIndColumn&);
  
  // Forbid assignment.
  SSMIndColumn& operator= (const SSMIndColumn&);
  
  // Initialize part of the object and open/create the file.
  // It is used by doCreate and getFile.
  void init();
  
  // Read the shape at the given row.
  // This will cache the information in the StIndArray
  // object for that row.
  StIndArray* getShape (uInt aRowNr);
  
  // Return a pointer to the array in the given row (for a get).
  StIndArray* getArrayPtr (uInt aRowNr);

  
  //# The shape off all arrays in case it is fixed
  IPosition       itsFixedShape;
  //# Switch indicating if the shape is fixed.
  Bool            isShapeFixed;
  //# The file containing the arrays.
  StManArrayFile* itsIosFile;
  //# The indirect array object.
  StIndArray      itsIndArray;
};



} //# NAMESPACE CASACORE - END

#endif
