//# MSMDirColumn.h: Memory storage manager for fixed shape table arrays
//# Copyright (C) 2003
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

#ifndef TABLES_MSMDIRCOLUMN_H
#define TABLES_MSMDIRCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/MSMColumn.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Memory storage manager for table arrays
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> MSMBase
//   <li> MSMColumn
// </prerequisite>

// <synopsis> 
// MSMDirColumn handles arrays in a table column.
// It only keeps them in memory, so they are not persistent.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class MSMDirColumn: public MSMColumn
{
public:
  // Create a column of the given type.
  MSMDirColumn (MSMBase* smptr, int dataType);

  // Frees up the storage.
  ~MSMDirColumn();

  // It can handle access to a slice in a cell.
  Bool canAccessSlice (Bool& reask) const;

  // It can handle access to an entire column.
  Bool canAccessArrayColumn (Bool& reask) const;

  // Set the (fixed) shape of the arrays in the entire column.
  void setShapeColumn (const IPosition& shape);

  // Add (newNrrow-oldNrrow) rows to the column.
  // Allocate the data arrays in these rows if the shape is fixed.
  void addRow (uInt newNrrow, uInt oldNrrow);

  // Get the dimensionality of the item in the given row.
  // 0 is returned if there is no array.
  uInt ndim (uInt rownr);

  // Get the shape of the array in the given row.
  // An zero-length IPosition is returned if there is no array.
  IPosition shape (uInt rownr);

  // Get an array value in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn get function).
  // <group>
  void getArrayBoolV     (uInt rownr, Array<Bool>* dataPtr);
  void getArrayuCharV    (uInt rownr, Array<uChar>* dataPtr);
  void getArrayShortV    (uInt rownr, Array<Short>* dataPtr);
  void getArrayuShortV   (uInt rownr, Array<uShort>* dataPtr);
  void getArrayIntV      (uInt rownr, Array<Int>* dataPtr);
  void getArrayuIntV     (uInt rownr, Array<uInt>* dataPtr);
  void getArrayfloatV    (uInt rownr, Array<float>* dataPtr);
  void getArraydoubleV   (uInt rownr, Array<double>* dataPtr);
  void getArrayComplexV  (uInt rownr, Array<Complex>* dataPtr);
  void getArrayDComplexV (uInt rownr, Array<DComplex>* dataPtr);
  void getArrayStringV   (uInt rownr, Array<String>* dataPtr);
  // </group>

  // Put an array value into the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn put function).
  // <group>
  void putArrayBoolV     (uInt rownr, const Array<Bool>* dataPtr);
  void putArrayuCharV    (uInt rownr, const Array<uChar>* dataPtr);
  void putArrayShortV    (uInt rownr, const Array<Short>* dataPtr);
  void putArrayuShortV   (uInt rownr, const Array<uShort>* dataPtr);
  void putArrayIntV      (uInt rownr, const Array<Int>* dataPtr);
  void putArrayuIntV     (uInt rownr, const Array<uInt>* dataPtr);
  void putArrayfloatV    (uInt rownr, const Array<float>* dataPtr);
  void putArraydoubleV   (uInt rownr, const Array<double>* dataPtr);
  void putArrayComplexV  (uInt rownr, const Array<Complex>* dataPtr);
  void putArrayDComplexV (uInt rownr, const Array<DComplex>* dataPtr);
  void putArrayStringV   (uInt rownr, const Array<String>* dataPtr);
  // </group>

  // Get a section of the array in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getSlice function).
  // <group>
  void getSliceBoolV     (uInt rownr, const Slicer&, Array<Bool>* dataPtr);
  void getSliceuCharV    (uInt rownr, const Slicer&, Array<uChar>* dataPtr);
  void getSliceShortV    (uInt rownr, const Slicer&, Array<Short>* dataPtr);
  void getSliceuShortV   (uInt rownr, const Slicer&, Array<uShort>* dataPtr);
  void getSliceIntV      (uInt rownr, const Slicer&, Array<Int>* dataPtr);
  void getSliceuIntV     (uInt rownr, const Slicer&, Array<uInt>* dataPtr);
  void getSlicefloatV    (uInt rownr, const Slicer&, Array<float>* dataPtr);
  void getSlicedoubleV   (uInt rownr, const Slicer&, Array<double>* dataPtr);
  void getSliceComplexV  (uInt rownr, const Slicer&, Array<Complex>* dataPtr);
  void getSliceDComplexV (uInt rownr, const Slicer&, Array<DComplex>* dataPtr);
  void getSliceStringV   (uInt rownr, const Slicer&, Array<String>* dataPtr);
  // </group>

  // Put into a section of the array in the given row.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn putSlice function).
  // <group>
  void putSliceBoolV     (uInt rownr, const Slicer&,
			  const Array<Bool>* dataPtr);
  void putSliceuCharV    (uInt rownr, const Slicer&,
			  const Array<uChar>* dataPtr);
  void putSliceShortV    (uInt rownr, const Slicer&,
			  const Array<Short>* dataPtr);
  void putSliceuShortV   (uInt rownr, const Slicer&,
			  const Array<uShort>* dataPtr);
  void putSliceIntV      (uInt rownr, const Slicer&,
			  const Array<Int>* dataPtr);
  void putSliceuIntV     (uInt rownr, const Slicer&,
			  const Array<uInt>* dataPtr);
  void putSlicefloatV    (uInt rownr, const Slicer&,
			  const Array<float>* dataPtr);
  void putSlicedoubleV   (uInt rownr, const Slicer&,
			  const Array<double>* dataPtr);
  void putSliceComplexV  (uInt rownr, const Slicer&,
			  const Array<Complex>* dataPtr);
  void putSliceDComplexV (uInt rownr, const Slicer&,
			  const Array<DComplex>* dataPtr);
  void putSliceStringV   (uInt rownr, const Slicer&,
			  const Array<String>* dataPtr);
  // </group>

  // Get all array values in the column.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn getColumn function).
  // <group>
  void getArrayColumnBoolV     (Array<Bool>* dataPtr);
  void getArrayColumnuCharV    (Array<uChar>* dataPtr);
  void getArrayColumnShortV    (Array<Short>* dataPtr);
  void getArrayColumnuShortV   (Array<uShort>* dataPtr);
  void getArrayColumnIntV      (Array<Int>* dataPtr);
  void getArrayColumnuIntV     (Array<uInt>* dataPtr);
  void getArrayColumnfloatV    (Array<float>* dataPtr);
  void getArrayColumndoubleV   (Array<double>* dataPtr);
  void getArrayColumnComplexV  (Array<Complex>* dataPtr);
  void getArrayColumnDComplexV (Array<DComplex>* dataPtr);
  void getArrayColumnStringV   (Array<String>* dataPtr);
  // </group>

  // Put all arrays in the column.
  // The buffer pointed to by dataPtr has to have the correct length
  // (which is guaranteed by the ArrayColumn putColumn function).
  // <group>
  void putArrayColumnBoolV     (const Array<Bool>* dataPtr);
  void putArrayColumnuCharV    (const Array<uChar>* dataPtr);
  void putArrayColumnShortV    (const Array<Short>* dataPtr);
  void putArrayColumnuShortV   (const Array<uShort>* dataPtr);
  void putArrayColumnIntV      (const Array<Int>* dataPtr);
  void putArrayColumnuIntV     (const Array<uInt>* dataPtr);
  void putArrayColumnfloatV    (const Array<float>* dataPtr);
  void putArrayColumndoubleV   (const Array<double>* dataPtr);
  void putArrayColumnComplexV  (const Array<Complex>* dataPtr);
  void putArrayColumnDComplexV (const Array<DComplex>* dataPtr);
  void putArrayColumnStringV   (const Array<String>* dataPtr);
  // </group>

  // Remove the value in the given row.
  void remove (uInt rownr);

  // Let the column create its arrays.
  void doCreate (uInt nrrow);

  // Check if the class invariants still hold.
  Bool ok() const;

private:
  // The (unique) sequence number of the column.
  uInt seqnr_p;
  // The shape of the array.
  IPosition shape_p;
  // The nr of elements in the array.
  uInt nrelem_p;


  // Delete the array in the given row.
  void deleteArray (uInt rownr);

  // Forbid copy constructor.
  MSMDirColumn (const MSMDirColumn&);

  // Forbid assignment.
  MSMDirColumn& operator= (const MSMDirColumn&);
};




} //# NAMESPACE CASACORE - END

#endif
