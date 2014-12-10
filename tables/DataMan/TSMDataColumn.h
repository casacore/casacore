//# TSMDataColumn.h: A data column in Tiled Storage Manager
//# Copyright (C) 1995,1996,1997,1999,2002
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

#ifndef TABLES_TSMDATACOLUMN_H
#define TABLES_TSMDATACOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/Conversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Slicer;


// <summary>
// A data column in Tiled Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TSMColumn>TSMColumn</linkto>
//   <li> <linkto class=TSMCube>TSMCube</linkto>
// </prerequisite>

// <etymology>
// TSMDataColumn handles a data column for a Tiled
// Storage Manager.
// </etymology>

// <synopsis> 
// TSMDataColumn is used by 
// <linkto class=TiledStMan>TiledStMan</linkto>
// to handle the access to
// a table column containing data of a tiled hypercube axis.
// The data in a cell can be a scalar or an array (depending on its
// definition in the table column description).
// The shapes of the coordinates and the data are related. Therefore
// the function setShape checks if the data shape matches the coordinates
// shape.
// <p>
// The data are held in a TSMCube object. The row number
// determines which TSMCube object has to be accessed.
// <p>
// The creation of a TSMDataColumn object is done by a TSMColumn object.
// This process is described in more detail in the class
// <linkto class=TSMColumn>TSMColumn</linkto>.
// </synopsis> 

// <motivation>
// Handling data columns in the Tiled Storage Manager is
// different from other columns.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TSMDataColumn : public TSMColumn
{
public:

    // Create a data column from the given column.
    TSMDataColumn (const TSMColumn& column);

    // Frees up the storage.
    virtual ~TSMDataColumn();

    // Return the size of a pixel in the tile in external format.
    uInt tilePixelSize() const;

    // Return the size of a pixel in the tile in local format.
    uInt localPixelSize() const;

    // Determine the length to store the given number of pixels.
    uInt dataLength (uInt nrPixels) const;

    // Set column sequence number.
    void setColumnNumber (uInt colnr);

    // It can handle access to a scalar column if there is one hypercube.
    Bool canAccessScalarColumn (Bool& reask) const;

    // It can handle access to an array column if there is one hypercube.
    Bool canAccessArrayColumn (Bool& reask) const;

    // It can handle access to a slice in a cell.
    Bool canAccessSlice (Bool& reask) const;

    // It can handle access to a slice in column if there is one hypercube.
    Bool canAccessColumnSlice (Bool& reask) const;

    // Changing array shapes for non-FixedShape columns when the
    // parent tiled storage manager can handle it.
    Bool canChangeShape() const;

    // Set the shape of the data array in the given row.
    // It will check if it matches already defined data and coordinates shapes.
    // It will define undefined data and coordinates shapes.
    void setShape (uInt rownr, const IPosition& shape);

    // Set the shape and tile shape of the array in the given row.
    // It will check if it matches already defined data and coordinates shapes.
    // It will define undefined data and coordinates shapes.
    // The tile shape is adjusted to the array shape (size 0 gets set to 1;
    // size > cubesize gets set to the cubesize).
    void setShapeTiled (uInt rownr, const IPosition& shape,
			const IPosition& tileShape);

    // Is the value shape defined in the given row?
    Bool isShapeDefined (uInt rownr);

    // Get the shape of the item in the given row.
    IPosition shape (uInt rownr);

    // Get a scalar value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the Scalar/ArrayColumn get function).
    // <group>
    void getBoolV     (uInt rownr, Bool* dataPtr);
    void getuCharV    (uInt rownr, uChar* dataPtr);
    void getShortV    (uInt rownr, Short* dataPtr);
    void getuShortV   (uInt rownr, uShort* dataPtr);
    void getIntV      (uInt rownr, Int* dataPtr);
    void getuIntV     (uInt rownr, uInt* dataPtr);
    void getfloatV    (uInt rownr, float* dataPtr);
    void getdoubleV   (uInt rownr, double* dataPtr);
    void getComplexV  (uInt rownr, Complex* dataPtr);
    void getDComplexV (uInt rownr, DComplex* dataPtr);
    // </group>

    // Put a scalar value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the Scalar/ArrayColumn put function).
    // <group>
    void putBoolV     (uInt rownr, const Bool* dataPtr);
    void putuCharV    (uInt rownr, const uChar* dataPtr);
    void putShortV    (uInt rownr, const Short* dataPtr);
    void putuShortV   (uInt rownr, const uShort* dataPtr);
    void putIntV      (uInt rownr, const Int* dataPtr);
    void putuIntV     (uInt rownr, const uInt* dataPtr);
    void putfloatV    (uInt rownr, const float* dataPtr);
    void putdoubleV   (uInt rownr, const double* dataPtr);
    void putComplexV  (uInt rownr, const Complex* dataPtr);
    void putDComplexV (uInt rownr, const DComplex* dataPtr);
    // </group>

    // Get the array value in the given row.
    // The array pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    // The default implementation thrown an "invalid operation exception".
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
    // </group>

    // Put the array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    // The default implementation thrown an "invalid operation exception".
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
    // </group>

    void getSliceBoolV     (uInt rownr, const Slicer& slicer,
			    Array<Bool>* dataPtr);
    void getSliceuCharV    (uInt rownr, const Slicer& slicer,
			    Array<uChar>* dataPtr);
    void getSliceShortV    (uInt rownr, const Slicer& slicer,
			    Array<Short>* dataPtr);
    void getSliceuShortV   (uInt rownr, const Slicer& slicer,
			    Array<uShort>* dataPtr);
    void getSliceIntV      (uInt rownr, const Slicer& slicer,
			    Array<Int>* dataPtr);
    void getSliceuIntV     (uInt rownr, const Slicer& slicer,
			    Array<uInt>* dataPtr);
    void getSlicefloatV    (uInt rownr, const Slicer& slicer,
			    Array<float>* dataPtr);
    void getSlicedoubleV   (uInt rownr, const Slicer& slicer,
			    Array<double>* dataPtr);
    void getSliceComplexV  (uInt rownr, const Slicer& slicer,
			    Array<Complex>* dataPtr);
    void getSliceDComplexV (uInt rownr, const Slicer& slicer,
			    Array<DComplex>* dataPtr);

    void putSliceBoolV     (uInt rownr, const Slicer& slicer,
			    const Array<Bool>* dataPtr);
    void putSliceuCharV    (uInt rownr, const Slicer& slicer,
			    const Array<uChar>* dataPtr);
    void putSliceShortV    (uInt rownr, const Slicer& slicer,
			    const Array<Short>* dataPtr);
    void putSliceuShortV   (uInt rownr, const Slicer& slicer,
			    const Array<uShort>* dataPtr);
    void putSliceIntV      (uInt rownr, const Slicer& slicer,
			    const Array<Int>* dataPtr);
    void putSliceuIntV     (uInt rownr, const Slicer& slicer,
			    const Array<uInt>* dataPtr);
    void putSlicefloatV    (uInt rownr, const Slicer& slicer,
			    const Array<float>* dataPtr);
    void putSlicedoubleV   (uInt rownr, const Slicer& slicer,
			    const Array<double>* dataPtr);
    void putSliceComplexV  (uInt rownr, const Slicer& slicer,
			    const Array<Complex>* dataPtr);
    void putSliceDComplexV (uInt rownr, const Slicer& slicer,
			    const Array<DComplex>* dataPtr);

    void getScalarColumnBoolV     (Vector<Bool>* arr);
    void getScalarColumnuCharV    (Vector<uChar>* arr);
    void getScalarColumnShortV    (Vector<Short>* arr);
    void getScalarColumnuShortV   (Vector<uShort>* arr);
    void getScalarColumnIntV      (Vector<Int>* arr);
    void getScalarColumnuIntV     (Vector<uInt>* arr);
    void getScalarColumnfloatV    (Vector<float>* arr);
    void getScalarColumndoubleV   (Vector<double>* arr);
    void getScalarColumnComplexV  (Vector<Complex>* arr);
    void getScalarColumnDComplexV (Vector<DComplex>* arr);

    void putScalarColumnBoolV     (const Vector<Bool>* arr);
    void putScalarColumnuCharV    (const Vector<uChar>* arr);
    void putScalarColumnShortV    (const Vector<Short>* arr);
    void putScalarColumnuShortV   (const Vector<uShort>* arr);
    void putScalarColumnIntV      (const Vector<Int>* arr);
    void putScalarColumnuIntV     (const Vector<uInt>* arr);
    void putScalarColumnfloatV    (const Vector<float>* arr);
    void putScalarColumndoubleV   (const Vector<double>* arr);
    void putScalarColumnComplexV  (const Vector<Complex>* arr);
    void putScalarColumnDComplexV (const Vector<DComplex>* arr);

    // Get the scalar values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumnCells function).
    // The default implementation loops through all rows.
    // <group>
    virtual void getScalarColumnCellsBoolV     (const RefRows& rownrs,
						Vector<Bool>* dataPtr);
    virtual void getScalarColumnCellsuCharV    (const RefRows& rownrs,
						Vector<uChar>* dataPtr);
    virtual void getScalarColumnCellsShortV    (const RefRows& rownrs,
						Vector<Short>* dataPtr);
    virtual void getScalarColumnCellsuShortV   (const RefRows& rownrs,
						Vector<uShort>* dataPtr);
    virtual void getScalarColumnCellsIntV      (const RefRows& rownrs,
						Vector<Int>* dataPtr);
    virtual void getScalarColumnCellsuIntV     (const RefRows& rownrs,
						Vector<uInt>* dataPtr);
    virtual void getScalarColumnCellsfloatV    (const RefRows& rownrs,
						Vector<float>* dataPtr);
    virtual void getScalarColumnCellsdoubleV   (const RefRows& rownrs,
						Vector<double>* dataPtr);
    virtual void getScalarColumnCellsComplexV  (const RefRows& rownrs,
						Vector<Complex>* dataPtr);
    virtual void getScalarColumnCellsDComplexV (const RefRows& rownrs,
						Vector<DComplex>* dataPtr);
    // </group>

    // Put the scalar values into some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn putColumnCells function).
    // The default implementation loops through all rows.
    // <group>
    virtual void putScalarColumnCellsBoolV     (const RefRows& rownrs,
						const Vector<Bool>* dataPtr);
    virtual void putScalarColumnCellsuCharV    (const RefRows& rownrs,
						const Vector<uChar>* dataPtr);
    virtual void putScalarColumnCellsShortV    (const RefRows& rownrs,
						const Vector<Short>* dataPtr);
    virtual void putScalarColumnCellsuShortV   (const RefRows& rownrs,
						const Vector<uShort>* dataPtr);
    virtual void putScalarColumnCellsIntV      (const RefRows& rownrs,
						const Vector<Int>* dataPtr);
    virtual void putScalarColumnCellsuIntV     (const RefRows& rownrs,
						const Vector<uInt>* dataPtr);
    virtual void putScalarColumnCellsfloatV    (const RefRows& rownrs,
						const Vector<float>* dataPtr);
    virtual void putScalarColumnCellsdoubleV   (const RefRows& rownrs,
						const Vector<double>* dataPtr);
    virtual void putScalarColumnCellsComplexV  (const RefRows& rownrs,
						const Vector<Complex>* dataPtr);
    virtual void putScalarColumnCellsDComplexV (const RefRows& rownrs,
					       const Vector<DComplex>* dataPtr);
    // </group>

    void getArrayColumnBoolV     (Array<Bool>* arr);
    void getArrayColumnuCharV    (Array<uChar>* arr);
    void getArrayColumnShortV    (Array<Short>* arr);
    void getArrayColumnuShortV   (Array<uShort>* arr);
    void getArrayColumnIntV      (Array<Int>* arr);
    void getArrayColumnuIntV     (Array<uInt>* arr);
    void getArrayColumnfloatV    (Array<float>* arr);
    void getArrayColumndoubleV   (Array<double>* arr);
    void getArrayColumnComplexV  (Array<Complex>* arr);
    void getArrayColumnDComplexV (Array<DComplex>* arr);

    void putArrayColumnBoolV     (const Array<Bool>* arr);
    void putArrayColumnuCharV    (const Array<uChar>* arr);
    void putArrayColumnShortV    (const Array<Short>* arr);
    void putArrayColumnuShortV   (const Array<uShort>* arr);
    void putArrayColumnIntV      (const Array<Int>* arr);
    void putArrayColumnuIntV     (const Array<uInt>* arr);
    void putArrayColumnfloatV    (const Array<float>* arr);
    void putArrayColumndoubleV   (const Array<double>* arr);
    void putArrayColumnComplexV  (const Array<Complex>* arr);
    void putArrayColumnDComplexV (const Array<DComplex>* arr);

    // Get the array values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn getColumnCells function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void getArrayColumnCellsBoolV     (const RefRows& rownrs,
					       Array<Bool>* dataPtr);
    virtual void getArrayColumnCellsuCharV    (const RefRows& rownrs,
					       Array<uChar>* dataPtr);
    virtual void getArrayColumnCellsShortV    (const RefRows& rownrs,
					       Array<Short>* dataPtr);
    virtual void getArrayColumnCellsuShortV   (const RefRows& rownrs,
					       Array<uShort>* dataPtr);
    virtual void getArrayColumnCellsIntV      (const RefRows& rownrs,
					       Array<Int>* dataPtr);
    virtual void getArrayColumnCellsuIntV     (const RefRows& rownrs,
					       Array<uInt>* dataPtr);
    virtual void getArrayColumnCellsfloatV    (const RefRows& rownrs,
					       Array<float>* dataPtr);
    virtual void getArrayColumnCellsdoubleV   (const RefRows& rownrs,
					       Array<double>* dataPtr);
    virtual void getArrayColumnCellsComplexV  (const RefRows& rownrs,
					       Array<Complex>* dataPtr);
    virtual void getArrayColumnCellsDComplexV (const RefRows& rownrs,
					       Array<DComplex>* dataPtr);
    // </group>

    // Put the array values into some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn putColumnCells function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void putArrayColumnCellsBoolV     (const RefRows& rownrs,
					       const Array<Bool>* dataPtr);
    virtual void putArrayColumnCellsuCharV    (const RefRows& rownrs,
					       const Array<uChar>* dataPtr);
    virtual void putArrayColumnCellsShortV    (const RefRows& rownrs,
					       const Array<Short>* dataPtr);
    virtual void putArrayColumnCellsuShortV   (const RefRows& rownrs,
					       const Array<uShort>* dataPtr);
    virtual void putArrayColumnCellsIntV      (const RefRows& rownrs,
					       const Array<Int>* dataPtr);
    virtual void putArrayColumnCellsuIntV     (const RefRows& rownrs,
					       const Array<uInt>* dataPtr);
    virtual void putArrayColumnCellsfloatV    (const RefRows& rownrs,
					       const Array<float>* dataPtr);
    virtual void putArrayColumnCellsdoubleV   (const RefRows& rownrs,
					       const Array<double>* dataPtr);
    virtual void putArrayColumnCellsComplexV  (const RefRows& rownrs,
					       const Array<Complex>* dataPtr);
    virtual void putArrayColumnCellsDComplexV (const RefRows& rownrs,
					       const Array<DComplex>* dataPtr);
    // </group>

    void getColumnSliceBoolV     (const Slicer& slicer, Array<Bool>* arr);
    void getColumnSliceuCharV    (const Slicer& slicer, Array<uChar>* arr);
    void getColumnSliceShortV    (const Slicer& slicer, Array<Short>* arr);
    void getColumnSliceuShortV   (const Slicer& slicer, Array<uShort>* arr);
    void getColumnSliceIntV      (const Slicer& slicer, Array<Int>* arr);
    void getColumnSliceuIntV     (const Slicer& slicer, Array<uInt>* arr);
    void getColumnSlicefloatV    (const Slicer& slicer, Array<float>* arr);
    void getColumnSlicedoubleV   (const Slicer& slicer, Array<double>* arr);
    void getColumnSliceComplexV  (const Slicer& slicer, Array<Complex>* arr);
    void getColumnSliceDComplexV (const Slicer& slicer, Array<DComplex>* arr);

    void putColumnSliceBoolV     (const Slicer& slicer,
				  const Array<Bool>* dataPtr);
    void putColumnSliceuCharV    (const Slicer& slicer,
				  const Array<uChar>* dataPtr);
    void putColumnSliceShortV    (const Slicer& slicer,
				  const Array<Short>* dataPtr);
    void putColumnSliceuShortV   (const Slicer& slicer,
				  const Array<uShort>* dataPtr);
    void putColumnSliceIntV      (const Slicer& slicer,
				  const Array<Int>* dataPtr);
    void putColumnSliceuIntV     (const Slicer& slicer,
				  const Array<uInt>* dataPtr);
    void putColumnSlicefloatV    (const Slicer& slicer,
				  const Array<float>* dataPtr);
    void putColumnSlicedoubleV   (const Slicer& slicer,
				  const Array<double>* dataPtr);
    void putColumnSliceComplexV  (const Slicer& slicer,
				  const Array<Complex>* dataPtr);
    void putColumnSliceDComplexV (const Slicer& slicer,
				  const Array<DComplex>* dataPtr);
 
    // Get the array values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn getColumnCells function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void getColumnSliceCellsBoolV     (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Bool>* dataPtr);
    virtual void getColumnSliceCellsuCharV    (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<uChar>* dataPtr);
    virtual void getColumnSliceCellsShortV    (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Short>* dataPtr);
    virtual void getColumnSliceCellsuShortV   (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<uShort>* dataPtr);
    virtual void getColumnSliceCellsIntV      (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Int>* dataPtr);
    virtual void getColumnSliceCellsuIntV     (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<uInt>* dataPtr);
    virtual void getColumnSliceCellsfloatV    (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<float>* dataPtr);
    virtual void getColumnSliceCellsdoubleV   (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<double>* dataPtr);
    virtual void getColumnSliceCellsComplexV  (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<Complex>* dataPtr);
    virtual void getColumnSliceCellsDComplexV (const RefRows& rownrs,
					       const Slicer& ns,
					       Array<DComplex>* dataPtr);
    // </group>

    // Put the array values into some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ArrayColumn putColumnSlice function).
    // The default implementation throws an "invalid operation exception".
    // <group>
    virtual void putColumnSliceCellsBoolV     (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Bool>* dataPtr);
    virtual void putColumnSliceCellsuCharV    (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<uChar>* dataPtr);
    virtual void putColumnSliceCellsShortV    (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Short>* dataPtr);
    virtual void putColumnSliceCellsuShortV   (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<uShort>* dataPtr);
    virtual void putColumnSliceCellsIntV      (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Int>* dataPtr);
    virtual void putColumnSliceCellsuIntV     (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<uInt>* dataPtr);
    virtual void putColumnSliceCellsfloatV    (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<float>* dataPtr);
    virtual void putColumnSliceCellsdoubleV   (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<double>* dataPtr);
    virtual void putColumnSliceCellsComplexV  (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<Complex>* dataPtr);
    virtual void putColumnSliceCellsDComplexV (const RefRows& rownrs,
					       const Slicer& ns,
					       const Array<DComplex>* dataPtr);
    // </group>

    // Read the data of the column from a tile.
    // (I.e. convert from external to local format).
    void readTile (void* to, const void* from, uInt nrPixels);

    // Write the data of the column into a tile.
    // (I.e. convert from local to external format).
    void writeTile (void* to, const void* from, uInt nrPixels);

    // Get the function to convert from external to local format
    // (or vice-versa if <src>writeFlag=True</src>).
    Conversion::ValueFunction* getConvertFunction (Bool writeFlag) const
      { return writeFlag ?  writeFunc_p : readFunc_p; }

    // Get nr of elements in a value to convert (usually 1, but 2 for Complex).
    size_t getNrConvert() const
      { return convPixelSize_p; }

    // Does a conversion (byte swap) needs to be done?
    Bool isConversionNeeded() const
      { return mustConvert_p; }

private:
    // The (canonical) size of a pixel in a tile.
    uInt tilePixelSize_p;
    // The local size of a pixel.
    uInt localPixelSize_p;
    // The multiplication factor for a conversion operation.
    // This is the pixel size when a memcpy can be used, otherwise it is 1.
    uInt convPixelSize_p;
    // Is a conversion necessary?
    Bool mustConvert_p;
    // The column sequence number.
    uInt colnr_p;
    // The conversion function needed when reading.
    Conversion::ValueFunction* readFunc_p;
    // The conversion function needed when writing.
    Conversion::ValueFunction* writeFunc_p;


    // Forbid copy constructor.
    TSMDataColumn (const TSMDataColumn&);

    // Forbid assignment.
    TSMDataColumn& operator= (const TSMDataColumn&);

    // Read or write a data cell in the cube.
    // A cell can contain a scalar or an array (depending on the
    // column definition).
    void accessCell (uInt rownr,
		     const void* dataPtr, Bool writeFlag);

    // Read or write a slice of a data cell in the cube.
    void accessCellSlice (uInt rownr, const Slicer& ns,
			  const void* dataPtr, Bool writeFlag);

    // Read or write an entire column.
    // This can only be done if one hypercube is used.
    void accessColumn (const void* dataPtr, Bool writeFlag);

    // Read or write a slice from the entire column.
    // This can only be done if one hypercube is used.
    void accessColumnSlice (const Slicer& ns,
			    const void* dataPtr, Bool writeFlag);

    // Read or write some cells in a column.
    // It tries to optimize by looking for regular row strides.
    void accessColumnCells (const RefRows& rownrs,  const IPosition& shape,
			    const void* dataPtr, Bool writeFlag);

    // Read or write some cells in a column.
    // It tries to optimize by looking for regular row strides.
    void accessColumnSliceCells (const RefRows& rownrs, const Slicer& ns,
				 const IPosition& shape,
				 const void* dataPtr, Bool writeFlag);

    // Read or write the full cells given by start,end,incr.
    void accessFullCells (TSMCube* hypercube,
			  char* dataPtr, Bool writeFlag,
			  const IPosition& start,
			  const IPosition& end,
			  const IPosition& incr);

    // Read or write the sliced cells given by start,end,incr.
    void accessSlicedCells (TSMCube* hypercube,
			    char* dataPtr, Bool writeFlag,
			    const IPosition& start,
			    const IPosition& end,
			    const IPosition& incr);
};


inline uInt TSMDataColumn::tilePixelSize() const
{
    return tilePixelSize_p;
}
inline uInt TSMDataColumn::localPixelSize() const
{
    return localPixelSize_p;
}
inline void TSMDataColumn::setColumnNumber (uInt colnr)
{
    colnr_p = colnr;
}
inline void TSMDataColumn::readTile (void* to, const void* from,
				     uInt nrPixels)
{
    readFunc_p (to, from, nrPixels * convPixelSize_p);
}
inline void TSMDataColumn::writeTile (void* to, const void* from,
				      uInt nrPixels)
{
    writeFunc_p (to, from, nrPixels * convPixelSize_p);
}



} //# NAMESPACE CASACORE - END

#endif
