//# TSMDataColumn.h: A data column in Tiled Storage Manager
//# Copyright (C) 1995,1996,1997,1999
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

#if !defined(AIPS_TSMDATACOLUMN_H)
#define AIPS_TSMDATACOLUMN_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/TSMColumn.h>
#include <aips/Tables/TSMCube.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/String.h>
#include <aips/OS/Conversion.h>

//# Forward Declarations
class Slicer;


// <summary>
// A data column in Tiled Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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
 
    // Read the data of the column from a tile.
    // (I.e. convert from external to local format).
    void readTile (void* to, const void* from, uInt nrPixels);

    // Write the data of the column into a tile.
    // (I.e. convert from local to external format).
    void writeTile (void* to, const void* from, uInt nrPixels);


private:
    // The (canonical) size of a pixel in a tile.
    uInt tilePixelSize_p;
    // The local size of a pixel.
    uInt localPixelSize_p;
    // The multiplication factor for a conversion operation.
    // This is the pixel size when a memcpy can be used, otherwise it is 1.
    uInt convPixelSize_p;
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


#endif
