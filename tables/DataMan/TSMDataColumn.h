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
    uint32_t tilePixelSize() const;

    // Return the size of a pixel in the tile in local format.
    uint32_t localPixelSize() const;

    // Determine the length to store the given number of pixels.
    uint64_t dataLength (uint64_t nrPixels) const;

    // Set column sequence number.
    void setColumnNumber (uint32_t colnr);

    // Changing array shapes for non-FixedShape columns when the
    // parent tiled storage manager can handle it.
    bool canChangeShape() const;

    // Set the shape of the data array in the given row.
    // It will check if it matches already defined data and coordinates shapes.
    // It will define undefined data and coordinates shapes.
    void setShape (rownr_t rownr, const IPosition& shape);

    // Set the shape and tile shape of the array in the given row.
    // It will check if it matches already defined data and coordinates shapes.
    // It will define undefined data and coordinates shapes.
    // The tile shape is adjusted to the array shape (size 0 gets set to 1;
    // size > cubesize gets set to the cubesize).
    void setShapeTiled (rownr_t rownr, const IPosition& shape,
			const IPosition& tileShape);

    // Is the value shape defined in the given row?
    bool isShapeDefined (rownr_t rownr);

    // Get the shape of the item in the given row.
    IPosition shape (rownr_t rownr);

    // Get the tile shape of the item in the given row.
    IPosition tileShape (rownr_t rownr);

    // Get a scalar value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the Scalar/ArrayColumn get function).
    // <group>
    virtual void getBool     (rownr_t rownr, bool* dataPtr);
    virtual void getuChar    (rownr_t rownr, unsigned char* dataPtr);
    virtual void getShort    (rownr_t rownr, int16_t* dataPtr);
    virtual void getuShort   (rownr_t rownr, uint16_t* dataPtr);
    virtual void getInt      (rownr_t rownr, int32_t* dataPtr);
    virtual void getuInt     (rownr_t rownr, uint32_t* dataPtr);
    virtual void getInt64    (rownr_t rownr, int64_t* dataPtr);
    virtual void getfloat    (rownr_t rownr, float* dataPtr);
    virtual void getdouble   (rownr_t rownr, double* dataPtr);
    virtual void getComplex  (rownr_t rownr, Complex* dataPtr);
    virtual void getDComplex (rownr_t rownr, DComplex* dataPtr);
    // </group>

    // Put a scalar value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the Scalar/ArrayColumn put function).
    // <group>
    virtual void putBool     (rownr_t rownr, const bool* dataPtr);
    virtual void putuChar    (rownr_t rownr, const unsigned char* dataPtr);
    virtual void putShort    (rownr_t rownr, const int16_t* dataPtr);
    virtual void putuShort   (rownr_t rownr, const uint16_t* dataPtr);
    virtual void putInt      (rownr_t rownr, const int32_t* dataPtr);
    virtual void putuInt     (rownr_t rownr, const uint32_t* dataPtr);
    virtual void putInt64    (rownr_t rownr, const int64_t* dataPtr);
    virtual void putfloat    (rownr_t rownr, const float* dataPtr);
    virtual void putdouble   (rownr_t rownr, const double* dataPtr);
    virtual void putComplex  (rownr_t rownr, const Complex* dataPtr);
    virtual void putDComplex (rownr_t rownr, const DComplex* dataPtr);
    // </group>

    // Get the array value in the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn get function).
    virtual void getArrayV (rownr_t rownr, ArrayBase& data);

    // Put the array value into the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn put function).
    virtual void putArrayV (rownr_t rownr, const ArrayBase& data);

    // Get into a section of the array in the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn putSlice function).
    virtual void getSliceV (rownr_t rownr, const Slicer& slicer,
                    ArrayBase& dataPtr);

    // Put into a section of the array in the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn putSlice function).
    virtual void putSliceV (rownr_t rownr, const Slicer& slicer,
                    const ArrayBase& data);

    // Get all array values in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void getArrayColumnV (ArrayBase& arr);

    // Put all array values in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void putArrayColumnV (const ArrayBase& arr);

    // Get the array values in some cells of the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumnCells function).
    virtual void getArrayColumnCellsV (const RefRows& rownrs,
                                       ArrayBase& data);

    // Put the array values into some cells of the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void putArrayColumnCellsV (const RefRows& rownrs,
                                       const ArrayBase& dataPtr);

    // Get a section of all arrays in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void getColumnSliceV (const Slicer& slicer, ArrayBase& arr);

    // Put a section into all array values in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    virtual void putColumnSliceV (const Slicer& slicer,
				  const ArrayBase& data);
 
    // Get a section from some cells of the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumnCells function).
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
                                       const Slicer& ns,
                                       ArrayBase& data);

    // Put into a section of some cells of the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumnSlice function).
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
                                       const Slicer& ns,
                                       const ArrayBase& data);

    // Read the data of the column from a tile.
    // (I.e. convert from external to local format).
    void readTile (void* to, const void* from, uint32_t nrPixels);

    // Write the data of the column into a tile.
    // (I.e. convert from local to external format).
    void writeTile (void* to, const void* from, uint32_t nrPixels);

    // Get the function to convert from external to local format
    // (or vice-versa if <src>writeFlag=true</src>).
    Conversion::ValueFunction* getConvertFunction (bool writeFlag) const
      { return writeFlag ?  writeFunc_p : readFunc_p; }

    // Get nr of elements in a value to convert (usually 1, but 2 for Complex).
    size_t getNrConvert() const
      { return convPixelSize_p; }

    // Does a conversion (byte swap) needs to be done?
    bool isConversionNeeded() const
      { return mustConvert_p; }

private:
    // The (canonical) size of a pixel in a tile.
    uint32_t tilePixelSize_p;
    // The local size of a pixel.
    uint32_t localPixelSize_p;
    // The multiplication factor for a conversion operation.
    // This is the pixel size when a memcpy can be used, otherwise it is 1.
    uint32_t convPixelSize_p;
    // Is a conversion necessary?
    bool mustConvert_p;
    // The column sequence number.
    uint32_t colnr_p;
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
    void accessCell (rownr_t rownr,
		     const void* dataPtr, bool writeFlag);

    // Read or write a slice of a data cell in the cube.
    void accessCellSlice (rownr_t rownr, const Slicer& ns,
			  const void* dataPtr, bool writeFlag);

    // Read or write an entire column.
    // This can only be done if one hypercube is used.
    void accessColumn (const void* dataPtr, bool writeFlag);

    // Read or write a slice from the entire column.
    // This can only be done if one hypercube is used.
    void accessColumnSlice (const Slicer& ns,
			    const void* dataPtr, bool writeFlag);

    // Read or write some cells in a column.
    // It tries to optimize by looking for regular row strides.
    void accessColumnCells (const RefRows& rownrs,  const IPosition& shape,
			    const void* dataPtr, bool writeFlag);

    // Read or write some cells in a column.
    // It tries to optimize by looking for regular row strides.
    void accessColumnSliceCells (const RefRows& rownrs, const Slicer& ns,
				 const IPosition& shape,
				 const void* dataPtr, bool writeFlag);

    // Read or write the full cells given by start,end,incr.
    void accessFullCells (TSMCube* hypercube,
			  char* dataPtr, bool writeFlag,
			  const IPosition& start,
			  const IPosition& end,
			  const IPosition& incr);

    // Read or write the sliced cells given by start,end,incr.
    void accessSlicedCells (TSMCube* hypercube,
			    char* dataPtr, bool writeFlag,
			    const IPosition& start,
			    const IPosition& end,
			    const IPosition& incr);
};


inline uint32_t TSMDataColumn::tilePixelSize() const
{
    return tilePixelSize_p;
}
inline uint32_t TSMDataColumn::localPixelSize() const
{
    return localPixelSize_p;
}
inline void TSMDataColumn::setColumnNumber (uint32_t colnr)
{
    colnr_p = colnr;
}
inline void TSMDataColumn::readTile (void* to, const void* from,
				     uint32_t nrPixels)
{
    readFunc_p (to, from, nrPixels * convPixelSize_p);
}
inline void TSMDataColumn::writeTile (void* to, const void* from,
				      uint32_t nrPixels)
{
    writeFunc_p (to, from, nrPixels * convPixelSize_p);
}



} //# NAMESPACE CASACORE - END

#endif
