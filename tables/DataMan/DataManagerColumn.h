//# DataManagerColumn.h: Abstract base class for a data manager column
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2001,2002
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

#ifndef TABLES_DATAMANAGERCOLUMN_H
#define TABLES_DATAMANAGERCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ColumnCache.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class IPosition;
class Slicer;
class RefRows;
class ArrayBase;


// <summary>
// Abstract base class for a column in a data manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManager
// </prerequisite>

// <etymology>
// DataManagerColumn handles a column for a data manager.
// </etymology>

// <synopsis> 
// DataManagerColumn is the abstract base class to handle a column in
// a data manager. Each data manager class must have one or more associated
// classes derived from DataManagerColumn to handle the columns.
// For example, storage manager StManAipsIO has columns classes
// StManColumnAipsIO, StManColumnArrayAipsIO and StManColumnIndArrayAipsIO
// to handle scalars, direct arrays and indirect arrays, resp..
// However, using multiple inheritance it is possible that the derived
// DataManager and DataManagerColumn classes are the same. This is used
// in class ScaledArrayEngine<S,T> which represents both the data manager
// and its column class. It can do that, because the virtual column engine
// <linkto class="ScaledArrayEngine:description">ScaledArrayEngine</linkto>
// can handle only one column.
//
// In the synopsis of class DataManager it is described how the (derived)
// DataManagerColumn objects gets created and deleted.
// 
// DataManagerColumn defines various virtual functions to get or put (slices)
// of data in a column. These functions are called by the table column
// classes ScalarColumnData and ArrayColumnData.
// It does not define functions create, open, flush and prepare like
// those defined in DataManager. It is left to the derived classes to
// define those as needed and to interact properly with their
// data manager object.
//
// The get/put interface has changed per 1-Sep-2017.
// The old interface for ArrayColumn::getArray worked as follows:
// <ol>
//  <li>ArrayColumn calls (virtual) BaseColumn::get passing the array as a void*.
//     void* is used to support the derived RefColumn class which is not templated.
//  <li>BaseColumn::get calls (virtual) DataManagerColumn::getArrayV.
//      This function can be implemented by a derived storage manager or
//      virtual column engine class.
//  <li>Storage managers derive from StManColumn. Its getArrayV function calls
//      the appropriate getArrayXXV function where XX is the data type (e.g. Int).
//      These getArrayXXV functions are implemented in the storage managers.
//  <li>Virtual column engines derive from the templated VirtArrCol class which
//      implements getArrayV by calling a templated virtual getArray function.
// </ol>
// The old interface for a function such as getArrayColumn works more or less
// the same. However, this function does not need to be implemented by a data manager.
// ArrayColumn will first ask the data manager if it supports getting an entire
// array column. If not, ArrayColumn will call getArray for each row.
// Functions such as getSlice, etc. work similarly.
//
// A new interface has been developed which should result in a smaller code base
// and simpler classes. The new interface could be developed thanks to same
// Array enhancements making it possible to use quite some Array functionality
// in non-templated classes.
// The new interface works differently in a number of points:
// <ul>
//  <li> Arrays are passed as ArrayBase* instead of void* making it possible to
//       get shapes, etc. in a non-templated way.
//  <li> ArrayColumn does not ask anymore if a data manager supports getArrayColumn.
//       Instead, the default implementation in DataManagerColumn::getArrayColumnV
//       will call getArrayV repetitively.
//  <li> The StManColumn interface is not really necessary anymore.
// </ul>
//
// However, some plug-in data managers exist outside the Casacore repository
// (e.g., LofarStMan and AdiosStMan). It should be possible to build and use them
// for some time with the old and new interface. To make this possible the new
// interface has to be backward compatible for some time. This is achieved by:
// <ul>
//  <li> StManColumn is maintained (but getArrayV takes ArrayBase&, not void*).
//       It calls getArrayXXV(void*) depending on the data type.
//       A storage manager can implemented getArrayV itself bypassing StManColumn.
//  <li> Functions such as getArrayColumn are a bit more complicated.
//       StManColumn::getArrayColumnV calls getArrayColumnXXV, which calls
//       DataManager::getArrayColumnAB doing the getArrayV per row.
//       A derived class can have getArrayColumnXXV implemented.
// </ul>
// </synopsis> 

// <motivation>
// An abstract base class is needed to support multiple data
// managers in the table system
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class DataManagerColumn
{
public:

    // Create a column.
    DataManagerColumn()
	: isFixedShape_p(False)
    {}

    // Frees up the storage.
    virtual ~DataManagerColumn();

    // Set the isFixedShape flag.
    void setIsFixedShape (Bool isFixedShape)
        { isFixedShape_p = isFixedShape; }

    // Is this a fixed shape column?
    Bool isFixedShape() const
        { return isFixedShape_p; }

    // Get the data type of the column as defined in DataType.h.
    virtual int dataType() const = 0;

    // Get the data type id of the column for dataType==TpOther.
    // The default implementation returns an emptry string.
    // This function is required for virtual column engines handling
    // non-standard data types. It is used to check the data type.
    virtual String dataTypeId() const;

    // Test if data can be put into this column.
    // This does not test if the data file is writable, only if
    // it is in principle allowed to store data into the column.
    // (It may not be allowed for virtual columns).
    // The default is True.
    virtual Bool isWritable() const;

    // Set the maximum length of the value (can be used for strings).
    // By default the maximum length is ignored.
    virtual void setMaxLength (uInt maxLength);

    // Set the shape of all (fixed-shaped) arrays in the column.
    // Effectively it is the same as setShapeColumn, but it also sets
    // the isFixedShape_p flag.
    void setFixedShapeColumn (const IPosition& shape)
        { setShapeColumn (shape); isFixedShape_p = True; }

    // Set the shape of an (variable-shaped) array in the given row.
    // By default it throws a "not possible" exception.
    virtual void setShape (uInt rownr, const IPosition& shape);

    // Set the shape and tile shape of an (variable-shaped) array
    // in the given row.
    // By default it ignores the tile shape (thus only sets the shape).
    virtual void setShapeTiled (uInt rownr, const IPosition& shape,
				const IPosition& tileShape);

    // Is the value shape defined in the given row?
    // By default it returns True.
    virtual Bool isShapeDefined (uInt rownr);

    // Get the dimensionality of the item in the given row.
    // By default it returns shape(rownr).nelements().
    virtual uInt ndim (uInt rownr);

    // Get the shape of the item in the given row.
    // By default it returns a zero-length IPosition (for a scalar value).
    virtual IPosition shape (uInt rownr);

    // Get the tile shape of the item in the given row.
    // By default it returns a zero-length IPosition.
    virtual IPosition tileShape (uInt rownr);

    // Can the data manager handle chaging the shape of an existing array?
    // Default is no.
    virtual Bool canChangeShape() const;

    // Get access to the ColumnCache object.
    // <group>
    ColumnCache& columnCache()
        { return colCache_p; }
    const ColumnCache* columnCachePtr() const
        { return &colCache_p; }
    // </group>

    // Get the scalar value in the given row.
    // These functions are non-virtual and are converted to their
    // virtual getXXV equivalent to achieve that a derived templated class
    // (such as VirtualScalarColumn) does not have to declare and implement
    // all these functions.
    // The compiler complains about hiding virtual functions if you do not
    // declare all virtual functions with the same name in a derived class.
    // <group>
    void get (uInt rownr, Bool* dataPtr)
	{ getBoolV (rownr, dataPtr); }
    void get (uInt rownr, uChar* dataPtr)
	{ getuCharV (rownr, dataPtr); }
    void get (uInt rownr, Short* dataPtr)
	{ getShortV (rownr, dataPtr); }
    void get (uInt rownr, uShort* dataPtr)
	{ getuShortV (rownr, dataPtr); }
    void get (uInt rownr, Int* dataPtr)
	{ getIntV (rownr, dataPtr); }
    void get (uInt rownr, uInt* dataPtr)
	{ getuIntV (rownr, dataPtr); }
    void get (uInt rownr, Int64* dataPtr)
	{ getInt64V (rownr, dataPtr); }
    void get (uInt rownr, float* dataPtr)
	{ getfloatV (rownr, dataPtr); } 
   void get (uInt rownr, double* dataPtr)
	{ getdoubleV (rownr, dataPtr); }
    void get (uInt rownr, Complex* dataPtr)
	{ getComplexV (rownr, dataPtr); }
    void get (uInt rownr, DComplex* dataPtr)
	{ getDComplexV (rownr, dataPtr); }
    void get (uInt rownr, String* dataPtr)
	{ getStringV (rownr, dataPtr); }
    // This function is the get for all non-standard data types.
    void get (uInt rownr, void* dataPtr)
	{ getOtherV (rownr, dataPtr); }
    // </group>

    // Put the scalar value into the given row.
    // These functions are non-virtual and are converted to their
    // virtual putXXV equivalent to achieve that a derived templated class
    // (such as VirtualScalarColumn) does not have to declare and implement
    // all these functions.
    // The compiler complains about hiding virtual functions if you do not
    // declare all virtual functions with the same name in a derived class.
    // <group>
    void put (uInt rownr, const Bool* dataPtr)
	{ putBoolV (rownr, dataPtr); }
    void put (uInt rownr, const uChar* dataPtr)
	{ putuCharV (rownr, dataPtr); }
    void put (uInt rownr, const Short* dataPtr)
	{ putShortV (rownr, dataPtr); }
    void put (uInt rownr, const uShort* dataPtr)
	{ putuShortV (rownr, dataPtr); }
    void put (uInt rownr, const Int* dataPtr)
	{ putIntV (rownr, dataPtr); }
    void put (uInt rownr, const uInt* dataPtr)
	{ putuIntV (rownr, dataPtr); }
    void put (uInt rownr, const Int64* dataPtr)
	{ putInt64V (rownr, dataPtr); }
    void put (uInt rownr, const float* dataPtr)
	{ putfloatV (rownr, dataPtr); }
    void put (uInt rownr, const double* dataPtr)
	{ putdoubleV (rownr, dataPtr); }
    void put (uInt rownr, const Complex* dataPtr)
	{ putComplexV (rownr, dataPtr); }
    void put (uInt rownr, const DComplex* dataPtr)
	{ putDComplexV (rownr, dataPtr); }
    void put (uInt rownr, const String* dataPtr)
	{ putStringV (rownr, dataPtr); }
    // This function is the put for all non-standard data types.
    void put (uInt rownr, const void* dataPtr)
	{ putOtherV (rownr, dataPtr); }
    // </group>

    // Get all scalar values in the column.
    // The vector given in <src>data</src> has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation does a getXXV per row.
    virtual void getScalarColumnV (ArrayBase& dataPtr);

    // Put all scalar values in the column.
    // The vector given in <src>data</src> has to have the correct length
    // (which is guaranteed by the ScalarColumn putColumn function).
    // The default implementation does a putXXV per row.
    virtual void putScalarColumnV (const ArrayBase& dataPtr);

    // Get some scalar values in the column.
    // The vector given in <src>data</src> has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation does a getXXV per row.
    virtual void getScalarColumnCellsV (const RefRows& rownrs,
					ArrayBase& dataPtr);

    // Put some scalar values in the column.
    // The vector given in <src>data</src> has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    // The default implementation does a putXXV per row.
    virtual void putScalarColumnCellsV (const RefRows& rownrs,
					const ArrayBase& dataPtr);

    // Get the array value in the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn get function).
    // The default implementation throws an "invalid operation" exception.
    virtual void getArrayV (uInt rownr, ArrayBase& dataPtr);

    // Put the array value into the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn put function).
    // The default implementation throws an "invalid operation" exception.
    virtual void putArrayV (uInt rownr, const ArrayBase& data);

    // Get all array values in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation does a getArrayV per row.
    virtual void getArrayColumnV (ArrayBase& data);

    // Put all array values in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation does a putArrayV per row.
    virtual void putArrayColumnV (const ArrayBase& data);

    // Get some array values in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation does a getArrayV per row.
    virtual void getArrayColumnCellsV (const RefRows& rownrs,
				       ArrayBase& data);

    // Put some array values in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation does a putArrayV per row.
    virtual void putArrayColumnCellsV (const RefRows& rownrs,
				       const ArrayBase& data);

    // Get a section of the array in the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getSlice function).
    // The default implementation does getArrayV and takes the slice.
    virtual void getSliceV (uInt rownr, const Slicer& slicer, ArrayBase& data);

    // Put into a section of the array in the given row.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn putSlice function).
    // The default implementation does get/putArrayV and puts the slice.
    virtual void putSliceV (uInt rownr, const Slicer& slicer,
			    const ArrayBase& data);

    // Get a section of all arrays in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation does a getSliceV per row.
    virtual void getColumnSliceV (const Slicer& slicer, ArrayBase& data);

    // Put into a section of all arrays in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation does a putSliceV per row.
    virtual void putColumnSliceV (const Slicer& slicer, const ArrayBase& data);

    // Get a section of some arrays in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn getColumn function).
    // The default implementation does a getSliceV per row.
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer, ArrayBase& data);

    // Put into a section of some arrays in the column.
    // The array given in <src>data</src> has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation does a putSliceV per row.
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer,
				       const ArrayBase& data);

    // Throw an "invalid operation" exception for the default
    // implementation of get.
    void throwGet() const;

    // Throw an "invalid operation" exception for the default
    // implementation of put.
    void throwPut() const;

    // Set the column name.
    void setColumnName (const String& colName)
      { colName_p = colName; }

    // Get rhe column name.
    const String& columnName() const
      { return colName_p; }

protected:
    // Get the scalar value in the given row.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    virtual void getBoolV     (uInt rownr, Bool* dataPtr);
    virtual void getuCharV    (uInt rownr, uChar* dataPtr);
    virtual void getShortV    (uInt rownr, Short* dataPtr);
    virtual void getuShortV   (uInt rownr, uShort* dataPtr);
    virtual void getIntV      (uInt rownr, Int* dataPtr);
    virtual void getuIntV     (uInt rownr, uInt* dataPtr);
    virtual void getInt64V    (uInt rownr, Int64* dataPtr);
    virtual void getfloatV    (uInt rownr, float* dataPtr);
    virtual void getdoubleV   (uInt rownr, double* dataPtr);
    virtual void getComplexV  (uInt rownr, Complex* dataPtr);
    virtual void getDComplexV (uInt rownr, DComplex* dataPtr);
    virtual void getStringV   (uInt rownr, String* dataPtr);
    // This function is the get for all non-standard data types.
    virtual void getOtherV    (uInt rownr, void* dataPtr);
    // </group>

    // Put the scalar value into the given row.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    virtual void putBoolV     (uInt rownr, const Bool* dataPtr);
    virtual void putuCharV    (uInt rownr, const uChar* dataPtr);
    virtual void putShortV    (uInt rownr, const Short* dataPtr);
    virtual void putuShortV   (uInt rownr, const uShort* dataPtr);
    virtual void putIntV      (uInt rownr, const Int* dataPtr);
    virtual void putuIntV     (uInt rownr, const uInt* dataPtr);
    virtual void putInt64V    (uInt rownr, const Int64* dataPtr);
    virtual void putfloatV    (uInt rownr, const float* dataPtr);
    virtual void putdoubleV   (uInt rownr, const double* dataPtr);
    virtual void putComplexV  (uInt rownr, const Complex* dataPtr);
    virtual void putDComplexV (uInt rownr, const DComplex* dataPtr);
    virtual void putStringV   (uInt rownr, const String* dataPtr);
    // This function is the put for all non-standard data types.
    virtual void putOtherV    (uInt rownr, const void* dataPtr);
    // </group>

    // The default implementations of get and put functions.
    // <group>
    void dmGetScalarColumnV (ArrayBase& dataPtr);
    void dmPutScalarColumnV (const ArrayBase& dataPtr);
    void dmGetScalarColumnCellsV (const RefRows& rownrs, ArrayBase& dataPtr);
    void dmPutScalarColumnCellsV (const RefRows& rownrs, const ArrayBase& dataPtr);
    void dmGetArrayColumnV (ArrayBase& data);
    void dmPutArrayColumnV (const ArrayBase& data);
    void dmGetArrayColumnCellsV (const RefRows& rownrs, ArrayBase& data);
    void dmPutArrayColumnCellsV (const RefRows& rownrs, const ArrayBase& data);
    void dmGetSliceV (uInt rownr, const Slicer& slicer, ArrayBase& data);
    void dmPutSliceV (uInt rownr, const Slicer& slicer, const ArrayBase& data);
    void dmGetColumnSliceV (const Slicer& slicer, ArrayBase& data);
    void dmPutColumnSliceV (const Slicer& slicer, const ArrayBase& data);
    void dmGetColumnSliceCellsV (const RefRows& rownrs,
                                 const Slicer& slicer, ArrayBase& data);
    void dmPutColumnSliceCellsV (const RefRows& rownrs,
                                 const Slicer& slicer, const ArrayBase& data);
    // </group>

private:
    Bool        isFixedShape_p;
    String      colName_p;
    ColumnCache colCache_p;

    // Set the shape of all (fixed-shaped) arrays in the column.
    // By default it throws a "not possible" exception.
    virtual void setShapeColumn (const IPosition& shape);

    // Get a slice from the array in the given row.
    // It reads the full array in the possibly reshaped ArrayBase object.
    void getSliceArr (uInt row, const Slicer& section,
                      CountedPtr<ArrayBase>& fullArr,
                      ArrayBase& arr);

    // Put a slice into the array in the given row.
    // It reads and writes the full array in the possibly reshaped ArrayBase
    // object.
    void putSliceArr (uInt row, const Slicer& section,
                      CountedPtr<ArrayBase>& fullArr,
                      const ArrayBase& arr);

    // The copy constructor cannot be used for this base class.
    // The private declaration of this constructor makes it unusable.
    DataManagerColumn (const DataManagerColumn&);

    // Assignment cannot be used for this base class.
    // The private declaration of this operator makes it unusable.
    DataManagerColumn& operator= (const DataManagerColumn&);
};



} //# NAMESPACE CASACORE - END

#endif
