//# BaseColumn.h: Abstract base class for a table column
//# Copyright (C) 1994,1995,1996,1997,1998
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

#ifndef TABLES_BASECOLUMN_H
#define TABLES_BASECOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class BaseColumnDesc;
class ColumnCache;
class TableRecord;
class RefRows;
class IPosition;
class Slicer;
class Sort;
template<class T> class Array;
template<class T> class Vector;

// <summary>
// Abstract base class for a table column
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> ColumnDesc
//   <li> Table
// </prerequisite>

// <etymology>
// This is the (abstract) base class to access a column in a table.
// </etymology>

// <synopsis> 
// This class is the base class for the derived column classes.
// It is a private class in the sense that the user cannot get
// access to it. All user access to a column is done via the
// classes TableColumn, ScalarColumn and ArrayColumn. They call
// the corresponding functions in this class and its derived classes.
// </synopsis> 

// <motivation>
// This class serves a the base for the more specialized column classes
// like FilledScalarColumn and RefColumn. It defines many virtual
// functions, which are implemented in the derived classes.
// Some of these functions are purely virtual, some have a default
// implementation throwing an "invalid operation" exception. In that
// way those latter functions only have to be implemented in the
// classes which handle those cases.
// <note role=tip> The class RefColumn is in fact implemented in terms of
// this class. Almost every function in RefColumn calls the corresponding
// function in BaseColumn with the correct row number.
// </note>
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class BaseColumn
{
public:

    // Construct it using the given column description.
    BaseColumn (const BaseColumnDesc*);

    virtual ~BaseColumn();

    // Test if the column is writable.
    virtual Bool isWritable() const = 0;

    // Test if the column is stored (otherwise it is virtual).
    virtual Bool isStored() const = 0;

    // Get access to the column keyword set.
    // <group>
    virtual TableRecord& rwKeywordSet() = 0;
    virtual TableRecord& keywordSet() = 0;
    // </group>

    // Get const access to the column description.
    const ColumnDesc& columnDesc() const
	{ return colDesc_p; }

    // Get nr of rows in the column.
    virtual uInt nrow() const = 0;

    // Test if the given cell contains a defined value.
    virtual Bool isDefined (uInt rownr) const = 0;

    // Set the shape of the array in the given row.
    virtual void setShape (uInt rownr, const IPosition& shape);

    // Set the shape and tile shape of the array in the given row.
    virtual void setShape (uInt rownr, const IPosition& shape,
			   const IPosition& tileShape);

    // Get the global #dimensions of an array (ie. for all rows).
    virtual uInt ndimColumn() const;

    // Get the global shape of an array (ie. for all rows).
    virtual IPosition shapeColumn() const;

    // Get the #dimensions of an array in a particular cell.
    virtual uInt ndim (uInt rownr) const;

    // Get the shape of an array in a particular cell.
    virtual IPosition shape (uInt rownr) const;

    // Ask the data manager if the shape of an existing array can be changed.
    // Default is no.
    virtual Bool canChangeShape() const;

    // Ask if the data manager can handle a scalar column.
    // Default is never.
    virtual Bool canAccessScalarColumn (Bool& reask) const;

    // Ask if the data manager can handle an array column.
    // Default is never.
    virtual Bool canAccessArrayColumn (Bool& reask) const;

    // Ask if the data manager can handle a collection of cells in a
    // scalar column. Default is never.
    virtual Bool canAccessScalarColumnCells (Bool& reask) const;

    // Ask if the data manager can handle a collection of cells in an
    // array column. Default is never.
    virtual Bool canAccessArrayColumnCells (Bool& reask) const;

    // Ask if the data manager can handle a cell slice.
    // Default is never.
    virtual Bool canAccessSlice (Bool& reask) const;

    // Ask if the data manager can handle a column slice.
    // Default is never.
    virtual Bool canAccessColumnSlice (Bool& reask) const;

    // Initialize the rows from startRow till endRow (inclusive)
    // with the default value defined in the column description.
    virtual void initialize (uInt startRownr, uInt endRownr) = 0;

    // Get the value from a particular cell.
    // This can be a scalar or an array.
    virtual void get (uInt rownr, void* dataPtr) const = 0;

    // Get a slice of an N-dimensional array in a particular cell.
    virtual void getSlice (uInt rownr, const Slicer&, void* dataPtr) const;

    // Get the vector of all scalar values in a column.
    virtual void getScalarColumn (void* dataPtr) const;

    // Get the array of all array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getArrayColumn (void* dataPtr) const;

    // Get subsections from all arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getColumnSlice (const Slicer&, void* dataPtr) const;

    // Get the vector of some scalar values in a column.
    virtual void getScalarColumnCells (const RefRows& rownrs,
				       void* dataPtr) const;

    // Get the array of some array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getArrayColumnCells (const RefRows& rownrs,
				      void* dataPtr) const;

    // Get subsections from some arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getColumnSliceCells (const RefRows& rownrs,
				      const Slicer&, void* dataPtr) const;

    // Put the value in a particular cell.
    // This can be a scalar or an array.
    virtual void put (uInt rownr, const void* dataPtr) = 0;

    // Put a slice of an N-dimensional array in a particular cell.
    virtual void putSlice (uInt rownr, const Slicer&, const void* dataPtr);

    // Put the vector of all scalar values in the column.
    virtual void putScalarColumn (const void* dataPtr);

    // Put the array of all array values in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putArrayColumn (const void* dataPtr);

    // Put into subsections of all table arrays in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putColumnSlice (const Slicer&, const void* dataPtr);

    // Get the vector of some scalar values in a column.
    virtual void putScalarColumnCells (const RefRows& rownrs,
				       const void* dataPtr);

    // Get the array of some array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putArrayColumnCells (const RefRows& rownrs,
				      const void* dataPtr);

    // Put subsections of some arrays in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putColumnSliceCells (const RefRows& rownrs,
				      const Slicer&, const void* dataPtr);

    // Get the value from the row and convert it to the required type.
    // This can only be used for scalar columns with a standard data type.
    // Note that an unsigned integer cannot be converted to a signed integer
    // with the same length. So only Int64 can handle all integer values.
    // <group>
    void getScalar (uInt rownr, Bool& value) const;
    void getScalar (uInt rownr, uChar& value) const;
    void getScalar (uInt rownr, Short& value) const;
    void getScalar (uInt rownr, uShort& value) const;
    void getScalar (uInt rownr, Int& value) const;
    void getScalar (uInt rownr, uInt& value) const;
    void getScalar (uInt rownr, Int64& value) const;
    void getScalar (uInt rownr, float& value) const;
    void getScalar (uInt rownr, double& value) const;
    void getScalar (uInt rownr, Complex& value) const;
    void getScalar (uInt rownr, DComplex& value) const;
    void getScalar (uInt rownr, String& value) const;
    void getScalar (uInt rownr, TableRecord& value) const;
    // </group>

    // Get a scalar for the other data types.
    // The given data type id must match the data type id of this column.
    void getScalar (uInt rownr, void* value, const String& dataTypeId) const;

    // Put the value into the row and convert it from the given type.
    // This can only be used for scalar columns with a standard data type.
    // <group>
    void putScalar (uInt rownr, const Bool& value);
    void putScalar (uInt rownr, const uChar& value);
    void putScalar (uInt rownr, const Short& value);
    void putScalar (uInt rownr, const uShort& value);
    void putScalar (uInt rownr, const Int& value);
    void putScalar (uInt rownr, const uInt& value);
    void putScalar (uInt rownr, const float& value);
    void putScalar (uInt rownr, const double& value);
    void putScalar (uInt rownr, const Complex& value);
    void putScalar (uInt rownr, const DComplex& value);
    void putScalar (uInt rownr, const String& value);
    void putScalar (uInt rownr, const Char* value)
        { putScalar (rownr, String(value)); }
    void putScalar (uInt rownr, const TableRecord& value);
    // </group>

    // Get a pointer to the underlying column cache.
    virtual ColumnCache& columnCache() = 0;

    // Set the maximum cache size (in bytes) to be used by a storage manager.
    virtual void setMaximumCacheSize (uInt nbytes) = 0;

    // Add this column and its data to the Sort object.
    // It may allocate some storage on the heap, which will be saved
    // in the argument dataSave.
    // The function freeSortKey must be called to free this storage.
    // <group>
    virtual void makeSortKey (Sort&, CountedPtr<BaseCompare>& cmpObj,
			      Int order, const void*& dataSave);
    // Do it only for the given row numbers.
    virtual void makeRefSortKey (Sort&, CountedPtr<BaseCompare>& cmpObj,
				 Int order, const Vector<uInt>& rownrs,
				 const void*& dataSave);
    // </group>

    // Free storage on the heap allocated by makeSortkey().
    // The pointer will be set to zero.
    virtual void freeSortKey (const void*& dataSave);

    // Allocate value buffers for the table iterator.
    // Also get a comparison object if undefined.
    // The function freeIterBuf must be called to free the buffers.
    virtual void allocIterBuf (void*& lastVal, void*& curVal,
			       CountedPtr<BaseCompare>& cmpObj);

    // Free the value buffers allocated by allocIterBuf.
    virtual void freeIterBuf (void*& lastVal, void*& curVal);

protected:
    // Throw exceptions for invalid scalar get or put.
    // <group>
    void throwGetScalar() const;
    void throwPutScalar() const;
    void throwGetType (const String& type) const;
    void throwPutType (const String& type) const;
    // </group>

    //# Data members
    const BaseColumnDesc*  colDescPtr_p;
    //# This ColumnDesc object is created to be able to return 
    //# a const ColumnDesc& by function columnDesc().
    ColumnDesc             colDesc_p;
};




} //# NAMESPACE CASACORE - END

#endif
