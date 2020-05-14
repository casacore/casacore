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
#include <casacore/casa/Arrays/ArrayFwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class ArrayBase;
class BaseColumnDesc;
class ColumnCache;
class TableRecord;
class RefRows;
class IPosition;
class Slicer;
class Sort;

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
    const ColumnDesc& columnDesc() const;

    // Get nr of rows in the column.
    virtual rownr_t nrow() const = 0;

    // Test if the given cell contains a defined value.
    virtual Bool isDefined (rownr_t rownr) const = 0;

    // Set the shape of the array in the given row.
    virtual void setShape (rownr_t rownr, const IPosition& shape);

    // Set the shape and tile shape of the array in the given row.
    virtual void setShape (rownr_t rownr, const IPosition& shape,
			   const IPosition& tileShape);

    // Get the global #dimensions of an array (ie. for all rows).
    virtual uInt ndimColumn() const;

    // Get the global shape of an array (ie. for all rows).
    virtual IPosition shapeColumn() const;

    // Get the #dimensions of an array in a particular cell.
    virtual uInt ndim (rownr_t rownr) const;

    // Get the shape of an array in a particular cell.
    virtual IPosition shape (rownr_t rownr) const;

    // Get the tile shape of an array in a particular cell.
    virtual IPosition tileShape (rownr_t rownr) const;

    // Ask the data manager if the shape of an existing array can be changed.
    // Default is no.
    virtual Bool canChangeShape() const;

    // Initialize the rows from startRow till endRow (inclusive)
    // with the default value defined in the column description.
    virtual void initialize (rownr_t startRownr, rownr_t endRownr) = 0;

    // Get a scalar value from a particular cell.
    virtual void get (rownr_t rownr, void* dataPtr) const;

    // Get an array from a particular cell.
    virtual void getArray (rownr_t rownr, ArrayBase& dataPtr) const;

    // Get a slice of an N-dimensional array in a particular cell.
    virtual void getSlice (rownr_t rownr, const Slicer&, ArrayBase& dataPtr) const;

    // Get the vector of all scalar values in a column.
    virtual void getScalarColumn (ArrayBase& dataPtr) const;

    // Get the array of all array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getArrayColumn (ArrayBase& dataPtr) const;

    // Get subsections from all arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getColumnSlice (const Slicer&, ArrayBase& dataPtr) const;

    // Get the vector of some scalar values in a column.
    virtual void getScalarColumnCells (const RefRows& rownrs,
				       ArrayBase& dataPtr) const;

    // Get the array of some array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getArrayColumnCells (const RefRows& rownrs,
				      ArrayBase& dataPtr) const;

    // Get subsections from some arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void getColumnSliceCells (const RefRows& rownrs,
				      const Slicer&, ArrayBase& dataPtr) const;

    // Put the scalar value in a particular cell.
    virtual void put (rownr_t rownr, const void* dataPtr);

    // Put the array value in a particular cell.
    virtual void putArray (rownr_t rownr, const ArrayBase& dataPtr);

    // Put a slice of an N-dimensional array in a particular cell.
    virtual void putSlice (rownr_t rownr, const Slicer&, const ArrayBase& dataPtr);

    // Put the vector of all scalar values in the column.
    virtual void putScalarColumn (const ArrayBase& dataPtr);

    // Put the array of all array values in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putArrayColumn (const ArrayBase& dataPtr);

    // Put into subsections of all table arrays in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putColumnSlice (const Slicer&, const ArrayBase& dataPtr);

    // Get the vector of some scalar values in a column.
    virtual void putScalarColumnCells (const RefRows& rownrs,
				       const ArrayBase& dataPtr);

    // Get the array of some array values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putArrayColumnCells (const RefRows& rownrs,
				      const ArrayBase& dataPtr);

    // Put subsections of some arrays in the column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim.
    // The arrays in the column have to have the same shape in all cells.
    virtual void putColumnSliceCells (const RefRows& rownrs,
				      const Slicer&, const ArrayBase& dataPtr);

    // Get the value from the row and convert it to the required type.
    // This can only be used for scalar columns with a standard data type.
    // Note that an unsigned integer cannot be converted to a signed integer
    // with the same length. So only Int64 can handle all integer values.
    // <group>
    void getScalar (rownr_t rownr, Bool& value) const;
    void getScalar (rownr_t rownr, uChar& value) const;
    void getScalar (rownr_t rownr, Short& value) const;
    void getScalar (rownr_t rownr, uShort& value) const;
    void getScalar (rownr_t rownr, Int& value) const;
    void getScalar (rownr_t rownr, uInt& value) const;
    void getScalar (rownr_t rownr, Int64& value) const;
    void getScalar (rownr_t rownr, float& value) const;
    void getScalar (rownr_t rownr, double& value) const;
    void getScalar (rownr_t rownr, Complex& value) const;
    void getScalar (rownr_t rownr, DComplex& value) const;
    void getScalar (rownr_t rownr, String& value) const;
    void getScalar (rownr_t rownr, TableRecord& value) const;
    // </group>

    // Get a scalar for the other data types.
    // The given data type id must match the data type id of this column.
    void getScalar (rownr_t rownr, void* value, const String& dataTypeId) const;

    // Put the value into the row and convert it from the given type.
    // This can only be used for scalar columns with a standard data type.
    // <group>
    void putScalar (rownr_t rownr, const Bool& value);
    void putScalar (rownr_t rownr, const uChar& value);
    void putScalar (rownr_t rownr, const Short& value);
    void putScalar (rownr_t rownr, const uShort& value);
    void putScalar (rownr_t rownr, const Int& value);
    void putScalar (rownr_t rownr, const uInt& value);
    void putScalar (rownr_t rownr, const Int64& value);
    void putScalar (rownr_t rownr, const float& value);
    void putScalar (rownr_t rownr, const double& value);
    void putScalar (rownr_t rownr, const Complex& value);
    void putScalar (rownr_t rownr, const DComplex& value);
    void putScalar (rownr_t rownr, const String& value);
    void putScalar (rownr_t rownr, const Char* value)
        { putScalar (rownr, String(value)); }
    void putScalar (rownr_t rownr, const TableRecord& value);
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
			      Int order, CountedPtr<ArrayBase>& dataSave);
    // Do it only for the given row numbers.
    virtual void makeRefSortKey (Sort&, CountedPtr<BaseCompare>& cmpObj,
				 Int order, const Vector<rownr_t>& rownrs,
				 CountedPtr<ArrayBase>& dataSave);
    // </group>

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

private:
    //# This ColumnDesc object is created to be able to return 
    //# a const ColumnDesc& by function columnDesc().
    mutable ColumnDesc     colDesc_p;
};




} //# NAMESPACE CASACORE - END

#endif
