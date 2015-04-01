//# VirtArrCol.h: Templated base class for virtual array column
//# Copyright (C) 1994,1995,1996,1999,2000
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

#ifndef TABLES_VIRTARRCOL_H
#define TABLES_VIRTARRCOL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Array;
class Slicer;


// <summary>
// Templated base class for virtual array column
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManagerColumn
//   <li> VirtualColumnEngine
// </prerequisite>

// <etymology>
// VirtualArrayColumn handles a virtual column containing an array.
// </etymology>

// <synopsis> 
// VirtualArrayColumn is the abstract base class to handle an array column
// for a virtual column engine (both direct and indirect arrays).
// It is derived from DataManagerColumn and reimplements some
// virtual functions to make life easier for the derived classes.
// It does the following:
// <ul>
//  <li>
//   It implements the dataType function, so it is not needed to implement
//   that in derived classes.
//  <li>
//   It has a default implementation of False for function isWritable.
//   Thus by default virtual scalar columns are not writable, which will
//   often be the case. Only if a virtual scalar column can be writable,
//   it has to be implemented in the derived class.
//  <li>
//   It has a default implementation for the functions dealing with
//   the array shapes. By default they throw an "invalid operation"
//   exception, so it is needed to implement them in the derived class.
//  <li>
//   In DataManagerColumn the functions get/putArrayV and get/putSliceV
//   are defined, which have a void* data argument. This is necessary
//   to handle arbitrary data types in the non-templated base class
//   DataManagerColumn.
//   In this templated VirtualArrayColumn class, virtual functions
//   get/putArray, get/putSlice, etc. have been defined. They cast
//   the void* data argument to Array<T>&, so in a derived class no care
//   has to be taken for that cast.
//   Furthermore a default implementation of the get/putSlice has been made.
//   They get/put the entire array (using get/putArray) and access the
//   required slice. For this purpose the function canAccessSlice has
//   also been implemented.
//   By default the get/putArray functions thrown an "invalid operation"
//   exception, so they have to be implemented in the derived class.
//  <li>
//   Similarly the functions get/putArrayColumnV and get/putColumnSliceV
//   have been templated to get/putArrayColumn and get/putColumnSlice.
//   The default implementation of these latter functions handle a
//   column by looping through its individual cells.
//   For this purpose the functions canAccessArrayColumn and
//   canAccessColumnSlice have also been implemented.
//  <li>
//   Similarly the functions get/putArrayColumnCellsV and
//   get/putColumnSliceCells have been templated to
//   get/putArrayColumnCells and get/putColumnSliceCells.
//   However, their implementations throw an exception and the function
//   canAccessArrayColumnCells has not implemented (so defaults to False).
//   However, it makes it possible that a derived class
//   (like <linkto class=ScaledComplexData>ScaledComplexData</linkto>)
//   can implement these functions.
// </ul>
// An example of a virtual array column class is ScaledComplexData. Note that
// this class is (indirectly) multiple derived from VirtualColumnEngine and
// VirtualArrayColumn, so it combines the functionality of DataManager
// and DataManagerColumn.
// This is possible, because one ScaledComplexData engine can handle only one
// column.
// </synopsis> 

// <motivation>
// This class reimplements some virtual functions implemented by
// DataManagerColumn and types the data argument. In that way they are
// easier to implement in derived classes. Furthermore they allow
// default implementations.
// </motivation>

// <templating arg=T>
//  <li> default constructor
//  <li> copy constructor
//  <li> assignment operator
//  <li> <src>static String dataTypeId();   // unique name of the class</src>
// </templating>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


template<class T>
class VirtualArrayColumn : public DataManagerColumn
{
public:

    // Create a column.
    VirtualArrayColumn()
	{;}

    // Frees up the storage.
    virtual ~VirtualArrayColumn();

    // Return the data type of the column.
    virtual int dataType() const;

    // Return the data type Id of the column.
    virtual String dataTypeId() const;

    // By default no data can be put in a virtual column.
    virtual Bool isWritable() const;

    // The class can handle a get/putSlice.
    virtual Bool canAccessSlice (Bool& reask) const;

    // The class can handle a get/putArrayColumn.
    virtual Bool canAccessArrayColumn (Bool& reask) const;

    // The class can handle a get/putColumnSlice.
    virtual Bool canAccessColumnSlice (Bool& reask) const;

protected:
    // Set the shape of all arrays in the column.
    // It is only called if the column contains direct arrays.
    // By default it throws a "not possible" exception.
    virtual void setShapeColumn (const IPosition& shape);

    // Set the shape of an array in the given row.
    // It is only called if the column contains indirect arrays.
    // By default it throws a "not possible" exception.
    virtual void setShape (uInt rownr, const IPosition& shape);

    // Is the value shape defined in the given row?
    // By default it throws a "not possible" exception.
    virtual Bool isShapeDefined (uInt rownr);

    // Get the dimensionality of the item in the given row.
    // By default it throws a "not possible" exception.
    virtual uInt ndim (uInt rownr);

    // Get the shape of the item in the given row.
    // By default it throws a "not possible" exception.
    virtual IPosition shape (uInt rownr);

    // Get the array value in the given row.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::get function).
    virtual void getArray (uInt rownr, Array<T>& data) = 0;

    // Put the array value into the given row.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::put function).
    // By default it throws a "not possible" exception.
    virtual void putArray (uInt rownr, const Array<T>& data);

    // Get a section of the array in the given row.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::getSlice function).
    // The default implementation gets the slice by getting the full
    // array first.
    virtual void getSlice (uInt rownr, const Slicer& slicer, Array<T>& data);

    // Put into a section of the array in the given row.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::putSlice function).
    // The default implementation gets the slice by accessing the full
    // array.
    virtual void putSlice (uInt rownr, const Slicer& slicer,
			   const Array<T>& data);

    // Get an entire column.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColum::getColumn function).
    // The default implementation gets the column row by row.
    virtual void getArrayColumn (Array<T>& data);

    // Put an entire column.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::putColumn function).
    // The default implementation puts the column row by row.
    virtual void putArrayColumn (const Array<T>& data);

    // Get some array values in the column.
    // The data array has to have the correct length
    // (which is guaranteed by the ArrayColumn::getColumn function).
    // By default it throws a "not possible" exception.
    virtual void getArrayColumnCells (const RefRows& rownrs, Array<T>& data);

    // Put some array values in the column.
    // The data array has to have the correct length
    // (which is guaranteed by the ArrayColumn::putColumn function).
    // By default it throws a "not possible" exception.
    virtual void putArrayColumnCells (const RefRows& rownrs,
				      const Array<T>& data);

    // Get a section of all arrays in the column.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::getColumn function).
    // The default implementation gets the column row by row.
    virtual void getColumnSlice (const Slicer& slicer, Array<T>& data);

    // Put a section of all arrays in the column.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn putColumn function).
    // The default implementation puts the column row by row.
    virtual void putColumnSlice (const Slicer& slicer, const Array<T>& data);

    // Get a section of some arrays in the column.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::getColumn function).
    // By default it throws a "not possible" exception.
    virtual void getColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer, Array<T>& data);

    // Put into a section of some arrays in the column.
    // The data array has to have the correct shape
    // (which is guaranteed by the ArrayColumn::putColumn function).
    // By default it throws a "not possible" exception.
    virtual void putColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer,
				      const Array<T>& data);

private:
    // Implement the virtual functions defined in DataManagerColumn.
    // Get the array value in the given row.
    void getArrayV (uInt rownr, void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put the array value into the given row.
    void putArrayV (uInt rownr, const void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Get some array values in the column.
    void getArrayColumnCellsV (const RefRows& rownrs, void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put some array values in the column.
    void putArrayColumnCellsV (const RefRows& rownrs, const void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Get a section of the array in the given row.
    void getSliceV (uInt rownr, const Slicer& slicer, void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put into a section of the array in the given row.
    void putSliceV (uInt rownr, const Slicer& slicer, const void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Get an entire column.
    void getArrayColumnV (void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put an entire column.
    void putArrayColumnV (const void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Get a section of all arrays in the column.
    void getColumnSliceV (const Slicer& slicer, void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put into section of all arrays in the column.
    void putColumnSliceV (const Slicer& slicer, const void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Get a section of some arrays in the column.
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer, void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put into a section of some arrays in the column.
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer,
				       const void* dataPtr);


private:
    // The object cannot be copied.
    VirtualArrayColumn (const VirtualArrayColumn<T>&);

    // The object cannot be assigned to.
    VirtualArrayColumn<T>& operator= (const VirtualArrayColumn<T>&);
};




} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/DataMan/VirtArrCol.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
