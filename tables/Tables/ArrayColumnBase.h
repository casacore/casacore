//# ArrayColumnBase.h: base class for access to an array table column
//# Copyright (C) 2013
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_ARRAYCOLUMNBASE_H
#define TABLES_ARRAYCOLUMNBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class BaseSlicesFunctor;
class RefRows;
class ColumnSlicer;
class IPosition;
class Slice;
class Slicer;
class String;


// <summary>
// Read and write access to an array table column with arbitrary data type
// </summary>

// <use visibility=export>

// <reviewed reviewer="dschieb" date="1994/08/10" tests="none">
// </reviewed>

// <prerequisite>
//   <li> Table
//   <li> TableColumn
// </prerequisite>

// <etymology>
// ArrayColumn<T> gives read and write access to an column in a table
// containing an array with data type T.
// </etymology>

// <synopsis> 
// ArrayColumnBase is the base class of the templated class ArrayColumn
// which allows readonly access to a column containing arrays with an
// arbitrary data type. It can handle direct as well as indirect arrays.
//
// All get and put functions are implemented in this base class as
// non-templated functions. Type-specific operations are done by means
// of virtual functions in the Array classes.
// </synopsis>
  
// <example>
// See module <linkto module="Tables#open">Tables</linkto>.
// </example>


  class ArrayColumnBase : public TableColumn
  {
  public:
    // The default constructor creates a null object, i.e. it
    // does not reference a table column.
    ArrayColumnBase();

    // Construct for the given column in the given table.
    ArrayColumnBase (const Table&, const String& columnName);

    // Construct from the given table column.
    // This constructor is useful if first a table column was constructed,
    // its type is determined and thereafter used to construct the
    // correct column object.
    ArrayColumnBase (const TableColumn& column);

    // Copy constructor (reference semantics).
    ArrayColumnBase (const ArrayColumnBase&);

    ~ArrayColumnBase();

    // Assignment uses reference semantics, thus works the same
    // as function reference.
    ArrayColumnBase& operator= (const ArrayColumnBase&);

    // Change the reference to another column.
    // This is in fact an assignment operator with reference semantics.
    // It removes the reference to the current column and creates
    // a reference to the column referenced in the other object.
    // It will handle null objects correctly.
    void reference (const ArrayColumnBase&);

    // Get the #dimensions of an array in a particular cell.
    // If the cell does not contain an array, 0 is returned.
    // Use the function isDefined to test if the cell contains an array.
    uInt ndim (rownr_t rownr) const
      { TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->ndim (rownr); }

    // Get the shape of an array in a particular cell.
    // If the cell does not contain an array, a 0-dim shape is returned.
    // Use the function isDefined to test if the cell contains an array.
    IPosition shape (rownr_t rownr) const
      { TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->shape (rownr); }

    // Counterparts of the acbGet() functions below not checking shapes, etc.
    // They are faster and can be used for performance reasons if one
    // knows for sure that the arguments are correct.
    // E.g., they are used internally in virtual column engines.
    // <group>
    void baseGet (rownr_t rownr, ArrayBase& array) const
      { baseColPtr_p->getArray (rownr, array); }
    void baseGetSlice (rownr_t rownr, const Slicer& arraySection,
                       ArrayBase& array) const
      { baseColPtr_p->getSlice (rownr, arraySection, array); }
    // </group>

    // Get the array value in a particular cell (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    void acbGet (rownr_t rownr, ArrayBase& array, Bool resize) const;

    // Get a slice of an N-dimensional array in a particular cell
    // (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    // The dimensionality of the slice must match the dimensionality
    // of the table array and the slice definition should not exceed
    // the shape of the table array.
    void acbGetSlice (rownr_t rownr, const Slicer& arraySection, ArrayBase& array,
                      Bool resize) const;

    // Get an irregular slice of an N-dimensional array in a particular cell
    // (i.e. table row)  as given by the vectors of Slice objects.
    // The outer vector represents the array axes.
    // A missing or empty axis means the entire axis.
    // The inner vector represents the slices to take for each axis.
    // For example, to get slices from 2-dim arrays:
    // <srcblock>
    // Vector<Vector<Slice> > slices(2);      // 2-dim
    // slices[1].resize (3);                  // 3 slices in 2nd dim
    // slices[1][0] = Slice(100,20);
    // slices[1][1] = Slice(200,18);
    // slices[1][2] = Slice(538,30,2);
    // // Get data. Vector of first axis is empty, thus entire axis is read.
    // Array<Complex> data = dataCol.getColumn (slices);
    // </srcblock>
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // with the last dimension representing the number of rows and the
    // other dimensions representing the shape of the slice.
    // The arrays in the column must have the same shape in all cells.
    void acbGetSlice (rownr_t rownr,
                      const Vector<Vector<Slice> >& arraySlices,
                      ArrayBase& arr, Bool resize) const;

    // Get the array of all values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim
    // with the last dimension representing the number of rows.
    // The arrays in the column must have the same shape in all cells.
    void acbGetColumn (ArrayBase& array, Bool resize) const;

    // Get regular slices from all arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // with the last dimension representing the number of rows and the
    // other dimensions representing the shape of the slice.
    // The arrays in the column must have the same shape in all cells.
    void acbGetColumn (const Slicer& arraySection, ArrayBase& array,
                       Bool resize) const;

    // Get irregular slices from all arrays in the column as given by the
    // vectors of Slice objects. The outer vector represents the array axes.
    // A missing or empty axis means the entire axis.
    // The inner vector represents the slices to take for each axis.
    // For example, to get slices from 2-dim arrays:
    // <srcblock>
    // Vector<Vector<Slice> > slices(2);      // 2-dim
    // slices[1].resize (3);                  // 3 slices in 2nd dim
    // slices[1][0] = Slice(100,20);
    // slices[1][1] = Slice(200,18);
    // slices[1][2] = Slice(538,30,2);
    // // Get data. Vector of first axis is empty, thus entire axis is read.
    // Array<Complex> data = dataCol.getColumn (slices);
    // </srcblock>
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // with the last dimension representing the number of rows and the
    // other dimensions representing the shape of the slice.
    // The arrays in the column must have the same shape in all cells.
    void acbGetColumn (const Vector<Vector<Slice> >& arraySection,
                       ArrayBase& array,
                       Bool resize) const;

    // Get the array of some values in a column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to get.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // The arrays in the column must have the same shape in all those cells.
    void acbGetColumnRange (const Slicer& rowRange, ArrayBase& arr,
                            Bool resize) const;
    void acbGetColumnCells (const RefRows& rownrs, ArrayBase& arr,
                            Bool resize) const;

    // Get slices from some arrays in a column.
    // The first Slicer object can be used to specify start, end (or length),
    // and stride of the rows to get. The second Slicer object can be
    // used to specify the slice to take from each array.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // The arrays in the column must have the same shape in all those cells.
    // <group>
    void acbGetColumnRange (const Slicer& rowRange,
                            const Slicer& arraySection, ArrayBase& arr,
                            Bool resize) const;
    void acbGetColumnCells (const RefRows& rownrs,
                            const Slicer& arraySection, ArrayBase& arr,
                            Bool resize) const;
    // </group>

    // Get various slices from the given rows.
    void acbGetColumnCells (const RefRows& rows,
                            const ColumnSlicer& columnSlicer,
                            ArrayBase& destination,
                            Bool resize) const;

    // Set the shape of the array in the given row.
    // Setting the shape is needed if the array is put in slices,
    // otherwise the table system would not know the shape.
    // <group>
    void setShape (rownr_t rownr, const IPosition& shape);

    // Try to store the array in a tiled way using the given tile shape.
    void setShape (rownr_t rownr, const IPosition& shape,
		   const IPosition& tileShape);
    // </group>

    // Counterparts of the acbPut() functions below not checking shapes, etc.
    // They are faster and can be used for performance reasons if one
    // knows for sure that the arguments are correct.
    // E.g., they are used internally in virtual column engines.
    // <group>
    void basePut (rownr_t rownr, const ArrayBase& array)
      { baseColPtr_p->putArray (rownr, array); }
    void basePutSlice (rownr_t rownr, const Slicer& arraySection,
                       const ArrayBase& array)
      { baseColPtr_p->putSlice (rownr, arraySection, array); }
    // </group>

    // Put the array in a particular cell (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    // If the shape of the table array in that cell has not already been
    // defined, it will be defined implicitly.
    void acbPut (rownr_t rownr, const ArrayBase& array);

    // Put into a slice of an N-dimensional array in a particular cell.
    // The row numbers count from 0 until #rows-1.
    // The shape of the table array must have been defined.
    // The dimensionality of the slice must match the dimensionality
    // of the table array and the slice definition should not exceed
    // the shape of the table array.
    void acbPutSlice (rownr_t rownr, const Slicer& arraySection,
                      const ArrayBase& array);

    void acbPutSlice (rownr_t rownr, const Vector<Vector<Slice> >& arraySlices,
                      const ArrayBase& arr);

    // Put the array of all values in the column.
    // If the column contains n-dim arrays, the source array must be (n+1)-dim
    // with the last dimension representing the number of rows.
    void acbPutColumn (const ArrayBase& array);

    // Put into subsections of the table arrays in the entire column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim
    // with the last dimension representing the number of rows and
    // other dimensions representing the shape of the slice.
    // The dimensionality of the slice must match the dimensionality
    // of the table array, thus must be n-dim. Also the slice definition
    // should not exceed the shape of the table arrays.
    void acbPutColumn (const Slicer& arraySection, const ArrayBase& array);

    void acbPutColumn (const Vector<Vector<Slice> >& arraySlices,
                       const ArrayBase& arr);

    // Put the array of some values in the column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to put.
    // If the column contains n-dim arrays, the source array must be (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // <group>
    void acbPutColumnRange (const Slicer& rowRange, const ArrayBase& arr);
    void acbPutColumnCells (const RefRows& rownrs, const ArrayBase& arr);
    // </group>

    // Put into subsection of the table arrays in some rows of the column.
    // The first Slicer object can be used to specify start, end (or length),
    // and stride of the rows to put. The second Slicer object can be
    // used to specify the slice to take from each array.
    // If the column contains n-dim arrays, the source array must be (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // <group>
    void acbPutColumnRange (const Slicer& rowRange,
                            const Slicer& arraySection, const ArrayBase& arr);
    void acbPutColumnCells (const RefRows& rownrs,
                            const Slicer& arraySection, const ArrayBase& arr);
    // </group>

    // Put various slices in the given rows.
    // <group>
    void acbPutColumnCells (const RefRows& rows,
                            const Vector<Vector<Slice> >& arraySlices,
                            const ArrayBase& source);
    void acbPutColumnCells (const RefRows& rows,
                            const ColumnSlicer& columnSlicer,
                            const ArrayBase& source);
    // </group>

    // Put the same value in all cells of the column.
    void acbFillColumn (const ArrayBase& value);

    // Put the contents of that column into this one.
    void acbPutColumn (const ArrayColumnBase& that);

    // Adapt the shape of the array if possible. If the array is empty or
    // if <src>resize=True</src>, the array is resized if needed.
    // Otherwise checkShape is used to throw an exception if not conforming.
    void adaptShape (const IPosition& shp,
                     ArrayBase& arr, Bool resize,
                     Int64 rownr, const String& where) const;

    // Throw an exception if the array does not have the expected shape.
    // However, False is returned if noSlicing and canChangeShape_p are True
    // (meaning no slices are put and the shape of a full row can change).
    // The column name is made part of the error message, as well as the rownr
    // if it is not negative (meaning a put of a column).
    Bool checkShape (const IPosition& expShape, const IPosition& arrShape,
                     Bool noSlicing, Int64 rownr, const String& where) const;

    // A common function used by all functions that can get or put irregular
    // array slices. The functor performs the get or put operation.
    void handleSlices (const Vector<Vector<Slice> >& slices,
                       BaseSlicesFunctor& functor,
                       const Slicer& slicer,
                       const ArrayBase& array) const;
  };




// <synopsis>
// ColumnSlicer is used in one of the ArrayColumn::getColumnCells functions.
// That method takes a potentially complex/ selection of data out of a
// column cell (e.g., multiple slices along each axis) and then puts them
// into a selection of a destination array.
// This is most easily represented as a set of source,destination slicers
// where one is applied to the cell and the other to the destination array.  
// </synopsis>
class ColumnSlicer
{
public:

  // Construct the object.
  // It takes over the pointers to the Slicer objects and deletes them
  // in the destructor.
  // The shape parameter is the shape of the destination array excluding
  // the row axis.  
  ColumnSlicer (const IPosition& shape,
                const Vector<Slicer*>& dataSlicers,
                const Vector<Slicer*>& destinationSlicers);

  // The destructor deletes all Slicer objects.
  ~ColumnSlicer();

  // Get the data slicers.
  const Vector<Slicer*>& getDataSlicers() const
    { return dataSlicers_p; }

  // Get the desintation slicers.
  const Vector<Slicer*>& getDestinationSlicers() const
    { return destinationSlicers_p; }

  // Get the shape.
  const IPosition& shape() const
    { return shape_p; }

private:
  // Delete all Slicer objects.
  void freeSlicers();

  // Check if the slicers match the array shape.
  String validateParameters() const; 

  //# Data members.
  Vector<Slicer*> dataSlicers_p;
  Vector<Slicer*> destinationSlicers_p;
  IPosition shape_p;
};



} //# NAMESPACE CASACORE - END

#endif
