//# ArrayColumn.h: access to an array table column with arbitrary data type
//# Copyright (C) 1994,1995,1996,1997,1998,2001
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

#ifndef TABLES_ARRAYCOLUMN_H
#define TABLES_ARRAYCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RefRows;
template<class T> class Array;
template<class T> class BaseSlicesFunctor;
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
// The class ArrayColumn allows readonly access to a column
// containing arrays with an arbitrary data type. It can handle direct
// as well as indirect arrays.
// It is possible to get the data in an individual cell (i.e. table row);
// either the whole array or a slice of the array can be accessed.
// It is also possible to get the column as a whole if the arrays
// in all cells of the column have the same shape (which is always true
// for direct arrays). As in the case of individual cells it is possible
// to get the entire arrays or a slice of the arrays.
//
// A default constructor is defined to allow construction of an array
// of ArrayColumn objects. However, this constructs an object not
// referencing a column. Functions like get, etc. will fail (i.e. result
// in a segmentation fault) when used on such objects. The functions
// isNull and throwIfNull can be used to test on this.
// The functions attach and reference can fill in the object.
//
// The assignment operator is not defined for this class, because it was
// felt it would be too confusing. Instead the function reference can
// be used to do assignment with reference semantics. An assignment
// with copy semantics makes no sense for a readonly column.
// </synopsis>

// <templating arg=T>
//  <li> Default constructor
//  <li> Copy constructor
//  <li> Assignment operator
// </templating>

// <example>
// See module <linkto module="Tables#open">Tables</linkto>.
// </example>


template<class T>
class ArrayColumn : public TableColumn
{
public:

    // The default constructor creates a null object, i.e. it
    // does not reference a table column.
    // The sole purpose of this constructor is to allow construction
    // of an array of ArrayColumn objects.
    // The functions reference and attach can be used to make a null object
    // reference a column.
    // Note that get functions, etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    ArrayColumn();

    // Construct for the given column in the given table.
    ArrayColumn (const Table&, const String& columnName);

    // Construct from the given table column.
    // This constructor is useful if first a table column was constructed,
    // its type is determined and thereafter used to construct the
    // correct column object.
    explicit ArrayColumn (const TableColumn&);

    // Copy constructor (reference semantics).
    ArrayColumn (const ArrayColumn<T>&);

    ~ArrayColumn();

    // Clone the object.
    virtual TableColumn* clone() const;

    // Assignment uses reference semantics, thus works the same
    // as function reference.
    ArrayColumn<T>& operator= (const ArrayColumn<T>&);

    // Change the reference to another column.
    // This is in fact an assignment operator with reference semantics.
    // It removes the reference to the current column and creates
    // a reference to the column referenced in the other object.
    // It will handle null objects correctly.
    void reference (const ArrayColumn<T>&);

    // Attach a column to the object.
    // This is in fact only a shorthand for 
    // <br><src> reference (ArrayColumn<T> (table, columnName)); </src>
    void attach (const Table& table, const String& columnName)
	{ reference (ArrayColumn<T> (table, columnName)); }

    // Get the #dimensions of an array in a particular cell.
    // If the cell does not contain an array, 0 is returned.
    // Use the function isDefined to test if the cell contains an array.
    uInt ndim (uInt rownr) const
	{ TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->ndim (rownr); }

    // Get the shape of an array in a particular cell.
    // If the cell does not contain an array, a 0-dim shape is returned.
    // Use the function isDefined to test if the cell contains an array.
    IPosition shape (uInt rownr) const
	{ TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->shape (rownr); }

    // Get the array value in a particular cell (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    // <group>
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the table array shape.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    void get (uInt rownr, Array<T>& array, Bool resize = False) const;
    Array<T> get (uInt rownr) const;
    Array<T> operator() (uInt rownr) const;
    // </group>

    // Get a slice of an N-dimensional array in a particular cell
    // (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    // The dimensionality of the slice must match the dimensionality
    // of the table array and the slice definition should not exceed
    // the shape of the table array.
    // <group>
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the shape of the
    // table array slice.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    void getSlice (uInt rownr, const Slicer& arraySection, Array<T>& array,
		   Bool resize = False) const;
    Array<T> getSlice (uInt rownr, const Slicer& arraySection) const;
    // </group>

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
    // <group>
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the resulting (n+1)-dim
    // array.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    void getSlice (uInt rownr,
                   const Vector<Vector<Slice> >& arraySlices,
                   Array<T>& arr, Bool resize = False) const;
    Array<T> getSlice (uInt rownr,
                       const Vector<Vector<Slice> >& arraySlices) const;
    // </group>

    // Get the array of all values in a column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim
    // with the last dimension representing the number of rows.
    // The arrays in the column must have the same shape in all cells.
    // <group>
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the resulting (n+1)-dim
    // array.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    void getColumn (Array<T>& array, Bool resize = False) const;
    Array<T> getColumn() const;
    // </group>

    // Get regular slices from all arrays in the column.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim.
    // with the last dimension representing the number of rows and the
    // other dimensions representing the shape of the slice.
    // The arrays in the column must have the same shape in all cells.
    // <group>
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the resulting (n+1)-dim
    // array.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    void getColumn (const Slicer& arraySection, Array<T>& array,
		    Bool resize = False) const;
    Array<T> getColumn (const Slicer& arraySection) const;
    // </group>

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
    // <group>
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the resulting (n+1)-dim
    // array.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    void getColumn (const Vector<Vector<Slice> >& arraySection, Array<T>& array,
		    Bool resize = False) const;
    Array<T> getColumn (const Vector<Vector<Slice> >& arraySection) const;
    // </group>

    // Get the array of some values in a column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to get.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // The arrays in the column must have the same shape in all those cells.
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the resulting (n+1)-dim
    // array.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    // <group>
    void getColumnRange (const Slicer& rowRange, Array<T>& arr,
			 Bool resize = False) const;
    Array<T> getColumnRange (const Slicer& rowRange) const;
    void getColumnCells (const RefRows& rownrs, Array<T>& arr,
			 Bool resize = False) const;
    Array<T> getColumnCells (const RefRows& rownrs) const;
    // </group>

    // Get slices from some arrays in a column.
    // The first Slicer object can be used to specify start, end (or length),
    // and stride of the rows to get. The second Slicer object can be
    // used to specify the slice to take from each array.
    // If the column contains n-dim arrays, the resulting array is (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // The arrays in the column must have the same shape in all those cells.
    // According to the assignment rules of class Array, the destination
    // array must be empty or its shape must conform the resulting (n+1)-dim
    // array.
    // However, if the resize flag is set the destination array will be
    // resized if not conforming.
    // <group>
    void getColumnRange (const Slicer& rowRange,
			 const Slicer& arraySection, Array<T>& arr,
			 Bool resize = False) const;
    Array<T> getColumnRange (const Slicer& rowRange,
			     const Slicer& arraySection) const;
    void getColumnCells (const RefRows& rownrs,
			 const Slicer& arraySection, Array<T>& arr,
			 Bool resize = False) const;
    Array<T> getColumnCells (const RefRows& rownrs,
			     const Slicer& arraySection) const;
    // </group>

    // Similar to getColumn (arraySlices, arr, resize) except it
    // gets the slices for the given rows instead of all rows.
    void getColumnCells (const RefRows& rows,
                         const Vector<Vector<Slice> >& arraySlices,
                         Array<T>& arr,
                         Bool resize = False) const;
    void getSliceForRows (const RefRows& rows,
                          const Vector<Vector<Slice> >& arraySlices,
                          Array<T>& destination) const
      { getColumnCells (rows, arraySlices, destination, True); }
 
    // The get() function like above which does not check shapes, etc.
    // It is faster and can be used for performance reasons if one
    // knows for sure that the arguments are correct.
    // E.g. it is used internally in virtual column engines.
    void baseGet (uInt rownr, Array<T>& array) const
      { baseColPtr_p->get (rownr, &array); }

    // Set the shape of the array in the given row.
    // Setting the shape is needed if the array is put in slices,
    // otherwise the table system would not know the shape.
    // <group>
    void setShape (uInt rownr, const IPosition& shape);

    // Try to store the array in a tiled way using the given tile shape.
    void setShape (uInt rownr, const IPosition& shape,
		   const IPosition& tileShape);
    // </group>

    // Put the array in a particular cell (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    // If the shape of the table array in that cell has not already been
    // defined, it will be defined implicitly.
    void put (uInt rownr, const Array<T>& array);

    // Copy the value of a cell of that column to a cell of this column.
    // The data types of both columns must be the same, otherwise an
    // exception is thrown.
    // <group>
    // Use the same row numbers for both cells.
    void put (uInt rownr, const ArrayColumn<T>& that)
	{ put (rownr, that, rownr); }
    // Use possibly different row numbers for that (i.e. input) and
    // and this (i.e. output) cell.
    void put (uInt thisRownr, const ArrayColumn<T>& that, uInt thatRownr);
    // </group>

    // Copy the value of a cell of that column to a cell of this column.
    // This function uses a generic TableColumn object as input.
    // The data types of both columns must be the same, otherwise an
    // exception is thrown.
    // <group>
    // Use the same row numbers for both cells.
    void put (uInt rownr, const TableColumn& that)
	{ put (rownr, that, rownr); }
    // Use possibly different row numbers for that (i.e. input) and
    // and this (i.e. output) cell.
    void put (uInt thisRownr, const TableColumn& that, uInt thatRownr);
    // </group>

    // Put into a slice of an N-dimensional array in a particular cell.
    // The row numbers count from 0 until #rows-1.
    // The shape of the table array must have been defined.
    // The dimensionality of the slice must match the dimensionality
    // of the table array and the slice definition should not exceed
    // the shape of the table array.
    void putSlice (uInt rownr, const Slicer& arraySection,
		   const Array<T>& array);

    void putSlice (uInt rownr, const Vector<Vector<Slice> >& arraySlices,
                   const Array<T>& arr);

    // Put the array of all values in the column.
    // If the column contains n-dim arrays, the source array must be (n+1)-dim
    // with the last dimension representing the number of rows.
    void putColumn (const Array<T>& array);

    // Put into subsections of the table arrays in the entire column.
    // If the column contains n-dim arrays, the source array is (n+1)-dim
    // with the last dimension representing the number of rows and
    // other dimensions representing the shape of the slice.
    // The dimensionality of the slice must match the dimensionality
    // of the table array, thus must be n-dim. Also the slice definition
    // should not exceed the shape of the table arrays.
    void putColumn (const Slicer& arraySection, const Array<T>& array);

    void putColumn (const Vector<Vector<Slice> >& arraySlices,
                    const Array<T>& arr);

    // Put the array of some values in the column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to put.
    // If the column contains n-dim arrays, the source array must be (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // <group>
    void putColumnRange (const Slicer& rowRange, const Array<T>& arr);
    void putColumnCells (const RefRows& rownrs, const Array<T>& arr);
    // </group>

    // Put into subsection of the table arrays in some rows of the column.
    // The first Slicer object can be used to specify start, end (or length),
    // and stride of the rows to put. The second Slicer object can be
    // used to specify the slice to take from each array.
    // If the column contains n-dim arrays, the source array must be (n+1)-dim
    // with the last dimension representing the number of rows in the slicer.
    // <group>
    void putColumnRange (const Slicer& rowRange,
			 const Slicer& arraySection, const Array<T>& arr);
    void putColumnCells (const RefRows& rownrs,
			 const Slicer& arraySection, const Array<T>& arr);
    // </group>

    // Same as putColumn(arraySlices, arr) except that it puts for the given
    // rows instead of all rows.
    // <group>
    void putColumnCells (const RefRows& rows,
                         const Vector<Vector<Slice> >& arraySlices,
                         const Array<T>& arr);
    void putSliceFromRows (const RefRows& rows,
                           const Vector<Vector<Slice> >& arraySlices,
                           const Array<T>& source)
      { putColumnCells (rows, arraySlices, source); }
    // </group>

    // Put the same value in all cells of the column.
    void fillColumn (const Array<T>& value);

    // Put the contents of a column with the same data type into this column.
    // To put the contents of a column with a different data type into
    // this column, the function TableColumn::putColumn can be used
    // (provided the data type promotion is possible).
    // In fact, this function is an assignment operator with copy semantics.
    void putColumn (const ArrayColumn<T>& that);

    // The put() function like above which does not check shapes, etc.
    // It is faster and can be used for performance reasons if one
    // knows for sure that the arguments are correct.
    // E.g. it is used internally in virtual column engines.
    void basePut (uInt rownr, const Array<T>& array)
      { baseColPtr_p->put (rownr, &array); }

private:
    // Check if the data type matches the column data type.
    void checkDataType() const;

    // Check the shape of the array. If the array is empty or if
    // <src>resize=True</src>, the array is resized if needed.
    // An exception is thrown if not conforming.
    void checkShape (const IPosition& shp,
                     Array<T>& arr, Bool resize,
                     const String& where) const;

protected:
    // A common function used by all functions that can get or put irregular
    // array slices. The functor performs the get or put operation.
    void handleSlices (const Vector<Vector<Slice> >& slices,
                       BaseSlicesFunctor<T>& functor,
                       const Slicer& slicer,
                       IPosition& arrEnd,
                       Array<T>& array) const;

    // Keep switches to determine if a slice or an entire column can
    // be accessed or the change of an array can be changed.
    // True = yes;  False = no.
    mutable Bool canAccessSlice_p;
    mutable Bool canAccessColumn_p;
    mutable Bool canAccessColumnSlice_p;
    // Keep switches to know if access knowledge is permanent or has
    // to be asked again the next time.
    mutable Bool reaskAccessSlice_p;
    mutable Bool reaskAccessColumn_p;
    mutable Bool reaskAccessColumnSlice_p;
};


} //# NAMESPACE CASACORE - END


//# Make old name ROArrayColumn still available.
#define ROArrayColumn ArrayColumn


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/ArrayColumn.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
