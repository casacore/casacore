//# SclarColumn.h: access to a scalar table column with arbitrary data type
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

#if !defined(AIPS_SCALARCOLUMN_H)
#define AIPS_SCALARCOLUMN_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/ColumnCache.h>

//# Forward Declarations
class BaseColumn;
class RefRows;
template<class T> class Vector;
class String;


// <summary>
// Readonly access to a scalar table column with arbitrary data type
// </summary>

// <use visibility=export>

// <reviewed reviewer="dschieb" date="1994/08/10" tests="none">
// </reviewed>
 
// <prerequisite>
//   <li> Table
//   <li> ROTableColumn
// </prerequisite>

// <etymology>
// ROScalarColumn<T> gives readonly access to an column in a table
// containing a scalar with data type T.
// </etymology>

// <synopsis> 
// The class ROScalarColumn allows readonly access to a column
// containing scalar values with an arbitrary data type.
// It is possible to get the data in an individual cell (i.e. table row)
// and to get the column as a whole.
//
// A default constructor is defined to allow construction of an array
// of ROScalarColumn objects. However, this constructs an object not
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

// <example>
// See module <linkto module="Tables#open">Tables</linkto>.
// </example>


template<class T>
class ROScalarColumn : virtual public ROTableColumn
{
public:

    // The default constructor creates a null object, i.e. it
    // does not reference a table column.
    // The sole purpose of this constructor is to allow construction
    // of an array of ROScalarColumn objects.
    // The functions reference and attach can be used to make a null object
    // reference a column.
    // Note that get functions, etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    ROScalarColumn();

    // Construct for the given column in the given table.
    ROScalarColumn (const Table&, const String& columnName);

    // Construct from the given table column.
    // This constructor is useful if first a table column was constructed,
    // its type is determined and thereafter used to construct the
    // correct column object.
    ROScalarColumn (const ROTableColumn&);

    // Copy constructor (reference semantics).
    ROScalarColumn (const ROScalarColumn<T>&);

    ~ROScalarColumn();

    // Clone the object.
    virtual ROTableColumn* clone() const;

    // Change the reference to another column.
    // This is in fact an assignment operator with reference semantics.
    // It removes the reference to the current column and creates
    // a reference to the column referenced in the other object.
    // It will handle null objects correctly.
    void reference (const ROScalarColumn<T>&);

    // Attach a column to the object.
    // This is in fact only a shorthand for 
    // <br><src> reference (ROScalarColumn<T> (table, columnName)); </src>
    void attach (const Table& table, const String& columnName)
	{ reference (ROScalarColumn<T> (table, columnName)); }

    // Get the data from a particular cell (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    // <group>
    void get (uInt rownr, T& value) const
    {
	TABLECOLUMNCHECKROW(rownr);
	Int off = colCachePtr_p->offset(rownr);
	if (off >= 0) {
	    value = ((T*)(colCachePtr_p->dataPtr()))[off];
	}else{
	    baseColPtr_p->get (rownr, &value);
	}
    }
    T operator() (uInt rownr) const
    {
	T value;
	get (rownr, value);
	return value;
    }
    // </group>

    // Get the vector of all values in the column.
    // According to the assignment rules of class Array, the destination
    // vector must be empty or its length must be the number of cells
    // in the column (i.e. the number of rows in the table).
    void getColumn (Vector<T>& vec, Bool resize = False) const;

    // Get the vector of all values in the column.
    Vector<T> getColumn() const;

    // Get the vector of a range of values in the column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to get.
    // According to the assignment rules of class Array, the destination
    // vector must be empty or its length must be the number of cells
    // in the column (i.e. the number of rows in the slicer).
    void getColumnRange (const Slicer& rowRange, Vector<T>& vec,
			 Bool resize = False) const;

    // Get the vector of a range of values in the column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to get..
    Vector<T> getColumnRange (const Slicer& rowRange) const;

    // Get the vector of some values in the column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to get.
    // According to the assignment rules of class Array, the destination
    // vector must be empty or its length must be the number of cells
    // in the column (i.e. the number of rows in the RefRows object).
    void getColumnCells (const RefRows& rownrs, Vector<T>& vec,
			 Bool resize = False) const;

    // Get the vector of some values in the column.
    Vector<T> getColumnCells (const RefRows& rownrs) const;

private:
    // Assignment makes no sense for a readonly class.
    // Declaring this operator private, makes it unusable.
    ROScalarColumn<T>& operator= (const ROScalarColumn<T>&);

    // Referencing any other typed column cannot be done.
    // If we do not put this function here, the conversion constructor
    //  ROScalarColumn (const ROTableColumn&)
    // will be used and no compile error is given.
    void reference (const ROTableColumn&);

    // Check if the data type matches the column data type.
    void checkDataType() const;

protected:
    // Keep a switch to determine if an entire column can be accessed.
    // All switches are Bool*, because they can be changed in a const function.
    // True = yes;  False = no.
    Bool* canAccessColumn_p;
    // Keep a switch to know if access knowledge is permanent or has
    // to be asked again the next time.
    Bool* reaskAccessColumn_p;
};




// <summary>
// Read/write access to a scalar table column with arbitrary data type
// </summary>

// <use visibility=export>

// <reviewed reviewer="dschieb" date="1994/08/10" tests="none">
// </reviewed>

// <prerequisite>
//   <li> Table
//   <li> TableColumn
//   <li> ROScalarColumn
// </prerequisite>

// <synopsis> 
// The class ScalarColumn allows read/write access to a column
// containing scalar values with an arbitrary data type.
// It augments the class ROScalarColumn with the possibility
// to put data into a cell or into the whole column.
//
// A default constructor is defined to allow construction of an array
// of ScalarColumn objects. However, this constructs an object not
// referencing a column. Functions like get, etc. will fail (i.e. result
// in a segmentation fault) when used on such objects. The functions
// ROTableColumn::isNull and throwIfNull can be used to test on this.
// The functions attach and reference can fill in the object.
//
// The assignment operator is not defined for this class, because it was
// felt it would be too confusing. Instead the function reference can
// be used to do assignment with reference semantics. An assignment
// with copy semantics can be done with a putColumn function.
// </synopsis> 

// <example>
// See module <linkto module="Tables#open">Tables</linkto>.
// </example>

template<class T> class ScalarColumn : public ROScalarColumn<T>,
                                       public TableColumn
{
public:

    // The default constructor creates a null object, i.e. it
    // does not reference a table column.
    // The sole purpose of this constructor is to allow construction
    // of an array of ScalarColumn objects.
    // The functions reference and attach can be used to make a null object
    // reference a column.
    // Note that get functions, etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    ScalarColumn();

    // Construct the scalar column object for the table column.
    ScalarColumn (const Table&, const String& columnName);

    // Construct from the given table column.
    // This constructor is useful if first a table column was constructed,
    // its type is determined and thereafter used to construct the
    // correct column object.
    ScalarColumn (const TableColumn&);

    // Copy constructor (reference semantics).
    ScalarColumn (const ScalarColumn<T>&);

    ~ScalarColumn();

    // Clone the object.
    virtual ROTableColumn* clone() const;

    // Change the reference to another column.
    // This is in fact an assignment operator with reference semantics.
    // It removes the reference to the current column and creates
    // a reference to the column referenced in the other object.
    // It will handle null objects correctly.
    void reference (const ScalarColumn<T>&);

    // Attach a column to the object.
    // This is in fact only a shorthand for 
    // <br><src> reference (ScalarColumn<T> (table, columnName)); </src>
    void attach (const Table& table, const String& columnName)
	{ reference (ScalarColumn<T> (table, columnName)); }

    // Put the value in a particular cell (i.e. table row).
    // The row numbers count from 0 until #rows-1.
    void put (uInt rownr, const T& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->put (rownr, &value); }

    // Copy the value of a cell of that column to a cell of this column.
    // The data types of both columns must be the same.
    // <group>
    // Use the same row numbers for both cells.
    void put (uInt rownr, const ROScalarColumn<T>& that)
	{ put (rownr, that, rownr); }
    // Use possibly different row numbers for that (i.e. input) and
    // and this (i.e. output) cell.
    void put (uInt thisRownr, const ROScalarColumn<T>& that, uInt thatRownr);
    // </group>

    // Copy the value of a cell of that column to a cell of this column.
    // This function uses a generic ROTableColumn object as input.
    // If possible the data will be promoted to the data type of this column.
    // Otherwise an exception is thrown.
    // <group>
    // Use the same row numbers for both cells.
    void put (uInt rownr, const ROTableColumn& that)
	{ put (rownr, that, rownr); }
    // Use possibly different row numbers for that (i.e. input) and
    // and this (i.e. output) cell.
    void put (uInt thisRownr, const ROTableColumn& that, uInt thatRownr);
    // </group>

    // Put the vector of all values in the column.
    // The length of the vector must be the number of cells in the column
    // (i.e. the number of rows in the table).
    void putColumn (const Vector<T>& vec);

    // Put the vector of a range of values in the column.
    // The Slicer object can be used to specify start, end (or length),
    // and stride of the rows to put.
    // The length of the vector must be the number of cells in the slice.
    void putColumnRange (const Slicer& rowRange, const Vector<T>& vec);

    // Put the vector of some values in the column.
    // The length of the vector must be the number of cells in the RefRows
    // object.
    void putColumnCells (const RefRows& rownrs, const Vector<T>& vec);

    // Put the same value in all cells of the column.
    void fillColumn (const T& value);

    // Put the contents of a column with the same data type into this column.
    // To put the contents of a column with a different data type into
    // this column, the function TableColumn::putColumn can be used
    // (provided the data type promotion is possible).
    // In fact, this function is an assignment operator with copy semantics.
    void putColumn (const ROScalarColumn<T>& that);

private:
    // Assigning one column to another suggests a deep copy.
    // Because the copy constructor has reference semantics, it was
    // felt it would be too confusing to allow assignment.
    // Instead the function reference (with reference semantics) and
    // putColumn (with copy semantics) exist.
    // Declaring this operator private, makes it unusable.
    ScalarColumn<T>& operator= (const ScalarColumn<T>&);

    // Referencing any other typed column cannot be done.
    // If we do not put this function here, the conversion constructor
    //  <src>ROScalarColumn (const TableColumn&)</src>
    // will be used and no compile error is given.
    // <group>
    void reference (const TableColumn&);
    void putColumn (const ROTableColumn&);
    // </group>
};



#endif
