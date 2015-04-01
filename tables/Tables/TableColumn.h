//# TableColumn.h: Access to a table column
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

#ifndef TABLES_TABLECOLUMN_H
#define TABLES_TABLECOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseColumn.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Table;
class BaseTable;


//# Check the number of rows in debug mode.
#if defined(AIPS_DEBUG)
# define TABLECOLUMNCHECKROW(ROWNR) \
    (checkRowNumber (ROWNR))
#else
# define TABLECOLUMNCHECKROW(ROWNR)
#endif


// <summary>
// Read/write access to a table column
// </summary>

// <use visibility=export>

// <reviewed reviewer="dschieb" date="1994/08/10" tests="none">
// </reviewed>

// <prerequisite>
//   <li> Table
//   <li> ColumnDesc
// </prerequisite>

// <synopsis>
// The class TableColumn gives read and write access to a column
// in a table. In particular access to the column description
// (for name, data type, etc.) and to the column keyword set
// can be obtained. 
// Another important function is isDefined, which tests if a
// cell (i.e. table row) in a column contains a value.
//
// The classes ScalarColumn<T> and ArrayColumn<T> have to be
// used to get/put the data in the column cells.
// However, TableColumn has get functions for the basic data types
// (Bool, uChar, Short, uSort, Int, uInt, float, double,
//  Complex, DComplex and String).
// Opposite to the get functions in ScalarColumn<T>, the
// TableColumn get functions support data type promotion.
//
// A default constructor is defined to allow construction of an array
// of TableColumn objects. However, this constructs an object not
// referencing a column. Functions like get, etc. will fail (i.e. result
// in a segmentation fault) when used on such objects. The functions
// isNull and throwIfNull can be used to test on this.
// The functions attach and reference can fill in the object.
// </synopsis>

// <example>
// See module <linkto module="Tables#open">Tables</linkto>.
// </example>


class TableColumn
{
friend class ForwardColumn;      //# for function baseColPtr()

public:

    // The default constructor creates a null object, i.e. it
    // does not reference a table column.
    // The sole purpose of this constructor is to allow construction
    // of an array of TableColumn objects.
    // The functions reference and attach can be used to make a null object
    // reference a column.
    // Note that get functions, etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    TableColumn();

    // Construct the object for a column in the table using its name.
    TableColumn (const Table&, const String& columnName);

    // Construct the object for a column in the table using its index.
    // This allows to loop through all columns in a table as:
    // <srcblock>
    //    for (uInt=0; i<tab.ncolumn(); i++) {
    //        TableColumn tabcol(tab,i);
    //    }
    // </srcblock>
    TableColumn (const Table&, uInt columnIndex);

    // Copy constructor (reference semantics).
    TableColumn (const TableColumn&);

    virtual ~TableColumn();

    // Assignment has reference semantics.
    // It copies the object, not the data of that column to this column.
    // Function <src>putColumn</src> can be used to copy the data of a column.
    // <br>It does the same as the reference function.
    TableColumn& operator= (const TableColumn&);	

    // Clone the object.
    virtual TableColumn* clone() const;

    // Change the reference to another column.
    // This is in fact an assignment operator with reference semantics.
    // It removes the reference to the current column and creates
    // a reference to the column referenced in the other object.
    // It will handle null objects correctly.
    void reference (const TableColumn&);

    // Attach a column to the object.
    // This is in fact only a shorthand for 
    // <<br><src> reference (TableColumn (table, columnName)); </src>
    // <group>
    void attach (const Table& table, const String& columnName)
	{ reference (TableColumn (table, columnName)); }
    void attach (const Table& table, uInt columnIndex)
	{ reference (TableColumn (table, columnIndex)); }
    // </group>

    // Test if the object is null, i.e. does not reference a column.
    Bool isNull() const
	{ return (baseColPtr_p == 0  ?  True : False); }

    // Throw an exception if the object is null, i.e.
    // if function isNull() is True.
    void throwIfNull() const;

    // Test if the column can be written to, thus if the column and
    // the underlying table can be written to.
    Bool isWritable() const
        { return baseTabPtr_p->isWritable()  &&  isColWritable_p; }

    // Test if the column is writable at all (virtual columns might not be).
    // Note that keywords can always be written, even for virtual columns.
    Bool isWritableAtAll() const
        { return isColWritable_p; }

    // Check if the column is writable and throw an exception if not.
    void checkWritable() const
        { if (!isWritable()) throwNotWritable(); }

    // Get readonly access to the column keyword set.
    const TableRecord& keywordSet() const
	{ return baseColPtr_p->keywordSet(); }

    // Get read/write access to the column keyword set.
    // An exception is thrown if the table is not writable.
    TableRecord& rwKeywordSet();

    // Get const access to the column description.
    // ColumnDesc functions have to be used to get the data type, etc..
    const ColumnDesc& columnDesc() const;

    // Get the Table object this column belongs to.
    Table table() const;

    // Get the number of rows in the column.
    uInt nrow() const
	{ return baseColPtr_p->nrow(); }

    // Can the shape of an already existing non-FixedShape array be changed?
    // This depends on the storage manager. Most storage managers
    // can handle it, but TiledDataStMan and TiledColumnStMan can not.
    Bool canChangeShape() const
        { return canChangeShape_p; }

    // Get the global #dimensions of an array (ie. for all cells in column).
    // This is always set for fixed shape arrays.
    // Otherwise, 0 will be returned.
    uInt ndimColumn() const
	{ return baseColPtr_p->ndimColumn(); }

    // Get the global shape of an array (ie. for all cells in the column).
    // This is always set for fixed shape arrays.
    // Otherwise, a 0-dim shape will be returned.
    IPosition shapeColumn() const
	{ return baseColPtr_p->shapeColumn(); }

    // Test if the given cell contains a defined value.
    Bool isDefined (uInt rownr) const
	{ TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->isDefined (rownr); }

    // Does the column has content in the given row (default is the first row)?
    // It has if it is defined and does not contain an empty array.
    Bool hasContent (uInt rownr=0) const;

    // Get the #dimensions of an array in a particular cell.
    uInt ndim (uInt rownr) const
	{ TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->ndim (rownr); }

    // Get the shape of an array in a particular cell.
    IPosition shape (uInt rownr) const
	{ TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->shape (rownr); }

    // Get the value of a scalar in the given row.
    // Data type promotion is possible.
    // These functions only work for the standard data types.
    // <group>
    void getScalar (uInt rownr, Bool& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, uChar& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, Short& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, uShort& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, Int& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, uInt& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, Int64& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, float& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, double& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, Complex& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, DComplex& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    void getScalar (uInt rownr, String& value) const
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr, value); }
    // </group>

    // Get the value from the row and convert it to the required type.
    // This can only be used for scalar columns with a standard data type.
    // <group>
    Bool     asBool     (uInt rownr) const;
    uChar    asuChar    (uInt rownr) const;
    Short    asShort    (uInt rownr) const;
    uShort   asuShort   (uInt rownr) const;
    Int      asInt      (uInt rownr) const;
    uInt     asuInt     (uInt rownr) const;
    float    asfloat    (uInt rownr) const;
    double   asdouble   (uInt rownr) const;
    Complex  asComplex  (uInt rownr) const;
    DComplex asDComplex (uInt rownr) const;
    String   asString   (uInt rownr) const;
    // </group>

    // Get the value of a scalar in the given row.
    // These functions work for all data types.
    // Data type promotion is possible for the standard data types.
    // The functions are primarily meant for ScalarColumn<T>.
    // <group>
    void getScalarValue (uInt rownr, Bool* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, uChar* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, Short* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, uShort* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, Int* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, uInt* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, float* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, double* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, Complex* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, DComplex* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, String* value, const String&) const
        { TABLECOLUMNCHECKROW(rownr); baseColPtr_p->getScalar (rownr,*value); }
    void getScalarValue (uInt rownr, void* value,
			 const String& dataTypeId) const
        { TABLECOLUMNCHECKROW(rownr);
	  baseColPtr_p->getScalar (rownr,value,dataTypeId); }
    // </group>

    // Copy the value of a cell of that column to a cell of this column.
    // This function only works for the standard data types.
    // Data type promotion will be done if needed.
    // An exception is thrown if this column is not writable or if
    // the data cannot be converted.
    // <group>
    // Use the same row numbers for both cells.
    void put (uInt rownr, const TableColumn& that)
	{ TABLECOLUMNCHECKROW(rownr); put (rownr, that, rownr); }
    // Use possibly different row numbers for that (i.e. input) and
    // and this (i.e. output) cell.
    virtual void put (uInt thisRownr, const TableColumn& that,
		      uInt thatRownr);
    // </group>

    // Copy the values of that column to this column.
    // The numbers of rows in both columns must be equal.
    // Data type promotion is possible.
    // An exception is thrown if the data cannot be converted.
    // This function is useful to copy one column to another without
    // knowing their data types.
    // In fact, this function is an assignment operator with copy semantics.
    void putColumn (const TableColumn& that);

    // Put the value of a scalar in the given row.
    // Data type promotion is possible.
    // These functions only work for the standard data types.
    // <group>
    void putScalar (uInt rownr, const Bool& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const uChar& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const Short& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const uShort& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const Int& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const uInt& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const float& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const double& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const Complex& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const DComplex& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const String& value)
	{ TABLECOLUMNCHECKROW(rownr); baseColPtr_p->putScalar (rownr, value); }
    void putScalar (uInt rownr, const Char* value)
	{ putScalar (rownr, String(value)); }
    // </group>

    // Check if the row number is valid.
    // It throws an exception if out of range.
    void checkRowNumber (uInt rownr) const
        { baseTabPtr_p->checkRowNumber (rownr); }

    // Set the maximum cache size (in bytes) to be used by a storage manager.
    void setMaximumCacheSize (uInt nbytes) const
        { baseColPtr_p->setMaximumCacheSize (nbytes); }

protected:
    BaseTable*  baseTabPtr_p;
    BaseColumn* baseColPtr_p;                //# pointer to real column object
    const ColumnCache* colCachePtr_p;
    Bool canChangeShape_p;
    Bool isColWritable_p;                    //# is the column writable at all?


    // Get the baseColPtr_p of this TableColumn object.
    BaseColumn* baseColPtr () const
	{ return baseColPtr_p; }

    // Get the baseColPtr_p of another TableColumn object.
    // This is needed for function put, because baseColPtr_p is a
    // protected member of TableColumn. Another TableColumn has
    // no access to that.
    BaseColumn* baseColPtr (const TableColumn& that) const
	{ return that.baseColPtr_p; }

private:
    // Throw the exception that the column is not writable.
    void throwNotWritable() const;
};


// Define ROTableColumn for backward compatibility.
typedef TableColumn ROTableColumn;


} //# NAMESPACE CASACORE - END

#endif
