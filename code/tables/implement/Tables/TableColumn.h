//# TableColumn.h: Access to a table column
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

#if !defined(AIPS_TABLECOLUMN_H)
#define AIPS_TABLECOLUMN_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/BaseColumn.h>
#include <aips/Utilities/String.h>
#include <aips/Lattices/IPosition.h>

//# Forward Declarations
class Table;
class BaseTable;


//# Check the number of rows in debug mode.
#define TABLECOLUMNCHECKROW(ROWNR)
#if defined(AIPS_DEBUG)
#undef TABLECOLUMNCHECKROW
#define TABLECOLUMNCHECKROW(ROWNR) \
    (checkRowNumber (ROWNR))
#endif


// <summary>
// Readonly access to a table column
// </summary>

// <use visibility=export>

// <reviewed reviewer="dschieb" date="1994/08/10" tests="none">
// </reviewed>

// <prerequisite>
//   <li> Table
//   <li> ColumnDesc
// </prerequisite>

// <etymology>
// ROTableColumn gives readonly access to an arbitrary column in a table.
// </etymology>

// <synopsis>
// The class ROTableColumn gives readonly access to a column
// in a table. In particular access to the column description
// (for name, data type, etc.) and to the column keyword set
// can be obtained. 
// Another important function is isDefined, which tests if a
// cell (i.e. table row) in a column contains a value.
//
// The classes ROScalarColumn<T> and ROArrayColumn<T> have to be
// used to get the data in the column cells.
// However, ROTableColumn has get functions for the basic data types
// (Bool, uChar, Short, uSort, Int, uInt, float, double,
//  Complex, DComplex and String).
// Opposite to the get functions in ROScalarColumn<T>, the
// ROTableColumn get functions support data type promotion.
//
// A default constructor is defined to allow construction of an array
// of ROTableColumn objects. However, this constructs an object not
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


class ROTableColumn
{
friend class ForwardColumn;      //# for function baseColPtr()

public:

    // The default constructor creates a null object, i.e. it
    // does not reference a table column.
    // The sole purpose of this constructor is to allow construction
    // of an array of ROTableColumn objects.
    // The functions reference and attach can be used to make a null object
    // reference a column.
    // Note that get functions, etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    ROTableColumn();

    // Construct the object for a column in the table using its name.
    ROTableColumn (const Table&, const String& columnName);

    // Construct the object for a column in the table using its index.
    // This allows to loop through all columns in a table as:
    // <srcblock>
    //    for (uInt=0; i<tab.ncolumn(); i++) {
    //        ROTableColumn tabcol(tab,i);
    //    }
    // </srcblock>
    ROTableColumn (const Table&, uInt columnIndex);

    // Copy constructor (reference semantics).
    ROTableColumn (const ROTableColumn&);

    virtual ~ROTableColumn();

    // Clone the object.
    virtual ROTableColumn* clone() const;

    // Change the reference to another column.
    // This is in fact an assignment operator with reference semantics.
    // It removes the reference to the current column and creates
    // a reference to the column referenced in the other object.
    // It will handle null objects correctly.
    void reference (const ROTableColumn&);

    // Attach a column to the object.
    // This is in fact only a shorthand for 
    // <<br><src> reference (ROTableColumn (table, columnName)); </src>
    // <group>
    void attach (const Table& table, const String& columnName)
	{ reference (ROTableColumn (table, columnName)); }
    void attach (const Table& table, uInt columnIndex)
	{ reference (ROTableColumn (table, columnIndex)); }
    // </group>

    // Test if the object is null, i.e. does not reference a column.
    Bool isNull() const
	{ return (baseColPtr_p == 0  ?  True : False); }

    // Throw an exception if the object is null, i.e.
    // if function isNull() is True.
    void throwIfNull() const;

    // Get const access to the column keyword set.
    const TableRecord& keywordSet() const
	{ return baseColPtr_p->keywordSet(); }

    // Get const access to the column description.
    // ColumnDesc functions have to be used to get the data type, etc..
    const ColumnDesc& columnDesc() const;

    // Get the number of rows in the column.
    uInt nrow() const
	{ return baseColPtr_p->nrow(); }

    // Get the global #dimensions of an array (ie. for all cells in the column).
    // This is always set for direct arrays and might be set for indirect
    // arrays. If not, 0 will be returned.
    uInt ndimColumn() const
	{ return baseColPtr_p->ndimColumn(); }

    // Get the global shape of an array (ie. for all cells in the column).
    // This is always set for direct arrays and might be set for indirect
    // arrays. If not, a 0-dim shape will be returned.
    IPosition shapeColumn() const
	{ return baseColPtr_p->shapeColumn(); }

    // Test if the given cell contains a defined value.
    Bool isDefined (uInt rownr) const
	{ TABLECOLUMNCHECKROW(rownr); return baseColPtr_p->isDefined (rownr); }

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

    // Check if the row number is valid.
    // It throws an exception if out of range.
    void checkRowNumber (uInt rownr) const;

    // Set the maximum cache size (in bytes) to be used by a storage manager.
    void setMaximumCacheSize (uInt nbytes) const
        { baseColPtr_p->setMaximumCacheSize (nbytes); }


protected:
    BaseTable*  baseTabPtr_p;
    BaseColumn* baseColPtr_p;                //# pointer to real column object
    const ColumnCache* colCachePtr_p;


    // Get the baseColPtr_p of this ROTableColumn object.
    BaseColumn* baseColPtr () const
	{ return baseColPtr_p; }

    // Get the baseColPtr_p of another ROTableColumn object.
    // This is needed for function put, because baseColPtr_p is a
    // protected member of ROTableColumn. Another TableColumn has
    // no access to that.
    BaseColumn* baseColPtr (const ROTableColumn& that) const
	{ return that.baseColPtr_p; }
	    

private:
    // Assignment makes no sense for a readonly class.
    // Declaring this operator private, makes it unusable.
    ROTableColumn& operator= (const ROTableColumn&);	
};



// <summary>
// Non-const access to a table column
// </summary>

// <use visibility=export>

// <reviewed reviewer="dschieb" date="1994/08/10" tests="none">
// </reviewed>

// <prerequisite>
//   <li> Table
//   <li> ROTableColumn
// </prerequisite>

// <synopsis>
// The class TableColumn augments the class ROTableColumn
// with write access to a table column.
//
// The classes ScalarColumn<T> and ArrayColumn<T> can be
// used to put typed data in the column cells.
//
// A default constructor is defined to allow construction of an array
// of TableColumn objects. However, this constructs an object not
// referencing a column. Functions like get, etc. will fail (i.e. result
// in a segmentation fault) is used on such objects. The functions
// isNull and throwIfNull can be used to test on this.
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


class TableColumn : virtual public ROTableColumn
{
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
    // function in case of doubt.
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

    ~TableColumn();

    // Clone the object.
    virtual ROTableColumn* clone() const;

    // Change the reference to another column.
    // This is in fact an assignment operator with reference semantics.
    // It removes the reference to the current column and creates
    // a reference to the column referenced in the other object.
    // It will handle null objects correctly.
    void reference (const TableColumn&);

    // Attach a column to the object.
    // This is in fact only a shorthand for 
    // <br><src> reference (TableColumn (table, columnName)); </src>
    // <group>
    void attach (const Table& table, const String& columnName)
	{ reference (TableColumn (table, columnName)); }
    void attach (const Table& table, uInt columnIndex)
	{ reference (TableColumn (table, columnIndex)); }
    // </group>

    // Get access to the column keyword set.
    TableRecord& rwKeywordSet()
	{ return baseColPtr_p->rwKeywordSet(); }

    // Copy the value of a cell of that column to a cell of this column.
    // This function only works for the standard data types.
    // Data type promotion will be done if needed.
    // An exception is thrown if the data cannot be converted.
    // <group>
    // Use the same row numbers for both cells.
    void put (uInt rownr, const ROTableColumn& that)
	{ TABLECOLUMNCHECKROW(rownr); put (rownr, that, rownr); }
    // Use possibly different row numbers for that (i.e. input) and
    // and this (i.e. output) cell.
    virtual void put (uInt thisRownr, const ROTableColumn& that,
		      uInt thatRownr);
    // </group>

    // Copy the values of that column to this column.
    // The numbers of rows in both columns must be equal.
    // Data type promotion is possible.
    // An exception is thrown if the data cannot be converted.
    // This function is useful to copy one column to another without
    // knowing their data types.
    // In fact, this function is an assignment operator with copy semantics.
    void putColumn (const ROTableColumn& that);

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

    //# The following routines add/remove/rename columns for the
    //# tables in all cells of the column.
    //# They are meant to add/rename/remove a keyword or column in a
    //# column containing tables without the flag VarDesc set; i.e. tables
    //# which should be the same in all rows.
    //# They are not allowed for a column with the VarDesc flag set.
    //# See ColumnDesc.h for more information on the VarDesc flag.
    //# <group>

    //# Add a column to all tables.
//#    virtual void addColumn (const ColumnDesc&);

    //# Rename a column in all tables.
//#    virtual void renameColumn (const String& newname, const String& oldname);

    //# Remove a column from all tables.
//#    virtual void removeColumn (const String& name);
    //# </group>

private:
    // Assigning one column to another suggests a deep copy.
    // Because the copy constructor has reference semantics, it was
    // felt it would be too confusing to allow assignment.
    // Instead the function reference (with reference semantics) and
    // putColumn (with copy semantics) exist.
    // Declaring this operator private, makes it unusable.
    TableColumn& operator= (const TableColumn&);
};



#endif
