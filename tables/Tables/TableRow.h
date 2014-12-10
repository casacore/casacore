//# TableRow.h: Access to a table row
//# Copyright (C) 1996,1999,2001
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

#ifndef TABLES_TABLEROW_H
#define TABLES_TABLEROW_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableColumn;
template<class T> class Vector;


// <summary>
// Readonly access to a table row
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1996/05/10" tests="tTableRow.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=TableRecord>TableRecord</linkto>
// </prerequisite>

// <synopsis>
// This class provides easy access to the contents of a table,
// one row at a time. 'Normal' access to a table is by columns, each of
// which contains values of the same type.
// A table row, by contrast, will be a collection
// of heterogeneous data, similar to a C struct. For
// this reason, the TableRow classes (ROTableRow and TableRow) are built
// around and provide access to the class
// <linkto class=TableRecord> TableRecord </linkto>.
// The TableRow delegates much of its behaviour to the TableRecord class.
// For example: 
// <srcblock>
//   Table table ("some.table");
//   ROTableRow row (table);                 // construct TableRow object
//   cout << row.record().description();     // show its description
//   // Get the values in row 17.
//   const TableRecord& record = row.get (17);
//   // column name is "Title", and automatically becomes the record
//   // key for this field of the record:
//   String row17title = record.asString ("Title");  
//   Int    row17count = record.asInt ("Count");
// </srcblock>
// The simplest constructor will include all columns in the TableRow object
// (although columns with a non-standard data type will be excluded,
// because they cannot be represented in a TableRecord).
// However, it is possible to be more selective and to include only
// some columns in the TableRow object. The various constructors show
// how this can be done.
// <p>
// It is possible to have multiple TableRow objects for the same table.
// They can contain different columns or they can share columns.
// 
// <p>
// On construction an internal <linkto class=TableRecord>TableRecord</linkto>
// object is created containing the required fields. The contents of this
// record will be changed with each get call, but the structure of it is
// fixed. This means that <linkto class=RORecordFieldPtr>RORecordFieldPtr
// </linkto> objects can be constructed once and used many times.
// This results in potentially faster access to the record, because it avoids
// unnecessary name lookups.
// </synopsis>

// <example>
// <srcblock>
// // Open the table as readonly and define a row object containing
// // the given columns.
// // Note that the function stringToVector is a very convenient
// // way to construct a Vector<String>.
// // Show the description of the fields in the row.
// Table table("Some.table");
// ROTableRow row (table, stringToVector("col1,col2,col3"));
// cout << row.record().description();
// // Loop through all rows and get their values.
// for (uInt i=0; i<table.nrow(); i++) {
//     const TableRecord& values = row.get (i);
//     someString = values.asString ("col1");
//     somedouble = values.asdouble ("col2");
//     someArrayInt = values.asArrayInt ("col3");
// }
//
// // Provided the structure of the record is known, the RecordFieldPtr
// // objects could be used as follows.
// // This is faster than the previous method, because it avoids a name
// // lookup for each iteration.
// RORecordFieldPtr<String> col1(row.record(), "col1");
// RORecordFieldPtr<double> col2(row.record(), "col2");
// RORecordFieldPtr<Array<Int> > col3(row.record(), "col3");
// for (uInt i=0; i<table.nrow(); i++) {
//     row.get (i);
//     someString = *col1;
//     somedouble = *col2;
//     someArrayInt = *col3;
// }
// </srcblock>
// Please note that the TableRecord& returned by the get() function is the
// same as returned by the record() function. Therefore the RORecordField
// objects can be created in advance.
// </example>

class ROTableRow
{
public:
    // Create a detached ROTableRow object.
    // This means that no Table, etc. is contained in it.
    // Function isAttached will return False for it.
    // <br>
    // This constructor should normally not be used, because it does not
    // result in a valid object. It should only be used when really needed
    // (e.g. when an array of objects has to be used).
    ROTableRow();

    // Create a ROTableRow object for the given Table.
    // Its TableRecord will contain all columns except columns with
    // datatype TpOther (i.e. non-standard data types).
    // <br>
    // If the flag <src>storedColumnsOnly</src> is True, only the
    // columns actually stored by a storage manager will be selected.
    // This is useful when the contents of an entire row have to be copied.
    // Virtual columns are calculated on-the-fly (often using stored columns),
    // thus it makes no sense to copy their data.
    // <note role=caution>
    //  If the table contains columns with large arrays, it may
    //  be better not to use this constructor. Each get will read in
    //  all data in the row, thus also the large data array(s).
    //  In that case it is better to use the constructor which
    //  includes selected columns only.
    // </note>
    explicit ROTableRow (const Table& table, Bool storedColumnsOnly = True);

    // Create a ROTableRow object for the given Table.
    // Its TableRecord will contain all columns given in the Vector.
    // An exception is thrown if an unknown column name is given.
    // <br>
    // When exclude=True, all columns except the given columns are taken.
    // In that case an unknown name does not result in an exception.
    ROTableRow (const Table& table, const Vector<String>& columnNames,
		Bool exclude = False);

    // Copy constructor (copy semantics).
    ROTableRow (const ROTableRow&);

    ~ROTableRow();

    // Assignment (copy semantics).
    ROTableRow& operator= (const ROTableRow&);

    // Test if a Table is attached to this object.
    Bool isAttached() const;

    // Get the Table used for this object.
    const Table& table() const;

    // Get the record containing all fields.
    const TableRecord& record() const;

    // Get the number of the last row read.
    // -1 is returned when no Table is attached or no row has been read yet.
    Int64 rowNumber() const;

    // Get a vector consisting of all columns names.
    // This can, for instance, be used to construct a TableRow object
    // with the same columns in another table.
    Vector<String> columnNames() const;

    // Get the values of all columns used from the given row.
    // When the given row number equals the current one, nothing
    // will be read unless the alwaysRead flag is set to True.
    // <br>The TableRecord& returned is the same one as returned by the
    // record() function. So one can ignore the return value of get().
    const TableRecord& get (uInt rownr, Bool alwaysRead = False) const;

    // Get the block telling for each column if its value in the row
    // was indefined in the table.
    // Note that array values might be undefined in the table, but in
    // the record they will be represented as empty arrays.
    const Block<Bool>& getDefined() const;

protected:
    // Copy that object to this object.
    // The writable flag determines if writable or readonly
    // TableColumn objects will be created.
    void copy (const ROTableRow& that);

    // Create the record, column, and field objects
    // for all columns in the table.
    // The writable flag determines if writable or readonly
    // TableColumn objects will be created.
    void create (const Table& table, Bool storedColumnsOnly, Bool writable);

    // Create the record, column, and field objects for the given columns.
    // The writable flag determines if writable or readonly
    // TableColumn objects will be created.
    void create (const Table& table, const Vector<String>& columnNames,
		 Bool exclude, Bool writable);

    // Put the values found in the internal TableRecord at the given row.
    // This is a helper function for class TableRow.
    void putRecord (uInt rownr);

    // Put a value in the given field in the TableRecord into the
    // given row and column.
    // This is a helper function for class TableRow.
    void putField (uInt rownr, const TableRecord& record,
		   Int whichColumn, Int whichField);

    // Set the switch to reread when the current row has been put.
    void setReread (uInt rownr);

    //# The record of all fields.
    TableRecord* itsRecord;
    //# The table used.
    Table        itsTable;
    //# The following block is actually a Block<TableColumn*>.
    //# However, using void* (and appropriate casts) saves on template
    //# instantiations.
    Block<void*> itsTabCols;
    //# The following block is actually a Block<Scalar/ArrayColumn<T>>.
    Block<void*> itsColumns;
    //# The following block is actually a block of RecordFieldPtr<T>*.
    //# These are used for fast access to the record.
    Block<void*> itsFields;
    //# Block to tell if the corresponding column value is defined.
    mutable Block<Bool> itsDefined;
    //# A cache for itsRecord.nfields()
    uInt         itsNrused;
    //# The last rownr read (-1 is nothing read yet).
    mutable Int64 itsLastRow;
    //# A switch to indicate that the last row has to be reread.
    //# This is the case when it has been put after being read.
    mutable Bool  itsReread;

private:
    // Initialize the object.
    void init();

    // Make a RecordDesc from the table with some excluded column names.
    void makeDescExclude (RecordDesc& description,
			  const Vector<String>& columnNames,
			  Bool writable);

    // Add a column to the record.
    // When skipOther is True, columns with a non-standard data type
    // will be silently skipped.
    void addColumnToDesc (RecordDesc& description,
			  const TableColumn& column, Bool skipOther);

    // Make the required objects. These are the TableRecord and for
    // each column a TableColumn and RecordFieldPtr.
    void makeObjects (const RecordDesc& description);

    // Delete all objects.
    void deleteObjects();
};




// <summary>
// Read/write access to a table row
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/05/10" tests="tTableRow.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ROTableRow>ROTableRow</linkto>
// </prerequisite>

// <synopsis>
// The class TableRow is derived from ROTableRow and as an extra it
// provides write-access to a row in a table.
// With the put function, all values in the TableRecord object will
// be put in the corresponding columns in the table row.
// There is, however, an extra consideration:
// <ul>
// <li> Constructing a TableRow object fails if the table is not
//      writable. Non-writable columns will not be part of the object.
//      If an explicitly given column is non-writable, the construction
//      will also fail.
// </ul>
// There are effectively 3 ways of writing data.
// <ol>
// <li> The function
//      <srcblock>
//          put (rownr, tableRecord);
//      </srcblock>
//      can be used to put all values from the given TableRecord,
//      which has to be conforming (i.e. matching order and names).
//      Optionally the conformance is checked.
//      This put function is capable of data type promotion.
//      For instance, if column COL1 is float, the corresponding
//      field in the TableRecord can be Int.
// <li> A faster way is using the functions <src>record</src>
//      and <src>put</src>. It is possible to use <linkto class=RecordFieldPtr>
//      RecordFieldPtr</linkto> objects to get direct access to the
//      fields in the record (provided the structure of the record
//      is known).
//      E.g.
//      <srcblock>
//         TableRow row (someTable, stringToVector("col1,col2,col3"));
//         RecordFieldPtr<String> col1(row.record(), "col1");
//         RecordFieldPtr<double> col2(row.record(), "col2");
//         RecordFieldPtr<Array<Int> > col3(row.record(), "col3");
//         for (uInt i=0; i<n; i++) {
//             *col1 = someString;
//             *col2 = somedouble;
//             *col3 = someArrayInt;
//             row.put (i);
//         }
//      </srcblock>
// <li>
// <li> The function
//      <srcblock>
//          putMatchingFields (rownr, tableRecord);
//      </srcblock>
//      can be used to put some fields from the given TableRecord.
//      Only fields having a corresponding name in the TableRow object
//      will be put. Similar to the first way data type promotion will
//      be applied for numeric scalars.
//      <br>E.g.: Suppose the TableRow object has columns A, C, and B,
//      and the given TableRecord has fields B, D, and C. Only fields B and C
//      will be put. As the example shows, the order of the fields is not
//      important.
//      <br>
//      This way is (much) slower than the other 2, because a name
//      lookup is involved for each field. It can, however, be more
//      convenient to use.
// </ol>
// </synopsis>

// <example>
// <srcblock>
// // Open the new table (with 10 rows) and define a row object containing
// // values from the given column.
// // Note that the function stringToVector is a very convenient
// // way to construct a Vector<String>.
// SetupNewTable newtab(tableDesc, Table::new);
// Table table(newtab, 10);
// TableRow row (table, stringToVector("col1,col2,col3,col4"));
// // Loop through all rows and get their values.
// for (uInt i=0; i<table.nrow(); i++) {
//     // Some magic filler function returns a filled TableRecord
//     // (with the correct fields in the correct order).
//     TableRecord record = fillerFunction();
//     row.put (i, record);
// }
// </srcblock>
// </example>

class TableRow : public ROTableRow
{
public:
    // Create a detached TableRow object.
    // This means that no Table, etc. is contained in it.
    // Function isAttached (in the base class) will return False for it.
    // <br>
    // This constructor should normally not be used, because it does not
    // result in a valid object. It should only be used when really needed
    // (e.g. when an array of objects has to be used).
    TableRow();

    // Create a TableRow object for the given Table.
    // Its TableRecord will contain all columns except columns with
    // datatype TpOther and columns which are not writable.
    // <br>
    // If the flag <src>storedColumnsOnly</src> is True, only the
    // columns actually stored by a storage manager will be selected.
    // This is useful when the contents of an entire row have to be copied.
    // Virtual columns are calculated on-the-fly (often using stored columns),
    // thus it makes no sense to copy their data.
    // <note role=caution>
    //  If the table contains columns with large arrays, it may
    //  be better not to use this constructor. Each get will read in
    //  all data in the row, thus also the large data array(s).
    //  In that case it is better to use the next constructor which
    //  works selectively.
    // </note>
    explicit TableRow (const Table& table, Bool storedColumnsOnly = True);

    // Create a TableRow object for the given Table.
    // Its TableRecord will contain all columns given in the Vector.
    // An exception is thrown if an unknown column name is given
    // or if a column is given which is not writable.
    // <br>
    // When exclude=True, all columns except the given columns are taken.
    // In that case an unknown name does not result in an exception
    // and non-writable columns are simply skipped.
    TableRow (const Table& table, const Vector<String>& columnNames,
	      Bool exclude = False);

    // Copy constructor (copy semantics).
    TableRow (const TableRow&);

    ~TableRow();

    // Assignment (copy semantics).
    TableRow& operator= (const TableRow&);

    // Get non-const access to the TableRecord in this object.
    // This can be used to change values in it which can thereafter
    // be put using the function <src>put(rownr)</src>.
    // <note> The returned TableRecord has a fixed structure, so it is
    //        not possible to add or remove fields. It is only possible
    //        to change values.
    // </note>
    TableRecord& record();

    // Put into the last row read.
    // An exception is thrown if no row has been read yet.
    // The values in the TableRecord contained in this object are put.
    // This TableRecord can be accessed and updated using the
    // function <src>record</src>.
    void put();

    // Put into the given row.
    // The values in the TableRecord contained in this object are put.
    // This TableRecord can be accessed and updated using the
    // function <src>record</src>.
    void put (uInt rownr);

    // Put the values found in the TableRecord in the appropriate columns
    // in the given row.
    // The names and order of the fields in the TableRecord must conform
    // those of the description of the TableRow. The data types of numeric
    // scalars do not need to conform exactly; they can be promoted
    // (e.g. an Int value in the record may correspond to a float column).
    // If not conforming, an exception is thrown.
    // <note> For performance reasons it is optional to check
    //        the name order conformance.
    // </note>
    // The <src>valuesDefined</src> block tells if the value in the
    // corresponding field in the record is actually defined.
    // If not, nothing will be written.
    // It is meant for array values which might be undefined in a table.
    // <group>
    void put (uInt rownr, const TableRecord& record,
	      Bool checkConformance = True);
    void put (uInt rownr, const TableRecord& record,
	      const Block<Bool>& valuesDefined,
	      Bool checkConformance = True);
    // </group>

    // Put the values found in the TableRecord. Only fields with a matching
    // name in the TableRow object will be put.
    // This makes it possible to put fields in a selective way.
    // <br>E.g.: If the TableRow contains columns A and B, and the
    // record contains fields B and C, only field B will be put.
    // <br>In principle the data types of the matching fields must match,
    // but data type promotion of numeric scalars will be applied.
    void putMatchingFields (uInt rownr, const TableRecord& record);

private:
    // Check if the names of the given record match this row.
    Bool namesConform (const TableRecord& that) const;
};


inline Bool ROTableRow::isAttached() const
{
    return  (itsRecord != 0);
}
inline const Table& ROTableRow::table() const
{
    return itsTable;
}
inline Int64 ROTableRow::rowNumber() const
{
    return itsLastRow;
}
inline const TableRecord& ROTableRow::record() const
{
    return *itsRecord;
}
inline const Block<Bool>& ROTableRow::getDefined() const
{
    return itsDefined;
}
inline TableRecord& TableRow::record()
{
    return *itsRecord;
}
inline void TableRow::put (uInt rownr)
{
    putRecord (rownr);
}



} //# NAMESPACE CASACORE - END

#endif
