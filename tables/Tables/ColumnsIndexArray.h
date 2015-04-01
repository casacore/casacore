//# ColumnsIndexArray.h: Index to an array column in a table
//# Copyright (C) 2001,2002
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

#ifndef TABLES_COLUMNSINDEXARRAY_H
#define TABLES_COLUMNSINDEXARRAY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class TableColumn;


// <summary>
// Index to an array column in a table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tColumnsIndexArray.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=Record>Record</linkto>
//   <li> <linkto class=RecordFieldPtr>RecordFieldPtr</linkto>
// </prerequisite>

// <synopsis>
// This class makes it possible to use transient indices on top
// of an array column in a table in order to speed up the process of
// finding rows based on a given key or key range.
// It is similar to class <linkto class=ColumnsIndex>ColumnsIndex</linkto>
// which is meant for one or more scalar columns.
// <p>
// When constructing a <src>ColumnsIndexArray</src> object, one has to define
// which column forms the key for this index on the given
// <src>table</src> object. 
// Not every data type is supported; only uChar, Short, Int, uInt, and
// String array columns are supported.
// The column can contain arrays of any shape and it can also contain
// empty cells. The class will probably mostly be used for vectors, as
// they seem to be the most logical way to hold multiple keys.
// <br>The data in the given column will be read, sorted,
// and stored in memory. When looking up a key or key range, the class
// will use a fast binary search on the data held in memory.
// <p>
// The <src>ColumnsIndexArray</src> object contains a
// <linkto class=Record>Record</linkto> object which can be used
// to define the key to be looked up. The record contains a field for
// the column in the index (with the same name and data type).
// The fastest way to fill the key is by creating a
// <linkto class=RecordFieldPtr>RecordFieldPtr</linkto> object for
// the field in the record (see the example) and fill it as needed.
// However, one can also use the <src>Record::define</src> function,
// but that is slower.
// <br>
// A second record is available to define the upper key
// in case a key range has to be looked up. The keys can be accessed
// using the various <src>accessKey</src> functions.
// <p>
// When a key is defined, the <src>getRowNumbers</src> function can be
// used to find the table rows containing the given key (range).
// Function <src>getRowNumber</src> can be used to lookup a single key
// if all keys in the index are unique (which can be tested with the
// <src>isUnique</src> function).
// <p>
// Instead of using the internal records holding the keys, one can also
// pass its own Record object to <src>getRowNumbers</src>.
// However, it will be slower.
// <p>
// After an index is created, it is possible to change the data
// in the underlying columns. However, the <src>ColumnsIndexArray</src> can
// not detect if the column data have changed. It can only detect if
// the number of rows has changed. If the column data have changed,
// the user has to use the <src>setChanged</src> function to indicate
// that the column has changed.
// <br>If data have changed, the entire index will be recreated by
// rereading and resorting the data. This will be deferred
// until the next key lookup.
// </synopsis>

// <example>
// Suppose one has table with a column NAME containing vectors.
// <srcblock>
// // Open the table and make an index for the column.
// Table tab("my.tab")
// ColumnsIndexArray colInx(tab, "NAME");
// // Make a RecordFieldPtr for the NAME field in the index key record.
// // Its data type has to match the data type of the column.
// RecordFieldPtr<String> nameFld(colInx.accessKey(), "NAME");
// // Find the row for a given name.
// Bool found;
// // Fill the key field and get the row number.
// // NAME is a unique key, so only one row number matches.
// // Otherwise function getRowNumbers had to be used.
// *nameFld = "MYNAME";
// uInt rownr = colInx.getRowNumber (found);
// if (!found) {
//     cout << "Name MYNAME is unknown" << endl;
// }
// // Now get a range of names and return the row numbers in ascending order.
// // This uses the fact that the 'unique' argument also sorts the data.
// RecordFieldPtr<String> nameUpp(colInx.accessUpperKey(), "NAME");
// *nameFld = "LOWER";
// *nameUpp = "UPPER";
// Vector<uInt> rownrs = colInx.getRowNumbers (True, True, True);
// </srcblock>

// <motivation>
// Bob Garwood needed such a class.
// </motivation>


class ColumnsIndexArray
{
public:
  // Create an index on the given table for the given column.
  // The column can be a scalar or an array column.
  // If <src>noSort==True</src>, the table is already in order of that
  // column and the sort step will not be done.
  // It only supports String and integer columns.
  ColumnsIndexArray (const Table&, const String& columnName);

  // Copy constructor (copy semantics).
  ColumnsIndexArray (const ColumnsIndexArray& that);

  ~ColumnsIndexArray();

  // Assignment (copy semantics).
  ColumnsIndexArray& operator= (const ColumnsIndexArray& that);

  // Are all keys in the index unique?
  Bool isUnique() const;

  // Return the names of the columns forming the index.
  const String& columnName() const;

  // Get the table for which this index is created.
  const Table& table() const;

  // Something has changed in the table, so the index has to be recreated.
  // The 2nd version indicates that a specific column has changed,
  // so only that column might need to be reread. If that column is not
  // part of the index, nothing will be done.
  // <br>Note that the class itself is keeping track if the number of
  // rows in the table changes.
  // <group>
  void setChanged();
  void setChanged (const String& columnName);
  // </group>

  // Access the key values.
  // These functions allow you to create RecordFieldPtr<T> objects
  // for each field in the key. In this way you can quickly fill in
  // the key.
  // <br>The records have a fixed type, so you cannot add or delete fields.
  // <br>Note that <src>accessKey</src> and <src>accessLowerKey</src>
  // are synonyms; they return the same underlying record.
  // <group>
  Record& accessKey();
  Record& accessLowerKey();
  Record& accessUpperKey();
  // </group>

  // Find the row number matching the key. All keys have to be unique,
  // otherwise an exception is thrown.
  // If no match is found, <src>found</src> is set to False.
  // The 2nd version makes it possible to pass in your own Record
  // instead of using the internal record via the <src>accessKey</src>
  // functions. Note that the given Record will be copied to the internal
  // record, thus overwrites it.
  // <group>
  uInt getRowNumber (Bool& found);
  uInt getRowNumber (Bool& found, const Record& key);
  // </group>

  // Find the row numbers matching the key. It should be used instead
  // of <src>getRowNumber</src> if the same key can exist multiple times.
  // The 2nd version makes it possible to pass in your own Record
  // instead of using the internal record via the <src>accessKey</src>
  // functions. Note that the given Record will be copied to the internal
  // record, thus overwrites it.
  // <br>A row can contain multiple equal values. In such a case the
  // same row number can occur multiple times in the output vector,
  // unless <src>unique</src> is set to True. Note that making the row
  // numbers unique implies a sort, so it can also be used to get the
  // row numbers in ascending order.
  // <group>
  Vector<uInt> getRowNumbers (Bool unique=False);
  Vector<uInt> getRowNumbers (const Record& key, Bool unique=False);
  // </group>

  // Find the row numbers matching the key range. The boolean arguments
  // tell if the lower and upper key are part of the range.
  // The 2nd version makes it possible to pass in your own Records
  // instead of using the internal records via the
  // <src>accessLower/UpperKey</src> functions.
  // Note that the given Records will be copied to the internal
  // records, thus overwrite them.
  // <br>A row can contain multiple matching values. In such a case the
  // same row number can occur multiple times in the output vector,
  // unless <src>unique</src> is set to True. Note that making the row
  // numbers unique implies a sort, so it can also be used to get the
  // row numbers in ascending order.
  // <group>
  Vector<uInt> getRowNumbers (Bool lowerInclusive, Bool upperInclusive,
			      Bool unique=False);
  Vector<uInt> getRowNumbers (const Record& lower, const Record& upper,
			      Bool lowerInclusive, Bool upperInclusive,
			      Bool unique=False);
  // </group>

protected:
  // Copy that object to this.
  void copy (const ColumnsIndexArray& that);

  // Delete all data in the object.
  void deleteObjects();

  // Add a column to the record description for the keys.
  // If the switch <src>arrayPossible</src> is True, the column can
  // be an array. Otherwise it has to be a scalar.
  void addColumnToDesc (RecordDesc& description,
			const TableColumn& column);

  // Make the various internal <src>RecordFieldPtr</src> objects.
  void makeObjects (const RecordDesc& description);

  // Read the data of the columns forming the index, sort them and
  // form the index.
  void readData();

  // Do a binary search on <src>itsUniqueIndexArray</src> for the key in
  // <src>fieldPtrs</src>.
  // If the key is found, <src>found</src> is set to True and the index
  // in <src>itsUniqueIndexArray</src> is returned.
  // If not found, <src>found</src> is set to False and the index
  // of the next higher key is returned.
  uInt bsearch (Bool& found, void* fieldPtr) const;

  // Compare the key in <src>fieldPtr</src> with the given index entry.
  // -1 is returned when less, 0 when equal, 1 when greater.
  static Int compare (void* fieldPtr,
		      void* dataPtr,
		      Int dataType,
		      Int index);

  // Fill the row numbers vector for the given start till end in the
  // <src>itsUniqueIndexArray</src> vector (end is not inclusive).
  // If <src>unique</src> is True, the row numbers will be made unique.
  void fillRowNumbers (Vector<uInt>& rows, uInt start, uInt end,
		       Bool unique) const;

  // Get the data if the column is an array.
  // <group>
  void getArray (Vector<uChar>& result, const String& name);
  void getArray (Vector<Short>& result, const String& name);
  void getArray (Vector<Int>& result, const String& name);
  void getArray (Vector<uInt>& result, const String& name);
  void getArray (Vector<String>& result, const String& name);
  // </group>

  // Fill the rownrs belonging to each array value.
  void fillRownrs (uInt npts, const Block<uInt>& nrel);

private:
  Table  itsTable;
  uInt   itsNrrow;
  Record* itsLowerKeyPtr;
  Record* itsUpperKeyPtr;
  Int     itsDataType;
  void*   itsDataVector;
  void*   itsData;              //# pointer to data in itsDataVector
  //# The following 2 blocks are actually blocks of RecordFieldPtr<T>*.
  //# They are used for fast access to the records.
  void*   itsLowerField;
  void*   itsUpperField;
  Bool         itsChanged;
  Vector<uInt> itsDataIndex;         //# Row numbers of all keys
  //# Indices in itsDataIndex for each unique key
  Vector<uInt> itsUniqueIndex;
  Block<uInt>  itsRownrs;            //# rownr for each value
  uInt*        itsDataInx;           //# pointer to data in itsDataIndex
  uInt*        itsUniqueInx;         //# pointer to data in itsUniqueIndex
};


inline Bool ColumnsIndexArray::isUnique() const
{
    return (itsDataIndex.nelements() == itsUniqueIndex.nelements());
}
inline const Table& ColumnsIndexArray::table() const
{
    return itsTable;
}
inline Record& ColumnsIndexArray::accessKey()
{
    return *itsLowerKeyPtr;
}
inline Record& ColumnsIndexArray::accessLowerKey()
{
    return *itsLowerKeyPtr;
}
inline Record& ColumnsIndexArray::accessUpperKey()
{
    return *itsUpperKeyPtr;
}



} //# NAMESPACE CASACORE - END

#endif
