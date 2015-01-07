//# ColumnsIndex.h: Index to one or more columns in a table
//# Copyright (C) 1998,1999,2001,2002
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

#ifndef TABLES_COLUMNSINDEX_H
#define TABLES_COLUMNSINDEX_H


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
template<typename T> class RecordFieldPtr;

// <summary>
// Index to one or more columns in a table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tColumnsIndex.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=Record>Record</linkto>
//   <li> <linkto class=RecordFieldPtr>RecordFieldPtr</linkto>
// </prerequisite>

// <synopsis>
// This class makes it possible to use transient indices on top
// of tables in order to speed up the process of finding rows
// based on a given key or key range.
// When constructing a <src>ColumnsIndex</src> object, one has to define
// which columns form the key for this index on the given
// <src>table</src> object.
// Only scalar columns are supported. The data in the given columns
// will be read, sorted (if needed), and stored in memory.
// When looking up a key or key range, the class will use a fast binary
// search on the data held in memory.
// <p>
// The <src>ColumnsIndex</src> object contains a
// <linkto class=Record>Record</linkto> object which can be used
// to define the key to be looked up. The record contains a field for
// each column in the index (with the same name and data type).
// The fastest way to fill the key is by creating a
// <linkto class=RecordFieldPtr>RecordFieldPtr</linkto> object for
// each field in the record (see the example) and fill it as needed.
// However, one can also use the <src>Record::define</src> function,
// but that is slower.
// <br>
// A second record is available to define the upper key
// when a key range has to be looked up. The keys can be accessed
// using the various <src>accessKey</src> functions.
// <p>
// When a key is defined, the <src>getRowNumbers</src> function can be
// used to find the table rows containing the given key (range).
// Function <src>getRowNumber</src> can be used if all keys in the index
// are unique (which can be tested with the <src>isUnique</src> function).
// <p>
// Instead of using the internal records holding the keys, one can also
// pass its own Record object to <src>getRowNumbers</src>.
// However, it will be slower.
// <p>
// When constructing the object, the user can supply his own compare
// function. The default compare function compares each field of the
// key in the normal way. A user's compare function makes it possible
// to compare in a special way. E.g. one could use near instead of ==
// on floating point fields. Another example (which is shown in one
// of the examples below) makes it possible to find a key in an
// index consisting of a time and width.
// <p>
// After an index is created, it is possible to change the data
// in the underlying columns. However, the <src>ColumnsIndex</src> can
// not detect if the column data have changed. It can only detect if
// the number of rows has changed. If the column data have changed,
// the user has to use the <src>setChanged</src> function to indicate
// that all columns or a particular column has changed.
// <br>If data have changed, the entire index will be recreated by
// rereading and optionally resorting the data. This will be deferred
// until the next key lookup.
// </synopsis>

// <example>
// Suppose one has an antenna table with key ANTENNA.
// <srcblock>
// // Open the table and make an index for column ANTENNA.
// Table tab("antenna.tab")
// ColumnsIndex colInx(tab, "ANTENNA");
// // Make a RecordFieldPtr for the ANTENNA field in the index key record.
// // Its data type has to match the data type of the column.
// RecordFieldPtr<Int> antFld(colInx.accessKey(), "ANTENNA");
// // Now loop in some way and find the row for the antenna
// // involved in that loop.
// Bool found;
// while (...) {
//     // Fill the key field and get the row number.
//     // ANTENNA is a unique key, so only one row number matches.
//     // Otherwise function getRowNumbers had to be used.
//     *antFld = antenna;
//     uInt antRownr = colInx.getRowNumber (found);
//     if (!found) {
//         cout << "Antenna " << antenna << " is unknown" << endl;
//     } else {
//         // antRownr can now be used to get data from that row in
//         // the antenna table.
//     }
// }
// </srcblock>
//
// The following example shows how multiple keys can be used and how
// a search on a range can be done.
// <srcblock>
// Table tab("sometable")
// // Note that TIME is the main key.
// // Also note that stringToVector (in ArrayUtil.h) is a handy
// // way to convert a String to a Vector<String>.
// ColumnsIndex colInx(tab, stringToVector("TIME,ANTENNA"));
// // Make a RecordFieldPtr for the fields in lower and upper key records.
// RecordFieldPtr<Double> timeLow(colInx.accessLowerKey(), "TIME");
// RecordFieldPtr<Int> antLow(colInx.accessLowerKey(), "ANTENNA");
// RecordFieldPtr<Double> timeUpp(colInx.accessUpperKey(), "TIME");
// RecordFieldPtr<Int> antUpp(colInx.accessUpperKey(), "ANTENNA");
// while (...) {
//     // Fill the key fields.
//     *timeLow = ...;
//     *antLow = ...;
//     *timeUpp = ...;
//     *antUpp = ...;
//     // Find the row numbers for keys between low and upp (inclusive).
//     Vector<uInt> rows = colInx.getRowNumbers (True, True);
// }
// </srcblock>
//
// The following example shows how a specific compare function
// could look like. A function like this will actually be used in the
// calibration software.
// <br>
// The table for which the index is built, has rows with the TIME as its key.
// However, each row is valid for a given interval, where TIME gives
// the middle of the interval and WIDTH the length of the interval.
// This means that the compare function has to test whether the key
// is part of the interval.
// <srcblock>
// Int myCompare (const Block<void*>& fieldPtrs,
//                const Block<void*>& dataPtrs,
//                const Block<Int>& dataTypes,
//                Int index)
// {
//   // Assert (for performance only in debug mode) that the correct
//   // fields are used.
//   DebugAssert (dataTypes.nelements() == 2, AipsError);
//   DebugAssert (dataTypes[0] == TpDouble  &&  dataTypes[1] == TpDouble,
//                AipsError);
//   // Now get the key to be looked up
//   // (an awfully looking cast has to be used).
//   const Double key = *(*(const RecordFieldPtr<Double>*)(fieldPtrs[0]));
//   // Get the time and width of the entry to be compared.
//   const Double time = ((const Double*)(dataPtrs[0]))[index];
//   const Double width = ((const Double*)(dataPtrs[1]))[index];
//   const Double start = time - width/2;
//   const Double end = time + width/2;
//   // Test if the key is before, after, or in the interval
//   // (representing less, greater, equal).
//   if (key < start) {
//     return -1;
//   } else if (key > end) {
//     return 1;
//   }
//   return 0;
// }
//
// // Now use this compare function in an actual index.
// Table tab("sometable")
// ColumnsIndex colInx(tab, stringToVector("TIME,WIDTH"), myCompare);
// // Make a RecordFieldPtr for the TIME field in the key record.
// // Note that although the WIDTH is part of the index, it is
// // not an actual key. So it does not need to be filled in.
// RecordFieldPtr<Double> time(colInx.accessLowerKey(), "TIME");
// Bool found;
// while (...) {
//     // Fill the key field.
//     *time = ...;
//     // Find the row number for this time.
//     uInt rownr = colInx.getRowNumber (found);
// }
// </srcblock>
// </example>

// <motivation>
// The calibration software needs to lookup keys in calibration tables
// very frequently. This class makes that process much easier and faster.
// </motivation>


class ColumnsIndex
{
public:
    // Define the signature of a comparison function.
    // The first block contains pointers to <src>RecordFieldPtr<T></src>
    // objects holding the key to be looked up.
    // The second block contains pointers to the column data.
    // The <src>index</src> argument gives the index in the column data.
    // The third block contains data types of those blocks (TpBool, etc.).
    // The function should return -1 if key is less than data,
    // 0 if equal, 1 if greater.
    // <br>An example above shows how a compare function can be used.
    typedef Int Compare (const Block<void*>& fieldPtrs,
			 const Block<void*>& dataPtrs,
			 const Block<Int>& dataTypes,
			 Int index);

    // Create an index on the given table for the given column.
    // The column has to be a scalar column.
    // If <src>noSort==True</src>, the table is already in order of that
    // column and the sort step will not be done.
    // The default compare function is provided by this class. It simply
    // compares each field in the key.
    ColumnsIndex (const Table&, const String& columnName,
		  Compare* compareFunction = 0, Bool noSort = False);

    // Create an index on the given table for the given columns, thus
    // the key is formed by multiple columns.
    // The columns have to be scalar columns.
    // If <src>noSort==True</src>, the table is already in order of those
    // columns and the sort step will not be done.
    // The default compare function is provided by this class. It simply
    // compares each field in the key.
    ColumnsIndex (const Table&, const Vector<String>& columnNames,
		  Compare* compareFunction = 0, Bool noSort = False);

    // Copy constructor (copy semantics).
    ColumnsIndex (const ColumnsIndex& that);

    ~ColumnsIndex();

    // Assignment (copy semantics).
    ColumnsIndex& operator= (const ColumnsIndex& that);

    // Are all keys in the index unique?
    Bool isUnique() const;

    // Return the names of the columns forming the index.
    Vector<String> columnNames() const;

    // Get the table for which this index is created.
    const Table& table() const;

    // Something has changed in the table, so the index has to be recreated.
    // The 2nd version indicates that a specific column has changed,
    // so only that column is reread. If that column is not part of the
    // index, nothing will be done.
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
    // <group>
    Vector<uInt> getRowNumbers();
    Vector<uInt> getRowNumbers (const Record& key);
    // </group>

    // Find the row numbers matching the key range. The boolean arguments
    // tell if the lower and upper key are part of the range.
    // The 2nd version makes it possible to pass in your own Records
    // instead of using the internal records via the
    // <src>accessLower/UpperKey</src> functions.
    // Note that the given Records will be copied to the internal
    // records, thus overwrite them.
    // <group>
    Vector<uInt> getRowNumbers (Bool lowerInclusive, Bool upperInclusive);
    Vector<uInt> getRowNumbers (const Record& lower, const Record& upper,
				Bool lowerInclusive, Bool upperInclusive);
    // </group>

    // Fill the internal key field from the corresponding external key.
    // The data type may differ.
    static void copyKeyField (void* field, int dtype, const Record& key);

protected:
    // Copy that object to this.
    void copy (const ColumnsIndex& that);

    // Delete all data in the object.
    void deleteObjects();

    // Add a column to the record description for the keys.
    void addColumnToDesc (RecordDesc& description,
			  const TableColumn& column);

    // Create the various members in the object.
    void create (const Table& table, const Vector<String>& columnNames,
		 Compare* compareFunction, Bool noSort);

    // Make the various internal <src>RecordFieldPtr</src> objects.
    void makeObjects (const RecordDesc& description);

    // Read the data of the columns forming the index, sort them and
    // form the index.
    void readData();

    // Do a binary search on <src>itsUniqueIndex</src> for the key in
    // <src>fieldPtrs</src>.
    // If the key is found, <src>found</src> is set to True and the index
    // in <src>itsUniqueIndex</src> is returned.
    // If not found, <src>found</src> is set to False and the index
    // of the next higher key is returned.
    uInt bsearch (Bool& found, const Block<void*>& fieldPtrs) const;

    // Compare the key in <src>fieldPtrs</src> with the given index entry.
    // -1 is returned when less, 0 when equal, 1 when greater.
    static Int compare (const Block<void*>& fieldPtrs,
			const Block<void*>& dataPtrs,
			const Block<Int>& dataTypes,
			Int index);

    // Fill the row numbers vector for the given start till end in the
    // <src>itsUniqueIndex</src> vector (end is not inclusive).
    void fillRowNumbers (Vector<uInt>& rows, uInt start, uInt end) const;

private:
    // Fill the internal key fields from the corresponding external key.
    void copyKey (Block<void*> fields, const Record& key);

    // Fill the internal key field from the corresponding external key.
    // The data type may differ.
    template <typename T>
    static void copyKeyField (RecordFieldPtr<T>& field, const Record& key)
    {
      key.get (field.name(), *field);
    }

    Table  itsTable;
    uInt   itsNrrow;
    Record* itsLowerKeyPtr;
    Record* itsUpperKeyPtr;
    Block<Int>   itsDataTypes;
    Block<void*> itsDataVectors;
    Block<void*> itsData;              //# pointer to data in itsDataVectors
    //# The following 2 blocks are actually blocks of RecordFieldPtr<T>*.
    //# They are used for fast access to the records.
    Block<void*> itsLowerFields;
    Block<void*> itsUpperFields;
    Block<Bool>  itsColumnChanged;
    Bool         itsChanged;
    Bool         itsNoSort;            //# True = sort is not needed
    Compare*     itsCompare;           //# Compare function
    Vector<uInt> itsDataIndex;         //# Row numbers of all keys
    //# Indices in itsDataIndex for each unique key
    Vector<uInt> itsUniqueIndex;
    uInt*        itsDataInx;           //# pointer to data in itsDataIndex
    uInt*        itsUniqueInx;         //# pointer to data in itsUniqueIndex
};


inline Bool ColumnsIndex::isUnique() const
{
    return (itsDataIndex.nelements() == itsUniqueIndex.nelements());
}
inline const Table& ColumnsIndex::table() const
{
    return itsTable;
}
inline Record& ColumnsIndex::accessKey()
{
    return *itsLowerKeyPtr;
}
inline Record& ColumnsIndex::accessLowerKey()
{
    return *itsLowerKeyPtr;
}
inline Record& ColumnsIndex::accessUpperKey()
{
    return *itsUpperKeyPtr;
}


} //# NAMESPACE CASACORE - END

#endif
