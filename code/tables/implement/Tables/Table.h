//# Table.h: Main interface classes to tables
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

#if !defined(AIPS_TABLE_H)
#define AIPS_TABLE_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/BaseTable.h>
#include <aips/Tables/TableLock.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/Sort.h>

//# Forward Declarations
class SetupNewTable;
class TableDesc;
class ColumnDesc;
class TableRecord;
class TableExprNode;
class DataManager;
class IPosition;
template<class T> class Vector;
template<class T> class Block;
template<class T> class PtrBlock;


// <summary>
// Main interface class to a read/write table
// </summary>

// <use visibility=export>

// <reviewed reviewer="TPPR" date="08.11.94" tests="tTable.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=SetupNewTable>SetupNewTable</linkto>
//   <li> <linkto class=TableDesc>TableDesc</linkto>
//   <li> <linkto class=TableColumn>TableColumn</linkto>
//   <li> <linkto class=ScalarColumn>ScalarColumn</linkto>
//   <li> <linkto class=ArrayColumn>ArrayColum</linkto>
//   <li> <linkto class=TableLock>TableLock</linkto>
// </prerequisite>

// <synopsis>
// Class Table can be used to create a new table or to access an existing
// table in read/write or readonly mode.
//
// To access the data in a Table, objects have to be created
// to access the columns. These objects are TableColumn,
// ScalarColumn<T> and ArrayColumn<T>, which can be created
// via their constructors.
// Furthermore the Table has a TableRecord object for holding keywords
// which can be read or written using the appropriate functions.
//
// To open an existing table, a simple Table constructor can be used.
// The possible construct options are:
// <ul>
//   <li> Old            readonly table (default option)
//   <li> Update         update existing table
//   <li> Delete         delete table
// </ul>
// Creating a new table requires more work, because columns have
// to be bound to storage managers or virtual column engines.
// Class SetupNewTable is needed for this purpose. The Tables module
// documentation explains in more detail how to create a table.
//
// Other Table objects can be created from a Table using
// the select, project and sort functions. In that way a subset
// of the table can be created and it can be read/written in the same
// way as a normal Table. However, writing has the effect that the
// underlying table gets written.
// </synopsis>

// <example>
// <srcblock>
// // Open a table to be updated.
// Table myTable ("theTable", Table::Update);
// // Write the column containing the scalar RA.
// ScalarColumn<double> raColumn(myTable, "RA");
// uInt nrrow = myTable.nrow();
// for (uInt i=0; i<nrrow; i++) {
//    raColumn.put (i, i+10);    // Put value i+10 into row i
// }
// </srcblock>
// </example>

// <motivation>
// Table is the envelope for the underlying counted referenced
// classes derived from BaseTable. In this way no pointers have
// to be used to get polymorphism.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> add, remove, rename columns.
//   <li> virtual concatenation of tables (if still necessary).
//   <li> maybe an isAttached function.
// </todo>


class Table : public Cleanup
{
friend class ROTableColumn;
friend class BaseTable;
friend class PlainTable;
friend class RefTable;
friend class TableIterator;
friend class RODataManAccessor;
friend class TableExprNode;

public:
    enum TableOption {
	// existing table
	Old=1,
	// create table
	New,
	// create table (may not exist)
	NewNoReplace,
	// new table, which gets marked for delete
	Scratch,
	// update existing table
	Update,
	// delete table
	Delete
    };

    // Define the signature of the function being called when the state
    // of a scratch table changes (i.e. created, closed, renamed,
    // (un)markForDelete).
    // <br>- <src>isScratch=True</src> indicates that a scratch table
    // is created (<src>oldName</src> is empty) or renamed
    // (<src>oldName</src> is not empty).
    // <br>- <src>isScratch=False</src> indicates that a scratch table
    // with name <src>name</src> is not scratch anymore (because it is
    // closed or because its state is set to non-scratch).
    typedef void ScratchCallback (const String& name, Bool isScratch,
				  const String& oldName);

    // Set the pointer to the StateCallback function.
    // It returns the current value of the pointer.
    // This function is called when changing the state of a table
    // (i.e. create, close, rename, (un)markForDelete).
    static const ScratchCallback* setScratchCallback (const ScratchCallback*);


    // Create a null Table object (i.e. no table is attached yet).
    // The sole purpose of this constructor is to allow construction
    // of an array of Table objects.
    // The assignment operator can be used to make a null object
    // reference a column.
    // Note that sort functions, etc. will cause a segmentation fault
    // when operating on a null object. It was felt it was too expensive
    // to test on null over and over again. The user should use the isNull
    // or throwIfNull function in case of doubt.
    Table();

    // Create a table object for an existing writable table.
    // The only options allowed are Old, Update, and Delete.
    // When the name of a table description is given, it is checked
    // if the table has that description.
    // Locking options can be given (see class
    // <linkto class=TableLock>TableLock</linkto>.
    // When the table with this name was already opened in this process,
    // the existing and new locking options are merged using
    // <src>TableLock::merge</src>.
    // The default locking mechanism is AutoLocking with a default
    // inspection interval of 5 seconds.
    // <group>
    explicit Table (const String& tableName, TableOption = Table::Old);
    Table (const String& tableName, const TableLock& lockOptions,
	   TableOption = Table::Old);
    Table (const String& tableName, const String& tableDescName,
	   TableOption = Table::Old);
    Table (const String& tableName, const String& tableDescName,
	   const TableLock& lockOptions, TableOption = Table::Old);
    // </group>

    // Make a table object for a new table, which can thereafter be used
    // for reading and writing.
    // If there are unbound columns, default storage managers an/ord virtual
    // column engines will be created and bound to those columns.
    // Create the table with the given nr of rows. If a storage manager
    // is used which does not allow addition of rows, the number of rows
    // in the table must already be given here.
    // Optionally the rows can be initialized with the default
    // values as defined in the column descriptions.
    // Locking options can be given (see class
    // <linkto class=TableLock>TableLock</linkto>.
    // The default locking mechanism is AutoLocking with a default
    // inspection interval of 5 seconds.
    // <group>
    explicit Table (SetupNewTable&, uInt nrrow = 0, Bool initialize = False);
    Table (SetupNewTable&, TableLock::LockOption,
	   uInt nrrow = 0, Bool initialize = False);
    Table (SetupNewTable&, const TableLock& lockOptions,
	   uInt nrrow = 0, Bool initialize = False);
    // </group>

    //# Virtually concatenate some tables.
    //# All tables must have the same description.
//#//    Table (const Block<Table>&);

    // Copy constructor (reference semantics).
    Table (const Table&);

    // The destructor flushes (i.e. writes) the table if it is opened
    // for output and not marked for delete.
    // It will flush if the destructor is called due to an exception,
    // because the Table object may not be correct.
    // Of course, in that case the flush function could be called explicitly.
    ~Table();

    //*display 8
    // This function is used by the exception handling mechanism we have
    // defined. It merely calls the destructor without flushing the data.
    // When real exceptions are available it will be unnecessary.
    void cleanup();

    // Assignment (reference semantics).
    Table& operator= (const Table&);

    // Can the table be deleted?
    // If true, function deleteTable can safely be called.
    // If not, message contains the reason why (e.g. 'table is not writable').
    // <group>
    static Bool canDeleteTable (const String& tableName);
    static Bool canDeleteTable (String& message, const String& tableName);
    // </group>

    // Delete the table.
    // An exception is thrown if the table cannot be deleted because
    // its is not writable or because it is still open in this or
    // another process.
    static void deleteTable (const String& tableName);

    // Try to reopen the table for read/write access.
    // An exception is thrown if the table is not writable.
    // Nothing is done if the table is already open for read/write.
    void reopenRW();

    // Is the table used (i.e. open) in this process.
    static Bool isOpened (const String& tableName);

    // Is the table used (i.e. open) in another process.
    Bool isMultiUsed() const;

    // Get the locking options.
    const TableLock& lockOptions() const;

    // Has this process the read or write lock, thus can the table
    // be read or written safely?
    // <group>
    Bool hasLock (FileLocker::LockType = FileLocker::Write) const;
    Bool hasLock (Bool write) const;
    // </group>

    // Try to lock the table for read or write access (default is write).
    // The number of attempts (default = forever) can be specified when
    // acquiring the lock does not succeed immediately. When nattempts>1,
    // the system waits 1 second between each attempt, so nattempts
    // is more or less equal to a wait period in seconds.
    // The return value is false when acquiring the lock failed.
    // When <src>PermanentLocking</src> is in effect, a lock is already
    // present, so nothing will be done.
    // <group>
    Bool lock (FileLocker::LockType = FileLocker::Write, uInt nattempts = 0);
    Bool lock (Bool write, uInt nattempts = 0);
    // </group>

    // Unlock the table. This will also synchronize the table data,
    // thus force the data to be written to disk.
    // When <src>PermanentLocking</src> is in effect, nothing will be done.
    void unlock();

    // Determine the number of locked tables opened with the AutoLock option
    // (Locked table means locked for read and/or write).
    static uInt nAutoLocks();

    // Unlock locked tables opened with the AutoLock option.
    // If <src>all=True</src> all such tables will be unlocked.
    // If <src>all=False</src> only tables requested by another process
    // will be unlocked.
    static void relinquishAutoLocks (Bool all = False);

    // Determine if column or keyword table data have changed
    // (or is being changed) since the last time this function was called.
    Bool hasDataChanged();

    // Flush the table, i.e. write out the buffers. When <src>sync=True</src>,
    // it is ensured that all data are physically written to disk.
    // Nothing will be done if the table is not writable.
    // At any time a flush can be executed, even when the table is marked
    // for delete.
    // When the table is marked for delete, the destructor will remove
    // files written by intermediate flushes.
    // Note that if necessary the destructor will do an implicit flush,
    // unless it is executed due to an exception.
    void flush (Bool sync=False);

    // Test if the object is null, i.e. does not reference a table yet.
    // This is the case if the default constructor is used.
    Bool isNull() const
	{ return (baseTabPtr_p == 0  ?  True : False); }

    // Throw an exception if the object is null, i.e.
    // if function isNull() is True.
    void throwIfNull() const;

    // Test if the given data type is native to the table system.
    // If not, a virtual column engine is needed to store data with that type.
    // With the function DataType::whatType it can be used in a templated
    // function like:
    // <srcblock>
    //     if (Table::isNativeDataType (whatType((T*)0))) {
    // </srcblock>
    static Bool isNativeDataType (DataType dtype);

    // Make the table file name.
    static String fileName (const String& tableName);

    // Test if a table with the given name exists and is readable.
    static Bool isReadable (const String& tableName);

    // Return the layout of a table (i.e. description and #rows).
    // This function has the advantage that only the minimal amount of
    // information required is read from the table, thus it is much
    // faster than a normal table open.
    // <br> The number of rows is returned. The description of the table
    // is stored in desc (its contents will be overwritten).
    // <br> An exception is thrown if the table does not exist.
    static uInt getLayout (TableDesc& desc, const String& tableName);

    // Get the table info of the table with the given name.
    // An empty object is returned when the table is unknown.
    static TableInfo tableInfo (const String& tableName);

    // Test if a table with the given name exists and is writable.
    static Bool isWritable (const String& tableName);

    // Find the non-writable files in a table.
    static Vector<String> nonWritableFiles (const String& tableName);

    // Test if this table is opened as writable.
    Bool isWritable() const;

    // Test if the given column is writable.
    // <group>
    Bool isColumnWritable (const String& columnName) const;
    Bool isColumnWritable (uInt columnIndex) const;
    // </group>

    // Test if the given column is stored (otherwise it is virtual).
    // <group>
    Bool isColumnStored (const String& columnName) const;
    Bool isColumnStored (uInt columnIndex) const;
    // </group>

    // Get readonly access to the table keyword set.
    const TableRecord& keywordSet() const;

    // Get read/write access to the table keyword set.
    // This requires that the table is locked (or it gets locked
    // when using AutoLocking mode).
    TableRecord& rwKeywordSet();

    // Get access to the TableInfo object.
    // <group>
    const TableInfo& tableInfo() const;
    TableInfo& tableInfo();
    // </group>

    // Write the TableInfo object.
    // Usually this is not necessary, because it is done automatically
    // when the table gets written (by table destructor or flush function).
    // This function is only useful when the table info has to be written
    // before the table gets written (e.g. when another process reads
    // the table while it gets filled).
    void flushTableInfo() const;

    // Get access to the table description.
    // This can be used to get nr of columns, etc..
    // <note role=tip>
    // The keyword set in the table description is not the
    // same set as the keyword set in the table itself.
    // </note>
    const TableDesc& tableDesc() const;

    // Get the table name.
    const String& tableName() const;

    // Rename the table and all its subtables.
    // The following options can be given:
    // <dl>
    // <dt> Table::Update
    // <dd> A table with this name must already exists, which will be
    //      overwritten. When succesfully renamed, the table is unmarked
    //      for delete (if necessary).
    // <dt> Table::New
    // <dd> When a table with this name exists, it will be overwritten.
    //      When succesfully renamed, the table is unmarked
    //      for delete (if necessary).
    // <dt> Table::NewNoReplace
    // <dd> When a table with this name already exists, an exception
    //      is thrown. When succesfully renamed, the table
    //      is unmarked for delete (if necessary).
    // <dt> Table::Scratch
    // <dd> Same as Table::New, but followed by markForDelete().
    // </dl>
    // The scratchCallback function is called when needed.
    void rename (const String& newName, TableOption);

    // Copy the table and all its subtables.
    // The following options can be given:
    // <dl>
    // <dt> Table::New
    // <dd> When a table with this name exists, it will be overwritten.
    // <dt> Table::NewNoReplace
    // <dd> When a table with this name already exists, an exception
    //      is thrown.
    // <dt> Table::Scratch
    // <dd> Same as Table::New, but followed by markForDelete().
    // </dl>
    void copy (const String& newName, TableOption) const;

    // Get the table option.
    int tableOption() const;

    // Mark the table for delete.
    // This means that the underlying table gets deleted when it is
    // actually destructed.
    // The scratchCallback function is called when needed.
    void markForDelete();

    // Unmark the table for delete.
    // This means the underlying table does not get deleted when destructed.
    // The scratchCallback function is called when needed.
    void unmarkForDelete();

    // Test if the table is marked for delete.
    Bool isMarkedForDelete() const;
    
    // Get the number of rows. This is unsynchronized.
    uInt nrow() const;

    // Test if it is possible to add a row to this table.
    // It is possible if all storage managers used for the table
    // support it.
    Bool canAddRow() const;

    // Add one or more rows at the end of the table.
    // This will fail for tables not supporting addition of rows.
    // Optionally the rows can be initialized with the default
    // values as defined in the column descriptions.
    void addRow (uInt nrrow = 1, Bool initialize = False);

    // Test if it is possible to remove a row from this table.
    // It is possible if all storage managers used for the table
    // support it.
    Bool canRemoveRow() const;

    // Remove the given row(s).
    // The latter form can be useful with the select and rowNumbers functions
    // to remove some selected rows from the table.
    // <br>It will fail for tables not supporting removal of rows.
    // <note role=warning>
    // The following code fragments do NOT have the same result:
    // <srcblock>
    //    tab.removeRow (10);      // remove row 10
    //    tab.removeRow (20);      // remove row 20, which was 21
    //    Vector<uInt> vec(2);
    //    vec(0) = 10;
    //    vec(1) = 20;
    //    tab.removeRow (vec);     // remove row 10 and 20
    // </srcblock>
    // because in the first fragment removing row 10 turns the former
    // row 21 into row 20.
    // </note>
    // <group>
    void removeRow (uInt rownr);
    void removeRow (const Vector<uInt>& rownrs);
    // </group>

    // Create a TableExprNode object for a column or for a keyword
    // in the table keyword set.
    // This can be used in selecting rows from a table using
    // <src>operator()</src> described below.
    // <br>The functions taking the fieldNames vector are meant for
    // the cases where the keyword or column contains records.
    // The fieldNames indicate which field to take from that record
    // (which can be a record again, etc.).
    // <group name=keycol>
    TableExprNode key (const String& keywordName) const;
    TableExprNode key (const Vector<String>& fieldNames) const;
    TableExprNode col (const String& columnName) const;
    TableExprNode col (const String& columnName,
		       const Vector<String>& fieldNames) const;
    TableExprNode keyCol (const String& name,
			  const Vector<String>& fieldNames) const;
    // </group>

    // Create a TableExprNode object for the rownumber function.
    // 'origin' Indicates which rownumber is the first.
    // C++ uses origin = 0 (default)
    // Glish and TaQL both use origin = 1
    TableExprNode nodeRownr (uInt origin=0) const;

    // Create a TableExprNode object for the rand function.
    TableExprNode nodeRandom () const;

    // Select rows from a table using an select expression consisting
    // of TableExprNode objects.
    // Basic TableExprNode objects can be created with the functions
    // <linkto file="Table.h#keycol">key</linkto> and especially
    // <linkto file="Table.h#keycol">col</linkto>.
    // Composite TableExprNode objects, representing an expression,
    // can be created by applying operations (like == and +)
    // to the basic ones. This is described in class
    // <linkto class="TableExprNode:description">TableExprNode</linkto>.
    // For example:
    // <srcblock>
    //    Table result = tab(tab.col("columnName") > 10);
    // </srcblock>
    // All rows for which the expression is true, will be selected and
    // "stored" in the result.
    // You need to include ExprNode.h for this purpose.
    Table operator() (const TableExprNode&) const;

    // Select rows using a vector of row numbers.
    // This can, for instance, be used to select the same rows as
    // were selected in another table (using the rowNumbers function).
    // <srcblock>
    //     Table result = thisTable (otherTable.rowNumbers());
    // </srcblock>
    Table operator() (const Vector<uInt>& rownrs) const;

    // Select rows using a mask block.
    // The length of the block must match the number of rows in the table.
    // If an element in the mask is True, the corresponding row will be
    // selected.
    Table operator() (const Block<Bool>& mask) const;

    // Project the given columns (i.e. select the columns).
    Table project (const Block<String>& columnNames) const;

    //# Virtually concatenate all tables in this column.
    //# The column cells must contain tables with the same description.
//#//    Table concatenate (const String& columnName) const;

    // Do logical operations on a table.
    // It can be used for row-selected or projected (i.e. column-selected)
    // tables. The tables involved must come from the same root table or
    // be the root table themselves.
    // <group>
    // Intersection with another table.
    Table operator& (const Table&) const;
    // Union with another table.
    Table operator| (const Table&) const;
    // Subtract another table.
    Table operator- (const Table&) const;
    // Xor with another table.
    Table operator^ (const Table&) const;
    // Take complement.
    Table operator! () const;
    // </group>

    // Sort a table on one or more columns of scalars.
    // Per column a compare function can be provided. By default
    // the standard compare function defined in Compare.h will be used.
    // Default sort order is ascending.
    // Default sorting algorithm is the heapsort.
    // <group>
    // Sort on one column.
    Table sort (const String& columnName,
		int = Sort::Ascending,
		int = Sort::HeapSort) const;
    // Sort on multiple columns. The principal column has to be the
    // first element in the Block of column names.
    Table sort (const Block<String>& columnNames,
		int = Sort::Ascending,
		int = Sort::HeapSort) const;
    // Sort on multiple columns. The principal column has to be the
    // first element in the Block of column names.
    // The order can be given per column.
    Table sort (const Block<String>& columnNames,
		const Block<Int>& sortOrders,
		int = Sort::HeapSort) const;
    // Sort on multiple columns. The principal column has to be the
    // first element in the Block of column names.
    // The order can be given per column.
    // Provide some special compare functions via a function pointer.
    // A zero function pointer means using the standard compare function
    // from class <linkto class="ObjCompare:description">ObjCompare</linkto>.
    Table sort (const Block<String>& columnNames,
		const PtrBlock<ObjCompareFunc*>& compareFunctionPointers,
		const Block<Int>& sortOrders,
		int = Sort::HeapSort) const;
    // </group>

    // Get a vector of row numbers.
    // In case the table is a subset of the root table, this tells which
    // rows of the root table are part of the subset.
    // In case the table is the root table itself, the result is a vector
    // containing the row numbers 0 .. #rows-1.
    Vector<uInt> rowNumbers() const;

    // Add a column to the table.
    // The data manager used for the column depend on the function used.
    // Exceptions are thrown when the column already exist or when the
    // table is not writable.
    // <group>
    // Use the first appropriate existing storage manager.
    // When there is none, a data manager is created using the default
    // data manager in the column description.
    void addColumn (const ColumnDesc& columnDesc);
    // Use an existing data manager with the given name or type.
    // When the flag byName is True, a name is given, otherwise a type.
    // When a name is given, an exception is thrown if the data manager is
    // unknown or does not allow addition of columns.
    // When a type is given, a storage manager of the given type will be
    // created when there is no such data manager allowing addition of rows.
    void addColumn (const ColumnDesc& columnDesc,
		    const String& dataManager, Bool byName);
    // Use the given data manager (which is a new one).
    void addColumn (const ColumnDesc& columnDesc,
		    const DataManager& dataManager);
    // </group>

    // Add a bunch of columns using the given new data manager.
    // All columns and possible hypercolumn definitions in the given table
    // description will be copied and added to the table.
    // This can be used in case of specific data managers which need to
    // be created with more than one column (e.g. the tiled hypercube
    // storage managers).
    void addColumn (const TableDesc& tableDesc,
		    const DataManager& dataManager);

    // Test if a column can be removed.
    // It can if the column exists and if the data manager it is using
    // supports removal of columns.
    Bool canRemoveColumn (const String& columnName) const;

    // Remove a column.
    void removeColumn (const String& columnName);

    // Test if a column can be renamed.
    Bool canRenameColumn() const;

    // Rename a column.
    void renameColumn (const String& newName, const String& oldName);

    //*display 4
    // Write a table to AipsIO (for TypedKeywords<Table>).
    // This will only write the table name.
    friend AipsIO& operator<< (AipsIO&, const Table&);

    //*display 4
    // Read a table from AipsIO (for TypedKeywords<Table>).
    // This will read the table name and open the table as writable
    // if the table file is writable, otherwise as readonly.
    friend AipsIO& operator>> (AipsIO&, Table&);

    //*display 4
    // Read a table from AipsIO (for TableKeywords).
    // This will read the table name and open the table as writable
    // if the switch is set and if the table file is writable.
    // otherwise it is opened as readonly.
    void getTableKeyword (AipsIO&, Bool openWritable);

    // Write a table to ostream (for TypedKeywords<Table>).
    // This only shows its name and number of columns and rows.
    friend ostream& operator<< (ostream&, const Table&);

protected:
    BaseTable*  baseTabPtr_p;                 //# ptr to table representation
    //# The isCounted_p flag is normally true.
    //# Only for internally used Table objects (i.e. in the DataManager)
    //# this flag is False, otherwise a mutual dependency would exist.
    //# The DataManager has a Table object, which gets deleted by the
    //# DataManager destructor. The DataManager gets deleted by the
    //# PlainTable destructor, which gets called when the last Table
    //# object gets destructed. That would never be the case if this
    //# internally used Table object was counted.
    Bool        isCounted_p;
    //# Counter of last call to hasDataChanged.
    uInt        lastModCounter_p;
    //# Pointer to the ScratchCallback function.
    static const ScratchCallback* scratchCallback_p;


    // Construct a Table object from a BaseTable*.
    // By default the object gets counted.
    Table (BaseTable*, Bool countIt = True);

    // Open an existing table.
    void open (const String& name, const String& type, int tableOption,
	       const TableLock& lockOptions);


private:
    // Get the pointer to the underlying BaseTable.
    // This is needed for some friend classes.
    BaseTable* baseTablePtr() const;

    // Look in the cache if the table is already open.
    // If so, check if table option matches.
    // If needed reopen the table for read/write and merge the lock options.
    BaseTable* lookCache (const String& name, int tableOption,
			  const TableLock& tableInfo);

    // Find the data manager with the given name.
    DataManager* findDataManager (const String& datamanagerName) const;
};



inline void Table::reopenRW()
    { baseTabPtr_p->reopenRW(); }
inline void Table::flush (Bool sync)
    { baseTabPtr_p->flush (sync); }

inline Bool Table::isMultiUsed() const
    { return baseTabPtr_p->isMultiUsed(); }
inline const TableLock& Table::lockOptions() const
    { return baseTabPtr_p->lockOptions(); }
inline Bool Table::lock (FileLocker::LockType type, uInt nattempts)
    { return baseTabPtr_p->lock (type, nattempts); }
inline Bool Table::lock (Bool write, uInt nattempts)
{
    return baseTabPtr_p->lock (write ? FileLocker::Write : FileLocker::Read,
			       nattempts);
}
inline void Table::unlock()
    { baseTabPtr_p->unlock(); }
inline Bool Table::hasLock (FileLocker::LockType type) const
    { return baseTabPtr_p->hasLock (type); }
inline Bool Table::hasLock (Bool write) const
{
    return baseTabPtr_p->hasLock (write ? FileLocker::Write : FileLocker::Read);
}

inline Bool Table::isWritable() const
    { return baseTabPtr_p->isWritable(); }
inline Bool Table::isColumnWritable (const String& columnName) const
    { return baseTabPtr_p->isColumnWritable (columnName); }
inline Bool Table::isColumnWritable (uInt columnIndex) const
    { return baseTabPtr_p->isColumnWritable (columnIndex); }

inline Bool Table::isColumnStored (const String& columnName) const
    { return baseTabPtr_p->isColumnStored (columnName); }
inline Bool Table::isColumnStored (uInt columnIndex) const
    { return baseTabPtr_p->isColumnStored (columnIndex); }

inline void Table::rename (const String& newName, TableOption option)
    { baseTabPtr_p->rename (newName, option); }
inline void Table::copy (const String& newName, TableOption option) const
    { baseTabPtr_p->copy (newName, option); }
inline void Table::markForDelete()
    { baseTabPtr_p->markForDelete (True, ""); }
inline void Table::unmarkForDelete()
    { baseTabPtr_p->unmarkForDelete(True, ""); }
inline Bool Table::isMarkedForDelete() const
    { return baseTabPtr_p->isMarkedForDelete(); }

inline uInt Table::nrow() const
    { return baseTabPtr_p->nrow(); }
inline BaseTable* Table::baseTablePtr() const
    { return baseTabPtr_p; }
inline const TableDesc& Table::tableDesc() const
    { return baseTabPtr_p->tableDesc(); }
inline const TableRecord& Table::keywordSet() const
    { return baseTabPtr_p->keywordSet(); }
inline TableRecord& Table::rwKeywordSet()
    { return baseTabPtr_p->rwKeywordSet(); }

inline TableInfo Table::tableInfo (const String& tableName)
    { return BaseTable::tableInfo (tableName); }
inline const TableInfo& Table::tableInfo() const
    { return baseTabPtr_p->tableInfo(); }
inline TableInfo& Table::tableInfo()
    { return baseTabPtr_p->tableInfo(); }
inline void Table::flushTableInfo() const
    { baseTabPtr_p->flushTableInfo(); }

inline const String& Table::tableName() const
    { return baseTabPtr_p->tableName(); }
inline int Table::tableOption() const
    { return baseTabPtr_p->tableOption(); }

inline Bool Table::canAddRow() const
    { return baseTabPtr_p->canAddRow(); }
inline Bool Table::canRemoveRow() const
    { return baseTabPtr_p->canRemoveRow(); }
inline Bool Table::canRemoveColumn (const String& columnName) const
    { return baseTabPtr_p->canRemoveColumn (columnName); }
inline Bool Table::canRenameColumn() const
    { return baseTabPtr_p->canRenameColumn(); }

inline void Table::addRow (uInt nrrow, Bool initialize)
    { baseTabPtr_p->addRow (nrrow, initialize); }
inline void Table::removeRow (uInt rownr)
    { baseTabPtr_p->removeRow (rownr); }
inline void Table::removeRow (const Vector<uInt>& rownrs)
    { baseTabPtr_p->removeRow (rownrs); }
inline void Table::addColumn (const ColumnDesc& columnDesc)
    { baseTabPtr_p->addColumn (columnDesc); }
inline void Table::addColumn (const ColumnDesc& columnDesc,
			      const String& dataManager, Bool byName)
    { baseTabPtr_p->addColumn (columnDesc, dataManager, byName); }
inline void Table::addColumn (const ColumnDesc& columnDesc,
			      const DataManager& dataManager)
    { baseTabPtr_p->addColumn (columnDesc, dataManager); }
inline void Table::addColumn (const TableDesc& tableDesc,
			      const DataManager& dataManager)
    { baseTabPtr_p->addColumn (tableDesc, dataManager); }
inline void Table::removeColumn (const String& columnName)
    { baseTabPtr_p->removeColumn (columnName); }
inline void Table::renameColumn (const String& newName, const String& oldName)
    { baseTabPtr_p->renameColumn (newName, oldName); }

inline DataManager* Table::findDataManager (const String& name) const
{
    return baseTabPtr_p->findDataManager (name);
}


#endif
