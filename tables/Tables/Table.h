//# Table.h: Main interface classes to tables
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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
//# You should have receied a copy of the GNU Library General Public License
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

#ifndef TABLES_TABLE_H
#define TABLES_TABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/DataMan/TSMOption.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/Sort.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class SetupNewTable;
class TableDesc;
class ColumnDesc;
class TableRecord;
class Record;
class TableExprNode;
class DataManager;
class IPosition;
template<class T> class Vector;
template<class T> class Block;
template<class T> class CountedPtr;


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
// The function <src>openTable</src> makes it possible to open a subtable
// of a table in a convenient way, even if the table is only a reference
// to another table (e.g., a selection).
//
// Creating a new table requires more work, because columns have
// to be bound to storage managers or virtual column engines.
// Class SetupNewTable is needed for this purpose. The Tables module
// documentation explains in more detail how to create a table.
// When creating a table, it can be specified which endian format to use.
// By default it uses the format specified in the aipsrc variable
// <code>table.endianformat</code> which defaults to
// <code>Table::LocalEndian</code> (thus the endian format of the
// machine being used).
//
// It is possible to create a Table object as the virtual concatenation of
// Tables having identical table descriptions. Subtables of those tables
// can optionally be concatenated as well.
// E.g. if a MeasurementSet is partioned in time, this mechanism makes it
// possible to view it as a single table. Furthermore, a subtable like
// SYSCAL can be concatenated as well, while the other subtables are identical
// in all partitions and are taken from the first table only.
//
// Other Table objects can be created from a Table using
// the select, project and sort functions. The result in so-called
// reference tables. In this way a subset of a table can be created and
// can be read/written in the same way as a normal Table. Writing has the
// effect that the underlying table gets written.
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


class Table
{
friend class TableColumn;
friend class BaseTable;
friend class PlainTable;
friend class MemoryTable;
friend class RefTable;
friend class ConcatTable;
friend class TableIterator;
friend class RODataManAccessor;
friend class TableExprNode;
friend class TableExprNodeRep;

public:
    // Define the possible options how a table can be opened.
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

    // Define the possible table types.
    enum TableType {
	// plain table (stored on disk)
        Plain,
	// table held in memory
	Memory
    };

    // Define the possible endian formats in which table data can be stored.
    enum EndianFormat {
	// store table data in big endian (e.g. SUN) format
	BigEndian=1,
	// store table data in little endian (e.g. Intel) format
	LittleEndian,
	// store data in the endian format of the machine used
	LocalEndian,
	// use endian format defined in the aipsrc variable table.endianformat
        // If undefined, it defaults to LocalEndian.
	AipsrcEndian
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

    // Set the pointer to the ScratchCallback function.
    // It returns the current value of the pointer.
    // This function is called when changing the state of a table
    // (i.e. create, close, rename, (un)markForDelete).
    static ScratchCallback* setScratchCallback (ScratchCallback*);


    // Create a null Table object (i.e. a NullTable is attached).
    // The sole purpose of this constructor is to allow construction
    // of an array of Table objects.
    // The assignment operator can be used to make a null object
    // reference a proper table.
    Table();

    // Create a table object for an existing table.
    // The only options allowed are Old, Update, and Delete.
    // If the name of a table description is given, it is checked
    // if the table has that description.
    // Locking options can be given (see class
    // <linkto class=TableLock>TableLock</linkto>.
    // If the table with this name was already opened in this process,
    // the existing and new locking options are merged using
    // <src>TableLock::merge</src>.
    // The default locking mechanism is DefaultLocking. If the table
    // is not open yet, it comes to AutoLocking with an inspection interval
    // of 5 seconds. Otherwise DefaultLocking keeps the locking options
    // of the already open table.
    // <group>
    explicit Table (const String& tableName, TableOption = Table::Old,
                    const TSMOption& = TSMOption());
    Table (const String& tableName, const TableLock& lockOptions,
	   TableOption = Table::Old, const TSMOption& = TSMOption());
    Table (const String& tableName, const String& tableDescName,
	   TableOption = Table::Old, const TSMOption& = TSMOption());
    Table (const String& tableName, const String& tableDescName,
	   const TableLock& lockOptions, TableOption = Table::Old,
           const TSMOption& = TSMOption());
    // </group>

    // Make a new empty table (plain (scratch) or memory type).
    // Columns should be added to make it a real one.
    // Note that the endian format is only relevant for plain tables.
    explicit Table (TableType, EndianFormat = Table::AipsrcEndian,
                    const TSMOption& = TSMOption());

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
    // <br>The data will be stored in the given endian format.
    // <group>
    explicit Table (SetupNewTable&, uInt nrrow = 0, Bool initialize = False,
		    EndianFormat = Table::AipsrcEndian,
                    const TSMOption& = TSMOption());
    Table (SetupNewTable&, TableType,
	   uInt nrrow = 0, Bool initialize = False,
	   EndianFormat = Table::AipsrcEndian, const TSMOption& = TSMOption());
    Table (SetupNewTable&, TableType, const TableLock& lockOptions,
	   uInt nrrow = 0, Bool initialize = False,
	   EndianFormat = Table::AipsrcEndian, const TSMOption& = TSMOption());
    Table (SetupNewTable&, TableLock::LockOption,
	   uInt nrrow = 0, Bool initialize = False,
	   EndianFormat = Table::AipsrcEndian, const TSMOption& = TSMOption());
    Table (SetupNewTable&, const TableLock& lockOptions,
	   uInt nrrow = 0, Bool initialize = False,
	   EndianFormat = Table::AipsrcEndian, const TSMOption& = TSMOption());
    // </group>

    // Create a table object as the virtual concatenation of
    // one or more of existing tables. The descriptions of all those tables
    // must be exactly the same.
    // <br>The keywordset of the virtual table is the set of the first table
    // including its subtables. However, it is possible to specify the names
    // of the subtables that have to be concantenated as well.
    // <br>In this way a concatenation of multiple MS-s can be made, where it
    // can be specified that, say, the SYSCAL table has to be concatenated too.
    // <br> When a concatenated table is written and if a non-empty
    // <src>subDirName</src> is given, the tables to be concatenated will be
    // moved to that subdirectory in the directory of the concatenated table.
    // This option is mainly used by the MSS structure used in CASA.
    // <br>
    // The only open options allowed are Old and Update.
    // Locking options can be given (see class
    // <linkto class=TableLock>TableLock</linkto>.
    // They apply to all underlying tables.
    // If a table was already opened in this process,
    // the existing and new locking options are merged using
    // <src>TableLock::merge</src>.
    // The default locking mechanism is DefaultLocking. If the table
    // is not open yet, it comes to AutoLocking with an inspection interval
    // of 5 seconds. Otherwise DefaultLocking keeps the locking options
    // of the already open table.
    // <group>
    explicit Table (const Block<Table>& tables,
		    const Block<String>& subTables = Block<String>(),
                    const String& subDirName = String());
    explicit Table (const Block<String>& tableNames,
		    const Block<String>& subTables = Block<String>(),
		    TableOption = Table::Old, const TSMOption& = TSMOption(),
                    const String& subDirName = String());
    Table (const Block<String>& tableNames,
	   const Block<String>& subTables,
	   const TableLock& lockOptions,
	   TableOption = Table::Old, const TSMOption& = TSMOption());
    // </group>

    // Copy constructor (reference semantics).
    Table (const Table&);

    // The destructor flushes (i.e. writes) the table if it is opened
    // for output and not marked for delete.
    // It will flush if the destructor is called due to an exception,
    // because the Table object may not be correct.
    // Of course, in that case the flush function could be called explicitly.
    // <br>It is virtual, so an object of a derived class like MeasurementSet
    // is destructed correctly through a Table pointer.
    virtual ~Table();

    // Assignment (reference semantics).
    Table& operator= (const Table&);

    // Try to open a table. The name of the table can contain subtable names
    // using :: as separator. In this way it is possible to directly open a
    // subtable of a RefTable or ConcatTable, which is not possible if the
    // table name is specified with slashes.
    // <br>The open process is as follows:
    // <ul>
    //  <li> It is tried to open the table with the given name.
    //  <li> If unsuccessful, the name is split into its parts using ::
    //       The first part is the main table which will be opened temporarily.
    //       The other parts are the successive subtable names (usually one).
    //       Each subtable is opened by looking it up in the keywords of the
    //       table above. The final subtable is returned.
    // </ul>
    // <br>An exception is thrown if the table cannot be opened.
    // <example>
    // Open the ANTENNA subtable of an MS which might be a selection of
    // a real MS.
    // <srcblock>
    // Table tab(Table::openTable ("sel.ms::ANTENNA");
    // </srcblock>
    // </example>
    // <group>
    static Table openTable (const String& tableName,
                            TableOption = Table::Old,
                            const TSMOption& = TSMOption());
    static Table openTable (const String& tableName,
                            const TableLock& lockOptions,
                            TableOption = Table::Old,
                            const TSMOption& = TSMOption());
    // </group>

    // Get the names of the tables this table consists of.
    // For a plain table it returns its name,
    // for a RefTable the name of the parent, and
    // for a ConcatTable the names of all its parts.
    // <br>Note that a part can be any type of table (e.g. a ConcatTable).
    // The recursive switch tells how to deal with that.
    Block<String> getPartNames (Bool recursive=False) const;

    // Is the root table of this table the same as that of the other one?
    Bool isSameRoot (const Table& other) const;

    // Can the table be deleted?
    // If true, function deleteTable can safely be called.
    // If not, message contains the reason why (e.g. 'table is not writable').
    // It checks if the table is writable, is not open in this process
    // and is not open in another process.
    // <br>If <src>checkSubTables</src> is set, it also checks if
    // a subtable is not open in another process.
    // <group>
    static Bool canDeleteTable (const String& tableName,
				Bool checkSubTables=False);
    static Bool canDeleteTable (String& message, const String& tableName,
				Bool checkSubTables=False);
    // </group>

    // Delete the table.
    // An exception is thrown if the table cannot be deleted because
    // its is not writable or because it is still open in this or
    // another process.
    // <br>If <src>checkSubTables</src> is set, it is also checked if
    // a subtable is used in another process.
    static void deleteTable (const String& tableName,
			     Bool checkSubTables=False);

    // Close all open subtables.
    void closeSubTables() const;

    // Try to reopen the table for read/write access.
    // An exception is thrown if the table is not writable.
    // Nothing is done if the table is already open for read/write.
    void reopenRW();

    // Get the endian format in which the table is stored.
    Table::EndianFormat endianFormat() const;

    // Get the storage option used for the table.
    const StorageOption& storageOption() const;

    // Is the table used (i.e. open) in this process.
    static Bool isOpened (const String& tableName);

    // Is the table used (i.e. open) in another process.
    // If <src>checkSubTables</src> is set, it is also checked if
    // a subtable is used in another process.
    Bool isMultiUsed (Bool checkSubTables=False) const;

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
    // acquiring the lock does not succeed immediately. If nattempts>1,
    // the system waits 1 second between each attempt, so nattempts
    // is more or less equal to a wait period in seconds.
    // The return value is false if acquiring the lock failed.
    // If <src>PermanentLocking</src> is in effect, a lock is already
    // present, so nothing will be done.
    // <group>
    Bool lock (FileLocker::LockType = FileLocker::Write, uInt nattempts = 0);
    Bool lock (Bool write, uInt nattempts = 0);
    // </group>

    // Unlock the table. This will also synchronize the table data,
    // thus force the data to be written to disk.
    // If <src>PermanentLocking</src> is in effect, nothing will be done.
    void unlock();

    // Determine the number of locked tables opened with the AutoLock option
    // (Locked table means locked for read and/or write).
    static uInt nAutoLocks();

    // Unlock locked tables opened with the AutoLock option.
    // If <src>all=True</src> all such tables will be unlocked.
    // If <src>all=False</src> only tables requested by another process
    // will be unlocked.
    static void relinquishAutoLocks (Bool all = False);

    // Get the names of tables locked in this process.
    // By default all locked tables are given (note that a write lock
    // implies a read lock), but it is possible to select on lock type
    // FileLocker::Write and on option (TableLock::AutoLocking,
    // TableLock::ReadLocking, or TableLock::PermanentLocking).
    static Vector<String> getLockedTables(FileLocker::LockType=FileLocker::Read,
                                          int lockOption=-1);

    // Determine if column or keyword table data have changed
    // (or is being changed) since the last time this function was called.
    Bool hasDataChanged();

    // Flush the table, i.e. write out the buffers. If <src>sync=True</src>,
    // it is ensured that all data are physically written to disk.
    // Nothing will be done if the table is not writable.
    // At any time a flush can be executed, even if the table is marked
    // for delete.
    // If the table is marked for delete, the destructor will remove
    // files written by intermediate flushes.
    // Note that if necessary the destructor will do an implicit flush,
    // unless it is executed due to an exception.
    // <br>If <src>fsync=True</src> the file contents are fsync-ed to disk,
    // thus ensured that the system buffers are actually written to disk.
    // <br>If <src>recursive=True</src> all subtables are flushed too.
    void flush (Bool fsync=False, Bool recursive=False);

    // Resynchronize the Table object with the table file.
    // This function is only useful if no read-locking is used, ie.
    // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
    // In that cases the table system does not acquire a read-lock, thus
    // does not synchronize itself automatically.
    void resync();

    // Test if the object is null, i.e. does not reference a proper table.
    // This is the case if the default constructor is used.
    Bool isNull() const
      { return (baseTabPtr_p == 0  ?  True : baseTabPtr_p->isNull()); }

    // Throw an exception if the object is null, i.e.
    // if function isNull() is True.
    void throwIfNull() const;

    // Test if the given data type is native to the table system.
    // If not, a virtual column engine is needed to store data with that type.
    // With the function DataType::whatType it can be used in a templated
    // function like:
    // <srcblock>
    //     if (Table::isNativeDataType (whatType(static_cast<T*>(0)))) {
    // </srcblock>
    static Bool isNativeDataType (DataType dtype);

    // Make the table file name.
    static String fileName (const String& tableName);

    // Test if a table with the given name exists and is readable.
    // If not, an exception is thrown if <src>throwIf==True</src>.
    static Bool isReadable (const String& tableName, bool throwIf=False);

    // Return the layout of a table (i.e. description and #rows).
    // This function has the advantage that only the minimal amount of
    // information required is read from the table, thus it is much
    // faster than a normal table open.
    // <br> The number of rows is returned. The description of the table
    // is stored in desc (its contents will be overwritten).
    // <br> An exception is thrown if the table does not exist.
    static uInt getLayout (TableDesc& desc, const String& tableName);

    // Get the table info of the table with the given name.
    // An empty object is returned if the table is unknown.
    static TableInfo tableInfo (const String& tableName);

    // Show the structure of the table.
    // It shows the columns (with types), the data managers, and the subtables.
    // Optionally the columns can be sorted alphabetically.
    void showStructure (std::ostream&,
                        Bool showDataMans=True,
                        Bool showColumns=True,
                        Bool showSubTables=False,
                        Bool sortColumns=False) const;

    // Test if a table with the given name exists and is writable.
    static Bool isWritable (const String& tableName, bool throwIf=False);

    // Find the non-writable files in a table.
    static Vector<String> nonWritableFiles (const String& tableName);

    // Test if this table is the root table (ie. if it is not the subset
    // of another table).
    Bool isRootTable() const;

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
    // If UserLocking is used, it will automatically acquire
    // and release a read lock if the table is not locked.
    const TableRecord& keywordSet() const;

    // Get read/write access to the table keyword set.
    // This requires that the table is locked (or it gets locked
    // if using AutoLocking mode).
    TableRecord& rwKeywordSet();

    // Get access to the TableInfo object.
    // <group>
    const TableInfo& tableInfo() const;
    TableInfo& tableInfo();
    // </group>

    // Write the TableInfo object.
    // Usually this is not necessary, because it is done automatically
    // when the table gets written (by table destructor or flush function).
    // This function is only useful if the table info has to be written
    // before the table gets written (e.g. when another process reads
    // the table while it gets filled).
    void flushTableInfo() const;

    // Get the table description.
    // This can be used to get nr of columns, etc..
    // <src>tableDesc()</src> gives the table description used when
    // constructing the table, while <src>actualTableDesc()</src> gives the
    // actual description, thus with the actual data managers used.
    // <group>
    const TableDesc& tableDesc() const;
    TableDesc actualTableDesc() const;
    // </group>

    // Return all data managers used and the columns served by them.
    // The info is returned in a record. It contains a subrecord per
    // data manager. Each subrecord contains the following fields:
    // <dl>
    //  <dt> TYPE
    //  <dd> a string giving the type of the data manager.
    //  <dt> NAME
    //  <dd> a string giving the name of the data manager.
    //  <dt> COLUMNS
    //  <dd> a vector of strings giving the columns served by the data manager.
    // </dl>
    // Data managers may return some additional fields (e.g. BUCKETSIZE).
    Record dataManagerInfo() const;

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
    // <dd> If a table with this name exists, it will be overwritten.
    //      When succesfully renamed, the table is unmarked
    //      for delete (if necessary).
    // <dt> Table::NewNoReplace
    // <dd> If a table with this name already exists, an exception
    //      is thrown. When succesfully renamed, the table
    //      is unmarked for delete (if necessary).
    // <dt> Table::Scratch
    // <dd> Same as Table::New, but followed by markForDelete().
    // </dl>
    // The scratchCallback function is called when needed.
    void rename (const String& newName, TableOption);

    // Copy the table and all its subtables.
    // Especially for RefTables <src>copy</src> and <src>deepCopy</src> behave
    // differently. <src>copy</src> makes a bitwise copy of the table, thus
    // the result is still a RefTable. On the other hand <src>deepCopy</src>
    // makes a physical copy of all referenced table rows and columns, thus
    // the result is a PlainTable.
    // <br>For PlainTables <src>deepCopy</src> is the same as <src>copy</src>
    // unless <src>valueCopy==True</src> is given. In that case the values
    // are copied which takes longer, but reorganizes the data files to get
    // rid of gaps in the data. Also if specific DataManager info is given
    // or if no rows have to be copied, a deep copy is made.
    // <br>The following options can be given:
    // <dl>
    // <dt> Table::New
    // <dd> If a table with this name exists, it will be overwritten.
    // <dt> Table::NewNoReplace
    // <dd> If a table with this name already exists, an exception
    //      is thrown.
    // <dt> Table::Scratch
    // <dd> Same as Table::New, but followed by markForDelete().
    // </dl>
    // <group>
    // The new table gets the given endian format. Note that the endian option
    // is only used if a true deep copy of a table is made.
    // <br>When making a deep copy, it is possible to specify the data managers
    // using the <src>dataManagerInfo</src> argument.
    // See <src>getDataManagerInfo</src> for more info about that record.
    // <br>If <src>noRows=True</src> no rows are copied. Also no rows are
    // copied in all subtables. It is useful if one wants to make a copy
    // of only the Table structure.
    void copy (const String& newName, TableOption, Bool noRows=False) const;
    void deepCopy (const String& newName,
		   TableOption, Bool valueCopy=False,
		   EndianFormat=AipsrcEndian,
		   Bool noRows=False) const;
    void deepCopy (const String& newName, const Record& dataManagerInfo,
		   TableOption, Bool valueCopy=False,
		   EndianFormat=AipsrcEndian,
		   Bool noRows=False) const;
    // </group>

    // Make a copy of a table to a MemoryTable object.
    // Use the given name for the memory table.
    Table copyToMemoryTable (const String& name, Bool noRows=False) const;

    // Get the table type.
    TableType tableType() const;

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
    
    // Get the number of rows.
    // It is unsynchronized meaning that it will not check if another
    // process updated the table, thus possible increased the number of rows.
    // If one wants to take that into account, he should acquire a
    // read-lock (using the lock function) before using nrow().
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
    // <br>The first <src>offset</src> matching rows will be skipped.
    // <br>If <src>maxRow>0</src>, the selection process will stop
    // when <src>maxRow</src> rows are selected.
    // <br>The TableExprNode argument can be empty (null) meaning that only
    // the <src>maxRow/offset</src> arguments are taken into account.
    Table operator() (const TableExprNode&, uInt maxRow=0, uInt offset=0) const;

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
    // Default sorting algorithm is the parallel sort.
    // <group>
    // Sort on one column.
    Table sort (const String& columnName,
		int = Sort::Ascending,
		int = Sort::ParSort) const;
    // Sort on multiple columns. The principal column has to be the
    // first element in the Block of column names.
    Table sort (const Block<String>& columnNames,
		int = Sort::Ascending,
		int = Sort::ParSort) const;
    // Sort on multiple columns. The principal column has to be the
    // first element in the Block of column names.
    // The order can be given per column.
    Table sort (const Block<String>& columnNames,
		const Block<Int>& sortOrders,
		int = Sort::ParSort) const;
    // Sort on multiple columns. The principal column has to be the
    // first element in the Block of column names.
    // The order can be given per column.
    // Provide some special comparisons via CountedPtrs of compare objects.
    // A null CountedPtr means using the standard compare object
    // from class <linkto class="ObjCompare:description">ObjCompare</linkto>.
    Table sort (const Block<String>& columnNames,
		const Block<CountedPtr<BaseCompare> >& compareObjects,
		const Block<Int>& sortOrders,
		int = Sort::ParSort) const;
    // </group>

    // Get a vector of row numbers in the root table of rows in this table.
    // In case the table is a subset of the root table, this tells which
    // rows of the root table are part of the subset.
    // In case the table is the root table itself, the result is a vector
    // containing the row numbers 0 .. #rows-1.
    // <br>Note that in general it is better to use the next
    // <src>rowNumbers(Table)</src> function.
    Vector<uInt> rowNumbers() const;

    // Get a vector of row numbers in that table of rows in this table.
    // In case the table is a subset of that table, this tells which
    // rows of that table are part of the subset.
    // In case the table is that table itself, the result is a vector
    // containing the row numbers 0 .. #rows-1.
    // <note role=caution>This function is in principle meant for cases
    // where this table is a subset of that table. However, it can be used
    // for any table. In that case the returned vector contains a very high
    // number (max_uint) for rows in this table not part of that table.
    // In that way they are invalid if used elsewhere.
    // <br>In the general case creating the row number vector can be slowish,
    // because it has to do two mappings. However, if this table is a subset
    // of that table and if they are in the same order, the mapping can be done
    // in a more efficient way. The argument <src>tryFast</src> can be used to
    // tell the function to try a fast conversion first. If that cannot be done,
    // it reverts to the slower way at the expense of an unsuccessful fast
    // attempt.
    // </note>
    // <srcblock>
    // Table tab("somename");
    // Table subset = tab(some_select_expression);
    // Vector<uInt> rownrs = subset.rowNumbers(tab);
    // </srcblock>
    // Note that one cannot be sure that table "somename" is the root
    // (i.e. original) table. It may also be a subset of another table.
    // In the latter case doing
    // <br> <src>    Vector<uInt> rownrs = subset.rowNumbers()</src>
    // does not give the row numbers in <src>tab</src>, but in the root table
    // (which is probably not what you want).
  Vector<uInt> rowNumbers (const Table& that, Bool tryFast=False) const;

    // Add a column to the table.
    // The data manager used for the column depend on the function used.
    // Exceptions are thrown if the column already exist or if the
    // table is not writable.
    // <br>If this table is a reference table (result of selection) and if
    // <src>addToParent=True</src> the column is also added to the parent
    // table.
    // <group>
    // Use the first appropriate existing storage manager.
    // If there is none, a data manager is created using the default
    // data manager in the column description.
    void addColumn (const ColumnDesc& columnDesc,
                    Bool addToParent = True);
    // Use an existing data manager with the given name or type.
    // If the flag byName is True, a name is given, otherwise a type.
    // If a name is given, an exception is thrown if the data manager is
    // unknown or does not allow addition of columns.
    // If a type is given, a storage manager of the given type will be
    // created if there is no such data manager allowing addition of rows.
    void addColumn (const ColumnDesc& columnDesc,
		    const String& dataManager, Bool byName,
                    Bool addToParent = True);
    // Use the given data manager (which is a new one).
    void addColumn (const ColumnDesc& columnDesc,
		    const DataManager& dataManager,
                    Bool addToParent = True);
    // </group>

    // Add a bunch of columns using the given new data manager.
    // All columns and possible hypercolumn definitions in the given table
    // description will be copied and added to the table.
    // This can be used in case of specific data managers which need to
    // be created with more than one column (e.g. the tiled hypercube
    // storage managers).
    // <br>The data manager can be given directly or by means of a record
    // describing the data manager in the standard way with the fields
    // TYPE, NAME, and SPEC. The record can contain those fields itself
    // or it can contain a single subrecord with those fields.
    // <br>If this table is a reference table (result of selection) and if
    // <src>addToParent=True</src> the columns are also added to the parent
    // table.
    // <group>
    void addColumn (const TableDesc& tableDesc,
		    const DataManager& dataManager,
                    Bool addToParent = True);
    void addColumn (const TableDesc& tableDesc,
		    const Record& dataManagerInfo,
                    Bool addToParent = True);
    // </group>

    // Test if columns can be removed.
    // It can if the columns exist and if the data manager it is using
    // supports removal of columns or if all columns from a data manager
    // would be removed..
    // <br>You can always remove columns from a reference table.
    // <group>
    Bool canRemoveColumn (const String& columnName) const;
    Bool canRemoveColumn (const Vector<String>& columnNames) const;
    // </group>

    // Remove columns.
    // <br>When removing columns from a reference table, the columns
    // are NOT removed from the underlying table.
    // <group>
    void removeColumn (const String& columnName);
    void removeColumn (const Vector<String>& columnName);
    // </group>

    // Test if a column can be renamed.
    Bool canRenameColumn (const String& columnName) const;

    // Rename a column.
    // An exception is thrown if the old name does not exist or
    // if the name already exists.
    // <note role=caution>
    // Renaming a column should be done with care, because other
    // columns may be referring this column. Also a hypercolumn definition
    // might be using the old name.
    // Finally if may also invalidate persistent selections of a table,
    // because the reference table cannot find the column anymore.
    // </note>
    void renameColumn (const String& newName, const String& oldName);

    void renameHypercolumn (const String& newName, const String& oldName);

    // Write a table to AipsIO (for <src>TypedKeywords<Table></src>).
    // This will only write the table name.
    friend AipsIO& operator<< (AipsIO&, const Table&);

    // Read a table from AipsIO (for <src>TypedKeywords<Table></src>).
    // This will read the table name and open the table as writable
    // if the table file is writable, otherwise as readonly.
    friend AipsIO& operator>> (AipsIO&, Table&);

    // Read a table from AipsIO (for <src>TableKeywords</src>).
    // This will read the table name and open the table as writable
    // if the switch is set and if the table file is writable.
    // otherwise it is opened as readonly.
    void getTableKeyword (AipsIO&, Bool openWritable);

    // Write a table to ostream (for <src>TypedKeywords<Table></src>).
    // This only shows its name and number of columns and rows.
    friend ostream& operator<< (ostream&, const Table&);

    // Find the data manager with the given name or for the given column name.
    DataManager* findDataManager (const String& name,
                                  Bool byColumn=False) const;


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
    static ScratchCallback* scratchCallback_p;


    // Construct a Table object from a BaseTable*.
    // By default the object gets counted.
    Table (BaseTable*, Bool countIt = True);

    // Open an existing table.
    void open (const String& name, const String& type, int tableOption,
	       const TableLock& lockOptions, const TSMOption& tsmOpt);


private:
    // Construct a BaseTable object from the table file.
    static BaseTable* makeBaseTable (const String& name, const String& type,
				     int tableOption,
				     const TableLock& lockOptions,
                                     const TSMOption& tsmOpt,
				     Bool addToCache, uInt locknr);


    // Get the pointer to the underlying BaseTable.
    // This is needed for some friend classes.
    BaseTable* baseTablePtr() const;

    // Look in the cache if the table is already open.
    // If so, check if table option matches.
    // If needed reopen the table for read/write and merge the lock options.
    BaseTable* lookCache (const String& name, int tableOption,
			  const TableLock& tableInfo);

    // Try if v1 is a subset of v2 and fill rows with its indices in v2.
    // Return False if not a proper subset.
    Bool fastRowNumbers (const Vector<uInt>& v1, const Vector<uInt>& v2,
                         Vector<uInt>& rows) const;

    // Show the info of the given columns.
    // Sort the columns if needed.
    void showColumnInfo (ostream& os, const TableDesc&, uInt maxNameLength,
                         const Array<String>& columnNames, Bool sort) const;
};



inline Bool Table::isSameRoot (const Table& other) const
    { return baseTabPtr_p->root() == other.baseTabPtr_p->root(); }

inline void Table::reopenRW()
    { baseTabPtr_p->reopenRW(); }
inline void Table::flush (Bool fsync, Bool recursive)
    { baseTabPtr_p->flush (fsync, recursive); }
inline void Table::resync()
    { baseTabPtr_p->resync(); }

inline const StorageOption& Table::storageOption() const
    { return baseTabPtr_p->storageOption(); }
inline Bool Table::isMultiUsed(Bool checkSubTables) const
    { return baseTabPtr_p->isMultiUsed(checkSubTables); }
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

inline Bool Table::isRootTable() const
    { return baseTabPtr_p == baseTabPtr_p->root(); }

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
inline void Table::deepCopy (const String& newName,
			     const Record& dataManagerInfo,
			     TableOption option,
			     Bool valueCopy,
			     EndianFormat endianFormat,
			     Bool noRows) const
    { baseTabPtr_p->deepCopy (newName, dataManagerInfo, option, valueCopy,
			      endianFormat, noRows); }
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
inline Table::TableType Table::tableType() const
    { return TableType(baseTabPtr_p->tableType()); }
inline int Table::tableOption() const
    { return baseTabPtr_p->tableOption(); }

inline Bool Table::canAddRow() const
    { return baseTabPtr_p->canAddRow(); }
inline Bool Table::canRemoveRow() const
    { return baseTabPtr_p->canRemoveRow(); }
inline Bool Table::canRemoveColumn (const Vector<String>& columnNames) const
    { return baseTabPtr_p->canRemoveColumn (columnNames); }
inline Bool Table::canRenameColumn (const String& columnName) const
    { return baseTabPtr_p->canRenameColumn (columnName); }

inline void Table::addRow (uInt nrrow, Bool initialize)
    { baseTabPtr_p->addRow (nrrow, initialize); }
inline void Table::removeRow (uInt rownr)
    { baseTabPtr_p->removeRow (rownr); }
inline void Table::removeRow (const Vector<uInt>& rownrs)
    { baseTabPtr_p->removeRow (rownrs); }
inline void Table::addColumn (const ColumnDesc& columnDesc, Bool addToParent)
    { baseTabPtr_p->addColumn (columnDesc, addToParent); }
inline void Table::addColumn (const ColumnDesc& columnDesc,
			      const String& dataManager, Bool byName,
                              Bool addToParent)
    { baseTabPtr_p->addColumn (columnDesc, dataManager, byName, addToParent); }
inline void Table::addColumn (const ColumnDesc& columnDesc,
			      const DataManager& dataManager, Bool addToParent)
    { baseTabPtr_p->addColumn (columnDesc, dataManager, addToParent); }
inline void Table::addColumn (const TableDesc& tableDesc,
			      const DataManager& dataManager, Bool addToParent)
    { baseTabPtr_p->addColumn (tableDesc, dataManager, addToParent); }
inline void Table::addColumn (const TableDesc& tableDesc,
			      const Record& dataManagerInfo, Bool addToParent)      { baseTabPtr_p->addColumns (tableDesc, dataManagerInfo, addToParent); }
inline void Table::removeColumn (const Vector<String>& columnNames)
    { baseTabPtr_p->removeColumn (columnNames); }
inline void Table::renameColumn (const String& newName, const String& oldName)
    { baseTabPtr_p->renameColumn (newName, oldName); }
inline void Table::renameHypercolumn (const String& newName, const String& oldName)
    { baseTabPtr_p->renameHypercolumn (newName, oldName); }

inline DataManager* Table::findDataManager (const String& name,
                                            Bool byColumn) const
{
  return baseTabPtr_p->findDataManager (name, byColumn);
}

inline void Table::showStructure (std::ostream& os,
                                  Bool showDataMans,
                                  Bool showColumns,
                                  Bool showSubTables,
                                  Bool sortColumns) const
    { baseTabPtr_p->showStructure (os, showDataMans, showColumns,
                                   showSubTables, sortColumns); }



} //# NAMESPACE CASACORE - END

#endif
