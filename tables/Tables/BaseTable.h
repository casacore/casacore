//# BaseTable.h: Abstract base class for tables
//# Copyright (C) 1994,1995,1996,1997,1998,2000,2001,2002,2003
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

#ifndef TABLES_BASETABLE_H
#define TABLES_BASETABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/StorageOption.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/IO/FileLocker.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RefTable;
// class TableDesc;  !Forward declaration not recognized SGI compiler
class TableLock;
class BaseColumn;
class ColumnDesc;
class TableRecord;
class Record;
class TableExprNode;
class BaseTableIterator;
class DataManager;
class IPosition;
template<class T> class Vector;
template<class T> class Block;
template<class T> class PtrBlock;
class AipsIO;


// <summary>
// Abstract base class for tables
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Table
//   <li> Sort
//   <li> TableExprNode
// </prerequisite>

// <etymology>
// BaseTable is the (abstract) base class for different kind of tables.
// </etymology>

// <synopsis> 
// BaseTables defines many virtual functions, which are actually
// implemented in the underlying table classes like PlainTable and
// RefTable. Other functions like sort and select are implemented
// in BaseTable itself.
//
// The functions in BaseTable and its derived classes can only be
// used by the table system classes. All user access is via the
// envelope class Table, which references (counted) BaseTable.
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Implement function renameColumn, removeColumn.
// </todo>


class BaseTable
{
public:

    // Initialize the object.
    BaseTable (const String& tableName, int tableOption, uInt nrrow);

    virtual ~BaseTable();

    // Link to this BaseTable object (i.e. increase reference count).
    void link();

    // Unlink from a BaseTable.
    // Delete it if no more references.
    static void unlink (BaseTable*);

    // Is the table a null table?
    // By default it is not.
    virtual Bool isNull() const;

    // Reopen the table for read/write.
    virtual void reopenRW() = 0;

    // Is the table stored in big or little endian format?
    virtual Bool asBigEndian() const = 0;

    // Get the storage option used for the table.
    virtual const StorageOption& storageOption() const = 0;

    // Is the table in use (i.e. open) in another process?
    // If <src>checkSubTables</src> is set, it is also checked if
    // a subtable is used in another process.
    virtual Bool isMultiUsed(Bool checkSubTables) const = 0;

    // Get the locking info.
    virtual const TableLock& lockOptions() const = 0;

    // Merge the given lock info with the existing one.
    virtual void mergeLock (const TableLock& lockOptions) = 0;

    // Has this process the read or write lock, thus can the table
    // be read or written safely?
    virtual Bool hasLock (FileLocker::LockType) const = 0;

    // Try to lock the table for read or write access.
    virtual Bool lock (FileLocker::LockType, uInt nattempts) = 0;

    // Unlock the table. This will also synchronize the table data,
    // thus force the data to be written to disk.
    virtual void unlock() = 0;

    // Flush the table, i.e. write it to disk.
    virtual void flush (Bool fsync, Bool recursive) = 0;

    // Resync the Table object with the table file.
    virtual void resync() = 0;

    // Get the modify counter.
    virtual uInt getModifyCounter() const = 0;

    // Set the table to being changed. By default it does nothing.
    virtual void setTableChanged();

    // Do not write the table (used in in case of exceptions).
    void doNotWrite()
	{ noWrite_p = True; }

    // Test if this table is writable.
    // This tells if values can be put into a column.
    virtual Bool isWritable() const = 0;

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

    // Get the table name.
    const String& tableName() const
	{ return name_p; }

    // Get the names of the tables this table consists of.
    // The default implementation adds the name of this table to the block.
    virtual void getPartNames (Block<String>& names, Bool recursive) const;

    // Rename the table.
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
    // The rename function in this base class renames the table file.
    // In a derived class (e.g. PlainTable) the function should also
    // be implemented to rename subtables in its keywords.
    virtual void rename (const String& newName, int tableOption);

    // Copy the table and all its subtables.
    // The default implementation of deepCopy is to call copy.
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
    // <group>
    virtual void copy (const String& newName, int tableOption) const;
    virtual void deepCopy (const String& newName,
			   const Record& dataManagerInfo,
			   int tableOption,
			   Bool valueCopy,
			   int endianFormat,
			   Bool noRows) const;
    // </group>

    // Get the table type.
    // By default it returns Table::Plain.
    virtual int tableType() const;

    // Get the table option.
    int tableOption() const
	{ return option_p; }
    
    // Mark the table for delete.
    // This means that the underlying table gets deleted when it is
    // actually destructed.
    // The scratchCallback function is called when needed.
    void markForDelete (Bool callback, const String& oldName);

    // Unmark the table for delete.
    // This means the underlying table does not get deleted when destructed.
    // The scratchCallback function is called when needed.
    void unmarkForDelete (Bool callback, const String& oldName);

    // Test if the table is marked for delete.
    Bool isMarkedForDelete() const
	{ return delete_p; }
    
    // Get the table description.
    const TableDesc& tableDesc() const
	{ return (tdescPtr_p == 0  ?  makeTableDesc() : *tdescPtr_p); }

    // Get the actual table description.
    virtual TableDesc actualTableDesc() const = 0;

    // Get the data manager info.
    virtual Record dataManagerInfo() const = 0;

    // Show the table structure (implementation of Table::showStructure).
    void showStructure (std::ostream&,
                        Bool showDataMan,
                        Bool showColumns,
                        Bool showSubTables,
                        Bool sortColumns);

    // Get readonly access to the table keyword set.
    virtual TableRecord& keywordSet() = 0;

    // Get read/write access to the table keyword set.
    // This requires that the table is locked (or it gets locked
    // when using AutoLocking mode).
    virtual TableRecord& rwKeywordSet() = 0;

    // Get access to the TableInfo object.
    TableInfo& tableInfo()
	{ return info_p; }

    // Get the table info of the table with the given name.
    // An empty object is returned when the table is unknown.
    static TableInfo tableInfo (const String& tableName);

    // Write the TableInfo object.
    virtual void flushTableInfo();

    // Get number of rows.
    uInt nrow() const
	{ return nrrow_p; }

    // Get a column object using its index.
    virtual BaseColumn* getColumn (uInt columnIndex) const = 0;

    // Get a column object using its name.
    virtual BaseColumn* getColumn (const String& columnName) const = 0;

    // Test if it is possible to add a row to this table.
    virtual Bool canAddRow() const;

    // Add one or more rows and possibly initialize them.
    // This will fail for tables not supporting addition of rows.
    virtual void addRow (uInt nrrow = 1, Bool initialize = True);

    // Test if it is possible to remove a row from this table.
    virtual Bool canRemoveRow() const;

    // Remove rows.
    // This will fail for tables not supporting removal of rows.
    // <note role=tip>
    // The following code fragments do NOT have the same result:
    // <srcblock>
    //    tab.removeRow (10);      // remove row 10
    //    tab.removeRow (20);      // remove row 20, which was 21
    //
    //    Vector<uInt> vec(2);
    //    vec(0) = 10;
    //    vec(1) = 20;
    //    tab.removeRow (vec);     // remove row 10 and 20
    // </srcblock>
    // because in the first fragment removing row 10 turns the former
    // row 21 into row 20.
    // </note>
    // <group>
    virtual void removeRow (uInt rownr);
    void removeRow (const Vector<uInt>& rownrs);
    // </group>

    // Find the data manager with the given name or for the given column.
    virtual DataManager* findDataManager (const String& name,
                                          Bool byColumn) const = 0;

    // Select rows using the given expression (which can be null).
    // Skip first <src>offset</src> matching rows.
    // Return at most <src>maxRow</src> matching rows.
    BaseTable* select (const TableExprNode&, uInt maxRow, uInt offset);

    // Select maxRow rows and skip first offset rows. maxRow=0 means all.
    BaseTable* select (uInt maxRow, uInt offset);

    // Select rows using a vector of row numbers.
    BaseTable* select (const Vector<uInt>& rownrs);

    // Select rows using a mask block.
    // The length of the block must match the number of rows in the table.
    // If True, the corresponding row will be selected.
    BaseTable* select (const Block<Bool>& mask);

    // Project the given columns (i.e. select the columns).
    BaseTable* project (const Block<String>& columnNames);

    //# Virtually concatenate all tables in this column.
    //# The column cells must contain tables with the same description.
//#//    BaseTable* concatenate (const String& columnName);

    // Do logical operations on a table.
    // <group>
    // intersection with another table
    BaseTable* tabAnd (BaseTable*);
    // union with another table
    BaseTable* tabOr  (BaseTable*);
    // subtract another table
    BaseTable* tabSub (BaseTable*);
    // xor with another table
    BaseTable* tabXor (BaseTable*);
    // take complement
    BaseTable* tabNot ();
    // </group>

    // Sort a table on one or more columns of scalars.
    BaseTable* sort (const Block<String>& columnNames,
		     const Block<CountedPtr<BaseCompare> >& compareObjects,
		     const Block<Int>& sortOrder, int sortOption);

    // Create an iterator.
    BaseTableIterator* makeIterator (const Block<String>& columnNames,
                                     const Block<CountedPtr<BaseCompare> >&,
				     const Block<Int>& orders, int option);

    // Add one or more columns to the table.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    virtual void addColumn (const ColumnDesc& columnDesc, Bool addToParent);
    virtual void addColumn (const ColumnDesc& columnDesc,
			    const String& dataManager, Bool byName,
                            Bool addToParent);
    virtual void addColumn (const ColumnDesc& columnDesc,
			    const DataManager& dataManager, Bool addToParent);
    virtual void addColumn (const TableDesc& tableDesc,
			    const DataManager& dataManager, Bool addToParent);
    // </group>

    // Add one or more columns to the table.
    // The data manager to use is described in the record.
    void addColumns (const TableDesc& tableDesc, const Record& dmInfo,
                     Bool addToParent);

    // Test if columns can be removed.
    virtual Bool canRemoveColumn (const Vector<String>& columnNames) const = 0;

    // Remove columns.
    virtual void removeColumn (const Vector<String>& columnNames) = 0;

    // Check if the set of columns can be removed.
    // It checks if columns have not been specified twice and it
    // checks if they exist.
    // If the flag is set an exception is thrown if errors are found.
    Bool checkRemoveColumn (const Vector<String>& columnNames,
			    Bool throwException) const;

    // Test if a column can be renamed.
    virtual Bool canRenameColumn (const String& columnName) const = 0;

    // Rename a column.
    virtual void renameColumn (const String& newName,
			       const String& oldName) = 0;

    // Rename a hypercolumn.
    virtual void renameHypercolumn (const String& newName,
				    const String& oldName) = 0;

    // Get a vector of row numbers.
    // By default it returns the row numbers 0..nrrow()-1.
    // It needs to be implemented for RefTable only.
    virtual Vector<uInt> rowNumbers() const;

    // Get pointer to root table (i.e. parent of a RefTable).
    // Default it is this table.
    // It is meant for the reference tables after a select or sort which
    // can then still name their parent as the root.
    virtual BaseTable* root();

    // Tell if the table is in row order.
    // By default it is, since normally a table is always in row order.
    // It is meant for RefTable-s, where the rows can be in
    // another (sorted) order.
    virtual Bool rowOrder() const;

    // By the default the table cannot return the storage of rownrs.
    // That can only be done by a RefTable, where it is implemented.
    virtual Vector<uInt>* rowStorage();

    // Adjust the row numbers to be the actual row numbers in the
    // root table. This is, for instance, used when a RefTable is sorted.
    // Optionally it also determines if the resulting rows are in order.
    virtual Bool adjustRownrs (uInt nrrow, Vector<uInt>& rownrs,
			       Bool determineOrder) const;

    // Do the actual sort.
    // The default implementation is suitable for almost all cases.
    // Only in RefTable a smarter implementation is provided.
    virtual BaseTable* doSort (PtrBlock<BaseColumn*>&,
                               const Block<CountedPtr<BaseCompare> >&,
			       const Block<Int>& sortOrder,
			       int sortOption);

    // Create a RefTable object.
    RefTable* makeRefTable (Bool rowOrder, uInt initialNrrow);

    // Check if the row number is valid.
    // It throws an exception if out of range.
    void checkRowNumber (uInt rownr) const
        { if (rownr >= nrrow_p + nrrowToAdd_p) checkRowNumberThrow (rownr); }

    // Get the table's trace-id.
    int traceId() const
        { return itsTraceId; }


protected:
    uInt           nrlink_p;            //# #references to this table
    uInt           nrrow_p;             //# #rows in this table
    uInt           nrrowToAdd_p;        //# #rows to be added
    TableDesc*     tdescPtr_p;          //# Pointer to table description
    String         name_p;              //# table name
    int            option_p;            //# Table constructor option
    Bool           noWrite_p;           //# False = do not write the table
    Bool           delete_p;            //# True = delete when destructed
    TableInfo      info_p;              //# Table information (type, etc.)
    Bool           madeDir_p;           //# True = table dir has been created
    int            itsTraceId;          //# table-id for TableTrace tracing


    // Do the callback for scratch tables (if callback is set).
    void scratchCallback (Bool isScratch, const String& oldName) const;

    // Create the table directory when needed (and possible).
    // When the file already exists, check if it is a directory.
    // It returns True when it actually created the directory.
    Bool makeTableDir();

    // Make a true deep copy of the table.
    void trueDeepCopy (const String& newName,
		       const Record& dataManagerInfo,
		       int tableOption,
		       int endianFormat,
		       Bool noRows) const;

    // Prepare for copying or renaming a table.
    // It checks if the target table already exists and removes it
    // when necessary.
    void prepareCopyRename (const String& newName, int tableOption) const;

    // Rename the subtables (used by rename function).
    virtual void renameSubTables (const String& newName,
				  const String& oldName);

    // Check if the table already exists.
    // Throw an exception if so.
    void throwIfTableExists();

    // Test if the table is opened for write.
    Bool openedForWrite() const;

    // Start writing a table. It does a putstart and writes <src>nrrow_p</src>.
    // It should be ended by calling <src>writeEnd</src>.
    void writeStart (AipsIO&, Bool bigEndian);

    // End writing a table.
    void writeEnd (AipsIO&);

    // Should the table be written.
    // This flag is False if an exception was thrown.
    Bool shouldNotWrite() const
	{ return noWrite_p; }

    // Read the TableInfo object.
    void getTableInfo();

private:
    // Copy constructor is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    BaseTable (const BaseTable&);

    // Assignment is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    BaseTable& operator= (const BaseTable&);

    // Show a possible extra table structure header.
    // It is used by e.g. RefTable to show which table is referenced.
    virtual void showStructureExtra (std::ostream&) const;

    // Show the info of the given columns.
    // Sort the columns if needed.
    void showColumnInfo (ostream& os, const TableDesc&, uInt maxNameLength,
                         const Array<String>& columnNames, Bool sort) const;

    // Throw an exception for checkRowNumber.
    void checkRowNumberThrow (uInt rownr) const;

    // Check if the tables combined in a logical operation have the
    // same root.
    void logicCheck (BaseTable* that);

    // Get the rownrs of the table in ascending order to be
    // used in the logical operation on the table.
    uInt logicRows (uInt*& rownrs, Bool& allocated);

    // Make an empty table description.
    // This is used if one asks for the description of a NullTable.
    // Creating an empty TableDesc in the NullTable takes too much time.
    // Furthermore it causes static initialization order problems.
    const TableDesc& makeTableDesc() const;

    // Make the name absolute.
    // It first checks if the name contains valid characters (not only . and /).
    String makeAbsoluteName (const String& name) const;
};




} //# NAMESPACE CASACORE - END

#endif
