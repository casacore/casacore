//# PlainTable.h: Class defining a plain regular table
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

#if !defined(AIPS_PLAINTABLE_H)
#define AIPS_PLAINTABLE_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/BaseTable.h>
#include <aips/Tables/TableCache.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableSyncData.h>
#include <aips/IO/AipsIO.h>

//# Forward Declarations
class SetupNewTable;
class TableLock;
class TableLockData;
class ColumnSet;
class IPosition;
class AipsIO;
class MemoryIO;


// <summary>
// Class defining a plain regular table
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> BaseTable
//   <li> BaseColumn
// </prerequisite>

// <etymology>
// PlainTable represents a plain regular table. This is opposed to a
// RefTable, which is a view on a PlainTable.
// </etymology>

// <synopsis> 
// PlainTable is a table consisting of a keyword set and a number of
// filled and virtual columns. The table control information and the
// keyword set is stored in an AipsIO file. The data in the filled columns
// are stored separately by storage managers.
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> notify RefTable's when deleting rows
// </todo>

class PlainTable : public BaseTable
{
public:

    // Construct the object for a new table.
    // It creates storage manager(s) for unbound columns and initializes
    // all storage managers. The given number of rows is stored in
    // the table and initialized if the flag is set.
    PlainTable (SetupNewTable&, uInt nrrow, Bool initialize,
		const TableLock& lockOptions);

    // Construct the object for an existing table.
    // It opens the table file, reads the table control information
    // and creates and initializes the required storage managers.
    PlainTable (AipsIO&, uInt version, const String& name, const String& type,
		uInt nrrow, int option, const TableLock& lockOptions);

    // The destructor flushes (i.e. writes) the table if it is opened
    // for output and not marked for delete.
    ~PlainTable();

    // Return the layout of a table (i.e. description and #rows).
    // This function has the advantage that only the minimal amount of
    // information required is read from the table, thus it is much
    // faster than a normal table open.
    // <br> The number of rows is returned. The description of the table
    // is stored in desc (its contents will be overwritten).
    static void getLayout (TableDesc& desc, AipsIO& ios);

    // Try to reopen the table for read/write access.
    // An exception is thrown if the table is not writable.
    // Nothing is done if the table is already open for read/write.
    virtual void reopenRW();

    // Is the table in use (i.e. open) in another process?
    virtual Bool isMultiUsed() const;

    // Get the locking info.
    virtual const TableLock& lockOptions() const;

    // Merge the given lock info with the existing one.
    virtual void mergeLock (const TableLock& lockOptions);

    // Has this process the read or write lock, thus can the table
    // be read or written safely?
    virtual Bool hasLock (FileLocker::LockType) const;

    // Try to lock the table for read or write access.
    virtual Bool lock (FileLocker::LockType, uInt nattempts);

    // Unlock the table. This will also synchronize the table data,
    // thus force the data to be written to disk.
    virtual void unlock();

    // Do a release of an AutoLock when the inspection interval has expired.
    void autoReleaseLock();

    // Flush the table, i.e. write it to disk.
    // Nothing will be done if the table is not writable.
    // A flush can be executed at any time.
    // When a table is marked for delete, the destructor will remove
    // files written by intermediate flushes.
    // Note that if necessary the destructor will do an implicit flush,
    // unless it is executed due to an exception.
    virtual void flush (Bool fsync);

    // Get the modify counter.
    virtual uInt getModifyCounter() const;

    // Set the table to being changed.
    void setTableChanged();

    // Convert a Table option to an AipsIO file option.
    // This is used by storage managers.
    static ByteIO::OpenOption toAipsIOFoption (int tableOption);

    // Test if the table is opened as writable.
    Bool isWritable() const;

    // Get readonly access to the table keyword set.
    TableRecord& keywordSet();

    // Get read/write access to the table keyword set.
    // This requires that the table is locked (or it gets locked
    // when using AutoLocking mode).
    TableRecord& rwKeywordSet();

    // The rename function in this derived class uses BaseTable::rename
    // to rename the table file. Thereafter the subtables in its
    // table and column keywords are renamed.
    void rename (const String& newName, int tableOption);

    // Get a column object using its index.
    BaseColumn* getColumn (uInt columnIndex) const;

    // Get a column object using its name.
    BaseColumn* getColumn (const String& columnName) const;

    // Test if it is possible to add a row to this table.
    Bool canAddRow() const;

    // Add one or more rows and possibly initialize them.
    // This will fail for tables not supporting addition of rows.
    void addRow (uInt nrrow, Bool initialize);

    // Test if it is possible to remove a row from this table.
    Bool canRemoveRow() const;

    // Remove the given row.
    // This will fail for tables not supporting removal of rows.
    void removeRow (uInt rownr);

    // Add a column to the table.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    void addColumn (const ColumnDesc& columnDesc);
    void addColumn (const ColumnDesc& columnDesc,
		    const String& dataManager, Bool byName);
    void addColumn (const ColumnDesc& columnDesc,
		    const DataManager& dataManager);
    void addColumn (const TableDesc& tableDesc,
		    const DataManager& dataManager);
    // </group>

    // Test if a column can be removed (no).
    Bool canRemoveColumn (const String& columnName) const;

    // Remove a column.
    void removeColumn (const String& columnName);

    // Find the data manager with the given name.
    DataManager* findDataManager (const String& dataManagerName) const;


    static TableCache tableCache;           //# cache of open (plain) tables

private:
    ColumnSet*     colSetPtr_p;              //# pointer to set of columns
    Bool           tableChanged_p;           //# Has the main data changed?
    TableLockData* lockPtr_p;                //# pointer to lock object
    TableSyncData  lockSync_p;               //# table synchronization

    // Copy constructor is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    PlainTable (const PlainTable&);

    // Assignment is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    PlainTable& operator= (const PlainTable&);

    // Rename the subtables (used by rename function).
    virtual void renameSubTables (const String& newName,
				  const String& oldName);

    // The callback function when a lock is released.
    // This flushes the table data, writes the synchronization data
    // into the MemoryIO object, and returns a pointer to it.
    // <group>
    static MemoryIO* releaseCallBack (void* plainTableObject, Bool always);
    MemoryIO* doReleaseCallBack (Bool always);
    // </group>

    // When needed, write the table control information in an AipsIO file.
    // Tell the storage managers to flush and close their files.
    // It returns a switch to tell if the table control information has
    // been written.
    Bool putFile (Bool always);

    // Read the table control information from the given AipsIO stream
    // and create and initialize the required storage managers.
    void getFile (AipsIO&);
};



inline void PlainTable::setTableChanged()
{
    tableChanged_p = True;
}



#endif
