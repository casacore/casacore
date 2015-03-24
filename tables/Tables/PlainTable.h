//# PlainTable.h: Class defining a plain regular table
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

#ifndef TABLES_PLAINTABLE_H
#define TABLES_PLAINTABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/tables/Tables/TableCache.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableSyncData.h>
#include <casacore/tables/DataMan/TSMOption.h>
#include <casacore/casa/IO/AipsIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
		const TableLock& lockOptions, int endianFormat,
                const TSMOption& tsmOption);

    // Construct the object for an existing table.
    // It opens the table file, reads the table control information
    // and creates and initializes the required storage managers.
    PlainTable (AipsIO&, uInt version, const String& name, const String& type,
		uInt nrrow, int option, const TableLock& lockOptions,
		const TSMOption& tsmOption, Bool addToCache, uInt locknr);

    // The destructor flushes (i.e. writes) the table if it is opened
    // for output and not marked for delete.
    virtual ~PlainTable();

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

    // Is the table stored in big or little endian format?
    virtual Bool asBigEndian() const;

    // Get the storage option used for the table.
    virtual const StorageOption& storageOption() const;

    // Is the table in use (i.e. open) in another process?
    // If <src>checkSubTables</src> is set, it is also checked if
    // a subtable is used in another process.
    virtual Bool isMultiUsed (Bool checkSubTables) const;

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
    // <src>always=True</src> means that the inspection is always done,
    // thus not every 25th call or so.
    void autoReleaseLock (Bool always = False);

    // Flush the table, i.e. write it to disk.
    // Nothing will be done if the table is not writable.
    // A flush can be executed at any time.
    // When a table is marked for delete, the destructor will remove
    // files written by intermediate flushes.
    // Note that if necessary the destructor will do an implicit flush,
    // unless it is executed due to an exception.
    virtual void flush (Bool fsync, Bool recursive);

    // Resync the Table object with the table file.
    virtual void resync();

    // Get the modify counter.
    virtual uInt getModifyCounter() const;

    // Set the table to being changed.
    virtual void setTableChanged();

    // Convert a Table option to an AipsIO file option.
    // This is used by storage managers.
    static ByteIO::OpenOption toAipsIOFoption (int tableOption);

    // Test if the table is opened as writable.
    virtual Bool isWritable() const;

    // Get the actual table description.
    virtual TableDesc actualTableDesc() const;

    // Get the data manager info.
    virtual Record dataManagerInfo() const;

    // Get readonly access to the table keyword set.
    virtual TableRecord& keywordSet();

    // Get read/write access to the table keyword set.
    // This requires that the table is locked (or it gets locked
    // when using AutoLocking mode).
    virtual TableRecord& rwKeywordSet();

    // Get a column object using its index.
    virtual BaseColumn* getColumn (uInt columnIndex) const;

    // Get a column object using its name.
    virtual BaseColumn* getColumn (const String& columnName) const;

    // Test if it is possible to add a row to this table.
    virtual Bool canAddRow() const;

    // Add one or more rows and possibly initialize them.
    // This will fail for tables not supporting addition of rows.
    virtual void addRow (uInt nrrow, Bool initialize);

    // Test if it is possible to remove a row from this table.
    virtual Bool canRemoveRow() const;

    // Remove the given row.
    // This will fail for tables not supporting removal of rows.
    virtual void removeRow (uInt rownr);

    // Add a column to the table.
    // The last Bool argument is not used in PlainTable, but can be used in
    // other classes derived from BaseTable.
    // <group>
    virtual void addColumn (const ColumnDesc& columnDesc, Bool);
    virtual void addColumn (const ColumnDesc& columnDesc,
			    const String& dataManager, Bool byName, Bool);
    virtual void addColumn (const ColumnDesc& columnDesc,
			    const DataManager& dataManager, Bool);
    virtual void addColumn (const TableDesc& tableDesc,
			    const DataManager& dataManager, Bool);
    // </group>

    // Test if columns can be removed.
    virtual Bool canRemoveColumn (const Vector<String>& columnNames) const;

    // Remove columns.
    virtual void removeColumn (const Vector<String>& columnNames);

    // Test if a column can be renamed (yes).
    virtual Bool canRenameColumn (const String& columnName) const;

    // Rename a column.
    virtual void renameColumn (const String& newName, const String& oldName);

    // Rename a hypercolumn.
    virtual void renameHypercolumn (const String& newName,
				    const String& oldName);

    // Find the data manager with the given name or for the given column.
    virtual DataManager* findDataManager (const String& name,
                                          Bool byColumn) const;


    // Get access to the TableCache.
    static TableCache& tableCache()
      { return theirTableCache; }

private:
    // Copy constructor is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    PlainTable (const PlainTable&);

    // Assignment is forbidden, because copying a table requires
    // some more knowledge (like table name of result).
    // Declaring it private, makes it unusable.
    PlainTable& operator= (const PlainTable&);

    // Close the object which is called by the destructor.
    void closeObject();

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

    // Synchronize the table after having acquired a lock which says
    // that main table data has changed.
    // It check if the columns did not change.
    // It updates the table and column keywords.
    void syncTable();

    // Determine and set the endian format (big or little).
    void setEndian (int endianFormat);

    // Throw an exception if the table is not writable.
    void checkWritable (const char* func) const;


    ColumnSet*     colSetPtr_p;        //# pointer to set of columns
    Bool           tableChanged_p;     //# Has the main data changed?
    Bool           addToCache_p;       //# Is table added to cache?
    TableLockData* lockPtr_p;          //# pointer to lock object
    TableSyncData  lockSync_p;         //# table synchronization
    Bool           bigEndian_p;        //# True  = big endian canonical
                                       //# False = little endian canonical
    TSMOption      tsmOption_p;
    //# cache of open (plain) tables
    static TableCache theirTableCache;
};



} //# NAMESPACE CASACORE - END

#endif
