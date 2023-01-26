//# ColumnSet.h: Class to manage a set of table columns
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

#ifndef TABLES_COLUMNSET_H
#define TABLES_COLUMNSET_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableLockData.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/tables/Tables/StorageOption.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/ArrayFwd.h>

#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class SetupNewTable;
class Table;
class TableDesc;
class TSMOption;
class BaseTable;
class TableAttr;
class ColumnDesc;
class PlainColumn;
class DataManager;
class MultiFile;
class Record;
class IPosition;
class AipsIO;

// <summary>
// Class to manage a set of table columns
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> PlainTable
//   <li> DataManager
// </prerequisite>

// <etymology>
// ColumnSet represent the set of columns in a table.
// </etymology>

// <synopsis> 
// ColumnSet contains all columns in a plain table (thus not in a RefTable).
// Furthermore it contains the set of data managers used by the columns
// in the table.
//
// The main purpose of the class is to deal with constructing, writing
// and reading the column objects. It is used by classes SetupNewTable
// and Table.
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class ColumnSet
{
public:

    // Construct from the table description.
    // This creates all underlying filled and virtual column objects.
    ColumnSet (TableDesc*, const StorageOption& = StorageOption());

    ~ColumnSet();

    // Reopen the data managers for read/write.
    void reopenRW();

    // Rename the necessary subtables in the column keywords.
    void renameTables (const String& newName, const String& oldName);

    // Get the storage option.
    const StorageOption& storageOption() const
      { return storageOpt_p; }

    // Get the possible MultiFileBase object used to combine files.
    std::shared_ptr<MultiFileBase> getMultiFile() const
      { return multiFile_p; }

    // Are subtables used in other processes.
    bool areTablesMultiUsed() const;

    // Get a column by name.
    PlainColumn* getColumn (const String& columnName) const;

    // Get a column by index.
    PlainColumn* getColumn (uint32_t columnIndex) const;

    // Add a data manager.
    // It increments seqCount_p and returns that as a unique sequence number.
    // This can, for instance, be used to create a unique file name.
    void addDataManager (DataManager*);

    // Initialize the data managers for a new table.
    // It creates the data manager column objects for each column
    // and it allows the data managers to link themselves to the
    // Table object and to initialize themselves.
    void initDataManagers (rownr_t nrrow, bool bigEndian,
                           const TSMOption& tsmOption,
                           Table& tab);

    // Link the ColumnSet object to the BaseTable object.
    void linkToTable (BaseTable* baseTableObject);

    // Link the ColumnSet object to the TableLockData object.
    void linkToLockObject (TableLockData* lockObject);

    // Check if the table is locked for read or write.
    // If manual or permanent locking is in effect, it checks if the
    // table is properly locked.
    // If autolocking is in effect, it locks the table when needed.
    // <group>
    void checkReadLock (bool wait);
    void checkWriteLock (bool wait);
    // </group>

    // Inspect the auto lock when the inspection interval has expired and
    // release it when another process needs the lock.
    void autoReleaseLock();

    // If needed, get a temporary user lock.
    // It returns false if the lock was already there.
    bool userLock (FileLocker::LockType, bool wait);

    // Release a temporary user lock if the given release flag is true.
    void userUnlock (bool releaseFlag);

    // Do all data managers and engines allow to add rows?
    bool canAddRow() const;

    // Do all data managers and engines allow to remove rows?
    bool canRemoveRow() const;

    // Can the given columns be removed from the data manager?
    bool canRemoveColumn (const Vector<String>& columnNames) const;

    // Can a column be renamed in the data manager?
    bool canRenameColumn (const String& columnName) const;

    // Add rows to all data managers.
    void addRow (rownr_t nrrow);

    // Remove a row from all data managers.
    // It will throw an exception if not possible.
    void removeRow (rownr_t rownr);

    // Remove the columns from the map and the data manager.
    void removeColumn (const Vector<String>& columnNames);

    // Rename the column in the map.
    void renameColumn (const String& newName, const String& oldName);

    // Add a column to the table.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    void addColumn (const ColumnDesc& columnDesc,
		    bool bigEndian, const TSMOption& tsmOption, Table& tab);
    void addColumn (const ColumnDesc& columnDesc,
		    const String& dataManager, bool byName,
		    bool bigEndian, const TSMOption& tsmOption, Table& tab);
    void addColumn (const ColumnDesc& columnDesc,
		    const DataManager& dataManager,
		    bool bigEndian, const TSMOption& tsmOption, Table& tab);
    void addColumn (const TableDesc& tableDesc,
		    const DataManager& dataManager,
		    bool bigEndian, const TSMOption& tsmOption, Table& tab);
    // </group>

    // Get nr of rows.
    rownr_t nrow() const;

    // Get the actual table description.
    TableDesc actualTableDesc() const;

    // Get the data manager info.
    // Optionally only the virtual engines are retrieved.
    Record dataManagerInfo (bool virtualOnly=false) const;

    // Get the trace-id of the table.
    int traceId() const
      { return baseTablePtr_p->traceId(); }

    // Initialize rows startRownr till endRownr (inclusive).
    void initialize (rownr_t startRownr, rownr_t endRownr);

    // Write all the data and let the data managers flush their data.
    // This function is called when a table gets written (i.e. flushed).
    // It returns true if any data manager wrote something.
    bool putFile (bool writeTable, AipsIO&, const TableAttr&, bool fsync);

    // Read the data, reconstruct the data managers, and link those to
    // the table object.
    // This function gets called when an existing table is read back.
    // It returns the number of rows in case a data manager thinks there are
    // more. That is in particular used by LofarStMan.
    rownr_t getFile (AipsIO&, Table& tab, rownr_t nrrow, bool bigEndian,
                     const TSMOption& tsmOption);

    // Set the table to being changed.
    void setTableChanged();

    // Get the data manager change flags (used by PlainTable).
    Block<bool>& dataManChanged();

    // Synchronize the data managers when data in them have changed.
    // It returns the number of rows it think it has, which is needed for
    // storage managers like LofarStMan.
    // <src>forceSync=true</src> means that the data managers are forced
    // to do a sync. Otherwise the contents of the lock file tell if a data
    // manager has to sync.
    rownr_t resync (rownr_t nrrow, bool forceSync);

    // Invalidate the column caches for all columns.
    void invalidateColumnCaches();

    // Get the correct data manager.
    // This is used by the column objects to link themselves to the
    // correct datamanagers when they are read back.
    DataManager* getDataManager (uint32_t seqnr) const;

    // Check if no double data manager names have been given.
    void checkDataManagerNames (const String& tableName) const;

    // Find the data manager with the given name or for the given column.
    // If the data manager or column is unknown, an exception is thrown.
    // A blank name means the data manager is unknown.
    DataManager* findDataManager (const String& name,
                                  bool byColumn=false) const;

    // Make a unique data manager name by appending a suffix _n if needed
    // where n is a number that makes the name unique.
    String uniqueDataManagerName (const String& name) const;

    // Synchronize the columns after it appeared that data in the
    // main table file have changed.
    // It cannot deal with changes in number of columns, so it throws an
    // exception when they have changed.
    // Keywords in all columns are updated.
    // The other ColumnSet gives the new data.
    void syncColumns (const ColumnSet& other, const TableAttr& defaultAttr);

private:
    // Remove the last data manager (used by addColumn after an exception).
    // It does the opposite of addDataManager.
    void removeLastDataManager();

    // Let the data managers (from the given index on) initialize themselves.
    void initSomeDataManagers (uint32_t from, Table& tab);

    // Let the data managers (from the given index on) prepare themselves.
    void prepareSomeDataManagers (uint32_t from);

    // Open or create the MultiFile if needed.
    void openMultiFile (uint32_t from, const Table& tab,
                        ByteIO::OpenOption);

    // Check if a data manager name has not already been used.
    // Start checking at the given index in the array.
    // It returns false if the name has already been used.
    // By default an exception is thrown if the name has already been used.
    bool checkDataManagerName (const String& name, uint32_t from,
                               const String& tableName,
			       bool doTthrow=true) const;

    // Do the actual addition of a column.
    void doAddColumn (const ColumnDesc& columnDesc, DataManager* dataManPtr);

    // Check if columns to be removed can be removed.
    // It returns a map of DataManager* telling how many columns for
    // a data manager have to be removed. A count of -1 means that all
    // columns have to be removed. For such columns the flag in the
    // returned Block is false, otherwise true.
    std::map<void*,int32_t> checkRemoveColumn (const Vector<String>& columnNames);

    // Check if the table is locked for read or write.
    // If manual or permanent locking is in effect, it checks if the
    // table is properly locked.
    // If autolocking is in effect, it locks the table when needed.
    void doLock (FileLocker::LockType, bool wait);


    //# Declare the variables.
    TableDesc*              tdescPtr_p;
    StorageOption           storageOpt_p;
    std::shared_ptr<MultiFileBase> multiFile_p;
    rownr_t                 nrrow_p;          //# #rows
    BaseTable*              baseTablePtr_p;
    TableLockData*          lockPtr_p;        //# lock object
    std::map<String,void*>  colMap_p;         //# list of PlainColumns
    uint32_t                    seqCount_p;       //# sequence number count
    //#                                           (used for unique seqnr)
    Block<void*>            blockDataMan_p;   //# list of data managers
    Block<bool>             dataManChanged_p; //# data has changed
};



inline rownr_t ColumnSet::nrow() const
{
    return nrrow_p;
}
inline void ColumnSet::linkToTable (BaseTable* baseTableObject)
{
    baseTablePtr_p = baseTableObject;
}
inline void ColumnSet::setTableChanged()
{
    baseTablePtr_p->setTableChanged();
}
inline void ColumnSet::linkToLockObject (TableLockData* lockObject)
{
    lockPtr_p = lockObject;
}
inline void ColumnSet::checkReadLock (bool wait)
{
    if (lockPtr_p->readLocking()
    &&  ! lockPtr_p->hasLock (FileLocker::Read)) {
	doLock (FileLocker::Read, wait);
    }
}
inline void ColumnSet::checkWriteLock (bool wait)
{
    if (! lockPtr_p->hasLock (FileLocker::Write)) {
	doLock (FileLocker::Write, wait);
    }
}
inline void ColumnSet::userUnlock (bool releaseFlag)
{
    if (releaseFlag) {
	lockPtr_p->release();
    }
}
inline void ColumnSet::autoReleaseLock()
{
    lockPtr_p->autoRelease();
}
inline Block<bool>& ColumnSet::dataManChanged()
{
    return dataManChanged_p;
}




} //# NAMESPACE CASACORE - END

#endif
