//# ColumnSet.h: Class to manage a set of table columns
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

#if !defined(AIPS_COLUMNSET_H)
#define AIPS_COLUMNSET_H


//# Includes
#include <aips/aips.h>
#include <aips/Tables/TableLockData.h>
#include <aips/Containers/SimOrdMap.h>
#include <aips/Utilities/String.h>

//# Forward Declarations
class SetupNewTable;
class Table;
class TableDesc;
class PlainTable;
class ColumnDesc;
class PlainColumn;
class DataManager;
class IPosition;
class AipsIO;


// <summary>
// Class to manage a set of table columns
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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
    ColumnSet (TableDesc*);

    ~ColumnSet();

    // Reopen the data managers for read/write.
    void reopenRW();

    // Rename the necessary subtables in the column keywords.
    void renameTables (const String& newName, const String& oldName);

    // Get a column by name.
    PlainColumn* getColumn (const String& columnName) const;

    // Get a column by index.
    PlainColumn* getColumn (uInt columnIndex) const;

    // Add a data manager.
    // It increments seqCount_p and returns that as a unique sequence number.
    // This can, for instance, be used to create a unique file name.
    void addDataManager (DataManager*);

    // Initialize the data managers for a new table.
    // It creates the data manager column objects for each column
    // and it allows the data managers to link themselves to the
    // Table object and to initialize themselves.
    void initDataManagers (uInt nrrow, Table& tab);

    // Link the ColumnSet object to the TableLockData object in the
    // PlainTable object.
    void linkToLockObject (PlainTable* plainTableObject,
			   TableLockData* lockObject);

    // Check if the table is locked for read or write.
    // If manual or permanent locking is in effect, it checks if the
    // table is properly locked.
    // If autolocking is in effect, it locks the table when needed.
    void checkLock (FileLocker::LockType, Bool wait);

    // Inspect the auto lock when the inspection interval has expired and
    // release it when another process needs the lock.
    void autoReleaseLock();

    // Do all data managers and engines allow to add rows?
    Bool canAddRow() const;

    // Do all data managers and engines allow to remove rows?
    Bool canRemoveRow() const;

    // Can a column be removed from the data manager?
    Bool canRemoveColumn (const String& columnName) const;

    // Add rows to all data managers.
    void addRow (uInt nrrow);

    // Remove a row from all data managers.
    // It will throw an exception if not possible.
    void removeRow (uInt rownr);

    // Add a column to the table.
    // The default implementation throws an "invalid operation" exception.
    // <group>
    void addColumn (const ColumnDesc& columnDesc, Table& tab);
    void addColumn (const ColumnDesc& columnDesc,
		    const String& dataManager, Bool byName, Table& tab);
    void addColumn (const ColumnDesc& columnDesc,
		    const DataManager& dataManager, Table& tab);
    void addColumn (const TableDesc& tableDesc,
		    const DataManager& dataManager, Table& tab);
    // </group>

    // Get nr of rows.
    uInt nrow() const;

    // Initialize rows startRownr till endRownr (inclusive).
    void initialize (uInt startRownr, uInt endRownr);

    // Write all the data and let the data managers flush their data.
    // This function is called when a table gets written (i.e. flushed).
    void putFile (Bool writeTable, AipsIO&, const String& tableName,
		  Bool fsync);

    // Read the data, reconstruct the data managers, and link those to
    // the table object.
    // This function gets called when an existing table is read back.
    void getFile (AipsIO&, Table& tab, uInt nrrow);

    // Set the table to being changed.
    void setTableChanged();

    // Get the data manager change flags (used by PlainTable).
    Block<Bool>& dataManChanged();

    // Synchronize the data managers when data in them have changed.
    void resync (uInt nrrow);

    // Invalidate the column caches for all columns.
    void invalidateColumnCaches();

    // Get the correct data manager.
    // This is used by the column objects to link themselves to the
    // correct datamanagers when they are read back.
    DataManager* getDataManager (uInt seqnr) const;

    // Check if no double data manager names have been given.
    void checkDataManagerNames() const;

    // Find the data manager with the given name.
    // When the data manager is unknown, an exception is thrown.
    // A blank name means the data manager is unknown.
    DataManager* findDataManager (const String& dataManagerName) const;

private:
    // Remove the last data manager (used by addColumn after an exception).
    // It does the opposite of addDataManager.
    void removeLastDataManager();

    // Let the data managers (from the given index on) initialize themselves.
    void initSomeDataManagers (uInt from, Table& tab);

    // Let the data managers (from the given index on) prepare themselves.
    void prepareSomeDataManagers (uInt from);

    // Check if a data manager name has not already been given.
    // Start checking at the given index in the array.
    void checkDataManagerName (const String& name, uInt from) const;

    // Do the actual addition of a column.
    void doAddColumn (const ColumnDesc& columnDesc, DataManager* dataManPtr);

    // Check if the table is locked for read or write.
    // If manual or permanent locking is in effect, it checks if the
    // table is properly locked.
    // If autolocking is in effect, it locks the table when needed.
    void doLock (FileLocker::LockType, Bool wait);


    //# Declare the variables.
    TableDesc*                      tdescPtr_p;
    uInt                            nrrow_p;        //# #rows
    PlainTable*                     plainTablePtr_p;
    TableLockData*                  lockPtr_p;      //# lock object
    SimpleOrderedMap<String,void*>  colMap_p;       //# list of PlainColumns
    uInt                            seqCount_p;     //# sequence number count
    //#                                                 (used for unique seqnr)
    Block<void*>                    blockDataMan_p; //# list of data managers
    Block<Bool>                     dataManChanged_p; //# data has changed
};



inline uInt ColumnSet::nrow() const
{
    return nrrow_p;
}
inline void ColumnSet::linkToLockObject (PlainTable* plainTableObject,
					 TableLockData* lockObject)
{
    plainTablePtr_p = plainTableObject;
    lockPtr_p = lockObject;
}
inline void ColumnSet::checkLock (FileLocker::LockType type, Bool wait)
{
    if (! lockPtr_p->hasLock (type)) {
	doLock (type, wait);
    }
}
inline void ColumnSet::autoReleaseLock()
{
    lockPtr_p->autoRelease();
}
inline Block<Bool>& ColumnSet::dataManChanged()
{
    return dataManChanged_p;
}



#endif
