//# PlainTable.cc: Class defining a regular table
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

#include <aips/aips.h>
#include <aips/Tables/PlainTable.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableLockData.h>
#include <aips/Tables/ColumnSet.h>
#include <aips/Tables/PlainColumn.h>
#include <aips/Tables/TableError.h>
#include <aips/Containers/Block.h>
#include <aips/Utilities/String.h>


//# Initialize the static TableCache object.
TableCache PlainTable::tableCache = TableCache();


PlainTable::PlainTable (SetupNewTable& newtab, uInt nrrow, Bool initialize,
			const TableLock& lockOptions)
: BaseTable      (newtab.name(), newtab.option(), 0),
  colSetPtr_p    (0),
  tableChanged_p (True),
  lockPtr_p      (0)
{
    // Set initially to no write in destructor.
    // At the end it is reset. In this way nothing is written if
    // an exception is thrown during initialization.
    noWrite_p  = True;
    //# Check if another Table was already constructed using this
    //# SetupNewTable (which is invalid).
    if (newtab.isUsed()) {
	throw (TableInvOper
	             ("SetupNewTable object already used for another Table"));
    }
    //# Check if a table with this name is not in the table cache.
    if (tableCache(name_p) != 0) {
	throw (TableInvOper ("SetupNewTable " + name_p +
			     " is already opened (is in the table cache)"));
    }
    //# If the table already exists, exit if it is in use.
    if (Table::isReadable (name_p)) {
	TableLockData tlock (TableLock (TableLock::UserLocking, 0));
	tlock.makeLock (name_p, False, FileLocker::Write);
	if (tlock.isMultiUsed()) {
	    throw (TableError ("Table " + name_p + " cannot be created; "
			       "it is in use in another process"));
	}
    }
    //# Create the data managers for unbound columns.
    //# Check if there are no data managers with equal names.
    newtab.handleUnbound();
    newtab.columnSetPtr()->checkDataManagerNames();
    //# Get the data from the SetupNewTable object.
    tdescPtr_p   = newtab.tableDescPtr();
    colSetPtr_p  = newtab.columnSetPtr();
    //# Create the table directory (and possibly delete existing files)
    //# as needed.
    makeTableDir();
    //# Create the lock object.
    //# When needed, it sets a permanent write lock.
    //# Acquire a write lock.
    lockPtr_p = new TableLockData (lockOptions, releaseCallBack, this);
    lockPtr_p->makeLock (name_p, True, FileLocker::Write);
    lockPtr_p->acquire (0, FileLocker::Write, 1);
    colSetPtr_p->linkToLockObject (this, lockPtr_p);
    //# Initialize the data managers.
    //# Set SetupNewTable object to in use.
    Table tab(this, False);
    colSetPtr_p->initDataManagers (nrrow, tab);
    newtab.setInUse();
    //# Initialize the columns if needed.
    if (initialize) {
	colSetPtr_p->initialize (0, nrrow-1);
    }
    //# Nrrow_p has to be set here, otherwise data managers may use the
    //# incorrect number of rows (similar behaviour as in function addRow).
    nrrow_p = nrrow;
    //# Release the write lock if UserLocking is used.
    if (lockPtr_p->option() == TableLock::UserLocking) {
	lockPtr_p->release();
    }
    //# Unmark for delete when needed.
    if (! newtab.isMarkedForDelete()) {
	unmarkForDelete (True, "");
    }
    //# The destructor can (in principle) write.
    noWrite_p = False;
    //# Add it to the table cache.
    tableCache.define (name_p, this);
}


PlainTable::PlainTable (AipsIO& ios, uInt version, const String& tabname,
			const String& type, uInt nrrow, int opt,
			const TableLock& lockOptions)
: BaseTable      (tabname, opt, nrrow),
  colSetPtr_p    (0),
  tableChanged_p (False),
  lockPtr_p      (0)
{
    //# Set initially to no write in destructor.
    //# At the end it is reset. In this way nothing is written if
    //# an exception is thrown during initialization.
    noWrite_p = True;
    //# Create the lock object.
    //# When needed, it sets a permanent (read or write) lock.
    //# Otherwise acquire a read lock to read in the table.
    lockPtr_p = new TableLockData (lockOptions, releaseCallBack, this);
    lockPtr_p->makeLock (name_p, False,
		   opt == Table::Old  ?  FileLocker::Read : FileLocker::Write);
    lockPtr_p->acquire (&(lockSync_p.memoryIO()), FileLocker::Read, 0);
    uInt ncolumn;
    Bool tableChanged;
    Block<Bool> dmChanged;
    lockSync_p.read (nrrow_p, ncolumn, tableChanged, dmChanged);
    tdescPtr_p = new TableDesc ("", TableDesc::Scratch);
    if (tdescPtr_p == 0) {
	throw (AllocError ("PlainTable::PlainTable",1));
    }
    tdescPtr_p->getFile (ios, isWritable(), tableName());  // read description
    // Check if the given table type matches the type in the file.
    if ((! type.empty())  &&  type != tdescPtr_p->getType()) {
	throw (TableInvType (type, tdescPtr_p->getType()));
	return;
    }
    // In the older Table files the keyword set was written separately
    // and was not part of the TableDesc.
    // So read it for those and merge it into the TableDesc keywords.
    if (version == 1) {
	TableRecord tmp;
	tmp.getRecord (ios, isWritable(), tableName());
	keywordSet().merge (tmp, RecordInterface::OverwriteDuplicates);
    }
    //# Construct and read the ColumnSet object.
    //# This will also construct the various DataManager objects.
    colSetPtr_p = new ColumnSet (tdescPtr_p);
    if (colSetPtr_p == 0) {
	throw (AllocError ("PlainTable(AipsIO&)", 1));
    }
    colSetPtr_p->linkToLockObject (this, lockPtr_p);
    //# Create a Table object to be used internally by the data managers.
    //# Do not count it, otherwise a mutual dependency exists.
    Table tab(this, False);
    colSetPtr_p->getFile (ios, tab, nrrow_p);
    //# Read the TableInfo object.
    getTableInfo();
    //# Release the read lock if UserLocking is used.
    if (lockPtr_p->option() == TableLock::UserLocking) {
	lockPtr_p->release();
    }
    //# The destructor can (in principle) write.
    noWrite_p = False;
    //# Add it to the table cache.
    tableCache.define (name_p, this);
}


PlainTable::~PlainTable()
{
    //# When needed, write and sync the table files if not marked for delete
    if (!isMarkedForDelete()) {
	if (openedForWrite()  &&  !shouldNotWrite()) {
	    lockPtr_p->release (True);
	}
    }else{
	//# Check if table can indeed be deleted.
	//# If not, set delete flag to False.
	if (isMultiUsed()) {
	    unmarkForDelete (False, "");
	    throw (TableError ("Table " + name_p + " cannot be deleted;"
			       " it is still used in another process"));
	}
    }
    //# Remove it from the table cache.
    tableCache.remove (name_p);
    //# Delete everything.
    delete colSetPtr_p;
    delete lockPtr_p;
}

//# Read description and #rows.
void PlainTable::getLayout (TableDesc& desc, AipsIO& ios)
{
    desc.getFile (ios, False, "");                     // read description
}

void PlainTable::reopenRW()
{
    // Exit if already open for write.
    if (isWritable()) {
	return;
    }
    // Exception when readonly table.
    if (! Table::isWritable (tableName())) {
	throw (TableError ("Table " + tableName() +
			   " cannot be opened for read/write"));
    }
    // When a permanent lock is in use, turn it into a write lock.
    lockPtr_p->makeLock (name_p, False, FileLocker::Write);
    // Reopen the storage managers and the subtables in all keyword sets.
    colSetPtr_p->reopenRW();
    keywordSet().reopenRW();
    option_p = Table::Update;
}

void PlainTable::renameSubTables (const String& newName,
				  const String& oldName)
{
    rwKeywordSet().renameTables (newName, oldName);
    colSetPtr_p->renameTables (newName, oldName);
}

Bool PlainTable::isMultiUsed() const
{
    return lockPtr_p->isMultiUsed();
}

const TableLock& PlainTable::lockOptions() const
{
    return *lockPtr_p;
}
void PlainTable::mergeLock (const TableLock& lockOptions)
{
    lockPtr_p->merge (lockOptions);
}
Bool PlainTable::hasLock (FileLocker::LockType type) const
{
    return lockPtr_p->hasLock (type);
}
Bool PlainTable::lock (FileLocker::LockType type, uInt nattempts)
{
    //# When the table is already locked (read locked is sufficient),
    //# no synchronization is needed (other processes could not write).
    Bool noSync = hasLock (FileLocker::Read);
    if (! lockPtr_p->acquire (&(lockSync_p.memoryIO()), type, nattempts)) {
	return False;
    }
    if (!noSync) {
	uInt ncolumn;
	Bool tableChanged;
	lockSync_p.read (nrrow_p, ncolumn, tableChanged,
			 colSetPtr_p->dataManChanged());
	colSetPtr_p->resync (nrrow_p);
    }
    return True;
}
void PlainTable::unlock()
{
    lockPtr_p->release();
}

void PlainTable::autoReleaseLock()
{
    lockPtr_p->autoRelease();
}

uInt PlainTable::getModifyCounter() const
{
    return lockSync_p.getModifyCounter();
}


void PlainTable::flush (Bool)
{
    if (openedForWrite()) {
	putFile (False);
    }
}


Bool PlainTable::putFile (Bool always)
{
    AipsIO ios;
    if (always  ||  tableChanged_p) {
	writeStart (ios);
	ios << "PlainTable";
	tdescPtr_p->putFile (ios, tableName());         // write description
	colSetPtr_p->putFile (True, ios, tableName(),
			      False);                   // write column data
	writeEnd (ios);
	//# Write the TableInfo.
	flushTableInfo();
	tableChanged_p = False;
	return True;
    }
    //# Only tell the data managers to write their data.
    colSetPtr_p->putFile (False, ios, tableName(), False);
    return False;
}

MemoryIO* PlainTable::releaseCallBack (void* plainTableObject, Bool always)
{
    return (*(PlainTable*)plainTableObject).doReleaseCallBack (always);
}
MemoryIO* PlainTable::doReleaseCallBack (Bool always)
{
    //# Invalidate the caches in the columns to be sure
    //# that the next get on a column reacquires a lock.
    colSetPtr_p->invalidateColumnCaches();
    //# Data does not need to be written when not opened for write.
    if (!openedForWrite()) {
	return 0;
    }
    Bool tableWritten = putFile (always);
    lockSync_p.write (nrrow_p, tdescPtr_p->ncolumn(), tableWritten,
		      colSetPtr_p->dataManChanged());
    return &(lockSync_p.memoryIO());
}


//# Test if the table is writable.
Bool PlainTable::isWritable() const
{
    if (option_p == Table::Old  ||  option_p == Table::Delete) {
	return False;
    }
    return True;
}


//# Get access to the keyword set.
TableRecord& PlainTable::keywordSet()
    { return tdescPtr_p->rwKeywordSet(); }

TableRecord& PlainTable::rwKeywordSet()
{
    colSetPtr_p->checkLock (FileLocker::Write, True);
    tableChanged_p = True;
    return tdescPtr_p->rwKeywordSet();
}
    
    

//# Get a column object.
BaseColumn* PlainTable::getColumn (uInt columnIndex) const
    { return colSetPtr_p->getColumn (columnIndex); }
BaseColumn* PlainTable::getColumn (const String& columnName) const
    { return colSetPtr_p->getColumn (columnName); }


//# The data managers have to be inspected to tell if adding, etc.
//# of rows and columns is possible.
Bool PlainTable::canAddRow() const
    { return colSetPtr_p->canAddRow(); }
Bool PlainTable::canRemoveRow() const
    { return colSetPtr_p->canRemoveRow(); }
Bool PlainTable::canRemoveColumn (const String& columnName) const
    { return colSetPtr_p->canRemoveColumn (columnName); }


//# Add rows.
void PlainTable::addRow (uInt nrrw, Bool initialize)
{
    if (! isWritable()) {
	throw (TableInvOper ("Table::addRow; table is not writable"));
    }
    //# Locking has to be done here, otherwise nrrow_p is not up-to-date
    //# when autoReleaseLock releases the lock and writes the data.
    colSetPtr_p->checkLock (FileLocker::Write, True);
    colSetPtr_p->addRow (nrrw);
    if (initialize) {
	colSetPtr_p->initialize (nrrow_p, nrrow_p+nrrw-1);
    }
    nrrow_p += nrrw;
    colSetPtr_p->autoReleaseLock();
}

void PlainTable::removeRow (uInt rownr)
{
    if (! isWritable()) {
	throw (TableInvOper ("Table::removeRow; table is not writable"));
    }
    //# Locking has to be done here, otherwise nrrow_p is not up-to-date
    //# when autoReleaseLock releases the lock and writes the data.
    colSetPtr_p->checkLock (FileLocker::Write, True);
    colSetPtr_p->removeRow (rownr);
    nrrow_p--;
    colSetPtr_p->autoReleaseLock();
}

void PlainTable::addColumn (const ColumnDesc& columnDesc)
{
    if (! isWritable()) {
	throw (TableInvOper ("Table::addColumn; table is not writable"));
    }
    Table tab(this, False);
    colSetPtr_p->addColumn (columnDesc, tab);
    tableChanged_p = True;
}
void PlainTable::addColumn (const ColumnDesc& columnDesc,
			    const String& dataManager, Bool byName)
{
    if (! isWritable()) {
	throw (TableInvOper ("Table::addColumn; table is not writable"));
    }
    Table tab(this, False);
    colSetPtr_p->addColumn (columnDesc, dataManager, byName, tab);
    tableChanged_p = True;
}
void PlainTable::addColumn (const ColumnDesc& columnDesc,
			    const DataManager& dataManager)
{
    if (! isWritable()) {
	throw (TableInvOper ("Table::addColumn; table is not writable"));
    }
    Table tab(this, False);
    colSetPtr_p->addColumn (columnDesc, dataManager, tab);
    tableChanged_p = True;
}
void PlainTable::addColumn (const TableDesc& tableDesc,
			    const DataManager& dataManager)
{
    if (! isWritable()) {
	throw (TableInvOper ("Table::addColumn; table is not writable"));
    }
    Table tab(this, False);
    colSetPtr_p->addColumn (tableDesc, dataManager, tab);
    tableChanged_p = True;
}

void PlainTable::removeColumn (const String&)
{
    if (! isWritable()) {
	throw (TableInvOper ("Table::removeColumn; table is not writable"));
    }
    throw (TableInvOper ("PlainTable::removeColumn not implemented yet"));
}


DataManager* PlainTable::findDataManager (const String& dataManagerName) const
{
    return colSetPtr_p->findDataManager (dataManagerName);
}


ByteIO::OpenOption PlainTable::toAipsIOFoption (int tabOpt)
{
    switch (tabOpt) {
    case Table::Old:
    case Table::Delete:
	return ByteIO::Old;
    case Table::Update:
	return ByteIO::Update;
    case Table::New:
	return ByteIO::New;
    case Table::NewNoReplace:
	return ByteIO::NewNoReplace;
    case Table::Scratch:
	return ByteIO::Scratch;
    }
    //# This statement is only there to satisfy strict compilers.
    return ByteIO::Scratch;
}
