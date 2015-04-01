//# PlainTable.cc: Class defining a regular table
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

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableLockData.h>
#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/TableTrace.h>
#include <casacore/tables/Tables/PlainColumn.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/System/AipsrcValue.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Initialize the static TableCache object.
TableCache PlainTable::theirTableCache;


PlainTable::PlainTable (SetupNewTable& newtab, uInt nrrow, Bool initialize,
			const TableLock& lockOptions, int endianFormat,
                        const TSMOption& tsmOption)
: BaseTable      (newtab.name(), newtab.option(), 0),
  colSetPtr_p    (0),
  tableChanged_p (True),
  addToCache_p   (True),
  lockPtr_p      (0),
  tsmOption_p    (tsmOption)
{
  try {
    // Determine and set the endian option.
    setEndian (endianFormat);
    // Replace default TSM option for new table.
    tsmOption_p.fillOption (True);
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
    if (tableCache()(name_p) != 0) {
        // OK it's in the cache but is it really there?
        if(File(name_p).exists()){
	   throw (TableInvOper ("SetupNewTable " + name_p +
			     " is already opened (is in the table cache)"));
        } else {
          tableCache().remove (name_p);
        }
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
    newtab.columnSetPtr()->checkDataManagerNames (name_p);
    //# Get the data from the SetupNewTable object.
    //# Set SetupNewTable object to in use.
    tdescPtr_p  = newtab.tableDescPtr();
    colSetPtr_p = newtab.columnSetPtr();
    colSetPtr_p->linkToTable (this);
    newtab.setInUse();
    //# Create the table directory (and possibly delete existing files)
    //# as needed.
    makeTableDir();
    //# Create the lock object.
    //# When needed, it sets a permanent write lock.
    //# Acquire a write lock.
    lockPtr_p = new TableLockData (lockOptions, releaseCallBack, this);
    lockPtr_p->makeLock (name_p, True, FileLocker::Write);
    lockPtr_p->acquire (0, FileLocker::Write, 1);
    colSetPtr_p->linkToLockObject (lockPtr_p);
    //# Initialize the data managers.
    Table tab(this, False);
    nrrowToAdd_p = nrrow;
    colSetPtr_p->initDataManagers (nrrow, bigEndian_p, tsmOption_p, tab);
    //# Initialize the columns if needed.
    if (initialize  &&  nrrow > 0) {
	colSetPtr_p->initialize (0, nrrow-1);
    }
    //# Nrrow_p has to be set here, otherwise data managers may use the
    //# incorrect number of rows (similar behaviour as in function addRow).
    nrrowToAdd_p = 0;
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
    tableCache().define (name_p, this);
    //# Trace if needed.
    itsTraceId = TableTrace::traceTable (name_p, 'n');
  } catch (AipsError&) {
    delete lockPtr_p;
    lockPtr_p = 0;
    delete colSetPtr_p;
    colSetPtr_p = 0;
    throw;
  }
}


PlainTable::PlainTable (AipsIO&, uInt version, const String& tabname,
			const String& type, uInt nrrow, int opt,
			const TableLock& lockOptions,
                        const TSMOption& tsmOption,
			Bool addToCache, uInt locknr)
: BaseTable      (tabname, opt, nrrow),
  colSetPtr_p    (0),
  tableChanged_p (False),
  addToCache_p   (addToCache),
  lockPtr_p      (0),
  tsmOption_p    (tsmOption)
{
    // Replace default TSM option for existing table.
    tsmOption_p.fillOption (False);
    //# Set initially to no write in destructor.
    //# At the end it is reset. In this way nothing is written if
    //# an exception is thrown during initialization.
    noWrite_p = True;
    //# Create the lock object.
    //# When needed, it sets a permanent (read or write) lock.
    //# Otherwise acquire a read lock (when needed) to read in the table
    //# or get the sync info.
    lockPtr_p = new TableLockData (lockOptions, releaseCallBack, this);
    lockPtr_p->makeLock (name_p, False,
		   opt == Table::Old  ?  FileLocker::Read : FileLocker::Write,
			 locknr);
    if (lockPtr_p->readLocking()) {
        lockPtr_p->acquire (&(lockSync_p.memoryIO()), FileLocker::Read, 0);
    } else {
        lockPtr_p->getInfo (lockSync_p.memoryIO());
    }
    uInt ncolumn;
    Bool tableChanged;
    Block<Bool> dmChanged;
    lockSync_p.read (nrrow_p, ncolumn, tableChanged, dmChanged);
    tdescPtr_p = new TableDesc ("", TableDesc::Scratch);

    //# Reopen the file to be sure that the internal stdio buffer is not reused.
    //# This is a terrible hack, but it works.
    //# However, One time a better solution is needed.
    //# Probably stdio should not be used, but class RegularFileIO or
    //# FilebufIO should do its own buffering and have a sync function.
    AipsIO ios (Table::fileName(tabname), ByteIO::Old);
    String tp;
    version = ios.getstart ("Table");
    uInt format;
    ios >> nrrow;
    ios >> format;
    bigEndian_p = (format==0);
    ios >> tp;
#if defined(TABLEREPAIR)
    cerr << "tableRepair: found " << nrrow << " rows; give new number: ";
    cin >> nrrow_p;
    if (nrrow != nrrow_p) {
      cerr << "Number of rows set to " << nrrow_p << endl;
      tableChanged_p = True;
    }
#endif

    TableAttr attr (tableName(), isWritable(), lockOptions);
    tdescPtr_p->getFile (ios, attr);            // read description
    // Check if the given table type matches the type in the file.
    if ((! type.empty())  &&  type != tdescPtr_p->getType()) {
        throw (TableInvType (tableName(), type, tdescPtr_p->getType()));
	return;
    }
    // In the older Table files the keyword set was written separately
    // and was not part of the TableDesc.
    // So read it for those and merge it into the TableDesc keywords.
    // Merging is done after attaching the lock to the ColumnSet,
    // because function keywordSet() uses the lock.
    TableRecord tmp;
    if (version == 1) {
        tmp.getRecord (ios, attr);
    }
    //# Construct and read the ColumnSet object.
    //# This will also construct the various DataManager objects.
    colSetPtr_p = new ColumnSet (tdescPtr_p);
    colSetPtr_p->linkToTable (this);
    colSetPtr_p->linkToLockObject (lockPtr_p);
    if (version == 1) {
	keywordSet().merge (tmp, RecordInterface::OverwriteDuplicates);
    }
    //# Create a Table object to be used internally by the data managers.
    //# Do not count it, otherwise a mutual dependency exists.
    Table tab(this, False);
    nrrow_p = colSetPtr_p->getFile (ios, tab, nrrow_p, bigEndian_p,
                                    tsmOption_p);
    //# Read the TableInfo object.
    getTableInfo();
    //# Release the read lock if UserLocking is used.
    if (lockPtr_p->option() == TableLock::UserLocking) {
	lockPtr_p->release();
    }
    //# The destructor can (in principle) write.
    noWrite_p = False;
    //# Add it to the table cache.
    if (addToCache) {
      tableCache().define (name_p, this);
    }
    //# Trace if needed.
    itsTraceId = TableTrace::traceTable (name_p, 'o');
}


PlainTable::~PlainTable()
{
  // If destructed during an exception, catch possible other exceptions to
  // avoid termination.
  if (std::uncaught_exception() ) {
    try {
      closeObject();
    } catch (std::exception& x) {
      try {
        cerr << "Exception in ~PlainTable during exception unwind:" << endl
             << "  " << x.what() << endl;
      } catch (...) {
      }
    }
  } else {
    closeObject();
  }
}

void PlainTable::closeObject()
{
    //# When needed, write and sync the table files if not marked for delete
    if (!isMarkedForDelete()) {
	if (openedForWrite()  &&  !shouldNotWrite()) {
	    lockPtr_p->release (True);
	}
    }else{
	//# Check if table can indeed be deleted.
	//# If not, set delete flag to False.
        //# It only checks if the main table is multi-used.
	if (isMultiUsed(False)) {
	    unmarkForDelete (False, "");
	    throw (TableError ("Table " + name_p + " cannot be deleted;"
			       " the table or a subtable is still used"
			       " in another process"));
	}
    }
    //# Remove it from the table cache (if added).
    if (addToCache_p) {
      tableCache().remove (name_p);
    }
    //# Trace if needed.
    TableTrace::traceClose (name_p);
    //# Delete everything.
    delete colSetPtr_p;
    delete lockPtr_p;
}

//# Read description and #rows.
void PlainTable::getLayout (TableDesc& desc, AipsIO& ios)
{
    desc.getFile (ios, TableAttr());                     // read description
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
    // Set table to opened for read/write.
    // Do this before reopening subtables, because that might cause
    // recursion (e.g. SORTED_TABLE in the MS).
    option_p = Table::Update;
    // Reopen the storage managers and the subtables in all keyword sets.
    colSetPtr_p->reopenRW();
    keywordSet().reopenRW();
    TableTrace::traceFile (itsTraceId, "reopenrw");
}

void PlainTable::renameSubTables (const String& newName,
				  const String& oldName)
{
    rwKeywordSet().renameTables (newName, oldName);
    colSetPtr_p->renameTables (newName, oldName);
}

Bool PlainTable::asBigEndian() const
{
    return bigEndian_p;
}

const StorageOption& PlainTable::storageOption() const
{
    return colSetPtr_p->storageOption();
}

Bool PlainTable::isMultiUsed (Bool checkSubTables) const
{
    if (lockPtr_p->isMultiUsed()) {
        return True;
    }
    if (checkSubTables) {
        if (const_cast<PlainTable*>(this)->
                                    keywordSet().areTablesMultiUsed()) {
	    return True;
	}
	return colSetPtr_p->areTablesMultiUsed();
    }
    return False;
}

const TableLock& PlainTable::lockOptions() const
{
    return *lockPtr_p;
}
void PlainTable::mergeLock (const TableLock& lockOptions)
{
    Bool isPerm = lockPtr_p->isPermanent();
    lockPtr_p->merge (lockOptions);
    // Acquire if needed a permanent lock.
    if (lockPtr_p->isPermanent()  &&  !isPerm) {
        lockPtr_p->makeLock (name_p, False,
			isWritable()  ?  FileLocker::Write : FileLocker::Read);
    }
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
    //# Acquire the required lock.
    //# Synchronize the table when it has changed.
    //# When table data has changed, a temporary PlainTable object is created
    //# to get the new keyword values, etc.. Deleting it causes all locks
    //# held by this process on the table to be released. So reacquire
    //# them when that happens.
    Bool tableChanged = True;
    while (tableChanged) {
	tableChanged = False;
	if (! lockPtr_p->acquire (&(lockSync_p.memoryIO()), type, nattempts)) {
	    return False;
	}
	if (!noSync) {
	    // Older readonly table files may have empty locksync data.
	    // Skip the sync-ing in that case.
	    uInt ncolumn;
            uInt nrrow;
	    if (! lockSync_p.read (nrrow, ncolumn, tableChanged,
				   colSetPtr_p->dataManChanged())) {
		tableChanged = False;
	    } else {
		if (ncolumn != tableDesc().ncolumn()) {
		    throw (TableError ("Table::lock cannot sync table "
                                       + tableName() + "; another process "
                                       "changed the number of columns"));
		}
		nrrow_p = colSetPtr_p->resync (nrrow, False);
		if (tableChanged  &&  ncolumn > 0) {
		    syncTable();
		}
	    }
	}
    }
    return True;
}

void PlainTable::syncTable()
{
    // Something changed in the table file itself.
    // Reread it into a PlainTable object (don't add it to the cache).
    // Use a different locknr for it to preserve possible existing locks.
    BaseTable* btab = Table::makeBaseTable
                         (tableName(), "", Table::Old,
			  TableLock(TableLock::PermanentLocking),
			  TSMOption(TSMOption::Buffer,0,0), False, 1);
    PlainTable* tab = (PlainTable*)btab;
    TableAttr defaultAttr (tableName(), isWritable(), lockOptions());
    // Now check if all columns are the same.
    // Update the column keywords.
    colSetPtr_p->syncColumns (*tab->colSetPtr_p, defaultAttr);
    // Adjust the attributes of subtables.
    // Update the table keywords.
    TableRecord& oldKeySet = keywordSet();
    TableRecord& newKeySet = tab->keywordSet();
    newKeySet.setTableAttr (oldKeySet, defaultAttr);
    oldKeySet = newKeySet;
    delete tab;
}


void PlainTable::unlock()
{
    lockPtr_p->release();
}

void PlainTable::autoReleaseLock (Bool always)
{
    lockPtr_p->autoRelease (always);
}

void PlainTable::setTableChanged()
{
    tableChanged_p = True;
}

uInt PlainTable::getModifyCounter() const
{
    return lockSync_p.getModifyCounter();
}


void PlainTable::flush (Bool fsync, Bool recursive)
{
    if (openedForWrite()) {
	putFile (False);
	// Flush subtables if wanted.
	if (recursive) {
	    keywordSet().flushTables (fsync);
	}
    }
}

void PlainTable::resync()
{
    TableTrace::traceFile (itsTraceId, "resync");
    Bool tableChanged = True;
    lockPtr_p->getInfo (lockSync_p.memoryIO());
    // Older readonly table files may have empty locksync data.
    // Skip the sync-ing in that case.
    uInt ncolumn;
    uInt nrrow;
    if (! lockSync_p.read (nrrow, ncolumn, tableChanged,
			   colSetPtr_p->dataManChanged())) {
        tableChanged = False;
    } else {
        if (ncolumn != tableDesc().ncolumn()) {
            throw (TableError ("Table::resync cannot sync table " +
                               tableName() + "; another process "
			       "changed the number of columns"));
	}
	nrrow_p = colSetPtr_p->resync (nrrow, True);
	if (tableChanged  &&  ncolumn > 0) {
	    syncTable();
	}
    }
}


Bool PlainTable::putFile (Bool always)
{
    TableTrace::traceFile (itsTraceId, "flush");
    Bool writeTab = always || tableChanged_p;
    Bool written = writeTab;
    {  // use scope to ensure AipsIO is closed (thus flushed) before lockfile
      AipsIO ios;
      TableAttr attr(tableName());
      if (writeTab) {
#ifdef AIPS_TRACE
        cout << "  full PlainTable::putFile" << endl;
#endif
	writeStart (ios, bigEndian_p);
	ios << "PlainTable";
	tdescPtr_p->putFile (ios, attr);                 // write description
	colSetPtr_p->putFile (True, ios, attr, False);   // write column data
	writeEnd (ios);
	//# Write the TableInfo.
	flushTableInfo();
      } else {
        //# Tell the data managers to write their data only.
        if (colSetPtr_p->putFile (False, ios, attr, False)) {
	    written = True;
#ifdef AIPS_TRACE
	    cout << "  data PlainTable::putFile on " << tableName() << endl;
#endif
	}
      }
    }
    // Write the change info if anything has been written.
    if (written) {
        lockSync_p.write (nrrow_p, tdescPtr_p->ncolumn(), tableChanged_p,
			  colSetPtr_p->dataManChanged());
	lockPtr_p->putInfo (lockSync_p.memoryIO());
    }
    // Clear the change-flags for the next round.
    tableChanged_p = False;
    colSetPtr_p->dataManChanged() = False;
    return writeTab;
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
    putFile (always);
    return 0;
}


//# Test if the table is writable.
Bool PlainTable::isWritable() const
{
    if (option_p == Table::Old  ||  option_p == Table::Delete) {
	return False;
    }
    return True;
}


// Get the actual table description.
TableDesc PlainTable::actualTableDesc() const
{
  return colSetPtr_p->actualTableDesc();
}

// Get the data manager info.
Record PlainTable::dataManagerInfo() const
{
  return colSetPtr_p->dataManagerInfo();
}


//# Get access to the keyword set.
TableRecord& PlainTable::keywordSet()
{
    Bool hasLocked = colSetPtr_p->userLock (FileLocker::Read, True);
    colSetPtr_p->checkReadLock (True);
    TableRecord& rec = tdescPtr_p->rwKeywordSet();
    colSetPtr_p->userUnlock (hasLocked);
    return rec;
}
TableRecord& PlainTable::rwKeywordSet()
{
    colSetPtr_p->checkWriteLock (True);
    TableRecord& rec = tdescPtr_p->rwKeywordSet();
    tableChanged_p = True;
    return rec;
}
    
    

//# Get a column object.
BaseColumn* PlainTable::getColumn (uInt columnIndex) const
    { return colSetPtr_p->getColumn (columnIndex); }
BaseColumn* PlainTable::getColumn (const String& columnName) const
    { return colSetPtr_p->getColumn (columnName); }


//# The data managers have to be inspected to tell if adding and removing
//# of rows and columns is possible.
Bool PlainTable::canAddRow() const
    { return colSetPtr_p->canAddRow(); }
Bool PlainTable::canRemoveRow() const
    { return colSetPtr_p->canRemoveRow(); }
Bool PlainTable::canRemoveColumn (const Vector<String>& columnNames) const
{
    if (!checkRemoveColumn (columnNames, False)) {
        return False;
    }
    return colSetPtr_p->canRemoveColumn (columnNames);
}

//# Renaming a column is possible.
Bool PlainTable::canRenameColumn (const String& columnName) const
    { return colSetPtr_p->canRenameColumn (columnName); }


//# Add rows.
void PlainTable::addRow (uInt nrrw, Bool initialize)
{
    if (nrrw > 0) {
        checkWritable("addRow");
	//# Locking has to be done here, otherwise nrrow_p is not up-to-date
	//# when autoReleaseLock releases the lock and writes the data.
	nrrowToAdd_p = nrrw;
	colSetPtr_p->checkWriteLock (True);
	colSetPtr_p->addRow (nrrw);
	if (initialize) {
	    colSetPtr_p->initialize (nrrow_p, nrrow_p+nrrw-1);
	}
	nrrowToAdd_p = 0;
	nrrow_p += nrrw;
	colSetPtr_p->autoReleaseLock();
    }
}

void PlainTable::removeRow (uInt rownr)
{
    checkWritable("rowmoveRow");
    //# Locking has to be done here, otherwise nrrow_p is not up-to-date
    //# when autoReleaseLock releases the lock and writes the data.
    colSetPtr_p->checkWriteLock (True);
    colSetPtr_p->removeRow (rownr);
    nrrow_p--;
    colSetPtr_p->autoReleaseLock();
}

void PlainTable::addColumn (const ColumnDesc& columnDesc, Bool)
{
    checkWritable("addColumn");
    Table tab(this, False);
    colSetPtr_p->addColumn (columnDesc, bigEndian_p, tsmOption_p, tab);
    tableChanged_p = True;
}
void PlainTable::addColumn (const ColumnDesc& columnDesc,
			    const String& dataManager, Bool byName, Bool)
{
    checkWritable("addColumn");
    Table tab(this, False);
    colSetPtr_p->addColumn (columnDesc, dataManager, byName, bigEndian_p,
                            tsmOption_p, tab);
    tableChanged_p = True;
}
void PlainTable::addColumn (const ColumnDesc& columnDesc,
			    const DataManager& dataManager, Bool)
{
    checkWritable("addColumn");
    Table tab(this, False);
    colSetPtr_p->addColumn (columnDesc, dataManager, bigEndian_p,
                            tsmOption_p, tab);
    tableChanged_p = True;
}
void PlainTable::addColumn (const TableDesc& tableDesc,
			    const DataManager& dataManager, Bool)
{
    checkWritable("addColumn");
    Table tab(this, False);
    colSetPtr_p->addColumn (tableDesc, dataManager, bigEndian_p,
                            tsmOption_p, tab);
    tableChanged_p = True;
}

void PlainTable::removeColumn (const Vector<String>& columnNames)
{
    checkWritable("removeColumn");
    colSetPtr_p->removeColumn (columnNames);
    tableChanged_p = True;
}

void PlainTable::renameColumn (const String& newName, const String& oldName)
{
    checkWritable("renameColumn");
    colSetPtr_p->renameColumn (newName, oldName);
    tableChanged_p = True;
}

void PlainTable::renameHypercolumn (const String& newName, const String& oldName)
{
    checkWritable("renameHyperColumn");
    tdescPtr_p->renameHypercolumn (newName, oldName);
    tableChanged_p = True;
}


DataManager* PlainTable::findDataManager (const String& name,
                                          Bool byColumn) const
{
  return colSetPtr_p->findDataManager (name, byColumn);
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


void PlainTable::setEndian (int endianFormat)
{
    int endOpt = endianFormat;
    if (endOpt == Table::AipsrcEndian) {
        String opt;
	// Default "big" was used until version 10.1203.00.
	////AipsrcValue<String>::find (opt, "table.endianformat", "big");
	AipsrcValue<String>::find (opt, "table.endianformat", "local");
	opt.downcase();
	if (opt == "big") {
	    endOpt = Table::BigEndian;
	} else if (opt == "little") {
	    endOpt = Table::LittleEndian;
	} else {
	    endOpt = Table::LocalEndian;
	}
    }
    if (endOpt == Table::LocalEndian) {
        bigEndian_p = HostInfo::bigEndian();
    } else {
        bigEndian_p = True;
	if (endOpt == Table::LittleEndian) {
	    bigEndian_p = False;
	}
    }
}

void PlainTable::checkWritable (const char* func) const
{
    if (! isWritable()) {
        throw (TableInvOper ("Table::" + String(func) + "; table "
                             + tableName() + " is not writable"));
    }
}

} //# NAMESPACE CASACORE - END
