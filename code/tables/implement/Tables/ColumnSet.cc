//# ColumnSet.cc: Class to manage a set of table columns
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

#include <aips/Tables/ColumnSet.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/PlainColumn.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/PlainTable.h>
#include <aips/Tables/DataManager.h>
#include <aips/Tables/TableError.h>
#include <aips/IO/MemoryIO.h>
#include <aips/Utilities/Assert.h>

#define BLOCKDATAMANVAL(I) ((DataManager*)(blockDataMan_p[I]))
#define COLMAPVAL(I)       ((PlainColumn*)(colMap_p.getVal(I)))
#define COLMAPNAME(NAME)   ((PlainColumn*)(colMap_p(NAME)))

ColumnSet::ColumnSet (TableDesc* tdesc)
: tdescPtr_p      (tdesc),
  plainTablePtr_p (0),
  lockPtr_p       (0),
  colMap_p        ((void *)0, tdesc->ncolumn()),
  seqCount_p      (0),
  blockDataMan_p  (0)
{
    //# Loop through all columns in the description and create
    //# a column out of them.
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	const ColumnDesc& cd = tdescPtr_p->columnDesc(i);
	colMap_p.define (cd.name(), cd.makeColumn(this));
    }
}


ColumnSet::~ColumnSet()
{
    uInt i;
    for (i=0; i<colMap_p.ndefined(); i++) {
	delete COLMAPVAL(i);
    }
    for (i=0; i<blockDataMan_p.nelements(); i++) {
	delete BLOCKDATAMANVAL(i);
    }
}


PlainColumn* ColumnSet::getColumn (const String& columnName) const
{
    tdescPtr_p->columnDesc(columnName);             // check if column exists
    return COLMAPNAME(columnName);
}

//# We cannot simply return COLMAPVAL(columnIndex), because the order of
//# the columns in the description is important. So first get the column
//# name and use that as key.
PlainColumn* ColumnSet::getColumn (uInt columnIndex) const
{
    const String& name = tdescPtr_p->columnDesc(columnIndex).name();
    return COLMAPNAME(name);
}

void ColumnSet::addDataManager (DataManager* dmPtr)
{
    uInt nr = blockDataMan_p.nelements();
    blockDataMan_p.resize (nr + 1);
    blockDataMan_p[nr] = dmPtr;
    dmPtr->setSeqnr (seqCount_p++);
}

void ColumnSet::removeLastDataManager()
{
    uInt nr = blockDataMan_p.nelements() - 1;
    delete BLOCKDATAMANVAL(nr);
    blockDataMan_p.resize (nr, True);
    seqCount_p--;
}

void ColumnSet::initDataManagers (uInt nrrow, Table& tab)
{
    uInt i;
    for (i=0; i<colMap_p.ndefined(); i++) {
	getColumn(i)->createDataManagerColumn();
    }
    //# Delete data managers without columns.
    uInt nr = 0;
    for (i=0; i<blockDataMan_p.nelements(); i++) {
 	if (BLOCKDATAMANVAL(i)->ncolumn() > 0) {
	    blockDataMan_p[nr++] = blockDataMan_p[i];
	}else{
	    delete BLOCKDATAMANVAL(i);
	}
    }
    //# Remove possible trailing elements by resizing the block.
    blockDataMan_p.resize (nr, True);    
    //# Set the number of rows.
    nrrow_p = nrrow;
    //# Initialize all data managers further.
    initSomeDataManagers (0, tab);
}

void ColumnSet::initSomeDataManagers (uInt from, Table& tab)
{
    uInt i;
    //# Link the data managers to the table.
    for (i=from; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->linkToTable (tab);
    }
    //# Now give the data managers the opportunity to create files as needed.
    //# Thereafter to prepare things.
    for (i=from; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->create (nrrow_p);
    }
    prepareSomeDataManagers (from);
}

void ColumnSet::prepareSomeDataManagers (uInt from)
{
    uInt i, j;
    for (i=from; i<blockDataMan_p.nelements(); i++) {
	if (BLOCKDATAMANVAL(i)->canReallocateColumns()) {
	    for (j=0; j<colMap_p.ndefined(); j++) {
		DataManagerColumn*& column = getColumn(j)->dataManagerColumn();
		column = BLOCKDATAMANVAL(i)->reallocateColumn (column);
	    }
	}
    }
    for (i=from; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->prepare();
    }
}


void ColumnSet::resync (uInt nrrow)
{
    //# There may be no sync data (when new table locked for first time).
    if (dataManChanged_p.nelements() > 0) {
	AlwaysAssert (dataManChanged_p.nelements() ==
		                   blockDataMan_p.nelements(), AipsError);
	for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	    if (dataManChanged_p[i]  ||  nrrow != nrrow_p) {
		BLOCKDATAMANVAL(i)->resync (nrrow);
	    }
	}
	nrrow_p = nrrow;
    }
}


void ColumnSet::invalidateColumnCaches()
{
    for (uInt i=0; i<colMap_p.ndefined(); i++) {
	COLMAPVAL(i)->columnCache().invalidate();
    }
}


//# Do all data managers allow to add and remove rows and columns?
Bool ColumnSet::canAddRow() const
{
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	if (! BLOCKDATAMANVAL(i)->canAddRow()) {
	    return False;
	}
    }
    return True;
}
Bool ColumnSet::canRemoveRow() const
{
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	if (! BLOCKDATAMANVAL(i)->canRemoveRow()) {
	    return False;
	}
    }
    return True;
}
Bool ColumnSet::canRemoveColumn (const String& columnName) const
{
    // Cannot be removed if column is unknown.
    if (! tdescPtr_p->isColumn (columnName)) {
	return False;
    }
    return getColumn(columnName)->dataManager()->canRemoveColumn();
}


//# Add rows to all data managers.
void ColumnSet::addRow (uInt nrrow)
{
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->addRow (nrrow);
    }
    nrrow_p += nrrow;
}
//# Remove a row from all data managers.
void ColumnSet::removeRow (uInt rownr)
{
    if (!canRemoveRow()) {
	throw (TableInvOper ("Cannot remove rows from this table"));
    }
    if (rownr >= nrrow_p) {
	throw (TableInvOper ("removeRow: rownr out of bounds"));
    }
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->removeRow (rownr);
    }
    nrrow_p--;
}


void ColumnSet::addColumn (const ColumnDesc& columnDesc, Table& tab)
{
    // Find a storage manager allowing addition of columns.
    // If found, add the column to it and exit.
    DataManager* dmptr;
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	dmptr = BLOCKDATAMANVAL(i);
	if (dmptr->isStorageManager()  &&  dmptr->canAddColumn()) {
	    doAddColumn (columnDesc, dmptr);
	    return;
	}
    }
    // No suitable data manager found.
    // Create the default storage manager and add the column to it.
    String dataManager (columnDesc.dataManagerType());
    dmptr = DataManager::getCtor(dataManager) (dataManager);
    addColumn (columnDesc, *dmptr, tab);
    delete dmptr;
}

void ColumnSet::addColumn (const ColumnDesc& columnDesc,
			   const String& dataManager, Bool byName,
			   Table& tab)
{
    // Give an error when no data manager name/type given.
    if (dataManager.empty()) {
	throw (TableInvOper ("Table::addColumn: no name/type given"));
    }
    // When given by name, find the data manager and add the column to it.
    // findDataManager throws an exception when the data manager is unknown.
    if (byName) {
	doAddColumn (columnDesc, findDataManager (dataManager));
	return;
    }
    // Find the first data manager with the given type allowing addition
    // of columns. If found, add the column and exit.
    DataManager* dmptr;
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	dmptr = BLOCKDATAMANVAL(i);
	if (dataManager == dmptr->dataManagerType()) {
	    if (dmptr->canAddColumn()) {
		doAddColumn (columnDesc, dmptr);
		return;
	    }
	}
    }
    // No suitable data manager found.
    // Create one of this type and add the column to it.
    dmptr = DataManager::getCtor(dataManager) (dataManager);
    addColumn (columnDesc, *dmptr, tab);
    delete dmptr;
}

void ColumnSet::doAddColumn (const ColumnDesc& columnDesc,
			     DataManager* dataManPtr)
{
    checkLock (FileLocker::Write, True);
    //# When the column already exists, TableDesc::addColumn throws
    //# an exception.
    //# The creation and binding of a column will always succeed.
    const String& name = columnDesc.name();
    ColumnDesc& cd = tdescPtr_p->addColumn (columnDesc);
    PlainColumn* col = cd.makeColumn (this);
    colMap_p.define (name, col);
    col->bind (dataManPtr);
    //# The creation of a column may fail as well as adding the
    //# column to the storage manager.
    //# Take care of this by catching an exception and deleting the stuff.
    //# Rethrow the exception by getting the message and throwing it.
    Bool error = False;
    String msg;
    DataManagerColumn* dmcol = 0;
    uInt nrcol = dataManPtr->ncolumn();
    try {
	col->createDataManagerColumn();
	dmcol = col->dataManagerColumn();
	dataManPtr->addColumn (dmcol);
    } catch (AipsError x) {
	error = True;
	msg = x.getMesg();
	//# Get the column pointer (it may not have been filled yet).
	//# When #columns has grown, the columnhas been already added.
	//# In that case remove it, which will also delete the column.
	//# Otherwise delete the column directly.
	dmcol = col->dataManagerColumn();
	if (dataManPtr->ncolumn() > nrcol) {
	    dataManPtr->removeColumn (dmcol);
	}else{
	    delete dmcol;
	}
	//# Delete the PlainColumn, remove from map and delete its description.
	delete col;
	colMap_p.remove (name);
	tdescPtr_p->removeColumn (name);
    } end_try;
    // Rethrow if there was an exception.
    if (error) {
	throw (AipsError (msg));
    }
    autoReleaseLock();
}

void ColumnSet::addColumn (const ColumnDesc& columnDesc,
			   const DataManager& dataManager, Table& tab)
{
    TableDesc td;
    td.addColumn (columnDesc);
    addColumn (td, dataManager, tab);
}

void ColumnSet::addColumn (const TableDesc& tableDesc,
			   const DataManager& dataManager, Table& tab)
{
    checkLock (FileLocker::Write, True);
    // Check if the data manager name has not been used already.
    checkDataManagerName (dataManager.dataManagerName(), 0);
    // Add the new table description to the current one.
    // This adds column and possible hypercolumn descriptions.
    // When failing, nothing will have been added.
    tdescPtr_p->add (tableDesc, False);
    // Clone the data manager (to get our own copy) and add it to the list.
    DataManager* dmptr = dataManager.clone();
    addDataManager (dmptr);
    // Loop through all new columns and construct column objects for them.
    // We have to use the column description in our table description.
    // Bind the column to the data manager and create a column in there.
    // An exception may be thrown in this loop, so things have to be cleaned.
    Bool error = False;
    String msg;
    try {
	for (uInt i=0; i<tableDesc.ncolumn(); i++) {
	    const ColumnDesc& cd = tdescPtr_p->columnDesc(tableDesc[i].name());
	    PlainColumn* col = cd.makeColumn (this);
	    colMap_p.define (cd.name(), col);
	    col->bind (dmptr);
	    col->createDataManagerColumn();
	}
	// Let the new data manager create space, etc. for its columns.
	initSomeDataManagers (blockDataMan_p.nelements() - 1, tab);
    } catch (AipsError x) {
	error = True;
	msg = x.getMesg();
	for (uInt i=0; i<tableDesc.ncolumn(); i++) {
	    const String& name = tableDesc[i].name();
	    if (colMap_p.isDefined (name)) {
		delete COLMAPNAME(name);
		colMap_p.remove (name);
	    }
	    tdescPtr_p->removeColumn (name);
	}
	removeLastDataManager();
    } end_try;
    // Rethrow if there was an exception.
    if (error) {
	throw (AipsError (msg));
    }
    autoReleaseLock();
}



DataManager* ColumnSet::findDataManager (const String& dataManagerName) const
{
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	if (dataManagerName == BLOCKDATAMANVAL(i)->dataManagerName()) {
	    return BLOCKDATAMANVAL(i);
	}
    }
    throw (TableInvOper ("Data manager " + dataManagerName + " is unknown"));
    return 0;
}

void ColumnSet::checkDataManagerNames() const
{
    // Loop through all data managers.
    // A name can appear only once (except a blank name).
    String name;
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	checkDataManagerName (BLOCKDATAMANVAL(i)->dataManagerName(), i+1);
    }
}
void ColumnSet::checkDataManagerName (const String& name, uInt from) const
{
    // Loop through all data managers.
    // A name can appear only once (except a blank name).
    if (! name.empty()) {
	for (uInt j=from; j<blockDataMan_p.nelements(); j++) {
	    if (name == BLOCKDATAMANVAL(j)->dataManagerName()) {
		throw (TableInvOper ("Data manager name " + name +
				     " is already used"));
	    }
	}
    }
}


//# Initialize rows.
void ColumnSet::initialize (uInt startRow, uInt endRow)
{
    for (uInt i=0; i<colMap_p.ndefined(); i++) {
	getColumn(i)->initialize (startRow, endRow);
    }
}


void ColumnSet::reopenRW()
{
    uInt i;
    // Reopen all data managers.
    for (i=0; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->reopenRW();
    }
    // Reopen tables in all column keyword sets.
    for (i=0; i<colMap_p.ndefined(); i++) {
	getColumn(i)->keywordSet().reopenRW();
    }
}

void ColumnSet::renameTables (const String& newName, const String& oldName)
{
    for (uInt i=0; i<colMap_p.ndefined(); i++) {
	getColumn(i)->rwKeywordSet().renameTables (newName, oldName);
    }
}


void ColumnSet::putFile (Bool writeTable, AipsIO& ios,
			 const String& tableName, Bool fsync)
{
    //# Only write the table data when the flag is set.
    dataManChanged_p.resize (blockDataMan_p.nelements());
    uInt i;
    if (writeTable) {
	//# The first version of ColumnSet did not put a version.
	//# Therefore a negative number is put as the version
	//# (because nrrow_p is always positive).
	ios << -2;          // version (must be negative !!!)
	ios << nrrow_p;
	ios << seqCount_p;
	//# Start with writing the data manager types.
	//# Only write with columns in them (thus count first).
	uInt nr=0;
	for (i=0; i<blockDataMan_p.nelements(); i++) {
	    if (BLOCKDATAMANVAL(i)->ncolumn() > 0) {
		nr++;
	    }
	}
	ios << nr;
	for (i=0; i<blockDataMan_p.nelements(); i++) {
	    if (BLOCKDATAMANVAL(i)->ncolumn() > 0) {
		ios << BLOCKDATAMANVAL(i)->dataManagerType();
		ios << BLOCKDATAMANVAL(i)->sequenceNr();
	    }
	}
	//# Now write all columns.
	for (i=0; i<colMap_p.ndefined(); i++) {
	    getColumn(i)->putFile (ios, tableName);
	}
    }
    //# Now write out the data in all data managers.
    MemoryIO memio;
    AipsIO aio(&memio);
    for (i=0; i<blockDataMan_p.nelements(); i++) {
	dataManChanged_p[i] = BLOCKDATAMANVAL(i)->flush (aio, fsync);
	if (writeTable) {
	    ios.put (uInt(memio.length()), memio.getBuffer());
	}
	memio.clear();
    }
}


void ColumnSet::getFile (AipsIO& ios, Table& tab, uInt nrrow)
{
    //# When the first value is negative, it is the version.
    //# Otherwise it is nrrow_p.
    Int version;
    uInt i, nr, seqnr, nrman;
    String str;
    ios >> version;
    if (version < 0) {
	version = -version;
	ios >> nrrow_p;
    }else{
	nrrow_p = version;
	version = 1;
    }
    //# Use nrrow from caller, since that is most accurate.
    nrrow_p = nrrow;
    ios >> nrman;
    ios >> nr;
    //# Construct the various data managers.
    for (i=0; i<nr; i++) {
	//# Get type name of data manager and its sequence nr.
	//# Find its "constructor" and construct it.
	//# Add it to the data manager list and set its sequence nr.
	ios >> str;
	ios >> seqnr;
	DataManager* dmp = DataManager::getCtor(str)(str);
	addDataManager (dmp);
        dmp->setSeqnr (seqnr);
    }
    //# Now set seqCount_p (because that was changed by addDataManager).
    seqCount_p = nrman;
    //# Now read in the columns and create the data manager columns.
    //# In the first version the columns were written in order of
    //# name. In the newer versions they are written in order of addition
    //# (which was needed to support addColumn properly and is better anyway).
    for (i=0; i<colMap_p.ndefined(); i++) {
	if (version == 1) {
	    COLMAPVAL(i)->getFile (ios, *this, tab.isWritable(),
				   tab.tableName());
	}else{
	    getColumn(i)->getFile (ios, *this, tab.isWritable(),
				   tab.tableName());
	}
    }
    //# Link the data managers to the table.
    for (i=0; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->linkToTable (tab);
    }
    //# Finally open the data managers and let them prepare themselves.
    for (i=0; i<nr; i++) {
	uChar* data;
	uInt leng;
	ios.getnew (leng, data);
	MemoryIO memio (data, leng);
	AipsIO aio(&memio);
	BLOCKDATAMANVAL(i)->open (nrrow_p, aio);
	delete [] data;
    }
    prepareSomeDataManagers (0);
}


//# Find the data manager with the given sequence number.
DataManager* ColumnSet::getDataManager (uInt seqnr) const
{
    DataManager* smp;
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	smp = BLOCKDATAMANVAL(i);
	if (smp->sequenceNr() == seqnr) {
	    return smp;
	}
    }
    throw (TableInternalError ("ColumnSet::getDataManager"));
    return 0;
}


void ColumnSet::doLock (FileLocker::LockType type, Bool wait)
{
    if (lockPtr_p->option() != TableLock::AutoLocking) {
	throw (TableError ("ColumnSet::checkLock: table should be locked "
			   "when using PermenentLocking or UserLocking"));
    }
    uInt nattempts = (wait  ?  plainTablePtr_p->lockOptions().maxWait() : 1);
    plainTablePtr_p->lock (type, nattempts);
}

void ColumnSet::setTableChanged()
{
    plainTablePtr_p->setTableChanged();
}
