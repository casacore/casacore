//# ColumnSet.cc: Class to manage a set of table columns
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

#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/PlainColumn.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <limits>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define BLOCKDATAMANVAL(I) (static_cast<DataManager*>(blockDataMan_p[I]))
#define COLMAPNAME(NAME)   (static_cast<PlainColumn*>(colMap_p.at(NAME)))
#define COLMAPCAST(PTR)    (static_cast<PlainColumn*>(PTR))


ColumnSet::ColumnSet (TableDesc* tdesc, const StorageOption& opt)
: tdescPtr_p      (tdesc),
  storageOpt_p    (opt),
  baseTablePtr_p  (0),
  lockPtr_p       (0),
  seqCount_p      (0),
  blockDataMan_p  (0)
{
    //# Loop through all columns in the description and create
    //# a column out of them.
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	const ColumnDesc& cd = tdescPtr_p->columnDesc(i);
	colMap_p.insert (std::make_pair(cd.name(), cd.makeColumn(this)));
    }
}


ColumnSet::~ColumnSet()
{
    for (auto& x : colMap_p) {
        delete COLMAPCAST(x.second);
    }
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	delete BLOCKDATAMANVAL(i);
    }
}


PlainColumn* ColumnSet::getColumn (const String& columnName) const
{
    tdescPtr_p->columnDesc(columnName);             // check if column exists
    return COLMAPNAME(columnName);
}

//# First get the column name and use that as key.
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

void ColumnSet::initDataManagers (rownr_t nrrow, Bool bigEndian,
                                  const TSMOption& tsmOption,
                                  Table& tab)
{
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->setEndian (bigEndian);
	BLOCKDATAMANVAL(i)->setTsmOption (tsmOption);
    }
    for (uInt i=0; i<colMap_p.size(); ++i) {
        getColumn(i)->createDataManagerColumn();
    }
    //# Delete data managers without columns.
    uInt nr = 0;
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
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
    openMultiFile (from, tab, ByteIO::New);
    //# Link the data managers to the table.
    for (uInt i=from; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->linkToTable (tab);
    }
    //# Now give the data managers the opportunity to create files as needed.
    //# Thereafter to prepare things.
    for (uInt i=from; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->create64 (nrrow_p);
    }
    prepareSomeDataManagers (from);
}

void ColumnSet::prepareSomeDataManagers (uInt from)
{
    for (uInt i=from; i<blockDataMan_p.nelements(); i++) {
	if (BLOCKDATAMANVAL(i)->canReallocateColumns()) {
	    for (uInt j=0; j<colMap_p.size(); j++) {
		DataManagerColumn*& column = getColumn(j)->dataManagerColumn();
		column = BLOCKDATAMANVAL(i)->reallocateColumn (column);
	    }
	}
    }
    for (uInt i=from; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->prepare();
    }
}

void ColumnSet::openMultiFile (uInt from, const Table& tab,
                               ByteIO::OpenOption opt)
{
  // Exit if MultiFile/HDF5 should not be used.
  if (storageOpt_p.option() != StorageOption::MultiFile  &&
      storageOpt_p.option() != StorageOption::MultiHDF5) {
    return;
  }
  // See if any data manager can use MultiFile/HDF5. 
  Bool useMultiFile = False;
  for (uInt i=from; i<blockDataMan_p.nelements(); i++) {
    useMultiFile = useMultiFile || BLOCKDATAMANVAL(i)->hasMultiFileSupport();
  }
  // If anyone does, use the MultiFile.
  if (useMultiFile) {
    // Create the object if not created yet.
    if (! multiFile_p) {
      if (storageOpt_p.option() == StorageOption::MultiFile) {
        multiFile_p.reset (new MultiFile (tab.tableName() + "/table.mf",
                                          opt, storageOpt_p.blockSize(),
                                          storageOpt_p.useODirect()));
      } else {
        multiFile_p.reset (new MultiHDF5 (tab.tableName() + "/table.mfh5",
                                          opt, storageOpt_p.blockSize()));
      }
    }
    // Pass it to the data managers.
    for (uInt i=from; i<blockDataMan_p.nelements(); i++) {
      BLOCKDATAMANVAL(i)->setMultiFile (multiFile_p);
    }
  }
}

rownr_t ColumnSet::resync (rownr_t nrrow, Bool forceSync)
{
    //# There may be no sync data (when new table locked for first time).
    if (dataManChanged_p.nelements() > 0) {
	AlwaysAssert (dataManChanged_p.nelements() ==
		                   blockDataMan_p.nelements(), AipsError);
	for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	    if (dataManChanged_p[i]  ||  nrrow != nrrow_p  ||  forceSync) {
                rownr_t nrr = BLOCKDATAMANVAL(i)->resync64 (nrrow);
                if (nrr > nrrow) {
                    nrrow = nrr;
                }
		dataManChanged_p[i] = False;
	    }
	}
	nrrow_p = nrrow;
    }
    return nrrow_p;
}


void ColumnSet::invalidateColumnCaches()
{
    for (auto& x : colMap_p) {
	COLMAPCAST(x.second)->columnCache().invalidate();
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
Bool ColumnSet::canRemoveColumn (const Vector<String>& columnNames) const
{
    // Cannot be removed if column is unknown.
    for (uInt i=0; i<columnNames.nelements(); i++) {
        if (! tdescPtr_p->isColumn (columnNames(i))) {
	    return False;
	}
	if (! getColumn(columnNames(i))->dataManager()->canRemoveColumn()) {
	    return False;
	}
    }
    return True;
}
Bool ColumnSet::canRenameColumn (const String& columnName) const
{
    // Cannot be renamed if column is unknown.
    if (! tdescPtr_p->isColumn (columnName)) {
	return False;
    }
    return getColumn(columnName)->dataManager()->canRenameColumn();
}


//# Add rows to all data managers.
void ColumnSet::addRow (rownr_t nrrow)
{
    // First add row to storage managers, thereafter to virtual engines.
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
        if (BLOCKDATAMANVAL(i)->isStorageManager()) {
	    BLOCKDATAMANVAL(i)->addRow64 (nrrow);
	}
    }
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
        if (! BLOCKDATAMANVAL(i)->isStorageManager()) {
	    BLOCKDATAMANVAL(i)->addRow64 (nrrow);
	}
    }
    nrrow_p += nrrow;
}
//# Remove a row from all data managers.
void ColumnSet::removeRow (rownr_t rownr)
{
    if (!canRemoveRow()) {
	throw (TableInvOper ("Rows cannot be removed from table " +
			     baseTablePtr_p->tableName() + 
			     "; its storage managers do not support it"));
    }
    if (rownr >= nrrow_p) {
	throw (TableInvOper ("removeRow: rownr " + String::toString(rownr) +
			     " too high in table " + baseTablePtr_p->tableName() +
			     " (#rows=" + String::toString(nrrow_p) + ")"));
    }
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->removeRow64 (rownr);
    }
    nrrow_p--;
}


void ColumnSet::addColumn (const ColumnDesc& columnDesc,
			   Bool bigEndian, const TSMOption& tsmOption,
                           Table& tab)
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
    // Make sure the data manager name is not already used.
    dmptr = DataManager::getCtor(columnDesc.dataManagerType())
                      (uniqueDataManagerName (columnDesc.dataManagerGroup()),
		       Record());
    addColumn (columnDesc, *dmptr, bigEndian, tsmOption, tab);
    delete dmptr;
}

void ColumnSet::addColumn (const ColumnDesc& columnDesc,
			   const String& dataManager, Bool byName,
			   Bool bigEndian, const TSMOption& tsmOption,
                           Table& tab)
{
    // Give an error when no data manager name/type given.
    if (dataManager.empty()) {
	throw (TableInvOper ("Table::addColumn: no datamanager name/type given "
			     "when adding column " + columnDesc.name() +
			     " to table " + baseTablePtr_p->tableName()));
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
    // Use the data manager as the data manager name.
    dmptr = DataManager::getCtor(dataManager)
                                   (uniqueDataManagerName(dataManager),
				    Record());
    addColumn (columnDesc, *dmptr, bigEndian, tsmOption, tab);
    delete dmptr;
}

void ColumnSet::doAddColumn (const ColumnDesc& columnDesc,
			     DataManager* dataManPtr)
{
    if (! dataManPtr->canAddColumn()) {
      throw TableError ("Table::addColumn - DataManager " +
			dataManPtr->dataManagerName() + " (" +
			dataManPtr->dataManagerType() +
			") does not support column addition to table " +
			baseTablePtr_p->tableName());
    }
    checkWriteLock (True);
    //# When the column already exists, TableDesc::addColumn throws
    //# an exception.
    //# The creation and binding of a column will always succeed.
    const String& name = columnDesc.name();
    ColumnDesc& cd = tdescPtr_p->addColumn (columnDesc);
    PlainColumn* col = cd.makeColumn (this);
    colMap_p.insert (std::make_pair(name, col));
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
    } catch (const std::exception& x) {
	error = True;
	msg = x.what();
	//# Get the column pointer (it may not have been filled yet).
	//# When #columns has grown, the column has been already added.
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
	colMap_p.erase (name);
	tdescPtr_p->removeColumn (name);
    } 
    // Rethrow if there was an exception.
    if (error) {
	throw (AipsError (msg));
    }
    autoReleaseLock();
}

void ColumnSet::addColumn (const ColumnDesc& columnDesc,
			   const DataManager& dataManager,
			   Bool bigEndian, const TSMOption& tsmOption,
                           Table& tab)
{
    TableDesc td;
    td.addColumn (columnDesc);
    addColumn (td, dataManager, bigEndian, tsmOption, tab);
}

void ColumnSet::addColumn (const TableDesc& tableDesc,
			   const DataManager& dataManager,
			   Bool bigEndian, const TSMOption& tsmOption,
                           Table& tab)
{
    checkWriteLock (True);
    // Check if the data manager name has not been used already.
    checkDataManagerName (dataManager.dataManagerName(), 0,
                          baseTablePtr_p->tableName());
    // Add the new table description to the current one.
    // This adds column and possible hypercolumn descriptions.
    // When failing, nothing will have been added.
    tdescPtr_p->add (tableDesc, False);
    // Clone the data manager (to get our own copy) and add it to the list.
    DataManager* dmptr = dataManager.clone();
    dmptr->setEndian (bigEndian);
    dmptr->setTsmOption (tsmOption);
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
	    colMap_p.insert (std::make_pair(cd.name(), col));
	    col->bind (dmptr);
	    col->createDataManagerColumn();
	}
	// Let the new data manager create space, etc. for its columns.
	initSomeDataManagers (blockDataMan_p.nelements() - 1, tab);
    } catch (const std::exception& x) {
	error = True;
	msg = x.what();
	for (uInt i=0; i<tableDesc.ncolumn(); i++) {
	    const String& name = tableDesc[i].name();
	    if (colMap_p.find(name) != colMap_p.end()) {
		delete COLMAPNAME(name);
		colMap_p.erase (name);
	    }
	    tdescPtr_p->removeColumn (name);
	}
	removeLastDataManager();
    } 
    // Rethrow if there was an exception.
    if (error) {
	throw (AipsError (msg));
    }
    autoReleaseLock();
}

void ColumnSet::removeColumn (const Vector<String>& columnNames)
{
    // Check if the columns can be removed.
    // Also find out about the data managers.
    std::map<void*,Int> dmCounts = checkRemoveColumn (columnNames);
    // Write lock table.
    checkWriteLock (True);
    // Remove all data managers possible.
    for (auto& x : dmCounts) {
        if (x.second < 0) {
	    DataManager* dmPtr = static_cast<DataManager *>(const_cast<void *>(x.first));
	    dmPtr->deleteManager();
	    Bool found = False;
	    for (uInt j=0; j<blockDataMan_p.nelements(); j++) {
	        if (dmPtr == blockDataMan_p[j]) {
		    found = True;
		    delete dmPtr;
		    uInt nrb = blockDataMan_p.nelements();
		    uInt nr = nrb - j - 1;
		    if (nr > 0) {
		        objmove (&blockDataMan_p[j], &blockDataMan_p[j+1], nr);
		    }
		    blockDataMan_p.resize (nrb - 1, True, True);
		    uInt nrc = dataManChanged_p.nelements();
		    if (j < nrc) {
		        nr = nrc - j - 1;
			if (nr > 0) {
			    objmove (&dataManChanged_p[j],
				     &dataManChanged_p[j+1], nr);
			}
			dataManChanged_p.resize (nrc - 1, True, True);
		    }
		    break;
		}
	    }
	    AlwaysAssert (found, AipsError);
	}
    }
    // Remove all columns from description, data managers, and maps.
    for (uInt i=0; i<columnNames.nelements(); i++) {
        const String& name = columnNames(i);
	tdescPtr_p->removeColumn (name);
	PlainColumn* colPtr = COLMAPNAME(name);
	DataManager* dmPtr = colPtr->dataManager();
	if (dmCounts.at(dmPtr) >= 0) {
	    DataManagerColumn* dmcolPtr = colPtr->dataManagerColumn();
	    dmPtr->removeColumn (dmcolPtr);
	}
	delete colPtr;
	colMap_p.erase (name);
    }
    autoReleaseLock();
}

std::map<void*,Int> ColumnSet::checkRemoveColumn 
					(const Vector<String>& columnNames)
{
    // Check if the column names are valid.
    baseTablePtr_p->checkRemoveColumn (columnNames, True);
    // Count how many columns in each data manager are to be deleted.
    std::map<void*,Int> dmCounts;
    for (uInt i=0; i<columnNames.nelements(); i++) {
        void* dmPtr = COLMAPNAME(columnNames[i])->dataManager();
        std::map<void*,Int>::iterator iter = dmCounts.find(dmPtr);
        if (iter == dmCounts.end()) {
            dmCounts.insert (std::make_pair(dmPtr, 1));
        } else {
            iter->second++;
        }
    }
    // If all columns in a data manager are to be deleted, set count to -1.
    for (auto& iter : dmCounts) {
        DataManager* dmPtr = static_cast<DataManager*>(const_cast<void*>(iter.first));
	if (iter.second == Int(dmPtr->ncolumn())) {
	    iter.second = -1;
	}
    }
    // Now we have to check if a column can be deleted.
    // It can if all columns of its data manager are deleted or
    // if the data manager can handle column deletion.
    // Set a flag for the columns for which the entire data manager
    // cannot be deleted, thus the column has to be deleted explicitly.
    for (uInt i=0; i<columnNames.nelements(); i++) {
        DataManager* dmPtr = COLMAPNAME(columnNames(i))->dataManager();
        if (dmCounts.at(dmPtr) >= 0  &&  ! dmPtr->canRemoveColumn()) {
	    throw TableInvOper ("Table::removeColumn - column " +
				columnNames(i) + " cannot be removed from table " +
				baseTablePtr_p->tableName());
	}
    }
    return dmCounts;
}

void ColumnSet::renameColumn (const String& newName, const String& oldName)
{
    if (! tdescPtr_p->isColumn (oldName)) {
        throw (TableInvOper ("Table::renameColumn; column " + oldName +
			     " does not exist in table " +
			     baseTablePtr_p->tableName()));
    }
    if (tdescPtr_p->isColumn (newName)) {
        throw (TableInvOper ("Table::renameColumn; new column " + newName +
			     " already exists in table " +
			     baseTablePtr_p->tableName()));
    }
    checkWriteLock (True);
    tdescPtr_p->renameColumn (newName, oldName);
    void* ptr = colMap_p.at(oldName);
    colMap_p.erase (oldName);
    colMap_p.insert (std::make_pair(newName, ptr));
    autoReleaseLock();
}



DataManager* ColumnSet::findDataManager (const String& name,
                                         Bool byColumn) const
{
    if (byColumn) {
        return COLMAPNAME(name)->dataManager();
    }
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
        DataManager* dmp = BLOCKDATAMANVAL(i);
        if (name == dmp->dataManagerName()) {
            return dmp;
        }
    }
    throw (TableInvOper ("Data manager " + name +
                         " is unknown in table " +
                         baseTablePtr_p->tableName()));
}

void ColumnSet::checkDataManagerNames (const String& tableName) const
{
    // Loop through all data managers.
    // A name can appear only once (except a blank name).
    String name;
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
      checkDataManagerName (BLOCKDATAMANVAL(i)->dataManagerName(), i+1,
                            tableName);
    }
}
Bool ColumnSet::checkDataManagerName (const String& name, uInt from,
                                      const String& tableName,
				      Bool doTthrow) const
{
    // Loop through all data managers.
    // A name can appear only once (except a blank name).
    if (! name.empty()) {
	for (uInt j=from; j<blockDataMan_p.nelements(); j++) {
	    if (name == BLOCKDATAMANVAL(j)->dataManagerName()) {
	        if (doTthrow) {
		    throw TableInvOper ("Data manager name " + name +
                                        " is already used in table " +
                                        tableName);
		}
		return False;
	    }
	}
    }
    return True;
}

String ColumnSet::uniqueDataManagerName (const String& name) const
{
    String dmName = name;
    Int nr = 0;
    while (! checkDataManagerName (dmName, 0, String(), False)) {
        nr++;
	dmName = name + '_' + String::toString(nr);
    }
    return dmName;
}


TableDesc ColumnSet::actualTableDesc() const
{
    TableDesc td = *tdescPtr_p;
    for (uInt i=0; i<td.ncolumn(); i++) {
        ColumnDesc& cd = td.rwColumnDesc(i);
	PlainColumn* pc = COLMAPNAME(cd.name());
	cd.dataManagerType() = pc->dataManager()->dataManagerType();
	cd.dataManagerGroup() = pc->dataManager()->dataManagerName();
	if (cd.isArray()  &&  cd.isFixedShape()) {
	    if (cd.shape().nelements() == 0) {
	        cd.setShape (pc->shapeColumn());
	    }
	}
    }
    return td;
}

Record ColumnSet::dataManagerInfo (Bool virtualOnly) const
{
    Record rec;
    uInt nrec=0;
    // Loop through all data managers.
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
        DataManager* dmPtr = BLOCKDATAMANVAL(i);
	if (!virtualOnly  ||  !dmPtr->isStorageManager()) {
	    Record subrec;
	    subrec.define ("TYPE", dmPtr->dataManagerType());
	    subrec.define ("NAME", dmPtr->dataManagerName());
            // Add info of the data manager to the record.
            dmPtr->dataManagerInfo (subrec);
	    // Loop through all columns with this data manager and add
	    // its name to the vector.
	    uInt ncol = colMap_p.size();
	    Vector<String> columns(ncol);
	    uInt nc=0;
            for (auto& x : colMap_p) {
	        if (COLMAPCAST(x.second)->dataManager() == dmPtr) {
                    columns(nc++) = x.first;
		}
	    }
	    if (nc > 0) {
	        columns.resize (nc, True);
		subrec.define ("COLUMNS", columns);
		rec.defineRecord (nrec, subrec);
		nrec++;
	    }
	}
    }
    return rec;
}


//# Initialize rows.
void ColumnSet::initialize (rownr_t startRow, rownr_t endRow)
{
    for (uInt i=0; i<colMap_p.size(); i++) {
        getColumn(i)->initialize (startRow, endRow);
    }
}


void ColumnSet::reopenRW()
{
    if (multiFile_p) {
        multiFile_p->reopenRW();
    }
    // Reopen all data managers.
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	BLOCKDATAMANVAL(i)->reopenRW();
    }
    // Reopen tables in all column keyword sets.
    for (uInt i=0; i<colMap_p.size(); i++) {
	getColumn(i)->keywordSet().reopenRW();
    }
}

void ColumnSet::renameTables (const String& newName, const String& oldName)
{
    for (uInt i=0; i<colMap_p.size(); i++) {
	getColumn(i)->rwKeywordSet().renameTables (newName, oldName);
    }
}

Bool ColumnSet::areTablesMultiUsed() const
{
    for (uInt i=0; i<colMap_p.size(); i++) {
        if (getColumn(i)->keywordSet().areTablesMultiUsed()) {
	    return True;
	}
    }
    return False;
}


Bool ColumnSet::putFile (Bool writeTable, AipsIO& ios,
			 const TableAttr& attr, Bool fsync)
{
    Bool written = False;
    //# Only write the table data when the flag is set.
    uInt nrold = dataManChanged_p.nelements();
    dataManChanged_p.resize (blockDataMan_p.nelements(), True);
    for (uInt i=nrold; i<dataManChanged_p.nelements(); i++) {
        dataManChanged_p[i] = False;
    }
    if (writeTable) {
	//# The first version of ColumnSet did not put a version.
	//# Therefore a negative number is put as the version
	//# (because nrrow_p is always positive).
        // Still use version 2 if MultiFile is not used and #rows fit in an Int.
        if (storageOpt_p.option() != StorageOption::SepFile  ||
            nrrow_p > rownr_t(std::numeric_limits<Int>::max())) {
          ios << Int(-3);          // version (must be negative !!!)
          ios << nrrow_p;
          ios << Int(storageOpt_p.option()) << storageOpt_p.blockSize();
        } else {
          ios << Int(-2);
          ios << uInt(nrrow_p);
        }
	ios << seqCount_p;
	//# Start with writing the data manager types.
	//# Only write with columns in them (thus count first).
	uInt nr=0;
	for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	    if (BLOCKDATAMANVAL(i)->ncolumn() > 0) {
		nr++;
	    }
	}
	ios << nr;
	for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	    if (BLOCKDATAMANVAL(i)->ncolumn() > 0) {
		ios << BLOCKDATAMANVAL(i)->dataManagerType();
		ios << BLOCKDATAMANVAL(i)->sequenceNr();
	    }
	}
	//# Now write all columns.
	for (uInt i=0; i<colMap_p.size(); i++) {
	    getColumn(i)->putFile (ios, attr);
	}
    }
    //# Now write out the data in all data managers.
    //# Keep track if a data manager indeed wrote something.
    auto memio = std::make_shared<MemoryIO>();
    AipsIO aio(memio);
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
        if (BLOCKDATAMANVAL(i)->flush (aio, fsync)) {
	    dataManChanged_p[i] = True;
	    written = True;
	}
	if (writeTable) {
	    ios.put (uInt(memio->length()), memio->getBuffer());
	}
	memio->clear();
    }
    if (multiFile_p) {
      multiFile_p->flush();
    }
    return written;
}


rownr_t ColumnSet::getFile (AipsIO& ios, Table& tab, rownr_t nrrow, Bool bigEndian,
                            const TSMOption& tsmOption)
{
    //# If the first value is negative, it is the version.
    //# Otherwise it is nrrow_p.
    Int version;
    uInt i, nr, seqnr, nrman;
    String str;
    ios >> version;
    if (version < 0) {
	version = -version;
        if (version <= 2) {
          // In older versions nrrow was an unsigned 4-byte integer.
          ios >> nr;
          nrrow_p = nr;
        } else {
          ios >> nrrow_p;
        }
    }else{
	nrrow_p = version;
	version = 1;
    }
    //# Use nrrow from caller, since that is most accurate.
    nrrow_p = nrrow;
    // Read StorageOption for newer versions.
    if (version >= 3) {
      Int opt, bufsz;
      ios >> opt >> bufsz;
      storageOpt_p = StorageOption (StorageOption::Option(opt), bufsz);
    } else {
      storageOpt_p = StorageOption (StorageOption::SepFile);
    }
    ios >> nrman;
    ios >> nr;
    //# Construct the various data managers.
    for (i=0; i<nr; i++) {
	//# Get type name of data manager and its sequence nr.
	//# Find its "constructor" and construct it.
	//# Add it to the data manager list and set its sequence nr.
	ios >> str;
	ios >> seqnr;
	DataManager* dmp = DataManager::getCtor(str)(str, Record());
	addDataManager (dmp);
        dmp->setSeqnr (seqnr);
	dmp->setEndian (bigEndian);
	dmp->setTsmOption (tsmOption);
    }
    // Open the MultiFile if used.
    openMultiFile (0, tab,
                   tab.isWritable()  ?  ByteIO::Update : ByteIO::Old);
    //# Now set seqCount_p (because that was changed by addDataManager).
    seqCount_p = nrman;
    //# Now read in the columns and create the data manager columns.
    //# In the first version the columns were written in order of
    //# name. In the newer versions they are written in order of addition
    //# (which was needed to support addColumn properly and is better anyway).
    if (version == 1) {
        for (auto& x : colMap_p) {
            COLMAPCAST(x.second)->getFile (ios, *this, TableAttr(tab));
        }
    }else{
        for (i=0; i<colMap_p.size(); i++) {
            getColumn(i)->getFile (ios, *this, TableAttr(tab));
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
        auto memio = std::make_shared<MemoryIO>(data, leng);
	AipsIO aio(memio);
	rownr_t nrrow = BLOCKDATAMANVAL(i)->open64 (nrrow_p, aio);
        if (nrrow > nrrow_p) {
          nrrow_p = nrrow;
        }
	delete [] data;
    }
    prepareSomeDataManagers (0);
    return nrrow_p;
}


//# Find the data manager with the given sequence number.
DataManager* ColumnSet::getDataManager (uInt seqnr) const
{
  DataManager* dmp = 0;
    for (uInt i=0; i<blockDataMan_p.nelements(); i++) {
	dmp = BLOCKDATAMANVAL(i);
	if (seqnr == dmp->sequenceNr()) {
	    return dmp;
	}
    }
    throw (TableInternalError ("ColumnSet::getDataManager"));
    return 0;
}


Bool ColumnSet::userLock (FileLocker::LockType type, Bool wait)
{
    // Acquire automatically a lock when:
    // - Userlocking
    // - not locked yet
    // - not NoReadLocking
    if (lockPtr_p->option() == TableLock::UserLocking) {
	if (! baseTablePtr_p->hasLock (type)) {
	    if (type != FileLocker::Read  ||  lockPtr_p->readLocking()) {
	        uInt nattempts = (wait  ?  0 : 1);
		baseTablePtr_p->lock (type, nattempts);
		return True;
	    }
	}
    }
    return False;
}

void ColumnSet::doLock (FileLocker::LockType type, Bool wait)
{
    if (lockPtr_p->option() != TableLock::AutoLocking) {
        String str = "PermanentLocking";
        if (lockPtr_p->option() == TableLock::UserLocking) {
	    str = "UserLocking";
	}
	throw (TableError ("ColumnSet::doLock: table " +
			   baseTablePtr_p->tableName() +
			   " should be locked when using " + str));
    }
    uInt nattempts = (wait  ?  baseTablePtr_p->lockOptions().maxWait() : 1);
    baseTablePtr_p->lock (type, nattempts);
}

void ColumnSet::syncColumns (const ColumnSet& other,
			     const TableAttr& defaultAttr)
{
    uInt ncol = colMap_p.size();
    if (other.colMap_p.size() != ncol) {
	throw (TableError ("ColumnSet::syncColumns; another process "
			   "changed the number of columns of table " +
			   baseTablePtr_p->tableName()));
    }
    for (uInt i=0; i<ncol; i++) {
	PlainColumn* thiscol = getColumn(i);
	PlainColumn* othercol = other.getColumn(i);
	if (thiscol->columnDesc() != othercol->columnDesc()) {
	    throw (TableError ("ColumnSet::syncColumns; another process "
			       "changed the description of column " +
		               thiscol->columnDesc().name() + " in table " +
			       baseTablePtr_p->tableName()));
	}
	// Adjust the attributes of subtables.
	// Update the table keywords.
	TableRecord& oldKeySet = thiscol->keywordSet();
	TableRecord& newKeySet = othercol->keywordSet();
	newKeySet.setTableAttr (oldKeySet, defaultAttr);
	oldKeySet = newKeySet;
    }
}

} //# NAMESPACE CASACORE - END

