//# MemoryTable.cc: Class for a table held in memory
//# Copyright (C) 2003
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


#include <casacore/tables/Tables/MemoryTable.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableLockData.h>
#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/PlainColumn.h>
#include <casacore/tables/DataMan/MemoryStMan.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/HostInfo.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

MemoryTable::MemoryTable (SetupNewTable& newtab, uInt nrrow, Bool initialize)
: BaseTable   (newtab.name(), newtab.option(), 0),
  colSetPtr_p (0),
  lockPtr_p   (0)
{
  //# Check if another Table was already constructed using this
  //# SetupNewTable (which is invalid).
  if (newtab.isUsed()) {
    throw (TableInvOper
	   ("SetupNewTable object already used for another Table"));
  }
  //# Use MemoryStMan for stored and unbound columns.
  TableDesc* tdescPtr  = newtab.tableDescPtr();
  ColumnSet* colSetPtr = newtab.columnSetPtr();
  MemoryStMan stman(colSetPtr->uniqueDataManagerName("MSMTAB"));
  for (uInt i=0; i<tdescPtr->ncolumn(); i++) {
    PlainColumn* col = colSetPtr->getColumn(i);
    if (!col->isBound()  ||  col->isStored()) {
      newtab.bindColumn (tdescPtr->columnDesc(i).name(), stman);
    }
  }
  //# Check if there are no data managers with equal names.
  newtab.columnSetPtr()->checkDataManagerNames ("MemoryTable");
  //# Get the data from the SetupNewTable object.
  //# Set SetupNewTable object to in use.
  tdescPtr_p  = tdescPtr;
  colSetPtr_p = colSetPtr;
  colSetPtr_p->linkToTable (this);
  newtab.setInUse();
  //# Create the lock object.
  lockPtr_p = new TableLockData (TableLock(TableLock::PermanentLocking),
				 0, this);
  colSetPtr_p->linkToLockObject (lockPtr_p);
  //# Initialize the data managers.
  Table tab(this, False);
  nrrowToAdd_p = nrrow;
  colSetPtr_p->initDataManagers (nrrow, False,
                                 TSMOption(TSMOption::Cache,0,0), tab);
  //# Initialize the columns if needed.
  if (initialize  &&  nrrow > 0) {
    colSetPtr_p->initialize (0, nrrow-1);
  }
  //# Nrrow_p has to be set here, otherwise data managers may use the
  //# incorrect number of rows (similar behaviour as in function addRow).
  nrrowToAdd_p = 0;
  nrrow_p = nrrow;
  // The table is transient, thus deleted when destructed.
  // It is set, so Table::isMarkedForDelete() returns True.
  markForDelete (False, "");
}

MemoryTable::~MemoryTable()
{
  delete lockPtr_p;
  delete colSetPtr_p;
}

void MemoryTable::reopenRW()
{}

Bool MemoryTable::asBigEndian() const
{
  return HostInfo::bigEndian();
}

const StorageOption& MemoryTable::storageOption() const
{
  return colSetPtr_p->storageOption();
}

Bool MemoryTable::isMultiUsed (Bool) const
{
  return False;
}

const TableLock& MemoryTable::lockOptions() const
{
  return *lockPtr_p;
}

void MemoryTable::mergeLock (const TableLock&)
{}

Bool MemoryTable::hasLock (FileLocker::LockType) const
{
  return True;
}

Bool MemoryTable::lock (FileLocker::LockType, uInt)
{
  return True;
}

void MemoryTable::unlock()
{}

void MemoryTable::flush (Bool, Bool)
{}

void MemoryTable::resync()
{}

 uInt MemoryTable::getModifyCounter() const
{
  return 0;
}

Bool MemoryTable::isWritable() const
{
  return True;
}

void MemoryTable::copy (const String& newName, int tableOption) const
{
  Record dmInfo = colSetPtr_p->dataManagerInfo();
  deepCopy (newName, dmInfo, tableOption, True, Table::AipsrcEndian, False);
}

void MemoryTable::deepCopy (const String& newName,
			    const Record& dataManagerInfo,
			    int tableOption, Bool, int endianFormat,
			    Bool noRows) const
{
  trueDeepCopy (newName, dataManagerInfo, tableOption, endianFormat, noRows);
}

void MemoryTable::rename (const String& newName, int)
{
  //# Rename the names of the subtables in the keywords.
  String oldName = name_p;
  renameSubTables (newName, oldName);
  name_p = newName;
}


int MemoryTable::tableType() const
{
  return Table::Memory;
}

TableDesc MemoryTable::actualTableDesc() const
{
  return *tdescPtr_p;
}

Record MemoryTable::dataManagerInfo() const
{
  return colSetPtr_p->dataManagerInfo();
}

TableRecord& MemoryTable::keywordSet()
{
  return tdescPtr_p->rwKeywordSet();
}

TableRecord& MemoryTable::rwKeywordSet()
{
  return tdescPtr_p->rwKeywordSet();
}

void MemoryTable::flushTableInfo()
{}

BaseColumn* MemoryTable::getColumn (uInt columnIndex) const
{
  return colSetPtr_p->getColumn (columnIndex);
}
BaseColumn* MemoryTable::getColumn (const String& columnName) const
{
  return colSetPtr_p->getColumn (columnName);
}

Bool MemoryTable::canAddRow() const
{
  return True;
}

void MemoryTable::addRow (uInt nrrw, Bool initialize)
{
  if (nrrw > 0) {
    nrrowToAdd_p = nrrw;
    colSetPtr_p->addRow (nrrw);
    if (initialize) {
      colSetPtr_p->initialize (nrrow_p, nrrow_p+nrrw-1);
    }
    nrrowToAdd_p = 0;
    nrrow_p += nrrw;
  }
}

Bool MemoryTable::canRemoveRow() const
{
  return True;
}

void MemoryTable::removeRow (uInt rownr)
{
  colSetPtr_p->removeRow (rownr);
  nrrow_p--;
}

void MemoryTable::addColumn (const ColumnDesc& columnDesc, Bool)
{
  Table tab(this, False);
  ColumnDesc cold(columnDesc);
  // Make sure the MemoryStMan is used.
  cold.dataManagerType() = "MemoryStMan";
  cold.dataManagerGroup() = "MSMTAB";
  colSetPtr_p->addColumn (cold, False,
                          TSMOption(TSMOption::Cache,0,0), tab);
}
void MemoryTable::addColumn (const ColumnDesc& columnDesc,
			     const String& dataManager, Bool byName, Bool)
{
  Table tab(this, False);
  if (byName) {
    colSetPtr_p->addColumn (columnDesc, dataManager, byName, False,
                            TSMOption(TSMOption::Cache,0,0), tab);
  } else {
    // Make sure the MemoryStMan is used if no virtual engine is used.
    DataManager* dmptr = DataManager::getCtor(dataManager)
                                                (dataManager, Record());
    addColumn (columnDesc, *dmptr, False);
    delete dmptr;
  }
}
void MemoryTable::addColumn (const ColumnDesc& columnDesc,
			     const DataManager& dataManager, Bool)
{
  Table tab(this, False);
  // Make sure the MemoryStMan is used if no virtual engine is used.
  if (dataManager.isStorageManager()) {
    addColumn (columnDesc, False);
  } else {
    colSetPtr_p->addColumn (columnDesc, dataManager, False,
                            TSMOption(TSMOption::Cache,0,0), tab);
  }
}
void MemoryTable::addColumn (const TableDesc& tableDesc,
			     const DataManager& dataManager, Bool)
{
  Table tab(this, False);
  // Make sure the MemoryStMan is used if no virtual engine is used.
  if (dataManager.isStorageManager()) {
    MemoryStMan stman(dataManager.dataManagerName());
    colSetPtr_p->addColumn (tableDesc, stman, False,
                            TSMOption(TSMOption::Cache,0,0), tab);
  } else {
    colSetPtr_p->addColumn (tableDesc, dataManager, False,
                            TSMOption(TSMOption::Cache,0,0), tab);
  }
}

Bool MemoryTable::canRemoveColumn (const Vector<String>&) const
{
  return True;
}

void MemoryTable::removeColumn (const Vector<String>& columnNames)
{
  colSetPtr_p->removeColumn (columnNames);
}

Bool MemoryTable::canRenameColumn (const String&) const
{
  return True;
}

void MemoryTable::renameColumn (const String& newName, const String& oldName)
{
  colSetPtr_p->renameColumn (newName, oldName);
}

void MemoryTable::renameHypercolumn (const String& newName, const String& oldName)
{
  tdescPtr_p->renameHypercolumn (newName, oldName);
}

DataManager* MemoryTable::findDataManager (const String& name,
                                           Bool byColumn) const
{
  return colSetPtr_p->findDataManager (name, byColumn);
}

} //# NAMESPACE CASACORE - END

