//# ConcatTable.cc: Class to view a concatenation of tables as a single table
//# Copyright (C) 2008
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

#include <casacore/tables/Tables/ConcatTable.h>
#include <casacore/tables/Tables/ConcatColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  ConcatTable::ConcatTable (AipsIO& ios, const String& name, uInt nrrow,
			    int option, const TableLock& lockOptions,
                            const TSMOption& tsmOption)
    : BaseTable (name, option, nrrow),
      colMap_p  (static_cast<ConcatColumn*>(0)),
      changed_p (False)
  {
    //# Read the file in.
    // Set initially to no write in destructor.
    // At the end it is reset. In this way nothing is written if
    // an exception is thrown during initialization.
    noWrite_p = True;
    getConcat (ios, option, lockOptions, tsmOption);
    noWrite_p = False;
  }

  ConcatTable::ConcatTable (const Block<BaseTable*>& tables,
			    const Block<String>& subTables,
                            const String& subDirName)
    : BaseTable       ("", Table::Scratch, 0),
      subTableNames_p (subTables),
      subDirName_p    (subDirName),
      colMap_p        (static_cast<ConcatColumn*>(0)),
      changed_p       (True)
  {
    noWrite_p = True;
    if (tables.nelements() == 0) {
      throw TableError("ConcatTable: at least one table has to be given");
    }
    baseTabPtr_p.resize (tables.nelements());
    baseTabPtr_p = 0;
    rows_p.reserve (tables.nelements() + 1);
    for (uInt i=0; i<tables.nelements(); ++i) {
      //# Link to referenced table, otherwise it will be destructed.
      baseTabPtr_p[i] = tables[i];;
      baseTabPtr_p[i]->link();
      rows_p.add (baseTabPtr_p[i]->nrow());
    }
    nrrow_p = rows_p.nrow();
    initialize();
    addInfo();
    noWrite_p = False;
  }

  ConcatTable::ConcatTable (const Block<String>& tableNames,
			    const Block<String>& subTables,
                            const String& subDirName,
			    int option,
			    const TableLock& lockOptions,
                            const TSMOption& tsmOption)
    : BaseTable       ("", Table::Scratch, 0),
      subTableNames_p (subTables),
      subDirName_p    (subDirName),
      colMap_p        (static_cast<ConcatColumn*>(0)),
      changed_p       (True)
  {
    noWrite_p = True;
    if (tableNames.nelements() == 0) {
      throw TableError("ConcatTable: at least one table has to be given");
    }
    openTables (tableNames, option, lockOptions, tsmOption);
    initialize();
    addInfo();
    noWrite_p = False;
  }

  ConcatTable::~ConcatTable()
  {
    //# When needed, write the table files if not marked for delete
    if (!isMarkedForDelete()) {
      if (openedForWrite()  &&  !shouldNotWrite()) {
	writeConcatTable (True);
      }
    }
    //# Delete all ConcatColumn objects.
    for (uInt i=0; i<colMap_p.ndefined(); i++) {
      delete colMap_p.getVal(i);
    }
    //# Unlink from root.
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      BaseTable::unlink (baseTabPtr_p[i]);
    }
  }


  void ConcatTable::addInfo()
  {
    //# The initial table info is a copy of the original.
    tableInfo() = baseTabPtr_p[0]->tableInfo();
    // Add a line for each table.
    tableInfo().readmeAddLine ("Virtual concatenation of the following tables:");
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      if (subDirName_p.empty()) {
	tableInfo().readmeAddLine ("  " + baseTabPtr_p[i]->tableName());
      } else {
	tableInfo().readmeAddLine ("  " + subDirName_p + "/" +
                                   Path(baseTabPtr_p[i]->tableName()).baseName());
      }	
    }
  }

  void ConcatTable::getPartNames (Block<String>& names, Bool recursive) const
  {
    if (recursive) {
      for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
        baseTabPtr_p[i]->getPartNames (names, recursive);
      }
    } else {
      uInt inx = names.size();
      names.resize (inx + baseTabPtr_p.nelements());
      for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
        names[inx+i] = baseTabPtr_p[i]->tableName();
      }
    }
  }

  void ConcatTable::reopenRW()
  {
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      baseTabPtr_p[i]->reopenRW();
    }
    option_p = Table::Update;
  }

  Bool ConcatTable::asBigEndian() const
  {
    return baseTabPtr_p[0]->asBigEndian();
  }

  const StorageOption& ConcatTable::storageOption() const
  {
    return baseTabPtr_p[0]->storageOption();
  }

  Bool ConcatTable::isMultiUsed (Bool) const
  {
    return False;
  }

  const TableLock& ConcatTable::lockOptions() const
  {
    return baseTabPtr_p[0]->lockOptions();
  }
  void ConcatTable::mergeLock (const TableLock& lockOptions)
  {
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      baseTabPtr_p[i]->mergeLock (lockOptions);
    }
  }
  Bool ConcatTable::hasLock (FileLocker::LockType type) const
  {
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      if (! baseTabPtr_p[i]->hasLock (type)) {
	return False;
      }
    }
    return True;
  }
  Bool ConcatTable::lock (FileLocker::LockType type, uInt nattempts)
  {
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      if (! baseTabPtr_p[i]->lock (type, nattempts)) {
	return False;
      }
    }
    return True;
  }
  void ConcatTable::unlock()
  {
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      baseTabPtr_p[i]->unlock();
    }
  }

  void ConcatTable::flush (Bool fsync, Bool recursive)
  {
    // Flush the underlying table.
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      baseTabPtr_p[i]->flush (fsync, recursive);
    }
    if (!isMarkedForDelete()) {
      if (openedForWrite()) {
	writeConcatTable (fsync);
      }
    }
  }

  void ConcatTable::resync()
  {
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      baseTabPtr_p[i]->resync();
    }
  }

  uInt ConcatTable::getModifyCounter() const
  {
    return baseTabPtr_p[0]->getModifyCounter();
  }


  //# Write a concatenate table into a file.
  void ConcatTable::writeConcatTable (Bool)
  {
    //# Write name and type of root and write object data.
    //# Do this only when something has changed.
    if (changed_p) {
      AipsIO ios;
      writeStart (ios, True);
      // writeStart has made the table directory.
      // Create the subDir directory if given.
      String sdName;
      if (! subDirName_p.empty()) {
        sdName = tableName() + '/' + subDirName_p + '/';
        Directory dir(sdName);
	dir.create();
      }
      ios << "ConcatTable";
      ios.putstart ("ConcatTable", 0);
      // Make the name of the base tables relative to this table.
      // First move a table if subDirName_p is set.
      ios << uInt(baseTabPtr_p.nelements());
      for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
        if (! subDirName_p.empty()) {
          baseTabPtr_p[i]->rename
            (sdName + Path(baseTabPtr_p[i]->tableName()).baseName(),
             Table::New);
        }
	ios << Path::stripDirectory (baseTabPtr_p[i]->tableName(),
				     tableName());
      }
      // Write the names to be concatenated.
      ios << subTableNames_p;
      ios.putend();
      writeEnd (ios);
      changed_p = False;
    }
    //# Write the TableInfo.
    flushTableInfo();
  }

  //# Read a concatenate table from a file and open the associated tables.
  void ConcatTable::getConcat (AipsIO& ios, int option,
			       const TableLock& lockOptions,
                               const TSMOption& tsmOption)
  {
    //# Open the file, read name and type of root and read object data.
    uInt nrtab;
    Block<String> rootNames;
    Int version = ios.getstart ("ConcatTable");
    AlwaysAssert (version==0, AipsError);
    ios >> nrtab;
    rootNames.resize(nrtab);
    for (uInt i=0; i<nrtab; ++i) {
      ios >> rootNames[i];
      rootNames[i] = Path::addDirectory (rootNames[i], tableName());
    }
    ios >> subTableNames_p;
    ios.getend();
    openTables (rootNames, option, lockOptions, tsmOption);
    initialize();
    //# Read the TableInfo object.
    getTableInfo();
  }

  void ConcatTable::openTables (const Block<String>& tableNames, int option,
				const TableLock& lockOptions,
                                const TSMOption& tsmOption)
  {
    //# Open the tables referenced to.
    baseTabPtr_p.resize (tableNames.nelements());
    baseTabPtr_p = 0;
    rows_p.reserve (tableNames.nelements() + 1);
    for (uInt i=0; i<tableNames.nelements(); ++i) {
      Table tab;
      if (option == Table::Old) {
	tab = Table(tableNames[i], lockOptions, Table::Old, tsmOption);
      } else {
        tab = Table(tableNames[i], lockOptions, Table::Update, tsmOption);
      }
      baseTabPtr_p[i] = tab.baseTablePtr();
      //# Link to referenced table, otherwise it will be destructed.
      baseTabPtr_p[i]->link();
      rows_p.add (tab.nrow());
    }
    nrrow_p = rows_p.nrow();
  }

  void ConcatTable::initialize()
  {
    // Check if all tables have the same description.
    // Note that we size the table instead of reserve, because push_back
    // gives the following warning for CountedPtr:
    //  "dereferencing pointer aonymous  does break strict-aliasing rule"
    vector<CountedPtr<TableDesc> > actualDesc(baseTabPtr_p.nelements());;
    Bool equalDataTypes;
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      actualDesc[i] = CountedPtr<TableDesc> (new TableDesc
					     (baseTabPtr_p[i]->actualTableDesc()));
      if (actualDesc[i]->columnDescSet().isEqual
	  (actualDesc[0]->columnDescSet(), equalDataTypes)) {
	if (equalDataTypes) {
	  continue;
	}
      }
      throw TableError("All tables in ConCatTable must have same description");
    }
    // For fixed shaped arrays check if all tables have the same shape.
    // If not, clear dimensionality and options.
    for (uInt i=0; i<actualDesc[0]->ncolumn(); ++i) {
      ColumnDesc& colDesc = actualDesc[0]->rwColumnDesc(i);
      if (colDesc.isArray()  &&
          (colDesc.options() & ColumnDesc::FixedShape) != 0) {
        Bool sameShape = true;
        for (uInt j=1; j<baseTabPtr_p.nelements(); ++j) {
          const ColumnDesc& cd = actualDesc[j]->columnDesc(i);
          if ((cd.options() & ColumnDesc::FixedShape) == 0  ||
              ! colDesc.shape().isEqual (cd.shape())) {
            sameShape = False;
            break;
          }
        }
        if (!sameShape) {
          colDesc.setNdim (0);
          colDesc.setOptions (0);
        }
      }
    }
    //# Use the table description.
    tdescPtr_p = new TableDesc (*(actualDesc[0]), TableDesc::Scratch);
    keywordSet_p = baseTabPtr_p[0]->keywordSet();
    // Create the concatColumns.
    makeConcatCol();
    // Handle the possible concatenated subtables.
    handleSubTables();
  }

  void ConcatTable::handleSubTables()
  {
    // Check for each subtable if it exists in all tables.
    // If fine, create a ConcatTable for each subtable.
    Block<Table> subtables(baseTabPtr_p.nelements());
    for (uInt i=0; i<subTableNames_p.nelements(); ++i) {
      const String& tname = subTableNames_p[i];
      for (uInt j=0; j<baseTabPtr_p.nelements(); ++j) {
	subtables[j] = baseTabPtr_p[j]->keywordSet().asTable (tname);
      }
      Table concSubtab(subtables);
      keywordSet_p.defineTable (tname, concSubtab);
    }
  }

  //# Read description and #rows.
  void ConcatTable::getLayout (TableDesc& desc, AipsIO& ios)
  {
    //# Open the file, read name and type of root and read object data.
    uInt nrtab;
    Block<String> rootNames, subNames;
    Int version = ios.getstart ("ConcatTable");
    AlwaysAssert (version==0, AipsError);
    ios >> nrtab;
    rootNames.resize(nrtab);
    for (uInt i=0; i<nrtab; ++i) {
      ios >> rootNames[i];
    }
    ios >> subNames;
    ios.getend();
    Table::getLayout (desc, rootNames[0]);
  }

  //# Create a ConcatColumn object for all columns in the description.
  //# Insert it with the name in the column map.
  void ConcatTable::makeConcatCol()
  {
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
      const ColumnDesc& cd = tdescPtr_p->columnDesc(i);
      colMap_p.define (cd.name(), cd.makeConcatColumn (this));
    }
  }

  Block<BaseColumn*> ConcatTable::getRefColumns (const String& columnName)
  {
    Block<BaseColumn*> cols(baseTabPtr_p.nelements());
    for (uInt i=0; i<cols.nelements(); ++i) {
      cols[i] = baseTabPtr_p[i]->getColumn (columnName);
    }
    return cols;
  }

  //# Test if the table is writable.
  Bool ConcatTable::isWritable() const
  {
    for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
      if (! baseTabPtr_p[i]->isWritable()) {
	return False;
      }
    }
    return True;
  }

  void ConcatTable::copy (const String& newName, int tableOption) const
  {
    if (!madeDir_p) {
      throw TableError
	("ConcatTable::copy: an unsaved table cannot be shallowly copied; "
	 "make a deep copy or save the table first");
    }
    BaseTable::copy (newName, tableOption);
  }

  void ConcatTable::deepCopy (const String& newName,
			      const Record& dataManagerInfo,
			      int tableOption, Bool, int endianFormat,
			      Bool noRows) const
  {
    trueDeepCopy (newName, dataManagerInfo, tableOption,
		  endianFormat, noRows);
  }

  int ConcatTable::tableType() const
  {
    return baseTabPtr_p[0]->tableType();
  }

  TableDesc ConcatTable::actualTableDesc() const
  {
    return *tdescPtr_p;
  }

  Record ConcatTable::dataManagerInfo() const
  {
    return baseTabPtr_p[0]->dataManagerInfo();
  }

  //# Get the keyword set.
  TableRecord& ConcatTable::keywordSet()
  {
    return keywordSet_p;
  }

  //# Get the keyword set.
  TableRecord& ConcatTable::rwKeywordSet()
  {
    return keywordSet_p;
  }

  BaseColumn* ConcatTable::getColumn (const String& columnName) const
  {
    tdescPtr_p->columnDesc(columnName);             // check if column exists
    return colMap_p(columnName);
  }
  //# We cannot simply return colMap_p.getVal(columnIndex), because the order of
  //# the columns in the description is important. So first get the column
  //# name and use that as key.
  BaseColumn* ConcatTable::getColumn (uInt columnIndex) const
  { 
    const String& name = tdescPtr_p->columnDesc(columnIndex).name();
    return colMap_p(name);
  }
    

void ConcatTable::checkAddColumn (const String& name, Bool addToParent)
{
  if (! isWritable()) {
    throw TableInvOper ("Table::addColumn; table is not writable");
  }
  if (tdescPtr_p->isColumn(name)) {
    throw TableInvOper ("Table::addColumn; column " + name + " already exists");
  }
  if (!addToParent) {
    throw TableInvOper ("ConcatTable::addColumn; column " + name +
                        " does not exist in parent table, but must not be added"
                        " (addToParent=False)");
  }
}


void ConcatTable::addColumn (const ColumnDesc& columnDesc, Bool addToParent)
{
  checkAddColumn (columnDesc.name(), addToParent);
  for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
    baseTabPtr_p[i]->addColumn (columnDesc, addToParent);
  }
  tdescPtr_p->addColumn (columnDesc);
}
void ConcatTable::addColumn (const ColumnDesc& columnDesc,
                             const String& dataManager, Bool byName,
                             Bool addToParent)
{
  checkAddColumn (columnDesc.name(), addToParent);
  for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
    baseTabPtr_p[i]->addColumn (columnDesc, dataManager, byName, addToParent); 
  }
  tdescPtr_p->addColumn (columnDesc);
}
void ConcatTable::addColumn (const ColumnDesc& columnDesc,
                             const DataManager& dataManager,
                             Bool addToParent)
{
  checkAddColumn (columnDesc.name(), addToParent);
  for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
    baseTabPtr_p[i]->addColumn (columnDesc,dataManager, addToParent);
  }
  tdescPtr_p->addColumn (columnDesc);
}
void ConcatTable::addColumn (const TableDesc& tableDesc,
                             const DataManager& dataManager,
                             Bool addToParent)
{
  // First check if all columns exist and can be added or not.
  // Collect all columns to be added to the parent.
  for (uInt i=0; i<tableDesc.ncolumn(); ++i) {
    checkAddColumn (tableDesc[i].name(), addToParent);
  }
  // Add to the parents.
  for (uInt i=0; i<baseTabPtr_p.nelements(); ++i) {
    baseTabPtr_p[i]->addColumn (tableDesc, dataManager, addToParent);
  }
  for (uInt i=0; i<tableDesc.ncolumn(); ++i) {
    tdescPtr_p->addColumn (tableDesc[i]);
  }
}

  //# Rows and columns cannot be removed and renamed.
  Bool ConcatTable::canRemoveRow() const
  { return False; }
  Bool ConcatTable::canRemoveColumn (const Vector<String>&) const
  { return False; }
  Bool ConcatTable::canRenameColumn (const String&) const
  { return False; }

  void ConcatTable::removeRow (uInt)
  {
    throw TableInvOper("ConcatTable cannot remove rows");
  }

  void ConcatTable::removeColumn (const Vector<String>&)
  {
    throw TableInvOper("ConcatTable cannot remove columns");
  }
 
  void ConcatTable::renameColumn (const String&, const String&)
  {
    throw TableInvOper("ConcatTable cannot rename columns");
  }

  void ConcatTable::renameHypercolumn (const String&, const String&)
  {
    throw TableInvOper("ConcatTable cannot rename hypercolumns");
  }


  DataManager* ConcatTable::findDataManager (const String& name,
                                             Bool byColumn) const
  {
    return baseTabPtr_p[0]->findDataManager (name, byColumn);
  }

  void ConcatTable::showStructureExtra (std::ostream& os) const
  {
    for (uInt i=0; i<baseTabPtr_p.size(); ++i) {
      os << (i==0 ? "concat " : "       ");
      os << baseTabPtr_p[i]->tableName() << " (" 
         << baseTabPtr_p[i]->nrow() << " rows, "
         << baseTabPtr_p[i]->tableDesc().ncolumn() << " columns)" << endl;
    }
  }

} //# NAMESPACE CASACORE - END
