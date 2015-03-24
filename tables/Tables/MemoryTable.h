//# MemoryTable.h: Class for a table held in memory
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

#ifndef TABLES_MEMORYTABLE_H
#define TABLES_MEMORYTABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class SetupNewTable;
class ColumnSet;
class TableLockData;


// <summary>
// Class for a table held in memory
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> BaseTable
// </prerequisite>

// <synopsis> 
// MemoryTable holds all its data in memory.
// It means that the data is not persistent. However, it can be copied to
// another table to make the data persistent.
// Furthermore it is a table as all other tables, so all table functions
// can be applied to it. Some functions (e.g. lock) won't do anything.
// Also all table operations like sorting, selecting, and iterating can
// be used.
//
// The constructor accepts a SetupNewTable object which can contain
// bindings of columns to any data manager. All bindings to storage
// managers will be replaced by a binding to the memory based storage
// manager <linkto class=MemoryStMan>MemoryStMan</linkto>. Also all
// unbound columns will be bound to MemoryStMan.
// Thus it is still possible that a column is bound to a virtual column
// engine like <linkto class=CompressComplex>CompressComplex</linkto>.
// </synopsis> 

//# <todo asof="$DATE:$">
//# </todo>


class MemoryTable : public BaseTable
{
public:

  // Create the table in memory using the definitions in the
  // SetupNewTable object.
  MemoryTable (SetupNewTable&, uInt nrrow, Bool initialize);

  // The destructor deletes all data.
  virtual ~MemoryTable();

  // Try to reopen the table (the underlying one) for read/write access.
  // It does nothing.
  virtual void reopenRW();

  // Is the table stored in big or little endian format?
  // It returns the endian format of the machine.
  virtual Bool asBigEndian() const;

  // Get the storage option used for the table.
  virtual const StorageOption& storageOption() const;

  // Is the table in use (i.e. open) in another process?
  // It always returns False.
  virtual Bool isMultiUsed (Bool checkSubTable) const;

  // Get the locking info.
  // It returns PermanentLocking.
  virtual const TableLock& lockOptions() const;

  // Merge the given lock info with the existing one.
  // It does nothing.
  virtual void mergeLock (const TableLock& lockOptions);

  // Has this process the read or write lock, thus can the table
  // be read or written safely?
  // It always returns True.
  virtual Bool hasLock (FileLocker::LockType) const;

  // Locking the table is a no-op.
  virtual Bool lock (FileLocker::LockType, uInt nattempts);

  // Unlocking the table is a no-op.
  virtual void unlock();

  // Flushing the table is a no-op.
  virtual void flush (Bool fsync, Bool recursive);

  // Resyncing the Table is a no-op.
  virtual void resync();

  // Get the modify counter. It always returns 0.
  virtual uInt getModifyCounter() const;

  // Test if the table is opened as writable. It always returns True.
  virtual Bool isWritable() const;

  // Copy the table and all its subtables.
  // It copies the contents of each row to get a real copy.
  // <group>
  virtual void copy (const String& newName, int tableOption) const;
  virtual void deepCopy (const String& newName,
			 const Record& dataManagerInfo,
			 int tableOption, Bool, int endianFormat,
			 Bool noRows) const;
  // </group>

  // Rename the table. The tableOption is ignored.
  virtual void rename (const String& newName, int tableOption);

  // Get the table type (Table::Memory).
  virtual int tableType() const;

  // Get the actual table description.
  virtual TableDesc actualTableDesc() const;

  // Get the data manager info.
  virtual Record dataManagerInfo() const;

  // Get readonly access to the table keyword set.
  virtual TableRecord& keywordSet();

  // Get read/write access to the table keyword set.
  virtual TableRecord& rwKeywordSet();

  // Write the TableInfo object. It does not do anything.
  virtual void flushTableInfo();

  // Get a column object using its index.
  virtual BaseColumn* getColumn (uInt columnIndex) const;

  // Get a column object using its name.
  virtual BaseColumn* getColumn (const String& columnName) const;

  // Test if it is possible to add a row to this table (yes).
  virtual Bool canAddRow() const;

  // Add one or more rows and possibly initialize them.
  // This will fail for tables not supporting addition of rows.
  virtual void addRow (uInt nrrow = 1, Bool initialize = True);

  // Test if it is possible to remove a row from this table (yes).
  virtual Bool canRemoveRow() const;

  // Remove the given row.
  virtual void removeRow (uInt rownr);

  // Add a column to the table.
  // If the DataManager is not a virtual engine, MemoryStMan will be used.
  // The last Bool argument is not used in MemoryTable, but can be used in
  // other classes derived from BaseTable.
  // <group>
  virtual void addColumn (const ColumnDesc& columnDesc, Bool addToParent);
  virtual void addColumn (const ColumnDesc& columnDesc,
			  const String& dataManager, Bool byName,
                          Bool addToParent);
  virtual void addColumn (const ColumnDesc& columnDesc,
			  const DataManager& dataManager, Bool addToParent);
  virtual void addColumn (const TableDesc& tableDesc,
			  const DataManager& dataManager, Bool addToParent);
  // </group>

  // Test if columns can be removed (yes).
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
  // There is only one storage manager (MemoryStMan) with name MSM.
  virtual DataManager* findDataManager (const String& name,
                                        Bool byColumn) const;


private:
  ColumnSet*     colSetPtr_p;        //# pointer to set of columns
  TableLockData* lockPtr_p;          //# pointer to lock object

  // Copy constructor is forbidden, because copying a table requires
  // some more knowledge (like table name of result).
  // Declaring it private, makes it unusable.
  MemoryTable (const MemoryTable&);

  // Assignment is forbidden, because copying a table requires
  // some more knowledge (like table name of result).
  // Declaring it private, makes it unusable.
  MemoryTable& operator= (const MemoryTable&);

  // Setup the main parts of the object.
  // <br>Create the initial name map from the table description.
  // This map maps a name to the name in the original table.
  // A rename might change the map.
  // <br>Create the RefColumn objects.
  // <br>Create the initial TableInfo as a copy of the original BaseTable.
  void setup (BaseTable* btp);
};



} //# NAMESPACE CASACORE - END

#endif
