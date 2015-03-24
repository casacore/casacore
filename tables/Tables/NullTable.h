//# NullTable.h: Class indicating a null Table object
//# Copyright (C) 2001,2002,2003
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

#ifndef TABLES_NULLTABLE_H
#define TABLES_NULLTABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class indicating a null Table object
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=BaseTable>BaseTable</linkto>
// </prerequisite>

// <etymology>
// NullTable represents a null table object, i.e. a Table object without
// an underlying table..
// </etymology>

// <synopsis> 
// Nullable is used to represent a null table.
// The default Table constructor used to a create a null pointer
// which results in core dumps when the Table object is actually used.
// The NullTable object makes it possible to catch such cases
// and throw an appropriate exception.
// </synopsis> 


class NullTable : public BaseTable
{
public:
  // Default constructor.
  NullTable();

  virtual ~NullTable();

  // The table is a null table.
  virtual Bool isNull() const;

  // All functions throw a "null table" exception.
  // <group>
  virtual void reopenRW();
  virtual Bool asBigEndian() const;
  virtual const StorageOption& storageOption() const;
  virtual Bool isMultiUsed (Bool checkSubTable) const;
  virtual const TableLock& lockOptions() const;
  virtual void mergeLock (const TableLock& lockOptions);
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual void flush (Bool fsync, Bool recursive);
  virtual void resync();
  virtual uInt getModifyCounter() const;
  virtual Bool isWritable() const;
  virtual void deepCopy (const String& newName,
			 const Record& dataManagerInfo,
			 int tableOption,
			 Bool valueCopy,
			 int endianFormat,
			 Bool noRows) const;
  virtual TableDesc actualTableDesc() const;
  virtual Record dataManagerInfo() const;
  virtual TableRecord& keywordSet();
  virtual TableRecord& rwKeywordSet();
  virtual BaseColumn* getColumn (uInt columnIndex) const;
  virtual BaseColumn* getColumn (const String& columnName) const;
  virtual Bool canAddRow() const;
  virtual void addRow (uInt nrrow, Bool initialize);
  virtual Bool canRemoveRow() const;
  virtual void removeRow (uInt rownr);
  virtual DataManager* findDataManager (const String& name,
                                        Bool byColumn) const;
  virtual void addColumn (const ColumnDesc& columnDesc, Bool addToParent);
  virtual void addColumn (const ColumnDesc& columnDesc,
			  const String& dataManager, Bool byName,
                          Bool addToParent);
  virtual void addColumn (const ColumnDesc& columnDesc,
			  const DataManager& dataManager, Bool addToParent);
  virtual void addColumn (const TableDesc& tableDesc,
			  const DataManager& dataManager, Bool addToParent);
  virtual Bool canRemoveColumn (const Vector<String>& columnNames) const;
  virtual void removeColumn (const Vector<String>& columnNames);
  virtual Bool canRenameColumn (const String& columnName) const;
  virtual void renameColumn (const String& newName, const String& oldName);
  virtual void renameHypercolumn (const String& newName,
				    const String& oldName);
  virtual Vector<uInt> rowNumbers() const;
  virtual BaseTable* root();
  virtual Bool rowOrder() const;
  virtual Vector<uInt>* rowStorage();
  virtual Bool adjustRownrs (uInt nrrow, Vector<uInt>& rownrs,
			     Bool determineOrder) const;
  virtual BaseTable* doSort (PtrBlock<BaseColumn*>&,
			     const Block<CountedPtr<BaseCompare> >&,
			     const Block<Int>& sortOrder,
			     int sortOption);
  virtual void renameSubTables (const String& newName,
				const String& oldName);
  // </group>

private:
  // Copy constructor is forbidden, because copying a table requires
  // some more knowledge (like table name of result).
  // Declaring it private, makes it unusable.
  NullTable (const NullTable&);

  // Assignment is forbidden, because copying a table requires
  // some more knowledge (like table name of result).
  // Declaring it private, makes it unusable.
  NullTable& operator= (const NullTable&);

  // Throw an exception with the name of the function.
  void throwError (const String& name) const;
};




} //# NAMESPACE CASACORE - END

#endif
