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

#ifndef TABLES_NULLTABLE_H
#define TABLES_NULLTABLE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/BaseTable.h>
#include <casacore/tables/Tables/TableError.h>


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
// an underlying table.
// </etymology>

// <synopsis> 
// NullTable is used to represent a null table.
// The default Table constructor used to a create a null pointer
// which resulted in core dumps when the Table object was actually used.
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
  virtual Bool isNull() const override;

  // All functions throw a "null table" exception.
  // <group>
  virtual void reopenRW() override;
  virtual Bool asBigEndian() const override;
  virtual const StorageOption& storageOption() const override;
  virtual Bool isMultiUsed (Bool checkSubTable) const override;
  virtual const TableLock& lockOptions() const override;
  virtual void mergeLock (const TableLock& lockOptions) override;
  virtual Bool hasLock (FileLocker::LockType) const override;
  virtual Bool lock (FileLocker::LockType, uInt nattempts) override;
  virtual void unlock() override;
  virtual void flush (Bool fsync, Bool recursive) override;
  virtual void resync() override;
  virtual uInt getModifyCounter() const override;
  virtual Bool isWritable() const override;
  virtual void deepCopy (const String& newName,
			 const Record& dataManagerInfo,
                         const StorageOption&,
			 int tableOption,
			 Bool valueCopy,
			 int endianFormat,
			 Bool noRows) const override;
  virtual TableDesc actualTableDesc() const override;
  virtual Record dataManagerInfo() const override;
  virtual TableRecord& keywordSet() override;
  virtual TableRecord& rwKeywordSet() override;
  virtual BaseColumn* getColumn (uInt columnIndex) const override;
  virtual BaseColumn* getColumn (const String& columnName) const override;
  virtual Bool canAddRow() const override;
  virtual void addRow (rownr_t nrrow, Bool initialize) override;
  virtual Bool canRemoveRow() const override;
  virtual void removeRow (rownr_t rownr) override;
  virtual DataManager* findDataManager (const String& name,
                                        Bool byColumn) const override;
  virtual void addColumn (const ColumnDesc& columnDesc, Bool addToParent) override;
  virtual void addColumn (const ColumnDesc& columnDesc,
			  const String& dataManager, Bool byName,
                          Bool addToParent) override;
  virtual void addColumn (const ColumnDesc& columnDesc,
			  const DataManager& dataManager, Bool addToParent) override;
  virtual void addColumn (const TableDesc& tableDesc,
			  const DataManager& dataManager, Bool addToParent) override;
  virtual Bool canRemoveColumn (const Vector<String>& columnNames) const override;
  virtual void removeColumn (const Vector<String>& columnNames) override;
  virtual Bool canRenameColumn (const String& columnName) const override;
  virtual void renameColumn (const String& newName, const String& oldName) override;
  virtual void renameHypercolumn (const String& newName,
				    const String& oldName) override;
  virtual Vector<rownr_t> rowNumbers() const override;
  virtual BaseTable* root() override;
  virtual Bool rowOrder() const override;
  virtual Vector<rownr_t>& rowStorage() override;
  virtual Bool adjustRownrs (rownr_t nrrow, Vector<rownr_t>& rownrs,
			     Bool determineOrder) const override;
  virtual std::shared_ptr<BaseTable> doSort (PtrBlock<BaseColumn*>&,
                                             const Block<CountedPtr<BaseCompare> >&,
                                             const Block<Int>&,
                                             int,
                                             std::shared_ptr<Vector<rownr_t>>,
                                             std::shared_ptr<Vector<size_t>>) override;
  virtual void renameSubTables (const String& newName,
				const String& oldName) override;
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

  // Make an exception message with the name of the function.
  TableError makeError (const String& name) const;
};




} //# NAMESPACE CASACORE - END

#endif
