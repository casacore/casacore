//# NullTable.cc: Class indicating a null Table object
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

#include <casacore/tables/Tables/NullTable.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

NullTable::NullTable()
: BaseTable ("Null table object", Table::Old, 0)
{
  delete_p = False;
}

NullTable::~NullTable()
{}

Bool NullTable::isNull() const
{
  return True;
}

void NullTable::reopenRW()
{
  throw makeError ("reopenRW");
}

Bool NullTable::asBigEndian() const
{
  throw makeError ("asBigEndian");
}

Bool NullTable::isMultiUsed (Bool) const
{
  throw makeError ("isMultiUsed");
}

const StorageOption& NullTable::storageOption() const
{
  throw makeError ("storageOption");
}

const TableLock& NullTable::lockOptions() const
{
  throw makeError ("lockOptions");
}

void NullTable::mergeLock (const TableLock&)
{
  throw makeError ("mergeLoc");
}

Bool NullTable::hasLock (FileLocker::LockType) const
{
  throw makeError ("hasLock");
}

Bool NullTable::lock (FileLocker::LockType, uInt)
{
  throw makeError ("lock");
}

void NullTable::unlock()
{
  throw makeError ("unlock");
}

void NullTable::flush (Bool, Bool)
{
  throw makeError ("flush");
}

void NullTable::resync()
{
  throw makeError ("resync");
}

uInt NullTable::getModifyCounter() const
{
  throw makeError ("getModifyCounter");
}

Bool NullTable::isWritable() const
{
  throw makeError ("isWritable");
}

void NullTable::deepCopy (const String&, const Record&,
                          const StorageOption&, int, Bool,
			  int, Bool) const
{
  throw makeError ("deepCopy");
}

TableDesc NullTable::actualTableDesc() const
{
  throw makeError ("actualTableDesc");
}

Record NullTable::dataManagerInfo() const
{
  throw makeError ("dataManagerInfo");
}

TableRecord& NullTable::keywordSet()
{
  throw makeError ("keywordSet");
}

TableRecord& NullTable::rwKeywordSet()
{
  throw makeError ("rwKeywordSet");
}

BaseColumn* NullTable::getColumn (uInt) const
{
  throw makeError ("getColumn");
}

BaseColumn* NullTable::getColumn (const String&) const
{
  throw makeError ("getColumn");
}

Bool NullTable::canAddRow() const
{
  throw makeError ("canAddRow");
}

void NullTable::addRow (rownr_t, Bool)
{
  throw makeError ("addRow");
}

Bool NullTable::canRemoveRow() const
{
  throw makeError ("canRemoveRow");
}

void NullTable::removeRow (rownr_t)
{
  throw makeError ("removeRow");
}

DataManager* NullTable::findDataManager (const String&, Bool) const
{
  throw makeError ("findDataManager");
}

void NullTable::addColumn (const ColumnDesc&, Bool)
{
  throw makeError ("addColumn");
}

void NullTable::addColumn (const ColumnDesc&,
			   const String&, Bool, Bool)
{
  throw makeError ("addColumn");
}

void NullTable::addColumn (const ColumnDesc&,
			   const DataManager&, Bool)
{
  throw makeError ("addColumn");
}

void NullTable::addColumn (const TableDesc& ,
			   const DataManager&, Bool)
{
  throw makeError ("addColumn");
}

Bool NullTable::canRemoveColumn (const Vector<String>&) const
{
  throw makeError ("canRemoveColumn");
}

void NullTable::removeColumn (const Vector<String>&)
{
  throw makeError ("removeColumn");
}

Bool NullTable::canRenameColumn (const String&) const
{
  throw makeError ("canRenameColumn");
}

void NullTable::renameColumn (const String&, const String&)
{
  throw makeError ("renameColumn");
}

void NullTable::renameHypercolumn (const String&, const String&)
{
  throw makeError ("renameHypercolumn");
}

Vector<rownr_t> NullTable::rowNumbers() const
{
  throw makeError ("rowNumbers");
}

BaseTable* NullTable::root()
{
  throw makeError ("root");
}

Bool NullTable::rowOrder() const
{
  throw makeError ("rowOrder");
}

Vector<rownr_t>& NullTable::rowStorage()
{
  throw makeError ("rowStorage");
}

Bool NullTable::adjustRownrs (rownr_t, Vector<rownr_t>&,
			      Bool) const
{
  throw makeError ("adjustRownrs");
}

  std::shared_ptr<BaseTable> NullTable::doSort (PtrBlock<BaseColumn*>&,
                                                const Block<CountedPtr<BaseCompare> >&,
                                                const Block<Int>&,
                                                int,
                                                std::shared_ptr<Vector<rownr_t>>,
                                                std::shared_ptr<Vector<size_t>>)
{
  throw makeError ("doSort");
}

void NullTable::renameSubTables (const String&,
				 const String&)
{
  throw makeError ("renameSubTables");
}


TableError NullTable::makeError (const String& name) const
{
  return TableError ("NullTable::" + name + " - Table object is empty");
}

} //# NAMESPACE CASACORE - END

