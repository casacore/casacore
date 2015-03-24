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
//#
//# $Id$

#include <casacore/tables/Tables/NullTable.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableError.h>
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
  throwError ("reopenRW");
}

Bool NullTable::asBigEndian() const
{
  throwError ("asBigEndian");
  return True;
}

Bool NullTable::isMultiUsed (Bool) const
{
  throwError ("isMultiUsed");
  return False;
}

const StorageOption& NullTable::storageOption() const
{
  throwError ("storageOption");
  return storageOption();          // to satisfy compiler
}

const TableLock& NullTable::lockOptions() const
{
  throwError ("lockOptions");
  return lockOptions();          // to satisfy compiler
}

void NullTable::mergeLock (const TableLock&)
{
  throwError ("mergeLoc");
}

Bool NullTable::hasLock (FileLocker::LockType) const
{
  throwError ("hasLock");
  return False;
}

Bool NullTable::lock (FileLocker::LockType, uInt)
{
  throwError ("lock");
  return False;
}

void NullTable::unlock()
{
  throwError ("unlock");
}

void NullTable::flush (Bool, Bool)
{
  throwError ("flush");
}

void NullTable::resync()
{
  throwError ("resync");
}

uInt NullTable::getModifyCounter() const
{
  throwError ("getModifyCounter");
  return 0;
}

Bool NullTable::isWritable() const
{
  throwError ("isWritable");
  return False;
}

void NullTable::deepCopy (const String&, const Record&, int, Bool,
			  int, Bool) const
{
  throwError ("deepCopy");
}

TableDesc NullTable::actualTableDesc() const
{
  throwError ("actualTableDesc");
  return actualTableDesc();       // to satisfy compiler
}

Record NullTable::dataManagerInfo() const
{
  throwError ("dataManagerInfo");
  return dataManagerInfo();       // to satisfy compiler
}

TableRecord& NullTable::keywordSet()
{
  throwError ("keywordSet");
  return keywordSet();            // to satisfy compiler
}

TableRecord& NullTable::rwKeywordSet()
{
  throwError ("rwKeywordSet");
  return rwKeywordSet();          // to satisfy compiler
}

BaseColumn* NullTable::getColumn (uInt) const
{
  throwError ("getColumn");
  return 0;
}

BaseColumn* NullTable::getColumn (const String&) const
{
  throwError ("getColumn");
  return 0;
}

Bool NullTable::canAddRow() const
{
  throwError ("canAddRow");
  return False;
}

void NullTable::addRow (uInt, Bool)
{
  throwError ("addRow");
}

Bool NullTable::canRemoveRow() const
{
  throwError ("canRemoveRow");
  return False;
}

void NullTable::removeRow (uInt)
{
  throwError ("removeRow");
}

DataManager* NullTable::findDataManager (const String&, Bool) const
{
  throwError ("findDataManager");
  return 0;
}

void NullTable::addColumn (const ColumnDesc&, Bool)
{
  throwError ("addColumn");
}

void NullTable::addColumn (const ColumnDesc&,
			   const String&, Bool, Bool)
{
  throwError ("addColumn");
}

void NullTable::addColumn (const ColumnDesc&,
			   const DataManager&, Bool)
{
  throwError ("addColumn");
}

void NullTable::addColumn (const TableDesc& ,
			   const DataManager&, Bool)
{
  throwError ("addColumn");
}

Bool NullTable::canRemoveColumn (const Vector<String>&) const
{
  throwError ("canRemoveColumn");
  return False;
}

void NullTable::removeColumn (const Vector<String>&)
{
  throwError ("removeColumn");
}

Bool NullTable::canRenameColumn (const String&) const
{
  throwError ("canRenameColumn");
  return False;
}

void NullTable::renameColumn (const String&, const String&)
{
  throwError ("renameColumn");
}

void NullTable::renameHypercolumn (const String&, const String&)
{
  throwError ("renameHypercolumn");
}

Vector<uInt> NullTable::rowNumbers() const
{
  throwError ("rowNumbers");
  return Vector<uInt>();
}

BaseTable* NullTable::root()
{
  throwError ("root");
  return 0;
}

Bool NullTable::rowOrder() const
{
  throwError ("rowOrde");
  return False;
}

Vector<uInt>* NullTable::rowStorage()
{
  throwError ("rowStorage");
  return 0;
}

Bool NullTable::adjustRownrs (uInt, Vector<uInt>&,
			      Bool) const
{
  throwError ("adjustRownrs");
  return False;
}

BaseTable* NullTable::doSort (PtrBlock<BaseColumn*>&,
			      const Block<CountedPtr<BaseCompare> >&,
			      const Block<Int>&,
			      int)
{
  throwError ("doSort");
  return 0;
}

void NullTable::renameSubTables (const String&,
				 const String&)
{
  throwError ("renameSubTables");
}


void NullTable::throwError (const String& name) const
{
  throw TableError ("NullTable::" + name + " - Table object is empty");
}

} //# NAMESPACE CASACORE - END

