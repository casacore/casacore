//# TableCache.cc: Cache of open tables
//# Copyright (C) 1994,1995,1997,1999
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

#include <casacore/tables/Tables/TableCache.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/PlainTable.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableCache::TableCache()
: tableMap_p(static_cast<void*>(0))
{}

TableCache::~TableCache()
{}

PlainTable* TableCache::operator() (const String& tableName) const
{
    ScopedMutexLock sc(itsMutex);
    return getTable (tableName);
}

PlainTable* TableCache::getTable (const String& tableName) const
{
    PlainTable** ptr = (PlainTable**)(tableMap_p.isDefined (tableName));
    if (ptr) {
	return *ptr;
    }
    return 0;
}

void TableCache::define (const String& tableName, PlainTable* tab)
{
    ScopedMutexLock sc(itsMutex);
    tableMap_p.define (tableName, tab);
}

void TableCache::remove (const String& tableName)
{
    // For static Table objects it is possible that the cache is
    // deleted before the Table.
    // Therefore do not delete if the map is already empty
    // (otherwise an exception is thrown).
    ScopedMutexLock sc(itsMutex);
    if (tableMap_p.ndefined() > 0) {
      try {
	tableMap_p.remove (tableName);
      } catch (AipsError&) {
	// Something strange has happened.
	// Throwing an exception causes an immediate crash (probably by
	// Table destructors).
	// So write a message on stderr;
	std::cerr << "Cannot remove table " << tableName
                  << " from the table cache; suggest restarting" << std::endl;
      }
    }
}

void TableCache::rename (const String& newName, const String& oldName)
{
    ScopedMutexLock sc(itsMutex);
    if (tableMap_p.isDefined (oldName)) {
	tableMap_p.rename (newName, oldName);
    }
}

uInt TableCache::nAutoLocks()
{
    ScopedMutexLock sc(itsMutex);
    uInt n=0;
    uInt ntab = tableMap_p.ndefined();
    for (uInt i=0; i<ntab; i++) {
	PlainTable& table = *static_cast<PlainTable*>(tableMap_p.getVal(i));
	if (table.lockOptions().option() == TableLock::AutoLocking) {
	    //# Having a read lock is enough.
	    if (table.hasLock (FileLocker::Read)) {
		n++;
	    }
	}
    }
    return n;
}

void TableCache::relinquishAutoLocks (Bool all)
{
    ScopedMutexLock sc(itsMutex);
    uInt ntab = tableMap_p.ndefined();
    for (uInt i=0; i<ntab; i++) {
	PlainTable& table = *static_cast<PlainTable*>(tableMap_p.getVal(i));
	if (table.lockOptions().option() == TableLock::AutoLocking) {
	    //# Having a read lock is enough.
	    if (table.hasLock (FileLocker::Read)) {
		if (all) {
		    table.unlock();
		}else{
		    table.autoReleaseLock (True);
		}
	    }
	}
    }
}

Vector<String> TableCache::getTableNames() const
{
    ScopedMutexLock sc(itsMutex);
    uInt ntab = tableMap_p.ndefined();
    Vector<String> names(ntab);
    for (uInt i=0; i<ntab; i++) {
	PlainTable& table = *static_cast<PlainTable*>(tableMap_p.getVal(i));
        names[i] = table.tableName();
    }
    return names;
}

Vector<String> TableCache::getLockedTables (FileLocker::LockType lockType,
                                            int lockOption)
{
    ScopedMutexLock sc(itsMutex);
    vector<String> names;
    uInt ntab = tableMap_p.ndefined();
    for (uInt i=0; i<ntab; i++) {
	PlainTable& table = *static_cast<PlainTable*>(tableMap_p.getVal(i));
	if (lockOption < 0  ||  table.lockOptions().option() == lockOption) {
	    if (table.hasLock (lockType)) {
                names.push_back (table.tableName());
	    }
	}
    }
    return Vector<String>(names);
}

void TableCache::flushTable (const String& name,
                             Bool fsync, Bool recursive)
{
  ScopedMutexLock sc(itsMutex);
  PlainTable* tab = getTable(name);
  if (tab) {
    tab->flush (fsync, recursive);
  }
}

PlainTable* TableCache::lookCache (const String& name, int tableOption,
                                   const TableLock& lockOptions)
{
    //# Exit if table is not in cache yet.
    PlainTable* btp = this->operator()(name);
    if (btp == 0) {
	return btp;
    }
    //# Check if option matches. It does if equal.
    //# Otherwise it does if option in cached table is "more".
    //# Note that class PlainTable already throws an exception if
    //# a new table is created with the same name as an open table.
    int cachedTableOption = btp->tableOption();
    if ((tableOption == cachedTableOption)
    ||  ((cachedTableOption == Table::New
      ||  cachedTableOption == Table::NewNoReplace
      ||  cachedTableOption == Table::Update)
     &&  (tableOption == Table::Update
      ||  tableOption == Table::Old))) {
	btp->mergeLock (lockOptions);
	return btp;
    }
    if (cachedTableOption == Table::Old  &&  tableOption == Table::Update) {
	btp->mergeLock (lockOptions);
	btp->reopenRW();
	return btp;
    }
    throw (TableInvOper ("Table " + name +
			 " cannot be opened/created (already in cache)"));
}


} //# NAMESPACE CASACORE - END

