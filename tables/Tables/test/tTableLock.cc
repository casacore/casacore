//# tTableLock.cc: Test TableLock class
//# Copyright (C) 2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/tables/Tables/TableLock.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the TableLock class.
// </summary>

#ifdef AIPS_TABLE_NOLOCKING
void checkLockOption (const TableLock& lock, TableLock::LockOption,
                      Bool, Bool)
{
    AlwaysAssertExit (lock.option() == TableLock::NoLocking);
    AlwaysAssertExit (! lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
}
#else
void checkLockOption (const TableLock& lock, TableLock::LockOption opt,
                      Bool readLock, Bool permLock)
{
    AlwaysAssertExit (lock.option() == opt);
    AlwaysAssertExit (lock.readLocking() == readLock);
    AlwaysAssertExit (lock.isPermanent() == permLock);
}
#endif

int main()
{
  {
    TableLock lock;
    checkLockOption (lock, TableLock::AutoLocking, True, False);
    AlwaysAssertExit (lock.interval() == 5);
    AlwaysAssertExit (lock.maxWait() == 0);
  }
  {
    TableLock lock(TableLock::AutoLocking);
    checkLockOption (lock, TableLock::AutoLocking, True, False);
    AlwaysAssertExit (lock.interval() == 5);
    AlwaysAssertExit (lock.maxWait() == 0);
  }
  {
    TableLock lock(TableLock::AutoNoReadLocking, 10, 1);
    checkLockOption (lock, TableLock::AutoLocking, False, False);
    AlwaysAssertExit (lock.interval() == 10);
    AlwaysAssertExit (lock.maxWait() == 1);
  }
  {
    TableLock lock(TableLock::UserLocking);
    checkLockOption (lock, TableLock::UserLocking, True, False);
  }
  {
    TableLock lock(TableLock::UserNoReadLocking);
    checkLockOption (lock, TableLock::UserLocking, False, False);
  }
  {
    TableLock lock(TableLock::PermanentLocking);
    checkLockOption (lock, TableLock::PermanentLocking, True, True);
  }
  {
    TableLock lock(TableLock::PermanentLockingWait);
    checkLockOption (lock, TableLock::PermanentLockingWait, True, True);
  }
  {
    TableLock lock1(TableLock::AutoNoReadLocking, 10, 1);
    TableLock lock2(TableLock::PermanentLockingWait);
    TableLock lock3(lock2);
    checkLockOption (lock3, TableLock::PermanentLockingWait, True, True);
    lock2 = lock1;
    checkLockOption (lock2, TableLock::AutoLocking, False, False);
    AlwaysAssertExit (lock2.interval() == 10);
    AlwaysAssertExit (lock2.maxWait() == 1);
  }

  // Test merging.
  {
    TableLock lock;
    lock.merge (TableLock());
    checkLockOption (lock, TableLock::AutoLocking, True, False);
    AlwaysAssertExit (lock.interval() == 5);
    AlwaysAssertExit (lock.maxWait() == 0);
    lock.merge (TableLock (TableLock::AutoNoReadLocking, 10, 1));
    checkLockOption (lock, TableLock::AutoLocking, True, False);
    AlwaysAssertExit (lock.interval() == 10);
    AlwaysAssertExit (lock.maxWait() == 1);
  }
  {
    TableLock lock (TableLock::PermanentLockingWait);
    lock.merge (TableLock());
    checkLockOption (lock, TableLock::PermanentLockingWait, True, True);
    lock.merge (TableLock(TableLock::AutoNoReadLocking));
    checkLockOption (lock, TableLock::PermanentLockingWait, True, True);
  }
  {
    TableLock lock (TableLock::UserLocking);
    lock.merge (TableLock());
    checkLockOption (lock, TableLock::UserLocking, True, False);
    ////checkLockOption (lock, TableLock::AutoLocking, True, False);
    lock.merge (TableLock(TableLock::AutoLocking, 20, 2));
    checkLockOption (lock, TableLock::AutoLocking, True, False);
    AlwaysAssertExit (lock.interval() == 20);
    AlwaysAssertExit (lock.maxWait() == 2);
    lock.merge (TableLock(TableLock::PermanentLockingWait));
    checkLockOption (lock, TableLock::PermanentLockingWait, True, True);
    lock.merge (TableLock(TableLock::PermanentLocking));
    checkLockOption (lock, TableLock::PermanentLocking, True, True);
  }
  {
    TableLock lock (TableLock::NoLocking);
    lock.merge (TableLock());
    checkLockOption (lock, TableLock::NoLocking, False, False);
    lock.merge (TableLock(TableLock::AutoLocking, 20, 2));
    checkLockOption (lock, TableLock::AutoLocking, True, False);
  }

  return 0;                           // exit with success status
}
