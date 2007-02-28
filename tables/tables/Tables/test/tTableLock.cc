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

#include <tables/Tables/TableLock.h>
#include <casa/Utilities/Assert.h>

#include <casa/namespace.h>
// <summary>
// Test program for the TableLock class.
// </summary>


int main()
{
  {
    TableLock lock;
    AlwaysAssertExit (lock.option() == TableLock::AutoLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
    AlwaysAssertExit (lock.interval() == 5);
    AlwaysAssertExit (lock.maxWait() == 0);
  }
  {
    TableLock lock(TableLock::AutoLocking);
    AlwaysAssertExit (lock.option() == TableLock::AutoLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
    AlwaysAssertExit (lock.interval() == 5);
    AlwaysAssertExit (lock.maxWait() == 0);
  }
  {
    TableLock lock(TableLock::AutoNoReadLocking, 10, 1);
    AlwaysAssertExit (lock.option() == TableLock::AutoLocking);
    AlwaysAssertExit (! lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
    AlwaysAssertExit (lock.interval() == 10);
    AlwaysAssertExit (lock.maxWait() == 1);
  }
  {
    TableLock lock(TableLock::UserLocking);
    AlwaysAssertExit (lock.option() == TableLock::UserLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
  }
  {
    TableLock lock(TableLock::UserNoReadLocking);
    AlwaysAssertExit (lock.option() == TableLock::UserLocking);
    AlwaysAssertExit (! lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
  }
  {
    TableLock lock(TableLock::PermanentLocking);
    AlwaysAssertExit (lock.option() == TableLock::PermanentLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (lock.isPermanent());
  }
  {
    TableLock lock(TableLock::PermanentLockingWait);
    AlwaysAssertExit (lock.option() == TableLock::PermanentLockingWait);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (lock.isPermanent());
  }
  {
    TableLock lock1(TableLock::AutoNoReadLocking, 10, 1);
    TableLock lock2(TableLock::PermanentLockingWait);
    TableLock lock3(lock2);
    AlwaysAssertExit (lock3.option() == TableLock::PermanentLockingWait);
    AlwaysAssertExit (lock3.readLocking());
    AlwaysAssertExit (lock3.isPermanent());
    lock2 = lock1;
    AlwaysAssertExit (lock2.option() == TableLock::AutoLocking);
    AlwaysAssertExit (! lock2.readLocking());
    AlwaysAssertExit (! lock2.isPermanent());
    AlwaysAssertExit (lock2.interval() == 10);
    AlwaysAssertExit (lock2.maxWait() == 1);
  }

  // Test merging.
  {
    TableLock lock;
    lock.merge (TableLock());
    AlwaysAssertExit (lock.option() == TableLock::AutoLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
    AlwaysAssertExit (lock.interval() == 5);
    AlwaysAssertExit (lock.maxWait() == 0);
    lock.merge (TableLock (TableLock::AutoNoReadLocking, 10, 1));
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
    AlwaysAssertExit (lock.interval() == 10);
    AlwaysAssertExit (lock.maxWait() == 1);
  }
  {
    TableLock lock (TableLock::PermanentLockingWait);
    lock.merge (TableLock());
    AlwaysAssertExit (lock.option() == TableLock::PermanentLockingWait);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (lock.isPermanent());
    lock.merge (TableLock(TableLock::AutoNoReadLocking));
    AlwaysAssertExit (lock.option() == TableLock::PermanentLockingWait);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (lock.isPermanent());
  }
  {
    TableLock lock (TableLock::UserLocking);
    lock.merge (TableLock());
    AlwaysAssertExit (lock.option() == TableLock::UserLocking);
    ////AlwaysAssertExit (lock.option() == TableLock::AutoLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
    lock.merge (TableLock(TableLock::AutoLocking, 20, 2));
    AlwaysAssertExit (lock.option() == TableLock::AutoLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (! lock.isPermanent());
    AlwaysAssertExit (lock.interval() == 20);
    AlwaysAssertExit (lock.maxWait() == 2);
    lock.merge (TableLock(TableLock::PermanentLockingWait));
    AlwaysAssertExit (lock.option() == TableLock::PermanentLockingWait);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (lock.isPermanent());
    lock.merge (TableLock(TableLock::PermanentLocking));
    AlwaysAssertExit (lock.option() == TableLock::PermanentLocking);
    AlwaysAssertExit (lock.readLocking());
    AlwaysAssertExit (lock.isPermanent());
  }

  return 0;                           // exit with success status
}
