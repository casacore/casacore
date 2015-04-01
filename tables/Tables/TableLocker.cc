//# TableLocker.cc: Class to hold a (user) lock on a table
//# Copyright (C) 1998,2000,2005
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


#include <casacore/tables/Tables/TableLocker.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableLocker::TableLocker (Table& table,
			  FileLocker::LockType type,
			  uInt nattempts)
: itsTable   (table),
  itsHadLock (table.hasLock(type))
{
  if (!itsHadLock) {
    if (type == FileLocker::Read  &&  !table.lockOptions().readLocking()) {
      // Read lock not needed if NoReadLocking.
      itsHadLock = True;
    } else {
      // Acquire the lock.
      if (! itsTable.lock (type, nattempts)) {
	String str = "write";
	if (type == FileLocker::Read) {
	  str = "read";
	}
	throw (TableError ("No " + str + " lock could be acquired on table " +
			   itsTable.tableName()));
      }
    }
  }
}

TableLocker::~TableLocker()
{
  if (!itsHadLock) {
    itsTable.unlock();
  }
}

} //# NAMESPACE CASACORE - END
