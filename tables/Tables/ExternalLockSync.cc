//# ExternalLockSync.cc: Class to hold table lock data
//# Copyright (C) 1997,1998
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


//# Includes
#include <casacore/tables/Tables/ExternalLockSync.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ExternalLockSync::ExternalLockSync (const TableLock& lockOptions)
: itsLock  (lockOptions, releaseCallBack, this),
  itsNrrow (0)
{}

ExternalLockSync::~ExternalLockSync()
{}

void ExternalLockSync::makeLock (const String& tableName,
				 bool create, FileLocker::LockType type)
{
    itsLock.makeLock (tableName, create, type);
}

bool ExternalLockSync::acquire (FileLocker::LockType type, uint32_t nattempts)
{
    if (! itsLock.acquire (&(itsSync.memoryIO()), type, nattempts)) {
	return false;
    }
    uint32_t nrcol;
    bool tableChanged;
    Block<bool> dataManChanged;
    itsSync.read (itsNrrow, nrcol, tableChanged, dataManChanged);
    return true;
}

MemoryIO* ExternalLockSync::releaseCallBack (void* lockSyncObject, bool always)
{
    return (*(ExternalLockSync*)lockSyncObject).doReleaseCallBack (always);
}

} //# NAMESPACE CASACORE - END

