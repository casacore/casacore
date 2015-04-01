//# TableLock.h: Class to hold table lock options
//# Copyright (C) 1997,1998,2000,2001
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

#ifndef TABLES_TABLELOCK_H
#define TABLES_TABLELOCK_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/LockFile.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Class to hold table lock options.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tTable" demos="">
// </reviewed>

// <prerequisite> 
//    <li> class <linkto class=Table>Table</linkto>
//    <li> class <linkto class=LockFile>LockFile</linkto>
// </prerequisite>

// <synopsis> 
// This class keeps the Table lock options.
// Currently these are the LockingOption and the inspection interval.
// <p>
// It also keeps the <src>LockFile</src> object used to do the
// actual locking/unlocking.

// <motivation> 
// Encapsulate Table locking info.
// </motivation>


class TableLock
{
public: 
    // Define the possible table locking options.
    // They offer the user the possibility to lock and synchronize access
    // to the table. A lot of locking degrades table performance; not only
    // because acquiring/releasing locks takes time, but especially
    // because table data has to be synchronized (thus written to disk)
    // when a lock is released. Otherwise the other processes see data
    // which is not up-to-date.
    enum LockOption {
	// The table is permanently locked.
	// A lock is set at the beginning and only released when
	// the table is closed. A read lock is used when the table is
	// opened for readonly; otherwise a write lock is used.
	// This means that multiple readers are possible.
	// The Table constructor exits with an exception when the
	// lock cannot be acquired.
	PermanentLocking,
	// The same as above, but the table constructor waits
	// until the lock gets available.
        PermanentLockingWait,
	// The system takes care of acquiring/releasing locks.
	// In principle it keeps the table locked, but from time to
	// time (defined by the inspection interval) it is checked whether
	// another process wants to access the table. If so, the lock
	// is released and probably re-acquired later.
	// This mode is the default mode.
	AutoLocking,
	// The user is taking care of locking the table by means
	// of the Table functions <src>lock</src> and <src>unlock</src>.
	// In this way transaction processing can be implemented.
	UserLocking,
	// The system takes care of acquiring/releasing locks.
	// It is similar to AutoLocking, but no locks are needed for
	// reading.
	AutoNoReadLocking,
	// The user is taking care of locking the table by means
	// of the Table functions <src>lock</src> and <src>unlock</src>.
	// It is similar to UserLocking, but no locks are needed for
	// reading.
	UserNoReadLocking,
        // Do not do any locking at all. This should be used with care
        // because concurrent access might result in table corruption.
        NoLocking,
	// This is the default locking option.
	// It means that AutoLocking will be used if the table is not
	// opened yet. Otherwise the locking options of the PlainTable
	// object already in use will be used.
	DefaultLocking
    };

    // Construct with given option and interval.
    // The default <src>LockOption</src> is <src>AutoLocking</src>.
    // In case of AutloLocking the inspection interval defines how often
    // the table system checks if another process needs a lock on the table.
    // It defaults to 5 seconds.
    // The maxWait defines the maximum number of seconds the table system
    // waits when acquiring a lock in AutoLocking mode. The default
    // is 0 seconds meaning indefinitely.
    // <group>
    TableLock (LockOption option = DefaultLocking);
    TableLock (LockOption option, double inspectionInterval, uInt maxWait = 0);
    // </group>

    // Copy constructor.
    TableLock (const TableLock& that);

    // Assignment.
    TableLock& operator= (const TableLock& that);

    // Merge that TableLock with this TableLock object by taking the
    // maximum option and minimum inspection interval.
    // The option order (ascending) is UserLocking, AutoLocking,
    // PermanentLocking.
    // When an interval was defaulted, it is not taken into account.
    // An option DefaultLocking is not taken into account.
    void merge (const TableLock& that);

    // Get the locking option.
    LockOption option() const;

    // Is read locking needed?
    Bool readLocking() const;

    // Is permanent locking used?
    Bool isPermanent() const;

    // Get the inspection interval.
    double interval() const;

    // Get the maximum wait period in AutoLocking mode.
    uInt maxWait() const;

    // Is table locking disabled (because AIPS_TABLE_NOLOCKING was set)?
    static Bool lockingDisabled();


private:
    LockOption  itsOption;
    Bool        itsReadLocking;
    uInt        itsMaxWait;
    double      itsInterval;
    Bool        itsIsDefaultLocking;
    Bool        itsIsDefaultInterval;


    // Set itsOption and itsReadLocking when needed.
    void init();
};



inline TableLock::LockOption TableLock::option() const
{
    return itsOption;
}

inline Bool TableLock::readLocking() const
{
    return itsReadLocking;
}

inline Bool TableLock::isPermanent() const
{
    return  (itsOption == PermanentLocking
	       ||  itsOption == PermanentLockingWait);
}

inline double TableLock::interval() const
{
    return itsInterval;
}

inline uInt TableLock::maxWait() const
{
    return itsMaxWait;
}



} //# NAMESPACE CASACORE - END

#endif
