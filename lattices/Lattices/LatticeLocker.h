//# LatticeLocker.h: Class to hold a (user) lock on a lattice
//# Copyright (C) 1999,2000
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

#ifndef LATTICES_LATTICELOCKER_H
#define LATTICES_LATTICELOCKER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/LatticeBase.h>
#include <casacore/tables/Tables/TableLock.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class to hold a (user) lock on a lattice.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTableLockSync.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Lattice>Lattice</linkto>
//   <li> <linkto class=TableLock>TableLock</linkto>
// </prerequisite>

// <synopsis>
// Class LatticeLocker can be used to acquire a (user) lock on a lattice.
// The lock can be a read or write lock.
// The destructor releases the lock when needed.
// <p>
// LatticeLocker simply uses the <src>lock</src> and <src>unlock</src>
// function of class Lattice.
// The advantage of LatticeLocker over these functions is that the
// destructor of LatticeLocker is called automatically by the system,
// so unlocking the lattice does not need to be done explicitly and
// cannot be forgotten. Especially in case of exception handling this
// can be quite an adavantage.
// <p>
// This class is meant to be used with the UserLocking option.
// It can, however, also be used with the other locking options.
// In case of PermanentLocking(Wait) it won't do anything at all.
// In case of AutoLocking it will acquire and release the lock when
// needed. However, it is possible that the system releases an
// auto lock before the LatticeLocker destructor is called.
// <p>
// The constructor of LatticeLocker will look if the lattice is
// already appropriately locked. If so, it will set a flag to
// prevent the destructor from unlocking the lattice. In this way
// nested locks can be used. I.e. one can safely use LatticeLocker
// in a function without having to be afraid that its destructor
// would undo a lock set in a higher function.
// <br>Similarly LatticeLocker will remember if a lattice was
// already read-locked, when a write-lock is acquired. In such a
// case the destructor will try to ensure that the lattice remains
// read-locked.
// </synopsis>

// <example>
// <srcblock>
// // Open a lattice to be updated.
// PagedArray<Float> myLattice (Table ("theLattice",
//                              LatticeLock::UserLocking,
//                              Lattice::Update);
// // Start of some critical section requiring a lock.
// {
//     LatticeLocker lock1 (myLattice, FileLocker::Write);
//     ... write the data
// }
// // The LatticeLocker destructor invoked by } unlocks the table.
// </srcblock>
// </example>

// <motivation>
// LatticeLocker makes it easier to unlock a lattice.
// It also makes it easier to use locking in a nested way.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class LatticeLocker
{
public:
    // The constructor acquires a read or write lock on a lattice.
    // If the lattice was already locked, the destructor will
    // not unlock the lattice. This means that the class can be used in
    // a nested way.
    // <br>
    // The number of attempts (default = forever) can be specified when
    // acquiring the lock does not succeed immediately. When nattempts>1,
    // the system waits 1 second between each attempt, so nattempts
    // is more or less equal to a wait period in seconds.
    // An exception is thrown when the lock cannot be acquired.
    explicit LatticeLocker (LatticeBase& lattice,
			    FileLocker::LockType,
			    uInt nattempts = 0);

    // If the constructor acquired the lock, the destructor releases
    // the lock and flushes the data if changed.
    ~LatticeLocker();

    // Has this process the read or write lock, thus can the table
    // be read or written safely?
    Bool hasLock (FileLocker::LockType) const;

private:
    // The copy constructor and assignment are not possible.
    // Note that only one lock can be held on a lattice, so copying a
    // TableLocker object imposes great difficulties which object should
    // release the lock.
    // It can be solved by turning LatticeLocker into a handle class
    // with a reference counted body class.
    // However, that will only be done when the need arises.
    // <group>
    LatticeLocker (const LatticeLocker&);
    LatticeLocker& operator= (const LatticeLocker&);
    // </group>

    //# Variables.
    LatticeBase* itsLatticePtr;
    Bool         itsOwnLock;
    Bool         itsHadReadLock;
};


inline Bool LatticeLocker::hasLock (FileLocker::LockType type) const
{
    return itsLatticePtr->hasLock (type);
}



} //# NAMESPACE CASACORE - END

#endif
