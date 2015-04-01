//# LatticeLocker.cc: Class to hold a (user) lock on a lattice
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


#include <casacore/lattices/Lattices/LatticeLocker.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeLocker::LatticeLocker (LatticeBase& lattice,
			      FileLocker::LockType type,
			      uInt nattempts)
: itsLatticePtr (&lattice),
  itsOwnLock    (False),
  itsHadReadLock(False)
{
    if (itsLatticePtr->hasLock (type)) {
	return;
    }
    itsHadReadLock = itsLatticePtr->hasLock (FileLocker::Read);
    if (! itsLatticePtr->lock (type, nattempts)) {
	String str = "write";
	if (type == FileLocker::Read) {
	    str = "read";
	}
	throw (AipsError ("LatticeLocker: no " + str +
			  " lock could be acquired on lattice " +
			  itsLatticePtr->name()));
    }
    itsOwnLock = True;
}

LatticeLocker::~LatticeLocker()
{
    if (itsOwnLock) {
	itsLatticePtr->unlock();
	if (itsHadReadLock) {
	    itsLatticePtr->lock (FileLocker::Read, 1);
	}
    }
}

} //# NAMESPACE CASACORE - END

