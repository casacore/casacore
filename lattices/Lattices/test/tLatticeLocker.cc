//# tLatticeLocker: Interactive test program for concurrent access to lattices
//# Copyright (C) 1999,2000,2001
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

#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/LatticeLocker.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This program tests concurrent access to lattices.


void b()
{
    // Open the table for update with UserLocking.
    Table tab ("tLatticeLocker_tmp.data",
	       TableLock(TableLock::UserLocking),
	       Table::Update);
    PagedArray<Int> pa(tab);
    LatticeLocker* latlock[10];
    uInt nrll = 0;
    Array<Int> arr(IPosition(2,4,4));
    arr = 0;
    Int val;
    Int opt;
    while (True) {
	cout << "0=quit, 1=rdlock, 2=wrlock, 3=get, 4=put, 5=unlock, 6=hasrl, "
	        "7=haswl: ";
	cin >> opt;
	if (opt == 1) {
	    if (nrll >= 10) {
		cout << "Cannot lock; already 10 lock objects in use" << endl;
	    } else {
		try {
		    latlock[nrll] = new LatticeLocker (pa, FileLocker::Read, 1);
		    nrll++;
		} catch (AipsError x) {
		    cout << x.getMesg() << endl;
		} 
	    }
	} else if (opt == 2) {
	    if (nrll >= 10) {
		cout << "Cannot lock; already 10 lock objects in use" << endl;
	    } else {
		try {
		    latlock[nrll] = new LatticeLocker (pa, FileLocker::Write,1);
		    nrll++;
		} catch (AipsError x) {
		    cout << x.getMesg() << endl;
		} 
	    }
	} else if (opt == 3  || opt == 4) {
	    if (! tab.hasLock ((opt==4))) {
		cout << "Cannot get/put; lattice is not (correctly) locked"
		     << endl;
	    } else {
		if (opt == 4) {
		    cout << "value: ";
		    cin >> val;
		    arr = val;
		    pa.put (arr);
		} else {
		    pa.get (arr);
		    cout << "lattice value = " << arr(IPosition(2,0,0)) << endl;
		}
	    }
	} else if (opt == 5) {
	    if (nrll > 0) {
		nrll--;
		delete latlock[nrll];
	    } else {
		cout << "no more lock objects" << endl;
	    }
	} else if (opt == 6) {
	    cout << "hasReadLock = " << pa.hasLock (FileLocker::Read) << endl;
	} else if (opt == 7) {
	    cout << "hasWriteLock = " << pa.hasLock (FileLocker::Write) << endl;
	} else {
	    break;
	}
    }
    for (uInt i=0; i<nrll; i++) {
	delete latlock[i];
    }
}


int main (int argc, const char* argv[])
{
    if (argc < 2) {
	cout << "Execute as: tLatticeLocker 1  to create new table"
	     << endl;
	cout << "            tLatticeLocker 0  to update existing table"
	     << endl;
    }else{
	try {
	    if (*(argv[1]) == '1') {
		PagedArray<Int> pa(IPosition(2,4,4), "tLatticeLocker_tmp.data");
	    }
	    b();
	} catch (AipsError x) {
	    cout << "Caught an exception: " << x.getMesg() << endl;
	    return 1;
	} 
    }
    return 0;                           // exit with success status
}
