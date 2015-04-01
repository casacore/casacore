//# tLockFile.cc: Program to test classes LockFile and FileLocker
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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


#include <casacore/casa/IO/LockFile.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
void doIt (const String& name, double interval)
{
    cout << "Choose between 2 locknrs (i.e. parts of file to lock)." << endl;
    cout << "The first one is a permanent LockFile object." << endl;
    cout << "The second one is deleted when leaving the inner loop" << endl;
    cout << "and can be used to check what happens if multiple LockFile" <<endl;
    cout << "objects are used on the same file and one is deleted" << endl;
    Path path(name);
    cout << "LockFile for " << path.absoluteName() << endl;
    cout << "Inspection interval = " << interval << " seconds" << endl;
    int op, lnr;
    cout << "Locking type (0=no, 1=permanent, other=normal):";
    cin >> op;
    //# Create 2 lock objects with given inspection interval.
    //# Let them start at a different offset in the file.
    LockFile lock1(name, interval, False, True, True, 0, op==1, op==0);
    LockFile* lockp;
    while (True) {
	cout << "locknr (1,2 0=end): ";
	cin >> lnr;
	if (lnr <= 0) break;
	if (lnr == 1) {
	    lockp = &lock1;
	} else {
            cout << "Locking type (0=no, 1=permanent, other=normal):";
            cin >> op;
	    lockp = new LockFile (name, interval, False, True, True, 1,
                                  op==1, op==0);
	}
	while (True) {
	    cout << "1=rlock, 2=wlock, 3=rlockw, 4=wlockw, 5=unlock, "
		    "6=status, 7=speed, 8=open/close 9=show, else=end: ";
	    cin >> op;
	    if (op == 1  ||  op == 3) {
		uInt nattempt = 1;
		if (op == 3) {
		    cout << "nattempts :";
		    cin >> nattempt;
		}
		if (lockp->acquire (FileLocker::Read, nattempt)) {
		    cout << " Read lock acquired" << endl;
		}else{
		    cout << " Already locked by another process" << endl;
		}
	    } else if (op == 2  ||  op == 4) {
		uInt nattempt = 1;
		if (op == 4) {
		    cout << "nattempts :";
		    cin >> nattempt;
		}
		if (lockp->acquire (FileLocker::Write, nattempt)) {
		    cout << " Write lock acquired" << endl;
		}else{
		    cout << " Already locked by another process" << endl;
		}
	    } else if (op == 5) {
		if (! lockp->release()) {
		    cout << " Lock could not be released" << endl;
		}
	    } else if (op == 6) {
		cout << " Last errno " << lockp->lastError() << ": "
		     << lockp->lastMessage() << endl;
		cout << " Request flag is " << lockp->inspect() << endl;
		cout << " canReadLock=" << lockp->canLock (FileLocker::Read);
		cout << ", canWriteLock=" << lockp->canLock (FileLocker::Write);
		cout << ", hasReadLock=" << lockp->hasLock (FileLocker::Read);
		cout << ", hasWriteLock=" << lockp->hasLock (FileLocker::Write);
		cout << ", isMultiUsed = " << lockp->isMultiUsed();
		cout << endl;
	    } else if (op == 7) {
		Timer timer;
		for (int i=0; i<500; i++) {
		    lockp->acquire (FileLocker::Write, 1);
		}
		timer.show ("Acquiring 500 locks:");
	    } else if (op == 8) {
	        RegularFileIO tmp(name);
	    } else if (op == 9) {
	        uInt pid;
		Bool perm;
		uInt res = LockFile::showLock (pid, perm, name);
		cout << "result=" << res << ", pid=" << pid
		     << ", permlocked=" << perm << endl;
	    } else {
		break;
	    }
	}
	if (lnr == 2) {
	    delete lockp;
	}
    }
}

void doTest()
{
    LockFile lock ("tLockFile_tmp.data", 0, True);
    AlwaysAssertExit (! lock.hasLock (FileLocker::Read));
    AlwaysAssertExit (! lock.hasLock (FileLocker::Write));
    MemoryIO memio;
    //# Acquire a lock and get information. No request is pending.
    AlwaysAssertExit (lock.acquire (memio));
    AlwaysAssertExit (memio.length() == 0);
    AlwaysAssertExit (! lock.inspect());
    AlwaysAssertExit (lock.hasLock (FileLocker::Read));
    AlwaysAssertExit (lock.hasLock (FileLocker::Write));
    cout << lock.canLock() << endl;
    //# Release the lock and reacquire it.
    AlwaysAssertExit (lock.release());
    AlwaysAssertExit (! lock.hasLock (FileLocker::Read));
    AlwaysAssertExit (! lock.hasLock (FileLocker::Write));
    AlwaysAssertExit (lock.acquire());
    AlwaysAssertExit (lock.acquire (memio));
    AlwaysAssertExit (memio.length() == 0);
    //# Release the lock and reacquire it.
    AlwaysAssertExit (lock.release (memio));
    AlwaysAssertExit (lock.acquire (memio));
    AlwaysAssertExit (memio.length() == 0);
    //# Store information (<1024 bytes) in the memio object.
    uInt value = 10;
    memio.write (sizeof(uInt), &value);
    AlwaysAssertExit (lock.release (memio));
    memio.seek (0);
    //# Store different value in memio to be sure acquire gets it right.
    value = 20;
    memio.write (sizeof(value), &value);
    //# Get lock and read info.
    lock.acquire (memio);
    AlwaysAssertExit (memio.length() == sizeof(value));
    memio.read (sizeof(value), &value);
    AlwaysAssertExit (value == 10);
    //# Write very long info.
    Int val[10000];
    Int i;
    for (i=0; i<10000; i++) {
	val[i] = i-5000;
    }
    value = 10000;
    memio.write (sizeof(value), &value);
    memio.write (sizeof(val), val);
    lock.release (memio);
    //# Acquire lock and get info.
    {
	MemoryIO memio2;
	uInt n;
	Int v[10000];
	lock.acquire (memio2);
	memio2.read (sizeof(n), &n);
	AlwaysAssertExit (n == 10);
	memio2.read (sizeof(n), &n);
	AlwaysAssertExit (n == 10000);
	memio2.read (sizeof(v), v);
	for (i=0; i<10000; i++) {
	    AlwaysAssertExit (v[i] == i-5000);
	}
    }
}

int main (int argc, const char* argv[])
{
    try {
	// If no argument given, tell how to run.
	// This option is needed to pass the standard runtests target.
	if (argc > 1) {
	    double interval = 5;
	    if (argc > 2) {
		istringstream istr(argv[2]);
		istr >> interval;
	    }
	    doIt (argv[1], interval);
	}else{
	    doTest();
	    cout << "Run as:   tLockFile <fileName> [inspectionInterval]"
		 << endl;
	    cout << "for a manual control of acquiring and releasing locks."
		 << endl;
	    cout << "Default inspection interval is 5 seconds." << endl;
	}
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}

