//# tLockFile.cc: Program to test classes LockFile and FileLocker
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
//#
//# $Id$


#include <aips/IO/LockFile.h>
#include <aips/IO/MemoryIO.h>
#include <aips/OS/RegularFile.h>
#include <aips/OS/Path.h>
#include <aips/OS/Timer.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <strstream.h>


void doIt (const String& name, double interval)
{
    //# Create object with given inspection interval.
    LockFile lock(name, interval);
    Path path(name);
    cout << "LockFile for " << path.absoluteName() << endl;
    cout << "Inspection interval = " << interval << " seconds" << endl;
    int op;
    while (True) {
	cout << "1=readlock, 2=writelock, 3=unlock, 4=inspect" << endl;
	cout << "5=canreadlock, 6=canwritelock, 7=speed, 8=isMultiUsed" << endl;
	cout << "9=message, else=end: ";
	cin >> op;
	if (op == 1) {
	    if (lock.acquire (FileLocker::Read, 1)) {
		cout << "Read lock acquired" << endl;
	    }else{
		cout << "Already locked" << endl;
	    }
	} else if (op == 2) {
	    if (lock.acquire (FileLocker::Write, 1)) {
		cout << "Write lock acquired" << endl;
	    }else{
		cout << "Already locked" << endl;
	    }
	} else if (op == 3) {
	    if (! lock.release()) {
		cout << "Lock could not be released" << endl;
	    }
	} else if (op == 4) {
	    cout << "Request flag is " << lock.inspect() << endl;
	} else if (op == 5) {
	    cout << "canReadLock = " << lock.canLock (FileLocker::Read) << endl;
	} else if (op == 6) {
	    cout << "canWriteLock = " << lock.canLock (FileLocker::Write)
		 << endl;
	} else if (op == 7) {
	    Timer timer;
	    for (int i=0; i<500; i++) {
		lock.acquire (FileLocker::Write, 1);
	    }
	    timer.show ("Acquiring 500 locks:");
	} else if (op == 8) {
	    cout << "isMultiUsed = " << lock.isMultiUsed() << endl;
	} else if (op == 9) {
	    cout << lock.lastError() << ": " << lock.lastMessage() << endl;
	} else {
	    break;
	}
    }
}

void doTest()
{
    LockFile lock ("tLockFile_tmp.data", 0, True);
    MemoryIO memio;
    //# Acquire a lock and get information. No request is pending.
    AlwaysAssertExit (lock.acquire (memio));
    AlwaysAssertExit (memio.length() == 0);
    AlwaysAssertExit (! lock.inspect());
    cout << lock.canLock() << endl;
    //# Release the lock and reacquire it.
    AlwaysAssertExit (lock.release());
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

main (int argc, char** argv)
{
    try {
	// If no argument given, tell how to run.
	// This option is needed to pass the standard runtests target.
	if (argc > 1) {
	    double interval = 5;
	    if (argc > 2) {
		istrstream istr(argv[2]);
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
    } end_try;
    cout << "OK" << endl;
    return 0;                           // exit with success status
}

