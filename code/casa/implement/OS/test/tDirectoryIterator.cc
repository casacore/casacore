//# tDirectoryIterator.cc: Test program for class DirectoryIterator
//# Copyright (C) 1996,2000
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


#include <aips/OS/DirectoryIterator.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions.h>
#include <iostream.h>


// <summary>
// Test program for class DirectoryIterator.
// </summary>

// This program tests the class DirectoryIterator.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
// <p>
// When an argument is given, no exceptions will be thrown.
// This can be used to check if no memory leaks occur in normal operation.


uInt countFiles (DirectoryIterator& iter)
{
    uInt n=0;
    while (! iter.pastEnd()) {
	n++;
	iter++;
    }
    return n;
}

void doIt (Bool doExcp)
{
    Directory dir("tDirectoryIterator_tmp");
    DirectoryIterator iter(dir, Regex(".*a.*"));
    AlwaysAssertExit (!iter.pastEnd());
    AlwaysAssertExit (iter.name() == "a");
    AlwaysAssertExit (iter.file().path().originalName() ==
		                               "tDirectoryIterator_tmp/a");
    iter++;
    AlwaysAssertExit (!iter.pastEnd());
    AlwaysAssertExit (iter.name() == "ca");
    AlwaysAssertExit (iter.file().path().originalName() ==
		                               "tDirectoryIterator_tmp/ca");
    ++iter;
    AlwaysAssertExit (!iter.pastEnd());
    AlwaysAssertExit (iter.name() == "ca.cc");
    AlwaysAssertExit (iter.file().path().originalName() ==
		                               "tDirectoryIterator_tmp/ca.cc");
    ++iter;
    AlwaysAssertExit (iter.pastEnd());
    if (doExcp) {
	try {
	    iter.name();
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // past end
	} 
	try {
	    iter++;
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // past end
	} 
    }
    iter.reset();
    AlwaysAssertExit (iter.name() == "a");
    AlwaysAssertExit (countFiles(iter) == 3);

    iter = DirectoryIterator(dir);
    AlwaysAssertExit (countFiles(iter) == 6);
    iter = DirectoryIterator(dir, Regex("."));
    AlwaysAssertExit (countFiles(iter) == 3);
    iter = DirectoryIterator(dir, Regex("c.*"));
    AlwaysAssertExit (countFiles(iter) == 4);
    DirectoryIterator itera(iter);
    AlwaysAssertExit (countFiles(itera) == 4);
    iter = DirectoryIterator(dir, Regex("cc.*"));
    AlwaysAssertExit (countFiles(iter) == 0);
    iter = DirectoryIterator(dir, Regex(".*\\.cc"));
    AlwaysAssertExit (countFiles(iter) == 1);

    if (doExcp) {
	try {
	    DirectoryIterator iter1 (Directory("tDirectoryIterator_tmp/sub"));
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // not existing
	} 
    }
}


main (int argc)
{
    try {
	doIt (ToBool (argc<2));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	cout << "at line " << x.thrownLine() 
	     << " in " << x.thrownFile() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}
