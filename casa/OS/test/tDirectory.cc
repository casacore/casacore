//# tDirectory.cc: Test program for class Directory
//# Copyright (C) 1996,1997,1999,2000,2001
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


#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for class Directory.
// </summary>

// This program tests the class Directory.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
// <p>
// When an argument is given, no exceptions will be thrown.
// This can be used to check if no memory leaks occur in normal operation.


void doIt (Bool doExcp)
{
    // Test the constructors.
    Directory tmp(Path("tDirectory_tmp/"));
    Directory test1("tDirectory_tmp/test1");
    Directory test2("tDirectory_tmp/test2");
    Directory linkDir(File("tDirectory_tmp/linkDir"));
    Directory linkDir2 (linkDir);
    Directory linkDir3 (linkDir);
    AlwaysAssertExit (linkDir.isDirectory());
    AlwaysAssertExit (linkDir2.isDirectory());
    AlwaysAssertExit (linkDir3.isDirectory());
    AlwaysAssertExit (linkDir3.path().expandedName() ==
		                                "tDirectory_tmp/linkDir");

    {
        // Directory::shellExpand
        Vector<String> list(1);
        list(0) = "tDirectory_tmp/*";
        Vector<String> list2 = Directory::shellExpand(list, False);
        Vector<String> list3 = Directory::shellExpand(list, True);

        genSort(list2);
        genSort(list3);
//
        AlwaysAssertExit(list2.nelements()==6);
        {
           Path path("./tDirectory_tmp/linkDir");
           AlwaysAssertExit(list2(0)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/test1");
           AlwaysAssertExit(list2(1)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/test2");
           AlwaysAssertExit(list2(2)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/test5");
           AlwaysAssertExit(list2(3)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/testFile");
           AlwaysAssertExit(list2(4)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/testLink");
           AlwaysAssertExit(list2(5)==path.absoluteName());
        }
//
        AlwaysAssertExit(list3.nelements()==6);
        AlwaysAssertExit(list3(0)==String("linkDir"));
        AlwaysAssertExit(list3(1)==String("test1"));
        AlwaysAssertExit(list3(2)==String("test2"));
        AlwaysAssertExit(list3(3)==String("test5"));
        AlwaysAssertExit(list3(4)==String("testFile"));
        AlwaysAssertExit(list3(5)==String("testLink"));
//
        list2.resize(0); list3.resize(0);
        list(0) = "tDirectory_tmp/te*";
        list2 = Directory::shellExpand(list, False);
        list3 = Directory::shellExpand(list, True);
        genSort(list2);
        genSort(list3);
//
        AlwaysAssertExit(list2.nelements()==5);
        {
           Path path("./tDirectory_tmp/test1");
           AlwaysAssertExit(list2(0)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/test2");
           AlwaysAssertExit(list2(1)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/test5");
           AlwaysAssertExit(list2(2)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/testFile");
           AlwaysAssertExit(list2(3)==path.absoluteName());
        }
        {
           Path path("./tDirectory_tmp/testLink");
           AlwaysAssertExit(list2(4)==path.absoluteName());
        }
//
        AlwaysAssertExit(list3.nelements()==5);
        AlwaysAssertExit(list3(0)==String("test1"));
        AlwaysAssertExit(list3(1)==String("test2"));
        AlwaysAssertExit(list3(2)==String("test5"));
        AlwaysAssertExit(list3(3)==String("testFile"));
        AlwaysAssertExit(list3(4)==String("testLink"));
//
        list2.resize(0); list3.resize(0);
        list(0) = "tDirectory_tmp/?ink*";
        list2 = Directory::shellExpand(list, False);
        list3 = Directory::shellExpand(list, True);
        genSort(list2);
        genSort(list3);
//
        AlwaysAssertExit(list2.nelements()==1);
        {
           Path path("./tDirectory_tmp/linkDir");
           AlwaysAssertExit(list2(0)==path.absoluteName());
        }
//
        AlwaysAssertExit(list3.nelements()==1);
        AlwaysAssertExit(list3(0)==String("linkDir"));
    }

    {
        // Directory::find
        Vector<String> found = tmp.find (Regex::fromString("test1"));
	genSort (found);
	cout << found << endl;
        Vector<String> found1 = tmp.find (Regex("test[12]"), True);
	genSort (found1);
	cout << found1 << endl;
        Vector<String> found2 = tmp.find (Regex(".*"), True);
	genSort (found2);
	cout << found2 << endl;
    }

    AlwaysAssertExit (! tmp.isEmpty());
    AlwaysAssertExit (tmp.nEntries() == 6);

    // Construct and create a new directory.
    Directory newDir ("tDirectory_tmp/newDir");
    tmp.setPermissions (0555);
    if (doExcp) {
	try {
	    newDir.create();
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // not writable
	} 
    }
    tmp.setPermissions (0755);
    AlwaysAssertExit (! newDir.isDirectory());
    AlwaysAssertExit (! newDir.exists());
    newDir.create();
    AlwaysAssertExit (newDir.isDirectory());
    AlwaysAssertExit (newDir.exists());
    AlwaysAssertExit (newDir.isEmpty());
    AlwaysAssertExit (newDir.nEntries() == 0);

    // Some erronous constructs.
    if (doExcp) {
	try {
	    Directory file1("tDirectory_tmp/test1/testLink2");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // symlink, no directory
	} 
	try {
	    Directory file1("tDirectory_tmp/test1/testFile2");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // symlink, no directory
	} 
    }
    tmp.setPermissions (0555);
    if (doExcp) {
	try {
	    Directory file1("tDirectory_tmp/something");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // not writable
	} 
    }
    tmp.setPermissions (0755);

    // Copy a directory tree to a new directory.
    test1.copy ("tDirectory_tmp/test3");
    Directory test3 ("tDirectory_tmp/test3");
    Directory test3dir ("tDirectory_tmp/test3/isDir1");
    AlwaysAssertExit (test3.isDirectory());
    AlwaysAssertExit (test3.nEntries() == 6);
    AlwaysAssertExit (test3dir.isDirectory());
    AlwaysAssertExit (test3dir.nEntries() == 3);

    // Copy a directory tree to a existing directory.
    AlwaysAssertExit (test2.isDirectory());
    AlwaysAssertExit (test2.nEntries() == 1);
    test1.copy ("tDirectory_tmp/test2");
    AlwaysAssertExit (test2.isDirectory());
    AlwaysAssertExit (test2.nEntries() == 6);
    AlwaysAssertExit (test1.isDirectory());
    AlwaysAssertExit (test1.nEntries() == 6);

    // Remove files and directory.
    test3dir.removeFiles();
    AlwaysAssertExit (test3dir.isDirectory());
    AlwaysAssertExit (test3dir.nEntries() == 0);
    test3dir.remove();
    AlwaysAssertExit (!test3dir.exists());
    AlwaysAssertExit (test3.nEntries() == 5);
    // Do an erronous remove.
    if (doExcp) {
	try {
	    test3.remove();
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // not empty
	} 
    }
    test3.removeRecursive();
    AlwaysAssertExit (!test3.exists());

    // Move a directory tree to a new directory.
    test1.move ("tDirectory_tmp/test3");
    AlwaysAssertExit (test3.isDirectory());
    AlwaysAssertExit (test3.nEntries() == 6);
    AlwaysAssertExit (test3dir.isDirectory());
    AlwaysAssertExit (test3dir.nEntries() == 3);
    AlwaysAssertExit (!test1.exists());

    // Move a directory tree to an existing directory.
    Directory test5 ("tDirectory_tmp/test5");
    AlwaysAssertExit (test5.isDirectory());
    AlwaysAssertExit (test5.nEntries() == 1);
    test2.move ("tDirectory_tmp/test5");
    AlwaysAssertExit (test5.isDirectory());
    AlwaysAssertExit (test5.nEntries() == 6);
    AlwaysAssertExit (!test2.exists());

    // Move a directory tree across a file system and back.
    Directory testtmp ("/tmp/test5");
    test5.move ("/tmp/test5");
    AlwaysAssertExit (testtmp.isDirectory());
    AlwaysAssertExit (testtmp.nEntries() == 6);
    AlwaysAssertExit (!test5.exists());
    testtmp.move ("tDirectory_tmp/test5");
    AlwaysAssertExit (test5.isDirectory());
    AlwaysAssertExit (test5.nEntries() == 6);
    AlwaysAssertExit (!testtmp.exists());

    // Test directory creation.
    Directory test6;
    test6 = test5;
    AlwaysAssertExit (test6.nEntries() == 6);
    if (doExcp) {
	try {
	    test6.create (False);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // already existing
	} 
    }
    test6.create (True);
    AlwaysAssertExit (test6.isEmpty());

    Directory test7("tDirectory_tmp/newDir2");
    RegularFile rfile("tDirectory_tmp/newDir2");
    rfile.create();
    if (doExcp) {
	try {
	    test7.create (False);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // already existing
	} 
    }
    rfile.remove();
    test7.create (False);
    AlwaysAssertExit (test7.isEmpty());

    // Remove a directory via a symlink (which will be removed too).
    SymLink slink("tDirectory_tmp/newDir2Link");
    slink.create ("newDir2");
    Directory test8("tDirectory_tmp/newDir2Link");
    AlwaysAssertExit (test8.isEmpty());
    AlwaysAssertExit (test7.exists());
    AlwaysAssertExit (slink.exists());

    // Test the freeSpace function.
    cout << ">>>" << endl;
    cout << "Free Space: " << test7.freeSpace() << ' '
	 << test8.freeSpace() << endl;
    cout << "Free Space in MB: " << test7.freeSpaceInMB() << ' '
	 << test8.freeSpaceInMB() << endl;
    cout << "<<<" << endl;

    test8.remove();
    AlwaysAssertExit (!test7.exists());
    AlwaysAssertExit (!slink.exists());
}


int main (int argc, const char*[])
{
    try {
	doIt ( (argc<2));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}
