//# tSymLink.cc: Test program for class SymLink
//# Copyright (C) 1993,1994,1995,1996,2000,2001
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


#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for class SymLink.
// </summary>

// This program tests the class SymLink.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
// <p>
// When an argument is given, no exceptions will be thrown.
// This can be used to check if no memory leaks occur in normal operation.


void doIt (Bool doExcp)
{
    SymLink linkA ("tSymLink_tmp/A");
    SymLink linkI ("tSymLink_tmp/I");
    if (doExcp) {
	try {
	    SymLink symLink1(Path("tSymLink_tmp/isFile"));
	}
	catch (AipsError x) {                                   // regular file
	    cout << x.getMesg () << endl;
	} 
	try {
	    SymLink symLink1(Path("tSymLink_tmp/isDir"));
	}
	catch (AipsError x) {
	    cout << x.getMesg () << endl;                       // directory
	} 
    }

    SymLink newLink(Path("tSymLink_tmp/newLink"));
    SymLink newLink2(Path("tSymLink_tmp/isDir/newLink2"));
    File file("tSymLink_tmp/isDir");
    File file1("tSymLink_tmp/newLink");
    File file2("tSymLink_tmp/isDir/newLink2");
    file.setPermissions (0444);
    if (doExcp) {
	try {
	    SymLink symLink1(Path("tSymLink_tmp/isDir/newB"));
	}
	catch (AipsError x) {
	    cout << x.getMesg () << endl;                    // cannot create
	} 
	try {
	    newLink2.create("a");
	}
	catch (AipsError x) {
	    cout << x.getMesg () << endl;                    // cannot create
	} 
    }
    file.setPermissions (0755);
    AlwaysAssertExit (! file1.exists());
    AlwaysAssertExit (! file2.exists());

    // Create the new sym links.
    newLink.create ("s");
    newLink.create ("$HOME");
    newLink2.create ("../E");
    AlwaysAssertExit (file1.isSymLink());
    AlwaysAssertExit (file2.isSymLink());

    // Get the value of $HOME. If it is a symlink, follow it.
    File homeFile ("$HOME");
    String homeName;
    if (homeFile.isSymLink()) {
	homeName = SymLink(homeFile).followSymLink().originalName();
    }else{
	homeName = homeFile.path().expandedName();
    }

    // Read and follow some symlinks.
    AlwaysAssertExit (linkA.readSymLink().originalName() == "tSymLink_tmp/B");
    AlwaysAssertExit (linkI.readSymLink().originalName() == "tSymLink_tmp/H");
    AlwaysAssertExit (newLink.readSymLink().originalName() ==
		                              Path("$HOME").expandedName());
    AlwaysAssertExit (newLink2.readSymLink().originalName() ==
		                              "tSymLink_tmp/isDir/../E");
    if (doExcp) {
	try {
	    linkA.followSymLink();
	}
	catch (AipsError x) {
	    cout << x.getMesg () << endl;                       // endless loop
	} 
    }
    AlwaysAssertExit (linkI.followSymLink().originalName() == homeName);
    AlwaysAssertExit (newLink.followSymLink().originalName() == homeName);
    AlwaysAssertExit (newLink2.followSymLink().originalName() == homeName);

    // Recreate an existing symlink.
    linkI.create ("B");
    AlwaysAssertExit (linkI.readSymLink().originalName() == "tSymLink_tmp/B");
    if (doExcp) {
	try {
	    linkI.followSymLink();
	}
	catch (AipsError x) {
	    cout << x.getMesg () << endl;                       // endless loop
	} 
    }

    // Copy a symlink.
    SymLink linkII ("tSymLink_tmp/II");
    AlwaysAssertExit (! linkII.exists());
    linkI.copy ("tSymLink_tmp/II");
    AlwaysAssertExit (linkII.isSymLink());
    AlwaysAssertExit (linkI.isSymLink());
    AlwaysAssertExit (linkII.readSymLink().originalName() ==
		                                       "tSymLink_tmp/B");
    SymLink linkIII ("tSymLink_tmp/isDir/I");
    AlwaysAssertExit (! linkIII.exists());
    linkI.copy ("tSymLink_tmp/isDir");
    AlwaysAssertExit (linkIII.isSymLink());
    AlwaysAssertExit (linkI.isSymLink());
    AlwaysAssertExit (linkIII.readSymLink().originalName() ==
		                                       "tSymLink_tmp/isDir/B");

    // Move a symlink.
    File linkisFile ("tSymLink_tmp/isFile");
    AlwaysAssertExit (linkisFile.isRegular());
    linkI.move ("tSymLink_tmp/isFile");
    AlwaysAssertExit (linkisFile.isSymLink());
    AlwaysAssertExit (! linkI.exists());
    AlwaysAssertExit (SymLink(linkisFile).readSymLink().originalName() ==
                                                       "tSymLink_tmp/B");
    SymLink linkIIa ("tSymLink_tmp/isDir/II");
    AlwaysAssertExit (! linkIIa.exists());
    linkII.move ("tSymLink_tmp/isDir");
    AlwaysAssertExit (linkIIa.isSymLink());
    AlwaysAssertExit (! linkII.exists());
    AlwaysAssertExit (linkIIa.readSymLink().originalName() ==
		                                       "tSymLink_tmp/isDir/B");

    // Test symlink creation.
    SymLink test6;
    test6 = linkIIa;
    AlwaysAssertExit (test6.readSymLink().originalName() ==
		                                       "tSymLink_tmp/isDir/B");
    if (doExcp) {
	try {
	    test6.create ("a", False);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // already existing
	} 
    }
    SymLink test7("tSymLink_tmp/newLink2");
    RegularFile rfile("tSymLink_tmp/newLink2");
    rfile.create();
    if (doExcp) {
	try {
	    test7.create ("a");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;               // already existing
	} 
    }
    rfile.remove();
    SymLink test8(test7);
    test8.create ("a", False);
    AlwaysAssertExit (test7.readSymLink().originalName() ==
		                                       "tSymLink_tmp/a");
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
