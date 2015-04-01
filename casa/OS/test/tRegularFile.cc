//# tRegularFile.cc: Test program for class RegularFile
//# Copyright (C) 1996,2000,2001
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


#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for class RegularFile.
// </summary>

// This program tests the class RegularFile.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
// <p>
// When an argument is given, no exceptions will be thrown.
// This can be used to check if no memory leaks occur in normal operation.


void doIt (Bool doExcp)
{
    // Create some File objects to test later.
    File isFile  ("tRegularFile_tmp/isFile");     // regular file
    File isLink  ("tRegularFile_tmp/isLink");     // symlink to isFile
    File isLink1 ("tRegularFile_tmp/isLink1");    // symlink to isLink

    // Test the constructors.
    RegularFile risFile (Path("tRegularFile_tmp/isFile"));    // reg.file
    RegularFile raName ("tRegularFile_tmp/aName");            // creatable
    RegularFile risLink1 (isLink1);                           // resolves
    RegularFile rFile5 (risLink1);                            // copy ctor
    AlwaysAssertExit (rFile5.path().originalName() ==
                                             "tRegularFile_tmp/isLink1");
    if (doExcp) {
	try {
	    RegularFile rf ("tRegularFile_tmp/a/b");         // not creatable
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
	try {
	    RegularFile rf ("tRegularFile_tmp");             // directory
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
	try {
	    RegularFile rf ("tRegularFile_tmp/isLink2");     // symlink to dir
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    }
    // Assignment.
    risLink1 = risLink1;
    AlwaysAssertExit (risLink1.path().originalName() ==
                                             "tRegularFile_tmp/isLink1");
    rFile5 = risFile;
    AlwaysAssertExit (rFile5.path().originalName() ==
                                             "tRegularFile_tmp/isFile");
    

    // Do an erronous create.
    if (doExcp) {
	try {
	    risFile.create (False);                         // already exists
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    }
    // Do a valid create and assure its timestamp is greater and it is empty.
    uInt time1 = risFile.modifyTime();
    risFile.create();
    AlwaysAssertExit (risFile.modifyTime() > time1);
    AlwaysAssertExit (risFile.size() == 0);

    // Copy a file.
    //   to another file
    RegularFile risCopy ("tRegularFile_tmp/moveto/isFile1");
    RegularFile risFile1 ("tRegularFile_tmp/isFile1");
    uInt size1 = risFile1.size();
    cout << size1 << endl;
    risFile1.copy ("tRegularFile_tmp/isFile");
    AlwaysAssertExit (risFile.size() == size1);
    //   to another directory.
    risFile1.copy ("tRegularFile_tmp/moveto");
    AlwaysAssertExit (risCopy.exists());
    AlwaysAssertExit (risCopy.size() == size1);
    //   invalid copies
    if (doExcp) {
	try {
	    risFile1.copy (Path("tRegularFile_tmp/aa/bb/cc"));
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // non-writable directory
	} 
	try {
	    risFile1.copy (Path("tRegularFile_tmp/isFile"), False);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // already exists
	} 
    }
    risCopy.setPermissions (0444);
    if (doExcp) {
	try {
	    risFile1.copy (Path("tRegularFile_tmp/moveto/isFile1"));
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // exists, non-writable
	} 
    }
    risCopy.setPermissions (0644);

    // Move a file.
    risFile.move ("tRegularFile_tmp/file2");
    AlwaysAssertExit (!risFile.exists());
    RegularFile file2("tRegularFile_tmp/file2");
    AlwaysAssertExit (file2.exists());
    if (doExcp) {
	try {
	    file2.move ("tRegularFile_tmp/moveto/isFile1", False);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // already exists
	} 
    }
    file2.move ("tRegularFile_tmp/moveto");
    AlwaysAssertExit (File("tRegularFile_tmp/moveto/file2").exists());
    AlwaysAssertExit (!file2.exists());


    // Remove a file.
    risCopy.remove();
    AlwaysAssertExit (!risCopy.exists());
    // Copy the link (result should get the link name).
    risCopy = RegularFile ("tRegularFile_tmp/moveto/isLink1");
    risLink1.copy (risCopy.path());
    AlwaysAssertExit (risCopy.exists());
    AlwaysAssertExit (risCopy.size() == size1);

    // Create file1 via a symlink; file1 should be empty thereafter.
    AlwaysAssertExit (risFile1.size() != 0);
    risLink1.create();
    AlwaysAssertExit (risFile1.size() == 0);

    // Remove the file and all its symlinks.
    risLink1.remove();
    AlwaysAssertExit (!risLink1.exists());
    AlwaysAssertExit (! File("tRegularFile_tmp/isLink").exists());
    AlwaysAssertExit (!risFile1.exists());
}


int main (int argc,const char*[])
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
