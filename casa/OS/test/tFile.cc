//# tFile.cc: Test program for class File
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


#include <casacore/casa/OS/File.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for class File.
// </summary>

// This program tests the class File.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
// <p>
// When an argument is given, no exceptions will be thrown.
// This can be used to check if no memory leaks occur in normal operation.


void doIt (Bool doExcp)
{
    // Test constructors.
    // The existing files have been created in tFile.exec.
    File isFile("tFile_tmp/isFile");             // regular file
    File isDir("tFile_tmp/isDir");               // directory
    File isLink(Path("tFile_tmp/isLink"));       // symlink to $HOME
    File exist1(isLink);                         // copy ctor
    File exist2("tFile_tmp/justAName");          // justAName does not exist
    File test1("tFile_tmp/test1/anco");          // test1 does not exist
    File test2("tFile_tmp/test2/anco");          // test2 does not exist
    File isLink2("tFile_tmp/isLink2");           // symlink to regular file
    File isDir2("tFile_tmp/isDir2");             // directory
    File test3("tFile_tmp/isDir2/testFile");     // testFile does not exist

    File bin("/bin");
    File nocreate("/bin/afsferwrfasd");
    File creatable("tFile_tmp/idaasdfasdfa");
    File creatable2("tFile_tmp/xxxx/idaasdfasdfa");

    // Test assignment.
    File isFile2;
    isFile2 = isFile;
    exist2 = exist2;
    
    AlwaysAssertExit (isFile.isRegular());
    AlwaysAssertExit (isDir.isDirectory());
    AlwaysAssertExit (isLink.isSymLink());

    AlwaysAssertExit (!isLink.isRegular());
    AlwaysAssertExit (!isFile.isDirectory());
    AlwaysAssertExit (!isFile.isSymLink());
    AlwaysAssertExit (!isFile.isCharacterSpecial());
    AlwaysAssertExit (!isFile.isBlockSpecial());
    AlwaysAssertExit (!isFile.isPipe());
    AlwaysAssertExit (!isFile.isSocket());
    AlwaysAssertExit (!isDir.isRegular());

    AlwaysAssertExit (isLink2.isSymLink());

    AlwaysAssertExit (exist1.exists());
    AlwaysAssertExit (isFile.isReadable());
    AlwaysAssertExit (isFile.isWritable());
    AlwaysAssertExit (!isFile.isExecutable());

    AlwaysAssertExit (!exist2.exists());
    AlwaysAssertExit (!exist2.isReadable());
    AlwaysAssertExit (!exist2.isWritable());
    AlwaysAssertExit (!exist2.isExecutable());
    AlwaysAssertExit (!exist2.isRegular());
    AlwaysAssertExit (!exist2.isSymLink());
    AlwaysAssertExit (!exist2.isDirectory());
    if (doExcp) {
	try {
	    exist2.userID();
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    }

    // Test permission setting.
    isFile.setPermissions(0022);
    AlwaysAssertExit (!isFile.isReadable());
    AlwaysAssertExit (!isFile.isWritable());
    AlwaysAssertExit (!isFile.isExecutable());
    isFile.setPermissions(0722);
    AlwaysAssertExit (isFile.isReadable());
    AlwaysAssertExit (isFile.isWritable());
    AlwaysAssertExit (isFile.isExecutable());

    // Test when a symbolic link is involved.
    File test4("tFile_tmp/isLink");
    File test5("tFile_tmp/isLink2");
    AlwaysAssertExit (test4.isDirectory(True));
    AlwaysAssertExit (!test4.isRegular(True));
    AlwaysAssertExit (!test4.isDirectory(False));
    AlwaysAssertExit (!test4.isRegular(False));
    AlwaysAssertExit (!test5.isDirectory(True));
    AlwaysAssertExit (test5.isRegular(True));
    AlwaysAssertExit (!test5.isDirectory(False));
    AlwaysAssertExit (!test5.isRegular(False));

    // Test if a file can be created.
    AlwaysAssertExit (exist2.canCreate());
    AlwaysAssertExit (!test1.canCreate());
    AlwaysAssertExit (!test2.canCreate());
    AlwaysAssertExit (test3.canCreate());
    AlwaysAssertExit (isDir2.canCreate());
    isDir2.setPermissions(0644);
    AlwaysAssertExit (isDir2.canCreate());
    AlwaysAssertExit (!test3.canCreate());
    isDir2.setPermissions(0744);

    cout << isFile.path().originalName() << endl;
    isFile.setPermissions(0322);
    cout << isFile.readPermissions() << endl;
    isFile.setPermissions(0744);

    // Now some system specific output.
    cout << ">>>" << endl;
    cout << isFile.userID() << endl;
    cout << isFile.groupID() << endl;
    
    cout << isFile.newUniqueName("justAName").originalName() << endl;
    cout << File::newUniqueName("").originalName() << endl;

    isFile.touch(216445240);
    cout << isFile.accessTimeString () << endl;
    cout << isFile.modifyTimeString () << endl;
    cout << isFile.statusChangeTimeString () << endl;
   
    isFile.touch();
    cout << isFile.accessTimeString () << endl;
    cout << isFile.modifyTimeString () << endl;
    cout << isFile.statusChangeTimeString () << endl;
    cout << isFile.accessTime () << endl;
    cout << isFile.modifyTime () << endl;
    cout << isFile.statusChangeTime () << endl;

    AlwaysAssertExit (bin.getWriteStatus() == File::NOT_OVERWRITABLE);
    AlwaysAssertExit (nocreate.getWriteStatus() == File::NOT_CREATABLE);
    AlwaysAssertExit (creatable.getWriteStatus() == File::CREATABLE);
    AlwaysAssertExit (creatable2.getWriteStatus() == File::NOT_CREATABLE);
    AlwaysAssertExit (isFile.getWriteStatus() == File::OVERWRITABLE);

    cout << "<<<" << endl;
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
