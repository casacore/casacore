//# tBucketFile.cc: Test program for the BucketFile class
//# Copyright (C) 1995,1996,2000
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

#include <aips/Tables/BucketFile.h>
#include <aips/Tables/TableError.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/RegularFile.h>
#include <iostream.h>

// <summary>
// Test program for the BucketFile class
// </summary>

void a();
void b();
void c();

main (int argc)
{
    try {
	a();
	b();
	// Do exceptional things only when needed.
	if (argc < 2) {
	    cout << ">>>" << endl;
	    c();
	    cout << "<<<" << endl;
	}
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}



// Build a file.
void a()
{
    // Create the file.
    BucketFile file ("tBucketFile_tmp.data");
    AlwaysAssertExit (file.isWritable());
    AlwaysAssertExit (file.name() == "tBucketFile_tmp.data");
    Int ival=10;
    float fval=20;
    file.write (&ival, sizeof(Int));
    file.write (&fval, sizeof(fval));
    Int ival2;
    float fval2;
    file.seek (0);
    file.read (&ival2, sizeof(Int));
    file.read (&fval2, sizeof(fval));
    AlwaysAssertExit (ival2 == ival);
    AlwaysAssertExit (fval2 == fval);
}

void b()
{
    // Open the file.
    BucketFile file ("tBucketFile_tmp.data", False);
    AlwaysAssertExit (! file.isWritable());
    AlwaysAssertExit (file.name() == "tBucketFile_tmp.data");
    file.open();
    Int ival=10;
    float fval=20;
    Int ival2;
    float fval2;
    file.read (&ival2, sizeof(Int));
    file.read (&fval2, sizeof(fval));
    AlwaysAssertExit (ival2 == ival);
    AlwaysAssertExit (fval2 == fval);

    // Set the file to read/write access.
    file.setRW();
    file.seek (0);
    file.read (&ival2, sizeof(Int));
    file.read (&fval2, sizeof(fval));
    AlwaysAssertExit (ival2 == ival);
    AlwaysAssertExit (fval2 == fval);
    file.write (&fval, sizeof(fval));
    file.write (&ival, sizeof(Int));
    file.seek (0);
    file.read (&ival2, sizeof(Int));
    file.read (&fval2, sizeof(fval));
    AlwaysAssertExit (ival2 == ival);
    AlwaysAssertExit (fval2 == fval);
    file.read (&fval2, sizeof(fval));
    file.read (&ival2, sizeof(Int));
    AlwaysAssertExit (ival2 == ival);
    AlwaysAssertExit (fval2 == fval);
}

void c()
{
    // Do some erronous calls.
    Bool flag = False;
    BucketFile file1 ("tBucketFile_tmp.data1", False);
    try {
	file1.open();
    } catch (AipsError x) {
	flag = True;
	cout << x.getMesg() << endl;
    } 
    AlwaysAssertExit (flag);
    
    // Make the file readonly to test on such errors.
    RegularFile rfile("tBucketFile_tmp.data");
    rfile.setPermissions (0444);
    
    flag = False;
    BucketFile file2 ("tBucketFile_tmp.data", True);
    try {
	file2.open();
    } catch (AipsError x) {
	flag = True;
	cout << x.getMesg() << endl;
    } 
    AlwaysAssertExit (flag);
    
    flag = False;
    BucketFile file3 ("tBucketFile_tmp.data", False);
    file3.setRW();
    try {
	file3.open();
    } catch (AipsError x) {
	flag = True;
	cout << x.getMesg() << endl;
    } 
    AlwaysAssertExit (flag);

    flag = False;
    BucketFile file4 ("tBucketFile_tmp.data", False);
    file4.open();
    try {
	file4.setRW();
    } catch (AipsError x) {
	flag = True;
	cout << x.getMesg() << endl;
    } 
    AlwaysAssertExit (flag);

    // Make it writable again.
    rfile.setPermissions (0644);
}
