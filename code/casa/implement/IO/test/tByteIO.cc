//# tByteIO.cc: Test program for class ByteIO and derived classes
//# Copyright (C) 1996,1997,2000
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

#include <aips/aips.h>
#include <aips/IO/FiledesIO.h>
#include <aips/IO/RegularFileIO.h>
#include <aips/IO/MemoryIO.h>
#include <aips/OS/RegularFile.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>


void checkLength (ByteIO& fio, uInt& curLength, uInt addLength)
{
    curLength += addLength;
    AlwaysAssertExit (fio.length() == curLength);
}


static Bool valb=True;
static Short vals=-3;
static uShort valus=2;
static Int vali=1000;
static uInt valui=32768;
static Long vall=-14793;
static uLong valul=17;
static float valf=1.2;
static double vald=-3.14;

void checkValues (ByteIO& fio, uShort incr)
{
    fio.seek (0);
    uInt curLength = fio.length();
    Bool resb;
    fio.read (sizeof(Bool), &resb);
    AlwaysAssertExit (resb == valb);
    Short ress;
    fio.read (sizeof(Short), &ress);
    AlwaysAssertExit (ress == vals - incr);
    uShort resus;
    fio.read (sizeof(uShort), &resus);
    AlwaysAssertExit (resus == valus + incr);
    Int resi;
    fio.read (sizeof(Int), &resi);
    AlwaysAssertExit (resi == vali);
    uInt resui;
    fio.read (sizeof(uInt), &resui);
    AlwaysAssertExit (resui == valui);
    Long resl;
    fio.read (sizeof(Long), &resl);
    AlwaysAssertExit (resl == vall);
    uLong resul;
    fio.read (sizeof(uLong), &resul);
    AlwaysAssertExit (resul == valul);
    float resf;
    fio.read (sizeof(float), &resf);
    AlwaysAssertExit (resf == valf);
    double resd;
    fio.read (sizeof(double), &resd);
    AlwaysAssertExit (resd == vald);
    AlwaysAssertExit (fio.length() == curLength);
}

void doIt (ByteIO& fio)
{
    uInt length = 0;
    AlwaysAssertExit (fio.length() == 0);
    fio.write (sizeof(Bool), &valb);
    checkLength (fio, length, sizeof(Bool));
    fio.write (sizeof(Short), &vals);
    checkLength (fio, length, sizeof(Short));
    fio.write (sizeof(uShort), &valus);
    checkLength (fio, length, sizeof(uShort));
    fio.write (sizeof(Int), &vali);
    checkLength (fio, length, sizeof(Int));
    fio.write (sizeof(uInt), &valui);
    checkLength (fio, length, sizeof(uInt));
    fio.write (sizeof(Long), &vall);
    checkLength (fio, length, sizeof(Long));
    fio.write (sizeof(uLong), &valul);
    checkLength (fio, length, sizeof(uLong));
    fio.write (sizeof(float), &valf);
    checkLength (fio, length, sizeof(float));
    fio.write (sizeof(double), &vald);
    checkLength (fio, length, sizeof(double));
    
    checkValues (fio, 0);
    
    fio.seek (sizeof(Bool));
    AlwaysAssertExit (fio.length() == length);
    uShort incr = 100;
    Short vals1 = vals - incr;
    Short ress;
    fio.write (sizeof(Short), &vals1);
    fio.seek (sizeof(Bool));
    fio.read (sizeof(Short), &ress);
    AlwaysAssertExit (ress == vals1);
    uShort valus1 = valus + incr;
    uShort resus;
    fio.write (sizeof(uShort), &valus1);
    fio.seek (-sizeof(uShort), ByteIO::Current);
    fio.read (sizeof(uShort), &resus);
    AlwaysAssertExit (resus == valus1);
    Int resi;
    fio.read (sizeof(Int), &resi);
    AlwaysAssertExit (resi == vali);
    AlwaysAssertExit (fio.length() == length);
    
    checkValues (fio, incr);
}

void checkReopen()
{
    RegularFile rfile("tByteIO_tmp.data");
    {
	RegularFileIO fio(rfile);
	checkValues (fio, 100);
	fio.reopenRW();
	fio.seek (sizeof(Bool));
	Short vals;
	fio.read (sizeof(Short), &vals);
	vals -= 50;
	fio.seek (sizeof(Bool));
	fio.write (sizeof(Short), &vals);
	uShort valus;
	fio.read (sizeof(uShort), &valus);
	valus += 50;
	fio.seek (-sizeof(uShort), ByteIO::Current);
	fio.write (sizeof(uShort), &valus);
	checkValues (fio, 150);
    }

    rfile.setPermissions (0444);
    RegularFileIO fio2(rfile);
    checkValues (fio2, 150);
    Bool flag = False;
    try {
	fio2.reopenRW();
    } catch (AipsError x) {
	flag = True;
    } 
    AlwaysAssertExit (flag);
    checkValues (fio2, 150);
    rfile.setPermissions (0644);
}

void testMemoryIO()
{
    {
	uChar buf[10];
	MemoryIO membuf (buf, sizeof(buf), ByteIO::New, 6);
	doIt (membuf);
	AlwaysAssertExit (membuf.getBuffer() != (const uChar*)&buf);
	uLong length = membuf.length();
	uInt incr = 20;
	membuf.seek (incr, ByteIO::End);
	AlwaysAssertExit (membuf.length() == length+incr);
	checkValues (membuf, 100);
	char val;
	Long lincr = incr;
	membuf.seek (-lincr, ByteIO::End);
	for (int i=0; i<incr; i++) {
	    membuf.read (1, &val);
	    AlwaysAssertExit (val == 0);
	}
	try {
	    membuf.read (1, &val);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // read beyond object
	} 
	try {
	    membuf.seek (-(length + incr + 1), ByteIO::Current);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // negative seek
	} 
	membuf.seek (-(length + incr), ByteIO::Current);
    }
    {
	char buf[10];
	MemoryIO membuf (buf, sizeof(buf), ByteIO::New, 0);
	try {
	    doIt (membuf);
	} catch (AipsError x) {                   // not expandable
	    cout << x.getMesg() << endl;
	} 
	try {
	    membuf.seek (10, ByteIO::End);
	} catch (AipsError x) {                   // not expandable
	    cout << x.getMesg() << endl;
	} 
	AlwaysAssertExit (membuf.getBuffer() == (const uChar*)buf);
    }
    {
	char* buf = new char[10];
	MemoryIO membuf (buf, 10, ByteIO::Scratch, 0, True);
	try {
	    doIt (membuf);
	} catch (AipsError x) {                   // not expandable
	    cout << x.getMesg() << endl;
	} 
	try {
	    membuf.seek (10, ByteIO::End);
	} catch (AipsError x) {                   // not expandable
	    cout << x.getMesg() << endl;
	} 
	AlwaysAssertExit (membuf.getBuffer() == (const uChar*)buf);
    }
}


main()
{
    try {
	testMemoryIO();

	MemoryIO file2;
	doIt (file2);
	
	MemoryIO file3 (file2.getBuffer(), file2.length());
	try {
	    file3.write (0, 0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;          // readonly
	} 
	checkValues (file3, 100);
	
	{
	    RegularFileIO file1(RegularFile("tByteIO_tmp.data"), ByteIO::New);
	    doIt (file1);
	}
	checkReopen();

	int fd = open ("tByteIO_tmp.data2", O_CREAT|O_TRUNC|O_RDWR, 0644);
	int flags = fcntl (fd, F_GETFL);
	if (flags & O_RDWR) {
	    cout << "read/write" << endl;
	} else if (flags & O_WRONLY) {
	    cout << "writeonly" << endl;
	} else {
	    cout << "readonly" << endl;
	}
	FiledesIO file4 (fd);
	doIt (file4);
	close (fd);

	int fd1 = open ("tByteIO_tmp.data2", O_RDONLY, 0644);
	int flags1 = fcntl (fd1, F_GETFL);
	if (flags1 & O_RDWR) {
	    cout << "read/write" << endl;
	} else if (flags1 & O_WRONLY) {
	    cout << "writeonly" << endl;
	} else {
	    cout << "readonly" << endl;
	}
	FiledesIO file5 (fd1);
	checkValues (file5, 100);
	close (fd1);

	int fd2 = creat ("tByteIO_tmp.data2", 0644);
	int flags2 = fcntl (fd2, F_GETFL);
	if (flags2 & O_RDWR) {
	    cout << "read/write" << endl;
	} else if (flags2 & O_WRONLY) {
	    cout << "writeonly" << endl;
	} else {
	    cout << "readonly" << endl;
	}
	FiledesIO file6 (fd2);
	close (fd2);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

