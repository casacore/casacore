//# tByteIO.cc: Test program for class ByteIO and derived classes
//# Copyright (C) 1996,1997,2000,2001,2002
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

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <unistd.h>
#include <fcntl.h>


#include <casacore/casa/namespace.h>
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
static Int64 vall=-14793;
static uInt64 valul=17;
static float valf=1.2;
static double vald=-3.14;

void checkValues (ByteIO& fio, uShort incr)
{
    fio.seek (0);
    uInt curLength = fio.length();
    Bool resb;
    AlwaysAssertExit (fio.read (sizeof(Bool), &resb) == sizeof(Bool));
    AlwaysAssertExit (resb == valb);
    Short ress;
    AlwaysAssertExit (fio.read (sizeof(Short), &ress) == sizeof(Short));
    AlwaysAssertExit (ress == vals - incr);
    uShort resus;
    AlwaysAssertExit (fio.read (sizeof(uShort), &resus) == sizeof(uShort));
    AlwaysAssertExit (resus == valus + incr);
    Int resi;
    AlwaysAssertExit (fio.read (sizeof(Int), &resi) == sizeof(Int));
    AlwaysAssertExit (resi == vali);
    uInt resui;
    AlwaysAssertExit (fio.read (sizeof(uInt), &resui) == sizeof(uInt));
    AlwaysAssertExit (resui == valui);
    Int64 resl;
    AlwaysAssertExit (fio.read (sizeof(Int64), &resl) == sizeof(Int64));
    AlwaysAssertExit (resl == vall);
    uInt64 resul;
    AlwaysAssertExit (fio.read (sizeof(uInt64), &resul) == sizeof(uInt64));
    AlwaysAssertExit (resul == valul);
    float resf;
    AlwaysAssertExit (fio.read (sizeof(float), &resf) == sizeof(float));
    AlwaysAssertExit (resf == valf);
    double resd;
    AlwaysAssertExit (fio.read (sizeof(double), &resd) == sizeof(double));
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
    fio.write (sizeof(Int64), &vall);
    checkLength (fio, length, sizeof(Int64));
    fio.write (sizeof(uInt64), &valul);
    checkLength (fio, length, sizeof(uInt64));
    fio.write (sizeof(float), &valf);
    checkLength (fio, length, sizeof(float));
    fio.write (sizeof(double), &vald);
    checkLength (fio, length, sizeof(double));
    
    checkValues (fio, 0);
    
    fio.seek (Int(sizeof(Bool)));
    AlwaysAssertExit (fio.length() == length);
    uShort incr = 100;
    Short vals1 = vals - incr;
    Short ress;
    fio.write (sizeof(Short), &vals1);
    fio.seek (Int(sizeof(Bool)));
    AlwaysAssertExit (fio.read (sizeof(Short), &ress) == sizeof(Short));
    AlwaysAssertExit (ress == vals1);
    uShort valus1 = valus + incr;
    uShort resus;
    fio.write (sizeof(uShort), &valus1);
    fio.seek (Int(-sizeof(uShort)), ByteIO::Current);
    AlwaysAssertExit (fio.read (sizeof(uShort), &resus) == sizeof(uShort));
    AlwaysAssertExit (resus == valus1);
    Int resi;
    AlwaysAssertExit (fio.read (sizeof(Int), &resi) == sizeof(Int));
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
	fio.seek (Int64(sizeof(Bool)));
	Short vals;
	fio.read (sizeof(Short), &vals);
	vals -= 50;
	fio.seek (Int64(sizeof(Bool)));
	fio.write (sizeof(Short), &vals);
	uShort valus;
	fio.read (sizeof(uShort), &valus);
	valus += 50;
	fio.seek (Int(-sizeof(uShort)), ByteIO::Current);
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
	Int64 length = membuf.length();
	Int incr = 20;
	membuf.seek (incr, ByteIO::End);
	AlwaysAssertExit (membuf.length() == length+incr);
	checkValues (membuf, 100);
	char val;
	Int64 lincr = incr;
	membuf.seek (-lincr, ByteIO::End);
	for (Int i=0; i<incr; i++) {
	    membuf.read (1, &val);
	    AlwaysAssertExit (val == 0);
	}
	try {
	    membuf.read (1, &val);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // read beyond object
	} 
	try {
	    membuf.seek (Int(-(length + incr + 1)), ByteIO::Current);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;         // negative seek
	} 
	membuf.seek (Int(-(length + incr)), ByteIO::Current);
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


int main()
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
	// Do regular io for various buffer sizes.
	for (uInt bs=1; bs<100; bs++) {
	    RegularFileIO file1(RegularFile("tByteIO_tmp.data"), ByteIO::New,
				bs);
	    doIt (file1);
	}

	int fd = open ("tByteIO_tmp.data2", O_CREAT|O_TRUNC|O_RDWR, 0644);
	int flags = fcntl (fd, F_GETFL);
	if (flags & O_RDWR) {
	    cout << "read/write" << endl;
	} else if (flags & O_WRONLY) {
	    cout << "writeonly" << endl;
	} else {
	    cout << "readonly" << endl;
	}
	FiledesIO file4 (fd, "");
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
	FiledesIO file5 (fd1, "");
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
	FiledesIO file6 (fd2, "");
	close (fd2);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

