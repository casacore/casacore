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
void checkLength (ByteIO& fio, uint32_t& curLength, uint32_t addLength)
{
    curLength += addLength;
    AlwaysAssertExit (fio.length() == curLength);
}


static bool valb=true;
static int16_t vals=-3;
static uint16_t valus=2;
static int32_t vali=1000;
static uint32_t valui=32768;
static int64_t vall=-14793;
static uint64_t valul=17;
static float valf=1.2;
static double vald=-3.14;

void checkValues (ByteIO& fio, uint16_t incr)
{
    fio.seek (0);
    uint32_t curLength = fio.length();
    bool resb;
    AlwaysAssertExit (fio.read (sizeof(bool), &resb) == sizeof(bool));
    AlwaysAssertExit (resb == valb);
    int16_t ress;
    AlwaysAssertExit (fio.read (sizeof(int16_t), &ress) == sizeof(int16_t));
    AlwaysAssertExit (ress == vals - incr);
    uint16_t resus;
    AlwaysAssertExit (fio.read (sizeof(uint16_t), &resus) == sizeof(uint16_t));
    AlwaysAssertExit (resus == valus + incr);
    int32_t resi;
    AlwaysAssertExit (fio.read (sizeof(int32_t), &resi) == sizeof(int32_t));
    AlwaysAssertExit (resi == vali);
    uint32_t resui;
    AlwaysAssertExit (fio.read (sizeof(uint32_t), &resui) == sizeof(uint32_t));
    AlwaysAssertExit (resui == valui);
    int64_t resl;
    AlwaysAssertExit (fio.read (sizeof(int64_t), &resl) == sizeof(int64_t));
    AlwaysAssertExit (resl == vall);
    uint64_t resul;
    AlwaysAssertExit (fio.read (sizeof(uint64_t), &resul) == sizeof(uint64_t));
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
    uint32_t length = 0;
    AlwaysAssertExit (fio.length() == 0);
    fio.write (sizeof(bool), &valb);
    checkLength (fio, length, sizeof(bool));
    fio.write (sizeof(int16_t), &vals);
    checkLength (fio, length, sizeof(int16_t));
    fio.write (sizeof(uint16_t), &valus);
    checkLength (fio, length, sizeof(uint16_t));
    fio.write (sizeof(int32_t), &vali);
    checkLength (fio, length, sizeof(int32_t));
    fio.write (sizeof(uint32_t), &valui);
    checkLength (fio, length, sizeof(uint32_t));
    fio.write (sizeof(int64_t), &vall);
    checkLength (fio, length, sizeof(int64_t));
    fio.write (sizeof(uint64_t), &valul);
    checkLength (fio, length, sizeof(uint64_t));
    fio.write (sizeof(float), &valf);
    checkLength (fio, length, sizeof(float));
    fio.write (sizeof(double), &vald);
    checkLength (fio, length, sizeof(double));
    
    checkValues (fio, 0);
    
    fio.seek (int32_t(sizeof(bool)));
    AlwaysAssertExit (fio.length() == length);
    uint16_t incr = 100;
    int16_t vals1 = vals - incr;
    int16_t ress;
    fio.write (sizeof(int16_t), &vals1);
    fio.seek (int32_t(sizeof(bool)));
    AlwaysAssertExit (fio.read (sizeof(int16_t), &ress) == sizeof(int16_t));
    AlwaysAssertExit (ress == vals1);
    uint16_t valus1 = valus + incr;
    uint16_t resus;
    fio.write (sizeof(uint16_t), &valus1);
    fio.seek (int32_t(-sizeof(uint16_t)), ByteIO::Current);
    AlwaysAssertExit (fio.read (sizeof(uint16_t), &resus) == sizeof(uint16_t));
    AlwaysAssertExit (resus == valus1);
    int32_t resi;
    AlwaysAssertExit (fio.read (sizeof(int32_t), &resi) == sizeof(int32_t));
    AlwaysAssertExit (resi == vali);
    AlwaysAssertExit (fio.length() == length);
    
    checkValues (fio, incr);

    AlwaysAssertExit (fio.length() == length);
    int64_t offset = sizeof(bool);
    incr = 100;
    vals1 = vals - incr;
    fio.pwrite (sizeof(int16_t), offset, &vals1);
    AlwaysAssertExit (fio.pread(sizeof(int16_t), offset, &ress) == sizeof(int16_t));
    AlwaysAssertExit (ress == vals1);
    fio.seek (offset);
    AlwaysAssertExit (fio.read(sizeof(int16_t), &ress) == sizeof(int16_t));
    AlwaysAssertExit (ress == vals1);
    offset += sizeof(int16_t);
    valus1 = valus + incr;
    fio.pwrite (sizeof(uint16_t), offset, &valus1);
    AlwaysAssertExit (fio.pread(sizeof(uint16_t), offset, &resus) == sizeof(uint16_t));
    AlwaysAssertExit (resus == valus1);
    AlwaysAssertExit (fio.read(sizeof(uint16_t), &ress) == sizeof(uint16_t));
    AlwaysAssertExit (resus == valus1);

    checkValues (fio, incr);
}

void checkReopen()
{
    RegularFile rfile("tByteIO_tmp.data");
    {
	RegularFileIO fio(rfile);
	checkValues (fio, 100);
	fio.reopenRW();
	fio.seek (int64_t(sizeof(bool)));
	int16_t vals;
	fio.read (sizeof(int16_t), &vals);
	vals -= 50;
	fio.seek (int64_t(sizeof(bool)));
	fio.write (sizeof(int16_t), &vals);
	uint16_t valus;
	fio.read (sizeof(uint16_t), &valus);
	valus += 50;
	fio.seek (int32_t(-sizeof(uint16_t)), ByteIO::Current);
	fio.write (sizeof(uint16_t), &valus);
	checkValues (fio, 150);
    }

    rfile.setPermissions (0444);
    RegularFileIO fio2(rfile);
    checkValues (fio2, 150);
    bool flag = false;
    try {
	fio2.reopenRW();
    } catch (std::exception& x) {
	flag = true;
    } 
    AlwaysAssertExit (flag);
    checkValues (fio2, 150);
    rfile.setPermissions (0644);
}

void testMemoryIO()
{
    {
	unsigned char buf[10];
	MemoryIO membuf (buf, sizeof(buf), ByteIO::New, 6);
	doIt (membuf);
	AlwaysAssertExit (membuf.getBuffer() != (const unsigned char*)&buf);
	int64_t length = membuf.length();
	int32_t incr = 20;
	membuf.seek (incr, ByteIO::End);
	AlwaysAssertExit (membuf.length() == length+incr);
	checkValues (membuf, 100);
	char val;
	int64_t lincr = incr;
	membuf.seek (-lincr, ByteIO::End);
	for (int32_t i=0; i<incr; i++) {
	    membuf.read (1, &val);
	    AlwaysAssertExit (val == 0);
	}
	try {
	    membuf.read (1, &val);
	} catch (std::exception& x) {
	    cout << x.what() << endl;         // read beyond object
	} 
	try {
	    membuf.seek (int32_t(-(length + incr + 1)), ByteIO::Current);
	} catch (std::exception& x) {
	    cout << x.what() << endl;         // negative seek
	} 
	membuf.seek (int32_t(-(length + incr)), ByteIO::Current);
    }
    {
	char buf[10];
	MemoryIO membuf (buf, sizeof(buf), ByteIO::New, 0);
	try {
	    doIt (membuf);
	} catch (std::exception& x) {                   // not expandable
	    cout << x.what() << endl;
	} 
	try {
	    membuf.seek (10, ByteIO::End);
	} catch (std::exception& x) {                   // not expandable
	    cout << x.what() << endl;
	} 
	AlwaysAssertExit (membuf.getBuffer() == (const unsigned char*)buf);
    }
    {
	char* buf = new char[10];
	MemoryIO membuf (buf, 10, ByteIO::Scratch, 0, true);
	try {
	    doIt (membuf);
	} catch (std::exception& x) {                   // not expandable
	    cout << x.what() << endl;
	} 
	try {
	    membuf.seek (10, ByteIO::End);
	} catch (std::exception& x) {                   // not expandable
	    cout << x.what() << endl;
	} 
	AlwaysAssertExit (membuf.getBuffer() == (const unsigned char*)buf);
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
	} catch (std::exception& x) {
	    cout << x.what() << endl;          // readonly
	} 
	checkValues (file3, 100);
	
	{
	    RegularFileIO file1(RegularFile("tByteIO_tmp.data"), ByteIO::New);
	    doIt (file1);
	}
	checkReopen();
	// Do regular io for various buffer sizes.
	for (uint32_t bs=1; bs<100; bs++) {
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
    } catch (std::exception& x) {
	cout << "Caught an exception: " << x.what() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

