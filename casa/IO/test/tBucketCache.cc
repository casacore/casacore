//# tBucketCache.cc: Test program for the BucketCache class
//# Copyright (C) 1995,1996,1999,2000,2001
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

#include <casacore/casa/IO/BucketCache.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the BucketCache class
// </summary>

void a (Bool);
void b (Bool);
void c (uInt bufSize);
void d (uInt bufSize);

int main (int argc, const char*[])
{
    try {
	a (argc<2);
	b (argc<2);
	c (0);
//	c (1024);
//	c (32768);
//	c (327680);
	d (0);
//	d (1024);
//	d (32768);
//	d (327680);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

static uInt counter = 0;

// The toLocal and fromLocal function.
// These versions copy the buffer in a reversed way.
char* aToLocal (void*, const char* data)
{
    char* ptr = new char[32768];
    char* out = ptr;
    char* last = out+32768;
    while (out < last) {
	out[3] = *data++;
	out[2] = *data++;
	out[1] = *data++;
	out[0] = *data++;
	out += 4;
    }
    return ptr;
}
void aFromLocal (void*, char* data, const char* local)
{
    char* out = data;
    char* last = out+32768;
    while (out < last) {
	out[3] = *local++;
	out[2] = *local++;
	out[1] = *local++;
	out[0] = *local++;
	out += 4;
    }
}
void aDeleteBuffer (void*, char* buffer)
{
    delete [] buffer;
}
char* aInitBuffer (void*)
{
    char* ptr = new char[32768];
    for (uInt i=0; i<32768; i++) {
	ptr[i] = 0;
    }
    *(Int*)ptr = ++counter;
    *(Int*)(ptr+32760) = counter;
    return ptr;
}

// The toLocal and fromLocal function.
// These versions do a straightforward copy (much faster).
char* bToLocal (void*, const char* data)
{
    char* ptr = new char[32768];
    memcpy (ptr, data, 32768);
    return ptr;
}
void bFromLocal (void*, char* data, const char* local)
{
    memcpy (data, local, 32768);
}


// Build a file.
void a (Bool)
{
    // Create the file.
    BucketFile file ("tBucketCache_tmp.data");
    file.open();
    BucketCache cache (&file, 512, 32768, 5, 10, 0, aToLocal, aFromLocal,
		       aInitBuffer, aDeleteBuffer);
    uInt i;
    union {
      char buf[32768];
      Int  bufi[32768/4];
    };
    for (i=0; i<32768; i++) {
	buf[i] = 0;
    }
    for (i=0; i<100; i++) {
	bufi[0] = i+1;
	bufi[32760/4] = i+10;
	char* ptr = new char[32768];
	memcpy (ptr, buf, 32768);
	cache.addBucket (ptr);
	cache.getBucket(0);
    }
    cache.extend (10);
    char* data = cache.getBucket (110);
    *(Int*)data = 110;
    *(Int*)(data+32760) = 110;
    cache.setDirty();
    Int rec[128];
    for (i=0; i<128; i++) {
	rec[i] = i;
    }
    rec[0] = cache.nBucket();
    cache.put ((char*)rec, 512, 0);
    cache.put ((char*)rec, 512, 512 + cache.nBucket()*32768);
    cout << "wrote " << cache.nBucket() << " buckets of 32768 bytes" << endl;
    cache.flush();
}

void b (Bool)
{
    // Open the file.
    BucketFile file("tBucketCache_tmp.data", False);
    file.open();
    Int i;
    Int rec[128];
    file.read ((char*)rec, 512);
    for (i=1; i<128; i++) {
	if (rec[i] != i) {
	    cout << "Error in rec pos " << i << endl;
	}
    }
    BucketCache cache (&file, 512, 32768, rec[0], 10, 0, aToLocal, aFromLocal,
		       aInitBuffer, aDeleteBuffer);
    cache.get ((char*)rec, 512, 512 + cache.nBucket()*32768);
    for (i=1; i<128; i++) {
	if (rec[i] != i) {
	    cout << "Error in rec pos " << i << endl;
	}
    }
    for (i=0; i<5; i++) {
	char* buf = cache.getBucket(i);
	if (*(Int*)buf != i+1  ||  *(Int*)(buf+32760) != i+1) {
	    cout << "Error in bucket " << i << endl;
	}
    }
    cache.resize (20);
    for (i=0; i<100; i++) {
	char* buf = cache.getBucket(i+5);
	if (*(Int*)buf != i+1  ||  *(Int*)(buf+32760) != i+10) {
	    cout << "Error in bucket " << i+5 << endl;
	}
    }
    cache.resize (4);
    for (i=0; i<10; i++) {
	char* buf = cache.getBucket (i+105);
	if (i == 5) {
	    if (*(Int*)buf != 110  ||  *(Int*)(buf+32760) != 110) {
		cout << "Error in bucket " << i << endl;
	    }
	}else{
	    if (*(Int*)buf != i+6  ||  *(Int*)(buf+32760) != i+6) {
		cout << "Error in bucket " << i+105 << endl;
	    }
	}
    }
    cout << "checked " << cache.nBucket() << " buckets" << endl;
}

void c (uInt)
{
    Timer timer;
    // Open the file.
    BucketFile file("tBucketCache_tmp.data", False);
    file.open();
    uInt i;
    Int rec[128];
    file.read ((char*)rec, 512);
    BucketCache cache (&file, 512, 32768, rec[0], 10, 0, aToLocal, aFromLocal,
		       aInitBuffer, aDeleteBuffer);
    cache.get ((char*)rec, 512, 512 + cache.nBucket()*32768);
    for (uInt j=0; j<25; j++) {
	for (i=0; i<100; i++) {
	    cache.getBucket(i);
	}
    }
    cout << "read 25x" << cache.nBucket() << " buckets with swap" << endl;
    cout << ">>> ";
    timer.show();
    cout << "<<<" << endl;
}

void d (uInt)
{
    Timer timer;
    // Open the file.
    BucketFile file("tBucketCache_tmp.data", False);
    file.open();
    uInt i;
    Int rec[128];
    file.read ((char*)rec, 512);
    BucketCache cache (&file, 512, 32768, rec[0], 10, 0, bToLocal, bFromLocal,
		       aInitBuffer, aDeleteBuffer);
    cache.get ((char*)rec, 512, 512 + cache.nBucket()*32768);
    for (uInt j=0; j<50; j++) {
	for (i=0; i<100; i++) {
	    cache.getBucket(i);
	}
    }
    cout << "read 50x" << cache.nBucket() << " buckets without swap" << endl;
    cout << cache.nBucket() << endl;
    cout << ">>> ";
    timer.show();
    cout << "<<<" << endl;
}
