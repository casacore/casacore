//# tStArrayFile.cc: Test program for the StManArrayFile class
//# Copyright (C) 1994,1995,1996,1998,1999,2000,2001,2002,2003
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

#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/stdio.h>                           // for sprintf

#include <casacore/casa/namespace.h>
// <summary> Test program for the StManArrayFile class </summary>

// This program tests the class StManArrayFile.
// This class is meant to store indirect table arrays, but could
// in principle also be used for other array purposes.

void a (Bool, uInt, Int64&, Int64&, Int64&, Int64&);
void b (Bool, Int64, Int64, Int64, Int64, Int64&, Int64&, Int64&, Int64&);
void c (Bool, Int64, Int64, Int64, Int64);

int main (int argc, const char* argv[])
{
    uInt stVersion = 0;
    uInt endVersion = 1;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> stVersion;
	endVersion = stVersion;
	if (argc > 2) {
	    istringstream istr(argv[2]);
	    istr >> endVersion;
	}
    }
    try {
	for (uInt i=stVersion; i<=endVersion; i++) {
	    Int64 off1, off2, off3, off4, offc1, offc2, offc3, offc4;
	    cout << "test of StArrayFile with version " << i
		 << " in canonical format " << endl;
	    a (True, i, off1, off2, off3, off4);
	    b (True, off1, off2, off3, off4, offc1, offc2, offc3, offc4);
	    c (True, off1, off2, off3, off4);
	    c (True, offc1, offc2, offc3, offc4);
	    cout << "test of StArrayFile with version " << i
		 << " in local format " << endl;
	    a (False, i, off1, off2, off3, off4);
	    b (False, off1, off2, off3, off4, offc1, offc2, offc3, offc4);
	    c (False, off1, off2, off3, off4);
	    c (False, offc1, offc2, offc3, offc4);
	}
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

// Write some arrays (in chunks).
void a (Bool canonical, uInt version,
	Int64& off1, Int64& off2, Int64& off3, Int64& off4)
{
    uInt l1,l2,l3,l4;
    Bool bbuf[10000];
    Int ibuf[10000];
    Complex cbuf[10000];
    String sbuf[10000];
    char str[16];
    for (uInt i=0; i<10000; i++) {
	if (i%3 == 0) {
	    bbuf[i] = True;
	}else{
	    bbuf[i] = False;
	}
	ibuf[i] = i;
	cbuf[i] = Complex(i+1,i+2);
	sprintf (str, "str %d", i);
	sbuf[i] = str;
    }
    StManArrayFile io("tStArrayFile_tmp.data", ByteIO::New, version,
		      canonical);
    cout << "Length=" << io.length() << endl;
    l1 = io.putShape (IPosition(2,100,100), off1, static_cast<Int*>(0));
    cout << l1 << " " << off1 << endl;
    cout << "Length=" << io.length() << endl;
    //# Note that because the data is not written here (but a bit later),
    //# valgrind gives an uninitialized error when the buffer gets written.
    l3 = io.putShape (IPosition(2,2000,5), off3, static_cast<String*>(0));
    cout << l3 << " " << off3 << endl;
    cout << "Length=" << io.length() << endl;
    if (version > 0) {
	io.putRefCount (5, off3);
    }
    io.put (off3+l3, 0, 3000, sbuf);
    cout << "Length=" << io.length() << endl;
    l2 = io.putShape (IPosition(1,10000), off2, static_cast<Complex*>(0));
    cout << l2 << " " << off2 << endl;
    cout << "Length=" << io.length() << endl;
    io.put (off3+l3, 3000, 1024, sbuf+3000);
    cout << "Length=" << io.length() << endl;
    io.put (off2+l2, 0, 10000, cbuf);
    io.put (off1+l1, 0, 10000, ibuf);
    cout << "Length=" << io.length() << endl;
    io.put (off3+l3, 4024, 5976, sbuf+4024);
    cout << "Length=" << io.length() << endl;
    l4 = io.putShape (IPosition(2,1000,10), off4, static_cast<Bool*>(0));
    cout << l4 << " " << off4 << endl;
    cout << "Length=" << io.length() << endl;
    io.put (off4+l4, 0, 10000, bbuf);
}

// Read back and update and copy some arrays.
void b (Bool canonical, Int64 off1, Int64 off2, Int64 off3, Int64 off4,
	Int64& offc1, Int64& offc2, Int64& offc3, Int64& offc4)
{
    StManArrayFile io("tStArrayFile_tmp.data", ByteIO::Update, 0, canonical);
    cout << "Length=" << io.length() << endl;
    IPosition shp, shp1, shp2, shp3, shp4;
    Int64 offs;
    uInt nref;
    Bool bbuf[10000];
    Int ibuf[10000];
    Complex cbuf[10000];
    String sbuf[10000], sbufo[10000];
    char str[16];
    Int i;
    for (i=0; i<10000; i++) {
	sprintf (str, "str %d", i);
	sbuf[i] = str;
    }
    uInt l1 = io.getShape (off1, shp);
    nref = io.getRefCount (off1);
    cout << l1 << " " << shp << " " << nref <<endl;
    shp1 = shp;
    uInt l2 = io.getShape (off2, shp);
    nref = io.getRefCount (off2);
    cout << l2 << " " << shp << " " << nref << endl;
    shp2 = shp;
    uInt l3 = io.getShape (off3, shp);
    nref = io.getRefCount (off3);
    cout << l3 << " " << shp << " " << nref << endl;
    shp3 = shp;
    uInt l4 = io.getShape (off4, shp);
    nref = io.getRefCount (off4);
    cout << l4 << " " << shp << " " << nref << endl;
    shp4 = shp;
    cout << "Length=" << io.length() << endl;
    cout << io.putShape (IPosition(2,10,5), offs, static_cast<String*>(0));
    cout << " " << offs << endl;
    cout << "Length=" << io.length() << endl;
    io.get (off3+l3, 0, 4096, sbufo);
    io.get (off3+l3, 4096, 5904, sbufo+4096);
    for (i=0; i<10000; i++) {
	if (sbuf[i] != sbufo[i]) {
	    cout << "Mismatch: " << sbuf[i] << " " << sbufo[i] << endl;
	}
    }
    io.get (off1+l1, 0, 3400, ibuf);
    io.get (off2+l2, 0, 8000, cbuf);
    io.get (off1+l1, 3400, 6600, ibuf+3400);
    io.get (off2+l2, 8000, 1999, cbuf+8000);
    io.get (off2+l2, 9999,  1, cbuf+9999);
    io.get (off4+l4, 0, 98, bbuf);
    io.get (off4+l4, 102, 1000, bbuf+102);
    io.get (off4+l4, 98, 4, bbuf+98);
    io.get (off4+l4, 1102, 1898, bbuf+1102);
    io.get (off4+l4, 3000, 7000, bbuf+3000);
    for (i=0; i<10000; i++) {
	Bool b = (i%3 == 0  ?  True : False);
	if (ibuf[i] != i  ||  cbuf[i] != Complex(i+1,i+2)  ||  bbuf[i] != b) {
	    cout << "mismatch " << i << ":" << ibuf[i] << " " << cbuf[i]
		 << " " << bbuf[i] << endl;
	}
    }
    cout << "Length=" << io.length() << endl;
    io.put (off3+l3, 1, 20, sbuf);
    cout << "Length=" << io.length() << endl;
    io.put (off1+l1, 1, 20, ibuf);
    io.put (off2+l2, 1, 20, cbuf);
    cout << "Length=" << io.length() << endl;
    io.put (off4+l4, 1, 20, bbuf);
    io.put (off4+l4, 23, 1, bbuf);
    io.put (off4+l4, 34, 4, bbuf);
    cout << "Length=" << io.length() << endl;
    uInt lc1 = io.putShape (shp1, offc1, static_cast<Int*>(0));
    cout << "copy to " << lc1 << " " << offc1 << endl;
    io.copyArrayInt (offc1+lc1, off1+l1, shp1.product());
    uInt lc2 = io.putShape (shp2, offc2, static_cast<Complex*>(0));
    cout << "copy to " << lc2 << " " << offc2 << endl;
    io.copyArrayComplex (offc2+lc2, off2+l2, shp2.product());
    uInt lc3 = io.putShape (shp3, offc3, static_cast<String*>(0));
    cout << "copy to " << lc3 << " " << offc3 << endl;
    io.copyArrayString (offc3+lc3, off3+l3, shp3.product());
    uInt lc4 = io.putShape (shp4, offc4, static_cast<Bool*>(0));
    cout << "copy to " << lc4 << " " << offc4 << endl;
    io.copyArrayBool (offc4+lc4, off4+l4, shp4.product());
}

// Read back.
void c (Bool canonical, Int64 off1, Int64 off2, Int64 off3, Int64 off4)
{
    StManArrayFile io("tStArrayFile_tmp.data", ByteIO::Old, 0, canonical);
    cout << "Length=" << io.length() << endl;
    uInt nref;
    IPosition shp;
    Bool bbuf[10000];
    Int ibuf[10000];
    Complex cbuf[10000];
    String sbuf[10000], sbufo[10000];
    char str[16];
    uInt i;
    for (i=0; i<10000; i++) {
	sprintf (str, "str %d", i);
	sbuf[i] = str;
    }
    uInt l1 = io.getShape (off1, shp);
    nref = io.getRefCount (off1);
    cout << l1 << " " << shp << " " << nref << endl;
    uInt l2 = io.getShape (off2, shp);
    nref = io.getRefCount (off2);
    cout << l2 << " " << shp << " " << nref << endl;
    uInt l3 = io.getShape (off3, shp);
    nref = io.getRefCount (off3);
    cout << l3 << " " << shp << " " << nref << endl;
    uInt l4 = io.getShape (off4, shp);
    nref = io.getRefCount (off4);
    cout << l4 << " " << shp << " " << nref << endl;
    io.get (off4+l4, 0, 10000, bbuf);
    io.get (off3+l3, 0, 10000, sbufo);
    io.get (off1+l1, 0, 10000, ibuf);
    io.get (off2+l2, 0, 10000, cbuf);
    Int j;
    for (i=0; i<10000; i++) {
	j = i;
	if (i>0 && i<21)
	    j = i-1;
	if (sbufo[i] != sbuf[j]) {
	    cout << "Mismatch " << i << ": " << sbuf[j] << " " << sbufo[j]
		 << endl;
	}
    }
    for (i=0; i<10000; i++) {
	Bool b = (i%3 == 0  ?  True : False);
	j = i;
	if (i>=1 && i<21) {
	    j = i-1;
	    b = (j%3 == 0  ?  True : False);
	}
	if (i == 23) {
	  b = True;
	}
	if (i>=34 && i<38) {
	    b = ((i-34)%3 == 0  ?  True : False);
	}
	if (ibuf[i] != j  ||  cbuf[i] != Complex(j+1,j+2)  ||  bbuf[i] != b)
	    cout << "mismatch in row " << i << ":" << ibuf[i] << " " << cbuf[i]
		 << " " << bbuf[i] << endl;
    }
}
