//# tAipsIOCarray.cc: This program tests the AipsIOCarray functions
//# Copyright (C) 1993,1994,1995,1996,1998,2000,2001,2002
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

//# Includes

#include <casacore/casa/IO/test/tAipsIOCarray.h>
#include <casacore/casa/IO/AipsIOCarray.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This test program tests the AipsIOCarray functions.
// It writes all kind of stuff, reads it back and writes it to stdout.
// A script compares this output with a reference output file.

//# Forward declaration.
void doit (Bool doExcp);


int main (int argc, const char*[])
{
    try {
	doit ( (argc<2));
    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } 
    cout << "end" << endl;
    return 0;                       // successfully executed
}

void doit (Bool)
{
    {
	Complex cp[1000];
	Int ip[1000];
	AipsIOCarrayEx1 ap[1000];
	for (uInt i=0; i<1000; i++) {
	    cp[i] = Complex (float(i), float(i+4));
	    ip[i] = i*2;
	    ap[i] = AipsIOCarrayEx1 (i+10, i+20);
	}
	AipsIO io ("tAipsIOCarray_tmp.data", ByteIO::New);
	io.putstart ("tAipsIOCarray",1);
	putAipsIO (io, uInt(1000), cp);
	putAipsIO (io, uInt(1000), ap);
	putAipsIO (io, uInt(1000), ip);
	io.putend();
    }
    {
	Int i;
	Complex cpi[1000];
	Int ipi[1000];
	AipsIOCarrayEx1 api[1000];
	uInt n;
	AipsIO io ("tAipsIOCarray_tmp.data");
	io.getstart ("tAipsIOCarray");
	io >> n;
	getAipsIO (io, n, cpi);
	for (i=0; i<1000; i++) {
	    if (cpi[i] != Complex (float(i), float(i+4))) {
		cout << "Get error in cp[" << i << "]" << endl;
	    }
	}
	io >> n;
	getAipsIO (io, n, api);
	for (i=0; i<1000; i++) {
	    if (api[i].a() != i+10  ||  api[i].b() != i+20) {
		cout << "Get error in ap[" << i << "]" << endl;
	    }
	}
	io >> n;
	getAipsIO (io, n, ipi);
	for (i=0; i<1000; i++) {
	    if (ipi[i] != i*2) {
		cout << "Get error in ip[" << i << "]" << endl;
	    }
	}
	io.getend();
    }
    {
	Int i;
	Complex* cpi;
	Int* ipi;
	AipsIOCarrayEx1* api;
	uInt n;
	AipsIO io ("tAipsIOCarray_tmp.data");
	io.getstart ("tAipsIOCarray");
	getnewAipsIO (io, n, &cpi);
	for (i=0; i<1000; i++) {
	    if (cpi[i] != Complex (float(i), float(i+4))) {
		cout << "Getnew error in cp[" << i << "]" << endl;
	    }
	}
	getnewAipsIO (io, n, &api);
	for (i=0; i<1000; i++) {
	    if (api[i].a() != i+10  ||  api[i].b() != i+20) {
		cout << "Getnew error in ap[" << i << "]" << endl;
	    }
	}
	getnewAipsIO (io, n, &ipi);
	for (i=0; i<1000; i++) {
	    if (ipi[i] != i*2) {
		cout << "Getnew error in ip[" << i << "]" << endl;
	    }
	}
	io.getend();
	delete [] cpi;
	delete [] ipi;
	delete [] api;
    }
}
