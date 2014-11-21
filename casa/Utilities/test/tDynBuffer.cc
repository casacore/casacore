//# tDynBuffer.cc: This program tests the DynBuffer class
//# Copyright (C) 1993,1994,1995,2001
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

#include <casacore/casa/Utilities/DynBuffer.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// This program tests the DynBuffer class.
// It allocates buffers, stores data in it and reads it back.
// The results are written to stdout. A script executes this test program.

int main () {
    uInt nrval,n;
    union {
        Char* ptr;
	uInt  p;                           // to display a pointer
    };
    DynBuffer buf;
    nrval = 100;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);       // allocate for nrval bytes
	cout << n << " " << p << endl;
	nrval -= n;
    }
    nrval = 1000;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);       // allocate for nrval bytes more
	cout << n << " " << p << endl;
	nrval -= n;
    }
    buf.nextstart();
    while (buf.next (n,ptr)) {             // loop through allocated buffers
	cout << "next " << n << " " << p << endl;
    }

    buf.allocstart();                      // dismiss buffer contents
    nrval = 1000;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);       // reuse previous buffers
	cout << n << " " << p << endl;
	nrval -= n;
    }
    nrval = 101;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);       // reuse and allocate for 1 byte more
	cout << n << " " << p << endl;
	nrval -= n;
    }
    buf.nextstart();
    while (buf.next (n,ptr)) {
	cout << "next " << n << " " << p << endl;
    }
    
    buf.remove(1);                         // remove buffers (keep 1)
    buf.allocstart();
    nrval = 1000;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);
	cout << n << " " << p << endl;
	nrval -= n;
    }
    nrval = 101;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);
	cout << n << " " << p << endl;
	nrval -= n;
    }
    buf.nextstart();
    while (buf.next (n,ptr)) {
	cout << "next " << n << " " << p << endl;
    }
    
    buf.remove(0);
    buf.allocstart();
    nrval = 1000;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);
	cout << n << " " << p << endl;
	nrval -= n;
    }
    nrval = 101;
    while (nrval > 0) {
	n = buf.alloc (nrval,8,ptr);
	cout << n << " " << p << endl;
	nrval -= n;
    }
    buf.nextstart();
    while (buf.next (n,ptr)) {
	cout << "next " << n << " " << p << endl;
    }

    return 0;                                 // exit with success status
}
