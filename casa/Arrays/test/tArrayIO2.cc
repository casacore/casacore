//# tArrayIO2.cc: This program tests the tArrayIO2 functions
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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

#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This test program tests the ArrayIO2 functions.
// It writes all kind of stuff, reads it back and writes it to stdout.
// A script compares this output with a reference output file.

//# Forward declaration.
void doBin (Bool doExcp);
void doMat();
void doVec();


int main (int argc, const char*[])
{
    try {
	doBin ( (argc<2));
	doMat();
	doVec();
    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } 
    cout << "end" << endl;
    return 0;                       // successfully executed
}


void doBin (Bool)
{
    Vector<Int> ip(1000);
    for (uInt i=0; i<1000; i++) {
	ip(i) = i*2;
    }
    write_array (ip, "tArrayIO2_tmp.data");
    Vector<Int> ipi(1000);
    read_array (ipi, "tArrayIO2_tmp.data");
    for (Int i=0; i<1000; i++) {
	if (ipi(i) != i*2) {
	    cout << "Get error in ip[" << i << "]" << endl;
	}
    }
}


void doMat()
{
    Matrix<Int> mat;
    readAsciiMatrix (mat, "tArrayIO2.in_mat");
    cout << mat;
    cout << endl;
    writeAsciiMatrix (mat, "tArrayIO2_tmp.mat");
}

void doVec()
{
    Vector<double> vec;
    readAsciiVector (vec, "tArrayIO2.in_vec");
    cout << vec;
    cout << endl;
    writeAsciiVector (vec, "tArrayIO2_tmp.vec");
}
