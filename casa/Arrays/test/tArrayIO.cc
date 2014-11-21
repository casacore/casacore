//# tArrayIO.cc: This program tests Array IO
//# Copyright (C) 1993,1994,1995,1996,1999,2002
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

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main()
{
    // Write -
    AipsIO io("arrtest.out", ByteIO::New);

    Vector<Int> v(100);
    indgen(v);
    io << v;

    Matrix<double> y(10,10);
    y = 1.0;
    y.diagonal() = 5.0;
    io << y;

    Cube<Int> z(4,4,4);
    z = 4;
    io << z;

    IPosition shape(4);
    shape = 3;
    Array<Int> a(shape);
    a = 33;
    io << a;

    io.close();
    io.open("arrtest.out", ByteIO::Old);

    Vector<Int> v2;
    Matrix<double> y2;
    Cube<Int> z2;
    Array<Int> a2;
    io >> v2 >> y2 >> z2 >> a2;
    AlwaysAssertExit(allEQ (v, v2) && allEQ (y, y2) &&
           allEQ (z, z2) && allEQ (a, a2));

    io.close();
    io.open("arrtest.out", ByteIO::Delete);
    io.close();

    cout << "OK\n";
    return 0;
}
