//# tArrayLogical.cc: Test program for Array logical operators
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

//# If AIPS_DEBUG is not set, the Assert's won't be called.
#if !defined(AIPS_DEBUG)
#define AIPS_DEBUG
#endif

//# For extra debugging
#if !defined(AIPS_ARRAY_INDEX_CHECK)
#define AIPS_ARRAY_INDEX_CHECK
#endif

#include <casacore/casa/iostream.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/LogiVector.h>


#include <casacore/casa/namespace.h>
int main()
{
    try {
        {
            cout << endl << "Testing Array logical operators." << endl;

            Vector<Int> x(5), y(5);
            LogicalVector b(5), c(5);
            x=1;
            indgen (y);

            cout << endl << "x= " << endl;
            cout << x << endl;
            cout << endl << "y= " << endl;
            cout << y << endl;

            AlwaysAssertExit (allSame(x));
            AlwaysAssertExit (!allSame(y));

            b = (x <= y);
            cout << endl << "b= (x <= y) = " << endl;
            cout << b << endl;

            b = (x < y);
            cout << endl << "b= (x < y) = " << endl;
            cout << b << endl;

            b = (x >= y);
            cout << endl << "b= (x >= y) = " << endl;
            cout << b << endl;

            b = (x > y);
            cout << endl << "b= (x > y) = " << endl;
            cout << b << endl;

            b = (x == y);
            cout << endl << "b= (x == y) = " << endl;
            cout << b << endl;

            b = (x != y);
            cout << endl << "b= (x != y) = " << endl;
            cout << b << endl;

            b = ((const Array<Int> &)y <= 1);
            cout << endl << "b= (y <= 1) = " << endl;
            cout << b << endl;

            b = ((const Array<Int> &)y < 1);
            cout << endl << "b= (y < 1) = " << endl;
            cout << b << endl;

            b = ((const Array<Int> &)y >= 1);
            cout << endl << "b= (y >= 1) = " << endl;
            cout << b << endl;

            b = ((const Array<Int> &)y > 1);
            cout << endl << "b= (y > 1) = " << endl;
            cout << b << endl;

            b = ((const Array<Int> &)y == 1);
            cout << endl << "b= (y == 1) = " << endl;
            cout << b << endl;

            b = ((const Array<Int> &)y != 1);
            cout << endl << "b= (y != 1) = " << endl;
            cout << b << endl;

            b = (1 <= (const Array<Int> &)y);
            cout << endl << "b= (1 <= y) = " << endl;
            cout << b << endl;

            b = (1 < (const Array<Int> &)y);
            cout << endl << "b= (1 < y) = " << endl;
            cout << b << endl;

            b = (1 >= (const Array<Int> &)y);
            cout << endl << "b= (1 >= y) = " << endl;
            cout << b << endl;

            b = (1 > (const Array<Int> &)y);
            cout << endl << "b= (1 > y) = " << endl;
            cout << b << endl;

            b = (1 == (const Array<Int> &)y);
            cout << endl << "b= (1 == y) = " << endl;
            cout << b << endl;

            b = (1 != (const Array<Int> &)y);
            cout << endl << "b= (1 != y) = " << endl;
            cout << b << endl;

            b = ! ((const Array<Int> &)y >= 3);
            c =   ((const Array<Int> &)y <  3);
            cout << endl << "b= ! (y >= 3) = " << endl;
            cout << b << endl;
            cout << "c=   (y <  3) = " << endl;
            cout << c << endl;
            cout << "allEQ (b, c) = " << allEQ (b, c) << endl;


            Vector<Double> x1(2), x2(2);
            x1 = 10000; x2(0) = 10001; x2(1) = 10002;
            cout << endl << " near(x1, x2, 0.99e-4) " << near(x1, x2, 0.99e-4);
            cout << endl << " near(x1, x2, 1.01e-4) " << near(x1, x2, 1.01e-4);
            cout << endl << " near(x1, x2, 2.01e-4) " << near(x1, x2, 2.01e-4);
            cout << endl << " nearAbs(x1, x2, 0.99) " << nearAbs(x1, x2, 0.99);
            cout << endl << " nearAbs(x1, x2, 1.01) " << nearAbs(x1, x2, 1.01);
            cout << endl << " nearAbs(x1, x2, 2.01) " << nearAbs(x1, x2, 2.01);

            cout << endl << "OK" << endl;
        }
    } catch (AipsError x) {
        cout << "\nCaught an exception: " << x.getMesg() << endl;
    } 

    cout << "OK" << endl;
    return 0;
}
