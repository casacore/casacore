//# tArrayLogical.cc: Test program for Array logical operators
//# Copyright (C) 1993,1994,1995,1996,1999,2000
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

#include <iostream.h>

#include <aips/aips.h>
#include <aips/Utilities/String.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>

#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Arrays/LogiVector.h>


main()
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
