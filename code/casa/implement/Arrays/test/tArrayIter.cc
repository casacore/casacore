//# tArrayIter.cc: This program tests Array iteration
//# Copyright (C) 1993,1994,1995,1999,2001
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

#include <casa/aips.h>
#include <casa/Arrays/ArrayIter.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Cube.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Assert.h>

#include <casa/iostream.h>

main()
{
{   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  0 dim. ......\n\n";

    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 0);
    IPosition index (2);
    Int iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] \n";
    }

    cout << "\nEND.  Testing ArrayPositionIterator.  0 dim. ......\n\n";

}

{   
    cout << "\nBEGIN.  Testing ArrayPositionIterator.  1 dim. ......\n\n";

    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 1);
    Array<Int> arr (shape);

    IPosition index (2);
    Int iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        arr (index) = 4 * iloop;
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] "
             << arr (index) << "\n";
    }

    cout << "\nEND.  Testing ArrayPositionIterator.  1 dim. ......\n\n";

    cout << "\nBEGIN.  Testing ArrayIterator.  1 dim. ......\n\n";

/*=========================================================================
    ArrayIterator<Int> arri (arr, 1);
    IPosition arriindex (1);

    arriindex (0) = 0;

    for ( iloop = 0; ! arri.pastEnd(); arri.next(), iloop++ ) {
        cout << iloop << "  " << (arri.array ()) (arriindex) << "\n";
    }

    cout << "\nEND.  Testing ArrayIterator.  1 dim. ......\n\n";
=========================================================================*/

}

/*------------------------------------------------------------------------*/

{   
    cout << "\nBEGIN.  Testing double ArrayPositionIterator.  1 dim. ......\n\n";

    IPosition shape(2);
    shape(0) = shape(1) = 5;

    ArrayPositionIterator ai (shape, 1);
    Array<double> arr (shape);

    IPosition index (2);
    Int iloop;

    for ( iloop = 0; ! ai.pastEnd(); ai.next(), iloop++ ) {
        index = ai.pos();
        arr (index) = double (4 * iloop) + 0.5;
        cout << iloop << " [ " << index (0) << " " << index (1) << " ] "
             << arr (index) << "\n";
    }

    cout << "\nEND.  Testing double ArrayPositionIterator.  1 dim. ......\n\n";

    cout << "\nBEGIN.  Testing double ArrayIterator.  1 dim. ......\n\n";

    ArrayIterator<double> arri (arr, 1);
    IPosition arriindex (1);

    arriindex (0) = 0;

    for ( iloop = 0; ! arri.pastEnd(); arri.next(), iloop++ ) {
        cout << iloop << "  " << (arri.array ()) (arriindex) << "\n";
    }

    cout << "\nEND.  Testing double ArrayIterator.  1 dim. ......\n\n";

}

{
    // Test that leading degenerate axes do not go away
    Array<Int> ai(IPosition(5, 1, 2, 3, 4, 5));
    indgen(ai);
    ArrayIterator<Int> iter(ai, 2);
    AlwaysAssertExit(iter.array().shape() == IPosition(2,1,2));

    // Test that a cursor as large as the array works
    ArrayIterator<Int> aiter(ai, 5);
    AlwaysAssertExit(allEQ(aiter.array(), ai));
    aiter.next();
    AlwaysAssertExit(aiter.pastEnd());
}

    return 0;
}
