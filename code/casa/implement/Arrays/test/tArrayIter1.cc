//# tArrayIter1.cc: This program test the Array-based iterators
//# Copyright (C) 1993,1994,1995,1996,1998
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

#include <iostream.h>

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayPosIter.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/MatrixIter.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Utilities/Assert.h>

main()
{
    IPosition shape(3);
    shape=3;
    ArrayPositionIterator api(shape, 1);
    Int count = 0;
    while (!api.pastEnd()) {
	count++;
	api.next();
    }
    AlwaysAssertExit(count == 9);

    Cube<Int> a(3,3,3);
    MatrixIterator<Int> ai(a);
    for (Int i = 0; i < 3; i++)
	a.xyPlane(i) = i;
    count = 0;
    while (!ai.pastEnd()) {
	AlwaysAssertExit(allEQ (ai.array(), count));
	AlwaysAssertExit(allEQ (ai.matrix().arrayCast(), count));
	count++;
	ai.next();
    }

    ReadOnlyMatrixIterator<Int> roai(a);
    for (Int j = 0; j < 3; j++)
	a.xyPlane(j) = j;
    count = 0;
    while (!roai.pastEnd()) {
	AlwaysAssertExit(allEQ (roai.array(), count));
	AlwaysAssertExit(allEQ (roai.matrix().arrayCast(), count));
	count++;
	roai.next();
    }

    VectorIterator<Int> ai2(a);
    count = 0;
    while (!ai2.pastEnd()) {
	ai2.vector().set(count);
	count++;
	ai2.next();
    }

    ai.origin();
    AlwaysAssertExit(ai.atStart());

    count = 0;
    while (!ai.pastEnd()) {
	for (i=0; i<3; i++) {
	    AlwaysAssertExit(allEQ (ai.matrix().column(i).arrayCast(), count));
	    count++;
	}
	ai.next();
    }

{
    // Test iterating by the same dimensionality as the array
    Vector<Int> theArray(100);
    theArray = 0;
    ArrayPositionIterator api(theArray.shape(), 1);
    uInt count = 0;
    while (! api.pastEnd()) {
      count++;
      api.next();
    }
    AlwaysAssertExit(count == 1);

    VectorIterator<Int> vi(theArray);
    count = 0;
    while (! vi.pastEnd()) {
      count++;
      vi.next();
    }
    AlwaysAssertExit(count == 1);
}

    cout << "OK\n";
    exit(0);
}
