//# tLCPolygon.cc: Test program for LCPolygon class
//# Copyright (C) 1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <lattices/Lattices/LCPolygon.h>
#include <tables/Tables/TableRecord.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>


#include <casa/namespace.h>
void doIt (const IPosition& latticeShape,
	   const Vector<Float>& x,
	   const Vector<Float>& y)
{
    LCPolygon polygon (x, y, latticeShape);
    cout << polygon.hasMask() << ' ' << polygon.maskArray() << endl;
    cout << polygon.boundingBox().start() << polygon.boundingBox().end()
	 << polygon.boundingBox().length() << polygon.latticeShape() << endl;
    cout << polygon.x() << polygon.y() << endl;
}


int main()
{
    try {
	{
	    // A simple rectangle.
	    Vector<Float> x(4), y(4);
	    x(0)=3; y(0)=3;
	    x(1)=6; y(1)=3;
	    x(2)=6; y(2)=5;
	    x(3)=3; y(3)=5;
	    doIt (IPosition (2,11,20), x, y);
	    // Make right side a bit different.
	    x(2)=7.9;
	    doIt (IPosition (2,11,20), x, y);
	}
	{
	    // A rectangle with bottom-right corner cut off.
	    Vector<Float> x(5), y(5);
	    x(0)=3; y(0)=3;
	    x(1)=5; y(1)=3;
	    x(2)=6; y(2)=4.1;
	    x(3)=6; y(3)=5;
	    x(4)=3; y(4)=5;
	    doIt (IPosition (2,11,20), x, y);
	    // Make right side a bit different.
	    x(2)=8;
	    doIt (IPosition (2,11,20), x, y);
	}
	{
	    Vector<Float> x(7), y(7);
	    x(0)=3; y(0)=3;
	    x(1)=5; y(1)=3;
	    x(2)=5; y(2)=5;
	    x(3)=2; y(3)=5;
	    x(4)=3.6; y(4)=4;
	    x(5)=2; y(5)=3;
	    x(6)=2; y(6)=2.1;
	    doIt (IPosition (2,11,20), x, y);
	    x(3)=3;
	    doIt (IPosition (2,11,20), x, y);
	}
	{
	    // A rectangle with an inner rectangle.
	    Vector<Float> x(11), y(11);
	    x(0)=3; y(0)=3;
	    x(1)=9; y(1)=3;
	    x(2)=9; y(2)=8;
	    x(3)=3; y(3)=8;
	    x(4)=3; y(4)=3;
	    x(5)=5; y(5)=4.8;
	    x(6)=7; y(6)=5;
	    x(7)=7; y(7)=7;
	    x(8)=5; y(8)=7;
	    x(9)=5; y(9)=5;
	    x(10)=3; y(10)=3;
	    doIt (IPosition (2,11,20), x, y);
	    x(6)=8.1; y(6)=4;
	    doIt (IPosition (2,11,20), x, y);
	}
	{
	    // A cross-like figure
	    Vector<Float> x(4), y(4);
	    x(0)=3; y(0)=3;
	    x(1)=9; y(1)=3;
	    x(2)=3; y(2)=8;
	    x(3)=9; y(3)=8;
	    doIt (IPosition (2,11,20), x, y);
	}
	{
	    // A pentagram (with the inner pentagon excluded)
	    Vector<Float> x(5), y(5);
	    x(0)=0.8; y(0)=0;
	    x(1)=3; y(1)=4;
	    x(2)=5.2; y(2)=0;
	    x(3)=0; y(3)=2.8;
	    x(4)=6; y(4)=2.8;
	    doIt (IPosition (2,11,20), x, y);
	}

	{
	    // Test some other functions
            IPosition latticeShape(2,100,200);
	    Vector<Float> x(4), y(4);
	    x(0)=3; y(0)=3;
	    x(1)=6; y(1)=3;
	    x(2)=6; y(2)=5;
	    x(3)=3; y(3)=5;

            LCPolygon p1(x, y, latticeShape);
            LCPolygon p2(p1);
            AlwaysAssertExit (p2 == p1);

            p2 = p1;
            AlwaysAssertExit (p2 == p1);

            y(3) = 8;
            LCPolygon p3(x, y, latticeShape);
            AlwaysAssertExit (p3 != p1);

            TableRecord rec = p3.toRecord("");
            LCPolygon* pP3 = LCPolygon::fromRecord(rec,"");
            AlwaysAssertExit (*pP3 == p3);
            delete pP3;

            LCRegion* pRegion = p3.cloneRegion();
            pP3 = (LCPolygon*)pRegion;
            AlwaysAssertExit (*pP3 == p3);
            delete pP3;
	}

    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
