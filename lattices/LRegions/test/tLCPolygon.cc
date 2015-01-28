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

#include <casacore/lattices/LRegions/LCPolygon.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt (const IPosition& latticeShape,
	   const Vector<Float>& x,
	   const Vector<Float>& y)
{
    LCPolygon polygon (x, y, latticeShape);
    cout << polygon.boundingBox().start() << polygon.boundingBox().end()
	 << polygon.boundingBox().length() << polygon.latticeShape() << endl;
    cout << polygon.x() << polygon.y() << endl;
    cout << polygon.hasMask() << ' ' << polygon.maskArray() << endl;
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
	    x(2)=8;
	    doIt (IPosition (2,11,20), x, y);
	    x(2)=8.1;
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
            // A rectangle where the left side is a bit strange.
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
            // A circle like polygon.
            Float x[] = {523.974, 523.933, 523.809, 523.604, 523.317, 522.953, 522.515, 522.002, 521.422, 520.776, 520.068, 519.306, 518.493, 517.635, 516.738, 515.809, 514.852, 513.877, 512.889, 511.893, 510.901, 509.913, 508.941, 507.991, 507.068, 506.179, 505.329, 504.527, 503.775, 503.082, 502.448, 501.882, 501.386, 500.962, 500.614, 500.347, 500.157, 500.052, 500.028, 500.087, 500.229, 500.452, 500.754, 501.133, 501.59, 502.117, 502.711, 503.372, 504.09, 504.864, 505.687, 506.554, 507.457, 508.394, 509.355, 510.333, 511.325, 512.319, 513.313, 514.297, 515.264, 516.209, 517.127, 518.007, 518.847, 519.638, 520.378, 521.06, 521.677, 522.231, 522.712, 523.12, 523.449, 523.7, 523.871, 523.96};
            Float y[] = {512.001, 512.997, 513.985, 514.963, 515.918, 516.846, 517.741, 518.595, 519.406, 520.165, 520.866, 521.508, 522.083, 522.589, 523.02, 523.377, 523.655, 523.851, 523.966, 523.999, 523.95, 523.816, 523.601, 523.306, 522.935, 522.485, 521.966, 521.376, 520.722, 520.006, 519.237, 518.418, 517.554, 516.65, 515.716, 514.755, 513.775, 512.785, 511.787, 510.79, 509.804, 508.831, 507.882, 506.96, 506.073, 505.227, 504.427, 503.68, 502.991, 502.364, 501.803, 501.312, 500.898, 500.557, 500.298, 500.116, 500.019, 500.005, 500.073, 500.223, 500.454, 500.766, 501.156, 501.62, 502.156, 502.758, 503.427, 504.153, 504.934, 505.764, 506.638, 507.548, 508.488, 509.454, 510.435, 511.43};
            IPosition shape(2, 1024,1024);
            Vector<Float> xv(IPosition(1,sizeof(x)/sizeof(Float)), x, SHARE);
            Vector<Float> yv(IPosition(1,sizeof(y)/sizeof(Float)), y, SHARE);
            doIt (shape, xv, yv);
        }
        {
            // The letter E.
            Float x[] = {1,11,11,4, 4,11,11, 4, 4,11,11, 1};
            Float y[] = {1, 1, 4,4,10,10,13,13,19,19,22,22};
            IPosition shape(2, 1024,1024);
            Vector<Float> xv(IPosition(1,sizeof(x)/sizeof(Float)), x, SHARE);
            Vector<Float> yv(IPosition(1,sizeof(y)/sizeof(Float)), y, SHARE);
            doIt (shape, xv, yv);
        }
        {
            // An almost empty diamond.
            Float x[] = {0.1, 4.0, 7.9, 4.0};
            Float y[] = {1.5, 1.1, 1.5, 2.1};
            IPosition shape(2, 1024,1024);
            Vector<Float> xv(IPosition(1,sizeof(x)/sizeof(Float)), x, SHARE);
            Vector<Float> yv(IPosition(1,sizeof(y)/sizeof(Float)), y, SHARE);
            doIt (shape, xv, yv);
        }
        {
            // A box with a negative start.
            Vector<Float> x(4), y(4);
            x[0] = -13;  y[0] = 1;
            x[1] = 4;    y[1] = 1;
            x[2] = 4;    y[2] = 6;
            x[3] = -13;  y[3] = 6;
            IPosition shape(2, 11, 6);
            doIt(shape, x, y);
        }
        {
            // A tilted box with all points outside lattice.
            Vector<Float> x(4), y(4);
            x[0] = -2;  y[0] = 3;
            x[1] = 3;   y[1] = -1;
            x[2] = 8;   y[2] = 3;
            x[3] = 3;   y[3] = 7;
            IPosition shape(2, 7, 7);
            doIt(shape, x, y);
        }
        {
            // A box entirely outside lattice.
            Vector<Float> x(4), y(4);
            x[0] = 12;  y[0] = 12;
            x[1] = 14;  y[1] = 12;
            x[2] = 14;  y[2] = 14;
            x[3] = 12;  y[3] = 14;
            IPosition shape(2, 7, 7);
            try {
              doIt(shape, x, y);
            } catch (AipsError& x) {
              cout << x.what() << endl;
            }
        }
        {
            // A polygon just inside lattice, but no points matching.
            Vector<Float> x(3), y(3);
            x[0] = 2.5;  y[0] = 1.1;
            x[1] = 3;    y[1] = 5;
            x[2] = 2;    y[2] = 5;
            IPosition shape(2, 5, 3);
            try {
              doIt(shape, x, y);
            } catch (AipsError& x) {
              cout << x.what() << endl;
            }
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
