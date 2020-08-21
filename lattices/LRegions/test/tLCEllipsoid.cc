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

#include <casacore/lattices/LRegions/LCEllipsoid.h>

#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/Tables/TableRecord.h>

#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>

void show(const LCEllipsoid& ellipse) {
    Array<bool> mask = ellipse.get();
    IPosition shape = mask.shape();
    IPosition index = shape-1;
    uInt j=0;
    while(True) {
        for (Int i=0; i<shape(0); i++) {
            index[0] = i;
            cout << mask(index) << " ";
        }
        cout << index << endl;
        for (j=1; j<shape.size(); j++) {
            if (index[j] == 0) {
                index[j] = shape[j]-1;
                cout << endl;
            }
            else {
                index[j]--;
                break;
            }

        }
        if (j == shape.size()) {
            break;
        }
    }
}

int main() {
    try {
        {
            Float theta = C::pi/3;
            Float xcenter = 30;
            Float ycenter = 30;
            IPosition latticeShape(2,60);
            Float major = 20;
            Float minor = 10;
            LCEllipsoid ellipse(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse);
            LCEllipsoid *copy = dynamic_cast<LCEllipsoid *>(LCEllipsoid::fromRecord(ellipse.toRecord(""), ""));
            AlwaysAssert(ellipse == *copy, AipsError);
            near(ellipse.theta(), copy->theta());
            delete copy;

            Float theta2 = theta + C::pi;
            LCEllipsoid ellipse2(
                xcenter, ycenter, major, minor, theta2, latticeShape
            );
            AlwaysAssert(ellipse == ellipse2, AipsError);
            near(ellipse.theta(), ellipse2.theta());

            Float theta3 = theta - C::pi;
            LCEllipsoid ellipse3(
                xcenter, ycenter, major, minor, theta3, latticeShape
            );
            AlwaysAssert(ellipse == ellipse3, AipsError);
            near(ellipse.theta(), ellipse3.theta());
        }
        {
            Float theta = 0;
            Float xcenter = 30;
            Float ycenter = 30;
            Vector<Float> center(2,xcenter);
            center[1] = ycenter;
            IPosition latticeShape(2,60);
            Float major = 20;
            Float minor = 10;
            Vector<Float> radii(2, major);
            radii[1] = minor;
            LCEllipsoid ellipse(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            LCEllipsoid ellipse2(
                    center, radii, latticeShape
            );
            show(ellipse);
            show(ellipse2);
            LCEllipsoid *copy = dynamic_cast<LCEllipsoid *>(LCEllipsoid::fromRecord(ellipse2.toRecord(""), ""));
            AlwaysAssert(ellipse == ellipse2, AipsError);
            AlwaysAssert(ellipse == *copy, AipsError);
            near(ellipse.theta(), ellipse2.theta());
            delete copy;

            Float theta2 = C::pi/2;
            LCEllipsoid ellipse3(
                xcenter, ycenter, major, minor, theta2, latticeShape
            );
            radii[0] = minor;
            radii[1] = major;
            LCEllipsoid ellipse4(
                center, radii, latticeShape
            );
            AlwaysAssert(ellipse3 == ellipse4, AipsError);
            near(ellipse3.theta(), ellipse4.theta());
        }
        {
            Float theta = C::pi/4;
            Float xcenter = 40;
            Float ycenter = 40;
            IPosition latticeShape(2,60);
            Float major = 10;
            Float minor = 5;
            // off center ellipse
            LCEllipsoid ellipse(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse);
        }
        {
            // all of ellipse outside lattice
            Float xcenter = 80;
            Float ycenter = 80;
            Vector<Float> center(2,xcenter);
            center[1] = ycenter;
            IPosition latticeShape(2,60);
            Float major = 20;
            Float minor = 10;
            Vector<Float> radii(2, major);
            radii[1] = minor;
            Bool thrown = False;
            try {
                LCEllipsoid e0(center, radii, latticeShape);
            }
            catch (const std::exception& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            // 2-D with non-zero theta, test exception is thrown from _define2D()
            // since ellipse totally outside lattice
            xcenter = 40;
            ycenter = 80;
            major = 40;
            minor = 10;
            // 5 degrees
            Float theta = C::pi/36;
            thrown = False;
            try {
                LCEllipsoid ellipse(
                    xcenter, ycenter, major, minor, theta, latticeShape
                );
            }
            catch (const std::exception& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
        }
        {
            // center outside lattice, but part of the ellipse in inside
            // lattice
            Float xcenter = -10;
            Float ycenter = -10;
            Vector<Float> center(2,xcenter);
            center[1] = ycenter;
            IPosition latticeShape(2,60);
            Float major = 30;
            Float minor = 20;
            Vector<Float> radii(2, major);
            radii[1] = minor;
            LCEllipsoid ellipse(center, radii, latticeShape);
            show(ellipse);

            center[0] = 69;
            LCEllipsoid ellipse1(center, radii, latticeShape);
            show(ellipse1);

            center[1] = 69;
            LCEllipsoid ellipse2(center, radii, latticeShape);
            show(ellipse2);

            center[0] = -10;
            LCEllipsoid ellipse3(center, radii, latticeShape);
            show(ellipse3);

            Float theta = C::pi/4;
            major = 36.01;
            minor = 16.01;
            xcenter = -1;
            ycenter = -1;
            LCEllipsoid ellipse4(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse4);

            xcenter = 60;
            ycenter = -1;
            LCEllipsoid ellipse5(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse5);

            xcenter = 60;
            ycenter = 60;
            LCEllipsoid ellipse6(
                    xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse6);

            xcenter = -1;
            ycenter = 60;
            LCEllipsoid ellipse7(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse7);
        }
    }
    catch (const std::exception& x) {
        cout << "Caught exception: " << x.what() << endl;
        return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
