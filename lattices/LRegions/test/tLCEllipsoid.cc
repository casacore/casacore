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
    uint32_t j=0;
    while(true) {
        for (int32_t i=0; i<shape(0); i++) {
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
            float theta = C::pi/3;
            float xcenter = 30;
            float ycenter = 30;
            IPosition latticeShape(2,60);
            float major = 20;
            float minor = 10;
            LCEllipsoid ellipse(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse);
            LCEllipsoid *copy = dynamic_cast<LCEllipsoid *>(LCEllipsoid::fromRecord(ellipse.toRecord(""), ""));
            AlwaysAssert(ellipse == *copy, AipsError);
            near(ellipse.theta(), copy->theta());
            delete copy;

            float theta2 = theta + C::pi;
            LCEllipsoid ellipse2(
                xcenter, ycenter, major, minor, theta2, latticeShape
            );
            AlwaysAssert(ellipse == ellipse2, AipsError);
            near(ellipse.theta(), ellipse2.theta());

            float theta3 = theta - C::pi;
            LCEllipsoid ellipse3(
                xcenter, ycenter, major, minor, theta3, latticeShape
            );
            AlwaysAssert(ellipse == ellipse3, AipsError);
            near(ellipse.theta(), ellipse3.theta());
        }
        {
            float theta = 0;
            float xcenter = 30;
            float ycenter = 30;
            Vector<float> center(2,xcenter);
            center[1] = ycenter;
            IPosition latticeShape(2,60);
            float major = 20;
            float minor = 10;
            Vector<float> radii(2, major);
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

            float theta2 = C::pi/2;
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
            float theta = C::pi/4;
            float xcenter = 40;
            float ycenter = 40;
            IPosition latticeShape(2,60);
            float major = 10;
            float minor = 5;
            // off center ellipse
            LCEllipsoid ellipse(
                xcenter, ycenter, major, minor, theta, latticeShape
            );
            show(ellipse);
        }
        {
            // all of ellipse outside lattice
            float xcenter = 80;
            float ycenter = 80;
            Vector<float> center(2,xcenter);
            center[1] = ycenter;
            IPosition latticeShape(2,60);
            float major = 20;
            float minor = 10;
            Vector<float> radii(2, major);
            radii[1] = minor;
            bool thrown = false;
            try {
                LCEllipsoid e0(center, radii, latticeShape);
            }
            catch (const std::exception& x) {
                thrown = true;
            }
            AlwaysAssert(thrown, AipsError);
            // 2-D with non-zero theta, test exception is thrown from _define2D()
            // since ellipse totally outside lattice
            xcenter = 40;
            ycenter = 80;
            major = 40;
            minor = 10;
            // 5 degrees
            float theta = C::pi/36;
            thrown = false;
            try {
                LCEllipsoid ellipse(
                    xcenter, ycenter, major, minor, theta, latticeShape
                );
            }
            catch (const std::exception& x) {
                thrown = true;
            }
            AlwaysAssert(thrown, AipsError);
        }
        {
            // center outside lattice, but part of the ellipse in inside
            // lattice
            float xcenter = -10;
            float ycenter = -10;
            Vector<float> center(2,xcenter);
            center[1] = ycenter;
            IPosition latticeShape(2,60);
            float major = 30;
            float minor = 20;
            Vector<float> radii(2, major);
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

            float theta = C::pi/4;
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
