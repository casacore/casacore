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

#include <images/Regions/WCEllipsoid.h>

#include <casa/BasicSL/Constants.h>
#include <casa/Utilities/Assert.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <lattices/Lattices/LCEllipsoid.h>
#include <tables/Tables/TableRecord.h>

#include <casa/iostream.h>

#include <casa/namespace.h>

/*
void show(const LCEllipsoid& ellipse) {
	Array<bool> mask = ellipse.get();
	IPosition shape = mask.shape();
	IPosition index = shape-1;
	uInt j=0;
	while(True) {
		for (uInt i=0; i<shape(0); i++) {
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
	*/
/*
	cout << shape << endl;
		for (Int j=shape(1)-1; j>=0; j--) {
			for (uInt i=0; i<shape(0); i++) {

			IPosition x;
			x.
	    	cout << mask[count] << " ";
	    	mask.
	    	count++;
		}
		cout << endl;
	}

}
*/

int main()
{
    try {
		CoordinateSystem csys = CoordinateUtil::defaultCoords3D();
    	{
    		Vector<Quantity> center(3, Quantity(0, "rad"));
    		center[1] += Quantity(20.5, "arcmin");
    		Vector<Quantity> radius(3, Quantity(5, "arcmin"));
    		IPosition pixelAxes(3, 0, 1, 2);
    		try {
    			WCEllipsoid(center, radius, pixelAxes, csys);
    			AlwaysAssert(False, AipsError);
    		}
    		catch (AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
    		}
    		radius[2] = Quantity(50, "MHz");
    		try {
    			WCEllipsoid(center, radius, pixelAxes, csys);
    			AlwaysAssert(False, AipsError);
    		}
    		catch (AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
    		}
    		center[2] = Quantity(1415, "GHz");
    		pixelAxes[2] = 3;
    		try {
    			WCEllipsoid(center, radius, pixelAxes, csys);
    			AlwaysAssert(False, AipsError);
    		}
    		catch (AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
    		}
    		pixelAxes = IPosition(3, 0, 0, 1);
    		try {
    			WCEllipsoid(center, radius, pixelAxes, csys);
    			AlwaysAssert(False, AipsError);
    		}
    		catch (AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
    		}
    		pixelAxes = IPosition(3, 0, 1, 2);
    		center.resize(2, True);
    		try {
    			WCEllipsoid(center, radius, pixelAxes, csys);
    			AlwaysAssert(False, AipsError);
    		}
    		catch (AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
    		}

    		// generic ellipsoid tests
    		center.resize(3, True);
    		center[2] = Quantity(1.41501, "GHz");
    		radius[1] = Quantity(1200, "arcsec");
    		radius[2] = Quantity(50, "kHz");

			WCEllipsoid ellipse(center, radius, pixelAxes, csys);
			AlwaysAssert(ellipse == ellipse, AipsError);
			WCEllipsoid ellipse2 = ellipse;
			AlwaysAssert(ellipse == ellipse2, AipsError);
			WCEllipsoid *ellipse3 = dynamic_cast<WCEllipsoid *>(ellipse.cloneRegion());
			AlwaysAssert(ellipse == *ellipse3, AipsError);
			TableRecord rec = ellipse.toRecord("");
			AlwaysAssert(ellipse == *(WCEllipsoid::fromRecord(rec, "")), AipsError);
			IPosition latticeShape(3, 20, 30, 40);
			IPosition pixelAxesMap(3, 0, 1, 2);
			IPosition outOrder(3, 0, 1, 2);
			LCRegion *lcReg = ellipse.doToLCRegion(
				csys, latticeShape, pixelAxesMap, outOrder
			);
			LCEllipsoid *lcEllipse = dynamic_cast<LCEllipsoid *>(lcReg);
			IPosition lcShape = lcReg->shape();

			Vector<Float> lcCenter = lcEllipse->center();
			AlwaysAssert(near(lcCenter[0], 0.0), AipsError);
			AlwaysAssert(near(lcCenter[1], 20.5), AipsError);
			AlwaysAssert(near(lcCenter[2], 10.0), AipsError);
			Vector<Float> lcRadii = lcEllipse->radii();
			Vector<Double> pixel(3, 1);
			Vector<Double> world1;
			csys.toWorld(world1, pixel);
			pixel = 2;

			Vector<Double> world2;

			csys.toWorld(world2, pixel);
			AlwaysAssert(near(lcRadii[0], 5.0), AipsError);
			AlwaysAssert(near(lcRadii[1], 20.0), AipsError);
			AlwaysAssert(near(lcRadii[2], 50.0), AipsError);

			outOrder = IPosition(3, 1, 2, 0);

			lcReg = ellipse.doToLCRegion(
				csys, latticeShape, pixelAxesMap, outOrder
			);
			AlwaysAssert(
				lcReg->shape()
					== IPosition(3, lcShape[2], lcShape[0], lcShape[1]),
				AipsError
			);
			lcEllipse = dynamic_cast<LCEllipsoid *>(lcReg);
			lcCenter = lcEllipse->center();
			AlwaysAssert(near(lcCenter[1], 0.0), AipsError);
			AlwaysAssert(near(lcCenter[2], 20.5), AipsError);
			AlwaysAssert(near(lcCenter[0], 10.0), AipsError);
			lcRadii = lcEllipse->radii();
			AlwaysAssert(near(lcRadii[1], 5.0), AipsError);
			AlwaysAssert(near(lcRadii[2], 20.0), AipsError);
			AlwaysAssert(near(lcRadii[0], 50.0), AipsError);

			outOrder = IPosition(3, 0, 1, 2);
			pixelAxesMap = IPosition(3, 1, 2, 0);

			lcReg = ellipse.doToLCRegion(
				csys, latticeShape, pixelAxesMap, outOrder
			);
			AlwaysAssert(
				lcReg->shape()
					== IPosition(3, lcShape[1], lcShape[2], lcShape[0]),
				AipsError
			);
			lcEllipse = dynamic_cast<LCEllipsoid *>(lcReg);
			lcCenter = lcEllipse->center();
			AlwaysAssert(near(lcCenter[2], 0.0), AipsError);
			AlwaysAssert(near(lcCenter[0], 20.5), AipsError);
			AlwaysAssert(near(lcCenter[1], 10.0), AipsError);
			lcRadii = lcEllipse->radii();
			AlwaysAssert(near(lcRadii[2], 5.0), AipsError);
			AlwaysAssert(near(lcRadii[0], 20.0), AipsError);
			AlwaysAssert(near(lcRadii[1], 50.0), AipsError);

			// pixelAxesmap and outOrder the same means no net change :)
			outOrder = IPosition(3, 1, 2, 0);
			lcReg = ellipse.doToLCRegion(
				csys, latticeShape, pixelAxesMap, outOrder
			);
			AlwaysAssert(
				lcReg->shape() == lcShape,
				AipsError
			);
			lcEllipse = dynamic_cast<LCEllipsoid *>(lcReg);
			lcCenter = lcEllipse->center();
			AlwaysAssert(near(lcCenter[0], 0.0), AipsError);
			AlwaysAssert(near(lcCenter[1], 20.5), AipsError);
			AlwaysAssert(near(lcCenter[2], 10.0), AipsError);
			lcRadii = lcEllipse->radii();
			AlwaysAssert(near(lcRadii[0], 5.0), AipsError);
			AlwaysAssert(near(lcRadii[1], 20.0), AipsError);
			AlwaysAssert(near(lcRadii[2], 50.0), AipsError);
    	}
    	{
			// sphere tests
    		Vector<Quantity> center(3, Quantity(1, "rad"));
			center[2] = Quantity(1415, "GHz");
	    	IPosition pixelAxes = IPosition(3, 0, 1, 2);
	    	Quantity r(1, "arcmin");
	    	try {
				// unit mismatch between center and radius
				WCEllipsoid sphere(
					center, r, pixelAxes, csys
				);
	    		AlwaysAssert(False, AipsError);
			}
			catch(AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
			}
	    	pixelAxes.resize(2, True);
    		center.resize(2, True);
			WCEllipsoid sphere(
				center, r, pixelAxes, csys
			);
			AlwaysAssert(sphere == sphere, AipsError);
			WCEllipsoid sphere2 = sphere;
			AlwaysAssert(sphere == sphere2, AipsError);
			WCEllipsoid *sphere3 = dynamic_cast<WCEllipsoid *>(sphere.cloneRegion());
			AlwaysAssert(sphere == *sphere3, AipsError);
			TableRecord rec = sphere.toRecord("");
			AlwaysAssert(sphere == *(WCEllipsoid::fromRecord(rec, "")), AipsError);


    	}
    	{
			// 2-D ellipse tests
    		Vector<Quantity> center(3, Quantity(1, "rad"));
			center[2] = Quantity(1415, "GHz");
    		Vector<Quantity> radius(3, Quantity(1, "arcmin"));
    		radius[2] = Quantity(50, "MHz");
	    	IPosition pixelAxes = IPosition(3, 0, 1, 2);
	    	try {
				// theta unit issue
	    		Quantity theta(4, "Hz");
	    		WCEllipsoid ellipse(
	    			center[0], center[1], radius[0], radius[1],
	    			theta, pixelAxes[0], pixelAxes[1], csys
	    		);
	    		AlwaysAssert(False, AipsError);
			}
			catch(AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
			}
	    	try {
				// axes unit mismatch
	    		Quantity theta(40, "deg");
	    		WCEllipsoid ellipse(
	    			center[0], center[1], radius[0], radius[1],
	    			theta, pixelAxes[0], pixelAxes[2], csys
	    		);
	    		AlwaysAssert(False, AipsError);
			}
			catch(AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
			}
			radius[0].setValue(2);
    		Quantity theta(40, "deg");
    		WCEllipsoid ellipse(
    			center[0], center[1], radius[0], radius[1],
    			theta, pixelAxes[0], pixelAxes[1], csys
    		);

			AlwaysAssert(ellipse == ellipse, AipsError);
			WCEllipsoid ellipse2 = ellipse;
			AlwaysAssert(ellipse == ellipse2, AipsError);
			WCEllipsoid *ellipse3 = dynamic_cast<WCEllipsoid *>(ellipse.cloneRegion());
			AlwaysAssert(ellipse == *ellipse3, AipsError);
			TableRecord rec = ellipse.toRecord("");
			AlwaysAssert(ellipse == *(WCEllipsoid::fromRecord(rec, "")), AipsError);

			// switch axes order
			try {
				// major axis smaller than minor axis
				ellipse = WCEllipsoid(
					center[1], center[0], radius[1], radius[0],
					theta, pixelAxes[1], pixelAxes[0], csys
				);
	    		AlwaysAssert(False, AipsError);
			}
			catch(AipsError x) {
				cout << "Caught as expected " << x.getMesg() << endl;
			}
			ellipse = WCEllipsoid(
				center[1], center[0], radius[0], radius[1],
				theta, pixelAxes[1], pixelAxes[0], csys
			);
			AlwaysAssert(ellipse == ellipse, AipsError);
			ellipse2 = ellipse;
			AlwaysAssert(ellipse == ellipse2, AipsError);
			ellipse3 = dynamic_cast<WCEllipsoid *>(ellipse.cloneRegion());
			AlwaysAssert(ellipse == *ellipse3, AipsError);
			rec = ellipse.toRecord("");
			AlwaysAssert(ellipse == *(WCEllipsoid::fromRecord(rec, "")), AipsError);
    	}

    } catch (AipsError x) {
    	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
