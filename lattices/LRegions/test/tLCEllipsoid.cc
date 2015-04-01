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
	*/
}

/*
void show(const LCEllipsoid2& ellipse) {
	Array<bool> mask = ellipse.get();
	uInt pos = 0;
	IPosition shape = mask.shape();
	cout << shape << endl;
		for (Int j=shape(1)-1; j>=0; j--) {
			for (uInt i=0; i<shape(0); i++) {

	    	cout << mask(IPosition(2,i,j)) << " ";
		}
		cout << endl;
	}
}
*/

int main()
{
    try {
    	/*
    	Vector<Float> center(2, 30);
    	Float radius = 10;
    	IPosition latticeShape(2,60);
    	LCEllipsoid circle(center, radius, latticeShape);
    	show(circle);
*/
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
    	/*
    	{

    	Vector<Float> phi(2,1.0471975511965976);
    	phi[0] = 0;
    	phi[1] *= 0.75;
    	//Vector<Float> phi(1,0);

    	Vector<Float> center(3, 30);
    	// center[1] = 5;
    	IPosition latticeShape(3,60);

    	Vector<Float> radii(3, 10);
    	radii[1] = 5;
    	radii[2] = 7;


    	LCEllipsoid2 ellipse(center, radii, phi, latticeShape);
    	show(ellipse);
    	}

	*/
    	/*
    	{

    	Vector<Float> phi(2, 1.0471975511965976);
    	phi[1] = 0.52359877559829882;
    	Vector<Float> center(3, 30);
    	IPosition latticeShape(3,60);

    	Vector<Float> radii(3, 20);
    	radii[1] = 10;
    	radii[2] = 5;
    	LCEllipsoid2 ellipse(center, radii, phi, latticeShape);
    	show(ellipse);
    	}
    	*/


    } catch (AipsError x) {
    	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
