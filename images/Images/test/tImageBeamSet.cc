//# tImageConcat.cc: This program tests the ImageConcat class
//# Copyright (C) 1996,1997,1999,2000,2001
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


#include <casa/aips.h>

#include <images/Images/ImageBeamSet.h>

#include <casa/iostream.h>

#include <casa/namespace.h>

int main() {
	try {
		{
			cout << "*** Test constructors, = operator" << endl;
			// empty beam set
			ImageBeamSet x;
			AlwaysAssert(x.empty(), AipsError);
			AlwaysAssert(x.size() == 0, AipsError);
			AlwaysAssert(x.nelements() == 0, AipsError);
			AlwaysAssert(x.getAxes().size() == 0, AipsError);
			GaussianBeam beam(
					Quantity(4, "arcsec"), Quantity(3, "arcsec"),
					Quantity(40, "deg")
			);
			Vector<ImageBeamSet::AxisType> types(2);
			types[0] = ImageBeamSet::SPECTRAL;
			types[1] = ImageBeamSet::POLARIZATION;
			ImageBeamSet b(IPosition(2, 20, 4), types);
			b.set(beam);
			ImageBeamSet c = b;
			AlwaysAssert(b == b, AipsError);
			AlwaysAssert(c == b, AipsError);
			ImageBeamSet d(b);
			AlwaysAssert(d == b, AipsError);
			c = x;
			x = b;
			AlwaysAssert(x == b, AipsError);
			ImageBeamSet k(beam);
			Vector<ImageBeamSet::AxisType> ytypes(1);
			ytypes[0] = ImageBeamSet::POLARIZATION;
			ImageBeamSet y(IPosition(1, 4), ytypes);
			// test different shapes
			y = b;
			AlwaysAssert(y == b, AipsError);


		}
		{
			cout << "*** test setBeam()" << endl;
			GaussianBeam beam0(Quantity(4, "arcsec"), Quantity(3, "arcsec"), Quantity(20, "deg"));
			IPosition shape(2, 3, 4);
			Vector<ImageBeamSet::AxisType> types(2);
			types[0] = ImageBeamSet::SPECTRAL;
			types[1] = ImageBeamSet::POLARIZATION;
			ImageBeamSet x(beam0, shape, types);
			GaussianBeam beam1(Quantity(5, "arcsec"), Quantity(4, "arcsec"), Quantity(20, "deg"));
			IPosition pos2(2, 1, 2);
			x.setBeam(beam1, pos2);
			IPosition axisPath = IPosition::makeAxisPath(shape.size());
            ArrayPositionIterator iter(shape, axisPath, False);
            while (! iter.pastEnd()) {
                const IPosition pos = iter.pos();
				GaussianBeam beam = x.getBeam(pos);
				if (pos == pos2) {
					AlwaysAssert(beam == beam1, AipsError);
				}
				else {
					AlwaysAssert(beam == beam0, AipsError);
				}
                iter.next();
			}
			{
				cout << "*** test getting max and min area beams" << endl;
				GaussianBeam init(
					Quantity(4, "arcsec"), Quantity(2, "arcsec"),
					Quantity(0, "deg")
				);
				IPosition shape(2, 3, 4);
				Vector<ImageBeamSet::AxisType> types(2);
				types[0] = ImageBeamSet::SPECTRAL;
				types[1] = ImageBeamSet::POLARIZATION;
				ImageBeamSet x(init, shape, types);
				AlwaysAssert(x.getMaxAreaBeam() == init, AipsError);
				AlwaysAssert(x.getMinAreaBeam() == init, AipsError);
				GaussianBeam maxBeam(
					Quantity(10, "arcsec"), Quantity(8, "arcsec"),
					Quantity(0, "deg")
				);
				GaussianBeam minBeam(
					Quantity(1, "arcsec"), Quantity(1, "arcsec"),
					Quantity(0, "deg")
				);
				IPosition maxBeamPos(2, 2, 1);
				IPosition minBeamPos(2, 2, 3);
				x.setBeam(maxBeam, maxBeamPos);
				x.setBeam(minBeam, minBeamPos);
				AlwaysAssert(x.getMaxAreaBeam() == maxBeam, AipsError);
				AlwaysAssert(x.getMinAreaBeam() == minBeam, AipsError);
				AlwaysAssert(x.getMaxAreaBeamPosition() == maxBeamPos, AipsError);
				AlwaysAssert(x.getMinAreaBeamPosition() == minBeamPos, AipsError);
				init = GaussianBeam(
					Quantity(6, "arcsec"), Quantity(6, "arcsec"),
					Quantity(0, "deg")
				);
				shape.resize(1);
				shape = IPosition(1, 3);
				types.resize(1);
				types[0] = ImageBeamSet::SPECTRAL;
				x = ImageBeamSet(init, shape, types);
				GaussianBeam beam8 = GaussianBeam(
					Quantity(8, "arcsec"), Quantity(8, "arcsec"),
					Quantity(0, "deg")
				);
				x.setBeam(beam8, IPosition(1,1));
				GaussianBeam beam10 = GaussianBeam(
					Quantity(10, "arcsec"), Quantity(10, "arcsec"),
					Quantity(0, "deg")
				);
				x.setBeam(beam10, IPosition(1,2));
				AlwaysAssert(x.getMaxAreaBeam() == beam10, AipsError);
				AlwaysAssert(
					x.getMaxAreaBeamPosition() == IPosition(1, 2),
					AipsError
				);
			}
			{
				cout << "*** test setBeams()" << endl;
				GaussianBeam init(
					Quantity(4, "arcsec"), Quantity(2, "arcsec"),
					Quantity(0, "deg")
				);
				IPosition shape(1, 3);
				Vector<ImageBeamSet::AxisType> types(1);
				types[0] = ImageBeamSet::SPECTRAL;
				ImageBeamSet x(init, shape, types);
				GaussianBeam beam2(
					Quantity(10, "arcsec"), Quantity(5, "arcsec"),
					Quantity(70, "deg")
				);
				GaussianBeam beam3(
					Quantity(11, "arcsec"), Quantity(5, "arcsec"),
					Quantity(70, "deg")
				);
				Array<GaussianBeam> beams(IPosition(1, 5), beam2);
				beams(IPosition(1, 3)) = beam3;
				x.setBeams(beams);
				AlwaysAssert(x.getBeams().shape() == IPosition(1, 5), AipsError);
				AlwaysAssert(x.getMaxAreaBeam() == beam3, AipsError);
			}
		}
        {
            cout << "*** test setBeam()" << endl;
            GaussianBeam beam0(
            	Quantity(4, "arcsec"), Quantity(3, "arcsec"),
            	Quantity(20, "deg")
            );
            IPosition shape(2, 3, 4); 
            Vector<ImageBeamSet::AxisType> types(2);
            types[0] = ImageBeamSet::SPECTRAL;
            types[1] = ImageBeamSet::POLARIZATION;
            ImageBeamSet x(beam0, shape, types);
            GaussianBeam beam1(
            	Quantity(5, "arcsec"), Quantity(4, "arcsec"),
            	Quantity(20, "deg")
            );
            IPosition pos2(2, 1, 2); 
            x.setBeam(beam1, pos2);
            IPosition axisPath = IPosition::makeAxisPath(shape.size());
            ArrayPositionIterator iter(shape, axisPath, False);
            while (! iter.pastEnd()) {
                const IPosition pos = iter.pos();
                GaussianBeam beam = x.getBeam(pos);
                if (pos == pos2) {
                    AlwaysAssert(beam == beam1, AipsError);
                }
                else {
                    AlwaysAssert(beam == beam0, AipsError);
                }
                iter.next();
            }
            {
                cout << "*** test setBeams()" << endl;
                GaussianBeam init(
                    Quantity(4, "arcsec"), Quantity(2, "arcsec"),
                    Quantity(0, "deg")
                );
                IPosition shape(1, 3); 
                Vector<ImageBeamSet::AxisType> types(1);
                types[0] = ImageBeamSet::SPECTRAL;
                ImageBeamSet x(init, shape, types);
                Array<GaussianBeam> beams(IPosition(1, 5));
                x.setBeams(beams);
            }
        }
        {
        	cout << "Test get max, min, median for polarizations" << endl;
        	ImageBeamSet beamSet;
        	IPosition pos;
        	Bool thrown = False;
        	try {
        		beamSet.getMaxAreaBeamForPol(pos, 0);
        	}
        	catch (const AipsError x) {
        		cout << "Caught expected exception: " << x.getMesg() << endl;
        		thrown = True;
        	}
        	AlwaysAssert(thrown, AipsError);

        	GaussianBeam beam0(
        		Quantity(4, "arcsec"), Quantity(3, "arcsec"),
        		Quantity(20, "deg")
        	);
        	beamSet = ImageBeamSet(beam0);
        	thrown = False;
        	try {
        		beamSet.getMaxAreaBeamForPol(pos, 0);
        	}
        	catch (const AipsError x) {
        		cout << "Caught expected exception: " << x.getMesg() << endl;
        		thrown = True;
        	}
        	AlwaysAssert(thrown, AipsError);
        	Vector<ImageBeamSet::AxisType> axisTypes(2);
        	axisTypes[0] = ImageBeamSet::SPECTRAL;
        	axisTypes[1] = ImageBeamSet::POLARIZATION;
        	beamSet = ImageBeamSet(beam0, IPosition(2, 3, 4), axisTypes);
        	IPosition gotPos;
        	for (uInt i=0; i<4; i++) {
        		GaussianBeam gotBeam = beamSet.getMaxAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		gotBeam = beamSet.getMinAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		gotBeam = beamSet.getMedianAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        	}
        	GaussianBeam beam1(
        		Quantity(5, "arcsec"), Quantity(3, "arcsec"),
        		Quantity(20, "deg")
        	);
        	beamSet.setBeam(beam1, IPosition(2, 2, 1));
        	GaussianBeam beam2(
        		Quantity(3, "arcsec"), Quantity(2, "arcsec"),
        		Quantity(20, "deg")
        	);
        	beamSet.setBeam(beam2, IPosition(2, 1, 1));
        	for (uInt i=0; i<4; i++) {
        		GaussianBeam gotBeam = beamSet.getMaxAreaBeamForPol(gotPos, i);
        		if (i == 1) {
        			AlwaysAssert(gotBeam == beam1, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 2, 1), AipsError);
        		}
        		else {
        			AlwaysAssert(gotBeam == beam0, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		}
        		gotBeam = beamSet.getMinAreaBeamForPol(gotPos, i);
        		if (i == 1) {
        			AlwaysAssert(gotBeam == beam2, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 1, i), AipsError);
        		}
        		else {
        			AlwaysAssert(gotBeam == beam0, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		}
        		gotBeam = beamSet.getMedianAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		if (i != 1) {
        			AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		}
        	}

        	beamSet = ImageBeamSet(beam0, IPosition(2, 4, 4), axisTypes);
        	for (uInt i=0; i<4; i++) {
        		GaussianBeam gotBeam = beamSet.getMaxAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		gotBeam = beamSet.getMinAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		gotBeam = beamSet.getMedianAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        	}
        	beamSet.setBeam(beam1, IPosition(2, 2, 1));
        	beamSet.setBeam(beam2, IPosition(2, 1, 1));
        	GaussianBeam beam3(
        		Quantity(4.5, "arcsec"), Quantity(3, "arcsec"),
        		Quantity(20, "deg")
        	);
        	beamSet.setBeam(beam3, IPosition(2, 0, 1));
        	for (uInt i=0; i<4; i++) {
        		GaussianBeam gotBeam = beamSet.getMaxAreaBeamForPol(gotPos, i);
        		if (i == 1) {
        			AlwaysAssert(gotBeam == beam1, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 2, 1), AipsError);
        		}
        		else {
        			AlwaysAssert(gotBeam == beam0, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		}
        		gotBeam = beamSet.getMinAreaBeamForPol(gotPos, i);
        		if (i == 1) {
        			AlwaysAssert(gotBeam == beam2, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 1, i), AipsError);
        		}
        		else {
        			AlwaysAssert(gotBeam == beam0, AipsError);
        			AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		}
        		gotBeam = beamSet.getMedianAreaBeamForPol(gotPos, i);
        		AlwaysAssert(gotBeam == beam0, AipsError);
        		if (i == 1) {
        			AlwaysAssert(gotPos == IPosition(2, 3, i), AipsError);
        		}
        		else {
        			AlwaysAssert(gotPos == IPosition(2, 0, i), AipsError);
        		}
        	}
        }
	}
	catch (const AipsError& x) {
		cout << x.getMesg() << endl;
		cout << "FAIL" << endl;
		return 1;
	}
	cout << "OK" << endl;
	return 0;
}

