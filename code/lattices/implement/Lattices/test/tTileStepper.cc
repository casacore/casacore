//# tTileStepper.cc: Test program for class TileStepper
//# Copyright (C) 1997,1998
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

#include <trial/Lattices/TileStepper.h>
#include <aips/Inputs/Input.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


main (int argc, char *argv[])
{
    Input inp(1);
    inp.Version(" ");
    inp.Create("nx", "128", "Number of pixels along the x-axis", "int");
    inp.Create("ny", "128", "Number of pixels along the y-axis", "int");
    inp.Create("nz", "128", "Number of pixels along the z-axis", "int");
    inp.Create("tx", "32", "Tile size along the x-axis", "int");
    inp.Create("ty", "32", "Tile size along the y-axis", "int");
    inp.Create("tz", "32", "Tile size along the z-axis", "int");
    inp.Create("blcx", "0", "Blc along the x-axis", "int");
    inp.Create("blcy", "0", "Blc along the y-axis", "int");
    inp.Create("blcz", "0", "Blc along the z-axis", "int");
    inp.Create("trcx", "1000000", "Trc along the x-axis", "int");
    inp.Create("trcy", "1000000", "Trc along the y-axis", "int");
    inp.Create("trcz", "1000000", "Trc along the z-axis", "int");
    inp.Create("incx", "1", "Inc along the x-axis", "int");
    inp.Create("incy", "1", "Inc along the y-axis", "int");
    inp.Create("incz", "1", "Inc along the z-axis", "int");
    inp.ReadArguments(argc, argv);

    const uInt nx=inp.GetInt("nx");
    const uInt ny=inp.GetInt("ny");
    const uInt nz=inp.GetInt("nz");
    const uInt tx=inp.GetInt("tx");
    const uInt ty=inp.GetInt("ty");
    const uInt tz=inp.GetInt("tz");
    const uInt blcx=inp.GetInt("blcx");
    const uInt blcy=inp.GetInt("blcy");
    const uInt blcz=inp.GetInt("blcz");
    const uInt trcx=inp.GetInt("trcx");
    const uInt trcy=inp.GetInt("trcy");
    const uInt trcz=inp.GetInt("trcz");
    const uInt incx=inp.GetInt("incx");
    const uInt incy=inp.GetInt("incy");
    const uInt incz=inp.GetInt("incz");

    // Check/adapt the values.
    IPosition shape(3, nx, ny, nz);
    IPosition tileShape (3, tx, ty, tz);
    IPosition blc (3, blcx, blcy, blcz);
    IPosition trc (3, trcx, trcy, trcz);
    IPosition inc (3, incx, incy, incz);
    for (uInt i=0; i<3; i++) {
	AlwaysAssertExit (shape(i) > 0);
	AlwaysAssertExit (tileShape(i) > 0);
	AlwaysAssertExit (blc(i) >= 0);
	AlwaysAssertExit (inc(i) > 0);
	if (tileShape(i) > shape(i)) tileShape(i) = shape(i);
	if (blc(i) > shape(i)-1) blc(i) = shape(i)-1;
	if (trc(i) > shape(i)-1) trc(i) = shape(i)-1;
	if (trc(i) < blc(i)) trc(i) = blc(i);
    }
    cout << "shape = " << shape << endl;
    cout << "tileshape = " << tileShape << endl;
    cout << "blc = " << blc << endl;
    cout << "trc = " << trc << endl;
    cout << "inc = " << inc << endl;
    {
	TileStepper stepper(shape, tileShape);
	stepper.subSection (blc, trc, inc);
	while (! stepper.atEnd()) {
	    cout << stepper.position() << ' ' << stepper.endPosition() << ' '
		 << stepper.cursorShape() << endl;
	    stepper++;
	}
	cout << "nsteps = " << stepper.nsteps() << endl;
	while (! stepper.atStart()) {
	    cout << stepper.position() << ' ' << stepper.endPosition() << ' '
		 << stepper.cursorShape() << endl;
	    stepper--;
	}
	cout << "nsteps = " << stepper.nsteps() << endl;
    }
    {
	TileStepper stepper(shape, tileShape, IPosition(3,2,1,0));
	stepper.subSection (blc, trc, inc);
	while (! stepper.atEnd()) {
	    cout << stepper.position() << ' ' << stepper.endPosition() << ' '
		 << stepper.cursorShape() << endl;
	    stepper++;
	}
	cout << "nsteps = " << stepper.nsteps() << endl;
	while (! stepper.atStart()) {
	    cout << stepper.position() << ' ' << stepper.endPosition() << ' '
		 << stepper.cursorShape() << endl;
	    stepper--;
	}
	cout << "nsteps = " << stepper.nsteps() << endl;
    }
}
