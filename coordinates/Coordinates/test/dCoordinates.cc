//# Program.cc: This program ...
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/Projection.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
//# Enumerated lines are in the Coordinates.h module header. If you
//# change them, change Coordinates.h to match.

int main()
{

  try {
    // Direction Coordinate
    Matrix<Double> xform(2,2);                                    // 1
    xform = 0.0; xform.diagonal() = 1.0;                          // 2
    DirectionCoordinate radec(MDirection::J2000,                  // 3
			      Projection(Projection::SIN),        // 4
			      135*C::pi/180.0, 60*C::pi/180.0,    // 5
			      -1*C::pi/180.0, 1*C::pi/180,        // 6
			      xform,                              // 7
			      128.0, 128.0,                       // 8
                              999.0, 999.0);

    Vector<String> units(2); units = "deg";                        //  9
    radec.setWorldAxisUnits(units);                               // 10
    
    Vector<Double> world(2), pixel(2);                            // 11
    pixel = 138.0;                                                // 12
    
    Bool ok = radec.toWorld(world, pixel);                        // 13
    if (!ok) {                                                    // 14
	cout << "Error: " << radec.errorMessage() << endl;        // 15
	return 1;                                                 // 16
    }                                                             // 17
    cout << world << " <--- " << pixel << endl;           // 18
    ok = radec.toPixel(pixel, world);                             // 19
    if (!ok) {
	cout << "Error: " << radec.errorMessage() << endl;
	return 1;
    }
    cout << world << " ---> " << pixel << endl;

    // StokesCoordinate
    Vector<Int> iquv(4);                                         // 20
    iquv(0) = Stokes::I; iquv(1) = Stokes::Q;                    // 21
    iquv(2) = Stokes::U; iquv(3) = Stokes::V;                    // 22
    StokesCoordinate stokes(iquv);                               // 23

    Int plane;                                                   // 24
    ok = stokes.toPixel(plane, Stokes::Q);                       // 25
    if (!ok) {
	cout << "Error: " << stokes.errorMessage() << endl;
	return 1;
    }
    cout << "Stokes Q is plane " << plane << endl;
    
    ok = stokes.toPixel(plane, Stokes::XX);                      // 26
    if (!ok) {
	cout << "Expected error: " << stokes.errorMessage() << endl;
	cout << "Continuing..." << endl;
    }
    cout << "Stokes XX is plane " << plane << endl;
    
    // SpectralCoordinate
    SpectralCoordinate spectral(MFrequency::TOPO,               // 27
				1400 * 1.0E+6,                  // 28
				20 * 1.0E+3,                    // 29
				0,                              // 30
				1420.40575 * 1.0E+6);           // 31
    units.resize(1); pixel.resize(1); world.resize(1);
    units = "MHz";
    spectral.setWorldAxisUnits(units);
    
    pixel = 50;
    ok = spectral.toWorld(world, pixel);
    if (!ok) {
	cout << "Error: " << spectral.errorMessage() << endl;
	return 1;
    }
    cout << world << " <--- " << pixel << endl;

    ok = spectral.toPixel(pixel, world);
    if (!ok) {
	cout << "Error: " << spectral.errorMessage() << endl;
	return 1;
    }
    cout << world << " ---> " << pixel << endl;

    // CoordinateSystem
    CoordinateSystem coordsys;
    coordsys.addCoordinate(radec);
    coordsys.addCoordinate(stokes);
    coordsys.addCoordinate(spectral);

    world.resize(4); pixel.resize(4);
    pixel(0) = 138; pixel(1) = 138; pixel(2) = 2; pixel(3) = 50;

    ok = coordsys.toWorld(world, pixel);
    if (!ok) {
	cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    cout << world << " <--- " << pixel << endl;

    ok = coordsys.toPixel(pixel, world);
    if (!ok) {
	cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    cout << world << " ---> " << pixel << endl;

    // CoordinateSystem::transpose
    Vector<Int> tran(4);
    tran(0) = 0; tran(1) = 1; tran(2) = 3; tran(3) = 2;
    coordsys.transpose(tran,tran);
    pixel(2) = 50; pixel(3) = 2;

    ok = coordsys.toWorld(world, pixel);
    if (!ok) {
	cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    cout << world << " <--- " << pixel << endl;

    ok = coordsys.toPixel(pixel, world);
    if (!ok) {
	cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    cout << world << " ---> " << pixel << endl;

    // CoordinateSystem::remove*Axis
    ok = coordsys.removePixelAxis(0, 138.0);
    if (!ok) {
	cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    pixel.resize(3); pixel(0) = 138; pixel(1) = 50; pixel(2) = 2;
    ok = coordsys.toWorld(world, pixel);
    if (!ok) {
	cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    cout << world << " <--- " << pixel << endl;

    ok = coordsys.toPixel(pixel, world);
    if (!ok) {
	cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    cout << world << " ---> " << pixel << endl;

    TableRecord rec;
    ok = coordsys.save(rec, "CS");
    if (!ok) {
        cout << "Error: " << coordsys.errorMessage() << endl;
	return 1;
    }
    CoordinateSystem* pCoordSys = CoordinateSystem::restore(rec,"CS");
    if (pCoordSys == 0) {
       cout << "Failed to restore from record" << endl;
       return 1;
    } else {
       delete pCoordSys;
    }

  } catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
  }

  cout << "ok" << endl;
  return 0;
}
