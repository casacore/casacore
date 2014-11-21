//# tGaussianConvert.cc: Test program for GaussianConvert
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
//#
 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/coordinates/Coordinates/GaussianConvert.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Utilities/Assert.h>



#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void doit (Double majorPixels, Double minorPixels, const Quantum<Double>& pa1,
           const CoordinateSystem& cSys,
           const Vector<uInt>& worldAxes,
           Double exMinorWorld, Double exMajorWorld, Double exPAWorld);


void doit2 (Vector<Double>& pixel, const CoordinateSystem& cSys,
           const Vector<uInt>& worldAxes);

int main()
{
   try {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      Vector<uInt> worldAxes(2);
      worldAxes(0) = 0;
      worldAxes(1) = 1;
//
      Vector<String> units(2);
      units(0) = "arcsec";
      units(1) = "arcsec";
      cSys.setWorldAxisUnits(units);
//
// Axis conversions
//
      {
         Vector<Double> deltas(2);
         deltas(0) = -1.0;
         deltas(1) = 1.0;
         cSys.setIncrement(deltas);
//      cout << "inc = " << cSys.increment() << endl;
//
         Double majorPixels = 10.0;
         Double minorPixels = 5.0;
         Quantum<Double> pa1(30.0, Unit("deg"));
         doit(majorPixels, minorPixels, pa1, cSys, worldAxes,
              5.0, 10.0, 30.0);
         
      }
      {
         Vector<Double> deltas(2);
         deltas(0) = 1.0;
         deltas(1) = 1.0;
         cSys.setIncrement(deltas);
//      cout << "inc = " << cSys.increment() << endl;
//
         Double majorPixels = 10.0;
         Double minorPixels = 5.0;
         Quantum<Double> pa1(30, Unit("deg"));
         doit(majorPixels, minorPixels, pa1, cSys, worldAxes,
              5.0, 10.0, 150.0);
      }
      {
         Vector<Double> deltas(2);
         deltas(0) = -1.0;
         deltas(1) = 2.0;
         cSys.setIncrement(deltas);
//      cout << "inc = " << cSys.increment() << endl;
//
         Double majorPixels = 10.0;
         Double minorPixels = 10.0;
         Quantum<Double> pa1(0.0, Unit("deg"));
         doit(majorPixels, minorPixels, pa1, cSys, worldAxes,
              10.0, 20.0, 0.0);
      }
      {
         Vector<Double> deltas(2);
         deltas(0) = -1.0;
         deltas(1) = 2.0;
         cSys.setIncrement(deltas);
//      cout << "inc = " << cSys.increment() << endl;
//
         Double majorPixels = 10.0;
         Double minorPixels = 5.0;
         Quantum<Double> pa1(90.0, Unit("deg"));
         doit(majorPixels, minorPixels, pa1, cSys, worldAxes,
              10.0, 10.0, 45.0);
      }
//
// Position conversions
//
      {
         Vector<Double> pixel(cSys.referencePixel().copy());
         doit2 (pixel, cSys, worldAxes);
      }
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return 1;
   }

   cout << "ok" << endl; 
   return 0;
}


void doit (Double majorPixels, Double minorPixels, const Quantum<Double>& pa1,
           const CoordinateSystem& cSys,
           const Vector<uInt>& worldAxes,
           Double exMinorWorld, Double exMajorWorld, Double exPAWorld)
{
//
// Convert from pixels to world
//
   GaussianConvert gc(cSys, worldAxes);
   Quantum<Double> majorWorld, minorWorld;
   Quantum<Double> pa2;
   AlwaysAssert(gc.toWorld(majorWorld, minorWorld, pa2,
                           majorPixels,  minorPixels, pa1), AipsError);
//
// Reflect back to pixels
//
   Double majorPixels2, minorPixels2;
   Quantum<Double> pa3;
   AlwaysAssert(gc.toPixel(majorPixels2, minorPixels2, pa3,
                           majorWorld, minorWorld, pa2), AipsError);
//
   cout << "Major axis : pixel, world, pixel = " << majorPixels << " " 
        << majorWorld << " " << majorPixels2 << endl;
   cout << "Minor axis : pixel, world, pixel = " << minorPixels << " " 
        << minorWorld << " " << minorPixels2 << endl;
   cout << "Position Angle : pixel, world, pixel = " 
        << pa1 << " " << pa2 << " " << pa3 << endl << endl;
//
   AlwaysAssert(near(majorWorld.getValue(),exMajorWorld,1e-6), AipsError);
   AlwaysAssert(near(minorWorld.getValue(),exMinorWorld,1e-6), AipsError);
   AlwaysAssert(near(pa2.getValue(),exPAWorld,1e-6), AipsError);
//
   AlwaysAssert(near(majorPixels,majorPixels2,1e-6), AipsError);
   AlwaysAssert(near(minorPixels,minorPixels2,1e-6), AipsError);
   AlwaysAssert(near(pa1.getValue(),pa3.getValue(),1e-6), AipsError);
}


void doit2 (Vector<Double>& pixel, const CoordinateSystem& cSys,
           const Vector<uInt>& worldAxes)
{
//
// Convert from pixels to world
//
   GaussianConvert gc(cSys, worldAxes);
   Vector<Quantum<Double> > world;
   AlwaysAssert(gc.toWorld(world, pixel), AipsError);
//
// Reflect back to pixels
//
   Vector<Double> pixel2;
   AlwaysAssert(gc.toPixel(pixel2, world), AipsError);
   AlwaysAssert(allNear(pixel2,pixel,1e-6), AipsError);
//
// Change units and see what happens
//
   world(0).convert(Unit("arcsec"));
   world(1).convert(Unit("arcmin"));
   AlwaysAssert(gc.toPixel(pixel2, world), AipsError);
   AlwaysAssert(allNear(pixel2,pixel,1e-6), AipsError);
}
