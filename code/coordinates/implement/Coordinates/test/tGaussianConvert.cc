//# tGaussianConvert.cc: Test program for GaussianConvert
//# Copyright (C) 1998,1999
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
 
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/Math.h>
#include <trial/Coordinates/GaussianConvert.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Exceptions/Error.h>
#include <aips/Quanta/Quantum.h>


#include <iostream.h>

void doit (Double majorPixels, Double minorPixels, const Quantum<Double>& pa1,
           const CoordinateSystem& cSys,
           const Vector<uInt>& worldAxes,
           Double exMinorWorld, Double exMajorWorld, Double exPAWorld);
      

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
      cSys.setWorldAxisUnits(units, False);
//      cout << "units = " << cSys.worldAxisUnits().ac() << endl;
//
      {
         Vector<Double> deltas(2);
         deltas(0) = -1.0;
         deltas(1) = 1.0;
         cSys.setIncrement(deltas);
//      cout << "inc = " << cSys.increment().ac() << endl;
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
//      cout << "inc = " << cSys.increment().ac() << endl;
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
//      cout << "inc = " << cSys.increment().ac() << endl;
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
//      cout << "inc = " << cSys.increment().ac() << endl;
//
         Double majorPixels = 10.0;
         Double minorPixels = 5.0;
         Quantum<Double> pa1(90.0, Unit("deg"));
         doit(majorPixels, minorPixels, pa1, cSys, worldAxes,
              10.0, 10.0, 45.0);
      }


   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      exit(1);
   }end_try;

   cout << "ok" << endl; 
   exit(0);
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


