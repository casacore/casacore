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
 
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/BasicMath/Math.h>
#include <coordinates/Coordinates/GaussianConvert.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <casa/Exceptions/Error.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>



#include <casa/iostream.h>

#include <casa/namespace.h>
void doit (Double majorPixels, Double minorPixels, const Quantum<Double>& pa1,
           const CoordinateSystem& cSys,
           const Vector<uInt>& worldAxes,
           Double exMinorWorld, Double exMajorWorld, Double exPAWorld);


void doit2 (Vector<Double>& pixel, const CoordinateSystem& cSys,
           const Vector<uInt>& worldAxes);

void doit3();

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
//
// Deconvolution
//
      {
         doit3();
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

void doit3()
{
   Quantum<Double> paModel;
   Quantum<Double> majModel;
   Quantum<Double> minModel;
   Quantum<Double> paSource(0.0, Unit("deg"));
   Quantum<Double> majSource(0.0, Unit("arcsec"));
   Quantum<Double> minSource(0.0, Unit("arcsec"));;
   Quantum<Double> paBeam(0.0, Unit("deg"));;
   Quantum<Double> majBeam(0.0, Unit("arcsec"));;
   Quantum<Double> minBeam(0.0, Unit("arcsec"));;
//
// Easy test 1 - P.A. = 0
//
   {
      Unit rad("rad");
      majSource.setValue(20.0);
      minSource.setValue(10.0);
      majBeam.setValue(15.0);
      minBeam.setValue(5.0);
//
      Double maj = sqrt(square(majSource.getValue(rad)) -
                        square(majBeam.getValue(rad)));
      Quantum<Double> majQ(maj, rad);
      majQ.convert(majSource.getFullUnit());
//
      Double min = sqrt(square(minSource.getValue(rad)) -
                        square(minBeam.getValue(rad)));
      Quantum<Double> minQ(min, rad);
      minQ.convert(minSource.getFullUnit());
//
      Quantum<Double> paQ(0.0,paSource.getFullUnit());
//
      Bool isPoint = GaussianConvert::deconvolve(majModel, minModel, paModel,
                                                 majSource, minSource, paSource,
                                                 majBeam, minBeam, paBeam);
      cout << "Source   = " << majSource << ", " << minSource << ", " << paSource << endl;
      cout << "Beam     = " << majBeam << ", " << minBeam << ", " << paBeam << endl;
      cout << "Model    = " << majModel << ", " << minModel << ", " << paModel << endl;
      cout << "Expected = " << majQ << ", " << minQ << ", " << paQ << endl;
      cout << "isPoint  = " << isPoint << endl << endl;
      AlwaysAssert(!isPoint, AipsError);
      AlwaysAssert((near(majQ.getValue(),majModel.getValue(),1e-6) &&
                    near(minQ.getValue(),minModel.getValue(),1e-6) &&
                    near(paQ.getValue(),paModel.getValue(),1e-6)), AipsError);
   }
//
// Easy test 2 - P.A. aligned
//
   {
      Unit rad("rad");
      majSource.setValue(20.0);
      minSource.setValue(10.0);
      paSource.setValue(45.0);
      majBeam.setValue(15.0);
      minBeam.setValue(5.0);
      paBeam.setValue(45.0);
//
      Double maj = sqrt(square(majSource.getValue(rad)) -
                        square(majBeam.getValue(rad)));
      Quantum<Double> majQ(maj, rad);
      majQ.convert(majSource.getFullUnit());
//
      Double min = sqrt(square(minSource.getValue(rad)) -
                        square(minBeam.getValue(rad)));
      Quantum<Double> minQ(min, rad);
      minQ.convert(minSource.getFullUnit());
//
      Quantum<Double> paQ(45.0,paSource.getFullUnit());
//
      Bool isPoint = GaussianConvert::deconvolve(majModel, minModel, paModel,
                                                 majSource, minSource, paSource,
                                                 majBeam, minBeam, paBeam);
      cout << "Source   = " << majSource << ", " << minSource << ", " << paSource << endl;
      cout << "Beam     = " << majBeam << ", " << minBeam << ", " << paBeam << endl;
      cout << "Model    = " << majModel << ", " << minModel << ", " << paModel << endl;
      cout << "Expected = " << majQ << ", " << minQ << ", " << paQ << endl;
      cout << "isPoint  = " << isPoint << endl << endl;
      AlwaysAssert(!isPoint, AipsError);
      AlwaysAssert((near(majQ.getValue(),majModel.getValue(),1e-6) &&
                    near(minQ.getValue(),minModel.getValue(),1e-6) &&
                    near(paQ.getValue(),paModel.getValue(),1e-6)), AipsError);
   }
//
// Easy test 3 - beam and source the same
//
   {
      Unit rad("rad");
      majSource.setValue(20.0);
      minSource.setValue(10.0);
      paSource.setValue(45.0);
      majBeam.setValue(20.00001);
      minBeam.setValue(10.00001);
      paBeam.setValue(45.0);
//
      Bool isPoint = GaussianConvert::deconvolve(majModel, minModel, paModel,
                                                 majSource, minSource, paSource,
                                                 majBeam, minBeam, paBeam);
      cout << "Source   = " << majSource << ", " << minSource << ", " << paSource << endl;
      cout << "Beam     = " << majBeam << ", " << minBeam << ", " << paBeam << endl;
      cout << "Model    = " << majModel << ", " << minModel << ", " << paModel << endl;
      cout << "Expected = " << majBeam << ", " << minBeam << ", " << paBeam << endl;
      cout << "isPoint  = " << isPoint << endl << endl;
      AlwaysAssert(isPoint, AipsError);
      AlwaysAssert((nearAbs(majModel.getValue(),majBeam.getValue(),1e-6) &&
                    nearAbs(minModel.getValue(),minBeam.getValue(),1e-6)),AipsError);
   }
}
