//# tCoordinateSystem.cc: Test program for CoordinateSystem
//# Copyright (C) 1998,1999,2000,2001,2002,2003,2004
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

 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/QualityCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>


DirectionCoordinate makeDirectionCoordinate(bool unitsAreDegrees=true,
                                            MDirection::Types type=MDirection::J2000);
SpectralCoordinate makeSpectralCoordinate ();
StokesCoordinate makeStokesCoordinate(bool silly=true);
QualityCoordinate makeQualityCoordinate();
LinearCoordinate makeLinearCoordinate(uint32_t nAxes=2);
TabularCoordinate makeTabularCoordinate();
CoordinateSystem makeCoordinateSystem(uint32_t& nCoords,
                                      Vector<int32_t>& types,
                                      Vector<String>& sTypes,
                                      uint32_t& iDC,
                                      uint32_t& iSpC,
                                      uint32_t& iTC,
                                      uint32_t& iStC,
                                      uint32_t& iQuC,
                                      uint32_t& iLC,
                                      DirectionCoordinate& dC,
                                      SpectralCoordinate& spC,
                                      TabularCoordinate& tC,
                                      StokesCoordinate& stC,
                                      QualityCoordinate& quC,
                                      LinearCoordinate& lC);


void doit (CoordinateSystem& lc, uint32_t nCoords,
           const Vector<int32_t>& types,
           const Vector<String>& sTypes,
           const uint32_t iDC,
           const uint32_t iSpC,
           const uint32_t iTC,
           const uint32_t iStC,
           const uint32_t iQuC,
           const uint32_t iLC,
           const DirectionCoordinate&,
           const SpectralCoordinate&,
           const TabularCoordinate&,
           const StokesCoordinate&,
           const QualityCoordinate&,
           const LinearCoordinate&);
void doit2 (CoordinateSystem& cSys);
void doit3 (CoordinateSystem& cSys);
void doit4 ();
void doit5 ();
void doit6 ();
void doit7 ();
void verifyCAS3264 ();
void spectralAxisNumber();
void polarizationAxisNumber();



int main()
{
   try {

      uint32_t nCoords;
      Vector<int32_t> types;
      Vector<String> sTypes;
      DirectionCoordinate dC;
      SpectralCoordinate spC;
      TabularCoordinate tC;
      StokesCoordinate stC = makeStokesCoordinate();   // No default constrcutor
      QualityCoordinate quC = makeQualityCoordinate(); // No default constrcutor
      LinearCoordinate lC;
      uint32_t iDC;
      uint32_t iSpC;
      uint32_t iTC;
      uint32_t iStC;
      uint32_t iQuC;
      uint32_t iLC;
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                      dC, spC, tC, stC, quC, lC);
      }
      {
         CoordinateSystem cSys1 = makeCoordinateSystem(nCoords, types, sTypes,
                                                       iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                       dC, spC, tC, stC, quC, lC);
         CoordinateSystem cSys2 = makeCoordinateSystem(nCoords, types, sTypes,
                                                       iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                       dC, spC, tC, stC, quC, lC);

// near tests

         if (!cSys1.near(cSys1)) {
            String msg = String("Failed near test 0 because ") +
                         cSys1.errorMessage();
            throw(AipsError(msg));
         }
         if (!cSys1.near(cSys2)) {
            String msg = String("Failed near test 1 because ") +
                         cSys1.errorMessage();
            throw(AipsError(msg));
         }
         Vector<int32_t> excludeAxes(2);
         excludeAxes(0) = 0;
         excludeAxes(1) = 2;
         if (!cSys1.near(cSys2, excludeAxes)) {
            String msg = String("Failed near test 2 because ") +
                         cSys1.errorMessage();
            throw(AipsError(msg));
         }

// nearPixel tests

         if (!cSys1.nearPixel(cSys1)) {
            String msg = String("Failed nearPixel test 0 because ") +
                         cSys1.errorMessage();
            throw(AipsError(msg));
         }
         if (!cSys1.nearPixel(cSys2)) {
            String msg = String("Failed nearPixel test 1 because ") +
                         cSys1.errorMessage();
            throw(AipsError(msg));
         }
//
         Vector<double> refVal1(cSys1.referenceValue().copy());         
         Vector<double> refPix1(cSys1.referencePixel().copy());         
         Vector<double> inc1(cSys1.increment().copy());         
         Vector<double> refVal2(cSys2.referenceValue().copy());         
         Vector<double> refPix2(cSys2.referencePixel().copy());         
         Vector<double> inc2(cSys2.increment().copy());         
//
         refVal1(0) += inc1(0);
         cSys1.setReferenceValue(refVal1);
         if (cSys1.nearPixel(cSys2)) {
            String msg = String("Unexpectedly passed nearPixel test 2");
            throw(AipsError(msg));
         }
         refVal1(0) -= inc1(0);
         cSys1.setReferenceValue(refVal1);
         refVal2(0) += inc2(0);
         cSys2.setReferenceValue(refVal2);
         if (cSys1.nearPixel(cSys2)) {
            String msg = String("Unexpectedly passed nearPixel test 3");
            throw(AipsError(msg));
         }
         refVal2(0) -= inc2(0);
         cSys2.setReferenceValue(refVal2);
//
//
         refPix1(0) += 1;
         cSys1.setReferencePixel(refPix1);
         if (cSys1.nearPixel(cSys2)) {
            String msg = String("Unexpectedly passed nearPixel test 4");
            throw(AipsError(msg));
         }
         refPix1(0) -= 1;
         cSys1.setReferencePixel(refPix1);
         refPix2(0) += 1;
         cSys2.setReferencePixel(refPix2);
         if (cSys1.nearPixel(cSys2)) {
            String msg = String("Unexpectedly passed nearPixel test 5");
            throw(AipsError(msg));
         }
         refPix2(0) -= 1;
         cSys2.setReferencePixel(refPix2);
////
         inc1(0) *= 2.0;
         cSys1.setIncrement(inc1);
         if (cSys1.nearPixel(cSys2)) {
            String msg = String("Unexpectedly passed nearPixel test 6");
            throw(AipsError(msg)); 
         }
         inc1(0) /= 2.0;
         cSys1.setIncrement(inc1);
         inc2(0) *= 2.0;
         cSys2.setIncrement(inc2);
         if (cSys1.nearPixel(cSys2)) {
            String msg = String("Unexpectedly passed nearPixel test 7");
            throw(AipsError(msg));
         }

         inc2(0) /= 2.0;
         cSys2.setIncrement(inc2);
////
         int32_t pAxis = cSys1.nPixelAxes() - 2;
         cSys1.removePixelAxis(pAxis, 0.0);
         pAxis = 1;
         cSys1.removePixelAxis(pAxis, 0.0);
         if (cSys1.nearPixel(cSys2)) {
            String msg = String("Unexpectedly passed nearPixel test 8");
            throw(AipsError(msg));
         }

//
         pAxis = cSys2.nPixelAxes() - 2;
         cSys2.removePixelAxis(pAxis, 0.0);
         pAxis = 1;
         cSys2.removePixelAxis(pAxis, 0.0);
         if (!cSys1.nearPixel(cSys2)) {
            String msg = String("Failed nearPixel test 9 because ") +
                         cSys1.errorMessage();
            throw(AipsError(msg));
         }
      }
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                      dC, spC, tC, stC, quC, lC);

         String oneStokes = cSys.stokesAtPixel((uint32_t)1);
         if (oneStokes!="RL") {
               throw(AipsError("Stokes value at pixel 1 is wrong!"));
         }
         String oneQual = cSys.qualityAtPixel((uint32_t)1);
         if (oneQual!="ERROR") {
               throw(AipsError("Quality value at pixel 1 is wrong!"));
         }
      }
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                      dC, spC, tC, stC, quC, lC);
         doit(cSys, nCoords, types, sTypes, 
              iDC, iSpC, iTC, iStC, iQuC, iLC,
              dC, spC, tC, stC, quC, lC);
      }
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                      dC, spC, tC, stC, quC, lC);
         doit2(cSys);
      }
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                      dC, spC, tC, stC, quC, lC);
         doit3(cSys);
      }
      {
         doit4();
      }
      {
         doit5();
      }
      {
//         doit6();
      }
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iQuC, iLC,
                                                      dC, spC, tC, stC, quC, lC);
//
         LogOrigin lO(String("tCoordinateSystem"), String("main()"), WHERE);
         LogIO os(lO);
         IPosition s1, s2;
         cSys.list(os, MDoppler::RADIO, s1, s2);
      }
      {
    	  verifyCAS3264();
      }
      {
    	  cout << "*** Test getWorldAxisOrder" << endl;
    	  CoordinateSystem csys = CoordinateUtil::defaultCoords(4);
    	  Vector<String> myNames(1, "spectral");
    	  bool ok = true;
    	  try {
    		  Vector<int32_t> axes = csys.getWorldAxesOrder(myNames, false, false);
    		  ok = false;
    	  }
    	  catch (const AipsError& e) {}
    	  AlwaysAssert(ok, AipsError);
		  Vector<int32_t> axes = csys.getWorldAxesOrder(myNames, false, true);
		  AlwaysAssert(axes[0] == 3, AipsError);

      }
      {
    	  spectralAxisNumber();
    	  polarizationAxisNumber();

      }
      {
    	  cout << "*** test isDirectionAbscissaLongitude()" << endl;
    	  CoordinateSystem csys = CoordinateUtil::defaultCoords(4);
    	  AlwaysAssert(csys.isDirectionAbscissaLongitude(), AipsError);
    	  Vector<int32_t> worldOrder(4);
    	  indgen(worldOrder);
    	  Vector<int32_t> pixelOrder = worldOrder.copy();
    	  pixelOrder[0] = 1;
    	  pixelOrder[1] = 0;
    	  csys.transpose(worldOrder, pixelOrder);
    	  AlwaysAssert(! csys.isDirectionAbscissaLongitude(), AipsError);
    	  csys.removePixelAxis(0, 0.0);
    	  try {
        	  csys.removePixelAxis(0, 0.0);
        	  // expected exception not thrown if we get here
        	  AlwaysAssert(false, AipsError);
    	  }
    	  catch (const std::exception& x) {}
    	  csys = CoordinateSystem();
    	  CoordinateUtil::addFreqAxis(csys);
    	  try {
    		  csys.removePixelAxis(0, 0.0);
    		  // expected exception not thrown if we get here
    		  AlwaysAssert(false, AipsError);
    	  }
    	  catch (const std::exception& x) {}
    	  csys = CoordinateUtil::defaultCoords(4);
    	  indgen(pixelOrder);
    	  pixelOrder[0] = 1;
    	  pixelOrder[1] = 2;
    	  pixelOrder[2] = 0;
    	  csys.transpose(worldOrder, pixelOrder);
    	  AlwaysAssert(! csys.isDirectionAbscissaLongitude(), AipsError);
    	  csys = CoordinateUtil::defaultCoords(4);
    	  indgen(pixelOrder);
    	  pixelOrder[1] = 2;
    	  pixelOrder[2] = 1;
    	  csys.transpose(worldOrder, pixelOrder);
    	  AlwaysAssert(csys.isDirectionAbscissaLongitude(), AipsError);

      }
      {
    	  cout << "*** test directionAxesNumbers" << endl;
    	  CoordinateSystem csys = CoordinateUtil::defaultCoords(4);
    	  Vector<int32_t> dan = csys.directionAxesNumbers();
    	  AlwaysAssert(dan[0] == 0 && dan[1] == 1, AipsError);
    	  Vector<int32_t> worldOrder(4);
    	  indgen(worldOrder);
    	  Vector<int32_t> pixelOrder = worldOrder.copy();
    	  pixelOrder[0] = 1;
    	  pixelOrder[1] = 3;
    	  pixelOrder[3] = 0;
    	  csys.transpose(worldOrder, pixelOrder);
    	  dan = csys.directionAxesNumbers();
    	  AlwaysAssert(dan[0] == 3 && dan[1] == 0, AipsError);

      }
      {
    	  cout << "*** test setRestFrequency()" << endl;
    	  bool   ok=false;
    	  String errorMsg;

    	  CoordinateSystem csys = CoordinateUtil::defaultCoords3D();

    	  int32_t pixelAxis, worldAxis, coordinate;
    	  CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coordinate, csys);
    	  // make sure negative rest frequency is refused
    	  Quantity freq(-100, "GHz");
    	  ok = csys.setRestFrequency (errorMsg, freq);
    	  AlwaysAssertExit (!ok);
    	  cerr << "Frequency correctly NOT set with message = " <<  errorMsg << endl;

    	  double x;
    	  setNaN(x);
    	  freq.setValue(x);
    	  ok = csys.setRestFrequency (errorMsg, freq);
    	  AlwaysAssertExit (!ok);
    	  cerr << "Frequency correctly NOT set with message = " <<  errorMsg << endl;

    	  setInf(x);
    	  freq.setValue(x);
    	  ok = csys.setRestFrequency (errorMsg, freq);
    	  AlwaysAssertExit (!ok);
    	  cerr << "Frequency correctly NOT set with message = " <<  errorMsg << endl;

    	  Quantity wavelength(0, "nm");

    	  ok = csys.setRestFrequency (errorMsg, wavelength);
    	  AlwaysAssertExit(!ok);
    	  cerr << "Frequency correctly NOT set with message = " <<  errorMsg << endl;

    	  Quantity vel(100, "km/s");
    	  ok = csys.setRestFrequency (errorMsg, vel);
    	  AlwaysAssertExit(!ok);
    	  cerr << "Frequency correctly NOT set with message = " <<  errorMsg << endl;

    	  freq = Quantity(100, "GHz");
    	  ok = csys.setRestFrequency (errorMsg, freq);
    	  AlwaysAssertExit (ok);
    	  cerr << "Frequency set to: " << freq << endl;
    	  const SpectralCoordinate &sCoo1 = csys.spectralCoordinate(coordinate);

    	  AlwaysAssertExit(near(1.0e+11, sCoo1.restFrequency(), 1.0e-8));

    	  freq.setValue(90);
    	  ok = csys.setRestFrequency (errorMsg, freq);
    	  AlwaysAssertExit (ok);
    	  cerr << "Frequency set to: " << freq << endl;
    	  const SpectralCoordinate &sCoo2 = csys.spectralCoordinate(coordinate);
    	  AlwaysAssertExit(near(0.9e+11, sCoo2.restFrequency(), 1.0e-8));
    	  cerr << "The input was verified" << endl;

    	  wavelength = Quantity(1, "mm");
    	  ok = csys.setRestFrequency (errorMsg, wavelength);
    	  AlwaysAssertExit (ok);
    	  cerr << "Frequency set to: " << wavelength << endl;
    	  const SpectralCoordinate &sCoo3 = csys.spectralCoordinate(coordinate);
    	  AlwaysAssertExit(near(QC::c( ).getValue()/1.0e-03, sCoo3.restFrequency(), 1.0e-8));
    	  cerr << "The input was verified" << endl;

      }
      {
          cout << "*** test toWorld using both native and conversion layer frames" << endl;
          CoordinateSystem csys = CoordinateUtil::defaultCoords(4);
          Matrix<double> xform(2, 2, 0);
          xform.diagonal() = 1;
          DirectionCoordinate dc(
              MDirection::J2000, Projection::SIN, Quantity(0, "deg"), Quantity(0, "deg"),
              Quantity(-1, "arcsec"), Quantity(1, "arcsec"), xform, 0, 0
          );
          dc.setReferenceConversion(MDirection::GALACTIC);
          SpectralCoordinate sc(
              MFrequency::LSRK, Quantity(1500, "MHz"), Quantity(1, "kHz"),
              0, Quantity(1500, "MHz")
          );
          MEpoch epoch(Quantity(60000, "d"), MEpoch::UTC);
          MPosition position(
              Quantity(10, "m"), Quantity(135, "deg"), Quantity(40, "deg"), MPosition::ITRF
          );
          MDirection direction(Quantity(20, "deg"), Quantity(50, "deg"), MDirection::J2000);
          sc.setReferenceConversion(MFrequency::CMB, epoch, position, direction);
          csys.replaceCoordinate(dc, 0);
          csys.replaceCoordinate(sc, 2);
          Vector<double> pixel(4, 0);
          pixel[0] = 60;
          pixel[1] = 60;
          pixel[3] = 10;
          Vector<double> world(4);
          csys.toWorld(world, pixel);
          AlwaysAssert(near(world[0], 1.6811, 1e-5), AipsError);
          AlwaysAssert(near(world[1], -1.05011, 1e-5), AipsError);
          AlwaysAssert(near(world[3], 1.50121e+09, 1e-5), AipsError);
          csys.toWorld(world, pixel, true);
          AlwaysAssert(near(world[0], 1.6811, 1e-5), AipsError);
          AlwaysAssert(near(world[1], -1.05011, 1e-5), AipsError);
          AlwaysAssert(near(world[3], 1.50121e+09, 1e-5), AipsError);
          csys.toWorld(world, pixel, false);
          AlwaysAssert(near(world[0], 6.28289, 1e-5), AipsError);
          AlwaysAssert(near(world[1], 2.90888e-4, 1e-5), AipsError);
          AlwaysAssert(world[3] == 1.50001e+09, AipsError);
      }

   }
   catch (const std::exception& x) {
      cerr << "Error " << x.what() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}



void doit (CoordinateSystem& cSys, uint32_t nCoords, const Vector<int32_t>& types,
           const Vector<String>& sTypes,
           const uint32_t iDC,
           const uint32_t iSpC,
           const uint32_t iTC,
           const uint32_t iStC,
           const uint32_t iQuC,
           const uint32_t iLC,
           const DirectionCoordinate& dC,
           const SpectralCoordinate& spC,
           const TabularCoordinate& tC,
           const StokesCoordinate& stC,
           const QualityCoordinate& quC,
           const LinearCoordinate& lC)
{

// Test copy constructor
   {
       CoordinateSystem cSys2(cSys);
       if (!cSys.near(cSys2)) {
            String msg = String("Failed copy constructor test because ") +
                         cSys.errorMessage();
            throw(AipsError(msg));
       }
   } 

// Test assignment

   {
       CoordinateSystem cSys2;
       cSys2 = cSys;
       if (!cSys.near(cSys2)) {
          String msg = String("Failed assignment test because ") +
                        cSys.errorMessage();   
          throw(AipsError(msg));
       }
   } 

// Test member functions
  
   if (cSys.nCoordinates() != nCoords) {
      throw(AipsError("Failed nCoordinates test"));
   }
   if (cSys.showType() != "System") {
      throw(AipsError("Failed showType test"));
   }
   if (cSys.type() != Coordinate::COORDSYS) {
      throw(AipsError("Failed type test 1"));
   }
   for (uint32_t i=0; i<nCoords; i++) {
      Coordinate::Type type = (Coordinate::Type)types(i);
      if (cSys.type(i) != type) {
         throw(AipsError("Failed type test 2"));
      }
   }
   for (uint32_t i=0; i<nCoords; i++) {
      if (cSys.showType(i) != sTypes(i)) {
         throw(AipsError("Failed type test 3"));
      }
   }
//
   String msg;
   int32_t iC;
   iC = cSys.findCoordinate(Coordinate::DIRECTION);
   if (iC != int32_t(iDC)) {
      throw(AipsError("Failed findCoordinate test 1"));
   }
   if (!dC.near(cSys.directionCoordinate(uint32_t(iC)))) {
      msg = String("Failed directionCoordinate test because ") +
            dC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::SPECTRAL);
   if (iC != int32_t(iSpC)) {
      throw(AipsError("Failed findCoordinate test 2"));
   }
   if (!spC.near(cSys.spectralCoordinate(uint32_t(iC)))) {
      msg = String("Failed spectralCoordinate test because ") +   
            spC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::TABULAR);
   if (iC != int32_t(iTC)) {
      throw(AipsError("Failed findCoordinate test 3"));
   }
   if (!tC.near(cSys.tabularCoordinate(uint32_t(iC)))) {
      msg = String("Failed tabularCoordinate test because ") +   
            tC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::STOKES);
   if (iC != int32_t(iStC)) {
      throw(AipsError("Failed findCoordinate test 4"));
   }
   if (!stC.near(cSys.stokesCoordinate(uint32_t(iC)))) {
      msg = String("Failed stokesCoordinate test because ") +   
                   stC.errorMessage();
      throw(AipsError(msg));
   }
   const int32_t stokesPixelAxis = cSys.pixelAxes(iC)(0);
   const int32_t stokesWorldAxis = cSys.worldAxes(iC)(0);
   const int32_t whichStokesCoordinate = iC;
   //
   iC = cSys.findCoordinate(Coordinate::QUALITY);
   if (iC != int32_t(iQuC)) {
	   throw(AipsError("Failed findCoordinate test 5"));
   }
   if (!quC.near(cSys.qualityCoordinate(uint32_t(iC)))) {
	   msg = String("Failed qualityCoordinate test because ") +
			   quC.errorMessage();
	   throw(AipsError(msg));
   }
   const int32_t qualityPixelAxis = cSys.pixelAxes(iC)(0);
   const int32_t qualityWorldAxis = cSys.worldAxes(iC)(0);
   const int32_t whichQualityCoordinate = iC;
//
   iC = cSys.findCoordinate(Coordinate::LINEAR);
   if (iC != int32_t(iLC)) {
      throw(AipsError("Failed findCoordinate test 6"));
   }
   if (!lC.near(cSys.linearCoordinate(uint32_t(iC)))) {
      msg = String("Failed linearCoordinate test because ") +   
                   lC.errorMessage();
      throw(AipsError(msg));
   }
//
   uint32_t nPixelAxes = 0;
   uint32_t nWorldAxes = 0;
   for (uint32_t i=0; i<nCoords; i++) {  
      nPixelAxes += cSys.coordinate(i).nPixelAxes();
      nWorldAxes += cSys.coordinate(i).nWorldAxes();
   }
   if (cSys.nPixelAxes() != nPixelAxes) {
      throw(AipsError("Failed nPixelAxes test"));
   }
   if (cSys.nWorldAxes() != nWorldAxes) {
      throw(AipsError("Failed nWorldAxes test"));
   }
//
// These tests are not conclusive because I cannot predict
// what the order of the world and pixle axes will be
// in the CoordinateSystem
//
   int32_t coordinate, axisInCoordinate;
   for (uint32_t i=0; i<cSys.nWorldAxes(); i++) {
      cSys.findWorldAxis(coordinate, axisInCoordinate, i);
      if (coordinate==-1) {
         throw(AipsError("Failed findWorldAxis test 1"));
      } else {
         Vector<int32_t> worldAxes = cSys.worldAxes(coordinate);      
         if (axisInCoordinate >= int32_t(worldAxes.nelements()) ||
             axisInCoordinate<0) {
            throw(AipsError("Failed findWorldAxis test 2"));
         }
         if (worldAxes.nelements() == 
             cSys.coordinate(coordinate).nWorldAxes()) {
            bool ok = false;

// Try and find the original world axis (i) in this list
// of world axes
//
            for (uint32_t j=0; j<worldAxes.nelements(); j++) {
               if (int32_t(i)==worldAxes(j)) {
                  ok = true;
                  break;
               }
            }
            if (!ok) {
               throw(AipsError("Failed findWorldAxis test 3"));
            }
         } else {
            throw(AipsError("Failed findWorldAxis test 4"));
         }
      }
   }
//
   for (uint32_t i=0; i<cSys.nPixelAxes(); i++) {
      cSys.findPixelAxis(coordinate, axisInCoordinate, i);
      if (coordinate==-1) {
         throw(AipsError("Failed findPixelAxis test 1"));
      } else {
         Vector<int32_t> pixelAxes = cSys.pixelAxes(coordinate);      
         if (axisInCoordinate >= int32_t(pixelAxes.nelements()) ||
             axisInCoordinate<0) {
            throw(AipsError("Failed findPixelAxis test 2"));
         }
         if (pixelAxes.nelements() == 
             cSys.coordinate(coordinate).nPixelAxes()) {
            bool ok = false;

// Try and find the original pixel axis (i) in this list
// of pixel axes
//
            for (uint32_t j=0; j<pixelAxes.nelements(); j++) {
               if (int32_t(i)==pixelAxes(j)) {
                  ok = true;
                  break;
               }
            }
            if (!ok) {
               throw(AipsError("Failed findPixelAxis test 3"));
            }
         } else {
            throw(AipsError("Failed findPixelAxis test 4"));
         }
      }
   }
//
// pixelAxisToWorldAxis and worldAxisToPixelAxis tests.  Not
// very convincing either.
//
   {
      for (uint32_t i=0; i<nCoords; i++) {  
         Vector<int32_t> pixelAxes = cSys.pixelAxes(i);
         Vector<int32_t> worldAxes = cSys.worldAxes(i);
         for (uint32_t j=0; j<pixelAxes.nelements(); j++) {
            int32_t worldAxis = cSys.pixelAxisToWorldAxis(pixelAxes(j));
//
// Try and find this world axis in the list
//
            bool ok = false;
            for (uint32_t k=0; k<worldAxes.nelements(); k++) {
               if (worldAxis==worldAxes(k)) {
                  ok = true;
                  break;
               }
            }
            if (!ok) {
               throw(AipsError("Failed findWorldAxis"));
            }
         }
//
         for (uint32_t j=0; j<worldAxes.nelements(); j++) {
            int32_t pixelAxis = cSys.worldAxisToPixelAxis(worldAxes(j));
//
// Try and find this pixel axis in the list
//
            bool ok = false;
            for (uint32_t k=0; k<pixelAxes.nelements(); k++) {
               if (pixelAxis==pixelAxes(k)) {
                  ok = true;
                  break;
               }
            }
            if (!ok) {
               throw(AipsError("Failed findWorldAxis test worldAxistoPixelAxis test"));
            }
         }
      }
   }
//
// Vaguely convinced that worldAxes works, continue on
// and check the world axis descriptors
//
   Vector<String> worldAxisNames = cSys.worldAxisNames();
   Vector<String> worldAxisUnits = cSys.worldAxisUnits();
   Vector<double> refValues = cSys.referenceValue();
   Vector<double> inc = cSys.increment();
//
   for (uint32_t i=0; i<cSys.nCoordinates(); i++) {
      Vector<int32_t> worldAxes = cSys.worldAxes(i);
      Vector<String> worldAxisNames2 = cSys.coordinate(i).worldAxisNames();
      Vector<String> worldAxisNames3(worldAxes.nelements());
      Vector<String> worldAxisUnits2 = cSys.coordinate(i).worldAxisUnits();
      Vector<String> worldAxisUnits3(worldAxes.nelements());
      Vector<double> refValues2 = cSys.coordinate(i).referenceValue();
      Vector<double> refValues3(worldAxes.nelements());
      Vector<double> inc2 = cSys.coordinate(i).increment();
      Vector<double> inc3(worldAxes.nelements());
//
      for (uint32_t j=0; j<worldAxes.nelements(); j++) {
         worldAxisNames3(j) = worldAxisNames(worldAxes(j));
         worldAxisUnits3(j) = worldAxisUnits(worldAxes(j));
         refValues3(j) = refValues(worldAxes(j));
         inc3(j) = inc(worldAxes(j));
      }
      if (!allEQ(worldAxisNames2, worldAxisNames3)) {
         throw(AipsError("Failed world axis name recovery test"));
      }
      if (!allEQ(worldAxisUnits2, worldAxisUnits3)) {
         throw(AipsError("Failed world axis unit recovery test"));
      }
      if (!allNear(refValues2, refValues3, 1e-6)) {
         throw(AipsError("Failed reference pixel recovery test"));
      }
      if (!allNear(inc2, inc3, 1e-6)) {
         throw(AipsError("Failed increment recovery test"));
      }
   }
//
   worldAxisNames(0) = "Horsies";
   if (!cSys.setWorldAxisNames(worldAxisNames)) {
      throw(AipsError(String("Failed to set world axis name because") + cSys.errorMessage()));
   }
   if (!allEQ(worldAxisNames, cSys.worldAxisNames())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
   double t = refValues(stokesWorldAxis);
   double u = refValues(qualityWorldAxis);
   refValues = refValues*(double)2.0;
   refValues(stokesWorldAxis)  = t;
   refValues(qualityWorldAxis) = u;
   if (!cSys.setReferenceValue(refValues)) {
      throw(AipsError(String("Failed to set reference value because") 
            + cSys.errorMessage()));
   }
   if (!allNear(refValues, cSys.referenceValue(), 1e-6)) {
      throw(AipsError("Failed reference value set/recovery test"));
   }
//
   t = inc(stokesWorldAxis);
   u = inc(qualityWorldAxis);
   inc = inc*(double)2.0;
   inc(stokesWorldAxis)  = t;
   inc(qualityWorldAxis) = u;
   if (!cSys.setIncrement(inc)) {
      throw(AipsError(String("Failed to set increment because") 
            + cSys.errorMessage()));
   }
   if (!allNear(inc, cSys.increment(), 1e-6)) {
      throw(AipsError("Failed increment set/recovery test"));
   }
   //
   t = inc(qualityWorldAxis);
   inc(0) = inc(0)*2;
   inc(qualityWorldAxis) = t;
   if (!cSys.setIncrement(inc)) {
	   throw(AipsError(String("Failed to set increment because")
			   + cSys.errorMessage()));
   }
   if (!allNear(inc, cSys.increment(), 1e-6)) {
	   throw(AipsError("Failed increment set/recovery test"));
   }
    //
   {
     iC = cSys.findCoordinate(Coordinate::DIRECTION);
     Vector<int32_t> worldAxes = cSys.worldAxes(iC);
     worldAxisUnits(worldAxes(0)) = "deg";
     worldAxisUnits(worldAxes(1)) = "arcmin";
     if (!cSys.setWorldAxisUnits(worldAxisUnits)) {
        throw(AipsError(String("Failed to set axis units because") 
              + cSys.errorMessage()));
     }
     if (!allEQ(worldAxisUnits, cSys.worldAxisUnits())) {
        throw(AipsError("Failed axis units set/recovery test"));
     }
   }
   Vector<String> bogus = worldAxisUnits.copy();
   bogus.resize(worldAxisUnits.size() - 1);
   AlwaysAssert(! cSys.setWorldAxisUnits(bogus), AipsError);
   try {
	   cSys.setWorldAxisUnits(bogus, true);
	   // this should have thrown an exception, if not, its a failure
	   AlwaysAssert(false, AipsError);
   }
   catch (const std::exception& x) {}

//
//
// Now check the pixel axis descriptors
//
   Vector<double> refPixels = cSys.referencePixel();
   for (uint32_t i=0; i<cSys.nCoordinates(); i++) {
      Vector<int32_t> pixelAxes = cSys.pixelAxes(i);
      Vector<double> refPixels2 = cSys.coordinate(i).referencePixel();
      Vector<double> refPixels3(pixelAxes.nelements());
//
      for (uint32_t j=0; j<pixelAxes.nelements(); j++) {
         refPixels3(j) = refPixels(pixelAxes(j));
      }
      if (!allNear(refPixels2, refPixels3, 1e-6)) {
         throw(AipsError("Failed reference pixel recovery test"));
      }
   }
   t = refPixels(stokesPixelAxis);
   u = refPixels(qualityPixelAxis);
   refPixels = refPixels*(double)2.0;
   refPixels(stokesPixelAxis) = t;
   refPixels(qualityPixelAxis) = u;
   if (!cSys.setReferencePixel(refPixels)) {
      throw(AipsError(String("Failed to set reference pixel because") 
            + cSys.errorMessage()));
   }
   if (!allNear(refPixels, cSys.referencePixel(), 1e-6)) {
      throw(AipsError("Failed reference pixel set/recovery test"));
   }
//
// Test the linear transform.  SHould be all 0 and diagonal of unity
//
   if (cSys.linearTransform().nrow() != cSys.nWorldAxes()) {
      throw(AipsError("Linear transform has wrong number of rows"));
   }
   if (cSys.linearTransform().ncolumn() != cSys.nWorldAxes()) {
      throw(AipsError("Linear transform has wrong number of columns"));
   }
   for (uint32_t i=0; i<cSys.linearTransform().nrow(); i++) {
      for (uint32_t j=0; j<cSys.linearTransform().ncolumn(); j++) {
         if (i==j) {
             if (cSys.linearTransform()(i,j)!=1.0) {
                throw(AipsError("Linear transform diagonal is not unity"));
             }
         } else {
             if (cSys.linearTransform()(i,j)!=0.0) {
                throw(AipsError("Linear transform off-diagonal is not zero"));
             }
         }
      }
   }
   Matrix<double> xform = cSys.linearTransform();
   xform(0,0) = 10.0; // this affects the direction coordinate which enforces that 
                      // the linear transform rows pertaining to it obey 
                      // for each i: xform(i,0)^2 + xform(i,1)^s == 1.0
                      // The normalization is tranferred to the increment.
   Matrix<double> xformOut(cSys.nWorldAxes(), cSys.nWorldAxes());
   xformOut = 0.;
   xformOut.diagonal() = 1.0;
   Vector<double> cdeltOut; 
   cdeltOut = cSys.increment();
   cdeltOut(0) *= 10.;

   if (!cSys.setLinearTransform(xform)) {
      throw(AipsError(String("Failed to set linear transform because") 
            + cSys.errorMessage()));
   }

   if (!allNear(xformOut, cSys.linearTransform(), 1e-6)) {
      throw(AipsError("Failed linear transform set/recovery test (wrong resulting xform)"));
   }
   if (!allNear(cdeltOut, cSys.increment(), 1E-6)) {
      throw(AipsError("Failed linear transform set/recovery test (wrong resulting cdelt)"));
   }

//
// Test FITS interface.  Do this with a CS without a TabularCoordinate
// because that is not reflected back by the FITS  conversion
//
   {
      CoordinateSystem cSys3;
      Vector<String> header;
      Record rec;
      IPosition shape;
      int32_t stokesFITSValue = -1;
//
      if (CoordinateSystem::fromFITSHeader(stokesFITSValue, cSys3, rec, header,
                                           shape, 0)) {
         throw(AipsError("Unexpectedly did not fail fromFITSHeader (0)"));
      }
   }
   {
      CoordinateSystem cSys2;
      cSys2.addCoordinate(makeDirectionCoordinate(false));
      StokesCoordinate  stokesCoord = makeStokesCoordinate(false);
      uint32_t shapeStokes = stokesCoord.stokes().nelements();
      uint32_t stokesAxis = 2;
      QualityCoordinate qualCoord   = makeQualityCoordinate();
      uint32_t shapeQual = qualCoord.quality().nelements();
      uint32_t qualAxis  = 3;
      cSys2.addCoordinate(stokesCoord);
      cSys2.addCoordinate(qualCoord);
      cSys2.addCoordinate(makeSpectralCoordinate());
      cSys2.addCoordinate(makeLinearCoordinate());
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      shape(qualAxis)   = shapeQual;
      if (!cSys2.toFITSHeader(rec, shape, true, 'c', false,
                        true, true)) {
         throw(AipsError(String("Failed to convert to FITS header (1)")));
      }

// Assigning cSys3 rather than leaving it empty will force testing of 
// more code in fromFITSHeader

// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3 = CoordinateUtil::defaultCoords2D();
//       Vector<String> header;
//       int32_t stokesFITSValue = -1;
//       if (!CoordinateSystem::fromFITSHeader(stokesFITSValue, cSys3, rec, header,
// 					    shape, 0)) {
// 	  throw(AipsError("Failed to convert from FITS header (1)"));
//       }
//       if (!cSys2.near(cSys3)) {
// 	  msg = String("Failed to/fromFITS consistency test (1) because ") +   
// 	      cSys2.errorMessage();
// 	  throw(AipsError(msg));
//       } 
   }

// Do lots of Stokes combinations to exercise as much code
// as possible

   {
      CoordinateSystem cSys2;
      Vector<int32_t> whichStokes(4);
      whichStokes(0) = Stokes::I;
      whichStokes(1) = Stokes::Q;
      whichStokes(2) = Stokes::U;
      whichStokes(3) = Stokes::V;
      StokesCoordinate stokesCoord(whichStokes);
      uint32_t shapeStokes = stokesCoord.stokes().nelements();
      uint32_t stokesAxis = 0;
      cSys2.addCoordinate(stokesCoord);
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      if (!cSys2.toFITSHeader(rec, shape, true, 'c', false,
                        true, true)) {
         throw(AipsError(String("Failed to convert to FITS header (2)")));
      }

// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3;
//       Vector<String> header;
//       int32_t stokesFITSValue = -1;
//       if (!CoordinateSystem::fromFITSHeader(stokesFITSValue, cSys3, rec, header, shape, 0)) {
// 	  throw(AipsError("Failed to convert from FITS header (2)"));
//       }
//       if (!cSys2.near(cSys3)) {
// 	  msg = String("Failed to/fromFITS consistency test (2) because ") +   
// 	      cSys2.errorMessage();
// 	  throw(AipsError(msg));
//       }
   }
   {
      CoordinateSystem cSys2;
      Vector<int32_t> whichStokes(4);
      whichStokes(0) = Stokes::RR;
      whichStokes(1) = Stokes::LL;
      whichStokes(2) = Stokes::RL;
      whichStokes(3) = Stokes::LR;
      StokesCoordinate stokesCoord(whichStokes);
      uint32_t shapeStokes = stokesCoord.stokes().nelements();
      uint32_t stokesAxis = 0;
      cSys2.addCoordinate(stokesCoord);
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      if (!cSys2.toFITSHeader(rec, shape, true, 'c', false,
                        true, true)) {
         throw(AipsError(String("Failed to convert to FITS header (3)")));
      }


// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3;
//       Vector<String> header;
//       int32_t stokesFITSValue = -1;
//       if (!CoordinateSystem::fromFITSHeader(stokesFITSValue, cSys3, rec, header, shape, 0)) {
//          throw(AipsError("Failed to convert from FITS header (3)"));
//       }
//       if (!cSys2.near(cSys3)) {
//          msg = String("Failed to/fromFITS consistency test (3) because ") +   
//                       cSys2.errorMessage();
//          throw(AipsError(msg));
//       }
   }
   {
      CoordinateSystem cSys2;
      Vector<int32_t> whichStokes(4);
      whichStokes(0) = Stokes::XX;
      whichStokes(1) = Stokes::YY;
      whichStokes(2) = Stokes::XY;
      whichStokes(3) = Stokes::YX;
      StokesCoordinate stokesCoord(whichStokes);
      uint32_t shapeStokes = stokesCoord.stokes().nelements();
      uint32_t stokesAxis = 0;
      cSys2.addCoordinate(stokesCoord);
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      if (!cSys2.toFITSHeader(rec, shape, true, 'c', false,
                        true, true)) {
         throw(AipsError(String("Failed to convert to FITS header (4)")));
      }

// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3;
//       Vector<String> header;
//       int32_t stokesFITSValue = -1;
//       if (!CoordinateSystem::fromFITSHeader(stokesFITSValue, cSys3, rec, header, shape, 0)) {
//          throw(AipsError("Failed to convert from FITS header (4)"));
//       }
//       if (!cSys2.near(cSys3)) {
//          msg = String("Failed to/fromFITS consistency test (4) because ") +   
//                       cSys2.errorMessage();
//          throw(AipsError(msg));
//       }
   }


//   
// Test record saving.
//
   Record rec;
   if (!cSys.save(rec, "coordsys")) {
      throw(AipsError("Saving to Record failed"));  
   }  
// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 
//    CoordinateSystem* pcSys = CoordinateSystem::restore(rec, "coordsys");
//    if (!pcSys->near(cSys, 1e-6)) {
//       throw(AipsError("Reflection through record interface (1) failed"));  
//    }
//   delete pcSys;
//
//    Record rec2 = rec.asRecord("coordsys");
//    pcSys = CoordinateSystem::restore(rec2, "");
//    if (!pcSys->near(cSys, 1e-6)) {
//       throw(AipsError("Reflection through record interface (2) failed"));  
//    }
//    delete pcSys;
	
//
// Test clone
//
   Coordinate* pcSys2 = cSys.clone();
   if (!pcSys2->near(cSys, 1e-6)) {
      throw(AipsError("Clone function failed"));  
   }
   delete pcSys2;
//
// Coordinate restoration
//
  {
      CoordinateSystem cSys2, cSys3;
      cSys2.addCoordinate(makeDirectionCoordinate());
      cSys2.addCoordinate(makeStokesCoordinate(false));
      cSys2.addCoordinate(makeQualityCoordinate());
      cSys2.addCoordinate(makeLinearCoordinate());
      cSys3 = cSys2;
//
      Vector<int32_t> wOrder(cSys2.nWorldAxes());
      Vector<int32_t> pOrder(cSys2.nPixelAxes());
      for (uint32_t i=0; i<wOrder.nelements(); i++) wOrder(i) = wOrder.nelements()-i-1;
      for (uint32_t i=0; i<pOrder.nelements(); i++) pOrder(i) = pOrder.nelements()-i-1;
      cSys2.transpose(wOrder, pOrder);
      cSys2.removePixelAxis(0, 0.0);
      cSys2.removeWorldAxis(2, 0.0);
      cSys2.restoreOriginal();
      if (!cSys2.near(cSys3, 1e-6)) {
         throw(AipsError("restoreOriginal test failed"));  
      }
   }
//
// axis removal.  hard to check rigorously
//
  {
     cSys.restoreOriginal();
     const uint32_t nWorldAxes = cSys.nWorldAxes();
     const uint32_t nPixelAxes = cSys.nPixelAxes();
     cSys.removeWorldAxis(0, 0.0);
     if (cSys.nWorldAxes()!=(nWorldAxes-1) ||
         cSys.nPixelAxes()!=(nPixelAxes-1)) {
        throw(AipsError("removeWorldAxis test failed"));  
     }
//
     cSys.restoreOriginal();
     cSys.removePixelAxis(0, 0.0);
     if (cSys.nWorldAxes()!=nWorldAxes ||
         cSys.nPixelAxes()!=(nPixelAxes-1)) {
        throw(AipsError("removePixelAxis test failed"));  
     }
  }   
//
// Coordinate replacement
//
  {
      CoordinateSystem cSys2;
      cSys2.addCoordinate(makeDirectionCoordinate(true, MDirection::J2000));
      cSys2.addCoordinate(makeStokesCoordinate(false));
      cSys2.replaceCoordinate(makeDirectionCoordinate(true, MDirection::B1950), 0);
      if (cSys2.type(0) != Coordinate::DIRECTION ||
          cSys2.type(1) != Coordinate::STOKES ||
          cSys2.nCoordinates()!=2) {
        throw(AipsError("Coordinate replacement test failed"));  
      }
  }   
//
// Obsinfo
//
    {
       cSys.restoreOriginal();
       ObsInfo oI1 = cSys.obsInfo();
       oI1.setTelescope("doggies");
       oI1.setObserver("rintintin");
       cSys.setObsInfo(oI1); 
       ObsInfo oI2 = cSys.obsInfo();
       if (oI2.telescope() != oI1.telescope() ||
           oI2.observer() != oI1.observer()) {       
          throw(AipsError("Failed ObsInfo test"));  
       }
    }
//
// SubImage.  
//
   {
      cSys.restoreOriginal();
      Vector<float> originShift(cSys.nPixelAxes());
      Vector<float> pixinc(cSys.nPixelAxes());
//
      {
        originShift = 1.0;
        pixinc = 1.0;
        Vector<int32_t> newShape;
//
        CoordinateSystem cSys2 = cSys.subImage(originShift, pixinc, newShape);
        if (cSys.nCoordinates() != cSys2.nCoordinates()) {
           throw(AipsError("Failed originShift creation test 1"));
        }
        Vector<double> pixel = cSys.referencePixel();
        Vector<double> pixel2 = cSys2.referencePixel() + 1.0;
        pixel2(stokesPixelAxis)  = pixel(stokesPixelAxis);
        pixel2(qualityPixelAxis) = pixel(qualityPixelAxis);
        if (!allNear(pixel, pixel2, 1e-6)) {
           throw(AipsError("Failed originShift test 1"));
        }   
//
        CoordinateSystem cSys3(cSys);
        cSys3.subImageInSitu (originShift, pixinc, newShape);
        if (cSys.nCoordinates() != cSys3.nCoordinates()) {
           throw(AipsError("Failed originShift creation test 2"));
        }
        Vector<double> pixel3 = cSys3.referencePixel() + 1.0;
        pixel3(stokesPixelAxis)  = pixel(stokesPixelAxis);
        pixel3(qualityPixelAxis) = pixel(qualityPixelAxis);
        if (!allNear(pixel, pixel3, 1e-6)) {
           throw(AipsError("Failed originShift test 2"));
        }   
      }
//
      {
        originShift = 0.0;
        pixinc = 2.0;
        Vector<int32_t> newShape;
//
        {
           CoordinateSystem cSys2 = cSys.subImage(originShift, pixinc, newShape);
           Vector<int32_t> oldStokes = cSys.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<int32_t> newStokes = cSys2.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<int32_t> oldQual   = cSys.qualityCoordinate(whichQualityCoordinate).quality();
           Vector<int32_t> newQual   = cSys2.qualityCoordinate(whichQualityCoordinate).quality();
           Vector<int32_t> newStokes2 = oldStokes(IPosition(1,0),
                                              IPosition(1,oldStokes.nelements()-1),
                                              IPosition(1,2));
           Vector<int32_t> newQual2   = oldQual(IPosition(1,0),
                   IPosition(1,oldQual.nelements()-1),
                   IPosition(1,2));
           if (!allEQ(newStokes, newStokes2)) {
              throw(AipsError("Failed Stokes originShift Stokes test"));
           } 
           if (!allEQ(newQual, newQual2)) {
              throw(AipsError("Failed Quality originShift Quality test"));
           }
        }
//
        {
           CoordinateSystem cSys2(cSys);
           cSys2.subImageInSitu (originShift, pixinc, newShape);
           Vector<int32_t> oldStokes = cSys.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<int32_t> newStokes = cSys2.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<int32_t> oldQual   = cSys.qualityCoordinate(whichQualityCoordinate).quality();
           Vector<int32_t> newQual   = cSys2.qualityCoordinate(whichQualityCoordinate).quality();
           Vector<int32_t> newStokes2 = oldStokes(IPosition(1,0),
                                              IPosition(1,oldStokes.nelements()-1),
                                              IPosition(1,2));
           Vector<int32_t> newQual2   = oldQual(IPosition(1,0),
                   IPosition(1,oldQual.nelements()-1),
                   IPosition(1,2));
           if (!allEQ(newStokes, newStokes2)) {
              throw(AipsError("Failed Stokes originShift Stokes test"));
           }
           if (!allEQ(newQual, newQual2)) {
              throw(AipsError("Failed Quality originShift Quality test"));
           }
         }
      }
   }
}

void doit2 (CoordinateSystem& cSys)

{
   int32_t stokesPixelAxis = -1;
   int32_t stokesWorldAxis = -1;
   int32_t iC = cSys.findCoordinate(Coordinate::STOKES);
   if (iC>=0) {
	   stokesPixelAxis = cSys.pixelAxes(iC)(0);
	   stokesWorldAxis = cSys.worldAxes(iC)(0);
   }
   int32_t qualityPixelAxis = -1;
   int32_t qualityWorldAxis = -1;
   iC = cSys.findCoordinate(Coordinate::QUALITY);
   if (iC>=0) {
	   qualityPixelAxis = cSys.pixelAxes(iC)(0);
	   qualityWorldAxis = cSys.worldAxes(iC)(0);
   }
//
// Test conversion
//
   Vector<double> pixel(cSys.referencePixel()), world;
   if (!cSys.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld #1 conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!allNear(world, cSys.referenceValue(), 1e-6)) {
         throw(AipsError("Coordinate conversion gave wrong results"));
   }

   if (!cSys.toPixel(pixel, world)) {
      throw(AipsError(String("toPixel conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!allNear(pixel, cSys.referencePixel(), 1e-6)) {
         throw(AipsError("Coordinate conversion gave wrong results"));
   }
//
   pixel(0) = 123.0;
   Vector<double> pixel2(pixel.copy());
   if (!cSys.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld #2 conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!cSys.toPixel(pixel, world)) {
      throw(AipsError(String("toPixel #3 conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!allNear(pixel2, pixel, 1e-6)) {
      throw(AipsError("Coordinate reflection gave wrong results"));
   }
//
   pixel = 2.0;
   pixel(stokesPixelAxis) = 0.0;
   pixel(qualityPixelAxis)= 0.0;
   IPosition iPixel(pixel.nelements());
   for (uint32_t i=0; i<iPixel.nelements(); i++) iPixel(i) = int32_t(pixel(i));
   Vector<double> world2;
   if (!cSys.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld #4 conversion failed because ")
                     + cSys.errorMessage()));
   }
   if (!cSys.toWorld(world2, iPixel)) {
      throw(AipsError(String("toWorld #5 conversion failed because ")
                     + cSys.errorMessage()));
   }
   if (!allNear(world, world2, 1e-6)) {
      throw(AipsError("toWorld consistency test failed"));
   }
//
   Vector<bool> failures;
   const uint32_t nBatch = 3;
   Matrix<double> pixel3(cSys.nPixelAxes(), nBatch);
   Matrix<double> world3(cSys.nWorldAxes(), nBatch);
   for (uint32_t i=0; i<nBatch; i++) {
      pixel3.column(i) = cSys.referencePixel();
   }
   if (!cSys.toWorldMany(world3, pixel3, failures)) {
      throw(AipsError(String("toWorldMany conversion failed because ")
                     + cSys.errorMessage()));
   }
   for (uint32_t i=0; i<nBatch; i++) {
      if (!allNear(world3.column(i), cSys.referenceValue(), 1e-6)) {
            throw(AipsError("toWorldMany conversion to world gave wrong results"));
      }
   }
//
   failures.resize(0);
   pixel3 = 0.0;
   if (!cSys.toPixelMany(pixel3, world3, failures)) {
      throw(AipsError(String("toPixelMany conversion failed because ")
                     + cSys.errorMessage()));
   }
   for (uint32_t i=0; i<nBatch; i++) {
      if (!allNear(pixel3.column(i), cSys.referencePixel(), 1e-6)) {
            throw(AipsError("toPixelMany conversion to world gave wrong results"));
      }
   }

// relative/absolute pixels

   {
      Vector<double> refPix = cSys.referencePixel();
      Vector<double> pixel4 = refPix.copy();
      cSys.makePixelRelative(pixel4);
      if (!allNear(pixel4, 0.0, 1e-6)) {
            throw(AipsError("Coordinate makePixelRelative 1 gave wrong results"));
      }
//
      pixel4 = refPix + 1.0;
      Vector<double> tmp = pixel4.copy();
      cSys.makePixelRelative(pixel4);
      if (!allNear(pixel4, 1.0, 1e-6)) {
            throw(AipsError("Coordinate makePixelRelative 2 gave wrong results"));
      }
      cSys.makePixelAbsolute (pixel4);
      if (!allNear(pixel4, tmp, 1e-6)) {
            throw(AipsError("Coordinate makePixelAbsolute 1 gave wrong results"));
      }
//
      pixel4 = refPix - 1.0;
      tmp = pixel4;
      cSys.makePixelRelative(pixel4);
      if (!allNear(pixel4, -1.0, 1e-6)) {
            throw(AipsError("Coordinate makePixelRelative 3 gave wrong results"));
      }
      cSys.makePixelAbsolute (pixel4);
      if (!allNear(pixel4, tmp, 1e-6)) {
            throw(AipsError("Coordinate makePixelAbsolute 2 gave wrong results"));
      }
   }     

// relative/absolute world

   {
      Vector<double> result = cSys.referenceValue().copy();
//
      Vector<double> refVal = cSys.referenceValue();
      Vector<double> world4 = refVal.copy();
      cSys.makeWorldRelative(world4);
      result = 0.0;
      result(stokesWorldAxis)  = refVal(stokesWorldAxis);
      result(qualityWorldAxis) = refVal(qualityWorldAxis);
      if (!allNearAbs(world4, result, 1e-6)) {
            throw(AipsError("makeWorldRelative 1 gave wrong results"));
      }
//
      Vector<double> incr = cSys.increment();
      world4 = refVal + incr;
      Vector<double> tmp = world4.copy();
      cSys.makeWorldRelative(world4);
      cSys.makeWorldAbsolute (world4);
      if (!allNearAbs(world4, tmp, 1e-6)) {
            throw(AipsError("makeWorldAbsolute/Relative reflection 1 failed"));
      }
//
      world4 = refVal - incr;
      tmp = world4;
      cSys.makeWorldRelative(world4);
      cSys.makeWorldAbsolute (world4);
      if (!allNearAbs(world4, tmp, 1e-6)) {
            throw(AipsError("Coordinate makeWorldAbsolute/Reflection 2 failed"));
      }
   }     
//
// Formatting
//
   {
      int32_t iDC = cSys.findCoordinate(Coordinate::DIRECTION);
      Vector<int32_t> worldAxes = cSys.worldAxes(iDC);
      Vector<String> worldAxisUnits = cSys.worldAxisUnits();
      worldAxisUnits(worldAxes(0)) = "rad";
      worldAxisUnits(worldAxes(1)) = "rad";
      cSys.setWorldAxisUnits(worldAxisUnits);
//
      String unit("rad");
      double val = 0.12343;
      Quantum<double> valq(0.12343, Unit("rad"));
      valq.convert(Unit("deg"));
      String str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(0),
                               true, true, 4);
      String str2 = cSys.formatQuantity(unit, Coordinate::FIXED, valq, worldAxes(0),
                                        true, true, 4);
      if (str != "0.1234" || str2 != "0.1234") {
         throw(AipsError("Failed format test 1"));
      }
      str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(1),
                        true, true, 4);
      str2 = cSys.formatQuantity(unit, Coordinate::FIXED, valq, worldAxes(1),
                         true, true, 4);
      if (str != "0.1234" || str2!="0.1234") {
         throw(AipsError("Failed format test 2"));
      }
//
      str = cSys.format(unit, Coordinate::SCIENTIFIC, val, worldAxes(0),
                        true, true, 4);
      str2 = cSys.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, worldAxes(0),
                                 true, true, 4);
      if (str != "1.2343e-01" || str2 != "1.2343e-01") {
         throw(AipsError("Failed format test 3"));
      }
      str = cSys.format(unit, Coordinate::SCIENTIFIC, val, worldAxes(1),
                        true, true, 4);
      str2 = cSys.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, worldAxes(1),
                                 true, true, 4);
      if (str != "1.2343e-01" || str2 != "1.2343e-01") {
         throw(AipsError("Failed format test 4"));
      }
   }
   {
      int32_t iSpC = cSys.findCoordinate(Coordinate::SPECTRAL);
      Vector<int32_t> worldAxes = cSys.worldAxes(iSpC);
//
      String unit;
      double val = 0.12343;
      Quantum<double> valq(0.12343, Unit("rad"));
      valq.convert(Unit("deg"));
      String str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(0),
                               true, true, 4);
      String str2 = cSys.formatQuantity(unit, Coordinate::FIXED, valq, worldAxes(0),
                                        true, true, 4);
      if (str != "0.1234" || str2 != "0.1234") {
         throw(AipsError("Failed format test 5"));
      }
   }
}

void doit3 (CoordinateSystem& cSys)
{
//
// Transposition
//
   {
      Vector<int32_t> newWorldOrder(cSys.nWorldAxes());
      Vector<int32_t> newPixelOrder(cSys.nPixelAxes());
      for (uint32_t i=0; i<cSys.nWorldAxes(); i++) newWorldOrder(i) = i; 
      for (uint32_t i=0; i<cSys.nPixelAxes(); i++) newPixelOrder(i) = i;

      int32_t iDC = cSys.findCoordinate(Coordinate::DIRECTION);
      Vector<int32_t> worldAxes = cSys.worldAxes(iDC);
      Vector<int32_t> pixelAxes = cSys.pixelAxes(iDC);
      Vector<int32_t> newWorldAxes(worldAxes.copy());
      Vector<int32_t> newPixelAxes(pixelAxes.copy());
      newWorldOrder(worldAxes(0)) = worldAxes(1);
      newWorldOrder(worldAxes(1)) = worldAxes(0);
      newPixelOrder(pixelAxes(0)) = pixelAxes(1);
      newPixelOrder(pixelAxes(1)) = pixelAxes(0);
      newWorldAxes(0) = worldAxes(1);
      newWorldAxes(1) = worldAxes(0);
      newPixelAxes(0) = pixelAxes(1);
      newPixelAxes(1) = pixelAxes(0);
//
      cSys.transpose(newWorldOrder, newPixelOrder);
      if (!allEQ(newWorldAxes, cSys.worldAxes(iDC)) ||
          !allEQ(newPixelAxes, cSys.pixelAxes(iDC))) {
         throw(AipsError("Failed transposition test"));
      }
   }
//
// World, pixel map
//
   {
      CoordinateSystem cSys2;
      cSys2.addCoordinate(makeStokesCoordinate(false));
      cSys2.addCoordinate(makeQualityCoordinate());
      cSys2.addCoordinate(makeDirectionCoordinate(false, MDirection::B1950));
      cSys2.addCoordinate(makeLinearCoordinate());
      CoordinateSystem cSys3 = cSys2;
      cSys3.replaceCoordinate(makeDirectionCoordinate(false, MDirection::J2000),2);
//
      Vector<int32_t> worldAxisMap, worldAxisTranspose;
      Vector<int32_t> pixelAxisMap, pixelAxisTranspose;
      Vector<bool> refChange;
      if (!cSys2.worldMap(worldAxisMap, worldAxisTranspose, refChange, cSys3)) {
         throw(AipsError("Failed to make world map 1"));
      }
      Vector<int32_t> wMap(cSys2.nWorldAxes()), wTranspose(cSys2.nWorldAxes());
      for (uint32_t i=0; i<cSys2.nWorldAxes(); i++) {
         wMap(i) = i; 
         wTranspose(i) = i; 
      }
      if (!allEQ(wMap, worldAxisMap) ||
          !allEQ(wTranspose, worldAxisTranspose)) {
         throw(AipsError("Failed worldMap test 1a"));
      }
      if (refChange(0)!=false || refChange(2)!=true ||
          refChange(3)!=true || refChange(4)!=false) {
         throw(AipsError("Failed worldMap test 1b"));
      }
//
      if (!cSys2.pixelMap(pixelAxisMap, pixelAxisTranspose, cSys3)) {
         throw(AipsError("Failed to make pixel map 1"));
      }
      Vector<int32_t> pMap(cSys2.nPixelAxes()), pTranspose(cSys2.nPixelAxes());
      for (uint32_t i=0; i<cSys2.nPixelAxes(); i++) {
         pMap(i) = i; 
         pTranspose(i) = i; 
      }
      if (!allEQ(pMap, pixelAxisMap) ||
          !allEQ(pTranspose, pixelAxisTranspose)) {
         throw(AipsError("Failed pixelMap test 1a"));
      }
//
      Vector<int32_t> newWorldOrder(cSys2.nWorldAxes());
      Vector<int32_t> newPixelOrder(cSys2.nPixelAxes());
      for (uint32_t i=0; i<cSys2.nWorldAxes(); i++) newWorldOrder(i) = i; 
      for (uint32_t i=0; i<cSys2.nPixelAxes(); i++) newPixelOrder(i) = i;
//
      int32_t iDC = cSys2.findCoordinate(Coordinate::DIRECTION);
      Vector<int32_t> worldAxes = cSys2.worldAxes(iDC);
      Vector<int32_t> pixelAxes = cSys2.pixelAxes(iDC);
      newWorldOrder(worldAxes(0)) = worldAxes(1);
      newWorldOrder(worldAxes(1)) = worldAxes(0);
      newPixelOrder(pixelAxes(0)) = pixelAxes(1);
      newPixelOrder(pixelAxes(1)) = pixelAxes(0);
//
      cSys2.transpose(newWorldOrder, newPixelOrder);
//
      if (!cSys2.worldMap(worldAxisMap, worldAxisTranspose, refChange, cSys3)) {
         throw(AipsError("Failed to make world map 2"));
      }
      if (!cSys2.pixelMap(pixelAxisMap, pixelAxisTranspose, cSys3)) {
         throw(AipsError("Failed to make pixel map 2"));
      }
      Vector<int32_t> newMap(wMap.copy());
      Vector<int32_t> newTranspose(worldAxisTranspose.copy());
      newMap(worldAxes(0)) = newWorldOrder(worldAxes(0));
      newMap(worldAxes(1)) = newWorldOrder(worldAxes(1));
      newTranspose(worldAxes(0)) = newWorldOrder(worldAxes(0));
      newTranspose(worldAxes(1)) = newWorldOrder(worldAxes(1));
//
      if (!allEQ(newMap, worldAxisMap) ||
          !allEQ(newTranspose, worldAxisTranspose)) {
         throw(AipsError("Failed worldMap test 2a"));
      }
      if (refChange(0)!=false || refChange(2)!=true ||
          refChange(3)!=true || refChange(4)!=false) {
         throw(AipsError("Failed worldMap test 2b"));
      }
      if (!allEQ(newMap, pixelAxisMap) ||
          !allEQ(newTranspose, pixelAxisTranspose)) {
         throw(AipsError("Failed pixelMap test 2a"));
      }

   }
}

void doit4()
//
// test mixed conversion functions 
//
{
   CoordinateSystem cSys;
   LinearCoordinate lC = makeLinearCoordinate(1);         // 0
   cSys.addCoordinate(lC);
   SpectralCoordinate spC = makeSpectralCoordinate();     // 1
   cSys.addCoordinate(spC);
   DirectionCoordinate dC = makeDirectionCoordinate(true);// 2 & 3
   cSys.addCoordinate(dC);
//
//   cout << "Reference pixel = " << cSys.referencePixel() << endl;
//   cout << "Reference value = " << cSys.referenceValue() << endl;
//
   Vector<double> pixelIn(cSys.nPixelAxes());
   Vector<double> worldIn(cSys.nWorldAxes());
   Vector<bool> pixelAxes(cSys.nPixelAxes());
   Vector<bool> worldAxes(cSys.nWorldAxes());
   Vector<double> worldOut, pixelOut;
   IPosition shape(cSys.nPixelAxes(), 512);
   if (!cSys.setWorldMixRanges(shape)) {
      throw(AipsError(String("setMixRanges failed with ") + cSys.errorMessage()));
   }
//
   Vector<double> dRefVal = dC.referenceValue();   
   Vector<int32_t> tmp = cSys.worldAxes(2);
//
// Force a failure.   ALl axes must be pixel or world
//
   pixelAxes.set(false);
   worldAxes.set(false);
   Vector<double> worldMin = cSys.worldMixMin();
   Vector<double> worldMax = cSys.worldMixMax();
   if (cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix forced failure 1 did not occur")));
   }
   pixelAxes(0) = true;
   if (cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix forced failure 2 did not occur")));
   }
//
// First test pure pixel->world and world->pixel via the
// mix function.
//
   pixelIn = cSys.referencePixel().copy();
   if (!cSys.toWorld(worldOut, pixelIn)) {
      throw(AipsError(String("toWorld conversion failed because ")
                  + cSys.errorMessage()));
   }
//
   pixelAxes.set(true);
   worldAxes.set(false);
   Vector<double> worldOut2;
   if (!cSys.toMix(worldOut2, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 1 conversion failed because ")
            + cSys.errorMessage()));
   }
   if (!allNear(worldOut, worldOut2, 1e-6)) {
      throw(AipsError("toWorld/toMix consistency test failed"));
   }
   if (!allNear(pixelOut, pixelIn, 1e-6)) {
      throw(AipsError("toWorld/toMix consistency test failed"));
   }
//
// Now try pure world->pixel
//
   worldIn = cSys.referenceValue().copy();
   if (!cSys.toPixel(pixelOut, worldIn)) {
      throw(AipsError(String("toPixel conversion failed because ")
                  + cSys.errorMessage()));
   }
//
   pixelAxes.set(false);
   worldAxes.set(true);
   Vector<double> pixelOut2;
   if (!cSys.toMix(worldOut, pixelOut2, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 2 conversion failed because ")
            + cSys.errorMessage()));
   }
   if (!allNear(pixelOut, pixelOut2, 1e-6)) {
      throw(AipsError("toPixel/toMix consistency test failed"));
   }
   if (!allNear(worldOut, worldIn, 1e-6)) {
      throw(AipsError("toPixel/toMix consistency test failed"));
   }
//
// Now do a real mix.  Use reference values/pixels so we
// can confirm correctness
//
   pixelIn(0) = cSys.referencePixel()(0);   // Linear pixel
   pixelIn(2) = cSys.referencePixel()(2);   // Direction long pixel
   pixelAxes.set(false);
   pixelAxes(0) = true;
   pixelAxes(2) = true;
//
   worldIn(1) = cSys.referenceValue()(1);   // Spectral world
   worldIn(3) = cSys.referenceValue()(3);   // Direction lat world
   worldAxes.set(false);
   worldAxes(1) = true;
   worldAxes(3) = true;
//         
   if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 3 conversion failed because ")
            + cSys.errorMessage()));
   }
//
   if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
      throw(AipsError("toMix consistency test 1 failed"));
   }
   if (!allNear(pixelOut, cSys.referencePixel(), 1e-8)) {
      throw(AipsError("toMix consistency test 1 failed"));
   }
//
// Try another one
//
   pixelIn(1) = cSys.referencePixel()(1);   // Spectral pixel
   pixelIn(3) = cSys.referencePixel()(3);   // Direction lat pixel
   pixelAxes.set(false);
   pixelAxes(1) = true;
   pixelAxes(3) = true;
//
   worldIn(0) = cSys.referenceValue()(0);   // Linear world
   worldIn(2) = cSys.referenceValue()(2);   // Direction long world
   worldAxes.set(false);
   worldAxes(0) = true;
   worldAxes(2) = true;
//         
   if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 4 conversion failed because ")
            + cSys.errorMessage()));
   }
   if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
      throw(AipsError("toMix consistency test 2 failed"));
   }
   if (!allNear(pixelOut, cSys.referencePixel(), 1e-8)) {
      throw(AipsError("toMix consistency test 2 failed"));
   }
//
// Now a non-reference value/pixel reflection test
//
   pixelIn(1) = 20.12;                      // Spectral pixel
   pixelIn(3) = shape(3) - 20;              // Direction lat pixel
   pixelAxes.set(false);
   pixelAxes(1) = true;
   pixelAxes(3) = true;
//
   worldIn(0) = cSys.referenceValue()(0) + 5*cSys.increment()(0);   // Linear world
   worldIn(2) = cSys.referenceValue()(2) - 10*cSys.increment()(2);  // Direction long world
   worldAxes.set(false);
   worldAxes(0) = true;
   worldAxes(2) = true;
//
   Vector<double> saveWorldIn(worldIn.copy());
   Vector<double> savePixelIn(pixelIn.copy());
//         
   if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 5 conversion failed because ")
            + cSys.errorMessage()));
   }
//
   pixelIn(0) = pixelOut(0);
   pixelIn(2) = pixelOut(2);
   pixelAxes.set(false);
   pixelAxes(0) = true; 
   pixelAxes(2) = true;  
//
   worldIn(1) = worldOut(1);
   worldIn(3) = worldOut(3);
   worldAxes.set(false);
   worldAxes(1) = true;
   worldAxes(3) = true;
//
   if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 6 conversion failed because ")
            + cSys.errorMessage()));
   }
//
   if (!near(worldOut(0), saveWorldIn(0), 1e-8)) {
      throw(AipsError("toMix consistency test 3 failed"));
   }
   if (!near(worldOut(2), saveWorldIn(2), 1e-8)) {
      throw(AipsError("toMix consistency test 3 failed"));
   }
   if (!near(pixelOut(1), savePixelIn(1), 1e-8)) {
      throw(AipsError("toMix consistency test 3 failed"));
   }
   if (!near(pixelOut(3), savePixelIn(3), 1e-8)) {
      throw(AipsError("toMix consistency test 3 failed"));
   }
//
// Now reorder the CS world axes,  Gulp
// Linear, Spectral, Direction -> Direction, Spectral, Linear
//
   Vector<int32_t> wOrder(cSys.nWorldAxes());
   Vector<int32_t> pOrder(cSys.nPixelAxes());
   for (uint32_t i=0; i<wOrder.nelements(); i++) wOrder(i) = wOrder.nelements()-i-1;
   for (uint32_t i=0; i<pOrder.nelements(); i++) pOrder(i) = i;
   cSys.transpose(wOrder, pOrder);
   worldMin = cSys.worldMixMin();
   worldMax = cSys.worldMixMax();
//
   pixelIn(1) = cSys.referencePixel()(1);   // Spectral pixel
   pixelIn(3) = cSys.referencePixel()(3);   // Direction lat pixel
   pixelAxes.set(false);
   pixelAxes(1) = true;
   pixelAxes(3) = true;
//
   worldIn(1) = cSys.referenceValue()(1);   // Direction long world
   worldIn(3) = cSys.referenceValue()(3);   // Linear world
   worldAxes.set(false);
   worldAxes(1) = true;
   worldAxes(3) = true;
//
   if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 7 conversion failed because ")
            + cSys.errorMessage()));
   }
   if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
      throw(AipsError("toMix consistency test 2 failed"));
   }
   if (!allNear(pixelOut, cSys.referencePixel(), 1e-8)) {
      throw(AipsError("toMix consistency test 2 failed"));
   }
//
// Now reorder the CS pixel axes,  Gulp.
//
   cSys.restoreOriginal();
   for (uint32_t i=0; i<wOrder.nelements(); i++) wOrder(i) = i;
   for (uint32_t i=0; i<pOrder.nelements(); i++) pOrder(i) = pOrder.nelements()-i-1;
   cSys.transpose(wOrder, pOrder);
   worldMin = cSys.worldMixMin();
   worldMax = cSys.worldMixMax();
//
   pixelIn(2) = cSys.referencePixel()(2);   // Spectral pixel
   pixelIn(0) = cSys.referencePixel()(0);   // Direction lat pixel
   pixelAxes.set(false);
   pixelAxes(2) = true;
   pixelAxes(0) = true;
//
   worldIn(2) = cSys.referenceValue()(2);   // Direction long world
   worldIn(0) = cSys.referenceValue()(0);   // Linear world
   worldAxes.set(false);
   worldAxes(2) = true;
   worldAxes(0) = true;
//
   if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 8 conversion failed because ")
            + cSys.errorMessage()));
   }
   if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
      throw(AipsError("toMix consistency test 2 failed"));
   }
   if (!allNear(pixelOut, cSys.referencePixel(), 1e-8)) {
      throw(AipsError("toMix consistency test 2 failed"));
   }
}

void doit5()
//
// test mixed conversion functions with axis removal
//
{
   {
      CoordinateSystem cSys;
      LinearCoordinate lC = makeLinearCoordinate(1);
      cSys.addCoordinate(lC);
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
//
      Vector<double> pixelIn(cSys.nPixelAxes());
      Vector<double> worldIn(cSys.nWorldAxes());
      Vector<bool> pixelAxes(cSys.nPixelAxes());
      Vector<bool> worldAxes(cSys.nWorldAxes());
      Vector<double> worldOut, pixelOut;
//
      Vector<double> worldMin(cSys.nWorldAxes());
      Vector<double> worldMax(cSys.nWorldAxes());
//
      pixelAxes.set(false);
      worldAxes.set(false);
      worldIn = cSys.referenceValue().copy();
      pixelIn = cSys.referencePixel().copy();
//
      if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                      worldAxes, pixelAxes, worldMin, worldMax)) {
         throw(AipsError(String("toMix conversion failed because ")
               + cSys.errorMessage()));
      }
      if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
         throw(AipsError(String("Failed removal test 1a")));
      }
      if (pixelOut.nelements()!=0) {
         throw(AipsError(String("Failed removal test 1a")));
      }
   }
   {
      CoordinateSystem cSys;
      LinearCoordinate lC = makeLinearCoordinate(1);
      cSys.addCoordinate(lC);
      cSys.removeWorldAxis(0, cSys.referenceValue()(0));
//
      Vector<double> pixelIn(cSys.nPixelAxes());
      Vector<double> worldIn(cSys.nWorldAxes());
      Vector<bool> pixelAxes(cSys.nPixelAxes());
      Vector<bool> worldAxes(cSys.nWorldAxes());
      Vector<double> worldOut, pixelOut;
//
      Vector<double> worldMin(cSys.nWorldAxes());
      Vector<double> worldMax(cSys.nWorldAxes());
//
      pixelAxes.set(false);
      worldAxes.set(false);
      worldIn = cSys.referenceValue().copy();
      pixelIn = cSys.referencePixel().copy();
//
      if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                      worldAxes, pixelAxes, worldMin, worldMax)) {
         throw(AipsError(String("toMix conversion failed because ")
               + cSys.errorMessage()));
      }
      if (worldOut.nelements()!=0) {
         throw(AipsError(String("Failed removal test 2a")));
      }
      if (pixelOut.nelements()!=0) {
         throw(AipsError(String("Failed removal test 2b")));
      }
   }
   {

// pr,pr->w,w

      CoordinateSystem cSys;
      DirectionCoordinate dC = makeDirectionCoordinate(true);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
//
      Vector<double> pixelIn(cSys.nPixelAxes());
      Vector<double> worldIn(cSys.nWorldAxes());
      Vector<bool> pixelAxes(cSys.nPixelAxes());
      Vector<bool> worldAxes(cSys.nWorldAxes());
      Vector<double> worldOut, pixelOut;      
      Vector<double> dRefVal = dC.referenceValue();
//
      Vector<double> worldMin(cSys.nWorldAxes());
      Vector<double> worldMax(cSys.nWorldAxes());
      Vector<int32_t> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(false);
      worldAxes.set(false);
      worldIn = cSys.referenceValue().copy();
      pixelIn = cSys.referencePixel().copy();
//
      if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                      worldAxes, pixelAxes, worldMin, worldMax)) {
         throw(AipsError(String("toMix conversion failed because ")
               + cSys.errorMessage()));
      }
      if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
         throw(AipsError(String("Failed removal test 3a")));
      }
      if (pixelOut.nelements()!=0) {
         throw(AipsError(String("Failed removal test 3b")));
      }
   }
   {

// pr,p->w,w

      CoordinateSystem cSys;
      DirectionCoordinate dC = makeDirectionCoordinate(true);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
//
      Vector<double> pixelIn(cSys.nPixelAxes());
      Vector<double> worldIn(cSys.nWorldAxes());
      Vector<bool> pixelAxes(cSys.nPixelAxes());
      Vector<bool> worldAxes(cSys.nWorldAxes());
      Vector<double> worldOut, pixelOut;
      Vector<double> dRefVal = dC.referenceValue();   
//
      Vector<double> worldMin(cSys.nWorldAxes());
      Vector<double> worldMax(cSys.nWorldAxes());
      Vector<int32_t> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(false); pixelAxes(0) = true;
      worldAxes.set(false);
      worldIn = cSys.referenceValue().copy();
      pixelIn = cSys.referencePixel().copy();
//
      if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                      worldAxes, pixelAxes, worldMin, worldMax)) {
         throw(AipsError(String("toMix conversion failed because ")
               + cSys.errorMessage()));
      }
      if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
         throw(AipsError(String("Failed removal test 4a")));
      }
      if (pixelOut.nelements()!=1) {
         throw(AipsError(String("Failed removal test 4b")));
      }
      if (!near(pixelOut(0), cSys.referencePixel()(0), 1e-8)) {
         throw(AipsError(String("Failed removal test 4c")));
      } 
   }
   {

// pr,w->w,p

      CoordinateSystem cSys;
      DirectionCoordinate dC = makeDirectionCoordinate(true);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
//
      Vector<double> pixelIn(cSys.nPixelAxes());
      Vector<double> worldIn(cSys.nWorldAxes());
      Vector<bool> pixelAxes(cSys.nPixelAxes());
      Vector<bool> worldAxes(cSys.nWorldAxes());
      Vector<double> worldOut, pixelOut;
      Vector<double> dRefVal = dC.referenceValue();   
//
      Vector<double> worldMin(cSys.nWorldAxes());
      Vector<double> worldMax(cSys.nWorldAxes());
      Vector<int32_t> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(false); 
      worldAxes.set(false); worldAxes(1) = true;
      worldIn = cSys.referenceValue().copy();
      pixelIn = cSys.referencePixel().copy();
//
      if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                      worldAxes, pixelAxes, worldMin, worldMax)) {
         throw(AipsError(String("toMix conversion failed because ")
               + cSys.errorMessage()));
      }
      if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
         throw(AipsError(String("Failed removal test 5a")));
      }
      if (pixelOut.nelements()!=1) {
         throw(AipsError(String("Failed removal test 5b")));
      }
      if (!near(pixelOut(0), cSys.referencePixel()(0), 1e-8)) {
         throw(AipsError(String("Failed removal test 5c")));
      } 
   }
   {

// w,pr->p,w

      CoordinateSystem cSys;
      DirectionCoordinate dC = makeDirectionCoordinate(true);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(1, cSys.referencePixel()(1));
//
      Vector<double> pixelIn(cSys.nPixelAxes());
      Vector<double> worldIn(cSys.nWorldAxes());
      Vector<bool> pixelAxes(cSys.nPixelAxes());
      Vector<bool> worldAxes(cSys.nWorldAxes());
      Vector<double> worldOut, pixelOut;
      Vector<double> dRefVal = dC.referenceValue();   
//
      Vector<double> worldMin(cSys.nWorldAxes());
      Vector<double> worldMax(cSys.nWorldAxes());
      Vector<int32_t> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(false); 

      worldAxes.set(false); worldAxes(0) = true;
      worldIn = cSys.referenceValue().copy();
      pixelIn = cSys.referencePixel().copy();
//
      if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                      worldAxes, pixelAxes, worldMin, worldMax)) {
         throw(AipsError(String("toMix conversion failed because ")
               + cSys.errorMessage()));
      }
      if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
         throw(AipsError(String("Failed removal test 6a")));
      }
      if (pixelOut.nelements()!=1) {
         throw(AipsError(String("Failed removal test 6b")));
      }
      if (!near(pixelOut(0), cSys.referencePixel()(0), 1e-8)) {
         throw(AipsError(String("Failed removal test 6c")));
      } 
   }
   {

// p,pr->w,w

      CoordinateSystem cSys;
      DirectionCoordinate dC = makeDirectionCoordinate(true);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(1, cSys.referencePixel()(1));
//
      Vector<double> pixelIn(cSys.nPixelAxes());
      Vector<double> worldIn(cSys.nWorldAxes());
      Vector<bool> pixelAxes(cSys.nPixelAxes());
      Vector<bool> worldAxes(cSys.nWorldAxes());
      Vector<double> worldOut, pixelOut;
      Vector<double> dRefVal = dC.referenceValue();   
//
      Vector<double> worldMin(cSys.nWorldAxes());
      Vector<double> worldMax(cSys.nWorldAxes());
      Vector<int32_t> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = -180.0;
         worldMax(tmp(0)) = 180.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = -90.0;
         worldMax(tmp(1)) = 90.0;
      }
//
      pixelAxes.set(false); pixelAxes(0) = true;
      worldAxes.set(false);
      worldIn = cSys.referenceValue().copy();
      pixelIn = cSys.referencePixel().copy();
//
      if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                      worldAxes, pixelAxes, worldMin, worldMax)) {
         throw(AipsError(String("toMix conversion failed because ")
               + cSys.errorMessage()));
      }
      if (!allNear(worldOut, cSys.referenceValue(), 1e-8)) {
         throw(AipsError(String("Failed removal test 7a")));
      }
      if (pixelOut.nelements()!=1) {
         throw(AipsError(String("Failed removal test 7b")));
      }
      if (!near(pixelOut(0), cSys.referencePixel()(0), 1e-8)) {
         throw(AipsError(String("Failed removal test 7c")));
      } 
   }
}


DirectionCoordinate makeDirectionCoordinate(bool unitsAreDegrees,
                                            MDirection::Types type)
{
   Projection proj = Projection::SIN;
   Vector<double> crval(2);
   Vector<double> crpix(2);
   Vector<double> cdelt(2); 
   Matrix<double> xform(2,2);
//
   crval(0) = 0.1; crval(1) = 0.5;
   crpix(0) = 100.0; crpix(1) = 120.0;
   cdelt(0) = 1e-6; cdelt(1) = 2e-6;
   xform = 0.0;
   xform.diagonal() = 1.0;
   DirectionCoordinate dC(type, proj, crval(0), crval(1),
                          cdelt(0), cdelt(1),
                          xform, crpix(0), crpix(1), 999.0, 
                          999.0);
//
   if (unitsAreDegrees) {
      Vector<String> units(2);
      units(0) = "deg";
      units(1) = "deg";
      dC.setWorldAxisUnits(units); 
   }
//
   return dC;
}


SpectralCoordinate makeSpectralCoordinate ()
{
   MFrequency::Types type = MFrequency::TOPO;
   double f0 = 1.4e9;
   double finc = 4e6;
   double refchan = 10.5;
   double restFreq = 1.420405752E9;
//
   return SpectralCoordinate(type, f0, finc, refchan, restFreq);
}


StokesCoordinate makeStokesCoordinate(bool silly)

{
   if (silly) {
      Vector<int32_t> whichStokes(5);
      whichStokes(0) = Stokes::Q;
      whichStokes(1) = Stokes::RL;
      whichStokes(2) = Stokes::YY;
      whichStokes(3) = Stokes::I;
      whichStokes(4) = Stokes::LL;
//
      Vector<String> stokesStrings(5);
      stokesStrings(0) = "Q";
      stokesStrings(1) = "RL";
      stokesStrings(2) = "YY";
      stokesStrings(3) = "I";
      stokesStrings(4) = "LL";
/*
      Vector<int32_t> whichStokes(5);
      whichStokes(0) = Stokes::Q;
      whichStokes(1) = Stokes::LL;
      whichStokes(2) = Stokes::XX;
      whichStokes(3) = Stokes::I;
      whichStokes(4) = Stokes::XY;
//
      Vector<String> stokesStrings(5);
      stokesStrings(0) = "Q";
      stokesStrings(1) = "LL";
      stokesStrings(2) = "XX";
      stokesStrings(3) = "I";
      stokesStrings(4) = "XY";
//
      Vector<int32_t> whichStokes(2);
      whichStokes(0) = Stokes::I;
      whichStokes(1) = Stokes::V;
//
      Vector<String> stokesStrings(2);
      stokesStrings(0) = "I";
      stokesStrings(1) = "V";
*/
//
      return StokesCoordinate(whichStokes);
   } else {
      Vector<int32_t> whichStokes(4);
      whichStokes(0) = Stokes::I;
      whichStokes(1) = Stokes::Q;
      whichStokes(2) = Stokes::U;
      whichStokes(3) = Stokes::V;
//
      Vector<String> stokesStrings(4);
      stokesStrings(0) = "I";
      stokesStrings(1) = "Q";
      stokesStrings(2) = "U";
      stokesStrings(3) = "V";
//
      return StokesCoordinate(whichStokes);
   }
}
 
QualityCoordinate makeQualityCoordinate()
{
	Vector<int32_t> whichQuality(2);
	whichQuality(0) = Quality::DATA;
	whichQuality(1) = Quality::ERROR;

	//
	return QualityCoordinate(whichQuality);
}

LinearCoordinate makeLinearCoordinate (uint32_t nAxes)
{
   Vector<String> names(nAxes);
   Vector<String> units(nAxes);
   Vector<double> crpix(nAxes);
   Vector<double> crval(nAxes);
   Vector<double> cdelt(nAxes);
   Matrix<double> xform(nAxes,nAxes);
//
   for (uint32_t i=0; i<nAxes; i++) {
      ostringstream oss;
      oss << i;
      names(i) = String("axis") + String(oss);
      crpix(i) = 10.0 * (i + 1);
      cdelt(i) = (i+1);
      crval(i) = crpix(i) * 3.13;
   }
   xform = 0.0; xform.diagonal() = 1.0;
   units.set(String("s"));
   if (nAxes>1) units(1) = "rad";
   if (nAxes>2) units(2) = "kg";
//
   return LinearCoordinate(names, units, crval, cdelt,
                           xform, crpix);
}


TabularCoordinate makeTabularCoordinate()
{
   String axisName = "TabularDoggies";
   String axisUnit = "km";
   double crval = 10.12;
   double crpix = -128.32;
   double cdelt = 3.145;
//
   return TabularCoordinate(crval, cdelt, crpix, axisUnit, axisName);
}
 
CoordinateSystem makeCoordinateSystem(uint32_t& nCoords,
                                      Vector<int32_t>& types,
                                      Vector<String>& sTypes,
                                      uint32_t& iDC,
                                      uint32_t& iSpC,
                                      uint32_t& iTC,
                                      uint32_t& iStC,
                                      uint32_t& iQuC,
                                      uint32_t& iLC,
                                      DirectionCoordinate& dC,
                                      SpectralCoordinate& spC,
                                      TabularCoordinate& tC,
                                      StokesCoordinate& stC,
                                      QualityCoordinate& quC,
                                      LinearCoordinate& lC)
{
   CoordinateSystem cSys;
   dC  = makeDirectionCoordinate();
   spC = makeSpectralCoordinate();
   tC  = makeTabularCoordinate();
   stC = makeStokesCoordinate();
   quC = makeQualityCoordinate();
   lC  = makeLinearCoordinate();
   cSys.addCoordinate(dC);
   cSys.addCoordinate(spC);
   cSys.addCoordinate(tC);
   cSys.addCoordinate(stC);
   cSys.addCoordinate(quC);
   cSys.addCoordinate(lC);
   iDC  = 0;
   iSpC = 1;
   iTC  = 2;
   iStC = 3;
   iQuC = 4;
   iLC  = 5;
   nCoords = 6;
   types.resize(6);
   types(0) = Coordinate::DIRECTION;
   types(1) = Coordinate::SPECTRAL;
   types(2) = Coordinate::TABULAR;
   types(3) = Coordinate::STOKES;
   types(4) = Coordinate::QUALITY;
   types(5) = Coordinate::LINEAR;
   sTypes.resize(6);
   sTypes(0) = "Direction";
   sTypes(1) = "Spectral";
   sTypes(2) = "Tabular";
   sTypes(3) = "Stokes";
   sTypes(4) = "Quality";
   sTypes(5) = "Linear";
   return cSys;
}



void doit6 ()
{
   CoordinateSystem cSys;
   SpectralCoordinate spC = makeSpectralCoordinate();     // 0
   cSys.addCoordinate(spC);
   DirectionCoordinate dC = makeDirectionCoordinate();    // 1 & 2
   cSys.addCoordinate(dC);
   Coordinate* pC = 0;
//
   Vector<bool> axes(cSys.nPixelAxes(), false);
   Vector<int32_t> shape(cSys.nPixelAxes(), 0);
   shape(0) = 64;
   shape(1) = 128;
   shape(2) = 256;

// Induced failures

   {

// No axes

      bool failed = false;
      try {
         pC = cSys.makeFourierCoordinate (axes, shape);
      } catch (std::exception& x) {
        failed = true;
      } 
      if (!failed) {
         throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
      }
      delete pC;
   }

   {

// Illegal axes

      bool failed = false;
      Vector<bool> axes2(20, true);
      try {
         pC = cSys.makeFourierCoordinate (axes2, shape);
      } catch (std::exception& x) {
        failed = true;
      } 
      if (!failed) {
         throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
      }
      delete pC;
   }

   {

// Illegal shape

      bool failed = false;
      Vector<int32_t> shape2(20, 100);
      try {
         pC = cSys.makeFourierCoordinate (axes, shape2);
      } catch (std::exception& x) {
        failed = true;
      } 
      if (!failed) {
         throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
      }
      delete pC;
   }

// These should work.   All the underlying coordinates have been 
// tested, so just make sure the right coordinate has been replaced

   {
      axes.set(false);
      axes(0) = true;
      pC = cSys.makeFourierCoordinate (axes, shape);
//
      Vector<String> units2 = pC->worldAxisUnits();
      Vector<String> names2 = pC->worldAxisNames();
      for (uint32_t i=0; i<cSys.nPixelAxes(); i++) {
        if (i==0) {
           if (units2(i)!=String("s")) {
              throw(AipsError("makeFourierCoordinate (1) failed units test"));
           }
           if (names2(i)!=String("Time")) {
              throw(AipsError("makeFourierCoordinate (1) failed names test"));
           }
        } else {
            if (units2(i)!=cSys.worldAxisUnits()(i)) {
               throw(AipsError("makeFourierCoordinate (1) failed units test"));
            }
            if (names2(i)!=cSys.worldAxisNames()(i)) {
               throw(AipsError("makeFourierCoordinate (1) failed names test"));
            }
        }
      }
      delete pC;
   }

   {
      axes.set(true);
      pC = cSys.makeFourierCoordinate (axes, shape);
//
      Vector<String> units2 = pC->worldAxisUnits();
      Vector<String> names2 = pC->worldAxisNames();
      if (units2(0)!=String("s") || units2(1)!=String("lambda") ||
          units2(2)!=String("lambda")) {
         throw(AipsError("makeFourierCoordinate (2) failed units test"));
      }
      if (names2(0)!=String("Time") || names2(1)!=String("UU") ||
          names2(2)!=String("VV")) {
         throw(AipsError("makeFourierCoordinate (2) failed names test"));
      }
      delete pC;
   }

}
   void verifyCAS3264() {
	   cout << __FUNCTION__ << endl;
	   CoordinateSystem cSys;
	   SpectralCoordinate spC = makeSpectralCoordinate();     // 0
	   cSys.addCoordinate(spC);
	   DirectionCoordinate dC = makeDirectionCoordinate();    // 1 & 2
	   cSys.addCoordinate(dC);
	   cout << cSys.nPixelAxes();
	   Vector<double> ftRef(3);
	   Vector<double> ref(3);
	   ref(0)= 22.5;
	   ref(1)= 18.2;
	   ref(2) = 31.48;
	   cerr << "Utils: ref = " << ref << endl;
	   cSys.setReferencePixel(ref);
	   cerr << "Utils coords.refpix: " << cSys.referencePixel() << endl;
   }

   void spectralAxisNumber() {
	   cout << "*** test spectralAxisNumber()" << endl;
	   CoordinateSystem csys = CoordinateUtil::defaultCoords4D();
	   AlwaysAssert(csys.spectralAxisNumber(false) == 3, AipsError);
	   AlwaysAssert(csys.spectralAxisNumber(true) == 3, AipsError);
	   Vector<int32_t> worldOrder(4);
	   worldOrder[0] = 3;
	   worldOrder[1] = 2;
	   worldOrder[2] = 1;
	   worldOrder[3] = 0;
	   Vector<int32_t> pixelOrder(4);
	   pixelOrder[0] = 1;
	   pixelOrder[1] = 2;
	   pixelOrder[2] = 3;
	   pixelOrder[3] = 0;
	   csys.transpose(worldOrder, pixelOrder);
	   AlwaysAssert(csys.spectralAxisNumber(false) == 2, AipsError);
	   AlwaysAssert(csys.spectralAxisNumber(true) == 0, AipsError);

	   csys.removePixelAxis(2, 0);
	   AlwaysAssert(csys.spectralAxisNumber(false) == -1, AipsError);
	   AlwaysAssert(csys.spectralAxisNumber(true) == 0, AipsError);

	   csys.replaceCoordinate(LinearCoordinate(), 2);
	   AlwaysAssert(csys.spectralAxisNumber(false) == -1, AipsError);
	   AlwaysAssert(csys.spectralAxisNumber(true) == -1, AipsError);

   }

   void polarizationAxisNumber() {
	   cout << "*** test polarizationAxisNumber()" << endl;

  	   CoordinateSystem csys = CoordinateUtil::defaultCoords4D();
  	   AlwaysAssert(csys.polarizationAxisNumber(false) == 2, AipsError);
  	   AlwaysAssert(csys.polarizationAxisNumber(true) == 2, AipsError);
  	   Vector<int32_t> worldOrder(4);
  	   worldOrder[0] = 3;
  	   worldOrder[1] = 2;
  	   worldOrder[2] = 1;
  	   worldOrder[3] = 0;
  	   Vector<int32_t> pixelOrder(4);
  	   pixelOrder[0] = 1;
  	   pixelOrder[1] = 0;
  	   pixelOrder[2] = 3;
  	   pixelOrder[3] = 2;
  	   csys.transpose(worldOrder, pixelOrder);
  	   AlwaysAssert(csys.polarizationAxisNumber(false) == 3, AipsError);
  	   AlwaysAssert(csys.polarizationAxisNumber(true) == 1, AipsError);

  	   csys.removePixelAxis(3, 0);
  	   AlwaysAssert(csys.polarizationAxisNumber(false) == -1, AipsError);
  	   AlwaysAssert(csys.polarizationAxisNumber(true) == 1, AipsError);

  	   csys.replaceCoordinate(LinearCoordinate(), 1);
  	   AlwaysAssert(csys.polarizationAxisNumber(false) == -1, AipsError);
  	   AlwaysAssert(csys.polarizationAxisNumber(true) == -1, AipsError);

     }

