//# tDirectionCoordinate.cc: Test program for DirectionCoordinate
//# Copyright (C) 1998
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
//#

 
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/Math.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/Projection.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableRecord.h>

#include <iostream.h>

DirectionCoordinate makeCoordinate(MDirection::Types type,
                                   Projection& proj,
                                   Vector<Double>& crval,
                                   Vector<Double>& crpix,
                                   Vector<Double>& cdelt,
                                   Matrix<Double>& xform);

void doit (DirectionCoordinate& lc, 
           MDirection::Types type,
           const Vector<String>& axisName,
           const String& axisUnit);

void doit2 (DirectionCoordinate& lc, 
            Vector<Double>& crval,
            Vector<Double>& crpix,
            Vector<Double>& cdelt,
            Matrix<Double>& xform);

void doit3 (DirectionCoordinate& lc);


int main()
{
   try {

      MDirection::Types type;
      Projection proj;
      Vector<Double> crval, crpix, cdelt;
      Matrix<Double> xform;


// Constructors

      {
         DirectionCoordinate lc;
      }
      {
         DirectionCoordinate lc = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
      }

// Test near function

     {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         DirectionCoordinate lc2 = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix, 
                                                 cdelt, xform);
         if (!lc.near(&lc2)) {
            throw(AipsError("Failed near test 1"));
         }
         Vector<Int> excludeAxes(1, 0);
         if (!lc.near(&lc2, excludeAxes)) {
            throw(AipsError("Failed near test 2"));
         }
     } 

// Test the rest

      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         Vector<String> axisNames(2); 
         axisNames(0) = "Right Ascension";
         axisNames(1) = "Declination";
         String axisUnit = "rad";
         doit(lc, MDirection::J2000, axisNames, axisUnit);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit2(lc, crval, crpix, cdelt, xform);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit3(lc);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::GALACTIC,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         Vector<String> axisNames(2); 
         axisNames(0) = "Galactic Longitude";
         axisNames(1) = "Galactic Latitude";
         String axisUnit = "rad";
         doit(lc, MDirection::GALACTIC, axisNames, axisUnit);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::GALACTIC,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit2(lc, crval, crpix, cdelt, xform);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::GALACTIC,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit3(lc);
      }


  } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}


DirectionCoordinate makeCoordinate(MDirection::Types type,
                                   Projection& proj,
                                   Vector<Double>& crval, 
                                   Vector<Double>& crpix, 
                                   Vector<Double>& cdelt, 
                                   Matrix<Double>& xform)
{
   crval.resize(2);
   crpix.resize(2);
   cdelt.resize(2);
   crval(0) = 0.1; crval(1) = 0.5;
   crpix(0) = 100.0; crpix(1) = 120.0;
   cdelt(0) = 1e-6; cdelt(1) = 2e-6;
   xform.resize(2,2);
   xform = 0.0;
   xform.diagonal() = 1.0;
   return DirectionCoordinate(type, proj, crval(0), crval(1),
                              cdelt(0), cdelt(1),
                              xform, crpix(0), crpix(1));
}
 


void doit (DirectionCoordinate& lc,
           MDirection::Types type,
           const Vector<String>& axisNames,
           const String& axisUnit)
{

// Test copy constructor

   {
       DirectionCoordinate lc2(lc);
       if (!lc.near(&lc2)) {
          throw(AipsError("Failed copy constructor test"));
       }
   } 

// Test assignment

   {
       DirectionCoordinate lc2;
       lc2 = lc;
       if (!lc.near(&lc2)) {
          throw(AipsError("Failed assignment test"));
       }
   } 

// Test member functions
  
   if (lc.type() != Coordinate::DIRECTION) {
      throw(AipsError("Failed type test"));
   }
   if (lc.directionType() != type) {
      throw(AipsError("Failed directionType test"));
   }
   if (lc.showType() != "Direction") {
      throw(AipsError("Failed showType test"));
   }
//
   if (lc.nPixelAxes() != 2) {
      throw(AipsError("Failed nPixelAxes test"));
   }
//
   if (lc.nWorldAxes() != 2) {
      throw(AipsError("Failed nWorldAxes test"));
   }
//
   if (!allEQ(axisNames.ac(), lc.worldAxisNames().ac())) {
      throw(AipsError("Failed world axis name recovery test"));
   }
   Vector<String> names(axisNames.copy());
   names(0) = "Horsies";
   if (!lc.setWorldAxisNames(names)) {
      throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
   }
   if (!allEQ(names.ac(), lc.worldAxisNames().ac())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
   Vector<String> units(2); units(0) = axisUnit; units(1) = axisUnit;
   if (!allEQ(units.ac(), lc.worldAxisUnits().ac())) {
      throw(AipsError("Failed world axis units recovery test"));
   }
   units(0) = "deg";
   if (!lc.setWorldAxisUnits(units)) {
      throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
   }
   if (!allEQ(units.ac(), lc.worldAxisUnits().ac())) {
      throw(AipsError("Failed world axis units set/recovery test"));
   }
//
// Test record saving
//
   TableRecord rec;
   if (!lc.save(rec, "Direction")) {
      throw(AipsError("Coordinate saving to Record failed"));  
   }  
   DirectionCoordinate* plc = DirectionCoordinate::restore(rec, "Direction");
   if (!plc->near(&lc, 1e-6)) {
      throw(AipsError("Coordinate reflection through record interface failed"));  
   }
   delete plc;

//
// Test clone
//
   Coordinate* plc2 = lc.clone();
   if (!plc2->near(&lc, 1e-6)) {
      throw(AipsError("Clone function failed"));  
   }
   delete plc2;
}


void doit2 (DirectionCoordinate& lc,
            Vector<Double>& crval, 
            Vector<Double>& crpix, 
            Vector<Double>& cdelt, 
            Matrix<Double>& xform)
{
//
   if (!allEQ(crval.ac(), lc.referenceValue().ac())) {
      throw(AipsError("Failed reference value recovery test"));
   }
//
   if (!allEQ(cdelt.ac(), lc.increment().ac())) {
      throw(AipsError("Failed increment recovery test"));
   }
//
   if (!allEQ(crpix.ac(), lc.referencePixel().ac())) {
      throw(AipsError("Failed reference pixel recovery test"));
   }
//
   if (!allEQ(xform.ac(), lc.linearTransform().ac())) {
      throw(AipsError("Failed Direction transform recovery test"));
   }
//
   crval(0) = 111.1;
   if (!lc.setReferenceValue(crval)) {
      throw(AipsError(String("Failed to set reference value because") + lc.errorMessage()));
   }
   if (!allEQ(crval.ac(), lc.referenceValue().ac())) {
      throw(AipsError("Failed reference value set/recovery test"));
   }
//
   cdelt(0) = -10.3;
   if (!lc.setIncrement(cdelt)) {
      throw(AipsError(String("Failed to set increment because") + lc.errorMessage()));
   }
   if (!allEQ(cdelt.ac(), lc.increment().ac())) {
      throw(AipsError("Failed increment set/recovery test"));
   }
//
   crpix(0) = 23.0;
   if (!lc.setReferencePixel(crpix)) {
      throw(AipsError(String("Failed to set reference pixel because") + lc.errorMessage()));
   }
   if (!allEQ(crpix.ac(), lc.referencePixel().ac())) {
      throw(AipsError("Failed reference pixel set/recovery test"));
  }
//       
   xform.diagonal() = -2.0;
   if (!lc.setLinearTransform(xform)) {
      throw(AipsError(String("Failed to set linear transform because") + lc.errorMessage()));
   }
   if (!allEQ(xform.ac(), lc.linearTransform().ac())) {
      throw(AipsError("Failed linear transform set/recovery test");)
   }
}

void doit3 (DirectionCoordinate& lc)
{
//
// Test conversion
//
   Vector<Double> pixel(2), world(2);
   pixel(0) = 12.2;
   pixel(1) = -22.2;
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
   }
//
   Vector<Double> pixel2(2);
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2.ac(), pixel.ac(), 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 1 failed"));
   }
//
   MDirection dir;
   if (!lc.toWorld(dir, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(dir.getAngle().getValue().ac(), world.ac(), 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 2 failed"));
   }
//
// Formatting
//
   Int prec;
   Coordinate::formatType fType = Coordinate::SCIENTIFIC;
   lc.getPrecision(prec, fType, True, 6, 4, 2);
   if (prec != 6) {
      throw(AipsError("Failed getPrecision test 1"));
   }
   fType = Coordinate::FIXED;
   lc.getPrecision(prec, fType, True, 6, 4, 2);
   if (prec != 4) {
      throw(AipsError("Failed getPrecision test 2"));
   }
//
   String unit;
   Double val = 0.12343;
   String str = lc.format(unit, Coordinate::FIXED, val, 0,
             True, 4);
   if (unit=="rad") {
      if (str != "0.1234") {
         throw(AipsError("Failed format test 1"));
      }
   } else if (unit=="deg") {
      if (str != "7.0720") {
         throw(AipsError("Failed format test 1"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
//
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 0,
             True, 4);
   if (unit=="rad") {
      if (str != "1.2343e-01") {
         throw(AipsError("Failed format test 2"));
      }
   } else if (unit=="deg") {
      if (str != "7.0720e+00") {
         throw(AipsError("Failed format test 2"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
//
   str = lc.format(unit, Coordinate::FIXED, val, 1,
             True, 4);
   if (unit=="rad") {
      if (str != "0.1234") {
         throw(AipsError("Failed format test 3"));
      }
   } else if (unit=="deg") {
      if (str != "7.0720") {
         throw(AipsError("Failed format test 3"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
//
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 1,
             True, 4);
   if (unit=="rad") {
      if (str != "1.2343e-01") {
         throw(AipsError("Failed format test 4"));
      }
   } else if (unit=="deg") {
      if (str != "7.0720e+00") {
         throw(AipsError("Failed format test 4"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
//
// Fairly ugly way to work out what kind of MDirection
// we have in the DC.  Need to know this to figure out what
// the formatting is going to do !
//
   str = lc.format(unit, Coordinate::TIME, val, 0,
             True, 4);
   MDirection::GlobalTypes globalType = dir.globalType(lc.directionType());
   if (globalType==MDirection::GRADEC) {
      if (str != "00:28:17.2843") {
         throw(AipsError("Failed format test 5"));
      }
   } else if (globalType==MDirection::GLONGLAT) {
      if (str != "+007.04.19.2650") {
         throw(AipsError("Failed format test 5"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
//
   str = lc.format(unit, Coordinate::TIME, val, 1,
             True, 4);
   if (globalType==MDirection::GRADEC) {
      if (str != "+07.04.19.2650") {
         throw(AipsError("Failed format test 6"));
      }
   } else if (globalType==MDirection::GLONGLAT) {
      if (str != "+007.04.19.2650") {
         throw(AipsError("Failed format test 6"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
}   


