//# tDirectionCoordinate.cc: Test program for DirectionCoordinate
//# Copyright (C) 1998,1999,2000
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
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/MVDirection.h>
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
void doit4 (DirectionCoordinate& lc);
void doit5 (DirectionCoordinate& lc);


int main()
{
   try {

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
         if (!lc.near(lc2)) {
            throw(AipsError(String("Failed near test 1 because") + lc.errorMessage()));
         }
         Vector<Int> excludeAxes(1, 0);
         if (!lc.near(lc2, excludeAxes)) {
            throw(AipsError(String("Failed near test 2 because") + lc.errorMessage()));
         }
     } 

// Test Quantum constructor interface

     {
        Matrix<Double> xform(2,2);
        xform = 0.0;
        xform.diagonal() = 1.0;
        Projection proj(Projection::SIN);
        MDirection::Types type(MDirection::B1950);
//
        Vector<Double> crval(2);
        Vector<Double> crpix(2);
        Vector<Double> cdelt(2);
//
        crval(0) = 0.1; crval(1) = 0.5;
        crpix(0) = 100.0; crpix(1) = 120.0;
        cdelt(0) = 1e-6; cdelt(1) = 2e-6;
//
        DirectionCoordinate dc1(type, proj, crval(0), crval(1),
                                cdelt(0), cdelt(1),
                                xform, crpix(0), crpix(1));
//
        Quantum<Double> lon(crval(0)*180.0/C::pi, "deg");
        Quantum<Double> lat(crval(1)*180.0/C::pi, "deg");
        Quantum<Double> dlon(cdelt(0)*180.0/C::pi, "deg");
        Quantum<Double> dlat(cdelt(1)*180.0/C::pi, "deg");
//
        DirectionCoordinate dc2(type, proj, lon, lat, dlon, dlat,
                                xform, crpix(0), crpix(1));
//
        if (!dc1.near(dc2)) {
           throw(AipsError(String("Quantum interface constructor failed consistency test")));
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
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit4(lc);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit5(lc);
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
   proj = Projection::SIN;
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
       if (!lc.near(lc2)) {
          throw(AipsError("Failed copy constructor test"));
       }
   } 

// Test assignment

   {
       DirectionCoordinate lc2;
       lc2 = lc;
       if (!lc.near(lc2)) {
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
   if (!allEQ(axisNames, lc.worldAxisNames())) {
      throw(AipsError("Failed world axis name recovery test"));
   }
   Vector<String> names(axisNames.copy());
   names(0) = "Horsies";
   if (!lc.setWorldAxisNames(names)) {
      throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
   }
   if (!allEQ(names, lc.worldAxisNames())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
   Vector<String> units(2); units(0) = axisUnit; units(1) = axisUnit;
   if (!allEQ(units, lc.worldAxisUnits())) {
      throw(AipsError("Failed world axis units recovery test"));
   }
   units(0) = "deg";
   if (!lc.setWorldAxisUnits(units)) {
      throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
   }
   if (!allEQ(units, lc.worldAxisUnits())) {
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
   if (!plc->near(lc, 1e-6)) {
      throw(AipsError("Coordinate reflection through record interface failed"));  
   }
   delete plc;

//
// Test clone
//
   Coordinate* plc2 = lc.clone();
   if (!plc2->near(lc, 1e-6)) {
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
   if (!allEQ(crval, lc.referenceValue())) {
      throw(AipsError("Failed reference value recovery test"));
   }
//
   if (!allEQ(cdelt, lc.increment())) {
      throw(AipsError("Failed increment recovery test"));
   }
//
   if (!allEQ(crpix, lc.referencePixel())) {
      throw(AipsError("Failed reference pixel recovery test"));
   }
//
   if (!allEQ(xform, lc.linearTransform())) {
      throw(AipsError("Failed Direction transform recovery test"));
   }
//
   crval(0) = 111.1;
   if (!lc.setReferenceValue(crval)) {
      throw(AipsError(String("Failed to set reference value because") + lc.errorMessage()));
   }
   if (!allEQ(crval, lc.referenceValue())) {
      throw(AipsError("Failed reference value set/recovery test"));
   }
//
   cdelt(0) = -10.3;
   if (!lc.setIncrement(cdelt)) {
      throw(AipsError(String("Failed to set increment because") + lc.errorMessage()));
   }
   if (!allEQ(cdelt, lc.increment())) {
      throw(AipsError("Failed increment set/recovery test"));
   }
//
   crpix(0) = 23.0;
   if (!lc.setReferencePixel(crpix)) {
      throw(AipsError(String("Failed to set reference pixel because") + lc.errorMessage()));
   }
   if (!allEQ(crpix, lc.referencePixel())) {
      throw(AipsError("Failed reference pixel set/recovery test"));
  }
//       
   xform.diagonal() = -2.0;
   if (!lc.setLinearTransform(xform)) {
      throw(AipsError(String("Failed to set linear transform because") + lc.errorMessage()));
   }
   if (!allEQ(xform, lc.linearTransform())) {
      throw(AipsError("Failed linear transform set/recovery test"));
   }
}

void doit3 (DirectionCoordinate& lc)
{
//
// Test conversion
//
   Vector<Double> pixel(2), world(2);
   Vector<String> axisUnits = lc.worldAxisUnits();
   axisUnits.set("rad");
   if (!lc.setWorldAxisUnits(axisUnits)) {
      throw(AipsError(String("failed to set world axis units to radians because ") + lc.errorMessage())); 
   }
//
   pixel(0) = 12.2;
   pixel(1) = -22.2;
//
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion (1) failed because ") + lc.errorMessage()));
   }
   Vector<Double> pixel2(2);
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion (1) failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2, pixel, 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 1 failed"));
   }
//
   MVDirection dirV;
   if (!lc.toWorld(dirV, pixel)) {
      throw(AipsError(String("toWorld conversion (3) failed because ") + lc.errorMessage())); 
   }
   axisUnits.set("deg");
   if (!lc.setWorldAxisUnits(axisUnits)) {
      throw(AipsError(String("failed to set world axis units to degrees because ") + lc.errorMessage())); 
   }
   if (!allNear(dirV.get(), world, 1e-6)) {
         throw(AipsError("Coordinate conversion values (MVDirection) are wrong"));
   }
   Vector<Double> pixel3;
   if (!lc.toPixel(pixel3, dirV)) {
      throw(AipsError(String("toPixel conversion (3) failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel3, pixel, 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 3 failed"));
   }
//
   MDirection dir;
   if (!lc.toWorld(dir, pixel)) {
      throw(AipsError(String("toWorld conversion (2) failed because ") + lc.errorMessage())); 
   }
   if (!allNear(dir.getValue().get(), world, 1e-6)) {
         throw(AipsError("Coordinate conversion values (MDirection) are wrong"));
   }
   axisUnits.set("rad");
   if (!lc.setWorldAxisUnits(axisUnits)) {
      throw(AipsError(String("failed to set world axis units to radians because ") + lc.errorMessage())); 
   }
   if (!lc.toPixel(pixel3, dir)) {
      throw(AipsError(String("toPixel conversion (2) failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel3, pixel, 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 2 failed"));
   }
//
// Formatting.  
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
   Quantum<Double> valq(0.12343, "rad");
   valq.convert(Unit("deg"));
//
   String str = lc.format(unit, Coordinate::FIXED, val, 0,
                          True, 4, True);
   if (unit!="rad" || str != "0.1234") {
      throw(AipsError("Failed format test 1a"));
   }
   uInt axis = 0;
   Int prec2 = 4;
   str = lc.formatQuantity(unit, Coordinate::FIXED, valq, axis,
                   True, prec2, True);
   if (unit!="rad" || str != "0.1234") {
      throw(AipsError("Failed format test 1b"));
   }
//
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 0,
                   True, 4, True);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 2a"));
   }
   str = lc.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, 0,
                     True, 4, True);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 2b"));
   }
//
   str = lc.format(unit, Coordinate::FIXED, val, 1,
                   True, 4, True);
   if (unit!="rad" || str != "0.1234") {
      throw(AipsError("Failed format test 3a"));
   }
   str = lc.formatQuantity(unit, Coordinate::FIXED, valq, 1,
                     True, 4, True);
   if (str != "0.1234") {
      throw(AipsError("Failed format test 3b"));
   }
//
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 1,
                   True, 4, True);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 4a"));
   }
   str = lc.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, 1,
                     True, 4, True);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 4b"));
   }
//
// Fairly ugly way to work out what kind of MDirection
// we have in the DC.  Need to know this to figure out what
// the formatting is going to do !
//
   str = lc.format(unit, Coordinate::TIME, val, 0,
                   True, 4);
   String str2 = lc.formatQuantity(unit, Coordinate::TIME, valq, 0,
                             True, 4);
   MDirection::GlobalTypes globalType = dir.globalType(lc.directionType());
   if (globalType==MDirection::GRADEC) {
      if (str != "00:28:17.2843" || str2 != "00:28:17.2843") {
         throw(AipsError("Failed format test 5a"));
      }
   } else if (globalType==MDirection::GLONGLAT) {
      if (str != "+007.04.19.2650" || str2 != "+007.04.19.2650") {
         throw(AipsError("Failed format test 5b"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
//
   str = lc.format(unit, Coordinate::TIME, val, 1,
                   True, 4);
   str2 = lc.formatQuantity(unit, Coordinate::TIME, valq, 1,
                      True, 4);
   if (globalType==MDirection::GRADEC) {
      if (str != "+07.04.19.2650" || str2 != "+07.04.19.2650") {
         throw(AipsError("Failed format test 6a"));
      }
   } else if (globalType==MDirection::GLONGLAT) {
      if (str != "+07.04.19.2650" || str2 != "+07.04.19.2650") {
         throw(AipsError("Failed format test 6b"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
}   


void doit4 (DirectionCoordinate& dC)
//
// Mixed conversions 
//
{
   Vector<Double> pixelIn(2), worldIn(2), pixelOut, worldOut;
   Vector<Bool> pixelAxes(2), worldAxes(2);
   Vector<Double> minWorld(2), maxWorld(2);
   Vector<String> units = dC.worldAxisUnits();
   Vector<Double> refVal = dC.referenceValue();
//
   Quantum<Double> off(10,Unit("deg"));
   off.convert(Unit(units(0)));
   Double tmp = refVal(0) - off.getValue();
   minWorld(0) = tmp;
   tmp = refVal(0) + off.getValue();
   maxWorld(0) = tmp;
//
   off.convert(Unit(units(1)));
   tmp = refVal(1) - off.getValue();
   minWorld(1) = tmp;
   tmp = refVal(1) + off.getValue();
   maxWorld(1) = tmp;
//
// Forced errors
//
   pixelAxes.set(False);
   worldAxes.set(False);
   if (dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Forced fail 1 of toMix did not occur")));
   }
   pixelAxes(0) = True;
   if (dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Forced fail 2 of toMix did not occur")));
   }
//
// pixel,pixel->world,world
//
   pixelAxes.set(True);
   worldAxes.set(False);
   pixelIn = dC.referencePixel().copy();
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Failed pixel->world conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-8)) {
      throw(AipsError(String("Failed pixel->world consistency test")));
   }
   if (!allNear(worldOut, dC.referenceValue(), 1e-8)) {
      throw(AipsError(String("Failed pixel->world consistency test")));
   }
//
// world,world->pixel,pixel
//
   pixelAxes.set(False);
   worldAxes.set(True);
   worldIn = dC.referenceValue().copy();
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Failed world->pixel conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test")));
   }
   if (!allNear(worldOut, dC.referenceValue(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test")));
   }
//
// world,pixel -> pixel,world
//
   worldIn(0) = dC.referenceValue()(0);
   pixelIn(1) = dC.referencePixel()(1);
   pixelAxes.set(False);
   worldAxes.set(False);
   worldAxes(0) = True;
   pixelAxes(1) = True;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test")));
   }
   if (!allNear(worldOut, dC.referenceValue(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test")));
   }
//
// pixel, world -> world, pixel
//
   pixelIn(0) = dC.referencePixel()(0);
   worldIn(1) = dC.referenceValue()(1);
   pixelAxes.set(False);
   worldAxes.set(False);
   worldAxes(1) = True;
   pixelAxes(0) = True;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test")));
   }
   if (!allNear(worldOut, dC.referenceValue(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test")));
   }
//
// world,pixel -> pixel,world -> world,pixel
// Do it off the reference values
//
   worldIn(0) = dC.referenceValue()(0) + 5*dC.increment()(0);
   pixelIn(1) = 2.32;
   pixelAxes.set(False); pixelAxes(1) = True;
   worldAxes.set(False); worldAxes(0) = True;
   Vector<Double> saveWorldIn(worldIn.copy());
   Vector<Double> savePixelIn(pixelIn.copy());
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   pixelIn(0) = pixelOut(0);
   worldIn(1) = worldOut(1);
   pixelAxes.set(False); pixelAxes(0) = True;
   worldAxes.set(False); worldAxes(1) = True;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!near(worldOut(0), saveWorldIn(0), 1e-8)) {
      throw(AipsError("Failed mixed conversion reflection test 1"));
   }
   if (!near(pixelOut(1), savePixelIn(1), 1e-8)) {
      throw(AipsError("Failed mixed conversion reflection test 1"));
   }
//
// pixel,world -> world,pixel -> pixel, world
// Do it off the reference values
//
   worldIn(1) = dC.referenceValue()(1) + 5*dC.increment()(1);
   pixelIn(0) = 2.32;
   pixelAxes.set(False); pixelAxes(0) = True;
   worldAxes.set(False); worldAxes(1) = True;
   saveWorldIn = worldIn.copy();
   savePixelIn = pixelIn.copy();
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   pixelIn(1) = pixelOut(1);
   worldIn(0) = worldOut(0);
   pixelAxes.set(False); pixelAxes(1) = True;
   worldAxes.set(False); worldAxes(0) = True;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!near(worldOut(1), saveWorldIn(1), 1e-8)) {
      throw(AipsError("Failed mixed conversion reflection test 1"));
   }
   if (!near(pixelOut(0), savePixelIn(0), 1e-8)) {
      throw(AipsError("Failed mixed conversion reflection test 1"));
   }
}

void doit5 (DirectionCoordinate& dC)
//
// Fourier coordinate. LinearXform has been tested
// pretty hard (it does the work for DirectionCoordinate) so don't fuss about with 
// the values much
//
{
   Vector<Bool> axes(2, True);
   Vector<Int> shape(2);
   shape(0) = 128;
   shape(1) = 256;

// All axes

   {
      Coordinate* pC = dC.makeFourierCoordinate (axes, shape);
//
      Vector<String> units2 = pC->worldAxisUnits();
      Vector<String> names2 = pC->worldAxisNames();
      Vector<Double> crval2 = pC->referenceValue();
      Vector<Double> crpix2 = pC->referencePixel();
      if (units2(0)!=String("lambda") || units2(1)!=String("lambda")) {
         throw(AipsError("makeFourierCoordinate (1) failed units test"));
      }
      if (names2(0)!=String("UU") || names2(1)!=String("VV")) {
         throw(AipsError("makeFourierCoordinate (1) failed names test"));
      }
      if (!allNear(crval2,0.0,1e-13)) {
         throw(AipsError("makeFourierCoordinate (1) failed crval test"));
      }
      for (uInt i=0; i<pC->nPixelAxes(); i++) { 
         if (!near(Double(Int(shape(i)/2)), crpix2(i))) {
            throw(AipsError("makeFourierCoordinate (1) failed crpix test"));
         }
      }
      delete pC;
   }

// Not all axes 

   {
      axes.set(True);
      axes(1) = False;
      Bool failed = False;
      Coordinate* pC = 0;
      try {
         pC = dC.makeFourierCoordinate (axes, shape);
      } catch (AipsError x) {
         failed = True;
      } end_try;
      if (!failed) {
         throw(AipsError("Failed to induce forced error in makeFourierCoordinate (2)"));
      }
      delete pC;
   }
}
