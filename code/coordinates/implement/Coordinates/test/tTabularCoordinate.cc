//# tTabularCoordinate.cc: Test program for TabularCoordinate
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
#include <trial/Coordinates/TabularCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableRecord.h>

#include <iostream.h>

TabularCoordinate makeLinearCoordinate(String& axisName,
                                       String& axisUnit,
                                       Double& crval,
                                       Double& crpix,
                                       Double& cdelt);
 
TabularCoordinate makeNonLinearCoordinate (String& axisName,
                                           String& axisUnit,
                                           Vector<Double>& pixelValues,
                                           Vector<Double>& worldValues);

void doit (TabularCoordinate& lc, 
           const String& axisName,
           const String& axisUnit);

void doitLinear (const Double refVal,
                 const Double refPix,
                 const Double incr,
                 const Double linTrans,
                 TabularCoordinate& lc);

void doitNonLinear (const Vector<Double>& pixelValues,
                    const Vector<Double>& worldValues,
                    TabularCoordinate& lc);


int main()
{
   try {

      String axisName, axisUnit;
      Double crpix, crval, cdelt;
      Matrix<Double> xform;
      Vector<Double> pixelValues;
      Vector<Double> worldValues;

// Constructors

      {
         TabularCoordinate lc;
      }
      {
         TabularCoordinate lc = makeLinearCoordinate(axisName, axisUnit, crval, crpix, cdelt);
      }
      {
         TabularCoordinate lc = makeNonLinearCoordinate(axisName, axisUnit, pixelValues, worldValues);
      }


// Test near function

     {
         TabularCoordinate lc  = makeLinearCoordinate(axisName, axisUnit, crval, crpix, cdelt);
         TabularCoordinate lc2 = makeLinearCoordinate(axisName, axisUnit, crval, crpix, cdelt);
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
         TabularCoordinate lc = makeLinearCoordinate(axisName, axisUnit, crval, crpix, cdelt);
         doit(lc, axisName, axisUnit);
      }
      {
         TabularCoordinate lc = makeLinearCoordinate(axisName, axisUnit, crval, crpix, cdelt);
         doitLinear(crval, crpix, cdelt, 1.0, lc);
      }
      {
         TabularCoordinate lc = makeNonLinearCoordinate(axisName, axisUnit, pixelValues, worldValues);
         doit(lc, axisName, axisUnit);
      }
      {
         TabularCoordinate lc = makeNonLinearCoordinate(axisName, axisUnit, pixelValues, worldValues);
         doitNonLinear(pixelValues, worldValues, lc);
      }

  } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}


TabularCoordinate makeLinearCoordinate(String& axisName,
                                       String& axisUnit,
                                       Double& crval,
                                       Double& crpix,
                                       Double& cdelt)
{
   crval = 10.12;
   crpix = -128.32;
   cdelt = 3.145;
   axisUnit = "km";
   axisName = "Doggies";
   return TabularCoordinate(crval, cdelt, crpix, axisUnit, axisName);
}
 
TabularCoordinate makeNonLinearCoordinate (String& axisName,
                                           String& axisUnit,
                                           Vector<Double>& pixelValues,
                                           Vector<Double>& worldValues)
{
   pixelValues.resize(5);
   worldValues.resize(5);
   pixelValues(0) = 10.0; pixelValues(1) = 22.0; pixelValues(2) = 80.0;
   pixelValues(3) = 102.2; pixelValues(4) = 102.3;    
   worldValues(0) = -100.0, worldValues(1) = -80.0; worldValues(2) = 140.2;
   worldValues(3) = 1000.212; worldValues(4) = 2100.3;    
   axisUnit = "km";
   axisName = "Doggies";
   return TabularCoordinate(pixelValues, worldValues, axisUnit, axisName);
}


void doit (TabularCoordinate& lc,
           const String& axisName,
           const String& axisUnit)
{

// Test copy constructor

   {
       TabularCoordinate lc2(lc);
       if (!lc.near(&lc2)) {
          throw(AipsError("Failed copy constructor test"));
       }
   } 

// Test assignment

   {
       TabularCoordinate lc2;
       lc2 = lc;
       if (!lc.near(&lc2)) {
          throw(AipsError("Failed assignment test"));
       }
   } 

// Test member functions
  
   if (lc.type() != Coordinate::TABULAR) {
      throw(AipsError("Failed type test"));
   }
   if (lc.showType() != "Tabular") {
      throw(AipsError("Failed showType test"));
   }
//
   if (lc.nPixelAxes() != 1) {
      throw(AipsError("Failed nPixelAxes test"));
   }
//
   if (lc.nWorldAxes() != 1) {
      throw(AipsError("Failed nWorldAxes test"));
   }
//
   Vector<String> names(1); names(0) = axisName;
   if (!allEQ(names.ac(), lc.worldAxisNames().ac())) {
      throw(AipsError("Failed world axis name recovery test"));
   }
   names(0) = "Horsies";
   if (!lc.setWorldAxisNames(names)) {
      throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
   }
   if (!allEQ(names.ac(), lc.worldAxisNames().ac())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
   Vector<String> units(1); units(0) = axisUnit;     
   if (!allEQ(units.ac(), lc.worldAxisUnits().ac())) {
      throw(AipsError("Failed world axis units recovery test"));
   }
   units(0) = "m";
   if (!lc.setWorldAxisUnits(units)) {
      throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
   }
   if (!allEQ(units.ac(), lc.worldAxisUnits().ac())) {
      throw(AipsError("Failed world axis units set/recovery test"));
   }
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
   Double val = 20.12345;
   String str = lc.format(unit, Coordinate::FIXED, val, 0,
             True, 4);
   if (str != "20.1234") {
      throw(AipsError("Failed format test 1"));
   }
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 0,
             True, 4);
   if (str != "2.0123e+01") {
      throw(AipsError("Failed format test 2"));
   }
//
// Test record saving
//
   TableRecord rec;
   if (!lc.save(rec, "Tabular")) {
      throw(AipsError("Coordinate saving to Record failed"));  
   }  
   TabularCoordinate* plc = TabularCoordinate::restore(rec, "Tabular");
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


void doitLinear (const Double refVal,
                 const Double refPix,
                 const Double incr,
                 const Double linTrans,
                 TabularCoordinate& lc)
{
   Vector<Double> crval(1); crval(0) = refVal;
   Vector<Double> crpix(1); crpix(0) = refPix;
   Vector<Double> cdelt(1); cdelt(0) = incr;
   Matrix<Double> xform(1,1); xform(0,0) = linTrans;
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
      throw(AipsError("Failed Tabular transform recovery test"));
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
//
// Test conversion
//
   Vector<Double> pixel(1), world2(1), world;
   pixel(0) = 12.2;
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
   }
//
// Compute expected values.  
//
   world2(0) = (pixel(0) - crpix(0)) * xform(0,0) * cdelt(0) + crval(0);
//
   if (!allNear(world2.ac(), world.ac(), 1e-6)) {
         throw(AipsError("toWorld conversion gave wrong answer"));
   }
//
   Vector<Double> pixel2;
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2.ac(), pixel.ac(), 1e-6)) {
         throw(AipsError("Coordinate conversion reflection failed"));
   }
}

void doitNonLinear (const Vector<Double>& pixelValues,
                    const Vector<Double>& worldValues,
                    TabularCoordinate& lc)
{
   Vector<Double> crval(1); crval(0) = worldValues(0);
   Vector<Double> crpix(1); crpix(0) = pixelValues(0);
   Vector<Double> cdelt(1);
   Matrix<Double> xform(1,1);
//
   if (!allEQ(crval.ac(), lc.referenceValue().ac())) {
      throw(AipsError("Failed reference value recovery test"));
   }
//
   if (!allEQ(crpix.ac(), lc.referencePixel().ac())) {
      throw(AipsError("Failed reference pixel recovery test"));
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
      throw(AipsError(String("Failed to set Tabular transform because") + lc.errorMessage()));
   }
   if (!allEQ(xform.ac(), lc.linearTransform().ac())) {
      throw(AipsError("Failed Tabular transform set/recovery test");)
   }
//
// Test conversion
//
   Vector<Double> pixel(1), world, pixel2;
   pixel(0) = 123.123;
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2.ac(), pixel.ac(), 1e-6)) {
      throw(AipsError("Coordinate conversion reflection failed"));
   }
}

