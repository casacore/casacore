//# tLinearCoordinate.cc: Test program for LinearCoordinate
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
#include <trial/Coordinates/LinearCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableRecord.h>

#include <iostream.h>

LinearCoordinate makeCoordinate(Vector<String>& names,
                                Vector<String>& units,
                                Vector<Double>& crpix,
                                Vector<Double>& crval,
                                Vector<Double>& cdelt,
                                Matrix<Double>& xform);

int main()
{
   try {

      Vector<String> names, units;
      Vector<Double> crpix, crval, cdelt;
      Matrix<Double> xform;

// Constructors

      {
         LinearCoordinate lc(1);
      }
      {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
      }

// Test near function

     {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         LinearCoordinate lc2 = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         if (!lc.near(&lc2)) {
            throw(AipsError("Failed near test 1"));
         }
         Vector<Int> excludeAxes(1, 1);
         if (!lc.near(&lc2, excludeAxes)) {
            throw(AipsError("Failed near test 2"));
         }
     } 

// Test copy constructor

     {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         LinearCoordinate lc2(lc);
         if (!lc.near(&lc2)) {
            throw(AipsError("Failed copy constructor test"));
         }
     } 

// Test assignment

     {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         LinearCoordinate lc2;
         lc2 = lc;
         if (!lc.near(&lc2)) {
            throw(AipsError("Failed assignment test"));
         }
     } 

// Test member functions
   
     {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         if (lc.type() != Coordinate::LINEAR) {
            throw(AipsError("Failed type test"));
         }
         if (lc.showType() != "Linear") {
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
         if (!allEQ(names.ac(), lc.worldAxisNames().ac())) {
            throw(AipsError("Failed world axis name recovery test"));
         }
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
         if (!allEQ(units.ac(), lc.worldAxisUnits().ac())) {
            throw(AipsError("Failed world axis units recovery test"));
         }
//       
         if (!allEQ(xform.ac(), lc.linearTransform().ac())) {
            throw(AipsError("Failed linear transform recovery test"));
         }
//
         names(0) = "horsies";
         if (!lc.setWorldAxisNames(names)) {
            throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
         }
         if (!allEQ(names.ac(), lc.worldAxisNames().ac())) {
            throw(AipsError("Failed axis name set/recovery test"));
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
         units(0) = "km";
         if (!lc.setWorldAxisUnits(units)) {
            throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
         }
         if (!allEQ(units.ac(), lc.worldAxisUnits().ac())) {
            throw(AipsError("Failed world axis units set/recovery test"));
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
         String str = lc.format(unit, Coordinate::FIXED, val, 1,
                   True, 4);
         if (str != "20.1234") {
            throw(AipsError("Failed format test 1"));
         }
         str = lc.format(unit, Coordinate::SCIENTIFIC, val, 1,
                   True, 4);
         if (str != "2.0123e+01") {
            throw(AipsError("Failed format test 2"));
         }
      }

// Test conversion

     {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         Vector<Double> pixel(2), world2(2), world;
         pixel(0) = 12.2;
         pixel(1) = -20.32;
         if (!lc.toWorld(world, pixel)) {
            throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
         }
//
// Compute expected values.  This only works because xform is unity
//
         world2(0) = (pixel(0) - crpix(0)) * cdelt(0) + crval(0);
         world2(1) = (pixel(1) - crpix(1)) * cdelt(1) + crval(1);
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
//
// Test conversion reflection with harder case
//
      {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         xform(0,0) = 1.0; xform(1,0) = 2.0; xform(0,1) = 1.5; xform(1,1) = 2.5;
         if (!lc.setLinearTransform(xform)) {
            throw(AipsError(String("Failed to set linear transform because") + lc.errorMessage()));
         }
//
         Vector<Double> pixel(2), world;
         pixel(0) = 12.2;
         pixel(1) = -20.32;
         if (!lc.toWorld(world, pixel)) {
            throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
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
//
// Test record saving
//
      {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         TableRecord rec;
         if (!lc.save(rec, "linear")) {
            throw(AipsError("Coordinate saving to Record failed"));  
         }  
         LinearCoordinate* plc = LinearCoordinate::restore(rec, "linear");
         if (!plc->near(&lc, 1e-6)) {
            throw(AipsError("Coordinate reflection through record interface failed"));  
         }
         delete plc;
      }
//
// Test clone
//
      {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         Coordinate* plc = lc.clone();
         if (!plc->near(&lc, 1e-6)) {
            throw(AipsError("Clone function failed"));  
         }
         delete plc;
      }

       
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}

LinearCoordinate makeCoordinate (Vector<String>& names,
                                 Vector<String>& units,
                                 Vector<Double>& crpix,
                                 Vector<Double>& crval,
                                 Vector<Double>& cdelt,
                                 Matrix<Double>& xform)
{
   names.resize(2);
   units.resize(2);
   crpix.resize(2);
   cdelt.resize(2);
   crval.resize(2);
   xform.resize(2,2);
   names(0) = "doggies"; names(1) = "fishies";
   units(0) = "m"; units(1) = "rad";
   crpix(0) = 10.0; crpix(1) = 20.0; 
   cdelt(0) = 1.0; cdelt(1) = 2.0;
   crval(0) = 10.0; crval(1) = 20;
   xform = 0.0; xform.diagonal() = 1.0;
//
   LinearCoordinate lc(names, units, crval, cdelt,
                       xform, crpix);
   return lc;
}

