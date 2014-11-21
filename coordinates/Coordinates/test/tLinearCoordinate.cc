//# tLinearCoordinate.cc: Test program for LinearCoordinate
//# Copyright (C) 1998,1999,2000,2001,2003,2004
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
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/TableRecord.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

LinearCoordinate makeCoordinate(Vector<String>& names,
                                Vector<String>& units,
                                Vector<Double>& crpix,
                                Vector<Double>& crval,
                                Vector<Double>& cdelt,
                                Matrix<Double>& xform,
                                uInt n=2);

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
         if (!lc.near(lc2)) {
            throw(AipsError("Failed near test 1"));
         }
         Vector<Int> excludeAxes(1, 1);
         if (!lc.near(lc2, excludeAxes)) {
            throw(AipsError("Failed near test 2"));
         }
     } 

// Test Quantum constructor interface
  
     {
        Matrix<Double> xform(2,2);
        xform = 0.0;
        xform.diagonal() = 1.0;
//       
        Vector<Double> crval(2);
        Vector<Double> crpix(2);
        Vector<Double> cdelt(2);
        Vector<String> names(2);
        Vector<String> units(2);
//
        crval(0) = 0.1; crval(1) = 0.5;
        crpix(0) = 100.0; crpix(1) = 120.0;
        cdelt(0) = 100.0; cdelt(1) = 1.0;
        units(0) = "m";
        units(1) = "m";
//
        LinearCoordinate lc1(names, units, crval, cdelt, xform, crpix);
//
        Vector<Quantum<Double> > crval2(2);
        Vector<Quantum<Double> > cdelt2(2);
        crval2(0) = Quantum<Double>(crval(0), units(0));
        crval2(1) = Quantum<Double>(crval(1), units(1));
        cdelt2(0) = Quantum<Double>(100*cdelt(0), "cm");
        cdelt2(1) = Quantum<Double>(100*cdelt(1), "cm");
//
        LinearCoordinate lc2(names, crval2, cdelt2, xform, crpix);
//
        if (!lc1.near(lc2)) {
           throw(AipsError(String("Quantum interface constructor failed consistency test")));
        }
      }


// Test copy constructor

     {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         LinearCoordinate lc2(lc);
         if (!lc.near(lc2)) {
            throw(AipsError("Failed copy constructor test"));
         }
     } 

// Test assignment

     {
         LinearCoordinate lc = makeCoordinate(names, units, crpix, crval, cdelt, xform);
         LinearCoordinate lc2;
         lc2 = lc;
         if (!lc.near(lc2)) {
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
         if (!allEQ(names, lc.worldAxisNames())) {
            throw(AipsError("Failed world axis name recovery test"));
         }
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
         if (!allEQ(units, lc.worldAxisUnits())) {
            throw(AipsError("Failed world axis units recovery test"));
         }
//       
         if (!allEQ(xform, lc.linearTransform())) {
            throw(AipsError("Failed linear transform recovery test"));
         }
//
         names(0) = "horsies";
         if (!lc.setWorldAxisNames(names)) {
            throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
         }
         if (!allEQ(names, lc.worldAxisNames())) {
            throw(AipsError("Failed axis name set/recovery test"));
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
         units(0) = "km";
         if (!lc.setWorldAxisUnits(units)) {
            throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
         }
         if (!allEQ(units, lc.worldAxisUnits())) {
            throw(AipsError("Failed world axis units set/recovery test 1"));
         }
         units(0) = "GHz";
         if (!lc.overwriteWorldAxisUnits(units)) {
            throw(AipsError(String("Failed to overwrite world axis units because ") + lc.errorMessage()));
         }
         if (!allEQ(units, lc.worldAxisUnits())) {
            throw(AipsError("Failed world axis units set/recovery test 2"));
         }
//       
         xform.diagonal() = -2.0;
         if (!lc.setLinearTransform(xform)) {
            throw(AipsError(String("Failed to set linear transform because") + lc.errorMessage()));
         }
         if (!allEQ(xform, lc.linearTransform())) {
            throw(AipsError("Failed linear transform set/recovery test"));
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
         Quantum<Double> valq(val, Unit(units(1)));
         String str = lc.format(unit, Coordinate::FIXED, val, 1,
                                True, True, 4);
         String str2 = lc.formatQuantity(unit, Coordinate::FIXED, valq, 1,
                                         True, True, 4);
         if (str != "20.1234" || str2 != "20.1234") {
            throw(AipsError("Failed format test 1"));
         }
//
         str = lc.format(unit, Coordinate::SCIENTIFIC, val, 1,
                         True, True, 4);
         str2 = lc.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, 1,
                                  True, True, 4);
         if (str != "2.0123e+01" || str2 != "2.0123e+01") {
            throw(AipsError("Failed format test 2"));
         }
//
         unit = "MHz";
         val = 20.0;
         str = lc.format(unit, Coordinate::FIXED, val, 0,
                         True, True, 4);
         if (str != "20000.0000") {
            throw(AipsError("Failed format test 3"));
         }
//
         {
           Vector<Double> w(2);
           Vector<String> u(2);
           w = lc.referenceValue();
           u = lc.worldAxisUnits();
           w(0) = 10.0;
           u(0) = "GHz";
           if (!lc.setReferenceValue(w)) {
             throw(AipsError(lc.errorMessage()));
           }
           if (!lc.setWorldAxisUnits(u)) {
             throw(AipsError(lc.errorMessage()));
           }
//
           unit = "MHz";
           val = 1.0;
           str = lc.format(unit, Coordinate::FIXED, val, 0,
                           False, True, 4);
           if (str != "11000.0000") {
               throw(AipsError("Failed format test 4"));
           }
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
         if (!allNear(world2, world, 1e-6)) {
               throw(AipsError("toWorld conversion gave wrong answer"));
         }
//
         Vector<Double> pixel2;
         if (!lc.toPixel(pixel2, world)) {
            throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
         }
         if (!allNear(pixel2, pixel, 1e-6)) {
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
         if (!allNear(pixel2, pixel, 1e-6)) {
            throw(AipsError("Coordinate conversion reflection failed"));
         }
      }
//
// Test Fourier Coordinate.  Hard to do much with it.  LinearXform has been tested
// pretty hard too.
//
     {
         LinearCoordinate lc0 = makeCoordinate(names, units, crpix, crval, cdelt, xform, 3);
         units(0) = "GHz"; units(1) = "Hz"; units(2) = "s";
         LinearCoordinate lc(names, units, crval, cdelt, xform, crpix);
//
         Vector<Bool> axes(names.nelements(), True);
         Vector<Int> shape(names.nelements());
         for (uInt i=0; i<shape.nelements(); i++) {
            shape(i) = 10*(i+2);
         }

// All axes

         {
            Coordinate* pC = lc.makeFourierCoordinate (axes, shape);
//
            Vector<String> units2 = pC->worldAxisUnits();
            Vector<String> names2 = pC->worldAxisNames();
            Vector<Double> crval2 = pC->referenceValue();
            Vector<Double> crpix2 = pC->referencePixel();
            if (units2(0)!=String("s") || units2(1)!=String("s") ||
                units2(2)!=String("Hz")) {
               throw(AipsError("makeFourierCoordinate (1) failed units test"));
            }
            if (names2(0)!=String("Time") || names2(1)!=String("Time") ||
                names2(2)!=String("Frequency")) {
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
            Coordinate* pC = lc.makeFourierCoordinate (axes, shape);
//
            const Vector<String>& units2 = pC->worldAxisUnits();
            const Vector<String>& names2 = pC->worldAxisNames();
            const Vector<Double>& crval2 = pC->referenceValue();
            const Vector<Double>& crpix2 = pC->referencePixel();
            if (units2(0)!=String("s") || units2(1)!=String("Hz") ||
                units2(2)!=String("Hz")) {
               throw(AipsError("makeFourierCoordinate (2) failed units test"));
            }
            if (names2(0)!=String("Time") || names2(1)!=names(1) ||
                names2(2)!=String("Frequency")) {
               throw(AipsError("makeFourierCoordinate (2) failed names test"));
            }
            for (uInt i=0; i<pC->nPixelAxes(); i++) {
               if (i==1) {
                  if (!near(crpix(i), crpix2(i))) {
                     throw(AipsError("makeFourierCoordinate (2) failed crpix test"));
                  }
                  if (!near(crval(i), crval2(i))) {
                     throw(AipsError("makeFourierCoordinate (2) failed crval test"));
                  }
               } else {
                  if (!near(Double(Int(shape(i)/2)), crpix2(i))) {
                     throw(AipsError("makeFourierCoordinate (2) failed crpix test"));
                  }
                  if (!near(0.0, crval2(i))) {
                     throw(AipsError("makeFourierCoordinate (2) failed crval test"));
                  }
               }
            }     
            delete pC;
         }


// Not all axes and non-diagonal PC

         {
            xform(0,1) = 1.0;
            LinearCoordinate lc2(names, units, crval, cdelt, xform, crpix);
//
            axes.set(True);
            axes(1) = False;
            Coordinate* pC = lc2.makeFourierCoordinate (axes, shape);
            if (pC) {
               delete pC;
               throw(AipsError("Failed to induce forced error in makeFourierCoordinate (3)"));
            }
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
         if (!plc->near(lc, 1e-6)) {
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
         if (!plc->near(lc, 1e-6)) {
            throw(AipsError("Clone function failed"));  
         }
         delete plc;
      }

       
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}

LinearCoordinate makeCoordinate (Vector<String>& names,
                                 Vector<String>& units,
                                 Vector<Double>& crpix,
                                 Vector<Double>& crval,
                                 Vector<Double>& cdelt,
                                 Matrix<Double>& xform,
                                 uInt n)
{
   Vector<String> uu(5);
   uu(0) = "m"; uu(1) = "rad"; uu(2) = "s";
   uu(3) = "GHz"; uu(4) = "pc";
//
   names.resize(n);
   units.resize(n);
   crpix.resize(n);
   cdelt.resize(n);
   crval.resize(n);
   xform.resize(n,n);
//
   for (uInt i=0; i<n; i++) {
      ostringstream oss;
      oss << i;
      names(i) = "axis" + String(oss);
      crpix(i) = 10*(i+1);
      cdelt(i) = i+1;
      crval(i) = 20*(i+1);
      if (i>4) {
         units(i) = uu(0);
      } else {
         units(i) = uu(i);
      }
   }
   xform = 0.0; xform.diagonal() = 1.0;
//
   LinearCoordinate lc(names, units, crval, cdelt,
                       xform, crpix);
   return lc;
}

