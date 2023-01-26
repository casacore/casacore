//# tTabularCoordinate.cc: Test program for TabularCoordinate
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

 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/Quantum.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

TabularCoordinate makeLinearCoordinate(String& axisName,
                                       String& axisUnit,
                                       double& crval,
                                       double& crpix,
                                       double& cdelt);
 
TabularCoordinate makeNonLinearCoordinate (String& axisName,
                                           String& axisUnit,
                                           Vector<double>& pixelValues,
                                           Vector<double>& worldValues);

void doit (TabularCoordinate& lc, 
           const String& axisName,
           const String& axisUnit);

void doitLinear (const double refVal,
                 const double refPix,
                 const double incr,
                 const double linTrans,
                 TabularCoordinate& lc);

void doitNonLinear (const Vector<double>& pixelValues,
                    const Vector<double>& worldValues,
                    TabularCoordinate& lc);


int main()
{
   try {

      String axisName, axisUnit;
      double crpix, crval, cdelt;
      Matrix<double> xform;
      Vector<double> pixelValues;
      Vector<double> worldValues;

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
         if (!lc.near(lc2)) {
            throw(AipsError("Failed near test 1"));
         }
         Vector<int32_t> excludeAxes(1, 0);
         if (!lc.near(lc2, excludeAxes)) {
            throw(AipsError("Failed near test 2"));
         }
     } 

// Test Quantum constructor interfaces
  
     {
        double crval = 100.0;
        double crpix = 1.0;
        double cdelt = 1.2;
        String name("length");
        String unit("m");
//
        TabularCoordinate tc1(crval, cdelt, crpix, unit, name);
//
        Quantum<double> crval2(crval, "m");
        Quantum<double> cdelt2(100*cdelt, "cm");
        TabularCoordinate tc2(crval2, cdelt2, crpix, name);
//
        if (!tc1.near(tc2)) {
           throw(AipsError(String("Quantum interface (1) constructor failed consistency test")));
        }
      }


     {
        Vector<double> world(3);
        Vector<double> pixel(3);
        pixel(0) = 1.0; pixel(1) = 3.0; pixel(2) = 6.0;
        world(0) = 10.0; world(1) = 30.0; world(2) = 60.0;
        String name("length");
        String unit("m");
//
        TabularCoordinate tc1(pixel, world, unit, name);
//
        Quantum<Vector<double> > world2(world, unit);
        TabularCoordinate tc2(pixel, world2, name);
//
        if (!tc1.near(tc2)) {
           throw(AipsError(String("Quantum interface (2) constructor failed consistency test")));
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

  } catch (std::exception& x) {
      cerr << "aipserror: error " << x.what() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}


TabularCoordinate makeLinearCoordinate(String& axisName,
                                       String& axisUnit,
                                       double& crval,
                                       double& crpix,
                                       double& cdelt)
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
                                           Vector<double>& pixelValues,
                                           Vector<double>& worldValues)
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
       if (!lc.near(lc2)) {
          throw(AipsError("Failed copy constructor test"));
       }
   } 

// Test assignment

   {
       TabularCoordinate lc2;
       lc2 = lc;
       if (!lc.near(lc2)) {
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
   if (!allEQ(names, lc.worldAxisNames())) {
      throw(AipsError("Failed world axis name recovery test"));
   }
   names(0) = "Horsies";
   if (!lc.setWorldAxisNames(names)) {
      throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
   }
   if (!allEQ(names, lc.worldAxisNames())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
   Vector<String> units(1); units(0) = axisUnit;     
   if (!allEQ(units, lc.worldAxisUnits())) {
      throw(AipsError("Failed world axis units recovery test"));
   }
   units(0) = "m";
   if (!lc.setWorldAxisUnits(units)) {
      throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
   }
   if (!allEQ(units, lc.worldAxisUnits())) {
      throw(AipsError("Failed world axis units set/recovery test"));
   }
//
   int32_t prec;
   Coordinate::formatType fType = Coordinate::SCIENTIFIC;
   lc.getPrecision(prec, fType, true, 6, 4, 2);
   if (prec != 6) {
      throw(AipsError("Failed getPrecision test 1"));
   }
   fType = Coordinate::FIXED;
   lc.getPrecision(prec, fType, true, 6, 4, 2);
   if (prec != 4) {
      throw(AipsError("Failed getPrecision test 2"));
   }
//
   String unit;
   double val = 20.12345;
   Quantum<double> valq(val, Unit(units(0)));
   String str = lc.format(unit, Coordinate::FIXED, val, 0,
                          true, true, 4);
   String str2 = lc.formatQuantity(unit, Coordinate::FIXED, valq, 0,
                                   true, true, 4);
   if (str != "20.1234" || str2 != "20.1234") {
      throw(AipsError("Failed format test 1"));
   }
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 0, true,
                   true, 4);
   str2 = lc.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, 0, true,
                            true, 4);
   if (str != "2.0123e+01" || str2 != "2.0123e+01") {
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


void doitLinear (const double refVal,
                 const double refPix,
                 const double incr,
                 const double linTrans,
                 TabularCoordinate& lc)
{
   Vector<double> crval(1); crval(0) = refVal;
   Vector<double> crpix(1); crpix(0) = refPix;
   Vector<double> cdelt(1); cdelt(0) = incr;
   Matrix<double> xform(1,1); xform(0,0) = linTrans;
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
      throw(AipsError("Failed Tabular transform recovery test"));
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
//
// Test conversion
//
   Vector<double> pixel(1), world2(1), world;
   pixel(0) = 12.2;
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
   }
//
// Compute expected values.  
//
   world2(0) = (pixel(0) - crpix(0)) * xform(0,0) * cdelt(0) + crval(0);
//
   if (!allNear(world2, world, 1e-6)) {
         throw(AipsError("toWorld conversion gave wrong answer"));
   }
//
   Vector<double> pixel2;
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2, pixel, 1e-6)) {
         throw(AipsError("Coordinate conversion reflection failed"));
   }
//
// Test Fourier coordinate
//
   {
      AlwaysAssert(lc.nPixelAxes()==1, AipsError);
      Vector<bool> axes(1, true);
      Vector<int32_t> shape(1);
      shape(0) = 128;

// All axes

      {
         Coordinate* pC = lc.makeFourierCoordinate (axes, shape);
//
         Vector<String> units2 = pC->worldAxisUnits();
         Vector<String> names2 = pC->worldAxisNames();
         Vector<double> crval2 = pC->referenceValue();
         Vector<double> crpix2 = pC->referencePixel();
         String tt = String("1/") + lc.worldAxisUnits()(0);
         if (units2(0)!=tt) {
            throw(AipsError("makeFourierCoordinate (1) failed units test"));
         }
         tt = String("Inverse(") + lc.worldAxisNames()(0) + String(")");
         if (names2(0)!=tt) {
            throw(AipsError("makeFourierCoordinate (1) failed names test"));
         }
         if (!allNear(crval2,0.0,1e-13)) {
            throw(AipsError("makeFourierCoordinate (1) failed crval test"));
         }
         for (uint32_t i=0; i<pC->nPixelAxes(); i++) {
            if (!near(double(int32_t(shape(i)/2)), crpix2(i))) {
               throw(AipsError("makeFourierCoordinate (1) failed crpix test"));
            }
         }
         delete pC;
      }

// No axes   
          
      {
         axes.set(false);
         Coordinate* pC = lc.makeFourierCoordinate (axes, shape);
         if (pC) {
            delete pC;
            throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
         }
      }
   }
}

void doitNonLinear (const Vector<double>& pixelValues,
                    const Vector<double>& worldValues,
                    TabularCoordinate& lc)
{
   Vector<double> crval(1); crval(0) = worldValues(0);
   Vector<double> crpix(1); crpix(0) = pixelValues(0);
   Vector<double> cdelt(1);
   Matrix<double> xform(1,1);
//
   if (!allEQ(crval, lc.referenceValue())) {
      throw(AipsError("Failed reference value recovery test"));
   }
//
   if (!allEQ(crpix, lc.referencePixel())) {
      throw(AipsError("Failed reference pixel recovery test"));
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
      throw(AipsError(String("Failed to set Tabular transform because") + lc.errorMessage()));
   }
   if (!allEQ(xform, lc.linearTransform())) {
      throw(AipsError("Failed Tabular transform set/recovery test"));
   }
//
// Test conversion
//
   Vector<double> pixel(1), world, pixel2;
   pixel(0) = 123.123;
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2, pixel, 1e-6)) {
      throw(AipsError("Coordinate conversion reflection failed"));
   }

// Fourier

   {
      Vector<bool> axes(lc.nPixelAxes(), true);      
      Vector<int32_t> shape(lc.nPixelAxes(), 10);
      Coordinate* pC = lc.makeFourierCoordinate (axes, shape);
      if (pC) {
         delete pC;
         throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
      }
   }
}

