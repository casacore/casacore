//# tStokesCoordinate.cc: Test program for StokesCoordinate
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
#include <trial/Coordinates/StokesCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableRecord.h>

#include <iostream.h>

StokesCoordinate makeCoordinate(Vector<Int>& whichStokes,
                                Vector<String>& stokesStrings);

void doit (StokesCoordinate& lc, 
           const Vector<Int>& whichStokes);

void doit2 (StokesCoordinate& lc, 
           const Vector<Int>& whichStokes);

void doit3 (StokesCoordinate& lc, 
           const Vector<Int>& whichStokes,
           const Vector<String>& stokesStrings);



int main()
{
   try {

      Vector<Int> whichStokes;
      Vector<String> stokesStrings;

// Constructors

      {
         StokesCoordinate lc = makeCoordinate(whichStokes, stokesStrings);
      }

// Test near function

     {
         StokesCoordinate lc  = makeCoordinate(whichStokes, stokesStrings);
         StokesCoordinate lc2 = makeCoordinate(whichStokes, stokesStrings);
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
         StokesCoordinate lc  = makeCoordinate(whichStokes, stokesStrings);
         doit(lc, whichStokes);
         doit2(lc, whichStokes);
      }
      {
         StokesCoordinate lc  = makeCoordinate(whichStokes, stokesStrings);
         doit3(lc, whichStokes, stokesStrings);
      }

  } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}


StokesCoordinate makeCoordinate(Vector<Int>& whichStokes,
                                Vector<String>& stokesStrings)
{
//
// Choose random and silly collection of Stokeseses

   whichStokes.resize(5);
   whichStokes(0) = Stokes::Q;
   whichStokes(1) = Stokes::RL;
   whichStokes(2) = Stokes::YY;
   whichStokes(3) = Stokes::RY;
   whichStokes(4) = Stokes::QQ;
//
   stokesStrings.resize(5);
   stokesStrings(0) = "Q";
   stokesStrings(1) = "RL";
   stokesStrings(2) = "YY";
   stokesStrings(3) = "RY";
   stokesStrings(4) = "QQ";
//
   return StokesCoordinate(whichStokes);
}
 


void doit (StokesCoordinate& lc,
           const Vector<Int>& whichStokes)
{

// Test copy constructor

   {
       StokesCoordinate lc2(lc);
       if (!lc.near(&lc2)) {
          throw(AipsError("Failed copy constructor test"));
       }
   } 

// Test assignment

   {
       Vector<Int> whichStokes2(1); whichStokes2(0) = Stokes::I;
       StokesCoordinate lc2 = StokesCoordinate(whichStokes2);
       lc2 = lc;
       if (!lc.near(&lc2)) {
          throw(AipsError("Failed assignment test"));
       }
   } 

// Test member functions
  
   if (lc.type() != Coordinate::STOKES) {
      throw(AipsError("Failed type test"));
   }
   if (lc.showType() != "Stokes") {
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
   Vector<String> axisNames(1); axisNames(0) = "Stokes";
   if (!allEQ(axisNames.ac(), lc.worldAxisNames().ac())) {
      throw(AipsError("Failed world axis name recovery test"));
   }
   axisNames(0) = "Horsies";
   if (!lc.setWorldAxisNames(axisNames)) {
      throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
   }
   if (!allEQ(axisNames.ac(), lc.worldAxisNames().ac())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
// There is no unit we can set
//
   Vector<String> axisUnits(1); axisUnits(0) = "";
   if (!allEQ(axisUnits.ac(), lc.worldAxisUnits().ac())) {
      throw(AipsError("Failed world axis units recovery test"));
   }
   if (!lc.setWorldAxisUnits(axisUnits)) {
      throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
   }
   if (!allEQ(axisUnits.ac(), lc.worldAxisUnits().ac())) {
      throw(AipsError("Failed world axis units set/recovery test"));
   }
//
   if (!allEQ(whichStokes.ac(), lc.stokes().ac())) {
      throw(AipsError("Failed Stokes recovery test"));
   }
//
// Test record saving
//
   TableRecord rec;
   if (!lc.save(rec, "Stokes")) {
      throw(AipsError("Coordinate saving to Record failed"));  
   }  
   StokesCoordinate* plc = StokesCoordinate::restore(rec, "Stokes");
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


void doit2 (StokesCoordinate& lc,
            const Vector<Int>& whichStokes)
{
   Vector<Double> crval(1); crval(0) = Double(whichStokes(0));
   if (!allEQ(crval.ac(), lc.referenceValue().ac())) {
      throw(AipsError("Failed reference value recovery test"));
   }
//
   Vector<Double> cdelt(1); cdelt(0) = 1.0;
   if (!allEQ(cdelt.ac(), lc.increment().ac())) {
      throw(AipsError("Failed increment recovery test"));
   }
//
   Vector<Double> crpix(1); crpix(0) = 0.0;
   if (!allEQ(crpix.ac(), lc.referencePixel().ac())) {
      throw(AipsError("Failed reference pixel recovery test"));
   }
//
   Matrix<Double> xform(1,1); xform(0,0) = 1.0;
   if (!allEQ(xform.ac(), lc.linearTransform().ac())) {
      throw(AipsError("Failed Stokes transform recovery test"));
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


void doit3 (StokesCoordinate& lc,
            const Vector<Int>& whichStokes,
            const Vector<String>& stokesStrings)
{
//
// Test conversion
//
   Vector<Double> pixel(1), world;
   pixel(0) = lc.referencePixel()(0);
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
   }
//
   Vector<Double> pixel2(1);
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2.ac(), pixel.ac(), 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 1 failed"));
   }
//
   world(0) = -10000.0;
   if (lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel succeeded unexpectedly")));
   } else {
//      cout << "Failed as expected with" << lc.errorMessage() << endl;
   }
//
   Int pixel3;
   for (Int i=0; i<Int(whichStokes.nelements()); i++) {
      Stokes::StokesTypes sType = Stokes::type(lc.stokes()(i)), sType2;
      if (!lc.toPixel(pixel3, sType)) {
         throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
      }
      if (!lc.toWorld(sType2, pixel3)) {
         throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
      }
      if (sType != sType2) {
         throw(AipsError(String("coordinate conversion and reflection failed because ") + lc.errorMessage()));
      }
   }
//
// Formatting
//
   String unit;
   
   for (Int i=0; i<Int(whichStokes.nelements()); i++) {
      Stokes::StokesTypes sType = Stokes::type(lc.stokes()(i));
      if (!lc.toPixel(pixel3, sType)) {
         throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
      }
      pixel2(0) = pixel3;
      if (!lc.toWorld(world, pixel2)) {
         throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
      }
//
      String str = lc.format(unit, Coordinate::FIXED, world(0), 0,
                             True, 4);
      if (str != stokesStrings(i)) {
         throw(AipsError(String("formatting failed")));
      }
   }
}   

