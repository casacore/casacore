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
//# $Id$
//#

 
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Containers/Record.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MDoppler.h>
#include <casa/Quanta/Unit.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <coordinates/Coordinates/LinearCoordinate.h>
#include <coordinates/Coordinates/StokesCoordinate.h>
#include <coordinates/Coordinates/TabularCoordinate.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <casa/Exceptions/Error.h>

#include <casa/iostream.h>
#include <casa/namespace.h>


DirectionCoordinate makeDirectionCoordinate(Bool unitsAreDegrees=True,
                                            MDirection::Types type=MDirection::J2000);
SpectralCoordinate makeSpectralCoordinate ();
StokesCoordinate makeStokesCoordinate(Bool silly=True);
LinearCoordinate makeLinearCoordinate(uInt nAxes=2);
TabularCoordinate makeTabularCoordinate();
CoordinateSystem makeCoordinateSystem(uInt& nCoords,
                                      Vector<Int>& types,
                                      Vector<String>& sTypes,
                                      uInt& iDC,
                                      uInt& iSpC,
                                      uInt& iTC,
                                      uInt& iStC,
                                      uInt& iLC,
                                      DirectionCoordinate& dC,
                                      SpectralCoordinate& spC,
                                      TabularCoordinate& tC,
                                      StokesCoordinate& stC,
                                      LinearCoordinate& lC);


void doit (CoordinateSystem& lc, uInt nCoords,
           const Vector<Int>& types,
           const Vector<String>& sTypes,
           const uInt iDC,
           const uInt iSpC,
           const uInt iTC,
           const uInt iStC,
           const uInt iLC,
           const DirectionCoordinate&,
           const SpectralCoordinate&,
           const TabularCoordinate&,
           const StokesCoordinate&,
           const LinearCoordinate&);
void doit2 (CoordinateSystem& cSys);
void doit3 (CoordinateSystem& cSys);
void doit4 ();
void doit5 ();
void doit6 ();
void doit7 ();



int main()
{
   try {

      uInt nCoords;
      Vector<Int> types;
      Vector<String> sTypes;
      DirectionCoordinate dC;
      SpectralCoordinate spC;
      TabularCoordinate tC;
      StokesCoordinate stC = makeStokesCoordinate();  // No default constrcutor
      LinearCoordinate lC;
      uInt iDC;
      uInt iSpC;
      uInt iTC;
      uInt iStC;
      uInt iLC;
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iLC,
                                                      dC, spC, tC, stC, lC);
      }
      {
         CoordinateSystem cSys1 = makeCoordinateSystem(nCoords, types, sTypes,
                                                       iDC, iSpC, iTC, iStC, iLC,
                                                       dC, spC, tC, stC, lC);
         CoordinateSystem cSys2 = makeCoordinateSystem(nCoords, types, sTypes,
                                                       iDC, iSpC, iTC, iStC, iLC,
                                                       dC, spC, tC, stC, lC);

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
         Vector<Int> excludeAxes(2);
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
         Vector<Double> refVal1(cSys1.referenceValue().copy());         
         Vector<Double> refPix1(cSys1.referencePixel().copy());         
         Vector<Double> inc1(cSys1.increment().copy());         
         Vector<Double> refVal2(cSys2.referenceValue().copy());         
         Vector<Double> refPix2(cSys2.referencePixel().copy());         
         Vector<Double> inc2(cSys2.increment().copy());         
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
         Int pAxis = cSys1.nPixelAxes() - 2;
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
                                                      iDC, iSpC, iTC, iStC, iLC,
                                                      dC, spC, tC, stC, lC);
         doit(cSys, nCoords, types, sTypes, 
              iDC, iSpC, iTC, iStC, iLC,
              dC, spC, tC, stC, lC);
      }
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iLC,
                                                      dC, spC, tC, stC, lC);
         doit2(cSys);
      }
      {
         CoordinateSystem cSys = makeCoordinateSystem(nCoords, types, sTypes,
                                                      iDC, iSpC, iTC, iStC, iLC,
                                                      dC, spC, tC, stC, lC);
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
                                                      iDC, iSpC, iTC, iStC, iLC,
                                                      dC, spC, tC, stC, lC);
//
         LogOrigin lO(String("tCoordinateSystem"), String("main()"), WHERE);
         LogIO os(lO);
         IPosition s1, s2;
         cSys.list(os, MDoppler::RADIO, s1, s2);
      }


   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}



void doit (CoordinateSystem& cSys, uInt nCoords, const Vector<Int>& types,
           const Vector<String>& sTypes,
           const uInt iDC,
           const uInt iSpC,
           const uInt iTC,
           const uInt iStC,
           const uInt iLC,
           const DirectionCoordinate& dC,
           const SpectralCoordinate& spC,
           const TabularCoordinate& tC,
           const StokesCoordinate& stC,
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
   for (uInt i=0; i<nCoords; i++) {
      Coordinate::Type type = (Coordinate::Type)types(i);
      if (cSys.type(i) != type) {
         throw(AipsError("Failed type test 2"));
      }
   }
   for (uInt i=0; i<nCoords; i++) {
      if (cSys.showType(i) != sTypes(i)) {
         throw(AipsError("Failed type test 3"));
      }
   }
//
   String msg;
   Int iC;
   iC = cSys.findCoordinate(Coordinate::DIRECTION);
   if (iC != Int(iDC)) {
      throw(AipsError("Failed findCoordinate test 1"));
   }
   if (!dC.near(cSys.directionCoordinate(uInt(iC)))) {
      msg = String("Failed directionCoordinate test because ") +
            dC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::SPECTRAL);
   if (iC != Int(iSpC)) {
      throw(AipsError("Failed findCoordinate test 2"));
   }
   if (!spC.near(cSys.spectralCoordinate(uInt(iC)))) {
      msg = String("Failed spectralCoordinate test because ") +   
            spC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::TABULAR);
   if (iC != Int(iTC)) {
      throw(AipsError("Failed findCoordinate test 3"));
   }
   if (!tC.near(cSys.tabularCoordinate(uInt(iC)))) {
      msg = String("Failed tabularCoordinate test because ") +   
            tC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::STOKES);
   if (iC != Int(iStC)) {
      throw(AipsError("Failed findCoordinate test 4"));
   }
   if (!stC.near(cSys.stokesCoordinate(uInt(iC)))) {
      msg = String("Failed stokesCoordinate test because ") +   
                   stC.errorMessage();
      throw(AipsError(msg));
   }
   const Int stokesPixelAxis = cSys.pixelAxes(iC)(0);
   const Int stokesWorldAxis = cSys.worldAxes(iC)(0);
   const Int whichStokesCoordinate = iC;
//
   iC = cSys.findCoordinate(Coordinate::LINEAR);
   if (iC != Int(iLC)) {
      throw(AipsError("Failed findCoordinate test 5"));
   }
   if (!lC.near(cSys.linearCoordinate(uInt(iC)))) {
      msg = String("Failed linearCoordinate test because ") +   
                   lC.errorMessage();
      throw(AipsError(msg));
   }
//
   uInt nPixelAxes = 0;
   uInt nWorldAxes = 0;
   for (uInt i=0; i<nCoords; i++) {  
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
   Int coordinate, axisInCoordinate;
   for (uInt i=0; i<cSys.nWorldAxes(); i++) {
      cSys.findWorldAxis(coordinate, axisInCoordinate, i);
      if (coordinate==-1) {
         throw(AipsError("Failed findWorldAxis test 1"));
      } else {
         Vector<Int> worldAxes = cSys.worldAxes(coordinate);      
         if (axisInCoordinate >= Int(worldAxes.nelements()) ||
             axisInCoordinate<0) {
            throw(AipsError("Failed findWorldAxis test 2"));
         }
         if (worldAxes.nelements() == 
             cSys.coordinate(coordinate).nWorldAxes()) {
            Bool ok = False;

// Try and find the original world axis (i) in this list
// of world axes
//
            for (uInt j=0; j<worldAxes.nelements(); j++) {
               if (Int(i)==worldAxes(j)) {
                  ok = True;
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
   for (uInt i=0; i<cSys.nPixelAxes(); i++) {
      cSys.findPixelAxis(coordinate, axisInCoordinate, i);
      if (coordinate==-1) {
         throw(AipsError("Failed findPixelAxis test 1"));
      } else {
         Vector<Int> pixelAxes = cSys.pixelAxes(coordinate);      
         if (axisInCoordinate >= Int(pixelAxes.nelements()) ||
             axisInCoordinate<0) {
            throw(AipsError("Failed findPixelAxis test 2"));
         }
         if (pixelAxes.nelements() == 
             cSys.coordinate(coordinate).nPixelAxes()) {
            Bool ok = False;

// Try and find the original pixel axis (i) in this list
// of pixel axes
//
            for (uInt j=0; j<pixelAxes.nelements(); j++) {
               if (Int(i)==pixelAxes(j)) {
                  ok = True;
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
      for (uInt i=0; i<nCoords; i++) {  
         Vector<Int> pixelAxes = cSys.pixelAxes(i);
         Vector<Int> worldAxes = cSys.worldAxes(i);
         for (uInt j=0; j<pixelAxes.nelements(); j++) {
            Int worldAxis = cSys.pixelAxisToWorldAxis(pixelAxes(j));
//
// Try and find this world axis in the list
//
            Bool ok = False;
            for (uInt k=0; k<worldAxes.nelements(); k++) {
               if (worldAxis==worldAxes(k)) {
                  ok = True;
                  break;
               }
            }
            if (!ok) {
               throw(AipsError("Failed findWorldAxis"));
            }
         }
//
         for (uInt j=0; j<worldAxes.nelements(); j++) {
            Int pixelAxis = cSys.worldAxisToPixelAxis(worldAxes(j));
//
// Try and find this pixel axis in the list
//
            Bool ok = False;
            for (uInt k=0; k<pixelAxes.nelements(); k++) {
               if (pixelAxis==pixelAxes(k)) {
                  ok = True;
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
   Vector<Double> refValues = cSys.referenceValue();
   Vector<Double> inc = cSys.increment();
//
   for (uInt i=0; i<cSys.nCoordinates(); i++) {
      Vector<Int> worldAxes = cSys.worldAxes(i);
      Vector<String> worldAxisNames2 = cSys.coordinate(i).worldAxisNames();
      Vector<String> worldAxisNames3(worldAxes.nelements());
      Vector<String> worldAxisUnits2 = cSys.coordinate(i).worldAxisUnits();
      Vector<String> worldAxisUnits3(worldAxes.nelements());
      Vector<Double> refValues2 = cSys.coordinate(i).referenceValue();
      Vector<Double> refValues3(worldAxes.nelements());
      Vector<Double> inc2 = cSys.coordinate(i).increment();
      Vector<Double> inc3(worldAxes.nelements());
//
      for (uInt j=0; j<worldAxes.nelements(); j++) {
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
   Double t = refValues(stokesWorldAxis);
   refValues(0) = refValues(0)*2;
   refValues(stokesWorldAxis) = t;
   if (!cSys.setReferenceValue(refValues)) {
      throw(AipsError(String("Failed to set reference value because") 
            + cSys.errorMessage()));
   }
   if (!allNear(refValues, cSys.referenceValue(), 1e-6)) {
      throw(AipsError("Failed reference value set/recovery test"));
   }
//
   t = inc(stokesWorldAxis);
   inc(0) = inc(0)*2;
   inc(stokesWorldAxis) = t;
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
     Vector<Int> worldAxes = cSys.worldAxes(iC);
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
//
//
// Now check the pixel axis descriptors
//
   Vector<Double> refPixels = cSys.referencePixel();
   for (uInt i=0; i<cSys.nCoordinates(); i++) {
      Vector<Int> pixelAxes = cSys.pixelAxes(i);
      Vector<Double> refPixels2 = cSys.coordinate(i).referencePixel();
      Vector<Double> refPixels3(pixelAxes.nelements());
//
      for (uInt j=0; j<pixelAxes.nelements(); j++) {
         refPixels3(j) = refPixels(pixelAxes(j));
      }
      if (!allNear(refPixels2, refPixels3, 1e-6)) {
         throw(AipsError("Failed reference pixel recovery test"));
      }
   }
   t = refPixels(stokesPixelAxis);
   refPixels(0) = refPixels(0)*2;
   refPixels(stokesPixelAxis) = t;
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
   for (uInt i=0; i<cSys.linearTransform().nrow(); i++) {
      for (uInt j=0; j<cSys.linearTransform().ncolumn(); j++) {
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
   Matrix<Double> xform = cSys.linearTransform();
   xform(0,0) = 10.0;
   if (!cSys.setLinearTransform(xform)) {
      throw(AipsError(String("Failed to set linear transform because") 
            + cSys.errorMessage()));
   }
   if (!allNear(xform, cSys.linearTransform(), 1e-6)) {
      throw(AipsError("Failed linear transform set/recovery test"));
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
      Int stokesFITSValue = -1;
//
      if (CoordinateSystem::fromFITSHeader(stokesFITSValue, cSys3, rec, header,
                                           shape, 0)) {
         throw(AipsError("Unexpectedly did not fail fromFITSHeader (0)"));
      }
   }
   {
      CoordinateSystem cSys2;
      cSys2.addCoordinate(makeDirectionCoordinate(False));
      StokesCoordinate stokesCoord = makeStokesCoordinate(False);
      uInt shapeStokes = stokesCoord.stokes().nelements();
      uInt stokesAxis = 2;
      cSys2.addCoordinate(stokesCoord);
      cSys2.addCoordinate(makeSpectralCoordinate());
      cSys2.addCoordinate(makeLinearCoordinate());
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      if (!cSys2.toFITSHeader(rec, shape, True, 'c', False,
                        True, True)) {
         throw(AipsError(String("Failed to convert to FITS header (1)")));
      }

// Assigning cSys3 rather than leaving it empty will force testing of 
// more code in fromFITSHeader

// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3 = CoordinateUtil::defaultCoords2D();
//       Vector<String> header;
//       Int stokesFITSValue = -1;
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
      Vector<Int> whichStokes(4);
      whichStokes(0) = Stokes::I;
      whichStokes(1) = Stokes::Q;
      whichStokes(2) = Stokes::U;
      whichStokes(3) = Stokes::V;
      StokesCoordinate stokesCoord(whichStokes);
      uInt shapeStokes = stokesCoord.stokes().nelements();
      uInt stokesAxis = 0;
      cSys2.addCoordinate(stokesCoord);
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      if (!cSys2.toFITSHeader(rec, shape, True, 'c', False,
                        True, True)) {
         throw(AipsError(String("Failed to convert to FITS header (2)")));
      }

// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3;
//       Vector<String> header;
//       Int stokesFITSValue = -1;
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
      Vector<Int> whichStokes(4);
      whichStokes(0) = Stokes::RR;
      whichStokes(1) = Stokes::LL;
      whichStokes(2) = Stokes::RL;
      whichStokes(3) = Stokes::LR;
      StokesCoordinate stokesCoord(whichStokes);
      uInt shapeStokes = stokesCoord.stokes().nelements();
      uInt stokesAxis = 0;
      cSys2.addCoordinate(stokesCoord);
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      if (!cSys2.toFITSHeader(rec, shape, True, 'c', False,
                        True, True)) {
         throw(AipsError(String("Failed to convert to FITS header (3)")));
      }


// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3;
//       Vector<String> header;
//       Int stokesFITSValue = -1;
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
      Vector<Int> whichStokes(4);
      whichStokes(0) = Stokes::XX;
      whichStokes(1) = Stokes::YY;
      whichStokes(2) = Stokes::XY;
      whichStokes(3) = Stokes::YX;
      StokesCoordinate stokesCoord(whichStokes);
      uInt shapeStokes = stokesCoord.stokes().nelements();
      uInt stokesAxis = 0;
      cSys2.addCoordinate(stokesCoord);
//
      Record rec;
      IPosition shape(cSys2.nPixelAxes(),64);
      shape(stokesAxis) = shapeStokes;
      if (!cSys2.toFITSHeader(rec, shape, True, 'c', False,
                        True, True)) {
         throw(AipsError(String("Failed to convert to FITS header (4)")));
      }

// This part still needs to be fixed. At the moment it doesn't use fromFITSHeader
// correctly 

//       CoordinateSystem cSys3;
//       Vector<String> header;
//       Int stokesFITSValue = -1;
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
      cSys2.addCoordinate(makeStokesCoordinate(False));
      cSys2.addCoordinate(makeLinearCoordinate());
      cSys3 = cSys2;
//
      Vector<Int> wOrder(cSys2.nWorldAxes());
      Vector<Int> pOrder(cSys2.nPixelAxes());
      for (uInt i=0; i<wOrder.nelements(); i++) wOrder(i) = wOrder.nelements()-i-1;
      for (uInt i=0; i<pOrder.nelements(); i++) pOrder(i) = pOrder.nelements()-i-1;
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
     const uInt nWorldAxes = cSys.nWorldAxes();
     const uInt nPixelAxes = cSys.nPixelAxes();
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
      cSys2.addCoordinate(makeDirectionCoordinate(True, MDirection::J2000));
      cSys2.addCoordinate(makeStokesCoordinate(False));
      cSys2.replaceCoordinate(makeDirectionCoordinate(True, MDirection::B1950), 0);
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
      Vector<Float> originShift(cSys.nPixelAxes());
      Vector<Float> pixinc(cSys.nPixelAxes());
//
      {
        originShift = 1.0;
        pixinc = 1.0;
        Vector<Int> newShape;
//
        CoordinateSystem cSys2 = cSys.subImage(originShift, pixinc, newShape);
        if (cSys.nCoordinates() != cSys2.nCoordinates()) {
           throw(AipsError("Failed originShift creation test 1"));
        }
        Vector<Double> pixel = cSys.referencePixel();
        Vector<Double> pixel2 = cSys2.referencePixel() + 1.0;
        pixel2(stokesPixelAxis) = pixel(stokesPixelAxis);
        if (!allNear(pixel, pixel2, 1e-6)) {
           throw(AipsError("Failed originShift test 1"));
        }   
//
        CoordinateSystem cSys3(cSys);
        cSys3.subImageInSitu (originShift, pixinc, newShape);
        if (cSys.nCoordinates() != cSys3.nCoordinates()) {
           throw(AipsError("Failed originShift creation test 2"));
        }
        Vector<Double> pixel3 = cSys3.referencePixel() + 1.0;
        pixel3(stokesPixelAxis) = pixel(stokesPixelAxis);
        if (!allNear(pixel, pixel3, 1e-6)) {
           throw(AipsError("Failed originShift test 2"));
        }   
      }
//
      {
        originShift = 1.0;
        pixinc = 2.0;
        Vector<Int> newShape;
//
        {
           CoordinateSystem cSys2 = cSys.subImage(originShift, pixinc, newShape);
           Vector<Int> oldStokes = cSys.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<Int> newStokes = cSys2.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<Int> newStokes2 = oldStokes(IPosition(1,1),
                                              IPosition(1,oldStokes.nelements()-1),
                                              IPosition(1,2));
           if (!allEQ(newStokes, newStokes2)) {
              throw(AipsError("Failed Stokes originShift Stokes test"));
           } 
        }
//
        {
           CoordinateSystem cSys2(cSys);
           cSys2.subImageInSitu (originShift, pixinc, newShape);
           Vector<Int> oldStokes = cSys.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<Int> newStokes = cSys2.stokesCoordinate(whichStokesCoordinate).stokes();
           Vector<Int> newStokes2 = oldStokes(IPosition(1,1),
                                              IPosition(1,oldStokes.nelements()-1),
                                              IPosition(1,2));
           if (!allEQ(newStokes, newStokes2)) {
              throw(AipsError("Failed Stokes originShift Stokes test"));
           }
         }
      }
   }
}

void doit2 (CoordinateSystem& cSys)

{
   Int stokesPixelAxis = -1;
   Int stokesWorldAxis = -1;
   Int iC = cSys.findCoordinate(Coordinate::STOKES);
   if (iC>=0) {
      stokesPixelAxis = cSys.pixelAxes(iC)(0);
      stokesWorldAxis = cSys.worldAxes(iC)(0);
   }
//
// Test conversion
//
   Vector<Double> pixel(cSys.referencePixel()), world;
   if (!cSys.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ")
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
   Vector<Double> pixel2(pixel.copy());
   if (!cSys.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!cSys.toPixel(pixel, world)) {
      throw(AipsError(String("toPixel conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!allNear(pixel2, pixel, 1e-6)) {
      throw(AipsError("Coordinate reflection gave wrong results"));
   }
//
   pixel = 2.0;
   pixel(stokesPixelAxis) = 0.0;
   IPosition iPixel(pixel.nelements());
   for (uInt i=0; i<iPixel.nelements(); i++) iPixel(i) = Int(pixel(i));
   Vector<Double> world2;
   if (!cSys.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ")
                     + cSys.errorMessage()));
   }
   if (!cSys.toWorld(world2, iPixel)) {
      throw(AipsError(String("toWorld conversion failed because ")
                     + cSys.errorMessage()));
   }
   if (!allNear(world, world2, 1e-6)) {
      throw(AipsError("toWorld consistency test failed"));
   }
//
   Vector<Bool> failures;
   const uInt nBatch = 3;
   Matrix<Double> pixel3(cSys.nPixelAxes(), nBatch);
   Matrix<Double> world3(cSys.nWorldAxes(), nBatch);
   for (uInt i=0; i<nBatch; i++) {
      pixel3.column(i) = cSys.referencePixel();
   }
   if (!cSys.toWorldMany(world3, pixel3, failures)) {
      throw(AipsError(String("toWorldMany conversion failed because ")
                     + cSys.errorMessage()));
   }
   for (uInt i=0; i<nBatch; i++) {
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
   for (uInt i=0; i<nBatch; i++) {
      if (!allNear(pixel3.column(i), cSys.referencePixel(), 1e-6)) {
            throw(AipsError("toPixelMany conversion to world gave wrong results"));
      }
   }

// relative/absolute pixels

   {
      Vector<Double> refPix = cSys.referencePixel();
      Vector<Double> pixel4 = refPix.copy();
      cSys.makePixelRelative(pixel4);
      if (!allNear(pixel4, 0.0, 1e-6)) {
            throw(AipsError("Coordinate makePixelRelative 1 gave wrong results"));
      }
//

      pixel4 = refPix + 1.0;
      Vector<Double> tmp = pixel4.copy();
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
      Vector<Double> result = cSys.referenceValue().copy();
//
      Vector<Double> refVal = cSys.referenceValue();
      Vector<Double> world4 = refVal.copy();
      cSys.makeWorldRelative(world4);
      result = 0.0; result(stokesWorldAxis) = refVal(stokesWorldAxis);
      if (!allNearAbs(world4, result, 1e-6)) {
            throw(AipsError("makeWorldRelative 1 gave wrong results"));
      }
//
      Vector<Double> incr = cSys.increment();
      world4 = refVal + incr;
      Vector<Double> tmp = world4.copy();
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
      Int iDC = cSys.findCoordinate(Coordinate::DIRECTION);
      Vector<Int> worldAxes = cSys.worldAxes(iDC);
      Vector<String> worldAxisUnits = cSys.worldAxisUnits();
      worldAxisUnits(worldAxes(0)) = "rad";
      worldAxisUnits(worldAxes(1)) = "rad";
      cSys.setWorldAxisUnits(worldAxisUnits);
//
      String unit("rad");
      Double val = 0.12343;
      Quantum<Double> valq(0.12343, Unit("rad"));
      valq.convert(Unit("deg"));
      String str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(0),
                               True, True, 4);
      String str2 = cSys.formatQuantity(unit, Coordinate::FIXED, valq, worldAxes(0),
                                        True, True, 4);
      if (str != "0.1234" || str2 != "0.1234") {
         throw(AipsError("Failed format test 1"));
      }
      str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(1),
                        True, True, 4);
      str2 = cSys.formatQuantity(unit, Coordinate::FIXED, valq, worldAxes(1),
                         True, True, 4);
      if (str != "0.1234" || str2!="0.1234") {
         throw(AipsError("Failed format test 2"));
      }
//
      str = cSys.format(unit, Coordinate::SCIENTIFIC, val, worldAxes(0),
                        True, True, 4);
      str2 = cSys.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, worldAxes(0),
                                 True, True, 4);
      if (str != "1.2343e-01" || str2 != "1.2343e-01") {
         throw(AipsError("Failed format test 3"));
      }
      str = cSys.format(unit, Coordinate::SCIENTIFIC, val, worldAxes(1),
                        True, True, 4);
      str2 = cSys.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, worldAxes(1),
                                 True, True, 4);
      if (str != "1.2343e-01" || str2 != "1.2343e-01") {
         throw(AipsError("Failed format test 4"));
      }
   }
   {
      Int iSpC = cSys.findCoordinate(Coordinate::SPECTRAL);
      Vector<Int> worldAxes = cSys.worldAxes(iSpC);
//
      String unit;
      Double val = 0.12343;
      Quantum<Double> valq(0.12343, Unit("rad"));
      valq.convert(Unit("deg"));
      String str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(0),
                               True, True, 4);
      String str2 = cSys.formatQuantity(unit, Coordinate::FIXED, valq, worldAxes(0),
                                        True, True, 4);
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
      Vector<Int> newWorldOrder(cSys.nWorldAxes());
      Vector<Int> newPixelOrder(cSys.nPixelAxes());
      for (uInt i=0; i<cSys.nWorldAxes(); i++) newWorldOrder(i) = i; 
      for (uInt i=0; i<cSys.nPixelAxes(); i++) newPixelOrder(i) = i;

      Int iDC = cSys.findCoordinate(Coordinate::DIRECTION);
      Vector<Int> worldAxes = cSys.worldAxes(iDC);
      Vector<Int> pixelAxes = cSys.pixelAxes(iDC);
      Vector<Int> newWorldAxes(worldAxes.copy());
      Vector<Int> newPixelAxes(pixelAxes.copy());
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
      cSys2.addCoordinate(makeStokesCoordinate(False));
      cSys2.addCoordinate(makeDirectionCoordinate(False, MDirection::B1950));
      cSys2.addCoordinate(makeLinearCoordinate());
      CoordinateSystem cSys3 = cSys2;
      cSys3.replaceCoordinate(makeDirectionCoordinate(False, MDirection::J2000),1);
//
      Vector<Int> worldAxisMap, worldAxisTranspose;
      Vector<Int> pixelAxisMap, pixelAxisTranspose;
      Vector<Bool> refChange;
      if (!cSys2.worldMap(worldAxisMap, worldAxisTranspose, refChange, cSys3)) {
         throw(AipsError("Failed to make world map 1"));
      }
      Vector<Int> wMap(cSys2.nWorldAxes()), wTranspose(cSys2.nWorldAxes());
      for (uInt i=0; i<cSys2.nWorldAxes(); i++) {
         wMap(i) = i; 
         wTranspose(i) = i; 
      }
      if (!allEQ(wMap, worldAxisMap) ||
          !allEQ(wTranspose, worldAxisTranspose)) {
         throw(AipsError("Failed worldMap test 1a"));
      }
      if (refChange(0)!=False || refChange(1)!=True ||
          refChange(2)!=True || refChange(3)!=False) {
         throw(AipsError("Failed worldMap test 1b"));
      }
//
      if (!cSys2.pixelMap(pixelAxisMap, pixelAxisTranspose, cSys3)) {
         throw(AipsError("Failed to make pixel map 1"));
      }
      Vector<Int> pMap(cSys2.nPixelAxes()), pTranspose(cSys2.nPixelAxes());
      for (uInt i=0; i<cSys2.nPixelAxes(); i++) {
         pMap(i) = i; 
         pTranspose(i) = i; 
      }
      if (!allEQ(pMap, pixelAxisMap) ||
          !allEQ(pTranspose, pixelAxisTranspose)) {
         throw(AipsError("Failed pixelMap test 1a"));
      }
//
      Vector<Int> newWorldOrder(cSys2.nWorldAxes());
      Vector<Int> newPixelOrder(cSys2.nPixelAxes());
      for (uInt i=0; i<cSys2.nWorldAxes(); i++) newWorldOrder(i) = i; 
      for (uInt i=0; i<cSys2.nPixelAxes(); i++) newPixelOrder(i) = i;
//
      Int iDC = cSys2.findCoordinate(Coordinate::DIRECTION);
      Vector<Int> worldAxes = cSys2.worldAxes(iDC);
      Vector<Int> pixelAxes = cSys2.pixelAxes(iDC);
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
      Vector<Int> newMap(wMap.copy());
      Vector<Int> newTranspose(worldAxisTranspose.copy());
      newMap(worldAxes(0)) = newWorldOrder(worldAxes(0));
      newMap(worldAxes(1)) = newWorldOrder(worldAxes(1));
      newTranspose(worldAxes(0)) = newWorldOrder(worldAxes(0));
      newTranspose(worldAxes(1)) = newWorldOrder(worldAxes(1));
//
      if (!allEQ(newMap, worldAxisMap) ||
          !allEQ(newTranspose, worldAxisTranspose)) {
         throw(AipsError("Failed worldMap test 2a"));
      }
      if (refChange(0)!=False || refChange(1)!=True ||
          refChange(2)!=True || refChange(3)!=False) {
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
   DirectionCoordinate dC = makeDirectionCoordinate(True);// 2 & 3
   cSys.addCoordinate(dC);
//
//   cout << "Reference pixel = " << cSys.referencePixel() << endl;
//   cout << "Reference value = " << cSys.referenceValue() << endl;
//
   Vector<Double> pixelIn(cSys.nPixelAxes());
   Vector<Double> worldIn(cSys.nWorldAxes());
   Vector<Bool> pixelAxes(cSys.nPixelAxes());
   Vector<Bool> worldAxes(cSys.nWorldAxes());
   Vector<Double> worldOut, pixelOut;
   IPosition shape(cSys.nPixelAxes(), 512);
   if (!cSys.setWorldMixRanges(shape)) {
      throw(AipsError(String("setMixRanges failed with ") + cSys.errorMessage()));
   }
//
   Vector<Double> dRefVal = dC.referenceValue();   
   Vector<Int> tmp = cSys.worldAxes(2);
//
// Force a failure.   ALl axes must be pixel or world
//
   pixelAxes.set(False);
   worldAxes.set(False);
   Vector<Double> worldMin = cSys.worldMixMin();
   Vector<Double> worldMax = cSys.worldMixMax();
   if (cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix forced failure 1 did not occur")));
   }
   pixelAxes(0) = True;
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
   pixelAxes.set(True);
   worldAxes.set(False);
   Vector<Double> worldOut2;
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
   pixelAxes.set(False);
   worldAxes.set(True);
   Vector<Double> pixelOut2;
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
   pixelAxes.set(False);
   pixelAxes(0) = True;
   pixelAxes(2) = True;
//
   worldIn(1) = cSys.referenceValue()(1);   // Spectral world
   worldIn(3) = cSys.referenceValue()(3);   // Direction lat world
   worldAxes.set(False);
   worldAxes(1) = True;
   worldAxes(3) = True;
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
   pixelAxes.set(False);
   pixelAxes(1) = True;
   pixelAxes(3) = True;
//
   worldIn(0) = cSys.referenceValue()(0);   // Linear world
   worldIn(2) = cSys.referenceValue()(2);   // Direction long world
   worldAxes.set(False);
   worldAxes(0) = True;
   worldAxes(2) = True;
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
   pixelAxes.set(False);
   pixelAxes(1) = True;
   pixelAxes(3) = True;
//
   worldIn(0) = cSys.referenceValue()(0) + 5*cSys.increment()(0);   // Linear world
   worldIn(2) = cSys.referenceValue()(2) - 10*cSys.increment()(2);  // Direction long world
   worldAxes.set(False);
   worldAxes(0) = True;
   worldAxes(2) = True;
//
   Vector<Double> saveWorldIn(worldIn.copy());
   Vector<Double> savePixelIn(pixelIn.copy());
//         
   if (!cSys.toMix(worldOut, pixelOut, worldIn, pixelIn, 
                   worldAxes, pixelAxes, worldMin, worldMax)) {
      throw(AipsError(String("toMix 5 conversion failed because ")
            + cSys.errorMessage()));
   }
//
   pixelIn(0) = pixelOut(0);
   pixelIn(2) = pixelOut(2);
   pixelAxes.set(False);
   pixelAxes(0) = True; 
   pixelAxes(2) = True;  
//
   worldIn(1) = worldOut(1);
   worldIn(3) = worldOut(3);
   worldAxes.set(False);
   worldAxes(1) = True;
   worldAxes(3) = True;
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
   Vector<Int> wOrder(cSys.nWorldAxes());
   Vector<Int> pOrder(cSys.nPixelAxes());
   for (uInt i=0; i<wOrder.nelements(); i++) wOrder(i) = wOrder.nelements()-i-1;
   for (uInt i=0; i<pOrder.nelements(); i++) pOrder(i) = i;
   cSys.transpose(wOrder, pOrder);
   worldMin = cSys.worldMixMin();
   worldMax = cSys.worldMixMax();
//
   pixelIn(1) = cSys.referencePixel()(1);   // Spectral pixel
   pixelIn(3) = cSys.referencePixel()(3);   // Direction lat pixel
   pixelAxes.set(False);
   pixelAxes(1) = True;
   pixelAxes(3) = True;
//
   worldIn(1) = cSys.referenceValue()(1);   // Direction long world
   worldIn(3) = cSys.referenceValue()(3);   // Linear world
   worldAxes.set(False);
   worldAxes(1) = True;
   worldAxes(3) = True;
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
   for (uInt i=0; i<wOrder.nelements(); i++) wOrder(i) = i;
   for (uInt i=0; i<pOrder.nelements(); i++) pOrder(i) = pOrder.nelements()-i-1;
   cSys.transpose(wOrder, pOrder);
   worldMin = cSys.worldMixMin();
   worldMax = cSys.worldMixMax();
//
   pixelIn(2) = cSys.referencePixel()(2);   // Spectral pixel
   pixelIn(0) = cSys.referencePixel()(0);   // Direction lat pixel
   pixelAxes.set(False);
   pixelAxes(2) = True;
   pixelAxes(0) = True;
//
   worldIn(2) = cSys.referenceValue()(2);   // Direction long world
   worldIn(0) = cSys.referenceValue()(0);   // Linear world
   worldAxes.set(False);
   worldAxes(2) = True;
   worldAxes(0) = True;
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
      Vector<Double> pixelIn(cSys.nPixelAxes());
      Vector<Double> worldIn(cSys.nWorldAxes());
      Vector<Bool> pixelAxes(cSys.nPixelAxes());
      Vector<Bool> worldAxes(cSys.nWorldAxes());
      Vector<Double> worldOut, pixelOut;
//
      Vector<Double> worldMin(cSys.nWorldAxes());
      Vector<Double> worldMax(cSys.nWorldAxes());
//
      pixelAxes.set(False);
      worldAxes.set(False);
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
      Vector<Double> pixelIn(cSys.nPixelAxes());
      Vector<Double> worldIn(cSys.nWorldAxes());
      Vector<Bool> pixelAxes(cSys.nPixelAxes());
      Vector<Bool> worldAxes(cSys.nWorldAxes());
      Vector<Double> worldOut, pixelOut;
//
      Vector<Double> worldMin(cSys.nWorldAxes());
      Vector<Double> worldMax(cSys.nWorldAxes());
//
      pixelAxes.set(False);
      worldAxes.set(False);
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
      DirectionCoordinate dC = makeDirectionCoordinate(True);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
//
      Vector<Double> pixelIn(cSys.nPixelAxes());
      Vector<Double> worldIn(cSys.nWorldAxes());
      Vector<Bool> pixelAxes(cSys.nPixelAxes());
      Vector<Bool> worldAxes(cSys.nWorldAxes());
      Vector<Double> worldOut, pixelOut;      
      Vector<Double> dRefVal = dC.referenceValue();
//
      Vector<Double> worldMin(cSys.nWorldAxes());
      Vector<Double> worldMax(cSys.nWorldAxes());
      Vector<Int> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(False);
      worldAxes.set(False);
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
      DirectionCoordinate dC = makeDirectionCoordinate(True);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
//
      Vector<Double> pixelIn(cSys.nPixelAxes());
      Vector<Double> worldIn(cSys.nWorldAxes());
      Vector<Bool> pixelAxes(cSys.nPixelAxes());
      Vector<Bool> worldAxes(cSys.nWorldAxes());
      Vector<Double> worldOut, pixelOut;
      Vector<Double> dRefVal = dC.referenceValue();   
//
      Vector<Double> worldMin(cSys.nWorldAxes());
      Vector<Double> worldMax(cSys.nWorldAxes());
      Vector<Int> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(False); pixelAxes(0) = True;
      worldAxes.set(False);
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
      DirectionCoordinate dC = makeDirectionCoordinate(True);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(0, cSys.referencePixel()(0));
//
      Vector<Double> pixelIn(cSys.nPixelAxes());
      Vector<Double> worldIn(cSys.nWorldAxes());
      Vector<Bool> pixelAxes(cSys.nPixelAxes());
      Vector<Bool> worldAxes(cSys.nWorldAxes());
      Vector<Double> worldOut, pixelOut;
      Vector<Double> dRefVal = dC.referenceValue();   
//
      Vector<Double> worldMin(cSys.nWorldAxes());
      Vector<Double> worldMax(cSys.nWorldAxes());
      Vector<Int> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(False); 
      worldAxes.set(False); worldAxes(1) = True;
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
      DirectionCoordinate dC = makeDirectionCoordinate(True);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(1, cSys.referencePixel()(1));
//
      Vector<Double> pixelIn(cSys.nPixelAxes());
      Vector<Double> worldIn(cSys.nWorldAxes());
      Vector<Bool> pixelAxes(cSys.nPixelAxes());
      Vector<Bool> worldAxes(cSys.nWorldAxes());
      Vector<Double> worldOut, pixelOut;
      Vector<Double> dRefVal = dC.referenceValue();   
//
      Vector<Double> worldMin(cSys.nWorldAxes());
      Vector<Double> worldMax(cSys.nWorldAxes());
      Vector<Int> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = dRefVal(0) - 10.0;
         worldMax(tmp(0)) = dRefVal(0) + 10.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = dRefVal(1) - 10.0;
         worldMax(tmp(1)) = dRefVal(1) + 10.0;
      }
//
      pixelAxes.set(False); 

      worldAxes.set(False); worldAxes(0) = True;
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
      DirectionCoordinate dC = makeDirectionCoordinate(True);
      cSys.addCoordinate(dC);
      cSys.removePixelAxis(1, cSys.referencePixel()(1));
//
      Vector<Double> pixelIn(cSys.nPixelAxes());
      Vector<Double> worldIn(cSys.nWorldAxes());
      Vector<Bool> pixelAxes(cSys.nPixelAxes());
      Vector<Bool> worldAxes(cSys.nWorldAxes());
      Vector<Double> worldOut, pixelOut;
      Vector<Double> dRefVal = dC.referenceValue();   
//
      Vector<Double> worldMin(cSys.nWorldAxes());
      Vector<Double> worldMax(cSys.nWorldAxes());
      Vector<Int> tmp = cSys.worldAxes(0);
      if (tmp(0)!=-1) {
         worldMin(tmp(0)) = -180.0;
         worldMax(tmp(0)) = 180.0;
      }
      if (tmp(1)!=-1) {
         worldMin(tmp(1)) = -90.0;
         worldMax(tmp(1)) = 90.0;
      }
//
      pixelAxes.set(False); pixelAxes(0) = True;
      worldAxes.set(False);
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


DirectionCoordinate makeDirectionCoordinate(Bool unitsAreDegrees,
                                            MDirection::Types type)
{
   Projection proj = Projection::SIN;
   Vector<Double> crval(2);
   Vector<Double> crpix(2);
   Vector<Double> cdelt(2); 
   Matrix<Double> xform(2,2);
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
   Double f0 = 1.4e9;
   Double finc = 4e6;
   Double refchan = 10.5;
   Double restFreq = 1.420405752E9;
//
   return SpectralCoordinate(type, f0, finc, refchan, restFreq);
}


StokesCoordinate makeStokesCoordinate(Bool silly)

{
   if (silly) {
      Vector<Int> whichStokes(5);
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
      Vector<Int> whichStokes(5);
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
      Vector<Int> whichStokes(2);
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
      Vector<Int> whichStokes(4);
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
 

LinearCoordinate makeLinearCoordinate (uInt nAxes)
{
   Vector<String> names(nAxes);
   Vector<String> units(nAxes);
   Vector<Double> crpix(nAxes);
   Vector<Double> crval(nAxes);
   Vector<Double> cdelt(nAxes);
   Matrix<Double> xform(nAxes,nAxes);
//
   for (uInt i=0; i<nAxes; i++) {
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
   Double crval = 10.12;
   Double crpix = -128.32;
   Double cdelt = 3.145;
//
   return TabularCoordinate(crval, cdelt, crpix, axisUnit, axisName);
}
 
CoordinateSystem makeCoordinateSystem(uInt& nCoords,
                                      Vector<Int>& types,
                                      Vector<String>& sTypes,
                                      uInt& iDC,
                                      uInt& iSpC,
                                      uInt& iTC,
                                      uInt& iStC,
                                      uInt& iLC,
                                      DirectionCoordinate& dC,
                                      SpectralCoordinate& spC,
                                      TabularCoordinate& tC,
                                      StokesCoordinate& stC,
                                      LinearCoordinate& lC)
{
   CoordinateSystem cSys;
   dC = makeDirectionCoordinate();
   spC = makeSpectralCoordinate();
   tC = makeTabularCoordinate();
   stC = makeStokesCoordinate();
   lC = makeLinearCoordinate();
   cSys.addCoordinate(dC);
   cSys.addCoordinate(spC);
   cSys.addCoordinate(tC);
   cSys.addCoordinate(stC);
   cSys.addCoordinate(lC);
   iDC = 0;
   iSpC = 1;
   iTC = 2;
   iStC = 3;
   iLC = 4;
   nCoords = 5;
   types.resize(5);
   types(0) = Coordinate::DIRECTION;
   types(1) = Coordinate::SPECTRAL;
   types(2) = Coordinate::TABULAR;
   types(3) = Coordinate::STOKES;
   types(4) = Coordinate::LINEAR;
   sTypes.resize(5);
   sTypes(0) = "Direction";
   sTypes(1) = "Spectral";
   sTypes(2) = "Tabular";
   sTypes(3) = "Stokes";
   sTypes(4) = "Linear";
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
   Vector<Bool> axes(cSys.nPixelAxes(), False);
   Vector<Int> shape(cSys.nPixelAxes(), 0);
   shape(0) = 64;
   shape(1) = 128;
   shape(2) = 256;

// Induced failures

   {

// No axes

      Bool failed = False;
      try {
         pC = cSys.makeFourierCoordinate (axes, shape);
      } catch (AipsError x) {
        failed = True;
      } 
      if (!failed) {
         throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
      }
      delete pC;
   }

   {

// Illegal axes

      Bool failed = False;
      Vector<Bool> axes2(20, True);
      try {
         pC = cSys.makeFourierCoordinate (axes2, shape);
      } catch (AipsError x) {
        failed = True;
      } 
      if (!failed) {
         throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
      }
      delete pC;
   }

   {

// Illegal shape

      Bool failed = False;
      Vector<Int> shape2(20, 100);
      try {
         pC = cSys.makeFourierCoordinate (axes, shape2);
      } catch (AipsError x) {
        failed = True;
      } 
      if (!failed) {
         throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
      }
      delete pC;
   }

// These should work.   All the underlying coordinates have been 
// tested, so just make sure the right coordinate has been replaced

   {
      axes.set(False);
      axes(0) = True;
      pC = cSys.makeFourierCoordinate (axes, shape);
//
      Vector<String> units2 = pC->worldAxisUnits();
      Vector<String> names2 = pC->worldAxisNames();
      for (uInt i=0; i<cSys.nPixelAxes(); i++) {
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
      axes.set(True);
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
