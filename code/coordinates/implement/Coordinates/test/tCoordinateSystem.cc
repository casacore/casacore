//# tCoordinateSystem.cc: Test program for CoordinateSystem
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
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/TabularCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableRecord.h>

#include <iostream.h>

DirectionCoordinate makeDirectionCoordinate();
SpectralCoordinate makeSpectralCoordinate ();
StokesCoordinate makeStokesCoordinate(Bool silly=True);
LinearCoordinate makeLinearCoordinate();
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
         if (!cSys1.near(&cSys2)) {
            String msg = String("Failed near test 1 because ") +
                         cSys1.errorMessage();
            throw(AipsError(msg));
         }
         Vector<Int> excludeAxes(2);
         excludeAxes(0) = 0;
         excludeAxes(1) = 2;
         if (!cSys1.near(&cSys2, excludeAxes)) {
            String msg = String("Failed near test 2 because ") +
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

  } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

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
       if (!cSys.near(&cSys2)) {
            String msg = String("Failed copy constructor test because ") +
                         cSys.errorMessage();
            throw(AipsError(msg));
       }
   } 

// Test assignment

   {
       CoordinateSystem cSys2;
       cSys2 = cSys;
       if (!cSys.near(&cSys2)) {
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
   if (!dC.near(&cSys.directionCoordinate(uInt(iC)))) {
      msg = String("Failed directionCoordinate test because ") +
            dC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::SPECTRAL);
   if (iC != Int(iSpC)) {
      throw(AipsError("Failed findCoordinate test 2"));
   }
   if (!spC.near(&cSys.spectralCoordinate(uInt(iC)))) {
      msg = String("Failed spectralCoordinate test because ") +   
            spC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::TABULAR);
   if (iC != Int(iTC)) {
      throw(AipsError("Failed findCoordinate test 3"));
   }
   if (!tC.near(&cSys.tabularCoordinate(uInt(iC)))) {
      msg = String("Failed tabularCoordinate test because ") +   
            tC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::STOKES);
   if (iC != Int(iStC)) {
      throw(AipsError("Failed findCoordinate test 4"));
   }
   if (!stC.near(&cSys.stokesCoordinate(uInt(iC)))) {
      msg = String("Failed stokesCoordinate test because ") +   
                   stC.errorMessage();
      throw(AipsError(msg));
   }
//
   iC = cSys.findCoordinate(Coordinate::LINEAR);
   if (iC != Int(iLC)) {
      throw(AipsError("Failed findCoordinate test 5"));
   }
   if (!lC.near(&cSys.linearCoordinate(uInt(iC)))) {
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
      if (!allEQ(worldAxisNames2.ac(), worldAxisNames3.ac())) {
         throw(AipsError("Failed world axis name recovery test"));
      }
      if (!allEQ(worldAxisUnits2.ac(), worldAxisUnits3.ac())) {
         throw(AipsError("Failed world axis unit recovery test"));
      }
      if (!allNear(refValues2.ac(), refValues3.ac(), 1e-6)) {
         throw(AipsError("Failed reference pixel recovery test"));
      }
      if (!allNear(inc2.ac(), inc3.ac(), 1e-6)) {
         throw(AipsError("Failed increment recovery test"));
      }
   }
//
   worldAxisNames(0) = "Horsies";
   if (!cSys.setWorldAxisNames(worldAxisNames)) {
      throw(AipsError(String("Failed to set world axis name because") + cSys.errorMessage()));
   }
   if (!allEQ(worldAxisNames.ac(), cSys.worldAxisNames().ac())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
   refValues(0) = refValues(0)*2;
   if (!cSys.setReferenceValue(refValues)) {
      throw(AipsError(String("Failed to set reference value because") 
            + cSys.errorMessage()));
   }
   if (!allNear(refValues.ac(), cSys.referenceValue().ac(), 1e-6)) {
      throw(AipsError("Failed reference value set/recovery test"));
   }
//
   inc(0) = inc(0)*2;
   if (!cSys.setIncrement(inc)) {
      throw(AipsError(String("Failed to set increment because") 
            + cSys.errorMessage()));
   }
   if (!allNear(inc.ac(), cSys.increment().ac(), 1e-6)) {
      throw(AipsError("Failed increment set/recovery test"));
   }
 //
   iC = cSys.findCoordinate(Coordinate::DIRECTION);
   Vector<Int> worldAxes = cSys.worldAxes(iC);
   worldAxisUnits(worldAxes(0)) = "deg";
   worldAxisUnits(worldAxes(1)) = "arcmin";
   if (!cSys.setWorldAxisUnits(worldAxisUnits, True)) {
      throw(AipsError(String("Failed to set axis units because") 
            + cSys.errorMessage()));
   }
   if (!allEQ(worldAxisUnits.ac(), cSys.worldAxisUnits().ac())) {
      throw(AipsError("Failed axis units set/recovery test"));
   }
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
      if (!allNear(refPixels2.ac(), refPixels3.ac(), 1e-6)) {
         throw(AipsError("Failed reference pixel recovery test"));
      }
   }
   refPixels(0) = refPixels(0)*2;
   if (!cSys.setReferencePixel(refPixels)) {
      throw(AipsError(String("Failed to set reference pixel because") 
            + cSys.errorMessage()));
   }
   if (!allNear(refPixels.ac(), cSys.referencePixel().ac(), 1e-6)) {
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
   if (!allNear(xform.ac(), cSys.linearTransform().ac(), 1e-6)) {
      throw(AipsError("Failed linear transform set/recovery test"));
   }
//
// Test FITS interface.  Do this with a CS without a TabularCoordinate
// because that is not reflected back by the FITS  conversion
//
   {
      CoordinateSystem cSys2;
      cSys2.addCoordinate(makeDirectionCoordinate());
      cSys2.addCoordinate(makeStokesCoordinate(False));
      cSys2.addCoordinate(makeSpectralCoordinate());
      cSys2.addCoordinate(makeLinearCoordinate());
//
      TableRecord rec;
      IPosition shape;
      if (!cSys2.toFITSHeader(rec, shape, True, 'c', False,
                        True, True)) {
         throw(AipsError(String("Failed to convert to FITS header because") 
            + cSys2.errorMessage()));
      }
//
/*
      for (uInt j=0; j<rec.nfields(); j++) {
        cout << "field " << rec.name(j) << " is of type " << rec.type(j) << endl;
      }
      cout << "crval=" << rec.asArrayDouble("crval").ac() << endl;
      cout << "crpix=" << rec.asArrayDouble("crpix").ac() << endl;
      cout << "cdelt=" << rec.asArrayDouble("cdelt").ac() << endl;      
*/
//
      CoordinateSystem cSys3;
      if (!CoordinateSystem::fromFITSHeader(cSys3, rec, True, 'c')) {
         throw(AipsError("Failed to convert from FITS header"));
      }
      if (!cSys2.near(&cSys3)) {
         msg = String("Failed to/fromFITS consistency test because ") +   
                      cSys2.errorMessage();
         throw(AipsError(msg));
      }
   }
//   
// Test record saving
//
   TableRecord rec;
   if (!cSys.save(rec, "coordsys")) {
      throw(AipsError("Saving to Record failed"));  
   }  
   CoordinateSystem* pcSys = CoordinateSystem::restore(rec, "coordsys");
   if (!pcSys->near(&cSys, 1e-6)) {
      throw(AipsError("Reflection through record interface failed"));  
   }
   delete pcSys;
//
// Test clone
//
   Coordinate* pcSys2 = cSys.clone();
   if (!pcSys2->near(&cSys, 1e-6)) {
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
      if (!cSys2.near(&cSys3, 1e-6)) {
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
      cSys2.addCoordinate(makeDirectionCoordinate());
      cSys2.addCoordinate(makeStokesCoordinate(False));
      cSys2.replaceCoordinate(makeLinearCoordinate(), 0);
      if (cSys2.type(0) != Coordinate::LINEAR ||
          cSys2.type(1) != Coordinate::STOKES ||
          cSys2.nCoordinates()!=2) {
        throw(AipsError("Coordinate replacemenet test failed"));  
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
// SubImage.  Any test I can do really just replicates the function
// code so it's pretty useless.
//
   {
      cSys.restoreOriginal();
      Vector<Int> originShift(cSys.nPixelAxes());
      Vector<Int> pixinc(cSys.nPixelAxes());
      originShift = 10.0;
      pixinc = 1.0;
      CoordinateSystem cSys2 = cSys.subImage(originShift, pixinc);
      if (cSys.nCoordinates() != cSys2.nCoordinates()) {
         throw(AipsError("Failed originShift creation"));
      }
//
      Vector<Double> pixel, pixel2;
      cSys.toPixel(pixel, cSys.referenceValue());
      cSys2.toPixel(pixel2, cSys2.referenceValue());
      pixel.ac() -= 10.0;
//
      if (!allNear(pixel.ac(), pixel2.ac(), 1e-6)) {
         throw(AipsError("Failed originShift test"));
      }   
   }
}

void doit2 (CoordinateSystem& cSys)

{
//
// Test conversion
//
   Vector<Double> pixel(cSys.referencePixel()), world;
   if (!cSys.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!allNear(world.ac(), cSys.referenceValue().ac(), 1e-6)) {
         throw(AipsError("Coordinate conversion gave wrong results"));
   }

   if (!cSys.toPixel(pixel, world)) {
      throw(AipsError(String("toPixel conversion failed because ")
             + cSys.errorMessage()));
   }
   if (!allNear(pixel.ac(), cSys.referencePixel().ac(), 1e-6)) {
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
   if (!allNear(pixel2.ac(), pixel.ac(), 1e-6)) {
      throw(AipsError("Coordinate reflection gave wrong results"));
   }
//
   pixel = 2.0;
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
   if (!allNear(world.ac(), world2.ac(), 1e-6)) {
      throw(AipsError("toWorld consistency test failed"));
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
      cSys.setWorldAxisUnits(worldAxisUnits, True);
//
      String unit;
      Double val = 0.12343;
      String str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(0),
                               True, 4);
      if (str != "0.1234") {
         throw(AipsError("Failed format test 1"));
      }
      str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(1),
                               True, 4);
      if (str != "0.1234") {
         throw(AipsError("Failed format test 2"));
      }
//
      str = cSys.format(unit, Coordinate::SCIENTIFIC, val, worldAxes(0),
                      True, 4);
      if (str != "1.2343e-01") {
         throw(AipsError("Failed format test 3"));
      }
      str = cSys.format(unit, Coordinate::SCIENTIFIC, val, worldAxes(1),
                      True, 4);
      if (str != "1.2343e-01") {
         throw(AipsError("Failed format test 4"));
      }
   }
   {
      Int iSpC = cSys.findCoordinate(Coordinate::SPECTRAL);
      Vector<Int> worldAxes = cSys.worldAxes(iSpC);
//
      String unit;
      Double val = 0.12343;
      String str = cSys.format(unit, Coordinate::FIXED, val, worldAxes(0),
                               True, 4);
      if (str != "0.1234") {
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
      if (!allEQ(newWorldAxes.ac(), cSys.worldAxes(iDC).ac()) ||
          !allEQ(newPixelAxes.ac(), cSys.pixelAxes(iDC).ac())) {
         throw(AipsError("Failed transposition test"));
      }
   }
//
// World map
//
   {
      CoordinateSystem cSys2;
      cSys2.addCoordinate(makeStokesCoordinate(False));
      cSys2.addCoordinate(makeDirectionCoordinate());
      cSys2.addCoordinate(makeLinearCoordinate());
      CoordinateSystem cSys3 = cSys2;
//
      Vector<Int> worldAxisMap, worldAxisTranspose;
      if (!cSys2.worldMap(worldAxisMap, worldAxisTranspose, cSys3)) {
         throw(AipsError("Failed to make world map 1"));
      }
      Vector<Int> wMap(cSys2.nWorldAxes()), wTranspose(cSys2.nWorldAxes());
      for (uInt i=0; i<cSys2.nWorldAxes(); i++) {
         wMap(i) = i; 
         wTranspose(i) = i; 
      }
      if (!allEQ(wMap.ac(), worldAxisMap.ac()) ||
          !allEQ(wTranspose.ac(), worldAxisTranspose.ac())) {
         throw(AipsError("Failed worldMap test 1"));
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
      if (!cSys2.worldMap(worldAxisMap, worldAxisTranspose, cSys3)) {
         throw(AipsError("Failed to make world map 2"));
      }
      Vector<Int> newMap(wMap.copy());
      Vector<Int> newTranspose(worldAxisTranspose.copy());
      newMap(worldAxes(0)) = newWorldOrder(worldAxes(0));
      newMap(worldAxes(1)) = newWorldOrder(worldAxes(1));
      newTranspose(worldAxes(0)) = newWorldOrder(worldAxes(0));
      newTranspose(worldAxes(1)) = newWorldOrder(worldAxes(1));
//
      if (!allEQ(newMap.ac(), worldAxisMap.ac()) ||
          !allEQ(newTranspose.ac(), worldAxisTranspose.ac())) {
         throw(AipsError("Failed worldMap test 2"));
      }
   }
}


DirectionCoordinate makeDirectionCoordinate()
{
   MDirection::Types type = MDirection::J2000;
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
   return DirectionCoordinate(type, proj, crval(0), crval(1),
                              cdelt(0), cdelt(1),
                              xform, crpix(0), crpix(1));
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
 

LinearCoordinate makeLinearCoordinate ()
{
   Vector<String> names(2);
   Vector<String> units(2);
   Vector<Double> crpix(2);
   Vector<Double> crval(2);
   Vector<Double> cdelt(2);
   Matrix<Double> xform(2,2);
//
   names(0) = "doggies"; names(1) = "fishies";
   units(0) = "m"; units(1) = "rad";
   crpix(0) = 10.0; crpix(1) = 20.0; 
   cdelt(0) = 1.0; cdelt(1) = 2.0;
   crval(0) = 10.0; crval(1) = 20;
   xform = 0.0; xform.diagonal() = 1.0;
//
   return LinearCoordinate(names, units, crval, cdelt,
                           xform, crpix);
}


TabularCoordinate makeTabularCoordinate()
{
   String axisName = "Doggies";
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
