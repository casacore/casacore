//# tSpectralCoordinate.cc: Test program for SpectralCoordinate
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
#include <aips/Logging/LogIO.h> 
#include <aips/Logging/LogOrigin.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Mathematics/Math.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/Assert.h>

#include <iostream.h>

SpectralCoordinate makeLinearCoordinate(MFrequency::Types type,
                                        Double& crval,
                                        Double& cdelt,
                                        Double& crpix,
                                        Double& restFreq);

SpectralCoordinate makeNonLinearCoordinate (MFrequency::Types type,
                                            Vector<Double>& freqs,
                                            Double& restFreq);


int main()
{
   try {

      Double f0, finc, refchan, restFreq;

      Vector<Double> freqs;
      Matrix<Double> xform(1,1); xform(0,0) = 1.0;
//
      Vector<String> names(1); names(0) = "Frequency";
      Vector<String> units(1); units(0) = "Hz";
      Vector<Double> crpix(1), crval(1), cdelt(1);

// Constructors

      {
         SpectralCoordinate lc = 
            makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
      }
      {
         SpectralCoordinate lc = 
            makeNonLinearCoordinate(MFrequency::TOPO, freqs, restFreq);
      }

// Test near function

     {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         SpectralCoordinate lc2 = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         if (!lc.near(&lc2)) {
            throw(AipsError("Failed near test 1"));
         }
         Vector<Int> excludeAxes(1, 1);
         if (!lc.near(&lc2, excludeAxes)) {
            throw(AipsError("Failed near test 2"));
         }
     } 

// Test Quantum constructor interfaces
   
     {
        Double crval = 1.4e9;
        Double crpix = 1.0;
        Double cdelt = 1.0e3;
        Double restFreq = 1.41e9;
        SpectralCoordinate sc1(MFrequency::TOPO, crval, cdelt, crpix, restFreq);
//
        Quantum<Double> crval2(100*crval, "cHz");
        Quantum<Double> cdelt2(100*cdelt, "cHz");
        Quantum<Double> restFreq2(100*restFreq, "cHz");
        SpectralCoordinate sc2(MFrequency::TOPO, crval2, cdelt2, crpix, restFreq2);
//
        if (!sc1.near(&sc2)) {  
           throw(AipsError(String("Quantum interface (1) constructor failed consistencey test")));
        }
      }

      {
        Vector<Double> freqs(3);
        freqs(0) = 1.4e9; freqs(1) = 1.5e9; freqs(2) = 1.7e9;
        Double restFreq = 1.41e9;
        SpectralCoordinate sc1(MFrequency::TOPO, freqs, restFreq);
//
        Quantum<Vector<Double> > freqs2(100.0*freqs, "cHz");
        Quantum<Double> restFreq2(100.0*restFreq, "cHz");
        SpectralCoordinate sc2(MFrequency::TOPO, freqs2, restFreq2);
//
        if (!sc1.near(&sc2)) {  
           throw(AipsError(String("Quantum interface (2) constructor failed consistencey test")));
        }
      }


// Test copy constructor

     {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         SpectralCoordinate lc2(lc);
         if (!lc.near(&lc2)) {
            throw(AipsError("Failed copy constructor test"));
         }
     } 

// Test assignment

     {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         SpectralCoordinate lc2;
         lc2 = lc;
         if (!lc.near(&lc2)) {
            throw(AipsError("Failed assignment test"));
         }
     } 

// Test member functions
   
     {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         crpix(0) = refchan;
         crval(0) = f0;
         cdelt(0) = finc;
//
         if (lc.type() != Coordinate::SPECTRAL) {
            throw(AipsError("Failed type test"));
         }
         if (lc.showType() != "Spectral") {
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
         if (!near(restFreq, lc.restFrequency())) {
            throw(AipsError("Failed rest frequency recovery test"));
         }
//
         if (lc.frequencySystem() != MFrequency::TOPO) {
            throw(AipsError("Failed frequency system recovery test"));
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
         units(0) = "GHz";
         if (!lc.setWorldAxisUnits(units)) {
            throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
         }
         if (!allEQ(units, lc.worldAxisUnits())) {
            throw(AipsError("Failed world axis units set/recovery test"));
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
         restFreq = 1.3e9;
         if (!lc.setRestFrequency(restFreq)) {
            throw(AipsError(String("Failed to set rest frequency because") + lc.errorMessage()));
         }
         if (!near(restFreq, lc.restFrequency())) {
            throw(AipsError("Failed rest frequency set/recovery test"));
         }
//
         lc.setFrequencySystem(MFrequency::LSR);
         if (lc.frequencySystem() != MFrequency::LSR) {
            throw(AipsError("Failed frequency system set/recovery test"));
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
         Quantum<Double> valq(20.12345, String(units(0)));
         String str = lc.format(unit, Coordinate::FIXED, val, 0,
                   True, 4);
         String str2 = lc.formatQuantity(unit, Coordinate::FIXED, valq, 0,
                   True, 4);
         if (str != "20.1234" || str2 != "20.1234") {
            throw(AipsError("Failed format test 1"));
         }
         str = lc.format(unit, Coordinate::SCIENTIFIC, val, 0,
                   True, 4);
         str2 = lc.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, 0,
                   True, 4);
         if (str != "2.0123e+01" || str2 != "2.0123e+01") {
            throw(AipsError("Failed format test 2"));
         }
//
         {
            Vector<Double> pixelValues = lc.pixelValues();
            Vector<Double> worldValues = lc.worldValues();
            if (pixelValues.nelements()!=0 || worldValues.nelements()!=0) {
               throw(AipsError("Failed linear pixel/worldValues function test"));
            }
         }
//
         {
            SpectralCoordinate lc2 = 
               makeNonLinearCoordinate(MFrequency::TOPO, freqs, restFreq);
            Vector<Double> pixelValues = lc2.pixelValues();
            Vector<Double> worldValues = lc2.worldValues();
            if (!allNear(worldValues, freqs, 1e-6)) {
               throw(AipsError("Failed non-linear worldValues function test"));
            }
            Vector<Double> pixels2(freqs.nelements());
            for (uInt i=0; i<pixels2.nelements(); i++) pixels2(i) = Double(i);
            if (!allNear(pixelValues, pixels2, 1e-6)) {
               throw(AipsError("Failed non-linear pixelValues function test"));
            }
         }

      }

//
// Test Fourier Coordinate.  Hard to do much with it.    SpectralCoordinate
// just passes the work on to TabularCoordinate
//
     {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         SpectralCoordinate lc2 = makeNonLinearCoordinate(MFrequency::TOPO, freqs, restFreq);
//
         AlwaysAssert(lc.nPixelAxes()==1, AipsError);
         Vector<Bool> axes(1, True);
         Vector<Int> shape(1);
         shape(0) = 128;
            
// All axes
               
         {   
            Coordinate* pC = lc.makeFourierCoordinate (axes, shape);
//
            Vector<String> units2 = pC->worldAxisUnits();
            Vector<String> names2 = pC->worldAxisNames();
            Vector<Double> crval2 = pC->referenceValue();
            Vector<Double> crpix2 = pC->referencePixel();
            if (units2(0)!=String("s")) {
               throw(AipsError("makeFourierCoordinate (1) failed units test"));
            }
            if (names2(0)!=String("Time")) {
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

// No axes

         {
            axes.set(False);
            Bool failed = False;
            Coordinate* pC = 0;
            try {
               pC = lc.makeFourierCoordinate (axes, shape);
            } catch (AipsError x) {
               failed = True;
            } end_try;
            if (!failed) {
               throw(AipsError("Failed to induce forced error (1) in makeFourierCoordinate"));
            }
            delete pC;
         }

// Non linear

         {
            axes.set(True);
            Bool failed = False;
            Coordinate* pC = 0;
            try {
               pC = lc2.makeFourierCoordinate (axes, shape);
            } catch (AipsError x) {
               failed = True;
            } end_try;
            if (!failed) {
               throw(AipsError("Failed to induce forced error (2) in makeFourierCoordinate"));
            }
            delete pC;
         }
      }
     

// Test conversion

     {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         Vector<Double> pixel(1), world;
         pixel(0) = 12.2;
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
         Double pix, wrld;
         pix = 12.2;
         if (!lc.toWorld(wrld, pix)) {
            throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
         }
//
         Double pix2;
         if (!lc.toPixel(pix2, wrld)) {
            throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
         }
         if (!allNear(pix2, pix, 1e-6)) {
               throw(AipsError("Coordinate conversion reflection failed"));
         }
     }
     {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         Double pixel;
         MFrequency world;
         pixel = 12.2;
         if (!lc.toWorld(world, pixel)) {
            throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
         }
//
         Double pixel2;
         if (!lc.toPixel(pixel2, world)) {
            throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
         }
         if (!allNear(pixel2, pixel, 1e-6)) {
               throw(AipsError("Coordinate conversion reflection failed"));
         }
      }
      {
         SpectralCoordinate lc = 
            makeNonLinearCoordinate(MFrequency::TOPO, freqs, restFreq);
//
         Vector<Double> pixel(1), world;
         pixel(0) = 12.2;
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
         Double pix, wrld;
         pix = 12.2;
         if (!lc.toWorld(wrld, pix)) {
            throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
         }
//
         Double pix2;
         if (!lc.toPixel(pix2, wrld)) {
            throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
         }
         if (!allNear(pix2, pix, 1e-6)) {
               throw(AipsError("Coordinate conversion reflection failed"));
         }
      }
      {
         SpectralCoordinate lc = 
            makeNonLinearCoordinate(MFrequency::TOPO, freqs, restFreq);
//
         Double pixel;
         MFrequency world;
         pixel = 12.2;
         if (!lc.toWorld(world, pixel)) {
            throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
         }

// Adjust coordinate units to make test harder

         Vector<String> units = lc.worldAxisUnits();
         units.set("KHz");
         lc.setWorldAxisUnits(units);
//
         Double pixel2;
         if (!lc.toPixel(pixel2, world)) {
            throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
         }
         if (!allNear(pixel2, pixel, 1e-6)) {
               throw(AipsError("Coordinate conversion reflection failed"));
         }
      }
      {
         SpectralCoordinate lc = 
            makeNonLinearCoordinate(MFrequency::TOPO, freqs, restFreq);
//
         Double pixel;
         MVFrequency world;
         pixel = 12.2;
         if (!lc.toWorld(world, pixel)) {
            throw(AipsError(String("toWorld conversion failed because ") + lc.errorMessage()));
         }

// Adjust coordinate units to make test harder

         Vector<String> units = lc.worldAxisUnits();
         units.set("KHz");
         lc.setWorldAxisUnits(units);
//
         Double pixel2;
         if (!lc.toPixel(pixel2, world)) {
            throw(AipsError(String("toPixel conversion failed because ") + lc.errorMessage()));
         }
         if (!allNear(pixel2, pixel, 1e-6)) {
               throw(AipsError("Coordinate conversion reflection failed"));
         }
      }
//
// Test record saving
//
      {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         TableRecord rec;
         if (!lc.save(rec, "linear")) {
            throw(AipsError("Coordinate saving to Record failed"));  
         }  
         SpectralCoordinate* plc = SpectralCoordinate::restore(rec, "linear");
         if (!plc->near(&lc, 1e-6)) {
            throw(AipsError("Coordinate reflection through record interface failed"));  
         }
         delete plc;
      }
//
// Test clone
//
      {
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         Coordinate* plc = lc.clone();
         if (!plc->near(&lc, 1e-6)) {
            throw(AipsError("Clone function failed"));  
         }
         delete plc;
      }
//
// Test FITS conversion
//
/*
      {
         LogOrigin or("tSpectralCoordinate", "main()", WHERE);
         LogIO os(or);
//
         SpectralCoordinate lc = makeLinearCoordinate(MFrequency::TOPO, f0, finc, refchan, restFreq);
         TableRecord rec;
//         lc.toFITS(rec, 0, os, False, True);
//
         SpectralCoordinate lc2;
         String errMsg;
         if (!SpectralCoordinate::fromFITS(lc2, errMsg, rec, 0, os,  True)) {
            throw(AipsError(String("fromFITS function failed because") + errMsg));  
         }
         if (!lc.near(&lc2, 1e-6)) {
            throw(AipsError("FITS reflection failed"));  
         }
      }
*/
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}

SpectralCoordinate makeLinearCoordinate (MFrequency::Types type,
                                         Double& f0,
                                         Double& finc,
                                         Double& refchan,
                                         Double& restFreq)
{
   refchan = 10.5;
   finc = 4e6;
   f0 = 1.4e9;
   restFreq = 1.420405752E9;
//
   return SpectralCoordinate(type, f0, finc, refchan, restFreq);
}



SpectralCoordinate makeNonLinearCoordinate (MFrequency::Types type,
                                            Vector<Double>& freqs,
                                            Double& restFreq)
{
   restFreq = 1.420405752E9;
   freqs.resize(5);
   freqs(0) = 1.4e9;
   freqs(1) = 1.41e9;
   freqs(2) = 1.43e9;
   freqs(3) = 1.44e9;
   freqs(4) = 1.47e9;

//
   return SpectralCoordinate(type, freqs, restFreq);
}


