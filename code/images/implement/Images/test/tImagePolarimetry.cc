//# tImagePolarimetry.cc: test ImagePolarimetry class
//# Copyright (C) 1996,1997,1998,1999,2000
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
// 
//
#include <trial/Images/ImagePolarimetry.h>

#include <aips/aips.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/TempImage.h>
#include <trial/Images/ImageExpr.h>
#include <trial/Images/SubImage.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Random.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/QC.h>
#include <aips/Tasking/Aipsrc.h>
#include <trial/Tasking/PGPlotter.h>
#include <aips/Utilities/String.h>

#include <iostream.h>

void addNoise (Array<Float>& slice, Normal& noiseGen);
ImageInterface<Float>* makeQUImage (Double& sigma, Double pa0, Double rm, 
                                    uInt nchan, Double f0, Double dF);
void setStokes (ImageInterface<Float>*& pIm, Float i, Float q, Float u, Float v, IPosition shape);
void setStokes (ImageInterface<Float>*& pIm, uInt stokesAxis, 
                uInt spectralAxis, Float rm, Float pa0);
void traditionalRotationMeasure (Double rm, Double rmFg, Double rmMax, Double pa0,
                                 Double dF, Int nchan,
                                 LogIO& os, const String& plotter);
void fourierRotationMeasure (Double rm, const String& plotter, LogIO& os);

main (int argc, char **argv)
{
try {


   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

   inputs.create("plotter", "", "plotter");
   inputs.create("rm", "-9999.0", "rm (rad/m/m)");
   inputs.create("rmmax", "-1.0", "rmmax (rad/m/m)");
   inputs.create("rmfg", "0.0", "rmfg (rad/m/m)");
   inputs.create("pa0", "0.0", "pa0 (deg)");
   inputs.create("dF", "128e6", "dF (Hz)");
   inputs.create("nchan", "32", "nchan");
   inputs.readArguments(argc, argv);
   const String plotter = inputs.getString("plotter");
   const Double rm = inputs.getDouble("rm");            // rad/m/m
   const Double rmMax = inputs.getDouble("rmmax");            // rad/m/m
   const Double rmFg = inputs.getDouble("rmfg");            // rad/m/m
   const Double pa0 = inputs.getDouble("pa0");           // deg
   const Double dF = inputs.getDouble("dF");           // Hz
   const Int nchan = inputs.getInt("nchan");     

//
   LogOrigin or("tImagePolarimetry", "main()", WHERE);
   LogIO os(or);

// Stokes values

   Float qVal = 2.0;
   Float uVal = -3.0;
   Float vVal = 0.1;
   Float iVal = sqrt(qVal*qVal + uVal*uVal + vVal*vVal);

// First do what we can without noise

   {
     os << LogIO::NORMAL << "Tests with no noise" << LogIO::POST;
     const uInt size = 10;
     IPosition shape(4,size,size,4,2);
     IPosition shape1(4,size,size,1,2);
     ImageInterface<Float>* pIm = new TempImage<Float>(shape, CoordinateUtil::defaultCoords4D());
     setStokes(pIm, iVal, qVal, uVal, vVal, shape1);
//
     ImagePolarimetry pol(*pIm);
     AlwaysAssert(pol.shape()==shape,AipsError);
     AlwaysAssert(pol.coordinates().near(pIm->coordinates()),AipsError);
     AlwaysAssert(pol.isMasked()==pIm->isMasked(),AipsError);
//
     CoordinateSystem dCS;
     IPosition shape2 = pol.singleStokesShape(dCS, Stokes::I);
     AlwaysAssert(shape2.isEqual(shape1),AipsError);
//
     pol.summary(os);
     {
        ImageExpr<Float> ie = pol.stokesI();
        AlwaysAssert(allNear(ie.get(), iVal, 1e-6), AipsError);
        ImageExpr<Float> ie2 = pol.stokes(ImagePolarimetry::I);
        AlwaysAssert(allNear(ie2.get(), iVal, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.stokesQ();
        AlwaysAssert(allNear(ie.get(), qVal, 1e-6), AipsError);
        ImageExpr<Float> ie2 = pol.stokes(ImagePolarimetry::Q);
        AlwaysAssert(allNear(ie2.get(), qVal, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.stokesU();
        AlwaysAssert(allNear(ie.get(), uVal, 1e-6), AipsError);
        ImageExpr<Float> ie2 = pol.stokes(ImagePolarimetry::U);
        AlwaysAssert(allNear(ie2.get(), uVal, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.stokesV();
        AlwaysAssert(allNear(ie.get(), vVal, 1e-6), AipsError);  
        ImageExpr<Float> ie2 = pol.stokes(ImagePolarimetry::V);
        AlwaysAssert(allNear(ie2.get(), vVal, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.linPolInt(False, 0.0);
        Float val = sqrt(qVal*qVal + uVal*uVal);
        AlwaysAssert(allNear(ie.get(), val, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.totPolInt(False, 0.0);
        Float val = sqrt(qVal*qVal + uVal*uVal + vVal*vVal);
        AlwaysAssert(allNear(ie.get(), val, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.linPolPosAng(True);
        Float val =  0.5 * atan2(uVal,qVal);
        AlwaysAssert(allNear(ie.get(), val, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.fracLinPol(False, 0.0);
        Float val = sqrt(qVal*qVal + uVal*uVal) / iVal;
        AlwaysAssert(allNear(ie.get(), val, 1e-6), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.fracTotPol(False, 0.0);
        Float val = sqrt(qVal*qVal + uVal*uVal +vVal*vVal) / iVal;
        AlwaysAssert(allNear(ie.get(), val, 1e-6), AipsError);
     }
     delete pIm; pIm = 0;
   }


// Error tests. Make an image of noise plus signal

   {
     os << LogIO::NORMAL << "Test error computations" << LogIO::POST;
     const uInt size = 256;
     IPosition shape(4,size,size,4,2);
     IPosition shape1(4,size,size,1,2);
     ImageInterface<Float>* pIm = new TempImage<Float>(shape, CoordinateUtil::defaultCoords4D());
     setStokes(pIm, iVal, qVal, uVal, vVal, shape1);
//
     MLCG generator;
     Double sigma = 0.01 * iVal;
     Normal noiseGen(0.0, sigma*sigma, &generator);
//
     Array<Float> slice = pIm->get();
     addNoise(slice, noiseGen);
     pIm->put(slice);
//
     ImagePolarimetry pol(*pIm);
//
     {
        Float sig = pol.sigma(1000.0);
        AlwaysAssert(near(sig, sigma, 1e-2), AipsError);
     }
     {
        Float sig = pol.sigmaStokesI(1000.0);
        AlwaysAssert(near(sig, sigma, 1e-2), AipsError);
     }
     {
        Float sig = pol.sigmaStokesQ(1000.0);
        AlwaysAssert(near(sig, sigma, 1e-2), AipsError);
     }
     {
        Float sig = pol.sigmaStokesU(1000.0);
        AlwaysAssert(near(sig, sigma, 1e-2), AipsError);
     }
     {
        Float sig = pol.sigmaStokesV(1000.0);
        AlwaysAssert(near(sig, sigma, 1e-2), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.linPolInt(True, 10.0, -1.0);
        Float mean2 = mean(ie.get());
        Float p = sqrt(qVal*qVal + uVal*uVal - sigma*sigma);
        AlwaysAssert(near(mean2, p, 1e-2), AipsError);
     }
     {
        Float err = pol.sigmaLinPolInt(10.0, -1.0);
        AlwaysAssert(near(err, sigma, 1e-2), AipsError);
     }
     {
        ImageExpr<Float> ie = pol.totPolInt(True, 10.0, -1.0);
        Float mean2 = mean(ie.get());
        Float p = sqrt(qVal*qVal + uVal*uVal +vVal*vVal - sigma*sigma);
        AlwaysAssert(near(mean2, p, 1e-2), AipsError);
     }
     {
        Float err = pol.sigmaTotPolInt(10.0, -1.0);
        AlwaysAssert(near(err, sigma, 1e-2), AipsError);
     }
     {
        Float p = sqrt(qVal*qVal + uVal*uVal);
        Float m = p / iVal;
        ImageExpr<Float> ie = pol.fracLinPol(True, 10.0, -1.0);
        Float mean2 = mean(ie.get());
        AlwaysAssert(near(mean2, m, 1e-2), AipsError);
//
        Float sigp = sigma;
        Float sigi = sigma;
        Float ee = m * sqrt( (sigp*sigp/p/p) + (sigi*sigi/iVal/iVal));
        ImageExpr<Float> err = pol.sigmaFracLinPol(10.0, -1.0);
        mean2 = mean(err.get());
        AlwaysAssert(near(mean2, ee, 1e-2), AipsError);
     }
     {
        Float p = sqrt(qVal*qVal + uVal*uVal + vVal*vVal);
        Float m = p / iVal;
        ImageExpr<Float> ie = pol.fracTotPol(True, 10.0, -1.0);
        Float mean2 = mean(ie.get());
        AlwaysAssert(near(mean2, m, 1e-2), AipsError);
//
        Float sigp = sigma;
        Float sigi = sigma;
        Float ee = m * sqrt( (sigp*sigp/p/p) + (sigi*sigi/iVal/iVal));
        ImageExpr<Float> err = pol.sigmaFracTotPol(10.0, -1.0);
        mean2 = mean(err.get());
        AlwaysAssert(allNear(mean2, ee, 1e-2), AipsError);
     }
     delete pIm;
   }

// Fourier Rotation Measure 

   {
     os << LogIO::NORMAL << "Test Fourier Rotation Measure" << LogIO::POST;
     fourierRotationMeasure(rm, plotter, os);
   }


// Traditional rotation measure

   {
     os << LogIO::NORMAL << "Test traditional Rotation Measure" << LogIO::POST;
     traditionalRotationMeasure(rm, rmFg, rmMax, pa0, dF, nchan, os, plotter);
//
     Double rm0 = 10000.0;
     Double dRm = rm0 / 9.0;
     for (Double x=-rm0; x<rm0; x+=dRm) {
        traditionalRotationMeasure(x, rmFg, x, pa0, dF, nchan, os, plotter);
     }
   }


}   catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     exit(1);
} end_try;
  cout << "ok" << endl;
  exit(0);
}


void addNoise (Array<Float>& slice, Normal& noiseGen)
{
   Bool deleteIt;
   Float* p = slice.getStorage(deleteIt);
   for (uInt i=0; i<slice.nelements(); i++) {
      p[i] += noiseGen();
   }
   slice.putStorage(p, deleteIt);
}


void setStokes (ImageInterface<Float>*& pIm, Float i, Float q, Float u, Float v, IPosition shape)
{
  Array<Float> slice(shape);
//
  slice.set(i);
  pIm->putSlice(slice, IPosition(4,0,0,0,0));   // I
//
  slice.set(q);
  pIm->putSlice(slice, IPosition(4,0,0,1,0));   // Q
//
  slice.set(u);
  pIm->putSlice(slice, IPosition(4,0,0,2,0));   // U
//
  slice.set(v);
  pIm->putSlice(slice, IPosition(4,0,0,3,0));   // V
}


void setStokes (ImageInterface<Float>*& pIm, uInt stokesAxis, 
                uInt spectralAxis, Float rm, Float pa0)
{

// Find spectral coordinate

   const CoordinateSystem& cSys = pIm->coordinates();   
   Int stokesCoord, spectralCoord, iDum;
   cSys.findPixelAxis(stokesCoord, iDum, stokesAxis);
   AlwaysAssert(iDum==0, AipsError);
   cSys.findPixelAxis(spectralCoord, iDum, spectralAxis);
   AlwaysAssert(stokesCoord>=0, AipsError);
   AlwaysAssert(iDum==0, AipsError);
   AlwaysAssert(spectralCoord>=0, AipsError);

// Compute Q and U with frequency.  Set image.

   IPosition shape(pIm->shape());
//
   Double c = QC::c.getValue(Unit("m/s"));
   Double lambdasq;
   SpectralCoordinate sc = cSys.spectralCoordinate(spectralCoord);
   MFrequency freq;
   IPosition blc(4,0);
   IPosition trc(shape-1);
//
   for (Int i=0; i<shape(3); i++) {
      AlwaysAssert(sc.toWorld(freq, Double(i)), AipsError);
      Double fac = c / freq.get(Unit("Hz")).getValue();
      lambdasq = fac*fac;
//
      Double chi = rm*lambdasq + pa0;
      Double q = cos(2*chi);
      Double u = sin(2*chi);
//
      blc(spectralAxis) = i;              // channel
      trc(spectralAxis) = i;
// 
      {
        blc(stokesAxis) = 0;                // Q       
        trc(stokesAxis) = 0;                
        Slicer sl(blc, trc, Slicer::endIsLast);
        SubImage<Float> subImage(*pIm, sl, True);
        subImage.set(q);
      }
      {
        blc(stokesAxis) = 1;                // U
        trc(stokesAxis) = 1;                
        Slicer sl(blc, trc, Slicer::endIsLast);
        SubImage<Float> subImage(*pIm, sl, True);
        subImage.set(u);
      }
   }
}



ImageInterface<Float>* makeQUImage (Double& sigma, Double pa0, Double rm,
                                    uInt nchan, Double f0, Double dF)
{
   CoordinateSystem cSys;
   CoordinateUtil::addDirAxes(cSys);
   Vector<Int> whichStokes(2);
   whichStokes(0) = Stokes::Q;
   whichStokes(1) = Stokes::U;
   StokesCoordinate stc(whichStokes);
   cSys.addCoordinate(stc);

// Make spectral coordinate 

   Double df = dF / (nchan-1);
   Double refpix = 0.0;
   SpectralCoordinate sc(MFrequency::TOPO, f0, df, refpix, f0);
   cSys.addCoordinate(sc);

// Make Q and U image

   IPosition shape(4,1,1,2,nchan);
   ImageInterface<Float>* pIm = new TempImage<Float>(shape, cSys);
//
   uInt stokesAxis = 2;
   uInt spectralAxis = 3;

// Fill image with Q and U. If RM not given, choose so no ambiguity
// between channels

   setStokes(pIm, stokesAxis, spectralAxis, rm, pa0);

// Add some noise or fitting won't work

   Array<Float> slice = pIm->get();
   Float maxVal = max(slice);
   MLCG gen;
   sigma = 0.0001 * maxVal;
   Normal noiseGen(0.0, sigma*sigma, &gen);
   addNoise(slice, noiseGen);
   pIm->put(slice);
//
   return pIm;
}


void traditionalRotationMeasure (Double rm, Double rmFg, Double rmMax, 
                                 Double pa0, Double dF, Int nchan, LogIO& os,
                                 const String& plotter)
{

// Make image with Q and U

   const Double f0 = 1.4e9;

// If RM not given, choose so no ambiguity between channels
     
   if (rm==-9999.0) {
      Double df = dF / Double(nchan);
      Double l1 = QC::c.getValue(Unit("m/s")) / f0;
      Double l2 = QC::c.getValue(Unit("m/s")) / (f0+df);
      rm = C::pi / 2 / (l1*l1 - l2*l2);
   }
   if (rmMax<0) rmMax = 1.1*rm;
   os << "rm, rmMax, rmFg, pa0 = " << rm << ", " << rmMax << ", " << rmFg << ", " << pa0 << LogIO::POST;
   Double sigma;
   pa0 *= C::pi/180.0;  // rad
   ImageInterface<Float>* pIm = makeQUImage(sigma, pa0, rm, nchan, f0, dF);

// Find rm, pa0 and error images

   ImagePolarimetry pol(*pIm);

// Make plot of P.A.

   if (!plotter.empty()) {
      ImageExpr<Float> ie = pol.linPolPosAng(False);
      uInt nchan = ie.shape()(3);
      IPosition blc(ie.ndim(),0);
      IPosition trc(ie.ndim(),0);
      trc(3) = nchan - 1;
      Array<Float> y = ie.getSlice(blc, trc-blc+1, True);
      Vector<Float> yy(y);
      Vector<Float> xx(nchan);
      for (uInt i=0; i<nchan; i++) xx(i) = i;
//
       PGPlotter pl(plotter);
       pl.env(xx(0), xx(nchan-1), yy(nchan-1), yy(0), 0, 0);
       pl.line(xx, yy);
    }

   CoordinateSystem cSysRM;
   Int fAxis, sAxis;
   IPosition shapeRM = pol.rotationMeasureShape(cSysRM, fAxis, sAxis, os, -1);
   ImageInterface<Float>* pRMOut = new TempImage<Float>(shapeRM, cSysRM);
   ImageInterface<Float>* pRMErrOut = new TempImage<Float>(shapeRM, cSysRM);
//
   CoordinateSystem cSysPA;
   IPosition shapePA = pol.positionAngleShape(cSysPA, fAxis, sAxis, os, -1);
   ImageInterface<Float>* pPA0Out = new TempImage<Float>(shapePA, cSysPA);
   ImageInterface<Float>* pPA0ErrOut = new TempImage<Float>(shapePA, cSysPA);
//
   Double maxPaErr = C::pi;
   pol.rotationMeasure(pRMOut, pRMErrOut, pPA0Out, pPA0ErrOut, -1, 
                       Float(rmMax), maxPaErr, Float(sigma), Float(rmFg));
   delete pIm;
//
   IPosition posRM(shapeRM.nelements(),0);
   Float rmFitted = pRMOut->get()(posRM);
   Float rmFittedError = pRMErrOut->get()(posRM);
//
   IPosition posPA(shapePA.nelements(),0);
   Float pa0Fitted = pPA0Out->get()(posPA);
   Float pa0FittedError = pPA0ErrOut->get()(posPA);

   pa0 *= 180.0 / C::pi;
//
   os << "Actual rm, pa0 = " << rm << " rad/m/m, " << pa0 << " deg" << endl;
   os << "Fitted rm, error = " << rmFitted << ", " << rmFittedError << " rad/m/m/" << endl;
   os << "Fitted pa0, error = " << pa0Fitted << ", " << pa0FittedError << " deg" << LogIO::POST;

//
   Float diff = abs(rmFitted-rm);
   if (! (diff < 3.0*rmFittedError)) {
      os << "Fitted Rotation Measure is wrong" << LogIO::EXCEPTION;
   }
   diff = abs(pa0Fitted-pa0);
   if (! (diff < 3.0*pa0FittedError)) {
      diff = abs(pa0Fitted-180.0-pa0);
      if (! (diff < 3.0*pa0FittedError)) {
         os << "Fitted position angle is wrong" << LogIO::EXCEPTION;
      }
   }
//
   delete pRMOut;  
   delete pRMErrOut;
   delete pPA0Out;
   delete pPA0ErrOut;
}


void fourierRotationMeasure (Double rm, const String& plotter, LogIO& os)
{

// Make image with Q and U

     uInt nchan = 256;
     Double f0 = 5.0e9;
     Double dF = 16e6;
     Double pa0 = 0.0;
     Double df = dF / (nchan-1);

// If RM not given, pick middle of range

     if (rm==-9999.0) {
        Double fc = f0 + -df/2.0 + dF/2.0;
        Double lambdac = QC::c.getValue(Unit("m/s")) / fc;
        const Float drm = C::pi * fc / 2.0 / lambdac / lambdac / dF;
        rm = nchan / 4 * drm;
     }
     Double sigma;
     ImageInterface<Float>* pIm = makeQUImage(sigma, pa0, rm, nchan, f0, dF);
     Int spectralAxis = CoordinateUtil::findSpectralAxis(pIm->coordinates());

// Do it

     ImagePolarimetry pol(*pIm);
     CoordinateSystem cSys;
     IPosition shape1 = pol.singleStokesShape(cSys, Stokes::Plinear);
     TempImage<Complex> polFFT(shape1, cSys);
     pol.fourierRotationMeasure(polFFT, False);       

// Where do we expect peak ?

     const CoordinateSystem& cSys2 = polFFT.coordinates();
     Double rminc = cSys2.increment()(spectralAxis);           
     Double rmrefpix = cSys2.referencePixel()(spectralAxis);      
     Int idx = ifloor((rm + rminc/2) / rminc + rmrefpix);

// Make sure peak in correct place of amplitude spectrum

     LatticeExprNode node(abs(polFFT));
     LatticeExpr<Float> le(node);
     ImageExpr<Float> ie(le,"");
     Vector<Float> amp = ie.get().reform(IPosition(1,nchan));
//
     IPosition minPos(1), maxPos(1);
     Float minVal, maxVal;
     minMax(minVal, maxVal, minPos, maxPos, amp);
//
     os << "Expect signal in channel " << idx << endl;
     os << "Maximum signal  in channel " << maxPos(0) << LogIO::POST;
     AlwaysAssert(maxPos(0)==idx, AipsError);

// Make a nice plot

     if (!plotter.empty()) {
       Vector<Float> rms(nchan);
       const Coordinate& coord = cSys2.coordinate(2);
       Vector<Double> w, p(1);
       for (uInt i=0; i<nchan; i++) {
         p(0) = Double(i);
         coord.toWorld(w, p);
         rms(i) = w(0);
       }
//
       PGPlotter pl(plotter);
       pl.env(rms(0), rms(nchan-1), minVal, maxVal, 0, 0);
       pl.line(rms, amp);
     }
     delete pIm;
}
