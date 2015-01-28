//# LatticeHistSpecialize.cc:  Defines non-templated classes for LatticeHistograms
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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

#include <casacore/lattices/LatticeMath/LatticeHistSpecialize.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>
#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>

#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/casa/System/PGPlotter.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

uInt LatticeHistSpecialize::bin(Float datum, Float dmin, Float width, uInt nBins)
{
   return min(nBins-1, uInt((datum-dmin)/width));
}


void LatticeHistSpecialize::process(
	const Float* pInData, const Bool* pInMask,
	Block<Float>* pHist, const Vector<Float>& clip,
	Float binWidth, uInt offset,
	uInt nrval, uInt nBins,
	uInt dataIncr, uInt maskIncr
) {
   Float datum;
   uInt rBin;
   uInt index;
//
   if (pInMask==0) {
      for (uInt i=0; i<nrval; i++) {
         datum = *pInData;
         if (LattStatsSpecialize::usePixelInc(clip(0), clip(1), datum) > 0.5) {
            rBin = bin(datum, clip(0), binWidth, nBins);
            index = rBin + offset;
            Float& hist = (*pHist)[index];
            hist += 1.0;
         }
         pInData += dataIncr;
      }
   } else {
      for (uInt i=0; i<nrval; i++) {
         datum = *pInData;
         if (*pInMask &&
             (LattStatsSpecialize::usePixelInc(clip(0), clip(1), datum) > 0.5)) {
            rBin = bin(datum, clip(0), binWidth, nBins);
            index = rBin + offset;
            Float& hist = (*pHist)[index];
            hist += 1.0;
         }
         pInData += dataIncr;
         pInMask += maskIncr;
      }
   }
}

void LatticeHistSpecialize::process(
	const Complex* pInData, const Bool* pInMask,
	Block<Complex>* pHist, const Vector<Complex>& clip,
	Complex binWidth, uInt offset,
	uInt nrval, uInt nBins,
	uInt dataIncr, uInt maskIncr
) {
   Complex datum, useIt;
   uInt rbin;
   uInt index;
//
   if (pInMask==0) {
      for (uInt i=0; i<nrval; i++) {
         datum = *pInData;
         useIt = LattStatsSpecialize::usePixelInc(clip(0), clip(1), datum);
         if (real(useIt) > 0.5) {
            rbin = bin(real(datum), real(clip(0)), real(binWidth), nBins);
//
            index = rbin + offset;
            Complex& hist1 = (*pHist)[index];
	    ///            hist1.real() += 1.0;
	    hist1 += Complex(1.0, 0.0);
         }
         if (imag(useIt) > 0.5) {
            rbin = bin(imag(datum), imag(clip(0)), imag(binWidth), nBins);
            index = rbin + offset;
            Complex& hist2 = (*pHist)[index];
	    ///            hist2.imag() += 1.0;
	    hist2 += Complex(0.0, 1.0);
         }
         pInData += dataIncr;
      }
   } else {
      for (uInt i=0; i<nrval; i++) {
         datum = *pInData;
         if (*pInMask) {
            useIt = LattStatsSpecialize::usePixelInc(clip(0), clip(1), datum);
            if (real(useIt) > 0.5) {
               rbin = bin(real(datum), real(clip(0)), real(binWidth), nBins);
               index = rbin + offset;
               Complex& hist1 = (*pHist)[index];
	       ///               hist1.real() += 1.0;
	       hist1 += Complex(1.0, 0.0);
            }
            if (imag(useIt) > 0.5) {
               rbin = bin(imag(datum), imag(clip(0)), imag(binWidth), nBins);
               index = rbin + offset;
               Complex& hist2 = (*pHist)[index];
	       ///               hist2.imag() += 1.0;
	       hist2 += Complex(0.0, 1.0);
	    }
         }
         pInData += dataIncr;
         pInMask += maskIncr;
      }
   }
}


void LatticeHistSpecialize::makeGauss(uInt& nGPts, Float& gMax,
                                     Vector<Float>& gX, Vector<Float>& gY,
                                     Float dMean, Float dSigma,
                                     Float dSum, Float xMin,
                                     Float xMax, Float binWidth,
                                     Bool doCumu, Bool doLog)
// 
// Make overlay Gaussian with the given parameters
//
{

// 100 points please
      
   nGPts = 100;
   gX.resize(nGPts);                
   gY.resize(nGPts);
      
// Set up Gaussian functional
                                    
   const Float gaussAmp = dSum * C::_1_sqrt2 * C::_1_sqrtpi / dSigma;
   const Float gWidth = sqrt(8.0*C::ln2) * dSigma;
   const Gaussian1D<Float> gauss(gaussAmp, dMean, gWidth);
   
// Generate Gaussian.
 
   Float dgx = (xMax - xMin) / Float(nGPts);
   Float xx;
   uInt i;
   for (i=0,xx=xMin,gMax=0.0; i<nGPts; i++) {
      gX(i) = xx;
      gY(i) = gauss(xx);
//      
      gMax = max(gMax, gY(i));
      xx += dgx;
   }
 
// Make cumulative if desired
     
   const Float scale = dgx / binWidth;
   if (doCumu) makeCumulative (gY, gMax, nGPts, scale);
 
   
// Take log if desired

   if (doLog) makeLogarithmic (gY, gMax, nGPts);
}     

      
void LatticeHistSpecialize::makeCumulative (Vector<Float>& counts,
                                           Float& yMax, uInt nBins, 
                                           Float scale)
{  
   counts(0) = scale * counts(0);
   for (uInt i=1; i<nBins; i++) {
      counts(i) = counts(i)*scale + counts(i-1);
   }
   yMax = counts(nBins-1);
}  

void LatticeHistSpecialize::makeCumulative (Vector<Complex>& counts,
                                           Complex& yMax, uInt nBins, 
                                           Float scale)
//
// Code is the same as Float.  Could really make this
// templated, but still need access to this function
// from IHS, so leave it here
//
{  
   counts(0) = scale * counts(0);
   for (uInt i=1; i<nBins; i++) {
      counts(i) = counts(i)*scale + counts(i-1);
   }
   yMax = counts(nBins-1);
}  

void LatticeHistSpecialize::makeLogarithmic (Vector<Float>& counts,
                                            Float& yMax,
                                            uInt nBins)
{
   yMax = 0.0;
   for (uInt i=0; i<nBins; i++) {
     if (counts(i) > 0.0) counts(i) = log10(counts(i));
     yMax = max(yMax, counts(i));
   }
}

void LatticeHistSpecialize::makeLogarithmic (Vector<Complex>& counts,
                                            Complex& yMax,
                                            uInt nBins)
{
   yMax = 0.0;
   for (uInt i=0; i<nBins; i++) {
///     if (real(counts(i)) > 0.0) counts(i).real() = log10(counts(i).real());
///     if (imag(counts(i)) > 0.0) counts(i).imag() = log10(counts(i).imag());
     if (real(counts(i)) > 0.0) {
       counts(i) = Complex(log10(counts(i).real()), counts(i).imag());
     }
     if (imag(counts(i)) > 0.0) {
       counts(i) = Complex(counts(i).real(), log10(counts(i).imag()));
     }
//
     ///     if (real(counts(i)) > real(yMax)) yMax.real() = real(counts(i));
     ///     if (imag(counts(i)) > imag(yMax)) yMax.imag() = imag(counts(i));
     if (real(counts(i)) > real(yMax)) {
       yMax = Complex(real(counts(i)), yMax.imag());
     }
     if (imag(counts(i)) > imag(yMax)) {
       yMax = Complex(yMax.real(), imag(counts(i)));
     }
   }
}

Float LatticeHistSpecialize::mul(Float v1, Float v2)
{
   return v1*v2;
}

Complex LatticeHistSpecialize::mul(Complex v1, Complex v2)
{
   return Complex(real(v1)*real(v2),imag(v1)*imag(v2));
}



void LatticeHistSpecialize::plot(PGPlotter& plotter, Bool doGauss, Bool doCumu, Bool doLog,
                               Float linearSum, Float yMax,
                               Float binWidth, const Vector<Float>& values, 
                               const Vector<Float>& counts, const Vector<Float>& stats,
                               uInt label, uInt ci, Bool page)
//
// The histogram is already in its desired form - linear, log, cumu
// yMax is in that form too.
// 
// label == 0  -> Both
//          1     Bottom/left
//          2     Top/right
//
{
   Float xMin = stats(LatticeStatsBase::MIN);
   Float xMax  = stats(LatticeStatsBase::MAX);
   Float yMin = 0.0;
   Float yMax2 = yMax;
//
   Vector<Float> gX, gY;                          
   if (doGauss) {  
      uInt nGPts = 0;
      Float gMax;
      makeGauss (nGPts, gMax, gX, gY, stats(LatticeStatsBase::MEAN),
                 stats(LatticeStatsBase::SIGMA), linearSum,
                 xMin, xMax, binWidth, doCumu, doLog);
      yMax2 = max(yMax2, gMax);
   }

// Stretch extrema by 5%
       
   LatticeStatsBase::stretchMinMax(xMin, xMax);
   LatticeStatsBase::stretchMinMax(yMin, yMax2);
//
   if (page) plotter.page();
   plotter.bbuf();
   plotter.swin(xMin, xMax, 0.0, yMax2);
   plotter.sci(ci);
   if (label==0) {
      plotter.box("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   } else if (label==1) {
      plotter.box("BNST", 0.0, 0, "BNST", 0.0, 0);
   } else if (label==2) {
      plotter.box("CMST", 0.0, 0, "CMST", 0.0, 0);
   }
//
   plotHist (values, counts, plotter);
   if (doGauss) plotter.line (gX, gY);

// Label

   plotter.sci(1);
   if (doCumu) {
      if (doLog) {
         plotter.lab("Pixel Value", "Log10 (Cumulative Counts)", "");
      } else {
         plotter.lab("Pixel Value", "Cumulative Counts", "");
      }
   } else {
      if (doLog) {
         plotter.lab("Pixel Value", "Log10 (Counts)", "");
      } else {
         plotter.lab("Pixel Value", "Counts", "");
      }
   }
   plotter.ebuf();
}


void LatticeHistSpecialize::plot(PGPlotter& plotter, Bool doGauss, Bool doCumu, Bool doLog,
                                Complex linearSum, Complex yMax,
                                Complex binWidth, const Vector<Complex>& values, 
                                const Vector<Complex>& counts, const Vector<Complex>& stats,
                                uInt, uInt, Bool)
//
// The histogram is already in its desired form - linear, log, cumu
// yMax is in that form too.
//
{
   plot(plotter, doGauss, doCumu, doLog, real(linearSum), real(yMax),
        real(binWidth), real(values), real(counts), real(stats), 1, 1, True);
   plot(plotter, doGauss, doCumu, doLog, imag(linearSum), imag(yMax),
        imag(binWidth), imag(values), imag(counts), imag(stats), 2, 7, False);
}


void LatticeHistSpecialize::plotHist (const Vector<Float>& x,
                                     const Vector<Float>& y,
                                     PGPlotter& plotter) 
{
   const Float width = (x(1) - x(0)) / 2.0; 
   Float xx, yy;
   for (uInt i=0; i<x.nelements(); i++) {
      xx = x(i) - width;
      yy = y(i);
//    
      plotter.move (xx, 0.0);
      plotter.draw (xx, yy);

      plotter.move (xx, yy);
      xx = x(i) + width;
      plotter.draw (xx, yy);
//
      plotter.move (xx, yy);
      plotter.draw (xx, 0.0);
    }
}



Float LatticeHistSpecialize::setBinWidth (Float dmin, Float dmax, uInt nBins)
{
   Float width = (dmax - dmin) / Float(nBins); 
   if (near(width,0.0f,1e-6)) {
      width = 0.001;
   }
   return width;
}

Complex LatticeHistSpecialize::setBinWidth (Complex dmin, Complex dmax, uInt nBins)
{
   return Complex(setBinWidth(real(dmin), real(dmax), nBins),
                  setBinWidth(imag(dmin), imag(dmax), nBins));
}





} //# NAMESPACE CASACORE - END

