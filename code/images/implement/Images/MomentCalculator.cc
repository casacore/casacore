//# MomentCalculator.cc: 
//# Copyright (C) 1996,1997
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
#include <trial/Lattices/MomentCalculator.h>

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <trial/Fitting/NonLinearFitLM.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Functionals/SumFunction.h>
#include <trial/Functionals/FuncWithAutoDerivs.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageMoments.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Lattices/Lattice.h>
#include <aips/Logging/LogIO.h> 
#include <aips/Utilities/Assert.h>


// C wrappers for PGPLOT

extern "C" {
#include <cpgplot.h>
};


// Base class MomentCalcBase 

template <class T> 
MomentCalcBase<T>::~MomentCalcBase()
{;}


template <class T>
Bool MomentCalcBase<T>::allNoise (T& dMean, 
                                  const Vector<T>& data,
                                  const Double peakSNR,
                                  const Double stdDeviation)
//
// Try and work out whether this spectrum is all noise
// or not.  We don't bother with it if it is noise.
// We compare the peak with sigma and a cutoff SNR
//
{
   T dMin, dMax;
   minMax (dMin, dMax, data.ac());
   dMean = mean(data.ac());

// Assume we are continuum subtracted so outside of line mean=0

   const T rat = max(abs(dMin),abs(dMax)) / stdDeviation;

//   cout << "min,max,mean,sigma,peakSNR,SNR=" << dMin << " " << dMax << " " << dMean
//        << stdDeviation_p << " " << peakSNR << " " << rat << LogIO::POST;
   if (rat < peakSNR) {
      return True;
   } else {
      return False;
   }
}



template <class T>
IPosition& MomentCalcBase<T>::blc(ImageMoments<T>& iMom)
{
// Get it from ImageMoments private data

   return iMom.blc_p;
}


template <class T>
IPosition& MomentCalcBase<T>::trc(ImageMoments<T>& iMom)
{
// Get it from ImageMoments private data

   return iMom.trc_p;
}


template <class T>
Bool& MomentCalcBase<T>::doAuto(ImageMoments<T>& iMom)
{
// Get it from ImageMoments private data

   return iMom.doAuto_p;
}


template <class T>
Bool& MomentCalcBase<T>::doFit(ImageMoments<T>& iMom)
{
// Get it from ImageMoments private data

   return iMom.doFit_p;
}



typedef Vector<Int> gpp_VectorInt;
template <class T>
void MomentCalcBase<T>::constructorCheck(Vector<T>& retMoments,
                                         Vector<T>& calcMoments, 
                                         const gpp_VectorInt& selectMoments,
                                         const Int nLatticeOut)
 {
// Number of output lattices must equal the number of moments
// the user asked to calculate

   AlwaysAssert(nLatticeOut == selectMoments.nelements(), AipsError);

// Number of requested moments must be in allowed range

   AlwaysAssert(selectMoments.nelements() <= nMaxMoments(), AipsError);
   AlwaysAssert(selectMoments.nelements() > 0, AipsError);

// Resize the vector that will hold the values for the desired moments

   retMoments.resize(selectMoments.nelements());

// Resize the vector that will hold ALL possible moments
   
   calcMoments.resize(nMaxMoments());
}


template <class T>
void MomentCalcBase<T>::costlyMoments(ImageMoments<T>& iMom,
                                      Bool& doMedianI,
                                      Bool& doMedianV,
                                      Bool& doAbsDev)
{
   typedef ImageMoments<Float> IM;
   for (Int i=0; i<iMom.moments_p.nelements(); i++) {
      if (iMom.moments_p(i) == IM::MEDIAN) doMedianI = True;
      if (iMom.moments_p(i) == IM::MEDIAN_COORDINATE) doMedianV = True;
      if (iMom.moments_p(i) == IM::ABS_MEAN_DEVIATION) doAbsDev = True;
   }      
}


template <class T>
String& MomentCalcBase<T>::device(ImageMoments<T>& iMom)
{
   return iMom.device_p;
}


template <class T>
Bool MomentCalcBase<T>::doCoordCalc(ImageMoments<T>& iMom)
{
// Figure out if we need to compute the coordinate of each profile pixel index
// for each profile.  This is very expensive for non-separable axes.

   typedef ImageMoments<Float> IM;
   for (Int i=0; i<iMom.moments_p.nelements(); i++) {
      if (iMom.moments_p(i) == IM::WEIGHTED_MEAN_COORDINATE ||
          iMom.moments_p(i) == IM::WEIGHTED_DISPERSION_COORDINATE) return True;
   }
   return False;
}



template <class T>
void MomentCalcBase<T>::drawHorizontal(const T& y)
//
// Draw a horizontal line across the full x range of the plot
//
{
   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);

   cpgmove (xMin, float(y));
   cpgdraw (xMax, float(y));
}

template <class T>
void MomentCalcBase<T>::drawLine (ImageMoments<T>& iMom,
                                  const Vector<T>& x,
                                  const Vector<T>& y)
//
// Draw  a spectrum on the current panel
// with the box already drawn
//
{
// Pass it on to ImageMoments who has to do this too

   iMom.drawLine(x, y);
} 


template <class T>
void MomentCalcBase<T>::drawLine (ImageMoments<T>& iMom,
                                  const Vector<T>& x,
                                  const Vector<T>& y,
                                  const Bool fixedYLimits,
                                  const Float yMinAuto,
                                  const Float yMaxAuto,
                                  const String xLabel,
                                  const String yLabel,
                                  const String title)
//
// Draw and label a spectrum on the current panel
//
{
// Find extrema
      
   const Int nPts = x.nelements();
   Float xMin = 0.0;
   Float xMax = Float(nPts);
   ImageUtilities::stretchMinMax (xMin, xMax); 
   
   T yMin, yMax;
   Float yMinF, yMaxF;
   if (!fixedYLimits) {
      minMax(yMin, yMax, y.ac());
      yMinF = Float(yMin);
      yMaxF = Float(yMax);
      ImageUtilities::stretchMinMax (yMinF, yMaxF);
   }


// Plot
 
   if (fixedYLimits) {
      cpgswin (float(xMin), float(xMax), yMinAuto, yMaxAuto);
   } else {  
      cpgswin (float(xMin), float(xMax), yMinF, yMaxF);
   }
   cpgbox ("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   drawLine (iMom, x, y);
   cpglab (xLabel.chars(), yLabel.chars(), "");
   cpgmtxt ("T", 1.0, 0.5, 0.5, title.chars());
}

template <class T>
void MomentCalcBase<T>::drawMeanSigma (const T dMean,
                                       const T dSigma)
//
// Draw a horizontal line on the spectrum plot at
// the mean value, and 2 horizontal lines at
// mean +/- sigma
//    
{
   cpgsci(7);
   drawHorizontal(dMean);
   cpgsci(5);
   drawHorizontal(dMean+dSigma);  
   drawHorizontal(dMean-dSigma);
   cpgsci(1);
}


template <class T>
void MomentCalcBase<T>::drawVertical (ImageMoments<T>& iMom,
                                      const T loc,
                                      const T yMin,
                                      const T yMax)
{  
// Pass it on to ImageMoments

   iMom.drawVertical(loc, yMin, yMax);
}
  


template <class T>
void MomentCalcBase<T>::drawWindow(ImageMoments<T>& iMom,
                                   const Vector<Int>& window)
//
// Mark the current window on the plot
//
{  
   float x1, x2, y1, y2;
   cpgqwin (&x1, &x2, &y1, &y2);
   drawVertical (iMom, float(window(0)), y1, y2);
   drawVertical (iMom, float(window(1)), y1, y2);
}


template <class T>
Bool MomentCalcBase<T>::fitGaussian (T& peak,
                                     T& pos,
                                     T& width,
                                     T& level,
                                     const Vector<T>& x,
                                     const Vector<T>& y,
                                     const T peakGuess,
                                     const T posGuess,
                                     const T widthGuess,
                                     const T levelGuess)
// 
// Fit Gaussian pos * exp(-4ln2*(x-pos)**2/width**2)
// width = fwhm
// 
{
      
// Create fitter

   NonLinearFitLM<T> fitter;

// Create and set the functionals
   
   const Gaussian1D<AutoDiff<T> > gauss; 
   const Polynomial<AutoDiff<T> > poly;  
   SumFunction<AutoDiff<T>,AutoDiff<T> > func;
   func.addFunction(gauss);
   func.addFunction(poly);         
   
   FuncWithAutoDerivs<T,T> autoFunc(func);
   fitter.setFunction(autoFunc);
   
//   Gaussian1D<AutoDiff<T> > gauss;
//   fitter.setFunction(gauss);

   
// Initial guess


   Vector<T> v(4);
                                   
   v(0) = peakGuess;             // peak
   v(1) = posGuess;              // position
   v(2) = widthGuess;            // width
   v(3) = levelGuess;            // level
  
   fitter.setFittedFuncParams(v);
   
   
// Set maximum number of iterations to 50.  Default is 10

   fitter.setMaxIter(50);


// Set converge criteria.

   T tol = 0.001;
   fitter.setCriteria(tol);
                                   

// perform fit
   
   Vector<T> resultSigma(x.nelements()); 
   resultSigma = 1;
   Vector<T> solution = fitter.fit(x, y, resultSigma);

// Return values of fit
   
//   cout << "SOlution = " << solution.ac() << LogIO::POST;
   
   peak  = solution(0);
   pos   = solution(1);
   width = abs(solution(2));
   level = solution(3);


// Return status

   return fitter.converged();
                                   
}



template <class T>
Bool& MomentCalcBase<T>::fixedYLimits(ImageMoments<T>& iMom) 
{
   return iMom.fixedYLimits_p;
}



template <class T>
Bool MomentCalcBase<T>::getAutoGaussianFit (ImageMoments<T>& iMom,
                                            Vector<T>& gaussPars,
                                            const Vector<T>& x,
                                            const Vector<T>& y,
                                            const Double peakSNR,
                                            const Double stdDeviation,
                                            const Bool doPlot,
                                            const Bool fixedYLimits,
                                            const Float yMinAuto,
                                            const Float yMaxAuto,
                                            const String xLabel,
                                            const String yLabel,
                                            const String title)
//
// Automatically fit a Gaussian and return the Gaussian parameters.
// If a plotting device is active, we also plot the spectra and fits
//
// Inputs:
//   x,y        Vector containing the data
//   doPlot     Plot spectrum and optionally the  window
//   x,yLabel   Labels
//   title
// Output:
//   gaussPars  The gaussian parameters, peak, pos, fwhm
//   Bool       If False then this spectrum has been rejected (all
//              noise, failed fit)
//
{
    
   
// Plot spectrum if desired
      
   if (doPlot) {
      cpgpage();
      drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto,
                xLabel, yLabel, title);
   }

// See if this spectrum is all noise first.  If so, forget it.
   
   T dMean;
   const Bool noisy = allNoise(dMean, y.ac(), peakSNR, stdDeviation);

// Draw on mean and sigma
  
   const T sigma = stdDeviation;
   if (doPlot) {
      drawMeanSigma (dMean, sigma);
      if (noisy) cpgmtxt ("T", 1.0, 0.0, 0.0, "NOISE");
   }  
   if (noisy) {
      gaussPars = 0;  
      return False;
   }

// Work out guesses for Gaussian

   T peakGuess, posGuess, widthGuess, levelGuess;
   T pos, width, peak, level;
   getAutoGaussianGuess(peakGuess, posGuess, widthGuess, x, y);
   levelGuess = mean(y.ac());
   peakGuess = peakGuess - levelGuess;

// Fit gaussian. Do it twice.
  
   if (!fitGaussian (peak, pos, width, level, x, y, peakGuess, posGuess,
                     widthGuess, levelGuess)) {
      gaussPars = 0;
      return False;
   }  
   gaussPars(0) = peak;
   gaussPars(1) = pos;
   gaussPars(2) = width;
   gaussPars(3) = level;

   
// Plot the fit
   
   if (doPlot) showGaussFit (iMom, peak, pos, width, level, x, y);
   
   return True;
}



template <class T>
void MomentCalcBase<T>::getAutoGaussianGuess (T& peakGuess,
                                              T& posGuess,
                                              T& widthGuess,
                                              const Vector<T>& x,
                                              const Vector<T>& y)
//
// Make a wild stab in the dark as to what the Gaussian
// parameters of this spectrum might be
//    
{

// Find peak and position of peak

   IPosition minPos(1);
   IPosition maxPos(1);
   T dMin, dMax;
   minMax(dMin, dMax, minPos, maxPos, y.ac());

   posGuess = x(maxPos(0));
   peakGuess = dMax;

// Nothing much is very robust.  Assume the line is reasonably
// sampled and set its width to a few pixels.  Totally ridiculous.
                                            
   widthGuess = 5;
                                            
//   cout << "Guess: peak,pos,width=" << peakGuess << ", " << posGuess << "," <<
//           widthGuess << LogIO::POST;

}


template <class T>
void MomentCalcBase<T>::getAutoWindow (ImageMoments<T>& iMom,
                                       Vector<Int>& window,
                                       const Vector<T>& x,
                                       const Vector<T>& y,
                                       const Double peakSNR,
                                       const Double stdDeviation,
                                       const Bool doFit,
                                       const Bool doPlot,
                                       const Bool fixedYLimits,                 
                                       const Float yMinAuto,                 
                                       const Float yMaxAuto,                 
                                       const String xLabel,
                                       const String yLabel,
                                       const String title)
//
// Automatically fit a Gaussian and return the +/- 3-sigma window or
// invoke Bosma's method to set a window.  If a plotting device is
// active, we also plot the spectra and fits
//
// Inputs:
//   x,y        Spectrum
//   doPlot     Plot spectrum and optionally the  window
//   x,yLabel   x label for plots
//   title 
// Output:
//   window     The window (pixels).  If both 0,  then discard this spectrum
//              and blank moments    
//
{
   if (doFit) {
      Vector<T> gaussPars(4);
      if (!getAutoGaussianFit (iMom, gaussPars, x, y, peakSNR, stdDeviation, 
                               doPlot, fixedYLimits, yMinAuto, yMaxAuto, 
                               xLabel, yLabel, title)) {
         window = 0;
         return;
      } else {
   
// Set 3-sigma limits.
 
         if (!setNSigmaWindow (window, gaussPars(1), gaussPars(2),
                               y.nelements(), 3)) {
            window = 0;
            return;
         }
      }
   } else {
// Invoke Albert's method (see AJ, 86, 1791)

      if (!getBosmaWindow (iMom, window, x, y, peakSNR, stdDeviation, 
                           doPlot, fixedYLimits, yMinAuto, yMaxAuto, 
                           xLabel, yLabel, title)) {
         window = 0;
         return;
      }
   }
   
// Plot window if desired
 
   if (doPlot) drawWindow (iMom, window);
                               
}


template <class T>
Bool MomentCalcBase<T>::getBosmaWindow (ImageMoments<T>& iMom,
                                        Vector<Int>& window,
                                        const Vector<T>& x,
                                        const Vector<T>& y,
                                        const Double peakSNR,
                                        const Double stdDeviation,
                                        const Bool doPlot,
                                        const Bool fixedYLimits,
                                        const Float yMinAuto,
                                        const Float yMaxAuto,
                                        const String xLabel,
                                        const String yLabel,
                                        const String title)
//
// Automatically work out the spectral window
// with Albert Bosma's algorithm.
//    
// Inputs: 
//   x,y       Spectrum
//   doPlot    Plot device active if True
//   x,yLabel  Labels for plots
// Output:
//   window    The window
//   Bool      False if we reject this spectrum
//
{
      
   if (doPlot) {
   
// Plot spectrum 
      
      cpgpage();
      drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto,
                xLabel, yLabel, title);
   }


// See if this spectrum is all noise first.  If so, forget it.
   
   T dMean;
   const Bool noisy1 = allNoise(dMean, y.ac(), peakSNR, stdDeviation);

// Draw on mean and sigma
 
   const T sigma = stdDeviation;
   if (doPlot) {
      drawMeanSigma (dMean, sigma);
      if (noisy1) cpgmtxt ("T", 1.0, 0.0, 0.0, "NOISE");
   }
   if (noisy1) {
      window = 0;
      return False;   
   }

// Find peak
   
   const Int nPts = y.nelements(); 
   IPosition minPos(1), maxPos(1);
   T yMin, yMax;
   minMax(yMin, yMax, minPos, maxPos, y.ac());
   Int iMin = max(0,maxPos(0)-2);   
   Int iMax = min(nPts-1,maxPos(0)+2);
   Double tol = sigma / (nPts - (iMax-iMin-1));
          
       
// Iterate to convergence
   
   Bool first = True;
   Bool converged = False;
   Bool more = True;
   Double mean;
   Double oldMean = 0;
   while (more) {

//     cout << LogIO::NORMAL << "iMin,iMax,oldmean,tol=" << iMin << "," << iMax << $
   
// Find mean outside of peak region

      Double sum = 0.0;
      for (Int i=0,j=0; i<nPts; i++) {
         if (i < iMin || i > iMax) {
            sum += Double(y(i));
            j++;
         }
      }
      if (j>0) mean = sum / Double(j);
   

// Interpret result

      if (!first && j>0 && abs(mean-oldMean) < tol) {
         converged = True;
         more = False;
      } else if (iMin==0 && iMax==nPts-1)
         more = False;
      else {
   
// Widen window and redetermine tolerance

         oldMean = mean;
         iMin = max(0,iMin - 2);
         iMax = min(nPts-1,iMax+2); 
         tol = sigma / (nPts - (iMax-iMin-1));
      }
      first = False;
   }   
      
// Return window

   if (converged) {
      window(0) = iMin;
      window(1) = iMax;
      return True;
   } else {
      window = 0;
      return False;   
   }
}  


template <class T>
void MomentCalcBase<T>::getButton(Bool& ditch,
                                  Bool& redo)
//
// Read the PGPLOT cursor and interpret the button
// pushed
//
{
// Fish out window
 
   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);

   float x = (xMin+xMax)/2;
   float y = (yMin+yMax)/2;
   char ch;
   cpgcurs (&x, &y, &ch);
   
   String str = ch;
   str.upcase();
   ditch = False;
   redo = False;  
   if (str == "X")
      ditch = True;
   else if (str == "D")
      redo = True;

}
    



template <class T>
void MomentCalcBase<T>::getInterDirectWindow (ImageMoments<T>& iMom,
                                              Bool& allSubsequent,
                                              LogIO& os,
                                              Vector<Int>& window,
                                              const Vector<T>& x,
                                              const Vector<T>& y,
                                              const Bool fixedYLimits,   
                                              const Float yMinAuto,   
                                              const Float yMaxAuto,
                                              const String xLabel,
                                              const String yLabel,
                                              const String title)
//
// With the cursor, mark the range_p for the window method
//
// Outputs:
//  window    The window (pixels)
//
{
 
// First plot the spectrum
   
   cpgpage();
   drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto,
             xLabel, yLabel, title);

       
// Try and get a decent range_p from user   
      
   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);
   Bool more = True;
   Bool ditch, redo;
   const Int nPts = y.nelements();   
   T tX, tY1, tY2;

   while (more) {
  
// Get and draw first location   

      Bool final = False;
      T x1 = nPts/2;
      allSubsequent = True;
      while (!getLoc(x1, allSubsequent, ditch, redo, os, final)) {};
      if (ditch) {
         window = 0;
         return;
      }

      if (!redo) {
         window(0) = max(0,Int(x1+0.5));
         tX = window(0);
         tY1 = yMin;
         tY2 = yMax;
         drawVertical (iMom, tX, tY1, tY2);
  

// Get and draw second location
  
         T x2 = Float(window(0));
         final = True;
         allSubsequent = True;
         while (!getLoc(x2, allSubsequent, ditch, redo, os, final)) {};
         if (ditch) {
            window = 0;
            return;
         } else if (redo) {
            cpgeras();
            drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto,
                      xLabel, yLabel, title);
         } else {
            window(1) = min(nPts-1,Int(x2+0.5));
            tX = window(1);
            drawVertical (iMom, tX, tY1, tY2);
         
// Set window
         
            Int iTemp = window(0);
            window(0) = min(iTemp, window(1));
            window(1) = max(iTemp, window(1));
         
// If they stuffed it up, have another go.  Erase the line and redraw
// the spectrum segment
         
            if (window(0) == window(1)) {
               os << LogIO::NORMAL << "Degenerate window, try again" << LogIO::POST;
               cpgeras();
               drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto,
                         xLabel, yLabel, title);

            } else
               more = False;
         } 
      }
   }
}



template <class T>
Bool MomentCalcBase<T>::getInterGaussianFit (ImageMoments<T>& iMom,
                                             Vector<T>& gaussPars,
                                             LogIO& os,
                                             const Vector<T>& x,
                                             const Vector<T>& y,
                                             const Bool fixedYLimits,
                                             const Float yMinAuto,
                                             const Float yMaxAuto,
                                             const String xLabel,
                                             const String yLabel,
                                             const String title)
// 
// With the cursor, define a guess for a Gaussian fit,
// and do the fit over and over until they are happy.
// Then return the Gaussian parameters.
//
// Inputs:
//   x,y       The abcissa and spectrum
//   x,yLabel  Labels
//   title     Title of plot
// Outputs:
//   gaussPars The gaussian parameters (peak, pos, width, level)
//   Bool      True if all successful, False if spectrum rejected
//
{
   
// First draw the spectrum
         
   cpgpage();
   drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto,
             xLabel, yLabel, title);

// Get users guess and fit until satisfied

   Bool more = True;
   Bool ditch, redo;
   Vector<Int> window(2);
   os << endl;


   while (more) {

// Get users guess for position, peak, width and fit window

      T peakGuess, posGuess, widthGuess, levelGuess, level;
      Bool reject;
      getInterGaussianGuess (iMom, peakGuess, posGuess, widthGuess, window,
                             reject, os, y.nelements());
      if (reject) {
         gaussPars = 0;   
         return False;
      }
  

// Get guess for level and adjust peak

      levelGuess = mean(y.ac());
      peakGuess = peakGuess - levelGuess;

   
// Fit a Gaussian
   
      Int n = window(1) - window(0) + 1;
      Vector<T> xFit(n);
      Vector<T> yFit(n);
      for (Int i=0; i<n; i++) {
         xFit(i) = x(i+window(0));
         yFit(i) = y(i+window(0));
      }
      T pos, width, peak;
      if (fitGaussian (peak, pos, width, level, xFit, yFit, peakGuess,
                       posGuess, widthGuess, levelGuess)) {
       
// Show fit

         showGaussFit (iMom, peak, pos, width, level, x, y);
      } else {
         os << LogIO::NORMAL << "Fit did not converge" << LogIO::POST;
      }


// Are we happy ?

   
      os << LogIO::NORMAL << "Accept (left),  redo (middle), reject (right)" << LogIO::POST;
      getButton(ditch, redo);
      if (ditch) {
         os << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
         gaussPars = 0;
         return False;
      } else if (redo) {
   
// Redraw spectrum
                       
         cpgeras();
         drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto,
                   xLabel, yLabel, title);
      } else {
         
// OK, set parameters of fit
         
         more = False;
         gaussPars(0) = peak;
         gaussPars(1) = pos;
         gaussPars(2) = width;
         gaussPars(3) = level;
         return True;
      }
   }
   return True;
}



template <class T>
void MomentCalcBase<T>::getInterGaussianGuess(ImageMoments<T>& iMom,
                                              T& peakGuess,
                                              T& posGuess,
                                              T& widthGuess,
                                              Vector<Int>& window,
                                              Bool& reject,
                                              LogIO& os,
                                              const Int nPts)
//
// Use the cursor to get the user's guess for the
// Gaussian peak, position and width (fwhm)
// and fitting window 
//
{
   os << LogIO::NORMAL << "Mark the location of the peak and position" << endl;
   os << "Press right button to reject spectrum" << LogIO::POST;

   float x1, x2, y1, y2;
   cpgqwin (&x1, &x2, &y1, &y2);
    
   
// Peak/pos first

   char ch;
   static float x = (x1+x2)/2;
   static float y = (y1+y2)/2;
   Bool miss=True;
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = ToBool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   String str(ch);    
   str.upcase();
   reject = False;
   
   if (str == "X") {
     os << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
     return;
   }
   cpgsci(3);
   cpgpt (1, &x, &y, 2);
   cpgupdt ();
   cpgsci (1);
   posGuess = x;
   peakGuess = y; 
   
     
// Now FWHM
     
   os << endl;
   os << LogIO::NORMAL << "Mark the location of the FWHM" << endl;
   os << "Press right button to reject spectrum" << LogIO::POST;
   miss = True;   
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = ToBool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   str = ch;  
   str.upcase();
   if (str == "X") {
     os << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
   }
   cpgsci (3);
   cpgpt (1, &x, &y, 2);
   cpgsci (1);
   y = float(peakGuess)/2;
   cpgupdt (); 
   widthGuess = 2*abs(posGuess-x);
  
// Now window
    
   os << endl;
   os << LogIO::NORMAL << "Mark the location of the fit window" << endl;
   os << "Press right button to reject spectrum" << endl;
   os << "Press middle button to fit the whole spectrum" << LogIO::POST;
   miss=True;
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = ToBool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   str = ch;   
   str.upcase();
   if (str == "X") {
     os << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
     return;
   } else if (str == "D") {
     os << LogIO::NORMAL << "Fit to entire spectrum" << LogIO::POST;
     window(0) = 0;
     window(1) = nPts-1;
     return; 
   }
   T tX = x;
   T tY1 = y1;
   T tY2 = y2;
   drawVertical (iMom, tX, tY1, tY2);
   window(0) = Int(x+0.5);
   
   miss = True;
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = ToBool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   str.upcase();   
   if (str == "X") {
     os << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
     return;
   } else if (str == "D") {
     os << LogIO::NORMAL << "Fit to entire spectrum" << LogIO::POST;
     window(0) = 0;
     window(1) = nPts-1;
     return;
   }
   tX = x;
   tY1 = y1;
   tY2 = y2;
   drawVertical (iMom, tX, tY1, tY2);
   window(1) = Int(x+0.5);
   Int iTemp = window(0);
   window(0) = min(iTemp, window(1));
   window(1) = max(iTemp, window(1));
   window(0) = max(0,window(0));
   window(1) = min(nPts-1,window(1));
   
   cpgsci(1);
//   cout << "Guess:peak,pos,width=" << peakGuess << "," << posGuess
//        << "," << widthGuess << LogIO::POST;   

}
   




template <class T>
void MomentCalcBase<T>::getInterWindow(ImageMoments<T>& iMom,
                                       Bool& allSubsequent, 
                                       LogIO& os,
                                       Vector<Int>& window,
                                       const Bool doFit,
                                       const Vector<T>& x,
                                       const Vector<T>& y,
                                       const Bool fixedYLimits,
                                       const Float yMinAuto,
                                       const Float yMaxAuto,
                                       const String xLabel,
                                       const String yLabel,
                                       const String title)
//
// Interactively select the moment window by fitting a Gaussian
// or directly setting the window with the cursor.
//
// Inputs:
//   x,y        Spectrum
//   x,yLabel   Labels for plots
//   title
// Output:
//   window     Include pixels in this range of indices.  If both 0,
//              then discard this spectrum and blank moments
//   allSubsequent 
//              If True, then the user has instructed that
//              all subsequent spectra are to use this window
//              and we are to stop the interactive plotting
{                                     
   if (doFit) {   
         
         
// We interactively fit a Gaussian and choose +/- 3 sigma limits as the range
      
      Vector<T> gaussPars(4);
      if (!getInterGaussianFit (iMom, gaussPars, os, x, y, fixedYLimits, yMinAuto, 
                                yMaxAuto, xLabel, yLabel, title)) {
         window = 0;
         return;
      } else {
    
// Set 3-sigma range
   
         if (!setNSigmaWindow (window, gaussPars(1), gaussPars(2), y.nelements(), 3)) {
            os << LogIO::NORMAL << "Window too small for this spectrum" << LogIO::POST;
            window = 0;
            return;
         }

// Mark window on plot
         
         cpgeras ();
         drawLine (iMom, x, y, fixedYLimits, yMinAuto, yMaxAuto, 
                   xLabel, yLabel, title);
         drawWindow (iMom, window);
      }
      allSubsequent = False;
   } else {

// The user just marks the range with the cursor

      getInterDirectWindow (iMom, allSubsequent, os, window, x, y, fixedYLimits, 
                            yMinAuto, yMaxAuto, xLabel, yLabel, title);
   }
  
   return;
}



template <class T>
Bool MomentCalcBase<T>::getLoc (T& x,
                                Bool& allSubsequent,
                                Bool& ditch,
                                Bool& redo,
                                LogIO& os,
                                const Bool final)
//
// Read the PGPLOT cursor and return its coordinates if not off the plot
// Also interpret which button was pressed
// 
// Inputs:
//   final   If we are marking a window, this indicates that we are
//           trying to mark the last location, so that allSubsequent
//           might be activated.
// Input/Output:
//   x       X location of cursor.  Input value is used to position cursor
// Outputs:
//   allSubsequent
//           If True it means that whatever we have done to this spectrum,
//           the user would like it done to all subsequent ones.
//   ditch   The user has indicated to reject this spectrum
//   redo    The user has indicated to redo whaetver it is we are doing !
//   Bool    False if cursor off the window
//                            
{
// Fish out window
      
   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);
      
// Position and read cursor
      
   float xx = float(x);
   static float yy = 0.0;
   char ch;
   cpgcurs (&xx, &yy, &ch);
       
// Interpret location and character

   String str = ch;
   str.upcase();
   ditch = False;
   redo = False;
   allSubsequent = False;
                              
   if (str == "X") {
      os << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
      ditch = True;
   } else if (str == "D") {
      os << LogIO::NORMAL << "Redoing window for this spectrum" << LogIO::POST;
      redo = True;
   } else {
      if (xx >= xMin && xx <= xMax) {
         x = xx;
      } else {
         os << LogIO::NORMAL << "Cursor out of range" << LogIO::POST;
         return False;
      }

      if (str == "S") {
         if (!final) {
            os << LogIO::NORMAL <<
                      "You must define both ends of the range before it can be" << endl;
            os   << "applied to all subsequent spectra. Enter S to define the" << endl;
            os   << "second extremum and indicate it will be used for all " << endl;
            os   << "subsequent spectra" << LogIO::POST;
            return False;
         } else {
            os << LogIO::NORMAL << "All subsequent spectra will use this window" << LogIO::POST;
            allSubsequent = True;
         }
      }
   } 
   return True;
}

      

template <class T>      
void MomentCalcBase<T>::makeAbcissa (Vector<T>& x,
                                     const Int& n)
{
   x.resize(n);
   for (Int i=0; i<n; i++) x(i) = i;
}



template <class T>
Int& MomentCalcBase<T>::momentAxis(ImageMoments<T>& iMom) 
{
// Get it from ImageMoments private data

   return iMom.momentAxis_p;
}

template <class T>
String& MomentCalcBase<T>::momentAxisName(ImageMoments<T>& iMom) 
{
// Return the name of the moment/profile axis

   Int worldMomentAxis = 
      iMom.pInImage_p->coordinates().pixelAxisToWorldAxis(iMom.momentAxis_p);

   return iMom.pInImage_p->coordinates().worldAxisNames()(worldMomentAxis);
}


template <class T>
Int MomentCalcBase<T>::nMaxMoments() const
{

// Get it from ImageMoments enum

   Int i = ImageMoments<T>::NMOMENTS;
   return i;
}


template <class T>
Double& MomentCalcBase<T>::peakSNR(ImageMoments<T>& iMom)
{
// Get it from ImageMoments private data

   return iMom.peakSNR_p;
}


typedef Vector<Float> gpp_VectorFloat;
template <class T>
void MomentCalcBase<T>::range(gpp_VectorFloat& pixelRange,
                              Bool& doInclude,
                              Bool& doExclude, 
                              ImageMoments<T>& iMom)
{
// Get it from ImageMoments private data

   pixelRange = iMom.range_p;
   doInclude = ToBool(!(iMom.noInclude_p));
   doExclude = ToBool(!(iMom.noExclude_p));
}


template <class T>
Vector<Int> MomentCalcBase<T>::selectMoments(ImageMoments<T>& iMom)
//
// Fill the moment selection vector according to what the user requests
//
{
   typedef ImageMoments<Float> IM;
   Vector<Int> sel(IM::NMOMENTS);

   Int j = 0;
   for (Int i=0; i<iMom.moments_p.nelements(); i++) {
      if (iMom.moments_p(i) == IM::AVERAGE) {
         sel(j++) = IM::AVERAGE;
      } else if (iMom.moments_p(i) == IM::INTEGRATED) {
         sel(j++) = IM::INTEGRATED;
      } else if (iMom.moments_p(i) == IM::WEIGHTED_MEAN_COORDINATE) {
         sel(j++) = IM::WEIGHTED_MEAN_COORDINATE;
      } else if (iMom.moments_p(i) == IM::WEIGHTED_DISPERSION_COORDINATE) {
         sel(j++) = IM::WEIGHTED_DISPERSION_COORDINATE;
      } else if (iMom.moments_p(i) == IM::MEDIAN) {
         sel(j++) = IM::MEDIAN;
      } else if (iMom.moments_p(i) == IM::STANDARD_DEVIATION) {
         sel(j++) = IM::STANDARD_DEVIATION;
      } else if (iMom.moments_p(i) == IM::RMS) {
         sel(j++) = IM::RMS;
      } else if (iMom.moments_p(i) == IM::ABS_MEAN_DEVIATION) { 
         sel(j++) = IM::ABS_MEAN_DEVIATION;
      } else if (iMom.moments_p(i) == IM::MAXIMUM) {
         sel(j++) = IM::MAXIMUM;
       } else if (iMom.moments_p(i) == IM::MAXIMUM_COORDINATE) {
         sel(j++) = IM::MAXIMUM_COORDINATE;
      } else if (iMom.moments_p(i) == IM::MINIMUM) {
         sel(j++) = IM::MINIMUM;
      } else if (iMom.moments_p(i) == IM::MINIMUM_COORDINATE) {
         sel(j++) = IM::MINIMUM_COORDINATE;
      } else if (iMom.moments_p(i) == IM::MEDIAN_COORDINATE) {
         sel(j++) = IM::MEDIAN_COORDINATE;
      }
   }
   sel.resize(j,True);
   return sel;
}



template <class T>
void MomentCalcBase<T>::setCalcMoments (ImageMoments<T>& iMom,
                                        Vector<T>& calcMoments,
                                        Vector<Double>& pixelIn,
                                        Vector<Double>& worldOut,
                                        const Bool doCoordCalc,
                                        const T dMedian,
                                        const T vMedian,
                                        const Int nPts,
                                        const Double s0,
                                        const Double s1,
                                        const Double s2,
                                        const Double s0Sq,
                                        const Double sumAbsDev,
                                        const Double dMin,
                                        const Double dMax,
                                        const Int iMin,
                                        const Int iMax) 
//
// Fill the moments array
//
// Outputs:
//   calcMoments The moments
//
{

// Short hand to fish ImageMoments enum values out
// Despite being our friend, we cannot refer to the
// enum values as just, say, "AVERAGE"

   typedef ImageMoments<Float> IM;


// Normalize and fill moments
    
   calcMoments(IM::AVERAGE) = s0 / nPts;
   calcMoments(IM::INTEGRATED) = s0;
   calcMoments(IM::WEIGHTED_MEAN_COORDINATE) = s1 / s0;
   calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) =
     (s2 / s0) - calcMoments(IM::WEIGHTED_MEAN_COORDINATE) *
                 calcMoments(IM::WEIGHTED_MEAN_COORDINATE);
   calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) =
      abs(calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE));
   if (calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) > 0.0) {
     calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) =
        sqrt(calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE));
   }
   else {
     calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) = 0.0;
   }
                 
// Standard deviation about mean of I
      
   if (Float((s0Sq - s0*s0/nPts)/(nPts-1)) > 0) {
      calcMoments(IM::STANDARD_DEVIATION) = sqrt((s0Sq - s0*s0/nPts)/(nPts-1));
   } else {
      calcMoments(IM::STANDARD_DEVIATION) = 0;
   }

// Rms of I
    
   calcMoments(IM::RMS) = sqrt(s0Sq/nPts);

      
// Absolute mean deviation
      
   calcMoments(IM::ABS_MEAN_DEVIATION) = sumAbsDev / nPts;
      
   
// Maximum value

   calcMoments(IM::MAXIMUM) = dMax;
   

// Coordinate of maximum value

   if (doCoordCalc) calcMoments(IM::MAXIMUM_COORDINATE) = 
      getMomentCoord(iMom, pixelIn, worldOut, Double(iMax));
  
      
// Minimum value
   calcMoments(IM::MINIMUM) = dMin;
   
   
// Coordinate of minimum value

   if (doCoordCalc) calcMoments(IM::MINIMUM_COORDINATE) = 
      getMomentCoord(iMom, pixelIn, worldOut, Double(iMin));

   
// Medians

   calcMoments(IM::MEDIAN) = dMedian;
   calcMoments(IM::MEDIAN_COORDINATE) = vMedian;
}



template <class T> 
Bool MomentCalcBase<T>::setNSigmaWindow (Vector<Int>& window,
                                         const T pos,
                                         const T width,
                                         const Int nPts,
                                         const Int N)
// 
// Take the fitted Gaussian position and width and
// set an N-sigma window.  If the window is too small
// return a Fail condition.
//
// Inputs:
//   pos,width   The position and width in pixels
//   nPts        The number of points in the spectrum that was fit
//   N           The N-sigma
// Outputs:
//   window      The window in pixels
//   Bool        False if window too small to be sensible
//
{
   window(0) = Int((pos-N*width)+0.5);
   window(0) = min(nPts-1,max(0,window(0)));
   window(1) = Int((pos+N*width)+0.5);
   window(1) = min(nPts-1,max(0,window(1)));
                                      
   if ( abs(window(1)-window(0)) < 3) return False;
   return True;
} 





template <class T> 
void MomentCalcBase<T>::setPosLabel (String& title,
                                     const IPosition& pos)
{  
   ostrstream oss;

   oss << "Position = " << pos+1 << ends;
   String temp(oss.str());
   title = temp;
}



template <class T>
void MomentCalcBase<T>::setUpCoords (ImageMoments<T>& iMom,
                                     Vector<Double>& pixelIn,
                                     Vector<Double>& worldOut,
                                     Vector<Double>& sepWorldCoord,
                                     LogIO& os)
// 
// This function does two things.  It sets up the pixelIn
// and worldOut vectors needed by getMomentCoord. It also
// precomputes the vector of coordinates for the moment axis
// profile if it is separable
//
{

// Resize these vectors used for coordinate tarnsformations

   pixelIn.resize(iMom.pInImage_p->ndim());
   worldOut.resize(iMom.pInImage_p->ndim());
   
  
// Find the coordinate for the moment axis
   
   Int coordinate, axisInCoordinate;
   iMom.pInImage_p->coordinates().findPixelAxis(coordinate, 
       axisInCoordinate,  iMom.momentAxis_p);  
  
// Find out whether this coordinate is separable or not
  
   Int nPixelAxes = iMom.pInImage_p->coordinates().coordinate(coordinate).nPixelAxes();
   Int nWorldAxes = iMom.pInImage_p->coordinates().coordinate(coordinate).nWorldAxes();

      
// Precompute the profile coordinates if it is separable
      
   if (nPixelAxes == 1 && nWorldAxes == 1) {
      pixelIn = iMom.pInImage_p->coordinates().referencePixel();
      sepWorldCoord.resize(iMom.trc_p(iMom.momentAxis_p)-iMom.blc_p(iMom.momentAxis_p)+1);
      for (Int i=0; i<sepWorldCoord.nelements(); i++) {
         sepWorldCoord(i) = getMomentCoord(iMom, pixelIn, worldOut, Double(i));
      }
   } else {
      os << LogIO::NORMAL
           << "You have asked for a coordinate moment from a non-separable " << endl;
      os << "axis.  This means a coordinate must be computed for each pixel " << endl;
      os << "of each profile which will cause performance degradation" << LogIO::POST;
   }
}



template <class T>
void MomentCalcBase<T>::showGaussFit(ImageMoments<T>& iMom,
                                     const T peak,
                                     const T pos,
                                     const T width,
                                     const T level,
                                     const Vector<T>& x,
                                     const Vector<T>& y)
// 
// Plot the Gaussian fit and residual
//
{
   const Int nDPts = x.nelements();
   T xMin = x(0);
   T xMax = x(nDPts-1);
   Int nGPts = 100;
   T dx = (xMax - xMin)/nGPts;

// Setup functional

   const Gaussian1D<T> gauss(peak, pos, width);
                                      
   
// Allocate arrays

   Vector<T> xG(nGPts);
   Vector<T> yG(nGPts);
   
   
// Generate plot values
 
   int i = 0;
   Float xx;
   for (i=0,xx=xMin; i<nGPts; xx+=dx,i++) {
      xG(i) = xx;
      yG(i) = gauss(xx) + level;
   }
   cpgsci (7);
   drawLine (iMom, xG, yG);


// Now difference
   
   Vector<T> d(nDPts);
   for (i=0; i<nDPts; i++) {
      d(i) = y(i) - gauss(x(i));
   }
   cpgsci (2);
   drawLine (iMom, x, d);
   cpgsci (1);
}



template <class T>
Double& MomentCalcBase<T>::stdDeviation(ImageMoments<T>& iMom)
{
   return iMom.stdDeviation_p;
}
      

template <class T>
void MomentCalcBase<T>::yAutoMinMax(Float& yMin, 
                                    Float& yMax, 
                                    ImageMoments<T>& iMom)
{
   yMin = iMom.yMin_p;
   yMax = iMom.yMax_p;
}
 

   


// Derived class MomentClip

template <class T>
MomentClip<T>::MomentClip(Lattice<T>* pMaskLattice,
                          ImageMoments<T>& iMom,
                          LogIO& os,
                          const Int nLatticeOut)
                  : pMaskLattice_p(pMaskLattice),
                    iMom_p(iMom),
                    os_p(os)
{

// Set moment selection vector

   selectMoments_p = selectMoments(iMom_p);

// Set/check some dimensionality

   constructorCheck(retMoments_p, calcMoments_p,
                    selectMoments_p, nLatticeOut);

// Fish out region

   blc_p = blc(iMom_p);
   trc_p = trc(iMom_p);

// Fish out moment axis

   Int momAxis = momentAxis(iMom_p);

// Set up slice shape for extraction from masking lattice

   if (pMaskLattice_p != 0) {
      stride_p.resize(pMaskLattice_p->ndim());
      stride_p = 1;

      sliceShape_p.resize(pMaskLattice_p->ndim());
      sliceShape_p = 1;
      sliceShape_p(momAxis) = pMaskLattice_p->shape()(momAxis);
   }

// Make all plots with same y range ?

   fixedYLimits_p = fixedYLimits(iMom_p);
   yAutoMinMax(yMinAuto_p, yMaxAuto_p, iMom_p);

// Fish out pixel selection range

   range(range_p, doInclude_p, doExclude_p, iMom_p);

// Are we computing the expensive moments ?

   costlyMoments(iMom_p, doMedianI_p, doMedianV_p, doAbsDev_p);

// Are we plotting ?

   doPlot_p = ToBool(!device(iMom_p).empty());

// What is the axis type of the moment axis

   momAxisType_p = momentAxisName(iMom_p);

// Are we computing coordinate-dependent moments.  If
// so precompute coordinate vector is momebt axis separable

   doCoordCalc_p = doCoordCalc(iMom_p);
   if (doCoordCalc_p) setUpCoords(iMom_p, pixelIn_p, worldOut_p,
                                  sepWorldCoord_p, os_p);
}


template <class T>
MomentClip<T>::~MomentClip()
{;}

template <class T> 
T MomentClip<T>::collapse(const Vector<T>& vector,
                                const IPosition& pos)
{
   throw(AipsError("MomentClip<T>::collapse(Vector<T>&, IPosition&): not implemented"));
   T tmp = 0;
   return tmp;
}


template <class T> 
Vector<T>& MomentClip<T>::multiCollapse(const Vector<T>& profile,
                                              const IPosition& inPos)
//
// Generate masked moments of this profile where the mask is
// generated from the primary lattice or an ancilliary lattice
//
{

// Fish out the masking image slice if needed.  Stupid slice functions require
// me to create the maskSlice empty every time so degenerate axes can be 
// chucked out.  The masking Lattice is only as big as the region that was 
// requested in ImageMoments, so we have to subtract of the blc in dealing with positions.

   if (pMaskLattice_p && (doInclude_p || doExclude_p)) {
      Array<T> maskSlice;
      pMaskLattice_p->getSlice(maskSlice, inPos-blc_p,
                               sliceShape_p, stride_p, True);
      maskSliceRef_p.reference(maskSlice);
   }


// Set up a pointer to the primary or ancilliary vector object
// Also make a pointer that we can use for fast access to the Vector
 
   const T* pMask = 0;      
   Bool deleteIt;
   if (pMaskLattice_p  && (doInclude_p || doExclude_p)) {
      pMaskProfile_p = &maskSliceRef_p;
      pMask = maskSliceRef_p.getStorage(deleteIt);
   } else {
      pMaskProfile_p = &profile;
      pMask = profile.getStorage(deleteIt);
   }


// Plot spectrum if asked
      
   if (doPlot_p) {
      makeAbcissa(abcissa_p, pMaskProfile_p->nelements());
      String xLabel;
      if (momAxisType_p.empty()) {
         xLabel = "x (pixels)";
      } else {
         xLabel = momAxisType_p + " (pixels)";
      }
      const String yLabel("Intensity");
      String title;
      setPosLabel (title, inPos);

      cpgpage();
      drawLine (iMom_p, abcissa_p, *pMaskProfile_p, fixedYLimits_p, 
                yMinAuto_p, yMaxAuto_p, xLabel, yLabel, title);


// Draw on clip levels and arrows

      if (doInclude_p || doExclude_p) {
         cpgsci (5);
         drawHorizontal(T(range_p(0)));
         drawHorizontal(T(range_p(1)));
      
         float xMin, xMax, yMin, yMax;
         cpgqwin (&xMin, &xMax, &yMin, &yMax);
         float x = xMin + 0.05*(xMax-xMin);
         float y = range_p(1) - 0.2*range_p(1);
         cpgarro (x, float(range_p(1)), x, y);
         y = range_p(0) + 0.2*range_p(0);
         cpgarro (x, y, x, float(range_p(0)));
         cpgsci(1);
      }
   }


// Resize array for median.  Is resized correctly later
 
   Int nPts = profile.nelements();
   selectedData_p.resize(nPts);
   selectedDataIndex_p.resize(nPts);


// Were the profile coordinates precomputed ?
      
   Bool preComp = ToBool(sepWorldCoord_p.nelements() > 0);


// Compute moments

   Double s0  = 0.0;
   Double s0Sq = 0.0;
   Double s1  = 0.0;
   Double s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   Double dMin =  1.0e30;
   Double dMax = -1.0e30;
   Double coord = 0.0;
   Int i, j;

   if (doInclude_p) {
      for (i=0,j=0; i<nPts; i++) {
         if (pMask[i] >= range_p(0) && pMask[i] <= range_p(1)) {
            if (preComp) {
               coord = sepWorldCoord_p(i);              
            } else if (doCoordCalc_p) {
               coord = getMomentCoord(iMom_p, pixelIn_p,
                                      worldOut_p, Double(i));       
            }
            accumSums(s0, s0Sq, s1, s2, iMin, iMax,     
                      dMin, dMax, i, profile(i), coord);
            selectedData_p(j) = profile(i);
            selectedDataIndex_p(j) = i;
            j++;
         }
      }
      nPts = j;
   } else if (doExclude_p) {
      for (i=0,j=0; i<nPts; i++) {
         if (pMask[i] <= range_p(0) || pMask[i] >= range_p(1)) {
            if (preComp) {
               coord = sepWorldCoord_p(i); 
            } else if (doCoordCalc_p) {
               coord = getMomentCoord(iMom_p, pixelIn_p,
                                      worldOut_p, Double(i));
            }
            accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                      dMin, dMax, i, profile(i), coord);
            selectedData_p(j) = profile(i);
            selectedDataIndex_p(j) = i;
            j++;
         }
      }
      nPts = j;
   } else {    

// No clip range, so ancilliary profile will not have been computed
// by ImageMoments

      for (i=0; i<nPts; i++) {
         if (preComp) {
            coord = sepWorldCoord_p(i);
         } else if (doCoordCalc_p) {
            coord = getMomentCoord(iMom_p, pixelIn_p,
                                   worldOut_p, Double(i));
         }
         accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                   dMin, dMax, i, profile(i), coord);
         selectedData_p(i) = profile(i);
         selectedDataIndex_p(i) = i;
      }
   }
 

// Delete pointer memory

   if (pMaskLattice_p  && (doInclude_p || doExclude_p)) {
      maskSliceRef_p.freeStorage(pMask, deleteIt);
   } else {
      profile.freeStorage(pMask, deleteIt);
   }


   
// If no points make moments zero. Blank at a later date.
               
   if (nPts==0) {
      retMoments_p = 0.0;
      return retMoments_p;
   }        
   
         
// Absolute deviations of I from mean needs an extra pass.
         
   Double sumAbsDev = 0.0;
   if (doAbsDev_p) {
      Double iMean = s0 / nPts;
      for (i=0; i<nPts; i++) sumAbsDev += abs(selectedData_p(i) - iMean);
   }
 
// Median of I
         
   T dMedian = 0.0;
   if (doMedianI_p) {
      selectedData_p.resize(nPts,True);
      dMedian = median(selectedData_p.ac());
   }
       
 
// Median coordinate.  ImageMoments will only be allowing this if
// we are not offering the ancilliary lattice, and with an include or exclude range.
// Pretty dodgy   
         
   T vMedian = 0.0;
   if (doMedianV_p) {
      if (doInclude_p || doExclude_p) {
            
// Treat spectrum as a probability distribution for velocity
// and generate cumulative probability (it's already sorted
// of course).
 
         selectedData_p.resize(nPts,True);
         selectedData_p(0) = abs(selectedData_p(0));
         T dataMax = selectedData_p(0);
         for (i=1; i<nPts; i++) {
            selectedData_p(i) += abs(selectedData_p(i-1));
            dataMax = max(dataMax,selectedData_p(i));
         }
// Find 1/2 way value (well, the first one that occurs)
 
         T halfMax = dataMax/2.0;
         Int iVal;
         for (i=0; i<nPts; i++) {
            if (selectedData_p(i) >= halfMax) {
               iVal = i;
               break;
            }
         }
 
// Linearly interpolate to velocity index
                                
         Double interpPixel;
         if (iVal > 0) {
            Double m = (selectedData_p(iVal) - selectedData_p(iVal-1)) /
                      (selectedDataIndex_p(iVal) - selectedDataIndex_p(iVal-1));
            Double b = selectedData_p(iVal) - m*selectedDataIndex_p(iVal);
            interpPixel = (selectedData_p(iVal) -b) / m;
         } else {
            interpPixel = selectedDataIndex_p(iVal);   
         }
 
// Find world coordinate of that pixel on the moment axis
 
        vMedian = getMomentCoord(iMom_p, pixelIn_p,
                                 worldOut_p, interpPixel);
      }   
   }
 
// Fill all moments array
   
   setCalcMoments(iMom_p, calcMoments_p, pixelIn_p, worldOut_p, doCoordCalc_p,
                  dMedian, vMedian, nPts, s0, s1, s2, s0Sq, 
                  sumAbsDev, dMin, dMax, iMin, iMax);

// Return vector of selected moments by reference

   for (i=0; i<selectMoments_p.nelements(); i++) {
      retMoments_p(i) = calcMoments_p(selectMoments_p(i));
   }


   return retMoments_p;
}





// Derived class MomentWindow

template <class T>
MomentWindow<T>::MomentWindow(Lattice<T>* pMaskLattice,
                              ImageMoments<T>& iMom,
                              LogIO& os,
                              const Int nLatticeOut)
                  : pMaskLattice_p(pMaskLattice),
                    iMom_p(iMom),
                    os_p(os)
{
// Set moment selection vector

   selectMoments_p = selectMoments(iMom_p);

// Set/check some dimensionality

   constructorCheck(retMoments_p, calcMoments_p,
                    selectMoments_p, nLatticeOut);

// Fish out region

   blc_p = blc(iMom_p);
   trc_p = trc(iMom_p);

// Fish out moment axis

   Int momAxis = momentAxis(iMom_p);

// Set up slice shape for extraction from masking lattice

   if (pMaskLattice_p != 0) {
      stride_p.resize(pMaskLattice_p->ndim());
      stride_p = 1;

      sliceShape_p.resize(pMaskLattice_p->ndim());
      sliceShape_p = 1;
      sliceShape_p(momAxis) = pMaskLattice_p->shape()(momAxis);
   }

// Make all plots with same y range ?

   fixedYLimits_p = fixedYLimits(iMom_p);
   yAutoMinMax(yMinAuto_p, yMaxAuto_p, iMom_p);

// Are we computing the expensive moments ?

   costlyMoments(iMom_p, doMedianI_p, doMedianV_p, doAbsDev_p);

// Are we plotting ?

   doPlot_p = ToBool(!device(iMom_p).empty());

// What is the axis type of the moment axis
   
   momAxisType_p = momentAxisName(iMom_p);

// Are we computing coordinate-dependent moments.  If
// so precompute coordinate vector is momebt axis separable

   doCoordCalc_p = doCoordCalc(iMom_p);
   if (doCoordCalc_p) setUpCoords(iMom_p, pixelIn_p, worldOut_p, 
                                  sepWorldCoord_p, os_p);

// Are we fitting, automatically or interactively ?

   doAuto_p = doAuto(iMom_p);
   doFit_p = doFit(iMom_p);

// Values to assess if spectrum is all noise or not

   peakSNR_p = peakSNR(iMom_p);
   stdDeviation_p = stdDeviation(iMom_p);
}


template <class T>
MomentWindow<T>::~MomentWindow()
{;}


template <class T> 
T MomentWindow<T>::collapse(const Vector<T>& vector,
                            const IPosition& pos)
{
   throw(AipsError("MomentWindow<T>::collapse(Vector<T>&, IPosition&): not implemented"));
   T tmp = 0;
   return tmp;
}


template <class T> 
Vector<T>& MomentWindow<T>::multiCollapse(const Vector<T>& profile,
                                          const IPosition& inPos)
//
// Generate windowed moments of this profile 
//
{

// Fish out masking image slice if needed.  Stupid slice functions require
// me to create the slice empty every time so degenerate axes can 
// be chucked out.  The masking Lattice is only as big as the region that 
// was requested in ImageMoments, so we have to subtract of the blc in 
// dealing with positions.


   if (pMaskLattice_p) {
      Array<T> maskSlice;
      pMaskLattice_p->getSlice(maskSlice, inPos-blc_p,
                               sliceShape_p, stride_p, True);
      maskSliceRef_p.reference(maskSlice);
   }


// Set up a pointer to the primary or ancillairy profile Vector object
// Also make a pointer that we can use for fast access to the Vector
 
   const T* pProfile = 0;      
   Bool deleteIt;
   if (pMaskLattice_p) {
      pProfile_p = &maskSliceRef_p;
      pProfile = maskSliceRef_p.getStorage(deleteIt);
   } else {
      pProfile_p = &profile;
      pProfile = profile.getStorage(deleteIt);
   }



// Make abcissa and labels
   
   static Bool allSubsequent = False;
   static Vector<Int> window(2);  
   static Int nPts = 0;
      
   makeAbcissa (abcissa_p, pProfile_p->nelements());
   String xLabel;
   if (momAxisType_p.empty()) {
      xLabel = "x (pixels)";
   } else {
      xLabel = momAxisType_p + " (pixels)";
   }
   const String yLabel("Intensity");
   String title;
   setPosLabel(title, inPos);



// Do the window selection

   if (doAuto_p) {
   
// Define the window automatically

      Vector<T> gaussPars;
      getAutoWindow (iMom_p, window,  abcissa_p, *pProfile_p, peakSNR_p, 
                     stdDeviation_p, doFit_p, doPlot_p, fixedYLimits_p, 
                     yMinAuto_p, yMaxAuto_p, xLabel, yLabel, title);
   } else {

// Define the window interactively, unless the user has told us when
// doing the previous spectrum that they wish to apply that window
// to all subsequent ones
    

      if (!doFit_p && !allSubsequent) {
         os_p << endl;
         os_p << LogIO::NORMAL << "Mark extremum (left), redo (middle), reject (right), all subsequent (S)" << LogIO::POST;
      }

      if (!allSubsequent) {
         getInterWindow (iMom_p, allSubsequent, os_p, window, doFit_p, abcissa_p, 
                         *pProfile_p, fixedYLimits_p, yMinAuto_p, yMaxAuto_p, 
                         xLabel, yLabel, title);
      } else if (nPts != 0) {
         cpgpage();
         drawLine (iMom_p, abcissa_p, *pProfile_p, fixedYLimits_p, yMinAuto_p, 
                   yMaxAuto_p, xLabel, yLabel, title);
         drawWindow (iMom_p, window);
      }
   }
   nPts = window(1) - window(0) + 1;


// If no points make moments zero. Blank at a later date.
               
   if (nPts==0) {
      retMoments_p = 0.0;
      return retMoments_p;
   }        


// Resize array for median.  Is resized correctly later
 
   selectedData_p.resize(nPts);
      

// Were the profile coordinates precomputed ?
      
   Bool preComp = ToBool(sepWorldCoord_p.nelements() > 0);


// Compute moments
            
   Double s0  = 0.0;
   Double s0Sq = 0.0;
   Double s1  = 0.0;
   Double s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   Double dMin =  1.0e30;
   Double dMax = -1.0e30;
   Double coord = 0.0;

   for (Int i=window(0); i<=window(1); i++) {
      if (preComp) {
         coord = sepWorldCoord_p(i);
      } else if (doCoordCalc_p) {
         coord = getMomentCoord(iMom_p, pixelIn_p,
                                worldOut_p, Double(i));
      }
      accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                dMin, dMax, i, pProfile[i], coord);
      selectedData_p(i-window(0)) = pProfile[i];
   }

         
// Absolute deviations of I from mean needs an extra pass.
         
   Double sumAbsDev = 0.0;
   if (doAbsDev_p) {
      Double iMean = s0 / nPts;
      for (Int i=window(0); i<=window(1); i++) {
         sumAbsDev += abs(Double(pProfile[i] - iMean));
      }
   }


// Delete memory associated with pointer

   if (pMaskLattice_p) {
      maskSliceRef_p.freeStorage(pProfile, deleteIt);
   } else {
      profile.freeStorage(pProfile, deleteIt);
   }

 
// Median of I
         
   T dMedian = 0.0;
   if (doMedianI_p) {
      selectedData_p.resize(nPts,True);
      dMedian = median(selectedData_p.ac());
   }
       
// Fill all moments array
   
   T vMedian = 0;   
   setCalcMoments(iMom_p, calcMoments_p, pixelIn_p, worldOut_p, doCoordCalc_p,
                  dMedian, vMedian, nPts, s0, s1, s2, s0Sq, 
                  sumAbsDev, dMin, dMax, iMin, iMax);


// Return vector of selected moments by reference

   for (i=0; i<selectMoments_p.nelements(); i++) {
      retMoments_p(i) = calcMoments_p(selectMoments_p(i));
   }


   return retMoments_p;
}




// Derived class MomentFit

template <class T>
MomentFit<T>::MomentFit(ImageMoments<T>& iMom,
                        LogIO& os,
                        const Int nLatticeOut)
                  : iMom_p(iMom),
                    os_p(os)
{
// Set moment selection vector

   selectMoments_p = selectMoments(iMom_p);

// Set/check some dimensionality

   constructorCheck(retMoments_p, calcMoments_p,
                    selectMoments_p, nLatticeOut);

// Fish out moment axis

   Int momAxis = momentAxis(iMom_p);

// Make all plots with same y range ?

   fixedYLimits_p = fixedYLimits(iMom_p);
   yAutoMinMax(yMinAuto_p, yMaxAuto_p, iMom_p);

// Are we computing the expensive moments ?

   costlyMoments(iMom_p, doMedianI_p, doMedianV_p, doAbsDev_p);

// Are we plotting ?
   doPlot_p = ToBool(!device(iMom_p).empty());

// What is the axis type of the moment axis

   momAxisType_p = momentAxisName(iMom_p);

// Are we computing coordinate-dependent moments.  If so
// precompute coordinate vector if moment axis is separable
 
   doCoordCalc_p = doCoordCalc(iMom_p);
   if (doCoordCalc_p) setUpCoords(iMom_p, pixelIn_p, worldOut_p, 
                                  sepWorldCoord_p, os_p);

// Are we fitting, automatically or interactively ?
   
   doAuto_p = doAuto(iMom_p);
   doFit_p = doFit(iMom_p);
         
// Values to assess if spectrum is all noise or not

   peakSNR_p = peakSNR(iMom_p);
   stdDeviation_p = stdDeviation(iMom_p);
}


template <class T>
MomentFit<T>::~MomentFit()
{;}


template <class T> 
T MomentFit<T>::collapse(const Vector<T>& vector,
                         const IPosition& inPos)
{
   throw(AipsError("MomentFit<T>::collapse(Vector<T>&, IPosition&): not implemented"));
   T tmp = 0;
   return tmp;
}




template <class T> 
Vector<T>& MomentFit<T>::multiCollapse(const Vector<T>& profile,
                                       const IPosition& inPos)
//
// Generate moments from a Gaussian fit of this profile
//
{

// Create the abcissa array and some labels
       
   const Int nPts = profile.nelements();
   Vector<T> gaussPars(4);
   makeAbcissa (abcissa_p, nPts);
   String xLabel;
   if (momAxisType_p.empty())
      xLabel = "x (pixels)";
   else
      xLabel = momAxisType_p + " (pixels)";
   String yLabel("Intensity"); 
   String title;
   setPosLabel(title, inPos);


// Fit the profile

   if (doAuto_p) {

// Automatic

      if (!getAutoGaussianFit (iMom_p, gaussPars, abcissa_p, profile, peakSNR_p, 
                               stdDeviation_p, doPlot_p, fixedYLimits_p,
                               yMinAuto_p, yMaxAuto_p, xLabel, yLabel, title)) {
         retMoments_p = 0;   
         return retMoments_p;
      }

   } else {

// Interactive
   
       if (!getInterGaussianFit(iMom_p, gaussPars, os_p, abcissa_p, profile, 
                                fixedYLimits_p, yMinAuto_p, yMaxAuto_p,
                                xLabel, yLabel, title)) {
         retMoments_p = 0;   
         return retMoments_p;
      }
   }
   

// Were the profile coordinates precomputed ?
      
   Bool preComp = ToBool(sepWorldCoord_p.nelements() > 0);


// Set Gaussian functional values.  We reuse the same functional that
// was used in the interactive fitting display process.

   gauss_p.setHeight(gaussPars(0));
   gauss_p.setCenter(gaussPars(1));
   gauss_p.setWidth(gaussPars(2));


// Compute moments
            
   Double s0  = 0.0;
   Double s0Sq = 0.0;
   Double s1  = 0.0;
   Double s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   Double dMin =  1.0e30;
   Double dMax = -1.0e30;
   Double coord = 0.0;
   T xx;
   Vector<T> gData(nPts);
   
   for (Int i=0; i<nPts; i++) {
      xx = i;
      gData(i) = gauss_p(xx) + gaussPars(3);
      
      if (preComp) {
         coord = sepWorldCoord_p(i);
      } else if (doCoordCalc_p) {
         coord = getMomentCoord(iMom_p, pixelIn_p, 
                                worldOut_p, Double(i));
      }
      accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                dMin, dMax, i, gData(i), coord);
   }
   


// Absolute deviations of I from mean needs an extra pass.
         
   Double sumAbsDev = 0.0;
   if (doAbsDev_p) {
      Double iMean = s0 / nPts;
      for (Int i=0; i<nPts; i++) {
         sumAbsDev += abs(Double(gData(i) - iMean));
      }
   }


// Median of I
         
   T dMedian = 0.0;
   if (doMedianI_p) dMedian = median(gData.ac());
   T vMedian = 0.0;
       
// Fill all moments array
   
   setCalcMoments(iMom_p, calcMoments_p, pixelIn_p, worldOut_p, doCoordCalc_p,
                  dMedian, vMedian, nPts, s0, s1, s2, s0Sq,
                  sumAbsDev, dMin, dMax, iMin, iMax);


// Return vector of selected moments by reference

   for (i=0; i<selectMoments_p.nelements(); i++) {
      retMoments_p(i) = calcMoments_p(selectMoments_p(i));
   }


   return retMoments_p;
}





