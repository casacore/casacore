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
#include <trial/Images/MomentCalculator.h>

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <trial/Fitting/NonLinearFitLM.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Functionals/SumFunction.h>
#include <trial/Functionals/FuncWithAutoDerivs.h>
#include <trial/Images/ImageMoments.h>
#include <trial/Images/ImageUtilities.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/QMath.h>
#include <aips/Logging/LogIO.h> 
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>



// Base class MomentCalcBase 

template <class T> 
MomentCalcBase<T>::~MomentCalcBase()
{;}

template <class T>
void MomentCalcBase<T>::init (uInt nOutPixelsPerCollapse)
{
   AlwaysAssert (nOutPixelsPerCollapse == 1, AipsError);
}


template <class T>
uInt MomentCalcBase<T>::allNoise (T& dMean, 
                                  const Vector<T>& data,
                                  const Vector<Bool>& mask,
                                  const T peakSNR,
                                  const T stdDeviation) const
//
// Try and work out whether this spectrum is all noise
// or not.  We don't bother with it if it is noise.
// We compare the peak with sigma and a cutoff SNR
//
// Returns 1 if all noise
// Returns 2 if all masked
// Returns 0 otherwise
//
{
   T dMin, dMax;
   uInt minPos, maxPos;
   if (!stats(dMin, dMax, minPos, maxPos, dMean, data, mask)) return 2;


// Assume we are continuum subtracted so outside of line mean=0

   const T rat = max(abs(dMin),abs(dMax)) / stdDeviation;

//   cout << "min,max,mean,sigma,peakSNR,SNR=" << dMin << " " << dMax << " " << dMean
//        << stdDeviation_p << " " << peakSNR << " " << rat << LogIO::POST;
   if (rat < peakSNR) {
      return 1;
   } else {
      return 0;
   }
}

typedef Vector<Int> gpp_VectorInt;
template <class T>
void MomentCalcBase<T>::constructorCheck(Vector<T>& calcMoments, 
                                         const gpp_VectorInt& selectMoments,
                                         const uInt nLatticeOut) const
 {
// Number of output lattices must equal the number of moments
// the user asked to calculate

   AlwaysAssert(nLatticeOut == selectMoments.nelements(), AipsError);

// Number of requested moments must be in allowed range

   AlwaysAssert(selectMoments.nelements() <= nMaxMoments(), AipsError);
   AlwaysAssert(selectMoments.nelements() > 0, AipsError);

// Resize the vector that will hold ALL possible moments
   
   calcMoments.resize(nMaxMoments());
}





template <class T>
void MomentCalcBase<T>::costlyMoments(ImageMoments<T>& iMom,
                                      Bool& doMedianI,
                                      Bool& doMedianV,
                                      Bool& doAbsDev) const
{
   typedef ImageMoments<Float> IM;
   for (uInt i=0; i<iMom.moments_p.nelements(); i++) {
      if (iMom.moments_p(i) == IM::MEDIAN) doMedianI = True;
      if (iMom.moments_p(i) == IM::MEDIAN_COORDINATE) doMedianV = True;
      if (iMom.moments_p(i) == IM::ABS_MEAN_DEVIATION) doAbsDev = True;
   }      
}

template <class T>
Bool& MomentCalcBase<T>::doAuto(ImageMoments<T>& iMom) const
{
// Get it from ImageMoments private data

   return iMom.doAuto_p;
}


template <class T>
Bool& MomentCalcBase<T>::doFit(ImageMoments<T>& iMom) const
{
// Get it from ImageMoments private data

   return iMom.doFit_p;
}



template <class T>
PGPlotter& MomentCalcBase<T>::device(ImageMoments<T>& iMom) const
{
   return iMom.plotter_p;
}


template <class T>
Bool MomentCalcBase<T>::doCoordCalc(ImageMoments<T>& iMom) const
{
// Figure out if we need to compute the coordinate of each profile pixel index
// for each profile.  This is very expensive for non-separable axes.

   typedef ImageMoments<Float> IM;
   for (uInt i=0; i<iMom.moments_p.nelements(); i++) {
      if (iMom.moments_p(i) == IM::WEIGHTED_MEAN_COORDINATE ||
          iMom.moments_p(i) == IM::WEIGHTED_DISPERSION_COORDINATE) return True;
   }
   return False;
}



template <class T>
void MomentCalcBase<T>::drawHorizontal(const T& y,
                                       PGPlotter& plotter) const

//
// Draw a horizontal line across the full x range of the plot
//
{
   Vector<Float> minMax(4);
   minMax = plotter.qwin();
   Float yy = convertT(y);

   plotter.move (minMax(0), yy);
   plotter.draw (minMax(1), yy);
}

template <class T>
void MomentCalcBase<T>::drawLine (const Vector<T>& x,
                                  const Vector<T>& y,
                                  PGPlotter& plotter) const
//
// Draw  a spectrum on the current panel
// with the box already drawn
//
{
// Pass it on to ImageMoments who has to do this too

   ImageMoments<T>::drawLine(x, y, plotter);
} 


template <class T>
Bool MomentCalcBase<T>::drawSpectrum (const Vector<T>& x,
                                      const Vector<T>& y,
                                      const Vector<Bool>& mask,
                                      const Bool fixedYLimits,
                                      const T yMinAuto,
                                      const T yMaxAuto,
                                      const String xLabel,
                                      const String yLabel,
                                      const String title,
                                      const Bool advance,
                                      PGPlotter& plotter) const
//
// Draw and label a spectrum on the next panel
// Some of the spectrum may be masked, so we draw it
// in chunks
//
// If the mask is all good, there are no elements in it.
//
// Returns false if all pixels are masked.
//
{
// Find number of segments in this vector. Bug out if none.


   Vector<uInt> start;
   Vector<uInt> nPtsPerSeg;
   uInt nSeg;
   if (mask.nelements() == 0) {
      nSeg = 1;
      start.resize(1);
      start(0) = 0;
      nPtsPerSeg.resize(1);
      nPtsPerSeg(0) = y.nelements();
   } else {
      lineSegments (nSeg, start, nPtsPerSeg, mask);
      if (nSeg == 0) return False;
   }
   

// Find extrema.
 
   T yMin, yMax, yMean;
   Float yMinF, yMaxF;
   if (!fixedYLimits) {
      if (mask.nelements() == 0) {
         minMax (yMin, yMax, y.ac());
      } else {
         uInt minPos, maxPos;       
         if(!stats(yMin, yMax, minPos, maxPos, yMean, y, mask)) return False;
      }

      yMinF = convertT(yMin);
      yMaxF = convertT(yMax);
      ImageUtilities::stretchMinMax (yMinF, yMaxF);
   } else {
      yMinF = convertT(yMinAuto);
      yMaxF = convertT(yMaxAuto);
   }

   const uInt nPts = x.nelements();
   Float xMin = 0.0;
   Float xMax = Float(nPts);
   ImageUtilities::stretchMinMax (xMin, xMax); 


// Draw box and label

   if (advance) plotter.page();
   plotter.swin (xMin, xMax, yMinF, yMaxF);
   plotter.box ("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   plotter.lab (xLabel.chars(), yLabel.chars(), "");
   plotter.mtxt ("T", 1.0, 0.5, 0.5, title.chars());


// Loop over segments and plot them
                                 
   Vector<Float> xtmp, ytmp;
   for (uInt i=0; i<nSeg; i++) {
      const uInt ip = start(i);
      if (nPtsPerSeg(i) == 1) {
          xtmp.resize(1);
          ytmp.resize(1);
          xtmp(0) = convertT(x(ip));
          ytmp(0) = convertT(y(ip));
          plotter.pt (xtmp, ytmp, 1);
      } else {
          xtmp.resize(nPtsPerSeg(i));
          ytmp.resize(nPtsPerSeg(i));
          for (uInt j=0; j<nPtsPerSeg(i); j++) {
             xtmp(j) = convertT(x(start(i)+j));
             ytmp(j) = convertT(y(start(i)+j));
          }
          plotter.line (xtmp, ytmp);
      }
   }
   return True;
}



template <class T>
void MomentCalcBase<T>::drawMeanSigma (const T dMean,
                                       const T dSigma,
                                       PGPlotter& plotter) const

//
// Draw a horizontal line on the spectrum plot at
// the mean value, and 2 horizontal lines at
// mean +/- sigma
//    
{
   plotter.sci(7);
   drawHorizontal(dMean, plotter);
   plotter.sci(5);
   drawHorizontal(dMean+dSigma, plotter);
   drawHorizontal(dMean-dSigma, plotter);
   plotter.sci(1);
}


template <class T>
void MomentCalcBase<T>::drawVertical (const T loc,
                                      const T yMin,
                                      const T yMax,
                                      PGPlotter& plotter) const
{  
// Pass it on to ImageMoments

   ImageMoments<T>::drawVertical(loc, yMin, yMax, plotter);
}
  



template <class T>
Bool MomentCalcBase<T>::findNextDatum (uInt& iFound,
                                       const uInt& n,
                                       const Vector<Bool>& mask,
                                       const uInt& iStart,
                                       const Bool& findGood) const
//
// Find the next good (or bad) point in an array.
// A good point in the array has a non-zero value.
//
// Inputs:
//  n        Number of points in array
//  mask     Vector containing counts.  
//  iStart   The index of the first point to consider
//  findGood If True look for next good point.
//           If False look for next bad point
// Outputs:
//  iFound   Index of found point
//  Bool     False if didn't find another valid datum
{
   for (uInt i=iStart; i<n; i++) {
      if ( (findGood && mask(i)) ||
           (!findGood && !mask(i)) ) {
        iFound = i;
        return True;
      }
   }
   return False;
}


template <class T>
Bool MomentCalcBase<T>::fitGaussian (uInt& nFailed, 
                                     T& peak,
                                     T& pos,
                                     T& width,
                                     T& level,
                                     const Vector<T>& x,
                                     const Vector<T>& y,
                                     const Vector<Bool>& mask,
                                     const T peakGuess,
                                     const T posGuess,
                                     const T widthGuess,
                                     const T levelGuess) const
// 
// Fit Gaussian pos * exp(-4ln2*(x-pos)**2/width**2)
// width = fwhm
// 
// Returns false if fit fails or all masked
//
{

// Select unmasked pixels

   uInt j = 0;
   Vector<T> xSel(y.nelements());
   Vector<T> ySel(y.nelements());
   for (uInt i=0; i<y.nelements(); i++) {
     if (mask(i)) {
       xSel(j) = x(i);
       ySel(j) = y(i);
       j++;
     }
   }
   uInt nPts = j;
   if (nPts == 0) return False;

   xSel.resize(nPts,True);
   ySel.resize(nPts,True);

      
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
                                   

// Perform fit on unmasked data

   Vector<T> resultSigma(nPts);
   Vector<T> solution;
   resultSigma = 1;
   try {
     solution = fitter.fit(xSel, ySel, resultSigma);
   } catch (AipsError x) {
      nFailed++;
      return False;
   } end_try;


// Return values of fit
   
//   cout << "SOlution = " << solution.ac() << LogIO::POST;
   
   peak  = solution(0);
   pos   = solution(1);
   width = abs(solution(2));
   level = solution(3);


// Return status

   if (!fitter.converged()) nFailed++;
   return fitter.converged();
                                   
}



template <class T>
Bool& MomentCalcBase<T>::fixedYLimits(ImageMoments<T>& iMom) const
{
   return iMom.fixedYLimits_p;
}



template <class T>
Bool MomentCalcBase<T>::getAutoGaussianFit (uInt& nFailed,
                                            Vector<T>& gaussPars,
                                            const Vector<T>& x,
                                            const Vector<T>& y,
                                            const Vector<Bool>& mask,
                                            const T peakSNR,
                                            const T stdDeviation,
                                            PGPlotter& plotter,
                                            const Bool fixedYLimits,
                                            const T yMinAuto,
                                            const T yMaxAuto,
                                            const String xLabel,
                                            const String yLabel,
                                            const String title) const
//
// Automatically fit a Gaussian and return the Gaussian parameters.
// If a plotting device is active, we also plot the spectra and fits
//
// Inputs:
//   x,y        Vector containing the data
//   mask       True is good
//   plotter    Plot spectrum and optionally the  window
//   x,yLabel   Labels
//   title
// Input/output
//   nFailed    Cumulative number of failed fits
// Output:
//   gaussPars  The gaussian parameters, peak, pos, fwhm
//   Bool       If False then this spectrum has been rejected (all
//              masked, all noise, failed fit)
//
{
    
   
// Plot spectrum if desired. If all masked, bug out.
      
   if (plotter.isAttached()) {
      if (!drawSpectrum (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto,
           xLabel, yLabel, title, True, plotter)) return False;
   }


// See if this spectrum is all noise.  If so, forget it.
// Return straight away if all masked
   
   T dMean;
   uInt iNoise = allNoise(dMean, y, mask, peakSNR, stdDeviation);
   if (iNoise == 2) return False;
 
// Draw on mean and sigma
  
   if (plotter.isAttached()) {
      drawMeanSigma (dMean, stdDeviation, plotter);
      if (iNoise==1) plotter.mtxt ("T", 1.0, 0.0, 0.0, "NOISE");
   }  
   if (iNoise==1) {
      gaussPars = 0;  
      return False;
   }

// Work out guesses for Gaussian

   T peakGuess, posGuess, widthGuess, levelGuess;
   T pos, width, peak, level;
   if (!getAutoGaussianGuess(peakGuess, posGuess, widthGuess, 
                             levelGuess, x, y, mask)) return False;
   peakGuess = peakGuess - levelGuess;


// Fit gaussian. Do it twice.
  
   if (!fitGaussian (nFailed, peak, pos, width, level, x, y, mask, peakGuess, 
                     posGuess, widthGuess, levelGuess)) {
      gaussPars = 0;
      return False;
   }  
   gaussPars(0) = peak;
   gaussPars(1) = pos;
   gaussPars(2) = width;
   gaussPars(3) = level;

   
// Plot the fit
   
   if (plotter.isAttached()) showGaussFit (peak, pos, width, level, x, 
                                           y, mask, plotter);
   
   return True;
}



template <class T>
Bool MomentCalcBase<T>::getAutoGaussianGuess (T& peakGuess,
                                              T& posGuess,
                                              T& widthGuess,
                                              T& levelGuess,
                                              const Vector<T>& x,
                                              const Vector<T>& y,
                                              const Vector<Bool>& mask) const
//
// Make a wild stab in the dark as to what the Gaussian
// parameters of this spectrum might be
//    
// Returns False if all masked
{

// Find peak and position of peak

   uInt minPos, maxPos;
   T dMin, dMax, dMean;
   if (!stats(dMin, dMax, minPos, maxPos, dMean, y, mask)) return False;

   posGuess = x(maxPos);
   peakGuess = dMax;
   levelGuess = dMean;

// Nothing much is very robust.  Assume the line is reasonably
// sampled and set its width to a few pixels.  Totally ridiculous.
                                            
   widthGuess = 5;
                                            
//   cout << "Guess: peak,pos,width=" << peakGuess << ", " << posGuess << "," <<
//           widthGuess << LogIO::POST;

   return True;
}




template <class T>
void MomentCalcBase<T>::getButton(Bool& ditch,
                                  Bool& redo,
                                  PGPlotter& plotter) const
//
// Read the PGPLOT cursor and interpret the button
// pushed
//
{
// Fish out window
 
   Vector<Float> minMax(4);
   minMax = plotter.qwin();


   Float x = (minMax(0)+minMax(1))/2;
   Float y = (minMax(2)+minMax(3))/2;
   String str;
   ImageMoments<T>::readCursor (plotter, x, y, str);
   str.upcase();
   ditch = False;
   redo = False;  
   if (str == "X") {
      ditch = True;
   } else if (str == "D") {
      redo = True;
   }

}


template <class T>
Bool MomentCalcBase<T>::getInterGaussianFit (uInt& nFailed,
                                             Vector<T>& gaussPars,
                                             LogIO& os,
                                             const Vector<T>& x,
                                             const Vector<T>& y,
                                             const Vector<Bool>& mask,
                                             const Bool fixedYLimits,
                                             const T yMinAuto,
                                             const T yMaxAuto,
                                             const String xLabel,
                                             const String yLabel,
                                             const String title,
                                             PGPlotter& plotter) const
//
// With the cursor, define a guess for a Gaussian fit,
// and do the fit over and over until they are happy.
// Then return the Gaussian parameters.
//
// Inputs:
//   x,y       The abcissa and spectrum
//   mask      Mask.  True is good.
//   x,yLabel  Labels
//   title     Title of plot
// Input/output
//   nFailed   Cumualtive number of failures in fitting
// Outputs:
//   gaussPars The gaussian parameters (peak, pos, width, level)
//   Bool      True if all successful, False if spectrum rejected
//             because all noise or all masked
//
{
   
// First draw the spectrum
         
   if (!drawSpectrum (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto,
                      xLabel, yLabel, title, True, plotter)) return False;

// Get user's guess and fit until satisfied

   Bool more = True;
   Bool ditch, redo;
   Vector<Int> window(2);
   os << endl;


   while (more) {

// Get users guess for position, peak, width and fit window

      T peakGuess, posGuess, widthGuess, levelGuess, level;
      Bool reject;
      getInterGaussianGuess (peakGuess, posGuess, widthGuess, window,
                             reject, os, y.nelements(), plotter);
      if (reject) {
         gaussPars = 0;   
         return False;
      }
  

// Get guess for level and adjust peak

      T dMin, dMax, dMean;
      uInt minPos, maxPos;
      stats (dMin, dMax, minPos, maxPos, dMean, y, mask);
      levelGuess = dMean;
      peakGuess = peakGuess - levelGuess;

   
// Fit a Gaussian
   
      Int n = window(1) - window(0) + 1;
      Vector<T> xFit(n);
      Vector<T> yFit(n); 
      Vector<Bool> maskFit(n);
      for (Int i=0; i<n; i++) {
         xFit(i) = x(i+window(0));
         yFit(i) = y(i+window(0));
         maskFit(i) = mask(i+window(0));
      }
      T pos, width, peak;
      if (fitGaussian (nFailed, peak, pos, width, level, xFit, yFit, maskFit,
                       peakGuess, posGuess, widthGuess, levelGuess)) {
       
// Show fit

         showGaussFit (peak, pos, width, level, x, y, mask, plotter);
      } else {
         os << LogIO::NORMAL << "Fit failed" << LogIO::POST;
      }


// Are we happy ?

   
      plotter.message("Accept (left),  redo (middle), reject (right)");
      getButton(ditch, redo, plotter);
      if (ditch) {
         plotter.message("Rejecting spectrum");
         gaussPars = 0;
         return False;
      } else if (redo) {
   
// Redraw spectrum
                       
         plotter.eras();
         drawSpectrum (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto,
                       xLabel, yLabel, title, False, plotter);
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
void MomentCalcBase<T>::getInterGaussianGuess(T& peakGuess,
                                              T& posGuess,
                                              T& widthGuess,
                                              Vector<Int>& window,
                                              Bool& reject,
                                              LogIO& os,
                                              const Int nPts,
                                              PGPlotter& plotter) const

//
// Use the cursor to get the user's guess for the
// Gaussian peak, position and width (fwhm)
// and fitting window 
//
{
   plotter.message("Mark location of peak & position - click right to reject spectrum");

   Vector<Float> minMax(4);
   minMax = plotter.qwin();
    
   
// Peak/pos first

   String str;
   Float xCurLocF = (minMax(0)+minMax(1))/2;
   Float yCurLocF = (minMax(2)+minMax(3))/2;
   Bool miss=True;
   while (miss) {
     ImageMoments<T>::readCursor(plotter, xCurLocF, yCurLocF, str);
     str.upcase();
     if (str == "X") {
        miss = False;
     } else {
        miss = ToBool(xCurLocF<minMax(0) || xCurLocF>minMax(1) ||
                      yCurLocF<minMax(2) || yCurLocF>minMax(3));
     }
     if (miss) plotter.message("Cursor off image");
   }

   reject = False;
   if (str == "X") {
     plotter.message("Rejecting spectrum");
     reject = True;
     return;
   }
   plotter.sci(3);

   Vector<Float> xDataF(1), yDataF(1);
   xDataF(0) = xCurLocF;
   yDataF(0) = yCurLocF;
   plotter.pt (xDataF, yDataF, 2);
   plotter.updt ();
   plotter.sci (1);
   posGuess = convertT(xCurLocF);
   peakGuess = convertT(yCurLocF);
   
     
// Now FWHM
     
   os << endl;
   plotter.message("Mark location of the FWHM - click right to reject spectrum");
   miss = True;   
   while (miss) {
     ImageMoments<T>::readCursor(plotter, xCurLocF, yCurLocF, str);
     str.upcase();
     if (str == "X") {
        miss = False;
     } else {
        miss = ToBool(xCurLocF<minMax(0) || xCurLocF>minMax(1) ||
                      yCurLocF<minMax(2) || yCurLocF>minMax(3));
     }
     if (miss) plotter.message("Cursor off image");
   }
   if (str == "X") {
     plotter.message("Rejecting spectrum");
     reject = True;
   }
   plotter.sci (3);

   xDataF(0) = xCurLocF;
   yDataF(0) = yCurLocF;
   plotter.pt (xDataF, yDataF, 2);
   plotter.sci (1);
   yCurLocF = convertT(peakGuess)/2;
   plotter.updt (); 
   widthGuess = 2*abs(posGuess-T(xCurLocF));
  
// Now window
    
   plotter.message(" ");
   plotter.message("Mark location of fit window; right to reject spectrum, middle fits whole spectrum");
   miss=True;
   while (miss) {
     ImageMoments<T>::readCursor(plotter, xCurLocF, yCurLocF, str);
     str.upcase();
     if (str == "X") {
        miss = False;
     } else {
        miss = ToBool(xCurLocF<minMax(0) || xCurLocF>minMax(1) || 
                      yCurLocF<minMax(2) || yCurLocF>minMax(3));
     }
     if (miss) plotter.message("Cursor off image");
   }
   if (str == "X") {
     plotter.message("Rejecting spectrum");
     reject = True;
     return;
   } else if (str == "D") {
     plotter.message("Fit to entire spectrum");
     window(0) = 0;
     window(1) = nPts-1;
     return; 
   }
   T tX = convertF(xCurLocF);
   T tY1 = minMax(2);
   T tY2 = minMax(3);
   drawVertical (tX, tY1, tY2, plotter);
   window(0) = Int(xCurLocF+0.5);
   
   miss = True;
   while (miss) {
     ImageMoments<T>::readCursor(plotter, xCurLocF, yCurLocF, str);
     str.upcase();
     if (str == "X") {
        miss = False;
     } else {
        miss = ToBool(xCurLocF<minMax(0) || xCurLocF>minMax(1) || 
                      yCurLocF<minMax(2) || yCurLocF>minMax(3));
     }
     if (miss) plotter.message("Cursor off image");
   }
   if (str == "X") {
     plotter.message("Rejecting spectrum");
     reject = True;
     return;
   } else if (str == "D") {
     plotter.message("Fit to entire spectrum");
     window(0) = 0;
     window(1) = nPts-1;
     return;
   }
   tX = convertT(xCurLocF);
   tY1 = minMax(2);
   tY2 = minMax(3);
   drawVertical (tX, tY1, tY2, plotter);
   window(1) = Int(xCurLocF+0.5);
   Int iTemp = window(0);
   window(0) = min(iTemp, window(1));
   window(1) = max(iTemp, window(1));
   window(0) = max(0,window(0));
   window(1) = min(nPts-1,window(1));
   
   plotter.sci(1);
//   cout << "Guess:peak,pos,width=" << peakGuess << "," << posGuess
//        << "," << widthGuess << LogIO::POST;   

}
   

template <class T>
Bool MomentCalcBase<T>::getLoc (T& x,
                                Bool& allSubsequent,
                                Bool& ditch,
                                Bool& redo,
                                const Bool final,
                                PGPlotter& plotter) const
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
      
   Vector<Float> minMax(4);
   minMax = plotter.qwin();
      
// Position and read cursor
      
   Float xCurLocF = convertT(x);
   Float yCurLocF = 0.0;
   String str;
   ImageMoments<T>::readCursor(plotter, xCurLocF, yCurLocF, str);
       
// Interpret location and character

   str.upcase();
   ditch = False;
   redo = False;
   allSubsequent = False;
                              
   if (str == "X") {
      plotter.message("Rejecting spectrum");
      ditch = True;
   } else if (str == "D") {
      plotter.message("Redoing window for this spectrum");
      redo = True;
   } else {
      if (xCurLocF >= minMax(0) && xCurLocF <= minMax(1)) {
         x = convertF(xCurLocF);
      } else {
         plotter.message("Cursor out of range");
         return False;
      }

      if (str == "S") {
         if (!final) {
            plotter.message("You must define both ends of the range before it can be\n");
            plotter.message("applied to all subsequent spectra. Enter S to define the\n");
            plotter.message("second extremum and indicate it will be used for all\n ");
            plotter.message("subsequent spectra\n");

            return False;
         } else {
            plotter.message("All subsequent spectra will use this window");
            allSubsequent = True;
         }
      }
   } 
   return True;
}


typedef Vector<Bool> gpp_Vector_Bool;
typedef Vector<uInt> gpp_Vector_uInt;
template <class T>
void MomentCalcBase<T>::lineSegments (uInt& nSeg,
                                      gpp_Vector_uInt& start, 
                                      gpp_Vector_uInt& nPts,
                                      const gpp_Vector_Bool& mask) const
//
// Examine an array and determine how many segments
// of good points it consists of.    A good point
// occurs if the array value is greater than zero.
//
// Inputs:
//   mask  The array mask. True is good.
// Outputs:
//   nSeg  Number of segments  
//   start Indices of start of each segment
//   nPts  Number of points in segment
//
{ 
   Bool finish = False;
   nSeg = 0;
   uInt iGood, iBad;
   const uInt n = mask.nelements();
   start.resize(n);
   nPts.resize(n);
 
   for (uInt i=0; !finish;) {
      if (!findNextDatum (iGood, n, mask, i, True)) {
         finish = True;
      } else {
         nSeg++;
         start(nSeg-1) = iGood;        
   
         if (!findNextDatum (iBad, n, mask, iGood, False)) {
            nPts(nSeg-1) = n - start(nSeg-1);
            finish = True;
         } else {
            nPts(nSeg-1) = iBad - start(nSeg-1);
            i = iBad + 1;
         }
      }
   }
   start.resize(nSeg,True);
   nPts.resize(nSeg,True);
}


template <class T>      
void MomentCalcBase<T>::makeAbcissa (Vector<T>& x,
                                     const Int& n) const
{
   x.resize(n);
   for (Int i=0; i<n; i++) x(i) = i;
}



template <class T>
Int& MomentCalcBase<T>::momentAxis(ImageMoments<T>& iMom) const
{
// Get it from ImageMoments private data

   return iMom.momentAxis_p;
}

template <class T>
String MomentCalcBase<T>::momentAxisName(ImageMoments<T>& iMom) const 
{
// Return the name of the moment/profile axis

   Int worldMomentAxis = 
      iMom.pInImage_p->coordinates().pixelAxisToWorldAxis(iMom.momentAxis_p);

   return iMom.pInImage_p->coordinates().worldAxisNames()(worldMomentAxis);
}


template <class T>
uInt MomentCalcBase<T>::nMaxMoments() const
{

// Get it from ImageMoments enum

   uInt i = ImageMoments<T>::NMOMENTS;
   return i;
}


template <class T>
T& MomentCalcBase<T>::peakSNR(ImageMoments<T>& iMom) const
{
// Get it from ImageMoments private data

   return iMom.peakSNR_p;
}


template <class T>
void MomentCalcBase<T>::selectRange(Vector<T>& pixelRange,
                                    Bool& doInclude,
                                    Bool& doExclude, 
                                    ImageMoments<T>& iMom) const
{
// Get it from ImageMoments private data

   pixelRange = iMom.selectRange_p;
   doInclude = ToBool(!(iMom.noInclude_p));
   doExclude = ToBool(!(iMom.noExclude_p));
}


template <class T>
Vector<Int> MomentCalcBase<T>::selectMoments(ImageMoments<T>& iMom) const
//
// Fill the moment selection vector according to what the user requests
//
{
   typedef ImageMoments<Float> IM;
   Vector<Int> sel(IM::NMOMENTS);

   uInt j = 0;
   for (uInt i=0; i<iMom.moments_p.nelements(); i++) {
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
void MomentCalcBase<T>::setPosLabel (String& title,
                                     const IPosition& pos) const
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
                                     LogIO& os) const
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
      sepWorldCoord.resize(iMom.pInImage_p->shape()(iMom.momentAxis_p));
      for (uInt i=0; i<sepWorldCoord.nelements(); i++) {
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
void MomentCalcBase<T>::showGaussFit(const T peak,
                                     const T pos,
                                     const T width,
                                     const T level,
                                     const Vector<T>& x,
                                     const Vector<T>& y,
                                     const Vector<Bool>& mask,
                                     PGPlotter& plotter) const
// 
// Plot the Gaussian fit and residual
//
{
   const uInt nDPts = x.nelements();
   T xMin = x(0);
   T xMax = x(nDPts-1);
   uInt nGPts = 100;
   T dx = (xMax - xMin)/nGPts;

// Setup functional

   const Gaussian1D<T> gauss(peak, pos, width);
                                      
   
// Allocate arrays

   Vector<T> xG(nGPts);
   Vector<T> yG(nGPts);
   
   
// Generate plot values
 
   uInt i;
   T xx;
   for (i=0,xx=xMin; i<nGPts; xx+=dx,i++) {
      xG(i) = xx;
      yG(i) = gauss(xx) + level;
   }
   plotter.sci (7);
   drawLine (xG, yG, plotter);


// Now difference
   
   Vector<T> xd(nDPts);
   Vector<T> d(nDPts);
   uInt j = 0;
   for (i=0; i<nDPts; i++) {
      if (mask(i)) {
         xd(j) = x(i);
         d(j) = y(i) - gauss(x(i));
         j++;
      }
   }
   if (j > 0) {
     xd.resize(j,True);
     d.resize(j,True);
     plotter.sci (2);
     drawLine (xd, d, plotter);
  }
  plotter.sci (1);
}


template <class T>      
Bool MomentCalcBase<T>::stats(T& dMin, 
                              T& dMax,  
                              uInt& minPos,
                              uInt& maxPos,
                              T& dMean,
                              const Vector<T>& profile,
                              const Vector<Bool>& mask) const
//
// Returns False if no unmasked points
//
{
   Bool deleteIt1, deleteIt2;
   const T* pProfile = profile.getStorage(deleteIt1);
   const Bool* pMask = mask.getStorage(deleteIt2);

   Int iStart = -1;
   uInt i = 0;
   uInt nPts = 0;
   NumericTraits<T>::PrecisionType sum = 0;

   while (i<profile.nelements() && iStart==-1) {
      if (pMask[i]) {
        dMax = pProfile[i];
        dMin = dMax;
        minPos = i;
        maxPos = i;
        sum = pProfile[i];
        nPts++;
        iStart = i+1;
      }
      i++;
   }
   if (iStart == -1) return False;

   for (i=iStart; i<profile.nelements(); i++) {
      if (pMask[i]) {
         dMin = min(dMin,pProfile[i]);
         dMax = max(dMax,pProfile[i]);
         minPos = i;
         maxPos = i;
         sum += pProfile[i];
         nPts++;
      }
   }
   dMean = sum / nPts;
   profile.freeStorage(pProfile, deleteIt1);
   mask.freeStorage(pMask, deleteIt2);

   return True;  
}


template <class T>
T& MomentCalcBase<T>::stdDeviation(ImageMoments<T>& iMom) const
{
   return iMom.stdDeviation_p;
}
      

template <class T>
void MomentCalcBase<T>::yAutoMinMax(T& yMin, 
                                    T& yMax, 
                                    ImageMoments<T>& iMom) const
{
   yMin = iMom.yMin_p;
   yMax = iMom.yMax_p;
}
 

   


// Derived class MomentClip

template <class T>
MomentClip<T>::MomentClip(Lattice<T>* pAncilliaryLattice,
                          ImageMoments<T>& iMom,
                          LogIO& os,
                          const uInt nLatticeOut)
: pAncilliaryLattice_p(pAncilliaryLattice),
  iMom_p(iMom),
  os_p(os)
{

// Set moment selection vector

   selectMoments_p = selectMoments(iMom_p);

// Set/check some dimensionality

   constructorCheck(calcMoments_p, selectMoments_p, nLatticeOut);

// Fish out moment axis

   Int momAxis = momentAxis(iMom_p);

// Set up slice shape for extraction from masking lattice

   if (pAncilliaryLattice_p != 0) {
      sliceShape_p.resize(pAncilliaryLattice_p->ndim());
      sliceShape_p = 1;
      sliceShape_p(momAxis) = pAncilliaryLattice_p->shape()(momAxis);
   }

// Make all plots with same y range ?

   fixedYLimits_p = fixedYLimits(iMom_p);
   yAutoMinMax(yMinAuto_p, yMaxAuto_p, iMom_p);

// Fish out pixel selection range

   selectRange(range_p, doInclude_p, doExclude_p, iMom_p);

// Are we computing the expensive moments ?

   costlyMoments(iMom_p, doMedianI_p, doMedianV_p, doAbsDev_p);

// Are we plotting ?

   plotter_p = device(iMom_p);

// What is the axis type of the moment axis

   momAxisType_p = momentAxisName(iMom_p);

// Are we computing coordinate-dependent moments.  If
// so precompute coordinate vector is momebt axis separable

   doCoordCalc_p = doCoordCalc(iMom_p);
   if (doCoordCalc_p) setUpCoords(iMom_p, pixelIn_p, worldOut_p,
                                  sepWorldCoord_p, os_p);

// Number of failed Gaussian fits 
   nFailed_p = 0;
}


template <class T>
MomentClip<T>::~MomentClip()
{;}

template <class T>
void MomentClip<T>::process(T&,
                            Bool&,
                            const Vector<T>&,
                            const Vector<Bool>&,
                            const IPosition&)
{
   throw(AipsError("MomentClip<T>::process(Vector<T>&, IPosition&): not implemented"));
}


template <class T> 
void MomentClip<T>::multiProcess(Vector<T>& moments,
                                 Vector<Bool>& momentsMask,
                                 const Vector<T>& profileIn,
                                 const Vector<Bool>& profileInMask,
                                 const IPosition& inPos)
//
// The profile comes with its own mask (or a null mask
// which means all good).  In addition, we create
// a further mask by applying the clip range to either
// the primary lattice, or the ancilliary lattice (e.g. 
// the smoothed lattice)
//
{

// Fish out the ancilliary image slice if needed.  Stupid slice functions 
// require me to create the slice empty every time so degenerate
// axes can be chucked out.  We set up a pointer to the primary or 
// ancilliary vector object  that we can use for fast access 


   const T* pProfileSelect = 0;      
   Bool deleteIt;
   if (pAncilliaryLattice_p && (doInclude_p || doExclude_p)) {
      Array<T> ancilliarySlice;
      IPosition stride(pAncilliaryLattice_p->ndim(),1);

      pAncilliaryLattice_p->getSlice(ancilliarySlice, inPos,
                               sliceShape_p, stride, True);
      ancilliarySliceRef_p.reference(ancilliarySlice);
 
      pProfileSelect_p = &ancilliarySliceRef_p;
      pProfileSelect = ancilliarySliceRef_p.getStorage(deleteIt);
   } else {
      pProfileSelect_p = &profileIn;
      pProfileSelect = profileIn.getStorage(deleteIt);
   }


// Plot spectrum if asked

   if (plotter_p.isAttached()) {
      makeAbcissa(abcissa_p, pProfileSelect_p->nelements());
      String xLabel;
      if (momAxisType_p.empty()) {
         xLabel = "x (pixels)";
      } else {
         xLabel = momAxisType_p + " (pixels)";
      }
      const String yLabel("Intensity");
      String title;
      setPosLabel (title, inPos);

      if (!drawSpectrum (abcissa_p, *pProfileSelect_p, profileInMask, 
                         fixedYLimits_p, yMinAuto_p, yMaxAuto_p, xLabel, 
                         yLabel, title, True, plotter_p)) {

// If all points were masked, it's over for this profile.

         moments = 0.0;
         momentsMask = False;

         if (pAncilliaryLattice_p && (doInclude_p || doExclude_p)) {
            ancilliarySliceRef_p.freeStorage(pProfileSelect, deleteIt);
         } else {
            profileIn.freeStorage(pProfileSelect, deleteIt);
         }
         return;
      }


// Draw on clip levels and arrows

      if (doInclude_p || doExclude_p) {
         plotter_p.sci (5);
         drawHorizontal(range_p(0), plotter_p);
         drawHorizontal(range_p(1), plotter_p);
      

         Float y1F = convertT(range_p(0));
         Float y2F = convertT(range_p(1));

         Vector<Float> minMax(4);
         minMax = plotter_p.qwin();
         Float xF = minMax(0) + 0.05*(minMax(1)-minMax(0));
         Float yF = y2F - 0.2*y1F;
         plotter_p.arro (xF, y2F, xF, yF);
         yF = y1F + 0.2*y2F;
         plotter_p.arro (xF, yF, xF, y1F);
         plotter_p.sci(1);
      } 
   }


// Resize array for median.  Is resized correctly later
 
   Int nPts = profileIn.nelements();
   selectedData_p.resize(nPts);
   selectedDataIndex_p.resize(nPts);


// Were the profile coordinates precomputed ?
      
   Bool preComp = ToBool(sepWorldCoord_p.nelements() > 0);


// Compute moments.  The actual moment computation always done with 
// the original data, regardless of whether the pixel selection is 
// done with the primary or ancilliary data.

   NumericTraits<T>::PrecisionType s0  = 0.0;
   NumericTraits<T>::PrecisionType s0Sq = 0.0;
   NumericTraits<T>::PrecisionType s1  = 0.0;
   NumericTraits<T>::PrecisionType s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   T dMin =  1.0e30;
   T dMax = -1.0e30;
   Double coord = 0.0;
   Int i, j;


   if (profileInMask.nelements() == 0) {

// No mask included.

      if (doInclude_p) {
         for (i=0,j=0; i<nPts; i++) {
            if (pProfileSelect[i] >= range_p(0) && 
                pProfileSelect[i] <= range_p(1)) {

               if (preComp) {
                  coord = sepWorldCoord_p(i);              
               } else if (doCoordCalc_p) {
                  coord = getMomentCoord(iMom_p, pixelIn_p,
                                         worldOut_p, Double(i));       
               }
               accumSums(s0, s0Sq, s1, s2, iMin, iMax,     
                         dMin, dMax, i, profileIn(i), coord);
               selectedData_p(j) = profileIn(i);
               selectedDataIndex_p(j) = i;
               j++;
            }
         }
      } else if (doExclude_p) {
         for (i=0,j=0; i<nPts; i++) {
            if (pProfileSelect[i] <= range_p(0) || 
                pProfileSelect[i] >= range_p(1)) {

               if (preComp) {
                  coord = sepWorldCoord_p(i); 
               } else if (doCoordCalc_p) {
                  coord = getMomentCoord(iMom_p, pixelIn_p,
                                         worldOut_p, Double(i));
               }
               accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                         dMin, dMax, i, profileIn(i), coord);
               selectedData_p(j) = profileIn(i);
               selectedDataIndex_p(j) = i;
               j++;
            }
         }
      } else {    
         for (i=0; i<nPts; i++) {
            if (preComp) {
               coord = sepWorldCoord_p(i);
            } else if (doCoordCalc_p) {
               coord = getMomentCoord(iMom_p, pixelIn_p,
                                      worldOut_p, Double(i));
            }
            accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                      dMin, dMax, i, profileIn(i), coord);
            selectedData_p(i) = profileIn(i);
            selectedDataIndex_p(i) = i;
         }
      }

   } else {

// Set up a pointer for faster access to the profile mask

      Bool deleteIt2;
      const Bool* pProfileInMask = profileInMask.getStorage(deleteIt2);

      if (doInclude_p) {
         for (i=0,j=0; i<nPts; i++) {
            if (pProfileInMask[i] &&
                pProfileSelect[i] >= range_p(0) && 
                pProfileSelect[i] <= range_p(1)) {

               if (preComp) {
                  coord = sepWorldCoord_p(i);              
               } else if (doCoordCalc_p) {
                  coord = getMomentCoord(iMom_p, pixelIn_p,
                                         worldOut_p, Double(i));       
               }
               accumSums(s0, s0Sq, s1, s2, iMin, iMax,     
                         dMin, dMax, i, profileIn(i), coord);
               selectedData_p(j) = profileIn(i);
               selectedDataIndex_p(j) = i;
               j++;
            }
         }
      } else if (doExclude_p) {
         for (i=0,j=0; i<nPts; i++) {
            if (pProfileInMask[i] &&
               (pProfileSelect[i] <= range_p(0) || 
                pProfileSelect[i] >= range_p(1))) {

               if (preComp) {
                  coord = sepWorldCoord_p(i); 
               } else if (doCoordCalc_p) {
                  coord = getMomentCoord(iMom_p, pixelIn_p,
                                         worldOut_p, Double(i));
               }
               accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                         dMin, dMax, i, profileIn(i), coord);
               selectedData_p(j) = profileIn(i);
               selectedDataIndex_p(j) = i;
               j++;
            }
         }
      } else {    
         for (i=0,j=0; i<nPts; i++) {
            if (pProfileInMask[i]) {
               if (preComp) {
                  coord = sepWorldCoord_p(i);
               } else if (doCoordCalc_p) {
                  coord = getMomentCoord(iMom_p, pixelIn_p,
                                         worldOut_p, Double(i));
               }
               accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                         dMin, dMax, i, profileIn(i), coord);
               selectedData_p(j) = profileIn(i);
               selectedDataIndex_p(j) = i;
               j++;
            }
         }
      }
      nPts = j;
      profileInMask.freeStorage(pProfileInMask, deleteIt2);
   }


// Delete pointer memory

   if (pAncilliaryLattice_p  && (doInclude_p || doExclude_p)) {
      ancilliarySliceRef_p.freeStorage(pProfileSelect, deleteIt);
   } else {
      profileIn.freeStorage(pProfileSelect, deleteIt);
   }

   
// If no points make moments zero and mask
               
   if (nPts==0) {
      moments = 0.0;
      momentsMask = False;
      return;
   }        
   
         
// Absolute deviations of I from mean needs an extra pass.
         
   NumericTraits<T>::PrecisionType sumAbsDev = 0;
   if (doAbsDev_p) {
      T iMean = s0 / nPts;
      for (i=0; i<nPts; i++) sumAbsDev += abs(selectedData_p(i) - iMean);
   }
 
// Median of I
         
   T dMedian = 0.0;
   if (doMedianI_p) {
      selectedData_p.resize(nPts,True);
      dMedian = median(selectedData_p.ac());
   }
       
 
// Median coordinate.  ImageMoments will only be allowing this if
// we are not offering the ancilliary lattice, and with an include 
// or exclude range.   Pretty dodgy   
         
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
         Int iVal = 0;
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

// Fill vector of selected moments 

   for (i=0; i<Int(selectMoments_p.nelements()); i++) {
      moments(i) = calcMoments_p(selectMoments_p(i));
      momentsMask(i) = True;
   }

}





// Derived class MomentWindow

template <class T>
MomentWindow<T>::MomentWindow(Lattice<T>* pAncilliaryLattice,
                              ImageMoments<T>& iMom,
                              LogIO& os,
                              const uInt nLatticeOut)
: pAncilliaryLattice_p(pAncilliaryLattice),
  iMom_p(iMom),
  os_p(os)
{
// Set moment selection vector

   selectMoments_p = selectMoments(iMom_p);

// Set/check some dimensionality

   constructorCheck(calcMoments_p, selectMoments_p, nLatticeOut);

// Fish out moment axis

   Int momAxis = momentAxis(iMom_p);

// Set up slice shape for extraction from masking lattice

   if (pAncilliaryLattice_p != 0) {
      sliceShape_p.resize(pAncilliaryLattice_p->ndim());
      sliceShape_p = 1;
      sliceShape_p(momAxis) = pAncilliaryLattice_p->shape()(momAxis);
   }

// Make all plots with same y range ?

   fixedYLimits_p = fixedYLimits(iMom_p);
   yAutoMinMax(yMinAuto_p, yMaxAuto_p, iMom_p);

// Are we computing the expensive moments ?

   costlyMoments(iMom_p, doMedianI_p, doMedianV_p, doAbsDev_p);

// Are we plotting ?

   plotter_p = device(iMom_p);

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

// Number of failed Gaussian fits 

   nFailed_p = 0;
}


template <class T>
MomentWindow<T>::~MomentWindow()
{;}

template <class T>
void MomentWindow<T>::process(T&,
                              Bool&,
                              const Vector<T>&,
                              const Vector<Bool>&,
                              const IPosition&)
{
   throw(AipsError("MomentWindow<T>::process not implemented"));
}


template <class T> 
void MomentWindow<T>::multiProcess(Vector<T>& moments,
                                   Vector<Bool>& momentsMask,
                                   const Vector<T>& profileIn,
                                   const Vector<Bool>& profileInMask,
                                   const IPosition& inPos)
//
// Generate windowed moments of this profile.
// The profile comes with its own mask (or a null mask
// which means all good).  In addition, we create
// a further mask by applying the clip range to either
// the primary lattice, or the ancilliary lattice (e.g. 
// the smoothed lattice)
//
{

// Fish out the ancilliary image slice if needed.  Stupid slice functions 
// require me to create the slice empty every time so degenerate
// axes can be chucked out.  We set up a pointer to the primary or 
// ancilliary vector object  that we can use for fast access 

   const T* pProfileSelect = 0;      
   Bool deleteIt;
   if (pAncilliaryLattice_p != 0) {
      Array<T> ancilliarySlice;
      IPosition stride(pAncilliaryLattice_p->ndim(),1);
      pAncilliaryLattice_p->getSlice(ancilliarySlice, inPos,
                               sliceShape_p, stride, True);
      ancilliarySliceRef_p.reference(ancilliarySlice);

      pProfileSelect_p = &ancilliarySliceRef_p;
      pProfileSelect = ancilliarySliceRef_p.getStorage(deleteIt);
   } else {
      pProfileSelect_p = &profileIn;
      pProfileSelect = profileIn.getStorage(deleteIt);
   }


// Make abcissa and labels
   
   static Bool allSubsequent = False;
   static Vector<Int> window(2);  
   static Int nPts = 0;
      
   makeAbcissa (abcissa_p, pProfileSelect_p->nelements());
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
      if (getAutoWindow(nFailed_p, window,  abcissa_p, *pProfileSelect_p, profileInMask,
                        peakSNR_p, stdDeviation_p, doFit_p, plotter_p, 
                        fixedYLimits_p, yMinAuto_p, yMaxAuto_p, xLabel,
                        yLabel, title)) {
         nPts = window(1) - window(0) + 1;
      } else {
         nPts = 0;
      }
   } else {

// Define the window interactively, unless the user has told us when
// doing the previous spectrum that they wish to apply that window
// to all subsequent ones
    

      if (!doFit_p && !allSubsequent) {
         plotter_p.message(" ");
         plotter_p.message("Mark extremum (left), redo (middle), reject (right), all subsequent (S)");
      }

      if (!allSubsequent) {
         if (getInterWindow (nFailed_p, allSubsequent, os_p, window, doFit_p, abcissa_p, 
                             *pProfileSelect_p, profileInMask, fixedYLimits_p, 
                             yMinAuto_p, yMaxAuto_p, xLabel, yLabel, title, 
                             plotter_p)) {
            nPts = window(1) - window(0) + 1;
         } else {
            nPts = 0;
         }
      } else if (nPts != 0) {
         if (drawSpectrum (abcissa_p, *pProfileSelect_p, profileInMask,
                           fixedYLimits_p, yMinAuto_p, yMaxAuto_p, xLabel, 
                           yLabel, title, True, plotter_p)) {
            drawWindow (window, plotter_p);
            nPts = window(1) - window(0) + 1;
         } else {
            nPts = 0;
         }
      }
   }


// If no points make moments zero and mask
               
   if (nPts==0) {
      moments = 0.0;
      momentsMask = False;

      if (pAncilliaryLattice_p) {
         ancilliarySliceRef_p.freeStorage(pProfileSelect, deleteIt);
      } else {
         profileIn.freeStorage(pProfileSelect, deleteIt);
      }
      return;
   }        


// Resize array for median.  Is resized correctly later
 
   selectedData_p.resize(nPts);
      

// Were the profile coordinates precomputed ?
      
   Bool preComp = ToBool(sepWorldCoord_p.nelements() > 0);


// Set up a pointer for fast access to the profile mask
// if it exists.

   Bool deleteIt2;
   const Bool* pProfileInMask = profileInMask.getStorage(deleteIt2);


// Accumulate sums and acquire selected data from primary lattice 
            
   NumericTraits<T>::PrecisionType s0  = 0.0;
   NumericTraits<T>::PrecisionType s0Sq = 0.0;
   NumericTraits<T>::PrecisionType s1  = 0.0;
   NumericTraits<T>::PrecisionType s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   T dMin =  1.0e30;
   T dMax = -1.0e30;
   Double coord = 0.0;

   Int i,j;
   for (i=window(0),j=0; i<=window(1); i++) {
      if (pProfileInMask[i]) {
         if (preComp) {
            coord = sepWorldCoord_p(i);
         } else if (doCoordCalc_p) {
            coord = getMomentCoord(iMom_p, pixelIn_p,
                                   worldOut_p, Double(i));
         }
         accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                   dMin, dMax, i, profileIn(i), coord);
         selectedData_p(j) = profileIn(i);
         j++;
      }
   }
   nPts = j;

         
// Absolute deviations of I from mean needs an extra pass.
   
   NumericTraits<T>::PrecisionType sumAbsDev = 0.0;
   if (doAbsDev_p) {
      T iMean = s0 / nPts;
      for (Int i=0; i<nPts; i++) sumAbsDev += abs(selectedData_p(i) - iMean);
   }



// Delete memory associated with pointers

   if (pAncilliaryLattice_p) {
      ancilliarySliceRef_p.freeStorage(pProfileSelect, deleteIt);
   } else {
      profileIn.freeStorage(pProfileSelect, deleteIt);
   }
   profileInMask.freeStorage(pProfileInMask, deleteIt2);

 
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


// Fill selected moments 

   for (i=0; i<Int(selectMoments_p.nelements()); i++) {
      moments(i) = calcMoments_p(selectMoments_p(i));
      momentsMask(i) = True;
   }
}



template <class T>
void MomentWindow<T>::drawWindow(const Vector<Int>& window,
                                 PGPlotter& plotter) const
//
// Mark the current window on the plot
//
{  
   Vector<Float> minMax(4);
   minMax = plotter.qwin();
   T yMin = convertF(minMax(2));
   T yMax = convertF(minMax(3));

   T x = window(0);
   drawVertical (x, yMin, yMax, plotter);
   x = window(1);
   drawVertical (x, yMin, yMax, plotter);
}


template <class T>
Bool MomentWindow<T>::getAutoWindow (uInt& nFailed,
                                     Vector<Int>& window,
                                     const Vector<T>& x,
                                     const Vector<T>& y,
                                     const Vector<Bool>& mask,
                                     const T peakSNR,
                                     const T stdDeviation,
                                     const Bool doFit,
                                     PGPlotter& plotter,
                                     const Bool fixedYLimits,                 
                                     const T yMinAuto,                 
                                     const T yMaxAuto,                 
                                     const String xLabel,
                                     const String yLabel,
                                     const String title) const
//
// Automatically fit a Gaussian and return the +/- 3-sigma window or
// invoke Bosma's method to set a window.  If a plotting device is
// active, we also plot the spectra and fits
//
// Inputs:
//   x,y        Spectrum
//   mask       Mask associated with spectrum. True is good.
//   plotter    Plot spectrum and optionally the  window
//   x,yLabel   x label for plots
//   title 
// Input/output
//   nFailed    Cumulative number of failed fits
// Output:
//   window     The window (pixels).  If both 0,  then discard this spectrum
//              and mask moments    
//
{
   if (doFit) {
      Vector<T> gaussPars(4);
      if (!getAutoGaussianFit (nFailed, gaussPars, x, y, mask, peakSNR, stdDeviation, 
                               plotter, fixedYLimits, yMinAuto, yMaxAuto, 
                               xLabel, yLabel, title)) {
         window = 0;
         return False;
      } else {
   
// Set 3-sigma limits.  This assumes that there are some unmasked
// points in the window !
 
         if (!setNSigmaWindow (window, gaussPars(1), gaussPars(2),
                               y.nelements(), 3)) {
            window = 0;
            return False;
         }
      }
   } else {
// Invoke Albert's method (see AJ, 86, 1791)

      if (!getBosmaWindow (window, x, y, mask, peakSNR, stdDeviation, 
                           plotter, fixedYLimits, yMinAuto, yMaxAuto, 
                           xLabel, yLabel, title)) {
         window = 0;
         return False;
      }
   }
   
// Plot window if desired
 
   if (plotter.isAttached()) drawWindow (window, plotter);

   return True;
}

template <class T>
Bool MomentWindow<T>::getInterDirectWindow (Bool& allSubsequent,
                                            LogIO& os,
                                            Vector<Int>& window,
                                            const Vector<T>& x,
                                            const Vector<T>& y,
                                            const Vector<Bool>& mask,
                                            const Bool fixedYLimits,   
                                            const T yMinAuto,   
                                            const T yMaxAuto,
                                            const String xLabel,
                                            const String yLabel,
                                            const String title,
                                            PGPlotter& plotter) const
//
// With the cursor, mark the range for the window method
//
// Outputs:
//  window    The window (pixels)
//
// Returns false if couldn't set window becasue spectrum
// was all masked
{
 
// First plot the spectrum
   
   if (!drawSpectrum (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto,
                      xLabel, yLabel, title, True, plotter)) return False;


// Try and get a decent range from user   
      
   Vector<Float> minMax(4);
   minMax = plotter.qwin();
   Bool more = True;
   Bool ditch, redo;
   const uInt nPts = y.nelements();   
   T tX, tY1, tY2;

   while (more) {
  
// Get and draw first location   

      Bool final = False;
      T x1 = convertF(Float(nPts))/2;
      allSubsequent = True;
      while (!getLoc(x1, allSubsequent, ditch, redo, final, plotter)) {};
      if (ditch) {
         window = 0;
         return False;
      }

      if (!redo) {
         window(0) = max(0,Int(x1+0.5));
         tX = window(0);
         tY1 = minMax(2);
         tY2 = minMax(3);
         drawVertical (tX, tY1, tY2, plotter);
  

// Get and draw second location
  
         T x2 = window(0);
         final = True;
         allSubsequent = True;
         while (!getLoc(x2, allSubsequent, ditch, redo, final, plotter)) {};
         if (ditch) {
            window = 0;
            return False;
         } else if (redo) {
            plotter.eras();
            drawSpectrum  (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto,
                          xLabel, yLabel, title, False, plotter);
         } else {
            window(1) = min(nPts-1,uInt(x2+0.5));
            tX = window(1);
            drawVertical (tX, tY1, tY2, plotter);
         
// Set window
         
            Int iTemp = window(0);
            window(0) = min(iTemp, window(1));
            window(1) = max(iTemp, window(1));
         
// If they stuffed it up, have another go.  Erase the line and redraw
// the spectrum segment
         
            if (window(0) == window(1)) {
               os << LogIO::NORMAL << "Degenerate window, try again" << LogIO::POST;
               plotter.eras();
               drawSpectrum (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto,
                            xLabel, yLabel, title, False, plotter);

            } else
               more = False;
         } 
      }
   }
   return True;
}


template <class T>
Bool MomentWindow<T>::getInterWindow(uInt& nFailed, 
                                     Bool& allSubsequent, 
                                     LogIO& os,
                                     Vector<Int>& window,
                                     const Bool doFit,
                                     const Vector<T>& x,
                                     const Vector<T>& y,
                                     const Vector<Bool>& mask,
                                     const Bool fixedYLimits,
                                     const T yMinAuto,
                                     const T yMaxAuto,
                                     const String xLabel,
                                     const String yLabel,
                                     const String title,
                                     PGPlotter& plotter) const
//
// Interactively select the moment window by fitting a Gaussian
// or directly setting the window with the cursor.
//
// Inputs:
//   x,y        Spectrum
//   mask       Mask associated with spectrum. True is good.
//   x,yLabel   Labels for plots
//   title
// Output:
//   window     Include pixels in this range of indices.  If both 0,
//              then discard this spectrum and mask moments
//   allSubsequent 
//              If True, then the user has instructed that
//              all subsequent spectra are to use this window
//              and we are to stop the interactive plotting
{                                     
   if (doFit) {   
         
         
// We interactively fit a Gaussian and choose +/- 3 sigma limits as the range
      
      Vector<T> gaussPars(4);
      if (!getInterGaussianFit (nFailed, gaussPars, os, x, y, mask, fixedYLimits, 
                                yMinAuto, yMaxAuto, xLabel, yLabel, 
                                title, plotter)) {
         window = 0;
         return False;
      } else {
    
// Set 3-sigma range
   
         if (!setNSigmaWindow (window, gaussPars(1), gaussPars(2), 
                               y.nelements(), 3)) {
            os << LogIO::NORMAL << "Window too small for this spectrum" << LogIO::POST;
            window = 0;
            return False;
         }

// Mark window on plot
         
         plotter.eras ();
         drawSpectrum (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto, 
                       xLabel, yLabel, title, False, plotter);
         drawWindow (window, plotter);
      }
      allSubsequent = False;
   } else {

// The user just marks the range with the cursor

      if (!getInterDirectWindow (allSubsequent, os, window, x, y, mask, 
                                 fixedYLimits, yMinAuto, yMaxAuto, xLabel,
                                 yLabel, title, plotter)) return False;
   }
  
   return True;
}


template <class T>
Bool MomentWindow<T>::getBosmaWindow (Vector<Int>& window,
                                      const Vector<T>& x,
                                      const Vector<T>& y,
                                      const Vector<Bool>& mask,
                                      const T peakSNR,
                                      const T stdDeviation,
                                      PGPlotter& plotter,
                                      const Bool fixedYLimits,
                                      const T yMinAuto,
                                      const T yMaxAuto,
                                      const String xLabel,
                                      const String yLabel,
                                      const String title) const
//
// Automatically work out the spectral window
// with Albert Bosma's algorithm.
//    
// Inputs: 
//   x,y       Spectrum
//   plotter   Plot device active if True
//   x,yLabel  Labels for plots
// Output:
//   window    The window
//   Bool      False if we reject this spectrum.  This may
//             be because it is all noise, or all masked
//
{
      
   if (plotter.isAttached()) {
   
// Plot spectrum 
      
      if (!drawSpectrum (x, y, mask, fixedYLimits, yMinAuto, yMaxAuto,
                        xLabel, yLabel, title, True, plotter)) return False;
   }


// See if this spectrum is all noise first.  If so, forget it.
// Return straight away if all maske

   T dMean;
   uInt iNoise = allNoise(dMean, y, mask, peakSNR, stdDeviation);
   if (iNoise == 2) return False;


// Draw on mean and sigma
 
   if (plotter.isAttached()) {
      drawMeanSigma (dMean, stdDeviation, plotter);
      if (iNoise==1) plotter.mtxt ("T", 1.0, 0.0, 0.0, "NOISE");
   }
   if (iNoise==1) {
      window = 0;
      return False;   
   }


// Find peak

   uInt minPos, maxPos;
   T yMin, yMax, yMean;
   stats(yMin, yMax, minPos, maxPos, yMean, y, mask);

   const Int nPts = y.nelements(); 
   Int iMin = max(0,Int(maxPos)-2);   
   Int iMax = min(nPts-1,Int(maxPos)+2);
   T tol = stdDeviation / (nPts - (iMax-iMin-1));
          
       
// Iterate to convergence
   
   Bool first = True;
   Bool converged = False;
   Bool more = True;
   yMean = 0;
   T oldYMean = 0;
   while (more) {

//     cout << LogIO::NORMAL << "iMin,iMax,oldmean,tol=" << iMin << "," << iMax << $
   
// Find mean outside of peak region

      NumericTraits<T>::PrecisionType sum = 0;
      for (Int i=0,j=0; i<nPts; i++) {
         if (mask(i) && (i < iMin || i > iMax)) {
            sum += y(i);
            j++;
         }
      }
      if (j>0) yMean = sum / j;
   

// Interpret result

      if (!first && j>0 && abs(yMean-oldYMean) < tol) {
         converged = True;
         more = False;
      } else if (iMin==0 && iMax==nPts-1)
         more = False;
      else {
   
// Widen window and redetermine tolerance

         oldYMean = yMean;
         iMin = max(0,iMin - 2);
         iMax = min(nPts-1,iMax+2); 
         tol = stdDeviation / (nPts - (iMax-iMin-1));
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
Bool MomentWindow<T>::setNSigmaWindow (Vector<Int>& window,
                                       const T pos,
                                       const T width,
                                       const Int nPts,
                                       const Int N) const
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




// Derived class MomentFit

template <class T>
MomentFit<T>::MomentFit(ImageMoments<T>& iMom,
                        LogIO& os,
                        const uInt nLatticeOut)
: iMom_p(iMom),
  os_p(os)
{
// Set moment selection vector

   selectMoments_p = selectMoments(iMom_p);

// Set/check some dimensionality

   constructorCheck(calcMoments_p, selectMoments_p, nLatticeOut);

// Make all plots with same y range ?

   fixedYLimits_p = fixedYLimits(iMom_p);
   yAutoMinMax(yMinAuto_p, yMaxAuto_p, iMom_p);

// Are we computing the expensive moments ?

   costlyMoments(iMom_p, doMedianI_p, doMedianV_p, doAbsDev_p);

// Are we plotting ?
   plotter_p = device(iMom_p);

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

// Number of failed Gaussian fits 

   nFailed_p = 0;

}


template <class T>
MomentFit<T>::~MomentFit()
{;}


template <class T> 
void MomentFit<T>::process(T&,
                            Bool&,
                            const Vector<T>&,
                            const Vector<Bool>&,
                            const IPosition&)
{
   throw(AipsError("MomentFit<T>::process not implemented"));
}



template <class T> 
void MomentFit<T>::multiProcess(Vector<T>& moments,
                                Vector<Bool>& momentsMask,
                                const Vector<T>& profileIn,
                                const Vector<Bool>& profileInMask,
                                const IPosition& inPos)
//
// Generate moments from a Gaussian fit of this profile
//
{

// Create the abcissa array and some labels
       
   Int nPts = profileIn.nelements();
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

      if (!getAutoGaussianFit (nFailed_p, gaussPars, abcissa_p, profileIn, profileInMask, 
                               peakSNR_p, stdDeviation_p, plotter_p, fixedYLimits_p,
                               yMinAuto_p, yMaxAuto_p, xLabel, yLabel, title)) {
         moments = 0;   
         momentsMask = False;
         return;
      }

   } else {

// Interactive
   
       if (!getInterGaussianFit(nFailed_p, gaussPars, os_p, abcissa_p, profileIn, profileInMask,
                                fixedYLimits_p, yMinAuto_p, yMaxAuto_p,
                                xLabel, yLabel, title, plotter_p)) {
         moments = 0;   
         momentsMask = False;
         return;
      }
   }
   

// Were the profile coordinates precomputed ?
      
   Bool preComp = ToBool(sepWorldCoord_p.nelements() > 0);


// Set Gaussian functional values.  We reuse the same functional that
// was used in the interactive fitting display process.

   gauss_p.setHeight(gaussPars(0));
   gauss_p.setCenter(gaussPars(1));
   gauss_p.setWidth(gaussPars(2));


// Compute moments from the fitted Gaussian
            
   NumericTraits<T>::PrecisionType s0  = 0.0;
   NumericTraits<T>::PrecisionType s0Sq = 0.0;
   NumericTraits<T>::PrecisionType s1  = 0.0;
   NumericTraits<T>::PrecisionType s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   T dMin =  1.0e30;
   T dMax = -1.0e30;
   Double coord = 0.0;
   T xx;
   Vector<T> gData(nPts);
   
   for (Int i=0,j=0; i<nPts; i++) {
      if (profileInMask(i)) {
         xx = i;
         gData(j) = gauss_p(xx) + gaussPars(3);
      
         if (preComp) {
            coord = sepWorldCoord_p(i);
         } else if (doCoordCalc_p) {
            coord = getMomentCoord(iMom_p, pixelIn_p, 
                                   worldOut_p, Double(i));
         }
         accumSums(s0, s0Sq, s1, s2, iMin, iMax,
                   dMin, dMax, i, gData(j), coord);
         j++;
      }
   }

// If no unmasked points go home.  This shouldn't happen
// as we can't have done a fit under these conditions.

   nPts = j;
   if (nPts == 0) {
      moments = 0;   
      momentsMask = False;
      return;
   }

   


// Absolute deviations of I from mean needs an extra pass.
         
   NumericTraits<T>::PrecisionType sumAbsDev = 0.0;
   if (doAbsDev_p) {
      T iMean = s0 / nPts;
      for (Int i=0; i<nPts; i++) sumAbsDev += abs(gData(i) - iMean);
   }


// Median of I
         
   T dMedian = 0.0;
   if (doMedianI_p) {
      gData.resize(nPts, True);
      dMedian = median(gData.ac());
   }
   T vMedian = 0.0;
       
// Fill all moments array
   
   setCalcMoments(iMom_p, calcMoments_p, pixelIn_p, worldOut_p, doCoordCalc_p,
                  dMedian, vMedian, nPts, s0, s1, s2, s0Sq,
                  sumAbsDev, dMin, dMax, iMin, iMax);


// Fill vector of selected moments 

   for (i=0; i<Int(selectMoments_p.nelements()); i++) {
      moments(i) = calcMoments_p(selectMoments_p(i));
      momentsMask(i) = True;
   }
}





