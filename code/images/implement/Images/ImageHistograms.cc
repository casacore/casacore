//# ImageHistograms.cc: generate histograms from an image
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

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>
  
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/ImageHistograms.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>

#include <iomanip.h>
#include <stdlib.h>
#include <strstream.h>


// C wrappers for PGPLOT

extern "C" {
#include <cpgplot.h>
};

// enums for min/max and statistics storage images

enum minmax {MIN=0, MAX=1, NMINMAX=2};
enum stats  {SUM=0, SUMSQ=1, NPTS=2, NSTATS=3};


// Public functions

template <class T>
ImageHistograms<T>::ImageHistograms (const ImageInterface<T>& imageU, 
                                     LogIO &os) : os_p(os)
//
// Constructor. 
//
{
   pHistImage_p = 0;
   pMinMaxImage_p = 0;
   pStatsImage_p = 0;

   binAll_p = True;
   goodParameterStatus_p = True;
   needStorageImage_p = True;
   doCumu_p = False;
   doGauss_p = False;
   doList_p = False;
   doLog_p = False;
   nBins_p = 25;
   nVirCursorIter_p = 0;
   cursorShape_p.resize(0);   
   device_p = "";
   nxy_p.resize(0); 
   range_p.resize(0);

  
   if (setNewImage(imageU)) {
// Default cursor axes are entire image

      Vector<Int> cursorAxes(pInImage_p->ndim());
      for (Int i=0; i<pInImage_p->ndim(); i++) cursorAxes(i) = i;

      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      goodParameterStatus_p = False;
   }
}

 
template <class T>
ImageHistograms<T>::ImageHistograms(const ImageHistograms<T> &other)
                      : os_p(other.os_p),
                        binAll_p(other.binAll_p),
                        goodParameterStatus_p(other.goodParameterStatus_p),
                        needStorageImage_p(other.needStorageImage_p),
                        doCumu_p(other.doCumu_p),
                        doGauss_p(other.doGauss_p),
                        doList_p(other.doList_p),
                        doLog_p(other.doLog_p),
                        nBins_p(other.nBins_p),
                        nVirCursorIter_p(other.nVirCursorIter_p),
                        cursorShape_p(other.cursorShape_p),
                        device_p(other.device_p),
                        nxy_p(other.nxy_p),
                        range_p(other.range_p)
//
// Copy constructor
//
{ 
 // Assign to image pointer
 
   pInImage_p = other.pInImage_p;
                        
// Copy storage images

   if (other.pHistImage_p !=0) {   
      pHistImage_p = new ArrayLattice<Int>(*(other.pHistImage_p));
   } else {
      pHistImage_p = 0;
   }

   if (other.pMinMaxImage_p !=0) {   
      pMinMaxImage_p = new ArrayLattice<T>(*(other.pMinMaxImage_p));
   } else {
      pMinMaxImage_p = 0;
   }

   if (other.pStatsImage_p !=0) {   
      pStatsImage_p = new ArrayLattice<Double>(*(other.pStatsImage_p));
   } else {
      pStatsImage_p = 0;
   }

}
      


template <class T>
ImageHistograms<T> &ImageHistograms<T>::operator=(const ImageHistograms<T> &other)
//
// Assignment operator
//
{
   if (this != &other) {
      
// Assign to image pointer
      
      pInImage_p = other.pInImage_p;
      
// Copy storage images
      
      if (other.pHistImage_p !=0) {   
         pHistImage_p = new ArrayLattice<Int>(*(other.pHistImage_p));
      } else {
         pHistImage_p = 0;
      }

      if (other.pMinMaxImage_p !=0) {   
         pMinMaxImage_p = new ArrayLattice<T>(*(other.pMinMaxImage_p));
      } else {
         pMinMaxImage_p = 0;
      }

      if (other.pStatsImage_p !=0) {   
         pStatsImage_p = new ArrayLattice<Double>(*(other.pStatsImage_p));
      } else {
         pStatsImage_p = 0;
      }

// Do the rest
  
      os_p = other.os_p;
      binAll_p = other.binAll_p;
      goodParameterStatus_p = other.goodParameterStatus_p;
      needStorageImage_p = other.needStorageImage_p;
      doCumu_p = other.doCumu_p;
      doGauss_p = other.doGauss_p;
      doList_p = other.doList_p;
      doLog_p = other.doLog_p;
      nBins_p = other.nBins_p;
      nVirCursorIter_p = other.nVirCursorIter_p;
      cursorShape_p = other.cursorShape_p;
      device_p = other.device_p;
      nxy_p = other.nxy_p;
      range_p = other.range_p;

      return *this;
   }
  
}

 

template <class T>
ImageHistograms<T>::~ImageHistograms()
//
// Destructor.  Delete storage images.
//
{
   if (pHistImage_p != 0) delete pHistImage_p;
   if (pMinMaxImage_p != 0) delete pMinMaxImage_p;
   if (pStatsImage_p != 0) delete pStatsImage_p;

}


template <class T>
Bool ImageHistograms<T>::setNBins (const Int& nBinsU)
//
// Set the number of bins
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }
      
   if (nBinsU <= 0) {
      os_p << LogIO::SEVERE << "Invalid number of bins" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   } else {
      nBins_p = nBinsU;
   }

// Signal that we need a new accumulation image

   needStorageImage_p = True;
   return True;
}


template <class T>
Bool ImageHistograms<T>::setIncludeRange(const Vector<Double>& includeU)
//
// Assign the desired inclusion range
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   Vector<Double> exclude;
   Bool noInclude, noExclude;
   ostrstream os;
   if (!ImageUtilities::setIncludeExclude(range_p, noInclude, noExclude,
                                          includeU, exclude, os)) {
      os_p << LogIO::SEVERE << "Invalid pixel inclusion range" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }
   binAll_p = noInclude;


// Signal that we need a new accumulation image
    
   needStorageImage_p = True;

   return True;
}



template <class T>
Bool ImageHistograms<T>::setGaussian (const Bool& doGaussU)
//
// Specify whether there should be a Gaussian overlay or not
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   doGauss_p = doGaussU;

   return True;
}


template <class T>
Bool ImageHistograms<T>::setForm (const Bool& doLogU, const Bool& doCumuU)
//
// Specify whether the form of the histogram should be linear/log
// or cumulative or not.
// 
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
    }

    doLog_p = doLogU;
    doCumu_p = doCumuU;

    return True;
}



template <class T>
Bool ImageHistograms<T>::setStatsList (const Bool& doListU)
//
// See if user wants to list statistics as well 
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   doList_p = doListU;

   return True;
} 


template <class T>
Bool ImageHistograms<T>::setPlotting(const String& deviceU,
                                     const Vector<Int>& nxyU)
//
// Assign the desired PGPLOT device name and number
// of subplots
//
{     
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }


// Plotting device and subplots.  nxy_p is set to [1,1] if zero length
 
   device_p = deviceU;
   nxy_p.resize(0);
   nxy_p = nxyU;
   ostrstream os;
   if (!ImageUtilities::setNxy(nxy_p, os)) {
      os_p << LogIO::SEVERE << "Invalid number of subplots" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }

   return True;
}


template <class T>
Bool ImageHistograms<T>::setNewImage(const ImageInterface<T>& image)
//    
// Assign pointer to image
//
{ 
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

  
   pInImage_p = &image;
   T *dummy = 0;
   DataType imageType = whatType(dummy);
   if (imageType !=TpFloat && imageType != TpDouble) {
       os_p << LogIO::SEVERE << "Histograms can only be evaluated from images of type : " <<
	   TpFloat << " and " << TpDouble << LogIO::POST;
       goodParameterStatus_p = False;
       pInImage_p = 0;
       return False;
   }

// Signal that we have changed the image and need a new accumulation
// image

   needStorageImage_p = True;

   return True;
}

template <class T>
Bool ImageHistograms<T>::display()
// 
// This function displays (plotting and listing) the requested
// histograms as a function of the display axes
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }


// Generate storage images if required

   if (needStorageImage_p) generateStorageImage();


// Display histograms
                 
   displayHistograms ();

   return True;
}




template <class T>
Bool ImageHistograms<T>::getHistograms (Array<Float>& values,
                                        Array<Float>& counts)
//
// Retrieve histograms values and counts into arrays
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }


// Generate storage images if required
   
   if (needStorageImage_p) generateStorageImage();


// Set up iterator to work through histogram storage image line by line
  
   IPosition cursorShape(pHistImage_p->ndim(),1);
   cursorShape(0) = pHistImage_p->shape()(0);
   RO_LatticeIterator<Int> histIterator(*pHistImage_p, cursorShape);


// Resize output arrays and setup vector iterators

   counts.resize(pHistImage_p->shape());
   values.resize(pHistImage_p->shape());
   VectorIterator<Float> valuesIterator(values);
   VectorIterator<Float> countsIterator(counts);


// Create image indexing IPositions. Make as much of them as we
// can outside of the iteration loop  
 
   Int lastAxis = pMinMaxImage_p->ndim() - 1;
   IPosition minPos(pMinMaxImage_p->ndim());
   IPosition maxPos(pMinMaxImage_p->ndim());
   minPos(lastAxis) = MIN;
   maxPos(lastAxis) = MAX;
   Vector<T> range(2);
  
// Iterate
   
   for (histIterator.reset(),valuesIterator.origin(),countsIterator.origin(); 
       !histIterator.atEnd(); histIterator++,valuesIterator.next(),countsIterator.next()) {
   
// Set indexing IPositions
   
      for (Int i=0; i<lastAxis; i++) minPos(i) = maxPos(i) = histIterator.position()(i+1);
 
 
// Work out the range for this histogram from the min/max storage image
   
      range(0) = (*pMinMaxImage_p)(minPos);   
      range(1) = (*pMinMaxImage_p)(maxPos);
   

// Extract the histogram in the appropriate form

      extractOneHistogram (valuesIterator.vector(), countsIterator.vector(), range, 
                           histIterator.vectorCursor());
   }
   return True;
}




// Private functions



template <class T>
void ImageHistograms<T>::accumulate (const IPosition& imageCursorPos,
                                     const Array<T>& cursor)

//
// Accumulate the histograms and statistical sums into the
// storage images for this chunk of the image
// 
// Inputs:
//   imageCursorPos  This is the location in the input image for the
//                   start of the current cursor chunk
//   cursor          Cursor array containing data
//
{             
   
// Fish out the min and max for this chunk of the data from the min max
// storage image
         
   IPosition minMaxPos(pMinMaxImage_p->ndim());   
   for (Int j=0; j<pMinMaxImage_p->ndim()-1; j++)
      minMaxPos(j) = imageCursorPos(displayAxes_p(j));
               
   Vector<T> clip(2);
   minMaxPos(pMinMaxImage_p->ndim()-1) = MIN;
   clip(0) = (*pMinMaxImage_p)(minMaxPos);
       
   minMaxPos(pMinMaxImage_p->ndim()-1) = MAX;
   clip(1) = (*pMinMaxImage_p)(minMaxPos);
      
         
// Set histogram bin width
   
   Int nBins = nBins_p;
   T binWidth = setBinWidth(clip, nBins);

          
// Construct vector iterator
       
   ReadOnlyVectorIterator<T> cursorIt(cursor);
   Int n1 = cursorIt.vector().nelements();


// Statistics accumulation

   Double sum = 0;
   Double sumSq = 0;
   Int nClippedPts = 0;
   T datum;

// Histogram accumulation vector

   Vector<Int> counts(nBins);
   counts = 0;

// Accumulate histogram and statistics sums.  If user didn't give
// range take slightly faster route with no ifs.
              
   if (!binAll_p) {
      while (!cursorIt.pastEnd()) {
         Int orig = cursorIt.vector().origin()(0);
         for (Int i=0; i<n1; i++) {
            datum = cursorIt.vector()(i+orig);
            if (datum >= clip(0) && datum <= clip(1)) {
               histAccum (counts, datum, clip(0), binWidth, nBins);
               statsAccum (sum, sumSq, nClippedPts, datum);
            }
         }
         cursorIt.next();
      }
   }
   else {
      while (!cursorIt.pastEnd()) {
         Int orig = cursorIt.vector().origin()(0);
         for (Int i=0; i<n1; i++) {
            datum = cursorIt.vector()(i+orig);
            histAccum (counts, datum, clip(0), binWidth, nBins);
            statsAccum (sum, sumSq, nClippedPts, datum);
         }
         cursorIt.next();   
      }
   }
   
// Update histogram storage image
   putInHist (imageCursorPos, counts);

   
// Update statistics accumulation image
   putInStats (imageCursorPos, sum, sumSq, nClippedPts);

}



template <class T>
Bool ImageHistograms<T>::displayHistograms ()
//
// Display the histograms as a function of the display axes
//
{
// Open plotting device

 
    if(cpgbeg(0, device_p.chars(), nxy_p(0), nxy_p(1)) != 1) {
       os_p << LogIO::SEVERE << "Cannot open display device" << LogIO::POST;
       return False;
    }
    cpgask(1);
    cpgsch (1.2);
    cpgsvp(0.1,0.9,0.1,0.9);
      
      
// Set up iterator to work through histogram storage image
// line by line
  
   IPosition cursorShape(pHistImage_p->ndim(),1);
   cursorShape(0) = pHistImage_p->shape()(0);
   RO_LatticeIterator<Int> histIterator(*pHistImage_p, cursorShape);
    

// Create storage image indexing IPositions. Make as much of them 
// as we can outside of the iteration loop  
 
   Int lastAxis = pMinMaxImage_p->ndim() - 1;
   IPosition minPos(pMinMaxImage_p->ndim());
   IPosition maxPos(pMinMaxImage_p->ndim());
   minPos(lastAxis) = MIN;
   maxPos(lastAxis) = MAX;
 
   lastAxis = pStatsImage_p->ndim() - 1;
   IPosition statsSumPos(pStatsImage_p->ndim());
   IPosition statsSumSqPos(pStatsImage_p->ndim());
   IPosition statsNptsPos(pStatsImage_p->ndim());
   statsSumPos  (lastAxis) = SUM;
   statsSumSqPos(lastAxis) = SUMSQ;
   statsNptsPos (lastAxis) = NPTS;
    
   IPosition histPos(pHistImage_p->ndim());
      
   Vector<T> range(2);
   Double sum, sumSq, mean, sigma, var;
   Int nPts;   
  
// Iterate
   
   for (histIterator.reset(); !histIterator.atEnd(); histIterator++) {
   
// Set indexing IPositions
   
      for (Int i=0; i<lastAxis; i++) {
         minPos(i) = maxPos(i) = statsSumPos(i) = statsSumSqPos(i) =
         statsNptsPos(i) = histIterator.position()(i+1);
      }
 
 
// Work out the range for this histogram from the min/max storage image
   
      range(0) = (*pMinMaxImage_p)(minPos);   
      range(1) = (*pMinMaxImage_p)(maxPos);
   
 
// Work out the mean and sigma from the data that made this histogram
// from the statistics storage image
   
      Double tmp = (*pStatsImage_p)(statsNptsPos);
      Int nStatsPts = Int(tmp + 0.1);
      sum = (*pStatsImage_p)(statsSumPos);
      sumSq = (*pStatsImage_p)(statsSumSqPos);
   
      mean = sum/nStatsPts;
      var = (sumSq - sum*sum/nStatsPts)/(nStatsPts-1);
      sigma = 0.0;
      if (var > 0.0) sigma = sqrt(var);
   
   
// Display the histogram
 
      displayOneHistogram (histIterator.vectorCursor(), histIterator.position(),
                           range, nStatsPts, sum, mean, sigma, var);
 
   }
      
// Close plotting device
 
   cpgend();
   return True;
}
 
 

template <class T>
void ImageHistograms<T>::displayOneHistogram (const Vector<Int>& intCounts,
                                              const IPosition& histPos,   
                                              const Vector<T>& range,
                                              const Int& nStatsPts,
                                              const Double& statsSum,
                                              const Double& statsMean,
                                              const Double& statsSigma,
                                              const Double& statsVar)
//
// Display the histogram and optionally the equivalent Gaussian
// 
{
    
// Are we going to see the Gaussian ?
 
   Bool doGauss2 = False;
   if (doGauss_p && statsSigma>0) doGauss2 = True;
 
   
// Make a floating copy of the histogram counts for log taking
// and easy conversion to a plot array
 
   Vector<Float> counts(intCounts.nelements());
   for (Int i=0; i<intCounts.nelements(); i++) counts(i) = intCounts(i);
 
 // Set bin width  
      
   Int nBins = nBins_p;
   Float binWidth = Float(setBinWidth(range, nBins));
      
    
// Get extrema    
      
   Float xMin = Float(range(0));
   Float xMax = Float(range(1));
   Float yMax = max(counts.ac());
 
   
// Get integral of linear histogram
   
   Float dSum = 0.0;
   if (doGauss2) dSum = sum(counts.ac()) * binWidth;
 
 
// Make histogram cumulative if desired
      
   if (doCumu_p) makeCumulative (counts, yMax, nBins, 1.0);
   
          
// Make histogram logarithmic if desired
         
   if (doLog_p) makeLogarithmic (counts, yMax, nBins);
   
 
// Generate the equivalent Gaussian if desired
      
   Vector<Float> gX, gY;
   Int nGPts;
   Float gMax;
   if (doGauss2) {
      makeGauss (nGPts, gMax, gX, gY, statsMean, statsSigma, dSum,
                 xMin, xMax, binWidth);   
      yMax = max(yMax, gMax);
   }
      
 
// Stretch extrema by 5%
         
   Float yMin = 0.0;
   ImageUtilities::stretchMinMax(xMin, xMax);
   ImageUtilities::stretchMinMax(yMin, yMax);


// Write statistics to a LogIO object

   if (doList_p) {
      os_p << endl;
      Int nDisplayAxes = displayAxes_p.nelements();
      if (nDisplayAxes > 0) {   
        for (Int i=0; i<nDisplayAxes; i++) {
          os_p << pInImage_p->coordinates().worldAxisNames()(displayAxes_p(i))
               << "=" << histPos(i+1)+1 << "  ";
        }
      }

// Have to convert LogIO object to ostream before can apply 
// the manipulators

      Int oPrec = 6;
      Int oWidth = 15;
      os_p.output().fill(' ');
      os_p.output().precision(oPrec);
      os_p.output().setf(ios::scientific, ios::floatfield);
      os_p.output().setf(ios::left, ios::adjustfield);
   
      os_p << endl << "No. binned = ";
      os_p.output() << setw(oWidth) << nStatsPts << endl;

      os_p << "Sum        = ";
      os_p.output() << setw(oWidth) << statsSum <<   "       Mean     = ";
      os_p.output() << setw(oWidth) << statsMean << endl;

      os_p << "Variance   = ";
      os_p.output() << setw(oWidth) << statsVar;
      if (statsVar > 0.0) {
         os_p << "       Sigma    = ";
         os_p.output() << setw(oWidth) << statsSigma << endl;
      } else {
         os_p << endl;
      }
 
      os_p << endl;  
      os_p << "Bin width  = ";
      os_p.output() << setw(oWidth) << binWidth << endl;
      os_p << "Min binned = ";
      os_p.output() << setw(oWidth) << range(0) << "       Max binned = ";
      os_p.output() << setw(oWidth) << range(1) << endl << endl << endl;
      os_p.post();
   }
      
// Generate abcissa and ordinate arrays
    
   float* px = new float[nBins];
   Float xx = range(0) + binWidth/2.0;
   for (i=0; i<nBins; i++) {
     px[i] = xx;
     xx += binWidth;
   }
   Bool deleteItY;
   const float* py = counts.getStorage(deleteItY);
    
   
// Plot
         
   cpgpage();
   cpgswin(xMin, xMax, 0.0, yMax);
   cpgbox("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   plotHist (nBins, px, py);
      
   delete [] px;
   counts.freeStorage(py, deleteItY);
   
//    
 
   if (doGauss2) {
      Bool deleteItX;
      const float* px = gX.getStorage(deleteItX); 
      Bool deleteItY;
      const float* py = gY.getStorage(deleteItY);
                 
      cpgline (nGPts, px, py);
                 
      gX.freeStorage(px, deleteItX);
      gY.freeStorage(py, deleteItY);
   }
 
// Label
  
   if (doCumu_p) {
      if (doLog_p)
         cpglab("Pixel Value", "Log10 (Cumulative Counts)", "");
      else
         cpglab("Pixel Value", "Cumulative Counts", "");
   }
   else {
      if (doLog_p)
         cpglab("Pixel Value", "Log10 (Counts)", "");
      else
         cpglab("Pixel Value", "Counts", "");
   }
   
      
// Write values of the display axes on the plot
 
   writeDispAxesValues (histPos, xMin, yMax);
   
}
 

template <class T>
void ImageHistograms<T>::extractOneHistogram (Vector<Float>& values, 
                                              Vector<Float>& counts,
                                              const Vector<T>& range, 
                                              const Vector<Int>& intCounts)
//
// Extract this histogram, convert to the appropriate form
// and return the values and counts
//
{

// Set bin width  
      
   Int nBins = nBins_p;
   Float binWidth = Float(setBinWidth(range, nBins));
      

// Copy histogram counts into output Float array and generate
// values (abcissa) array
 
   Float xx = range(0) + binWidth/2.0;
   Float yMax = -1.0;
   for (Int i=0; i<intCounts.nelements(); i++) {
      values(i) = xx;
      counts(i) = intCounts(i);
      xx += binWidth;
      yMax = max(yMax,counts(i));
   }
 
   
// Make histogram cumulative if desired
      
   if (doCumu_p) makeCumulative (counts, yMax, nBins, 1.0);
   
          
// Make histogram logarithmic if desired
         
   if (doLog_p) makeLogarithmic (counts, yMax, nBins);

}

template <class T>
Bool ImageHistograms<T>::setAxes (const Vector<Int>& axesU)
//
// This function sets the cursor axes and the display axes
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }


// Set cursor arrays (can't assign to potentially zero length array)

   Vector<Int> cursorAxes(axesU);
   ostrstream os;
   if (!ImageUtilities::setCursor(nVirCursorIter_p, cursorShape_p, 
        cursorAxes, pInImage_p, True, 2, os)) {
      os_p << LogIO::SEVERE << "Invalid cursor axes given" << LogIO::POST;
      return False;
   }   
   
// Set display axes array

   ImageUtilities::setDisplayAxes (displayAxes_p, cursorAxes, pInImage_p->ndim());


// Signal that we have changed the axes and need a new accumulation image

   needStorageImage_p = True;

   return True;
}



template <class T>
void ImageHistograms<T>::findMinMax (RO_LatticeIterator<T>& imageIterator)
               
//    
// In order to work out a histogram we need to know the min and max
// of each data chunk to be histogrammed.  These are stored in an image.
// If the user specifies the range fill the min max storage image with
// those values, otherwise iterate through the data to work it out
//
// The min/max image is stored with the display axes first and the min and
// max as the last axes.
// 
// 
// Inputs:
//   imageIterator  The iterator already setup to iterate
//                  through image.
//
{
// Create indexing IPositions and fill in values of last axis
         
   Int lastAxis = pMinMaxImage_p->ndim() - 1;
   IPosition minPos(pMinMaxImage_p->ndim());
   IPosition maxPos(pMinMaxImage_p->ndim());
   minPos(lastAxis) = MIN;
   maxPos(lastAxis) = MAX;
         
   
// Now take quick route if user gave range else do it the hard way
   
   if (range_p.nelements() != 0) {
      os_p << LogIO::NORMAL << "Fill minmax image" << LogIO::POST;

// Set up the min and max slices we will put in place
      
      IPosition sliceShape(pMinMaxImage_p->ndim(),1);
      if (pMinMaxImage_p->ndim() > 1) {
         sliceShape(0) = pMinMaxImage_p->shape()(0);
      } else {
         sliceShape(0) = 1;
      }
      
      Array<T> sliceMin(sliceShape);
      Array<T> sliceMax(sliceShape);

      sliceMin.set(T(range_p(0)));
      sliceMax.set(T(range_p(1)));
  
         
// User gave the range.  Fill in the minMax image with it.  We iterate
// through by a shape of [n,1,1,....,2] and  put min and max slices
         
      IPosition cursorShape(pMinMaxImage_p->ndim(),1);
      cursorShape(0) = pMinMaxImage_p->shape()(0);
      cursorShape(lastAxis) = 2;
            
      RO_LatticeIterator<T> minMaxIterator(*pMinMaxImage_p, cursorShape);
      IPosition stride(pMinMaxImage_p->ndim(),1);
         
         
// Iterate and put slices

      for (minMaxIterator.reset(); !minMaxIterator.atEnd(); minMaxIterator++) {
         for (Int j=0; j<lastAxis; j++)
            minPos(j) = maxPos(j) = minMaxIterator.position()(j);
         pMinMaxImage_p->putSlice (sliceMin, minPos, stride);
         pMinMaxImage_p->putSlice (sliceMax, maxPos, stride);
      }
   } else {

// Damn, the user didn't tell us the range to bin.  Work through the
// image and for each chunk we want to histogram, find the min and max

      T dMin, dMax, tmp;
      Bool init = True;
      Int nIter = 0;
      os_p << LogIO::NORMAL << "Finding min/max for each histogram data chunk" << LogIO::POST;

      for (imageIterator.reset(); !imageIterator.atEnd(); imageIterator++) {

         minMax(dMin, dMax, imageIterator.cursor());

         for (Int j=0; j<lastAxis; j++)
            minPos(j) = maxPos(j) = imageIterator.position()(displayAxes_p(j));
  
         if (init) {
            (*pMinMaxImage_p)(minPos) = dMin;
            (*pMinMaxImage_p)(maxPos) = dMax;
         } else {
            tmp = (*pMinMaxImage_p)(minPos);
            if (dMin < tmp) (*pMinMaxImage_p)(minPos) = dMin;

            tmp = (*pMinMaxImage_p)(maxPos);
            if (dMax > tmp) (*pMinMaxImage_p)(maxPos) = dMax;
         }
            

// Do we need to initialize the min/max axes ?
   
         nIter++;
         if (nIter == nVirCursorIter_p) {
            init = True;  
            nIter = 0;
         } else {
           init = False;
         }
      }
   }
}





template <class T>
void ImageHistograms<T>::generateStorageImage()
//
// Iterate through the image and generate the histogram accumulation image
//
{

// Work out dimensions of histogram accumulation image

   IPosition histImageShape;
   ImageUtilities::setStorageImageShape(histImageShape, False, nBins_p,
                                        displayAxes_p, pInImage_p->shape());

// Create new histogram storage image. 

   if (pHistImage_p != 0) delete pHistImage_p;
   pHistImage_p = new ArrayLattice<Int>(histImageShape);
   pHistImage_p->set(0);
   os_p << LogIO::NORMAL << "Created new storage images" << LogIO::POST;

   needStorageImage_p = False;     


// Create image iterator
   
   RO_LatticeIterator<T> pixelIterator(*pInImage_p, cursorShape_p);


// Before we can proceed, we need to know the min and max of each chunk
// of data that we will want to make a histogram for.  If the user does
// not give us the range to histogram, we must work it out by making an
// extra pass through the data.  We store the min and max in a storage image

   IPosition minMaxImageShape;
   ImageUtilities::setStorageImageShape(minMaxImageShape, True, Int(NMINMAX),
                                        displayAxes_p, pInImage_p->shape());
   if (pMinMaxImage_p != 0) delete pMinMaxImage_p;
   pMinMaxImage_p = new ArrayLattice<T>(minMaxImageShape);
   pMinMaxImage_p->set(T(0.0)); 
   findMinMax (pixelIterator);


// Create statistics storage image (keeps sums of histogram chunks so we can
// work out the mean and sigma for the Gaussian overlays)

   IPosition statsImageShape;
   ImageUtilities::setStorageImageShape(statsImageShape, True, Int(NSTATS),
                                        displayAxes_p, pInImage_p->shape());
   if (pStatsImage_p != 0) delete pStatsImage_p;
   pStatsImage_p = new ArrayLattice<Double>(statsImageShape);
   pStatsImage_p->set(Double(0.0));
   
   
// Iterate through image and accumulate histograms
     
   os_p << LogIO::NORMAL << "Begin accumulating histograms" << LogIO::POST;
   for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {
      accumulate (pixelIterator.position(), pixelIterator.cursor());
   }
}



template <class T>
inline void ImageHistograms<T>::histAccum (Vector<Int>& counts,
                                           const T& datum,
                                           const T& dMin,
                                           const T& binWidth,
                                           const Int& nBins)
//
// Determine the histogram bin that this datum falls
// in and increment the histogram storage vector
// 
{
   
   Int iBin = min(nBins-1, Int((datum-dMin)/binWidth));
   counts(iBin)++;
}


template <class T>
void ImageHistograms<T>::makeCumulative (Vector<Float>& counts,
                                         Float& yMax,
                                         const Int& nBins,
                                         const Float& scale)
{
   counts(0) = scale * counts(0);
   for (Int i=1; i<nBins; i++) counts(i) = counts(i)*scale + counts(i-1);
           
   yMax = counts(nBins-1);
}
                          

template <class T>
void ImageHistograms<T>::makeGauss (Int& nGPts,
                                    Float& gMax,
                                    Vector<Float>& gX,
                                    Vector<Float>& gY,
                                    const Double& dMean,
                                    const Double& dSigma,
                                    const Float& dSum,
                                    const Float& xMin,
                                    const Float& xMax,
                                    const Float& binWidth)
//
// Make overlay Gaussian with the given parameters
{
      
// 100 points please
 
   nGPts = 100;
   gX.resize(nGPts);
   gY.resize(nGPts);
 
   
// Set up Gaussian functional
   
   Float gaussAmp = dSum * C::_1_sqrt2 * C::_1_sqrtpi / dSigma;
   Float gWidth = sqrt(8.0*C::ln2) * dSigma;
   Gaussian1D<Float> gauss(gaussAmp, Float(dMean), gWidth);
 
   
// Generate Gaussian
     
   Float dgx = (xMax - xMin) / Float(nGPts);
   Float xx, tmp;
   Int i;
   for (i=0,xx=xMin,gMax=0.0; i<nGPts; i++) {
      gX(i) = xx;
      gY(i) = gauss(xx);
      tmp = gY(i);
      gMax = max(gMax, tmp);
      xx += dgx;
   }
 
 
// Make cumulative if desired
   
   Float scale = dgx / binWidth;
   if (doCumu_p) makeCumulative (gY, gMax, nGPts, scale);
   
   
// Take log if desired
   
   if (doLog_p) makeLogarithmic (gY, gMax, nGPts);
         
}
   
   
   
template <class T>
void ImageHistograms<T>::makeLogarithmic (Vector<Float>& counts,
                                          Float& yMax,
                                          const Int& nBins)
{
   yMax = 0.0;
   for (Int i=0; i<nBins; i++) {
     if (counts(i) > 0.0) counts(i) = log10(counts(i));
     yMax = max(yMax, counts(i)); 
   }
}
                


template <class T>
void ImageHistograms<T>::plotHist (const Int& n, 
                                  const float* px, 
                                  const float* py)
//
// Inputs
//   n      Number of points
//   px,py  Abcissa and ordinate arrays to plot
//
{ 
   Float width = (px[1] - px[0]) / 2.0;
   for (Int i=0; i<n; i++) {
      float xx = px[i] - width;
 
      cpgmove (xx, 0.0);
      cpgdraw (xx, py[i]);
                          
      cpgmove (xx, py[i]);
      xx = px[i] + width;
      cpgdraw (xx, py[i]);
   
      cpgmove (xx, py[i]);
      cpgdraw (xx, 0.0);
    }
}


template <class T>
void ImageHistograms<T>::putInHist (const IPosition& imageCursorPos,
                                    const Vector<Int>& newCounts)
//
// Update the histogram storage image with the histogram
// accumulated from the current chunk
//
{   

// Location of current histogram in storage image

   IPosition histPos(pHistImage_p->ndim());
   histPos(0) = 0;
   for (Int j=1; j<pHistImage_p->ndim(); j++)
      histPos(j) = imageCursorPos(displayAxes_p(j-1));

   
// Get current histogram for this location
   
   IPosition stride(pHistImage_p->ndim(),1);
   IPosition shape(pHistImage_p->ndim(),1);
   shape(0) = pHistImage_p->shape()(0);
   Array<Int> counts(shape);   
   pHistImage_p->getSlice (counts, histPos, shape, stride);
   

// Update it.  Because the bloody get/put slice functions do not work as they
// should I have to make the slice multi-dimensional and piss about here with
// a one pass iterator just to add the new counts in.

   VectorIterator<Int> posIt(counts);
   while (!posIt.pastEnd()) {
      posIt.vector().ac() += newCounts.ac();
      posIt.next();
   }
  

// Put it back
 
   pHistImage_p->putSlice (counts, histPos, stride);
}



template <class T>
void ImageHistograms<T>::putInStats (const IPosition& imageCursorPos,
                                     const Double& sum, 
                                     const Double& sumSq,
                                     const Int& n)
//
// Increment the statistics storage image.  This is only
// called once per data cursor chunk so we don't worry
// about passing in pre-computed IPositions or accessing
// lattices pixel by pixel
{     
   Int lastAxis = pStatsImage_p->ndim()-1;
   IPosition statsPos(pStatsImage_p->ndim());
   for (Int j=0; j<lastAxis; j++) 
      statsPos(j) = imageCursorPos(displayAxes_p(j));
   
   statsPos(lastAxis) = SUM;
   Double tmp = (*pStatsImage_p)(statsPos);
   tmp += sum;
   (*pStatsImage_p)(statsPos) = tmp;
   
   statsPos(lastAxis) = SUMSQ;
   tmp = (*pStatsImage_p)(statsPos);
   tmp += sumSq;
   (*pStatsImage_p)(statsPos) = tmp;

   statsPos(lastAxis) = NPTS;
   tmp = (*pStatsImage_p)(statsPos);
   tmp += n;
   (*pStatsImage_p)(statsPos) = tmp;
}



template <class T>
T ImageHistograms<T>::setBinWidth (const Vector<T>& clip,
                                          const Int& nBins)
//
// Set the bin width for the current histogram
//
{ 
return ((clip(1) - clip(0)) / nBins);
}


template <class T>
inline void ImageHistograms<T>::statsAccum (Double& sum,
                                            Double& sumSq,
                                            Int& n,
                                            const T& datum)
{
   sum += datum;
   sumSq += datum*datum;
   n++;
}


template <class T>
void ImageHistograms<T>::writeDispAxesValues (const IPosition& histPos,
                                              const Float& xMin,
                                              const Float& yMax)
{
   
   const int BUFL=64;
   char buf[BUFL];
   ostrstream oss(buf,BUFL,ios::out);
 
 
// Fill the string stream with the name and value of each higher order
// display axis than the one used for the abcissa, if there are any
  
   Int nDisplayAxes = displayAxes_p.nelements();
   if (nDisplayAxes > 0) {
     for (Int i=0; i<nDisplayAxes; i++) {
       oss << "  " << pInImage_p->coordinates().worldAxisNames()(displayAxes_p(i))
           << "="  << histPos(i+1) + 1;
     }           
     oss << ends;
     char* tLabel = oss.str();
   
// Write on plot
 
     float xb[4], yb[4];
     cpgqtxt (0.0, 0.0, 0.0, 0.0, "X", xb, yb);
     float dx = xb[3] - xb[0];
     cpgqtxt (0.0, 0.0, 0.0, 0.0, tLabel, xb, yb);
     float dy = yb[1] - yb[0];
                           
     float mx = xMin + dx; 
     float my = yMax + 0.5*dy;
      
     int tbg;
     cpgqtbg(&tbg);
     cpgstbg(0);
     cpgptxt (mx, my, 0.0, 0.0, tLabel);
     cpgstbg(tbg);
   }
}

