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
#include <aips/Measures/MVAngle.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>
  
#include <trial/Coordinates.h>  
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/ImageHistograms.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/PagedArray.h>

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
                        cursorAxes_p(other.cursorAxes_p),
                        displayAxes_p(other.displayAxes_p),
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
      pHistImage_p = new PagedArray<Int>(*(other.pHistImage_p));
   } else {
      pHistImage_p = 0;
   }

   if (other.pMinMaxImage_p !=0) {   
      pMinMaxImage_p = new PagedArray<T>(*(other.pMinMaxImage_p));
   } else {
      pMinMaxImage_p = 0;
   }

   if (other.pStatsImage_p !=0) {   
      pStatsImage_p = new PagedArray<Double>(*(other.pStatsImage_p));
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
         pHistImage_p = new PagedArray<Int>(*(other.pHistImage_p));
      } else {
         pHistImage_p = 0;
      }

      if (other.pMinMaxImage_p !=0) {   
         pMinMaxImage_p = new PagedArray<T>(*(other.pMinMaxImage_p));
      } else {
         pMinMaxImage_p = 0;
      }

      if (other.pStatsImage_p !=0) {   
         pStatsImage_p = new PagedArray<Double>(*(other.pStatsImage_p));
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
      cursorAxes_p = other.cursorAxes_p;
      displayAxes_p = other.displayAxes_p;
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


// Create storage image indexing IPositions and slice arrays. 
// Fill them in as much as we can outside of the iteration loop  
 
   IPosition minMaxPos(pMinMaxImage_p->ndim());
   IPosition minMaxShape = sliceMinMaxShape();
   Array<T> minMaxSlice(minMaxShape);   
   IPosition minMaxStride(pMinMaxImage_p->ndim(),1);
   Int lastMinMaxAxis = pMinMaxImage_p->ndim() - 1;

   Int lastHistAxis = pHistImage_p->ndim() - 1;
   Vector<T> range(2);

// Iterate through histogram storage image
   
   for (histIterator.reset(),valuesIterator.origin(),countsIterator.origin(); 
       !histIterator.atEnd(); histIterator++,valuesIterator.next(),countsIterator.next()) {
   
// Set indexing IPositions
   
       minMaxPos(lastMinMaxAxis) = 0;
       for (Int i=0; i<lastHistAxis; i++) minMaxPos(i) = histIterator.position()(i+1);
 
// Work out the range for this histogram from the min/max storage image
 
      pMinMaxImage_p->getSlice (minMaxSlice, minMaxPos, minMaxShape, minMaxStride);
      minMaxPos = 0;
      minMaxPos(lastMinMaxAxis) = MIN;
      range(0) = minMaxSlice(minMaxPos);
      minMaxPos(lastMinMaxAxis) = MAX;
      range(1) = minMaxSlice(minMaxPos);

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
// storage image.  

   IPosition shape = sliceMinMaxShape();
   Array<T> slice(shape);

   IPosition start = locInMinMax(imageCursorPos);
   pMinMaxImage_p->getSlice (slice, start, shape, 
                             IPosition(pMinMaxImage_p->ndim(),1));

   Int lastAxis = pMinMaxImage_p->ndim()-1;
   Vector<T> clip(2);
   start = 0;
   start(lastAxis) = MIN;
   clip(0) = slice(start);
       
   start(lastAxis) = MAX;
   clip(1) = slice(start);

         
// Set histogram bin width
   
   Int nBins = nBins_p;
   T binWidth = setBinWidth(clip, nBins);

          
// Construct vector iterator to iterate through the cursor
       
   ReadOnlyVectorIterator<T> cursorIt(cursor);
   Int n1 = cursorIt.vector().nelements();


// Statistics accumulation objects

   Vector<Double> stats(NSTATS);
   stats = 0.0;
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
               statsAccum (stats, datum);
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
            statsAccum (stats, datum);
         }
         cursorIt.next();   
      }
   }
   
// Update histogram storage image

   putInHist (imageCursorPos, counts);

   
// Update statistics accumulation image

   putInStats (imageCursorPos, stats);

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


// Create storage image indexing IPositions and slice arrays. 
// Fill them in as much as we can outside of the iteration loop  
 
   IPosition minMaxPos(pMinMaxImage_p->ndim());
   IPosition minMaxShape = sliceMinMaxShape();
   Array<T> minMaxSlice(minMaxShape);   
   IPosition minMaxStride(pMinMaxImage_p->ndim(),1);
   Int lastMinMaxAxis = pMinMaxImage_p->ndim() - 1;

   IPosition statsPos(pStatsImage_p->ndim());
   IPosition statsShape = sliceStatsShape();
   Array<Double> statsSlice(statsShape);
   IPosition statsStride(pStatsImage_p->ndim(),1);    
   Int lastStatsAxis = pStatsImage_p->ndim() - 1;

   Int lastHistAxis = pHistImage_p->ndim() - 1;
      
   Vector<T> range(2);
   Double sum, sumSq, mean, sigma, var;
   Int nPts;   
  

// Iterate through histogram storage image

   for (histIterator.reset(); !histIterator.atEnd(); histIterator++) {
   

// Fill in the rest of the indexing IPositions
   
      for (Int i=0; i<lastHistAxis; i++) {
         minMaxPos(i) = statsPos(i) = histIterator.position()(i+1);
      }
      minMaxPos(lastMinMaxAxis) = 0;
      statsPos(lastStatsAxis) = 0;
 
// Work out the range for this histogram from the min/max storage image

      pMinMaxImage_p->getSlice (minMaxSlice, minMaxPos, minMaxShape, minMaxStride);
      minMaxPos = 0;
      minMaxPos(lastMinMaxAxis) = MIN;
      range(0) = minMaxSlice(minMaxPos);
      minMaxPos(lastMinMaxAxis) = MAX;
      range(1) = minMaxSlice(minMaxPos);


// Work out the mean and sigma from the data that made this histogram
// from the statistics storage image
   
      pStatsImage_p->getSlice (statsSlice, statsPos, statsShape, statsStride);
      statsPos = 0;
      statsPos(lastStatsAxis) = SUM;
      sum = statsSlice(statsPos);
      statsPos(lastStatsAxis) = SUMSQ;
      sumSq = statsSlice(statsPos);
      statsPos(lastStatsAxis) = NPTS;
      nPts = Int(statsSlice(statsPos) + 0.1);
   
      mean = sum/nPts;
      var = (sumSq - sum*sum/nPts)/(nPts-1);
      sigma = 0.0;
      if (var > 0.0) sigma = sqrt(var);
   
   
// Display the histogram

      if (!displayOneHistogram (histIterator.vectorCursor(), histIterator.position(),
                                range, nPts, sum, mean, sigma, var)) return False;
 
   }
      
// Close plotting device
 
   cpgend();
   return True;
}
 
 

template <class T>
Bool ImageHistograms<T>::displayOneHistogram (const Vector<Int>& intCounts,
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
//  Inputs
//    histPos    location in histogram storage image of start of 
//               this histogram. Remember that the first axis
//               of the storage image has the counts.
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

// List coordinates of display axes for this histogram

      os_p << endl;
      Int nDisplayAxes = displayAxes_p.nelements();
      if (nDisplayAxes > 0) {   
         Vector<String> sWorld(1);
         Vector<Double> pixel(1);
         Int oPrec = 6;

         for (Int j=0; j<nDisplayAxes; j++) {
            pixel(0) = histPos(j+1);
            if (!pix2World (sWorld, displayAxes_p(j), pixel, oPrec)) return False;

            Int worldAxis = pixelAxisToWorldAxis(pInImage_p->coordinates(), displayAxes_p(j));
            String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);

            os_p <<  ImageUtilities::shortAxisName(name)
                 << "=" << histPos(j+1)+1 << " (" << sWorld(0) << ")";
            if (j < nDisplayAxes-1) os_p << ", ";
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
 
   if (!writeDispAxesValues (histPos, xMin, yMax)) return False;
   
   return True;
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
         
   IPosition minPos(pMinMaxImage_p->ndim(),0);
   IPosition maxPos(pMinMaxImage_p->ndim(),0);
   minPos(pMinMaxImage_p->ndim()-1) = MIN;
   maxPos(pMinMaxImage_p->ndim()-1) = MAX;

   
// Now take quick route if user gave range else do it the hard way
   
   if (range_p.nelements() != 0) {
      os_p << LogIO::NORMAL << "Fill min/max storage image" << LogIO::POST;


// Set up the slice we will put in place.  Only the first and last axes of
// the slice are of non-unit shape.  If its only 1D, the shape is just [NMINMAX]
      
      IPosition sliceShape = sliceMinMaxShape();
      sliceShape(0) = pMinMaxImage_p->shape()(0);
      Array<T> slice(sliceShape);

      if (pMinMaxImage_p->ndim() > 1) {
         for (Int i=0; i<sliceShape(0); i++) {
            minPos(0) = maxPos(0) = i;
            slice(minPos) = range_p(0);
            slice(maxPos) = range_p(1);
         }
      } else {
         slice(minPos) = range_p(0);
         slice(maxPos) = range_p(1);
      }
  
// We iterate through by a shape of [n,1,1,....,2] and put the min/max slice
         
      LatticeIterator<T> minMaxIterator(*pMinMaxImage_p, sliceShape);

      for (minMaxIterator.reset(); !minMaxIterator.atEnd(); minMaxIterator++) {
         minMaxIterator.cursor() = slice;
      }
   } else {
      os_p << LogIO::NORMAL << "Finding min/max for each histogram data chunk" << LogIO::POST;

// Damn, the user didn't tell us the range to bin.  Work through the
// image and for each chunk we want to histogram, find the min and max


      T dMin, dMax, tmp;
      Bool init = True;
      Int nIter = 0;

      IPosition sliceShape = sliceMinMaxShape();
      Array<T> slice(sliceShape);
      IPosition stride(pMinMaxImage_p->ndim(),1);
      IPosition pos;

      for (imageIterator.reset(); !imageIterator.atEnd(); imageIterator++) {

// Find min and max

         minMax(dMin, dMax, imageIterator.cursor());

// Find location in min/max image of this min/max slice
 
         pos = locInMinMax (imageIterator.position());

// Fill slice 

         if (init) {
            slice(minPos) = dMin;
            slice(maxPos) = dMax;
         } else {
            pMinMaxImage_p->getSlice (slice, pos, sliceShape, stride);
            slice(minPos) = min(slice(minPos), dMin);
            slice(maxPos) = max(slice(maxPos), dMax);
         }

// Put it in

         pMinMaxImage_p->putSlice (slice, pos, stride);            


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
// Generate the histogram, min/max and statistics storage images.
// The iterate through the image and fill these storage images.
//
{

// Create image iterator
   
   RO_LatticeIterator<T> pixelIterator(*pInImage_p, cursorShape_p);


// Find the directory of the input image

   File inputImageName(pInImage_p->name());
   const String path = inputImageName.path().dirName() + "/";


// Delete old histogram storage image

   if (pHistImage_p != 0) delete pHistImage_p;

   {      

// Create scratch histogram storage image file name.  

      Path fileName = File::newUniqueName(path, String("PagedArray"));
      SetupNewTable setup(fileName.absoluteName(), TableDesc(), Table::Scratch);
      Table myTable(setup);   


// Set storage image shape.  The first axis is the histogram axis
      
      IPosition storeImageShape;  
      ImageUtilities::setStorageImageShape(storeImageShape, False, nBins_p,
                                           displayAxes_p, pInImage_p->shape());

// Set tile shape.   The histogram storage image is only ever accessed by
// vectors along the first axis.   Therefore set the tile shape to be unity
// except for the first axis and equal to the length of the first axis
// (the number of bins) for the first axis 

      IPosition tileShape(storeImageShape.nelements(),1);
      tileShape(0) = storeImageShape(0);

// Create new histogram storage image.    The first axis
// is the histogram axis, the higher axes are the display axes

      pHistImage_p = new PagedArray<Int>(storeImageShape, myTable, tileShape);
      pHistImage_p->set(0);
   }



// Delete old min/max storage image

   if (pMinMaxImage_p != 0) delete pMinMaxImage_p;

   {      

// Generate the min/max storage image file name

      Path fileName = File::newUniqueName(path, String("PagedArray"));
      SetupNewTable setup(fileName.absoluteName(), TableDesc(), Table::Scratch);
      Table myTable(setup);   

// Set storage image shape.  The last axis is min and max.  The preceding
// ones are the display axes
      
      IPosition storeImageShape;
      ImageUtilities::setStorageImageShape(storeImageShape, True, Int(NMINMAX),
                                           displayAxes_p, pInImage_p->shape());

// Set tile shape.   The min/max storage image is accessed by slices along the
// first (length first display axis) and last (length NMINMAX) axes.  Therefore set 
// the tile shape to be unity on all  the other axes.  For the first axis take
// the length of the axis and for the last axis its just NMINMAX (which is just
// 2 or so).

      IPosition tileShape(storeImageShape.nelements(),1);
      tileShape(tileShape.nelements()-1) = storeImageShape(tileShape.nelements()-1);
      if (displayAxes_p.nelements() > 0) tileShape(0) = storeImageShape(0);
      
// Create new min/max storage image.   

      pMinMaxImage_p = new PagedArray<T>(storeImageShape, myTable, tileShape);
      pMinMaxImage_p->set(T(0.0)); 
   }



// Delete old statistics storage image

   if (pStatsImage_p != 0) delete pStatsImage_p;

   {

// Create statistics storage image file name

      Path fileName = File::newUniqueName(path, String("PagedArray"));
      SetupNewTable setup(fileName.absoluteName(), TableDesc(), Table::Scratch);
      Table myTable(setup);   

// Set storage image shape.  The last axis is the statistics axis
      
      IPosition storeImageShape;
      ImageUtilities::setStorageImageShape(storeImageShape, True, Int(NSTATS),
                                           displayAxes_p, pInImage_p->shape());

// Set tile shape.   The statistics storage image is accessed by slices along the
// last axis (length NSTATS) only. So the tile shape is set to unity on all  
// the other axes.  

      IPosition tileShape(storeImageShape.nelements(),1);
      tileShape(tileShape.nelements()-1) = storeImageShape(tileShape.nelements()-1);
      
// Create new statistics storage image.   

      pStatsImage_p = new PagedArray<Double>(storeImageShape, myTable, tileShape);
      pStatsImage_p->set(Double(0.0));
   }


// Note to the unwary user that we have made the storage images

   os_p << LogIO::NORMAL << "Created new storage images" << LogIO::POST;
   needStorageImage_p = False;     
   

// Before we can proceed, we need to know the min and max of each chunk
// of data that we will want to make a histogram for.  If the user does
// not give us the range to histogram, we must work it out by making an
// extra pass through the data.  

   findMinMax (pixelIterator);

   

// Iterate through image and accumulate histogram and statistics images
     
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
inline IPosition ImageHistograms<T>::locInHist (const IPosition& imageCursorPos)
//
// Given a location in the input image, find the start location for a 
// histogram slice in the histogram storage image at this location
//
{
   IPosition pos(pHistImage_p->ndim(),0);
   for (Int j=1; j<pHistImage_p->ndim(); j++) pos(j) = imageCursorPos(displayAxes_p(j-1));

   return pos;
}

template <class T>
inline IPosition ImageHistograms<T>::locInMinMax (const IPosition& imageCursorPos)
//
// Given a location in the input image, find the start location for a 
// min/max slice in the min/max storage image at this location
//
{
   IPosition pos(pMinMaxImage_p->ndim(),0);
   for (Int j=0; j<pMinMaxImage_p->ndim()-1; j++) pos(j) = imageCursorPos(displayAxes_p(j));

   return pos;
}

template <class T>
inline IPosition ImageHistograms<T>::locInStats (const IPosition& imageCursorPos)
//
// Given a location in the input image, find the start location for a 
// statistics slice in the statistics storage image at this location
//
{
   IPosition pos(pStatsImage_p->ndim(),0);
   for (Int j=0; j<pStatsImage_p->ndim()-1; j++) pos(j) = imageCursorPos(displayAxes_p(j));

   return pos;
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
Bool ImageHistograms<T>::pix2World (Vector<String>& sWorld,
                                    const Int& pixelAxis,
                                    const Vector<Double>& pixel,
                                    const Int& prec) 
//
// Convert the vector of pixel coordinates to a formatted Vector
// of strings giving the world coordinate in the specified world axis.
// 
// Inputs
//   pixelAxis   The pixel axis whose coordinates we are interested in
//   pixel       Vector of pixel coordinates (0 rel) to transform
//               for the pixel axis of interest
//   prec        Precision to format output of scientific
//               formatted numbers (linear axes etc)
// Outputs
//   sWorld      Vector of formatted strings of world coordinates
//               for the pixel axis
//
{
   Int n1 = pixel.nelements();
   sWorld.resize(n1);
   
// Get coordinate system.
   
   CoordinateSystem cSys = pInImage_p->coordinates();


// Create pixel and world vectors for all pixel axes. Initialize pixel values
// to reference pixel, but if an axis is a cursor axis (whose coordinate is
// essentially being averaged) set the pixel to the mean pixel.

   Vector<Double> pix(cSys.nPixelAxes());
   Vector<Double> world(cSys.nPixelAxes());
   pix = cSys.referencePixel(); 
   for (Int i=0; i<pix.nelements(); i++) {
     if (ImageUtilities::inVector(i, cursorAxes_p)) {
       pix(i) = Double(pInImage_p->shape()(i)) / 2.0;
     }
   }
         
            
// Find the world axis for this pixel axis
            
   Int worldAxis = pixelAxisToWorldAxis (cSys, pixelAxis);
            
            
// Find coordinate for this pixel axis
            
   Int coordinate, axisInCoordinate, otherAxisInCoordinate;
   cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);

          
// Convert to world and format depending upon coordinate type

   if (cSys.type(coordinate) == Coordinate::DIRECTION) {

         
// Find name of pixel axis

      String tString = cSys.worldAxisNames()(worldAxis);
      tString.upcase();
         
         
// Loop over list of pixel coordinates and convert to world
         
      for (Int i=0; i<n1; i++) {
         pix(pixelAxis) = pixel(i);

         if (!cSys.toWorld(world,pix)) return False;
         MVAngle mVA(world(pixelAxis));
         
         if (tString.contains("RIGHT ASCENSION")) {
            sWorld(i) = mVA.string(MVAngle::TIME,8);
         } else if (tString.contains("DECLINATION")) {
            sWorld(i) = mVA.string(MVAngle::DIG2,8);
         } else {
            ostrstream oss;
            oss.setf(ios::scientific, ios::floatfield);
            oss.setf(ios::left);
            oss.precision(prec);
            oss << mVA.degree() << ends;
            String temp(oss.str());
            sWorld(i) = temp;   
         }
      }
   } else if (cSys.type(coordinate) == Coordinate::SPECTRAL) {
      for (Int i=0; i<n1; i++) {
         pix(pixelAxis) = pixel(i);
         if (!cSys.toWorld(world,pix)) return False;
         
         ostrstream oss;
         oss.setf(ios::scientific, ios::floatfield);
         oss.setf(ios::left);
         oss.precision(prec);
         oss << world(pixelAxis) << ends;
         String temp(oss.str());
         sWorld(i) = temp;
      }
   } else if (cSys.type(coordinate) == Coordinate::LINEAR) {
      for (Int i=0; i<n1; i++) {
         pix(pixelAxis) = pixel(i);
         if (!cSys.toWorld(world,pix)) return False;

         ostrstream oss;
         oss.setf(ios::scientific, ios::floatfield);
         oss.setf(ios::left);
         oss.precision(prec);
         oss << world(pixelAxis) << ends;
         String temp(oss.str());
         sWorld(i) = temp;
      }
   } else if (cSys.type(coordinate) == Coordinate::STOKES) {
      const StokesCoordinate coord = cSys.stokesCoordinate(coordinate);
      for (Int i=0; i<n1; i++) {
         Stokes::StokesTypes iStokes;
         Int pix = Int(pixel(i));
         if (!coord.toWorld(iStokes, pix)) return False;
         sWorld(i) = Stokes::name(Stokes::type(iStokes));
      }
   }
   return True;
}

template <class T>
Int ImageHistograms<T>::pixelAxisToWorldAxis(const CoordinateSystem& cSys,
                                             const Int& pixelAxis)
//       
// Find the world axis for the given pixel axis
// in a coordinate system
//
{
   Int coordinate, axisInCoordinate;
   cSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
   return cSys.worldAxes(coordinate)(axisInCoordinate);
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
// accumulated from the current chunk of data
//
// Input
//   imageCursorPos  This is the location in the input image for the
//                   start of the current cursor chunk
//   newCounts       The counts to add in
{   

// Location of current histogram in storage image

   IPosition pos = locInHist(imageCursorPos);


// Get current histogram for this location
   
   IPosition stride(pHistImage_p->ndim(),1);
   IPosition shape(pHistImage_p->ndim(),1);
   shape(0) = pHistImage_p->shape()(0);
   Array<Int> slice(shape);   
   pHistImage_p->getSlice (slice, pos, shape, stride);
   

// Update it.

   VectorIterator<Int> it(slice);
   while (!it.pastEnd()) {
      it.vector().ac() += newCounts.ac();
      it.next();
   }
  

// Put it back
 
   pHistImage_p->putSlice (slice, pos, stride);
}



template <class T>
void ImageHistograms<T>::putInStats (const IPosition& imageCursorPos,
                                     const Vector<Double>& newStats)
//
// Increment the statistics storage image.  
//
// Inputs
//   imageCursorPos  This is the location in the input image for the
//                   start of the current cursor chunk
//   newStats        Statistical sums
{     


// Location of current statistics vector in storage image

   IPosition pos = locInStats(imageCursorPos);

// Get statistics slice 
   

   IPosition shape = sliceStatsShape();
   Array<Double> slice(shape);   
   IPosition stride(pStatsImage_p->ndim(),1);
   pStatsImage_p->getSlice (slice, pos, shape, stride);

// Update

   Int lastAxis = pStatsImage_p->ndim()-1;

   IPosition pos2(pStatsImage_p->ndim(),0);  
   pos2(lastAxis) = SUM;
   slice(pos2) += newStats(SUM);
   pos2(lastAxis) = SUMSQ;
   slice(pos2) += newStats(SUMSQ);
   pos2(lastAxis) = NPTS;
   slice(pos2) += newStats(NPTS);


// Put it back

   pStatsImage_p->putSlice (slice, pos, stride);

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
inline IPosition ImageHistograms<T>::sliceMinMaxShape ()
//
// Return the shape of a min/max storage image slice. 
{
//
// min/max on last axis.  

   Int lastAxis =  pMinMaxImage_p->ndim() - 1;
   IPosition shape(pMinMaxImage_p->ndim(),1);
   shape(lastAxis) = pMinMaxImage_p->shape()(lastAxis);
   return shape;
}


template <class T>
inline IPosition ImageHistograms<T>::sliceStatsShape ()
//
// Return the shape of a statistics storage image slice. 
{
//
// stats on last axis.  

   Int lastAxis =  pStatsImage_p->ndim() - 1;
   IPosition shape(pStatsImage_p->ndim(),1);
   shape(lastAxis) = pStatsImage_p->shape()(lastAxis);
   return shape;
}


template <class T>
inline void ImageHistograms<T>::statsAccum (Vector<Double>& stats,
                                            const T& datum)
{
   stats(SUM) += datum;
   stats(SUMSQ) += datum*datum;

// make sure roundoff doesn't accumulate

   Int i = Int(stats(NPTS));
   stats(NPTS) = i + 1;
}



template <class T>
Bool ImageHistograms<T>::writeDispAxesValues (const IPosition& histPos,
                                              const Float& xMin,
                                              const Float& yMax)
{
   
// Fill the string stream with the name and value of each display axis
  
   ostrstream oss;
   Int nDisplayAxes = displayAxes_p.nelements();
   if (nDisplayAxes > 0) {
      Vector<String> sWorld(1);
      Vector<Double> pixel(1);

      for (Int j=0; j<nDisplayAxes; j++) {
         pixel(0) = histPos(j+1);
         if (!pix2World (sWorld, displayAxes_p(j), pixel, 6)) return False;

         Int worldAxis = pixelAxisToWorldAxis(pInImage_p->coordinates(), displayAxes_p(j));
         String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);

         oss << "  " << ImageUtilities::shortAxisName(name)
             << "="  << histPos(j+1)+1 << " (" << sWorld(0) << ")";
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

   return True;
}

