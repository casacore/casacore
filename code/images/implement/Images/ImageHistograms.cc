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
#include <aips/Tables/Table.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>
  
#include <trial/Coordinates.h>  
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/ImageHistograms.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/CopyLattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/TiledStepper.h>

#include <iomanip.h>
#include <stdlib.h>
#include <strstream.h>


// C wrappers for PGPLOT

extern "C" {
#include <cpgplot.h>
};

// enums for min/max and statistics storage images
// stats enum also used for location of items in vector 
// returned by getStats

enum minmax {MIN, MAX, NMINMAX};
enum stats  {SUM, SUMSQ, NPTS, MEAN, SIGMA, VAR, RMS, NSTATS2, NSTATS=NPTS+1};


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
   blc_p.resize(0);
   trc_p.resize(0);
   inc_p.resize(0);
   device_p = "";
   nxy_p.resize(0); 
   range_p.resize(0);

  
   if (setNewImage(imageU)) {

// Cursor axes defaults to all
   
      Vector<Int> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   
// Region defaults to entire image
   
      IPosition blc, trc, inc;
      goodParameterStatus_p = setRegion(blc, trc, inc, False);

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
                        blc_p(other.blc_p),
                        trc_p(other.trc_p),
                        inc_p(other.inc_p),
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
  
// Copy storage images and assign storage image pointers

   copyStorageImages(other);
}      


template <class T>
ImageHistograms<T> &ImageHistograms<T>::operator=(const ImageHistograms<T> &other)
//
// Assignment operator.   Any storage images associated with the object
// being assigned to are deleted first.
//
{
   if (this != &other) {
      
// Assign to image pointer
      
      pInImage_p = other.pInImage_p;
      
// Copy storage images and assign storage image pointers

      if (pHistImage_p != 0) {
         delete pHistImage_p;
         pHistImage_p = 0;
      }
      if (pMinMaxImage_p != 0) {
         delete pMinMaxImage_p;
         pMinMaxImage_p = 0;
      }
      if (pStatsImage_p != 0) {
         delete pStatsImage_p;
         pStatsImage_p = 0;
      }
      copyStorageImages(other);

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
      blc_p = other.blc_p;
      trc_p = other.trc_p;
      inc_p = other.inc_p;
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
// Destructor.  Delete storage images memory
//
{
   if (pHistImage_p != 0) delete pHistImage_p;
   if (pMinMaxImage_p != 0) delete pMinMaxImage_p;
   if (pStatsImage_p != 0) delete pStatsImage_p;

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

   cursorAxes_p.resize(0);   
   cursorAxes_p = axesU;

   if (cursorAxes_p.nelements() == 0) {
   
// User didn't give any axes.  Set them to all.
 
      cursorAxes_p.resize(pInImage_p->ndim());
      for (Int i=0; i<pInImage_p->ndim(); i++) cursorAxes_p(i) = i;
   }


// Signal that we have changed the axes and need a new accumulation image

   needStorageImage_p = True;

   return True;
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
Bool ImageHistograms<T>::setRegion(const IPosition& blcU,
                                   const IPosition& trcU,
                                   const IPosition& incU,
                                   const Bool& listRegion)
//
// Select the region of interest
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;   
      return False;
   }
      
// Check OK
  
   blc_p.resize(0);
   blc_p = blcU;
   trc_p.resize(0);
   trc_p = trcU;
   inc_p.resize(0);
   inc_p = incU;
   ImageUtilities::verifyRegion(blc_p, trc_p, inc_p, pInImage_p->shape());
   if (listRegion) {
      os_p << LogIO::NORMAL << "Selected region : " << blc_p+1 << " to " 
        << trc_p+1 << LogIO::POST;
   }

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

   if (needStorageImage_p) {
      if (!generateStorageImage()) return False;
   }


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
   
   if (needStorageImage_p) {
      if (!generateStorageImage()) return False;      
   }


// Set up iterator to work through histogram storage image line by line
// Since I already set the tile shape to be unity on all but the first 
// axis, just use the LatticeStepper (default) navigator, which will also 
// guarentee the access pattern
  
   IPosition cursorShape(pHistImage_p->ndim(),1);
   cursorShape(0) = pHistImage_p->shape()(0);
   RO_LatticeIterator<Int> histIterator(*pHistImage_p, cursorShape);


// Resize output arrays and setup vector iterators

   counts.resize(pHistImage_p->shape());
   values.resize(pHistImage_p->shape());

   VectorIterator<Float> valuesIterator(values);
   VectorIterator<Float> countsIterator(counts);

   Vector<T> range(2);
   Float linearSum, linearYMax;

// Iterate through histogram storage image
   
   for (histIterator.reset(),valuesIterator.origin(),countsIterator.origin(); 
       !histIterator.atEnd(); histIterator++,valuesIterator.next(),countsIterator.next()) {
 
// Work out the range for this histogram from the min/max storage image

      getMinMax(range, histIterator.position(), False);

// Extract the histogram in the appropriate form

      extractOneHistogram (linearSum, linearYMax, valuesIterator.vector(), 
                           countsIterator.vector(), range, 
                           histIterator.vectorCursor());
   }

   return True;
}




// Private functions



template <class T>
void ImageHistograms<T>::accumulate (const IPosition& imagePosition,
                                     const Array<T>& cursor)
//
// Accumulate the histograms and statistical sums into the
// storage images for this chunk of the image
// 
// Inputs:
//   imagePosition  This is the location in the input image for the
//                  start of the current cursor chunk
//   cursor         Cursor array containing data
//
{             

// Fish out the min and max for this chunk of the data from the min max
// storage image.  

   Vector<T> clip(2);
   getMinMax(clip, imagePosition, True);
         

// Set histogram bin width
   
   const Int nBins = nBins_p;
   const T binWidth = setBinWidth(clip, nBins);

          
// Construct vector iterator to iterate through the cursor
       
   ReadOnlyVectorIterator<T> cursorIt(cursor);
   const Int n1 = cursorIt.vector().nelements();


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

   putInHist(imagePosition, counts);

   
// Update statistics accumulation image

   putInStats(imagePosition, stats);

}


template <class T>
void ImageHistograms<T>::copyStorageImages(const ImageHistograms<T>& other)
//
// Copy historgam strage images from other and assign new pointers for *this
//
{

// Histogram storage image

   if (other.pHistImage_p !=0) {   
      IPosition shape =other.pHistImage_p->shape();
      IPosition tileShape = other.pHistImage_p->tileShape();
      Table myTable = ImageUtilities::setScratchTable(other.pInImage_p->name(),
                            String("ImageHistograms_Hist_"));
      pHistImage_p = new PagedArray<Int>(shape, myTable, tileShape);
      CopyLattice(pHistImage_p->lc(), other.pHistImage_p->lc());
   } else {
      pHistImage_p = 0;
   }


// Min/max storage image

   if (other.pMinMaxImage_p !=0) {   
      IPosition shape = other.pMinMaxImage_p->shape();
      IPosition tileShape = other.pMinMaxImage_p->tileShape();
      Table myTable = ImageUtilities::setScratchTable(other.pInImage_p->name(),
                            String("ImageHistograms_MinMax_"));
      pMinMaxImage_p = new PagedArray<T>(shape, myTable, tileShape);
      CopyLattice(pMinMaxImage_p->lc(), other.pMinMaxImage_p->lc());
   } else {
      pMinMaxImage_p = 0;
   }


// Statistics storage image

   if (other.pStatsImage_p !=0) {   
      IPosition shape =other.pStatsImage_p->shape();
      IPosition tileShape = other.pStatsImage_p->tileShape();
      Table myTable = ImageUtilities::setScratchTable(other.pInImage_p->name(),
                            String("ImageHistograms_Sums_"));
      pStatsImage_p = new PagedArray<Double>(shape, myTable, tileShape);
      CopyLattice(pStatsImage_p->lc(), other.pStatsImage_p->lc());
   } else {
      pStatsImage_p = 0;
   }
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
      
      
// Set up iterator to work through histogram storage image line by line.
// We don't use the TiledStepper because we already set the tile shape sensibly, 
// and this will guarentee the access pattern is row based rather than tile based
 
   IPosition cursorShape(pHistImage_p->ndim(),1);
   cursorShape(0) = pHistImage_p->shape()(0);
   RO_LatticeIterator<Int> histIterator(*pHistImage_p, cursorShape);


// Histogram vectors and other bits and pieces
      
   Vector<Float> counts(pHistImage_p->shape()(0));
   Vector<Float> values(pHistImage_p->shape()(0));
   Vector<T> range(2);
   Vector<Double> stats(NSTATS2);
   Float linearSum, linearYMax;
   Int nPts;   
  

// Iterate through histogram storage image

   for (histIterator.reset(); !histIterator.atEnd(); histIterator++) {
   
// Work out the range for this histogram from the min/max storage image

      getMinMax(range, histIterator.position(), False);

// Work out the mean and sigma from the data that made this histogram
// from the statistics storage image

      getStats (stats, histIterator.position(), False);

// Extract histogram in the form requested for plotting

      extractOneHistogram (linearSum, linearYMax, values, counts, range,
                           histIterator.vectorCursor());

// Display the histogram

      if (!displayOneHistogram (linearSum, linearYMax, histIterator.position(), 
                           range, stats, values, counts)) return False;
 
   }
      
// Close plotting device
 
   cpgend();
   return True;
}
 
 
template <class T>
Bool ImageHistograms<T>::displayOneHistogram (const Float &linearSum,
                                              const Float &linearYMax,
                                              const IPosition& histPos,
                                              const Vector<T>& range,
                                              const Vector<Double> &stats,
                                              const Vector<Float>& values,
                                              const Vector<Float>& counts)

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
   if (doGauss_p && stats(SIGMA)>0) doGauss2 = True;
 

// Set bin width  
      
   const Int nBins = nBins_p;
   const Float binWidth = Float(setBinWidth(range, nBins));
      
    
// Get extrema    
      
   Float xMin = Float(range(0));
   Float xMax = Float(range(1));
   Float yMin = 0.0;
   Float yMax = linearYMax; 
   

// Generate the equivalent Gaussian if desired
      
   Vector<Float> gX, gY;
   Int nGPts;
   Float gMax;
   if (doGauss2) {
      makeGauss (nGPts, gMax, gX, gY, stats(MEAN), stats(SIGMA), linearSum,
                 xMin, xMax, binWidth);   
      yMax = max(yMax, gMax);
   }
      
 
// Stretch extrema by 5%
         
   ImageUtilities::stretchMinMax(xMin, xMax);
   ImageUtilities::stretchMinMax(yMin, yMax);


// Write statistics to a LogIO object

   if (doList_p) {

// List coordinates of display axes for this histogram

      os_p << endl;
      const Int nDisplayAxes = displayAxes_p.nelements();
      if (nDisplayAxes > 0) {   
         Vector<String> sWorld(1);
         Vector<Double> pixels(1);
         const Int oPrec = 6;

         for (Int j=0; j<nDisplayAxes; j++) {
            pixels(0) = Double(locHistInImage(histPos)(j+1));
            if (!ImageUtilities::pixToWorld (sWorld, pInImage_p->coordinates(),
                                        displayAxes_p(j), cursorAxes_p,
                                        blc_p, trc_p, pixels, oPrec)) return False;
            const Int worldAxis = 
              ImageUtilities::pixelAxisToWorldAxis(pInImage_p->coordinates(), displayAxes_p(j));
            const String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);

            os_p <<  ImageUtilities::shortAxisName(name)
                 << "=" << locHistInImage(histPos)(j+1)+1 << " (" << sWorld(0) << ")";
            if (j < nDisplayAxes-1) os_p << ", ";
         }
      }

// Have to convert LogIO object to ostream before can apply 
// the manipulators

      const Int oPrec = 6;
      const Int oWidth = 15;
      os_p.output().fill(' ');
      os_p.output().precision(oPrec);
      os_p.output().setf(ios::scientific, ios::floatfield);
      os_p.output().setf(ios::left, ios::adjustfield);
   
      os_p << endl << "No. binned = ";
      os_p.output() << setw(oWidth) << Int(stats(NPTS)+0.1) << endl;

      os_p << "Sum        = ";
      os_p.output() << setw(oWidth) << stats(SUM) <<   "       Mean     = ";
      os_p.output() << setw(oWidth) << stats(MEAN) << endl;

      os_p << "Variance   = ";
      os_p.output() << setw(oWidth) << stats(VAR);
      if (stats(VAR)> 0.0) {
         os_p << "       Sigma    = ";
         os_p.output() << setw(oWidth) << stats(SIGMA) << endl;
      } else {
         os_p << endl;
      }
      os_p << "Rms        = ";
      os_p.output() << setw(oWidth) << stats(RMS) << endl;
 
      os_p << endl;  
      os_p << "Bin width  = ";
      os_p.output() << setw(oWidth) << binWidth << endl;
      os_p << "Min binned = ";
      os_p.output() << setw(oWidth) << range(0) << "       Max binned = ";
      os_p.output() << setw(oWidth) << range(1) << endl << endl << endl;
      os_p.post();
   }
      
// Generate abcissa and ordinate arrays for plotting
    
   Bool deleteItX;
   const float* px = values.getStorage(deleteItX);
   Bool deleteItY;
   const float* py = counts.getStorage(deleteItY);
    
   
// Plot
         
   cpgpage();
   cpgswin(xMin, xMax, 0.0, yMax);
   cpgbox("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   plotHist (nBins, px, py);
      
   counts.freeStorage(px, deleteItX);
   counts.freeStorage(py, deleteItY);
   
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
void ImageHistograms<T>::extractOneHistogram (Float &linearSum,
                                              Float &linearYMax,
                                              Vector<Float>& values, 
                                              Vector<Float>& counts,
                                              const Vector<T>& range, 
                                              const Vector<Int>& intCounts)

//
// Extract this histogram, convert to the appropriate form
// and return the values and counts
//
{

// Set bin width  
      
   const Int nBins = nBins_p;
   const Float binWidth = Float(setBinWidth(range, nBins));


// Copy histogram counts into output Float array and generate
// values (abcissa) array
 
   Float xx = range(0) + binWidth/2.0;
   linearYMax = -1.0;
   linearSum = 0.0;
   for (Int i=0; i<intCounts.nelements(); i++) {
      values(i) = xx;
      counts(i) = intCounts(i);
      xx += binWidth;
      linearYMax = max(linearYMax,counts(i));
      linearSum += counts(i);
   }
   linearSum = linearSum*binWidth;
 
   
// Make histogram cumulative if desired
      
   if (doCumu_p) makeCumulative (counts, linearYMax, nBins, 1.0);
   
          
// Make histogram logarithmic if desired
         
   if (doLog_p) makeLogarithmic (counts, linearYMax, nBins);

}



template <class T>
void ImageHistograms<T>::fillMinMax (RO_LatticeIterator<T>* imageIterator,
                                     const Int& nVirCursorIter)
               
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
//   nVirCursorIter  NUmber of iterations to get through the virtual cursor
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
      
      IPosition sliceShape = minMaxSliceShape();
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

      const IPosition sliceShape = minMaxSliceShape();
      Array<T> slice(sliceShape);
      const IPosition stride(pMinMaxImage_p->ndim(),1);
      IPosition pos;

      for (imageIterator->reset(); !imageIterator->atEnd(); (*imageIterator)++) {

// Find min and max

         minMax(dMin, dMax, imageIterator->cursor());

// Find location in min/max image of this min/max slice
 
         pos = locInMinMax(imageIterator->position());

// Fill slice 

         if (init) {
            slice(minPos) = dMin;
            slice(maxPos) = dMax;
         } else {
            pMinMaxImage_p->getSlice(slice, pos, sliceShape, stride);
            slice(minPos) = min(slice(minPos), dMin);
            slice(maxPos) = max(slice(maxPos), dMax);
         }

// Put it in

         pMinMaxImage_p->putSlice (slice, pos, stride);            


// Do we need to initialize the min/max axes ?
   
         nIter++;
         if (nIter == nVirCursorIter) {
            init = True;  
            nIter = 0;
         } else {
           init = False;
         }
      }
   }
}





template <class T>
Bool ImageHistograms<T>::generateStorageImage()
//
// Generate the histogram, min/max and statistics storage images.
// Then iterate through the image and fill these storage images.
//
{

// Note to the unwary user 

   os_p << LogIO::NORMAL << "Creating new storage images" << LogIO::POST;
   needStorageImage_p = False;     
   

// Set up input image pixel iterator and navigator.  Do this first so we 
// have the subLattice available to make the storage images

   RO_LatticeIterator<T>* pPixelIterator;
   IPosition latticeShape;
   Int nVirCursorIter;

   if (cursorAxes_p.nelements() == 1) {
      TiledStepper imageNavigator (pInImage_p->shape(),
                                   pInImage_p->niceCursorShape(pInImage_p->maxPixels()),
                                   cursorAxes_p(0));
      
// Apply region and get shape of Lattice that we are iterating through
       
//      imageNavigator.subSection(blc_p, trc_p);
//      latticeShape = imageNavigator.subLatticeShape();
      latticeShape = imageNavigator.latticeShape();
      blc_p = 0;
      trc_p = 0;
               
// Create the image iterator
 
      pPixelIterator = new RO_LatticeIterator<T>(*pInImage_p, imageNavigator);
      nVirCursorIter = 1;
   } else {
 
// Make Navigator with dummy cursor shape
      
      LatticeStepper imageNavigator(pInImage_p->shape(),
                                    IPosition(pInImage_p->ndim(),1));
 
// Apply region and get shape of Lattice that we are iterating through
// Increment ignored for now.
 
      imageNavigator.subSection(blc_p, trc_p);
      latticeShape = imageNavigator.subLatticeShape();
      
// Now that we know the shape of the Lattice, figure out the cursor shape
   
      ostrstream os;
      IPosition cursorShape;
      if (!ImageUtilities::setCursor(nVirCursorIter, cursorShape,
               cursorAxes_p, latticeShape,
               pInImage_p->niceCursorShape(pInImage_p->maxPixels()),
               True, 2, os)) {
         os_p << LogIO::SEVERE << "Invalid cursor axes given" << LogIO::POST;
         return False;
      }
                                           
// Set the cursor shape in the Navigator
      
      imageNavigator.setCursorShape(cursorShape);
 
// Create the image iterator
 
      pPixelIterator = new RO_LatticeIterator<T>(*pInImage_p, imageNavigator);
      
   }
      
 
// Set the display axes vector.
      
   ImageUtilities::setDisplayAxes (displayAxes_p, cursorAxes_p,
                                   latticeShape.nelements());
 

// Delete old histogram storage image

   if (pHistImage_p != 0) delete pHistImage_p;


   {      

// Set storage image shape.  The first axis is the histogram axis
      
      IPosition storeImageShape;  
      ImageUtilities::setStorageImageShape(storeImageShape, False, nBins_p,
                                           displayAxes_p, latticeShape);

// Set tile shape.   The histogram storage image is only accessed by
// vectors along the first axis.   Therefore set the tile shape to be unity
// except for the first axis and equal to the length of the first axis   
// (the number of bins) for the first axis.  

      IPosition tileShape(storeImageShape.nelements(),1);
      tileShape(0) = storeImageShape(0);


// Create new histogram storage image.    The first axis
// is the histogram axis, the higher axes are the display axes


      Table myTable = ImageUtilities::setScratchTable(pInImage_p->name(),
                               String("ImageHistograms_Hist_"));
      pHistImage_p = new PagedArray<Int>(storeImageShape, myTable, tileShape);
      pHistImage_p->set(0);
   }



// Delete old min/max storage image

   if (pMinMaxImage_p != 0) delete pMinMaxImage_p;

   {      

// Set storage image shape.  The last axis is min and max.  The preceding
// ones are the display axes
      
      IPosition storeImageShape;
      ImageUtilities::setStorageImageShape(storeImageShape, True, Int(NMINMAX),
                                           displayAxes_p, latticeShape);

// Set tile shape.   The min/max storage image is accessed by slices along the
// first (length first display axis) and last (length NMINMAX) axes.  Therefore set 
// the tile shape to be unity on all  the other axes.  For the first axis take
// the length of the axis and for the last axis its just NMINMAX (which is just
// 2 or so).

      IPosition tileShape(storeImageShape.nelements(),1);
      tileShape(tileShape.nelements()-1) = storeImageShape(tileShape.nelements()-1);
      if (displayAxes_p.nelements() > 0) tileShape(0) = storeImageShape(0);
      
// Create new min/max storage image.   

      Table myTable = ImageUtilities::setScratchTable(pInImage_p->name(),
                                String("ImageHistograms_MinMax_"));
      pMinMaxImage_p = new PagedArray<T>(storeImageShape, myTable, tileShape);
      pMinMaxImage_p->set(T(0.0)); 
   }



// Delete old statistics storage image

   if (pStatsImage_p != 0) delete pStatsImage_p;

   {

// Set storage image shape.  The first axis is the statistics axis
      
      IPosition storeImageShape;
      ImageUtilities::setStorageImageShape(storeImageShape, False, Int(NSTATS),
                                           displayAxes_p, latticeShape);

// Set tile shape.   The statistics storage image is accessed by slices along the
// first axis (length NSTATS) only. So the tile shape is set to unity on all  
// the other axes.  

      IPosition tileShape(storeImageShape.nelements(),1);
      tileShape(0) = storeImageShape(0);
      
// Create new statistics storage image.   

      Table myTable = ImageUtilities::setScratchTable(pInImage_p->name(),
                                String("ImageHistograms_Sums_"));
      pStatsImage_p = new PagedArray<Double>(storeImageShape, myTable, tileShape);
      pStatsImage_p->set(Double(0.0));
   }



// Before we can proceed, we need to know the min and max of each chunk
// of data that we will want to make a histogram for.  If the user does
// not give us the range to histogram, we must work it out by making an
// extra pass through the data.  

   fillMinMax(pPixelIterator, nVirCursorIter);
   

// Iterate through image and accumulate histogram and statistics images
     
   os_p << LogIO::NORMAL << "Begin accumulating histograms" << LogIO::POST;
   for (pPixelIterator->reset(); !pPixelIterator->atEnd();(*pPixelIterator)++) {
      accumulate (pPixelIterator->position(), pPixelIterator->cursor());
   }

   delete pPixelIterator;
   return True;
}


template <class T> 
void ImageHistograms<T>::getMinMax (Vector<T> &range,
                                    const IPosition &pos,
                                    const Bool &posInImage)
//
// Extract the min/max slice from the given position in either the
// the input image or the histogram storage image.  Tests show
// that the creation of the temporaries in this function cause
// no performance loss.  This function is called once per
// cursor chunk when creating the histograms, and once per
// histogram when reading them from the storage image.
//
// Inputs
//  posInImage   If this is True, then the given position, "pos" is that
//               in the input image.  If it is False, then it is a 
//               position in the histogram image
{
// Set location in min/max image to extract slice

   IPosition minMaxPos(pMinMaxImage_p->ndim());
   Int lastMinMaxAxis = pMinMaxImage_p->ndim() - 1;
   if (posInImage) {
      minMaxPos = locInMinMax(pos);
   } else {
      minMaxPos(lastMinMaxAxis) = 0;
      for (Int i=0; i<lastMinMaxAxis; i++) minMaxPos(i) = pos(i+1);
   }

// Get the slice

   const IPosition minMaxShape = minMaxSliceShape();
   Array<T> minMaxSlice(minMaxShape);   
   pMinMaxImage_p->getSlice (minMaxSlice, minMaxPos, minMaxShape, 
                             IPosition(pMinMaxImage_p->ndim(),1));

// Fill return vector

   minMaxPos = 0;
   minMaxPos(lastMinMaxAxis) = MIN;
   range(0) = minMaxSlice(minMaxPos);
   minMaxPos(lastMinMaxAxis) = MAX;
   range(1) = minMaxSlice(minMaxPos);
}



template <class T> 
void ImageHistograms<T>::getStats (Vector<Double> &stats, 
                                   const IPosition &pos,
                                   const Bool &posInImage)
//
// Extract statistical sums slice from the given position in either the
// the input image or the histogram storage image.  Tests show that the
// creation of the temporaries in this function cause no performance loss.  
// This function is called once per cursor chunk when creating the histograms, 
// and once per histogram when reading them from the storage image.
//
// Inputs
//  posInImage   If this is True, then the given position, "pos" is that
//               in the input image.  If it is False, then it is a 
//               position in the histogram image
// Outputs
//  stats        Must already be correct shape on input
{
// Set position of start of slice

   IPosition statsPos;
   if (posInImage) {
      statsPos = locInStats(pos);
   } else {
      statsPos = pos;
   }

// Get slice

   const IPosition statsShape = statsSliceShape();
   Array<Double> statsSlice(statsShape);
   pStatsImage_p->getSlice (statsSlice, statsPos, statsShape, IPosition(pStatsImage_p->ndim(),1));

// Return values

   statsPos = 0;
   statsPos(0) = SUM;
   stats(SUM) = statsSlice(statsPos);
   statsPos(0) = SUMSQ;
   stats(SUMSQ) = statsSlice(statsPos);
   statsPos(0) = NPTS;
   stats(NPTS) = Int(statsSlice(statsPos) + 0.1);
   
   stats(MEAN) = stats(SUM) / stats(NPTS);
   stats(VAR) = (stats(SUMSQ) - stats(SUM)*stats(SUM)/stats(NPTS))/(stats(NPTS)-1);
   stats(SIGMA) = 0.0;
   if (stats(VAR) > 0.0) stats(SIGMA) = sqrt(stats(VAR));
   stats(RMS) = sqrt(stats(SUMSQ)/stats(NPTS));
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
inline IPosition ImageHistograms<T>::locInHist (const IPosition& imagePosition)
//
// Given a location in the input image, find the start location for a 
// histogram slice in the histogram storage image at this location
//
{
   IPosition pos(pHistImage_p->ndim(),0);
   for (Int j=1; j<pHistImage_p->ndim(); j++) {
      pos(j) = imagePosition(displayAxes_p(j-1)) - blc_p(displayAxes_p(j-1));
   }
   return pos;
}


template <class T>
inline IPosition ImageHistograms<T>::locInMinMax (const IPosition& imagePosition)
//
// Given a location in the input image, find the start location for a 
// min/max slice in the min/max storage image at this location
//
{
   IPosition pos(pMinMaxImage_p->ndim(),0);
   for (Int j=0; j<pMinMaxImage_p->ndim()-1; j++) {
      pos(j) = imagePosition(displayAxes_p(j)) - blc_p(displayAxes_p(j));
   }
   return pos;
}

template <class T>
inline IPosition ImageHistograms<T>::locInStats (const IPosition& imagePosition)
//
// Given a location in the input image, find the start location for a 
// statistics slice in the statistics storage image.  The first axis
// is the statistics axis
//
{
   IPosition pos(pStatsImage_p->ndim(),0);
   for (Int j=1; j<pStatsImage_p->ndim(); j++) {
      pos(j) = imagePosition(displayAxes_p(j-1)) - blc_p(displayAxes_p(j-1));
   }

   return pos;
}

template <class T>
inline IPosition ImageHistograms<T>::locHistInImage(const IPosition& histPos)
//
// Given a location in the histogram storage image, convert those locations on
// the non-histogram axis (the first one) to account for the
// lattice subsectioning
//
{
   IPosition pos(histPos);
   for (Int j=1; j<pos.nelements(); j++)
     pos(j) = histPos(j) + blc_p(displayAxes_p(j-1));
 
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
   
   const Float gaussAmp = dSum * C::_1_sqrt2 * C::_1_sqrtpi / dSigma;
   const Float gWidth = sqrt(8.0*C::ln2) * dSigma;
   const Gaussian1D<Float> gauss(gaussAmp, Float(dMean), gWidth);
 
   
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
   
   const Float scale = dgx / binWidth;
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
                                   const float* const px,
                                   const float* const py)
//
// Inputs
//   n      Number of points
//   px,py  Abcissa and ordinate arrays to plot
//
{ 
   const Float width = (px[1] - px[0]) / 2.0;
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
void ImageHistograms<T>::putInHist (const IPosition& imagePosition,
                                    const Vector<Int>& newCounts)
//
// Update the histogram storage image with the histogram
// accumulated from the current chunk of data
//
// Input
//   imagePosition  This is the location in the input image for the
//                  start of the current cursor chunk
//   newCounts      The counts to add in
{   

// Location of current histogram in storage image

   const IPosition pos = locInHist(imagePosition);

// Get current histogram for this location
   
   const IPosition stride(pHistImage_p->ndim(),1);
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
void ImageHistograms<T>::putInStats (const IPosition& imagePosition,
                                     const Vector<Double>& newStats)
//
// Increment the statistics storage image.   First axis is the
// statistics axis
//
// Inputs
//   imagePosition  This is the location in the input image for the
//                   start of the current cursor chunk
//   newStats        Statistical sums
//
{     

// Get statistics 

   Vector<Double> oldStats(NSTATS2);
   getStats(oldStats, imagePosition, True);

// Update

   const IPosition shape = statsSliceShape();
   Array<Double> slice(shape);   

   IPosition pos(pStatsImage_p->ndim(),0);  
   pos(0) = SUM;
   slice(pos) = oldStats(SUM) + newStats(SUM);
   pos(0) = SUMSQ;
   slice(pos) = oldStats(SUMSQ) + newStats(SUMSQ);
   pos(0) = NPTS;
   slice(pos) = oldStats(NPTS) + newStats(NPTS);


// Put it back

   pStatsImage_p->putSlice (slice, locInStats(imagePosition), 
                            IPosition(pStatsImage_p->ndim(),1));

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
inline IPosition ImageHistograms<T>::minMaxSliceShape ()
//
// Return the shape of a min/max storage image slice. 
{
//
// min/max on last axis.  

   const Int lastAxis =  pMinMaxImage_p->ndim() - 1;
   IPosition shape(pMinMaxImage_p->ndim(),1);
   shape(lastAxis) = pMinMaxImage_p->shape()(lastAxis);
   return shape;
}


template <class T>
inline IPosition ImageHistograms<T>::statsSliceShape ()
//
// Return the shape of a statistics storage image slice. 
// The first axis is the statistics axis
{
   IPosition shape(pStatsImage_p->ndim(),1);
   shape(0) = pStatsImage_p->shape()(0);
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
   const Int nDisplayAxes = displayAxes_p.nelements();
   if (nDisplayAxes > 0) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);

      for (Int j=0; j<nDisplayAxes; j++) {
         pixels(0) = Double(locHistInImage(histPos)(j+1));
         if (!ImageUtilities::pixToWorld (sWorld, pInImage_p->coordinates(),
                                     displayAxes_p(j), cursorAxes_p,
                                     blc_p, trc_p, pixels, 6)) return False;
         Int worldAxis = 
           ImageUtilities::pixelAxisToWorldAxis(pInImage_p->coordinates(), displayAxes_p(j));
         String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);

         oss << "  " << ImageUtilities::shortAxisName(name)
             << "="  << locHistInImage(histPos)(j+1) + 1 << " (" << sWorld(0) << ")";
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

   return False;
}

