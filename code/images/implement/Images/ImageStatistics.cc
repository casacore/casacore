//# ImageStatistics.cc: generate statistics from an image
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
#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/DataType.h>

#include <trial/Coordinates.h>  
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/ImageStatistics.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/CopyLattice.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/TiledStepper.h>
#include <trial/Tasking/ProgressMeter.h>


#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <strstream.h>


// C wrappers for PGPLOT

extern "C" {
#include <cpgplot.h>
};

// Public functions

template <class T>
ImageStatistics<T>::ImageStatistics (const ImageInterface<T>& imageU,
                                     LogIO& osU) : os_p(osU)
//
// Constructor. 
//
{
   goodParameterStatus_p = True;
   pStoreImage_p = 0;
   needStorageImage_p = True;

   noInclude_p = True;
   noExclude_p = True;
   doList_p = False;

   doneSomeGoodPoints_p = False;
   someGoodPointsValue_p = False;

   nxy_p.resize(0);
   statsToPlot_p.resize(0);
   range_p.resize(0);
   device_p = "";
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blc_p.resize(0);
   trc_p.resize(0);
   inc_p.resize(0);
  
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
ImageStatistics<T>::ImageStatistics(const ImageStatistics<T> &other) 
                      : os_p(other.os_p), 
                        pInImage_p(other.pInImage_p),
                        cursorAxes_p(other.cursorAxes_p),
                        displayAxes_p(other.displayAxes_p), 
                        nxy_p(other.nxy_p),
                        statsToPlot_p(other.statsToPlot_p), 
                        range_p(other.range_p),
                        device_p(other.device_p), 
                        doList_p(other.doList_p),
                        noInclude_p(other.noInclude_p), 
                        noExclude_p(other.noExclude_p),
                        goodParameterStatus_p(other.goodParameterStatus_p),
                        needStorageImage_p(other.needStorageImage_p),
                        doneSomeGoodPoints_p(other.doneSomeGoodPoints_p),
                        someGoodPointsValue_p(other.someGoodPointsValue_p),
                        minPos_p(other.minPos_p), 
                        maxPos_p(other.maxPos_p),
                        blc_p(other.blc_p),
                        trc_p(other.trc_p),
                        inc_p(other.inc_p)
//
// Copy constructor.  Storage image is copied.
//
{
// Copy storage image and assigg storage image pointer

   copyStorageImage (other);

}


template <class T>
ImageStatistics<T> &ImageStatistics<T>::operator=(const ImageStatistics<T> &other)
//
// Assignment operator.  Any storage image associated with the object
// being assigned to is deleted first
//
{
   if (this != &other) {

// Assign to image pointer

      pInImage_p = other.pInImage_p;

// Copy storage image and assign storage image pointer

      if (pStoreImage_p != 0) {
         delete pStoreImage_p;
         pStoreImage_p = 0;
      }
      copyStorageImage(other);

// Do the rest

      os_p = other.os_p;
      cursorAxes_p = other.cursorAxes_p;
      displayAxes_p = other.displayAxes_p; 
      nxy_p = other.nxy_p;
      statsToPlot_p = other.statsToPlot_p; 
      range_p = other.range_p;
      device_p = other.device_p; 
      doList_p = other.doList_p;
      goodParameterStatus_p = other.goodParameterStatus_p;
      needStorageImage_p = other.needStorageImage_p;
      doneSomeGoodPoints_p = other.doneSomeGoodPoints_p;
      someGoodPointsValue_p = other.someGoodPointsValue_p;
      noInclude_p = other.noInclude_p; 
      noExclude_p = other.noExclude_p;
      minPos_p = other.minPos_p; 
      maxPos_p = other.maxPos_p;
      blc_p = other.blc_p;
      trc_p = other.trc_p;
      inc_p = other.inc_p;
   }
   return *this;
}


 

template <class T>
ImageStatistics<T>::~ImageStatistics()
//
// Destructor.  Delete storage image memory
//
{
   if (pStoreImage_p != 0) delete pStoreImage_p;
}


template <class T>
Bool ImageStatistics<T>::setAxes (const Vector<Int>& axesU)
//
// This function sets the cursor axes and the display axes
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }


// Assign cursor axes.

   cursorAxes_p.resize(0);
   cursorAxes_p = axesU;

   if (cursorAxes_p.nelements() == 0) {
   
// User didn't give any axes.  Set them to all.
       
      cursorAxes_p.resize(pInImage_p->ndim());
      for (Int i=0; i<pInImage_p->ndim(); i++) cursorAxes_p(i) = i;
   } else {
      for (Int i=0; i<cursorAxes_p.nelements(); i++) {
         if (cursorAxes_p(i) < 0 || cursorAxes_p(i) > pInImage_p->ndim()-1) {
            os_p << LogIO::SEVERE << "Invalid cursor axes" << LogIO::POST;
            return False;
         }
      }
   }

// Signal that we have changed the axes and need a new accumulaiton
// image

   needStorageImage_p = True;

   return True;
}


template <class T>
Bool ImageStatistics<T>::setInExCludeRange(const Vector<Double>& includeU,
                                           const Vector<Double>& excludeU)
//
// Assign the desired exclude range
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }
      
   ostrstream os;
   if (!ImageUtilities::setIncludeExclude(range_p, noInclude_p, noExclude_p,
                                          includeU, excludeU, os)) {
      os_p << LogIO::SEVERE << "Invalid pixel in/exclusion range" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }


// Signal that we have changed the pixel range and need a new accumulaiton
// image
    
   needStorageImage_p = True;

   return True;
}


template <class T>
Bool ImageStatistics<T>::setList (const Bool& doList)
//
// See if user wants to list statistics as well as plot them
//
{

   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }
      
   doList_p = doList;

   return True;
} 


template <class T>
Bool ImageStatistics<T>::setPlotting(const Vector<Int>& statsToPlotU,
                                     const String& deviceU,
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


// Make sure requested statistics are valid

   statsToPlot_p.resize(0);
   statsToPlot_p = statsToPlotU;
   for (Int i=0; i<statsToPlot_p.nelements(); i++) {
      if (statsToPlot_p(i) < 0 || statsToPlot_p(i) > NSTATS-1) {
         os_p << LogIO::SEVERE << "Invalid statistic requested for display" 
              << endl << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
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

// Set mean and sigma if no statistics requested

   if (!device_p.empty() && statsToPlot_p.nelements()==0) {
      statsToPlot_p.resize(2);
      statsToPlot_p(0) = MEAN;
      statsToPlot_p(1) = SIGMA;
   }

   return True;
}



template <class T>
Bool ImageStatistics<T>::setNewImage(const ImageInterface<T>& image)
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
      os_p << LogIO::SEVERE << "Statistics can only be evaluated from images of type : " 
           << TpFloat << " and " << TpDouble << endl << LogIO::POST;
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
Bool ImageStatistics<T>::setRegion(const IPosition& blcU,
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
      os_p << LogIO::NORMAL << "Selected region : " << blc_p+1<< " to "
        << trc_p+1 << LogIO::POST;
   }

   return True;
}





template <class T>
Bool ImageStatistics<T>::display()
// 
// This function displays (plotting and listing) the requested
// statistics as a function of the display axes
//
{
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl 
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False;
   }

// Do we have anything to do

   if (!doList_p && device_p.empty()) {
     os_p << LogIO::NORMAL
          << "There is nothing to plot or list" << endl << LogIO::POST;
     return True;
   }


// Open plotting device if required and set up some plotting things

   if (!device_p.empty()) {
      if(cpgbeg(0, device_p.chars(), nxy_p(0), nxy_p(1)) != 1) {
         os_p << LogIO::SEVERE << endl << "Couldn't open display device" 
              << endl << LogIO::POST;
         return False;
      }
      cpgask(1);
      cpgsch (1.2);
      cpgsvp(0.1,0.9,0.1,0.9);
   }


// Generate storage image if required

   if (needStorageImage_p) {
      if (!generateStorageImage()) return False;
   }


// If we don't have any display axes just summarise the image statistics

   if (displayAxes_p.nelements() == 0) {
     summStats ();
     return True;
   }


// Size of plotting abcissa axis

   const Int n1 = pStoreImage_p->shape()(0);


// Allocate ordinate arrays for plotting and listing

   Matrix<Float> ord(n1,NSTATS);


// Iterate through storage image by planes (first and last axis of storage image)
// Specify which axes are the matrix  axes so that we can discard other
// degenerate axes with the matrixCursor function.   n1 is only 
// constrained to be n1 >= 1

   IPosition cursorShape(pStoreImage_p->ndim(),1);
   cursorShape(0) = pStoreImage_p->shape()(0);
   cursorShape(pStoreImage_p->ndim()-1) = pStoreImage_p->shape()(pStoreImage_p->ndim()-1);

   IPosition matrixAxes(2);
   matrixAxes(0) = 0; matrixAxes(1) = pStoreImage_p->ndim()-1;

   LatticeStepper stepper(pStoreImage_p->shape(), cursorShape,
                          matrixAxes, IPosition::makeAxisPath(pStoreImage_p->ndim()));
   RO_LatticeIterator<Double> pixelIterator(*pStoreImage_p, stepper);

   for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {

// Convert accumulations to  mean, sigma, and rms. Make sure we do all 
// calculations with double precision values. 
 
      Matrix<Double>  matrix(pixelIterator.matrixCursor());   // Reference semantics
      for (Int i=0; i<n1; i++) {
         const Int nPts = Int(matrix(i,NPTS)+0.1);
         if (nPts > 0) {
            ord(i,MEAN) = matrix(i,SUM) / matrix(i,NPTS);
            Double tmp = 0.0;
            if (nPts > 1) tmp = (matrix(i,SUMSQ) - (matrix(i,SUM)*matrix(i,SUM)/matrix(i,NPTS))) / 
                                (matrix(i,NPTS)-1);
            if (tmp > 0.0) {
               ord(i,SIGMA) = sqrt(tmp);
            } else {
               ord(i,SIGMA) = 0.0;
            }
            ord(i,RMS) = sqrt(matrix(i,SUMSQ)/matrix(i,NPTS));
         }
      }


// Extract the direct (NPTS, SUM etc) values from the cursor matrix into the plot matrix
// There is no easy way to do this other than as I have

      Int j;
      for (i=0; i<NACCUM; i++) {
         for (j=0; j<n1; j++) ord(j,i) = matrix(j,i);
      }


// Plot statistics

      if (!device_p.empty()) {
        if (!plotStats (pixelIterator.position(), ord)) return False;
      }


// List statistics

      if (doList_p) {
         if (!listStats(pixelIterator.position(), ord)) return False;
      }
    }


// Finish up

   if (!device_p.empty()) cpgend();
   return True;
}


template <class T>
Bool ImageStatistics<T>::getNPts(Array<T>& stats)
// 
// This function retrieves the NPTS statistics from the
// accumulation image
//
{
// Check class status

   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl 
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False;
   }

// Retrieve storage array statistic

   Array<Double> slice;
   if (retrieveStorageStatistic (slice, Int(NPTS))) copyArray(stats, slice);

   return True;
}


template <class T>
Bool ImageStatistics<T>::getSum(Array<T>& stats)
// 
// This function retrieves the SUM statistics from the
// accumulation image
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }


// Retrieve storage array statistic

   Array<Double> slice;
   if (retrieveStorageStatistic (slice, Int(SUM))) copyArray (stats, slice);

   return True;
}


template <class T>
Bool ImageStatistics<T>::getSumSquared (Array<T>& stats)
// 
// This function retrieves the SUMSQ statistics from the
// accumulation image
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl 
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }


// Retrieve storage array statistic

   Array<Double> slice;
   if (retrieveStorageStatistic (slice, Int(SUMSQ))) copyArray (stats, slice);

   return True;
}

template <class T>
Bool ImageStatistics<T>::getMin(Array<T>& stats)
// 
// This function retrieves the MIN statistics from the
// accumulation image
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
    }

// Retrieve storage array statistic

   Array<Double> slice;
   if (retrieveStorageStatistic (slice, Int(MIN))) copyArray (stats, slice);

   return True;
}


template <class T>
Bool ImageStatistics<T>::getMax(Array<T>& stats)
// 
// This function retrieves the MAX statistics from the
// accumulation image
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }


// Retrieve storage array statistic

   Array<Double> slice;
   if (retrieveStorageStatistic (slice, Int(MAX))) copyArray (stats, slice);

   return True;
}



template <class T>
Bool ImageStatistics<T>::getMean(Array<T>& stats)
// 
// This function calculates the MEAN statistics from the
// accumulation image
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }

// Do it

   if (!calculateStatistic(stats, Int(MEAN))) return False;

   return True;
}


template <class T>
Bool ImageStatistics<T>::getSigma(Array<T>& stats)
// 
// This function calculates the SIGMA statistics from the
// accumulation image
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }

// Do it

   if (!calculateStatistic(stats, Int(SIGMA))) return False;

   return True;
}


template <class T>
Bool ImageStatistics<T>::getRms(Array<T>& stats)
// 
// This function calculates the RMS statistics from the
// accumulation image
//
{
 
// Check class status
 
   if (!goodParameterStatus_p) {
     os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }

// Do it

   if (!calculateStatistic(stats, Int(RMS))) return False;

   return True;
}



// Private functions

template <class T>
void ImageStatistics<T>::accumulate (Int& nIter,
                                     const Int& nVirCursorIter,
                                     const IPosition& cursorPos,
                                     const Array<T>& cursor)
//
// Main work routine which takes the data in the current cursor and
// accumulate it into a storage or accumulation image at the appropriate
// locations.  Thus it collapses all data on the cursor axes and accumulates
// as a function of the display axes.
//
// Inputs:
//   nVirCursorIter  NUmber of iterations to get through the virtual cursor
//   cursorPos       Location in input image of start of cursor
//   cursor          Cursor array
// Input/output:
//   nIter           The number of iterations through the image so far. It is
//                   reset to zero every time we do enough iterations to have
//                   worked through the entire virtual cursor (specified by the user's
//                   axes) so that we know we have to reinit min and max.  
{


// Iterate through cursor array by vectors as its faster than
// doing it pixel by pixel

   ReadOnlyVectorIterator<T> posIt(cursor);
   IPosition tMinPos(cursor.ndim()), tMaxPos(cursor.ndim());
   Double sMin, sMax;
   Double sum = 0;
   Double sumsq = 0;
   Double tmp;
   Int nPts = 0;
   const Int n1 = posIt.vector().nelements();
   Int i;


// Iterate; {} destroys iterator when done with it

   {
      Bool init = True;
      while (!posIt.pastEnd()) {
         Int orig = posIt.vector().origin()(0);

         if (!noInclude_p) {

// Inclusion range

            for (i=0; i<n1; i++) {
               tmp = posIt.vector()(i+orig);
               if (tmp >= range_p(0) && tmp <= range_p(1)) {
                  if (init) {
                     sMin = tmp + 1;
                     sMax = tmp - 1;
                     init = False;
                  }
                  accumulate2 (sum, sumsq, sMin, sMax, nPts, tMinPos, tMaxPos,
                               i, posIt.pos(), tmp);
               }
            }
         } else if (!noExclude_p) {

// Exclusion range

            for (i=0; i<n1; i++) {
               tmp = posIt.vector()(i+orig);
               if (tmp < range_p(0) || tmp > range_p(1)) {
                  if (init) {                
                     sMin = tmp + 1;
                     sMax = tmp - 1;
                     init = False;
                  }
                  accumulate2 (sum, sumsq, sMin, sMax, nPts, tMinPos, tMaxPos,
                               i, posIt.pos(), tmp);
               }
            }
         } else {

// All data accepted
 
            if (init) {
               sMin = posIt.vector()(orig) + 1;
               sMax = posIt.vector()(orig) - 1;
               init = False;
            }
            for (i=0; i<n1; i++) accumulate2 (sum, sumsq, sMin, sMax, nPts, tMinPos,
                                              tMaxPos, i, posIt.pos(), 
                                              posIt.vector()(i+orig));
         }
         posIt.next();
      }
   }


// Extract statistics slice

   const IPosition start = locInStats(cursorPos);
   const IPosition shape = statsSliceShape();
   Array<Double> slice(shape);
   pStoreImage_p->getSlice(slice, start, shape, 
                           IPosition(pStoreImage_p->ndim(),1));

// Update slice

   IPosition pos(start.nelements(),0);
   const Int lastAxis = pStoreImage_p->ndim() - 1;

   pos(lastAxis) = NPTS;
   slice(pos) += nPts;

   pos(lastAxis) = SUM;
   slice(pos) += sum;

   pos(lastAxis) = SUMSQ;
   slice(pos) += sumsq;

   pos(lastAxis) = MIN;
   if (nIter == 0 || (nIter !=0 && sMin < slice(pos))) {
     slice(pos) = sMin;
     minPos_p = cursorPos;
     for (i=0; i<cursor.ndim(); i++) minPos_p(i) += tMinPos(i);
   }
   pos(lastAxis) = MAX;
   if (nIter == 0 || (nIter !=0 && sMax > slice(pos))) {
     slice(pos) = sMax;
     maxPos_p = cursorPos;
     for (i=0; i<cursor.ndim(); i++) maxPos_p(i) += tMaxPos(i);
   }


// Put it back

   pStoreImage_p->putSlice(slice, start);


// Work out if it is time to initialize the min and max accumulators
// This algorithm will only work if the virtual cursor is worked through
// before the next one is encountered, or the virtual cursor is the 
// whole image

   nIter++;
   if (nIter == nVirCursorIter) nIter = 0;

}


template <class T>
void ImageStatistics<T>::accumulate2 (Double& sum,
                                      Double& sumsq,  
                                      Double& sMin, 
                                      Double& sMax,
                                      Int& nPts,
                                      IPosition& tMinPos,
                                      IPosition& tMaxPos,
                                      const Int& i,
                                      const IPosition& pos,
                                      const Double& datum)
{
   nPts++;
   sum += datum;
   sumsq += datum*datum;

   if (datum > sMax) {
      sMax = datum;
      tMaxPos = pos;
      tMaxPos(0) += i;
   }
   if (datum < sMin) {
      sMin = datum;
      tMinPos = pos;
      tMinPos(0) += i;
   }
}


template <class T>
Bool ImageStatistics<T>::calculateStatistic (Array<T>& slice, const Int& ISTAT)
//
// Calculate desired statistic from storage image and return in array
//
// Input/output:
//  slice      The statistics are returned in this array.  WIll be of zero 
//             size on output if there were no good points.
//
{

// Rezize slice to nothing first

   slice.resize(IPosition(0,0));


// Generate storage image if required

   if (needStorageImage_p) {
      if (!generateStorageImage()) return False;
   }


// Return asap if no good points

   if (!someGoodPoints()) return True;


// Retrieve nPts statistics

   Array<Double> nPts;
   retrieveStorageStatistic (nPts, Int(NPTS));
   ReadOnlyVectorIterator<Double> nPtsIt(nPts);
   const Int n1 = nPtsIt.vector().nelements();

// Setup

   slice.resize(nPts.shape());
   slice = 0.0;
   VectorIterator<T> sliceIt(slice);

// Do it

   Int n;
   Double tmp;
   if (ISTAT == MEAN) {
       Array<Double> sum;
       retrieveStorageStatistic (sum, Int(SUM));
       ReadOnlyVectorIterator<Double> sumIt(sum);

       while (!nPtsIt.pastEnd()) {
          for (Int i=0; i<n1; i++) {
             n = Int(nPtsIt.vector()(i)+0.1);
             if(n > 0) sliceIt.vector()(i) = sumIt.vector()(i) / n;
          }
          nPtsIt.next();
          sumIt.next();
          sliceIt.next();
       }
    } else if (ISTAT == SIGMA) {
       Array<Double> sum;
       retrieveStorageStatistic (sum, Int(SUM));
       ReadOnlyVectorIterator<Double> sumIt(sum);

       Array<Double> sumSq;
       retrieveStorageStatistic (sumSq, Int(SUMSQ));
       ReadOnlyVectorIterator<Double> sumSqIt(sumSq);

       while (!nPtsIt.pastEnd()) {
          for (Int i=0; i<n1; i++) {
             n = Int(nPtsIt.vector()(i) + 0.1);
             if(n > 1) {
                tmp = (sumSqIt.vector()(i) -
                   (sumIt.vector()(i)*sumIt.vector()(i)/n)) / (n-1);
                if (tmp > 0.0) sliceIt.vector()(i) = sqrt(tmp);
             }
          }
          nPtsIt.next();
          sumIt.next();
          sumSqIt.next();
          sliceIt.next();
       }
    } else if (ISTAT == RMS) {
       Array<Double> sumSq;
       retrieveStorageStatistic (sumSq, Int(SUMSQ));
       ReadOnlyVectorIterator<Double> sumSqIt(sumSq);

       while (!nPtsIt.pastEnd()) {
          for (Int i=0; i<n1; i++) {
             n = Int(nPtsIt.vector()(i) + 0.1);
             if(n > 0) sliceIt.vector()(i) = sqrt(sumSqIt.vector()(i)/n);
          }
          nPtsIt.next();
          sumSqIt.next();
          sliceIt.next();
       }
    } else {
       os_p << LogIO::SEVERE << "Internal error" << endl << LogIO::POST;
       slice.resize(IPosition(0,0));
    }

   return True;
}



template <class T>
void ImageStatistics<T>::copyArray (Array<T>&outSlice, const Array<Double>& inSlice)
//
// Copy the values in the input array to the output array
//
{

// Resize output

   outSlice.resize(inSlice.shape());


   if (inSlice.shape().product() == 1) {

// Take easy path for degenerate array

      IPosition posIn(inSlice.ndim(),0);
      IPosition posOut(outSlice.ndim(),0);
      outSlice(posOut) = inSlice(posIn);

   } else {

// Set up to iterate by vectors 

      ReadOnlyVectorIterator<Double> inSliceIt(inSlice);
      VectorIterator<T> outSliceIt(outSlice);
      const Int n1 = inSliceIt.vector().nelements();
      Int i;

// Iterate and copy

      while (!inSliceIt.pastEnd()) {

// We have to copy them element by element because
// the types may be different (e.g. Double -> Float)

         for (i=0; i<n1; i++) outSliceIt.vector()(i) = inSliceIt.vector()(i);

         inSliceIt.next();
         outSliceIt.next();
      }
   }
}


template <class T>
void ImageStatistics<T>::copyStorageImage(const ImageStatistics<T> &other) 
//
// Copy storage image from other and assign new pointer for *this
{
   if (other.pStoreImage_p !=0) {   
      IPosition shape =other.pStoreImage_p->shape();
      IPosition tileShape = other.pStoreImage_p->tileShape();
      Table myTable = ImageUtilities::setScratchTable(other.pInImage_p->name(),
                            String("ImageStatistics_Sums_"));
      pStoreImage_p = new PagedArray<Double>(shape, myTable, tileShape);
      CopyLattice(pStoreImage_p->lc(), other.pStoreImage_p->lc());
   } else {
      pStoreImage_p = 0;
   }
}


template <class T>
Bool ImageStatistics<T>::findNextDatum (Int& iFound, 
                                        const Int& n,
                                        const float* const pn, 
                                        const Int& iStart,
                                        const Bool& findGood)
//
// Find the next good (or bad) point in an array.
// A good point in the array has a non-zero value.
//
// Inputs:
//  n        Number of points in array
//  pn       Pointer to array containing counts
//  iStart   The index of the first point to consider
//  findGood If True look for next good point.  
//           If False look for next bad point
// Outputs:
//  iFound   Index of found point
//  Bool     False if didn't find another valid datum
{
   for (Int i=iStart; i<n; i++) {
      if ( (findGood && pn[i]>0.5) ||
           (!findGood && pn[i]<0.5) ) {
        iFound = i;
        return True;
      }
   }
   return False;
}


template <class T>
Bool ImageStatistics<T>::findNextLabel (String& subLabel,
                                        Int& iLab,
                                        String& label)
//
// Find the next comma delimitered sublabel in a string
//
// Inputs:
//  label    The label
//  iLab     The number of the current sublabel (starts at 0)
// Output 
//  subLabel The next sublabel
//  Bool     False if there were no more sublabels
//
{
   static Int iStart=0;
   if (iLab==0) iStart = 0;
   Int iLen = label.length();

   if (iStart >= iLen) {
      subLabel = "";
      return False;
   }

   for (Int i=iStart; i<iLen; i++) {
      String c(label.elem(i));
      if (c == ",") {
         Int n = i - iStart;
         subLabel = String(label(iStart,n));
         iStart = i + 1;        
         return True;

      }
   }


// substring extends to end of string

   Int n = iLen - iStart;
   subLabel = String(label(iStart,n));
   iStart = iLen;
   return True;
}
      


template <class T>
Bool ImageStatistics<T>::generateStorageImage()
//
// Iterate through the image and generate the accumulation image
{
// Delete old storage image

   if (pStoreImage_p != 0) delete pStoreImage_p;

// Warn user

   os_p << LogIO::NORMAL << "Creating new storage image" << endl << LogIO::POST;


// Set up input image pixel iterator and navigator.  Do this first so we 
// have the subLattice available to make the storage image

   RO_LatticeIterator<T>* pPixelIterator;
   IPosition latticeShape;
   Int nVirCursorIter;

   if (cursorAxes_p.nelements() == 1) {

// Set TiledStepper for profiles.  There is no hangover possible with this navigator

      TiledStepper imageNavigator (pInImage_p->shape(), 
                                   pInImage_p->niceCursorShape(pInImage_p->maxPixels()),
                                   cursorAxes_p(0));

// Apply region and get shape of Lattice that we are iterating through

      imageNavigator.subSection(blc_p, trc_p);
      latticeShape = imageNavigator.subLatticeShape();

// Create the image iterator

      pPixelIterator = new RO_LatticeIterator<T>(*pInImage_p, imageNavigator);
      nVirCursorIter = 1;
   } else {

// Make Navigator with dummy cursor shape.  Use resize hangover policy

      LatticeStepper imageNavigator(pInImage_p->shape(), 
                                    IPosition(pInImage_p->ndim(),1),
                                    LatticeStepper::RESIZE);

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

   {

// Work out dimensions of storage image

      IPosition storeImageShape;
      ImageUtilities::setStorageImageShape(storeImageShape, True, Int(NACCUM),
                                           displayAxes_p, latticeShape);

// The storage image is accessed by vectors along the last (statistics) axis (when 
// filling), the first axis (first display axis; when plotting and listing) and N-1
// dimensional slices (when retrieving statistics for the user).  However, the  
// latter will be used less often than the rest so optimize the tile shape ignoring 
// it. Since the tile size is small on the last axis, this won't impose much of a 
// penalty when accessing by first axis slices.  There will be no hangover.

      IPosition tileShape(storeImageShape.nelements(),1);
      tileShape(0) = storeImageShape(0);
      tileShape(tileShape.nelements()-1) = storeImageShape(storeImageShape.nelements()-1);
  
      Table myTable = ImageUtilities::setScratchTable(pInImage_p->name(),
                                                String("ImageStatistics_Sums_"));
      pStoreImage_p = new PagedArray<Double>(storeImageShape, myTable, tileShape);
      pStoreImage_p->set(Double(0.0));

   }


// Set up min/max location variables

   minPos_p.resize(latticeShape.nelements());
   maxPos_p.resize(latticeShape.nelements());


// Iterate through image and accumulate statistical sums

   Double meterMax = Double(latticeShape.product())/Double(pPixelIterator->cursor().shape().product());
   ProgressMeter clock(0.0, meterMax, "Generate Storage Image", "Accumulation iterations", 
                       "", "", True, max(1,Int(meterMax/20)));
   Double meterValue = 0.0;

   os_p << LogIO::NORMAL << "Begin accumulation" << LogIO::POST;
   Int nIter =0;
   for (pPixelIterator->reset(); !pPixelIterator->atEnd(); (*pPixelIterator)++) {

// Note that the cursor position reflects the full image (i.e. there
// are no subsectioning offsets)

      accumulate (nIter, nVirCursorIter, pPixelIterator->position(), 
                  pPixelIterator->cursor());

      meterValue += 1.0;
      clock.update(meterValue);
   }  
   needStorageImage_p = False;     
   doneSomeGoodPoints_p = False;

   delete pPixelIterator;
   return True;
}


   

template <class T>
void ImageStatistics<T>::lineSegments (Int& nSeg,
                                       Vector<Int>& start,
                                       Vector<Int>& nPts,
                                       const float* const pn,
                                       const Int& n)
//
// Examine an array and determine how many segments
// of good points it consists of.    A good point
// occurs if the array value is greater than zero.
//
// Inputs:
//   pn    The array
//   n     Number of points in array
// Outputs:
//   nSeg  Number of segments
//   start Indices of start of each segment
//   nPts  Number of points in segment
//
{
   Bool none;
   Bool finish = False;
   nSeg = 0;
   Int iGood, iBad;
   start.resize(n);
   nPts.resize(n);

   for (Int i=0; !finish;) {
      if (!findNextDatum (iGood, n, pn, i, True)) {
         finish = True;
      } else {
         nSeg++;
         start(nSeg-1) = iGood;

         if (!findNextDatum (iBad, n, pn, iGood, False)) {
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

typedef Matrix<Float> gpp_MatrixFloat;
template <class T>
Bool ImageStatistics<T>::listStats (const IPosition& dPos,
                                    const gpp_MatrixFloat& stats)
//
// List the statistics for this row to the logger
//
// Inputs:
//   dPos    The location of the start of the cursor in the
//           storage image for this row
//   stats   Statistics matrix
// Outputs:
//   Bool    Indicates coordinate transformations failed
//
{
   os_p << endl;

// Get number of statistics and display axes

   const Int nDisplayAxes = displayAxes_p.nelements();
   const Int nStatsAxes = cursorAxes_p.nelements();

// Set up the manipulators. We list the number of points as an integer so find
// out how big the field width needs to be.  Min of 6 so label fits.

   Int nMax = 0;
   const Int n1 = stats.shape()(0);
   for (Int j=0; j<n1; j++) nMax = max(nMax, Int(stats.column(NPTS)(j)+0.1));
   const Int logNMax = Int(log10(Double(nMax))) + 2;
   const Int oIWidth = max(5, logNMax);
   const Int oDWidth = 15;
   const Int oSWidth = 7;

// Have to convert LogIO object to ostream before can apply
// the manipulators

   os_p.output().fill(' '); 
   os_p.output().setf(ios::scientific, ios::floatfield);
   os_p.output().setf(ios::left, ios::adjustfield);


// Write the pixel and world coordinate of the higher order display axes to the logger

   if (nDisplayAxes > 1) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);

      for (j=1; j<nDisplayAxes; j++) {
         Int worldAxis = 
            pInImage_p->coordinates().pixelAxisToWorldAxis(displayAxes_p(j));
         String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);
         pixels(0) = Double(locInImage(dPos)(j));

         if (!ImageUtilities::pixToWorld (sWorld, pInImage_p->coordinates(),
                                     displayAxes_p(j), cursorAxes_p,
                                     blc_p, trc_p, pixels, -1)) return False;
         os_p <<  ImageUtilities::shortAxisName(name)
              << " = " << locInImage(dPos)(j)+1 << " (" << sWorld(0) << ")";
         if (j < nDisplayAxes-1) os_p << ", ";
      }
   }


// Find the width of the field into which we are going to write the coordinate value
// of the first display axis.  Do this by formatting a dummy value.

   Vector<String> sWorld(1);
   Vector<Double> pixels(1);
   pixels(0) = 1.0;
   ImageUtilities::pixToWorld(sWorld, pInImage_p->coordinates(),
                              displayAxes_p(0), cursorAxes_p, 
                              blc_p, trc_p, pixels, -1);
   String cName = ImageUtilities::shortAxisName(pInImage_p->coordinates().worldAxisNames()(displayAxes_p(0)));
   Int oCWidth = max(cName.length(), sWorld(0).length()) + 1;
   
// Write headers

   os_p << endl;
   Int len0;
   if (nStatsAxes == 1) {
      os_p << "Profile ";
      len0 = 8;
   }
   else if (nStatsAxes == 2) {
      os_p << "Plane ";
      len0 = 6;
   }
   else if (nStatsAxes == 3) {
      os_p << "Cube ";
      len0 = 5;
   }
   else {
      os_p << "Hyper-cube ";
      len0 = 11;
   }


   os_p.output() << setw(oCWidth) << cName;
   os_p.output() << setw(oIWidth) << "Npts";
   os_p.output() << setw(oDWidth) << "Sum";
   os_p.output() << setw(oDWidth) << "Mean"; 
   os_p.output() << setw(oDWidth) << "Rms";
   os_p.output() << setw(oDWidth) << "Sigma";
   os_p.output() << setw(oDWidth) << "Minimum";
   os_p.output() << setw(oDWidth) << "Maximum" << endl;


// Convert pixel coordinates Vector of the first display axis to world coordinates

   sWorld.resize(n1);
   pixels.resize(n1);
   for (j=0; j<n1; j++) pixels(j) = Double(j)+blc_p(displayAxes_p(0));
   if (!ImageUtilities::pixToWorld(sWorld, pInImage_p->coordinates(),
                              displayAxes_p(0), cursorAxes_p, 
                              blc_p, trc_p, pixels, -1)) return False;


// Write statistics to logger

   for (j=0; j<n1; j++) {
      os_p.output() << setw(len0)     << j+blc_p(displayAxes_p(0))+1;
      os_p.output() << setw(oCWidth)   << sWorld(j);
      os_p.output() << setw(oIWidth)   << Int(stats.column(NPTS)(j)+0.1);

      if (Int(stats.column(NPTS)(j)+0.1) > 0) {
         os_p.output() << setw(oDWidth)   << stats.column(SUM)(j);
         os_p.output() << setw(oDWidth)   << stats.column(MEAN)(j);
         os_p.output() << setw(oDWidth)   << stats.column(RMS)(j);
         os_p.output() << setw(oDWidth)   << stats.column(SIGMA)(j);
         os_p.output() << setw(oDWidth)   << stats.column(MIN)(j);
         os_p.output() << setw(oDWidth)   << stats.column(MAX)(j);
      }
      os_p << endl;
   }
   os_p.post();

   return True;
}


template <class T>
IPosition ImageStatistics<T>::locInImage(const IPosition& storagePosition)
//
// Given a location in the storage image, convert those locations on
// the non-statistics axis (the last one) to account for the 
// lattice subsectioning
//
{  
   IPosition pos(storagePosition);
   for (Int j=0; j<pos.nelements()-1; j++) {
     pos(j) = storagePosition(j) + blc_p(displayAxes_p(j));
   }
   return pos;
}

template <class T>
IPosition ImageStatistics<T>::locInStats(const IPosition& imagePosition)
//
// Given a location in the input image, find the corresponding start 
// location for a statistics slice in the statistics storage image
//
{  
   IPosition pos(pStoreImage_p->ndim(),0);
   for (Int j=0; j<pStoreImage_p->ndim()-1; j++) {
     pos(j) = imagePosition(displayAxes_p(j)) - blc_p(displayAxes_p(j));
   }


   return pos;
}


template <class T>
Bool ImageStatistics<T>::plotStats (const IPosition& dPos,
                                    const Matrix<Float>& stats)
//
// Plot the desired statistics.  
//
// Inputs:
//   dPos    The location of the start of the cursor in the 
//           storage image for this line 
//   stats   Statistics matrix
//
{
// Work out what we are plotting

   const Bool doMean  = ToBool(ImageUtilities::inVector(Int(MEAN), statsToPlot_p) != -1);
   const Bool doSigma = ToBool(ImageUtilities::inVector(Int(SIGMA), statsToPlot_p) != -1);
   const Bool doRms   = ToBool(ImageUtilities::inVector(Int(RMS), statsToPlot_p) != -1);
   const Bool doSum   = ToBool(ImageUtilities::inVector(Int(SUM), statsToPlot_p) != -1);
   const Bool doSumSq = ToBool(ImageUtilities::inVector(Int(SUMSQ), statsToPlot_p) != -1);
   const Bool doMin   = ToBool(ImageUtilities::inVector(Int(MIN), statsToPlot_p) != -1);
   const Bool doMax   = ToBool(ImageUtilities::inVector(Int(MAX), statsToPlot_p) != -1);
   const Bool doNPts  = ToBool(ImageUtilities::inVector(Int(NPTS), statsToPlot_p) != -1);

   Bool none;
   Bool first = True;
   Int nL = 0;
   Int nR = 0;

// Generate abcissa

   const Int n1 = stats.shape()(0);
   Vector<Float> abc(n1);
   for (Int j=0; j<n1; j++) abc(j) = j+1;


// Find extrema.  Return if there were no valid points to plot

   Float yMin, yMax, xMin, xMax, yLMin, yLMax, yRMin, yRMax;

   minMax(none, xMin, xMax, abc, stats.column(NPTS), n1);
   if (none) return True;

// Left hand y axis

   if (doMean) {
      minMax(none, yLMin, yLMax, stats.column(MEAN), stats.column(NPTS), n1);
      first = False;
      nL++;
   }
   if (doSum) {
      minMax(none, yMin, yMax, stats.column(SUM), stats.column(NPTS), n1);
      if (first) {
         yLMin = yMin;
         yLMax = yMax;
      } else {
         yLMin = min(yLMin,yMin);
         yLMax = max(yLMax,yMax);
      }
      first = False;
      nL++;
   }
   if (doSumSq) {
      minMax(none, yMin, yMax, stats.column(SUMSQ), stats.column(NPTS), n1);
      if (first) {
         yLMin = yMin;
         yLMax = yMax;
      } else {
         yLMin = min(yLMin,yMin);
         yLMax = max(yLMax,yMax);
      }
      first = False;
      nL++;
   }
   if (doMin) {
      minMax(none, yMin, yMax, stats.column(MIN), stats.column(NPTS), n1);
      if (first) {
         yLMin = yMin;
         yLMax = yMax;
      } else {
         yLMin = min(yLMin,yMin);
         yLMax = max(yLMax,yMax);
      }
      first = False;
      nL++;
   }
   if (doMax) {
      minMax(none, yMin, yMax, stats.column(MAX), stats.column(NPTS), n1);
      if (first) {
         yLMin = yMin;
         yLMax = yMax;
      } else {
         yLMin = min(yLMin,yMin);
         yLMax = max(yLMax,yMax);
      }
      first = False;
      nL++;
   }
   if (doNPts) {
      minMax(none, yMin, yMax, stats.column(NPTS), stats.column(NPTS), n1);
      if (first) {
         yLMin = yMin;
         yLMax = yMax;
      } else {
         yLMin = min(yLMin,yMin);
         yLMax = max(yLMax,yMax);
      }
      first = False;
      nL++;
   }


// Right hand y axis

   first = True;
   if (doSigma) {
      minMax(none, yRMin, yRMax, stats.column(SIGMA), stats.column(NPTS), n1);
      first = False;
      nR++;
   }
   if (doRms) {
      minMax(none, yMin, yMax, stats.column(RMS), stats.column(NPTS), n1);
      if (first) {
         yRMin = yMin;
         yRMax = yMax;
      } else {
         yRMin = min(yRMin,yMin);
         yRMax = max(yRMax,yMax);
      }
      nR++;
   }

   ImageUtilities::stretchMinMax(xMin, xMax); 
   if (nL>0) ImageUtilities::stretchMinMax(yLMin, yLMax);
   if (nR>0) ImageUtilities::stretchMinMax(yRMin, yRMax);


// Set axis labels.

   String xLabel = pInImage_p->coordinates().
     worldAxisNames()(displayAxes_p(0)) + " (pixels)";
   String yLLabel = "";
   String yRLabel = "";

   Int nLLabs = 0;
   if (nL>0) {
      if (doMean) {
         yLLabel += "Mean,";
         nLLabs++;
      }
      if (doSum) {
         yLLabel += "Sum,";
         nLLabs++;
      }
      if (doSumSq) {
         yLLabel += "Sum Squared,";
         nLLabs++;
      }
      if (doMin) {
         yLLabel += "Min,";
         nLLabs++;
      }
      if (doMax) {
         yLLabel += "Max,";
         nLLabs++;
      }
      if (doNPts) {
         yLLabel += "nPts,";
         nLLabs++;
      }
      yLLabel.del(Int(yLLabel.length()-1),1);
   }

   Int nRLabs = 0;
   if (nR>0) {
      if (doSigma) {
         yRLabel += "Sigma,";
         nRLabs++;
      }
      if (doRms) {
         yRLabel += "Rms,";
         nRLabs++;
      }
      yRLabel.del(Int(yRLabel.length()-1),1);
   }
   
// Do plots

   Vector<Int> lCols(nL);
   Vector<Int> rCols(nR);
   int ls = 0;
   int i = -1;
   Bool initColours = True;
   cpgpage();

   if (nL>0) {
      cpgswin(xMin, xMax, yLMin, yLMax);
      if (nR>0) 
         cpgbox("BCNST", 0.0, 0, "BNST", 0.0, 0);
      else
         cpgbox("BCNST", 0.0, 0, "BCNST", 0.0, 0);
      cpglab(xLabel.chars(), "", "");

      if (doMean) {
         if (++ls > 5) ls = 1;
         cpgsls (ls);

         lCols(++i) = niceColour (initColours);
         cpgsci (lCols(i));

         multiPlot ( n1, abc, stats.column(MEAN), stats.column(NPTS));
      }
      if (doSum) {
         if (++ls > 5) ls = 1;
         cpgsls (ls);

         lCols(++i) = niceColour (initColours);
         cpgsci (lCols(i));

         multiPlot (n1, abc, stats.column(SUM), stats.column(NPTS));
      }
      if (doSumSq) {
         if (++ls > 5) ls = 1;
         cpgsls (ls);

         lCols(++i) = niceColour (initColours);
         cpgsci (lCols(i));

         multiPlot (n1, abc, stats.column(SUMSQ), stats.column(NPTS));
      }
      if (doMin) {
         if (++ls > 5) ls = 1;
         cpgsls (ls);

         lCols(++i) = niceColour (initColours);
         cpgsci (lCols(i));

         multiPlot (n1, abc, stats.column(MIN), stats.column(NPTS));
      }
      if (doMax) {
         if (++ls > 5) ls = 1;
         cpgsls (ls);

         lCols(++i) = niceColour (initColours);
         cpgsci (lCols(i));

         multiPlot (n1, abc, stats.column(MAX), stats.column(NPTS));
      }
      if (doNPts) {
         if (++ls > 5) ls = 1;
         cpgsls (ls);

         lCols(++i) = niceColour (initColours);
         cpgsci (lCols(i));

         multiPlot (n1, abc, stats.column(NPTS), stats.column(NPTS));
      }

// Y label

      multiColourYLabel ("L", yLLabel, lCols, nLLabs);
   }
   cpgsls (1);
   cpgsci (1);


   i = -1;
   if (nR>0) {
      cpgswin(xMin, xMax, yRMin, yRMax);
      cpgsci (1); 
      if (nL>0) 
         cpgbox("", 0.0, 0, "CMST", 0.0, 0);
      else {
         cpgbox("BCNST", 0.0, 0, "BCMST", 0.0, 0);
         cpglab(xLabel.chars(), "", "");
      }

      if (doSigma) {
         if (++ls > 5) ls = 1;
         cpgsls(ls);

         rCols(++i) = niceColour (initColours);
         cpgsci (rCols(i));

         multiPlot (n1, abc, stats.column(SIGMA), stats.column(NPTS));
      }
      if (doRms) {
         if (++ls > 5) ls = 1;
         cpgsls(ls);

         rCols(++i) = niceColour (initColours);
         cpgsci (rCols(i));

         multiPlot (n1, abc, stats.column(RMS), stats.column(NPTS));
      }

// Y label

      multiColourYLabel ("R", yRLabel, rCols, nRLabs);

   }
   cpgsls(1);
   cpgsci (1);


// Write values of other display axes on plot
   
   ostrstream oss;
   if (displayAxes_p.nelements() > 1) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);

      for (Int j=1; j<displayAxes_p.nelements(); j++) {
         Int worldAxis = 
            pInImage_p->coordinates().pixelAxisToWorldAxis(displayAxes_p(j));
         String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);
         pixels(0) = Double(locInImage(dPos)(j));

         if (!ImageUtilities::pixToWorld (sWorld, pInImage_p->coordinates(),
                                     displayAxes_p(j), cursorAxes_p,
                                     blc_p, trc_p, pixels, -1)) return False;
         oss << "  " << ImageUtilities::shortAxisName(name)
             << "=" << locInImage(dPos)(j)+1 << " (" << sWorld(0) << ")";
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
      float my;
      if (nR > 0) 
         my = yRMax + 0.5*dy;
      else
         my = yLMax + 0.5*dy;

      int tbg;
      cpgqtbg(&tbg);
      cpgstbg(0);
      cpgptxt (mx, my, 0.0, 0.0, tLabel);
      cpgstbg(tbg);
   }

   return True;
}


template <class T>
void ImageStatistics<T>::multiColourYLabel (const String& LRLoc, 
                                            String& label,      
                                            const Vector<Int>& colours,
                                            const Int& nLabs)
//
// Draw each Y-axis sublabel in a string with a different colour
//
{
// Get attributes

   float x1, x2, y1, y2;
   cpgqwin (&x1, &x2, &y1, &y2);
   int sci;
   cpgqci (&sci);


// Find y-location of start of string as fraction of window

   float xb[4], yb[4];
   cpgqtxt (0.0, 0.0, 90.0, 0.0, label.chars(), xb, yb);
   float dy = yb[2]-yb[0];
   float yLoc = abs(0.5*(y2-y1-dy)/(y2-y1));


// Loop over number of sub-labels and write them in colour

   String subLabel;
   float just = 0.0;
   float disp = 2.5;
   if (LRLoc == "R") disp = 3.0;
   for (Int iLab=0; iLab<nLabs; iLab++) {

// Fish out next sub label

      if (!findNextLabel (subLabel, iLab, label)) {
         cpgsci (sci);
         return;
      } 
      
       
// Write it

      if (iLab < nLabs-1) subLabel = subLabel + ",";
      if (iLab > 0) subLabel.prepend(" ");
      cpgsci (colours(iLab));
      cpgmtxt (LRLoc.chars(), disp, yLoc, just, subLabel.chars());


// Increment y location.  pgqtxt won't count a leading blank so
// replace it with a character for length counting purposes. These
// stupid string classes make this very hard work.

      String s2;
      if (iLab > 0) {
         String s(subLabel(1,subLabel.length()-1));
         s2 = "x" + s;
      } else
         s2 = subLabel;
      cpgqtxt (0.0, 0.0, 90.0, 0.0, s2.chars(), xb, yb);
      dy = abs((yb[2]-yb[0])/(y2-y1));
      yLoc += dy;
   }                       

// Set colour back to what it was

   cpgsci (sci);
   return;
}




template <class T>
void ImageStatistics<T>::multiPlot (const Int& n1,
                                    const Vector<Float>& x,
                                    const Vector<Float>& y,
                                    const Vector<Float>& n)
//
// Plot an array which may have some blanked points.
// Thus we plot it in segments
//
// Inputs:
//  n1       Number of points (good and bad)
//  x,y,n    Abcissa, ordinate, and "masking" array
//           (if > 0 plot it)
{

// Get pointers

   Bool deleteX, deleteY, deleteN;
   const float* px = x.getStorage(deleteX);
   const float* py = y.getStorage(deleteY);
   const float* pn = n.getStorage(deleteN);

// Find number of segments in th<is array

   Int nSeg = 0;
   Vector<Int> start;
   Vector<Int> nPts;
   lineSegments (nSeg, start, nPts, pn,  n1);

// Loop over segments and plot them

   for (Int i=0; i<nSeg; i++) {
      Int ip = start(i);
      if (nPts(i) == 1) {
         cpgpt (1, &px[ip], &py[ip], 1);
      } else {
         cpgline (nPts(i), &px[ip], &py[ip]);
      }
   }

// Delete memory

   x.freeStorage(px, deleteX);
   y.freeStorage(py, deleteY);
   n.freeStorage(pn, deleteN);

}


template <class T>
void ImageStatistics<T>::minMax (Bool& none,
                                 Float& dMin, 
                                 Float& dMax,  
                                 const Vector<Float>& d,
                                 const Vector<Float>& n,
                                 const Int& n1)
//
// Inputs:
//   d   Vector to find min and max of
//   n   Vector which gives the number of points
//       that were used to compute the value in pt.  If zero,
//       that means there were no valid points and we don't
//       want to consider the corresponding pd[i] value
//   n1  Number of points in d and n arrays
// Outputs:
//   none       No valid points in array
//   dMin,DMax  Min and max of array pd

{
   Bool init = True;
   none = True;

   for (Int i=0; i<n1; i++) {
     if (n(i) > 0.5) {
        if (init) {
           dMin = d(i);
           dMax = d(i);
           init = False;
        } else {
           dMin = min(dMin, d(i));
           dMax = max(dMax, d(i));
        }
        none = False;
     }
   }
}


template <class T>
Int ImageStatistics<T>::niceColour (Bool& initColours)
{
   static colourIndex = 1;
   if (initColours) {
      colourIndex = 1;
      initColours = False;
   }
      
   colourIndex++;
   if (colourIndex == 4 || colourIndex == 14) colourIndex++;
   return colourIndex;
}


template <class T>
Bool ImageStatistics<T>::retrieveStorageStatistic(Array<Double>& slice, const Int& ISTAT)
//
// Retrieve values from accumulation image
//
// Input
//   ISTAT        Points at location of desired statistic in 
//                accumulation image (last axis)
// Input/output
//   slice        The statistics; should be of zero size on input
//
{

// Resize output array to nothing first

    slice.resize(IPosition(0,0));


// Generate storage image if required

   if (needStorageImage_p) {
      if (!generateStorageImage()) return False;
   }


// Were there some good points ?  

   const Int nDim = pStoreImage_p->ndim();
   if (someGoodPoints()) {


// Get desired statistic slice. Discard degenerate axes (requires
// empty array on input)

      IPosition sliceShape(pStoreImage_p->shape());
      sliceShape(nDim-1) = 1;

      IPosition pos(nDim,0);
      pos(nDim-1) = ISTAT;

      pStoreImage_p->getSlice(slice, pos, sliceShape, 
                              IPosition(nDim,1), True);
      return True;
   } else {
      return False;
   }

}




template <class T>
Bool ImageStatistics<T>::someGoodPoints ()
//
// If any of the locations in the statistics storage array contain
// some valid points return true straight away.  DOn't bother
// looking again if we already looked !
//
{

   if (doneSomeGoodPoints_p) {
      return someGoodPointsValue_p;
   } else {
      doneSomeGoodPoints_p = True;

      if (pStoreImage_p->ndim() == 1) {

// If storage image only 1D take cheap way out. Can't invoke
// retrieveStorageStatistic or we will be stuck in a time loop

         const IPosition shape = statsSliceShape();
         Array<Double> stats(shape);
         IPosition pos(1,0);

         pStoreImage_p->getSlice(stats, pos, shape, IPosition(1,1));
   
         pos(0) = NPTS;
         if (Int(stats(pos)+0.1) > 0) {
            someGoodPointsValue_p = True;
         } else {
            someGoodPointsValue_p = False;
         }
         return someGoodPointsValue_p;
      } else {


// Iterate through storage image by planes (first and last axis of storage image)
// Specify which axes are the matrix  axes so that we can discard other
// degenerate axes with the matrixCursor function.   n1 is only 
// constrained to be n1 >= 1

         IPosition cursorShape(pStoreImage_p->ndim(),1);
         const Int n1 = pStoreImage_p->shape()(0);
         cursorShape(0) = n1;
         cursorShape(pStoreImage_p->ndim()-1) = pStoreImage_p->shape()(pStoreImage_p->ndim()-1);

         IPosition matrixAxes(2);
         matrixAxes(0) = 0; matrixAxes(1) = pStoreImage_p->ndim()-1;

         LatticeStepper stepper(pStoreImage_p->shape(), cursorShape,
                                matrixAxes, IPosition::makeAxisPath(pStoreImage_p->ndim()));
         RO_LatticeIterator<Double> pixelIterator(*pStoreImage_p, stepper);

         for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {
            for (Int i=0; i<n1; i++) {
               if (Int(pixelIterator.matrixCursor()(i,NPTS)+0.1) > 0) {
                  someGoodPointsValue_p = True;
                  return someGoodPointsValue_p;
               }
            }
         }
         someGoodPointsValue_p = False;
         return someGoodPointsValue_p;
      }
   }
}


template <class T>
IPosition ImageStatistics<T>::statsSliceShape ()
// 
// Return the shape of a slice from the statistics storage
// image for a single spatial location.  The last axis
// is the statistics axis
{
   IPosition shape(pStoreImage_p->ndim(),1);
   shape(pStoreImage_p->ndim()-1) = 
      pStoreImage_p->shape()(pStoreImage_p->ndim()-1);
   return shape;
}



template <class T>
void ImageStatistics<T>::summStats ()
// 
// List the summary of the statistics to the logger in the
// case that the statistics storage image is 1D only
//
{
// Fish out statistics with a slice

   const IPosition shape = statsSliceShape();
   Array<Double> stats(shape);
   pStoreImage_p->getSlice (stats, IPosition(1,0), shape, IPosition(1,1));

   IPosition pos(1);
   pos(0) = NPTS;
   Int nPts = Int(stats(pos)+0.1);
   pos(0) = SUM;

   Double sum = stats(pos);
   pos(0) = SUMSQ;
            
   Double sumSq = stats(pos);
                         
   Double mean = 0.0;
   if (nPts > 0) mean = sum/nPts;
            
   Double var = 0.0;
   if (nPts > 1) var = (sumSq - sum*sum/nPts)/(nPts-1);

   Double rms = 0.0;
   if (sumSq > 0 && nPts > 0) rms = sqrt(sumSq/nPts);

   pos(0) = MIN;
   Double dMin = stats(pos);
   pos(0) = MAX;
   Double dMax = stats(pos);

// Have to convert LogIO object to ostream before can apply
// the manipulators

   const Int oPrec = 6;
   const Int oWidth = 15;
   os_p.output().fill(' '); 
   os_p.output().precision(oPrec);
   os_p.output().setf(ios::scientific, ios::floatfield);
   os_p.output().setf(ios::left, ios::adjustfield);

   os_p << endl; 
   if ( Int(nPts+0.1) > 0) {
      os_p << "No pts   = ";
      os_p.output() << setw(oWidth) << Int(nPts+0.1) << endl;
      os_p << "Sum      = ";
      os_p.output() << setw(oWidth) << sum << "       Mean     = ";
      os_p.output() << setw(oWidth) << mean << endl;
      os_p << "Variance = ";
      os_p.output() << setw(oWidth) << var;
      if (var > 0.0) {
         os_p << "       Sigma    = ";
         os_p.output() << setw(oWidth) << sqrt(var) << endl;
      } else {
         os_p << endl;
      }
      os_p << "Rms      = ";
      os_p.output() << setw(oWidth) << rms << endl;
      os_p << endl;
      os_p << "Minimum value at " << minPos_p+1 << " = ";
      os_p.output()  << setw(oWidth) << dMin << endl;
      os_p << "Maximum value at " << maxPos_p+1 << " = ";
      os_p.output()  << setw(oWidth) << dMax << endl;   
   } else
      os_p << "No valid points found " << endl;

   os_p << endl << endl;
   os_p.post();
}

 

