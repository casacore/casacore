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

#include <trial/Images/ImageStatistics.h>

#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Arrays/Matrix.h>
#include <trial/Coordinates.h>  
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/MaskedImage.h>
#include <trial/Images/SubImage.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LCBox.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/QMath.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/DataType.h>


#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <strstream.h>



// ImageStatistics

// Public functions

template <class T>
ImageStatistics<T>::ImageStatistics (const MaskedImage<T>& imageU,
                                     LogIO& osU, 
                                     Bool showProgressU)
// 
// Constructor
//
: os_p(osU),
  pStoreImage_p(0),
  doList_p(False),
  noInclude_p(True),
  noExclude_p(True),
  goodParameterStatus_p(True),
  needStorageImage_p(True),
  doneSomeGoodPoints_p(False),
  someGoodPointsValue_p(False),
  haveLogger_p(True),
  showProgress_p(showProgressU),
  fixedMinMax_p(False)
{
   nxy_p.resize(0);
   statsToPlot_p.resize(0);   
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);

   if (setNewImage(imageU)) {

// Cursor axes defaults to all

      Vector<Int> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      goodParameterStatus_p = False;
   }
}


template <class T>
ImageStatistics<T>::ImageStatistics (const MaskedImage<T>& imageU,
                                     Bool showProgressU)  
// 
// Constructor
//
: pStoreImage_p(0),
  doList_p(False),
  noInclude_p(True),
  noExclude_p(True),
  goodParameterStatus_p(True),
  needStorageImage_p(True),
  doneSomeGoodPoints_p(False),
  someGoodPointsValue_p(False),
  haveLogger_p(False),
  showProgress_p(showProgressU),
  fixedMinMax_p(False)
{
   nxy_p.resize(0);
   statsToPlot_p.resize(0);
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);

   if (setNewImage(imageU)) {

// Cursor axes defaults to all

      Vector<Int> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
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
                        plotter_p(other.plotter_p), 
                        doList_p(other.doList_p),
                        noInclude_p(other.noInclude_p), 
                        noExclude_p(other.noExclude_p),
                        goodParameterStatus_p(other.goodParameterStatus_p),
                        doneSomeGoodPoints_p(other.doneSomeGoodPoints_p),
                        someGoodPointsValue_p(other.someGoodPointsValue_p),
                        haveLogger_p(other.haveLogger_p),
                        showProgress_p(other.showProgress_p),
                        fixedMinMax_p(other.fixedMinMax_p),
                        minPos_p(other.minPos_p), 
                        maxPos_p(other.maxPos_p),
                        blcParent_p(other.blcParent_p)
//
// Copy constructor.  Storage image is not copied.
//
{
   pStoreImage_p = 0;
   needStorageImage_p = True;
}


template <class T>
ImageStatistics<T> &ImageStatistics<T>::operator=(const ImageStatistics<T> &other)
//
// Assignment operator.  Storage image is not copied
//
{
   if (this != &other) {

// Assign to image pointer

      pInImage_p = other.pInImage_p;


// Delete storage image 

      if (pStoreImage_p != 0) {
         delete pStoreImage_p;
         pStoreImage_p = 0;
      }
      needStorageImage_p = True;


// Do the rest

      os_p = other.os_p;
      cursorAxes_p = other.cursorAxes_p;
      displayAxes_p = other.displayAxes_p; 
      nxy_p = other.nxy_p;
      statsToPlot_p = other.statsToPlot_p; 
      range_p = other.range_p;
      plotter_p = other.plotter_p; 
      doList_p = other.doList_p;
      noInclude_p = other.noInclude_p; 
      noExclude_p = other.noExclude_p;
      goodParameterStatus_p = other.goodParameterStatus_p;

      doneSomeGoodPoints_p = other.doneSomeGoodPoints_p;
      someGoodPointsValue_p = other.someGoodPointsValue_p;
      haveLogger_p = other.haveLogger_p;
      showProgress_p = other.showProgress_p;
      fixedMinMax_p = other.fixedMinMax_p;
      minPos_p = other.minPos_p; 
      maxPos_p = other.maxPos_p;
      blcParent_p = other.blcParent_p;
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
      if (haveLogger_p) os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

// Save current cursor axes

   Vector<Int> saveAxes(cursorAxes_p.copy());

// Assign cursor axes.

   cursorAxes_p.resize(0);
   cursorAxes_p = axesU;

   if (cursorAxes_p.nelements() == 0) {
   
// User didn't give any axes.  Set them to all.
       
      cursorAxes_p.resize(pInImage_p->ndim());
      for (uInt i=0; i<pInImage_p->ndim(); i++) cursorAxes_p(i) = i;
   } else {
      for (uInt i=0; i<cursorAxes_p.nelements(); i++) {
         if (cursorAxes_p(i) < 0 || cursorAxes_p(i) > Int(pInImage_p->ndim()-1)) {
            if (haveLogger_p) os_p << LogIO::SEVERE << "Invalid cursor axes" << LogIO::POST;
            return False;
         }
      }
   }


// Signal that we have changed the axes and need a new accumulation
// image

   if (saveAxes.nelements() != cursorAxes_p.nelements() ||
       !allEQ(saveAxes.ac(), cursorAxes_p.ac())) needStorageImage_p = True;


// Set the display axes vector.  We also do this in ::generateStorageImage
// but it is possible the user will want to see the display axes
// via the public function "displayAxes" before any real work is done
// so poke this in here too.

   ImageUtilities::setDisplayAxes (displayAxes_p, cursorAxes_p, 
                                   pInImage_p->ndim());


   return True;
}


template <class T>
Bool ImageStatistics<T>::setInExCludeRange(const Vector<T>& includeU,
                                           const Vector<T>& excludeU,
                                           Bool setMinMaxToIncludeU)
//
// Assign the desired exclude range
//
{
   if (!goodParameterStatus_p) {
      if (haveLogger_p) os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

// Save current ranges

   Vector<T> saveRange(range_p.copy());
   Bool saveFixedMinMax = fixedMinMax_p;

// Check
      
   ostrstream os;
   if (!setIncludeExclude(range_p, noInclude_p, noExclude_p,
                          includeU, excludeU, os)) {
      if (haveLogger_p) os_p << LogIO::SEVERE << "Invalid pixel in/exclusion range" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }

// Can't have fixed min and max with an exclusion range

   fixedMinMax_p = setMinMaxToIncludeU;
   if (!noExclude_p && fixedMinMax_p) {
      if (haveLogger_p) {
         os_p << LogIO::SEVERE 
              << "Can't have a fixed min and max with an exclusion range" 
              << LogIO::POST;
      }
      goodParameterStatus_p = False;
      return False;
   }

// Can only have fixed min and max range if user gives it

   if (noInclude_p) fixedMinMax_p = False;


// Signal that we have changed the pixel range and need a new accumulation
// image
   
   if ( (saveFixedMinMax != fixedMinMax_p) ||
        (saveRange.nelements() != range_p.nelements()) ||
        (!allEQ(saveRange.ac(), range_p.ac())) ) needStorageImage_p = True;    

   return True;
}

template <class T>
Bool ImageStatistics<T>::setList (const Bool& doList)
//
// See if user wants to list statistics as well as plot them
//
{

   if (!goodParameterStatus_p) {
      if (haveLogger_p) os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }
      
   doList_p = doList;

   return True;
} 


template <class T>
Bool ImageStatistics<T>::setPlotting(const Vector<Int>& statsToPlotU,
                                     const PGPlotter& plotter,
                                     const Vector<Int>& nxyU)
//
// Assign the desired PGPLOT device name and number
// of subplots
//
{     
   if (!goodParameterStatus_p) {
      if (haveLogger_p) os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

// Is plotter attached ?

   plotter_p = plotter;
   if (!plotter_p.isAttached()) {
      if (haveLogger_p) {
         os_p << LogIO::SEVERE << "Plotter is not attached" << LogIO::POST; 
      }
      goodParameterStatus_p = False;
      return False;
   }

// Make sure requested statistics are valid

   statsToPlot_p.resize(0);
   statsToPlot_p = statsToPlotU;
   for (uInt i=0; i<statsToPlot_p.nelements(); i++) {
      if (statsToPlot_p(i) < 0 || statsToPlot_p(i) > NSTATS-1) {
         if (haveLogger_p) os_p << LogIO::SEVERE << "Invalid statistic requested for display" 
              << endl << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
   }   
   

// Plotting device and subplots.  nxy_p is set to [1,1] if zero length
 
   nxy_p.resize(0);
   nxy_p = nxyU;
   ostrstream os;
   if (!ImageUtilities::setNxy(nxy_p, os)) {
      if (haveLogger_p) os_p << LogIO::SEVERE << "Invalid number of subplots" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }


// Set mean and sigma if no statistics requested

   if (statsToPlot_p.nelements()==0) {
      statsToPlot_p.resize(2);
      statsToPlot_p(0) = MEAN;
      statsToPlot_p(1) = SIGMA;
   }

   return True;
}



template <class T>
Bool ImageStatistics<T>::setNewImage(const MaskedImage<T>& image)
//    
// Assign pointer to image
//
{ 
   if (!goodParameterStatus_p) {
      if (haveLogger_p) os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }
  
   pInImage_p = &image;

   T *dummy = 0;
   DataType imageType = whatType(dummy);
   if (imageType !=TpFloat && imageType != TpDouble) {
      if (haveLogger_p) os_p << LogIO::SEVERE << "Statistics can only be evaluated from images of type : " 
           << TpFloat << " and " << TpDouble << endl << LogIO::POST;
      goodParameterStatus_p = False;
      pInImage_p = 0;
      return False;
   }


// This is the location of the input SubImage in
// the parent Image

   blcParent_p = pInImage_p->region().slicer().start();

// Signal that we have changed the image and need a new accumulation
// image

   needStorageImage_p = True;
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
     if (haveLogger_p) os_p << LogIO::SEVERE << endl 
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False;
   }

// Do we have anything to do

   if (!doList_p && !plotter_p.isAttached()) {
     if (haveLogger_p) os_p << LogIO::NORMAL
          << "There is nothing to plot or list" << endl << LogIO::POST;
     return True;
   }


// Open plotting device if required and set up some plotting things

   if (plotter_p.isAttached()) {
       plotter_p.subp(nxy_p(0), nxy_p(1));
       plotter_p.ask(True);
       plotter_p.sch (1.2);
       plotter_p.svp(0.1,0.9,0.1,0.9);
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

   const uInt n1 = pStoreImage_p->shape()(0);


// Allocate ordinate arrays for plotting and listing.  Try to preserve
// the true Type of the data as long as we can.  Eventually, for 
// plotting we have to make it real valued

   Matrix<T> ord(n1,NSTATS);


// Iterate through storage image by planes (first and last axis of storage image)
// Specify which axes are the matrix  axes so that we can discard other
// degenerate axes with the matrixCursor function.   n1 is only 
// constrained to be n1 >= 1

   IPosition cursorShape(pStoreImage_p->ndim(),1);
   cursorShape(0) = pStoreImage_p->shape()(0);
   cursorShape(pStoreImage_p->ndim()-1) = pStoreImage_p->shape()(pStoreImage_p->ndim()-1);

   IPosition matrixAxes(2);
   matrixAxes(0) = 0; 
   matrixAxes(1) = pStoreImage_p->ndim()-1;

   LatticeStepper stepper(pStoreImage_p->shape(), cursorShape,
                          matrixAxes, IPosition::makeAxisPath(pStoreImage_p->ndim()));
   RO_LatticeIterator<T> pixelIterator(*pStoreImage_p, stepper);

   T *dummy = 0;
   DataType templateType = whatType(dummy);

   for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {

// Convert accumulations to  mean, sigma, and rms.   
// I will have to revisit this.   It will be ugly.
 
      Matrix<T>  matrix(pixelIterator.matrixCursor());   // Reference semantics

      for (uInt i=0; i<n1; i++) {

// Ugly.  real(Float) and real(Double) come from QMath.  Otherwise from Math
// We are also going to assume that the <T> does not include Int, as
// that would mean that the "ord" Matrix would be an Int and we would
// lose accuracy in working out the rms etc

         Int nPts = Int(real(matrix(i,NPTS))+0.1);
         switch (templateType) {
         case TpFloat:
         case TpDouble:
           if (nPts > 0) {
              ord(i,MEAN) = matrix(i,SUM) / nPts;
              NumericTraits<T>::PrecisionType tmp = 0.0;
              if (nPts > 1) tmp = (matrix(i,SUMSQ) - (matrix(i,SUM)*matrix(i,SUM)/nPts)) / 
                                  (nPts-1);
              ord(i,VARIANCE) = tmp;
              if (tmp > 0.0) {
                 ord(i,SIGMA) = sqrt(tmp);
              } else {
                 ord(i,SIGMA) = 0.0;
              }
              ord(i,RMS) = sqrt(matrix(i,SUMSQ)/nPts);
           }
           break;
         case TpComplex:
         case TpDComplex:
         default:
           ;
         }
      }


// Extract the direct (NPTS, SUM etc) values from the cursor matrix into the plot matrix
// There is no easy way to do this other than as I have

      uInt j;
      for (i=0; i<NACCUM; i++) {
         for (j=0; j<n1; j++) ord(j,i) = matrix(j,i);
      }


// Plot statistics

      if (plotter_p.isAttached()) {
        if (!plotStats (pixelIterator.position(), ord, plotter_p)) return False;
      }


// List statistics

      if (doList_p) {
         if (!listStats(pixelIterator.position(), ord)) return False;
      }
   }


// Finish up

   if (plotter_p.isAttached()) {
       plotter_p.updt();
   }
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
     if (haveLogger_p) os_p << LogIO::SEVERE << endl 
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False;
   }

// Retrieve storage array statistic

   return retrieveStorageStatistic(stats, Int(NPTS));
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
     if (haveLogger_p) os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }


// Retrieve storage array statistic

   return retrieveStorageStatistic(stats, Int(SUM));
}


template <class T>
Bool ImageStatistics<T>::getStats(Vector<T>& stats,
                                  const IPosition& pos,
                                  const Bool posInImage)
// 
// This function retrieves the statistics from the
// accumulation image at the specified location.  
//
// Inputs
//   posInImage   If true the location is given as image coordinates
//                The non-display axis values will be ignored.
//                Otherwise the position should be for the
//                display axes only.
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     if (haveLogger_p) {
        os_p << LogIO::SEVERE << endl
             << "The internal status of class is bad.  You have ignored errors" << endl
             << "in setting the arguments." << endl << endl << LogIO::POST;
     }
     return False; 
   }


// Retrieve storage array statistics

   stats.resize(NSTATS);
   if (!retrieveStorageStatistic(stats, pos, posInImage)) return False;

// Compute the rest

   uInt n = Int(real(stats(NPTS))+0.1);
   if (n == 0) {
      stats.resize(0);
      return  True;
   }

   NumericTraits<T>::PrecisionType tmp;
   stats(MEAN) = stats(SUM) / n;
   stats(SIGMA) = T(0.0);
   if (n > 1) {
      tmp = (stats(SUMSQ) - (stats(SUM)*stats(SUM)/n)) / (n-1);
      stats(VARIANCE) = tmp;
      if (tmp > 0.0) stats(SIGMA) = sqrt(tmp);
   }
   stats(RMS) = T(0.0);
   if (n > 0) stats(RMS) = sqrt(stats(SUMSQ)/n);

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
     if (haveLogger_p) {
        os_p << LogIO::SEVERE << endl
             << "The internal status of class is bad.  You have ignored errors" << endl 
             << "in setting the arguments." << endl << endl << LogIO::POST;
     }
     return False; 
   }


// Retrieve storage array statistic

   return retrieveStorageStatistic (stats, Int(SUMSQ));
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
     if (haveLogger_p) {
        os_p << LogIO::SEVERE << endl
             << "The internal status of class is bad.  You have ignored errors" << endl
             << "in setting the arguments." << endl << endl << LogIO::POST;
     }
     return False; 
    }

// Retrieve storage array statistic

   return retrieveStorageStatistic(stats, Int(MIN));
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
     if (haveLogger_p) os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }


// Retrieve storage array statistic

   return retrieveStorageStatistic (stats, Int(MAX));
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
     if (haveLogger_p) os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(MEAN));
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
     if (haveLogger_p) os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(SIGMA));
}

template <class T>
Bool ImageStatistics<T>::getVariance(Array<T>& stats)
// 
// This function calculates the VARIANCE statistics from the
// accumulation image
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     if (haveLogger_p) os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(VARIANCE));
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
     if (haveLogger_p) os_p << LogIO::SEVERE << endl
          << "The internal status of class is bad.  You have ignored errors" << endl
          << "in setting the arguments." << endl << endl << LogIO::POST;
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(RMS));
}



// Private functions


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

   Array<T> nPts;
   retrieveStorageStatistic (nPts, Int(NPTS));
   ReadOnlyVectorIterator<T> nPtsIt(nPts);
   const uInt n1 = nPtsIt.vector().nelements();

// Setup

   slice.resize(nPts.shape());
   slice = 0.0;
   VectorIterator<T> sliceIt(slice);

// Do it

   Int n;
   NumericTraits<T>::PrecisionType tmp;
   if (ISTAT == MEAN) {
       Array<T> sum;
       retrieveStorageStatistic (sum, Int(SUM));
       ReadOnlyVectorIterator<T> sumIt(sum);

       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
             n = Int(real(nPtsIt.vector()(i))+0.1);
             if(n > 0) sliceIt.vector()(i) = sumIt.vector()(i) / n;
          }
          nPtsIt.next();
          sumIt.next();
          sliceIt.next();
       }
    } else if (ISTAT == SIGMA) {
       Array<T> sum;
       retrieveStorageStatistic (sum, Int(SUM));
       ReadOnlyVectorIterator<T> sumIt(sum);

       Array<T> sumSq;
       retrieveStorageStatistic (sumSq, Int(SUMSQ));
       ReadOnlyVectorIterator<T> sumSqIt(sumSq);

       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
             n = Int(real(nPtsIt.vector()(i))+0.1);
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
    } else if (ISTAT == VARIANCE) {
       Array<T> sum;
       retrieveStorageStatistic (sum, Int(SUM));
       ReadOnlyVectorIterator<T> sumIt(sum);

       Array<T> sumSq;
       retrieveStorageStatistic (sumSq, Int(SUMSQ));
       ReadOnlyVectorIterator<T> sumSqIt(sumSq);

       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
             n = Int(real(nPtsIt.vector()(i))+0.1);
             if(n > 1) {
                tmp = (sumSqIt.vector()(i) -
                   (sumIt.vector()(i)*sumIt.vector()(i)/n)) / (n-1);
             }
          }
          nPtsIt.next();
          sumIt.next();
          sumSqIt.next();
          sliceIt.next();
       }
    } else if (ISTAT == RMS) {
       Array<T> sumSq;
       retrieveStorageStatistic (sumSq, Int(SUMSQ));
       ReadOnlyVectorIterator<T> sumSqIt(sumSq);

       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
             n = Int(real(nPtsIt.vector()(i))+0.1);
             if(n > 0) sliceIt.vector()(i) = sqrt(sumSqIt.vector()(i)/n);
          }
          nPtsIt.next();
          sumSqIt.next();
          sliceIt.next();
       }
    } else {
       if (haveLogger_p) os_p << LogIO::SEVERE << "Internal error" << endl << LogIO::POST;
       slice.resize(IPosition(0,0));
       return False;
    }

   return True;
}




template <class T>
Bool ImageStatistics<T>::findNextDatum (uInt& iFound, 
                                        const uInt& n,
                                        const Vector<T>& mask,
                                        const uInt& iStart,
                                        const Bool& findGood)
//
// Find the next good (or bad) point in an array.
// A good point in the array has a non-zero value.
//
// Inputs:
//  n        Number of points in array
//  mask     Vector containing counts.  If <T> complex,
//           the information is only in the real part
//  iStart   The index of the first point to consider
//  findGood If True look for next good point.  
//           If False look for next bad point
// Outputs:
//  iFound   Index of found point
//  Bool     False if didn't find another valid datum
{
   for (uInt i=iStart; i<n; i++) {
      if ( (findGood && real(mask(i))>0.5) ||
           (!findGood && real(mask(i))<0.5) ) {
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
   if (haveLogger_p) os_p << LogIO::NORMAL << "Creating new storage image" << endl << LogIO::POST;


// Set the display axes vector (possibly already set in ::setAxes)

   ImageUtilities::setDisplayAxes (displayAxes_p, cursorAxes_p, 
                                   pInImage_p->ndim());


// Work out dimensions of storage image (statistics accumulations
// are along the last axis)

    IPosition storeImageShape;
    ImageUtilities::setStorageImageShape(storeImageShape, True, Int(NACCUM),
                                         displayAxes_p, pInImage_p->shape());

// Set the storage image tile shape to the tile shape of the
// axes of the parent image from which it is created.  
// For the statistics axis, set the tile shape to NACCUM (small).

    IPosition tileShape(storeImageShape.nelements(),1);
    for (uInt i=0; i<tileShape.nelements()-1; i++) {
       tileShape(i) = pInImage_p->niceCursorShape()(displayAxes_p(i));
    }
    tileShape(tileShape.nelements()-1) = storeImageShape(storeImageShape.nelements()-1);
    Table myTable = ImageUtilities::setScratchTable(pInImage_p->name(),
                        String("ImageStatistics::"));

// Create storage image

    pStoreImage_p = new PagedArray<T>(TiledShape(storeImageShape,
                                      tileShape), myTable);


// Set up min/max location variables

   minPos_p.resize(pInImage_p->shape().nelements());
   maxPos_p.resize(pInImage_p->shape().nelements());


// Iterate through image and accumulate statistical sums

   ImageStatsTiledCollapser<T> collapser(range_p, noInclude_p, noExclude_p,
                                         fixedMinMax_p, blcParent_p);


   ImageStatisticsProgress* pProgressMeter = 0;
   if (showProgress_p) pProgressMeter = new ImageStatisticsProgress();

// This is the first output axis (there is only one in IS) getting 
// collapsed values

   Int newOutAxis = pStoreImage_p->ndim()-1;
   LatticeApply<T>::tiledApply(*pStoreImage_p, *pInImage_p, 
                               collapser, IPosition(cursorAxes_p),
                               newOutAxis, pProgressMeter);
   if (pProgressMeter !=0) {
      delete pProgressMeter;
      pProgressMeter = 0;
   }
   collapser.minMaxPos(minPos_p, maxPos_p);

   needStorageImage_p = False;     
   doneSomeGoodPoints_p = False;

   return True;
}


   

template <class T>
void ImageStatistics<T>::lineSegments (uInt& nSeg,
                                       Vector<uInt>& start,
                                       Vector<uInt>& nPts,
                                       const Vector<T>& mask)
//
// Examine an array and determine how many segments
// of good points it consists of.    A good point
// occurs if the array value is greater than zero.
//
// Inputs:
//   mask  The array.  Note that even if <T> is complex, only
//         the real part of this array contains the information.
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
Bool ImageStatistics<T>::listStats (const IPosition& dPos,
                                    const Matrix<T>& stats)
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
   if (!haveLogger_p) {

// We will consider this situation as successful

      return True;
   }

   os_p << endl;

// Get number of statistics and display axes

   const uInt nDisplayAxes = displayAxes_p.nelements();
   const uInt nStatsAxes = cursorAxes_p.nelements();

// Set up the manipulators. We list the number of points as an integer so find
// out how big the field width needs to be.  Min of 6 so label fits.

   Int nMax = 0;
   const uInt n1 = stats.shape()(0);
   for (uInt j=0; j<n1; j++) nMax = max(nMax, Int(real(stats.column(NPTS)(j))+0.1));
   const Int logNMax = Int(log10(Double(nMax))) + 2;
   const uInt oIWidth = max(5, logNMax);
   const uInt oDWidth = 15;

// Have to convert LogIO object to ostream before can apply
// the manipulators

   os_p.output().fill(' '); 
   os_p.output().setf(ios::scientific, ios::floatfield);
   os_p.output().setf(ios::left, ios::adjustfield);


// Write the pixel and world coordinate of the higher order display axes to the logger

   if (nDisplayAxes > 1) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);
      IPosition blc(pInImage_p->ndim(),0);
      IPosition trc(pInImage_p->shape()-1);


      for (j=1; j<nDisplayAxes; j++) {
         Int worldAxis = 
            pInImage_p->coordinates().pixelAxisToWorldAxis(displayAxes_p(j));
         String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);
         pixels(0) = Double(locInImage(dPos)(j));

         if (!ImageUtilities::pixToWorld (sWorld, pInImage_p->coordinates(),
                                     displayAxes_p(j), cursorAxes_p,
                                     blc, trc, pixels, -1)) return False;
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
   IPosition blc(pInImage_p->ndim(),0);
   IPosition trc(pInImage_p->shape()-1);

   ImageUtilities::pixToWorld(sWorld, pInImage_p->coordinates(),
                              displayAxes_p(0), cursorAxes_p, 
                              blc, trc, pixels, -1);
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

   for (j=0; j<n1; j++) pixels(j) = Double(j);
   if (!ImageUtilities::pixToWorld(sWorld, pInImage_p->coordinates(),
                              displayAxes_p(0), cursorAxes_p, 
                              blc, trc, pixels, -1)) return False;


// Write statistics to logger.  We write the pixel location
// relative to the parent image

   for (j=0; j<n1; j++) {
      os_p.output() << setw(len0)     << j+blcParent_p(displayAxes_p(0))+1;
      os_p.output() << setw(oCWidth)   << sWorld(j);
      os_p.output() << setw(oIWidth)   << Int(real(stats.column(NPTS)(j))+0.1);

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
// the non-statistics axis (the statistics axis is the last one) to 
// account for the  location of the subImage in the parent image
//
{  
   IPosition pos(storagePosition);
   for (uInt j=0; j<pos.nelements()-1; j++) {
     pos(j) = storagePosition(j) + blcParent_p(displayAxes_p(j));
   }
   return pos;
}



template <class T>
void ImageStatistics<T>::multiColourYLabel (String& label,      
                                            PGPlotter& plotter,
                                            const String& LRLoc, 
                                            const Vector<uInt>& colours,
                                            const Int& nLabs)

//
// Draw each Y-axis sublabel in a string with a different colour
//
{
// Get attributes


   Vector<Float> result= plotter.qwin();
   Float y1 = result(2);
   Float y2 = result(3);
   Int sci = plotter.qci();


// Find y-location of start of string as fraction of window

   result.resize(0);
   result = plotter.qtxt(0.0, 0.0, 90.0, 0.0, label);
   Vector<Float> xb = result(Slice(0,4));
   Vector<Float> yb = result(Slice(4,4));
   Float dy = yb(2)-yb(0);
   Float yLoc = abs(0.5*(y2-y1-dy)/(y2-y1));


// Loop over number of sub-labels and write them in colour

   String subLabel;
   Float just = 0.0;
   Float disp = 2.5;
   if (LRLoc == "R") disp = 3.0;
   for (Int iLab=0; iLab<nLabs; iLab++) {

// Fish out next sub label

      if (!findNextLabel (subLabel, iLab, label)) {
         plotter.sci (sci);
         return;
      } 
      
       
// Write it

      if (iLab < nLabs-1) subLabel = subLabel + ",";
      if (iLab > 0) subLabel.prepend(" ");
      plotter.sci (colours(iLab));
      plotter.mtxt (LRLoc, disp, yLoc, just, subLabel);


// Increment y location.  pgqtxt won't count a leading blank so
// replace it with a character for length counting purposes. These
// stupid string classes make this very hard work.

      String s2;
      if (iLab > 0) {
         String s(subLabel(1,subLabel.length()-1));
         s2 = "x" + s;
      } else
         s2 = subLabel;
      result.resize(0);
      result = plotter.qtxt (0.0, 0.0, 90.0, 0.0, s2.chars());
      xb = result(Slice(0,4));
      yb = result(Slice(4,4));
      dy = abs((yb(2)-yb(0))/(y2-y1));
      yLoc += dy;
   }                       

// Set colour back to what it was

   plotter.sci (sci);
   return;
}




template <class T>
void ImageStatistics<T>::multiPlot (PGPlotter& plotter,
                                    const Vector<T>& x,
                                    const Vector<T>& y,
                                    const Vector<T>& mask)
//
// Plot an array which may have some blanked points.
// Thus we plot it in segments
//
// Currently, only the real part of the <T>  data is
// plotted
//
// Inputs:
//  x,y,mask   Abcissa, ordinate, and "masking" array
//           (if > 0 plot it)
{

// Find number of segments in this array

   uInt nSeg = 0;
   Vector<uInt> start;
   Vector<uInt> nPts;
   lineSegments (nSeg, start, nPts, mask);

// Loop over segments and plot them

   Vector<Float> xtmp, ytmp;
   for (uInt i=0; i<nSeg; i++) {
      const uInt ip = start(i);
      if (nPts(i) == 1) {
	  xtmp.resize(1); 
          ytmp.resize(1); 
          xtmp(0) = real(x(ip));
          ytmp(0) = real(y(ip));
	  plotter.pt (xtmp, ytmp, 1);
      } else {
	  xtmp.resize(nPts(i)); 
          ytmp.resize(nPts(i));
          for (uInt j=0; j<nPts(i); j++) {
             xtmp(j) = real(x(start(i)+j));
             ytmp(j) = real(y(start(i)+j));
          }
	  plotter.line (xtmp, ytmp);
      }
   }
}


template <class T>
void ImageStatistics<T>::minMax (Bool& none,
                                 T& dMin, 
                                 T& dMax,  
                                 const Vector<T>& d,
                                 const Vector<T>& n)
//
//
// Inputs:
//   d   Vector to find min and max of
//   n   Vector which gives the number of points
//       that were used to compute the value in pt.  If zero,
//       that means there were no valid points and we don't
//       want to consider the corresponding pd[i] value
// Outputs:
//   none       No valid points in array
//   dMin,DMax  Min and max of array pd

{
   Bool init = True;
   none = True;
   const Int n1 = d.nelements();

   for (Int i=0; i<n1; i++) {
     if (real(n(i)) > 0.5) {
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
Bool ImageStatistics<T>::plotStats (const IPosition& dPos,
                                    const Matrix<T>& stats,
                                    PGPlotter& plotter)
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
   const Bool doVar   = ToBool(ImageUtilities::inVector(Int(VARIANCE), statsToPlot_p) != -1);
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

// Generate abcissa. Note that T(n) where T is COmplex results in (n+0i)

   const Int n1 = stats.shape()(0);
   Vector<T> abc(n1);
   for (Int j=0; j<n1; j++) abc(j) = T(j+1);


// Find extrema.  Return if there were no valid points to plot

   T yMin, yMax, xMin, xMax, yLMin, yLMax, yRMin, yRMax;

   minMax(none, xMin, xMax, abc, stats.column(NPTS));
   if (none) return True;

// Left hand y axis

   if (doMean) {
      minMax(none, yLMin, yLMax, stats.column(MEAN), stats.column(NPTS));
      first = False;
      nL++;
   }
   if (doSum) {
      minMax(none, yMin, yMax, stats.column(SUM), stats.column(NPTS));
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
      minMax(none, yMin, yMax, stats.column(SUMSQ), stats.column(NPTS));
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
      minMax(none, yMin, yMax, stats.column(MIN), stats.column(NPTS));
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
      minMax(none, yMin, yMax, stats.column(MAX), stats.column(NPTS));
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
      minMax(none, yMin, yMax, stats.column(NPTS), stats.column(NPTS));
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
      minMax(none, yRMin, yRMax, stats.column(SIGMA), stats.column(NPTS));
      first = False;
      nR++;
   }
   if (doVar) {
      minMax(none, yMin, yMax, stats.column(VARIANCE), stats.column(NPTS));
      if (first) {
         yRMin = yMin;
         yRMax = yMax;
      } else {
         yRMin = min(yRMin,yMin);
         yRMax = max(yRMax,yMax);
      }
      first = False;
      nR++;
   }
   if (doRms) {
      minMax(none, yMin, yMax, stats.column(RMS), stats.column(NPTS));
      if (first) {
         yRMin = yMin;
         yRMax = yMax;
      } else {
         yRMin = min(yRMin,yMin);
         yRMax = max(yRMax,yMax);
      }
      nR++;
   }

   stretchMinMax(xMin, xMax); 
   if (nL>0) stretchMinMax(yLMin, yLMax);
   if (nR>0) stretchMinMax(yRMin, yRMax);


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
      if (doVar) {
         yRLabel += "Variance,";
         nRLabs++;
      }
      if (doRms) {
         yRLabel += "Rms,";
         nRLabs++;
      }
      yRLabel.del(Int(yRLabel.length()-1),1);
   }
   
// Do plots.  Here we convert to real 

   Vector<uInt> lCols(nL);
   Vector<uInt> rCols(nR);
   Int ls = 0;
   Int i = -1;
   Bool initColours = True;
   plotter.page();

   if (nL>0) {
      plotter.swin(real(xMin), real(xMax), real(yLMin), real(yLMax));
      if (nR>0) 
         plotter.box("BCNST", 0.0, 0, "BNST", 0.0, 0);
      else
         plotter.box("BCNST", 0.0, 0, "BCNST", 0.0, 0);
      plotter.lab(xLabel, "", "");

      if (doMean) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(MEAN), stats.column(NPTS));
      }
      if (doSum) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(SUM), stats.column(NPTS));
      }
      if (doSumSq) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(SUMSQ), stats.column(NPTS));
      }
      if (doMin) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(MIN), stats.column(NPTS));
      }
      if (doMax) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(MAX), stats.column(NPTS));
      }
      if (doNPts) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(NPTS), stats.column(NPTS));
      }

// Y label

      multiColourYLabel (yLLabel, plotter_p, "L", lCols, nLLabs);

   }
   plotter.sls (1);
   plotter.sci (1);


   i = -1;
   if (nR>0) {
      plotter.swin(real(xMin), real(xMax), real(yRMin), real(yRMax));
      plotter.sci (1); 
      if (nL>0) 
         plotter.box("", 0.0, 0, "CMST", 0.0, 0);
      else {
         plotter.box("BCNST", 0.0, 0, "BCMST", 0.0, 0);
         plotter.lab(xLabel, "", "");
      }

      if (doSigma) {
         if (++ls > 5) ls = 1;
         plotter.sls(ls);

         rCols(++i) = niceColour (initColours);
         plotter.sci (rCols(i));

         multiPlot(plotter, abc, stats.column(SIGMA), stats.column(NPTS));
      }
      if (doVar) {
         if (++ls > 5) ls = 1;
         plotter.sls(ls);

         rCols(++i) = niceColour (initColours);
         plotter.sci (rCols(i));

         multiPlot(plotter, abc, stats.column(VARIANCE), stats.column(NPTS));
      }
      if (doRms) {
         if (++ls > 5) ls = 1;
         plotter.sls(ls);

         rCols(++i) = niceColour (initColours);
         plotter.sci (rCols(i));

         multiPlot(plotter, abc, stats.column(RMS), stats.column(NPTS));
      }

// Y label

      multiColourYLabel (yRLabel, plotter, "R", rCols, nRLabs);
   }
   plotter.sls(1);
   plotter.sci (1);


// Write values of other display axes on plot
   
   ostrstream oss;
   if (displayAxes_p.nelements() > 1) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);
      IPosition blc(pInImage_p->ndim(),0);
      IPosition trc(pInImage_p->shape()-1);

      for (uInt j=1; j<displayAxes_p.nelements(); j++) {
         Int worldAxis = 
            pInImage_p->coordinates().pixelAxisToWorldAxis(displayAxes_p(j));
         String name = pInImage_p->coordinates().worldAxisNames()(worldAxis);
         pixels(0) = Double(locInImage(dPos)(j));

         if (!ImageUtilities::pixToWorld (sWorld, pInImage_p->coordinates(),
                                     displayAxes_p(j), cursorAxes_p,
                                     blc, trc, pixels, -1)) return False;
         oss << "  " << ImageUtilities::shortAxisName(name)
             << "=" << locInImage(dPos)(j)+1 << " (" << sWorld(0) << ")";
      }   
      oss << ends;
      char* tLabel = oss.str();


// Write on plot
      
      Vector<Float> result(8);
      result = plotter.qtxt (0.0, 0.0, 0.0, 0.0, "X");
      Vector<Float> xb = result(Slice(0,4));
      Vector<Float> yb = result(Slice(4,4));
      Float dx = xb(3) - xb(0);
      result = plotter.qtxt (0.0, 0.0, 0.0, 0.0, tLabel);
      xb = result(Slice(0,4));
      yb = result(Slice(4,4));
      Float dy = yb(1) - yb(0);

      Float mx = xMin + dx;
      Float my;
      if (nR > 0) {
         my = yRMax + 0.5*dy;
      } else {
         my = yLMax + 0.5*dy;
      }

      Int tbg;
      tbg = plotter.qtbg();
      plotter.stbg(0);
      plotter.ptxt (mx, my, 0.0, 0.0, tLabel);
      plotter.stbg(tbg);
   }

   return True;
}


template <class T>
Bool ImageStatistics<T>::retrieveStorageStatistic(Array<T>& slice, 
                                                  const Int& ISTAT)
//
// Retrieve values from accumulation image
//
// Input
//   ISTAT        Points at location of desired statistic in 
//                accumulation image (last axis)
// Input/output
//   slice        The statistics; should be of zero size on input
//
// Returns false if internal class state is bad.
{

// Generate storage image if required

   if (needStorageImage_p) {
      if (!generateStorageImage()) return False;
   }


// Were there some good points ?  

   const Int nDim = pStoreImage_p->ndim();
   slice.resize(IPosition(0,0));

   if (someGoodPoints()) {


// Get desired statistic slice. Discard degenerate axes (requires
// empty array on input)

      IPosition sliceShape(pStoreImage_p->shape());
      sliceShape(nDim-1) = 1;

      IPosition pos(nDim,0);
      pos(nDim-1) = ISTAT;

      pStoreImage_p->getSlice(slice, pos, sliceShape, 
                              IPosition(nDim,1), True);
   }
   return True;
}



template <class T>
Bool ImageStatistics<T>::retrieveStorageStatistic(Vector<T>& slice, 
                                                  const IPosition& pos,
                                                  const Bool posInImage)
//
// Retrieve values from accumulation image
//
// Input
//   pos          Locations for the display axes in the storage image
//   posInImage   If true the location is given as image coordinates
//                The non-display axis values will be ignored.
//                Otherwise the position should be for the
//                display axes only.
//
// Input/output
//   slice        The statistics; should be long enough on input
//
{


// Make sure we have a correctly size position

   if (posInImage) {
      if (pos.nelements() != pInImage_p->ndim()) {
         if (haveLogger_p) {
            os_p << LogIO::SEVERE << "Incorrectly sized position given" << LogIO::POST;
         }
         slice.resize(0);
         return False;
      }
   } else {
      if (pos.nelements() != displayAxes_p.nelements()) {
         if (haveLogger_p) {
            os_p << LogIO::SEVERE << "Incorrectly sized position given" << LogIO::POST;
         }
         slice.resize(0);
         return False;
      }
   }


// Generate storage image if required

   if (needStorageImage_p) {
      if (!generateStorageImage()) return False;
   }


// Get accumulation sums slice from storage image.
// Last axis is statistics axis

   const uInt nDim = displayAxes_p.nelements();
   IPosition slicePos(nDim+1,0);
   if (posInImage) {

// Discard non display axes

      for (uInt i=0; i<nDim; i++) {
         slicePos(i) = pos(displayAxes_p(i));
      }
   } else {

// Use position as is

      for (uInt i=0; i<nDim; i++) {
         slicePos(i) = pos(i);
      }
   }


// Get slice

   IPosition sliceShape(nDim+1,1);
   sliceShape(nDim) = NACCUM;
   Array<T> tSlice;
   pStoreImage_p->getSlice(tSlice, slicePos, sliceShape, 
                           IPosition(nDim+1,1), False);

// Copy to vector      

   slicePos = 0;
   for (uInt i=0; i<NACCUM; i++) {
      slicePos(nDim) = i;
      slice(i) = tSlice(slicePos);         
   }
   return True;
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
         Array<T> stats(shape);
         IPosition pos(1,0);

         pStoreImage_p->getSlice(stats, pos, shape, IPosition(1,1));
   
         pos(0) = NPTS;
         if (Int(real(stats(pos))+0.1) > 0) {
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
         matrixAxes(0) = 0; 
         matrixAxes(1) = pStoreImage_p->ndim()-1;

         LatticeStepper stepper(pStoreImage_p->shape(), cursorShape,
                                matrixAxes, IPosition::makeAxisPath(pStoreImage_p->ndim()));
         RO_LatticeIterator<T> pixelIterator(*pStoreImage_p, stepper);

         for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {
            for (Int i=0; i<n1; i++) {
               if (Int(real(pixelIterator.matrixCursor()(i,NPTS))+0.1) > 0) {
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
   Array<T> stats(shape);
   pStoreImage_p->getSlice (stats, IPosition(1,0), shape, IPosition(1,1));

   IPosition pos(1);

   pos(0) = NPTS;
   Int nPts = Int(real(stats(pos))+0.1);

   pos(0) = SUM;
   NumericTraits<T>::PrecisionType sum = stats(pos);

   pos(0) = SUMSQ;
   NumericTraits<T>::PrecisionType sumSq = stats(pos);
                         
   NumericTraits<T>::PrecisionType mean = 0.0;
   if (nPts > 0) mean = sum/nPts;
            
   NumericTraits<T>::PrecisionType var = 0.0;
   if (nPts > 1) var = (sumSq - sum*sum/nPts)/(nPts-1);

   NumericTraits<T>::PrecisionType rms = 0.0;
   if (sumSq > 0 && nPts > 0) rms = sqrt(sumSq/nPts);

   pos(0) = MIN;
   T dMin = stats(pos);
   pos(0) = MAX;
   T dMax = stats(pos);

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
      if (!fixedMinMax_p) {
         os_p << "Minimum value at " << minPos_p+1 << " = ";
         os_p.output()  << setw(oWidth) << dMin << endl;
         os_p << "Maximum value at " << maxPos_p+1 << " = ";
         os_p.output()  << setw(oWidth) << dMax << endl;   
      }
   } else
      os_p << "No valid points found " << endl;

   os_p << endl << endl;
   os_p.post();
}

 
template <class T>
void ImageStatistics<T>::stretchMinMax (T& dMin,
                                        T& dMax)
//
// Stretch a range by 5%  
//  
// Input/output:
//   dMin,Max     The range to stretch
// 
{    
   T delta = 0.05*(dMax-dMin);
   T absmax = max(abs(dMax),abs(dMin));
   if (delta < 1.0e-5*absmax) delta = 0.01 * absmax;
                                 
   if (dMin==dMax) {
      if (dMin==0.0) {
         dMin = -1.0;
         dMax = 1.0;
      }  
      else {
         dMin = dMin - 0.05*dMin;
         dMax = dMax + 0.05*dMax;
      }  
   }
   else {
      dMin = dMin - delta;
      dMax = dMax + delta;
   }
}
   
 
template <class T>  
Bool ImageStatistics<T>::setIncludeExclude (Vector<T>& range,
                                            Bool& noInclude,
                                            Bool& noExclude,
                                            const Vector<T>& include,
                                            const Vector<T>& exclude,
                                            ostream& os)
//
// Take the user's data inclusion and exclusion data ranges and
// generate the range and Booleans to say what sort it is
//
// Inputs:
//   include   Include range given by user. Zero length indicates
//             no include range
//   exclude   Exclude range given by user. As above.
//   os        Output stream for reporting
// Outputs:
//   noInclude If True user did not give an include range
//   noExclude If True user did not give an exclude range
//   range     A pixel value selection range.  Will be resized to
//             zero length if both noInclude and noExclude are True  
//   Bool      True if successfull, will fail if user tries to give too
//             many values for includeB or excludeB, or tries to give
//             values for both
{
   noInclude = True;
   range.resize(0);
   if (include.nelements() == 0) {
     ;
   } else if (include.nelements() == 1) {
      range.resize(2);
      range(0) = -abs(include(0));
      range(1) =  abs(include(0));
      noInclude = False;
   } else if (include.nelements() == 2) {
      range.resize(2);
      range(0) = min(include(0),include(1));
      range(1) = max(include(0),include(1));
      noInclude = False;  
   } else {
      os << endl << "Too many elements for argument include" << endl;
      return False;
   }
   
   noExclude = True;
   if (exclude.nelements() == 0) {
      ;
   } else if (exclude.nelements() == 1) {
      range.resize(2);
      range(0) = -abs(exclude(0));
      range(1) =  abs(exclude(0));
      noExclude = False;
   } else if (exclude.nelements() == 2) {
      range.resize(2); 
      range(0) = min(exclude(0),exclude(1));
      range(1) = max(exclude(0),exclude(1));
      noExclude = False;
   } else {
      os << endl << "Too many elements for argument exclude" << endl;
      return False;
   }
   if (!noInclude && !noExclude) {
      os << "You can only give one of arguments include or exclude" << endl;
      return False;
   }
   return True;
} 
 




// ImageStatsTiledCollapser


template <class T>
ImageStatsTiledCollapser<T>::ImageStatsTiledCollapser(const Vector<T>& pixelRange, 
                                                      Bool noInclude, 
                                                      Bool noExclude,
                                                      Bool fixedMinMax,
                                                      const IPosition& blcParent)
: range_p(pixelRange),
  noInclude_p(noInclude),
  noExclude_p(noExclude),
  fixedMinMax_p(fixedMinMax),
  minPos_p(0),
  maxPos_p(0),
  blcParent_p(blcParent)
{;}


template <class T>
void ImageStatsTiledCollapser<T>::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == NACCUM, AipsError);
}

template <class T>
void ImageStatsTiledCollapser<T>::initAccumulator (uInt n1, uInt n3)
{
//   cout << "Init accumulator called" << endl;
//   cout << "n1,n3=" << n1 << ", " << n3 << endl;
   pSum_p = new Block<NumericTraits<T>::PrecisionType>(n1*n3);
   pSumSq_p = new Block<NumericTraits<T>::PrecisionType>(n1*n3);
   pNPts_p = new Block<uInt>(n1*n3);
   pMin_p = new Block<T>(n1*n3);
   pMax_p = new Block<T>(n1*n3);
   pInitMinMax_p = new Block<Bool>(n1*n3);

   pSum_p->set(0);
   pSumSq_p->set(0);
   pNPts_p->set(0);
   pMin_p->set(0);
   pMax_p->set(0);
   pInitMinMax_p->set(True);

   n1_p = n1;
   n3_p = n3;

}

template <class T>
void ImageStatsTiledCollapser<T>::process (uInt index1,
                                           uInt index3,
                                           const T* pInData, 
                                           const Bool* pInMask, 
                                           uInt inIncr, 
                                           uInt nrval,
                                           const IPosition& startPos, 
                                           const IPosition& shape)
//
// Process the data in the current chunk.   Everything in this
// chunk belongs in one output location in the accumulation
// images
//
{
   uInt index = index1 + index3*n1_p;
//   cout << "::process  index=" << index << endl;
   NumericTraits<T>::PrecisionType& sum = (*pSum_p)[index];
   NumericTraits<T>::PrecisionType& sumSq = (*pSumSq_p)[index];
   uInt& nPts = (*pNPts_p)[index];
   T& dataMin = (*pMin_p)[index];
   T& dataMax = (*pMax_p)[index];
   Bool& minMaxInit = (*pInitMinMax_p)[index];

// If these are != -1 after the accumulating, then
// the min and max were updated

   Int minLoc = -1;
   Int maxLoc = -1;

   if (pInMask == 0) {

// All pixels are good

      if (!noInclude_p) {
          
// Inclusion range
     
         T datum;
         for (uInt i=0; i<nrval; i++) {
            datum = *pInData;
            if (datum >= range_p(0) && datum <= range_p(1)) {
              accumulate(nPts, sum, sumSq, dataMin, dataMax, 
                         minLoc, maxLoc, minMaxInit, 
                         fixedMinMax_p, datum, i);
            }
            pInData += inIncr;
         }
         if (fixedMinMax_p) {
            dataMin = range_p(0);
            dataMax = range_p(1);
         }
      } else if (!noExclude_p) {

// Exclusion range

         T datum;
         for (uInt i=0; i<nrval; i++) {
            datum = *pInData;
            if (datum < range_p(0) || datum > range_p(1)) {
              accumulate(nPts, sum, sumSq, dataMin, dataMax, 
                         minLoc, maxLoc, minMaxInit, False, datum, i);
            }
            pInData += inIncr;
         }
      } else {
 
// All data accepted

         T datum;
         for (uInt i=0; i<nrval; i++) {
            datum = *pInData;
            accumulate(nPts, sum, sumSq, dataMin, dataMax, 
                       minLoc, maxLoc, minMaxInit, False, datum, i);
            pInData += inIncr;
         }
      }
   } else {

// Some pixels are bad

      if (!noInclude_p) {
          
// Inclusion range
     
         T datum;
         Bool mask;
         for (uInt i=0; i<nrval; i++) {
            datum = *pInData;
            mask = *pInMask;
            if (mask && datum >= range_p(0) && datum <= range_p(1)) {
              accumulate(nPts, sum, sumSq, dataMin, dataMax, 
                         minLoc, maxLoc, minMaxInit, False, datum, i);
            }
            pInData += inIncr;
            pInMask += inIncr;
         }
         if (fixedMinMax_p) {
            dataMin = range_p(0);
            dataMax = range_p(1);
         }
      } else if (!noExclude_p) {

// Exclusion range

         T datum;
         Bool mask;
         for (uInt i=0; i<nrval; i++) {
            datum = *pInData;
            mask = *pInMask;
            if (mask && (datum < range_p(0) || datum > range_p(1))) {
              accumulate(nPts, sum, sumSq, dataMin, dataMax, 
                         minLoc, maxLoc, minMaxInit, False, datum, i);
            }
            pInData += inIncr;
            pInMask += inIncr;
         }
      } else {
 
// All data accepted

         T datum;
         Bool mask;
         for (uInt i=0; i<nrval; i++) {
            datum = *pInData;
            mask = *pInMask;
            if (mask) {
               accumulate(nPts, sum, sumSq, dataMin, dataMax, 
                          minLoc, maxLoc, minMaxInit, False, datum, i);
            }
            pInData += inIncr;
            pInMask += inIncr;
         }
      }
   }

// Update overall min and max location.  These are never updated
// if fixedMinMax is true.

   if (minLoc != -1) {
     minPos_p = blcParent_p + startPos + toIPositionInArray(minLoc, shape);
   }
   if (maxLoc != -1) {
     maxPos_p = blcParent_p + startPos + toIPositionInArray(maxLoc, shape);
   }
}

template <class T>
void ImageStatsTiledCollapser<T>::endAccumulator(Array<T>& result,
                                                 Array<Bool>& resultMask,
                                                 const IPosition& shape)
{ 

// Reshape arrays.  The mask is always true.  Any locations
// in the storage image for which there were no valid points 
// will have the NPTS field set to zero.  That is what
// we use to effectively mask it.

    result.resize(shape);
    resultMask.resize(shape);
    resultMask.set(True);

// 

    Bool deleteRes;
    T* res = result.getStorage (deleteRes);
    T* resptr = res;

//    cout << "Array shape = " << result.shape() << endl;

    const NumericTraits<T>::PrecisionType* sumPtr = pSum_p->storage();
    const NumericTraits<T>::PrecisionType* sumSqPtr = pSumSq_p->storage();
    const uInt* nPtsPtr = pNPts_p->storage();
    const T* minPtr = pMin_p->storage();
    const T* maxPtr = pMax_p->storage();

//    cout << "EndAccumulator:: n1_p = " << n3_p << endl;
//    cout << "EndAccumulator:: n3_p = " << n3_p << endl;

    uInt i,j,k;
    T* resptr_root = resptr;
    k = 0;
    for (i=0; i<n3_p; i++) {
       resptr = resptr_root + (Int(NPTS) * n1_p);
       for (j=0; j<n1_p; j++,k++) {   
          *resptr++ = T(*nPtsPtr++);
       }

       resptr = resptr_root + (Int(SUM) * n1_p);
       for (j=0; j<n1_p; j++,k++) {   
          *resptr++ = T(*sumPtr++);
       }

       resptr = resptr_root + (Int(SUMSQ) * n1_p);
       for (j=0; j<n1_p; j++,k++) {   
          *resptr++ = T(*sumSqPtr++);
       }

       resptr = resptr_root + (Int(MIN) * n1_p);
       objcopy (resptr, minPtr, n1_p); 
       resptr += n1_p;
       minPtr += n1_p;

       resptr = resptr_root + (Int(MAX) * n1_p);
       objcopy (resptr, maxPtr, n1_p); 
       resptr += n1_p;
       maxPtr += n1_p;

       resptr_root += n1_p * Int(NACCUM);

    }

    delete pSum_p;
    delete pSumSq_p;
    delete pNPts_p;
    delete pMin_p;
    delete pMax_p;
    delete pInitMinMax_p;

    result.putStorage (res, deleteRes);

//    cout << "result=" << result.ac() << endl;

}


template <class T>
void ImageStatsTiledCollapser<T>::minMaxPos(IPosition& minPos, IPosition& maxPos)
{
   minPos.resize(minPos_p.nelements());
   minPos = minPos_p;
   maxPos.resize(maxPos_p.nelements());
   maxPos = maxPos_p;
}

