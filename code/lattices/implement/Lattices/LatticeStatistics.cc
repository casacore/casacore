//# LatticeStatistics.cc: generate statistics from a MaskedLattice
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

#include <trial/Lattices/LatticeStatistics.h>
#include <trial/Lattices/LattStatsSpecialize.h>
#include <trial/Lattices/LattStatsProgress.h>

#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <trial/Lattices/MaskedLattice.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/SubLattice.h>
#include <aips/Lattices/TempLattice.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <aips/Mathematics/Math.h>
#include <aips/Quanta/QMath.h>
#include <aips/Tasking/AppInfo.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/ValType.h>

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <strstream.h>

#include <aips/OS/Timer.h>



template <class T>
LatticeStatistics<T>::LatticeStatistics (const MaskedLattice<T>& lattice,
                                         LogIO& os,  
                                         Bool showProgress,
                                         Bool forceDisk)
// 
// Constructor
//
: os_p(os),
  pInLattice_p(0), 
  pStoreLattice_p(0),
  doList_p(False),
  noInclude_p(True),
  noExclude_p(True),
  goodParameterStatus_p(True),
  needStorageLattice_p(True),
  doneSomeGoodPoints_p(False),
  someGoodPointsValue_p(False),
  haveLogger_p(True),
  showProgress_p(showProgress),
  fixedMinMax_p(False),
  forceDisk_p(forceDisk),
  doRobust_p(False),
  error_p("")
{
   nxy_p.resize(0);
   statsToPlot_p.resize(0);   
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);

   if (setNewLattice(lattice)) {

// Cursor axes defaults to all

      Vector<Int> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      goodParameterStatus_p = False;
   }

// Avoid double deletion by LogIO::cleanup
   os_p.makePermanent();

}


template <class T>
LatticeStatistics<T>::LatticeStatistics (const MaskedLattice<T>& lattice,
                                         Bool showProgress,
                                         Bool forceDisk)
// 
// Constructor
//
: pInLattice_p(0), 
  pStoreLattice_p(0),
  doList_p(False),
  noInclude_p(True),
  noExclude_p(True),
  goodParameterStatus_p(True),
  needStorageLattice_p(True),
  doneSomeGoodPoints_p(False),
  someGoodPointsValue_p(False),
  haveLogger_p(False),
  showProgress_p(showProgress),
  fixedMinMax_p(False),
  forceDisk_p(forceDisk),
  doRobust_p(False),
  error_p("")
{
   nxy_p.resize(0);
   statsToPlot_p.resize(0);
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);
//
   if (setNewLattice(lattice)) {

// Cursor axes defaults to all

      Vector<Int> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      goodParameterStatus_p = False;
   }
}


template <class T>
LatticeStatistics<T>::LatticeStatistics(const LatticeStatistics<T> &other) 
: pInLattice_p(0),
  pStoreLattice_p(0)
//
// Copy constructor.  Storage lattice is not copied.
//
{
   operator=(other);
}


template <class T>
LatticeStatistics<T> &LatticeStatistics<T>::operator=(const LatticeStatistics<T> &other)
//
// Assignment operator.  Storage lattice is not copied
//
{
   if (this != &other) {

// Deal with lattice pointer

      if (pInLattice_p!=0) delete pInLattice_p;
      pInLattice_p = other.pInLattice_p->cloneML();

// Delete storage lattice 

      if (pStoreLattice_p != 0) {
         delete pStoreLattice_p;
         pStoreLattice_p = 0;
      }
      needStorageLattice_p = True;


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
//
      doneSomeGoodPoints_p = other.doneSomeGoodPoints_p;
      someGoodPointsValue_p = other.someGoodPointsValue_p;
      haveLogger_p = other.haveLogger_p;
      showProgress_p = other.showProgress_p;
      fixedMinMax_p = other.fixedMinMax_p;
      minPos_p = other.minPos_p; 
      maxPos_p = other.maxPos_p;
      blcParent_p = other.blcParent_p;
      forceDisk_p = other.forceDisk_p;
      doRobust_p = other.doRobust_p;
      error_p = other.error_p;
   }
   return *this;
}


 

template <class T>
LatticeStatistics<T>::~LatticeStatistics()
//
// Destructor.  
//
{
   delete pInLattice_p;
   pInLattice_p = 0;
   if (pStoreLattice_p != 0) {
      delete pStoreLattice_p;
      pStoreLattice_p = 0;
   }
}


template <class T>
Bool LatticeStatistics<T>::setAxes (const Vector<Int>& axes)
//
// This function sets the cursor axes and the display axes
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

// Save current cursor axes

   Vector<Int> saveAxes(cursorAxes_p.copy());

// Assign cursor axes.

   cursorAxes_p.resize(0);
   cursorAxes_p = axes;

   if (cursorAxes_p.nelements() == 0) {
   
// User didn't give any axes.  Set them to all.
       
      cursorAxes_p.resize(pInLattice_p->ndim());
      for (uInt i=0; i<pInLattice_p->ndim(); i++) cursorAxes_p(i) = i;
   } else {

// Sort axes into increasing order and check

      GenSort<Int>::sort(cursorAxes_p, Sort::Ascending, Sort::QuickSort|Sort::NoDuplicates);
//
      for (uInt i=0; i<cursorAxes_p.nelements(); i++) {
         if (cursorAxes_p(i) < 0 || cursorAxes_p(i) > Int(pInLattice_p->ndim()-1)) {
            error_p = "Invalid cursor axes";
            return False;
         }
      }
   }

// Signal that we have changed the axes and need a new accumulation
// lattice

   if (saveAxes.nelements() != cursorAxes_p.nelements() ||
       !allEQ(saveAxes, cursorAxes_p)) needStorageLattice_p = True;

// Set the display axes vector.  We also do this in ::generateStorageLattice
// but it is possible the user will want to see the display axes
// via the public function "displayAxes" before any real work is done
// so poke this in here too.

   displayAxes_p.resize(0);
   displayAxes_p = IPosition::otherAxes(pInLattice_p->ndim(),
                                        cursorAxes_p).asVector();

   return True;
}


template <class T>
Bool LatticeStatistics<T>::setInExCludeRange(const Vector<T>& include,
                                             const Vector<T>& exclude,
                                             Bool setMinMaxToInclude)
//
// Assign the desired exclude range
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

// Save current ranges

   Vector<T> saveRange(range_p.copy());
   Bool saveFixedMinMax = fixedMinMax_p;

// Check
      
   ostrstream os;
   Bool saveNoInclude = noInclude_p;
   Bool saveNoExclude = noExclude_p;
   if (!LattStatsSpecialize::setIncludeExclude(error_p, range_p, noInclude_p, 
                                               noExclude_p, include, exclude)) {
      goodParameterStatus_p = False;
      return False;
   }

// Can't have fixed min and max with an exclusion range

   fixedMinMax_p = setMinMaxToInclude;
   if (!noExclude_p && fixedMinMax_p) {
      if (haveLogger_p) {
         error_p = "Can't have a fixed min and max with an exclusion range";
      }
      goodParameterStatus_p = False;
      return False;
   }

// Can only have fixed min and max range if user gives it

   if (noInclude_p) fixedMinMax_p = False;


// Signal that we have changed the pixel range and need a new accumulation
// lattice
   
   if ( (saveNoInclude!=noInclude_p) ||
        (saveNoExclude!=noExclude_p) ||
        (saveFixedMinMax != fixedMinMax_p) ||
        (saveRange.nelements() != range_p.nelements()) ||
        (!allEQ(saveRange, range_p)) ) {
      needStorageLattice_p = True;    
   }
   return True;
}

template <class T>
Bool LatticeStatistics<T>::setList (const Bool& doList)
//
// See if user wants to list statistics as well as plot them
//
{
   if (!goodParameterStatus_p) {
      return False;
   }
   doList_p = doList;

   return True;
} 


template <class T>
Bool LatticeStatistics<T>::setPlotting(PGPlotter& plotter,
                                       const Vector<Int>& statsToPlot,
                                       const Vector<Int>& nxy)
//
// Assign the desired PGPLOT device name and number
// of subplots
//
{     
   if (!goodParameterStatus_p) {
      return False;
   }

// Is new plotter attached ?
     
   if (!plotter.isAttached()) {
      if (haveLogger_p) {
         error_p = "Input plotter is not attached";
      }
      goodParameterStatus_p = False;
      return False;
   }

// Don't reattach to the same plotter.  The assignment will
// close the previous device

   if (plotter_p.isAttached()) {
      if (plotter_p.qid() != plotter.qid()) plotter_p = plotter;
   } else {
      plotter_p = plotter;
   }


// Make sure requested statistics are valid
// Set need robust statistics flag here as well

   statsToPlot_p.resize(0);
   statsToPlot_p = statsToPlot;
   for (uInt i=0; i<statsToPlot_p.nelements(); i++) {
      if (statsToPlot_p(i) < 0 || statsToPlot_p(i) > NSTATS-1) {
         error_p = "Invalid statistic requested for display";
         goodParameterStatus_p = False;
         return False;
      } 

// If the user wants robust stats, signal this and if they
// did not previously ask for them, signify we need to
// regenerate the storage lattice as well - the robust
// stats are just written directly into the storage lattice

      if (statsToPlot_p(i)==Int(LatticeStatsBase::MEDIAN) ||
          statsToPlot_p(i)==Int(LatticeStatsBase::MEDABSDEVMED) ||
          statsToPlot_p(i)==Int(LatticeStatsBase::QUARTILE)) {
         if (!doRobust_p) {
            needStorageLattice_p = True;
         }
         doRobust_p = True;
      }
   }   
   

// Plotting device and subplots.  nxy_p is set to [1,1] if zero length
 
   nxy_p.resize(0);
   nxy_p = nxy;
   ostrstream os;
   if (!LatticeStatsBase::setNxy(nxy_p, os)) {
      error_p = "Invalid number of subplots";
      goodParameterStatus_p = False;
      return False;
   }


// Set mean and sigma if no statistics requested

   if (statsToPlot_p.nelements()==0) {
      error_p = "No plot statistics requested, setting mean and sigma";
      statsToPlot_p.resize(2);
      statsToPlot_p(0) = MEAN;
      statsToPlot_p(1) = SIGMA;
   }

   return True;
}



template <class T>
Bool LatticeStatistics<T>::setNewLattice(const MaskedLattice<T>& lattice)
{ 
   if (!goodParameterStatus_p) {
      return False;
   }
//
   T* dummy = 0;
   DataType latticeType = whatType(dummy);
   if (latticeType !=TpFloat && latticeType!=TpComplex) {
      ostrstream oss;
      oss << "Statistics cannot yet be evaluated from lattices of type : " << latticeType << endl;
      error_p = String(oss);
      goodParameterStatus_p = False;
      return False;
   }

// Make a clone of the lattice

   if (pInLattice_p!=0) delete pInLattice_p;
   pInLattice_p = lattice.cloneML();

// This is the location of the input SubLattice in
// the parent Lattice

   blcParent_p = pInLattice_p->region().slicer().start();

// Signal that we have changed the lattice and need a new accumulation
// lattice

   needStorageLattice_p = True;
   return True;
}




template <class T>
Bool LatticeStatistics<T>::display()
// 
// This function displays (plotting and listing) the requested
// statistics as a function of the display axes
//
{
   if (!goodParameterStatus_p) {
     return False;
   }

// Do we have anything to do

   if (!doList_p && !plotter_p.isAttached() && haveLogger_p) {
      os_p << LogIO::NORMAL << "There is nothing to plot or list" << LogIO::POST;
     return True;
   }


// Set up some plotting things

   if (plotter_p.isAttached()) {
       plotter_p.subp(nxy_p(0), nxy_p(1));
       plotter_p.ask(True);
       plotter_p.sch (1.2);
       plotter_p.svp(0.1,0.9,0.1,0.9);
   }


// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;
   }


// If we don't have any display axes just summarise the lattice statistics

   if (displayAxes_p.nelements() == 0) {
     summStats ();
     return True;
   }

// Size of plotting abcissa axis

   const uInt n1 = pStoreLattice_p->shape()(0);

// Allocate ordinate arrays for plotting and listing.  Try to preserve
// the true Type of the data as long as we can.  Eventually, for 
// plotting we have to make it real valued

   Matrix<T> ord(n1,NSTATS);

// Iterate through storage lattice by planes (first and last axis of storage lattice)
// Specify which axes are the matrix  axes so that we can discard other
// degenerate axes with the matrixCursor function.   n1 is only 
// constrained to be n1 >= 1

   IPosition cursorShape(pStoreLattice_p->ndim(),1);
   cursorShape(0) = pStoreLattice_p->shape()(0);
   cursorShape(pStoreLattice_p->ndim()-1) = pStoreLattice_p->shape()(pStoreLattice_p->ndim()-1);

   IPosition matrixAxes(2);
   matrixAxes(0) = 0; 
   matrixAxes(1) = pStoreLattice_p->ndim()-1;

   LatticeStepper stepper(pStoreLattice_p->shape(), cursorShape,
                          matrixAxes, IPosition::makeAxisPath(pStoreLattice_p->ndim()));
   RO_LatticeIterator<T> pixelIterator(*pStoreLattice_p, stepper);

// Get beam area

   Double beamArea;
   Bool hasBeam = getBeamArea(beamArea);
//
   for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {
 
// Convert accumulations to  mean, sigma, and rms.   
 
      Matrix<T>  matrix(pixelIterator.matrixCursor());   // Reference semantics
      for (uInt i=0; i<n1; i++) {
         const T& nPts = matrix(i,NPTS);
         if (LattStatsSpecialize::hasSomePoints(nPts)) {
            ord(i,MEAN) = LattStatsSpecialize::getMean(matrix(i,SUM), nPts);
            if (hasBeam) ord(i,FLUX) = matrix(i,SUM) / beamArea;
            ord(i,VARIANCE) = LattStatsSpecialize::getVariance(matrix(i,SUM),
                                                                   matrix(i,SUMSQ), nPts);
            ord(i,SIGMA) = LattStatsSpecialize::getSigma(ord(i,VARIANCE));
            ord(i,RMS) =  LattStatsSpecialize::getRms(matrix(i,SUMSQ), nPts);  
          }
      }

// Extract the direct (NPTS, SUM etc) values from the cursor matrix into the plot matrix
// There is no easy way to do this other than as I have

      for (uInt i=0; i<LatticeStatsBase::NACCUM; i++) {
         for (uInt j=0; j<n1; j++) ord(j,i) = matrix(j,i);
      }


// Plot statistics

      if (plotter_p.isAttached()) {
        if (!plotStats (hasBeam, pixelIterator.position(), ord, plotter_p)) return False;
      }


// List statistics

      if (doList_p) {
         if (!listStats(hasBeam, pixelIterator.position(), ord)) return False;
      }
   }


// Finish up

   if (plotter_p.isAttached()) {
       plotter_p.updt();
   }
   return True;
}

template <class T>
Bool LatticeStatistics<T>::getMedian(Array<T>& stats)
// 
// This function retrieves the MEDIAN statistics from the
// accumulation lattice
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Retrieve storage array statistic

   if (!doRobust_p) needStorageLattice_p = True;
   doRobust_p = True;
   return retrieveStorageStatistic(stats, Int(MEDIAN));
}

 
template <class T>
Bool LatticeStatistics<T>::getMedAbsDevMed(Array<T>& stats)
// 
// This function retrieves the MEDABSDEVMED statistics from the
// accumulation lattice
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Retrieve storage array statistic

   if (!doRobust_p) needStorageLattice_p = True;
   doRobust_p = True;
   return retrieveStorageStatistic(stats, Int(MEDABSDEVMED));
}


template <class T>
Bool LatticeStatistics<T>::getQuartile(Array<T>& stats)
// 
// This function retrieves the QUARTILE statistics from the
// accumulation lattice
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Retrieve storage array statistic

   if (!doRobust_p) needStorageLattice_p = True;
   doRobust_p = True;
   return retrieveStorageStatistic(stats, Int(QUARTILE));
}




template <class T>
Bool LatticeStatistics<T>::getNPts(Array<T>& stats)
// 
// This function retrieves the NPTS statistics from the
// accumulation lattice
//
{
// Check class status

   if (!goodParameterStatus_p) {
     return False;
   }

// Retrieve storage array statistic

   return retrieveStorageStatistic(stats, Int(NPTS));
}


template <class T>
Bool LatticeStatistics<T>::getSum(Array<T>& stats)
// 
// This function retrieves the SUM statistics from the
// accumulation lattice
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }


// Retrieve storage array statistic

   return retrieveStorageStatistic(stats, Int(SUM));
}


template <class T>
Bool LatticeStatistics<T>::getStatistic (Array<T>& stats, 
                                         LatticeStatsBase::StatisticsTypes type)
{
   if (type==LatticeStatsBase::NPTS) {
      return getNPts(stats);
   } else if (type==LatticeStatsBase::SUM) {
      return getSum(stats);
   } else if (type==LatticeStatsBase::SUMSQ) {
      return getSumSquared(stats);
   } else if (type==LatticeStatsBase::MEDIAN) {
      return getMedian(stats);
   } else if (type==LatticeStatsBase::MEDABSDEVMED) {
      return getMedAbsDevMed(stats);
   } else if (type==LatticeStatsBase::QUARTILE) {
      return getQuartile(stats);
   } else if (type==LatticeStatsBase::MIN) {
      return getMin(stats);
   } else if (type==LatticeStatsBase::MAX) {
      return getMax(stats);
   } else if (type==LatticeStatsBase::VARIANCE) {
      return getVariance(stats);
   } else if (type==LatticeStatsBase::SIGMA) {
      return getSigma(stats);
   } else if (type==LatticeStatsBase::FLUX) {
      return getFluxDensity(stats);
   }
   return True;
}


template <class T>
Bool LatticeStatistics<T>::getStats(Vector<T>& stats,
                                    const IPosition& pos,
                                    const Bool posInLattice)
// 
// This function retrieves the statistics from the
// accumulation lattice at the specified location.  
//
// Inputs
//   posInLattice   If true the location is given as image coordinates
//                  The non-display axis values will be ignored.
//                  Otherwise the position should be for the
//                  display axes only.
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Retrieve storage array statistics

   stats.resize(NSTATS);
   if (!retrieveStorageStatistic(stats, pos, posInLattice)) return False;

// Compute the rest

   const T& n = stats(NPTS);
   if (!LattStatsSpecialize::hasSomePoints(n)) {
      stats.resize(0);
      return  True;
   }
//
   stats(MEAN) = LattStatsSpecialize::getMean(stats(SUM), n);
   stats(VARIANCE) = LattStatsSpecialize::getVariance(stats(SUM),
                                                      stats(SUMSQ), n);
   stats(SIGMA) = LattStatsSpecialize::getSigma(stats(VARIANCE));
   stats(RMS) =  LattStatsSpecialize::getRms(stats(SUMSQ), n);  
//
   Double beamArea;
   if (getBeamArea(beamArea)) {
      stats(FLUX) = stats(SUM) / beamArea;
   } else {
      stats(FLUX) = 0;
   }
//
   return True;
}


template <class T>
Bool LatticeStatistics<T>::getSumSquared (Array<T>& stats)
// 
// This function retrieves the SUMSQ statistics from the
// accumulation lattice
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }


// Retrieve storage array statistic

   return retrieveStorageStatistic (stats, Int(SUMSQ));
}

template <class T>
Bool LatticeStatistics<T>::getMin(Array<T>& stats)
// 
// This function retrieves the MIN statistics from the
// accumulation lattice
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
    }

// Retrieve storage array statistic

   return retrieveStorageStatistic(stats, Int(MIN));
}


template <class T>
Bool LatticeStatistics<T>::getMax(Array<T>& stats)
// 
// This function retrieves the MAX statistics from the
// accumulation lattice
//
{
// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Retrieve storage array statistic

   return retrieveStorageStatistic (stats, Int(MAX));
}



template <class T>
Bool LatticeStatistics<T>::getMean(Array<T>& stats)
// 
// This function calculates the MEAN statistics from the
// accumulation lattice
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(MEAN));
}


template <class T>
Bool LatticeStatistics<T>::getFluxDensity(Array<T>& stats)
// 
// This function calculates the Flux density from the
// accumulation lattice if it can.
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(FLUX));
}


template <class T>
Bool LatticeStatistics<T>::getSigma(Array<T>& stats)
// 
// This function calculates the SIGMA statistics from the
// accumulation lattice
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(SIGMA));
}

template <class T>
Bool LatticeStatistics<T>::getVariance(Array<T>& stats)
// 
// This function calculates the VARIANCE statistics from the
// accumulation lattice
//
{

// Check class status
 
   if (!goodParameterStatus_p) {
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(VARIANCE));
}


template <class T>
Bool LatticeStatistics<T>::getRms(Array<T>& stats)
// 
// This function calculates the RMS statistics from the
// accumulation lattice
//
{
 
// Check class status

   if (!goodParameterStatus_p) {
     return False; 
   }

// Do it

   return calculateStatistic(stats, Int(RMS));
}


template <class T>
Bool LatticeStatistics<T>::getMinMaxPos(IPosition& minPos, IPosition& maxPos)
{
   if (!goodParameterStatus_p) {
     return False; 
   }
//
   if (displayAxes_p.nelements() == 0) {
      minPos.resize(minPos_p.nelements());
      minPos = minPos_p;
      maxPos.resize(maxPos_p.nelements());
      maxPos = maxPos_p;
   } else {
      minPos.resize(0);
      maxPos.resize(0);
   }
   return True;
}



// Private functions


template <class T>
Bool LatticeStatistics<T>::calculateStatistic (Array<T>& slice, const Int& ISTAT)
//
// Calculate desired statistic from storage lattice and return in array
//
// Input/output:
//  slice      The statistics are returned in this array.  WIll be of zero 
//             size on output if there were no good points.
//
{

// Rezize slice to nothing first

   slice.resize(IPosition(0,0));


// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;
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

   if (ISTAT == MEAN) {
       Array<T> sum;
       retrieveStorageStatistic (sum, Int(SUM));
       ReadOnlyVectorIterator<T> sumIt(sum);

       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
             sliceIt.vector()(i) = 
                LattStatsSpecialize::getMean(sumIt.vector()(i),
                                             nPtsIt.vector()(i));
          }
          nPtsIt.next();
          sumIt.next();
          sliceIt.next();
       }
   } else if (ISTAT == FLUX) {
       Double beamArea;
       if (!getBeamArea(beamArea)) {
          slice.resize(IPosition(0,0));
          return False;
       }
//
       Array<T> sum;
       retrieveStorageStatistic (sum, Int(SUM));
       ReadOnlyVectorIterator<T> sumIt(sum);
       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
             if (LattStatsSpecialize::hasSomePoints(nPtsIt.vector()(i))) {
                sliceIt.vector()(i) = sumIt.vector()(i) / beamArea;
             }
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
             sliceIt.vector()(i) = 
                LattStatsSpecialize::getSigma(sumIt.vector()(i),
                                              sumSqIt.vector()(i),
                                              nPtsIt.vector()(i));
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
             sliceIt.vector()(i) = 
                LattStatsSpecialize::getVariance(sumIt.vector()(i),
                                                 sumSqIt.vector()(i),
                                                 nPtsIt.vector()(i));

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
             sliceIt.vector()(i) = 
                LattStatsSpecialize::getRms(sumSqIt.vector()(i),
                                            nPtsIt.vector()(i));
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
void LatticeStatistics<T>::closePlotting()
{
   if (plotter_p.isAttached()) plotter_p.detach();
}



template <class T>
Bool LatticeStatistics<T>::findNextDatum (uInt& iFound, 
                                        const uInt& n,
                                        const Vector<T>& mask,
                                        const uInt& iStart,
                                        const Bool& findGood) const
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
Bool LatticeStatistics<T>::findNextLabel (String& subLabel,
                                        Int& iLab,
                                        String& label) const
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
Bool LatticeStatistics<T>::generateStorageLattice()
//
// Iterate through the lattice and generate the storage lattice
// The shape of the storage lattice is n1, n2, ..., NACCUM
// where n1, n2 etc are the display axes
{

// Delete old storage lattice

   if (pStoreLattice_p != 0) delete pStoreLattice_p;

// Set the display axes vector (possibly already set in ::setAxes)

   displayAxes_p.resize(0);
   displayAxes_p = IPosition::otherAxes(pInLattice_p->ndim(),
                                        cursorAxes_p).asVector();


// Work out dimensions of storage lattice (statistics accumulations
// are along the last axis)

    IPosition storeLatticeShape;
    LatticeStatsBase::setStorageImageShape(storeLatticeShape, True, Int(LatticeStatsBase::NACCUM),
                                           displayAxes_p, pInLattice_p->shape());

// Set the storage lattice tile shape to the tile shape of the
// axes of the parent lattice from which it is created.  
// For the statistics axis, set the tile shape to NACCUM (small).

    IPosition tileShape(storeLatticeShape.nelements(),1);
    for (uInt i=0; i<tileShape.nelements()-1; i++) {
       tileShape(i) = pInLattice_p->niceCursorShape()(displayAxes_p(i));
    }
    tileShape(tileShape.nelements()-1) = storeLatticeShape(storeLatticeShape.nelements()-1);

// Create storage lattice.  If lattice is > 10% of available memory,
// put it on disk.

    uInt memory = AppInfo::memoryInMB();
    Double useMemory = Double(memory)/10.0;
    if (forceDisk_p) useMemory = 0.0;
    if (haveLogger_p) {
       os_p << LogIO::NORMAL 
            << "Creating new statistics storage lattice" << endl << LogIO::POST;
    }
    pStoreLattice_p = new TempLattice<T>(TiledShape(storeLatticeShape,
                                         tileShape), useMemory);

// Set up min/max location variables

    minPos_p.resize(pInLattice_p->shape().nelements());
    maxPos_p.resize(pInLattice_p->shape().nelements());


// Iterate through lattice and accumulate statistical sums

    StatsTiledCollapser<T> collapser(range_p, noInclude_p, noExclude_p,
                                     fixedMinMax_p);
    LattStatsProgress* pProgressMeter = 0;
    if (showProgress_p) pProgressMeter = new LattStatsProgress();

// This is the first output axis (there is only one in IS) getting 
// collapsed values.
// Output has to be a MaskedLattice, so make a writable SubLattice.

    Int newOutAxis = pStoreLattice_p->ndim()-1;
    SubLattice<T> outLatt (*pStoreLattice_p, True);
    LatticeApply<T>::tiledApply(outLatt, *pInLattice_p, 
                                collapser, IPosition(cursorAxes_p),
                                newOutAxis, pProgressMeter);
    if (pProgressMeter !=0) {
       delete pProgressMeter;
       pProgressMeter = 0;
    }
    collapser.minMaxPos(minPos_p, maxPos_p);

// Do robust statistics separately as required.

    generateRobust();
//
    needStorageLattice_p = False;     
    doneSomeGoodPoints_p = False;

    return True;
}

template <class T>
void LatticeStatistics<T>::generateRobust ()
{
   Bool showMsg = doRobust_p && displayAxes_p.nelements()==0;
   if (showMsg) os_p << "Computing robust statistics" << LogIO::POST;
//
   const uInt nCursorAxes = cursorAxes_p.nelements();
   const IPosition latticeShape(pInLattice_p->shape());
   IPosition cursorShape(pInLattice_p->ndim(),1);
   for (uInt i=0; i<nCursorAxes; i++) {
      cursorShape(cursorAxes_p(i)) = latticeShape(cursorAxes_p(i));
   }
//
   IPosition axisPath = cursorAxes_p;
   axisPath.append(displayAxes_p);
   LatticeStepper stepper(latticeShape, cursorShape, axisPath);
   for (stepper.reset(); !stepper.atEnd(); stepper++) {
      IPosition pos = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDIAN);
      IPosition pos2 = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDABSDEVMED);
      IPosition pos3 = locInStorageLattice(stepper.position(), LatticeStatsBase::QUARTILE);
//
      if (doRobust_p) {
         Slicer slicer(stepper.position(), stepper.endPosition(), Slicer::endIsLast);
         SubLattice<T> subLat(*pInLattice_p, slicer);
//
         LatticeExprNode node(median(subLat));
         T dummy;
         if (showMsg) os_p << "  Median" << LogIO::POST;
         T lelMed = LattStatsSpecialize::getNodeScalarValue(node, dummy);
         LatticeExprNode node2(median(abs(subLat-lelMed)));
         if (showMsg) os_p << "  Median of absolute deviations from median" << LogIO::POST;
         T lelMed2 = LattStatsSpecialize::getNodeScalarValue(node2, dummy);
//
         if (showMsg) os_p << "  Inter quartile range" << LogIO::POST;
         Array<T> data = subLat.get();
         Bool deleteData;
         T* pData = data.getStorage(deleteData);
         const uInt n = data.nelements();
         T iqa = GenSort<T>::kthLargest (pData, n, Int(0.25*n));
         T iqb = GenSort<T>::kthLargest (pData, n, Int(0.75*n));
         data.putStorage(pData, deleteData);
         T iqr = (iqb-iqa)/2.0;

// Whack it in storage lattice somewhere or other

         pStoreLattice_p->putAt(lelMed, pos);
         pStoreLattice_p->putAt(lelMed2, pos2);
         pStoreLattice_p->putAt(iqr, pos3);
      } else {

// Stick zero in storage lattice (it's not initialized)

         T val = 0;
         pStoreLattice_p->putAt(val, pos);
         pStoreLattice_p->putAt(val, pos2);
         pStoreLattice_p->putAt(val, pos3);
      }
   }
}

   

template <class T>
void LatticeStatistics<T>::lineSegments (uInt& nSeg,
                                       Vector<uInt>& start,
                                       Vector<uInt>& nPts,
                                       const Vector<T>& mask) const
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
void LatticeStatistics<T>::listMinMax(ostrstream& osMin,
                                      ostrstream& osMax,
                                      Int oWidth, DataType type)
//
// Min/max locations only meaningful for Float images currently.
// We report locations relative to the start of the parent lattice
//
{
   if (!fixedMinMax_p) {
      os_p << "Minimum value ";
      os_p.output() << setw(oWidth) << String(osMin);
      if (type==TpFloat) {
         os_p <<  " at " << blcParent_p + minPos_p+1;
      }
      os_p.post();
//
      os_p << "Maximum value ";
      os_p.output() << setw(oWidth) << String(osMax);
      if (type==TpFloat) {
         os_p <<  " at " << blcParent_p + maxPos_p+1 << endl;
      }
      os_p << endl;
      os_p.post();
   }
}

template <class T>
Bool LatticeStatistics<T>::listStats (Bool hasBeam, const IPosition& dPos,
                                      const Matrix<T>& stats)
//
// List the statistics for this row to the logger
//
// Inputs:
//   dPos    The location of the start of the cursor in the
//           storage lattice for this row
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

   Int oDWidth = 15;
   T* dummy = 0;
   DataType type = whatType(dummy);
   if (type==TpComplex) {
      oDWidth = 2*oDWidth + 3;    // (x,y)
   }

// Have to convert LogIO object to ostream before can apply
// the manipulators. Also formatting Complex numbers with
// the setw manipulator fails, so I go to a lot of trouble
// with ostrstreams (which are useable only once).

   Int oPrec = 6;   
   setStream(os_p.output(), oPrec);

// Write the pixel and world coordinate of the higher order display axes to the logger

   if (nDisplayAxes > 1) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);
      IPosition blc(pInLattice_p->ndim(),0);
      IPosition trc(pInLattice_p->shape()-1);
//
      for (uInt j=1; j<nDisplayAxes; j++) {
         os_p <<  "Axis " << displayAxes_p(j) + 1 << " = " 
              << locInLattice(dPos,True)(j)+1;
         if (j < nDisplayAxes-1) os_p << ", ";
      }
   }


// Find the width of the field into which we are going to write the coordinate value
// of the first display axis.  Do this by formatting a dummy value.

   Vector<String> sWorld(1);
   Vector<Double> pixels(1);
   pixels(0) = 1.0;
   IPosition blc(pInLattice_p->ndim(),0);
   IPosition trc(pInLattice_p->shape()-1);

   
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
//
   os_p.output() << setw(oDWidth) << "Npts";
   os_p.output() << setw(oDWidth) << "Sum";
   if (hasBeam) os_p.output() << setw(oDWidth) << "FluxDensity";
   os_p.output() << setw(oDWidth) << "Mean";  
   if (doRobust_p) os_p.output() << setw(oDWidth) << "Median"; 
   os_p.output() << setw(oDWidth) << "Rms";
   os_p.output() << setw(oDWidth) << "Sigma";
   os_p.output() << setw(oDWidth) << "Minimum";
   os_p.output() << setw(oDWidth) << "Maximum" << endl;

// Write statistics to logger.  We write the pixel location
// relative to the parent lattice

   const uInt n1 = stats.shape()(0);
   for (uInt j=0; j<n1; j++) {
      os_p.output() << setw(len0)     << j+blcParent_p(displayAxes_p(0))+1;
      ostrstream os00; setStream(os00, oPrec); 
      os00 << stats.column(NPTS)(j);
      os_p.output() << setw(oDWidth)   << String(os00);

//
      if (LattStatsSpecialize::hasSomePoints(stats.column(NPTS)(j))) {

// I hate ostrstreams.  The bloody things are one shot.

         ostrstream os0, os1, os2, os3, os4, os5, os6, os7, os8;
         setStream(os0, oPrec); setStream(os1, oPrec); setStream(os2, oPrec); 
         setStream(os3, oPrec); setStream(os4, oPrec); setStream(os5, oPrec);  
         setStream(os6, oPrec); setStream(os7, oPrec); setStream(os8, oPrec); 
//

         os0 << stats.column(SUM)(j);
         if (hasBeam) os1 << stats.column(FLUX)(j);
         os2 << stats.column(MEAN)(j);
         if (doRobust_p) os8 << stats.column(MEDIAN)(j);
         os3 << stats.column(RMS)(j);
         os4 << stats.column(SIGMA)(j);
         os5 << stats.column(MIN)(j);
         os6 << stats.column(MAX)(j);
//
         os_p.output() << setw(oDWidth)   << String(os0);
         if (hasBeam) os_p.output() << setw(oDWidth)   << String(os1);
         os_p.output() << setw(oDWidth)   << String(os2);
         if (doRobust_p) os_p.output() << setw(oDWidth)   << String(os8);
         os_p.output() << setw(oDWidth)   << String(os3);
         os_p.output() << setw(oDWidth)   << String(os4);
         os_p.output() << setw(oDWidth)   << String(os5);
         os_p.output() << setw(oDWidth)   << String(os6);
      }
      os_p.output() << endl;
   }
   os_p.post();

   return True;
}


template <class T>
IPosition LatticeStatistics<T>::locInLattice(const IPosition& storagePosition,
                                             Bool relativeToParent) const
                 
//
// Given a location in the storage lattice, convert those locations on
// the non-statistics axis (the statistics axis is the last one) to 
// account for the  location of the subLattice in the parent lattice
//
{  
   IPosition pos(storagePosition);
   for (uInt j=0; j<pos.nelements()-1; j++) {
     if (relativeToParent) {
        pos(j) = storagePosition(j) + blcParent_p(displayAxes_p(j));
     } else {
        pos(j) = storagePosition(j);
     }
   }
   return pos;
}


template <class T>
IPosition LatticeStatistics<T>::locInStorageLattice(const IPosition& latticePosition,
                                                    LatticeStatsBase::StatisticsTypes type) const
//
// Given a location in the input lattice, figure out where it lives
// in the storage lattice
//
{  
   uInt iType = uInt(type);
   if (iType >= uInt(LatticeStatsBase::NACCUM)) {
      throw(AipsError("Illegal statistics accumulation type"));
   }
//
   const uInt nDim = pStoreLattice_p->ndim();
   IPosition pos(nDim,0);
   pos(nDim-1) = iType;
//
   for (uInt j=0; j<displayAxes_p.nelements(); j++) {
      pos(j) = latticePosition(displayAxes_p(j));
   }
   return pos;
}



template <class T>
void LatticeStatistics<T>::multiColourYLabel (String& label,      
                                            PGPlotter& plotter,
                                            const String& LRLoc, 
                                            const Vector<uInt>& colours,
                                            const Int& nLabs) const

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
void LatticeStatistics<T>::multiPlot (PGPlotter& plotter,
                                      const Vector<T>& x,
                                      const Vector<T>& y,
                                      const Vector<T>& mask) const
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

   Vector<Float> xF, yF;
   for (uInt i=0; i<nSeg; i++) {
      const uInt ip = start(i);
      if (nPts(i) == 1) {
	  xF.resize(1); 
          yF.resize(1); 
          xF(0) = convertT(x(ip));
          yF(0) = convertT(y(ip));
	  plotter.pt (xF, yF, 1);
      } else {
	  xF.resize(nPts(i)); 
          yF.resize(nPts(i));
          for (uInt j=0; j<nPts(i); j++) {
             xF(j) = convertT(x(start(i)+j));
             yF(j) = convertT(y(start(i)+j));
          }
	  plotter.line (xF, yF);
      }
   }
}


template <class T>
void LatticeStatistics<T>::minMax (Bool& none,
                                   T& dMin, 
                                   T& dMax,  
                                   const Vector<T>& d,
                                   const Vector<T>& n) const
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
Int LatticeStatistics<T>::niceColour (Bool& initColours) const
{
   static Int colourIndex = 1;
   if (initColours) {
      colourIndex = 1;
      initColours = False;
   }
      
   colourIndex++;
   if (colourIndex == 4 || colourIndex == 14) colourIndex++;
   return colourIndex;
}


template <class T>
Bool LatticeStatistics<T>::plotStats (Bool hasBeam, 
                                      const IPosition& dPos,
                                      const Matrix<T>& stats,
                                      PGPlotter& plotter) 
//
// Plot the desired statistics.    
//
// Inputs:
//   dPos    The location of the start of the cursor in the 
//           storage lattice for this line 
//   stats   Statistics matrix
//
{

// The plotting for Complex just take the real part which is
// not very useful.  Until I do someting better, stub it out

   T* dummy = 0;
   DataType type = whatType(dummy);
   if (type!=TpFloat) {
      os_p << LogIO::WARN << "Plotting not yet available for complex images " << LogIO::POST;
      return True;
   }

// Work out what we are plotting

   const uInt n = statsToPlot_p.nelements();
   Bool doMean, doSigma, doVar, doRms, doSum, doSumSq;
   Bool doMin, doMax, doNPts, doFlux, doMedian;
   Bool doMedAbsDevMed, doQuartile;
   linearSearch(doMean, statsToPlot_p, Int(MEAN), n);
   linearSearch(doMedian, statsToPlot_p, Int(MEDIAN), n);
   linearSearch(doMedAbsDevMed, statsToPlot_p, Int(MEDABSDEVMED), n);
   linearSearch(doQuartile, statsToPlot_p, Int(QUARTILE), n);
   linearSearch(doSigma, statsToPlot_p, Int(SIGMA), n);
   linearSearch(doVar, statsToPlot_p, Int(VARIANCE), n);
   linearSearch(doRms, statsToPlot_p, Int(RMS), n);
   linearSearch(doSum, statsToPlot_p, Int(SUM), n);
   linearSearch(doSumSq, statsToPlot_p, Int(SUMSQ), n);
   linearSearch(doMin, statsToPlot_p, Int(MIN), n);
   linearSearch(doMax, statsToPlot_p, Int(MAX), n);
   linearSearch(doNPts, statsToPlot_p, Int(NPTS), n);
   linearSearch(doFlux, statsToPlot_p, Int(FLUX), n);
   if (!hasBeam) doFlux = False;
//
   Bool none;
   Bool first = True;
   Int nL = 0;
   Int nR = 0;

// Generate abcissa. Note that T(n) where T is COmplex results in (n+0i)

   const Int n1 = stats.shape()(0);
   Vector<T> abc(n1);
   for (Int j=0; j<n1; j++) abc(j) = convertF(Float(j+1));


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
   if (doMedian) {
      minMax(none, yLMin, yLMax, stats.column(MEDIAN), stats.column(NPTS));
      first = False;
      nL++;
   }
   if (doFlux) {
      minMax(none, yLMin, yLMax, stats.column(FLUX), stats.column(NPTS));
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
   if (doMedAbsDevMed) {
      minMax(none, yMin, yMax, stats.column(MEDABSDEVMED), stats.column(NPTS));
      if (first) {
         yRMin = yMin;
         yRMax = yMax;
      } else {
         yRMin = min(yRMin,yMin);
         yRMax = max(yRMax,yMax);
      }
      nR++;
   }
   if (doQuartile) {
      minMax(none, yMin, yMax, stats.column(QUARTILE), stats.column(NPTS));
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


// Set labels.

   String hLabel, xLabel;
   getLabels(hLabel, xLabel, dPos);
//
   String yLLabel = "";
   String yRLabel = "";

   Int nLLabs = 0;
   if (nL>0) {
      if (doMean) {
         yLLabel += "Mean,";
         nLLabs++;
      }
      if (doMedian) {
         yLLabel += "Median,";
         nLLabs++;
      }
      if (doFlux) {
         yLLabel += "Flux,";
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
      if (doMedAbsDevMed) {
         yRLabel += "MedAbsDevMed,";
         nRLabs++;
      }
      if (doQuartile) {
         yRLabel += "Quartile,";
         nRLabs++;
      }
      yRLabel.del(Int(yRLabel.length()-1),1);
   }
   
// Do plots.  Here we convert to real  for now.  To properly
// make this deal with Complex I will have to be cleverer

   Vector<uInt> lCols(nL);
   Vector<uInt> rCols(nR);
   Int ls = 0;
   Int i = -1;
   Bool initColours = True;
   plotter.page();

   if (nL>0) {
      plotter.swin(real(xMin), real(xMax), real(yLMin), real(yLMax));
      if (nR>0) {
         plotter.box("BCNST", 0.0, 0, "BNST", 0.0, 0);
      } else {
         plotter.box("BCNST", 0.0, 0, "BCNST", 0.0, 0);
      }
      plotter.lab(xLabel, "", "");

      if (doMean) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(MEAN), stats.column(NPTS));
      }
      if (doMedian) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(MEDIAN), stats.column(NPTS));
      }
      if (doFlux) {
         if (++ls > 5) ls = 1;
         plotter.sls (ls);

         lCols(++i) = niceColour (initColours);
         plotter.sci (lCols(i));

         multiPlot(plotter, abc, stats.column(FLUX), stats.column(NPTS));
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
      if (doMedAbsDevMed) {
         if (++ls > 5) ls = 1;
         plotter.sls(ls);

         rCols(++i) = niceColour (initColours);
         plotter.sci (rCols(i));

         multiPlot(plotter, abc, stats.column(MEDABSDEVMED), stats.column(NPTS));
      }
      if (doQuartile) {
         if (++ls > 5) ls = 1;
         plotter.sls(ls);

         rCols(++i) = niceColour (initColours);
         plotter.sci (rCols(i));

         multiPlot(plotter, abc, stats.column(QUARTILE), stats.column(NPTS));
      }

// Y label

      multiColourYLabel (yRLabel, plotter, "R", rCols, nRLabs);
   }
   plotter.sls(1);
   plotter.sci (1);


// Write values of other display axes on plot

   if (displayAxes_p.nelements()>1) {

// Write on plot
      
      Vector<Float> result(8);
      result = plotter.qtxt (0.0, 0.0, 0.0, 0.0, "X");
      Vector<Float> xb = result(Slice(0,4));
      Vector<Float> yb = result(Slice(4,4));
      Float dx = xb(3) - xb(0);
      result = plotter.qtxt (0.0, 0.0, 0.0, 0.0, hLabel.chars());
      xb = result(Slice(0,4));
      yb = result(Slice(4,4));
      Float dy = yb(1) - yb(0);

      Float mx = real(xMin) + dx;
      Float my;
      if (nR > 0) {
         my = real(yRMax) + 0.5*dy;
      } else {
         my = real(yLMax) + 0.5*dy;
      }

      Int tbg;
      tbg = plotter.qtbg();
      plotter.stbg(0);
      plotter.ptxt (mx, my, 0.0, 0.0, hLabel.chars());
      plotter.stbg(tbg);
   }
   return True;
}


// virtual functions

template <class T>
Bool LatticeStatistics<T>::getBeamArea (Double& beamArea) const
{
   beamArea = -1.0;
   return False;
}


template <class T>
void LatticeStatistics<T>::getLabels(String& hLabel, String& xLabel, const IPosition& dPos) const
//
// Get labels for top of plot and listing for the higher order axes
// and get the label for the X-axis when plotting
//
{
   ostrstream oss0;
   oss0 << "Axis " << displayAxes_p(0)+1 << " (pixels)";
   xLabel = String(oss0);
//
   const uInt n = displayAxes_p.nelements();
   hLabel =String("");
   if (n > 1) {
      ostrstream oss;
      for (uInt j=1; j<n; j++) {
         oss <<  "Axis " << displayAxes_p(j)+1 << "=" 
             << locInLattice(dPos,True)(j)+1;
         if (j < n-1) oss << ", ";
      }
      hLabel = String(oss);
   }
}



template <class T>
Bool LatticeStatistics<T>::retrieveStorageStatistic(Array<T>& slice, 
                                                  const Int& ISTAT)
//
// Retrieve values from accumulation lattice
//
// Input
//   ISTAT        Points at location of desired statistic in 
//                accumulation lattice (last axis)
// Input/output
//   slice        The statistics; should be of zero size on input
//
// Returns false if internal class state is bad.
{

// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;
   }


// Were there some good points ?  

   const Int nDim = pStoreLattice_p->ndim();
   slice.resize(IPosition(0,0));

   if (someGoodPoints()) {


// Get desired statistic slice. Discard degenerate axes (requires
// empty array on input)

      IPosition sliceShape(pStoreLattice_p->shape());
      sliceShape(nDim-1) = 1;

      IPosition pos(nDim,0);
      pos(nDim-1) = ISTAT;

      pStoreLattice_p->getSlice(slice, pos, sliceShape, 
                              IPosition(nDim,1), True);
   }
   return True;
}



template <class T>
Bool LatticeStatistics<T>::retrieveStorageStatistic(Vector<T>& slice, 
                                                  const IPosition& pos,
                                                  const Bool posInLattice)
//
// Retrieve values from accumulation lattice
//
// Input
//   pos          Locations for the display axes in the storage lattice
//   posInLattice If true the location is given as lattice coordinates
//                The non-display axis values will be ignored.
//                Otherwise the position should be for the
//                display axes only.
//
// Input/output
//   slice        The statistics; should be long enough on input
//
{


// Make sure we have a correctly size position

   if (posInLattice) {
      if (pos.nelements() != pInLattice_p->ndim()) {
         error_p = "Incorrectly sized position given";
         slice.resize(0);
         return False;
      }
   } else {
      if (pos.nelements() != displayAxes_p.nelements()) {
         error_p = "Incorrectly sized position given";
         slice.resize(0);
         return False;
      }
   }


// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;
   }


// Get accumulation sums slice from storage lattice.
// Last axis is statistics axis

   const uInt nDim = displayAxes_p.nelements();
   IPosition slicePos(nDim+1,0);
   if (posInLattice) {

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
   sliceShape(nDim) = LatticeStatsBase::NACCUM;
   Array<T> tSlice;
   pStoreLattice_p->getSlice(tSlice, slicePos, sliceShape, 
                           IPosition(nDim+1,1), False);

// Copy to vector      

   slicePos = 0;
   for (uInt i=0; i<LatticeStatsBase::NACCUM; i++) {
      slicePos(nDim) = i;
      slice(i) = tSlice(slicePos);         
   }
   return True;
}




template <class T>
Bool LatticeStatistics<T>::someGoodPoints ()
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

      if (pStoreLattice_p->ndim() == 1) {

// If storage lattice only 1D take cheap way out. Can't invoke
// retrieveStorageStatistic or we will be stuck in a time loop

         const IPosition shape = statsSliceShape();
         Array<T> stats(shape);
         IPosition pos(1,0);

         pStoreLattice_p->getSlice(stats, pos, shape, IPosition(1,1));
   
         pos(0) = NPTS;
         if (Int(real(stats(pos))+0.1) > 0) {
            someGoodPointsValue_p = True;
         } else {
            someGoodPointsValue_p = False;
         }
         return someGoodPointsValue_p;
      } else {


// Iterate through storage lattice by planes (first and last axis of storage lattice)
// Specify which axes are the matrix  axes so that we can discard other
// degenerate axes with the matrixCursor function.   n1 is only 
// constrained to be n1 >= 1

         IPosition cursorShape(pStoreLattice_p->ndim(),1);
         const Int n1 = pStoreLattice_p->shape()(0);
         cursorShape(0) = n1;
         cursorShape(pStoreLattice_p->ndim()-1) = pStoreLattice_p->shape()(pStoreLattice_p->ndim()-1);

         IPosition matrixAxes(2);
         matrixAxes(0) = 0; 
         matrixAxes(1) = pStoreLattice_p->ndim()-1;

         LatticeStepper stepper(pStoreLattice_p->shape(), cursorShape,
                                matrixAxes, IPosition::makeAxisPath(pStoreLattice_p->ndim()));
         RO_LatticeIterator<T> pixelIterator(*pStoreLattice_p, stepper);

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
IPosition LatticeStatistics<T>::statsSliceShape () const
// 
// Return the shape of a slice from the statistics storage
// lattice for a single spatial location.  The last axis
// is the statistics axis
{
   IPosition shape(pStoreLattice_p->ndim(),1);
   shape(pStoreLattice_p->ndim()-1) = 
      pStoreLattice_p->shape()(pStoreLattice_p->ndim()-1);
   return shape;
}



template <class T>
void LatticeStatistics<T>::summStats ()
// 
// List the summary of the statistics to the logger in the
// case that the statistics storage lattice is 1D only
//
{
// Fish out statistics with a slice

   const IPosition shape = statsSliceShape();
   Array<T> stats(shape);
   pStoreLattice_p->getSlice (stats, IPosition(1,0), shape, IPosition(1,1));
//
   IPosition pos(1);
//
   pos(0) = NPTS;
   T nPts = stats(pos);
//
   pos(0) = SUM;
   T sum = stats(pos);
//
   pos(0) = MEDIAN;
   T median = stats(pos);
//
   pos(0) = MEDABSDEVMED;
   T medAbsDevMed = stats(pos);
//
   pos(0) = QUARTILE;
   T quartile= stats(pos);
//
   pos(0) = SUMSQ;
   T sumSq = stats(pos);
//                         
   T mean = LattStatsSpecialize::getMean(sum, nPts);
   T var = LattStatsSpecialize::getVariance(sum, sumSq, nPts);
   T rms = LattStatsSpecialize::getRms(sumSq, nPts);
   T sigma = LattStatsSpecialize::getSigma(var);
//
   pos(0) = MIN;
   T dMin = stats(pos);
   pos(0) = MAX;
   T dMax = stats(pos);

// Get beam

   Double beamArea;
   Bool hasBeam = getBeamArea(beamArea);

// Have to convert LogIO object to ostream before can apply
// the manipulators.  Also formatting Complex numbers with
// the setw manipulator fails, so I go to a lot of trouble
// with ostrstreams (which are useable only once).

   const Int oPrec = 6;
   Int oWidth = 14;
   T* dummy = 0;
   DataType type = whatType(dummy);
   if (type==TpComplex) {
      oWidth = 32;
   }
   setStream(os_p.output(), oPrec);
   ostrstream os00, os0, os1, os2, os3, os4, os5, os6, os7, os8;
   ostrstream os9, os10;
   setStream(os00, oPrec); 
   setStream(os0, oPrec); setStream(os1, oPrec); setStream(os2, oPrec); 
   setStream(os3, oPrec); setStream(os4, oPrec); setStream(os5, oPrec);  
   setStream(os6, oPrec); setStream(os7, oPrec); setStream(os8, oPrec); 
   setStream(os9, oPrec); setStream(os10, oPrec); 
//
   os_p << endl << LogIO::POST;
   if (LattStatsSpecialize::hasSomePoints(nPts)) {
      os00 << nPts;
      os1 << sum; 
      os2 << mean; 
      os3 << var; 
      os4 << sigma;
      os5 << rms;
      os6 << dMin; 
      os7 << dMax; 
      os8 << median;
      os9 << medAbsDevMed;
      os10 << quartile;
      if (hasBeam) {
         os0 << sum/beamArea;
      }
//
      os_p << "Number points = ";
      os_p.output() << setw(oWidth) << String(os00) << "       Sum      = ";
      os_p.output() << setw(oWidth) << String(os1) << endl;
      os_p.post();
//
      if (hasBeam) {
         os_p << "Flux density  = ";
         os0 << sum/beamArea;
         os_p.output() << setw(oWidth) << String(os0) << " Jy" << endl;
         os_p.post();
      }
//
      os_p << "Mean          = ";
      os_p.output() << setw(oWidth) << String(os2);
      if (doRobust_p) {
         os_p.output()  << "       Median   = ";
         os_p.output() << setw(oWidth) << String(os8) << endl;
      }
      os_p.post();
//
      os_p << "Variance      = ";
      os_p.output() << setw(oWidth) << String(os3);
//
      if (var > 0.0) {
         os_p << "       Sigma    = ";
         os_p.output() << setw(oWidth) << String(os4) << endl;
         os_p.post();
      } else {
         os_p.post();
      }
//
      os_p << "Rms           = ";
      os_p.output() << setw(oWidth) << String(os5) << endl;
      os_p << endl;
      os_p.post();
//
      if (doRobust_p) {
         os_p << "MedAbsDevMed  = ";
         os_p.output() << setw(oWidth) << String(os9);
         os_p.output()  << "       Quartile = ";
         os_p.output() << setw(oWidth) << String(os10) << endl;
         os_p.post();
      }
//
      os_p << endl << LogIO::POST;
      listMinMax(os6, os7, oWidth, type);
   } else {
      os_p << "No valid points found " << LogIO::POST;
   }
   os_p << endl << LogIO::POST;
}

 
template <class T>
void LatticeStatistics<T>::stretchMinMax (T& dMin, T& dMax) const
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
void LatticeStatistics<T>::setStream (ostream& os, Int oPrec)
{
   os.fill(' '); 
   os.precision(oPrec);
   os.setf(ios::scientific, ios::floatfield);
   os.setf(ios::left, ios::adjustfield);
}





// StatsTiledCollapser


template <class T>
StatsTiledCollapser<T>::StatsTiledCollapser(const Vector<T>& pixelRange, 
                                            Bool noInclude, 
                                            Bool noExclude,
                                            Bool fixedMinMax)
: range_p(pixelRange),
  noInclude_p(noInclude),
  noExclude_p(noExclude),
  fixedMinMax_p(fixedMinMax),
  minPos_p(0),
  maxPos_p(0)
{;}


template <class T>
void StatsTiledCollapser<T>::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == LatticeStatsBase::NACCUM, AipsError);
}

template <class T>
void StatsTiledCollapser<T>::initAccumulator (uInt n1, uInt n3)
{
   pSum_p = new Block<NumericTraits<T>::PrecisionType>(n1*n3);
   pSumSq_p = new Block<NumericTraits<T>::PrecisionType>(n1*n3);
   pNPts_p = new Block<T>(n1*n3);
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
void StatsTiledCollapser<T>::process (uInt index1,
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
// lattices
//
{
   uInt index = index1 + index3*n1_p;
   NumericTraits<T>::PrecisionType& sum = (*pSum_p)[index];
   NumericTraits<T>::PrecisionType& sumSq = (*pSumSq_p)[index];
   T& nPts = (*pNPts_p)[index];
   T& dataMin = (*pMin_p)[index];
   T& dataMax = (*pMax_p)[index];
   Bool& minMaxInit = (*pInitMinMax_p)[index];

// If these are != -1 after the accumulating, then
// the min and max were updated

   Int minLoc = -1;
   Int maxLoc = -1;
// 
   T useIt;
   if (pInMask == 0) {

// All pixels are good

      if (!noInclude_p) {
          
// Inclusion range
     
         T datum;
         for (uInt i=0; i<nrval; i++) {
            datum = *pInData;
            useIt = LattStatsSpecialize::usePixelInc (range_p(0), range_p(1), datum);
            LattStatsSpecialize::accumulate(nPts, sum, sumSq, 
                   dataMin, dataMax, minLoc, maxLoc, minMaxInit, 
                   fixedMinMax_p, datum, i, useIt);
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
            useIt = LattStatsSpecialize::usePixelExc (range_p(0), range_p(1), datum);
            LattStatsSpecialize::accumulate(nPts, sum, sumSq, 
                  dataMin, dataMax, minLoc, maxLoc, minMaxInit, 
                  False, datum, i, useIt);
            pInData += inIncr;
         }
      } else {
 
// All data accepted

         LattStatsSpecialize::setUseItTrue(useIt);
         for (uInt i=0; i<nrval; i++) {
            LattStatsSpecialize::accumulate(nPts, sum, sumSq, 
                 dataMin, dataMax, minLoc, maxLoc, minMaxInit, 
                 False, *pInData, i, useIt);
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
            if (mask) {
               useIt = LattStatsSpecialize::usePixelInc (range_p(0), range_p(1), datum);
               LattStatsSpecialize::accumulate(nPts, sum, sumSq, 
                    dataMin, dataMax, minLoc, maxLoc, minMaxInit, 
                    False, datum, i, useIt);
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
            if (mask) {
              useIt = LattStatsSpecialize::usePixelExc (range_p(0), range_p(1), datum);
              LattStatsSpecialize::accumulate(nPts, sum, sumSq, 
                    dataMin, dataMax, minLoc, maxLoc, minMaxInit, 
                    False, datum, i, useIt);
            }
            pInData += inIncr;
            pInMask += inIncr;
         }
      } else {

// All data accepted

         LattStatsSpecialize::setUseItTrue(useIt);
         for (uInt i=0; i<nrval; i++) {
            if (*pInMask) {
               LattStatsSpecialize::accumulate(nPts, sum, sumSq, 
                     dataMin, dataMax, minLoc, maxLoc, minMaxInit, 
                     False, *pInData, i, useIt);
            }
            pInData += inIncr;
            pInMask += inIncr;
         }
      }
   }

// Update overall min and max location.  These are never updated
// if fixedMinMax is true.  These values are only meaningful for
// Float images.  For Complex they are useless currently.

   T* dummy = 0;
   DataType type = whatType(dummy);
//
   if (type==TpFloat) {
      if (minLoc != -1) {
        minPos_p = startPos + toIPositionInArray(minLoc, shape);
      }
      if (maxLoc != -1) {
        maxPos_p = startPos + toIPositionInArray(maxLoc, shape);
      }
   }
}

template <class T>
void StatsTiledCollapser<T>::endAccumulator(Array<T>& result,
                                            Array<Bool>& resultMask,
                                            const IPosition& shape)
{ 

// Reshape arrays.  The mask is always true.  Any locations
// in the storage lattice for which there were no valid points 
// will have the NPTS field set to zero.  That is what
// we use to effectively mask it.

    result.resize(shape);
    resultMask.resize(shape);
    resultMask.set(True);
// 
    Bool deleteRes;
    T* res = result.getStorage (deleteRes);
    T* resptr = res;
//
    const NumericTraits<T>::PrecisionType* sumPtr = pSum_p->storage();
    const NumericTraits<T>::PrecisionType* sumSqPtr = pSumSq_p->storage();
    const T* nPtsPtr = pNPts_p->storage();
    const T* minPtr = pMin_p->storage();
    const T* maxPtr = pMax_p->storage();
//
    uInt i,j,k;
    T* resptr_root = resptr;
    k = 0;
    for (i=0; i<n3_p; i++) {
       resptr = resptr_root + (Int(LatticeStatsBase::NPTS) * n1_p);
       for (j=0; j<n1_p; j++,k++) {   
          objcopy (resptr, nPtsPtr, n1_p); 
//          *resptr++ = T(*nPtsPtr++);
       }
//
       resptr = resptr_root + (Int(LatticeStatsBase::SUM) * n1_p);
       for (j=0; j<n1_p; j++,k++) {   
          *resptr++ = *sumPtr++;
       }
//
       resptr = resptr_root + (Int(LatticeStatsBase::SUMSQ) * n1_p);
       for (j=0; j<n1_p; j++,k++) {   
          *resptr++ = *sumSqPtr++;
       }
//
       resptr = resptr_root + (Int(LatticeStatsBase::MIN) * n1_p);
       objcopy (resptr, minPtr, n1_p); 
       resptr += n1_p;
       minPtr += n1_p;
//
       resptr = resptr_root + (Int(LatticeStatsBase::MAX) * n1_p);
       objcopy (resptr, maxPtr, n1_p); 
       resptr += n1_p;
       maxPtr += n1_p;
//
       resptr_root += n1_p * Int(LatticeStatsBase::NACCUM);
    }
//
    delete pSum_p;
    delete pSumSq_p;
    delete pNPts_p;
    delete pMin_p;
    delete pMax_p;
    delete pInitMinMax_p;
//
    result.putStorage (res, deleteRes);
}


template <class T>
void StatsTiledCollapser<T>::minMaxPos(IPosition& minPos, IPosition& maxPos)
{
   minPos.resize(minPos_p.nelements());
   minPos = minPos_p;
   maxPos.resize(maxPos_p.nelements());
   maxPos = maxPos_p;
}
