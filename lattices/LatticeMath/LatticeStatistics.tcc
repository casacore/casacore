//# LatticeStatistics.cc: generate statistics from a MaskedLattice
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003,2004
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

#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>
#include <casacore/lattices/LatticeMath/LattStatsProgress.h>

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/LatticeMath/LatticeApply.h>
#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.h>
#include <casacore/lattices/LatticeMath/MaskedLatticeStatsDataProvider.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicMath/ConvertScalar.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Utilities/PtrHolder.h>

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValType.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/OS/Timer.h>

#include <casacore/scimath/Mathematics/ClassicalStatistics.h>
#include <casacore/scimath/Mathematics/FitToHalfStatistics.h>
#include <casacore/scimath/Mathematics/HingesFencesStatistics.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LatticeStatistics<T>::LatticeStatistics (const MaskedLattice<T>& lattice,
                                         LogIO& os,  
                                         Bool showProgress,
                                         Bool forceDisk)
// 
// Constructor
//
: os_p(os),
  goodParameterStatus_p(True),
  haveLogger_p(True),
  fixedMinMax_p(False),
  doRobust_p(False),
  doList_p(False),
  error_p(""),
  pInLattice_p(0), 
  pStoreLattice_p(0),
  noInclude_p(True),
  noExclude_p(True),
  needStorageLattice_p(True),
  doneSomeGoodPoints_p(False),
  someGoodPointsValue_p(False),
  showProgress_p(showProgress),
  forceDisk_p(forceDisk),
  doneFullMinMax_p(False),
  _sa(), _algorithm(StatisticsData::CLASSICAL), _algConf() {
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
}


template <class T>
LatticeStatistics<T>::LatticeStatistics (const MaskedLattice<T>& lattice,
                                         Bool showProgress,
                                         Bool forceDisk)
// 
// Constructor
//
: goodParameterStatus_p(True),
  haveLogger_p(False),
  fixedMinMax_p(False),
  doRobust_p(False),
  doList_p(False),
  error_p(""),
  pInLattice_p(0), 
  pStoreLattice_p(0),
  noInclude_p(True),
  noExclude_p(True),
  needStorageLattice_p(True),
  doneSomeGoodPoints_p(False),
  someGoodPointsValue_p(False),
  showProgress_p(showProgress),
  forceDisk_p(forceDisk),
  doneFullMinMax_p(False),
  _sa(), _algorithm(StatisticsData::CLASSICAL), _algConf()
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
: pInLattice_p(0), pStoreLattice_p(0),
  _sa(), _algorithm(StatisticsData::CLASSICAL), _algConf()
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

      if (pInLattice_p!=0) {
    	  delete pInLattice_p;
      }
      pInLattice_p = other.pInLattice_p->cloneML();
// Delete storage lattice 

      if (pStoreLattice_p != 0) {
         delete pStoreLattice_p;
         pStoreLattice_p = 0;
      }
      needStorageLattice_p = True;
// Do the rest

      os_p = other.os_p;
	   cursorAxes_p.resize(other.cursorAxes_p.shape());
      cursorAxes_p = other.cursorAxes_p;
	   displayAxes_p.resize(other.displayAxes_p.size());
      displayAxes_p = other.displayAxes_p; 
      nxy_p.resize(other.nxy_p.size());
      nxy_p = other.nxy_p;
      statsToPlot_p.resize(other.statsToPlot_p.size());
      statsToPlot_p = other.statsToPlot_p; 
	   range_p.resize(other.range_p.size());
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
      minPos_p.resize(other.minPos_p.size());
      minPos_p = other.minPos_p; 
      maxPos_p.resize(other.maxPos_p.size());
      maxPos_p = other.maxPos_p;
      blcParent_p.resize(other.blcParent_p.size());
      blcParent_p = other.blcParent_p;
      forceDisk_p = other.forceDisk_p;
      doRobust_p = other.doRobust_p;
      doList_p = other.doList_p;
      error_p = other.error_p;
      doneFullMinMax_p= other.doneFullMinMax_p;
      minFull_p = other.minFull_p;
      maxFull_p = other.maxFull_p;
      _sa.resize(0);
      _algorithm = other._algorithm;
      _algConf = other._algConf;
   }
   return *this;
}

template <class T>
LatticeStatistics<T>::~LatticeStatistics() {
   delete pInLattice_p;
   pInLattice_p = 0;
   delete pStoreLattice_p;
   pStoreLattice_p = 0;
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

// Signal that we have changed the axes and need a new storage lattice

   if (saveAxes.nelements() != cursorAxes_p.nelements() ||
       !allEQ(saveAxes, cursorAxes_p)) needStorageLattice_p = True;

// Set the display axes vector.  We also do this in generateStorageLattice
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
      
   ostringstream os;
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


// Signal that we have changed the pixel range and need a new storage lattice
   
   if ( (saveNoInclude!=noInclude_p) ||
        (saveNoExclude!=noExclude_p) ||
        (saveFixedMinMax != fixedMinMax_p) ||
        (saveRange.nelements() != range_p.nelements()) ||
        (!allEQ(saveRange, range_p)) ) {
      needStorageLattice_p = True;    
      doneFullMinMax_p = False;
   }
//
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

      if (
    		  statsToPlot_p(i)==Int(LatticeStatsBase::MEDIAN)
    		  || statsToPlot_p(i)==Int(LatticeStatsBase::MEDABSDEVMED)
    		  || statsToPlot_p(i)==Int(LatticeStatsBase::QUARTILE)
    		  || statsToPlot_p(i)==Int(LatticeStatsBase::Q1)
    		  || statsToPlot_p(i)==Int(LatticeStatsBase::Q3)
      ) {
         if (!doRobust_p) {
            needStorageLattice_p = True;
         }
         doRobust_p = True;
      }
   }   
   

// Plotting device and subplots.  nxy_p is set to [1,1] if zero length
 
   nxy_p.resize(0);
   nxy_p = nxy;
   ostringstream os;
   if (!LatticeStatsBase::setNxy(nxy_p, os)) {
      error_p = "Invalid number of subplots";
      goodParameterStatus_p = False;
      return False;
   }


// Set mean and sigma if no statistics requested

   if (statsToPlot_p.nelements()==0) {
      error_p = "No plot statistics requested, setting mean and std dev";
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
      ostringstream oss;
      oss << "Statistics cannot yet be evaluated from lattices of type : " << latticeType << endl;
      error_p = oss.str();
      goodParameterStatus_p = False;
      return False;
   }

// Make a clone of the lattice

   if (pInLattice_p!=0) delete pInLattice_p;
   pInLattice_p = lattice.cloneML();

// This is the location of the input SubLattice in
// the parent Lattice

   blcParent_p = pInLattice_p->region().slicer().start();

// Signal that we have changed the lattice and need a new storage  lattice

   needStorageLattice_p = True;
   return True;
}


template <class T>
Bool LatticeStatistics<T>::getConvertedStatistic (Array<T>& stats, 
                                                  LatticeStatsBase::StatisticsTypes type,
                                                  Bool dropDeg)
{
   Array<AccumType> tmp;
   Bool ok = getStatistic(tmp, type, dropDeg);
   stats.resize(tmp.shape());
   convertArray(stats, tmp);
   return ok;
}


template <class T> Bool LatticeStatistics<T>::getStatistic(
	Array<AccumType>& stats,
	LatticeStatsBase::StatisticsTypes type,
	Bool dropDeg
) {
   if (!goodParameterStatus_p) {
     return False;
   }
   if (needStorageLattice_p) generateStorageLattice();
   if (type==LatticeStatsBase::NPTS) {
      return retrieveStorageStatistic(stats, NPTS, dropDeg);
   } else if (type==LatticeStatsBase::SUM) {
      return retrieveStorageStatistic(stats, SUM, dropDeg);
   } else if (type==LatticeStatsBase::SUMSQ) {
      return retrieveStorageStatistic(stats, SUMSQ, dropDeg);
   }
   else if (
		   type == LatticeStatsBase::MEDIAN
		   || type == LatticeStatsBase::MEDABSDEVMED
		   || type == LatticeStatsBase::QUARTILE
		   || type == LatticeStatsBase::Q1
		   || type == LatticeStatsBase::Q3
   ) {
	   if (!doRobust_p) {
		   doRobust_p = True;
		   generateRobust();
	   }
	   return retrieveStorageStatistic(stats, type, dropDeg);
   }
   else if (type==LatticeStatsBase::MIN) {
      return retrieveStorageStatistic(stats, MIN, dropDeg);
   } else if (type==LatticeStatsBase::MAX) {
      return retrieveStorageStatistic(stats, MAX, dropDeg);
   } else if (type==LatticeStatsBase::MEAN) {
	   // we prefer to calculate the mean rather than use the accumulated value
	   	   // because the accumulated value may include accumulated finite precision errors
	     // return retrieveStorageStatistic(stats, MEAN, dropDeg);
	   return calculateStatistic(stats, MEAN, dropDeg);
   } else if (type==LatticeStatsBase::VARIANCE) {
	      return retrieveStorageStatistic(stats, VARIANCE, dropDeg);
   } else if (type==LatticeStatsBase::SIGMA) {
      return calculateStatistic (stats, SIGMA, dropDeg);
   } else if (type==LatticeStatsBase::RMS) {
      return calculateStatistic (stats, RMS, dropDeg);
   } else if (type==LatticeStatsBase::FLUX) {
      return calculateStatistic (stats, FLUX, dropDeg);
   }
   return True;
}


template <class T>
Bool LatticeStatistics<T>::getStats(
	Vector<AccumType>& stats, const IPosition& pos,
	const Bool posInLattice
) {
	// This function retrieves the statistics from the storage
	// lattice at the specified location.

	// Inputs
	//   posInLattice   If true the location is given as image coordinates
	//                  The non-display axis values will be ignored.
	//                  Otherwise the position should be for the
	//                  display axes only.
	// Check class status
	if (!goodParameterStatus_p) {
		return False;
	}
	// Retrieve storage array statistics

	stats.resize(NSTATS);
	if (!retrieveStorageStatistic(stats, pos, posInLattice)) {
		return False;
	}
	// Compute the rest

	const AccumType& n = stats(NPTS);
	if (!LattStatsSpecialize::hasSomePoints(n)) {
		stats.resize(0);
		return  True;
	}
	//stats(MEAN) = LattStatsSpecialize::getMean(stats(SUM), n);
	//stats(VARIANCE) = LattStatsSpecialize::getVariance(stats(SUM),                                               stats(SUMSQ), n);
	stats(SIGMA) = LattStatsSpecialize::getSigma(stats(VARIANCE));
	stats(RMS) =  LattStatsSpecialize::getRms(stats(SUMSQ), n);
	stats(FLUX) = 0;
	if (_canDoFlux()) {
		Array<Double> beamArea;
		if (_getBeamArea(beamArea)) {
			IPosition beamPos = pos;
			if (posInLattice) {
				this->_latticePosToStoragePos(beamPos, pos);
			}
			stats(FLUX) = _flux(stats(SUM), beamArea(beamPos)).getValue();
		}
		else {
			stats(FLUX) = _flux(stats(SUM), 0).getValue();
		}
	}
	return True;
}

template <class T>
Bool LatticeStatistics<T>::getMinMaxPos(IPosition& minPos, IPosition& maxPos)
{
   if (!goodParameterStatus_p) {
     return False; 
   }

// Generate storage lattice if required
   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;
   }
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


template <class T>
Bool LatticeStatistics<T>::getFullMinMax(T& dataMin, T& dataMax)
{
  if (!doneFullMinMax_p) {

// Specialize

     LattStatsSpecialize::minMax (minFull_p, maxFull_p, pInLattice_p,
                                  range_p, noInclude_p, noExclude_p);
     doneFullMinMax_p = True;
  }
//
  dataMin = minFull_p;
  dataMax = maxFull_p;
//
   return (maxFull_p > minFull_p);
}



// Private functions


template <class T>
Bool LatticeStatistics<T>::calculateStatistic (Array<AccumType>& slice, 
                                               LatticeStatsBase::StatisticsTypes type,
                                               Bool dropDeg)
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
   Array<AccumType> nPts;
   retrieveStorageStatistic (nPts, NPTS, dropDeg);
   ReadOnlyVectorIterator<AccumType> nPtsIt(nPts);
   const uInt n1 = nPtsIt.vector().nelements();

// Setup
   slice.resize(nPts.shape());
   slice = 0.0;
   VectorIterator<AccumType> sliceIt(slice);
// Do it
   Array<AccumType> sum;
   Array<AccumType> sumSq;
   if (type==MEAN) {
	   retrieveStorageStatistic (sum, SUM, dropDeg);
       ReadOnlyVectorIterator<AccumType> sumIt(sum);
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
   }
   else if (type==FLUX) {
	   if (! _canDoFlux()) {
		   slice.resize(IPosition(0,0));
		   return False;
	   }
       Array<Double> beamArea;
       Bool gotBeamArea = _getBeamArea(beamArea);
       retrieveStorageStatistic (sum, SUM, dropDeg);
       ReadOnlyVectorIterator<AccumType> sumIt(sum);
       PtrHolder<ReadOnlyVectorIterator<Double> > beamAreaIter(
    		   gotBeamArea ? new ReadOnlyVectorIterator<Double>(beamArea) : 0
       );
       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
             if (LattStatsSpecialize::hasSomePoints(nPtsIt.vector()(i))) {
                //sliceIt.vector()(i) = sumIt.vector()(i) / beamAreaIter.vector()(i);
            	sliceIt.vector()(i) = _flux(
            		sumIt.vector()(i), gotBeamArea ? beamAreaIter->vector()(i) : 0
            	).getValue();
             }
          }
          nPtsIt.next();
          sumIt.next();
          sliceIt.next();
          if (gotBeamArea) {
        	  beamAreaIter->next();
          }
       }
    }
   else if (type==SIGMA) {
       Array<AccumType> variance;
       retrieveStorageStatistic (variance, VARIANCE, dropDeg);
       ReadOnlyVectorIterator<AccumType> varianceIt(variance);

       while (!nPtsIt.pastEnd()) {
          for (uInt i=0; i<n1; i++) {
              sliceIt.vector()(i) =
                 LattStatsSpecialize::getSigma(varianceIt.vector()(i));
          }
          nPtsIt.next();
          varianceIt.next();
          sliceIt.next();
       }
    }

    else if (type==RMS) {
       retrieveStorageStatistic (sumSq, SUMSQ, dropDeg);
       ReadOnlyVectorIterator<AccumType> sumSqIt(sumSq);
//
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
void LatticeStatistics<T>::setAlgorithm(
	StatisticsData::ALGORITHM algorithm
) {
	if (_algorithm != algorithm) {
		_algorithm = algorithm;
		_algConf.hf = -1;
		_algConf.ct = FitToHalfStatisticsData::CMEAN;
		_algConf.ud = FitToHalfStatisticsData::LE_CENTER;
		_algConf.cv = 0;
		needStorageLattice_p = True;
	}
}

template <class T>
void LatticeStatistics<T>::configureHingesFences(Double f) {
	ThrowIf(
		_algorithm != StatisticsData::HINGESFENCES,
		"Logic Error: _algorithm is not set to HingesFences"
	);
	if (! near(f, _algConf.hf)) {
		_algConf.hf = f;
		needStorageLattice_p = True;
	}
}

template <class T>
void LatticeStatistics<T>::configureFitToHalf(
	FitToHalfStatisticsData::CENTER centerType,
	FitToHalfStatisticsData::USE_DATA useData,
	AccumType centerValue
) {
	ThrowIf(
		_algorithm != StatisticsData::FITTOHALF,
		"Logic Error: _algorithm is not set to FITTOHALF"
	);
	if (
		centerType != _algConf.ct
		|| useData != _algConf.ud
		|| (
			centerType == FitToHalfStatisticsData::CVALUE
			&& ! near(centerValue, _algConf.cv)
		)
	) {
		_algConf.ct = centerType;
		_algConf.ud = useData;
		_algConf.cv = centerValue;
		needStorageLattice_p = True;
	}
}

template <class T>
Bool LatticeStatistics<T>::generateStorageLattice()

// Iterate through the lattice and generate the storage lattice
// The shape of the storage lattice is n1, n2, ..., NACCUM
// where n1, n2 etc are the display axes
{
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
    uInt memory = HostInfo::memoryTotal()/1024;
    Double useMemory = Double(memory)/10.0;
    if (forceDisk_p) useMemory = 0.0;
    if (haveLogger_p) {
       os_p << LogIO::NORMAL1
            << "Creating new statistics storage lattice of shape " << storeLatticeShape << endl << LogIO::POST;
    }
    delete pStoreLattice_p;
    pStoreLattice_p = new TempLattice<AccumType>(TiledShape(storeLatticeShape,
                                                 tileShape), useMemory);
// Set up min/max location variables

    //minPos_p.resize(pInLattice_p->shape().nelements());
    //maxPos_p.resize(pInLattice_p->shape().nelements());
    maxPos_p.resize(0);
    minPos_p.resize(0);


    CountedPtr<LattStatsProgress> pProgressMeter;
    if (showProgress_p) {
    	pProgressMeter = new LattStatsProgress();
    }
    const uInt nCursorAxes = cursorAxes_p.nelements();
    const IPosition latticeShape(pInLattice_p->shape());
    IPosition cursorShape(pInLattice_p->ndim(),1);
    for (uInt i=0; i<nCursorAxes; i++) {
    	cursorShape(cursorAxes_p(i)) = latticeShape(cursorAxes_p(i));
    }

    IPosition axisPath = cursorAxes_p;
    axisPath.append(displayAxes_p);
    LatticeStepper stepper(latticeShape, cursorShape, axisPath);
    uInt stepperSteps = pProgressMeter.null() 
        ? 0 : latticeShape.product()/cursorShape.product();
    DataRanges range;
    if (! noInclude_p || ! noExclude_p) {
    	range.push_back(std::pair<T, T>(range_p[0], range_p[1]));
    }

    T currentMax = 0;
    T currentMin = 0;
    T overallMax = 0;
    T overallMin = 0;
    Bool isReal = whatType(&currentMax);
    _sa.resize(storeLatticeShape.product()/storeLatticeShape.last());
    typename vector<CountedPtr<StatisticsAlgorithm<AccumType, const T*, const Bool*> > >::iterator curSA = _sa.begin();
    for (stepper.reset(); ! stepper.atEnd(); stepper++, ++curSA) {
        IPosition curPos = stepper.position();
    	IPosition posMax = locInStorageLattice(curPos, LatticeStatsBase::MAX);
    	IPosition posMean = locInStorageLattice(curPos, LatticeStatsBase::MEAN);
    	IPosition posMin = locInStorageLattice(curPos, LatticeStatsBase::MIN);
    	IPosition posNpts= locInStorageLattice(curPos, LatticeStatsBase::NPTS);
    	IPosition posSum = locInStorageLattice(curPos, LatticeStatsBase::SUM);
    	IPosition posSumsq = locInStorageLattice(curPos, LatticeStatsBase::SUMSQ);
    	IPosition posVariance = locInStorageLattice(curPos, LatticeStatsBase::VARIANCE);

    	// Create SubLattice from chunk
    	Slicer slicer(curPos, stepper.endPosition(), Slicer::endIsLast);
    	SubLattice<T> subLat(*pInLattice_p, slicer);
        if (! pProgressMeter.null() && stepper.atStart()) {
            uInt nSublatticeSteps = subLat.shape().product()/subLat.niceCursorShape().product();
            pProgressMeter->init(stepperSteps*nSublatticeSteps);
        }

    	// we use T rather than AccumType for the first templated variable because
    	// we lose no accuracy and if AccumType=complex<double> and T=complex<float>,
    	// this code will not compile

    	LatticeStatsDataProviderBase<T> *dataProvider = NULL;
    	if (subLat.isMasked()) {
    		dataProvider = new MaskedLatticeStatsDataProvider<T>(subLat);
    	}
    	else {
    		dataProvider = new LatticeStatsDataProvider<T>(subLat);
    	}

    	/*
    	if (! pProgressMeter.null()) {
    		dataProvider->setProgressMeter(pProgressMeter);
    	}
    	*/
    	// FIXME having Bool variables that are true when a fundamental property is negated
    	// is incredibly confusing and certainly not best practice. Rename and adjust
    	// the meaning of noInclude_p and noExclude_p
    	if (! noInclude_p || ! noExclude_p) {
    		dataProvider->setRanges(range, ! noInclude_p);
    	}

    	// its annoying that valid implicit casting of CountedPtr's won't compile, so
    	// we have to do this the hard way. The ThrowIf statement can be removed once
    	// this has been thoroughly exercised.
    	CountedPtr<StatsDataProvider<AccumType, const T*, const Bool*> > mydp
    		= dynamic_cast<StatsDataProvider<AccumType, const T*, const Bool*> *>(dataProvider);
    	ThrowIf (mydp.null(), "Logic Error: dynamic cast failed");
    	switch (_algorithm) {
    	case StatisticsData::CLASSICAL:
    		*curSA = new ClassicalStatistics<AccumType, const T*, const Bool*>();
    		break;
    	case StatisticsData::HINGESFENCES: {
    		*curSA = new HingesFencesStatistics<AccumType, const T*, const Bool*>(_algConf.hf);
    		break;
    	}
    	case StatisticsData::FITTOHALF: {
    		*curSA = new FitToHalfStatistics<AccumType, const T*, const Bool*>(
    			_algConf.ct, _algConf.ud, _algConf.cv
    		);
    		break;
    	}
    	default:
    		ThrowCc("Logic Error: Unhandled algorithm " + String::toString(_algorithm));
    	}

    	(*curSA)->setDataProvider(mydp);
    	if (! pProgressMeter.null()) {
    		dataProvider->setProgressMeter(CountedPtr<LattStatsProgress>(NULL));
    	}
    	StatsData<AccumType> stats = (*curSA)->getStatistics();
    	pStoreLattice_p->putAt(stats.mean, posMean);
    	pStoreLattice_p->putAt(stats.npts, posNpts);
    	pStoreLattice_p->putAt(stats.sum, posSum);
    	pStoreLattice_p->putAt(stats.sumsq, posSumsq);
    	pStoreLattice_p->putAt(stats.variance, posVariance);
    	if (fixedMinMax_p && ! noInclude_p) {
    		currentMax = range[0].second;
    	}
    	else if (! stats.max.null()) {
    		currentMax = *stats.max;
    	}
    	pStoreLattice_p->putAt(currentMax, posMax);
    	if (fixedMinMax_p && ! noInclude_p) {
    		currentMin = range[0].first;
    	}
    	else if (! stats.min.null()) {
    	    currentMin = *stats.min;
    	}
    	pStoreLattice_p->putAt(currentMin, posMin);
    	if (isReal) {
    		// CAUTION The way this has worked in the past apparently for
    		// lattices is that the max and min positions are representative
    		// of the *entire* lattice, and were not stored on a sublattice
    		// by sublattice basis. This is easy to fix now,
    		// but for backward compatibility, I'm leaving this functionality as
    		// it has been.
    		if (! fixedMinMax_p || noInclude_p) {
    			if (stepper.atStart()) {
    				IPosition myMaxPos, myMinPos;
    				dataProvider->minMaxPos(myMinPos, myMaxPos);
    				if (myMinPos.size() > 0) {
    					minPos_p = subLat.positionInParent(myMinPos);
    				}
    				if (myMaxPos.size() > 0) {
    					maxPos_p = subLat.positionInParent(myMaxPos);
    				}
    				overallMin = currentMin;
    				overallMax = currentMax;
    			}
    			else if (
    				currentMax > overallMax || currentMin < overallMin
    			) {
    				IPosition myMaxPos, myMinPos;
    				dataProvider->minMaxPos(myMinPos, myMaxPos);
    				if (currentMin < overallMin) {
    					if (myMinPos.size() > 0) {
    						minPos_p = subLat.positionInParent(myMinPos);
    					}
    					overallMin = currentMin;
    				}
    				if (currentMax > overallMax) {
    					if (myMaxPos.size() > 0) {
    						maxPos_p = subLat.positionInParent(myMaxPos);
    					}
    					overallMax = currentMax;
    				}
    			}
    		}
    	}
    }
    pProgressMeter = NULL;
    // Do robust statistics separately as required.
    generateRobust();
    needStorageLattice_p = False;     
    doneSomeGoodPoints_p = False;
    return True;
}

template <class T>
void LatticeStatistics<T>::generateRobust () {
	Bool showMsg = haveLogger_p && doRobust_p && displayAxes_p.nelements()==0;
	if (showMsg) os_p << LogIO::NORMAL1 << "Computing robust statistics" << LogIO::POST;

	const uInt nCursorAxes = cursorAxes_p.nelements();
	const IPosition latticeShape(pInLattice_p->shape());
	IPosition cursorShape(pInLattice_p->ndim(),1);
	for (uInt i=0; i<nCursorAxes; i++) {
		cursorShape(cursorAxes_p(i)) = latticeShape(cursorAxes_p(i));
	}
	IPosition axisPath = cursorAxes_p;
	axisPath.append(displayAxes_p);
	LatticeStepper stepper(latticeShape, cursorShape, axisPath);
	std::set<Double> quartiles;
	if (doRobust_p) {
		quartiles.insert(0.25);
		quartiles.insert(0.75);
	}
	std::map<Double, AccumType> quantileToValue;
    typename vector<CountedPtr<StatisticsAlgorithm<AccumType, const T*, const Bool*> > >::iterator curSA = _sa.begin();
	for (stepper.reset(); ! stepper.atEnd(); stepper++) {
		IPosition pos = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDIAN);
		IPosition pos2 = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDABSDEVMED);
		IPosition pos3 = locInStorageLattice(stepper.position(), LatticeStatsBase::QUARTILE);
		IPosition posQ1 = locInStorageLattice(stepper.position(), LatticeStatsBase::Q1);
		IPosition posQ3 = locInStorageLattice(stepper.position(), LatticeStatsBase::Q3);
		if (! doRobust_p) {
			// Stick zero in storage lattice (it's not initialized)
			AccumType val(0);
			pStoreLattice_p->putAt(val, pos);
			pStoreLattice_p->putAt(val, pos2);
			pStoreLattice_p->putAt(val, pos3);
			pStoreLattice_p->putAt(val, posQ1);
			pStoreLattice_p->putAt(val, posQ3);
			continue;
		}
		quantileToValue.clear();
		// computing the median and the quartiles simultaneously minimizes
		// the number of necessary data scans, as opposed to first calling
		// getMedian() and getQuartiles() separately
		AccumType median = (*curSA)->getMedianAndQuantiles(quantileToValue, quartiles);
		AccumType q1 = quantileToValue[0.25];
		AccumType q3 = quantileToValue[0.75];
		AccumType iqr = q3 - q1;
		AccumType medabsdevmed = (*curSA)->getMedianAbsDevMed();
		// Whack results into storage lattice
		pStoreLattice_p->putAt(median, pos);
		pStoreLattice_p->putAt(medabsdevmed, pos2);
		pStoreLattice_p->putAt(iqr, pos3);
		pStoreLattice_p->putAt(q1, posQ1);
		pStoreLattice_p->putAt(q3, posQ3);
		++curSA;
	}
}

template <class T>
void LatticeStatistics<T>::listMinMax(ostringstream& osMin,
                                      ostringstream& osMax,
                                      Int oWidth, DataType type)

// Min/max locations only meaningful for Float images currently.
// We report locations relative to the start of the parent lattice
{
   if (!fixedMinMax_p) {
       os_p << LogIO::NORMAL << "Minimum value ";
      os_p.output() << setw(oWidth) << String(osMin);
      if (type==TpFloat && minPos_p.size() > 0) {
         os_p <<  " at " << blcParent_p + minPos_p+1;
      }
      os_p.post();
      os_p << "Maximum value ";
      os_p.output() << setw(oWidth) << String(osMax);
      if (type==TpFloat && maxPos_p.size() > 0) {
         os_p <<  " at " << blcParent_p + maxPos_p+1 << endl;
      }
      os_p << endl;
      os_p.post();
   }
}

template <class T>
Bool LatticeStatistics<T>::listStats (Bool hasBeam, const IPosition& dPos,
                                      const Matrix<AccumType>& stats)
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
// with ostringstreams (which are useable only once).

   Int oPrec = 6;   
   setStream(os_p.output(), oPrec);

// Write the pixel and world coordinate of the higher order display axes to the logger

   if (nDisplayAxes > 1) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);
      IPosition blc(pInLattice_p->ndim(),0);
      IPosition trc(pInLattice_p->shape()-1);
//
      os_p << LogIO::NORMAL;
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

   os_p << LogIO::NORMAL << endl;
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
   os_p.output() << setw(oDWidth) << "Npts";
   os_p.output() << setw(oDWidth) << "Sum";
   if (_canDoFlux()) os_p.output() << setw(oDWidth) << "FluxDensity";
   os_p.output() << setw(oDWidth) << "Mean";  
   if (doRobust_p) os_p.output() << setw(oDWidth) << "Median"; 
   os_p.output() << setw(oDWidth) << "Rms";
   os_p.output() << setw(oDWidth) << "Std dev";
   os_p.output() << setw(oDWidth) << "Minimum";
   os_p.output() << setw(oDWidth) << "Maximum" << endl;

// Write statistics to logger.  We write the pixel location
// relative to the parent lattice

   const uInt n1 = stats.shape()(0);
   for (uInt j=0; j<n1; j++) {
      os_p.output() << setw(len0)     << j+blcParent_p(displayAxes_p(0))+1;
      ostringstream os00; setStream(os00, oPrec); 
      os00 << stats.column(NPTS)(j);
      os_p.output() << setw(oDWidth)   << os00.str();

      if (LattStatsSpecialize::hasSomePoints(stats.column(NPTS)(j))) {

// Convert to strings.

         ostringstream os0, os1, os2, os3, os4, os5, os6, os7, os8, os9;
         setStream(os0, oPrec); setStream(os1, oPrec); setStream(os2, oPrec); 
         setStream(os3, oPrec); setStream(os4, oPrec); setStream(os5, oPrec);  
         setStream(os6, oPrec); setStream(os7, oPrec); setStream(os8, oPrec); 
         setStream(os9, oPrec); 

         os0 << stats.column(SUM)(j);
         if (_canDoFlux()) os1 << stats.column(FLUX)(j);
         os2 << stats.column(MEAN)(j);
         if (doRobust_p) os8 << stats.column(MEDIAN)(j);
         os3 << stats.column(RMS)(j);
         os4 << stats.column(SIGMA)(j);
         os5 << stats.column(MIN)(j);
         os6 << stats.column(MAX)(j);

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
Bool LatticeStatistics<T>::getLayerStats(
        String& stats, Double area, 
        Int zAxis, Int zLayer, Int hAxis, Int hLayer) { 

   if (!goodParameterStatus_p) {
     return False;
   }

   if (needStorageLattice_p) {
       if (!generateStorageLattice()) { 
         return False;
      }
   }

   if (displayAxes_p.nelements() == 0) {
      const IPosition shape = statsSliceShape();
      Array<AccumType> statsV(shape);
      pStoreLattice_p->getSlice (statsV, IPosition(1,0), shape, IPosition(1,1));
      IPosition pos(1);
      pos(0) = NPTS;
      AccumType nPts = statsV(pos);
      pos(0) = SUM;
      AccumType  sum = statsV(pos);
      pos(0) = MEDIAN;
      AccumType  median = statsV(pos);
      pos(0) = MEDABSDEVMED;
      //AccumType  medAbsDevMed = statsV(pos);
      pos(0) = QUARTILE;
      //AccumType  quartile= statsV(pos);
      pos(0) = SUMSQ;
      AccumType  sumSq = statsV(pos);
      // pos(0) = MEAN;
      // AccumType  mean = statsV(pos);
      pos(0) = VARIANCE;
      AccumType  var = statsV(pos);

      // prefer the calculated mean over the accumulated mean because
      // the accumulated mean can have accumulated precision errors
      AccumType  mean = LattStatsSpecialize::getMean(sum, nPts);
      // AccumType  var = LattStatsSpecialize::getVariance(sum, sumSq, nPts);
      AccumType  rms = LattStatsSpecialize::getRms(sumSq, nPts);
      AccumType  sigma = LattStatsSpecialize::getSigma(var);

      pos(0) = MIN;
      AccumType  dMin = statsV(pos);
      pos(0) = MAX;
      AccumType  dMax = statsV(pos);

      if (!LattStatsSpecialize::hasSomePoints(nPts)) 
          return False;

      stringstream os;
      const Int oPrec = 6;
      Int oDWidth = 15;
      T* dummy = 0;
      DataType type = whatType(dummy);
      if (type==TpComplex) {
         oDWidth = 2*oDWidth + 3; 
      }

      os.setf(ios::left, ios::adjustfield);
      os << setw(10) << "Npts";
      os << setw(oDWidth) << "Sum";
      if (_canDoFlux())
         os << setw(oDWidth) << "Flux (Jy)";
      os << setw(oDWidth) << "Mean";  
      if (doRobust_p) 
         os << setw(oDWidth) << "Median"; 
      os << setw(oDWidth) << "Rms";
      os << setw(oDWidth) << "Std dev";
      os << setw(oDWidth) << "Minimum";
      os << setw(oDWidth) << "Maximum" << endl;

      os.fill(' '); 
      os.precision(0);
      os.setf(ios::fixed, ios::floatfield);
      os.setf(ios::left, ios::adjustfield);
      os << setw(10)
         << nPts;

      setStream(os, oPrec);
      os << setw(oDWidth)
         << sum;
      if (_canDoFlux()) {
            setStream(os, oPrec);
            os << setw(oDWidth)
               << _flux(sum, area);
       }
       setStream(os, oPrec);
       os << setw(oDWidth)
          << mean;
       if (doRobust_p){ 
          setStream(os, oPrec);
          os << setw(oDWidth)
             << median;
       }
       setStream(os, oPrec);
       os << setw(oDWidth)
          << rms;
       setStream(os, oPrec);
       os << setw(oDWidth)
          << sigma;
       setStream(os, oPrec);
       os << setw(oDWidth)
          << dMin;
       setStream(os, oPrec);
       os << setw(oDWidth)
          << dMax;

      /*
      if (!fixedMinMax_p) {
         os << "Minimum value ";
         os << setw(oWidth) << String(os6);
         if (type==TpFloat) {
            os <<  " at " << blcParent_p + minPos_p+1;
         }

         os << "Maximum value ";
         os << setw(oWidth) << String(os7);
         if (type==TpFloat) {
            os <<  " at " << blcParent_p + maxPos_p+1 << endl;
         }
      }
      */

      stats += os.str();
      stats += '\n';
      return True;
   }

   const uInt n1 = pStoreLattice_p->shape()(0);

   Matrix<AccumType> ord(n1,NSTATS);

   IPosition cursorShape(pStoreLattice_p->ndim(),1);
   cursorShape(0) = pStoreLattice_p->shape()(0);
   cursorShape(pStoreLattice_p->ndim()-1) = 
       pStoreLattice_p->shape()(pStoreLattice_p->ndim()-1);
   IPosition matrixAxes(2);
   matrixAxes(0) = 0; 
   matrixAxes(1) = pStoreLattice_p->ndim()-1;

   LatticeStepper 
       stepper(pStoreLattice_p->shape(), cursorShape, matrixAxes, 
                IPosition::makeAxisPath(pStoreLattice_p->ndim()));
   RO_LatticeIterator<AccumType> 
       pixelIterator(*pStoreLattice_p, stepper);
   uInt zAx = -1;
   uInt hAx = -1;
   for (uInt j=0; j<displayAxes_p.nelements(); j++) {
      if (zAxis == displayAxes_p(j))
         zAx = j;
      if (hAxis == displayAxes_p(j))
         hAx = j;
   }

   Int layer = 0;
   ostringstream os;
   for (pixelIterator.reset(); 
        !pixelIterator.atEnd(); pixelIterator++) {
      IPosition dPos = pixelIterator.position();
      if (displayAxes_p.nelements() == 2) {
    	  if (zAx == 1) {
    		  if (dPos[1] != zLayer) {
    			  continue;
    		  }
    		  else {
    			  layer = hLayer;
    		  }
    	  }
    	  if (hAx == 1) {
    		  if (dPos[1] != hLayer) {
    			  continue;
    		  }
    		  else {
    			  layer = zLayer;
    		  }
    	  }
      }
      if (displayAxes_p.nelements() == 1) {
         layer = zLayer;
      }

      Matrix<AccumType>  matrix(pixelIterator.matrixCursor());  
      for (uInt i=0; i<n1; i++) {
         const AccumType& nPts = matrix(i,NPTS);
         if (LattStatsSpecialize::hasSomePoints(nPts)) {
            ord(i,MEAN) = 
               LattStatsSpecialize::getMean(matrix(i,SUM), nPts);
            if (_canDoFlux()) {
            	ord(i,FLUX) = _flux(matrix(i,SUM), area).getValue();
            }
            /*
            ord(i,VARIANCE) = LattStatsSpecialize::getVariance(
                              matrix(i,SUM), matrix(i,SUMSQ), nPts);
                              */
            ord(i,SIGMA) = LattStatsSpecialize::getSigma(
                              matrix(i,VARIANCE));
            ord(i,RMS) =  LattStatsSpecialize::getRms(
                              matrix(i,SUMSQ), nPts);  
          }
      }

      for (uInt i=0; i<LatticeStatsBase::NACCUM; i++) {
         for (uInt j=0; j<n1; j++) ord(j,i) = matrix(j,i);
      }

      listLayerStats(ord, os, layer);
      break;
   }
   stats += os.str();
   stats += '\n';

   return True;
}

template <class T>
Bool LatticeStatistics<T>::getLayerStats(
	stat_list &stats, Double area,
        Int zAxis, Int zLayer, Int hAxis, Int hLayer) {

    char buffer[256];

    if (!goodParameterStatus_p) {
	return False;
    }

    if (needStorageLattice_p) {
	if (!generateStorageLattice()) {
	    return False;
	}
    }

    if (displayAxes_p.nelements() == 0) {
	const IPosition shape = statsSliceShape();
	Array<AccumType> statsV(shape);
	pStoreLattice_p->getSlice (statsV, IPosition(1,0), shape, IPosition(1,1));
	IPosition pos(1);
	pos(0) = NPTS;
	int nPts = (int)statsV(pos);
	pos(0) = SUM;
	AccumType  sum = statsV(pos);
	pos(0) = MEDIAN;
	AccumType  median = statsV(pos);
	pos(0) = MEDABSDEVMED;
	//AccumType  medAbsDevMed = statsV(pos);
	pos(0) = QUARTILE;
	//AccumType  quartile= statsV(pos);
	pos(0) = SUMSQ;
	AccumType  sumSq = statsV(pos);
	// pos(0) = MEAN;
	// AccumType  mean = statsV(pos);
	pos(0) = VARIANCE;
	AccumType  var = statsV(pos);

	// prefer the calculated mean over the accumulated mean because
	// the accumulated mean can have accumulated precision errors
	AccumType  mean = LattStatsSpecialize::getMean(sum, nPts);
	// AccumType  var = LattStatsSpecialize::getVariance(sum, sumSq, nPts);
	AccumType  rms = LattStatsSpecialize::getRms(sumSq, nPts);
	AccumType  sigma = LattStatsSpecialize::getSigma(var);

	pos(0) = MIN;
	AccumType  dMin = statsV(pos);
	pos(0) = MAX;
	AccumType  dMax = statsV(pos);

	if (!LattStatsSpecialize::hasSomePoints(nPts))
	    return False;

	//const Int oPrec = 6;
	Int oDWidth = 15;
	T* dummy = 0;
	DataType type = whatType(dummy);
	if (type==TpComplex) {
	    oDWidth = 2*oDWidth + 3;
	}

	sprintf( buffer, "%d", nPts );
	stats.push_back(stat_element("Npts",buffer));

	sprintf( buffer, "%e", sum );
	stats.push_back(stat_element("Sum",buffer));


	if ( _canDoFlux()) {
	    sprintf( buffer, "%e", _flux(sum, area ).getValue());
	    stats.push_back(stat_element("FluxDensity",buffer));
	}

	sprintf( buffer, "%e", mean );
	stats.push_back(stat_element("Mean",buffer));

	if (doRobust_p) {
	    sprintf( buffer, "%e", median );
	    stats.push_back(stat_element("Median",buffer));
	}

	sprintf( buffer, "%e", rms );
	stats.push_back(stat_element("Rms",buffer));

	sprintf( buffer, "%e", sigma );
	stats.push_back(stat_element("Std dev",buffer));

	sprintf( buffer, "%e", dMin );
	stats.push_back(stat_element("Minimum",buffer));

	sprintf( buffer, "%e", dMax );
	stats.push_back(stat_element("Maximum",buffer));

	return True;
    }

    const uInt n1 = pStoreLattice_p->shape()(0);

    Matrix<AccumType> ord(n1,NSTATS);

    IPosition cursorShape(pStoreLattice_p->ndim(),1);
    cursorShape(0) = pStoreLattice_p->shape()(0);
    cursorShape(pStoreLattice_p->ndim()-1) = pStoreLattice_p->shape()(pStoreLattice_p->ndim()-1);
    IPosition matrixAxes(2);
    matrixAxes(0) = 0;
    matrixAxes(1) = pStoreLattice_p->ndim()-1;

    LatticeStepper stepper( pStoreLattice_p->shape(), cursorShape, matrixAxes,
			    IPosition::makeAxisPath(pStoreLattice_p->ndim()));
    RO_LatticeIterator<AccumType> pixelIterator(*pStoreLattice_p, stepper);
    uInt zAx = -1;
    uInt hAx = -1;
    for (uInt j=0; j<displayAxes_p.nelements(); j++) {
	if (zAxis == displayAxes_p(j))
	    zAx = j;
	if (hAxis == displayAxes_p(j))
	    hAx = j;
    }

    Int layer = 0;
    for ( pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++ ) {
	IPosition dPos = pixelIterator.position();
        if (displayAxes_p.nelements() == 2) {
            if (zAx == 1) {
                if (dPos[1] != zLayer) {
                    continue;
                } else {
                    layer = hLayer;
                }
            }
            if (hAx == 1) {
                if (dPos[1] != hLayer) {
                    continue;
                } else {
                    layer = zLayer;
                }
            }
        }
        if (displayAxes_p.nelements() == 1) {
            layer = zLayer;
        }
	Matrix<AccumType>  matrix(pixelIterator.matrixCursor());
	for (uInt i=0; i<n1; i++) {
	    const AccumType& nPts = matrix(i,NPTS);
	    if (LattStatsSpecialize::hasSomePoints(nPts)) {
		ord(i,MEAN) = LattStatsSpecialize::getMean(matrix(i,SUM), nPts);
		if (_canDoFlux()) {
			ord(i,FLUX) = _flux(matrix(i,SUM), area).getValue();
		}
		//ord(i,VARIANCE) = LattStatsSpecialize::getVariance( matrix(i,SUM), matrix(i,SUMSQ), nPts);
		ord(i,SIGMA) = LattStatsSpecialize::getSigma(matrix(i,VARIANCE));
		ord(i,RMS) =  LattStatsSpecialize::getRms(matrix(i,SUMSQ), nPts);
	    }
	}

	for (uInt i=0; i<LatticeStatsBase::NACCUM; i++) {
	    for (uInt j=0; j<n1; j++) ord(j,i) = matrix(j,i);
	}

	//const uInt nDisplayAxes = displayAxes_p.nelements();
	const uInt n1 = ord.shape()(0);

	Int oDWidth = 15;
	T* dummy = 0;
	DataType type = whatType(dummy);
	if (type==TpComplex) {
	    oDWidth = 2*oDWidth + 3;
	}

	//Int oPrec = 6;

	Vector<String> sWorld(1);
	Vector<Double> pixels(1);
	pixels(0) = 1.0;
	IPosition blc(pInLattice_p->ndim(),0);
	IPosition trc(pInLattice_p->shape()-1);

	//Write statistics to logger.  We write the pixel location
	//relative to the parent lattice
	for (uInt j=0; j<n1; j++) {
	    if (layer == (Int)j || n1 == 1)  {

		sprintf( buffer, "%d", (int) ord.column(NPTS)(j) );
		stats.push_back(stat_element("Npts",buffer));

		if ( LattStatsSpecialize::hasSomePoints(ord.column(NPTS)(j)) ){

		    sprintf( buffer, "%e", ord.column(SUM)(j) );
		    stats.push_back(stat_element("Sum",buffer));

		    if (_canDoFlux()) {
			sprintf( buffer, "%e", ord.column(FLUX)(j) );
			stats.push_back(stat_element("FluxDensity",buffer));
		    }

		    sprintf( buffer, "%e", ord.column(MEAN)(j) );
		    stats.push_back(stat_element("Mean",buffer));

		    if (doRobust_p){
			sprintf( buffer, "%e", ord.column(MEDIAN)(j) );
			stats.push_back(stat_element("Median",buffer));
		    }

		    sprintf( buffer, "%e", ord.column(RMS)(j) );
		    stats.push_back(stat_element("Rms",buffer));

		    sprintf( buffer, "%e", ord.column(SIGMA)(j) );
		    stats.push_back(stat_element("Std dev",buffer));

		    sprintf( buffer, "%e", ord.column(MIN)(j) );
		    stats.push_back(stat_element("Minimum",buffer));

		    sprintf( buffer, "%e", ord.column(MAX)(j) );
		    stats.push_back(stat_element("Maximum",buffer));
		}
	    }
	}
	break;
    }
    return True;
}


template <class T>
Bool LatticeStatistics<T>::listLayerStats (
    const Matrix<AccumType>& stats, ostringstream& os, Int zLayer) 
{

   //const uInt nDisplayAxes = displayAxes_p.nelements();
   const uInt n1 = stats.shape()(0);

   Int oDWidth = 15;
   T* dummy = 0;
   DataType type = whatType(dummy);
   if (type==TpComplex) {
      oDWidth = 2*oDWidth + 3; 
   }

   Int oPrec = 6;   

   setStream(os, oPrec);
   Vector<String> sWorld(1);
   Vector<Double> pixels(1);
   pixels(0) = 1.0;
   IPosition blc(pInLattice_p->ndim(),0);
   IPosition trc(pInLattice_p->shape()-1);

   /* 
   Int len0;
   if (nStatsAxes == 1) {
      len0 = 8;
      os << setw(len0) << "Profile ";
   }
   else if (nStatsAxes == 2) {
      len0 = 6;
      os << setw(len0) << "Plane ";
   }
   else if (nStatsAxes == 3) {
      len0 = 5;
      os << setw(len0) << "Cube ";
   }
   else {
      len0 = 11;
      os << setw(len0) << "Hyper-cube ";
   }
   */
   

   os << setw(10) << "Npts";
   os << setw(oDWidth) << "Sum";
   if (_canDoFlux()) {
	   //FIXME Unit not correct in all cases
      os << setw(oDWidth) << "Flux (Jy)";
   }
   os << setw(oDWidth) << "Mean";  
   if (doRobust_p) 
      os << setw(oDWidth) << "Median"; 
   os << setw(oDWidth) << "Rms";
   os << setw(oDWidth) << "Std dev";
   os << setw(oDWidth) << "Minimum";
   os << setw(oDWidth) << "Maximum" << endl;

   //Write statistics to logger.  We write the pixel location
   //relative to the parent lattice
   for (uInt j=0; j<n1; j++) {
      if (zLayer == (Int)j || n1 == 1)  {

      //os << setw(len0)     
      //   << j+blcParent_p(displayAxes_p(0));

      //setStream(os, oPrec);
      os.fill(' '); 
      os.precision(0);
      os.setf(ios::fixed, ios::floatfield);
      os.setf(ios::left, ios::adjustfield);
      os << setw(10)
         << stats.column(NPTS)(j);

      if(LattStatsSpecialize::hasSomePoints(stats.column(NPTS)(j))){

         setStream(os, oPrec);
         os << setw(oDWidth)
            << stats.column(SUM)(j);
         if (_canDoFlux()) {
            setStream(os, oPrec);
            os << setw(oDWidth)
               << stats.column(FLUX)(j);
         }
         setStream(os, oPrec);
         os << setw(oDWidth)
            << stats.column(MEAN)(j);
         if (doRobust_p){ 
            setStream(os, oPrec);
            os << setw(oDWidth)
               << stats.column(MEDIAN)(j);
         }
         setStream(os, oPrec);
         os << setw(oDWidth)
            << stats.column(RMS)(j);
         setStream(os, oPrec);
         os << setw(oDWidth)
            << stats.column(SIGMA)(j);
         setStream(os, oPrec);
         os << setw(oDWidth)
            << stats.column(MIN)(j);
         setStream(os, oPrec);
         os << setw(oDWidth)
            << stats.column(MAX)(j);
      }
      os << endl;
      }
   }
   
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
   ThrowIf(
		   iType >= uInt(LatticeStatsBase::NACCUM),
		   "Illegal statistics accumulation type " + String::toString(type)
   );

   const uInt nDim = pStoreLattice_p->ndim();
   IPosition pos(nDim,0);
   pos(nDim-1) = iType;
   for (uInt j=0; j<displayAxes_p.nelements(); j++) {
      pos(j) = latticePosition(displayAxes_p(j));
   }
   return pos;
}

template <class T>
void LatticeStatistics<T>::minMax (Bool& none,
                                   AccumType& dMin, AccumType& dMax,  
                                   const Vector<AccumType>& d,
                                   const Vector<AccumType>& n) const
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
Bool LatticeStatistics<T>::display()

// This function displays (plotting and listing) the requested
// statistics as a function of the display axes

{
   if (!goodParameterStatus_p) {
     return False;
   }

// Do we have anything to do

   if (!doList_p && !plotter_p.isAttached() && haveLogger_p) {
      os_p << LogIO::NORMAL1 << "There is nothing to plot or list" << LogIO::POST;
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

   Matrix<AccumType> ord(n1,NSTATS);

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
   RO_LatticeIterator<AccumType> pixelIterator(*pStoreLattice_p, stepper);

// Get beam area

   /*
	* dmehring 2012may23: Changing beam area from Double to Array<Double> to support
	* per plane beams. However, I'm quite confused at what this method is doing and so
    * am not sure how to integrate the beamArea array into it. Is plotting of statistics
    * even supported any longer? It doesn't seem to be from the casapy user interface;
    * in ImageAnalysis::statistics, the plotting device is explicitly set "/NULL" independent
    * of user inputs. Until I'm sure how to handle the Array of beam areas correctly and have
    * definitive proof plotting of statistics is actually still used, I'm commenting out the
    * beam specific code here and in the nested loop below.

   Array<Double> beamArea;
   Bool hasBeam = _getBeamArea(beamArea);
   */
   Bool hasBeam = False;
//
   for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {
 
// Convert accumulations to  mean, sigma, and rms.   
 
      Matrix<AccumType>  matrix(pixelIterator.matrixCursor());   // Reference semantics
      for (uInt i=0; i<n1; i++) {
         const AccumType& nPts = matrix(i,NPTS);
         if (LattStatsSpecialize::hasSomePoints(nPts)) {
            ord(i,MEAN) = LattStatsSpecialize::getMean(matrix(i,SUM), nPts);
            /*
             * see 2012may23 comment above
            if (hasBeam) {
            	ord(i,FLUX) = matrix(i,SUM) / beamArea;
            }
            */
            ord(i,SIGMA) = LattStatsSpecialize::getSigma(ord(i,VARIANCE));
            ord(i,RMS) =  LattStatsSpecialize::getRms(matrix(i,SUMSQ), nPts);  
          }
      }

// Extract the direct (NPTS, SUM etc) values from the cursor matrix into the plot matrix
// There is no easy way to do this other than as I have

      for (uInt i=0; i<LatticeStatsBase::NACCUM; i++) {
         for (uInt j=0; j<n1; j++) {
        	 ord(j,i) = matrix(j,i);
         }
      }


// Plot statistics

      if (plotter_p.isAttached()) {
        if (!plotStats (pixelIterator.position(), ord, plotter_p)) return False;
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
void LatticeStatistics<T>::lineSegments (uInt& nSeg,
                                       Vector<uInt>& start,
                                       Vector<uInt>& nPts,
                                       const Vector<AccumType>& mask) const
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
                                      const Vector<AccumType>& x,
                                      const Vector<AccumType>& y,
                                      const Vector<AccumType>& mask) const
//
// Plot an array which may have some blanked points.
// Thus we plot it in segments
//
// Currently, only the real part of the <AccumType>  data is
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
          xF(0) = convertATtoF(x(ip));
          yF(0) = convertATtoF(y(ip));
	  plotter.pt (xF, yF, 1);
      } else {
	  xF.resize(nPts(i)); 
          yF.resize(nPts(i));
          for (uInt j=0; j<nPts(i); j++) {
             xF(j) = convertATtoF(x(start(i)+j));
             yF(j) = convertATtoF(y(start(i)+j));
          }
	  plotter.line (xF, yF);
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
Bool LatticeStatistics<T>::plotStats (
                                      const IPosition& dPos,
                                      const Matrix<AccumType>& stats,
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
   if (! _canDoFlux() ) {
	   doFlux = False;
   }
//
   Bool none;
   Bool first = True;
   Int nL = 0;
   Int nR = 0;

// Generate abcissa. Note that T(n) where T is COmplex results in (n+0i)

   const Int n1 = stats.shape()(0);
   Vector<AccumType> abc(n1);
   for (Int j=0; j<n1; j++) abc(j) = AccumType(j+1);

// Find extrema.  Return if there were no valid points to plot

   AccumType yMin, yMax, xMin, xMax, yLMin, yLMax, yRMin, yRMax;
   // avoid compiler warning by initializing yMin, yMax
   yMin = 0;
   yMax = 0;
   minMax(none, xMin, xMax, abc, stats.column(NPTS));
   if (none) return True;

// Left hand y axis

   if (doMean) {
      minMax(none, yLMin, yLMax, stats.column(MEAN), stats.column(NPTS));
      first = False;
      nL++;
   }
   if (doMedian) {
      minMax(none, yMin, yMax, stats.column(MEDIAN), stats.column(NPTS));
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
   if (doFlux) {
      minMax(none, yMin, yMax, stats.column(FLUX), stats.column(NPTS));
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
//
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
         yRLabel += "Std dev,";
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


template <class T>
void LatticeStatistics<T>::closePlotting()
{
   if (plotter_p.isAttached()) plotter_p.detach();
}




// virtual functions

template <class T>
Bool LatticeStatistics<T>::_getBeamArea(
	Array<Double>& beamArea
) const {
	if (pStoreLattice_p->ndim() == 1) {
		beamArea.resize(IPosition(1, 0));
	}
	else {
		IPosition shape(pStoreLattice_p->ndim() - 1);
		for (uInt i=0; i<shape.nelements(); i++) {
			shape[i] = pStoreLattice_p->shape()[i];
		}
		beamArea.resize(shape);
	}
	beamArea.set(-1.0);
	return False;
}


template <class T>
Bool LatticeStatistics<T>::findNextDatum (uInt& iFound, 
                                          const uInt& n,
                                          const Vector<AccumType>& mask,
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
void LatticeStatistics<T>::getLabels(String& hLabel, String& xLabel, const IPosition& dPos) const
//
// Get labels for top of plot and listing for the higher order axes
// and get the label for the X-axis when plotting
//
{
   ostringstream oss0;
   oss0 << "Axis " << displayAxes_p(0)+1 << " (pixels)";
   xLabel = oss0.str();
//
   const uInt n = displayAxes_p.nelements();
   hLabel =String("");
   if (n > 1) {
      ostringstream oss;
      for (uInt j=1; j<n; j++) {
         oss <<  "Axis " << displayAxes_p(j)+1 << "=" 
             << locInLattice(dPos,True)(j)+1;
         if (j < n-1) oss << ", ";
      }
      hLabel = String(oss);
   }
}


template <class T>
Bool LatticeStatistics<T>::retrieveStorageStatistic(Array<AccumType>& slice, 
                                                    const LatticeStatsBase::StatisticsTypes type,
                                                    const Bool dropDeg)
//
// Retrieve values from storage lattice
//
// Input
//   type         Statistics type
// Input/output
//   slice        The statistics; should be of zero size on input
//
// Returns false if internal class state is bad.
{
// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) {
    	  return False;
      }
   }

// Were there some good points ?  

   const Int nDim = pStoreLattice_p->ndim();
   slice.resize(IPosition(0,0));

   if (someGoodPoints()) {

// Get desired statistic slice. Discard degenerate axes (requires
// empty array on input)

      IPosition sliceShape(pStoreLattice_p->shape());
      sliceShape(nDim-1) = 1;

      Int ISTAT = Int(type);
      IPosition pos(nDim,0);
      pos(nDim-1) = ISTAT;

      pStoreLattice_p->getSlice(slice, pos, sliceShape, 
                                IPosition(nDim,1), dropDeg);
   }
   return True;
}



template <class T>
Bool LatticeStatistics<T>::retrieveStorageStatistic(
	Vector<AccumType>& slice,  const IPosition& pos,
    const Bool posInLattice
) {
//
// Retrieve values from storage lattice
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
   if (! posInLattice) {
      if (pos.nelements() != displayAxes_p.nelements()) {
         error_p = "Incorrectly sized position given";
         slice.resize(0);
         return False;
      }
   }


// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) {
    	  return False;
      }
   }

// Get accumulation sums slice from storage lattice.
// Last axis is statistics axis

   const uInt nDim = displayAxes_p.nelements();
   IPosition slicePos(nDim+1,0);
   if (posInLattice) {
	   _latticePosToStoragePos(slicePos, pos);
   }
   else {

	   // Use position as is

      for (uInt i=0; i<nDim; i++) {
         slicePos(i) = pos(i);
      }
   }

   IPosition sliceShape(nDim+1,1);
   sliceShape(nDim) = LatticeStatsBase::NACCUM;
   Array<AccumType> tSlice;
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

template <class T> void LatticeStatistics<T>::_latticePosToStoragePos(
	IPosition& storagePos, const IPosition& latticePos
) {
    if (latticePos.nelements() != pInLattice_p->ndim()) {
    	ostringstream oss;
    	oss << "LatticeStatistics::" << __FUNCTION__
    		<< "Incorrectly sized position given";
    	storagePos.resize(0);
    	throw AipsError(oss.str());
    }

    if (storagePos.size() < displayAxes_p.size()) {
    	ostringstream oss;
    	oss << "LatticeStatistics::" << __FUNCTION__
    		<< "storage position does not have enough elements";
    }
    if (latticePos.size() < displayAxes_p.size()) {
    	ostringstream oss;
    	oss << "LatticeStatistics::" << __FUNCTION__
    		<< "lattice position does not have enough elements";
    }
    // do NOT resize storagePos. It can have more elements than
    // latticePos as defined by the caller.
    for (uInt i=0; i<displayAxes_p.nelements(); i++) {
    	storagePos[i] = latticePos(displayAxes_p[i]);
    }
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
         Array<AccumType> stats(shape);
         IPosition pos(1,0);

         pStoreLattice_p->getSlice(stats, pos, shape, IPosition(1,1));
   
         pos(0) = NPTS;
         // this needs to be Int64, not Int as it was, to support > 2.1 Gpixel images
         // of course it will still fail for > 9.1 Epixel images, but hopefully we
         // won't have to worry about those for a few more Moore timescales.
         someGoodPointsValue_p = Int64(real(stats(pos))+0.1) > 0;
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
//
         IPosition matrixAxes(2);
         matrixAxes(0) = 0; 
         matrixAxes(1) = pStoreLattice_p->ndim()-1;
//
         LatticeStepper stepper(pStoreLattice_p->shape(), cursorShape,
                                matrixAxes, IPosition::makeAxisPath(pStoreLattice_p->ndim()));
         RO_LatticeIterator<AccumType> pixelIterator(*pStoreLattice_p, stepper);

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
   Array<AccumType> stats(shape);
   pStoreLattice_p->getSlice (stats, IPosition(1,0), shape, IPosition(1,1));

   IPosition pos(1);
   pos(0) = NPTS;
   AccumType nPts = stats(pos);
   pos(0) = SUM;
   AccumType  sum = stats(pos);
   pos(0) = MEDIAN;
   AccumType  median = stats(pos);

   pos(0) = MEDABSDEVMED;
   AccumType  medAbsDevMed = stats(pos);
//
   pos(0) = QUARTILE;
   AccumType  quartile= stats(pos);

   pos(0) = Q1;
   AccumType  q1 = stats(pos);

   pos(0) = Q3;
   AccumType  q3 = stats(pos);
//
   pos(0) = SUMSQ;
   AccumType  sumSq = stats(pos);
   pos(0) = MEAN;
   AccumType  mean = stats(pos);

   pos(0) = VARIANCE;
   AccumType  var = stats(pos);
//                         
  // AccumType  mean = LattStatsSpecialize::getMean(sum, nPts);
  //  AccumType  var = LattStatsSpecialize::getVariance(sum, sumSq, nPts);
   AccumType  rms = LattStatsSpecialize::getRms(sumSq, nPts);
   AccumType  sigma = LattStatsSpecialize::getSigma(var);
//
   pos(0) = MIN;
   AccumType  dMin = stats(pos);
   pos(0) = MAX;
   AccumType  dMax = stats(pos);

   // Do this check so that we only print the stats when we have values.   
   if (LattStatsSpecialize::hasSomePoints(nPts)) {
	   displayStats(
           nPts, sum, median, medAbsDevMed, quartile, sumSq,
    	   mean, var, rms, sigma, dMin, dMax, q1, q3
       );
   }
}
   
template <class T>
void LatticeStatistics<T>::displayStats (
	AccumType nPts, AccumType sum, AccumType median,
    AccumType medAbsDevMed, AccumType quartile,
    AccumType /*sumSq*/, AccumType mean,
    AccumType var, AccumType rms, AccumType sigma,
    AccumType dMin, AccumType dMax,
    AccumType q1, AccumType q3
) {
// Get beam

   //Array<Double> beamArea;
   //Bool hasBeam = _getBeamArea(beamArea);

// Have to convert LogIO object to ostream before can apply
// the manipulators.  Also formatting Complex numbers with
// the setw manipulator fails, so I go to a lot of trouble
// with ostringstreams (which are useable only once).

   const Int oPrec = 6;
   Int oWidth = 14;
   T* dummy = 0;
   DataType type = whatType(dummy);
   if (type==TpComplex) {
      oWidth = 32;
   }
   setStream(os_p.output(), oPrec);
   ostringstream os00, os0, os1, os2, os3, os4, os5, os6, os7, os8;
   ostringstream os9, os10, os11, os12, os13;
   setStream(os00, oPrec); 
   setStream(os0, oPrec); setStream(os1, oPrec); setStream(os2, oPrec); 
   setStream(os3, oPrec); setStream(os4, oPrec); setStream(os5, oPrec);  
   setStream(os6, oPrec); setStream(os7, oPrec); setStream(os8, oPrec); 
   setStream(os9, oPrec); setStream(os10, oPrec), setStream(os11, oPrec);
   setStream(os12, oPrec);
   setStream(os13, oPrec);
//
   os_p << LogIO::NORMAL << endl << LogIO::POST;
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
      os12 << q1;
      os13 << q3;
      os_p << "Number points = ";
      os_p.output() << setw(oWidth) << String(os00) << "       Sum      = ";
      os_p.output() << setw(oWidth) << String(os1) << endl;
      os_p.post();
      /*
      if (hasBeam) {
    	  // beamArea guaranteed to only have one value in this method.
         os_p << "Flux density  = ";
         os0 << sum/(*(beamArea.begin()));
         os_p.output() << setw(oWidth) << String(os0) << " Jy" << endl;
         os_p.post();
      }
      */
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
         os_p << "       Std dev   = ";
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
         os_p.output()  << "            IQR = ";
         os_p.output() << setw(oWidth) << String(os10) << endl;
         os_p.output()  << " First Quartile = ";
         os_p.output() << setw(oWidth) << String(os12) << endl;
         os_p.output()  << " Third Quartile = ";
         os_p.output() << setw(oWidth) << String(os13) << endl;
         os_p.post();
      }
      os_p << endl << LogIO::POST;
      listMinMax(os6, os7, oWidth, type);
   } else {
      os_p << "No valid points found " << LogIO::POST;
   }
   os_p << endl << LogIO::POST;
}
 
template <class T>
void LatticeStatistics<T>::stretchMinMax (AccumType& dMin, AccumType& dMax) const
//
// Stretch a range by 5%  
//  
// Input/output:
//   dMin,Max     The range to stretch
// 
{    
   AccumType delta = AccumType(0.05)*(dMax-dMin);
   AccumType absmax = max(abs(dMax),abs(dMin));
   if (delta < AccumType(1.0e-5)*absmax) delta = AccumType(0.01) * absmax;
                                 
   if (dMin==dMax) {
      if (dMin==AccumType(0.0)) {
         dMin = AccumType(-1.0);
         dMax = AccumType(1.0);
      }  
      else {
         dMin = dMin - AccumType(0.05)*dMin;
         dMax = dMax + AccumType(0.05)*dMax;
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


template <class T, class U>
StatsTiledCollapser<T,U>::StatsTiledCollapser(const Vector<T>& pixelRange, 
                                              Bool noInclude, Bool noExclude,
                                              Bool fixedMinMax)
: /*range_p(pixelRange), */
  noInclude_p(noInclude),
  noExclude_p(noExclude),
  fixedMinMax_p(fixedMinMax),
  minPos_p(0),
  maxPos_p(0), _cs(), _ranges() ,_first()
{
	if (! noInclude || ! noExclude) {
		_ranges.resize(1);
		_ranges[0] = std::pair<U, U>(pixelRange[0], pixelRange[1]);
	}
	T dummy = 0;
	_type = whatType(&dummy);
}


template <class T, class U>
void StatsTiledCollapser<T,U>::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == LatticeStatsBase::NACCUM, AipsError);
}

template <class T, class U>
void StatsTiledCollapser<T,U>::initAccumulator (uInt n1, uInt n3)
{
   n1_p = n1;
   n3_p = n3;
   _first = vector<Bool>(n1*n3, True);
   _cs = vector<ClassicalStatistics<U, const T*, const Bool*> >(n1*n3);
   typename vector<ClassicalStatistics<U, const T*, const Bool*> >::iterator iter = _cs.begin();
   typename vector<ClassicalStatistics<U, const T*, const Bool*> >::iterator end = _cs.end();
   while (iter != end) {
	   iter->setCalculateAsAdded(True);
	   ++iter;
   }
}

template <class T, class U>
void StatsTiledCollapser<T,U>::process (
	uInt index1, uInt index3,
	const T* pInData, const Bool* pInMask,
	uInt dataIncr, uInt maskIncr,
	uInt nrval, const IPosition& startPos,
	const IPosition& shape
) {
	uInt index = index1 + index3*n1_p;
	Record oldStats;
	Int oldMaxDataset = -1;
	Int oldMinDataset = -1;
	if (_type == TpFloat && ! _first[index]) {
		oldStats = _cs[index].getStatistics();
		if (oldStats.isDefined("maxDatasetIndex")) {
			oldMaxDataset = oldStats.asuInt("maxDatasetIndex");
		}
		if (oldStats.isDefined("minDatasetIndex")) {
			oldMinDataset = oldStats.asuInt("minDatasetIndex");
		}
	}
	if (pInMask) {
		// mask
		if (noInclude_p && noExclude_p) {
			// no range
			_cs[index].addData(
				pInData, pInMask,
				nrval, dataIncr, maskIncr
			);
		}
		else {
			// range
			_cs[index].addData(
				pInData, pInMask, nrval,
				_ranges, noExclude_p, dataIncr, maskIncr
			);
		}
	}
	else {
		// no mask
		if (noInclude_p && noExclude_p) {
			// no range
			_cs[index].addData(
				pInData, nrval, dataIncr
			);
		}
		else {
			// range
			_cs[index].addData(
				pInData, nrval, _ranges,
				noExclude_p, dataIncr
			);
		}
	}
	Record updatedStats = _cs[index].getStatistics();
	if (_type == TpFloat) {
		if (
			updatedStats.isDefined("maxDatasetIndex")
			&& updatedStats.asInt("maxDatasetIndex") != oldMaxDataset
		) {
			maxPos_p = startPos + toIPositionInArray(
				updatedStats.asInt64("maxIndex"), shape
			);
		}
		if (
			updatedStats.isDefined("minDatasetIndex")
			&& updatedStats.asInt("minDatasetIndex") != oldMinDataset
		) {
			minPos_p = startPos + toIPositionInArray(
				updatedStats.asInt64("minIndex"), shape
			);

		}
		_first[index] = False; 
	}
}

template <class T, class U>
void StatsTiledCollapser<T,U>::endAccumulator(Array<U>& result,
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
    Bool deleteRes;
    U* res = result.getStorage (deleteRes);
    U* resptr = res;

    vector<U> means, npts, sums, sumsqs, variances;
    vector<T> maxes, mins;
    typename vector<ClassicalStatistics<U, const T*, const Bool*> >::iterator csIter = _cs.begin();
    typename vector<ClassicalStatistics<U, const T*, const Bool*> >::iterator csEnd = _cs.end();
    Record stats;
    T t;
    U u;
    while (csIter != csEnd) {
    	stats = csIter->getStatistics();
    	stats.get(
    		StatisticsData::toString(StatisticsData::MEAN), u
    	);
    	means.push_back(u);
    	stats.get(
    		StatisticsData::toString(StatisticsData::NPTS), u
    	);
    	npts.push_back(u);
    	stats.get(
    		StatisticsData::toString(StatisticsData::SUM), u
    	);
    	sums.push_back(u);
    	stats.get(
    		StatisticsData::toString(StatisticsData::SUMSQ), u
    	);
    	sumsqs.push_back(u);
    	stats.get(
    		StatisticsData::toString(StatisticsData::VARIANCE), u
    	);
    	variances.push_back(u);
    	if (fixedMinMax_p && ! noInclude_p) {
    		t = _ranges[0].second;
    	}
    	else if (stats.isDefined(StatisticsData::toString(StatisticsData::MAX))) {
    		stats.get(
    			StatisticsData::toString(StatisticsData::MAX), t
    		);
    	}
    	else {
    		t = T(0);
    	}
    	maxes.push_back(t);
    	if (fixedMinMax_p && ! noInclude_p) {
    		t = _ranges[0].first;
    	}
    	else if (stats.isDefined(StatisticsData::toString(StatisticsData::MIN))) {
    		stats.get(
    			StatisticsData::toString(StatisticsData::MIN), t
    		);
    	}
    	else {
    		t = 0;
    	}
    	mins.push_back(t);
    	++csIter;
    }
    U* meanPtr = &(means[0]);
    U* nPtsPtr = &(npts[0]);
    U* sumPtr = &(sums[0]);
    U* sumSqPtr = &(sumsqs[0]);
    U* variancePtr = &(variances[0]);
    const T* minPtr = &(mins[0]);
    const T* maxPtr = &(maxes[0]);

    uInt i,j;
    U* resptr_root = resptr;
    for (i=0; i<n3_p; i++) {
       resptr = resptr_root + (Int(LatticeStatsBase::NPTS) * n1_p);
       objcopy (resptr, nPtsPtr, n1_p);
       nPtsPtr += n1_p;

       resptr = resptr_root + (Int(LatticeStatsBase::SUM) * n1_p);
       objcopy (resptr, sumPtr, n1_p);
       sumPtr += n1_p;

       resptr = resptr_root + (Int(LatticeStatsBase::SUMSQ) * n1_p);
       objcopy (resptr, sumSqPtr, n1_p);
       sumSqPtr += n1_p;

       resptr = resptr_root + (Int(LatticeStatsBase::MEAN) * n1_p);
       objcopy (resptr, meanPtr, n1_p);
       meanPtr += n1_p;

       resptr = resptr_root + (Int(LatticeStatsBase::VARIANCE) * n1_p);
       objcopy (resptr, variancePtr, n1_p);
       variancePtr += n1_p;

       resptr = resptr_root + (Int(LatticeStatsBase::MIN) * n1_p);
       for (j=0; j<n1_p; j++) {   
          convertScalar (*resptr++, *minPtr++);
       }

       resptr = resptr_root + (Int(LatticeStatsBase::MAX) * n1_p);
       for (j=0; j<n1_p; j++) {   
          convertScalar (*resptr++, *maxPtr++);
       }

       resptr_root += n1_p * Int(LatticeStatsBase::NACCUM);
    }
    result.putStorage (res, deleteRes);
}

template <class T, class U>
void StatsTiledCollapser<T,U>::minMaxPos(IPosition& minPos, IPosition& maxPos)
{
   minPos.resize(minPos_p.nelements());
   minPos = minPos_p;
   maxPos.resize(maxPos_p.nelements());
   maxPos = maxPos_p;
}

} //# NAMESPACE CASACORE - END

