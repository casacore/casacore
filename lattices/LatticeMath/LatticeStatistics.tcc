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

#ifndef LATTICES_LATTICESTATISTICS_TCC
#define LATTICES_LATTICESTATISTICS_TCC

#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>
#include <casacore/lattices/LatticeMath/LattStatsProgress.h>
#include <casacore/lattices/LatticeMath/StatsTiledCollapser.h>

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

#include <casacore/scimath/Mathematics/ChauvenetCriterionStatistics.h>
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
  _algConf(), _chauvIters() {
   nxy_p.resize(0);
   statsToPlot_p.resize(0);   
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);
   configureClassical();
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
  _algConf(), _chauvIters()
{
   nxy_p.resize(0);
   statsToPlot_p.resize(0);
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);
   configureClassical();
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
  _algConf(other._algConf), _chauvIters(other._chauvIters),
  _aOld(other._aOld), _bOld(other._bOld), _aNew(other._aNew), _bNew(other._bNew)
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

      if (! pStoreLattice_p.null()) {
        // delete pStoreLattice_p;
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
      _algConf = other._algConf;
      _chauvIters = other._chauvIters;
      _aNew = other._aNew;
      _bNew = other._bNew;
      _aOld = other._aOld;
      _bOld = other._bOld;
   }
   return *this;
}

template <class T>
LatticeStatistics<T>::~LatticeStatistics() {
   delete pInLattice_p;
   pInLattice_p = 0;
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
   if (needStorageLattice_p) {
	   generateStorageLattice();
   }
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
	stats(SIGMA) = LattStatsSpecialize::getSigma(stats(VARIANCE));
	stats(RMS) =  LattStatsSpecialize::getRms(stats(SUMSQ), n);
	stats(FLUX) = 0;
	if (_canDoFlux()) {
		Quantum<AccumType> q;
		if (! _computeFlux(q, stats(SUM), pos, posInLattice)) {
			return False;
		}
		stats(FLUX) = q.getValue();
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
Bool LatticeStatistics<T>::_computeFlux(
	Array<AccumType>&, const Array<AccumType>&, const Array<AccumType>&
) {
	ThrowCc("This object does not support computing fluxes");
}

template <class T>
Bool LatticeStatistics<T>::_computeFlux(
	Quantum<AccumType>&, AccumType, const IPosition&,
	Bool
) {
	ThrowCc("This object does not support computing fluxes");
}

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
	   if (_canDoFlux()) {
		   retrieveStorageStatistic (sum, SUM, dropDeg);
		   return _computeFlux(slice, nPts, sum);
	   }
	   else {
		   slice.resize(IPosition(0,0));
		   return False;
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
void LatticeStatistics<T>::configureClassical() {
	_algConf.algorithm = StatisticsData::CLASSICAL;
	_setDefaultCoeffs();
}

template <class T>
void LatticeStatistics<T>::configureClassical(
	Double aOld, Double bOld, Double aNew, Double bNew
) {
	_algConf.algorithm = StatisticsData::CLASSICAL;
	_aOld = aOld;
	_bOld = bOld;
	_aNew = aNew;
	_bNew = bNew;
}


template <class T>
void LatticeStatistics<T>::configureHingesFences(Double f) {
	if (
		_algConf.algorithm != StatisticsData::HINGESFENCES
		|| ! near(f, _algConf.hf)
	) {
		_algConf.algorithm = StatisticsData::HINGESFENCES;
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
	if (
		_algConf.algorithm != StatisticsData::FITTOHALF
		|| centerType != _algConf.ct
		|| useData != _algConf.ud
		|| (
			centerType == FitToHalfStatisticsData::CVALUE
			&& ! near(centerValue, _algConf.cv)
		)
	) {
		_algConf.algorithm = StatisticsData::FITTOHALF;
		_algConf.ct = centerType;
		_algConf.ud = useData;
		_algConf.cv = centerValue;
		needStorageLattice_p = True;
	}
}

template <class T>
void LatticeStatistics<T>::configureChauvenet(
	Double zscore, Int maxIterations
) {
	if (
		_algConf.algorithm != StatisticsData::CHAUVENETCRITERION
		|| ! near(zscore, _algConf.zs)
		|| maxIterations != _algConf.mi
	) {
		_algConf.algorithm = StatisticsData::CHAUVENETCRITERION;
		_algConf.zs = zscore;
		_algConf.mi = maxIterations;
		needStorageLattice_p = True;
	}
}

template <class T>
Bool LatticeStatistics<T>::generateStorageLattice() {

	// Iterate through the lattice and generate the storage lattice
	// The shape of the storage lattice is n1, n2, ..., NACCUM
	// where n1, n2 etc are the display axes

	// Set the display axes vector (possibly already set in ::setAxes)
	displayAxes_p.resize(0);
 	displayAxes_p = IPosition::otherAxes(
 		pInLattice_p->ndim(), cursorAxes_p
	).asVector();

 	// Work out dimensions of storage lattice (statistics accumulations
 	// are along the last axis)
    IPosition storeLatticeShape;
    LatticeStatsBase::setStorageImageShape(
    	storeLatticeShape, True, Int(LatticeStatsBase::NACCUM),
    	displayAxes_p, pInLattice_p->shape()
    );

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
    pStoreLattice_p = new TempLattice<AccumType>(
    	TiledShape(storeLatticeShape,
    	tileShape), useMemory
    );
    // Set up min/max location variables

    CountedPtr<LattStatsProgress> pProgressMeter = showProgress_p
    	? new LattStatsProgress() : NULL;
    Double timeOld = 0;
    Double timeNew = 0;
    uInt nsets = pStoreLattice_p->size()/storeLatticeShape.getLast(1)[0];

    if (_algConf.algorithm == StatisticsData::CLASSICAL) {
        uInt nel = pInLattice_p->size()/nsets;
        timeOld = nsets*(_aOld + _bOld*nel);
        timeNew = nsets*(_aNew + _bNew*nel);
    }
    //Timer timer;
    if (
    	_algConf.algorithm == StatisticsData::CLASSICAL
    	&& timeOld < timeNew 
    ) {
    	// use older method for higher performance in the large loop count
    	// regime
        //timer.mark();
        minPos_p.resize(pInLattice_p->shape().nelements());
    	maxPos_p.resize(pInLattice_p->shape().nelements());
    	StatsTiledCollapser<T,AccumType> collapser(
    		range_p, noInclude_p, noExclude_p,
    		fixedMinMax_p
    	);
    	Int newOutAxis = pStoreLattice_p->ndim()-1;
    	SubLattice<AccumType> outLatt (*pStoreLattice_p, True);
    	LatticeApply<T,AccumType>::tiledApply(
    		outLatt, *pInLattice_p,
    		collapser, IPosition(cursorAxes_p),
    		newOutAxis,
    		pProgressMeter.null() ? NULL : &*pProgressMeter
    	);
    	collapser.minMaxPos(minPos_p, maxPos_p);
    }
    else {
        _doStatsLoop(nsets, pProgressMeter);
    }
    pProgressMeter = NULL;

    // Do robust statistics separately as required.
    generateRobust();
    needStorageLattice_p = False;
    doneSomeGoodPoints_p = False;
    return True;
}

template <class T>
void LatticeStatistics<T>::_doStatsLoop(
	uInt nsets, CountedPtr<LattStatsProgress> progressMeter
) {
	// use high performance method for low iteration count
	maxPos_p.resize(0);
	minPos_p.resize(0);
	const uInt nCursorAxes = cursorAxes_p.nelements();
	const IPosition latticeShape(pInLattice_p->shape());
	IPosition cursorShape(pInLattice_p->ndim(),1);
	for (uInt i=0; i<nCursorAxes; i++) {
		cursorShape(cursorAxes_p(i)) = latticeShape(cursorAxes_p(i));
	}
	IPosition axisPath = cursorAxes_p;
	axisPath.append(displayAxes_p);
	LatticeStepper stepper(latticeShape, cursorShape, axisPath);
	T currentMax = 0;
	T currentMin = 0;
	T overallMax = 0;
	T overallMin = 0;
	Bool isReal = whatType(&currentMax);

	CountedPtr<StatisticsAlgorithm<AccumType, const T*, const Bool*> > sa = _createStatsAlgorithm();
	LatticeStatsDataProvider<T> lattDP;
	MaskedLatticeStatsDataProvider<T> maskedLattDP;
	LatticeStatsDataProviderBase<T> *dataProvider;
	_configureDataProviders(lattDP, maskedLattDP);
	Bool nsetsIsLarge = nsets > 50;
	if (! progressMeter.null()) {
		if (nsetsIsLarge) {
			progressMeter->init(nsets);
		}
		else {
			lattDP.setProgressMeter(progressMeter);
			if (pInLattice_p->isMasked()) {
				maskedLattDP.setProgressMeter(progressMeter);
			}
		}
	}
	_chauvIters.clear();
	IPosition curPos, posMax, posMean, posMin, posNpts,
		posSum, posSumsq, posVariance;
	Slicer slicer(stepper.position(), stepper.endPosition(), Slicer::endIsLast);
	SubLattice<T> subLat(*pInLattice_p, slicer);
	StatsData<AccumType> stats;
	for (stepper.reset(); ! stepper.atEnd(); stepper++) {
		curPos = stepper.position();
		posMax = locInStorageLattice(curPos, LatticeStatsBase::MAX);
		posMean = locInStorageLattice(curPos, LatticeStatsBase::MEAN);
		posMin = locInStorageLattice(curPos, LatticeStatsBase::MIN);
		posNpts = locInStorageLattice(curPos, LatticeStatsBase::NPTS);
		posSum = locInStorageLattice(curPos, LatticeStatsBase::SUM);
		posSumsq = locInStorageLattice(curPos, LatticeStatsBase::SUMSQ);
		posVariance = locInStorageLattice(curPos, LatticeStatsBase::VARIANCE);
		slicer.setStart(curPos);
		slicer.setEnd(stepper.endPosition());
		subLat.setRegion(slicer);
		if (
			stepper.atStart() && ! progressMeter.null()
			&& ! nsetsIsLarge
		) {
			uInt nelem = subLat.size();
			uInt nSublatticeSteps = nelem > 4096*4096
				? nelem/subLat.advisedMaxPixels()
				: 1;
			progressMeter->init(nsets*nSublatticeSteps);
		}
		if(subLat.isMasked()) {
			maskedLattDP.setLattice(subLat);
			dataProvider = &maskedLattDP;
		}
		else {
			lattDP.setLattice(subLat);
			dataProvider = &lattDP;
		}
		sa->setDataProvider(dataProvider);
		stats = sa->getStatistics();
		if (_algConf.algorithm == StatisticsData::CHAUVENETCRITERION) {
			ChauvenetCriterionStatistics<AccumType, const T*, const Bool*> *ch
				= dynamic_cast<ChauvenetCriterionStatistics<AccumType, const T*, const Bool*> *>(
					&*sa
				);
			ostringstream os;
			os << curPos;
			// using strings as keys rather than the IPosition objects directly because for some reason,
			// only one IPosition gets added to the map, and then no other ones get added.
			// I don't understand, things seem to work OK when I try this in tIPosition, but not here.
			_chauvIters[os.str()] = ch->getNiter();
		}
		pStoreLattice_p->putAt(stats.mean, posMean);
		pStoreLattice_p->putAt(stats.npts, posNpts);
		pStoreLattice_p->putAt(stats.sum, posSum);
		pStoreLattice_p->putAt(stats.sumsq, posSumsq);
		pStoreLattice_p->putAt(stats.variance, posVariance);
		if (fixedMinMax_p && ! noInclude_p) {
			currentMax = range_p[1];
		}
		else if (! stats.max.null()) {
			currentMax = *stats.max;
		}
		pStoreLattice_p->putAt(currentMax, posMax);
		if (fixedMinMax_p && ! noInclude_p) {
			currentMin = range_p[0];
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
		if(! progressMeter.null() && nsetsIsLarge) {
			(*progressMeter)++;
		}
	}
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
	std::set<Double> fractions;
	CountedPtr<StatisticsAlgorithm<AccumType, const T*, const Bool*> > sa;
	LatticeStatsDataProvider<T> lattDP;
	MaskedLatticeStatsDataProvider<T> maskedLattDP;

	IPosition curPos, pos, pos2, pos3, posQ1, posQ3,
		posNpts, posMax, posMin;
	Slicer slicer;
	SubLattice<T> subLat;
	std::map<Double, AccumType> quantiles;
	CountedPtr<uInt64> knownNpts;
	CountedPtr<AccumType> knownMax, knownMin;
	if (doRobust_p) {
		fractions.insert(0.25);
		fractions.insert(0.75);
		sa = _createStatsAlgorithm();
		_configureDataProviders(lattDP, maskedLattDP);
		slicer = Slicer(stepper.position(), stepper.endPosition(), Slicer::endIsLast);
		subLat = SubLattice<T>(*pInLattice_p, slicer);
	}
	for (stepper.reset(); ! stepper.atEnd(); stepper++) {
        curPos = stepper.position();
		pos = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDIAN);
		pos2 = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDABSDEVMED);
		pos3 = locInStorageLattice(stepper.position(), LatticeStatsBase::QUARTILE);
		posQ1 = locInStorageLattice(stepper.position(), LatticeStatsBase::Q1);
		posQ3 = locInStorageLattice(stepper.position(), LatticeStatsBase::Q3);
		if (doRobust_p) {
			posNpts = locInStorageLattice(stepper.position(), LatticeStatsBase::NPTS);
			knownNpts = new uInt64((uInt64)abs(pStoreLattice_p->getAt(posNpts)));
		}
		if (! doRobust_p || *knownNpts == 0) {
			// Stick zero in storage lattice (it's not initialized)
			AccumType val(0);
			pStoreLattice_p->putAt(val, pos);
			pStoreLattice_p->putAt(val, pos2);
			pStoreLattice_p->putAt(val, pos3);
			pStoreLattice_p->putAt(val, posQ1);
			pStoreLattice_p->putAt(val, posQ3);
			continue;
		}

		posMax = locInStorageLattice(stepper.position(), LatticeStatsBase::MAX);
		posMin = locInStorageLattice(stepper.position(), LatticeStatsBase::MIN);

		quantiles.clear();

		slicer.setStart(curPos);
		slicer.setEnd(stepper.endPosition());
		subLat.setRegion(slicer);
		if (subLat.isMasked()) {
			maskedLattDP.setLattice(subLat);
			sa->setDataProvider(&maskedLattDP);
		}
		else {
			lattDP.setLattice(subLat);
			sa->setDataProvider(&lattDP);
		}
		// computing the median and the quartiles simultaneously minimizes
		// the number of necessary data scans, as opposed to first calling
		// getMedian() and getQuartiles() separately
		knownMin = new AccumType(pStoreLattice_p->getAt(posMin));
		knownMax = new AccumType(pStoreLattice_p->getAt(posMax));
		pStoreLattice_p->putAt(
			sa->getMedianAndQuantiles(
				quantiles, fractions, knownNpts, knownMin, knownMax
			),
			pos
		);
		pStoreLattice_p->putAt(sa->getMedianAbsDevMed(), pos2);
		pStoreLattice_p->putAt(quantiles[0.75] - quantiles[0.25], pos3);
		pStoreLattice_p->putAt(quantiles[0.25], posQ1);
		pStoreLattice_p->putAt(quantiles[0.75], posQ3);
	}
}

template <class T>
CountedPtr<StatisticsAlgorithm<typename LatticeStatistics<T>::AccumType, const T*, const Bool*> >
LatticeStatistics<T>::_createStatsAlgorithm() const {
	CountedPtr<StatisticsAlgorithm<AccumType, const T*, const Bool*> > sa;
	switch (_algConf.algorithm) {
	case StatisticsData::CLASSICAL:
		sa = new ClassicalStatistics<AccumType, const T*, const Bool*>();
		return sa;
	case StatisticsData::HINGESFENCES: {
		sa = new HingesFencesStatistics<AccumType, const T*, const Bool*>(_algConf.hf);
		return sa;
	}
	case StatisticsData::FITTOHALF: {
		sa = new FitToHalfStatistics<AccumType, const T*, const Bool*>(
			_algConf.ct, _algConf.ud, _algConf.cv
		);
		return sa;
	}
	case StatisticsData::CHAUVENETCRITERION: {
		sa = new ChauvenetCriterionStatistics<AccumType, const T*, const Bool*>(
			_algConf.zs, _algConf.mi
		);
		return sa;
	}
	default:
		ThrowCc(
			"Logic Error: Unhandled algorithm "
				+ String::toString(_algConf.algorithm)
		);
	}
}

template <class T>
void LatticeStatistics<T>::_configureDataProviders(
	LatticeStatsDataProvider<T>& lattDP,
	MaskedLatticeStatsDataProvider<T>& maskedLattDP
) const {
	if (! noInclude_p || ! noExclude_p) {
		DataRanges range;
		if (! noInclude_p || ! noExclude_p) {
			range.push_back(std::pair<T, T>(range_p[0], range_p[1]));
		}
		lattDP.setRanges(range, ! noInclude_p);
		if (pInLattice_p->isMasked()) {
			maskedLattDP.setRanges(range, ! noInclude_p);
		}
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
    	  Bool unused;
            setStream(os, oPrec);
            os << setw(oDWidth)
               << _flux(unused, sum, area);
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
      Bool canDoFlux = _canDoFlux();
      Bool unused;
      for (uInt i=0; i<n1; i++) {
         const AccumType& nPts = matrix(i,NPTS);
         if (LattStatsSpecialize::hasSomePoints(nPts)) {
            ord(i,MEAN) = 
               LattStatsSpecialize::getMean(matrix(i,SUM), nPts);
            if (canDoFlux) {
            	ord(i,FLUX) = _flux(unused, matrix(i,SUM), area).getValue();
            }
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
		Bool unused;
	    sprintf( buffer, "%e", _flux(unused, sum, area ).getValue());
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
	Bool unused;
	for (uInt i=0; i<n1; i++) {
	    const AccumType& nPts = matrix(i,NPTS);
	    if (LattStatsSpecialize::hasSomePoints(nPts)) {
		ord(i,MEAN) = LattStatsSpecialize::getMean(matrix(i,SUM), nPts);
		if (_canDoFlux()) {
			ord(i,FLUX) = _flux(unused, matrix(i,SUM), area).getValue();
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

   if (!doList_p && haveLogger_p) {
      os_p << LogIO::NORMAL1 << "There is nothing to plot or list" << LogIO::POST;
     return True;
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

   Bool hasBeam = False;
//
   for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {
 
// Convert accumulations to  mean, sigma, and rms.   
      Matrix<AccumType>  matrix(pixelIterator.matrixCursor());   // Reference semantics
      for (uInt i=0; i<n1; i++) {
         const AccumType& nPts = matrix(i,NPTS);
         if (LattStatsSpecialize::hasSomePoints(nPts)) {
            ord(i,MEAN) = LattStatsSpecialize::getMean(matrix(i,SUM), nPts);
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

// List statistics

      if (doList_p) {
         if (!listStats(hasBeam, pixelIterator.position(), ord)) return False;
      }
   }
   return True;
}

// virtual functions
/*
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
*/
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
    ThrowIf(
    	latticePos.nelements() != pInLattice_p->ndim(),
    	"Incorrectly sized position given"
    );
    ThrowIf(
    	storagePos.size() < displayAxes_p.size(),
    	"storage position does not have enough elements"
    );;
    ThrowIf(
    	latticePos.size() < displayAxes_p.size(),
    	"lattice position does not have enough elements"
    );
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
   AccumType  rms = LattStatsSpecialize::getRms(sumSq, nPts);
   AccumType  sigma = LattStatsSpecialize::getSigma(var);

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

} //# NAMESPACE CASACORE - END


#endif
