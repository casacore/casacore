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

#include <casacore/scimath/StatsFramework/ChauvenetCriterionStatistics.h>
#include <casacore/scimath/StatsFramework/FitToHalfStatistics.h>
#include <casacore/scimath/StatsFramework/HingesFencesStatistics.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LatticeStatistics<T>::LatticeStatistics (const MaskedLattice<T>& lattice,
                                         LogIO& os,  
                                         bool showProgress,
                                         bool forceDisk,
                                         bool clone)
// 
// Constructor
//
: os_p(os),
  goodParameterStatus_p(true),
  haveLogger_p(true),
  fixedMinMax_p(false),
  doRobust_p(false),
  doList_p(false),
  error_p(""),
  pInLattice_p(0), 
  pStoreLattice_p(0),
  noInclude_p(true),
  noExclude_p(true),
  needStorageLattice_p(true),
  doneSomeGoodPoints_p(false),
  someGoodPointsValue_p(false),
  showProgress_p(showProgress),
  forceDisk_p(forceDisk),
  doneFullMinMax_p(false),
  _saf(), _chauvIters(), _latticeStatsAlgortihm() {
   nxy_p.resize(0);
   statsToPlot_p.resize(0);   
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);
   configureClassical();
   if (setNewLattice(lattice, clone)) {

// Cursor axes defaults to all

      Vector<int32_t> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      goodParameterStatus_p = false;
   }
}

template <class T>
LatticeStatistics<T>::LatticeStatistics (const MaskedLattice<T>& lattice,
                                         bool showProgress,
                                         bool forceDisk,
                                         bool clone)
// 
// Constructor
//
: goodParameterStatus_p(true),
  haveLogger_p(false),
  fixedMinMax_p(false),
  doRobust_p(false),
  doList_p(false),
  error_p(""),
  pInLattice_p(0), 
  pStoreLattice_p(0),
  noInclude_p(true),
  noExclude_p(true),
  needStorageLattice_p(true),
  doneSomeGoodPoints_p(false),
  someGoodPointsValue_p(false),
  showProgress_p(showProgress),
  forceDisk_p(forceDisk),
  doneFullMinMax_p(false),
  _saf(), _chauvIters(), _latticeStatsAlgortihm()
{
   nxy_p.resize(0);
   statsToPlot_p.resize(0);
   range_p.resize(0);
   minPos_p.resize(0);
   maxPos_p.resize(0);
   blcParent_p.resize(0);
   configureClassical();
   if (setNewLattice(lattice, clone)) {

// Cursor axes defaults to all

      Vector<int32_t> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      goodParameterStatus_p = false;
   }
}

template <class T>
LatticeStatistics<T>::LatticeStatistics(const LatticeStatistics<T> &other) 
: pInLattice_p(0), pStoreLattice_p(0),
  _saf(other._saf), _chauvIters(other._chauvIters),
  _aOld(other._aOld), _bOld(other._bOld), _aNew(other._aNew), _bNew(other._bNew),
  _latticeStatsAlgortihm(
      other._latticeStatsAlgortihm
          ? new LatticeStatsAlgorithm(*other._latticeStatsAlgortihm) : NULL
  )
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

      _inLatPtrMgr.reset(other.pInLattice_p->cloneML());
      pInLattice_p = _inLatPtrMgr.get();
// Delete storage lattice 

      if (! pStoreLattice_p.null()) {
        // delete pStoreLattice_p;
         pStoreLattice_p = 0;
      }

      needStorageLattice_p = true;
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
      _saf = other._saf;
      _chauvIters = other._chauvIters;
      _aNew = other._aNew;
      _bNew = other._bNew;
      _aOld = other._aOld;
      _bOld = other._bOld;
      _latticeStatsAlgortihm.set(
          other._latticeStatsAlgortihm
              ? new LatticeStatsAlgorithm(*other._latticeStatsAlgortihm)
              : NULL
      );
   }
   return *this;
}

template <class T>
LatticeStatistics<T>::~LatticeStatistics() {}

template <class T>
bool LatticeStatistics<T>::setAxes (const Vector<int32_t>& axes)
//
// This function sets the cursor axes and the display axes
//
{
   if (!goodParameterStatus_p) {
      return false;
   }

// Save current cursor axes

   Vector<int32_t> saveAxes(cursorAxes_p.copy());

// Assign cursor axes.

   cursorAxes_p.resize(0);
   cursorAxes_p = axes;
   uint32_t ndim = pInLattice_p->ndim();
   if (cursorAxes_p.nelements() == 0) {
   
// User didn't give any axes.  Set them to all.
       
      cursorAxes_p.resize(ndim);
      for (uint32_t i=0; i<ndim; ++i) cursorAxes_p(i) = i;
   } else {

// Sort axes into increasing order and check

      GenSort<int32_t>::sort(cursorAxes_p, Sort::Ascending, Sort::QuickSort|Sort::NoDuplicates);
//
      for (uint32_t i=0; i<cursorAxes_p.nelements(); i++) {
         if (cursorAxes_p(i) < 0 || cursorAxes_p(i) > int32_t(ndim - 1)) {
            ostringstream oss;
            oss << "Invalid cursor axes: " << axes;
             error_p = oss.str();
            return false;
         }
      }
   }

// Signal that we have changed the axes and need a new storage lattice

   if (saveAxes.nelements() != cursorAxes_p.nelements() ||
       !allEQ(saveAxes, cursorAxes_p)) needStorageLattice_p = true;

// Set the display axes vector.  We also do this in generateStorageLattice
// but it is possible the user will want to see the display axes
// via the public function "displayAxes" before any real work is done
// so poke this in here too.

   displayAxes_p.resize(0);
   displayAxes_p = IPosition::otherAxes(ndim,
                                        cursorAxes_p).asVector();
   return true;
}

template <class T>
void LatticeStatistics<T>::setComputeQuantiles(bool b) {
    doRobust_p = b;
}

template <class T>
bool LatticeStatistics<T>::setInExCludeRange(const Vector<T>& include,
                                             const Vector<T>& exclude,
                                             bool setMinMaxToInclude)
// Assign the desired exclude range
{
   if (!goodParameterStatus_p) {
      return false;
   }

// Save current ranges

   Vector<T> saveRange(range_p.copy());
   bool saveFixedMinMax = fixedMinMax_p;

// Check
      
   ostringstream os;
   bool saveNoInclude = noInclude_p;
   bool saveNoExclude = noExclude_p;
   if (!LattStatsSpecialize::setIncludeExclude(error_p, range_p, noInclude_p, 
                                               noExclude_p, include, exclude)) {
      goodParameterStatus_p = false;
      return false;
   }

// Can't have fixed min and max with an exclusion range

   fixedMinMax_p = setMinMaxToInclude;
   if (!noExclude_p && fixedMinMax_p) {
      if (haveLogger_p) {
         error_p = "Can't have a fixed min and max with an exclusion range";
      }
      goodParameterStatus_p = false;
      return false;
   }

// Can only have fixed min and max range if user gives it

   if (noInclude_p) fixedMinMax_p = false;


// Signal that we have changed the pixel range and need a new storage lattice
   
   if (
       saveNoInclude != noInclude_p || saveNoExclude != noExclude_p
       || saveFixedMinMax != fixedMinMax_p
       || saveRange.size() != range_p.size()
       || !allEQ(saveRange, range_p)
   ) {
      needStorageLattice_p = true;    
      doneFullMinMax_p = false;
   }
   return true;
}

template <class T>
bool LatticeStatistics<T>::setList (const bool& doList)
//
// See if user wants to list statistics as well as plot them
//
{
   if (!goodParameterStatus_p) {
      return false;
   }
   doList_p = doList;
   return true;
} 

template <class T>
bool LatticeStatistics<T>::setNewLattice(
    const MaskedLattice<T>& lattice, bool clone
) {
   if (!goodParameterStatus_p) {
      return false;
   }
   DataType latticeType = whatType<T>();
   if (latticeType != TpFloat && latticeType != TpComplex && latticeType != TpDouble) {
      ostringstream oss;
      oss << "Statistics cannot yet be evaluated from lattices of type : " << latticeType << endl;
      error_p = oss.str();
      goodParameterStatus_p = false;
      return false;
   }

   if (clone) {
       _inLatPtrMgr.reset(lattice.cloneML());
       pInLattice_p = _inLatPtrMgr.get();
   }
   else {
       _inLatPtrMgr.reset();
       pInLattice_p = &lattice;
   }

// This is the location of the input SubLattice in
// the parent Lattice

   blcParent_p = pInLattice_p->region().slicer().start();

// Signal that we have changed the lattice and need a new storage  lattice

   needStorageLattice_p = true;
   return true;
}

template <class T>
bool LatticeStatistics<T>::getConvertedStatistic (Array<T>& stats, 
                                                  LatticeStatsBase::StatisticsTypes type,
                                                  bool dropDeg)
{
   Array<AccumType> tmp;
   bool ok = getStatistic(tmp, type, dropDeg);
   stats.resize(tmp.shape());
   convertArray(stats, tmp);
   return ok;
}


template <class T>
StatisticsData::ALGORITHM LatticeStatistics<T>::_getAlgorithm() const {
    return _saf.algorithm();
}

template <class T> bool LatticeStatistics<T>::getStatistic(
    Array<AccumType>& stats,
    LatticeStatsBase::StatisticsTypes type,
    bool dropDeg
) {
    if (_getAlgorithm() == StatisticsData::BIWEIGHT) {
        ThrowIf(
            type == LatticeStatsBase::FLUX,
            "The biweight algorithm does not support"
            "computation of the flux"
        );
        ThrowIf(
            type == LatticeStatsBase::RMS,
            "The biweight algorithm does not support"
            "computation of the rms"
        );
        ThrowIf(
            type == LatticeStatsBase::SUM,
            "The biweight algorithm does not support"
            "computation of the sum"
        );
        ThrowIf(
            type == LatticeStatsBase::SUMSQ,
            "The biweight algorithm does not support"
            "computation of the sum of squres"
        );
        ThrowIf(
            type == LatticeStatsBase::VARIANCE,
            "The biweight algorithm does not support"
            "computation of the variance"
        );
        ThrowIf(
            type == LatticeStatsBase::MEDIAN
            || type == LatticeStatsBase::MEDABSDEVMED
            || type == LatticeStatsBase::QUARTILE
            || type == LatticeStatsBase::Q1
            || type == LatticeStatsBase::Q3,
            "The biweight algorithm does not support"
            "computation of quantile or quantile-like values"
        );

    }
   if (!goodParameterStatus_p) {
     return false;
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
           doRobust_p = true;
           generateRobust();
       }
       return retrieveStorageStatistic(stats, type, dropDeg);
   }
   else if (type==LatticeStatsBase::MIN) {
      return retrieveStorageStatistic(stats, MIN, dropDeg);
   } else if (type==LatticeStatsBase::MAX) {
      return retrieveStorageStatistic(stats, MAX, dropDeg);
   } else if (type==LatticeStatsBase::MEAN) {
        if (_saf.algorithm() == StatisticsData::BIWEIGHT) {
            return retrieveStorageStatistic(stats, MEAN, dropDeg);
        }
       // we prefer to calculate the mean rather than use the accumulated value
              // because the accumulated value may include accumulated finite precision errors
         // return retrieveStorageStatistic(stats, MEAN, dropDeg);
       return calculateStatistic(stats, MEAN, dropDeg);
   } else if (type==LatticeStatsBase::VARIANCE) {
          return retrieveStorageStatistic(stats, VARIANCE, dropDeg);
   } else if (type==LatticeStatsBase::SIGMA) {
      retrieveStorageStatistic(stats, SIGMA, dropDeg);
   } else if (type==LatticeStatsBase::RMS) {
      return calculateStatistic (stats, RMS, dropDeg);
   } else if (type==LatticeStatsBase::FLUX) {
      return calculateStatistic (stats, FLUX, dropDeg);
   }
   return true;
}

template <class T>
bool LatticeStatistics<T>::getStats(
    Vector<AccumType>& stats, const IPosition& pos,
    const bool posInLattice
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
        return false;
    }
    // Retrieve storage array statistics

    stats.resize(NSTATS);
    if (!retrieveStorageStatistic(stats, pos, posInLattice)) {
        return false;
    }
    // Compute the rest

    const AccumType& n = stats(NPTS);
    if (n <= 0) {
        stats.resize(0);
        return  true;
    }
    stats(RMS) =  _rms(stats(SUMSQ), n);
    stats(FLUX) = 0;
    if (_canDoFlux()) {
        Quantum<AccumType> q;
        if (! _computeFlux(q, stats(SUM), pos, posInLattice)) {
            return false;
        }
        stats(FLUX) = q.getValue();
    }
    return true;
}

template <class T>
bool LatticeStatistics<T>::getMinMaxPos(IPosition& minPos, IPosition& maxPos)
{
    ThrowIf(
        _saf.algorithm() == StatisticsData::BIWEIGHT,
        "The biweight algorithm does not support "
        "computing minimum and maximum positions"
    );
   if (!goodParameterStatus_p) {
     return false; 
   }

// Generate storage lattice if required
   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return false;
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
   return true;
}

template <class T>
bool LatticeStatistics<T>::getFullMinMax(T& dataMin, T& dataMax)
{
  if (!doneFullMinMax_p) {

// Specialize

     LattStatsSpecialize::minMax (minFull_p, maxFull_p, pInLattice_p,
                                  range_p, noInclude_p, noExclude_p);
     doneFullMinMax_p = true;
  }
//
  dataMin = minFull_p;
  dataMax = maxFull_p;
//
   return (maxFull_p > minFull_p);
}

// Private functions

template <class T>
bool LatticeStatistics<T>::_computeFlux(
    Array<AccumType>&, const Array<AccumType>&, const Array<AccumType>&
) {
    ThrowCc("This object does not support computing fluxes");
}

template <class T>
bool LatticeStatistics<T>::_computeFlux(
    Quantum<AccumType>&, AccumType, const IPosition&,
    bool
) {
    ThrowCc("This object does not support computing fluxes");
}

template <class T>
bool LatticeStatistics<T>::calculateStatistic (Array<AccumType>& slice, 
                                               LatticeStatsBase::StatisticsTypes type,
                                               bool dropDeg)
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
      if (!generateStorageLattice()) return false;
   }
// Return asap if no good points

   if (!someGoodPoints()) return true;

// Retrieve nPts statistics
   Array<AccumType> nPts;
   retrieveStorageStatistic (nPts, NPTS, dropDeg);
   ReadOnlyVectorIterator<AccumType> nPtsIt(nPts);
   const uint32_t n1 = nPtsIt.vector().nelements();

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
       AccumType npts(0);
       while (!nPtsIt.pastEnd()) {
           for (uint32_t i=0; i<n1; ++i) {
               npts = nPtsIt.vector()(i);
               sliceIt.vector()(i) = _mean(sumIt.vector()(i), npts);
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
           return false;
       }
   }
    else if (type==RMS) {
       retrieveStorageStatistic (sumSq, SUMSQ, dropDeg);
       ReadOnlyVectorIterator<AccumType> sumSqIt(sumSq);
       AccumType npts = 0;
       while (!nPtsIt.pastEnd()) {
          for (uint32_t i=0; i<n1; i++) {
              npts = nPtsIt.vector()(i);
              sliceIt.vector()(i) = _rms(sumSqIt.vector()(i), npts);
          }
          nPtsIt.next();
          sumSqIt.next();
          sliceIt.next();
       }
    }
    else {
       if (haveLogger_p) os_p << LogIO::SEVERE << "Internal error" << endl << LogIO::POST;
       slice.resize(IPosition(0,0));
       return false;
    }
   return true;
}

template <class T>
bool LatticeStatistics<T>::configureBiweight(int32_t maxIter, double c) {
    bool reconfig = _saf.algorithm() != StatisticsData::BIWEIGHT;
    if (! reconfig) {
        StatisticsAlgorithmFactoryData::BiweightData data
        = _saf.biweightData();
        reconfig = maxIter != data.maxIter || ! near(c, data.c);
    }
    if (reconfig) {
        _saf.configureBiweight(maxIter, c);
        needStorageLattice_p = true;
    }
    return reconfig;
}

template <class T>
bool LatticeStatistics<T>::configureClassical() {
    bool reconfig = false;
    if (_saf.algorithm() != StatisticsData::CLASSICAL) {
        _saf.configureClassical();
        needStorageLattice_p = true;
        reconfig = true;
    }
    _setDefaultCoeffs();
    return reconfig;
}

template <class T>
bool LatticeStatistics<T>::configureClassical(
    double aOld, double bOld, double aNew, double bNew
) {
    bool reconfig = false;
    if (_saf.algorithm() != StatisticsData::CLASSICAL) {
        _saf.configureClassical();
        needStorageLattice_p = true;
        reconfig = true;
    }
    _aOld = aOld;
    _bOld = bOld;
    _aNew = aNew;
    _bNew = bNew;
    return reconfig;
}

template <class T>
bool LatticeStatistics<T>::configureHingesFences(double f) {
    bool reconfig = false;
    if (
        _saf.algorithm() != StatisticsData::HINGESFENCES
        || ! near(f, _saf.hingesFencesFactor())
    ) {
        _saf.configureHingesFences(f);
        needStorageLattice_p = true;
        reconfig = true;
    }
    return reconfig;
}

template <class T>
bool LatticeStatistics<T>::configureFitToHalf(
    FitToHalfStatisticsData::CENTER centerType,
    FitToHalfStatisticsData::USE_DATA useData,
    AccumType centerValue
) {
    bool reconfig = _saf.algorithm() != StatisticsData::FITTOHALF;
    if (! reconfig) {
        StatisticsAlgorithmFactoryData::FitToHalfData<AccumType> data
            = _saf.fitToHalfData();
        reconfig = centerType != data.center || useData != data.side
            || (
                centerType == FitToHalfStatisticsData::CVALUE
                && ! near(centerValue, data.centerValue)
            );
    }
    if (reconfig) {
        _saf.configureFitToHalf(centerType, useData, centerValue);
        needStorageLattice_p = true;
    }
    return reconfig;
}

template <class T>
bool LatticeStatistics<T>::configureChauvenet(
    double zscore, int32_t maxIterations
) {
    bool reconfig = _saf.algorithm() != StatisticsData::CHAUVENETCRITERION;
    if (! reconfig) {
        typename StatisticsAlgorithmFactoryData::ChauvenetData data
            = _saf.chauvenetData();
        reconfig = ! near(zscore, data.zScore) || maxIterations != data.maxIter;
    }
    if (reconfig) {
        _saf.configureChauvenet(zscore, maxIterations);
        needStorageLattice_p = true;
    }
    return reconfig;
}

template <class T>
void LatticeStatistics<T>::forceUseStatsFrameworkUsingDataProviders() {
    _latticeStatsAlgortihm.set(
        new LatticeStatsAlgorithm(STATS_FRAMEWORK_DATA_PROVIDERS)
    );
}

template <class T>
void LatticeStatistics<T>::forceUseStatsFrameworkUsingArrays() {
    _latticeStatsAlgortihm.set(
        new LatticeStatsAlgorithm(STATS_FRAMEWORK_ARRAYS)
    );
}

template <class T>
void LatticeStatistics<T>::forceUseOldTiledApplyMethod() {
    _latticeStatsAlgortihm.set(
        new LatticeStatsAlgorithm(TILED_APPLY)
    );
}

template <class T>
void LatticeStatistics<T>::forceAllowCodeDecideWhichAlgortihmToUse() {
    _latticeStatsAlgortihm.set(NULL);
}

template <class T>
bool LatticeStatistics<T>::generateStorageLattice() {
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
    IPosition shape = pInLattice_p->shape();
    LatticeStatsBase::setStorageImageShape(
        storeLatticeShape, true, int32_t(LatticeStatsBase::NACCUM),
        displayAxes_p, shape
    );
    // Set the storage lattice tile shape to the tile shape of the
    // axes of the parent lattice from which it is created.
    // For the statistics axis, set the tile shape to NACCUM (small).
    IPosition tileShape(storeLatticeShape.nelements(),1);
    for (uint32_t i=0; i<tileShape.nelements()-1; i++) {
       tileShape(i) = pInLattice_p->niceCursorShape()(displayAxes_p(i));
    }
    tileShape(tileShape.nelements()-1) = storeLatticeShape(storeLatticeShape.nelements()-1);
    // Create storage lattice.  If lattice is > 10% of available memory,
    // put it on disk.
    uint32_t memory = HostInfo::memoryTotal()/1024;
    double useMemory = double(memory)/10.0;
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
    double timeOld = 0;
    double timeNew = 0;
    uint32_t nsets = pStoreLattice_p->size()/storeLatticeShape.getLast(1)[0];
    bool forceTiledApply = _latticeStatsAlgortihm
        && *_latticeStatsAlgortihm == TILED_APPLY;
    ThrowIf(
        forceTiledApply && _saf.algorithm() != StatisticsData::CLASSICAL,
        "Tiled Apply method can only be run using the Classical Statistics algorithm"
    );
    bool skipTiledApply =  _latticeStatsAlgortihm
        && *_latticeStatsAlgortihm != TILED_APPLY;
    bool tryOldMethod = _saf.algorithm() == StatisticsData::CLASSICAL && ! skipTiledApply;
    if (tryOldMethod) {
        if (! forceTiledApply) {
            uint32_t nel = pInLattice_p->size()/nsets;
            timeOld = nsets*(_aOld + _bOld*nel);
            timeNew = nsets*(_aNew + _bNew*nel);
            tryOldMethod = timeOld < timeNew;
        }
    }
    bool ranOldMethod = false;
    uint32_t ndim = shape.size();
    if (tryOldMethod) {
        if (forceTiledApply && haveLogger_p) {
            os_p << LogIO::NORMAL
                << "Forcing use of Tiled Apply method" << LogIO::POST;
        }
        // use older method for higher performance in the large loop count
        // regime
        minPos_p.resize(ndim);
        maxPos_p.resize(ndim);
        StatsTiledCollapser<T,AccumType> collapser(
            range_p, noInclude_p, noExclude_p,
            fixedMinMax_p
        );
        int32_t newOutAxis = pStoreLattice_p->ndim()-1;
        SubLattice<AccumType> outLatt(*pStoreLattice_p, true);
        try {
            LatticeApply<T,AccumType>::tiledApply(
                outLatt, *pInLattice_p,
                collapser, IPosition(cursorAxes_p),
                newOutAxis,
                pProgressMeter.null() ? NULL : &*pProgressMeter
            );
            collapser.minMaxPos(minPos_p, maxPos_p);
            ranOldMethod = true;
        }
        catch (const AipsError& x) {
            // if the data or mask arrays are not contiguous,
            // an exception will be thrown. Catch it here, so
            // _doStatsLoop() can be run instead.
            ThrowIf(
                forceTiledApply,
                "Forced TileApply method, but underlying arrays are "
                "non-contiguous, so it failed."
            );
        }
        if (ranOldMethod && doRobust_p) {
            // Do "robust" (quantile) statistics separately if required.
            // In the current method, we only call generateRobust()
            // if the old tiled apply method was used to compute the
            // accumulated stats. Quantile stats are now
            // computed in concert with accumulated stats when
            // the statistics framework is used (when _doStatsLoop()
            // is called)
            generateRobust();
        }
    }
    if (! ranOldMethod) {
        _doStatsLoop(nsets, pProgressMeter);
    }
    needStorageLattice_p = false;
    doneSomeGoodPoints_p = false;
    return true;
}

template <class T>
void LatticeStatistics<T>::_doStatsLoop(
    uint32_t nsets, CountedPtr<LattStatsProgress> progressMeter
) {
    maxPos_p.resize(0);
    minPos_p.resize(0);
    const auto nCursorAxes = cursorAxes_p.size();
    const auto latticeShape(pInLattice_p->shape());
    IPosition cursorShape(pInLattice_p->ndim(),1);
    for (uint32_t i=0; i<nCursorAxes; ++i) {
        cursorShape(cursorAxes_p(i)) = latticeShape(cursorAxes_p(i));
    }
    IPosition axisPath = cursorAxes_p;
    axisPath.append(displayAxes_p);
    _chauvIters.clear();
    LatticeStepper stepper(latticeShape, cursorShape, axisPath);
    Slicer slicer(stepper.position(), stepper.endPosition(), Slicer::endIsLast);
    SubLattice<T> subLat(*pInLattice_p, slicer);
    stepper.reset();
    slicer.setStart(stepper.position());
    slicer.setEnd(stepper.endPosition());
    subLat.setRegion(slicer);
    const auto setSize = subLat.size();
    const auto nMaxThreads = OMP::nMaxThreads();
    const auto nDPMaxThreads = min(
        nMaxThreads, setSize/ClassicalStatisticsData::BLOCK_SIZE + 1
    );
    const auto nArrMaxThreads = min(nMaxThreads, nsets);
    auto computed = false;
    const auto forceUsingArrays = _latticeStatsAlgortihm
        && *_latticeStatsAlgortihm == STATS_FRAMEWORK_ARRAYS;
    if (nArrMaxThreads >= nDPMaxThreads || forceUsingArrays) {
        if (forceUsingArrays && haveLogger_p) {
            os_p << LogIO::NORMAL
                << "Forcing use of Stats Framework using Arrays method" << LogIO::POST;
        }
        const auto subCursorShape = _cursorShapeForArrayMethod(setSize);
        if (subCursorShape.product() >= nDPMaxThreads || forceUsingArrays) {
            _computeStatsUsingArrays(progressMeter, subCursorShape);
            computed = true;
        }
    }
    const auto forceUsingDP = _latticeStatsAlgortihm
        && *_latticeStatsAlgortihm == STATS_FRAMEWORK_DATA_PROVIDERS;
    if (! computed || forceUsingDP) {
        if (forceUsingDP && haveLogger_p) {
            os_p << LogIO::NORMAL
                << "Forcing use of Stats Framework using Data Providers method" << LogIO::POST;
        }
        _computeStatsUsingLattDataProviders(
            stepper, subLat, slicer, progressMeter, nsets
        );
    }
    if (! doRobust_p) {
        // zero out the quantile stats since they will not be computed.
        // For the old TiledApply method, this is done by zeroing out
        // the array prior to computing the stats
        const auto ndim = pStoreLattice_p->ndim();
        const auto arrShape = pStoreLattice_p->shape().removeAxes(
            IPosition(1, ndim - 1)
        );
        Array<AccumType> zeros(arrShape, AccumType(0));
        IPosition start(ndim, 0);
        start[ndim - 1] = LatticeStatsBase::MEDABSDEVMED;
        pStoreLattice_p->putSlice(zeros, start);
        start[ndim - 1] = LatticeStatsBase::MEDIAN;
        pStoreLattice_p->putSlice(zeros, start);
        start[ndim - 1] = LatticeStatsBase::Q1;
        pStoreLattice_p->putSlice(zeros, start);
        start[ndim - 1] = LatticeStatsBase::Q3;
        pStoreLattice_p->putSlice(zeros, start);
        start[ndim - 1] = LatticeStatsBase::QUARTILE;
        pStoreLattice_p->putSlice(zeros, start);
    }
}

template <class T>
IPosition LatticeStatistics<T>::_cursorShapeForArrayMethod(uint64_t setSize) const {
    const uint32_t ndim = pInLattice_p->ndim();
    IPosition cursorShape(ndim, 1);
    const auto isChauv = _saf.algorithm() == StatisticsData::CHAUVENETCRITERION;
    // arbitrary, but reasonable, max memory limit in bytes for storing arrays in bytes
    static const uint64_t limit = 2e7;
    static const uint32_t sizeT = sizeof(T);
    static const uint32_t sizeBool = sizeof(bool);
    static const uint32_t sizeInt = sizeof(int32_t);
    static const uint32_t sizeStats = sizeof(StatsData<AccumType>);
    const uint32_t posSize = sizeof(int32_t) * ndim;
    uint32_t chunkMult = pInLattice_p->isMasked() ? sizeT + sizeBool : sizeT;
    uint64_t chunkSize = chunkMult*setSize + sizeStats + posSize;
    if (isChauv) {
        chunkSize += sizeInt;
    }
    const uint64_t nIterToAccum = limit/chunkSize;
    if (nIterToAccum == 0) {
        // chunk size is too big, we cannot use this method
        return IPosition(0);
    }
    auto latShape = pInLattice_p->shape();
    const auto nCursorAxes = cursorAxes_p.size();
    for (uint32_t i=0; i<nCursorAxes; ++i) {
        const auto curAx = cursorAxes_p[i];
        cursorShape[curAx] = latShape[curAx];
    }
    uint64_t x = nIterToAccum;
    const auto nDisplayAxes = displayAxes_p.size();
    for (uint32_t i=0; i<nDisplayAxes; ++i) {
        cursorShape[displayAxes_p[i]] = min(x, (uint64_t)latShape[displayAxes_p[i]]);
        x /= cursorShape[displayAxes_p[i]];
        if (x == 0) {
            break;
        }
    }
    return cursorShape;
}

template <class T>
void LatticeStatistics<T>::_computeStatsUsingArrays(
    CountedPtr<LattStatsProgress> progressMeter,
    const IPosition& cursorShape
) {
    T overallMax = 0;
    T overallMin = 0;
    bool isReal = whatType<T>();
    const uint32_t nMaxThreads = OMP::nMaxThreads();
    IPosition displayAxes(displayAxes_p);
    uint32_t nArraysMax = cursorShape.keepAxes(displayAxes).product();
    uint32_t nSA = min(nMaxThreads, nArraysMax);
    StatisticsAlgorithmFactory<
        AccumType, typename Array<T>::const_iterator, Array<bool>::const_iterator
    > saf2;
    _saf.copy(saf2);
    std::vector<
        CountedPtr<
            StatisticsAlgorithm<
                AccumType, typename Array<T>::const_iterator,
                Array<bool>::const_iterator
            >
        >
    > sa(nSA);
    for (uint32_t i=0; i<nSA; ++i) {
        sa[i] = saf2.createStatsAlgorithm();
    }
    CountedPtr<DataRanges> range;
    if (! noInclude_p || ! noExclude_p) {
        range.reset(new DataRanges());
        range->push_back(std::pair<T, T>(range_p[0], range_p[1]));
    }
    const bool isChauv = _saf.algorithm() == StatisticsData::CHAUVENETCRITERION;
    std::vector<Array<T> > dataArray;
    std::vector<Array<bool> > maskArray;
    std::vector<IPosition> curPos;
    bool isMasked = pInLattice_p->isMasked();
    IPosition latShape = pInLattice_p->shape();
    const uint32_t nCursorAxes = cursorAxes_p.size();
    IPosition chunkSliceStart(latShape.size(), 0);
    IPosition chunkSliceEnd = chunkSliceStart;
    for (uint32_t i=0; i<nCursorAxes; ++i) {
        uint32_t curAx = cursorAxes_p[i];
        chunkSliceEnd[curAx] = latShape[curAx] - 1;
    }
    const IPosition chunkSliceEndAtChunkIterBegin = chunkSliceEnd;
    uint32_t nDisplayAxes = displayAxes_p.size();
    IPosition cp;
    IPosition arrayShape;
    LatticeStepper myStepper(latShape, cursorShape, LatticeStepper::RESIZE);
    uint32_t nIter = 1;
    uint32_t ndim = latShape.size();
    for (uint32_t i=0; i<ndim; ++i) {
        nIter *= ceil((float)latShape[i]/(float)cursorShape[i]);
    }
    if (! progressMeter.null()) {
        progressMeter->init(nIter);
    }
    RO_MaskedLatticeIterator<T> latIter(*pInLattice_p, myStepper);
    for (latIter.reset(); ! latIter.atEnd(); ++latIter) {
        cp = latIter.position();
        const Array<T>& chunk = latIter.cursor();
        IPosition chunkShape = chunk.shape();
        const Array<bool> maskChunk = isMasked ? latIter.getMask() : Array<bool>();
        uint32_t nSets = chunkShape.keepAxes(displayAxes).product();
        if (dataArray.size() != nSets) {
            dataArray.resize(nSets);
            curPos.resize(nSets);
            if (isMasked) {
                maskArray.resize(nSets);
            }
        }
        chunkSliceStart = 0;
        chunkSliceEnd = chunkSliceEndAtChunkIterBegin;
        bool done = false;
        uint32_t setIndex = 0;
        while (! done) {
            // use assign rather than = because array shapes can differ, throwing
            // a conformance exception if = is used
            dataArray[setIndex].assign(chunk(chunkSliceStart, chunkSliceEnd));
            if (isMasked) {
                Array<bool> maskSlice = maskChunk(chunkSliceStart, chunkSliceEnd);
                // use assign rather than = because array shapes can differ
                maskArray[setIndex].assign(allTrue(maskSlice) ? Array<bool>() : maskSlice);
            }
            curPos[setIndex] = cp + chunkSliceStart;
            done = true;
            for (uint32_t i=0; i<nDisplayAxes; ++i) {
                uint32_t dax = displayAxes_p[i];
                if (chunkSliceStart[dax] < chunkShape[dax] - 1) {
                    ++chunkSliceStart[dax];
                    ++chunkSliceEnd[dax];
                    done = false;
                    ++setIndex;
                    break;
                }
                else {
                    chunkSliceStart[dax] = 0;
                    chunkSliceEnd[dax] = 0;
                }
            }
        }
        uint32_t nArrays = dataArray.size();
        uint32_t nthreads = min(nMaxThreads, nArrays);
        _doComputationUsingArrays(
            sa, overallMin, overallMax, arrayShape, dataArray,
            maskArray, curPos, nthreads,
            isChauv, isMasked, isReal, range
        );
        if(! progressMeter.null()) {
            (*progressMeter)++;
        }
    }
}

template <class T>
void LatticeStatistics<T>::_doComputationUsingArrays(
    std::vector<
        CountedPtr<
            StatisticsAlgorithm<
                AccumType, typename Array<T>::const_iterator,
                Array<bool>::const_iterator
            >
        >
    >& sa, T& overallMin, T& overallMax, IPosition& arrayShape,
    std::vector<Array<T> >& dataArray,
    std::vector<Array<bool> >& maskArray, std::vector<IPosition>& curPos,
    uint32_t
#ifdef _OPENMP
         nthreads
#endif
                 , bool isChauv,
    bool isMasked, bool isReal, CountedPtr<const DataRanges> range
) {
    uint32_t nArrays = dataArray.size();
    bool fixedCurMinMax = (fixedMinMax_p && ! noInclude_p);
    T currentMin = fixedCurMinMax ? range_p[0] : 0;
    T currentMax = fixedCurMinMax ? range_p[1] : 0;
    std::vector<StatsData<AccumType> > statsArray(nArrays);
    std::vector<AccumType> q1(doRobust_p ? nArrays : 0);
    std::vector<AccumType> q3(doRobust_p ? nArrays : 0);
    std::vector<uint32_t> chauvIterArray(isChauv ? nArrays : 0);
    ostringstream chos;
#ifdef _OPENMP
# pragma omp parallel for num_threads(nthreads)
#endif
    for (uint32_t i=0; i<nArrays; ++i) {
#ifdef _OPENMP
        uint32_t tid = omp_get_thread_num();
#else
        uint32_t tid = 0;
#endif
        if (isMasked && maskArray[i].size() > 0) {
            if (! range) {
                sa[tid]->setData(
                   dataArray[i].begin(), maskArray[i].begin(), dataArray[i].size()
                );
            }
            else {
                sa[tid]->setData(
                    dataArray[i].begin(), maskArray[i].begin(), dataArray[i].size(),
                    *range, ! noInclude_p
                );
            }
        }
        else {
            if (! range) {
                sa[tid]->setData(dataArray[i].begin(), dataArray[i].size());
            }
            else {
                sa[tid]->setData(
                    dataArray[i].begin(), dataArray[i].size(), *range, ! noInclude_p
                );
            }
        }
        statsArray[i] = sa[tid]->getStatistics();
        if (doRobust_p) {
            _computeQuantilesForStatsFramework(
                statsArray[i], q1[i], q3[i], sa[tid]
            );
        }
        if (isChauv) {
            ChauvenetCriterionStatistics<
                AccumType, typename Array<T>::const_iterator,
                Array<bool>::const_iterator
            > *ch = dynamic_cast<
                ChauvenetCriterionStatistics<
                    AccumType, typename Array<T>::const_iterator,
                    Array<bool>::const_iterator
                >
            * >(&*sa[tid]);
            chauvIterArray[i] = ch->getNiter();
        }
    }
    for (uint32_t i=0; i<nArrays; ++i) {
        StatsData<AccumType> stats = statsArray[i];
        IPosition mypos = curPos[i];
        AccumType qq1 = doRobust_p ? q1[i] : 0;
        AccumType qq3 = doRobust_p ? q3[i] : 0;
        if (! fixedCurMinMax) {
            currentMin = stats.min.null() ? 0 : *stats.min;
            currentMax = stats.max.null() ? 0 : *stats.max;
        }
        _fillStorageLattice(
            currentMin, currentMax, mypos, stats, doRobust_p, qq1, qq3
        );
        if (isChauv) {
            chos.str("");
            chos << mypos;
            _chauvIters[chos.str()] = chauvIterArray[i];
        }
        if (isReal && (! fixedMinMax_p || noInclude_p)) {
            bool atStart = arrayShape.empty();
            if (atStart) {
                arrayShape = dataArray[0].shape();
            }
            // toIPositionInArray() is expensive, so only do it if necessary
            if (atStart || currentMin < overallMin || currentMax > overallMax) {
                IPosition minPos, maxPos;
                // in FitToHalf, one of minpos, maxpos will not be defined
                if (stats.minpos.first >= 0) {
                    minPos = mypos + toIPositionInArray(stats.minpos.second, arrayShape);
                }
                if (stats.maxpos.first >= 0) {
                    maxPos = mypos + toIPositionInArray(stats.maxpos.second, arrayShape);
                }
                _updateMinMaxPos(
                    overallMin, overallMax, currentMin, currentMax,
                    minPos, maxPos, atStart
                );
            }
        }
    }
}

template <class T>
void LatticeStatistics<T>::_computeStatsUsingLattDataProviders(
    LatticeStepper& stepper, SubLattice<T> subLat, Slicer& slicer,
    CountedPtr<LattStatsProgress> progressMeter, uint32_t nsets
) {
    bool fixedCurMinMax = (fixedMinMax_p && ! noInclude_p);
    T currentMin = fixedCurMinMax ? range_p[0] : 0;
    T currentMax = fixedCurMinMax ? range_p[1] : 0;
    T overallMax = 0;
    T overallMin = 0;
    bool isReal = whatType<T>();
    IPosition curPos;
    LatticeStatsDataProvider<T> lattDP;
    MaskedLatticeStatsDataProvider<T> maskedLattDP;
    LatticeStatsDataProviderBase<T> *dataProvider;
    _configureDataProviders(lattDP, maskedLattDP);
    bool nsetsIsLarge = nsets > 50;
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
    CountedPtr<StatisticsAlgorithm<AccumType, const T*, const bool*> > sa
        = _saf.createStatsAlgorithm();
    bool isChauv = _saf.algorithm() == StatisticsData::CHAUVENETCRITERION;
    AccumType q1, q3;
    for (stepper.reset(); ! stepper.atEnd(); stepper++) {
        curPos = stepper.position();
        slicer.setStart(curPos);
        slicer.setEnd(stepper.endPosition());
        // the setRegion() call is a bottleneck when nsets is large
        subLat.setRegion(slicer);
        if(subLat.isMasked()) {
            maskedLattDP.setLattice(subLat);
            dataProvider = &maskedLattDP;
        }
        else {
            lattDP.setLattice(subLat);
            dataProvider = &lattDP;
        }
        if (
            stepper.atStart() && ! progressMeter.null()
            && ! nsetsIsLarge
        ) {
            // if _doRobust_p = true, one scan for accumulated stats
            // + one scan for median and quantiles + one scan for
            // medabsdevmed = at least 3. In practice this can be more
            // because there can be multiple scans for median/quantiles
            // and medabsdevmed for large data sets.
            uint32_t mult = doRobust_p ? 3 : 1;
            progressMeter->init(mult*nsets*dataProvider->estimatedSteps());
        }
        sa->setDataProvider(dataProvider);
        StatsData<AccumType> stats = sa->getStatistics();
        if (! fixedCurMinMax) {
            currentMin = stats.min.null() ? 0 : *stats.min;
            currentMax = stats.max.null() ? 0 : *stats.max;
        }
        if (isChauv) {
            ChauvenetCriterionStatistics<AccumType, const T*, const bool*> *ch
                = dynamic_cast<
                    ChauvenetCriterionStatistics<AccumType, const T*, const bool*>
                *>(
                    &*sa
                );
            ostringstream os;
            os << curPos;
            // using strings as keys rather than the IPosition objects directly because for some reason,
            // only one IPosition gets added to the map, and then no other ones get added.
            // I don't understand, things seem to work OK when I try this in tIPosition, but not here.
            _chauvIters[os.str()] = ch->getNiter();
        }
        if (isReal && (! fixedMinMax_p || noInclude_p)) {
            IPosition maxPos, minPos;
            bool atStart = stepper.atStart();
            if (atStart || currentMin < overallMin || currentMax > overallMax) {
                dataProvider->minMaxPos(minPos, maxPos);
                _updateMinMaxPos(
                    overallMin, overallMax, currentMin, currentMax,
                    minPos, maxPos, atStart
                );
            }
        }
        // quantile computation must come after minpos/maxpos update because the
        // data provider is reset for the quantile computation and the min/max pos
        // info is lost when that happens
        if (doRobust_p) {
            _computeQuantilesForStatsFramework(
                stats, q1, q3, sa
            );
        }
        _fillStorageLattice(currentMin, currentMax, curPos, stats, doRobust_p, q1, q3);
        if(! progressMeter.null() && nsetsIsLarge) {
            (*progressMeter)++;
        }
    }
}

template <class T>
void LatticeStatistics<T>::_updateMinMaxPos(
    T& overallMin, T& overallMax, T currentMin, T currentMax,
    const IPosition& minPos, const IPosition& maxPos,
    bool atStart
) {
    // CAUTION The way this has worked in the past apparently for
    // lattices is that the max and min positions are representative
    // of the *entire* lattice, and were not stored on a sublattice
    // by sublattice basis. This is easy to fix now,
    // but for backward compatibility, I'm leaving this functionality as
    // it has been.
    if (atStart) {
        if (! minPos.empty()) {
            minPos_p = minPos;
        }
        if (! maxPos.empty()) {
            maxPos_p = maxPos;
        }
        overallMin = currentMin;
        overallMax = currentMax;
    }
    else if (
        currentMax > overallMax || currentMin < overallMin
    ) {
        if (currentMin < overallMin) {
            if (! minPos.empty()) {
                minPos_p = minPos;
            }
            overallMin = currentMin;
        }
        if (currentMax > overallMax) {
            if (! maxPos.empty()) {
                maxPos_p = maxPos;
            }
            overallMax = currentMax;
        }
    }
}

template <class T>
void LatticeStatistics<T>::_fillStorageLattice(
    T currentMin, T currentMax, const IPosition& curPos,
    const StatsData<AccumType>& stats, bool doQuantiles,
    AccumType q1, AccumType q3
) {
    const uint32_t ndim = pStoreLattice_p->ndim();
    IPosition pos(ndim,0);
    const uint32_t nDispAxes = displayAxes_p.size();
    for (uint32_t j=0; j<nDispAxes; ++j) {
        pos(j) = curPos(displayAxes_p(j));
    }
    std::map<LatticeStatsBase::StatisticsTypes, AccumType> statsMap;
    statsMap[MAX] = currentMax;
    statsMap[MIN] = currentMin;
    statsMap[MEAN] = stats.mean;
    statsMap[NPTS] = stats.npts;
    statsMap[SUM] = stats.sum;
    statsMap[SUMSQ] = stats.sumsq;
    statsMap[VARIANCE] = stats.variance;
    statsMap[SIGMA] = stats.stddev;
    if (doQuantiles) {
        statsMap[MEDIAN] = *stats.median;
        statsMap[MEDABSDEVMED] = *stats.medAbsDevMed;
        statsMap[Q1] = q1;
        statsMap[Q3] = q3;
        statsMap[QUARTILE] = q3 - q1;
    }
    typename std::map<LatticeStatsBase::StatisticsTypes, AccumType>::const_iterator iter
        = statsMap.begin();
    typename std::map<LatticeStatsBase::StatisticsTypes, AccumType>::const_iterator end
        = statsMap.end();
    uint32_t last = ndim - 1;
    for (; iter!=end; ++iter) {
        const LatticeStatsBase::StatisticsTypes key = iter->first;
        pos[last] = key;
        pStoreLattice_p->putAt(iter->second, pos);
    }
}

template <class T>
void LatticeStatistics<T>::generateRobust () {
    bool showMsg = haveLogger_p && displayAxes_p.empty();
    if (showMsg) {
        os_p << LogIO::NORMAL << "Computing quantiles..." << LogIO::POST;
    }
    const uint32_t nCursorAxes = cursorAxes_p.size();
    const IPosition latticeShape(pInLattice_p->shape());
    IPosition cursorShape(pInLattice_p->ndim(),1);
    for (uint32_t i=0; i<nCursorAxes; ++i) {
        cursorShape(cursorAxes_p(i)) = latticeShape(cursorAxes_p(i));
    }
    IPosition axisPath = cursorAxes_p;
    axisPath.append(displayAxes_p);
    LatticeStepper stepper(latticeShape, cursorShape, axisPath);
    CountedPtr<StatisticsAlgorithm<AccumType, const T*, const bool*> > sa;
    LatticeStatsDataProvider<T> lattDP;
    MaskedLatticeStatsDataProvider<T> maskedLattDP;
    IPosition curPos, pos, pos2, pos3, posQ1, posQ3,
        posNpts, posMax, posMin;
    Slicer slicer;
    SubLattice<T> subLat;
    uint64_t knownNpts;
    AccumType knownMax, knownMin;
    sa = _saf.createStatsAlgorithm();
    _configureDataProviders(lattDP, maskedLattDP);
    slicer = Slicer(stepper.position(), stepper.endPosition(), Slicer::endIsLast);
    subLat = SubLattice<T>(*pInLattice_p, slicer);
    AccumType median, medAbsDevMed, q1, q3;
    for (stepper.reset(); ! stepper.atEnd(); stepper++) {
        curPos = stepper.position();
        pos = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDIAN);
        pos2 = locInStorageLattice(stepper.position(), LatticeStatsBase::MEDABSDEVMED);
        pos3 = locInStorageLattice(stepper.position(), LatticeStatsBase::QUARTILE);
        posQ1 = locInStorageLattice(stepper.position(), LatticeStatsBase::Q1);
        posQ3 = locInStorageLattice(stepper.position(), LatticeStatsBase::Q3);
        posNpts = locInStorageLattice(stepper.position(), LatticeStatsBase::NPTS);
        knownNpts = (uint64_t)abs(pStoreLattice_p->getAt(posNpts));
        if (knownNpts == 0) {
            // Stick zero in storage lattice (it's not initialized)
            static const AccumType val(0);
            pStoreLattice_p->putAt(val, pos);
            pStoreLattice_p->putAt(val, pos2);
            pStoreLattice_p->putAt(val, pos3);
            pStoreLattice_p->putAt(val, posQ1);
            pStoreLattice_p->putAt(val, posQ3);
            continue;
        }
        posMax = locInStorageLattice(stepper.position(), LatticeStatsBase::MAX);
        posMin = locInStorageLattice(stepper.position(), LatticeStatsBase::MIN);
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
        knownMin = pStoreLattice_p->getAt(posMin);
        knownMax = pStoreLattice_p->getAt(posMax);
        _computeQuantiles(
            median, medAbsDevMed, q1, q3, sa,
            knownNpts, knownMin, knownMax
        );
        pStoreLattice_p->putAt(median, pos);
        pStoreLattice_p->putAt(medAbsDevMed, pos2);
        pStoreLattice_p->putAt(q3 - q1, pos3);
        pStoreLattice_p->putAt(q1, posQ1);
        pStoreLattice_p->putAt(q3, posQ3);
    }
}

template <class T>
template <class U, class V>
void LatticeStatistics<T>::_computeQuantiles(
    AccumType& median, AccumType& medAbsDevMed, AccumType& q1, AccumType& q3,
    CountedPtr<StatisticsAlgorithm<AccumType, U, V> > statsAlg,
    uint64_t knownNpts, AccumType knownMin, AccumType knownMax
) const {
    static const std::set<double> fracs = quartileFracs();
    std::map<double, AccumType> quantiles;
    static const uint32_t maxArraySizeBytes = 1e8;
    // try to prevent multiple passes for
    // large images
    uint64_t nBins = max((uint64_t)10000, knownNpts/1000);
    // computing the median and the quartiles simultaneously minimizes
    // the number of necessary data scans, as opposed to first calling
    // getMedian() and getQuartiles() separately
    CountedPtr<uint64_t> npts(new uint64_t(knownNpts));
    CountedPtr<AccumType> mymin(new AccumType(knownMin));
    CountedPtr<AccumType> mymax(new AccumType(knownMax));
    median = statsAlg->getMedianAndQuantiles(
        quantiles, fracs, npts, mymin, mymax,
        maxArraySizeBytes, false, nBins
    );
    q1 = quantiles[0.25];
    q3 = quantiles[0.75];
    medAbsDevMed = statsAlg->getMedianAbsDevMed(
        npts, mymin, mymax, maxArraySizeBytes, false, nBins
    );
}

template <class T>
template <class U, class V>
      void LatticeStatistics<T>::_computeQuantilesForStatsFramework(
      StatsData<AccumType>& stats, AccumType& q1, AccumType& q3,
      CountedPtr<StatisticsAlgorithm<AccumType, U, V> > statsAlg
) const {
    if (stats.npts > 0) {
        AccumType median, medAbsDevMed;
        _computeQuantiles(
            median, medAbsDevMed, q1, q3, statsAlg,
            stats.npts, *stats.min, *stats.max
        );
        stats.median.reset(new AccumType(median));
        stats.medAbsDevMed.reset(new AccumType(medAbsDevMed));
    }
    else {
        stats.median.reset(new AccumType(0));
        stats.medAbsDevMed.reset(new AccumType(0));
        q1 = 0;
        q3 = 0;
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
                                      int32_t oWidth, DataType type)

// Min/max locations only meaningful for float images currently.
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
bool LatticeStatistics<T>::listStats (bool hasBeam, const IPosition& dPos,
                                      const Matrix<AccumType>& stats)
//
// List the statistics for this row to the logger
//
// Inputs:
//   dPos    The location of the start of the cursor in the
//           storage lattice for this row
//   stats   Statistics matrix
// Outputs:
//   bool    Indicates coordinate transformations failed
//
{
   if (!haveLogger_p) {

// We will consider this situation as successful

      return true;
   }
   os_p << endl;


// Get number of statistics and display axes

   const uint32_t nDisplayAxes = displayAxes_p.nelements();
   const uint32_t nStatsAxes = cursorAxes_p.nelements();

// Set up the manipulators. We list the number of points as an integer so find
// out how big the field width needs to be.  Min of 6 so label fits.

   int32_t oDWidth = 15;
   DataType type = whatType<T>();
   if (type==TpComplex) {
      oDWidth = 2*oDWidth + 3;    // (x,y)
   }

// Have to convert LogIO object to ostream before can apply
// the manipulators. Also formatting Complex numbers with
// the setw manipulator fails, so I go to a lot of trouble
// with ostringstreams (which are useable only once).

   int32_t oPrec = 6;   
   setStream(os_p.output(), oPrec);

// Write the pixel and world coordinate of the higher order display axes to the logger

   uint32_t ndim = pInLattice_p->ndim();
   IPosition shape = pInLattice_p->shape();
   if (nDisplayAxes > 1) {
      Vector<String> sWorld(1);
      Vector<double> pixels(1);
      IPosition blc(ndim, 0);
      IPosition trc(shape - 1);
//
      os_p << LogIO::NORMAL;
      for (uint32_t j=1; j<nDisplayAxes; j++) {
         os_p <<  "Axis " << displayAxes_p(j) + 1 << " = " 
              << locInLattice(dPos,true)(j)+1;
         if (j < nDisplayAxes-1) os_p << ", ";
      }
   }


// Find the width of the field into which we are going to write the coordinate value
// of the first display axis.  Do this by formatting a dummy value.

   Vector<String> sWorld(1);
   Vector<double> pixels(1);
   pixels(0) = 1.0;
   IPosition blc(ndim, 0);
   IPosition trc(shape - 1);

// Write headers

   os_p << LogIO::NORMAL << endl;
   int32_t len0;
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

   const uint32_t n1 = stats.shape()(0);
   for (uint32_t j=0; j<n1; j++) {
      os_p.output() << setw(len0)     << j+blcParent_p(displayAxes_p(0))+1;
      ostringstream os00; setStream(os00, oPrec); 
      os00 << stats.column(NPTS)(j);
      os_p.output() << setw(oDWidth)   << os00.str();

      if (stats.column(NPTS)(j) > 0) {

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

   return true;
}

template <class T>
bool LatticeStatistics<T>::getLayerStats(
        String& stats, double area, 
        int32_t zAxis, int32_t zLayer, int32_t hAxis, int32_t hLayer) { 

   if (!goodParameterStatus_p) {
     return false;
   }

   if (needStorageLattice_p) {
       if (!generateStorageLattice()) { 
         return false;
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
      AccumType  mean = _mean(sum, nPts);
      AccumType  rms = _rms(sumSq, nPts);
      AccumType  sigma = sqrt(var);

      pos(0) = MIN;
      AccumType  dMin = statsV(pos);
      pos(0) = MAX;
      AccumType  dMax = statsV(pos);

      if (nPts <= 0) {
          return false;
      }

      stringstream os;
      const int32_t oPrec = 6;
      int32_t oDWidth = 15;
      DataType type = whatType<T>();
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
          bool unused;
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
      return true;
   }

   const uint32_t n1 = pStoreLattice_p->shape()(0);

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
   uint32_t zAx = -1;
   uint32_t hAx = -1;
   for (uint32_t j=0; j<displayAxes_p.nelements(); j++) {
      if (zAxis == displayAxes_p(j))
         zAx = j;
      if (hAxis == displayAxes_p(j))
         hAx = j;
   }

   int32_t layer = 0;
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
      bool canDoFlux = _canDoFlux();
      bool unused;
      for (uint32_t i=0; i<n1; i++) {
         const AccumType& nPts = matrix(i,NPTS);
         if (nPts > 0) {
            ord(i,MEAN) = _mean(matrix(i,SUM), nPts);
            if (canDoFlux) {
                ord(i,FLUX) = _flux(unused, matrix(i,SUM), area).getValue();
            }
            ord(i,SIGMA) = sqrt(matrix(i,VARIANCE));
            ord(i,RMS) =  _rms(matrix(i,SUMSQ), nPts);
          }
      }

      for (uint32_t i=0; i<LatticeStatsBase::NACCUM; i++) {
         for (uint32_t j=0; j<n1; j++) ord(j,i) = matrix(j,i);
      }

      listLayerStats(ord, os, layer);
      break;
   }
   stats += os.str();
   stats += '\n';

   return true;
}

template <class T>
bool LatticeStatistics<T>::getLayerStats(
    stat_list &stats, double area,
        int32_t zAxis, int32_t zLayer, int32_t hAxis, int32_t hLayer) {

    char buffer[256];

    if (!goodParameterStatus_p) {
    return false;
    }

    if (needStorageLattice_p) {
    if (!generateStorageLattice()) {
        return false;
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
    AccumType  mean = _mean(sum, nPts);
    AccumType  rms = _rms(sumSq, nPts);
    AccumType  sigma = sqrt(var);

    pos(0) = MIN;
    AccumType  dMin = statsV(pos);
    pos(0) = MAX;
    AccumType  dMax = statsV(pos);

    if (nPts <= 0) {
        return false;
    }

    //const int32_t oPrec = 6;
    int32_t oDWidth = 15;
    DataType type = whatType<T>();
    if (type==TpComplex) {
        oDWidth = 2*oDWidth + 3;
    }

    sprintf( buffer, "%d", nPts );
    stats.push_back(stat_element("Npts",buffer));

    sprintf( buffer, "%e", sum );
    stats.push_back(stat_element("Sum",buffer));

    if ( _canDoFlux()) {
        bool unused;
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

    return true;
    }

    const uint32_t n1 = pStoreLattice_p->shape()(0);

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
    uint32_t zAx = -1;
    uint32_t hAx = -1;
    for (uint32_t j=0; j<displayAxes_p.nelements(); j++) {
    if (zAxis == displayAxes_p(j))
        zAx = j;
    if (hAxis == displayAxes_p(j))
        hAx = j;
    }

    int32_t layer = 0;
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
    bool unused;
    for (uint32_t i=0; i<n1; i++) {
        const AccumType& nPts = matrix(i,NPTS);
        if (nPts > 0) {
        ord(i,MEAN) = _mean(matrix(i,SUM), nPts);
        if (_canDoFlux()) {
            ord(i,FLUX) = _flux(unused, matrix(i,SUM), area).getValue();
        }
        ord(i,SIGMA) = sqrt(matrix(i,VARIANCE));
        ord(i,RMS) =  _rms(matrix(i,SUMSQ), nPts);
        }
    }

    for (uint32_t i=0; i<LatticeStatsBase::NACCUM; i++) {
        for (uint32_t j=0; j<n1; j++) ord(j,i) = matrix(j,i);
    }

    //const uint32_t nDisplayAxes = displayAxes_p.nelements();
    const uint32_t n1 = ord.shape()(0);

    int32_t oDWidth = 15;
    DataType type = whatType<T>();
    if (type==TpComplex) {
        oDWidth = 2*oDWidth + 3;
    }

    //int32_t oPrec = 6;

    Vector<String> sWorld(1);
    Vector<double> pixels(1);
    pixels(0) = 1.0;
    IPosition blc(pInLattice_p->ndim(),0);
    IPosition trc(pInLattice_p->shape()-1);

    //Write statistics to logger.  We write the pixel location
    //relative to the parent lattice
    for (uint32_t j=0; j<n1; j++) {
        if (layer == (int32_t)j || n1 == 1)  {

        sprintf( buffer, "%d", (int) ord.column(NPTS)(j) );
        stats.push_back(stat_element("Npts",buffer));

        if (ord.column(NPTS)(j) > 0){

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
    return true;
}

template <class T>
bool LatticeStatistics<T>::listLayerStats (
    const Matrix<AccumType>& stats, ostringstream& os, int32_t zLayer) 
{

   //const uint32_t nDisplayAxes = displayAxes_p.nelements();
   const uint32_t n1 = stats.shape()(0);

   int32_t oDWidth = 15;
   DataType type = whatType<T>();
   if (type==TpComplex) {
      oDWidth = 2*oDWidth + 3; 
   }

   int32_t oPrec = 6;   

   setStream(os, oPrec);
   Vector<String> sWorld(1);
   Vector<double> pixels(1);
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
   for (uint32_t j=0; j<n1; j++) {
      if (zLayer == (int32_t)j || n1 == 1)  {

      //os << setw(len0)     
      //   << j+blcParent_p(displayAxes_p(0));

      //setStream(os, oPrec);
      os.fill(' '); 
      os.precision(0);
      os.setf(ios::fixed, ios::floatfield);
      os.setf(ios::left, ios::adjustfield);
      os << setw(10)
         << stats.column(NPTS)(j);

      if(stats.column(NPTS)(j) > 0){

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
   
   return true;
}

template <class T>
IPosition LatticeStatistics<T>::locInLattice(const IPosition& storagePosition,
                                             bool relativeToParent) const
                 
//
// Given a location in the storage lattice, convert those locations on
// the non-statistics axis (the statistics axis is the last one) to 
// account for the  location of the subLattice in the parent lattice
//
{  
   IPosition pos(storagePosition);
   for (uint32_t j=0; j<pos.nelements()-1; j++) {
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
   uint32_t iType = uint32_t(type);
   ThrowIf(
           iType >= uint32_t(LatticeStatsBase::NACCUM),
           "Illegal statistics accumulation type " + String::toString(type)
   );

   const uint32_t nDim = pStoreLattice_p->ndim();
   IPosition pos(nDim,0);
   pos(nDim-1) = iType;
   for (uint32_t j=0; j<displayAxes_p.nelements(); j++) {
      pos(j) = latticePosition(displayAxes_p(j));
   }
   return pos;
}

template <class T>
void LatticeStatistics<T>::minMax (bool& none,
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
   bool init = true;
   none = true;
   const int32_t n1 = d.nelements();

   for (int32_t i=0; i<n1; i++) {
     if (real(n(i)) > 0.5) {
        if (init) {
           dMin = d(i);
           dMax = d(i);
           init = false;
        } else {
           dMin = min(dMin, d(i));
           dMax = max(dMax, d(i));
        }
        none = false;
     }
   }
}

template <class T>
bool LatticeStatistics<T>::display()

// This function displays (plotting and listing) the requested
// statistics as a function of the display axes

{
   if (!goodParameterStatus_p) {
     return false;
   }

// Do we have anything to do

   if (!doList_p && haveLogger_p) {
      os_p << LogIO::NORMAL1 << "There is nothing to plot or list" << LogIO::POST;
     return true;
   }

// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return false;
   }

// If we don't have any display axes just summarise the lattice statistics
   if (displayAxes_p.nelements() == 0) {
     summStats ();
     return true;
   }

// Size of plotting abcissa axis

   const uint32_t n1 = pStoreLattice_p->shape()(0);

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

   bool hasBeam = false;
//
   for (pixelIterator.reset(); !pixelIterator.atEnd(); pixelIterator++) {
 
// Convert accumulations to  mean, sigma, and rms.   
      Matrix<AccumType>  matrix(pixelIterator.matrixCursor());   // Reference semantics
      for (uint32_t i=0; i<n1; i++) {
         const AccumType& nPts = matrix(i,NPTS);
         if (nPts > 0) {
            ord(i,MEAN) = _mean(matrix(i,SUM), nPts);
            ord(i,SIGMA) = sqrt(ord(i,VARIANCE));
            ord(i,RMS) =  _rms(matrix(i,SUMSQ), nPts);
          }
      }

// Extract the direct (NPTS, SUM etc) values from the cursor matrix into the plot matrix
// There is no easy way to do this other than as I have

      for (uint32_t i=0; i<LatticeStatsBase::NACCUM; i++) {
         for (uint32_t j=0; j<n1; j++) {
             ord(j,i) = matrix(j,i);
         }
      }

// List statistics

      if (doList_p) {
         if (!listStats(hasBeam, pixelIterator.position(), ord)) return false;
      }
   }
   return true;
}

template <class T>
bool LatticeStatistics<T>::retrieveStorageStatistic(Array<AccumType>& slice, 
                                                    const LatticeStatsBase::StatisticsTypes type,
                                                    const bool dropDeg)
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
          return false;
      }
   }
// Were there some good points ?  

   const int32_t nDim = pStoreLattice_p->ndim();
   slice.resize(IPosition(0,0));
   if (someGoodPoints()) {
// Get desired statistic slice. Discard degenerate axes (requires
// empty array on input)

      IPosition sliceShape(pStoreLattice_p->shape());
      sliceShape(nDim-1) = 1;

      int32_t ISTAT = int32_t(type);
      IPosition pos(nDim,0);
      pos(nDim-1) = ISTAT;
      pStoreLattice_p->getSlice(slice, pos, sliceShape, 
                                IPosition(nDim,1), dropDeg);
   }
   return true;
}

template <class T>
bool LatticeStatistics<T>::retrieveStorageStatistic(
    Vector<AccumType>& slice,  const IPosition& pos,
    const bool posInLattice
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
         return false;
      }
   }


// Generate storage lattice if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) {
          return false;
      }
   }

// Get accumulation sums slice from storage lattice.
// Last axis is statistics axis

   const uint32_t nDim = displayAxes_p.nelements();
   IPosition slicePos(nDim+1,0);
   if (posInLattice) {
       _latticePosToStoragePos(slicePos, pos);
   }
   else {

       // Use position as is

      for (uint32_t i=0; i<nDim; i++) {
         slicePos(i) = pos(i);
      }
   }

   IPosition sliceShape(nDim+1,1);
   sliceShape(nDim) = LatticeStatsBase::NACCUM;
   Array<AccumType> tSlice;
   pStoreLattice_p->getSlice(tSlice, slicePos, sliceShape, 
                           IPosition(nDim+1,1), false);
// Copy to vector      

   slicePos = 0;
   for (uint32_t i=0; i<LatticeStatsBase::NACCUM; i++) {
      slicePos(nDim) = i;
      slice(i) = tSlice(slicePos);
   }
   return true;
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
    for (uint32_t i=0; i<displayAxes_p.nelements(); i++) {
        storagePos[i] = latticePos(displayAxes_p[i]);
    }
}

template <class T>
bool LatticeStatistics<T>::someGoodPoints ()
//
// If any of the locations in the statistics storage array contain
// some valid points return true straight away.  DOn't bother
// looking again if we already looked !
//
{
   if (doneSomeGoodPoints_p) {
      return someGoodPointsValue_p;
   } else {
      doneSomeGoodPoints_p = true;
      if (pStoreLattice_p->ndim() == 1) {

// If storage lattice only 1D take cheap way out. Can't invoke
// retrieveStorageStatistic or we will be stuck in a time loop

         const IPosition shape = statsSliceShape();
         Array<AccumType> stats(shape);
         IPosition pos(1,0);

         pStoreLattice_p->getSlice(stats, pos, shape, IPosition(1,1));
   
         pos(0) = NPTS;
         // this needs to be int64_t, not int32_t as it was, to support > 2.1 Gpixel images
         // of course it will still fail for > 9.1 Epixel images, but hopefully we
         // won't have to worry about those for a few more Moore timescales.
         someGoodPointsValue_p = int64_t(real(stats(pos))+0.1) > 0;
         return someGoodPointsValue_p;
      } else {


// Iterate through storage lattice by planes (first and last axis of storage lattice)
// Specify which axes are the matrix  axes so that we can discard other
// degenerate axes with the matrixCursor function.   n1 is only 
// constrained to be n1 >= 1

         IPosition cursorShape(pStoreLattice_p->ndim(),1);
         const int32_t n1 = pStoreLattice_p->shape()(0);
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
            for (int32_t i=0; i<n1; i++) {
               if (int32_t(real(pixelIterator.matrixCursor()(i,NPTS))+0.1) > 0) {
                  someGoodPointsValue_p = true;
                  return someGoodPointsValue_p;
               }
            }
         }
         someGoodPointsValue_p = false;
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
   AccumType  rms = _rms(sumSq, nPts);
   pos(0) = SIGMA;
   AccumType  sigma = stats(pos);

   pos(0) = MIN;
   AccumType  dMin = stats(pos);
   pos(0) = MAX;
   AccumType  dMax = stats(pos);
   // Do this check so that we only print the stats when we have values.   
   if (nPts > 0) {
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

   const int32_t oPrec = 6;
   int32_t oWidth = 14;
   DataType type = whatType<T>();
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
   if (nPts > 0) {
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
void LatticeStatistics<T>::setStream (ostream& os, int32_t oPrec)
{
   os.fill(' '); 
   os.precision(oPrec);
   os.setf(ios::scientific, ios::floatfield);
   os.setf(ios::left, ios::adjustfield);
}

} //# NAMESPACE CASACORE - END


#endif
