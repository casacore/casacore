//# LatticeHistograms.cc: generate histograms from a Lattice
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef LATTICES_LATTICEHISTOGRAMS_TCC
#define LATTICES_LATTICEHISTOGRAMS_TCC

#include <casacore/lattices/LatticeMath/LatticeHistograms.h>
#include <casacore/lattices/LatticeMath/LatticeHistSpecialize.h>
#include <casacore/lattices/LatticeMath/LatticeHistProgress.h>
#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/LatticeMath/LatticeApply.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/System/PGPlotter.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/iomanip.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/sstream.h>





namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Public functions

template <class T>
LatticeHistograms<T>::LatticeHistograms (const MaskedLattice<T>& lattice, 
                                         LogIO &os, Bool showProgress,
                                         Bool forceDisk)
: os_p(os),
  goodParameterStatus_p(True),
  error_p(""),
  pInLattice_p(0),
  pStoreLattice_p(0),
  pStats_p(0),
  binAll_p(True),
  needStorageLattice_p(True),
  doCumu_p(False),
  doGauss_p(False),
  doList_p(False),
  doLog_p(False),
  haveLogger_p(True),
  showProgress_p(showProgress),
  forceDisk_p(forceDisk),
  nBins_p(25)
//
// Constructor. 
//
{
   nxy_p.resize(0); 
   range_p.resize(0);
   blcParent_p.resize(0);

   if (setNewLattice(lattice)) {

// Cursor axes defaults to all
   
      Vector<Int> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      os_p << error_p << LogIO::EXCEPTION;
   }
}


template <class T>
LatticeHistograms<T>::LatticeHistograms (const MaskedLattice<T>& lattice, 
                                         Bool showProgress,
                                         Bool forceDisk)
: 
  goodParameterStatus_p(True),
  error_p(""),
  pInLattice_p(0),
  pStoreLattice_p(0),
  pStats_p(0),
  binAll_p(True),
  needStorageLattice_p(True),
  doCumu_p(False),
  doGauss_p(False),
  doList_p(False),
  doLog_p(False),
  haveLogger_p(False),
  showProgress_p(showProgress),
  forceDisk_p(forceDisk),
  nBins_p(25)
//
// Constructor. 
//
{
   nxy_p.resize(0); 
   range_p.resize(0);
   blcParent_p.resize(0);

   if (setNewLattice(lattice)) {

// Cursor axes defaults to all
   
      Vector<Int> cursorAxes;
      goodParameterStatus_p = setAxes(cursorAxes);
   } else {
      os_p << error_p << LogIO::EXCEPTION;
   }
}

 
template <class T>
LatticeHistograms<T>::LatticeHistograms(const LatticeHistograms<T> &other)
 : pInLattice_p(0),
   pStoreLattice_p(0),
   pStats_p(0)
//
// Copy constructor.  Storage lattice not copied.
//
{ 
   operator=(other);
}      


template <class T>
LatticeHistograms<T> &LatticeHistograms<T>::operator=(const LatticeHistograms<T> &other)
//
// Assignment operator.   Storage lattices not copied.
//
{
   if (this != &other) {
      
// Deal with pointer
      
      if (pInLattice_p!=0) delete pInLattice_p;
      pInLattice_p = other.pInLattice_p->cloneML();
      
// Delete storage and statistics objects.

      if (pStoreLattice_p != 0) {
         delete pStoreLattice_p;
         pStoreLattice_p = 0;
      }
//
      if (pStats_p != 0) {
         delete pStats_p;
         pStats_p = 0;
      }
      needStorageLattice_p = True;

// Do the rest
  
      os_p = other.os_p;
      binAll_p = other.binAll_p;
      goodParameterStatus_p = other.goodParameterStatus_p;
      doCumu_p = other.doCumu_p;
      doGauss_p = other.doGauss_p;
      doList_p = other.doList_p;
      doLog_p = other.doLog_p;
      haveLogger_p = other.haveLogger_p;
      showProgress_p = other.showProgress_p;
      nBins_p = other.nBins_p;
      cursorAxes_p = other.cursorAxes_p;
      displayAxes_p = other.displayAxes_p;
      plotter_p = other.plotter_p;
      nxy_p = other.nxy_p;
      range_p = other.range_p;
      blcParent_p = other.blcParent_p;
      forceDisk_p = other.forceDisk_p;
      error_p = other.error_p;
   }
   return *this;
}

 

template <class T>
LatticeHistograms<T>::~LatticeHistograms()
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
   if (pStats_p != 0) {
      delete pStats_p;
      pStats_p = 0;
   }
}


template <class T>
Bool LatticeHistograms<T>::setAxes (const Vector<Int>& axes)
//
// This function sets the cursor axes and the display axes
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

// Save current cursor axes

   Vector<Int> saveAxes(cursorAxes_p.copy());

// Set cursor arrays (can't assign to potentially zero length array)

   cursorAxes_p.resize(0);   
   cursorAxes_p = axes;

   if (cursorAxes_p.nelements() == 0) {
   
// User didn't give any axes.  Set them to all.
 
      cursorAxes_p.resize(pInLattice_p->ndim());
      for (uInt i=0; i<pInLattice_p->ndim(); i++) cursorAxes_p(i) = i;
   } else {
      for (uInt i=0; i<cursorAxes_p.nelements(); i++) {
         if (cursorAxes_p(i) < 0 || cursorAxes_p(i) > Int(pInLattice_p->ndim()-1)) {
            error_p = "Invalid cursor axes";
            return False;
         }
      }
   }

// Set the display axes 

   displayAxes_p.resize(0);
   displayAxes_p = IPosition::otherAxes(pInLattice_p->ndim(),
                                        cursorAxes_p).asVector();

// Signal that we have changed the axes and need new accumulation lattices
   
   if (saveAxes.nelements() != cursorAxes_p.nelements() ||
       !allEQ(saveAxes, cursorAxes_p)) needStorageLattice_p = True;

   return True;
}


template <class T>
Bool LatticeHistograms<T>::setNBins (const uInt& nBins)
//
// Set the number of bins
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

// Save number of bins

   const uInt saveNBins = nBins_p;

   if (nBins < 1) {
      error_p = "Invalid number of bins";
      goodParameterStatus_p = False;
      return False;
   } else {
      nBins_p = nBins;
   }

// Signal that we need a new accumulation lattice

   if (saveNBins != nBins_p) needStorageLattice_p = True;

   return True;
}


template <class T>
Bool LatticeHistograms<T>::setIncludeRange(const Vector<T>& include)
//
// Assign the desired inclusion range
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

// Save current ranges   
          
   Vector<T> saveRange(range_p.copy());
   

// CHeck    

   Bool noInclude;
   ostringstream os;
   if (!setInclude(range_p, noInclude, include, os)) {
      error_p = "Invalid pixel inclusion range";
      goodParameterStatus_p = False;
      return False;
   }
   binAll_p = noInclude;


// Signal that we need new accumulation lattices

   if (saveRange.nelements() != range_p.nelements() ||
       !allEQ(saveRange, range_p)) needStorageLattice_p = True;

   return True;
}



template <class T>
Bool LatticeHistograms<T>::setGaussian (const Bool& doGauss)
//
// Specify whether there should be a Gaussian overlay or not
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

   doGauss_p = doGauss;

   return True;
}


template <class T>
Bool LatticeHistograms<T>::setForm (const Bool& doLog, const Bool& doCumu)
//
// Specify whether the form of the histogram should be linear/log
// or cumulative or not.
// 
{
   if (!goodParameterStatus_p) {
      return False;
    }

    doLog_p = doLog;
    doCumu_p = doCumu;

    return True;
}


template <class T>
Bool LatticeHistograms<T>::setStatsList (const Bool& doList)
//
// See if user wants to list statistics as well 
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

   doList_p = doList;

   return True;
} 


template <class T>
Bool LatticeHistograms<T>::setPlotting(PGPlotter& plotter,
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
      error_p = "Input plotter is not attached";
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
  

// Plotting device and subplots.  nxy_p is set to [1,1] if zero length
 
   nxy_p.resize(0);
   nxy_p = nxy;
   ostringstream os;
   if (!LatticeStatsBase::setNxy(nxy_p, os)) {
      error_p = "Invalid number of subplots";
      goodParameterStatus_p = False;
      return False;
   }

   return True;
}


template <class T>
Bool LatticeHistograms<T>::setNewLattice(const MaskedLattice<T>& lattice)
//    
// Assign pointer to lattice
//
{ 
   if (!goodParameterStatus_p) {
      return False;
   }

   T* dummy = 0;
   DataType latticeType = whatType(dummy);
   if (latticeType !=TpFloat && latticeType != TpComplex) {
      ostringstream oss;
      oss << "Lattices of type " << latticeType << " are not currently supported" << endl;
      error_p = String(oss);
      goodParameterStatus_p = False;
      pInLattice_p = 0;
      return False;
   }

// Clone pointer
      
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
void LatticeHistograms<T>::closePlotting()
{  
   if (plotter_p.isAttached()) plotter_p.detach();
}
 

template <class T>
Bool LatticeHistograms<T>::display()
// 
// This function displays (plotting and listing) the requested
// histograms as a function of the display axes
//
{
   if (!goodParameterStatus_p) {
      return False;
   }


// Generate storage lattices if required

   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;
   }


// Display histograms
                 
   displayHistograms ();

   return True;
}




template <class T>
Bool LatticeHistograms<T>::getHistograms (Array<T>& values,
                                        Array<T>& counts)
//
// Retrieve histograms values and counts into arrays
//
{
   if (!goodParameterStatus_p) {
      return False;
   }


// Generate storage lattices if required
   
   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;      
   }


// Set up iterator to work through histogram storage lattice line by line
// Use the LatticeStepper (default) which will guarentee the access pattern.  
// There will be no overhang (as tile shape for first axis is length of axis)
  
   IPosition cursorShape(pStoreLattice_p->ndim(),1);
   cursorShape(0) = pStoreLattice_p->shape()(0);

   IPosition vectorAxis(1,0);
   vectorAxis(0) = 0;
   
// Make the stepper explicitly so we can specify the cursorAxes
// and then vectorCursor will cope with an axis of length 1
// (it is possible the user could ask for a histogram with one bin !)

   LatticeStepper histStepper(pStoreLattice_p->shape(), cursorShape,
                              vectorAxis, IPosition::makeAxisPath(pStoreLattice_p->ndim()));
   RO_LatticeIterator<T> histIterator(*pStoreLattice_p, histStepper);


// Resize output arrays and setup vector iterators

   counts.resize(pStoreLattice_p->shape());
   values.resize(pStoreLattice_p->shape());

   VectorIterator<T> valuesIterator(values);
   VectorIterator<T> countsIterator(counts);

   Vector<T> stats;
   T linearSum, linearYMax;

// Iterate through histogram storage lattice
   
   for (histIterator.reset(),valuesIterator.origin(),countsIterator.origin(); 
       !histIterator.atEnd(); histIterator++,valuesIterator.next(),countsIterator.next()) {
 
// Find statistics from the data that made this histogram 

      getStatistics (stats, histIterator.position());


// Extract the histogram in the appropriate form
 
      extractOneHistogram (linearSum, linearYMax, valuesIterator.vector(), 
                           countsIterator.vector(), stats,
                           histIterator.vectorCursor());
   }

   return True;
}



template <class T>
Bool LatticeHistograms<T>::getHistogram (Vector<T>& values,
                                        Vector<T>& counts,
                                        const IPosition& pos,
                                        const Bool posInLattice)
//
// Retrieve histogram values and counts from specified
// location into vectors
//
// Inputs:
//   posInLattice   If true the location is given as lattice coordinates
//                The non-display axis values will be ignored.
//                Otherwise the position should be for the
//                display axes only.
//
{
   if (!goodParameterStatus_p) {
      return False;
   }

// Make sure we have a correctly size position
      
   if (posInLattice) {
      if (pos.nelements() != pInLattice_p->ndim()) {
         error_p = "Incorrectly sized position given";
         values.resize(0);
         counts.resize(0);
         return False;
      }
   } else {
      if (pos.nelements() != displayAxes_p.nelements()) {
         error_p = "Incorrectly sized position given";
         values.resize(0);
         counts.resize(0);
         return False;
      }
   }

  
// Generate storage lattices if required
   
   if (needStorageLattice_p) {
      if (!generateStorageLattice()) return False;      
   }


// Set position for getting slice from storage lattice
         
   const uInt nDim = displayAxes_p.nelements();
   IPosition histPos(nDim+1,0);
   if (posInLattice) {
         
// Discard non display axes
          
      for (uInt i=0; i<nDim; i++) {
         histPos(i+1) = pos(displayAxes_p(i));
      }
   } else {
 
// Use position as is
 
      for (uInt i=0; i<nDim; i++) {
         histPos(i+1) = pos(i);
      }
   }

            
// Get histogram slice of integer counts (i.e. linear,
// not cumulative or logarithmic etc)
 
   IPosition sliceShape(nDim+1,1);
   sliceShape(0) = nBins_p;
   Array<T> intCounts;
   pStoreLattice_p->getSlice(intCounts, histPos, sliceShape,
                           IPosition(nDim+1,1), False);

// Copy integer counts to a Vector

   Vector<T> intCountsV(nBins_p);
   histPos = 0;
   for (uInt i=0; i<nBins_p; i++) {
      histPos(0) = i;
      intCountsV(i) = intCounts(histPos);
   }


// Get statistics slice.  

   Vector<AccumType> statsA;
   Vector<T> statsT;
   pStats_p->getStats(statsA, pos, posInLattice);
   statsT.resize(statsA.nelements());
   convertArray (statsT, statsA);

// Convert to desired form and make values vector too

   counts.resize(nBins_p);
   values.resize(nBins_p);
   T linearSum, linearYMax;
   extractOneHistogram (linearSum, linearYMax, values,
                        counts, statsT, intCountsV);

   return True;

}




// Private functions

template <class T>
Bool LatticeHistograms<T>::displayHistograms ()
//
// Display the histograms as a function of the display axes
//
{

// Set up for plotting

   if (plotter_p.isAttached()) {
      plotter_p.subp(nxy_p(0), nxy_p(1));
      plotter_p.ask(True);
      plotter_p.sch(1.2);
      plotter_p.svp(0.1,0.9,0.1,0.9);
   } else {
      error_p = "Plotter is not attached";
      return False;
   }
      
      
// Set up iterator to work through histogram storage lattice line by line.
// We don't use the TiledLineStepper to guarentee the access pattern is 
// row based rather than tile based.  There will be no overhang because
// the tile shape for the histogram axis is the size of the histogram

   IPosition cursorShape(pStoreLattice_p->ndim(),1);
   cursorShape(0) = pStoreLattice_p->shape()(0);

   IPosition vectorAxis(1); 
   vectorAxis(0) = 0;


// Make the stepper explicitly so we can specify the cursorAxes
// and then vectorCursor will cope with an axis of length 1
// (it is possible the user could ask for a histogram with one bin !)

   LatticeStepper histStepper(pStoreLattice_p->shape(), cursorShape,
                              vectorAxis, IPosition::makeAxisPath(pStoreLattice_p->ndim()));
   RO_LatticeIterator<T> histIterator(*pStoreLattice_p, histStepper);

// Histogram vectors and other bits and pieces
      
   Vector<T> counts(pStoreLattice_p->shape()(0));
   Vector<T> values(pStoreLattice_p->shape()(0));
   Vector<T> stats;
   T linearSum, linearYMax;
   IPosition latticePos(pInLattice_p->ndim(),0);
  
// Iterate through histogram storage lattice

   for (histIterator.reset(); !histIterator.atEnd(); histIterator++) {
   
// Find statistics from the data that made this histogram 

      getStatistics (stats, histIterator.position());

// Extract histogram in the form requested for plotting

      extractOneHistogram (linearSum, linearYMax, values, counts, stats,
                           histIterator.vectorCursor());

// Display the histogram

      if (!displayOneHistogram (linearSum, linearYMax, 
                                histIterator.position(), 
                                stats, values, counts, 
                                plotter_p)) return False;
   }
   return True;
}
 
 
template <class T>
Bool LatticeHistograms<T>::displayOneHistogram (const T& linearSum,
                                                const T& linearYMax,
                                                const IPosition& histPos,
                                                const Vector<T>& stats,
                                                const Vector<T>& values,
                                                const Vector<T>& counts,
                                                PGPlotter& plotter)

//
// Display the histogram and optionally the equivalent Gaussian
//
//  Inputs
//    histPos    location in histogram storage lattice of start of 
//               this histogram. Remember that the first axis
//               of the storage lattice has the counts.
//
{

// Are we going to see the Gaussian ?
 
   Bool doGauss2 = False;
   if (doGauss_p && stats(LatticeStatsBase::SIGMA)>0) doGauss2 = True;
 
// Set binwidth

   const T binWidth = LatticeHistSpecialize::setBinWidth(stats(LatticeStatsBase::MIN),
                                                         stats(LatticeStatsBase::MAX),
                                                         nBins_p);
// Do plots

   LatticeHistSpecialize::plot(plotter, doGauss_p, doCumu_p, doLog_p,
                               linearSum, linearYMax, binWidth, values, 
                               counts, stats, 0, 1, True);

// Write values of the display axes on the plot
 
   T* dummy = 0;
   DataType type = whatType(dummy);
   Float nchar = 0.5;
   if (type==TpComplex) nchar = 1.5;
   String coords = writeCoordinates(histPos);
   if (!writeDispAxesValues (coords, plotter, nchar)) return False;

   if (haveLogger_p && doList_p) {

// List pixel coordinates of display axes for this histogram

 
// Write statistics to a LogIO object

      os_p << coords << endl;
      listStatistics(os_p, stats, binWidth);

   }
   return True;
}

 

template <class T>
void LatticeHistograms<T>::extractOneHistogram (T& linearSum,
                                                T& linearYMax,
                                                Vector<T>& values, 
                                                Vector<T>& counts,
                                                const Vector<T>& stats, 
                                                const Vector<T>& intCounts)

//
// Extract this histogram, convert to the appropriate form
// and return the values and counts
//
{

// FIsh out min and max

   Vector<T> range(2);
   range(0) = stats(LatticeStatsBase::MIN);
   range(1) = stats(LatticeStatsBase::MAX);

// Set bin width  
      
   const uInt nBins = nBins_p;
   const T binWidth = LatticeHistSpecialize::setBinWidth(range(0), range(1), nBins);

// Copy histogram counts into output T array and generate
// values (abcissa) array
 
   T xx = range(0) + binWidth/2.0;
   linearYMax = -1.0;
   linearSum = 0.0;
   for (uInt i=0; i<intCounts.nelements(); i++) {
      values(i) = xx;
      counts(i) = intCounts(i);
      xx += binWidth;
      linearYMax = LattStatsSpecialize::max(linearYMax,counts(i));
      linearSum += counts(i);
   }
   linearSum = LatticeHistSpecialize::mul(linearSum, binWidth);

// Make histogram cumulative if desired
      
   if (doCumu_p) LatticeHistSpecialize::makeCumulative (counts, linearYMax, nBins, 1.0);
          

// Make histogram logarithmic if desired
         
   if (doLog_p) LatticeHistSpecialize::makeLogarithmic (counts, linearYMax, nBins);
}




template <class T>
Bool LatticeHistograms<T>::generateStorageLattice()
//
// Generate the histogram, and statistics storage lattices.
//
{
// Set the display axes vector if needed

   if (displayAxes_p.nelements()==0) {
      displayAxes_p.resize(0);
      displayAxes_p = IPosition::otherAxes(pInLattice_p->ndim(),
                                           cursorAxes_p).asVector();
   }

// Make the statistics object 

   if (!makeStatistics()) return False;


// Fill the histogram storage lattice

   makeHistograms();

   needStorageLattice_p = False;     
   return True;
}


template <class T> 
void LatticeHistograms<T>::getStatistics (Vector<T> &stats, 
                                          const IPosition& histPos) const
//
// Extract statistics slice for the given position in the
// histogram storage lattice.  
//
// Input:
//   histPos  The location in the histogram storage lattice
// Outputs
//  stats     The statistics for this chunk.  
{

// Discard the histogram axis location

   uInt n = displayAxes_p.nelements();
   IPosition pos;
   if (n > 0) {
      pos.resize(n);
      for (uInt i=0; i<n; i++) {
         pos(i) = histPos(i+1);
      }
   }

// Get the statistics
   
   Vector<AccumType> statsA;
   pStats_p->getStats(statsA, pos, False);
   stats.resize(statsA.nelements());
   convertArray (stats, statsA);
}

template <class T>
void LatticeHistograms<T>::listStatistics(LogIO& os,
                                          const Vector<T>& stats,
                                          T binWidth)
{

// Have to convert LogIO object to ostream before can apply 
// the manipulators

      const Int oPrec = 6;
      setStream(os.output(), oPrec);
      ostringstream os0, os1, os2, os3, os4, os5, os6, os7;
      setStream(os0, oPrec); setStream(os1, oPrec); setStream(os2, oPrec);
      setStream(os3, oPrec); setStream(os4, oPrec); setStream(os5, oPrec);
      setStream(os6, oPrec); setStream(os7, oPrec);
//
      T* dummy = 0;
      DataType type = whatType(dummy);
      Int oWidth;
      if (type==TpFloat) {  
         oWidth = 15;               //
      } else if (type==TpComplex) {
         oWidth = 33;               // (x, y)
      }
//
      os << "No. binned = ";
      os.output() << setw(oWidth) << Int64(std::real(stats(LatticeStatsBase::NPTS))+0.1) << endl;

      os << "Sum        = ";
      os0 << stats(LatticeStatsBase::SUM);
      os.output() << setw(oWidth) << String(os0) <<   "       Mean     = ";
      os1 << stats(LatticeStatsBase::MEAN);
      os.output() << setw(oWidth) << String(os1) << endl;
//
      os << "Variance   = ";
      os2 << stats(LatticeStatsBase::VARIANCE);
      os.output() << setw(oWidth) << String(os2);
//
      if (stats(LatticeStatsBase::VARIANCE)> 0.0) {
         os << "       Sigma    = ";
         os3 << stats(LatticeStatsBase::SIGMA);
         os.output() << setw(oWidth) << String(os3) << endl;
      } else {
         os << endl;
      }
      os << "Rms        = ";
      os4 << stats(LatticeStatsBase::RMS);
      os.output() << setw(oWidth) << String(os4) << endl;
 
      os << endl;  
      os << "Bin width  = ";
      os5 << binWidth;
      os.output() << setw(oWidth) << String(os5) << endl;
      os << "Min binned = ";
      os6 << stats(LatticeStatsBase::MIN);
      os.output() << setw(oWidth) << String(os6) << "       Max binned = ";
      os7 << stats(LatticeStatsBase::MAX);
      os.output() << setw(oWidth) << String(os7) << endl << endl << endl;
      os.post();
}


template <class T>
IPosition LatticeHistograms<T>::locHistInLattice(const IPosition& storagePosition,
                                                 Bool relativeToParent) const
//
// Given a location in the histogram storage lattice, convert those locations on
// the non-histogram axis (the histogram axis is the first one) to locations
// in the original parent lattice.  Optionally account for the location of the 
// subLattice in the parent lattice
//
{
   IPosition pos(storagePosition);
   for (uInt j=1; j<pos.nelements(); j++) {
     if (relativeToParent) {
        pos(j) = storagePosition(j) + blcParent_p(displayAxes_p(j-1));
     } else {
        pos(j) = storagePosition(j);
     }
   }
   return pos;  
}


template <class T>
Bool LatticeHistograms<T>::makeStatistics()
{

// Create LatticeStatistics object.  Show progress meter.

   if (pStats_p != 0) delete pStats_p;
   pStats_p = new LatticeStatistics<T>(*pInLattice_p, os_p, showProgress_p, forceDisk_p);

// Set state.  Make sure that the min/max is set to the
// user's include range if there is one.  LatticeHistograms
// only allows an inclusion range, and range_p is already
// filled with it.

   Vector<T> exclude;
   if (!pStats_p->setInExCludeRange(range_p, exclude, True)) return False;
   if (!pStats_p->setAxes(cursorAxes_p)) return False;

// We get an arbitary statistics slice here so as to
// activate the statistics object and make it a bit
// more obvious to the user the order in which things are done.

   Vector<AccumType> stats;
   IPosition pos(displayAxes_p.nelements(),0);
   if (!pStats_p->getStats(stats, pos, False)) return False;

   return True;
}



template <class T>
void LatticeHistograms<T>::makeHistograms()
{
   if (haveLogger_p) {
      os_p << LogIO::DEBUG1 << "Creating new histogram storage lattice" << LogIO::POST;
   }

// Set storage lattice shape.  The first axis is the histogram axis 
 
   IPosition storeLatticeShape;
   LatticeStatsBase::setStorageImageShape(storeLatticeShape, False, Int(nBins_p),
                                          displayAxes_p, pInLattice_p->shape());

// Set the storage lattice tile shape to the tile shape of the
// axes of the parent lattice from which it is created.
// For the histogram axis, set the tile shape to the number of bins
// (which probably won't be too big, but could be !)

    IPosition tileShape(storeLatticeShape.nelements(),1);
    for (uInt i=1; i<tileShape.nelements(); i++) {
       tileShape(i) = pInLattice_p->niceCursorShape()(displayAxes_p(i-1));
    }
    tileShape(0) = storeLatticeShape(0);


// Delete old histogram storage lattice
 
   if (pStoreLattice_p != 0) delete pStoreLattice_p;

// Create storage lattice

   uInt memory = HostInfo::memoryTotal()/1024;
   Double useMemory = Double(memory)/10.0;
   if (forceDisk_p) useMemory = 0.0;
   pStoreLattice_p = new TempLattice<T>(TiledShape(storeLatticeShape,
                                      tileShape), useMemory);

// Create collapser for LatticeApply

   HistTiledCollapser<T> collapser(pStats_p, nBins_p);
   LatticeHistProgress* pProgressMeter = 0;
   if (showProgress_p) pProgressMeter = new LatticeHistProgress();

// This is the first output axis (there is only one in IH) getting
// collapsed values
 
   Int newOutAxis = 0;

// Iterate through lattice and create histograms
// Output has to be a MaskedLattice, so make a writable SubLattice.

   SubLattice<T> outLatt (*pStoreLattice_p, True);
   LatticeApply<T,T>::tiledApply(outLatt, *pInLattice_p, 
                                 collapser, IPosition(cursorAxes_p),
                                 newOutAxis, pProgressMeter);
   if (pProgressMeter != 0) {
      delete pProgressMeter;
      pProgressMeter = 0;
   }
}


                



template <class T>
Bool LatticeHistograms<T>::setInclude(Vector<T>& range,
                                    Bool& noInclude,
                                    const Vector<T>& include,
                                    ostream& os)
//
// Take the user's data inclusion range
//
// Inputs:
//   include   Include range given by user. Zero length indicates
//             no include range
//   os        Output stream for reporting
// Outputs:
//   noInclude If True user did not give an include range
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
   return True;
}

template <class T>
String LatticeHistograms<T>::writeCoordinates(const IPosition& histPos) const
//
// Write pixel coordinates relative to parent lattice
//
{
   ostringstream oss;
   const Int nDisplayAxes = displayAxes_p.nelements();
   if (nDisplayAxes > 0) {   
      for (Int j=0; j<nDisplayAxes; j++) {
         oss << "Axis " << displayAxes_p(j) + 1 << "=" 
             << locHistInLattice(histPos,True)(j+1)+1;
         if (j < nDisplayAxes-1) oss << ", ";
      }
   }
   return String(oss);
}


template <class T>
Bool LatticeHistograms<T>::writeDispAxesValues (const String& coords,
                                                PGPlotter& plotter,
                                                Float nchar) const
{
   
// Fill the string stream with the name and value of each display axis
  
   const Int nDisplayAxes = displayAxes_p.nelements();
   if (nDisplayAxes > 0) {

// Write on plot
 
      Vector<Float> box(8);
      box = plotter.qtxt (0.0, 0.0, 0.0, 0.0, "X");
      Float dx = box(3) - box(0);

      const char* tLabel = coords.chars();
      box = plotter.qtxt (0.0, 0.0, 0.0, 0.0, tLabel);
      Float dy = box(5) - box(4);
                     
      Vector<Float> win = plotter.qwin();
      Float mx = win(0) + dx; 
      Float my = win(3) + nchar*dy;
//      
      Int tbg = plotter.qtbg();
      plotter.stbg(0);
      plotter.ptxt (mx, my, 0.0, 0.0, tLabel);
      plotter.stbg(tbg);
   }

   return True;
}


template <class T>
void LatticeHistograms<T>::setStream (ostream& os, Int oPrec)
{
    os.fill(' ');
    os.precision(oPrec);
    os.setf(ios::scientific, ios::floatfield);
    os.setf(ios::left, ios::adjustfield);
}
   


// HistTiledCollapser
   
 
template <class T>
HistTiledCollapser<T>::HistTiledCollapser(LatticeStatistics<T>* pStats, uInt nBins)
: pStats_p(pStats),
  nBins_p(nBins)
{;}
   
template <class T>
HistTiledCollapser<T>::~HistTiledCollapser<T>() {}

template <class T>
void HistTiledCollapser<T>::init (uInt nOutPixelsPerCollapse)
{
    AlwaysAssert (nOutPixelsPerCollapse == nBins_p, AipsError);
}   
   
template <class T>
void HistTiledCollapser<T>::initAccumulator (uInt n1, uInt n3)
//
// pHist_p contains the histograms for each chunk
// It is T not uInt so we can handle Complex types
{
   pHist_p = new Block<T>(nBins_p*n1*n3);
   pHist_p->set(0);
//          
   n1_p = n1;
   n3_p = n3;
}


template <class T>
void HistTiledCollapser<T>::process (
	uInt index1, uInt index3, const T* pInData,
	const Bool* pInMask, uInt dataIncr, uInt maskIncr,
	uInt nrval, const IPosition& startPos, const IPosition&
) {
//
// Process the data in the current chunk.   Everything in this
// chunk belongs in one output location in the accumulation
// lattices
//

// Fish out the min and max for this chunk of the data 
// from the statistics object

   typedef typename NumericTraits<T>::PrecisionType AccumType; 
   Vector<AccumType> stats;
   pStats_p->getStats(stats, startPos, True);

// Assignment from AccumType to T ok (e.g. Double to FLoat)

   Vector<T> clip(2);
   clip(0) = stats(LatticeStatsBase::MIN);
   clip(1) = stats(LatticeStatsBase::MAX);

// Set histogram bin width
   
   const T binWidth = LatticeHistSpecialize::setBinWidth(clip(0), clip(1), nBins_p);

// Fill histograms.  

   uInt offset = (nBins_p*index1) + (nBins_p*n1_p*index3);
   LatticeHistSpecialize::process(
		   pInData, pInMask, pHist_p, clip,
		   binWidth, offset, nrval,
		   nBins_p, dataIncr, maskIncr
   );
}



template <class T>
void HistTiledCollapser<T>::endAccumulator(Array<T>& result,
                                           Array<Bool>& resultMask,
                                           const IPosition& shape)
{


// Reshape arrays.  The mask is always true.  Any locations
// in the storage lattice for which there were no valid points
// will have the NPTS field set to zero.  That is what
// we use to effectively mask it.  
          
    resultMask.resize(shape);
    resultMask.set(True);
    result.resize(shape);
//
    Bool deleteRes;
    T* res = result.getStorage (deleteRes);
    T* resptr = res;
    const T* histPtr = pHist_p->storage();

// The histogram storage lattice has the logical shape
// [nBins, n1, n3].  

    for (uInt k=0; k<nBins_p*n1_p*n3_p; k++) {
       *resptr++ = *histPtr++;
    }
    
    result.putStorage (res, deleteRes);
    delete pHist_p;
}      

} //# NAMESPACE CASACORE - END


#endif
