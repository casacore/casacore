//# tLatticeStatistics.cc: test LatticeStatistics class
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
// 
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Logging.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeStatistics.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/LatticeStatsBase.h>
#include <casacore/lattices/Lattices/LatticeUtilities.h>
#include <casacore/lattices/Lattices/LCSlicer.h>
#include <casacore/casa/System/PGPlotter.h>

#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doitFloat(LogIO& os);
void do1DFloat (const Vector<Float>& results,
                const Vector<Bool>& hasResult, 
                const Array<Float>& inArr,
                LogIO& os);
void do2DFloat (const Vector<Float>& results,
                const Vector<Bool>& hasResult, 
                const Array<Float>& inArr,
                LogIO& os);
void test1DFloat (LatticeStatistics<Float>& stats, const Vector<Float>& results,
                  const Vector<Bool>& hasResult, const IPosition& shape);
void test2DFloat (LatticeStatistics<Float>& stats, const Vector<Float>& results,
                  const Vector<Bool>& hasResult, const IPosition& shape);


int main()
{
   try {
      LogOrigin lor("tLatticeStatistics", "main()", WHERE);
      LogIO os(lor);
//
      doitFloat(os);
   } catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
  } 
  return 0;
}
   
 
void doitFloat (LogIO& os) 
{   
      
// Construct lattice

   IPosition shape(1);
   shape = 64;
   Array<Float> inArr(shape);
   indgen(inArr);
//
//
   Vector<Float> results(LatticeStatsBase::NSTATS);
   Vector<Bool> hasResult(LatticeStatsBase::NSTATS);
   hasResult = True;
//
   results(LatticeStatsBase::NPTS) = Float(shape(0));
   results(LatticeStatsBase::SUM) = sum(inArr);
   results(LatticeStatsBase::SUMSQ) = sum(square(inArr));
   Float med = median(inArr);
   results(LatticeStatsBase::MEDIAN) = med;
   results(LatticeStatsBase::MEDABSDEVMED) = median(abs(inArr-med));
   Float t1 = fractile(inArr, 0.25);
   Float t2 = fractile(inArr, 0.75);
   results(LatticeStatsBase::QUARTILE) = (t2-t1);
   results(LatticeStatsBase::MIN) = min(inArr);
   results(LatticeStatsBase::MAX) = max(inArr);
   results(LatticeStatsBase::MEAN) = mean(inArr);
   results(LatticeStatsBase::VARIANCE) = variance(inArr);
   results(LatticeStatsBase::SIGMA ) = stddev(inArr);
   results(LatticeStatsBase::RMS ) = rms(inArr);
//
   hasResult(LatticeStatsBase::FLUX) = False;

// Make 1D Lattice and test

   do1DFloat(results, hasResult, inArr, os);

// Make 2D lattice and test

   do2DFloat(results, hasResult, inArr, os);
}


void do1DFloat (const Vector<Float>& results,
                const Vector<Bool>& hasResult, 
                const Array<Float>& inArr, LogIO& os)
{
   const IPosition shape = inArr.shape();
   ArrayLattice<Float> inLat(inArr);
   SubLattice<Float> subLat(inLat);
   LatticeStatistics<Float> stats(subLat, os, False, False);

// Test

   test1DFloat (stats, results, hasResult, shape);

// Test copy constructor - feeble test
     
   {
      LatticeStatistics<Float> stats2(stats);
      test1DFloat (stats2, results, hasResult, shape);
   }

// Test assignment operator - feeble test
   
   {
      LatticeStatistics<Float> stats2(stats);
      stats = stats2;
      test1DFloat (stats, results, hasResult, shape);
   }

// Test setNewLattice - feeble test

   {
      AlwaysAssert(stats.setNewLattice(subLat), AipsError);
      test1DFloat (stats, results, hasResult, shape);
   }
}


void do2DFloat (const Vector<Float>& results,
                const Vector<Bool>& hasResult, 
                const Array<Float>& arr, LogIO& os)
{
   uInt nX = arr.shape()(0);
   uInt nY = 20;
   IPosition shape(2,nX,nY);

// Fill Lattice with replicated rows

   ArrayLattice<Float> lat(shape);
   Slicer slice(IPosition(2,0,0),shape,Slicer::endIsLength);
   LatticeUtilities::replicate (lat, slice, arr);
   SubLattice<Float> subLat(lat);

// Make LS object and set axes so that we work out stats
// over first axis as a function of nY replicated rows

   LatticeStatistics<Float> stats(subLat, os, False, False);
   Vector<Int> axes(1);
   axes = 0;
   AlwaysAssert(stats.setAxes(axes), AipsError);

// Test

   test2DFloat (stats, results, hasResult, shape);

// Test copy constructor - feeble test
     
   {
      LatticeStatistics<Float> stats2(stats);
      test2DFloat (stats2, results, hasResult, shape);
   }

// Test assignment operator - feeble test
   
   {
      LatticeStatistics<Float> stats2(stats);
      stats = stats2;
      test2DFloat (stats, results, hasResult, shape);
   }

// Test setNewLattice - feeble test

   {
      AlwaysAssert(stats.setNewLattice(subLat), AipsError);
      test2DFloat (stats, results, hasResult, shape);
   }
}



void test1DFloat (LatticeStatistics<Float>& stats, const Vector<Float>& results,
                  const Vector<Bool>& hasResult, const IPosition& shape)
{
   AlwaysAssert(stats.displayAxes().nelements()==0, AipsError);
//
   typedef NumericTraits<Float>::PrecisionType AccumType;
   Double tol = 1.0e-6;
//
   {
      IPosition pos(1,0);
      Vector<AccumType> data;
      AlwaysAssert(stats.getStats(data, pos, True), AipsError);
   }
//
   {
      const Int nStats = LatticeStatsBase::NSTATS;
      for (Int i=0; i<nStats; i++) {
        Array<AccumType> a;
        LatticeStatsBase::StatisticsTypes t = static_cast<LatticeStatsBase::StatisticsTypes>(i);
        IPosition pos(1,0);
//
        if (t==LatticeStatsBase::FLUX) {
           AlwaysAssert(!stats.getStatistic (a, t, True), AipsError);
        } else {
           AlwaysAssert(stats.getStatistic (a, t, True), AipsError);
        }
        if (hasResult(i)) {
           AlwaysAssert(a.shape()==IPosition(1,1),AipsError);
           AlwaysAssert(near(a(pos),results(i),tol), AipsError);
        }
//
        Array<Float> b;
        if (t==LatticeStatsBase::FLUX) {
           AlwaysAssert(!stats.getConvertedStatistic (b, t, True), AipsError);
        } else {
           AlwaysAssert(stats.getConvertedStatistic (b, t, True), AipsError);
        }
        if (hasResult(i)) {
           AlwaysAssert(b.shape()==IPosition(1,1),AipsError);
           AlwaysAssert(near(b(pos),results(i),tol), AipsError);
        }
      }
   }
//  
   {
      IPosition minPos, maxPos;
      AlwaysAssert(stats.getMinMaxPos(minPos, maxPos), AipsError);
      AlwaysAssert(minPos.nelements()==1, AipsError);      
      AlwaysAssert(minPos(0)==0, AipsError);      
      AlwaysAssert(maxPos(0)=shape(0)-1, AipsError);
   }
//
   {
      Float dMin, dMax;
      AlwaysAssert(stats.getFullMinMax (dMin, dMax), AipsError);
      AlwaysAssert(near(results(LatticeStatsBase::MIN),dMin,tol), AipsError);
      AlwaysAssert(near(results(LatticeStatsBase::MAX),dMax,tol), AipsError);
   }
}


void test2DFloat (LatticeStatistics<Float>& stats, const Vector<Float>& results,
                  const Vector<Bool>& hasResult, const IPosition& shape)
{
   AlwaysAssert(shape.nelements()==2,AipsError);
   const Vector<Int> dA = stats.displayAxes();
   AlwaysAssert(dA.nelements()==1, AipsError);
   AlwaysAssert(dA(0)==1, AipsError);
   const uInt nY = shape(1);
//
   typedef NumericTraits<Float>::PrecisionType AccumType;
   Double tol = 1.0e-6;
//
   {
      IPosition pos(2,0,0);
      Vector<AccumType> data;
      AlwaysAssert(stats.getStats(data, pos, True), AipsError);
      AlwaysAssert(data.shape()==IPosition(1,LatticeStatsBase::NSTATS),AipsError);
   }

// Check stats correct for each row 

   {
      const Int nStats = LatticeStatsBase::NSTATS;
      for (Int i=0; i<nStats; i++) {
        Array<AccumType> a;
        LatticeStatsBase::StatisticsTypes t = static_cast<LatticeStatsBase::StatisticsTypes>(i);
        IPosition pos(1,0);
//
        if (t==LatticeStatsBase::FLUX) {
           AlwaysAssert(!stats.getStatistic (a, t, True), AipsError);
        } else {
           AlwaysAssert(stats.getStatistic (a, t, True), AipsError);
        }
        if (hasResult(i)) {
           AlwaysAssert(a.shape()==IPosition(1,nY),AipsError);
           for (uInt j=0; j<nY; j++) {
              pos(0) = j;
              AlwaysAssert(near(a(pos),results(i),tol), AipsError);
           }
        }
//
        Array<Float> b;
        if (t==LatticeStatsBase::FLUX) {
           AlwaysAssert(!stats.getConvertedStatistic (b, t, True), AipsError);
        } else {
           AlwaysAssert(stats.getConvertedStatistic (b, t, True), AipsError);
        }
        if (hasResult(i)) {
           AlwaysAssert(b.shape()==IPosition(1,nY),AipsError);
           for (uInt j=0; j<nY; j++) {
              pos(0) = j;
              AlwaysAssert(near(b(pos),results(i),tol), AipsError);
           }
        }
      }
   }
//  
   {
      Float dMin, dMax;
      AlwaysAssert(stats.getFullMinMax (dMin, dMax), AipsError);
      AlwaysAssert(near(results(LatticeStatsBase::MIN),dMin,tol), AipsError);
      AlwaysAssert(near(results(LatticeStatsBase::MAX),dMax,tol), AipsError);
   }
}

