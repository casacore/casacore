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
#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Mathematics/NumericTraits.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Regex.h>
#include <aips/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeStatistics.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LatticeStatsBase.h>
#include <trial/Lattices/LCSlicer.h>
#include <trial/Tasking/PGPlotter.h>

#include <aips/iostream.h>


void doitFloat(LogIO& os);
void testFloat (LatticeStatistics<Float>& stats, const Vector<Float>& results,
                const Vector<Bool>& hasResult, const IPosition& shape);


int main (int argc, char **argv)
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
   ArrayLattice<Float> inLat(inArr);
   SubLattice<Float> subLat(inLat);
//
   Vector<Float> results(LatticeStatsBase::NSTATS);
   Vector<Bool> hasResult(LatticeStatsBase::NSTATS);
   hasResult = True;
//
   results(LatticeStatsBase::NPTS) = Float(shape(0));
   results(LatticeStatsBase::SUM) = sum(inArr);
   results(LatticeStatsBase::SUMSQ) = sum(square(inArr));
   results(LatticeStatsBase::MEDIAN) = median(inArr);
   Float t1 = fractile(inArr, 0.25);
   Float t2 = fractile(inArr, 0.75);
   results(LatticeStatsBase::QUARTILE) = (t2-t1)/2.0;
   results(LatticeStatsBase::MIN) = min(inArr);
   results(LatticeStatsBase::MAX) = max(inArr);
   results(LatticeStatsBase::MEAN) = mean(inArr);
   results(LatticeStatsBase::VARIANCE) = variance(inArr);
   results(LatticeStatsBase::SIGMA ) = stddev(inArr);
//
   hasResult(LatticeStatsBase::MEDABSDEVMED) = False;
   hasResult(LatticeStatsBase::RMS) = False;
   hasResult(LatticeStatsBase::FLUX) = False;

// Construct statistics object
   
   LatticeStatistics<Float> stats(subLat, os, False, False);

// Set axes

   Vector<Int> axes(1);
   axes = 0;
   AlwaysAssert(stats.setAxes(axes), AipsError);

// Test

   testFloat (stats, results, hasResult, shape);

// Test copy constructor - feeble test
     
    {
       LatticeStatistics<Float> stats2(stats);
       testFloat (stats2, results, hasResult, shape);
    }

// Test assignment operator - feeble test
   
    {
       LatticeStatistics<Float> stats2(stats);
       stats = stats2;
       testFloat (stats, results, hasResult, shape);
    }

// Test setNewLattice - feeble test

    {
       AlwaysAssert(stats.setNewLattice(subLat), AipsError);
       testFloat (stats, results, hasResult, shape);
    }
}


void testFloat (LatticeStatistics<Float>& stats, const Vector<Float>& results,
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

