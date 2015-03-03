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
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>
#include <casacore/lattices/Lattices/LatticeUtilities.h>
#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/scimath/Mathematics/ClassicalStatistics.h>

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
		doitFloat(os);

		Vector<Float> data(1000);
		Vector<Float>::iterator iter = data.begin();
		Vector<Float>::iterator end = data.end();
		uInt count = 0;
		while(iter != end) {
			*iter = count % 2 == 0 ? (Float)count : -(Float)(count*count);
			++iter;
			++count;
		}
		{
			ArrayLattice<Float> latt(data);
			SubLattice<Float> subLatt(latt);
			LatticeStatistics<Float> stats(subLatt);
			Array<Double> median, iqr, medabsdevmed, npts, q1, q3;
			stats.getStatistic(median, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(*median.begin() == -0.5, AipsError);
			stats.getStatistic(q1, LatticeStatsBase::Q1, False);
			AlwaysAssert(*q1.begin() == -251001, AipsError);
			stats.getStatistic(q3, LatticeStatsBase::Q3, False);
			AlwaysAssert(*q3.begin() == 498, AipsError);
			Vector<Float> range(2, 0.1);
			range[1] = 1001;
			stats.setInExCludeRange(range, Vector<Float>(), False);
			stats.getStatistic(median, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(*median.begin() == 500, AipsError);
			stats.getStatistic(iqr, LatticeStatsBase::QUARTILE, False);
			AlwaysAssert(*iqr.begin() == 500, AipsError);
			stats.getStatistic(medabsdevmed, LatticeStatsBase::MEDABSDEVMED, False);
			AlwaysAssert(*medabsdevmed.begin() == 250, AipsError);
			stats.getStatistic(q1, LatticeStatsBase::Q1, False);
			AlwaysAssert(*q1.begin() == 250, AipsError);
			stats.getStatistic(q3, LatticeStatsBase::Q3, False);
			AlwaysAssert(*q3.begin() == 750, AipsError);

			// exclude range
			stats.setInExCludeRange(Vector<Float>(), range, False);
			stats.getStatistic(median, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(*median.begin() == -249001, AipsError);
			stats.getStatistic(iqr, LatticeStatsBase::QUARTILE, False);
			AlwaysAssert(*iqr.begin() == 499000, AipsError);
			stats.getStatistic(medabsdevmed, LatticeStatsBase::MEDABSDEVMED, False);
			AlwaysAssert(*medabsdevmed.begin() == 216240, AipsError);
			stats.getStatistic(q1, LatticeStatsBase::Q1, False);
			AlwaysAssert(*q1.begin() == -561001, AipsError);
			stats.getStatistic(q3, LatticeStatsBase::Q3, False);
			AlwaysAssert(*q3.begin() == -62001, AipsError);

			// mask
			Vector<Bool> mask(1000);
			Vector<Bool>::iterator miter = mask.begin();
			Vector<Bool>::iterator mend = mask.end();
			count = 0;
			while (miter != mend) {
				*miter = count % 3 == 0;
				++miter;
				++count;
			}
			subLatt.setPixelMask(ArrayLattice<Bool>(mask), True);
			stats = LatticeStatistics<Float>(subLatt);
			stats.getStatistic(npts, LatticeStatsBase::NPTS, False);
			AlwaysAssert(*npts.begin() == 334, AipsError);
			stats.getStatistic(median, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(*median.begin() == -4.5, AipsError);
			stats.getStatistic(q1, LatticeStatsBase::Q1, False);
			AlwaysAssert(*q1.begin() == -251001, AipsError);
			stats.getStatistic(q3, LatticeStatsBase::Q3, False);
			AlwaysAssert(*q3.begin() == 498, AipsError);

			// include range
			stats.setInExCludeRange(range, Vector<Float>(), False);
			stats.getStatistic(median, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(*median.begin() == 501, AipsError);
			stats.getStatistic(iqr, LatticeStatsBase::QUARTILE, False);
			AlwaysAssert(*iqr.begin() == 498, AipsError);
			stats.getStatistic(medabsdevmed, LatticeStatsBase::MEDABSDEVMED, False);
			AlwaysAssert(*medabsdevmed.begin() == 249, AipsError);
			stats.getStatistic(q1, LatticeStatsBase::Q1, False);
			AlwaysAssert(*q1.begin() == 252, AipsError);
			stats.getStatistic(q3, LatticeStatsBase::Q3, False);
			AlwaysAssert(*q3.begin() == 750, AipsError);

			// exclude range
			stats.setInExCludeRange(Vector<Float>(), range, False);
			stats.getStatistic(npts, LatticeStatsBase::NPTS, False);
			AlwaysAssert(*npts.begin() == 168, AipsError);
			stats.getStatistic(median, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(*median.begin() == -248013, AipsError);
			stats.getStatistic(iqr, LatticeStatsBase::QUARTILE, False);
			AlwaysAssert(*iqr.begin() == 505008, AipsError);
			stats.getStatistic(medabsdevmed, LatticeStatsBase::MEDABSDEVMED, False);
			AlwaysAssert(*medabsdevmed.begin() == 216216, AipsError);
			stats.getStatistic(q1, LatticeStatsBase::Q1, False);
			AlwaysAssert(*q1.begin() == -567009, AipsError);
			stats.getStatistic(q3, LatticeStatsBase::Q3, False);
			AlwaysAssert(*q3.begin() == -62001, AipsError);

			// corner case when lattice is completely masked
			mask.set(False);
			subLatt.setPixelMask(ArrayLattice<Bool>(mask), True);
			stats = LatticeStatistics<Float>(subLatt);
			stats.getStatistic(npts, LatticeStatsBase::NPTS, False);
			AlwaysAssert(npts.size() == 0, AipsError);
		}
#if 0
		{
			// using setAlgorithm()
			ArrayLattice<Float> latt(data);
			SubLattice<Float> subLatt(latt);
			LatticeStatistics<Float> stats(subLatt);
			stats.setAlgorithm(StatisticsData::CLASSICAL);
			Array<Double> mean;
			Float expec = casacore::mean(data);
			stats.getStatistic(mean, LatticeStatsBase::MEAN, False);
			AlwaysAssert(near(*mean.begin(), expec), AipsError);
			stats.setAlgorithm(StatisticsData::HINGESFENCES);
			stats.getStatistic(mean, LatticeStatsBase::MEAN, False);
			AlwaysAssert(near(*mean.begin(), expec), AipsError);
			stats.configureHingesFences(0.0);
			stats.getStatistic(mean, LatticeStatsBase::MEAN, False);
			expec = -41960.081836;
			AlwaysAssert(near(*mean.begin(), expec), AipsError);
			Bool exceptionThrown = False;
			try {
				stats.configureFitToHalf();
			}
			catch (const AipsError& x) {
				exceptionThrown = True;
			}
			AlwaysAssert(exceptionThrown, AipsError);
			stats.setAlgorithm(StatisticsData::FITTOHALF);
			stats.configureFitToHalf(
					FitToHalfStatisticsData::CMEAN,
					FitToHalfStatisticsData::LE_CENTER
			);
			Array<Double> v;
			stats.getStatistic(v, LatticeStatsBase::MEAN, False);
			Double m = *v.begin();
			AlwaysAssert(near(m, casacore::mean(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(near(*v.begin(), m), AipsError);
			stats.getStatistic(v, LatticeStatsBase::NPTS, False);
			Int npts = (Int)*v.begin();
			AlwaysAssert(npts == 592, AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUM, False);
			Double sum = *v.begin();
			AlwaysAssert(near(sum, m*npts), AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUMSQ, False);
			AlwaysAssert(near(*v.begin(), 127119111260752.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MIN, False);
			AlwaysAssert(near(*v.begin(), casacore::min(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MAX, False);
			AlwaysAssert(near(*v.begin(), 2*m - casacore::min(data)), AipsError);
			IPosition minPos, maxPos;
			stats.getMinMaxPos(minPos, maxPos);
			AlwaysAssert(minPos.size() == 1 && minPos[0] == 999, AipsError);
			AlwaysAssert(maxPos.size() == 0, AipsError);
			stats.getStatistic(v, LatticeStatsBase::Q1, False);
			AlwaysAssert(near(*v.begin(), -497025.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::Q3, False);
			AlwaysAssert(near(*v.begin(), 161375.0), AipsError);

			stats.configureFitToHalf(
				FitToHalfStatisticsData::CMEAN,
				FitToHalfStatisticsData::GE_CENTER
			);
			stats.getStatistic(v, LatticeStatsBase::MEAN, False);
			m = *v.begin();
			AlwaysAssert(near(m, casacore::mean(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MEDIAN, False);
			AlwaysAssert(near(*v.begin(), m), AipsError);
			stats.getStatistic(v, LatticeStatsBase::NPTS, False);
			npts = (Int)*v.begin();
			AlwaysAssert(npts == 1408, AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUM, False);
			sum = *v.begin();
			AlwaysAssert(near(sum, m*npts), AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUMSQ, False);
			AlwaysAssert(near(*v.begin(), 72880554407048.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MIN, False);
			AlwaysAssert(near(*v.begin(), 2*m - casacore::max(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MAX, False);
			AlwaysAssert(near(*v.begin(), casacore::max(data)), AipsError);
			stats.getMinMaxPos(minPos, maxPos);
			AlwaysAssert(minPos.size() == 0, AipsError);
			AlwaysAssert(maxPos.size() == 1 && maxPos == 998, AipsError);

			stats.configureFitToHalf(
				FitToHalfStatisticsData::CMEDIAN,
				FitToHalfStatisticsData::LE_CENTER
			);
			stats.getStatistic(v, LatticeStatsBase::MEAN, False);
			m = *v.begin();
			AlwaysAssert(near(m, -0.5), AipsError);
			stats.getStatistic(v, LatticeStatsBase::NPTS, False);
			npts = (Int)*v.begin();
			AlwaysAssert(npts == 1000, AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUM, False);
			sum = *v.begin();
			AlwaysAssert(near(sum, m*npts), AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUMSQ, False);
			AlwaysAssert(near(*v.begin(), 199999000001300.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MIN, False);
			AlwaysAssert(near(*v.begin(), casacore::min(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MAX, False);
			AlwaysAssert(near(*v.begin(),2*m - casacore::min(data)), AipsError);
			stats.getMinMaxPos(minPos, maxPos);
			AlwaysAssert(minPos.size() == 1 && minPos[0] == 999, AipsError);
			AlwaysAssert(maxPos.size() == 0, AipsError);

			stats.configureFitToHalf(
				FitToHalfStatisticsData::CMEDIAN,
				FitToHalfStatisticsData::GE_CENTER
			);
			stats.getStatistic(v, LatticeStatsBase::MEAN, False);
			m = *v.begin();
			AlwaysAssert(near(m, -0.5), AipsError);
			stats.getStatistic(v, LatticeStatsBase::NPTS, False);
			npts = (Int)*v.begin();
			AlwaysAssert(npts == 1000, AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUM, False);
			sum = *v.begin();
			AlwaysAssert(near(sum, m*npts), AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUMSQ, False);
			AlwaysAssert(near(*v.begin(), 332833500.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MIN, False);
			AlwaysAssert(near(*v.begin(), 2*m - casacore::max(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MAX, False);
			AlwaysAssert(near(*v.begin(), casacore::max(data)), AipsError);
			stats.getMinMaxPos(minPos, maxPos);
			AlwaysAssert(minPos.size() == 0, AipsError);
			AlwaysAssert(maxPos.size() == 1 && maxPos[0] == 998, AipsError);

			stats.configureFitToHalf(
				FitToHalfStatisticsData::CVALUE,
				FitToHalfStatisticsData::LE_CENTER,
				65
			);
			stats.getStatistic(v, LatticeStatsBase::MEAN, False);
			m = *v.begin();
			AlwaysAssert(m == 65, AipsError);
			stats.getStatistic(v, LatticeStatsBase::NPTS, False);
			npts = (Int)*v.begin();
			AlwaysAssert(npts == 1066, AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUM, False);
			sum = *v.begin();
			AlwaysAssert(near(sum, m*npts), AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUMSQ, False);
			AlwaysAssert(near(*v.begin(), 200042675448460.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MIN, False);
			AlwaysAssert(near(*v.begin(), min(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MAX, False);
			AlwaysAssert(near(*v.begin(), 2*m - casacore::min(data)), AipsError);
			stats.getMinMaxPos(minPos, maxPos);
			AlwaysAssert(minPos.size() == 1 && minPos[0] == 999, AipsError);
			AlwaysAssert(maxPos.size() == 0, AipsError);

			stats.configureFitToHalf(
				FitToHalfStatisticsData::CVALUE,
				FitToHalfStatisticsData::GE_CENTER,
				65
			);
			stats.getStatistic(v, LatticeStatsBase::MEAN, False);
			m = *v.begin();
			AlwaysAssert(m == 65, AipsError);
			stats.getStatistic(v, LatticeStatsBase::NPTS, False);
			npts = (Int)*v.begin();
			AlwaysAssert(npts == 934, AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUM, False);
			sum = *v.begin();
			AlwaysAssert(near(sum, m*npts), AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUMSQ, False);
			AlwaysAssert(near(*v.begin(), 275539340.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MIN, False);
			AlwaysAssert(near(*v.begin(), 2*m - max(data)), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MAX, False);
			AlwaysAssert(near(*v.begin(), casacore::max(data)), AipsError);
			stats.getMinMaxPos(minPos, maxPos);
			AlwaysAssert(minPos.size() == 0, AipsError);
			AlwaysAssert(maxPos.size() == 1 && maxPos[0] == 998, AipsError);

			// mask
			Vector<Bool> mask(1000);
			Vector<Bool>::iterator miter = mask.begin();
			Vector<Bool>::iterator mend = mask.end();
			count = 0;
			while (miter != mend) {
				*miter = count % 3 == 0;
				++miter;
				++count;
			}
			subLatt.setPixelMask(ArrayLattice<Bool>(mask), True);
			stats = LatticeStatistics<Float>(subLatt);
			stats.setAlgorithm(StatisticsData::FITTOHALF);
			stats.configureFitToHalf(
				FitToHalfStatisticsData::CMEAN,
				FitToHalfStatisticsData::LE_CENTER
			);
			stats.getStatistic(v, LatticeStatsBase::MEAN, False);
			m = *v.begin();
			AlwaysAssert(near(m, -167083.5), AipsError);
			stats.getStatistic(v, LatticeStatsBase::NPTS, False);
			npts = (Int)*v.begin();
			AlwaysAssert(npts == 198, AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUM, False);
			sum = *v.begin();
			AlwaysAssert(near(sum, m*npts), AipsError);
			stats.getStatistic(v, LatticeStatsBase::SUMSQ, False);
			AlwaysAssert(near(*v.begin(), 42804555931071.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MIN, False);
			AlwaysAssert(near(*v.begin(), -998001.0), AipsError);
			stats.getStatistic(v, LatticeStatsBase::MAX, False);
			AlwaysAssert(near(*v.begin(), 2*-167083.5 - -998001.0), AipsError);
			stats.getMinMaxPos(minPos, maxPos);
			AlwaysAssert(minPos.size() == 1 && minPos[0] == 999, AipsError);
			AlwaysAssert(maxPos.size() == 0, AipsError);

		}
#endif
	}
	catch (const AipsError& x) {
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
   results(LatticeStatsBase::Q1) = t1;
   results(LatticeStatsBase::Q3) = t2;
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

   {
      IPosition minPos, maxPos;
      AlwaysAssert(stats.getMinMaxPos(minPos, maxPos), AipsError);
      AlwaysAssert(minPos.nelements()==1, AipsError);      
      AlwaysAssert(minPos(0)==0, AipsError);      
      AlwaysAssert(maxPos(0)=shape(0)-1, AipsError);
   }

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

