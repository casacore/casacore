//# tStatAcc.cc: Test program for class StatAcc
//# Copyright (C) 1999,2000,2001
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
//# $Id: tStatAcc.cc 20329 2008-06-06 07:59:22Z gervandiepen $

#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays.h>
#include <casacore/scimath/Mathematics/FitToHalfStatistics.h>
#include <casacore/casa/Exceptions/Error.h>

#include <vector>

#include <casacore/casa/namespace.h>

#define COMMA ,

int main() {
    try {
    	vector<Double> v0(5);
    	v0[0] = 2;
    	v0[1] = 1;
    	v0[2] = 1.5;
    	v0[3] = 4;
    	v0[4] = 2.5;
    	vector<Double> v1(3);
    	v1[0] = 5;
    	v1[1] = 8;
    	v1[2] = 10;
    	{
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 3.4), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(sd.mean == 2.2, AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		Double npts = 6;
    		Double sumsq = 32.98;
    		Double nvariance = 3.94;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, 13.2), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    		);
    	}
    	{
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::GE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 4.0), AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 3, AipsError);
    		AlwaysAssert(sd.mean == 2.2, AipsError);
    		AlwaysAssert(near(*sd.min, 0.4), AipsError);
    		AlwaysAssert(sd.minpos.first == -1, AipsError);
    		AlwaysAssert(sd.minpos.second == -1, AipsError);
    		Double npts = 4;
    		Double sumsq = 26.02;
    		Double nvariance = 6.66;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, 8.8), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 3),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    		);
    	}
    	{
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEDIAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 3.0), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(sd.mean == 2, AipsError);
    		AlwaysAssert(near(*sd.min, 1.0), AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		Double npts = 6;
    		Double sumsq = 26.5;
    		Double nvariance = 2.5;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, 12.0), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    	    	);
    	}
    	{
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEDIAN, FitToHalfStatisticsData::GE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 4.0), AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 3, AipsError);
    		AlwaysAssert(sd.mean == 2, AipsError);
    		AlwaysAssert(near(*sd.min, 0.0), AipsError);
    		AlwaysAssert(sd.minpos.first == -1, AipsError);
    		AlwaysAssert(sd.minpos.second == -1, AipsError);
    		Double npts = 6;
    		Double sumsq = 32.5;
    		Double nvariance = 8.5;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, 12.0), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 3),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    		);
    	}
    	{
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 3
    		);
    		fh.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 5.0), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(sd.mean == 3, AipsError);
    		AlwaysAssert(near(*sd.min, 1.0), AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		Double npts = 8;
    		Double sumsq = 87;
    		Double nvariance = 15;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, 24.0), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    		);
    	}
    	{
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 2.5
    		);
    		fh.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 4.0), AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 3, AipsError);
    		AlwaysAssert(sd.mean == 2.5, AipsError);
    		AlwaysAssert(near(*sd.min, 1.0), AipsError);
    		AlwaysAssert(sd.minpos.first == -1, AipsError);
    		AlwaysAssert(sd.minpos.second == -1, AipsError);
    		Double npts = 4;
    		Double sumsq = 29.5;
    		Double nvariance = 4.5;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, 10.0), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 3),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    		);
    	}
    	Double k[] = {1.5, 1, 2, 4, 2.5};
    	{
    		// just another way of specifying the data
    		FitToHalfStatistics<Double, Double*, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(k, 5);
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 3.4), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(sd.mean == 2.2, AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		Double npts = 6;
    		Double sumsq = 32.98;
    		Double nvariance = 3.94;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, 13.2), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    		);
    	}
    	{
    		// two datasets
    		// 2, 1, 1.5, 4, 2.5
    		// 5, 8, 10
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		StatsData<Double> sd = fh.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 7.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(sd.mean == 4.25, AipsError);
    		AlwaysAssert(*sd.min == 1.0, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		Double npts = 10;
    		Double sumsq = 233.25;
    		Double nvariance = 52.625;
    		Double mean = 4.25;
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*npts), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
    		);
    		// Now reverse the order that the datasets were added. results
    		// should be the same except for min dataset location
    		fh.setData(v1.begin(), v1.size());
    		fh.addData(v0.begin(), v0.size());
            sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 7.5), AipsError);
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(near(sd.mean, mean), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == 1, AipsError);
            AlwaysAssert(sd.minpos.second == 1, AipsError);
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, mean*npts), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
            	fh.getStatisticIndex(StatisticsData::MAX)
            	== std::pair<Int64 COMMA Int64>(-1, -1),
            	AipsError
            );
            AlwaysAssert(
            	fh.getStatisticIndex(StatisticsData::MIN)
            	== std::pair<Int64 COMMA Int64>(1, 1),
            	AipsError
            );
            AlwaysAssert(fh.getStatistic(
            	StatisticsData::NPTS) == npts, AipsError
            );
            AlwaysAssert(
            	near(
            		fh.getStatistic(StatisticsData::RMS),
            		sqrt(sumsq/npts)
            	), AipsError
            );
    	}

    	{
    		// Verify class does not support computing stats as
    		// datasets are added
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		Bool exceptionRaised = False;
    		try {
    			fh.setCalculateAsAdded(True);
    		}
    		catch (AipsError& x) {
    			exceptionRaised = True;
    		}
    		AlwaysAssert(exceptionRaised, AipsError);
    	}

    	{
    		// two datasets, stride = 2,1
    		// 2, 1.5, 2.5
    		// 5, 8, 10
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size(), 2);
    		fh.addData(v1.begin(), v1.size());
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 6;
    		Double sumsq = 568.0/3.0;
    		Double nvariance = 295.0/6.0;
    		Double mean = 29.0/6.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 1.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 1.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 2, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*npts), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 2),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/npts)
    			), AipsError
    		);
    	}
    	{
    		// data ranges
    		// 4, 2.5
    		// 8
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 5;
    		r0[0].second = -5;
    		Bool expectedFail = False;
    		try {
    			fh.setData(v0.begin(), 3, r0);
    		}
    		catch (const AipsError& x) {
    			expectedFail = True;
    		}
    		AlwaysAssert(expectedFail, AipsError);
    		r0[0].first = 2.4;
    		r0[0].second = 6;
    		vector<std::pair<Double, Double> > r1(2);
    		r1[0].first = 9;
    		r1[0].second = 11;
    		r1[1].first = 2;
    		r1[1].second = 7;
    		fh.setData(v0.begin(), v0.size(), r0);
    		fh.addData(v1.begin(), v1.size(), r1, False);
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 4;
    		Double sumsq = 1903.0/18.0;
    		Double nvariance =  221.0/18.0;
    		Double mean = 14.5/3.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 2.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*npts), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 4),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/npts)
    			), AipsError
    		);
    	}
    	{
    		// mask
    		// 4, 2.5
    		// 8
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Bool> m0(v0.size());
    		m0[0] = False;
    		m0[1] = False;
    		m0[2] = False;
    		m0[3] = True;
    		m0[4] = True;
    		vector<Bool> m1(v1.size());
    		m1[0] = False;
    		m1[1] = True;
    		m1[2] = False;
    		fh.setData(v0.begin(), m0.begin(), v0.size());
    		fh.addData(v1.begin(), m1.begin(), v1.size());
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 4;
    		Double sumsq = 1903.0/18.0;
    		Double nvariance =  221.0/18.0;
    		Double mean = 14.5/3.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 2.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*npts), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 4),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/npts)
    			), AipsError
    		);
    	}
    	{
    		// mask and ranges
    		// 4, 2.5
    		// 8
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Bool> m0(v0.size());
    		m0[0] = False;
    		m0[1] = True;
    		m0[2] = True;
    		m0[3] = True;
    		m0[4] = True;
    		vector<Bool> m1(v1.size());
    		m1[0] = True;
    		m1[1] = True;
    		m1[2] = False;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 0.9;
    		r0[0].second = 1.6;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 6;
    		r1[0].second = 9;
    		fh.setData(v0.begin(), m0.begin(), v0.size(), r0, False);
    		fh.addData(v1.begin(), m1.begin(), v1.size(), r1, True);
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 4;
    		Double sumsq = 1903.0/18.0;
    		Double nvariance =  221.0/18.0;
    		Double mean = 14.5/3.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 2.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*npts), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 4),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/npts)
    			), AipsError
    		);
    	}
    	{
    		// weights
    		// 1, 1.5, 4, 2.5
    		// 5, 8, 10
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		vector<Double> w1(v1.size());
    		w1[0] = 1;
    		w1[1] = 2;
    		w1[2] = 3;
    		fh.setData(v0.begin(), w0.begin(), w0.size());
    		fh.addData(v1.begin(), w1.begin(), w1.size());
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 8;
    		Double sumofweights = 28;
    		Double sumsq = 641.44;
    		Double nvariance =  123.72;
    		Double mean = 4.3;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 1), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/sumofweights)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(sumofweights - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*sumofweights), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(sumofweights - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/sumofweights)
    			), AipsError
    		);
    	}
    	{
    		// weights and ranges
    		// 4, 2.5
    		// 8
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		vector<Double> w1(v1.size());
    		w1[0] = 1;
    		w1[1] = 2;
    		w1[2] = 3;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 0.9;
    		r0[0].second = 1.6;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 6;
    		r1[0].second = 9;
    		fh.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
    		fh.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 4;
    		Double sumofweights = 18;
    		Double sumsq = 154146.0/484.0;
    		Double nvariance = 11568.0/484.0;
    		Double mean = 44.5/11.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 2.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/sumofweights)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(sumofweights - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*sumofweights), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(sumofweights - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 4),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/sumofweights)
    			), AipsError
    		);
    	}
    	{
    		// weights, ranges, and masks
    		// 4, 2.5
    		// 8
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		vector<Double> w1(v1.size());
    		w1[0] = 1;
    		w1[1] = 2;
    		w1[2] = 3;
    		vector<Bool> m0(v0.size());
    		m0[0] = True;
    		m0[1] = True;
    		m0[2] = True;
    		m0[3] = True;
    		m0[4] = True;
    		vector<Bool> m1(v1.size());
    		m1[0] = True;
    		m1[1] = True;
    		m1[2] = False;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 0.9;
    		r0[0].second = 1.6;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 6;
    		r1[0].second = 12;
    		fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
    		fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 4;
    		Double sumofweights = 18;
    		Double sumsq = 154146.0/484.0;
    		Double nvariance = 11568.0/484.0;
    		Double mean = 44.5/11.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 2.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/sumofweights)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(sumofweights - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*sumofweights), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(sumofweights - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 4),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/sumofweights)
    			), AipsError
    		);
    	}
    	{
    		// weights, masks
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		vector<Double> w1(v1.size());
    		w1[0] = 1;
    		w1[1] = 2;
    		w1[2] = 3;
    		vector<Bool> m0(v0.size());
    		m0[0] = True;
    		m0[1] = False;
    		m0[2] = False;
    		m0[3] = True;
    		m0[4] = True;
    		vector<Bool> m1(v1.size());
    		m1[0] = False;
    		m1[1] = True;
    		m1[2] = False;
    		fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
    		fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
    		StatsData<Double> sd = fh.getStatistics();
    		Double npts = 4;
    		Double sumofweights = 18;
    		Double sumsq = 154146.0/484.0;
    		Double nvariance = 11568.0/484.0;
    		Double mean = 44.5/11.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(near(*sd.max, 2*mean - 2.5), AipsError);
    		AlwaysAssert(sd.maxpos.first == -1, AipsError);
    		AlwaysAssert(sd.maxpos.second == -1, AipsError);
    		AlwaysAssert(near(sd.mean, mean), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == npts, AipsError);
    		AlwaysAssert(near(sd.rms, sqrt(sumsq/sumofweights)), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(nvariance/(sumofweights - 1))), AipsError);
    		AlwaysAssert(near(sd.sum, mean*sumofweights), AipsError);
    		AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
    		AlwaysAssert(near(sd.variance, nvariance/(sumofweights - 1)), AipsError);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(-1, -1),
    			AipsError
    		);
    		AlwaysAssert(
    			fh.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 4),
    			AipsError
    		);
    		AlwaysAssert(fh.getStatistic(
    			StatisticsData::NPTS) == npts, AipsError
    		);
    		AlwaysAssert(
    			near(
    				fh.getStatistic(StatisticsData::RMS),
    				sqrt(sumsq/sumofweights)
    			), AipsError
    		);
    	}
    	{
    		// getMinMax(), two datasets
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1, AipsError);
    		AlwaysAssert(mymax == 7.5, AipsError);
    		fh = FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator>(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::GE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == -1.5, AipsError);
    		AlwaysAssert(mymax == 10, AipsError);
    	}
    	{
    		// getMinMax(), two datasets, stride = 2,1
    		// 2, 1.5, 2.5
    		// 5, 8, 10
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size(), 2);
    		fh.addData(v1.begin(), v1.size());
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1.5, AipsError);
    		AlwaysAssert(near(mymax, 2*29.0/6.0 - 1.5), AipsError);
    	}
    	{
    		// getMaxMin(), data ranges
    		// 2.5, 4
    		// 8
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 2.4;
    		r0[0].second = 6;
    		vector<std::pair<Double, Double> > r1(2);
    		r1[0].first = 9;
    		r1[0].second = 11;
    		r1[1].first = 2;
    		r1[1].second = 7;
    		fh.setData(v0.begin(), v0.size(), r0);
    		fh.addData(v1.begin(), v1.size(), r1, False);
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(near(mymax, 43.0/6.0), AipsError);
    	}
    	{
    		// getMinMax(), mask
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Bool> m0(v0.size());
    		m0[0] = False;
    		m0[1] = False;
    		m0[2] = False;
    		m0[3] = True;
    		m0[4] = True;
    		vector<Bool> m1(v1.size());
    		m1[0] = False;
    		m1[1] = True;
    		m1[2] = False;
    		fh.setData(v0.begin(), m0.begin(), v0.size());
    		fh.addData(v1.begin(), m1.begin(), v1.size());
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(near(mymax, 43.0/6.0), AipsError);
    	}
    	{
    		// getMinMax(), mask and ranges
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Bool> m0(v0.size());
    		m0[0] = False;
    		m0[1] = True;
    		m0[2] = True;
    		m0[3] = True;
    		m0[4] = True;
    		vector<Bool> m1(v1.size());
    		m1[0] = True;
    		m1[1] = True;
    		m1[2] = False;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 0.9;
    		r0[0].second = 1.6;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 6;
    		r1[0].second = 9;
    		fh.setData(v0.begin(), m0.begin(), v0.size(), r0, False);
    		fh.addData(v1.begin(), m1.begin(), v1.size(), r1, True);
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(near(mymax, 43.0/6.0), AipsError);
    	}
    	{
    		// getMinMax, weights
    		// 2, 1.5, 4, 2.5
    		// 5, 8
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Double> w0(v0.size());
    		w0[0] = 1;
    		w0[1] = 0;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		vector<Double> w1(v1.size());
    		w1[0] = 1;
    		w1[1] = 2;
    		w1[2] = 0;
    		fh.setData(v0.begin(), w0.begin(), w0.size());
    		fh.addData(v1.begin(), w1.begin(), w1.size());
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1.5, AipsError);
    		AlwaysAssert(near(mymax, 5.5), AipsError);
    	}
    	{
    		// 4, 2.5
    		// 8
    		// getMinMax(), weights and ranges
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		vector<Double> w1(v1.size());
    		w1[0] = 1;
    		w1[1] = 2;
    		w1[2] = 3;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 0.9;
    		r0[0].second = 1.6;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 6;
    		r1[0].second = 9;
    		fh.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
    		fh.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(near(mymax, 123.0/22.0), AipsError);
    	}

    	{
    		// getMinMax(), weights, ranges, and masks
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);

    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		vector<Double> w1(v1.size());
    		w1[0] = 1;
    		w1[1] = 2;
    		w1[2] = 3;
    		vector<Bool> m0(v0.size());
    		m0[0] = True;
    		m0[1] = True;
    		m0[2] = True;
    		m0[3] = True;
    		m0[4] = True;
    		vector<Bool> m1(v1.size());
    		m1[0] = True;
    		m1[1] = True;
    		m1[2] = False;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 0.9;
    		r0[0].second = 1.6;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 6;
    		r1[0].second = 12;
    		fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
    		fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
    		Double mymin, mymax;
    		fh.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(near(mymax, 123.0/22.0), AipsError);
    	}

    	{
    		// getNPts(), two datasets
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 1.5
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		uInt64 npts = fh.getNPts();
    		AlwaysAssert(npts == 4, AipsError);
    		// check calling it again works
    		npts = fh.getNPts();
    		AlwaysAssert(npts == 4, AipsError);
    		// check clearing data and doing it again works
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		npts = fh.getNPts();
    		AlwaysAssert(npts == 4, AipsError);

    		fh = FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator>(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 1.5
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		npts = fh.getNPts();
    		AlwaysAssert(npts == 14, AipsError);
    		// check calling it again works
    		npts = fh.getNPts();
    		AlwaysAssert(npts == 14, AipsError);
    		// check clearing data and doing it again works
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		npts = fh.getNPts();
    		AlwaysAssert(npts == 14, AipsError);
    	}
    	{
    		// general quantile exceptions
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		Bool thrown = False;
    		try {
    			fh.getQuantile(0);
    		}
    		catch (const AipsError& x) {
    			thrown = True;
    		}
    		AlwaysAssert(thrown, AipsError);
    		thrown = False;
    		try {
    			fh.getQuantile(1);
    		}
    		catch (const AipsError& x) {
    			thrown = True;
    		}
    		AlwaysAssert(thrown, AipsError);
    	}
    	{
    		// getQuantile(), no weights, no mask, no ranges
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		// mean is 4.25
    		// real + virtual dataset
    		// 1, 1.5, 2, 2.5, 4, 4.5, 6, 6.5, 7, 7.5
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		Double q = fh.getQuantile(0.1);
    		AlwaysAssert(q == 1, AipsError);
    		q = fh.getQuantile(0.2);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = fh.getQuantile(0.3);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.4);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = fh.getQuantile(0.5);
    		AlwaysAssert(q == 4, AipsError);
    		q = fh.getQuantile(0.6);
    		AlwaysAssert(q == 4.5, AipsError);
    		q = fh.getQuantile(0.7);
    		AlwaysAssert(q == 6, AipsError);
    		q = fh.getQuantile(0.8);
    		AlwaysAssert(q == 6.5, AipsError);
    		q = fh.getQuantile(0.9);
    		AlwaysAssert(q == 7, AipsError);


    		fh = FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator>(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::GE_CENTER
    		);
    		// mean is 4.25
    		// real + virtual dataset
    		// mean is 4.25
    		// real + virtual dataset
    		// -1.5, 0.5, 3.5, 5, 8, 10
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		q = fh.getQuantile(0.1);
    		AlwaysAssert(q == -1.5, AipsError);
    		q = fh.getQuantile(0.2);
    		AlwaysAssert(q == 0.5, AipsError);
    		q = fh.getQuantile(0.3);
    		AlwaysAssert(q == 0.5, AipsError);
    		q = fh.getQuantile(0.4);
    		AlwaysAssert(q == 3.5, AipsError);
    		q = fh.getQuantile(0.5);
    		AlwaysAssert(q == 3.5, AipsError);
    		q = fh.getQuantile(0.6);
    		AlwaysAssert(q == 5, AipsError);
    		q = fh.getQuantile(0.7);
    		AlwaysAssert(q == 8, AipsError);
    		q = fh.getQuantile(0.8);
    		AlwaysAssert(q == 8, AipsError);
    		q = fh.getQuantile(0.9);
    		AlwaysAssert(q == 10, AipsError);
    	}

    	{
    		// getQuantile(): two datasets, stride = 2,1
    		// 1.5, 2, 2.5 5, 8, 10
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 2
    		);
    		// real + virtual -6, -4, -1, 1.5, 2, 2, 2.5, 5, 8, 10
    		fh.setData(v0.begin(), v0.size(), 2);
    		fh.addData(v1.begin(), v1.size());
    		Double q = fh.getQuantile(0.1);
    		AlwaysAssert(q == -6, AipsError);
    		q = fh.getQuantile(0.2);
    		AlwaysAssert(q == -4, AipsError);
    		q = fh.getQuantile(0.3);
    		AlwaysAssert(q == -1, AipsError);
    		q = fh.getQuantile(0.4);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = fh.getQuantile(0.5);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.6);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.7);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = fh.getQuantile(0.8);
    		AlwaysAssert(q == 5, AipsError);
    		q = fh.getQuantile(0.9);
    		AlwaysAssert(q == 8, AipsError);

    		fh = FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator>(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 2
    		);
    		// real + virtual 1.5, 2, 2, 2.5
    		fh.setData(v0.begin(), v0.size(), 2);
    		fh.addData(v1.begin(), v1.size());
    		q = fh.getQuantile(0.1);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = fh.getQuantile(0.2);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = fh.getQuantile(0.3);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.4);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.5);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.6);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.7);
    		AlwaysAssert(q == 2, AipsError);
    		q = fh.getQuantile(0.8);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = fh.getQuantile(0.9);
    		AlwaysAssert(q == 2.5, AipsError);
    	}
    	{
    		// leave in for compile check
    		FitToHalfStatistics<Complex, vector<Complex>::const_iterator, vector<Bool>::const_iterator> fh;
    	}
    	{
    		// getMedian()
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 4.25
    		);
    		fh.addData(v0.begin(), v0.size());
    		Double median = fh.getMedian();
    		AlwaysAssert(median == 4.25, AipsError);
    		fh.reset();
    		vector<Bool> m0(v0.size(), True);
    		m0[0] = False;
    		fh.addData(v0.begin(), m0.begin(), v0.size());
    		median = fh.getMedian();
    		AlwaysAssert(median == 4.25, AipsError);
    	}
    	{
    		// getMedianAndQuantiles
    		std::set<Double> quantiles;
    		quantiles.insert(0.1);
    		quantiles.insert(0.2);
    		quantiles.insert(0.3);
    		quantiles.insert(0.4);
    		quantiles.insert(0.5);
    		quantiles.insert(0.6);
    		quantiles.insert(0.7);
    		quantiles.insert(0.8);
    		quantiles.insert(0.9);
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		std::map<Double, Double> quantileToValue;
    		Double median = fh.getMedianAndQuantiles(quantileToValue, quantiles);
    		AlwaysAssert(median == 4.25, AipsError);
    		AlwaysAssert(quantileToValue[0.1] == 1, AipsError);
    		AlwaysAssert(quantileToValue[0.2] == 1.5, AipsError);
    		AlwaysAssert(quantileToValue[0.3] == 2, AipsError);
    		AlwaysAssert(quantileToValue[0.4] == 2.5, AipsError);
    		AlwaysAssert(quantileToValue[0.5] == 4, AipsError);
    		AlwaysAssert(quantileToValue[0.6] == 4.5, AipsError);
    		AlwaysAssert(quantileToValue[0.7] == 6, AipsError);
    		AlwaysAssert(quantileToValue[0.8] == 6.5, AipsError);
    		AlwaysAssert(quantileToValue[0.9] == 7, AipsError);

    		fh = FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator>(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::GE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		median = fh.getMedianAndQuantiles(quantileToValue, quantiles);
    		AlwaysAssert(median == 4.25, AipsError);
    		//-1.5, 0.5, 3.5, 5, 8, 10
    		AlwaysAssert(quantileToValue[0.1] == -1.5, AipsError);
    		AlwaysAssert(quantileToValue[0.2] == 0.5, AipsError);
    		AlwaysAssert(quantileToValue[0.3] == 0.5, AipsError);
    		AlwaysAssert(quantileToValue[0.4] == 3.5, AipsError);
    		AlwaysAssert(quantileToValue[0.5] == 3.5, AipsError);
    		AlwaysAssert(quantileToValue[0.6] == 5, AipsError);
    		AlwaysAssert(quantileToValue[0.7] == 8, AipsError);
    		AlwaysAssert(quantileToValue[0.8] == 8, AipsError);
    		AlwaysAssert(quantileToValue[0.9] == 10, AipsError);
    	}
    	{
    		// getMedianAbsDevMed()
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.setData(v0.begin(), v0.size());
    		fh.addData(v1.begin(), v1.size());
    		// 1, 1.5, 2, 2.5, 4, 4.5, 6, 6.5, 7, 7.5

    		Double medabsdevmed = fh.getMedianAbsDevMed();
    	    AlwaysAssert(medabsdevmed == 2.25, AipsError);
    	}
    	uInt npts = (uInt)1e6;
    	vector<Double> bigData(npts);
    	vector<Double>::iterator iter = bigData.begin();
    	vector<Double>::iterator end = bigData.end();
    	{
    		uInt64 count = 0;
    		while(iter != end) {
    			*iter = count % 2 == 0 ? count : (-1)*(Double)(count*count);
    			++iter;
    			++count;
    		}
    	}
    	vector<Bool> bigMask(npts, True);
    	bigMask[0] = False;
    	{
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 400
    		);
    		fh.setData(bigData.begin(), bigData.size());
    		// getMedian() with binning, no ranges, weights, or mask
    		// The array size should be ignored, because the median is trivial.
    		Double median = fh.getMedian(NULL, NULL, NULL, 100);
    		AlwaysAssert(median == 400, AipsError);

    		fh = FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator>(
    		    FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 250
    		);
    		fh.setData(bigData.begin(), bigData.size());
    		median = fh.getMedian(NULL, NULL, NULL, 100);
    		AlwaysAssert(median == 250, AipsError);
    	}
    	{
    		// getMedianAbsDevMed() with binning, no ranges, weights, or mask
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 400
    		);
    		fh.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double medabsdevmed = fh.getMedianAbsDevMed(NULL, NULL, NULL, 100);
    		AlwaysAssert(medabsdevmed == 249799040801ULL, AipsError);

    		fh = FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator>(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 250
    		);
    		fh.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		medabsdevmed = fh.getMedianAbsDevMed(NULL, NULL, NULL, 100);
    		AlwaysAssert(medabsdevmed == 499874, AipsError);

    	}
    	{
    		// large array with all the same values, getMedianAndQuantile()
    		vector<Double> big(100000, 30);
    		FitToHalfStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.addData(big.begin(), big.size());
    		std::set<Double> quantiles;
    		quantiles.insert(0.25);
    		quantiles.insert(0.75);
    		CountedPtr<uInt64> npts;
    		CountedPtr<Double> mymin, mymax;
    		std::map<Double, Double> quantileToValue;
    		Double median = fh.getMedianAndQuantiles(
    			quantileToValue, quantiles, npts, mymin, mymax, 100
    		);
    		AlwaysAssert(median == 30, AipsError);
    		AlwaysAssert(quantileToValue[0.25] == 30, AipsError);
    		AlwaysAssert(quantileToValue[0.75] == 30, AipsError);
    	}
    	{
    		// a large array so we test binning
    		Array<Double> big(IPosition(1, 100000));
    		Array<Double>::iterator biter = big.begin();
    		Array<Double>::iterator bend = big.end();
    		uInt count = 0;
    		while (biter != bend) {
    			*biter = count % 2 == 0 ? (Float)count : -(Float)count - 0.5;
    			++biter;
    			++count;
    		}
    		FitToHalfStatistics<Double, Array<Double>::const_iterator, Array<Bool>::const_iterator> fh(
    			FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
    		);
    		fh.addData(big.begin(), big.size());
    		std::set<Double> quantiles;
    		quantiles.insert(0.25);
    		quantiles.insert(0.75);
    		std::map<Double, Double> quantileToValue;
    		CountedPtr<uInt64> npts;
    		CountedPtr<Double> mymin, mymax;
    		Double median = fh.getMedianAndQuantiles(
    			quantileToValue, quantiles, npts, mymin, mymax, 100
    		);
    		AlwaysAssert(near(median, -0.75), AipsError);
    		AlwaysAssert(near(quantileToValue[0.25],-50001.5), AipsError);
    		AlwaysAssert(near(quantileToValue[0.75], 49998.0), AipsError);

    		fh = FitToHalfStatistics<Double, Array<Double>::const_iterator, Array<Bool>::const_iterator>(
    			FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 4
    		);
    		fh.addData(big.begin(), big.size());
    		quantileToValue.clear();
    		npts = NULL;
    		mymin = NULL;
    		mymax = NULL;
    		median = fh.getMedianAndQuantiles(
    			quantileToValue, quantiles, npts, mymin, mymax, 100
    		);
    		AlwaysAssert(near(median, 4.0), AipsError);
    		AlwaysAssert(near(quantileToValue[0.25], -49994.0), AipsError);
    		AlwaysAssert(near(quantileToValue[0.75], 50000.0), AipsError);
    	}
    }
    catch (const AipsError& x) {
        cout << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}






