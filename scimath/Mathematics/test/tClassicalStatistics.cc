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
#include <casacore/scimath/Mathematics/ClassicalStatistics.h>
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
    	v0[3] = 3;
    	v0[4] = 2.5;
    	vector<Double> v1(3);
    	v1[0] = 5;
    	v1[1] = 8;
    	v1[2] = 10;
    	Double k[] = {1.5, 1, 2, 3, 2.5};
    	{
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 3, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second, AipsError);
    		AlwaysAssert(sd.mean == 2, AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == 5, AipsError);
    		cout << "rms " << sd.rms << endl;
    		AlwaysAssert(sd.rms == sqrt(22.5/5.0), AipsError);
    		AlwaysAssert(sd.stddev == sqrt(0.625), AipsError);
    		AlwaysAssert(sd.sum == 10, AipsError);
    		AlwaysAssert(sd.sumsq == 22.5, AipsError);
    		AlwaysAssert(sd.variance == 0.625, AipsError);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 3),
    			AipsError
    		);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::NPTS) == 5, AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::RMS) == sqrt(22.5/5.0), AipsError
    		);
    	}
    	{
    		// just another way of specifying the data
    		ClassicalStatistics<Double, Double*, Bool*> cs1;
    		cs1.setData(k, 5);
    		StatsData<Double> sd = cs1.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 3, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second, AipsError);
    		AlwaysAssert(sd.mean == 2, AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == 5, AipsError);
    		AlwaysAssert(sd.rms == sqrt(22.5/5.0), AipsError);
    		AlwaysAssert(sd.stddev == sqrt(0.625), AipsError);
    		AlwaysAssert(sd.sum == 10, AipsError);
    		AlwaysAssert(sd.sumsq == 22.5, AipsError);
    		AlwaysAssert(sd.variance == 0.625, AipsError);
    		AlwaysAssert(
    			cs1.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 3),
    			AipsError
    		);
    		AlwaysAssert(
    			cs1.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(cs1.getStatistic(
    			StatisticsData::NPTS) == 5, AipsError
    		);
    		AlwaysAssert(cs1.getStatistic(
    			StatisticsData::RMS) == sqrt(22.5/5.0), AipsError
    		);
    	}
    	{
    		// two datasets
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size());
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (211.5 - 33.0*33.0/8.0)/7.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 10, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 2, AipsError);
    		AlwaysAssert(sd.mean == 33.0/8.0, AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == 8, AipsError);
    		AlwaysAssert(sd.rms == sqrt(211.5/8.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 33, AipsError);
    		AlwaysAssert(sd.sumsq == 211.5, AipsError);
    		AlwaysAssert(sd.variance == variance, AipsError);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 2),
    			AipsError
    		);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::NPTS) == 8, AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::RMS) == sqrt(211.5/8.0), AipsError
    		);
    		// Now reverse the order that the datasets were added. results
    		// should be the same except for min and max dataset locations
    		cs.setData(v1.begin(), v1.size());
    		cs.addData(v0.begin(), v0.size());
            sd = cs.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 10, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 2, AipsError);
    		AlwaysAssert(sd.mean == 33.0/8.0, AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 1, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == 8, AipsError);
    		AlwaysAssert(sd.rms == sqrt(211.5/8.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 33, AipsError);
    		AlwaysAssert(sd.sumsq == 211.5, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 2),
    			AipsError
    		);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::NPTS) == 8, AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::RMS) == sqrt(211.5/8.0), AipsError
    		);
    	}
    	{
    		// Test accumulating as datasets are added.
    		vector<Double> t0(5);
    		t0[0] = 1.5;
    		t0[1] = 1;
    		t0[2] = 2;
    		t0[3] = 3;
    		t0[4] = 2.5;
    		vector<Double> t1(3);
    		t1[0] = 5;
    		t1[1] = 8;
    		t1[2] = 10;
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setCalculateAsAdded(False);
    		cs.setData(t0.begin(), t0.size());
    		std::fill(t0.begin(), t0.begin()+t0.size(), 0);
    		cs.addData(t1.begin(), t1.size());
    		std::fill(t1.begin(), t1.begin()+t1.size(), 0);
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 0, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 0, AipsError);
    		AlwaysAssert(sd.mean == 0, AipsError);
    		AlwaysAssert(*sd.min == 0, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 0, AipsError);
    		AlwaysAssert(sd.npts == 8, AipsError);
    		AlwaysAssert(sd.rms == 0, AipsError);
    		AlwaysAssert(sd.stddev == 0, AipsError);
    		AlwaysAssert(sd.sum == 0, AipsError);
    		AlwaysAssert(sd.sumsq == 0, AipsError);
    		AlwaysAssert(sd.variance == 0, AipsError);

    		t0[0] = 1.5;
    		t0[1] = 1;
    		t0[2] = 2;
    		t0[3] = 3;
    		t0[4] = 2.5;
    		t1[0] = 5;
    		t1[1] = 8;
    		t1[2] = 10;

    		Bool exceptionRaised = False;
    		try {
    			cs.setCalculateAsAdded(True);
    		}
    		catch (AipsError& x) {
    			exceptionRaised = True;
    		}
    		AlwaysAssert(exceptionRaised, AipsError);
    		cs.reset();
    		cs.setCalculateAsAdded(True);
    		cs.setData(t0.begin(), t0.size());
    		std::fill(t0.begin(), t0.begin()+t0.size(), 0);
    		cs.addData(t1.begin(), t1.size());
    		std::fill(t1.begin(), t1.begin()+t1.size(), 0);
    		sd = cs.getStatistics();
    		Double variance = (211.5 - 33.0*33.0/8.0)/7.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 10, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 2, AipsError);
    		AlwaysAssert(sd.mean == 33.0/8.0, AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == 8, AipsError);
    		AlwaysAssert(sd.rms == sqrt(211.5/8.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 33, AipsError);
    		AlwaysAssert(sd.sumsq == 211.5, AipsError);
    		AlwaysAssert(sd.variance == variance, AipsError);
    	}
    	{
    		// two datasets, stride = 2,1
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size(), 2);
    		cs.addData(v1.begin(), v1.size());
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (201.5 - 29.0*29.0/6.0)/5.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 10, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 2, AipsError);
    		AlwaysAssert(sd.mean == 29.0/6.0, AipsError);
    		AlwaysAssert(*sd.min == 1.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 2, AipsError);
    		AlwaysAssert(sd.npts == 6, AipsError);
    		AlwaysAssert(sd.rms == sqrt(201.5/6.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 29, AipsError);
    		AlwaysAssert(sd.sumsq == 201.5, AipsError);
    		AlwaysAssert(sd.variance == variance, AipsError);
    	}
    	{
    		// data ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 5;
    		r0[0].second = -5;
    		Bool expectedFail = False;
    		try {
    			cs.setData(v0.begin(), 3, r0);
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
    		cs.setData(v0.begin(), v0.size(), r0);
    		cs.addData(v1.begin(), v1.size(), r1, False);
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (79.25 - 13.5*13.5/3.0)/2.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(sd.mean == 13.5/3.0, AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == 3, AipsError);
    		AlwaysAssert(sd.rms == sqrt(79.25/3.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 13.5, AipsError);
    		AlwaysAssert(sd.sumsq == 79.25, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    	}
    	{
    		// mask
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), m0.begin(), v0.size());
    		cs.addData(v1.begin(), m1.begin(), v1.size());
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (79.25 - 13.5*13.5/3.0)/2.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(sd.mean == 13.5/3.0, AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == 3, AipsError);
    		AlwaysAssert(sd.rms == sqrt(79.25/3.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 13.5, AipsError);
    		AlwaysAssert(sd.sumsq == 79.25, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    	}
    	{
    		// mask and ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), m0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), m1.begin(), v1.size(), r1, True);
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (79.25 - 13.5*13.5/3.0)/2.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(sd.mean == 13.5/3.0, AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == 3, AipsError);
    		AlwaysAssert(sd.rms == sqrt(79.25/3.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 13.5, AipsError);
    		AlwaysAssert(sd.sumsq == 79.25, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    	}
    	{
    		// weights
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), w0.size());
    		cs.addData(v1.begin(), w1.begin(), w1.size());
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (529.0 - 82.0*82.0/20.0)/19.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 10, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 2, AipsError);
    		AlwaysAssert(near(sd.mean, 4.1), AipsError);
    		AlwaysAssert(*sd.min == 1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == 7, AipsError);
    		AlwaysAssert(sd.rms == sqrt(529.0/20.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 82.0, AipsError);
    		AlwaysAssert(sd.sumweights == 20.0, AipsError);
    		AlwaysAssert(sd.sumsq == 529.0, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    	}
    	{
    		// weights and ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (195.25 - 40.5*40.5/11.0)/10.0;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(near(sd.mean, 40.5/11.0), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == 3, AipsError);
    		AlwaysAssert(sd.rms == sqrt(195.25/11.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 40.5, AipsError);
    		AlwaysAssert(sd.sumweights == 11.0, AipsError);
    		AlwaysAssert(sd.sumsq == 195.25, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    	}
    	{
    		// weights, ranges, and masks
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (195.25 - 40.5*40.5/11.0)/10.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(near(sd.mean, 40.5/11.0), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == 3, AipsError);
    		AlwaysAssert(sd.rms == sqrt(195.25/11.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 40.5, AipsError);
    		AlwaysAssert(sd.sumweights == 11.0, AipsError);
    		AlwaysAssert(sd.sumsq == 195.25, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(
    			cs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 4),
    			AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::NPTS) == 3, AipsError
    		);
    		AlwaysAssert(cs.getStatistic(
    			StatisticsData::RMS) == sqrt(195.25/11.0), AipsError
    		);
    	}
    	{
    		// weights, masks
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
    		cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
    		StatsData<Double> sd = cs.getStatistics();
    		Double variance = (195.25 - 40.5*40.5/11.0)/10.0;
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(near(sd.mean, 40.5/11.0), AipsError);
    		AlwaysAssert(*sd.min == 2.5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 4, AipsError);
    		AlwaysAssert(sd.npts == 3, AipsError);
    		AlwaysAssert(sd.rms == sqrt(195.25/11.0), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
    		AlwaysAssert(sd.sum == 40.5, AipsError);
    		AlwaysAssert(sd.sumweights == 11.0, AipsError);
    		AlwaysAssert(sd.sumsq == 195.25, AipsError);
    		AlwaysAssert(near(sd.variance, variance), AipsError);
    	}

    	{
    		// getMinMax(), two datasets
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size());
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1, AipsError);
    		AlwaysAssert(mymax == 10, AipsError);
    	}
    	{
    		// getMinMax(), two datasets, stride = 2,1
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), 3, 2);
    		cs.addData(v1.begin(), v1.size());
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1.5, AipsError);
    		AlwaysAssert(mymax == 10, AipsError);
    	}
    	{
    		// getMaxMin(), data ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 2.4;
    		r0[0].second = 6;
    		vector<std::pair<Double, Double> > r1(2);
    		r1[0].first = 9;
    		r1[0].second = 11;
    		r1[1].first = 2;
    		r1[1].second = 7;
    		cs.setData(v0.begin(), v0.size(), r0);
    		cs.addData(v1.begin(), v1.size(), r1, False);
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(mymax == 8, AipsError);
    	}
    	{
    		// getMinMax(), mask
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), m0.begin(), v0.size());
    		cs.addData(v1.begin(), m1.begin(), v1.size());
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(mymax == 8, AipsError);
    	}
    	{
    		// getMinMax(), mask and ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), m0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), m1.begin(), v1.size(), r1, True);
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(mymax == 8, AipsError);
    	}
    	{
    		// getMinMax, weights
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), w0.size());
    		cs.addData(v1.begin(), w1.begin(), w1.size());
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1.5, AipsError);
    		AlwaysAssert(mymax == 8, AipsError);
    	}
    	{
    		// getMinMax(), weights and ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(mymax == 8, AipsError);
    	}
    	{
    		// getMinMax(), weights, ranges, and masks
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2.5, AipsError);
    		AlwaysAssert(mymax == 8, AipsError);
    	}




    	{
    		// general quantile exceptions
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size());
    		Bool thrown = False;
    		try {
    			cs.getQuantile(0);
    		}
    		catch (const AipsError& x) {
    			thrown = True;
    		}
    		AlwaysAssert(thrown, AipsError);
    		thrown = False;
    		try {
    			cs.getQuantile(1);
    		}
    		catch (const AipsError& x) {
    			thrown = True;
    		}
    		AlwaysAssert(thrown, AipsError);
    	}
    	{
    		// getQuantile(), no weights, no mask, no ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size());
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 1.0, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.0, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 5.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 10.0, AipsError);
    	}
    	{
    		// getQuantile(): two datasets, stride = 2,1
    		// 1.5, 2, 2.5 5, 8, 10
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size(), 2);
    		cs.addData(v1.begin(), v1.size());
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 2.0, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.0, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 5.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 10.0, AipsError);
    	}
    	{
    		// getQuantile(), ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		vector<std::pair<Double, Double> > r0(1), r1(2);
    		r0[0].first = 2.4;
    		r0[0].second = 6;
    		r1[0].first = 9;
    		r1[0].second = 11;
    		r1[1].first = 2;
    		r1[1].second = 7;
    		cs.setData(v0.begin(), v0.size(), r0);
    		cs.addData(v1.begin(), v1.size(), r1, False);
    		// 2.5, 3, 8
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 8.0, AipsError);
    	}
    	{
    		// getQuantile(): mask
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), m0.begin(), v0.size());
    		cs.addData(v1.begin(), m1.begin(), v1.size());
    		// 2.5, 3, 8
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 8.0, AipsError);
    	}
    	{
    		//getQuantile(): mask and ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), m0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), m1.begin(), v1.size(), r1, True);
    		// 2.5, 3, 8
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 8.0, AipsError);
    	}
    	{
    		// getQuantile(): weights
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), w0.size());
    		cs.addData(v1.begin(), w1.begin(), w1.size());
    		// 1, 1.5, 2.5, 3, 5, 8, 10
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 1.0, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 5.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 5.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 10.0, AipsError);
    	}
    	{
    		// getQuantile(): ranges and weights
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
    		// 2.5, 3, 8
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 8.0, AipsError);
    	}
    	{
    		// getQuantile(): weights and mask
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
    		cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
    		// 2.5, 3, 8
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 8.0, AipsError);
    	}
    	{
    		// getQuantile(): weights, mask, ranges
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
    		cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
    		// 2.5, 3, 8
    		Double q = cs.getQuantile(0.1);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.2);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.3);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = cs.getQuantile(0.4);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.5);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.6);
    		AlwaysAssert(q == 3.0, AipsError);
    		q = cs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = cs.getQuantile(0.9);
    		AlwaysAssert(q == 8.0, AipsError);
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
    		std::map<Double, Double> qs = cs.getQuantiles(quantiles);
    		AlwaysAssert(qs[0.1] == 2.5, AipsError);
    		AlwaysAssert(qs[0.2] == 2.5, AipsError);
    		AlwaysAssert(qs[0.3] == 2.5, AipsError);
    		AlwaysAssert(qs[0.4] == 3.0, AipsError);
    		AlwaysAssert(qs[0.5] == 3.0, AipsError);
    		AlwaysAssert(qs[0.6] == 3.0, AipsError);
    		AlwaysAssert(qs[0.7] == 8.0, AipsError);
    		AlwaysAssert(qs[0.8] == 8.0, AipsError);
    		AlwaysAssert(qs[0.9] == 8.0, AipsError);
    	}
    	{
    		// leave in for compile check
    		ClassicalStatistics<Complex, vector<Complex>::const_iterator, vector<Bool>::const_iterator> cs;
    	}
    	{
    		// getMedian()
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.addData(v0.begin(), v0.size());
    		Double median = cs.getMedian();
    		AlwaysAssert(median == 2, AipsError);
    		cs.reset();
    		vector<Bool> m0(v0.size(), True);
    		m0[0] = False;
    		cs.addData(v0.begin(), m0.begin(), v0.size());
    		median = cs.getMedian();
    		AlwaysAssert(median == 2, AipsError);
    	}
    	{
    		// getMedianAndQuantiles (even sized data set)
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
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size());
    		std::map<Double, Double> quantileToValue;
    		Double median = cs.getMedianAndQuantiles(quantileToValue, quantiles);
    		AlwaysAssert(median == 2.75, AipsError);
    		AlwaysAssert(quantileToValue[0.1] == 1.0, AipsError);
    		AlwaysAssert(quantileToValue[0.2] == 1.5, AipsError);
    		AlwaysAssert(quantileToValue[0.3] == 2.0, AipsError);
    		AlwaysAssert(quantileToValue[0.4] == 2.5, AipsError);
    		AlwaysAssert(quantileToValue[0.5] == 2.5, AipsError);
    		AlwaysAssert(quantileToValue[0.6] == 3.0, AipsError);
    		AlwaysAssert(quantileToValue[0.7] == 5.0, AipsError);
    		AlwaysAssert(quantileToValue[0.8] == 8.0, AipsError);
    		AlwaysAssert(quantileToValue[0.9] == 10.0, AipsError);
    	}
    	{
    		// getMedianAndQuantiles (odd sized data set)
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
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 9;
    		r0[0].second = 11;
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size(), r0, False);
    		std::map<Double, Double> quantileToValue;
    		Double median = cs.getMedianAndQuantiles(quantileToValue, quantiles);
    		AlwaysAssert(median == 2.5, AipsError);
    		AlwaysAssert(quantileToValue[0.1] == 1.0, AipsError);
    		AlwaysAssert(quantileToValue[0.2] == 1.5, AipsError);
    		AlwaysAssert(quantileToValue[0.3] == 2.0, AipsError);
    		AlwaysAssert(quantileToValue[0.4] == 2.0, AipsError);
    		AlwaysAssert(quantileToValue[0.5] == 2.5, AipsError);
    		AlwaysAssert(quantileToValue[0.6] == 3.0, AipsError);
    		AlwaysAssert(quantileToValue[0.7] == 3.0, AipsError);
    		AlwaysAssert(quantileToValue[0.8] == 5.0, AipsError);
    		AlwaysAssert(quantileToValue[0.9] == 8.0, AipsError);
    	}
    	{
    		// getMedianAndQuantiles (even sized data set)
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size());
    		Double medabsdevmed = cs.getMedianAbsDevMed();
    	    AlwaysAssert(medabsdevmed == 1.5, AipsError);
    	}
    	{
    		// getMedianAndQuantiles (odd sized data set)
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 9;
    		r0[0].second = 11;
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size(), r0, False);
    		Double medabsdevmed = cs.getMedianAbsDevMed();
    		AlwaysAssert(medabsdevmed == 1.0, AipsError);
    	}
    	uInt npts = (uInt)1e6;
    	vector<Double> bigData(npts);
    	vector<Double>::iterator iter = bigData.begin();
    	vector<Double>::iterator end = bigData.end();
    	uInt64 count = 0;
    	while(iter != end) {
    		*iter = count % 2 == 0 ? (Float)count : -Float(count*count);
    		++iter;
    		++count;
    	}
    	vector<Bool> bigMask(npts, True);
    	bigMask[0] = False;
    	{
    		// getMedian() with binning, no ranges, weights, or mask
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double median = cs.getMedian(NULL, NULL, NULL, 100);
    		AlwaysAssert(median == -0.5, AipsError);

    	}
    	{
    		// getMedian() with mask, but no weights or ranges, using binning
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigMask.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double median = cs.getMedian(NULL, NULL, NULL, 100);
    		AlwaysAssert(median == -1, AipsError);
    	}
    	{
    		// getMedianAbsDevMed() with binning, no ranges, weights, or mask
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double medabsdevmed = cs.getMedianAbsDevMed(NULL, NULL, NULL, 100);
    		AlwaysAssert(medabsdevmed == 998999.5, AipsError);
    	}
    	{
    		// getMedianAbsDevMed() with mask, but no weights or ranges, using binning
    		ClassicalStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigMask.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double medabsdevmed = cs.getMedianAbsDevMed(NULL, NULL, NULL, 100);
    		AlwaysAssert(medabsdevmed == 999001, AipsError);
    	}
    	{ // large array with all the same values, getMedianAndQuartile()
    		vector<Float> big(100000, 30);
    		ClassicalStatistics<Double, vector<Float>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.addData(big.begin(), big.size());
    		std::set<Double> quantiles;
    		quantiles.insert(0.25);
    		quantiles.insert(0.75);
    		CountedPtr<uInt64> npts;
    		CountedPtr<Double> mymin, mymax;
    		std::map<Double, Double> quantileToValue;
    		Double median = cs.getMedianAndQuantiles(
    			quantileToValue, quantiles, npts, mymin, mymax, 100
    		);
    		AlwaysAssert(median == 30, AipsError);
    		AlwaysAssert(quantileToValue[0.25] == 30, AipsError);
    		AlwaysAssert(quantileToValue[0.75] == 30, AipsError);
    	}
    	{ // two large array with two unique values, getMedianAndQuartile()
    		ClassicalStatistics<Double, vector<Float>::const_iterator, vector<Bool>::const_iterator> cs;
    		vector<Float> big(100000, 30);
    		cs.addData(big.begin(), big.size());
    		vector<Float> big2(50000, -10);
    		cs.addData(big2.begin(), big2.size());
    		std::set<Double> quantiles;
    		quantiles.insert(0.25);
    		quantiles.insert(0.75);
    		CountedPtr<uInt64> npts;
    		CountedPtr<Double> mymin, mymax;
    		std::map<Double, Double> quantileToValue;
    		Double median = cs.getMedianAndQuantiles(
    			quantileToValue, quantiles, npts, mymin, mymax, 100
    		);
    		AlwaysAssert(median == 30, AipsError);
    		AlwaysAssert(quantileToValue[0.25] == -10, AipsError);
    		AlwaysAssert(quantileToValue[0.75] == 30, AipsError);
    	}
    }

    catch (const AipsError& x) {
        cout << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}






