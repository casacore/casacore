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
#include <casacore/scimath/Mathematics/HingesFencesStatistics.h>
#include <casacore/casa/Exceptions/Error.h>

#include <vector>

#include <casacore/casa/namespace.h>

#define COMMA ,

int main() {
    try {
    	// The first group of tests test the default behavior, which is identical to
    	// that of ClassicalStatistics

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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 3, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 3, AipsError);
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
    		HingesFencesStatistics<Double, Double*, Bool*> cs1;
    		cs1.setData(k, 5);
    		StatsData<Double> sd = cs1.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 3, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 3, AipsError);
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    	}
    	{
    		// two datasets, stride = 2,1
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs;
    		hfs.setData(v0.begin(), v0.size(), 2);
    		hfs.addData(v1.begin(), v1.size());
    		StatsData<Double> sd = hfs.getStatistics();
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), v0.size());
    		cs.addData(v1.begin(), v1.size());
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1, AipsError);
    		AlwaysAssert(mymax == 10, AipsError);
    	}
    	{
    		// getMinMax(), two datasets, stride = 2,1
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(v0.begin(), 3, 2);
    		cs.addData(v1.begin(), v1.size());
    		Double mymin, mymax;
    		cs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 1.5, AipsError);
    		AlwaysAssert(mymax == 10, AipsError);
    	}
    	{
    		// getMaxMin(), data ranges
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs;
    		hfs.setData(v0.begin(), v0.size(), 2);
    		hfs.addData(v1.begin(), v1.size());
    		Double q = hfs.getQuantile(0.1);
    		AlwaysAssert(q == 1.5, AipsError);
    		q = hfs.getQuantile(0.2);
    		AlwaysAssert(q == 2.0, AipsError);
    		q = hfs.getQuantile(0.3);
    		AlwaysAssert(q == 2.0, AipsError);
    		q = hfs.getQuantile(0.4);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = hfs.getQuantile(0.5);
    		AlwaysAssert(q == 2.5, AipsError);
    		q = hfs.getQuantile(0.6);
    		AlwaysAssert(q == 5.0, AipsError);
    		q = hfs.getQuantile(0.7);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = hfs.getQuantile(0.8);
    		AlwaysAssert(q == 8.0, AipsError);
    		q = hfs.getQuantile(0.9);
    		AlwaysAssert(q == 10.0, AipsError);
    	}
    	{
    		// getQuantile(), ranges
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Complex, vector<Complex>::const_iterator, vector<Bool>::const_iterator> cs;
    	}
    	{
    		// getMedian()
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
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
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double median = cs.getMedian(NULL, NULL, NULL, 100);
    		AlwaysAssert(median == -0.5, AipsError);

    	}
    	{
    		// getMedian() with mask, but no weights or ranges, using binning
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigMask.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double median = cs.getMedian(NULL, NULL, NULL, 100);
    		AlwaysAssert(median == -1, AipsError);
    	}
    	{
    		// getMedianAbsDevMed() with binning, no ranges, weights, or mask
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double medabsdevmed = cs.getMedianAbsDevMed(NULL, NULL, NULL, 100);
    		AlwaysAssert(medabsdevmed == 998999.5, AipsError);
    	}
    	{
    		// getMedianAbsDevMed() with mask, but no weights or ranges, using binning
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.setData(bigData.begin(), bigMask.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double medabsdevmed = cs.getMedianAbsDevMed(NULL, NULL, NULL, 100);
    		AlwaysAssert(medabsdevmed == 999001, AipsError);
    	}
    	{ // large array with all the same values, getMedianAndQuartile()
    		vector<Float> big(100000, 0);
    		HingesFencesStatistics<Double, vector<Float>::const_iterator, vector<Bool>::const_iterator> cs;
    		cs.addData(big.begin(), big.size());
    		std::set<Double> quantiles;
    		quantiles.insert(0.25);
    		quantiles.insert(0.75);
    		CountedPtr<uInt64> npts;
    		CountedPtr<Double> mymin, mymax;
    		std::map<Double, Double> quantileToValue;
    		Double median = cs.getMedianAndQuantiles(
    			quantileToValue, quantiles, npts, mymin, mymax, 99999
    		);
    		AlwaysAssert(median == 0, AipsError);
    		AlwaysAssert(quantileToValue[0.25] == 0, AipsError);
    		AlwaysAssert(quantileToValue[0.75] == 0, AipsError);

    	}
    	// now begin testing the specialized behavior of HingesFencesStatistics
    	v0.resize(12);
    	v0[0] = 5;
    	v0[1] = 2;
    	v0[2] = 6;
    	v0[3] = 10;
    	v0[4] = 7;
    	v0[5] = -1;
    	v0[6] = 15;
    	v0[7] = 11;
    	v0[8] = 6;
    	v0[9] = 20;
    	v0[10] = -3;
    	v0[11] = 14;
    	// for v, the members between Q1 and Q3 inclusive are
    	// 2, 5, 6, 6, 7, 10, 11
    	{
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(v0.begin(), v0.size());
    		StatsData<Double> sd = hfs.getStatistics();
    		Double eSum = 47;
    		Double eNpts = 7;
    		Double eSumSq = 371;
    		Double eVar = 9.238095238095239;
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 7, AipsError);
    		AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    		AlwaysAssert(*sd.min == 2, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    		AlwaysAssert(sd.stddev == sqrt(eVar), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(sd.variance == eVar, AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    				== std::pair<Int64 COMMA Int64>(0, 7),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    				== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    		);
    	}
    	{
    		// just another way of specifying the data
    		Double kk[] = {5, 2, 6, 10, 7, -1, 15, 11, 6, 20, -3, 14};
    		HingesFencesStatistics<Double, Double*, Bool*> hfs(0);
    		hfs.setData(kk, 12);
    		Double eSum = 47;
    		Double eNpts = 7;
    		Double eSumSq = 371;
    		Double eVar = 9.238095238095239;
    		StatsData<Double> sd = hfs.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 7, AipsError);
    		AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    		AlwaysAssert(*sd.min == 2, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    		AlwaysAssert(sd.stddev == sqrt(eVar), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(sd.variance == eVar, AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 7),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    		);
    	}
    	{
    		// two datasets
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(v0.begin(), v0.size()/2);
    		hfs.addData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2);
    		Double eSum = 47;
    		Double eNpts = 7;
    		Double eSumSq = 371;
    		Double eVar = 9.238095238095239;
    		StatsData<Double> sd = hfs.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    		AlwaysAssert(*sd.min == 2, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    		AlwaysAssert(sd.stddev == sqrt(eVar), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(sd.variance == eVar, AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    		);
    		// Now reverse the order that the datasets were added. results
    		// should be the same except for min and max dataset locations
    		hfs.setData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2);
    		hfs.addData(v0.begin(), v0.size()/2);
    		sd = hfs.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 0, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    		AlwaysAssert(*sd.min == 2, AipsError);
    		AlwaysAssert(sd.minpos.first == 1, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(eVar)), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(near(sd.variance, eVar), AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    		);
    	}
    	{
    		// verify that datasets cannot be accumulated as added
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		Bool except = False;
    		try {
    			hfs.setCalculateAsAdded(True);
    		}
    		catch (const AipsError&) {
    			except = True;
    		}
    		AlwaysAssert(except, AipsError);
    	}
    	{
    		// two datasets, stride = 2,1
    		// values of the inner quartile in this case are 6, 7, 11, 6, 14
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(v0.begin(), v0.size()/2, 2);
    		hfs.addData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2);
    		Double eSum = 44;
    		Double eNpts = 5;
    		Double eSumSq = 438;
    		Double eVar = 12.7;
    		StatsData<Double> sd = hfs.getStatistics();
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 14, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 5, AipsError);
    		AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    		AlwaysAssert(*sd.min == 6, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 2, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    		AlwaysAssert(sd.stddev == sqrt(eVar), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(sd.variance == eVar, AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 5),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 2),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    		);
    	}
    	{
    		// data ranges
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14

    		// 5, 6, 15, 20, -3, 14
    		// 5, 6, 15, 14
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 5;
    		r0[0].second = -5;
    		Bool expectedFail = False;
    		try {
    			hfs.setData(v0.begin(), v0.size(), r0);
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
    		hfs.setData(v0.begin(), v0.size()/2, r0);
    		hfs.addData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2, r1, False);
    		StatsData<Double> sd = hfs.getStatistics();
    		Double eSum = 40;
    		Double eNpts = 4;
    		Double eSumSq = 482;
    		Double eVar = (eSumSq - eSum*eSum/eNpts)/(eNpts - 1);
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 15, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 0, AipsError);
    		AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    		AlwaysAssert(*sd.min == 5, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 0, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    		AlwaysAssert(sd.stddev == sqrt(eVar), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(sd.variance == eVar, AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 0),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 0),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    		);
    	}
    	{
    		// mask
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14
    		// 10, 7, 15, 11, 14
    		// 10, 11, 14
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		vector<Bool> m0(v0.size());
    		m0[0] = False;
    		m0[1] = False;
    		m0[2] = False;
    		m0[3] = True;
    		m0[4] = True;
    		m0[5] = False;
    		m0[6] = True;
    		m0[7] = True;
    		m0[8] = False;
    		m0[9] = False;
    		m0[10] = False;
    		m0[11] = True;
    		hfs.setData(v0.begin(), m0.begin(), v0.size()/2);
    		hfs.addData(v0.begin() + v0.size()/2, m0.begin() + m0.size()/2, v0.size() - v0.size()/2);
    		StatsData<Double> sd = hfs.getStatistics();
    		Double eSum = 35;
    		Double eNpts = 3;
    		Double eSumSq = 417;
    		Double eVar = (eSumSq - eSum*eSum/eNpts)/(eNpts - 1);
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(! sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 14, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 5, AipsError);
    		AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    		AlwaysAssert(*sd.min == 10, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 3, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(eVar)), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(near(sd.variance, eVar), AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 5),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 3),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    		);
    	}
    	{
    		// mask and ranges
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14
    		// 2, 6, 10, 7, -1
    		// 11, 6, 20, -3, 14
    		// 2, 6, 10, -1
    		// 11, 6, 20, 14
    		// 2, 6, 10, 11, 6
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		vector<Bool> m0(v0.size());
    		m0[0] = False;
    		m0[1] = True;
    		m0[2] = True;
    		m0[3] = True;
    		m0[4] = True;
    	    m0[5] = True;
    	    m0[6] = False;
    	    m0[7] = True;
    	    m0[8] = True;
    	    m0[9] = True;
    	    m0[10] = True;
    	    m0[11] = True;
    	    vector<std::pair<Double, Double> > r0(1);
    	    r0[0].first = 7;
    	    r0[0].second = 8;
    	    vector<std::pair<Double, Double> > r1(1);
    	    r1[0].first = 6;
    	    r1[0].second = 21;
    	    hfs.setData(v0.begin(), m0.begin(), v0.size()/2, r0, False);
    	    hfs.addData(v0.begin() + v0.size()/2, m0.begin() + m0.size()/2, v0.size() - v0.size()/2, r1, True);
    	    StatsData<Double> sd = hfs.getStatistics();
    	    Double eSum = 35;
    	    Double eNpts = 5;
    	    Double eSumSq = 297;
    	    Double eVar = (eSumSq - eSum*eSum/eNpts)/(eNpts - 1);
    	    AlwaysAssert(sd.masked, AipsError);
    	    AlwaysAssert(! sd.weighted, AipsError);
    	    AlwaysAssert(*sd.max == 11, AipsError);
    	    AlwaysAssert(sd.maxpos.first == 1, AipsError);
    	    AlwaysAssert(sd.maxpos.second == 1, AipsError);
    	    AlwaysAssert(sd.mean == eSum/eNpts, AipsError);
    	    AlwaysAssert(*sd.min == 2, AipsError);
    	    AlwaysAssert(sd.minpos.first == 0, AipsError);
    	    AlwaysAssert(sd.minpos.second == 1, AipsError);
    	    AlwaysAssert(sd.npts == eNpts, AipsError);
    	    AlwaysAssert(sd.rms == sqrt(eSumSq/eNpts), AipsError);
    	    AlwaysAssert(near(sd.stddev, sqrt(eVar)), AipsError);
    	    AlwaysAssert(sd.sum == eSum, AipsError);
    	    AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    	    AlwaysAssert(near(sd.variance, eVar), AipsError);
    	    AlwaysAssert(
    	    	hfs.getStatisticIndex(StatisticsData::MAX)
    	    	== std::pair<Int64 COMMA Int64>(1, 1),
    	    	AipsError
    	    );
    	    AlwaysAssert(
    	    	hfs.getStatisticIndex(StatisticsData::MIN)
    	    	== std::pair<Int64 COMMA Int64>(0, 1),
    	    	AipsError
    	    );
    	    AlwaysAssert(hfs.getStatistic(
    	    	StatisticsData::NPTS) == eNpts, AipsError
    	    );
    	    AlwaysAssert(hfs.getStatistic(
    	    	StatisticsData::RMS) == sqrt(eSumSq/eNpts), AipsError
    	    );
    	}
    	{
    		// weights
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14

    		// 2, 6, 10, 7, -1
    		// 11, 6, 20, -3, 14

    		// 2, 6, 10, 7
    		// 11, 6

    		// 4 + 18 + 40 + 35 + 22 + 18
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3;
    		w0[3] = 4;
    		w0[4] = 5;
    		w0[5] = 1;
    		w0[6] = 0;
    		w0[7] = 2;
    		w0[8] = 3;
    		w0[9] = 2;
    		w0[10] = 1;
    		w0[11] = 2;
    		hfs.setData(v0.begin(), w0.begin(), v0.size()/2);
    		hfs.addData(v0.begin() + v0.size()/2, w0.begin() + w0.size()/2, v0.size() - v0.size()/2);
    		StatsData<Double> sd = hfs.getStatistics();
    		Double eSum = 137;
    		Double eSumWeights = 19;
    		Double eNpts = 6;
    		Double eSumSq = 1111;
    		Double eVar = (eSumSq - eSum*eSum/eSumWeights)/(eSumWeights - 1);
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(sd.mean == eSum/eSumWeights, AipsError);
    		AlwaysAssert(*sd.min == 2, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 1, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eSumWeights), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(eVar)), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(near(sd.variance, eVar), AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 1),
    			AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eSumWeights), AipsError
    		);
    	}
    	{
    		// weights and ranges
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14

    		// 2, 6, 10, 7, -1
    		// 11, 6, 20, -3, 14

    		// 6, 10, 7, -1
    		// 11, 6, 14

    		// 6, 10, 7
    		// 11, 6

    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3; // *6 = 18
    		w0[3] = 4; // *10 = 40
    		w0[4] = 5; // *7 = 35
    		w0[5] = 1;
    		w0[6] = 0;
    		w0[7] = 2; // *11 = 22
    		w0[8] = 3; // *6 = 18
    		w0[9] = 2;
    		w0[10] = 1;
    		w0[11] = 2;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 1;
    		r0[0].second = 2;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 0;
    		r1[0].second = 15;
    		hfs.setData(v0.begin(), w0.begin(), v0.size()/2, r0, False);
    		hfs.addData(
    			v0.begin() + v0.size()/2, w0.begin() + w0.size()/2,
    			v0.size() - v0.size()/2, r1, True
    		);
    		StatsData<Double> sd = hfs.getStatistics();
    		Double eSum = 133;
    		Double eSumWeights = 17;
    		Double eNpts = 5;
    		Double eSumSq = 1103;
    		Double eVar = (eSumSq - eSum*eSum/eSumWeights)/(eSumWeights - 1);
    		AlwaysAssert(! sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(sd.mean == eSum/eSumWeights, AipsError);
    		AlwaysAssert(*sd.min == 6, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 2, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eSumWeights), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(eVar)), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(near(sd.variance, eVar), AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 2),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatistic(StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eSumWeights), AipsError
    		);
    	}
    	{
    		// weights, ranges, and masks
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14

    		// 2, 6, 10, 7, -1
    		// 11, 6, 20, -3, 14

    		// 6, 10, 7, -1
    		// 11, 6, 14

    		// 6, 10, -1
    		// 11, 14

    		// 6, 10
    		// 11



    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2;
    		w0[2] = 3; // *6 = 18
    		w0[3] = 4; // *10 = 40
    		w0[4] = 5;
    		w0[5] = 1;
    		w0[6] = 0;
    		w0[7] = 2; // *11 = 22
    		w0[8] = 3;
    		w0[9] = 2;
    		w0[10] = 1;
    		w0[11] = 2;
    		vector<std::pair<Double, Double> > r0(1);
    		r0[0].first = 1;
    		r0[0].second = 2;
    		vector<std::pair<Double, Double> > r1(1);
    		r1[0].first = 0;
    		r1[0].second = 15;
    		vector<Bool> m0(v0.size(), True);
    		m0[4] = False;
    		m0[8] = False;
    		hfs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size()/2, r0, False);
    		hfs.addData(
    			v0.begin() + v0.size()/2, w0.begin() + w0.size()/2,
    			m0.begin() + m0.size()/2,
    			v0.size() - v0.size()/2, r1, True
    		);
    		StatsData<Double> sd = hfs.getStatistics();
    		Double eSum = 80;
    		Double eSumWeights = 9;
    		Double eNpts = 3;
    		Double eSumSq = 750;
    		Double eVar = (eSumSq - eSum*eSum/eSumWeights)/(eSumWeights - 1);
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(near(sd.mean, eSum/eSumWeights), AipsError);
    		AlwaysAssert(*sd.min == 6, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 2, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eSumWeights), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(eVar)), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(near(sd.variance, eVar), AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 2),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatistic(StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eSumWeights), AipsError
    		);
    	}
    	{
    		// weights, masks
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14

    		// 2, 6, 10, 7, -1
    		// 11, 6, 20, -3, 14

    		// 2, 6, 10, -1
    		// 11, 20, -3, 14

    		// 2, 6, 10, -1
    		// 11

    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		vector<Double> w0(v0.size());
    		w0[0] = 0;
    		w0[1] = 2; // *2 = 4
    		w0[2] = 3; // *6 = 18
    		w0[3] = 4; // *10 = 40
    		w0[4] = 5;
    		w0[5] = 1; // *-1 = -1
    		w0[6] = 0;
    		w0[7] = 2; // *11 = 22
    		w0[8] = 3;
    		w0[9] = 2;
    		w0[10] = 1;
    		w0[11] = 2;
    		vector<Bool> m0(v0.size(), True);
    		m0[4] = False;
    		m0[8] = False;
    		hfs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size()/2);
    		hfs.addData(
    			v0.begin() + v0.size()/2, w0.begin() + w0.size()/2,
    			m0.begin() + m0.size()/2,
    			v0.size() - v0.size()/2
    		);
    		StatsData<Double> sd = hfs.getStatistics();
    		Double eSum = 83;
    		Double eSumWeights = 12;
    		Double eNpts = 5;
    		Double eSumSq = 759;
    		Double eVar = (eSumSq - eSum*eSum/eSumWeights)/(eSumWeights - 1);
    		AlwaysAssert(sd.masked, AipsError);
    		AlwaysAssert(sd.weighted, AipsError);
    		AlwaysAssert(*sd.max == 11, AipsError);
    		AlwaysAssert(sd.maxpos.first == 1, AipsError);
    		AlwaysAssert(sd.maxpos.second == 1, AipsError);
    		AlwaysAssert(near(sd.mean, eSum/eSumWeights), AipsError);
    		AlwaysAssert(*sd.min == -1, AipsError);
    		AlwaysAssert(sd.minpos.first == 0, AipsError);
    		AlwaysAssert(sd.minpos.second == 5, AipsError);
    		AlwaysAssert(sd.npts == eNpts, AipsError);
    		AlwaysAssert(sd.rms == sqrt(eSumSq/eSumWeights), AipsError);
    		AlwaysAssert(near(sd.stddev, sqrt(eVar)), AipsError);
    		AlwaysAssert(sd.sum == eSum, AipsError);
    		AlwaysAssert(sd.sumsq == eSumSq, AipsError);
    		AlwaysAssert(near(sd.variance, eVar), AipsError);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MAX)
    			== std::pair<Int64 COMMA Int64>(1, 1),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatisticIndex(StatisticsData::MIN)
    			== std::pair<Int64 COMMA Int64>(0, 5),
    			AipsError
    		);
    		AlwaysAssert(
    			hfs.getStatistic(StatisticsData::NPTS) == eNpts, AipsError
    		);
    		AlwaysAssert(hfs.getStatistic(
    			StatisticsData::RMS) == sqrt(eSumSq/eSumWeights), AipsError
    		);
    	}
    	{
    		// getMinMax(), two datasets
    		// 5, 2, 6, 10, 7, -1
    		// 15, 11, 6, 20, -3, 14

    		// 5, 2, 6, 10, 7
    		// 11, 6

    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(v0.begin(), v0.size()/2);
    		hfs.addData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2);
    		Double mymin, mymax;
    		hfs.getMinMax(mymin, mymax);
    		AlwaysAssert(mymin == 2, AipsError);
    		AlwaysAssert(mymax == 11, AipsError);
    	}
    	{
    		// general quantile exceptions
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(v0.begin(), v0.size()/2);
    		hfs.addData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2);
    		Bool thrown = False;
    		try {
    			hfs.getQuantile(0);
    		}
    		catch (const AipsError& x) {
    			thrown = True;
    		}
    		AlwaysAssert(thrown, AipsError);
    		thrown = False;
    		try {
    			hfs.getQuantile(1);
    		}
    		catch (const AipsError& x) {
    			thrown = True;
    		}
    		AlwaysAssert(thrown, AipsError);
    	}
    	{

    		// getQuantile(), no weights, no mask, no ranges
    		// 5, 2, 6, 10, 7
    		// 11, 6
    		// 2, 5, 6, 6, 7, 10, 11
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(v0.begin(), v0.size()/2);
    		hfs.addData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2);
    		Double q = hfs.getQuantile(0.1);
    		AlwaysAssert(q == 2, AipsError);
    		q = hfs.getQuantile(0.2);
    		AlwaysAssert(q == 5, AipsError);
    		q = hfs.getQuantile(0.3);
    		AlwaysAssert(q == 6, AipsError);
    		q = hfs.getQuantile(0.4);
    		AlwaysAssert(q == 6, AipsError);
    		q = hfs.getQuantile(0.5);
    		AlwaysAssert(q == 6, AipsError);
    		q = hfs.getQuantile(0.6);
    		AlwaysAssert(q == 7, AipsError);
    		q = hfs.getQuantile(0.7);
    		AlwaysAssert(q == 7, AipsError);
    		q = hfs.getQuantile(0.8);
    		AlwaysAssert(q == 10, AipsError);
    		q = hfs.getQuantile(0.9);
    		AlwaysAssert(q == 11, AipsError);
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
    		std::map<Double, Double> qs = hfs.getQuantiles(quantiles);
    		AlwaysAssert(qs[0.1] == 2, AipsError);
    		AlwaysAssert(qs[0.2] == 5, AipsError);
    		AlwaysAssert(qs[0.3] == 6, AipsError);
    		AlwaysAssert(qs[0.4] == 6, AipsError);
    		AlwaysAssert(qs[0.5] == 6, AipsError);
    		AlwaysAssert(qs[0.6] == 7, AipsError);
    		AlwaysAssert(qs[0.7] == 7, AipsError);
    		AlwaysAssert(qs[0.8] == 10, AipsError);
    		AlwaysAssert(qs[0.9] == 11, AipsError);
    	}
    	{
    		// getMedianAbsDevMed()
    		// 5, 2, 6, 10, 7
    		// 11, 6
    		// 2, 5, 6, 6, 7, 10, 11
    		// 4, 1, 0, 0, 1, 4, 5
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(v0.begin(), v0.size()/2);
    		hfs.addData(v0.begin() + v0.size()/2, v0.size() - v0.size()/2);
    		Double medabsdevmed = hfs.getMedianAbsDevMed();
    		AlwaysAssert(medabsdevmed == 1, AipsError);
    	}
    	{
    		// getMedian() with binning, no ranges, weights, or mask
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double median = hfs.getMedian(NULL, NULL, NULL, 100);
    		AlwaysAssert(median == -1, AipsError);
    	}
    	{
    		// getMedianAbsDevMed() with binning, no ranges, weights, or mask
    		HingesFencesStatistics<Double, vector<Double>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.setData(bigData.begin(), bigData.size());
    		// enforce a small internal array size so binning algorithm is used
    		Double medabsdevmed = hfs.getMedianAbsDevMed(NULL, NULL, NULL, 100);
    		AlwaysAssert(medabsdevmed == 499295.0, AipsError);
    	}
    	{
    		// large array with all the same values, getMedianAndQuartile()
    		vector<Float> big(100000, 0);
    		HingesFencesStatistics<Double, vector<Float>::const_iterator, vector<Bool>::const_iterator> hfs(0);
    		hfs.addData(big.begin(), big.size());
    		std::set<Double> quantiles;
    		quantiles.insert(0.25);
    		quantiles.insert(0.75);
    		CountedPtr<uInt64> npts;
    		CountedPtr<Double> mymin, mymax;
    		std::map<Double, Double> quantileToValue;
    		Double median = hfs.getMedianAndQuantiles(
    			quantileToValue, quantiles, npts, mymin, mymax, 50000
    		);
    		AlwaysAssert(median == 0, AipsError);
    		AlwaysAssert(quantileToValue[0.25] == 0, AipsError);
    		AlwaysAssert(quantileToValue[0.75] == 0, AipsError);
    	}
    }
    catch (const AipsError& x) {
        cout << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}






