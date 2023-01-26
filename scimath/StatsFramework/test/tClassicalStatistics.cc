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

#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays.h>
#include <casacore/scimath/StatsFramework/ClassicalStatistics.h>
#include <casacore/casa/Exceptions/Error.h>

#include <vector>

#include <casacore/casa/namespace.h>

#define COMMA ,

int main() {
    try {
        std::vector<double> v0(5);
        v0[0] = 2;
        v0[1] = 1;
        v0[2] = 1.5;
        v0[3] = 3;
        v0[4] = 2.5;
        std::vector<double> v1(3);
        v1[0] = 5;
        v1[1] = 8;
        v1[2] = 10;
        double k[] = {1.5, 1, 2, 3, 2.5};
        {
            ClassicalStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > cs;
            cs.setData(v0.begin(), v0.size());
            StatsData<double> sd = cs.getStatistics();
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
                cs.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(0, 3),
                AipsError
            );
            AlwaysAssert(
                cs.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            ClassicalStatistics<double, double*, bool*> cs1;
            cs1.setData(k, 5);
            StatsData<double> sd = cs1.getStatistics();
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
                == std::pair<int64_t COMMA int64_t>(0, 3),
                AipsError
            );
            AlwaysAssert(
                cs1.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            ClassicalStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (211.5 - 33.0*33.0/8.0)/7.0;
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
            AlwaysAssert(near(sd.variance, variance), AipsError);
            AlwaysAssert(
                cs.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(1, 2),
                AipsError
            );
            AlwaysAssert(
                cs.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
                == std::pair<int64_t COMMA int64_t>(0, 2),
                AipsError
            );
            AlwaysAssert(
                cs.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(1, 1),
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
            std::vector<double> t0(5);
            t0[0] = 1.5;
            t0[1] = 1;
            t0[2] = 2;
            t0[3] = 3;
            t0[4] = 2.5;
            std::vector<double> t1(3);
            t1[0] = 5;
            t1[1] = 8;
            t1[2] = 10;
            ClassicalStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > cs;
            cs.setCalculateAsAdded(false);
            cs.setData(t0.begin(), t0.size());
            std::fill(t0.begin(), t0.begin()+t0.size(), 0);
            cs.addData(t1.begin(), t1.size());
            std::fill(t1.begin(), t1.begin()+t1.size(), 0);
            StatsData<double> sd = cs.getStatistics();
            // not accumulating as added, so everything is zero,
            // and with multi-threading, the min and max positions
            // could be anywhere in the datasets since all values
            // are equal, so no longer test for those
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 0, AipsError);
            AlwaysAssert(sd.mean == 0, AipsError);
            AlwaysAssert(*sd.min == 0, AipsError);
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

            bool exceptionRaised = false;
            try {
                cs.setCalculateAsAdded(true);
            }
            catch (std::exception& x) {
                exceptionRaised = true;
            }
            AlwaysAssert(exceptionRaised, AipsError);
            cs.reset();
            cs.setCalculateAsAdded(true);
            cs.setData(t0.begin(), t0.size());
            std::fill(t0.begin(), t0.begin()+t0.size(), 0);
            cs.addData(t1.begin(), t1.size());
            std::fill(t1.begin(), t1.begin()+t1.size(), 0);
            sd = cs.getStatistics();
            double variance = (211.5 - 33.0*33.0/8.0)/7.0;
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
            ClassicalStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > cs;
            cs.setData(v0.begin(), v0.size(), 2);
            cs.addData(v1.begin(), v1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (201.5 - 29.0*29.0/6.0)/5.0;
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
            AlwaysAssert(near(sd.variance, variance), AipsError);
        }
        {
            // data ranges
            ClassicalStatistics<
                double, std::vector<double>::const_iterator, std::vector<bool>::const_iterator
            > cs;
            std::vector<std::pair<double, double> > r0(1);
            r0[0].first = 5;
            r0[0].second = -5;
            bool expectedFail = false;
            try {
                cs.setData(v0.begin(), 3, r0);
            }
            catch (const std::exception& x) {
                expectedFail = true;
            }
            AlwaysAssert(expectedFail, AipsError);
            r0[0].first = 2.4;
            r0[0].second = 6;
            std::vector<std::pair<double, double> > r1(2);
            r1[0].first = 9;
            r1[0].second = 11;
            r1[1].first = 2;
            r1[1].second = 7;
            cs.setData(v0.begin(), v0.size(), r0);
            cs.addData(v1.begin(), v1.size(), r1, false);
            StatsData<double> sd = cs.getStatistics();
            double variance = (79.25 - 13.5*13.5/3.0)/2.0;
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<bool> m0(v0.size());
            m0[0] = false;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            cs.setData(v0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), m1.begin(), v1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (79.25 - 13.5*13.5/3.0)/2.0;
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

            // test cloning gives same results
            std::shared_ptr<
                ClassicalStatistics<
                    double, std::vector<double>::const_iterator,
                    std::vector<bool>::const_iterator
                >
            > cs1(
                dynamic_cast<
                    ClassicalStatistics<
                        double, std::vector<double>::const_iterator,
                        std::vector<bool>::const_iterator
                    >*
                >(cs.clone())
            );
            StatsData<double> sd1 = cs1->getStatistics();
            AlwaysAssert(sd1.masked, AipsError);
            AlwaysAssert(! sd1.weighted, AipsError);
            AlwaysAssert(*sd1.max == *sd.max, AipsError);
            AlwaysAssert(sd1.maxpos.first == sd.maxpos.first , AipsError);
            AlwaysAssert(sd1.maxpos.second == sd.maxpos.second, AipsError);
            AlwaysAssert(sd1.mean == sd.mean, AipsError);
            AlwaysAssert(*sd1.min == *sd.min, AipsError);
            AlwaysAssert(sd1.minpos.first == sd.minpos.first, AipsError);
            AlwaysAssert(sd1.minpos.second == sd.minpos.second, AipsError);
            AlwaysAssert(sd1.npts == sd.npts, AipsError);
            AlwaysAssert(sd1.rms == sd.rms, AipsError);
            AlwaysAssert(sd1.stddev == sd.stddev, AipsError);
            AlwaysAssert(sd1.sum == sd.sum, AipsError);
            AlwaysAssert(sd1.sumsq == sd.sumsq, AipsError);
            AlwaysAssert(sd1.variance == sd.variance, AipsError);
        }
        {
            // mask and ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<bool> m0(v0.size());
            m0[0] = false;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), m1.begin(), v1.size(), r1, true);
            StatsData<double> sd = cs.getStatistics();
            double variance = (79.25 - 13.5*13.5/3.0)/2.0;
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            cs.setData(v0.begin(), w0.begin(), w0.size());
            cs.addData(v1.begin(), w1.begin(), w1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (529.0 - 82.0*82.0/20.0)/19.0;
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
            // integer weights
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            cs.setData(v0.begin(), w0.begin(), w0.size());
            cs.addData(v1.begin(), w1.begin(), w1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (529.0 - 82.0*82.0/20.0)/19.0;
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            StatsData<double> sd = cs.getStatistics();
            double variance = (195.25 - 40.5*40.5/11.0)/10.0;
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
            // integer weights; ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            StatsData<double> sd = cs.getStatistics();
            double variance = (195.25 - 40.5*40.5/11.0)/10.0;
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 12;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            StatsData<double> sd = cs.getStatistics();
            double variance = (195.25 - 40.5*40.5/11.0)/10.0;
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
                == std::pair<int64_t COMMA int64_t>(1, 1),
                AipsError
            );
            AlwaysAssert(
                cs.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            // integer weights; ranges, and masks
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 12;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            StatsData<double> sd = cs.getStatistics();
            double variance = (195.25 - 40.5*40.5/11.0)/10.0;
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
                == std::pair<int64_t COMMA int64_t>(1, 1),
                AipsError
            );
            AlwaysAssert(
                cs.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (195.25 - 40.5*40.5/11.0)/10.0;
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
            // weights, masks, no max/min (CAS-11859 fix)
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            std::set<StatisticsData::STATS> targets;
            // excludes max/min, so exercises another code branch
            targets.insert(StatisticsData::VARIANCE);
            cs.setStatsToCalculate(targets);
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (195.25 - 40.5*40.5/11.0)/10.0;
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(near(sd.mean, 40.5/11.0), AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            AlwaysAssert(sd.rms == sqrt(195.25/11.0), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(variance)), AipsError);
            AlwaysAssert(sd.sum == 40.5, AipsError);
            AlwaysAssert(sd.sumweights == 11.0, AipsError);
            AlwaysAssert(sd.sumsq == 195.25, AipsError);
            AlwaysAssert(near(sd.variance, variance), AipsError);
        }
        {
            // integer weights; masks
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            StatsData<double> sd = cs.getStatistics();
            double variance = (195.25 - 40.5*40.5/11.0)/10.0;
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size());
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1, AipsError);
            AlwaysAssert(mymax == 10, AipsError);
        }
        {
            // getMinMax(), two datasets, stride = 2,1
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), 3, 2);
            cs.addData(v1.begin(), v1.size());
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.5, AipsError);
            AlwaysAssert(mymax == 10, AipsError);
        }
        {
            // getMinMax(), data ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 2.4;
            r0[0].second = 6;
            vector<std::pair<double, double> > r1(2);
            r1[0].first = 9;
            r1[0].second = 11;
            r1[1].first = 2;
            r1[1].second = 7;
            cs.setData(v0.begin(), v0.size(), r0);
            cs.addData(v1.begin(), v1.size(), r1, false);
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), mask
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<bool> m0(v0.size());
            m0[0] = false;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            cs.setData(v0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), m1.begin(), v1.size());
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), mask and ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<bool> m0(v0.size());
            m0[0] = false;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), m1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax, weights
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 1;
            w0[1] = 0;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 0;
            cs.setData(v0.begin(), w0.begin(), w0.size());
            cs.addData(v1.begin(), w1.begin(), w1.size());
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax, integer weights
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 1;
            w0[1] = 0;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 0;
            cs.setData(v0.begin(), w0.begin(), w0.size());
            cs.addData(v1.begin(), w1.begin(), w1.size());
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), weights and ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), integer weights, and ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), weights, ranges, and masks
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 12;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), integer weights, ranges, and masks
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 12;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }


        {
            // general quantile exceptions
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size());
            bool thrown = false;
            try {
                cs.getQuantile(0);
            }
            catch (const std::exception& x) {
                thrown = true;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = false;
            try {
                cs.getQuantile(1);
            }
            catch (const std::exception& x) {
                thrown = true;
            }
            AlwaysAssert(thrown, AipsError);
        }
        {
            // getQuantile(), no weights, no mask, no ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size());
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size(), 2);
            cs.addData(v1.begin(), v1.size());
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<std::pair<double, double> > r0(1), r1(2);
            r0[0].first = 2.4;
            r0[0].second = 6;
            r1[0].first = 9;
            r1[0].second = 11;
            r1[1].first = 2;
            r1[1].second = 7;
            cs.setData(v0.begin(), v0.size(), r0);
            cs.addData(v1.begin(), v1.size(), r1, false);
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<bool> m0(v0.size());
            m0[0] = false;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            cs.setData(v0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), m1.begin(), v1.size());
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<bool> m0(v0.size());
            m0[0] = false;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), m1.begin(), v1.size(), r1, true);
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            cs.setData(v0.begin(), w0.begin(), w0.size());
            cs.addData(v1.begin(), w1.begin(), w1.size());
            // 1, 1.5, 2.5, 3, 5, 8, 10
            double q = cs.getQuantile(0.1);
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
            // getQuantile(): integer weights
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            cs.setData(v0.begin(), w0.begin(), w0.size());
            cs.addData(v1.begin(), w1.begin(), w1.size());
            // 1, 1.5, 2.5, 3, 5, 8, 10
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            // getQuantile(): ranges and integer weights
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            cs.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            // getQuantile(): integer weights and mask
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = false;
            m0[2] = false;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = false;
            m1[1] = true;
            m1[2] = false;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            vector<double> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<double> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 12;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            std::set<double> quantiles;
            quantiles.insert(0.1);
            quantiles.insert(0.2);
            quantiles.insert(0.3);
            quantiles.insert(0.4);
            quantiles.insert(0.5);
            quantiles.insert(0.6);
            quantiles.insert(0.7);
            quantiles.insert(0.8);
            quantiles.insert(0.9);
            std::map<double, double> qs = cs.getQuantiles(quantiles);
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
            // getQuantile(): integer weights, mask, ranges
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> cs;
            vector<int32_t> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<int32_t> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<bool> m0(v0.size());
            m0[0] = true;
            m0[1] = true;
            m0[2] = true;
            m0[3] = true;
            m0[4] = true;
            vector<bool> m1(v1.size());
            m1[0] = true;
            m1[1] = true;
            m1[2] = false;
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<double, double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 12;
            cs.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            cs.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            // 2.5, 3, 8
            double q = cs.getQuantile(0.1);
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
            std::set<double> quantiles;
            quantiles.insert(0.1);
            quantiles.insert(0.2);
            quantiles.insert(0.3);
            quantiles.insert(0.4);
            quantiles.insert(0.5);
            quantiles.insert(0.6);
            quantiles.insert(0.7);
            quantiles.insert(0.8);
            quantiles.insert(0.9);
            std::map<double, double> qs = cs.getQuantiles(quantiles);
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
            ClassicalStatistics<Complex, vector<Complex>::const_iterator, vector<bool>::const_iterator> cs;
        }
        {
            // getMedian()
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.addData(v0.begin(), v0.size());
            double median = cs.getMedian();
            AlwaysAssert(median == 2, AipsError);
            median = cs.getStatistic(StatisticsData::MEDIAN);
            AlwaysAssert(median == 2, AipsError);
            cs.reset();
            vector<bool> m0(v0.size(), true);
            m0[0] = false;
            cs.addData(v0.begin(), m0.begin(), v0.size());
            median = cs.getMedian();
            AlwaysAssert(median == 2, AipsError);
        }
        {
            // getMedianAndQuantiles (even sized data set)
            std::set<double> quantiles;
            quantiles.insert(0.1);
            quantiles.insert(0.2);
            quantiles.insert(0.3);
            quantiles.insert(0.4);
            quantiles.insert(0.5);
            quantiles.insert(0.6);
            quantiles.insert(0.7);
            quantiles.insert(0.8);
            quantiles.insert(0.9);
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size());
            std::map<double, double> quantileToValue;
            double median = cs.getMedianAndQuantiles(quantileToValue, quantiles);
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
            std::set<double> quantiles;
            quantiles.insert(0.1);
            quantiles.insert(0.2);
            quantiles.insert(0.3);
            quantiles.insert(0.4);
            quantiles.insert(0.5);
            quantiles.insert(0.6);
            quantiles.insert(0.7);
            quantiles.insert(0.8);
            quantiles.insert(0.9);
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 9;
            r0[0].second = 11;
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size(), r0, false);
            std::map<double, double> quantileToValue;
            double median = cs.getMedianAndQuantiles(quantileToValue, quantiles);
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
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size());
            double medabsdevmed = cs.getMedianAbsDevMed();
            AlwaysAssert(medabsdevmed == 1.5, AipsError);
        }
        {
            // getMedianAndQuantiles (odd sized data set)
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 9;
            r0[0].second = 11;
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(v0.begin(), v0.size());
            cs.addData(v1.begin(), v1.size(), r0, false);
            double medabsdevmed = cs.getMedianAbsDevMed();
            AlwaysAssert(medabsdevmed == 1.0, AipsError);
        }
        uint32_t npts = (uint32_t)1e6;
        vector<double> bigData(npts);
        vector<double>::iterator iter = bigData.begin();
        vector<double>::iterator end = bigData.end();
        uint64_t count = 0;
        while(iter != end) {
            *iter = count % 2 == 0 ? double(count) : -double(count*count);
            ++iter;
            ++count;
        }
        vector<bool> bigMask(npts, true);
        bigMask[0] = false;
        {
            // getMedian() with binning, no ranges, weights, or mask
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(bigData.begin(), bigData.size());
            // enforce a small internal array size so binning algorithm is used
            double median = cs.getMedian(NULL, NULL, NULL, 100);
            AlwaysAssert(median == -0.5, AipsError);
        }
        {
            // getMedian() with mask, but no weights or ranges, using binning
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(bigData.begin(), bigMask.begin(), bigData.size());
            // enforce a small internal array size so binning algorithm is used
            double median = cs.getMedian(NULL, NULL, NULL, 100);
            AlwaysAssert(median == -1, AipsError);
        }
        {
            // getMedianAbsDevMed() with binning, no ranges, weights, or mask
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(bigData.begin(), bigData.size());
            // enforce a small internal array size so binning algorithm is used
            double medabsdevmed = cs.getMedianAbsDevMed(NULL, NULL, NULL, 100);
            AlwaysAssert(medabsdevmed == 998999.5, AipsError);
        }
        {
            // getMedianAbsDevMed() with mask, but no weights or ranges, using binning
            ClassicalStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> cs;
            cs.setData(bigData.begin(), bigMask.begin(), bigData.size());
            // enforce a small internal array size so binning algorithm is used
            double medabsdevmed = cs.getMedianAbsDevMed(NULL, NULL, NULL, 100);
            AlwaysAssert(medabsdevmed == 999001, AipsError);
        }
        {
            // large array with all the same values, getMedianAndQuartile()
            vector<float> big(100000, 30);
            ClassicalStatistics<double, vector<float>::const_iterator, vector<bool>::const_iterator> cs;
            cs.addData(big.begin(), big.size());
            std::set<double> quantiles;
            quantiles.insert(0.25);
            quantiles.insert(0.75);
            CountedPtr<uint64_t> npts;
            CountedPtr<double> mymin, mymax;
            std::map<double, double> quantileToValue;
            double median = cs.getMedianAndQuantiles(
                quantileToValue, quantiles, npts, mymin, mymax, 100
            );
            AlwaysAssert(median == 30, AipsError);
            AlwaysAssert(quantileToValue[0.25] == 30, AipsError);
            AlwaysAssert(quantileToValue[0.75] == 30, AipsError);
        }
        {
            // two large array with two unique values, getMedianAndQuartile()
            ClassicalStatistics<double, vector<float>::const_iterator, vector<bool>::const_iterator> cs;
            vector<float> big(100000, 30);
            cs.addData(big.begin(), big.size());
            vector<float> big2(50000, -10);
            cs.addData(big2.begin(), big2.size());
            std::set<double> quantiles;
            quantiles.insert(0.25);
            quantiles.insert(0.75);
            CountedPtr<uint64_t> npts;
            CountedPtr<double> mymin, mymax;
            std::map<double, double> quantileToValue;
            double median = cs.getMedianAndQuantiles(
                quantileToValue, quantiles, npts, mymin, mymax, 100
            );
            AlwaysAssert(median == 30, AipsError);
            AlwaysAssert(quantileToValue[0.25] == -10, AipsError);
            AlwaysAssert(quantileToValue[0.75] == 30, AipsError);
        }
        {
            // medium sized randomized array, that can be sorted in memory in one go
            ClassicalStatistics<double, vector<float>::const_iterator, vector<bool>::const_iterator> cs;
            vector<float> big(100000);
            uint32_t count = 0;
            vector<float>::iterator iter = big.begin();
            vector<float>::iterator end = big.end();
            for (; iter!=end; ++iter, ++count) {
                *iter = count;
            }
            random_shuffle(big.begin(), big.end());
            cs.addData(big.begin(), big.size());
            std::set<double> quantiles;
            quantiles.insert(0.25);
            quantiles.insert(0.75);
            std::map<double, double> quantileToValue;
            double median = cs.getMedianAndQuantiles(
                quantileToValue, quantiles
            );
            AlwaysAssert(median == 49999.5, AipsError);
            AlwaysAssert(quantileToValue[0.25] == 24999, AipsError);
            AlwaysAssert(quantileToValue[0.75] == 74999, AipsError);
        }
        {
            // large array, getMinMax()
            ClassicalStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > cs;
            std::vector<double> big(1e7);
            uint32_t count = 0;
            std::vector<double>::iterator iter = big.begin();
            std::vector<double>::iterator end = big.end();
            for (; iter!=end; ++iter, ++count) {
                *iter = count;
            }
            cs.addData(big.begin(), big.size());
            double mymin, mymax;
            cs.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 0, AipsError);
            AlwaysAssert(mymax == big.size()-1, AipsError);
            // do it again, but shuffle the elements
            random_shuffle(big.begin(), big.end());
            ClassicalStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > cs1;
            cs1.addData(big.begin(), big.size());
            cs1.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 0, AipsError);
            AlwaysAssert(mymax == big.size()-1, AipsError);
        }
        {
            // tests for getNPts()
            uint32_t n = 6;
            uint32_t size[] = {5000, 80000, 6500, 100000, 19256, 7482};
            std::vector<std::vector<double> > data(n);
            ClassicalStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > cs;
            uint64_t expec = 0;
            for (uint32_t i=0; i<n; ++i) {
                uint32_t s = size[i];
                expec += s;
                data[i].resize(s);
                std::fill(data[i].begin(), data[i].begin()+s, 0);
                cs.addData(data[i].begin(), s);
            }
            AlwaysAssert(cs.getNPts() == expec, AipsError);
            cs.reset();
            std::vector<bool> mask3(size[3]);
            std::fill(mask3.begin(), mask3.begin()+size[3], false);
            mask3[1000] = true;
            mask3[1500] = true;
            expec -= (size[3] - 2);
            for (uint32_t i=0; i<n; ++i) {
                uint32_t s = size[i];
                if (i == 3) {
                    cs.addData(data[i].begin(), mask3.begin(), s);
                }
                else {
                    cs.addData(data[i].begin(), s);
                }
            }
            AlwaysAssert(cs.getNPts() == expec, AipsError);
        }
        {
            ClassicalStatistics<
                double, vector<double>::const_iterator, vector<bool>::const_iterator
            > cs;
            vector<double> v {4, 7, 12, 18};
            vector<double> w {0.5, 02, 1, 0.9};
            cs.setData(v.cbegin(), w.cbegin(), v.size());
            auto stats = cs.getStatistics();
            cout << "mean " << stats.mean  << endl;
            cout << "variance " << stats.variance << endl;
            cout << "nvariance " << stats.nvariance << endl;
            cout << "nvar/n " << stats.nvariance/stats.npts << endl;

        }
    }
    catch (const std::exception& x) {
        cout << x.what() << endl;
        return 1;
    } 
    return 0;
}






