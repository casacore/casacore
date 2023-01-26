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
#include <casacore/scimath/StatsFramework/FitToHalfStatistics.h>
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
        v0[3] = 4;
        v0[4] = 2.5;
        std::vector<double> v1(3);
        v1[0] = 5;
        v1[1] = 8;
        v1[2] = 10;
        {
            FitToHalfStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 3.4), AipsError);
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(sd.mean == 2.2, AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == 0, AipsError);
            AlwaysAssert(sd.minpos.second == 1, AipsError);
            double npts = 6;
            double sumsq = 32.98;
            double nvariance = 3.94;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, 13.2), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            // CAS-10760, test that setStatsToCalculate() works correctly
            FitToHalfStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            std::set<StatisticsData::STATS> x;
            x.insert(StatisticsData::VARIANCE);
            fh.setStatsToCalculate(x);
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            double variance = fh.getStatistic(StatisticsData::VARIANCE);
            double npts = 6;
            double nvariance = 3.94;
            AlwaysAssert(near(variance, nvariance/(npts - 1)), AipsError);
            double mean = fh.getStatistic(StatisticsData::MEAN);
            AlwaysAssert(near(mean, 13.2/6), AipsError);
        }
        {
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::GE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 4.0), AipsError);
            AlwaysAssert(sd.maxpos.first == 0, AipsError);
            AlwaysAssert(sd.maxpos.second == 3, AipsError);
            AlwaysAssert(sd.mean == 2.2, AipsError);
            AlwaysAssert(near(*sd.min, 0.4), AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            double npts = 4;
            double sumsq = 26.02;
            double nvariance = 6.66;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, 8.8), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(0, 3),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEDIAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 3.0), AipsError);
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(sd.mean == 2, AipsError);
            AlwaysAssert(near(*sd.min, 1.0), AipsError);
            AlwaysAssert(sd.minpos.first == 0, AipsError);
            AlwaysAssert(sd.minpos.second == 1, AipsError);
            double npts = 6;
            double sumsq = 26.5;
            double nvariance = 2.5;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, 12.0), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEDIAN, FitToHalfStatisticsData::GE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 4.0), AipsError);
            AlwaysAssert(sd.maxpos.first == 0, AipsError);
            AlwaysAssert(sd.maxpos.second == 3, AipsError);
            AlwaysAssert(sd.mean == 2, AipsError);
            AlwaysAssert(near(*sd.min, 0.0), AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            double npts = 6;
            double sumsq = 32.5;
            double nvariance = 8.5;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, 12.0), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(0, 3),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 3
            );
            fh.setData(v0.begin(), v0.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 5.0), AipsError);
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(sd.mean == 3, AipsError);
            AlwaysAssert(near(*sd.min, 1.0), AipsError);
            AlwaysAssert(sd.minpos.first == 0, AipsError);
            AlwaysAssert(sd.minpos.second == 1, AipsError);
            double npts = 8;
            double sumsq = 87;
            double nvariance = 15;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, 24.0), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 2.5
            );
            fh.setData(v0.begin(), v0.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 4.0), AipsError);
            AlwaysAssert(sd.maxpos.first == 0, AipsError);
            AlwaysAssert(sd.maxpos.second == 3, AipsError);
            AlwaysAssert(sd.mean == 2.5, AipsError);
            AlwaysAssert(near(*sd.min, 1.0), AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            double npts = 4;
            double sumsq = 29.5;
            double nvariance = 4.5;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, 10.0), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(0, 3),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(fh.getStatistic(
                StatisticsData::NPTS) == npts, AipsError
            );
            AlwaysAssert(fh.getStatistic(
                StatisticsData::RMS) == sqrt(sumsq/npts), AipsError
            );
        }
        double k[] = {1.5, 1, 2, 4, 2.5};
        {
            // just another way of specifying the data
            FitToHalfStatistics<double, double*, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(k, 5);
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 3.4), AipsError);
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(sd.mean == 2.2, AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == 0, AipsError);
            AlwaysAssert(sd.minpos.second == 1, AipsError);
            double npts = 6;
            double sumsq = 32.98;
            double nvariance = 3.94;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, 13.2), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(near(*sd.max, 7.5), AipsError);
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(sd.mean == 4.25, AipsError);
            AlwaysAssert(*sd.min == 1.0, AipsError);
            AlwaysAssert(sd.minpos.first == 0, AipsError);
            AlwaysAssert(sd.minpos.second == 1, AipsError);
            double npts = 10;
            double sumsq = 233.25;
            double nvariance = 52.625;
            double mean = 4.25;
            AlwaysAssert(sd.npts == npts, AipsError);
            AlwaysAssert(near(sd.rms, sqrt(sumsq/npts)), AipsError);
            AlwaysAssert(near(sd.stddev, sqrt(nvariance/(npts - 1))), AipsError);
            AlwaysAssert(near(sd.sum, mean*npts), AipsError);
            AlwaysAssert(near(sd.sumsq, sumsq), AipsError);
            AlwaysAssert(near(sd.variance, nvariance/(npts - 1)), AipsError);
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(1, 1),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            bool exceptionRaised = false;
            try {
                fh.setCalculateAsAdded(true);
            }
            catch (std::exception& x) {
                exceptionRaised = true;
            }
            AlwaysAssert(exceptionRaised, AipsError);
        }

        {
            // two datasets, stride = 2,1
            // 2, 1.5, 2.5
            // 5, 8, 10
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size(), 2);
            fh.addData(v1.begin(), v1.size());
            StatsData<double> sd = fh.getStatistics();
            double npts = 6;
            double sumsq = 568.0/3.0;
            double nvariance = 295.0/6.0;
            double mean = 29.0/6.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 2),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 5;
            r0[0].second = -5;
            bool expectedFail = false;
            try {
                fh.setData(v0.begin(), 3, r0);
            }
            catch (const std::exception& x) {
                expectedFail = true;
            }
            AlwaysAssert(expectedFail, AipsError);
            r0[0].first = 2.4;
            r0[0].second = 6;
            vector<std::pair<double, double> > r1(2);
            r1[0].first = 9;
            r1[0].second = 11;
            r1[1].first = 2;
            r1[1].second = 7;
            fh.setData(v0.begin(), v0.size(), r0);
            fh.addData(v1.begin(), v1.size(), r1, false);
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumsq = 1903.0/18.0;
            double nvariance =  221.0/18.0;
            double mean = 14.5/3.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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

            // test cloning gives same results
            std::shared_ptr<
                FitToHalfStatistics<
                    double, std::vector<double>::const_iterator,
                    std::vector<bool>::const_iterator
                >
            > fh1(
                dynamic_cast<
                    FitToHalfStatistics<
                        double, std::vector<double>::const_iterator,
                        std::vector<bool>::const_iterator
                    >*
                >(fh.clone())
            );
            StatsData<double> sd1 = fh1->getStatistics();
            AlwaysAssert(sd1.masked == sd.masked, AipsError);
            AlwaysAssert(sd1.weighted == sd.weighted, AipsError);
            AlwaysAssert(*sd1.max == *sd.max, AipsError);
            AlwaysAssert(sd1.maxpos.first == sd.maxpos.first, AipsError);
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
            AlwaysAssert(
                fh1->getStatisticIndex(StatisticsData::MAX)
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh1->getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
                AipsError
            );
            AlwaysAssert(fh1->getStatistic(
                StatisticsData::NPTS) == npts, AipsError
            );
            AlwaysAssert(
                near(
                    fh1->getStatistic(StatisticsData::RMS),
                    sqrt(sumsq/npts)
                ), AipsError
            );
        }
        {
            // mask
            // 4, 2.5
            // 8
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), m0.begin(), v0.size());
            fh.addData(v1.begin(), m1.begin(), v1.size());
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumsq = 1903.0/18.0;
            double nvariance =  221.0/18.0;
            double mean = 14.5/3.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), m0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), m1.begin(), v1.size(), r1, true);
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumsq = 1903.0/18.0;
            double nvariance =  221.0/18.0;
            double mean = 14.5/3.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), w0.size());
            fh.addData(v1.begin(), w1.begin(), w1.size());
            StatsData<double> sd = fh.getStatistics();
            double npts = 8;
            double sumofweights = 28;
            double sumsq = 641.44;
            double nvariance =  123.72;
            double mean = 4.3;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            // integer weights
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), w0.size());
            fh.addData(v1.begin(), w1.begin(), w1.size());
            StatsData<double> sd = fh.getStatistics();
            double npts = 8;
            double sumofweights = 28;
            double sumsq = 641.44;
            double nvariance =  123.72;
            double mean = 4.3;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 1),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumofweights = 18;
            double sumsq = 154146.0/484.0;
            double nvariance = 11568.0/484.0;
            double mean = 44.5/11.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            // integer weights and ranges
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumofweights = 18;
            double sumsq = 154146.0/484.0;
            double nvariance = 11568.0/484.0;
            double mean = 44.5/11.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumofweights = 18;
            double sumsq = 154146.0/484.0;
            double nvariance = 11568.0/484.0;
            double mean = 44.5/11.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            // integer weights; ranges, and masks
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumofweights = 18;
            double sumsq = 154146.0/484.0;
            double nvariance = 11568.0/484.0;
            double mean = 44.5/11.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumofweights = 18;
            double sumsq = 154146.0/484.0;
            double nvariance = 11568.0/484.0;
            double mean = 44.5/11.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            // integer weights, masks
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            StatsData<double> sd = fh.getStatistics();
            double npts = 4;
            double sumofweights = 18;
            double sumsq = 154146.0/484.0;
            double nvariance = 11568.0/484.0;
            double mean = 44.5/11.0;
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
                == std::pair<int64_t COMMA int64_t>(-1, -1),
                AipsError
            );
            AlwaysAssert(
                fh.getStatisticIndex(StatisticsData::MIN)
                == std::pair<int64_t COMMA int64_t>(0, 4),
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1, AipsError);
            AlwaysAssert(mymax == 7.5, AipsError);
            fh = FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator>(
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size(), 2);
            fh.addData(v1.begin(), v1.size());
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.5, AipsError);
            AlwaysAssert(near(mymax, 2*29.0/6.0 - 1.5), AipsError);
        }
        {
            // getMaxMin(), data ranges
            // 2.5, 4
            // 8
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            vector<std::pair<double, double> > r0(1);
            r0[0].first = 2.4;
            r0[0].second = 6;
            vector<std::pair<double, double> > r1(2);
            r1[0].first = 9;
            r1[0].second = 11;
            r1[1].first = 2;
            r1[1].second = 7;
            fh.setData(v0.begin(), v0.size(), r0);
            fh.addData(v1.begin(), v1.size(), r1, false);
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(near(mymax, 43.0/6.0), AipsError);
        }
        {
            // getMinMax(), mask
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), m0.begin(), v0.size());
            fh.addData(v1.begin(), m1.begin(), v1.size());
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(near(mymax, 43.0/6.0), AipsError);
        }
        {
            // getMinMax(), mask and ranges
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), m0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), m1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(near(mymax, 43.0/6.0), AipsError);
        }
        {
            // getMinMax, weights
            // 2, 1.5, 4, 2.5
            // 5, 8
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), w0.size());
            fh.addData(v1.begin(), w1.begin(), w1.size());
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.5, AipsError);
            AlwaysAssert(near(mymax, 5.5), AipsError);
        }
        {
            // getMinMax, integer weights
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), w0.size());
            fh.addData(v1.begin(), w1.begin(), w1.size());
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.5, AipsError);
            AlwaysAssert(near(mymax, 5.5), AipsError);
        }
        {
            // 4, 2.5
            // 8
            // getMinMax(), weights and ranges
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(near(mymax, 123.0/22.0), AipsError);
        }
        {
            // getMinMax(), integer weights and ranges
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
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
            fh.setData(v0.begin(), w0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(near(mymax, 123.0/22.0), AipsError);
        }
        {
            // getMinMax(), weights, ranges, and masks
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );

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
            fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(near(mymax, 123.0/22.0), AipsError);
        }
        {
            // getMinMax(), integer weights, ranges, and masks
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator, vector<int32_t>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );

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
            fh.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, false);
            fh.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, true);
            double mymin, mymax;
            fh.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(near(mymax, 123.0/22.0), AipsError);
        }
        {
            // getNPts(), two datasets
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 1.5
            );
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            uint64_t npts = fh.getNPts();
            AlwaysAssert(npts == 4, AipsError);
            // check calling it again works
            npts = fh.getNPts();
            AlwaysAssert(npts == 4, AipsError);
            // check clearing data and doing it again works
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            npts = fh.getNPts();
            AlwaysAssert(npts == 4, AipsError);

            fh = FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator>(
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            bool thrown = false;
            try {
                fh.getQuantile(0);
            }
            catch (const std::exception& x) {
                thrown = true;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = false;
            try {
                fh.getQuantile(1);
            }
            catch (const std::exception& x) {
                thrown = true;
            }
            AlwaysAssert(thrown, AipsError);
        }
        {
            // getQuantile(), no weights, no mask, no ranges
            FitToHalfStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            // mean is 4.25
            // real + virtual dataset
            // 1, 1.5, 2, 2.5, 4, 4.5, 6, 6.5, 7, 7.5
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            double q = fh.getQuantile(0.1);
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

            fh = FitToHalfStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            >(
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 2
            );
            // real + virtual -6, -4, -1, 1.5, 2, 2, 2.5, 5, 8, 10
            fh.setData(v0.begin(), v0.size(), 2);
            fh.addData(v1.begin(), v1.size());
            double q = fh.getQuantile(0.1);
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

            fh = FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator>(
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
            FitToHalfStatistics<Complex, vector<Complex>::const_iterator, vector<bool>::const_iterator> fh;
        }
        {
            // getMedian()
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 4.25
            );
            fh.addData(v0.begin(), v0.size());
            double median = fh.getMedian();
            AlwaysAssert(median == 4.25, AipsError);
            fh.reset();
            vector<bool> m0(v0.size(), true);
            m0[0] = false;
            fh.addData(v0.begin(), m0.begin(), v0.size());
            median = fh.getMedian();
            AlwaysAssert(median == 4.25, AipsError);
        }
        {
            // getMedianAndQuantiles
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            std::map<double, double> quantileToValue;
            double median = fh.getMedianAndQuantiles(quantileToValue, quantiles);
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

            fh = FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator>(
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
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.setData(v0.begin(), v0.size());
            fh.addData(v1.begin(), v1.size());
            // 1, 1.5, 2, 2.5, 4, 4.5, 6, 6.5, 7, 7.5

            double medabsdevmed = fh.getMedianAbsDevMed();
            AlwaysAssert(medabsdevmed == 2.25, AipsError);
        }
        uint32_t npts = (uint32_t)1e6;
        vector<double> bigData(npts);
        vector<double>::iterator iter = bigData.begin();
        vector<double>::iterator end = bigData.end();
        {
            uint64_t count = 0;
            while(iter != end) {
                *iter = count % 2 == 0 ? count : (-1)*(double)(count*count);
                ++iter;
                ++count;
            }
        }
        vector<bool> bigMask(npts, true);
        bigMask[0] = false;
        {
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 400
            );
            fh.setData(bigData.begin(), bigData.size());
            // getMedian() with binning, no ranges, weights, or mask
            // The array size should be ignored, because the median is trivial.
            double median = fh.getMedian(NULL, NULL, NULL, 100);
            AlwaysAssert(median == 400, AipsError);

            fh = FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator>(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 250
            );
            fh.setData(bigData.begin(), bigData.size());
            median = fh.getMedian(NULL, NULL, NULL, 100);
            AlwaysAssert(median == 250, AipsError);
        }
        {
            // getMedianAbsDevMed() with binning, no ranges, weights, or mask
            FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER, 400
            );
            fh.setData(bigData.begin(), bigData.size());
            // enforce a small internal array size so binning algorithm is used
            double medabsdevmed = fh.getMedianAbsDevMed(NULL, NULL, NULL, 100);
            AlwaysAssert(medabsdevmed == 249799040801ULL, AipsError);

            fh = FitToHalfStatistics<double, vector<double>::const_iterator, vector<bool>::const_iterator>(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::GE_CENTER, 250
            );
            fh.setData(bigData.begin(), bigData.size());
            // enforce a small internal array size so binning algorithm is used
            medabsdevmed = fh.getMedianAbsDevMed(NULL, NULL, NULL, 100);
            AlwaysAssert(medabsdevmed == 499874, AipsError);

        }
        {
            // large array with all the same values, getMedianAndQuantile()
            std::vector<double> big(100000, 30);
            FitToHalfStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.addData(big.begin(), big.size());
            std::set<double> quantiles;
            quantiles.insert(0.25);
            quantiles.insert(0.75);
            CountedPtr<uint64_t> npts;
            CountedPtr<double> mymin, mymax;
            std::map<double, double> quantileToValue;
            double median = fh.getMedianAndQuantiles(
                quantileToValue, quantiles, npts, mymin, mymax, 100
            );
            AlwaysAssert(median == 30, AipsError);
            AlwaysAssert(quantileToValue[0.25] == 30, AipsError);
            AlwaysAssert(quantileToValue[0.75] == 30, AipsError);
        }
        {
            // a large array so we test binning
            Array<double> big(IPosition(1, 100000));
            Array<double>::iterator biter = big.begin();
            Array<double>::iterator bend = big.end();
            uint32_t count = 0;
            while (biter != bend) {
                *biter = count % 2 == 0 ? (float)count : -(float)count - 0.5;
                ++biter;
                ++count;
            }
            FitToHalfStatistics<double, Array<double>::const_iterator, Array<bool>::const_iterator> fh(
                FitToHalfStatisticsData::CMEAN, FitToHalfStatisticsData::LE_CENTER
            );
            fh.addData(big.begin(), big.size());
            std::set<double> quantiles;
            quantiles.insert(0.25);
            quantiles.insert(0.75);
            std::map<double, double> quantileToValue;
            CountedPtr<uint64_t> npts;
            CountedPtr<double> mymin, mymax;
            double median = fh.getMedianAndQuantiles(
                quantileToValue, quantiles, npts, mymin, mymax, 100
            );
            AlwaysAssert(near(median, -0.75, 1e-12), AipsError);
            AlwaysAssert(near(quantileToValue[0.25],-50001.5), AipsError);
            AlwaysAssert(near(quantileToValue[0.75], 49998.0), AipsError);

            fh = FitToHalfStatistics<double, Array<double>::const_iterator, Array<bool>::const_iterator>(
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
        {
            // CAS-10760 fix for null set equivalent
            FitToHalfStatistics<
                double, std::vector<double>::const_iterator,
                std::vector<bool>::const_iterator
            > fh(
                FitToHalfStatisticsData::CVALUE, FitToHalfStatisticsData::LE_CENTER
            );
            fh.addData(v0.begin(), v0.size());
            StatsData<double> sd = fh.getStatistics();
            AlwaysAssert(sd.npts == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(sd.rms == 0, AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(! sd.max, AipsError);
            AlwaysAssert(! sd.min, AipsError);
        }
    }
    catch (const std::exception& x) {
        cout << x.what() << endl;
        return 1;
    } 
    return 0;
}






