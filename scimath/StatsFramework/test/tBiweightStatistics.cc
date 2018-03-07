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
#include <casacore/scimath/StatsFramework/BiweightStatistics.h>
#include <casacore/casa/Exceptions/Error.h>

#include <vector>

#include <casacore/casa/namespace.h>

#define COMMA ,

int main() {
    try {
        std::vector<Double> v0(5);
        v0[0] = 2;
        v0[1] = 1;
        v0[2] = 1.4;
        v0[3] = 3;
        v0[4] = 2.5;
        std::vector<Double> v1(3);
        v1[0] = 5;
        v1[1] = 8;
        v1[2] = 10;
        Double k[] = {1.4, 1, 2, 3, 2.5};
        const Double eps = 1e-11;
        {
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bws(10);
            bws.setData(v0.begin(), v0.size());
            // test unsupported stats through an exception
            StatisticsData::STATS stat = StatisticsData::MEAN;
            for (uInt i=0; i<5; ++i) {
                Bool thrown = False;
                switch(i) {
                case 0:
                    stat = StatisticsData::MEDIAN;
                    break;
                case 1:
                    stat = StatisticsData::RMS;
                    break;
                case 2:
                    stat = StatisticsData::SUM;
                    break;
                case 3:
                    stat = StatisticsData::VARIANCE;
                    break;
                case 4:
                    stat = StatisticsData::FIRST_QUARTILE;
                    break;
                default:
                    break;
                }
                try {
                    bws.getStatistic(stat);
                }
                catch (const AipsError& x) {
                    thrown = True;
                }
                AlwaysAssert(thrown, AipsError);
            }
            StatsData<Double> sd = bws.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 3, AipsError);
            // isn't set be this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 1.98059737309;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 5, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.868802742897;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            Bool thrown = False;
            try {
                bws.getStatisticIndex(StatisticsData::MAX);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = False;
            try {
                bws.getStatisticIndex(StatisticsData::MIN);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(bws.getStatistic(
                StatisticsData::NPTS) == 5, AipsError
            );
            thrown = False;
            try {
                bws.getStatistic(StatisticsData::RMS);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(
                near(
                    bws.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bws.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
            AlwaysAssert(bws.getNiter() == 1, AipsError);
        }
        {
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bws(-1);
            bws.setData(v0.begin(), v0.size());
            StatsData<Double> sd = bws.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 3, AipsError);
            // isn't set be this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 1.98056452649;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 5, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.865625126924;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            Bool thrown = False;
            try {
                bws.getStatisticIndex(StatisticsData::MAX);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = False;
            try {
                bws.getStatisticIndex(StatisticsData::MIN);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(bws.getStatistic(
                StatisticsData::NPTS) == 5, AipsError
            );
            thrown = False;
            try {
                bws.getStatistic(StatisticsData::RMS);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(
                near(
                    bws.getStatistic(StatisticsData::MEAN),
                    expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bws.getStatistic(StatisticsData::STDDEV),
                    expStdev, eps
                ), AipsError
            );
            AlwaysAssert(bws.getNiter() == -1, AipsError);
        }
        {
            // just another way of specifying the data
            BiweightStatistics<Double, Double*, Bool*> bw1(10);
            bw1.setData(k, 5);
            StatsData<Double> sd = bw1.getStatistics();
            StatisticsData::STATS stat = StatisticsData::MEAN;
            for (uInt i=0; i<5; ++i) {
                Bool thrown = False;
                switch(i) {
                case 0:
                    stat = StatisticsData::MEDIAN;
                    break;
                case 1:
                    stat = StatisticsData::RMS;
                    break;
                case 2:
                    stat = StatisticsData::SUM;
                    break;
                case 3:
                    stat = StatisticsData::VARIANCE;
                    break;
                case 4:
                    stat = StatisticsData::FIRST_QUARTILE;
                    break;
                default:
                    break;
                }
                try {
                    bw1.getStatistic(stat);
                }
                catch (const AipsError& x) {
                    thrown = True;
                }
                AlwaysAssert(thrown, AipsError);
            }
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 3, AipsError);
            // isn't set be this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 1.98059737309;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 5, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.868802742897;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            Bool thrown = False;
            try {
                bw1.getStatisticIndex(StatisticsData::MAX);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = False;
            try {
                bw1.getStatisticIndex(StatisticsData::MIN);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(bw1.getStatistic(
                StatisticsData::NPTS) == 5, AipsError
            );
            thrown = False;
            try {
                bw1.getStatistic(StatisticsData::RMS);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(
                near(
                    bw1.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw1.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
            AlwaysAssert(bw1.getNiter() == 1, AipsError);
        }
        {
            // two datasets
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bw;
            bw.setData(v0.begin(), v0.size());
            bw.addData(v1.begin(), v1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 10, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 3.97415612639;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 8, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 3.43760003872;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            Bool thrown = False;
            try {
                bw.getStatisticIndex(StatisticsData::MAX);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = False;
            try {
                bw.getStatisticIndex(StatisticsData::MIN);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 8, AipsError
            );
            thrown = False;
            try {
                bw.getStatistic(StatisticsData::RMS);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
            AlwaysAssert(bw.getNiter() == 2, AipsError);
            // Now reverse the order that the datasets were added. results
            // should be the same except for min and max dataset locations
            bw.setData(v1.begin(), v1.size());
            bw.addData(v0.begin(), v0.size());
            sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 10, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 8, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            thrown = False;
            try {
                bw.getStatisticIndex(StatisticsData::MAX);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = False;
            try {
                bw.getStatisticIndex(StatisticsData::MIN);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 8, AipsError
            );
            thrown = False;
            try {
                bw.getStatistic(StatisticsData::RMS);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
            AlwaysAssert(bw.getNiter() == 2, AipsError);
        }
        {
            // Test accumulating as datasets are added.
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bw;
            bw.setCalculateAsAdded(False);
            bw.setData(v0.begin(), v0.size());
            bw.addData(v1.begin(), v1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 10, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 3.97415612639;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 8, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 3.43760003872;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 8, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
            AlwaysAssert(bw.getNiter() == 2, AipsError);
            Bool thrown = False;
            try {
                bw.setCalculateAsAdded(True);
            }
            catch (AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            bw.reset();
            thrown = False;
            try {
                bw.setCalculateAsAdded(True);
            }
            catch (AipsError& x) {
                thrown = True;
            }
        }
        {
            // two datasets, stride = 2,1
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bw;
            bw.setData(v0.begin(), v0.size(), 2);
            bw.addData(v1.begin(), v1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 10, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 4.74754715912;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1.4, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 6, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 3.77813315235;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 6, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // data ranges
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator, std::vector<Bool>::const_iterator
            > bw;
            std::vector<std::pair<Double, Double> > r0(1);
            r0[0].first = 5;
            r0[0].second = -5;
            Bool thrown = False;
            try {
                bw.setData(v0.begin(), 3, r0);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            r0[0].first = 2.4;
            r0[0].second = 6;
            std::vector<std::pair<Double, Double> > r1(2);
            r1[0].first = 9;
            r1[0].second = 11;
            r1[1].first = 2;
            r1[1].second = 7;
            bw.setData(v0.begin(), v0.size(), r0);
            bw.addData(v1.begin(), v1.size(), r1, False);
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // mask
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), m0.begin(), v0.size());
            bw.addData(v1.begin(), m1.begin(), v1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
            // test cloning gives same results
            SHARED_PTR<
                BiweightStatistics<
                    Double, std::vector<Double>::const_iterator,
                    std::vector<Bool>::const_iterator
                >
            > bw1(
                dynamic_cast<
                    BiweightStatistics<
                        Double, std::vector<Double>::const_iterator,
                        std::vector<Bool>::const_iterator
                    >*
                >(bw.clone())
            );
            sd = bw1->getStatistics();
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw1->getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw1->getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw1->getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // mask and ranges
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), m0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), m1.begin(), v1.size(), r1, True);
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(! sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // weights, which don't have any effect for this algorithm,
            // except for weight = 0
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), w0.begin(), w0.size());
            bw.addData(v1.begin(), w1.begin(), w1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 10, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 4.31952746181;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 7, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 3.64182681772;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 7, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // integer weights
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator, vector<Int>::const_iterator
            > bw;
            vector<Int> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<Int> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            bw.setData(v0.begin(), w0.begin(), w0.size());
            bw.addData(v1.begin(), w1.begin(), w1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 10, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 4.31952746181;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 1, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 7, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 3.64182681772;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 7, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // weights and ranges
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // integer weights; ranges
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator, vector<Int>::const_iterator
            > bw;
            vector<Int> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<Int> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<Double, Double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<Double, Double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            bw.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(! sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // weights, ranges, and masks
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // integer weights; ranges, and masks
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator, vector<Int>::const_iterator
            > bw;
            vector<Int> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<Int> w1(v1.size());
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
            bw.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // weights, masks
            BiweightStatistics<
                Double, vector<Double>::const_iterator, vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            bw.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // integer weights; masks
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator, vector<Int>::const_iterator
            > bw;
            vector<Int> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<Int> w1(v1.size());
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
            bw.setData(v0.begin(), w0.begin(), m0.begin(), v0.size());
            bw.addData(v1.begin(), w1.begin(), m1.begin(), v1.size());
            StatsData<Double> sd = bw.getStatistics();
            AlwaysAssert(sd.masked, AipsError);
            AlwaysAssert(sd.weighted, AipsError);
            AlwaysAssert(*sd.max == 8, AipsError);
            // isn't set by this algorithm
            AlwaysAssert(sd.maxpos.first == -1, AipsError);
            AlwaysAssert(sd.maxpos.second == -1, AipsError);
            Double expMean = 2.7501751458;
            AlwaysAssert(near(sd.mean, expMean, eps), AipsError);
            AlwaysAssert(*sd.min == 2.5, AipsError);
            AlwaysAssert(sd.minpos.first == -1, AipsError);
            AlwaysAssert(sd.minpos.second == -1, AipsError);
            AlwaysAssert(sd.npts == 3, AipsError);
            // not computed
            AlwaysAssert(sd.rms == 0, AipsError);
            Double expStdev = 0.437209840794;
            AlwaysAssert(near(sd.stddev, expStdev, eps), AipsError);
            AlwaysAssert(sd.sum == 0, AipsError);
            AlwaysAssert(sd.sumsq == 0, AipsError);
            AlwaysAssert(sd.variance == 0, AipsError);
            AlwaysAssert(bw.getStatistic(
                StatisticsData::NPTS) == 3, AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::MEAN), expMean, eps
                ), AipsError
            );
            AlwaysAssert(
                near(
                    bw.getStatistic(StatisticsData::STDDEV), expStdev, eps
                ), AipsError
            );
        }
        {
            // getMinMax(), two datasets
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
            bw.setData(v0.begin(), v0.size());
            bw.addData(v1.begin(), v1.size());
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1, AipsError);
            AlwaysAssert(mymax == 10, AipsError);
        }
        {
            // getMinMax(), two datasets, stride = 2,1
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
            bw.setData(v0.begin(), 3, 2);
            bw.addData(v1.begin(), v1.size());
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.4, AipsError);
            AlwaysAssert(mymax == 10, AipsError);
        }
        {
            // getMinMax(), data ranges
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
            vector<std::pair<Double, Double> > r0(1);
            r0[0].first = 2.4;
            r0[0].second = 6;
            vector<std::pair<Double, Double> > r1(2);
            r1[0].first = 9;
            r1[0].second = 11;
            r1[1].first = 2;
            r1[1].second = 7;
            bw.setData(v0.begin(), v0.size(), r0);
            bw.addData(v1.begin(), v1.size(), r1, False);
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), mask
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), m0.begin(), v0.size());
            bw.addData(v1.begin(), m1.begin(), v1.size());
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), mask and ranges
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), m0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), m1.begin(), v1.size(), r1, True);
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax, weights
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), w0.begin(), w0.size());
            bw.addData(v1.begin(), w1.begin(), w1.size());
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.4, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax, integer weights
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator, vector<Int>::const_iterator
            > bw;
            vector<Int> w0(v0.size());
            w0[0] = 1;
            w0[1] = 0;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<Int> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 0;
            bw.setData(v0.begin(), w0.begin(), w0.size());
            bw.addData(v1.begin(), w1.begin(), w1.size());
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 1.4, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), weights and ranges
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), integer weights, and ranges
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator, vector<Int>::const_iterator
            > bw;
            vector<Int> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<Int> w1(v1.size());
            w1[0] = 1;
            w1[1] = 2;
            w1[2] = 3;
            vector<std::pair<Double, Double> > r0(1);
            r0[0].first = 0.9;
            r0[0].second = 1.6;
            vector<std::pair<Double, Double> > r1(1);
            r1[0].first = 6;
            r1[0].second = 9;
            bw.setData(v0.begin(), w0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), v1.size(), r1, True);
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), weights, ranges, and masks
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
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
            bw.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // getMinMax(), integer weights, ranges, and masks
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator, vector<Int>::const_iterator
            > bw;
            vector<Int> w0(v0.size());
            w0[0] = 0;
            w0[1] = 2;
            w0[2] = 3;
            w0[3] = 4;
            w0[4] = 5;
            vector<Int> w1(v1.size());
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
            bw.setData(v0.begin(), w0.begin(), m0.begin(), v0.size(), r0, False);
            bw.addData(v1.begin(), w1.begin(), m1.begin(), v1.size(), r1, True);
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 2.5, AipsError);
            AlwaysAssert(mymax == 8, AipsError);
        }
        {
            // general quantile exceptions
            BiweightStatistics<
                Double, vector<Double>::const_iterator,
                vector<Bool>::const_iterator
            > bw;
            bw.setData(v0.begin(), v0.size());
            bw.addData(v1.begin(), v1.size());
            Bool thrown = False;
            try {
                bw.getQuantile(0.1);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = False;
            try {
                bw.getMedian();
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            thrown = False;
            try {
                std::map<Double, Double> qToV;
                std::set<Double> qs;
                qs.insert(0.1);
                bw.getMedianAndQuantiles(qToV, qs);
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
            try {
                bw.getMedianAbsDevMed();
            }
            catch (const AipsError& x) {
                thrown = True;
            }
            AlwaysAssert(thrown, AipsError);
        }
        {
            // large array, getMinMax()
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bw;
            std::vector<Double> big(1e7);
            uInt count = 0;
            std::vector<Double>::iterator iter = big.begin();
            std::vector<Double>::iterator end = big.end();
            for (; iter!=end; ++iter, ++count) {
                *iter = count;
            }
            bw.addData(big.begin(), big.size());
            Double mymin, mymax;
            bw.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 0, AipsError);
            AlwaysAssert(mymax == big.size()-1, AipsError);
            // do it again, but shuffle the elements
            random_shuffle(big.begin(), big.end());
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bw1;
            bw1.addData(big.begin(), big.size());
            bw1.getMinMax(mymin, mymax);
            AlwaysAssert(mymin == 0, AipsError);
            AlwaysAssert(mymax == big.size()-1, AipsError);
        }
        {
            // tests for getNPts()
            uInt n = 6;
            uInt size[] = {5000, 80000, 6500, 100000, 19256, 7482};
            std::vector<std::vector<Double> > data(n);
            BiweightStatistics<
                Double, std::vector<Double>::const_iterator,
                std::vector<Bool>::const_iterator
            > bw;
            uInt64 expec = 0;
            for (uInt i=0; i<n; ++i) {
                uInt s = size[i];
                expec += s;
                data[i].resize(s);
                std::fill(data[i].begin(), data[i].begin()+s, 0);
                bw.addData(data[i].begin(), s);
            }
            AlwaysAssert(bw.getNPts() == expec, AipsError);
            bw.reset();
            std::vector<Bool> mask3(size[3]);
            std::fill(mask3.begin(), mask3.begin()+size[3], False);
            mask3[1000] = True;
            mask3[1500] = True;
            expec -= (size[3] - 2);
            for (uInt i=0; i<n; ++i) {
                uInt s = size[i];
                if (i == 3) {
                    bw.addData(data[i].begin(), mask3.begin(), s);
                }
                else {
                    bw.addData(data[i].begin(), s);
                }
            }
            AlwaysAssert(bw.getNPts() == expec, AipsError);
        }
    }
    catch (const AipsError& x) {
        cout << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}
