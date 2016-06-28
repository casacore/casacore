//# Copyright (C) 2000,2001
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
//# $Id: Array.h 21545 2015-01-22 19:36:35Z gervandiepen $

#ifndef SCIMATH_PARALLELCLASSICALSTATISTICS_TCC
#define SCIMATH_PARALLELCLASSICALSTATISTICS_TCC

#include <casacore/scimath/Mathematics/ParallelClassicalStatistics.h>

namespace casacore {

CASA_STATD
ParallelClassicalStatistics<CASA_STATP>::ParallelClassicalStatistics(uInt nThreadsMax)
	: ClassicalStatistics<CASA_STATP>(), _nThreadsMax(nThreadsMax) {}

CASA_STATD
ParallelClassicalStatistics<CASA_STATP>::~ParallelClassicalStatistics() {}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::setCalculateAsAdded(
    Bool c
) {
    ThrowIf(
        c, "This class does not support computing "
        "statistics as data sets are added"
    );
}

CASA_STATD
StatsData<AccumType> ParallelClassicalStatistics<CASA_STATP>::_getStatistics() {
    StatsData<AccumType>& statsData = this->_getStatsData();
    if (! this->_getMustAccumulate()) {
        return statsData;
    }
    this->_initIterators();
    statsData.masked = False;
    statsData.weighted = False;
    ClassicalStatistics<CASA_STATP> *cs = new ClassicalStatistics<CASA_STATP>[_nThreadsMax];
    std::pair<Int64, Int64> myInitPos(-1, -1);
    std::pair<Int64, Int64> *maxpos = new std::pair<Int64, Int64>[8*_nThreadsMax];
    std::pair<Int64, Int64> *minpos = new std::pair<Int64, Int64>[8*_nThreadsMax];
    for (uInt i=0; i<_nThreadsMax; ++i) {
        cs[i].setCalculateAsAdded(True);
        maxpos[8*i] = myInitPos;
        minpos[8*i] = myInitPos;
    }
    vector<typename ClassicalStatistics<CASA_STATP>::InitContainer> ci(_nThreadsMax);
    Bool initMin = True;
    Bool initMax = True;
    AccumType mymax = 0;
    AccumType mymin = 0;
    StatsDataProvider<CASA_STATP> *dataProvider = this->_getDataProvider();
    Bool updateMaxMin = dataProvider && this->_getDoMaxMin();
    while (True) {
        typename ClassicalStatistics<CASA_STATP>::InitContainer initVars
            = this->_initAndGetLoopVars();
        uInt nthreads = initVars.count == 1
            ? 1 : min(_nThreadsMax, initVars.count/100 + 2);
        uInt extra = initVars.count % nthreads;
        uInt ciCount = initVars.count/nthreads;
        for (uInt i=0; i<nthreads; ++i) {
            ci[i] = initVars;
            for (uInt j=0; j<i*initVars.dataStride; ++j) {
                // stagger the iterators for each thread
                ++ci[i].dataIter;
                if (ci[i].hasWeights) {
                    ++ci[i].weightsIter;
                }
            }
            ci[i].count = ciCount;
            if (extra > 0) {
                ++ci[i].count;
                --extra;
            }
            if (ci[i].hasMask) {
                for (uInt j=0; j<i*initVars.maskStride; ++j) {
                    ++ci[i].maskIter;
                }
            }
        }
        uInt dataStride = initVars.dataStride*nthreads;
        uInt maskStride = initVars.maskStride*nthreads;
#pragma omp parallel for
        for (uInt i=0; i<nthreads; ++i) {
            if (ci[i].hasWeights) {
                if (ci[i].hasMask) {
                    if (ci[i].hasRanges) {
                        cs[i].addData(
                            ci[i].dataIter, ci[i].weightsIter,
                            ci[i].maskIter, ci[i].count, initVars.ranges,
                            initVars.isIncludeRanges, dataStride, True,
                            maskStride
                        );
                    }
                    else {
                        cs[i].addData(
                            ci[i].dataIter, ci[i].weightsIter,
                            ci[i].maskIter, ci[i].count, dataStride,
                            True, maskStride
                        );
                    }
                }
                else if (ci[i].hasRanges) {
                    cs[i].addData(
                        ci[i].dataIter, ci[i].weightsIter,
                        ci[i].count, initVars.ranges,
                        initVars.isIncludeRanges, dataStride, True
                    );    
                }
                else {
                    // has weights, but no mask nor ranges
                    cs[i].addData(
                        ci[i].dataIter, ci[i].weightsIter,
                        ci[i].count, dataStride, True
                    );
                }
            }
            else if (ci[i].hasMask) {
                // this data set has no weights, but does have a mask
                if (ci[i].hasRanges) {
                    cs[i].addData(
                        ci[i].dataIter, ci[i].maskIter,
                        ci[i].count, initVars.ranges,
                        initVars.isIncludeRanges, dataStride, True,
                        maskStride
                    );
                }
                else {
                    cs[i].addData(
                        ci[i].dataIter, ci[i].maskIter,
                        ci[i].count, dataStride, True, maskStride
                    );
                }
            }
            else if (ci[i].hasRanges) {
                // this data set has no weights no mask, but does have a set of ranges
                // associated with it
                cs[i].addData(
                    ci[i].dataIter, ci[i].count, initVars.ranges,
                    initVars.isIncludeRanges, dataStride, True
                );
            }
            else {
                // simplest case, this data set has no weights, no mask, nor any ranges associated
                // with it, and its stride is 1. No filtering of the data is necessary.
                cs[i].addData(
                    ci[i].dataIter, ci[i].count, dataStride, True
                );
            }
            StatsData<AccumType> sd = cs[i].getStatistics();
            // need to adjust the maxpos and minpos to take into account the
            // staggering and stride
            if (sd.maxpos.first != maxpos[8*i].first) {
                // maxpos changed
                maxpos[8*i].first = sd.maxpos.first;
                maxpos[8*i].second = i*initVars.dataStride + sd.maxpos.second;
                if (updateMaxMin) {
#pragma omp critical
                    {
                        if (*(sd.max) > mymax || initMax) {
                            mymax = *(sd.max);
                            initMax = False;
                            dataProvider->updateMaxPos(maxpos[8*i]);
                        }
                    }
                }
            }
            if (sd.minpos.first != minpos[8*i].first) {
                // minpos changed
                minpos[8*i].first = sd.minpos.first;
                minpos[8*i].second = i*initVars.dataStride + sd.minpos.second;
                if (updateMaxMin) {
#pragma omp critical
                    {
                        if (*(sd.min) < mymin || initMin) {
                            mymin = *(sd.min);
                            initMin = False;
                            dataProvider->updateMinPos(minpos[8*i]);
                        }
                    }
                }
            }
        }
        if (this->_increment(True)) {
            break;
        }
    }
    vector<StatsData<AccumType> > xstats;
    for (uInt i=0; i<_nThreadsMax; ++i) {
        if (cs[i].hasData()) {
            StatsData<AccumType> sd = cs[i].getStatistics();
            sd.maxpos = maxpos[8*i];
            sd.minpos = minpos[8*i];
            xstats.push_back(sd);
        }
    }
    delete [] cs;
    delete [] maxpos;
    delete [] minpos;
    StatsData<AccumType> vstats = StatisticsUtilities<AccumType>::combine(xstats);
    statsData.masked = vstats.masked;
    statsData.max = vstats.max;
    statsData.maxpos = vstats.maxpos;
    statsData.mean = vstats.mean;
    statsData.min = vstats.min;
    statsData.minpos = vstats.minpos;
    statsData.npts = vstats.npts;
    statsData.nvariance = vstats.nvariance;
    statsData.rms = vstats.rms;
    statsData.stddev = vstats.stddev;
    statsData.sum = vstats.sum;
    statsData.sumsq = vstats.sumsq;
    statsData.sumweights = vstats.sumweights;
    statsData.variance = vstats.variance;
    statsData.weighted = vstats.weighted;
    this->_setMustAccumulate(False);
   	return copy(statsData);
}

}

#endif
