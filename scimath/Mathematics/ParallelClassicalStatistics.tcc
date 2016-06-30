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

#ifndef SCIMATH_PARALLELCLASSICALSTATISTICS_TCC
#define SCIMATH_PARALLELCLASSICALSTATISTICS_TCC

#include <casacore/scimath/Mathematics/ParallelClassicalStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

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
        return copy(statsData);
    }
    this->_initIterators();
    Int64 *maxpos = new Int64[8*_nThreadsMax];
    Int64 *minpos = new Int64[8*_nThreadsMax];
    AccumType *mymax = new AccumType[8*_nThreadsMax];
    AccumType *mymin = new AccumType[8*_nThreadsMax];
    StatsData<AccumType> *tStats = new StatsData<AccumType>[8*_nThreadsMax];
    uInt64 *ngood = new uInt64[8*_nThreadsMax];
    for (uInt i=0; i<_nThreadsMax; ++i) {
        uInt idx8 = 8*i;
        tStats[idx8] = initializeStatsData<AccumType>();
        maxpos[idx8] = -1;
        minpos[idx8] = -1;
        mymin[idx8] = 0;
        mymax[idx8] = 0;
    }
    vector<typename ClassicalStatistics<CASA_STATP>::InitContainer> ci(_nThreadsMax);
    while (True) {
        typename ClassicalStatistics<CASA_STATP>::InitContainer initVars
            = this->_initAndGetLoopVars();
        uInt nthreads = initVars.count == 1
            ? 1 : min(_nThreadsMax, initVars.count/100 + 2);
        uInt extra = initVars.count % nthreads;
        uInt ciCount = initVars.count/nthreads;
        vector<uInt> initialOffset(nthreads);
        for (uInt i=0; i<nthreads; ++i) {
            ngood[8*i] = 0;
            ci[i] = initVars;
            ci[i].count = ciCount;
            if (extra > 0) {
                ++ci[i].count;
                --extra;
            }
            initialOffset[i] = i*initVars.dataStride;
            for (uInt j=0; j<initialOffset[i]; ++j) {
                // stagger the iterators for each thread
                ++ci[i].dataIter;
                if (ci[i].hasWeights) {
                    ++ci[i].weightsIter;
                }
            }
            if (ci[i].hasMask) {
                for (uInt j=0; j<i*initVars.maskStride; ++j) {
                    ++ci[i].maskIter;
                }
            }
        }
        uInt dataStride = initVars.dataStride*nthreads;
        uInt maskStride = initVars.maskStride*nthreads;
        if (initVars.hasWeights) {
            statsData.weighted = True;
        }
        if (initVars.hasMask) {
            statsData.weighted = True;
        }
#pragma omp parallel for
        for (uInt i=0; i<nthreads; ++i) {
            uInt idx8 = 8*i;
            this->_computeStats(
                tStats[idx8], ngood[idx8], mymin[idx8], mymax[idx8],
                minpos[idx8], maxpos[idx8], ci[i].dataIter, ci[i].maskIter,
                ci[i].weightsIter, dataStride, maskStride, ci[i].count
            );
        }
        for (uInt i=0; i<nthreads; ++i) {
            uInt idx8 = 8*i;
            _updateMaxMin2(
                tStats[idx8], mymin[idx8], mymax[idx8], minpos[idx8],
                maxpos[idx8], initialOffset[i], dataStride, this->_getIDataset()
            );
        }
        if (this->_increment(True)) {
            break;
        }
    }
    vector<StatsData<AccumType> > xstats;
    for (uInt i=0; i<_nThreadsMax; ++i) {
        StatsData<AccumType> sd = tStats[8*i];
        xstats.push_back(sd);
    }
    delete [] tStats;
    delete [] ngood;
    delete [] maxpos;
    delete [] minpos;
    delete [] mymax;
    delete [] mymin;
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

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_updateMaxMin2(
    StatsData<AccumType>& threadStats, AccumType mymin,
    AccumType mymax, Int64 minpos, Int64 maxpos, uInt initialOffset,
    uInt dataStride, const Int64& currentDataset
) {
    Bool maxUpdated = False;
    Bool minUpdated = False;
    // update the per thread max/min if necessary
    if (
        maxpos >= 0
        && (threadStats.max.null() || mymax > *threadStats.max)
    ) {
        threadStats.maxpos.first = currentDataset;
        threadStats.maxpos.second = initialOffset + maxpos*dataStride;
        threadStats.max = new AccumType(mymax);
        maxUpdated = True;
    }
    if (
        minpos >= 0
        && (threadStats.min.null() || mymin < *threadStats.min)
    ) {
        threadStats.minpos.first = currentDataset;
        threadStats.minpos.second = initialOffset + minpos*dataStride;
        threadStats.min = new AccumType(mymin);
        minUpdated = True;
    }
    StatsDataProvider<CASA_STATP> *dataProvider
        = this->_getDataProvider();
    if (! dataProvider || (! maxUpdated && ! minUpdated)) {
        // no data provider, or max and min for this thread not updated, just return
        return;
    }
    // if there is a data provider, and the max and/or min updated,
    // we have to update the data provider as we go

    StatsData<AccumType>& stats = this->_getStatsData();
    if (
        maxUpdated && (stats.max.null() || mymax > *stats.max)
    ) {
        stats.maxpos.first = currentDataset;
        stats.maxpos.second = initialOffset + maxpos*dataStride;
        if (dataProvider) {
            dataProvider->updateMaxPos(stats.maxpos);
        }
        stats.max = new AccumType(mymax);
    }
    if (minUpdated && (stats.min.null() || mymin < *stats.min)) {
        stats.minpos.first = currentDataset;
        stats.minpos.second = initialOffset + minpos*dataStride;
        if (dataProvider) {
            dataProvider->updateMinPos(stats.minpos);
        }
        stats.min = new AccumType(mymin);
    }
}

}

#endif
