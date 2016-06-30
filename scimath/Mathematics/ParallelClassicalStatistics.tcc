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
void ParallelClassicalStatistics<CASA_STATP>::_accumulate2(
    StatsData<AccumType>& stats, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos, const AccumType& datum, Int64 count
) {
    if (this->_getDoMaxMin()) {
        StatisticsUtilities<AccumType>::accumulate(
            stats.npts, stats.sum, stats.mean, stats.nvariance,
            stats.sumsq, mymin, mymax, minpos, maxpos, datum, count
        );
    }
    else {
        StatisticsUtilities<AccumType>::accumulate(
            stats.npts, stats.sum, stats.mean, stats.nvariance,
            stats.sumsq, datum
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_accumulate2(
    StatsData<AccumType>& stats, AccumType& mymin, AccumType& mymax, Int64& minpos, Int64& maxpos,
    const AccumType& datum, const AccumType& weight, Int64 count
) {
    if (this->_getDoMaxMin()) {
        StatisticsUtilities<AccumType>::waccumulate(
            stats.npts, stats.sumweights, stats.sum, stats.mean,
            stats.nvariance, stats.sumsq, mymin, mymax, minpos,
            maxpos, datum, weight, count
        );
    }
    else {
        StatisticsUtilities<AccumType>::waccumulate(
            stats.npts, stats.sumweights, stats.sum, stats.mean,
            stats.nvariance, stats.sumsq, weight, datum
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_unweightedStats2(
    StatsData<AccumType>& stats, uInt64& ngood, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride
) {
    DataIterator datum = dataBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1;
    while (count < nr) {
        _accumulate2(
            stats, mymin, mymax, minpos, maxpos, *datum, count
        );
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, unityStride, dataStride
        );
    }
    ngood = nr;
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_unweightedStats2(
        StatsData<AccumType>& stats, uInt64& ngood, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate2(
                stats, mymin, mymax, minpos, maxpos, *datum, count
            );
            ++ngood;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, unityStride, dataStride
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_unweightedStats2(
        StatsData<AccumType>& stats, uInt64& ngood, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1 && maskStride == 1;
    while (count < nr) {
        if (*mask) {
            _accumulate2(
                stats, mymin, mymax, minpos, maxpos, *datum, count
            );
            ++ngood;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, unityStride, dataStride, maskStride
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_unweightedStats2(
        StatsData<AccumType>& stats, uInt64& ngood, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude

) {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1 && maskStride == 1;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate2(
                stats, mymin, mymax, minpos, maxpos, *datum, count
            );
            ++ngood;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, unityStride, dataStride, maskStride
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_weightedStats2(
        StatsData<AccumType>& stats, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1;
    while (count < nr) {
        if (*weight > 0) {
            _accumulate2(
                stats, mymin, mymax, minpos, maxpos, *datum, *weight, count
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, unityStride, dataStride
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_weightedStats2(
        StatsData<AccumType>& stats, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate2(
                stats, mymin, mymax, minpos, maxpos, *datum, *weight, count
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, unityStride, dataStride
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_weightedStats2(
        StatsData<AccumType>& stats, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1 && maskStride == 1;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate2(
                stats, mymin, mymax, minpos, maxpos, *datum, *weight, count
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, unityStride, dataStride, maskStride
        );
    }
}

CASA_STATD
void ParallelClassicalStatistics<CASA_STATP>::_weightedStats2(
        StatsData<AccumType>& stats, AccumType& mymin, AccumType& mymax,
    Int64& minpos, Int64& maxpos,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    Bool unityStride = dataStride == 1 && maskStride == 1;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _accumulate2(
                stats, mymin, mymax, minpos, maxpos, *datum, *weight, count
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, unityStride, dataStride, maskStride
        );
    }
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
            if (initVars.hasWeights) {
                tStats[idx8].weighted = True;
                if (initVars.hasMask) {
                    tStats[idx8].masked = True;
                    if (initVars.hasRanges) {
                        _weightedStats2(
                            tStats[idx8], mymin[idx8], mymax[idx8],
                            minpos[idx8], maxpos[idx8],
                            ci[i].dataIter, ci[i].weightsIter, ci[i].count,
                            dataStride, ci[i].maskIter, maskStride,
                            initVars.ranges, initVars.isIncludeRanges
                        );
                    }
                    else {
                        _weightedStats2(
                            tStats[idx8], mymin[idx8], mymax[idx8],
                            minpos[idx8], maxpos[idx8],
                            ci[i].dataIter, ci[i].weightsIter, ci[i].count,
                            dataStride, ci[i].maskIter, maskStride
                        );
                    }
                }
                else if (initVars.hasRanges) {
                    _weightedStats2(
                        tStats[idx8], mymin[idx8], mymax[idx8],
                        minpos[idx8], maxpos[idx8], ci[i].dataIter,
                        ci[i].weightsIter, ci[i].count, dataStride,
                        initVars.ranges, initVars.isIncludeRanges
                    );
                }
                else {
                    // has weights, but no mask nor ranges
                    _weightedStats2(
                        tStats[idx8], mymin[idx8], mymax[idx8],
                        minpos[idx8], maxpos[idx8], ci[i].dataIter,
                        ci[i].weightsIter, ci[i].count, dataStride
                    );
                }
            }
            else if (initVars.hasMask) {
                // this data set has no weights, but does have a mask
                tStats[idx8].masked = True;
                if (initVars.hasRanges) {
                    _unweightedStats2(
                        tStats[idx8], ngood[idx8], mymin[idx8], mymax[idx8],
                        minpos[idx8], maxpos[idx8],
                        ci[i].dataIter, ci[i].count,
                        dataStride, ci[i].maskIter, maskStride,
                        initVars.ranges, initVars.isIncludeRanges
                    );
                }
                else {
                    _unweightedStats2(
                        tStats[idx8], ngood[idx8], mymin[idx8], mymax[idx8],
                        minpos[idx8], maxpos[idx8],
                        ci[i].dataIter, ci[i].count,
                        dataStride, ci[i].maskIter, maskStride
                    );
                }
            }
            else if (initVars.hasRanges) {
                // this data set has no weights no mask, but does have a set of ranges
                // associated with it
                _unweightedStats2(
                    tStats[idx8], ngood[idx8], mymin[idx8], mymax[idx8],
                    minpos[idx8], maxpos[idx8],
                    ci[i].dataIter, ci[i].count,
                    dataStride, initVars.ranges, initVars.isIncludeRanges
                );
            }
            else {
                // simplest case, this data set has no weights, no mask, nor any ranges associated
                // with it, and its stride is 1. No filtering of the data is necessary.
                _unweightedStats2(
                    tStats[idx8], ngood[idx8], mymin[idx8], mymax[idx8],
                    minpos[idx8], maxpos[idx8], ci[i].dataIter,
                    ci[i].count, dataStride
                );
            }
            if (! initVars.hasWeights) {
                tStats[idx8].sumweights += ngood[idx8];
            }
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
