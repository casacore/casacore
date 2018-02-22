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

#ifndef SCIMATH_CLASSICALSTATISTICS_TCC
#define SCIMATH_CLASSICALSTATISTICS_TCC

#include <casacore/scimath/StatsFramework/ClassicalStatistics.h>

#include <casacore/scimath/StatsFramework/ClassicalStatisticsData.h>
#include <casacore/scimath/StatsFramework/StatisticsIncrementer.h>
#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>
#include <casacore/casa/Utilities/PtrHolder.h>

#include <iomanip>

namespace casacore {

// min > max indicates that these quantities have not be calculated
CASA_STATD
ClassicalStatistics<CASA_STATP>::ClassicalStatistics()
    : StatisticsAlgorithm<CASA_STATP>(),
      _statsData(initializeStatsData<AccumType>()),
      _calculateAsAdded(False), _doMaxMin(True),
      _mustAccumulate(False),
      _qComputer(
          new ClassicalQuantileComputer<CASA_STATP>(
              &this->_getDataset()
          )
      ) {
    reset();
}

CASA_STATD
ClassicalStatistics<CASA_STATP>::ClassicalStatistics(
    CountedPtr<ClassicalQuantileComputer<CASA_STATP> > qc
) : StatisticsAlgorithm<CASA_STATP>(),
      _statsData(initializeStatsData<AccumType>()),
      _calculateAsAdded(False), _doMaxMin(True),
      _mustAccumulate(False), _qComputer(qc) {
    reset();
}

CASA_STATD
ClassicalStatistics<CASA_STATP>::~ClassicalStatistics() {}

CASA_STATD
ClassicalStatistics<CASA_STATP>::ClassicalStatistics(
    const ClassicalStatistics<CASA_STATP>& cs
) : StatisticsAlgorithm<CASA_STATP>(cs),
    _statsData(cs._statsData), _calculateAsAdded(cs._calculateAsAdded),
    _doMaxMin(cs._doMaxMin), _mustAccumulate(cs._mustAccumulate),
    _qComputer(
        (ClassicalQuantileComputer<CASA_STATP>*)(
            cs._qComputer->clone()
        )
    ) {
    _qComputer->setDataset(&this->_getDataset());
}

CASA_STATD
ClassicalStatistics<CASA_STATP>&
ClassicalStatistics<CASA_STATP>::operator=(
    const ClassicalStatistics<CASA_STATP>& other
) {
    if (this == &other) {
        return *this;
    }
    StatisticsAlgorithm<CASA_STATP>::operator=(other);
    _statsData = copy(_statsData);
    _calculateAsAdded = other._calculateAsAdded;
    _doMaxMin = other._doMaxMin;
    _mustAccumulate = other._mustAccumulate;
    _qComputer.reset(
        (ClassicalQuantileComputer<CASA_STATP>*)(
            other._qComputer->clone()
        )
    );
    // setting the dataset in the quantile calculator must be done explicitly
    _qComputer->setDataset(&this->_getDataset());
    return *this;
}

CASA_STATD
StatisticsAlgorithm<CASA_STATP>* ClassicalStatistics<CASA_STATP>::clone() const {
    return new ClassicalStatistics<CASA_STATP>(*this);
}

CASA_STATD
AccumType ClassicalStatistics<CASA_STATP>::getMedian(
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    if (_getStatsData().median) {
        return *_getStatsData().median;
    }
    uInt64 mynpts;
    AccumType mymin, mymax;
    _doNptsMinMax(
        mynpts, mymin, mymax, knownNpts, knownMin, knownMax
    );
    _getStatsData().median = new AccumType(
        _qComputer->getMedian(
            mynpts, mymin, mymax, binningThreshholdSizeBytes,
            persistSortedArray, nBins
        )
    );
    return *_getStatsData().median;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_doNptsMinMax(
    uInt64& mynpts, AccumType& mymin, AccumType& mymax,
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax
) {
    if (knownMin && knownMax) {
        ThrowIf(
            *knownMax < *knownMin,
            "Provided max " + String::toString(*knownMax)
        + " is less than provided min " + String::toString(*knownMin)
        );
        mymin = *knownMin;
        mymax = *knownMax;
    }
    if (knownNpts) {
        mynpts = *knownNpts;
        ThrowIf(mynpts == 0, "No valid data found");
    }
    if ((! knownMin || ! knownMax) && ! knownNpts) {
        getMinMaxNpts(mynpts, mymin, mymax);
    }
    else if (! knownMin || ! knownMax) {
        getMinMax(mymin, mymax);
    }
    else if (! knownNpts) {
        mynpts = getNPts();
        ThrowIf(mynpts == 0, "No valid data found");
    }
}

CASA_STATD
AccumType ClassicalStatistics<CASA_STATP>::getMedianAbsDevMed(
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    if (_getStatsData().medAbsDevMed) {
        return *_getStatsData().medAbsDevMed;
    }
    uInt64 mynpts;
    AccumType mymin, mymax;
    _doNptsMinMax(
        mynpts, mymin, mymax, knownNpts, knownMin, knownMax
    );
    _getStatsData().medAbsDevMed = new AccumType(
        _qComputer->getMedianAbsDevMed(
            mynpts, mymin,
            mymax, binningThreshholdSizeBytes,
            persistSortedArray, nBins
        )
    );
    return *_getStatsData().medAbsDevMed;
}

CASA_STATD
AccumType ClassicalStatistics<CASA_STATP>::getMedianAndQuantiles(
    std::map<Double, AccumType>& quantiles, const std::set<Double>& fractions,
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    uInt64 mynpts;
    AccumType mymin, mymax;
    _doNptsMinMax(
        mynpts, mymin, mymax, knownNpts, knownMin, knownMax
    );
    _getStatsData().median = new AccumType(
        _qComputer->getMedianAndQuantiles(
            quantiles, fractions, mynpts, mymin, mymax, binningThreshholdSizeBytes,
            persistSortedArray, nBins
        )
    );
    return *_getStatsData().median;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::getMinMax(
    AccumType& mymin, AccumType& mymax
) {
    if (! _getStatsData().min || ! _getStatsData().max) {
        ThrowIf(
            _calculateAsAdded,
            "Min and max cannot be calculated unless all data are available "
            "simultaneously. To ensure that will be the case, call "
            "setCalculateAsAdded(False) on this object"
        );
        _doMinMax(mymin, mymax);
        _getStatsData().min = new AccumType(mymin);
        _getStatsData().max = new AccumType(mymax);
        return;
    }
    mymin = *_getStatsData().min;
    mymax = *_getStatsData().max;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::getMinMaxNpts(
    uInt64& npts, AccumType& mymin, AccumType& mymax
) {
    if (! _getStatsData().min || ! _getStatsData().max) {
        ThrowIf(
            _calculateAsAdded,
            "Min and max cannot be calculated unless all data are available "
            "simultaneously. To ensure that will be the case, call "
            "setCalculateAsAdded(False) on this object"
        );
        if (_getStatsData().npts == 0) {
            ThrowIf(
                _calculateAsAdded,
                "npts cannot be calculated unless all data are available "
                "simultaneously. To ensure that will be the case, call "
                "setCalculateAsAdded(False) on this object"
            );
            _getStatsData().npts = _doMinMaxNpts(mymin, mymax);
            _getStatsData().min = new AccumType(mymin);
            _getStatsData().max = new AccumType(mymax);
        }
        else {
            // this will update _getStatsData().min and _getStatsData().max
            getMinMax(mymin, mymax);
        }
    }
    else if (_getStatsData().npts == 0) {
        // we don't need to capture the return value as npts is set outside
        // the block because this call sets _getStatsData().npts
        getNPts();
    }
    mymin = *_getStatsData().min;
    mymax = *_getStatsData().max;
    npts = (uInt64)_getStatsData().npts;
}

CASA_STATD
uInt64 ClassicalStatistics<CASA_STATP>::getNPts() {
    if (_getStatsData().npts == 0) {
        ThrowIf(
            _calculateAsAdded,
            "npts cannot be calculated unless all data are available "
            "simultaneously. To ensure that will be the case, call "
            "setCalculateAsAdded(False) on this object"
        );
        _getStatsData().npts = _doNpts();
    }
    return (uInt64)_getStatsData().npts;
}

CASA_STATD
std::map<Double, AccumType> ClassicalStatistics<CASA_STATP>::getQuantiles(
    const std::set<Double>& fractions, CountedPtr<uInt64> knownNpts,
    CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt64 nBins
) {
    ThrowIf(
        _calculateAsAdded,
        "Quantiles cannot be calculated unless all data are available "
        "simultaneously. To ensure that will be the case, call "
        "setCalculateAsAdded(False) on this object"
    );
    uInt64 mynpts;
    AccumType mymin, mymax;
    _doNptsMinMax(
        mynpts, mymin, mymax, knownNpts, knownMin, knownMax
    );
    return _qComputer->getQuantiles(
        fractions, mynpts, mymin, mymax, binningThreshholdSizeBytes,
        persistSortedArray, nBins
    );
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::setCalculateAsAdded(
    Bool c
) {
    const StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ThrowIf (
        ds.getDataProvider() && c,
        "Logic Error: It is nonsensical to call " + String(__func__) + " method "
        "with a True value if one is using a data provider"
    );
    ThrowIf(
        ds.iDataset() > 0,
        "Logic Error: " + String(__func__)
        + " cannot be called after the first dataset has been set"
    );
    _calculateAsAdded = c;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::setDataProvider(
    StatsDataProvider<CASA_STATP> *dataProvider
) {
    ThrowIf(
        _calculateAsAdded,
        "Logic Error: setCalculateAsAdded(True) has previously been called, "
        "in which case it is nonsensical to use a data provider. Please call "
        "setCalculateAsAdded(False), and then set the data provider"
    );
    StatisticsAlgorithm<CASA_STATP>::setDataProvider(dataProvider);
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::setStatsToCalculate(
    std::set<StatisticsData::STATS>& stats
) {
    const StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ThrowIf(
        _calculateAsAdded && ds.iDataset() > 0,
        "Cannot set stats to be calculated after setting the first dataset when "
        "stats are to be calculated as data are added"
    );
    _doMaxMin = stats.empty()
        || stats.find(StatisticsData::MAX) != stats.end()
        || stats.find(StatisticsData::MIN) != stats.end();
    StatisticsAlgorithm<CASA_STATP>::setStatsToCalculate(stats);
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_addData() {
    _qComputer->_setSortedArray(std::vector<AccumType>());
    _getStatsData().median = NULL;
    _mustAccumulate = True;
    if (_calculateAsAdded) {
        // just need to call it, don't need the return value here
        _getStatistics();
        StatisticsAlgorithm<CASA_STATP>::reset();
        _qComputer->reset();
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::reset() {
    _clearStats();
    StatisticsAlgorithm<CASA_STATP>::reset();
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_clearStats() {
    _statsData = initializeStatsData<AccumType>();
    this->_getDataset().resetIDataset();
    _qComputer->reset();
    _mustAccumulate = True;
}

CASA_STATD
std::pair<Int64, Int64> ClassicalStatistics<CASA_STATP>::getStatisticIndex(
    StatisticsData::STATS stat
) {
    ThrowIf(
        ! (stat == StatisticsData::MAX || stat == StatisticsData::MIN),
        "Index only available for max and min"
    );
    ThrowIf(
        ! _doMaxMin,
        "You must specify to calculate the max "
        "and/or min if you want this index"
    );
    std::set<StatisticsData::STATS> stats = this->_getStatsToCalculate();
    ThrowIf(
        ! stats.empty()
        && (
            (
                stat == StatisticsData::MAX
                && stats.find(StatisticsData::MAX) == stats.end()
            )
            || (
                stat == StatisticsData::MIN
                && stats.find(StatisticsData::MIN) == stats.end()
            )
        ),
        "You did not request to compute this statistic"
    );
    // this call will calculate maxpos and minpos
    _getStatistics();
    if (stat == StatisticsData::MAX) {
        return _getStatsData().maxpos;
    }
    else if (stat == StatisticsData::MIN) {
        return _getStatsData().minpos;
    }
    else {
        ThrowCc(
            "Logic Error: This branch should never be "
            "executed. Please file a defect report."
        );
    }
}

CASA_STATD
StatsData<AccumType> ClassicalStatistics<CASA_STATP>::_getInitialStats() const {
    static const StatsData<AccumType> stats = initializeStatsData<AccumType>();
    return stats;
}

CASA_STATD
AccumType ClassicalStatistics<CASA_STATP>::_getStatistic(
    StatisticsData::STATS stat
) {
    switch (stat) {
        case StatisticsData::MEDIAN:
            return this->getMedian();
        case StatisticsData::MEDABSDEVMED:
            return this->getMedianAbsDevMed();
        case StatisticsData::FIRST_QUARTILE:
            {
                std::set<Double> f;
                f.insert(0.25);
                return this->getQuantiles(f)[0.25];
            }
        case StatisticsData::THIRD_QUARTILE:
            {
                std::set<Double> f;
                f.insert(0.75);
                return this->getQuantiles(f)[0.75];
            }
        case StatisticsData::INNER_QUARTILE_RANGE:
            {
                std::set<Double> f;
                f.insert(0.25);
                f.insert(0.75);
                std::map<Double, AccumType> qs = this->getQuantiles(f);
                return qs[0.75] - qs[0.25];
            }
        default:
            AccumType value;
            Record r = toRecord(_getStatistics());
            String statString = StatisticsData::toString(stat);
            ThrowIf(
                ! r.isDefined(statString),
                "Logic Error: stat " + statString + " is not defined. "
                "Please file a defect report"
            );
            r.get(statString, value);
            return value;
    }
}

CASA_STATD
StatsData<AccumType> ClassicalStatistics<CASA_STATP>::_getStatistics() {
    StatsData<AccumType>& stats = _getStatsData();
    if (! _mustAccumulate) {
        return copy(stats);
    }
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ds.initIterators();
    uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds.getDataProvider()
    );
    PtrHolder<StatsData<AccumType> > tStats(
        new StatsData<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*i;
        tStats[idx8] = _getInitialStats();
        // set nominal max and mins so accumulate
        // doesn't segfault
        tStats[idx8].min = new AccumType(0);
        tStats[idx8].max = new AccumType(0);
    }
    while (True) {
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk = ds.initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        ds.initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
        if (chunk.weights) {
            stats.weighted = True;
        }
        if (chunk.mask) {
            stats.masked = True;
        }
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt64 ngood = 0;
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            LocationType location(ds.iDataset(), offset[idx8]);
            _computeStats(
                tStats[idx8], ngood, location, dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, chunk
            );
            ds.incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        for (uInt tid=0; tid<nthreads; ++tid) {
            // LattStatsDataProvider relies on min and max
            // being updated after each increment of the data provider
            uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
            _updateDataProviderMaxMin(tStats[idx8]);
        }
        if (ds.increment(True)) {
            break;
        }
    }
    std::vector<StatsData<AccumType> > xstats;
    for (uInt i=0; i<nThreadsMax; ++i) {
        // in case no max/min was set, clear the nominal values
        // set above
        StatsData<AccumType>& s 
            = tStats[ClassicalStatisticsData::CACHE_PADDING*i];
        if (s.minpos.first < 0) {
            s.min.reset();
        }
        if (s.maxpos.first < 0) {
            s.max.reset();
        }
        if(s.npts > 0) {
            xstats.push_back(s);
        }
    }
    if (stats.npts > 0) {
        // we've accumulated some stats previously so we must
        // account for that here
        xstats.push_back(stats);
    }
    StatsData<AccumType> vstats = StatisticsUtilities<AccumType>::combine(xstats);
    stats.masked = vstats.masked;
    stats.max = vstats.max;
    stats.maxpos = vstats.maxpos;
    stats.mean = vstats.mean;
    stats.min = vstats.min;
    stats.minpos = vstats.minpos;
    stats.npts = vstats.npts;
    stats.nvariance = vstats.nvariance;
    stats.rms = vstats.rms;
    stats.stddev = vstats.stddev;
    stats.sum = vstats.sum;
    stats.sumsq = vstats.sumsq;
    stats.sumweights = vstats.sumweights;
    stats.variance = vstats.variance;
    stats.weighted = vstats.weighted;
    _mustAccumulate = False;
    return copy(stats);
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 count,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        stats.weighted = True;
        if (chunk.mask) {
            stats.masked = True;
            if (chunk.ranges) {
                _weightedStats(
                    stats, location, dataIter, weightsIter, count,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _weightedStats(
                    stats, location, dataIter, weightsIter, count,
                    chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _weightedStats(
                stats, location, dataIter, weightsIter,
                count, chunk.dataStride, chunk.ranges->first,
                chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _weightedStats(
                stats, location, dataIter, weightsIter,
                count, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        stats.masked = True;
        if (chunk.ranges) {
            _unweightedStats(
                stats, ngood, location, dataIter, count, chunk.dataStride,
                maskIter, chunk.mask->second, chunk.ranges->first,
                chunk.ranges->second
            );
        }
        else {
            _unweightedStats(
                stats, ngood, location, dataIter, count,
                chunk.dataStride, maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _unweightedStats(
            stats, ngood, location, dataIter, count,
            chunk.dataStride, chunk.ranges->first,
            chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it, and its stride is 1. No filtering of the data is necessary.
        _unweightedStats(
            stats, ngood, location, dataIter,
            count, chunk.dataStride
        );
    }
    if (! chunk.weights) {
        stats.sumweights += ngood;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator&, uInt64 nr, uInt
) const {
    npts += nr;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumulate(
    StatsData<AccumType>& stats, const AccumType& datum, const LocationType& location
) {
    if (_doMaxMin) {
        StatisticsUtilities<AccumType>::accumulate (
            stats.npts, stats.sum, stats.mean, stats.nvariance,
            stats.sumsq, *stats.min, *stats.max, stats.minpos,
            stats.maxpos, datum, location
        );
    }
    else {
        StatisticsUtilities<AccumType>::accumulate (
            stats.npts, stats.sum, stats.mean, stats.nvariance,
            stats.sumsq, datum
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumulate(
    StatsData<AccumType>& stats, const AccumType& datum,
    const AccumType& weight, const LocationType& location
) {
    if (_doMaxMin) {
        StatisticsUtilities<AccumType>::waccumulate (
            stats.npts, stats.sumweights, stats.sum, stats.mean,
            stats.nvariance, stats.sumsq, *stats.min, *stats.max,
            stats.minpos, stats.maxpos, datum, weight, location
        );
    }
    else {
        StatisticsUtilities<AccumType>::waccumulate (
            stats.npts, stats.sumweights, stats.sum, stats.mean,
            stats.nvariance, stats.sumsq, weight, datum
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_doMinMax(
    AccumType& datamin, AccumType& datamax
) {
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ds.initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(ds.getDataProvider());
    PtrHolder<CountedPtr<AccumType> > tmin(
        new CountedPtr<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<CountedPtr<AccumType> > tmax(
        new CountedPtr<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    while (True) {
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk = ds.initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        ds.initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeMinMax(
                tmax[idx8], tmin[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, chunk
            );
            ds.incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds.increment(False)) {
            break;
        }
    }
    CountedPtr<AccumType> mymax;
    CountedPtr<AccumType> mymin;
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        if (tmin[idx8]) {
            if (! mymin || *tmin[idx8] < *mymin) {
                mymin = tmin[idx8];
            }
        }
        if (tmax[idx8]) {
            if (! mymax || *tmax[idx8] > *mymax) {
                mymax = tmax[idx8];
            }
        }
    }
    ThrowIf (
        ! mymax || ! mymin,
        "No valid data found"
    );
    datamin = *mymin;
    datamax = *mymax;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeMinMax(
    CountedPtr<AccumType>& mymax, CountedPtr<AccumType>& mymin,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 dataCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _minMax(
                    mymin, mymax, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _minMax(
                    mymin, mymax, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _minMax(
                mymin, mymax, dataIter, weightsIter, dataCount,
                chunk.dataStride, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _minMax(
                mymin, mymax, dataIter, weightsIter,
                dataCount, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _minMax(
                mymin, mymax, dataIter, dataCount,
                chunk.dataStride, maskIter, chunk.mask->second,
                chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            _minMax(
                mymin, mymax, dataIter, dataCount,
                chunk.dataStride, maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _minMax(
            mymin, mymax, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it. No filtering of the data is necessary.
        _minMax(mymin, mymax, dataIter, dataCount, chunk.dataStride);
    }
}

CASA_STATD
uInt64 ClassicalStatistics<CASA_STATP>::_doMinMaxNpts(
    AccumType& datamin, AccumType& datamax
) {
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ds.initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(ds.getDataProvider());
    PtrHolder<CountedPtr<AccumType> > tmin(
        new CountedPtr<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<CountedPtr<AccumType> > tmax(
        new CountedPtr<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<uInt64> npts(
        new uInt64[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        npts[idx8] = 0;
    }
    while (True) {
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk = ds.initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        ds.initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeMinMaxNpts(
                npts[idx8], tmax[idx8], tmin[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, chunk
            );
            ds.incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds.increment(False)) {
            break;
        }
    }
    CountedPtr<AccumType> mymax;
    CountedPtr<AccumType> mymin;
    uInt64 myNpts = 0;
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        if (tmin[idx8]) {
            if (! mymin || *tmin[idx8] < *mymin) {
                mymin = tmin[idx8];
            }
        }
        if (tmax[idx8]) {
            if (! mymax || *tmax[idx8] > *mymax) {
                mymax = tmax[idx8];
            }
        }
        myNpts += npts[idx8];
    }
    ThrowIf (
        ! mymax || ! mymin || myNpts == 0,
        "No valid data found"
    );
    datamin = *mymin;
    datamax = *mymax;
    return myNpts;
}


CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeMinMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymax, CountedPtr<AccumType>& mymin,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 dataCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _minMaxNpts(
                    npts, mymin, mymax, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _minMaxNpts(
                    npts, mymin, mymax, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _minMaxNpts(
                npts, mymin, mymax, dataIter, weightsIter, dataCount,
                chunk.dataStride, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _minMaxNpts(
                npts, mymin, mymax, dataIter, weightsIter,
                dataCount, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _minMaxNpts(
                npts, mymin, mymax, dataIter, dataCount,
                chunk.dataStride, maskIter, chunk.mask->second,
                chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            _minMaxNpts(
                npts, mymin, mymax, dataIter, dataCount,
                chunk.dataStride, maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _minMaxNpts(
            npts, mymin, mymax, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it. No filtering of the data is necessary.
        _minMaxNpts(npts, mymin, mymax, dataIter, dataCount, chunk.dataStride);
    }
}

CASA_STATD
uInt64 ClassicalStatistics<CASA_STATP>::_doNpts() {
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ds.initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds.getDataProvider()
    );
    PtrHolder<uInt64> npts(
        new uInt64[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        npts[idx8] = 0;
    }
    while (True) {
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk = ds.initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        ds.initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeNpts(
                npts[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, chunk
            );
            ds.incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds.increment(False)) {
            break;
        }
    }
    uInt64 myNpts = 0;
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        myNpts += npts[idx8];
    }
    ThrowIf (myNpts == 0, "No valid data found");
    return myNpts;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeNpts(
    uInt64& npts, DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 dataCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _accumNpts(
                    npts, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _accumNpts(
                    npts, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _accumNpts(
                npts, dataIter, weightsIter, dataCount, chunk.dataStride,
                chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _accumNpts(
                npts, dataIter, weightsIter, dataCount, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _accumNpts(
                npts, dataIter, dataCount, chunk.dataStride, maskIter,
                chunk.mask->second, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            _accumNpts(
                npts, dataIter, dataCount, chunk.dataStride,
                maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _accumNpts(
            npts, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it.
        _accumNpts(npts, dataIter, dataCount, chunk.dataStride);
    }
}

#define _minMaxCode \
    if (mymin) { \
        if (*datum < *mymin) { \
            *mymin = *datum; \
        } \
        else if (*datum > *mymax) { \
            *mymax = *datum; \
        } \
    } \
    else { \
        mymin = new AccumType(*datum); \
        mymax = new AccumType(*datum); \
    }

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _minMaxCode
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// same as _minMaxCode with the addition of npts accumulation
#define _minMaxNptsCode \
    _minMaxCode \
    ++npts;

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    npts += nr;
    while (count < nr) {
        // yes we really want _minMaxCode not _minMaxNptsCode here because
        // npts is easy addition in this case because of no data filtering
        _minMaxCode
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxNptsCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _minMaxNptsCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxNptsCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _minMaxNptsCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxNptsCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _minMaxNptsCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _minMaxNptsCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_updateDataProviderMaxMin(
    const StatsData<AccumType>& threadStats
) {
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    StatsDataProvider<CASA_STATP> *dataProvider = ds.getDataProvider();
    if (! dataProvider) {
        return;
    }
    // if there is a data provider, and the max and/or min updated,
    // we have to update the data provider after each data set is
    // processed, because the LatticeStatsDataProvider currently
    // requires that.
    StatsData<AccumType>& stats = _getStatsData();
    Bool same = &threadStats == &stats;
    const Int64 idataset = ds.iDataset();
    if (
        idataset == threadStats.maxpos.first
        && (! stats.max || *threadStats.max > *stats.max)
    ) {
        if (! same) {
            // make certain to make a copy, do not assign
            // one pointer to another
            stats.maxpos = threadStats.maxpos;
            stats.max = new AccumType(*threadStats.max);
        }
        dataProvider->updateMaxPos(stats.maxpos);
    }
    if (
        idataset == threadStats.minpos.first
        && (! stats.min || (*threadStats.min) < (*stats.min))
    ) {
        if (! same) {
            // make certain to make a copy of the value, do not assign
            // one pointer to another
            stats.minpos = threadStats.minpos;
            stats.min = new AccumType(*threadStats.min);
        }
        dataProvider->updateMinPos(stats.minpos);
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _accumulate(
            stats, *datum, location
        );
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
        location.second += dataStride;
    }
    ngood = nr;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate(
                stats, *datum, location
            );
            ++ngood;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _accumulate(
                stats, *datum, location
            );
            ++ngood;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate(
                stats, *datum, location
            );
            ++ngood;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _accumulate(
                stats, *datum, *weight, location
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate(
                stats, *datum, *weight, location
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _accumulate(
                stats, *datum, *weight, location
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _accumulate(
                stats, *datum, *weight, location
            );
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

}

#endif
