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

#ifndef SCIMATH_CLASSICALSTATISTICS_TCC
#define SCIMATH_CLASSICALSTATISTICS_TCC

#include <casacore/scimath/Mathematics/ClassicalStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsIncrementer.h>
#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iomanip>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace casacore {

CASA_STATD
const uInt ClassicalStatistics<CASA_STATP>::CACHE_PADDING = 8;

CASA_STATD
const uInt ClassicalStatistics<CASA_STATP>::BLOCK_SIZE = 2000;

// min > max indicates that these quantities have not be calculated
CASA_STATD
ClassicalStatistics<CASA_STATP>::ClassicalStatistics()
    : StatisticsAlgorithm<CASA_STATP>(),
      _statsData(initializeStatsData<AccumType>()),
      _idataset(0), _calculateAsAdded(False), _doMaxMin(True),
      _doMedAbsDevMed(False), _mustAccumulate(False) {
    reset();
}

CASA_STATD
ClassicalStatistics<CASA_STATP>::~ClassicalStatistics() {}

CASA_STATD
ClassicalStatistics<CASA_STATP>::ClassicalStatistics(
    const ClassicalStatistics<CASA_STATP>& cs
) : StatisticsAlgorithm<CASA_STATP>(cs),
    _statsData(cs._statsData),
    _idataset(cs._idataset),_calculateAsAdded(cs._calculateAsAdded),
    _doMaxMin(cs._doMaxMin), _doMedAbsDevMed(cs._doMedAbsDevMed), _mustAccumulate(cs._mustAccumulate),
    _hasData((cs._hasData)) {
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
    _idataset = other._idataset;
    _calculateAsAdded = other._calculateAsAdded;
    _doMaxMin = other._doMaxMin;
    _doMedAbsDevMed = other._doMedAbsDevMed;
    _mustAccumulate = other._mustAccumulate;
    _hasData = other._hasData;
    return *this;
}

CASA_STATD
AccumType ClassicalStatistics<CASA_STATP>::getMedian(
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    if (! _getStatsData().median.null()) {
        return *_getStatsData().median;
    }
    std::set<uInt64> indices = _medianIndices(knownNpts);
    std::map<uInt64, AccumType> indexToValue = _indicesToValues(
        knownNpts, knownMin, knownMax,
        binningThreshholdSizeBytes/sizeof(AccumType),
        indices, persistSortedArray, nBins
    );
    _getStatsData().median = indexToValue.size() == 1
        ? new AccumType(indexToValue[*indices.begin()])
        : new AccumType(
            (
                indexToValue[*indices.begin()]
                + indexToValue[*indices.rbegin()]
            )/AccumType(2)
        );
    return *_getStatsData().median;
}

CASA_STATD
std::set<uInt64> ClassicalStatistics<CASA_STATP>::_medianIndices(
    CountedPtr<uInt64> knownNpts
) {
    std::set<uInt64> indices;
    uInt64 mynpts = knownNpts.null() ? getNPts() : *knownNpts;
    if (mynpts % 2 == 0) {
        indices.insert(mynpts/2 - 1);
        indices.insert(mynpts/2);
    }
    else {
        indices.insert(mynpts/2);
    }
    return indices;
}

CASA_STATD
AccumType ClassicalStatistics<CASA_STATP>::getMedianAbsDevMed(
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    if (! _getStatsData().medAbsDevMed.null()) {
        return *_getStatsData().medAbsDevMed;
    }

    // This call calculates the _median of the data set which is stored internally and
    // used, but is not necessary to be captured in the return value here.
    getMedian(
        knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
        persistSortedArray, nBins
    );
    std::set<uInt64> indices = _medianIndices(knownNpts);
    // throw the proper switch
    _doMedAbsDevMed = True;
    std::map<uInt64, AccumType> indexToValue = _indicesToValues(
        knownNpts, knownMin, knownMax,
        binningThreshholdSizeBytes/sizeof(AccumType),
        indices, persistSortedArray, nBins
    );
    _doMedAbsDevMed = False;
    _getStatsData().medAbsDevMed = indexToValue.size() == 1
        ? new AccumType(indexToValue[*indices.begin()])
        : new AccumType(
            (
                indexToValue[*indices.begin()]
                + indexToValue[*indices.rbegin()]
            )/AccumType(2)
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
    std::set<uInt64> medianIndices;
    quantiles.clear();
    CountedPtr<uInt64> mynpts = knownNpts.null() ? new uInt64(getNPts()) : knownNpts;
    ThrowIf(
        *mynpts == 0,
        "No valid data found"
    );
    if (_getStatsData().median.null()) {
        medianIndices = _medianIndices(mynpts);
    }
    std::map<Double, uInt64> quantileToIndex = StatisticsData::indicesFromFractions(
        *mynpts, fractions
    );
    std::set<uInt64> indices = medianIndices;
    std::map<Double, uInt64>::const_iterator qToIIter = quantileToIndex.begin();
    std::map<Double, uInt64>::const_iterator qToIEnd = quantileToIndex.end();
    while(qToIIter != qToIEnd) {
        indices.insert(qToIIter->second);
        ++qToIIter;
    }
    std::map<uInt64, AccumType> indexToValue = _indicesToValues(
        mynpts, knownMin, knownMax,
        binningThreshholdSizeBytes/sizeof(AccumType),
        indices, persistSortedArray, nBins
    );
    if (_getStatsData().median.null()) {
        _getStatsData().median = *mynpts % 2 == 0
            ? new AccumType(
                (
                    indexToValue[*medianIndices.begin()]
                    + indexToValue[*medianIndices.rbegin()]
                )/AccumType(2)
            )
            : new AccumType(indexToValue[*medianIndices.begin()]);
    }
    std::set<Double>::const_iterator fIter = fractions.begin();
    std::set<Double>::const_iterator fEnd = fractions.end();
    while (fIter != fEnd) {
        quantiles[*fIter] = indexToValue[quantileToIndex[*fIter]];
        ++fIter;
    }
    return *_getStatsData().median;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::getMinMax(
    AccumType& mymin, AccumType& mymax
) {
    if ( _getStatsData().min.null() || _getStatsData().max.null()) {
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
    const std::set<Double>& fractions, CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    if (fractions.empty()) {
        return std::map<Double, AccumType>();
    }
    ThrowIf(
        _calculateAsAdded,
        "Quantiles cannot be calculated unless all data are available "
        "simultaneously. To ensure that will be the case, call "
        "setCalculateAsAdded(False) on this object"
    );
    ThrowIf(
        *fractions.begin() <= 0 || *fractions.rbegin() >= 1,
        "Value of all quantiles must be between 0 and 1 (noninclusive)"
    );
    uInt64 mynpts = knownNpts.null() ? getNPts() : *knownNpts;
    ThrowIf(mynpts == 0, "No valid data found");
    std::map<Double, uInt64> quantileToIndexMap = StatisticsData::indicesFromFractions(
        mynpts, fractions
    );
    // This seemingly convoluted way of doing things with maps is necessary because
    // multiple quantiles can map to the same sorted array index, and multiple array
    // indices can map the same value if the values in the array are not unique.
    std::map<Double, AccumType> quantileToValue;
    std::set<uInt64> uniqueIndices;
    std::map<Double, uInt64>::const_iterator qToIIter = quantileToIndexMap.begin();
    std::map<Double, uInt64>::const_iterator qToIEnd = quantileToIndexMap.end();
    while(qToIIter != qToIEnd) {
        uniqueIndices.insert(qToIIter->second);
        ++qToIIter;
    }
    std::map<uInt64, AccumType> indexToValue = _indicesToValues(
        knownNpts, knownMin, knownMax,
        binningThreshholdSizeBytes/sizeof(AccumType),
        uniqueIndices, persistSortedArray, nBins
    );
    qToIIter = quantileToIndexMap.begin();
    while (qToIIter != qToIEnd) {
        Double quantile = qToIIter->first;
        uInt64 index = qToIIter->second;
        quantileToValue[quantile] = indexToValue[index];
        ++qToIIter;
    }
    return quantileToValue;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::reset() {
    _clearData();
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::setCalculateAsAdded(
    Bool c
) {
    ThrowIf (
        this->_getDataProvider() && c,
        "Logic Error: It is nonsensical to call " + String(__func__) + " method "
        "with a True value if one is using a data provider"
    );
    ThrowIf(
        _idataset > 0,
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
    _hasData = True;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::setStatsToCalculate(
    std::set<StatisticsData::STATS>& stats
) {
    ThrowIf(
        _calculateAsAdded && _idataset > 0,
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
    this->_setSortedArray(vector<AccumType>());
    _getStatsData().median = NULL;
    _mustAccumulate = True;
    _hasData = True;
    if (_calculateAsAdded) {
        _getStatistics();
        StatisticsAlgorithm<CASA_STATP>::_clearData();
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_clearData() {
    _clearStats();
    StatisticsAlgorithm<CASA_STATP>::_clearData();
    _hasData = False;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_clearStats() {
    _statsData = initializeStatsData<AccumType>();
    _idataset = 0;
    _doMedAbsDevMed = False;
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

CASA_STATD
StatsData<AccumType> ClassicalStatistics<CASA_STATP>::_getStatistics() {
    StatsData<AccumType>& stats = _getStatsData();
    if (! _mustAccumulate) {
        return copy(stats);
    }
    _initIterators();
#ifdef _OPENMP
    uInt nThreadsMax = omp_get_max_threads();
#else
    uInt nThreadsMax = 1;
#endif
    DataIterator *dataIter = new DataIterator[CACHE_PADDING*nThreadsMax];
    MaskIterator *maskIter = new MaskIterator[CACHE_PADDING*nThreadsMax];
    WeightsIterator *weightsIter = new WeightsIterator[CACHE_PADDING*nThreadsMax];
    uInt64 *offset = new uInt64[CACHE_PADDING*nThreadsMax];
    StatsData<AccumType> *tStats = new StatsData<AccumType>[CACHE_PADDING*nThreadsMax];
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = CACHE_PADDING*i;
        tStats[idx8] = _getInitialStats();
        // set nominal max and mins so accumulate
        // doesn't segfault
        tStats[idx8].min = new AccumType(0);
        tStats[idx8].max = new AccumType(0);
    }
    while (True) {
        _initLoopVars();
        uInt nBlocks = _myCount/BLOCK_SIZE;
        uInt extra = _myCount % BLOCK_SIZE;
        if (extra > 0) {
            ++nBlocks;
        }
        uInt nthreads = _myCount == 1
            ? 1 : min(nThreadsMax, nBlocks + 1);
        uInt dataStride = _myStride;
        uInt maskStride = _maskStride;
        if (_hasWeights) {
            stats.weighted = True;
        }
        if (_hasMask) {
            stats.masked = True;
        }
        for (uInt tid=0; tid<nthreads; ++tid) {
            uInt idx8 = CACHE_PADDING*tid;
            dataIter[idx8] = _myData;
            offset[idx8] = tid*BLOCK_SIZE*dataStride;
            std::advance(dataIter[idx8], offset[idx8]);
            if (_hasWeights) {
                weightsIter[idx8] = _myWeights;
                std::advance(weightsIter[idx8], offset[idx8]);
            }
            if (_hasMask) {
                maskIter[idx8] = _myMask;
                std::advance(maskIter[idx8], tid*BLOCK_SIZE*maskStride);
            }
        }
#pragma omp parallel for num_threads(nthreads)
        for (uInt i=0; i<nBlocks; ++i) {
#ifdef _OPENMP
            uInt tid = omp_get_thread_num();
#else
            uInt tid = 0;
#endif
            uInt64 ngood = 0;
            uInt idx8 = CACHE_PADDING*tid;
            uInt dataCount = _myCount - offset[idx8] < BLOCK_SIZE ? extra : BLOCK_SIZE;
            LocationType location(_idataset, offset[idx8]);
            _computeStats(
                tStats[idx8], ngood, location, dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataStride, maskStride, dataCount
            );
            uInt increment = nthreads*BLOCK_SIZE*dataStride;
            std::advance(dataIter[idx8], increment);
            if (_hasWeights) {
                std::advance(weightsIter[idx8], increment);
            }
            if (_hasMask) {
                std::advance(maskIter[idx8], nthreads*BLOCK_SIZE*maskStride);
            }
            offset[idx8] += increment;
        }
        for (uInt tid=0; tid<nthreads; ++tid) {
            uInt idx8 = CACHE_PADDING*tid;
            _updateDataProviderMaxMin(tStats[idx8]);
        }
        if (_increment(True)) {
            break;
        }
    }
    delete [] dataIter;
    delete [] maskIter;
    delete [] weightsIter;
    delete [] offset;
    vector<StatsData<AccumType> > xstats;
    for (uInt i=0; i<nThreadsMax; ++i) {
        // in case no max/min was set, clear the nominal values
        // set above
        StatsData<AccumType>& s = tStats[CACHE_PADDING*i];
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
    delete [] tStats;
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
    DataIterator dataIter, MaskIterator maskIter, WeightsIterator weightsIter,
    uInt dataStride, uInt maskStride, uInt64 count
) {
    if (_hasWeights) {
        stats.weighted = True;
        if (_hasMask) {
            stats.masked = True;
            if (_hasRanges) {
                _weightedStats(
                    stats, location, dataIter, weightsIter, count,
                    dataStride, maskIter, maskStride,
                    _myRanges, _myIsInclude
                );
            }
            else {
                _weightedStats(
                    stats, location, dataIter, weightsIter, count,
                    dataStride, maskIter, maskStride
                );
            }
        }
        else if (_hasRanges) {
            _weightedStats(
                stats, location, dataIter, weightsIter,
                count, dataStride,_myRanges, _myIsInclude
            );
        }
        else {
            // has weights, but no mask nor ranges
            _weightedStats(
                stats, location, dataIter, weightsIter,
                count, dataStride
            );
        }
    }
    else if (_hasMask) {
        // this data set has no weights, but does have a mask
        stats.masked = True;
        if (_hasRanges) {
            _unweightedStats(
                stats, ngood, location, dataIter, count, dataStride,
                maskIter, maskStride, _myRanges, _myIsInclude
            );
        }
        else {
            _unweightedStats(
                stats, ngood, location, dataIter, count,
                dataStride, maskIter, maskStride
            );
        }
    }
    else if (_hasRanges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _unweightedStats(
            stats, ngood, location, dataIter, count,
            dataStride, _myRanges, _myIsInclude
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it, and its stride is 1. No filtering of the data is necessary.
        _unweightedStats(
            stats, ngood, location,
            dataIter, count, dataStride
        );
    }
    if (! _hasWeights) {
        stats.sumweights += ngood;
    }
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_increment(Bool includeIDataset) {
    if (includeIDataset) {
        ++_idataset;
    }
    StatsDataProvider<CASA_STATP> *dataProvider = this->_getDataProvider();
    if (dataProvider) {
        ++(*dataProvider);
        if (dataProvider->atEnd()) {
            dataProvider->finalize();
            return True;
        }
    }
    else {
        ++_diter;
        if (_diter == _dend) {
            return True;
        }
        ++_citer;
        ++_dsiter;
        ++_dataCount;
    }
    return False;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& /*dataBegin*/, Int64 nr, uInt /*dataStride*/
) const {
    npts += nr;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
vector<vector<uInt64> > ClassicalStatistics<CASA_STATP>::_binCounts(
    vector<CountedPtr<AccumType> >& sameVal,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc
) {
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iDesc = bDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eDesc = binDesc.end();
    if (binDesc.size() > 1) {
        typename StatisticsUtilities<AccumType>::BinDesc prevDesc;
        while (iDesc != eDesc) {
            if (iDesc != bDesc) {
                ThrowIf (
                    iDesc->minLimit <= prevDesc.minLimit,
                    "Logic Error: histograms are not monotonically increasing"
                );
            }
            prevDesc = *iDesc;
            ++iDesc;
        }
    }
    vector<Bool> allSame(binDesc.size(), True);
    vector<vector<uInt64> > bins(binDesc.size());
    iDesc = bDesc;
    vector<vector<uInt64> >::iterator bBins = bins.begin();
    vector<vector<uInt64> >::iterator iBins = bBins;
    vector<vector<uInt64> >::iterator eBins = bins.end();
    while (iBins != eBins) {
        *iBins = vector<uInt64>(iDesc->nBins, 0);
        ++iDesc;
        ++iBins;
    }
    sameVal = vector<CountedPtr<AccumType> >(binDesc.size(), NULL);
    vector<AccumType> maxLimit(binDesc.size());
    typename vector<AccumType>::iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::iterator iMaxLimit = bMaxLimit;
    typename vector<AccumType>::iterator eMaxLimit = maxLimit.end();
    iDesc = bDesc;
    while(iMaxLimit != eMaxLimit) {
        *iMaxLimit = iDesc->minLimit + (AccumType)(iDesc->nBins)*(iDesc->binWidth);
        ++iMaxLimit;
        ++iDesc;
    }
    _initIterators();
    while (True) {
        _initLoopVars();
        if (_hasWeights) {
            if (_hasMask) {
                if (_hasRanges) {
                    _findBins(
                        bins, sameVal, allSame, _myData, _myWeights, _myCount,
                        _myStride, _myMask, _maskStride, _myRanges, _myIsInclude,
                        binDesc, maxLimit
                    );
                }
                else {
                    _findBins(
                        bins, sameVal, allSame, _myData, _myWeights,
                        _myCount, _myStride, _myMask, _maskStride,
                        binDesc, maxLimit
                    );
                }
            }
            else if (_hasRanges) {
                _findBins(
                    bins, sameVal, allSame, _myData, _myWeights, _myCount,
                    _myStride, _myRanges, _myIsInclude,
                    binDesc, maxLimit
                );
            }
            else {
                // has weights, but no mask nor ranges
                _findBins(
                    bins, sameVal, allSame, _myData, _myWeights, _myCount, _myStride,
                    binDesc, maxLimit
                );
            }
        }
        else if (_hasMask) {
            // this data set has no weights, but does have a mask
            if (_hasRanges) {
                _findBins(
                    bins, sameVal, allSame, _myData, _myCount, _myStride,
                    _myMask, _maskStride, _myRanges, _myIsInclude,
                    binDesc, maxLimit
                );
            }
            else {
                _findBins(
                    bins, sameVal, allSame, _myData, _myCount, _myStride, _myMask, _maskStride,
                    binDesc, maxLimit
                );
            }
        }
        else if (_hasRanges) {
            // this data set has no weights no mask, but does have a set of ranges
            // associated with it
            _findBins(
                bins, sameVal, allSame, _myData, _myCount, _myStride,
                _myRanges, _myIsInclude,
                binDesc, maxLimit
            );
        }
        else {
            // simplest case, this data set has no weights, no mask, nor any ranges associated
            // with it. No filtering of the data is necessary.
            _findBins(
                bins, sameVal, allSame, _myData, _myCount, _myStride,
                binDesc, maxLimit
            );
        }
        if (_increment(False)) {
            break;
        }
    }
    return bins;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_createDataArray(
    vector<AccumType>& ary
) {
    _initIterators();
    while (True) {
        _initLoopVars();
        if (_hasWeights) {
            if (_hasMask) {
                if (_hasRanges) {
                    _populateArray(
                        ary, _myData, _myWeights, _myCount,
                        _myStride, _myMask, _maskStride, _myRanges, _myIsInclude
                    );
                }
                else {
                    _populateArray(
                        ary, _myData, _myWeights,
                        _myCount, _myStride, _myMask, _maskStride
                    );
                }
            }
            else if (_hasRanges) {
                _populateArray(
                    ary, _myData, _myWeights, _myCount,
                    _myStride, _myRanges, _myIsInclude
                );
            }
            else {
                // has weights, but no mask nor ranges
                _populateArray(
                    ary, _myData, _myWeights, _myCount, _myStride
                );
            }
        }
        else if (_hasMask) {
            // this data set has no weights, but does have a mask
            if (_hasRanges) {
                _populateArray(
                    ary, _myData, _myCount, _myStride,
                    _myMask, _maskStride, _myRanges, _myIsInclude
                );
            }
            else {
                _populateArray(
                    ary, _myData, _myCount, _myStride, _myMask, _maskStride
                );
            }
        }
        else if (_hasRanges) {
            // this data set has no weights no mask, but does have a set of ranges
            // associated with it
            _populateArray(
                ary, _myData, _myCount, _myStride,
                _myRanges, _myIsInclude
            );
        }
        else {
            // simplest case, this data set has no weights, no mask, nor any ranges associated
            // with it, and its stride is 1. No filtering of the data is necessary.
            _populateArray(
                ary, _myData, _myCount, _myStride
            );
        }
        if (_increment(False)) {
            break;
        }
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_createDataArrays(
    vector<vector<AccumType> >& arys, const vector<std::pair<AccumType, AccumType> > &includeLimits,
    uInt maxCount
) {
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iLimits = bLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eLimits = includeLimits.end();
    std::pair<AccumType, AccumType> prevLimits;
    while(iLimits != eLimits) {
        if (iLimits->first >= iLimits->second) {
            ostringstream os;
            os << "Logic Error: bin limits are nonsensical: " << *iLimits;
            ThrowCc(os.str());
        }
        if (iLimits != bLimits) {
            if (
                iLimits->first <= prevLimits.first
                || iLimits->second <= prevLimits.second
            ) {
                ostringstream os;
                os << "Logic Error: bin limits are not in order: " << prevLimits << " , " << *iLimits;
                ThrowCc(os.str());
            }
        }
        prevLimits = *iLimits;
        ++iLimits;
    }
    _initIterators();
    uInt currentCount = 0;
    while (True) {
        _initLoopVars();
        if (_hasWeights) {
            if (_hasMask) {
                if (_hasRanges) {
                    _populateArrays(
                        arys, currentCount, _myData, _myWeights, _myCount,
                        _myStride, _myMask, _maskStride, _myRanges, _myIsInclude,
                        includeLimits, maxCount
                    );
                }
                else {
                    _populateArrays(
                        arys, currentCount, _myData, _myWeights,
                        _myCount, _myStride, _myMask, _maskStride,
                        includeLimits, maxCount
                    );
                }
            }
            else if (_hasRanges) {
                _populateArrays(
                    arys, currentCount, _myData, _myWeights, _myCount,
                    _myStride, _myRanges, _myIsInclude,
                    includeLimits, maxCount
                );
            }
            else {
                // has weights, but no mask nor ranges
                _populateArrays(
                    arys, currentCount, _myData, _myWeights, _myCount, _myStride,
                    includeLimits, maxCount
                );
            }
        }
        else if (_hasMask) {
            // this data set has no weights, but does have a mask
            if (_hasRanges) {
                _populateArrays(
                    arys, currentCount, _myData, _myCount, _myStride,
                    _myMask, _maskStride, _myRanges, _myIsInclude,
                    includeLimits, maxCount
                );
            }
            else {
                _populateArrays(
                    arys, currentCount, _myData, _myCount, _myStride, _myMask, _maskStride,
                    includeLimits, maxCount
                );
            }
        }
        else if (_hasRanges) {
            // this data set has no weights no mask, but does have a set of ranges
            // associated with it
            _populateArrays(
                arys, currentCount, _myData, _myCount, _myStride,
                _myRanges, _myIsInclude, includeLimits, maxCount
            );
        }
        else {
            // simplest case, this data set has no weights, no mask, nor any ranges associated
            // with it, and its stride is 1. No filtering of the data is necessary.
            _populateArrays(
                arys, currentCount, _myData, _myCount, _myStride,
                includeLimits, maxCount
            );
        }
        if (_increment(False)) {
            break;
        }
    }
}

CASA_STATD
vector<std::map<uInt64, AccumType> > ClassicalStatistics<CASA_STATP>::_dataFromMultipleBins(
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, uInt maxArraySize,
    const vector<std::set<uInt64> >& dataIndices, uInt64 nBins
) {
    // dataIndices are relative to minimum bin minimum border
    vector<CountedPtr<AccumType> > sameVal(binDesc.size(), NULL);
    vector<vector<uInt64> > binCounts = _binCounts(sameVal, binDesc);
    vector<std::set<uInt64> >::const_iterator bIdxSet = dataIndices.begin();
    vector<std::set<uInt64> >::const_iterator iIdxSet = bIdxSet;
    vector<std::set<uInt64> >::const_iterator eIdxSet = dataIndices.end();
    typename vector<CountedPtr<AccumType> >::const_iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::const_iterator iSameVal = bSameVal;
    vector<vector<uInt64> >::const_iterator bCountSet = binCounts.begin();
    vector<vector<uInt64> >::const_iterator iCountSet = bCountSet;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iDesc = bDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eDesc = binDesc.end();
    std::map<AccumType, std::map<uInt64, AccumType> > histToIdxValMap;
    vector<uInt64> vnpts;
    vector<std::pair<AccumType, AccumType> > vlimits;
    vector<std::set<uInt64> > vindices;
    vector<std::map<uInt64, uInt64> > vNewToOld;
    // This is necessary for accounting. Map the lower limit of
    // a single bin to the lower limit of its associated histogram
    std::map<AccumType, AccumType> binToHistogramMap;
    while (iIdxSet != eIdxSet) {
        std::set<uInt64>::const_iterator iIdx = iIdxSet->begin();
        std::set<uInt64>::const_iterator eIdx = iIdxSet->end();
        if (iSameVal->null()) {
            // values in this histogram are not all the same
            vector<uInt64>::const_iterator bCounts = iCountSet->begin();
            vector<uInt64>::const_iterator iCounts = bCounts;
            vector<uInt64>::const_iterator eCounts = iCountSet->end();
            uInt64 dataCount = 0;
            uInt64 prevDataCount = 0;
            uInt64 loopCount = 0;
            while (iIdx != eIdx) {
                ThrowIf(iCounts == eCounts, "Logic Error: ran out of bins, accounting error");
                dataCount += *iCounts;
                if (*iIdx < dataCount) {
                    // datum at index exists in current bin
                    std::pair<AccumType, AccumType> binLimits;
                    binLimits.first = iDesc->minLimit + (AccumType)loopCount*(iDesc->binWidth);
                    binLimits.second = binLimits.first + iDesc->binWidth;
                    std::set<uInt64> newDataIndices;
                    std::map<uInt64, uInt64> newToOld;
                    while(iIdx != eIdx && *iIdx < dataCount) {
                        // this loop takes into account that multiple indices
                        // could fall in the same bin
                        uInt64 oldIdx = *iIdx;
                        uInt64 newIdx = oldIdx - prevDataCount;
                        newDataIndices.insert(newIdx);
                        newToOld[newIdx] = oldIdx;
                        ++iIdx;
                    }
                    vNewToOld.push_back(newToOld);
                    vnpts.push_back(*iCounts);
                    vlimits.push_back(binLimits);
                    // because multiple single bins can be in the same histogram,
                    // we need to keep track of which bins belong to which histogram
                    // for accounting below
                    binToHistogramMap[binLimits.first] = iDesc->minLimit;
                    vindices.push_back(newDataIndices);
                }
                prevDataCount = dataCount;
                ++iCounts;
                ++loopCount;
            }
        }
        else {
            // values in this histogram are all the same
            std::map<uInt64, AccumType> mymap;
            while (iIdx != eIdx) {
                mymap[*iIdx] = *(*iSameVal);
                ++iIdx;
            }
            histToIdxValMap[iDesc->minLimit] = mymap;
        }
        ++iIdxSet;
        ++iSameVal;
        ++iCountSet;
        ++iDesc;
    }
    if (! vnpts.empty()) {
        vector<std::map<uInt64, AccumType> > dataFromBins = _dataFromSingleBins(
            vnpts, maxArraySize, vlimits, vindices, nBins
        );
        typename vector<std::map<uInt64, AccumType> >::const_iterator iDataSet = dataFromBins.begin();
        typename vector<std::map<uInt64, AccumType> >::const_iterator eDataSet = dataFromBins.end();
        vector<std::map<uInt64, uInt64> >::iterator iNewToOld = vNewToOld.begin();
        typename vector<std::pair<AccumType, AccumType> >::const_iterator iVLimits = vlimits.begin();
        while(iDataSet != eDataSet) {
            AccumType myHistKey = binToHistogramMap[iVLimits->first];
            std::map<uInt64, AccumType> mymap;
            typename std::map<uInt64, AccumType>::const_iterator iData = iDataSet->begin();
            typename std::map<uInt64, AccumType>::const_iterator eData = iDataSet->end();
            while(iData != eData) {
                uInt64 newIdx = iData->first;
                uInt64 oldIdx = (*iNewToOld)[newIdx];
                mymap[oldIdx] = iData->second;
                ++iData;
            }
            histToIdxValMap[myHistKey].insert(mymap.begin(), mymap.end());
            ++iNewToOld;
            ++iDataSet;
            ++iVLimits;
        }
    }
    vector<std::map<uInt64, AccumType> > ret;
    iDesc = bDesc;
    while (iDesc != eDesc) {
        ret.push_back(histToIdxValMap[iDesc->minLimit]);
        ++iDesc;
    }
    return ret;
}

CASA_STATD
vector<std::map<uInt64, AccumType> > ClassicalStatistics<CASA_STATP>::_dataFromSingleBins(
    const vector<uInt64>& binNpts, uInt maxArraySize,
    const vector<std::pair<AccumType, AccumType> >& binLimits,
    const vector<std::set<uInt64> >& dataIndices, uInt64 nBins
) {
    uInt64 totalPts = 0;
    vector<uInt64>::const_iterator bNpts = binNpts.begin();
    vector<uInt64>::const_iterator iNpts = bNpts;
    vector<uInt64>::const_iterator eNpts = binNpts.end();
    while (iNpts != eNpts) {
        totalPts += *iNpts;
        ++iNpts;
    }
    if (totalPts <= maxArraySize) {
        // contents of bin is small enough to be sorted in memory, so
        // get the bin limits and stuff the good points within those limits
        // in an array and sort it
        vector<vector<AccumType> > dataArrays(binLimits.size(), vector<AccumType>(0));
        _createDataArrays(dataArrays, binLimits, totalPts);
        typename vector<vector<AccumType> >::iterator bArrays = dataArrays.begin();
        typename vector<vector<AccumType> >::iterator iArrays = bArrays;
        typename vector<vector<AccumType> >::iterator eArrays = dataArrays.end();
        iNpts = bNpts;
        while (iArrays != eArrays) {
            ThrowIf(
                iArrays->size() != *iNpts,
                "Logic Error: data array has " + String::toString(iArrays->size())
                + " elements but it should have " + String::toString(*iNpts)
                + ". Please file a bug report and include your dataset and your inputs"
            );
            ++iArrays;
            ++iNpts;
        }
        std::vector<std::set<uInt64> >::const_iterator bIdxSet = dataIndices.begin();
        std::vector<std::set<uInt64> >::const_iterator iIdxSet = bIdxSet;
        std::vector<std::set<uInt64> >::const_iterator eIdxSet = dataIndices.end();
        iNpts = bNpts;
        vector<std::map<uInt64, AccumType> > ret(binLimits.size());
        typename vector<std::map<uInt64, AccumType> >::iterator bRet = ret.begin();
        typename vector<std::map<uInt64, AccumType> >::iterator iRet = bRet;
        // typename vector<std::map<uInt64, AccumType> >::iterator eRet = ret.end();
        iArrays = bArrays;
        while(iIdxSet != eIdxSet) {
            std::set<uInt64>::const_iterator initer = iIdxSet->begin();
            std::set<uInt64>::const_iterator inend = iIdxSet->end();
            uInt prevIdx = 0;
            while (initer != inend) {
                ThrowIf(
                    *initer >= *iNpts,
                    "Logic Error: aryIdx " + String::toString(*initer) + " is too large. "
                    "It should be no larger than " + String::toString(*iNpts-1)
                    + ". Please file a defect report and include your dataset and your inputs"
                );
                (*iRet)[*initer] = GenSort<AccumType>::kthLargest(
                    &((*iArrays)[prevIdx]), *iNpts - prevIdx, *initer - prevIdx
                );
                prevIdx = *initer;
                ++initer;
            }
            ++iIdxSet;
            ++iNpts;
            ++iArrays;
            ++iRet;
        }
        return ret;
    }
    else {
        // bin contents are too large to be sorted in memory, this bin must be sub-binned
        typename vector<std::pair<AccumType, AccumType> >::const_iterator bLimits = binLimits.begin();
        typename vector<std::pair<AccumType, AccumType> >::const_iterator iLimits = bLimits;
        typename vector<std::pair<AccumType, AccumType> >::const_iterator eLimits = binLimits.end();
        vector<typename StatisticsUtilities<AccumType>::BinDesc> binDesc;
        while (iLimits != eLimits) {
            // we want at least 1000 bins
            nBins = max(nBins, 1000);
            typename StatisticsUtilities<AccumType>::BinDesc histogram;
            _makeBins(
                histogram, iLimits->first, iLimits->second,
                nBins, False
            );
            binDesc.push_back(histogram);
            ++iLimits;
        }
        return _dataFromMultipleBins(binDesc, maxArraySize, dataIndices, nBins);
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_convertToAbsDevMedArray(
    vector<AccumType>& myArray, AccumType median
) {
    typename vector<AccumType>::iterator iter = myArray.begin();
    typename vector<AccumType>::iterator end = myArray.end();
    while (iter != end) {
        *iter = abs(*iter - median);
        ++iter;
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_doMinMax(
    AccumType& datamin, AccumType& datamax
) {
    _initIterators();
    CountedPtr<AccumType> mymax;
    CountedPtr<AccumType> mymin;
    while (True) {
        _initLoopVars();
        if (_hasWeights) {
            if (_hasMask) {
                if (_hasRanges) {
                    _minMax(
                        mymin, mymax, _myData, _myWeights, _myCount, _myStride,
                        _myMask, _maskStride, _myRanges, _myIsInclude
                    );
                }
                else {
                    _minMax(
                        mymin, mymax, _myData, _myWeights, _myCount,
                        _myStride, _myMask, _maskStride
                    );
                }
            }
            else if (_hasRanges) {
                _minMax(
                    mymin, mymax, _myData, _myWeights, _myCount,
                    _myStride, _myRanges, _myIsInclude
                );
            }
            else {
                // has weights, but no mask nor ranges
                _minMax(
                    mymin, mymax, _myData, _myWeights, _myCount, _myStride
                );
            }
        }
        else if (_hasMask) {
            // this data set has no weights, but does have a mask
            if (_hasRanges) {
                _minMax(
                    mymin, mymax, _myData, _myCount, _myStride, _myMask,
                    _maskStride, _myRanges, _myIsInclude
                );
            }
            else {
                _minMax(
                    mymin, mymax, _myData, _myCount,
                    _myStride, _myMask, _maskStride
                );
            }
        }
        else if (_hasRanges) {
            // this data set has no weights no mask, but does have a set of ranges
            // associated with it
            _minMax(
                mymin, mymax, _myData, _myCount,
                _myStride, _myRanges, _myIsInclude
            );
        }
        else {
            // simplest case, this data set has no weights, no mask, nor any ranges associated
            // with it. No filtering of the data is necessary.
            _minMax(mymin, mymax, _myData, _myCount, _myStride);
        }
        if (_increment(False)) {
            break;
        }
    }
    ThrowIf (
        mymax.null() || mymin.null(),
        "No valid data found"
    );
    datamin = *mymin;
    datamax = *mymax;
}

CASA_STATD
Int64 ClassicalStatistics<CASA_STATP>::_doNpts() {
    _initIterators();
    uInt64 npts = 0;
    while (True) {
        _initLoopVars();
        if (_hasWeights) {
            if (_hasMask) {
                if (_hasRanges) {
                    _accumNpts(
                        npts, _myData, _myWeights, _myCount, _myStride,
                        _myMask, _maskStride, _myRanges, _myIsInclude
                    );
                }
                else {
                    _accumNpts(
                        npts, _myData, _myWeights, _myCount,
                        _myStride, _myMask, _maskStride
                    );
                }
            }
            else if (_hasRanges) {
                _accumNpts(
                    npts, _myData, _myWeights, _myCount,
                    _myStride, _myRanges, _myIsInclude
                );
            }
            else {
                // has weights, but no mask nor ranges
                _accumNpts(
                    npts, _myData, _myWeights, _myCount, _myStride
                );
            }
        }
        else if (_hasMask) {
            // this data set has no weights, but does have a mask
            if (_hasRanges) {
                _accumNpts(
                    npts, _myData, _myCount, _myStride, _myMask,
                    _maskStride, _myRanges, _myIsInclude
                );
            }
            else {
                _accumNpts(
                    npts, _myData, _myCount,
                    _myStride, _myMask, _maskStride
                );
            }
        }
        else if (_hasRanges) {
            // this data set has no weights no mask, but does have a set of ranges
            // associated with it
            _accumNpts(
                npts, _myData, _myCount,
                _myStride, _myRanges, _myIsInclude
            );
        }
        else {
            // simplest case, this data set has no weights, no mask, nor any ranges associated
            // with it.
            _accumNpts(
                npts, _myData, _myCount, _myStride
            );
        }
        if (_increment(False)) {
            break;
        }
    }
    ThrowIf (npts == 0, "No valid data found");
    return npts;
}

// Tried making this into an inline method, but performance decreased by 20 - 25% when
// finding the median and quartiles on a 200 Mpix image. So the #define seems to be
// the better choice from a performance standpoint.
#define _findBinCode \
    AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *_statsData.median) : *datum; \
    if (myDatum >= bBinDesc->minLimit && myDatum < *maxLimit.rbegin()) { \
        iCounts = bCounts; \
        iSameVal = bSameVal; \
        iAllSame = bAllSame; \
        iBinDesc = bBinDesc; \
        iMaxLimit = bMaxLimit; \
        while (iBinDesc != eBinDesc) { \
            if (myDatum >= iBinDesc->minLimit && myDatum < *iMaxLimit) { \
                AccumType idx = (myDatum - iBinDesc->minLimit)/iBinDesc->binWidth; \
                ++(*iCounts)[StatisticsUtilities<AccumType>::getInt(idx)]; \
                if (*iAllSame) { \
                    if (iSameVal->null()) { \
                        *iSameVal = new AccumType(myDatum); \
                    } \
                    else { \
                        *iAllSame = myDatum == *(*iSameVal); \
                        if (! *iAllSame) { \
                            *iSameVal = NULL; \
                        } \
                    } \
                } \
                break; \
            } \
            ++iCounts; \
            ++iSameVal; \
            ++iAllSame; \
            ++iBinDesc; \
            ++iMaxLimit; \
        } \
    }

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    Int64 count = 0;
    while (count < nr) {
         _findBinCode
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_findBins(
    vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const vector<AccumType>& maxLimit
) const {
    vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
    vector<vector<uInt64> >::iterator iCounts = bCounts;
    typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    vector<Bool>::iterator bAllSame = allSame.begin();
    vector<Bool>::iterator iAllSame = bAllSame;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
std::map<uInt64, AccumType> ClassicalStatistics<CASA_STATP>::_indicesToValues(
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt maxArraySize,
    const std::set<uInt64>& indices, Bool persistSortedArray, uInt64 nBins
) {
    std::map<uInt64, AccumType> indexToValue;
    if (
        _valuesFromSortedArray(
            indexToValue, knownNpts, indices, maxArraySize,
            persistSortedArray
        )
    ) {
        return indexToValue;
    }
    AccumType mymin, mymax;
    if (knownMin.null() || knownMax.null()) {
        getMinMax(mymin, mymax);
    }
    else {
        mymin = *knownMin;
        mymax = *knownMax;
    }
    if (_doMedAbsDevMed) {
        mymax = max(
            abs(mymax - *_getStatsData().median),
            abs(mymin - *_getStatsData().median)
        );
        mymin = AccumType(0);
    }
    if (mymax == mymin) {
        // data set values are all the same
        std::set<uInt64>::const_iterator iter = indices.begin();
        std::set<uInt64>::const_iterator end = indices.end();
        while(iter != end) {
            indexToValue[*iter] = mymin;
            ++iter;
        }
        return indexToValue;
    }
    vector<std::set<uInt64> > vindices(1, indices);
    AccumType pad = 1e-6*(mymax - mymin);
    std::pair<AccumType, AccumType> limits(mymin - pad, mymax + pad);
    vector<std::pair<AccumType, AccumType> > vlimits(1, limits);
    uInt64 mynpts = knownNpts.null() ? getNPts() : *knownNpts;
    vector<uInt64> vmynpts(1, mynpts);
    return _dataFromSingleBins(
        vmynpts, maxArraySize, vlimits, vindices, nBins
    )[0];
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_initIterators() {
    ThrowIf(! _hasData, "No data sets have been added");
    if (this->_getDataProvider()) {
        this->_getDataProvider()->reset();
    }
    else {
        _dataCount = 0;
        const vector<DataIterator>& data = this->_getData();
        _diter = data.begin();
        _dend = data.end();
        const vector<uInt>& dataStrides = this->_getDataStrides();
        _dsiter = dataStrides.begin();
        const vector<Int64>& counts = this->_getCounts();
        _citer = counts.begin();
        _masks = this->_getMasks();
        _weights = this->_getWeights();
        _ranges = this->_getRanges();
        _isIncludeRanges = this->_getIsIncludeRanges();
    }
    _hasRanges = False;
    _myRanges.clear();
    _myIsInclude = False;
    _hasMask = False;
    _hasWeights = False;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_initLoopVars() {
    StatsDataProvider<CASA_STATP> *dataProvider
        = this->_getDataProvider();
    if (dataProvider) {
        _myData = dataProvider->getData();
        _myCount = dataProvider->getCount();
        _myStride = dataProvider->getStride();
        _hasRanges = dataProvider->hasRanges();
        if (_hasRanges) {
            _myRanges = dataProvider->getRanges();
            _myIsInclude = dataProvider->isInclude();
        }
        _hasMask = dataProvider->hasMask();
        if (_hasMask) {
            _myMask = dataProvider->getMask();
            _maskStride = dataProvider->getMaskStride();
        }
        _hasWeights = dataProvider->hasWeights();
        if (_hasWeights) {
            _myWeights = dataProvider->getWeights();
        }
    }
    else {
        _myData = *_diter;
        _myCount = *_citer;
        _myStride = *_dsiter;
        typename std::map<uInt, DataRanges>::const_iterator rangeI = _ranges.find(_dataCount);
        _hasRanges = rangeI != _ranges.end();
        if (_hasRanges) {
            _myRanges = rangeI->second;
            _myIsInclude = _isIncludeRanges.find(_dataCount)->second;
        }
        typename std::map<uInt, MaskIterator>::const_iterator maskI = _masks.find(_dataCount);
        _hasMask = maskI != _masks.end();
        if (_hasMask) {
            _myMask = maskI->second;
            _maskStride = this->_getMaskStrides().find(_dataCount)->second;
        }
        _hasWeights = _weights.find(_dataCount) != _weights.end();
        if (_hasWeights) {
            _myWeights = _weights.find(_dataCount)->second;
        }
    }
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_isNptsSmallerThan(
    vector<AccumType>& unsortedAry, uInt maxArraySize
) {
    _initIterators();
    Bool limitReached = False;
    while (True) {
        _initLoopVars();
        if (_hasWeights) {
            if (_hasMask) {
                if (_hasRanges) {
                    limitReached = _populateTestArray(
                        unsortedAry, _myData, _myWeights, _myCount,
                        _myStride, _myMask, _maskStride, _myRanges, _myIsInclude,
                        maxArraySize
                    );
                }
                else {
                    limitReached = _populateTestArray(
                        unsortedAry, _myData, _myWeights,
                        _myCount, _myStride, _myMask, _maskStride,
                        maxArraySize
                    );
                }
            }
            else if (_hasRanges) {
                limitReached = _populateTestArray(
                    unsortedAry, _myData, _myWeights, _myCount,
                    _myStride, _myRanges, _myIsInclude,
                    maxArraySize
                );
            }
            else {
                // has weights, but no mask nor ranges
                limitReached = _populateTestArray(
                    unsortedAry, _myData, _myWeights,
                    _myCount, _myStride, maxArraySize
                );
            }
        }
        else if (_hasMask) {
            // this data set has no weights, but does have a mask
            if (_hasRanges) {
                limitReached = _populateTestArray(
                    unsortedAry, _myData, _myCount, _myStride,
                    _myMask, _maskStride, _myRanges, _myIsInclude,
                    maxArraySize
                );
            }
            else {
                limitReached = _populateTestArray(
                    unsortedAry, _myData, _myCount, _myStride, _myMask,
                    _maskStride, maxArraySize
                );
            }
        }
        else if (_hasRanges) {
            // this data set has no weights no mask, but does have a set of ranges
            // associated with it
            limitReached = _populateTestArray(
                unsortedAry, _myData, _myCount, _myStride,
                _myRanges, _myIsInclude, maxArraySize
            );
        }
        else {
            // simplest case, this data set has no weights, no mask, nor any ranges associated
            // with it, and its stride is 1. No filtering of the data is necessary.
            limitReached = _populateTestArray(
                unsortedAry, _myData, _myCount, _myStride, maxArraySize
            );
        }
        if (limitReached) {
            unsortedAry.clear();
            return False;
        }
        if (_increment(False)) {
            break;
        }
    }
    _getStatsData().npts = unsortedAry.size();
    return True;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_makeBins(
    typename StatisticsUtilities<AccumType>::BinDesc& bins, AccumType minData, AccumType maxData, uInt maxBins, Bool allowPad
) {

    bins.nBins = maxBins;
    bins.minLimit = minData;
    AccumType maxLimit = maxData;
    if (allowPad) {
        AccumType pad = (maxData - minData)/1e3;
        if (pad == (AccumType)0) {
            // try to handle Int like AccumTypes
            pad = AccumType(1);
        }
        bins.minLimit -= pad;
        maxLimit += pad;
    }
    bins.binWidth = (maxLimit - bins.minLimit)/(AccumType)bins.nBins;
}

#define _minMaxCode \
    if (! mymin.null()) { \
        if (*datum < *mymin) { \
            mymin = new AccumType(*datum); \
        } \
        else if (*datum > *mymax) { \
            mymax = new AccumType(*datum); \
        } \
    } \
    else { \
        mymin = new AccumType(*datum); \
        mymax = new AccumType(*datum); \
    }

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _minMaxCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _populateArrayCode1 \
    AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *_statsData.median) : *datum; \
    ary.push_back(myDatum);

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
    Int64 count = 0;
    DataIterator datum = dataBegin;
    while (count < nr) {
        _populateArrayCode1
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    Int64 count = 0;
    DataIterator datum = dataBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    Int64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    Int64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize performance
// We make use of the fact that bins are in ascending order, so if datum is
// less than current bin minimum value, it will not be in any remaining bins and
// so we can break out of the loop without having to test each bin.
#define _populateArraysCode \
    AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *_statsData.median) : *datum; \
    if (myDatum >= includeLimits.begin()->first && myDatum < includeLimits.rbegin()->second) { \
        iIncludeLimits = bIncludeLimits; \
        iArys = bArys; \
        while (iIncludeLimits != eIncludeLimits) { \
            if (myDatum < iIncludeLimits->first) { \
                break; \
            } \
            if (myDatum < iIncludeLimits->second) { \
                iArys->push_back(myDatum); \
                ++currentCount; \
                if (currentCount == maxCount) { \
                    return; \
                } \
                break; \
            } \
            ++iIncludeLimits; \
            ++iArys; \
        } \
    }


CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    Int64 count = 0;
    DataIterator datum = dataBegin;
    while (count < nr) {
        _populateArraysCode
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    Int64 count = 0;
    DataIterator datum = dataBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    Int64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    Int64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_populateArrays(
    vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
    typename vector<vector<AccumType> >::iterator bArys = arys.begin();
    typename vector<vector<AccumType> >::iterator iArys = bArys;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    uInt maxElements
) const {
    if (ary.size() + nr > maxElements) {
        return True;
    }
    Int64 count = 0;
    DataIterator datum = dataBegin;
    while (count < nr) {
        ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - *_statsData.median) : *datum);
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
    return False;
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _PopulateTestArrayCode \
    ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - *_statsData.median) : *datum); \
    ++npts; \
    if (npts > maxElements) { \
        return True; \
    }

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    uInt maxElements
) const {
    Int64 count = 0;
    uInt npts = ary.size();
    DataIterator datum = dataBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    uInt maxElements
) const {
    Int64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt npts = ary.size();
    while (count < nr) {
        if (*mask) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    Int64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt npts = ary.size();
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
    uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    uInt npts = ary.size();
    while (count < nr) {
        if (*weight > 0) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    uInt npts = ary.size();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    uInt npts = ary.size();
    while (count < nr) {
        if (*mask && *weight > 0) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_populateTestArray(
    vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    uInt npts = ary.size();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_updateDataProviderMaxMin(
    const StatsData<AccumType>& threadStats
) {
    StatsDataProvider<CASA_STATP> *dataProvider
        = this->_getDataProvider();
    if (! dataProvider) {
        return;
    }
    // if there is a data provider, and the max and/or min updated,
    // we have to update the data provider after each data set is
    // processed, because the LatticeStatsDataProvider currently
    // requires that.
    StatsData<AccumType>& stats = _getStatsData();
    Bool same = &threadStats == &stats;
    if (
        _idataset == threadStats.maxpos.first 
        && (stats.max.null() || *threadStats.max > *stats.max)
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
        _idataset == threadStats.minpos.first 
        && (stats.min.null() || (*threadStats.min) < (*stats.min))
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride
) {
    DataIterator datum = dataBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
Bool ClassicalStatistics<CASA_STATP>::_valuesFromSortedArray(
    std::map<uInt64, AccumType>& values, CountedPtr<uInt64> knownNpts,
    const std::set<uInt64>& indices, uInt maxArraySize, Bool persistSortedArray
) {
    values.clear();
    // I need a little wiggle room, the caller can't make the maximum array size
    // ridiculously small
    maxArraySize = max(maxArraySize, (uInt)1000);
    vector<AccumType> myArray;
    if (_doMedAbsDevMed && ! this->_getSortedArray().empty()) {
        // make a copy
        vector<AccumType> pSorted = this->_getSortedArray();
        myArray = pSorted;
        _convertToAbsDevMedArray(myArray, *_getStatsData().median);
    }
    if (! _doMedAbsDevMed) {
        myArray = this->_getSortedArray();
    }
    uInt64 myNpts = _getStatsData().npts > 0
        ? (uInt64)_getStatsData().npts
        : knownNpts.null()
          ? 0 : *knownNpts;
    ThrowIf(myNpts == 0, "No valid data found");
    if (myArray.empty()) {
        if (myNpts > 0) {
            // we have already computed npts
            if (myNpts <= maxArraySize) {
                // npts is smaller than the max array size, so create the array and sort
                // it in memory
                _createDataArray(myArray);
            }
            else {
                // data is too large to be sorted in memory
                return False;
            }
        }
        else {
            // we have to calculate the number of good points
            if (! this->_getDataProvider()) {
                // we first get an upper limit by adding up the counts
                uInt nr = 0;
                const vector<Int64>& counts = this->_getCounts();
                vector<Int64>::const_iterator citer = counts.begin();
                vector<Int64>::const_iterator cend = counts.end();
                while (citer != cend) {
                    nr += *citer;
                    ++citer;
                }
                if (nr <= maxArraySize) {
                    // data can be sorted in memory
                    _createDataArray(myArray);
                }
                else {
                    return False;
                }
            }
            // last resort. scan through the dataset to determine if npts is small enough
            // if it is, myArray will be populated with unsorted data
            if (myArray.empty() && ! _isNptsSmallerThan(myArray, maxArraySize)) {
                return False;
            }
        }
    }
    values = StatisticsAlgorithm<CASA_STATP>::_valuesFromArray(
        myArray, indices
    );
    if (! _doMedAbsDevMed) {
        if (persistSortedArray) {
            this->_setSortedArray(myArray);
        }
        else {
            this->_setSortedArray(vector<AccumType>());
        }
    }
    return True;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
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
