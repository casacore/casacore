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

#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/OS/OMP.h>
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
    _doMaxMin(cs._doMaxMin), _doMedAbsDevMed(cs._doMedAbsDevMed),
    _mustAccumulate(cs._mustAccumulate) {}

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
void ClassicalStatistics<CASA_STATP>::setCalculateAsAdded(
    Bool c
) {
    ThrowIf (
        this->_getDataset().getDataProvider() && c,
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
    this->_setSortedArray(std::vector<AccumType>());
    _getStatsData().median = NULL;
    _mustAccumulate = True;
    if (_calculateAsAdded) {
        // just need to call it, don't need the return value here
        _getStatistics();
        StatisticsAlgorithm<CASA_STATP>::reset();
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
    _initIterators();
    uInt nThreadsMax = _nThreadsMax();
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
        _initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        _initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
        if (_hasWeights) {
            stats.weighted = True;
        }
        if (_hasMask) {
            stats.masked = True;
        }
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt64 ngood = 0;
            uInt idx8 = _threadIdx();
            uInt64 dataCount = _myCount - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            LocationType location(_idataset, offset[idx8]);
            _computeStats(
                tStats[idx8], ngood, location, dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount
            );
            _incrementThreadIters(
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
        if (_increment(True)) {
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
void ClassicalStatistics<CASA_STATP>::_incrementThreadIters(
    DataIterator& dataIter, MaskIterator& maskIter,
    WeightsIterator& weightsIter, uInt64& offset, uInt nthreads
) const {
    uInt increment = nthreads*ClassicalStatisticsData::BLOCK_SIZE*_myStride;
    if (offset+increment >= _myCount*_myStride) {
        // necessary because in some cases std::advance will segfault
        // if advanced past the end of the data structure
        return;
    }
    std::advance(dataIter, increment);
    if (_hasWeights) {
        std::advance(weightsIter, increment);
    }
    if (_hasMask) {
        std::advance(maskIter, nthreads*ClassicalStatisticsData::BLOCK_SIZE*_maskStride);
    }
    offset += increment;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 count
) {
    if (_hasWeights) {
        stats.weighted = True;
        if (_hasMask) {
            stats.masked = True;
            if (_hasRanges) {
                _weightedStats(
                    stats, location, dataIter, weightsIter, count,
                    _myStride, maskIter, _maskStride,
                    _myRanges, _myIsInclude
                );
            }
            else {
                _weightedStats(
                    stats, location, dataIter, weightsIter, count,
                    _myStride, maskIter, _maskStride
                );
            }
        }
        else if (_hasRanges) {
            _weightedStats(
                stats, location, dataIter, weightsIter,
                count, _myStride,_myRanges, _myIsInclude
            );
        }
        else {
            // has weights, but no mask nor ranges
            _weightedStats(
                stats, location, dataIter, weightsIter,
                count, _myStride
            );
        }
    }
    else if (_hasMask) {
        // this data set has no weights, but does have a mask
        stats.masked = True;
        if (_hasRanges) {
            _unweightedStats(
                stats, ngood, location, dataIter, count, _myStride,
                maskIter, _maskStride, _myRanges, _myIsInclude
            );
        }
        else {
            _unweightedStats(
                stats, ngood, location, dataIter, count,
                _myStride, maskIter, _maskStride
            );
        }
    }
    else if (_hasRanges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _unweightedStats(
            stats, ngood, location, dataIter, count,
            _myStride, _myRanges, _myIsInclude
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it, and its stride is 1. No filtering of the data is necessary.
        _unweightedStats(
            stats, ngood, location,
            dataIter, count, _myStride
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
    StatsDataProvider<CASA_STATP> *dataProvider = this->_getDataset().getDataProvider();
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
    const DataIterator&, Int64 nr, uInt
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
uInt ClassicalStatistics<CASA_STATP>::_nThreadsMax() const {
    uInt nthr = OMP::nMaxThreads();
    if (nthr > 1) {
        const StatsDataProvider<CASA_STATP> *dataProvider = this->_getDataset().getDataProvider();
        if (dataProvider) {
            uInt n = dataProvider->getNMaxThreads();
            if (n > 0) {
                return n;
            }
        }
    }
    return nthr;
}

CASA_STATD
uInt ClassicalStatistics<CASA_STATP>::_threadIdx() const {
#ifdef _OPENMP
    uInt tid = omp_get_thread_num();
#else
    uInt tid = 0;
#endif
    return tid * ClassicalStatisticsData::CACHE_PADDING;
}

CASA_STATD
std::vector<std::vector<uInt64> > ClassicalStatistics<CASA_STATP>::_binCounts(
    std::vector<CountedPtr<AccumType> >& sameVal,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc
) {
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iDesc = bDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eDesc = binDesc.end();
    if (binDesc.size() > 1) {
        // sanity check
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
    std::vector<Bool> allSame(binDesc.size(), True);
    // the elements in the outer vector are histograms. The elements in the inner
    // vector are the bins in the corresponding histograms. The Int64 elements
    // are the number of data points in those bins
    std::vector<std::vector<uInt64> > bins(binDesc.size());
    std::vector<std::vector<uInt64> >::iterator iBins = bins.begin();
    std::vector<std::vector<uInt64> >::iterator eBins = bins.end();
    // initialize all bin counts to 0
    for (iDesc = bDesc; iBins!=eBins; ++iBins, ++iDesc) {
        *iBins = std::vector<uInt64>(iDesc->nBins, 0);
    }
    // same val indicates if all values in a histogram (the vector elements) are the same
    sameVal = std::vector<CountedPtr<AccumType> >(binDesc.size(), NULL);
    // maxLimit are the maximum limits for each histogram. set them here.
    std::vector<AccumType> maxLimit(binDesc.size());
    typename std::vector<AccumType>::iterator iMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::iterator eMaxLimit = maxLimit.end();
    for (iDesc=bDesc; iMaxLimit!=eMaxLimit; ++iMaxLimit, ++iDesc) {
        *iMaxLimit = iDesc->minLimit + (AccumType)(iDesc->nBins)*(iDesc->binWidth);
    }
    _initIterators();
    const uInt nThreadsMax = _nThreadsMax();
    // The PtrHolders hold references to C arrays of length
    // ClassicalStatisticsData::CACHE_PADDING*nThreadsMax.
    // Only every CACHE_PADDING*nth element will be populated
    PtrHolder<std::vector<std::vector<uInt64> > > tBins(
        new std::vector<std::vector<uInt64> >[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<std::vector<CountedPtr<AccumType> > > tSameVal(
        new std::vector<CountedPtr<AccumType> >[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<std::vector<Bool> > tAllSame(
        new std::vector<Bool>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        tBins[idx8] = bins;
        tSameVal[idx8] = sameVal;
        tAllSame[idx8] = allSame;
    }
    while (True) {
        _initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        _initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = _threadIdx();
            uInt64 dataCount = _myCount - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeBins(
                tBins[idx8], tSameVal[idx8], tAllSame[idx8], dataIter[idx8],
                maskIter[idx8], weightsIter[idx8], dataCount, binDesc, maxLimit
            );
            _incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (_increment(False)) {
            break;
        }
    }
    StatisticsUtilities<AccumType>::mergeResults(
        bins, sameVal, allSame, tBins, tSameVal, tAllSame, nThreadsMax
    );
    return bins;
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeBins(
    std::vector<std::vector<uInt64> >& bins, std::vector<CountedPtr<AccumType> >& sameVal,
    std::vector<Bool>& allSame, DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 count,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
    const std::vector<AccumType>& maxLimit
) {
    if (_hasWeights) {
        if (_hasMask) {
            if (_hasRanges) {
                _findBins(
                    bins, sameVal, allSame, dataIter, weightsIter, count,
                    _myStride, maskIter, _maskStride, _myRanges, _myIsInclude,
                    binDesc, maxLimit
                );
            }
            else {
                _findBins(
                    bins, sameVal, allSame, dataIter, weightsIter,
                    count, _myStride, maskIter, _maskStride,
                    binDesc, maxLimit
                );
            }
        }
        else if (_hasRanges) {
            _findBins(
                bins, sameVal, allSame, dataIter, weightsIter, count,
                _myStride, _myRanges, _myIsInclude,
                binDesc, maxLimit
            );
        }
        else {
            // has weights, but no mask nor ranges
            _findBins(
                bins, sameVal, allSame, dataIter, weightsIter, count, _myStride,
                binDesc, maxLimit
            );
        }
    }
    else if (_hasMask) {
        // this data set has no weights, but does have a mask
        if (_hasRanges) {
            _findBins(
                bins, sameVal, allSame, dataIter, count, _myStride,
                maskIter, _maskStride, _myRanges, _myIsInclude,
                binDesc, maxLimit
            );
        }
        else {
            _findBins(
                bins, sameVal, allSame, dataIter, count, _myStride, maskIter, _maskStride,
                binDesc, maxLimit
            );
        }
    }
    else if (_hasRanges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _findBins(
            bins, sameVal, allSame, dataIter, count, _myStride,
            _myRanges, _myIsInclude,
            binDesc, maxLimit
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it. No filtering of the data is necessary.
        _findBins(
            bins, sameVal, allSame, dataIter, count, _myStride,
            binDesc, maxLimit
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_createDataArray(
    std::vector<AccumType>& ary
) {
    _initIterators();
    const uInt nThreadsMax = _nThreadsMax();
    PtrHolder<std::vector<AccumType> > tAry(
        new std::vector<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    while (True) {
        _initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        _initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = _threadIdx();
            uInt64 dataCount = _myCount - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeDataArray(
                tAry[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount
            );
            _incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (_increment(False)) {
            break;
        }
    }
    // merge the per-thread arrays
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        const std::vector<AccumType>& v = tAry[ClassicalStatisticsData::CACHE_PADDING*tid];
        ary.insert(ary.end(), v.begin(), v.end());
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeDataArray(
    std::vector<AccumType>& ary, DataIterator dataIter,
    MaskIterator maskIter, WeightsIterator weightsIter,
    uInt64 dataCount
) {
    if (_hasWeights) {
        if (_hasMask) {
            if (_hasRanges) {
                _populateArray(
                    ary, dataIter, weightsIter, dataCount,
                    _myStride, maskIter, _maskStride, _myRanges, _myIsInclude
                );
            }
            else {
                _populateArray(
                    ary, dataIter, weightsIter,
                    dataCount, _myStride, maskIter, _maskStride
                );
            }
        }
        else if (_hasRanges) {
            _populateArray(
                ary, dataIter, weightsIter, dataCount,
                _myStride, _myRanges, _myIsInclude
            );
        }
        else {
            // has weights, but no mask nor ranges
            _populateArray(
                ary, dataIter, weightsIter, dataCount, _myStride
            );
        }
    }
    else if (_hasMask) {
        // this data set has no weights, but does have a mask
        if (_hasRanges) {
            _populateArray(
                ary, dataIter, dataCount, _myStride,
                maskIter, _maskStride, _myRanges, _myIsInclude
            );
        }
        else {
            _populateArray(
                ary, dataIter, dataCount, _myStride, maskIter, _maskStride
            );
        }
    }
    else if (_hasRanges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _populateArray(
            ary, dataIter, dataCount, _myStride,
            _myRanges, _myIsInclude
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it, and its stride is 1. No filtering of the data is necessary.
        _populateArray(
            ary, dataIter, dataCount, _myStride
        );
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_createDataArrays(
    std::vector<std::vector<AccumType> >& arys,
    const std::vector<std::pair<AccumType, AccumType> >& includeLimits,
    uInt64 maxCount
) {
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iLimits = bLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eLimits = includeLimits.end();
    std::pair<AccumType, AccumType> prevLimits;
    while(iLimits != eLimits) {
        // sanity checks
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
    const uInt nThreadsMax = _nThreadsMax();
    PtrHolder<std::vector<std::vector<AccumType> > > tArys(
        new std::vector<std::vector<AccumType> >[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<uInt64> tCurrentCount(
        new uInt64[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        tArys[idx8] = arys;
    }
    uInt64 currentCount = 0;
    while (currentCount < maxCount) {
        _initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        _initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
        for (uInt tid=0; tid<nThreadsMax; ++tid) {
            uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
            tCurrentCount[idx8] = currentCount;
        }
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = _threadIdx();
            uInt64 dataCount = _myCount - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeDataArrays(
                tArys[idx8], tCurrentCount[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, includeLimits, maxCount
            );
            _incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        // currentCount could be updated inside the threaded loop for finer
        // granularity, but that would require a critical block which
        // might negatively affect performance. Doing it after the main
        // loop seems a reasonable trade off between possibly short
        // circuiting earlier vs performance hits if that does not happen
        uInt64 prevCount = currentCount;
        for (uInt tid=0; tid<nThreadsMax; ++tid) {
            uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
            currentCount += (tCurrentCount[idx8] - prevCount);
        }
        if (_increment(False)) {
            break;
        }
    }
    // CAS-10906 This appears to happen extemely rarely (one report in several years).
    // To the best of my determination, it happens when the data iterator type and
    // the AccumType are not the same (eg Float and Double, respectively), because
    // the data are implicitly cast to AccumType. This slight value change at the data type
    // precision level apparently can, in very rare instances, cause a data point that
    // would fall exactly at the upper edge of a bin (and so under normal circumstances
    // would not be included in that bin), to fall below the upper edge and so be erroneously
    // be included in that bin, leading to an unexpected data point count in that bin. 
    // The perfect solution would probably be not to do the casting and also make the bin limits
    // the same type as the data type, but in practice this requires changes in many, many places
    // in this code, and some of these changes may not be internally consistent. And in the end,
    // I'm not 100% certain this would resolve the issue. The simplest solution for such an
    // infrequently occurring problem is simply to change the binning configuration (which is
    // already pretty arbitrary) so that the bin edges fall elsewhere and try again. This
    // is what is done in _dataFromSingleBins() which catches this exception and then changes
    // the binning configuration and tries again.
    // Smarter binning at the outset might also help, for example, bins now have uniform widths,
    // but most of the datasets we deal with have a pseudo Gaussian distribution. So, this issue
    // is more likely to occur in densely populated bins (and in fact, the one report of this
    // issue was tied to computing the median of such a distribution where the associated bin
    // would have had (nearly) the largest number of points of all other bins. So, a binning
    // configuration where bins are narrower near the distribution center so that the number of
    // points per bin is about constant for all bins might make more sense. But again, that's a
    // significant undertaking, and for a single event every few years, seems over engineering
    // at this point, not to mention that such a configuration is potentially computationally
    // expensive. In the end, this code may, at some very low probability, always be vulnerable
    // to these types of machine precision issues. - dmehring 16nov2017
    ThrowIf(
        currentCount != maxCount, "Accounting error"
    );
    // merge the per-thread arrays
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        typename std::vector<std::vector<AccumType> >::iterator iter = arys.begin();
        typename std::vector<std::vector<AccumType> >::iterator end = arys.end();
        typename std::vector<std::vector<AccumType> >::const_iterator titer = tArys[idx8].begin();
        for (; iter!=end; ++iter, ++titer) {
            iter->insert(iter->end(), titer->begin(), titer->end());
        }
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_computeDataArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 dataCount,
    const std::vector<std::pair<AccumType, AccumType> >& includeLimits,
    uInt64 maxCount
) {
    if (_hasWeights) {
        if (_hasMask) {
            if (_hasRanges) {
                _populateArrays(
                    arys, currentCount, dataIter, weightsIter, dataCount,
                    _myStride, maskIter, _maskStride, _myRanges, _myIsInclude,
                    includeLimits, maxCount
                );
            }
            else {
                _populateArrays(
                    arys, currentCount, dataIter, weightsIter,
                    dataCount, _myStride, maskIter, _maskStride,
                    includeLimits, maxCount
                );
            }
        }
        else if (_hasRanges) {
            _populateArrays(
                arys, currentCount, dataIter, weightsIter, dataCount,
                _myStride, _myRanges, _myIsInclude,
                includeLimits, maxCount
            );
        }
        else {
            // has weights, but no mask nor ranges
            _populateArrays(
                arys, currentCount, dataIter, weightsIter,
                dataCount, _myStride, includeLimits, maxCount
            );
        }
    }
    else if (_hasMask) {
        // this data set has no weights, but does have a mask
        if (_hasRanges) {
            _populateArrays(
                arys, currentCount, dataIter, dataCount, _myStride,
                maskIter, _maskStride, _myRanges, _myIsInclude,
                includeLimits, maxCount
            );
        }
        else {
            _populateArrays(
                arys, currentCount, dataIter, dataCount, _myStride,
                maskIter, _maskStride, includeLimits, maxCount
            );
        }
    }
    else if (_hasRanges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _populateArrays(
            arys, currentCount, dataIter, dataCount, _myStride,
            _myRanges, _myIsInclude, includeLimits, maxCount
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it, and its stride is 1. No filtering of the data is necessary.
        _populateArrays(
            arys, currentCount, dataIter, dataCount,
            _myStride, includeLimits, maxCount
        );
    }
}

CASA_STATD
std::vector<std::map<uInt64, AccumType> > ClassicalStatistics<CASA_STATP>::_dataFromMultipleBins(
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, uInt64 maxArraySize,
    const std::vector<std::set<uInt64> >& dataIndices, uInt64 nBins
) {
    // dataIndices are relative to minimum bin minimum border
    std::vector<CountedPtr<AccumType> > sameVal(binDesc.size(), NULL);
    std::vector<std::vector<uInt64> > binCounts = _binCounts(sameVal, binDesc);
    std::vector<std::set<uInt64> >::const_iterator bIdxSet = dataIndices.begin();
    std::vector<std::set<uInt64> >::const_iterator iIdxSet = bIdxSet;
    std::vector<std::set<uInt64> >::const_iterator eIdxSet = dataIndices.end();
    typename std::vector<CountedPtr<AccumType> >::const_iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::const_iterator iSameVal = bSameVal;
    std::vector<std::vector<uInt64> >::const_iterator bCountSet = binCounts.begin();
    std::vector<std::vector<uInt64> >::const_iterator iCountSet = bCountSet;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iDesc = bDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eDesc = binDesc.end();
    std::map<AccumType, std::map<uInt64, AccumType> > histToIdxValMap;
    std::vector<uInt64> vnpts;
    std::vector<std::pair<AccumType, AccumType> > vlimits;
    std::vector<std::set<uInt64> > vindices;
    std::vector<std::map<uInt64, uInt64> > vNewToOld;
    // This is necessary for accounting. Map the lower limit of
    // a single bin to the lower limit of its associated histogram
    std::map<AccumType, AccumType> binToHistogramMap;
    while (iIdxSet != eIdxSet) {
        std::set<uInt64>::const_iterator iIdx = iIdxSet->begin();
        std::set<uInt64>::const_iterator eIdx = iIdxSet->end();
        if (iSameVal->null()) {
            // values in this histogram are not all the same
            std::vector<uInt64>::const_iterator bCounts = iCountSet->begin();
            std::vector<uInt64>::const_iterator iCounts = bCounts;
            std::vector<uInt64>::const_iterator eCounts = iCountSet->end();
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
        std::vector<std::map<uInt64, AccumType> > dataFromBins = _dataFromSingleBins(
            vnpts, maxArraySize, vlimits, vindices, nBins
        );
        typename std::vector<std::map<uInt64, AccumType> >::const_iterator iDataSet = dataFromBins.begin();
        typename std::vector<std::map<uInt64, AccumType> >::const_iterator eDataSet = dataFromBins.end();
        std::vector<std::map<uInt64, uInt64> >::iterator iNewToOld = vNewToOld.begin();
        typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iVLimits = vlimits.begin();
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
    std::vector<std::map<uInt64, AccumType> > ret;
    iDesc = bDesc;
    while (iDesc != eDesc) {
        ret.push_back(histToIdxValMap[iDesc->minLimit]);
        ++iDesc;
    }
    return ret;
}

CASA_STATD
std::vector<std::map<uInt64, AccumType> > ClassicalStatistics<CASA_STATP>::_dataFromSingleBins(
    const std::vector<uInt64>& binNpts, uInt64 maxArraySize,
    const std::vector<std::pair<AccumType, AccumType> >& binLimits,
    const std::vector<std::set<uInt64> >& dataIndices, uInt64 nBins
) {
    uInt64 totalPts = std::accumulate(binNpts.begin(), binNpts.end(), 0);
    if (totalPts <= maxArraySize) {
        // contents of bin is small enough to be sorted in memory, so
        // get the bin limits and stuff the good points within those limits
        // in an array and sort it
        std::vector<std::vector<AccumType> > dataArrays(binLimits.size(), std::vector<AccumType>(0));
        _createDataArrays(dataArrays, binLimits, totalPts);
        std::vector<uInt64>::const_iterator bNpts = binNpts.begin();
        std::vector<uInt64>::const_iterator iNpts = bNpts;
        typename std::vector<std::vector<AccumType> >::iterator bArrays = dataArrays.begin();
        typename std::vector<std::vector<AccumType> >::iterator iArrays = bArrays;
        typename std::vector<std::vector<AccumType> >::iterator eArrays = dataArrays.end();
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
        std::vector<std::map<uInt64, AccumType> > ret(binLimits.size());
        typename std::vector<std::map<uInt64, AccumType> >::iterator bRet = ret.begin();
        typename std::vector<std::map<uInt64, AccumType> >::iterator iRet = bRet;
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
        static uInt maxLoopCount = 5;
        uInt loopCount = 0;
        // we want at least 1000 bins
        nBins = max(nBins, (uInt64)1000);
        while (True) {
            // bin contents are too large to be sorted in memory, this bin must be sub-binned
            typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bLimits = binLimits.begin();
            typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iLimits = bLimits;
            typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eLimits = binLimits.end();
            std::vector<typename StatisticsUtilities<AccumType>::BinDesc> binDesc;
            while (iLimits != eLimits) {
                typename StatisticsUtilities<AccumType>::BinDesc histogram;
                StatisticsUtilities<AccumType>::makeBins(
                    histogram, iLimits->first, iLimits->second,
                    nBins, False
                );
                binDesc.push_back(histogram);
                ++iLimits;
            }
            try {
                return _dataFromMultipleBins(binDesc, maxArraySize, dataIndices, nBins);
            }
            catch (const AipsError& x) {
                // reconfigure bins and try again. This happens very rarely.
                // See comment in _createDataArrays() at the source of the issue 
                ThrowIf(loopCount == maxLoopCount, "Tried 5 times, giving up");
                // increase nBins by an irrational multiplier slightly greater than 1
                uInt64 nBinsPrev = nBins;
                nBins *= (C::pi/3.0);
                LogIO log;
                log << LogIO::WARN << "Accounting error, changing number of bins from "
                    << nBinsPrev << " to " << nBins << " and recomputing." << LogIO::POST;
                ++loopCount;
            }
        }
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_doMinMax(
    AccumType& datamin, AccumType& datamax
) {
    _initIterators();
    const uInt nThreadsMax = _nThreadsMax();
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
        _initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        _initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = _threadIdx();
            uInt64 dataCount = _myCount - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeMinMax(
                tmax[idx8], tmin[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount
            );
            _incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (_increment(False)) {
            break;
        }
    }
    CountedPtr<AccumType> mymax;
    CountedPtr<AccumType> mymin;
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        if (! tmin[idx8].null()) {
            if (mymin.null() || *tmin[idx8] < *mymin) {
                mymin = tmin[idx8];
            }
        }
        if (! tmax[idx8].null()) {
            if (mymax.null() || *tmax[idx8] > *mymax) {
                mymax = tmax[idx8];
            }
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
void ClassicalStatistics<CASA_STATP>::_computeMinMax(
    CountedPtr<AccumType>& mymax, CountedPtr<AccumType>& mymin,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 dataCount
) {
    if (_hasWeights) {
        if (_hasMask) {
            if (_hasRanges) {
                _minMax(
                    mymin, mymax, dataIter, weightsIter, dataCount, _myStride,
                    maskIter, _maskStride, _myRanges, _myIsInclude
                );
            }
            else {
                _minMax(
                    mymin, mymax, dataIter, weightsIter, dataCount,
                    _myStride, maskIter, _maskStride
                );
            }
        }
        else if (_hasRanges) {
            _minMax(
                mymin, mymax, dataIter, weightsIter, dataCount,
                _myStride, _myRanges, _myIsInclude
            );
        }
        else {
            // has weights, but no mask nor ranges
            _minMax(
                mymin, mymax, dataIter, weightsIter, dataCount, _myStride
            );
        }
    }
    else if (_hasMask) {
        // this data set has no weights, but does have a mask
        if (_hasRanges) {
            _minMax(
                mymin, mymax, dataIter, dataCount, _myStride, maskIter,
                _maskStride, _myRanges, _myIsInclude
            );
        }
        else {
            _minMax(
                mymin, mymax, dataIter, dataCount,
                _myStride, maskIter, _maskStride
            );
        }
    }
    else if (_hasRanges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _minMax(
            mymin, mymax, dataIter, dataCount,
            _myStride, _myRanges, _myIsInclude
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it. No filtering of the data is necessary.
        _minMax(mymin, mymax, dataIter, dataCount, _myStride);
    }
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
    typename std::vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
    CountedPtr<AccumType> knownMax, uInt64 maxArraySize,
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
        ThrowIf(
            *knownMax < *knownMin,
            "Provided max " + String::toString(*knownMax)
            + " is less than provided min " + String::toString(*knownMin)
        );
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
    std::vector<std::set<uInt64> > vindices(1, indices);
    AccumType pad = 1e-6*(mymax - mymin);
    std::pair<AccumType, AccumType> limits(mymin - pad, mymax + pad);
    std::vector<std::pair<AccumType, AccumType> > vlimits(1, limits);
    uInt64 mynpts = knownNpts.null() ? getNPts() : *knownNpts;
    std::vector<uInt64> vmynpts(1, mynpts);
    return _dataFromSingleBins(
        vmynpts, maxArraySize, vlimits, vindices, nBins
    )[0];
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_initIterators() {
    ThrowIf(this->_getDataset().empty(), "No data sets have been added");
    StatsDataProvider<CASA_STATP> *dataProvider = this->_getDataset().getDataProvider();
    if (dataProvider) {
        dataProvider->reset();
    }
    else {
        _dataCount = 0;
        const std::vector<DataIterator>& data = this->_getDataset().getData();
        _diter = data.begin();
        _dend = data.end();
        const std::vector<uInt>& dataStrides = this->_getDataset().getDataStrides();
        _dsiter = dataStrides.begin();
        const std::vector<Int64>& counts = this->_getDataset().getCounts();
        _citer = counts.begin();
        _masks = this->_getDataset().getMasks();
        _weights = this->_getDataset().getWeights();
        _ranges = this->_getDataset().getRanges();
        _isIncludeRanges = this->_getDataset().getIsIncludeRanges();
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
        = this->_getDataset().getDataProvider();
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
            _maskStride = this->_getDataset().getMaskStrides().find(_dataCount)->second;
        }
        _hasWeights = _weights.find(_dataCount) != _weights.end();
        if (_hasWeights) {
            _myWeights = _weights.find(_dataCount)->second;
        }
    }
}

CASA_STATD
void ClassicalStatistics<CASA_STATP>::_initThreadVars(
    uInt& nBlocks, uInt64& extra, uInt& nthreads, PtrHolder<DataIterator>& dataIter,
    PtrHolder<MaskIterator>& maskIter, PtrHolder<WeightsIterator>& weightsIter,
    PtrHolder<uInt64>& offset, uInt nThreadsMax
) const {
    uInt n = ClassicalStatisticsData::CACHE_PADDING*nThreadsMax;
    dataIter.set(new DataIterator[n], True);
    maskIter.set(new MaskIterator[n], True);
    weightsIter.set(new WeightsIterator[n], True);
    offset.set(new uInt64[n], True);
    nBlocks = _myCount/ClassicalStatisticsData::BLOCK_SIZE;
    extra = _myCount % ClassicalStatisticsData::BLOCK_SIZE;
    if (extra > 0) {
        ++nBlocks;
    }
    nthreads = min(nThreadsMax, nBlocks);
    for (uInt tid=0; tid<nthreads; ++tid) {
        // advance the per-thread iterators to their correct starting
        // locations
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        dataIter[idx8] = _myData;
        offset[idx8] = tid*ClassicalStatisticsData::BLOCK_SIZE*_myStride;
        std::advance(dataIter[idx8], offset[idx8]);
        if (_hasWeights) {
            weightsIter[idx8] = _myWeights;
            std::advance(weightsIter[idx8], offset[idx8]);
        }
        if (_hasMask) {
            maskIter[idx8] = _myMask;
            std::advance(maskIter[idx8], tid*ClassicalStatisticsData::BLOCK_SIZE*_maskStride);
        }
    }
}

CASA_STATD
Bool ClassicalStatistics<CASA_STATP>::_isNptsSmallerThan(
    std::vector<AccumType>& unsortedAry, uInt maxArraySize
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

#define _minMaxCode \
    if (! mymin.null()) { \
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
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
    StatsDataProvider<CASA_STATP> *dataProvider = this->_getDataset().getDataProvider();
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
        _idataset == threadStats.minpos.first 
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
    const std::set<uInt64>& indices, uInt64 maxArraySize, Bool persistSortedArray
) {
    values.clear();
    // I need a little wiggle room, the caller can't make the maximum array size
    // ridiculously small
    maxArraySize = max(maxArraySize, (uInt64)1000);
    std::vector<AccumType> myArray;
    if (_doMedAbsDevMed && ! this->_getSortedArray().empty()) {
        // make a copy
        std::vector<AccumType> pSorted = this->_getSortedArray();
        myArray = pSorted;
        StatisticsUtilities<AccumType>::convertToAbsDevMedArray(
            myArray, *_getStatsData().median
        );
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
            if (! this->_getDataset().getDataProvider()) {
                // we first get an upper limit by adding up the counts
                const std::vector<Int64>& counts = this->_getDataset().getCounts();
                uInt64 nr = accumulate(counts.begin(), counts.end(), 0);
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
    values = StatisticsUtilities<AccumType>::indicesToValues(
        myArray, indices
    );
    if (! _doMedAbsDevMed) {
        if (persistSortedArray) {
            this->_setSortedArray(myArray);
        }
        else {
            this->_setSortedArray(std::vector<AccumType>());
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
