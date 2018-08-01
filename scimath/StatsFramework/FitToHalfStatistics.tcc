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

#ifndef SCIMATH_FITTOHALFSTATISTICS_TCC
#define SCIMATH_FITTOHALFSTATISTICS_TCC

#include <casacore/scimath/StatsFramework/FitToHalfStatistics.h>

#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>

#include <iomanip>

namespace casacore {

CASA_STATD
const AccumType FitToHalfStatistics<CASA_STATP>::TWO = AccumType(2);

// min > max indicates that these quantities have not be calculated
CASA_STATD
FitToHalfStatistics<CASA_STATP>::FitToHalfStatistics(
    FitToHalfStatisticsData::CENTER centerType,
    FitToHalfStatisticsData::USE_DATA useData,
    AccumType centerValue
) : ConstrainedRangeStatistics<CASA_STATP>(
        CountedPtr<ConstrainedRangeQuantileComputer<CASA_STATP> >(
            new ConstrainedRangeQuantileComputer<CASA_STATP>(
                &this->_getDataset())
            )
        ),
      _centerType(centerType),
      _useLower(useData == FitToHalfStatisticsData::LE_CENTER),
      _centerValue(centerValue),
      _statsData(initializeStatsData<AccumType>()) {
    reset();
}

CASA_STATD
FitToHalfStatistics<CASA_STATP>::FitToHalfStatistics(
    const FitToHalfStatistics<CASA_STATP>& other
) : ConstrainedRangeStatistics<CASA_STATP>(other),
    _centerType(other._centerType), _useLower(other._useLower),
    _centerValue(other._centerValue), _statsData(copy(other._statsData)),
    _doMedAbsDevMed(other._doMedAbsDevMed), _rangeIsSet(other._rangeIsSet),
    _realMax(other._realMax.null() ? NULL : new AccumType(*other._realMax)),
    _realMin(other._realMin.null() ? NULL : new AccumType(*other._realMin)),
    _isNullSet(False), _range(other._range) {}

CASA_STATD
FitToHalfStatistics<CASA_STATP>::~FitToHalfStatistics() {}

CASA_STATD
FitToHalfStatistics<CASA_STATP>&
FitToHalfStatistics<CASA_STATP>::operator=(
    const FitToHalfStatistics<CASA_STATP>& other
) {
    if (this == &other) {
        return *this;
    }
    ConstrainedRangeStatistics<CASA_STATP>::operator=(other);
    _centerType = other._centerType;
    _useLower = other._useLower;
    _centerValue = other._centerValue;
    _statsData = copy(other._statsData);
    _doMedAbsDevMed = other._doMedAbsDevMed;
    _rangeIsSet = other._rangeIsSet;
    _realMax = other._realMax.null() ? NULL : new AccumType(*other._realMax);
    _realMin = other._realMin.null() ? NULL : new AccumType(*other._realMin);
    _isNullSet = other._isNullSet;
    _range = other._range;
    return *this;
}

CASA_STATD
StatisticsAlgorithm<CASA_STATP>*
FitToHalfStatistics<CASA_STATP>::clone() const {
    return new FitToHalfStatistics<CASA_STATP>(*this);
}

CASA_STATD
AccumType FitToHalfStatistics<CASA_STATP>::getMedian(
    CountedPtr<uInt64> , CountedPtr<AccumType> ,
    CountedPtr<AccumType> , uInt , Bool , uInt
) {
    CountedPtr<AccumType> median = _getStatsData().median;
    if (! median) {
        median = new AccumType(_centerValue);
        _getStatsData().median = median;
        this->_getQuantileComputer()->setMedian(median);
    }
    return *median;
}

CASA_STATD
AccumType FitToHalfStatistics<CASA_STATP>::getMedianAndQuantiles(
    std::map<Double, AccumType>& quantileToValue,
    const std::set<Double>& quantiles, CountedPtr<uInt64> knownNpts,
    CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
) {
    // The median is trivial, we just need to compute the quantiles
    quantileToValue = getQuantiles(
        quantiles, knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
        persistSortedArray, nBins
    );
    return getMedian();
}

CASA_STATD
AccumType FitToHalfStatistics<CASA_STATP>::getMedianAbsDevMed(
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt nBins
) {
    if (! _getStatsData().medAbsDevMed) {
        _setRange();
        ThrowIf(
            _isNullSet,
            "No data included using current configuration, "
            "cannot compute medianabsdevmed"
        );
        // The number of points to hand to the base class is the number of real
        // data points, or exactly half of the total number of points
        CountedPtr<uInt64> realNPts = knownNpts.null()
            ? new uInt64(getNPts()/2) : new uInt64(*knownNpts/2);
        CountedPtr<AccumType> realMin, realMax;
        // need to set the median in the quantile computer object here. The
        // getMedian() call will do that, so we don't need to capture the return
        // value
        getMedian();
        _getStatsData().medAbsDevMed = new AccumType(
            ConstrainedRangeStatistics<CASA_STATP>::getMedianAbsDevMed(
                realNPts, knownMin, knownMax, binningThreshholdSizeBytes,
                persistSortedArray, nBins
            )
        );
    }
    return *_getStatsData().medAbsDevMed;
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_getMinMax(
        CountedPtr<AccumType>& realMin, CountedPtr<AccumType>& realMax,
    CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax
) {
    realMin = new AccumType(_centerValue);
    realMax = new AccumType(_centerValue);
    AccumType mymin, mymax;
    if (knownMin.null() || knownMax.null()) {
        getMinMax(mymin, mymax);
    }
    else {
        mymin = *knownMin;
        mymax = *knownMax;
    }
    if (_useLower) {
        realMin = new AccumType(mymin);
    }
    else {
        realMax = new AccumType(mymax);
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::getMinMax(
    AccumType& mymin, AccumType& mymax
) {
    // do not do a _realMin/Max existence check in the if condition, because if
    // _getStatsData().min() and .max are not null, forcing this recalculation
    // will likely give bogus results because _getStatsData().min/max are used
    // higher up the inheritence chain and will now be set to the full
    // (real + virtual) dataset min/max, not the real portion only min/max
    if ( ! _getStatsData().min || ! _getStatsData().max) {
        _setRange();
        ThrowIf(
            _isNullSet,
            "No data included using current configuration, "
            "cannot compute min and max"
        );
        // This call returns the min and max of the real portion of the dataset
        ConstrainedRangeStatistics<CASA_STATP>::getMinMax(mymin, mymax);
        // note that _realMin and _realMax are also computed during the
        // calculation of accumulated statistics, in
        // _updateDataProviderMaxMin(). if those have been done previously, this
        // if block won't be entered so they will not be computed again here
        _realMin = new AccumType(mymin);
        _realMax = new AccumType(mymax);
        if (_useLower) {
            mymax = TWO*_centerValue - mymin;
        }
        else {
            mymin = TWO*_centerValue - mymax;
        }
        _getStatsData().min = new AccumType(mymin);
        _getStatsData().max = new AccumType(mymax);
    }
    else {
        mymin = *_getStatsData().min;
        mymax = *_getStatsData().max;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_getRealMinMax(
    AccumType& realMin, AccumType& realMax
) {
    // if they exist, just return copies of them
    if (! _realMin || ! _realMax) {
        // real portion min/max not yet computed, they should be computed in
        // getMinMax()
        AccumType mymin, mymax;
        getMinMax(mymin, mymax);
        // should always be OK, but just to be sure, check
        ThrowIf(
            ! _realMin || ! _realMax,
            "Logic Error: _realMin/_realMax not computed as they should have "
            "been, please file a bug report which includes a pointer to the "
            "dataset you used and your complete inputs"
        );
    }
    // return copies
    realMin = *_realMin;
    realMax = *_realMax;
}

CASA_STATD
std::map<Double, AccumType> FitToHalfStatistics<CASA_STATP>::getQuantiles(
    const std::set<Double>& fractions, CountedPtr<uInt64> knownNpts,
    CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
) {
    ThrowIf(
        *fractions.begin() <= 0 || *fractions.rbegin() >= 1,
        "Value of all quantiles must be between 0 and 1 (noninclusive)"
    );
    ThrowIf (
        ! knownNpts.null() && *knownNpts % 2 != 0,
        "knownNpts must be even for this class"
    );
    _setRange();
    ThrowIf(
        _isNullSet,
        "No data included using current configuration, cannot compute quantiles"
    );
    // fractions that exist in the virtual part of the dataset are determined
    // from the real fractions reflected about the center point.
    std::set<Double> realPortionFractions;
    //auto fiter = fractions.cbegin();
    //auto fend = fractions.cend();
    // map the actual (full dataset) fractions to the real portion fractions
    std::map<Double, Double> actualToReal;
    Double freal = 0;
    std::map<Double, AccumType> actual;
    //for ( ; fiter != fend; ++fiter) {
    for_each(
        fractions.cbegin(), fractions.cend(),
        [this, &actual, &knownNpts, &freal, &realPortionFractions, &actualToReal]
         (Double q) {
        if (near(q, 0.5)) {
            AccumType realMin, realMax;
            _getRealMinMax(realMin, realMax);
            actual[q] = _useLower ? realMax : TWO*_centerValue - realMin;
        }
        else {
            auto isVirtualQ = (_useLower && q > 0.5)
                || (! _useLower && q < 0.5);
            if (isVirtualQ) {
                // quantile is in virtual part of data set
                std::set<Double> actualF;
                actualF.insert(q);
                uInt64 allNPts = knownNpts.null() ? getNPts() : *knownNpts;
                auto actualFToI = StatisticsData::indicesFromFractions(
                    allNPts, actualF
                );
                auto actualIdx = actualFToI[q];
                auto realIdx = _useLower
                    ? allNPts - (actualIdx + 1) : allNPts/2 - (actualIdx + 1);
                if (_useLower && (realIdx == allNPts/2 - 1)) {
                    // the actual index is the reflection of the maximum
                    // value of the real portion of the dataset
                    AccumType realMin, realMax;
                    _getRealMinMax(realMin, realMax);
                    actual[q] = TWO*_centerValue - realMax;
                }
                else if (! _useLower && realIdx == 0) {
                    // the actual index is the reflection of the minimum
                    // value of the real portion of the dataset
                    AccumType realMin, realMax;
                    _getRealMinMax(realMin, realMax);
                    actual[q] = TWO*_centerValue - realMin;
                }
                else {
                    freal = Double(realIdx + 1)/Double(allNPts/2);
                    if (freal == 1) {
                        AccumType mymin, mymax;
                        getMinMax(mymin, mymax);
                        actual[q] = mymin;
                    }
                    else {
                        realPortionFractions.insert(freal);
                        actualToReal[q] = freal;
                    }
                }
            }
            else {
                // quantile is in the real part of the dataset
                freal = _useLower ? 2*q : 2*(q - 0.5);
                realPortionFractions.insert(freal);
                actualToReal[q] = freal;
            }
        }
    });
    if (realPortionFractions.empty()) {
        return actual;
    }
    // if given, knownNpts should be the number of points in the full dataset,
    // or twice the number in the real portion of the dataset. Points in only
    // the real portion is what scanning will find, so we need to cut the number
    // of points in half. This is also true if we have to compute using
    // getNPts(), so we need our own value to pass in to the call of the base
    // class' method.
    CountedPtr<uInt64> realNPts = knownNpts.null()
        ? new uInt64(getNPts()/2) : new uInt64(*knownNpts/2);
    CountedPtr<AccumType> realMin, realMax;
    _getMinMax(realMin, realMax, knownMin, knownMax);
    auto realPart = ConstrainedRangeStatistics<CASA_STATP>::getQuantiles(
        realPortionFractions, realNPts, realMin, realMax,
        binningThreshholdSizeBytes, persistSortedArray, nBins
    );
    // fiter = fractions.begin();
    // while (fiter != fend) {
    for_each(
        fractions.cbegin(), fractions.cend(),
        [this, &actual, &actualToReal, &realPart](Double q) {
        if (actual.find(q) == actual.end()) {
            Double realF = actualToReal[q];
            auto actualValue = realPart[realF];
            if ((_useLower && q > 0.5) || (! _useLower && q < 0.5)) {
                // quantile in virtual part of the data set, reflect
                // corresponding real value to get actual value
                actualValue = TWO*_centerValue - actualValue;
            }
            actual[q] = actualValue;
        }
       // ++fiter;
    });
    return actual;
}

CASA_STATD
uInt64 FitToHalfStatistics<CASA_STATP>::getNPts() {
    if (_getStatsData().npts == 0) {
        _setRange();
        if (_isNullSet) {
            return 0;
        }
        // guard against subsequent calls multiplying by two
        _getStatsData().npts
            = 2*ConstrainedRangeStatistics<CASA_STATP>::getNPts();
    }
    return (uInt64)_getStatsData().npts;
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::setCalculateAsAdded(Bool c) {
    ThrowIf(
        c, "FitToHalfStatistics does not support calculating statistics "
        "incrementally as data sets are added"
    );
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::reset() {
    _doMedAbsDevMed = False;
    _statsData = initializeStatsData<AccumType>();
    _rangeIsSet = False;
    _realMax.reset();
    _realMin.reset();
    ConstrainedRangeStatistics<CASA_STATP>::reset();
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::setStatsToCalculate(
    std::set<StatisticsData::STATS>& stats
) {
    if (! stats.empty() && _centerType == FitToHalfStatisticsData::CMEAN) {
        stats.insert(StatisticsData::MEAN);
    }
    ConstrainedRangeStatistics<CASA_STATP>::setStatsToCalculate(stats);
}

CASA_STATD
StatsData<AccumType> FitToHalfStatistics<CASA_STATP>::_getInitialStats() const {
    StatsData<AccumType> stats = initializeStatsData<AccumType>();
    stats.mean = _centerValue;
    return stats;
}

CASA_STATD
StatsData<AccumType> FitToHalfStatistics<CASA_STATP>::_getStatistics() {
    ConstrainedRangeStatistics<CASA_STATP>::_getStatistics();
    StatsData<AccumType>& stats = _getStatsData();
    if (stats.npts == 0) {
        return copy(stats);
    }
    stats.sum = stats.mean * stats.sumweights;
    if (_useLower) {
        stats.maxpos.first = -1;
        stats.maxpos.second = -1;
        stats.max = new AccumType(TWO*_centerValue - *stats.min);
    }
    else {
        stats.minpos.first = -1;
        stats.minpos.second = -1;
        stats.min = new AccumType(TWO*_centerValue - *stats.max);
    }
    return copy(stats);
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_setRange() {
    if (_rangeIsSet) {
        return;
    }
    ClassicalStatistics<CASA_STATP> cs(*this);
    // if FitToHalfStatisticsData::CMEDIAN, the quantile computer object in the
    // cs object will use the ConstrainedRangeQuantile methods, which is not
    // what we want here. So we have to explicitly ensure that the cs object
    // uses a bona-fide ClassicalStatisticsQuantileComputer object for
    // computation of the median. From a pedantic POV, the dataset used should
    // be the same (as in the same memory address) as in the cs object, but in
    // this case a copy will suffice and it does not require making
    // ClassicalStatistics::_getQuantileComputer() public.
    CountedPtr<ClassicalQuantileComputer<CASA_STATP> > qComputer(
        new ClassicalQuantileComputer<CASA_STATP>(&this->_getDataset())
    );
    cs.setQuantileComputer(qComputer);
    if (
        _centerType == FitToHalfStatisticsData::CMEAN
        || _centerType == FitToHalfStatisticsData::CMEDIAN
    ) {
        _centerValue = _centerType == FitToHalfStatisticsData::CMEAN
            ? cs.getStatistic(StatisticsData::MEAN)
            : cs.getMedian();
    }
    _getStatsData().mean = _centerValue;
    _getStatsData().median = new AccumType(_centerValue);
    this->_getQuantileComputer()->setMedian(_getStatsData().median);
    AccumType mymin, mymax;
    cs.getMinMax(mymin, mymax);
    if (_useLower) {
        _range = new std::pair<AccumType, AccumType>(mymin, _centerValue);
        _isNullSet = mymin > _centerValue;
    }
    else {
        _range = new std::pair<AccumType, AccumType>(_centerValue, mymax);
        _isNullSet = mymax < _centerValue;
    }
    // median must be set after _setRange(_range) call, because the _setRange()
    // call clears stats (and therefore will clear the median if it is set prior
    // to that call)
    ConstrainedRangeStatistics<CASA_STATP>::_setRange(_range);
    this->_getQuantileComputer()->setMedian(_getStatsData().median);
    _rangeIsSet = True;
}

// use a define to ensure code is compiled inline
#define _unweightedStatsCodeFH \
    if (*datum >= _range->first && *datum <= _range->second) { \
        StatisticsUtilities<AccumType>::accumulateSym( \
            stats.npts, stats.nvariance, stats.sumsq, *stats.min, *stats.max, \
            stats.minpos, stats.maxpos, *datum, location, _centerValue \
        ); \
        ngood += 2; \
    }

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) {
    auto datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _unweightedStatsCodeFH
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
        location.second += dataStride;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) {
    auto datum = dataBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _unweightedStatsCodeFH
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
        location.second += dataStride;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) {
    auto datum = dataBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _unweightedStatsCodeFH
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) {
    auto datum = dataBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _unweightedStatsCodeFH
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_updateDataProviderMaxMin(
    const StatsData<AccumType>& threadStats
) {
    // _realMin and _realMax are updated here during computation of accumulated
    // stats, even if there isn't a data provider. It is better to do it here
    // than in the accumulation methods, as the accumulation methods can be
    // called (and usually are for CASA) in a multi-threaded context. So, the
    // updates there would have to be put in omp critical blocks, thus impacting
    // performance. So this isn't necessarily updating the data provider (ie if
    // one doesn't exist) but it is necessary to do even if there isn't a data
    // provider in a method that is always called in a single-thread context.
    StatsDataProvider<CASA_STATP> *dataProvider
        = this->_getDataset().getDataProvider();
    StatsData<AccumType>& stats = _getStatsData();
    const Int64 iDataset = this->_getDataset().iDataset();
    if (
        iDataset == threadStats.maxpos.first
        && (stats.max.null() || *threadStats.max > *stats.max)
    ) {
        if (_realMax.null() || *threadStats.max > *_realMax) {
            _realMax = new AccumType(*threadStats.max);
            if (dataProvider && ! _useLower) {
                dataProvider->updateMaxPos(threadStats.maxpos);
            }
        }
    }
    if (
        iDataset == threadStats.minpos.first
        && (stats.min.null() || (*threadStats.min) < (*stats.min))
    ) {
        if (_realMin.null() || (*threadStats.min) < *_realMin) {
            _realMin = new AccumType(*threadStats.min);
            if (dataProvider && _useLower) {
                dataProvider->updateMinPos(threadStats.minpos);
            }
        }
    }
}

// use #define to ensure code is compiled inline
#define _weightedStatsCodeFH \
    if (*datum >= _range->first && *datum <= _range->second) { \
        StatisticsUtilities<AccumType>::waccumulateSym( \
            stats.npts, stats.sumweights, stats.nvariance, \
            stats.sumsq, *stats.min, *stats.max, stats.minpos, stats.maxpos, \
            *datum, *weight, location, _centerValue \
        ); \
    }

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _weightedStatsCodeFH
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _weightedStatsCodeFH
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _weightedStatsCodeFH
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void FitToHalfStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _weightedStatsCodeFH
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

}

#endif
