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

#ifndef SCIMATH_CONSTRAINEDRANGESTATISTICS_TCC
#define SCIMATH_CONSTRAINEDRANGESTATISTICS_TCC

#include <casacore/scimath/StatsFramework/ConstrainedRangeStatistics.h>

#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>

namespace casacore {

CASA_STATD
ConstrainedRangeStatistics<CASA_STATP>::ConstrainedRangeStatistics(
    std::shared_ptr<ConstrainedRangeQuantileComputer<CASA_STATP>> qc
) : ClassicalStatistics<CASA_STATP>(qc) {
    reset();
}

CASA_STATD
ConstrainedRangeStatistics<CASA_STATP>::ConstrainedRangeStatistics(
    const ConstrainedRangeStatistics<CASA_STATP>& other
) : ClassicalStatistics<CASA_STATP>(other), _range(other._range) {}


CASA_STATD
ConstrainedRangeStatistics<CASA_STATP>::~ConstrainedRangeStatistics() {}

CASA_STATD
ConstrainedRangeStatistics<CASA_STATP>&
ConstrainedRangeStatistics<CASA_STATP>::operator=(
    const ConstrainedRangeStatistics<CASA_STATP>& other
) {
    if (this == &other) {
        return *this;
    }
    ClassicalStatistics<CASA_STATP>::operator=(other);
    _range = other._range;
    return *this;
}

CASA_STATD
AccumType ConstrainedRangeStatistics<CASA_STATP>::getMedian(
    std::shared_ptr<uInt64> knownNpts, std::shared_ptr<AccumType> knownMin,
    std::shared_ptr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt nBins
) {
    if (! this->_getStatsData().median) {
        _setRange();
        std::shared_ptr<AccumType> median (new AccumType(
            ClassicalStatistics<CASA_STATP>::getMedian(
                knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
                persistSortedArray, nBins
            )
        ));
        this->_getStatsData().median = median;
        this->_getQuantileComputer()->setMedian(median);
    }
    return *this->_getStatsData().median;
}

CASA_STATD
AccumType ConstrainedRangeStatistics<CASA_STATP>::getMedianAbsDevMed(
    std::shared_ptr<uInt64> knownNpts, std::shared_ptr<AccumType> knownMin,
    std::shared_ptr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt nBins
) {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::getMedianAbsDevMed(
        knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
        persistSortedArray, nBins
    );
}

CASA_STATD
AccumType ConstrainedRangeStatistics<CASA_STATP>::getMedianAndQuantiles(
    std::map<Double, AccumType>& quantileToValue,
    const std::set<Double>& quantiles, std::shared_ptr<uInt64> knownNpts,
    std::shared_ptr<AccumType> knownMin, std::shared_ptr<AccumType> knownMax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
) {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::getMedianAndQuantiles(
        quantileToValue, quantiles, knownNpts, knownMin, knownMax,
        binningThreshholdSizeBytes, persistSortedArray, nBins
    );
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::getMinMax(
    AccumType& mymin, AccumType& mymax
) {
    _setRange();
    ClassicalStatistics<CASA_STATP>::getMinMax(mymin, mymax);
}

CASA_STATD
uInt64 ConstrainedRangeStatistics<CASA_STATP>::getNPts() {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::getNPts();
}

CASA_STATD
std::map<Double, AccumType>
ConstrainedRangeStatistics<CASA_STATP>::getQuantiles(
    const std::set<Double>& quantiles, std::shared_ptr<uInt64> knownNpts,
    std::shared_ptr<AccumType> knownMin, std::shared_ptr<AccumType> knownMax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
) {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::getQuantiles(
        quantiles, knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
        persistSortedArray, nBins
    );
}

CASA_STATD
LocationType ConstrainedRangeStatistics<CASA_STATP>::getStatisticIndex(
    StatisticsData::STATS stat
) {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::getStatisticIndex(stat);
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::reset() {
    _range = nullptr;
    ClassicalStatistics<CASA_STATP>::reset();
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    auto datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*datum >= _range->first && *datum <= _range->second) {
            ++npts;
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    auto datum = dataBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *datum >= _range->first && *datum <= _range->second
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            ++npts;
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    auto datum = dataBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *datum >= _range->first && *datum <= _range->second) {
            ++npts;
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
        auto datum = dataBegin;
        auto mask = maskBegin;
        uInt64 count = 0;
        auto beginRange = ranges.cbegin();
        auto endRange = ranges.cend();
        while (count < nr) {
            if (
                *mask && *datum >= _range->first && *datum <= _range->second
                && StatisticsUtilities<AccumType>::includeDatum(
                    *datum, beginRange, endRange, isInclude
                )
            ) {
                ++npts;
            }
            StatisticsIncrementer<CASA_STATQ>::increment(
                datum, count, mask, dataStride, maskStride
            );
        }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
        auto datum = dataBegin;
        auto weight = weightsBegin;
        uInt64 count = 0;
        while (count < nr) {
            if (
                *datum >= _range->first && *datum <= _range->second
                && *weight > 0
            ) {
                ++npts;
            }
            StatisticsIncrementer<CASA_STATQ>::increment(
                datum, count, weight, dataStride
            );
        }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *datum >= _range->first && *datum <= _range->second
            && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            ++npts;
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask && *datum >= _range->first && *datum <= _range->second
            && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            ++npts;
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (
            *mask && *datum >= _range->first && *datum <= _range->second
            && *weight > 0
        ) {
            ++npts;
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// non-virtual version of method
CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_setRange(
    std::shared_ptr<std::pair<AccumType, AccumType>> r
) {
    this->_clearStats();
    _range = r;
    (
        (ConstrainedRangeQuantileComputer<CASA_STATP>*)(
            this->_getQuantileComputer().get()
        )
    )->setRange(*r);
}

CASA_STATD
AccumType ConstrainedRangeStatistics<CASA_STATP>::_getStatistic(
    StatisticsData::STATS stat
) {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::_getStatistic(stat);
}

CASA_STATD
StatsData<AccumType> ConstrainedRangeStatistics<CASA_STATP>::_getStatistics() {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::_getStatistics();
}

#define _minMaxCodeCR \
    if (*datum >= _range->first && *datum <= _range->second) { \
        _minMaxCode \
    }

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    auto datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _minMaxCodeCR
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    auto datum = dataBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _minMaxCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _minMaxCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _minMaxCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

#define _minMaxNptsCodeCR \
    if (*datum >= _range->first && *datum <= _range->second) { \
        _minMaxNptsCode \
    }

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    auto datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _minMaxNptsCodeCR
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    auto datum = dataBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

#define _unweightedStatsCodeCR \
    if (*datum >= _range->first && *datum <= _range->second) { \
        this->_accumulate(stats, *datum, location); \
        ++ngood; \
    }

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) {
    auto datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _unweightedStatsCodeCR
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
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
            _unweightedStatsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) {
    auto datum = dataBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _unweightedStatsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
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
            _unweightedStatsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

// use #define to ensure code is compiled inline

#define _weightedStatsCodeCR \
    if (*datum >= _range->first && *datum <= _range->second) { \
        this->_accumulate(stats, *datum, *weight, location); \
    }

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
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
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
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
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
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
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

}

#endif
