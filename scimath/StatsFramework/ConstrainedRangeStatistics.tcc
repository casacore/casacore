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

// Default constructor is not allowed to be called
CASA_STATD
ConstrainedRangeStatistics<CASA_STATP>::ConstrainedRangeStatistics()
    : ClassicalStatistics<CASA_STATP>(),
     _range() {
    reset();
}

CASA_STATD
ConstrainedRangeStatistics<CASA_STATP>::ConstrainedRangeStatistics(
    CountedPtr<ConstrainedRangeQuantileComputer<CASA_STATP> > qc
) : ClassicalStatistics<CASA_STATP>(qc), _range() {
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
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    if (! this->_getStatsData().median) {
        _setRange();
        CountedPtr<AccumType> median = new AccumType(
            ClassicalStatistics<CASA_STATP>::getMedian(
                knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
                persistSortedArray, nBins
            )
        );
        this->_getStatsData().median = median;
        this->_getQuantileComputer()->setMedian(median);
    }
    return *this->_getStatsData().median;
}

CASA_STATD
AccumType ConstrainedRangeStatistics<CASA_STATP>::getMedianAbsDevMed(
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt64 nBins
) {
    _setRange();
    AccumType medabsdevmed = ClassicalStatistics<CASA_STATP>::getMedianAbsDevMed(
        knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
        persistSortedArray, nBins
    );
    return medabsdevmed;
}

CASA_STATD
AccumType ConstrainedRangeStatistics<CASA_STATP>::getMedianAndQuantiles(
    std::map<Double, AccumType>& quantileToValue, const std::set<Double>& quantiles,
    CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
    CountedPtr<AccumType> knownMax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt64 nBins
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
std::map<Double, AccumType> ConstrainedRangeStatistics<CASA_STATP>::getQuantiles(
    const std::set<Double>& quantiles, CountedPtr<uInt64> knownNpts,
    CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt64 nBins
) {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::getQuantiles(
        quantiles, knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
        persistSortedArray, nBins
    );
}

CASA_STATD
std::pair<Int64, Int64> ConstrainedRangeStatistics<CASA_STATP>::getStatisticIndex(
    StatisticsData::STATS stat
) {
    _setRange();
    return ClassicalStatistics<CASA_STATP>::getStatisticIndex(stat);
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::reset() {
    _range = NULL;
    ClassicalStatistics<CASA_STATP>::reset();
}
CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*datum >= _range->first && *datum <= _range->second) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
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
            *datum >= _range->first && *datum <= _range->second
            && StatisticsUtilities<AccumType>::includeDatum(
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
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *datum >= _range->first && *datum <= _range->second) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
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
                *mask && *datum >= _range->first && *datum <= _range->second
                && StatisticsUtilities<AccumType>::includeDatum(
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
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride
) const {
        DataIterator datum = dataBegin;
        WeightsIterator weight = weightsBegin;
        Int64 count = 0;
        while (count < nr) {
            if (
                *datum >= _range->first && *datum <= _range->second
                && *weight > 0
            ) {
                ++npts;
            }
            StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
                datum, count, weight, dataStride
            );
        }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
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
            *datum >= _range->first && *datum <= _range->second
            && *weight > 0
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
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
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
            *mask && *datum >= _range->first && *datum <= _range->second
            && *weight > 0
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
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (
            *mask && *datum >= _range->first && *datum <= _range->second
            && *weight > 0
        ) {
            ++npts;
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// non-virtual version of method
CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_setRange(
    CountedPtr<std::pair<AccumType, AccumType> > r
) {
    this->_clearStats();
    _range = r;
    ConstrainedRangeQuantileComputer<CASA_STATP>* ptr
        = (ConstrainedRangeQuantileComputer<CASA_STATP>*)this->_getQuantileComputer().get();
    ptr->setRange(*r);
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
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    Int64 count = 0;
    while (count < nr) {
        _minMaxCodeCR
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask) {
            _minMaxCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _minMaxCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
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
            _minMaxCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
    CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _minMaxCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
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
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    Int64 count = 0;
    while (count < nr) {
        _minMaxNptsCodeCR
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask) {
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
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
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _minMaxNptsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
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
    StatsData<AccumType>& stats, uInt64& ngood, /* AccumType& mymin,
    AccumType& mymax, LocationType& minpos, LocationType& maxpos, */
    LocationType& location, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride
) {
    DataIterator datum = dataBegin;
    Int64 count = 0;
    while (count < nr) {
        _unweightedStatsCodeCR
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, /* AccumType& mymin,
    AccumType& mymax, LocationType& minpos, LocationType& maxpos, */
    LocationType& location, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude
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
            _unweightedStatsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, /* AccumType& mymin,
    AccumType& mymax, LocationType& minpos, LocationType& maxpos, */
    LocationType& location, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask) {
            _unweightedStatsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, /* AccumType& mymin,
    AccumType& mymax, LocationType& minpos, LocationType& maxpos, */
    LocationType& location, const DataIterator& dataBegin, Int64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
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
            _unweightedStatsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
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
    StatsData<AccumType>& stats, /* AccumType& mymin, AccumType& mymax,
    LocationType& minpos, LocationType& maxpos, */ LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, /* AccumType& mymin, AccumType& mymax,
    LocationType& minpos, LocationType& maxpos, */ LocationType& location,
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
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, /* AccumType& mymin, AccumType& mymax,
    LocationType& minpos, LocationType& maxpos, */ LocationType& location,
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
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, /* AccumType& mymin, AccumType& mymax,
    LocationType& minpos, LocationType& maxpos, */ LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    Int64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _weightedStatsCodeCR
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
        location.second += dataStride;
    }
}

}

#endif
