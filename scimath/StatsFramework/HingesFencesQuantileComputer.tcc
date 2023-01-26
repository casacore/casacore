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

#ifndef SCIMATH_HINGESFENCESQUANTILECOMPUTER_TCC
#define SCIMATH_HINGESFENCESQUANTILECOMPUTER_TCC

#include <casacore/scimath/StatsFramework/HingesFencesQuantileComputer.h>

namespace casacore {

CASA_STATD
HingesFencesQuantileComputer<CASA_STATP>::HingesFencesQuantileComputer(
    StatisticsDataset<CASA_STATP>* dataset
) : ConstrainedRangeQuantileComputer<CASA_STATP>(dataset) {}

CASA_STATD
HingesFencesQuantileComputer<CASA_STATP>::HingesFencesQuantileComputer(
    const HingesFencesQuantileComputer<CASA_STATP>& other
) : ConstrainedRangeQuantileComputer<CASA_STATP>(other),
    _hasRange(other._hasRange) {}

CASA_STATD
HingesFencesQuantileComputer<CASA_STATP>::~HingesFencesQuantileComputer() {}

CASA_STATD
HingesFencesQuantileComputer<CASA_STATP>&
HingesFencesQuantileComputer<CASA_STATP>::operator=(
    const HingesFencesQuantileComputer<CASA_STATP>& other
) {
    if (this == &other) {
        return *this;
    }
    ConstrainedRangeQuantileComputer<CASA_STATP>::operator=(other);
    _hasRange = other._hasRange;
    return *this;
}

CASA_STATD
StatisticsAlgorithmQuantileComputer<CASA_STATP>*
HingesFencesQuantileComputer<CASA_STATP>::clone() const {
    return new HingesFencesQuantileComputer<CASA_STATP>(*this);
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::reset() {
    ConstrainedRangeQuantileComputer<CASA_STATP>::reset();
    _hasRange = false;
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin,
            nr, dataStride, binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin,
            nr, dataStride, binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, nr,
            dataStride, ranges, isInclude, binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, nr,
            dataStride, ranges, isInclude, binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, nr,
            dataStride, maskBegin, maskStride, binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, nr,
            dataStride, maskBegin, maskStride, binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude, binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude, binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride, ranges, isInclude,
            binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride, ranges, isInclude,
            binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const std::vector<AccumType>& maxLimit
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride, binDesc, maxLimit
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_findBins(
            binCounts, sameVal, allSame, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride, binDesc, maxLimit
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
    uint32_t dataStride, const DataRanges& ranges, bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
    uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            ranges, isInclude, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            ranges, isInclude, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            maskBegin, maskStride, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            maskBegin, maskStride, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude, const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride, maskBegin,
            maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride, maskBegin,
            maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const IncludeLimits& includeLimits,
    uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,
            nr, dataStride, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,
            nr, dataStride, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin, nr, dataStride,
            ranges, isInclude, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin, nr, dataStride,
            ranges, isInclude, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, maxElements
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude, uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, ranges, isInclude, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, ranges, isInclude, maxElements
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, maskBegin, maskStride, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, maskBegin, maskStride, maxElements
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude, uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, maskBegin,
            maskStride, ranges, isInclude, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, nr, dataStride, maskBegin,
            maskStride, ranges, isInclude, maxElements
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride, maxElements
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude, uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude,
            maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude,
            maxElements
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, maxElements
        );
    }
}

CASA_STATD
bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude, uint32_t maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, maxElements
        );
    }
}

}

#endif
