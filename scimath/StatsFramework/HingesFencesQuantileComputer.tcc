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

// The default constructor is disallowed
CASA_STATD
HingesFencesQuantileComputer<CASA_STATP>::HingesFencesQuantileComputer()
    : ConstrainedRangeQuantileComputer<CASA_STATP>(NULL), _hasRange(False) {}

CASA_STATD
HingesFencesQuantileComputer<CASA_STATP>::HingesFencesQuantileComputer(
    StatisticsDataset<CASA_STATP>* dataset
) : ConstrainedRangeQuantileComputer<CASA_STATP>(dataset), _hasRange(False) {}

CASA_STATD
HingesFencesQuantileComputer<CASA_STATP>::HingesFencesQuantileComputer(
    const HingesFencesQuantileComputer<CASA_STATP>& other
) : ConstrainedRangeQuantileComputer<CASA_STATP>(other), _hasRange(other._hasRange) {}

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
    _hasRange = False;
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride,
            ranges, isInclude
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride,
            ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride,
            maskBegin, maskStride
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, nr, dataStride,
            maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
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
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,
            nr, dataStride
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,
            nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,    nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArray(
            ary, dataBegin, weightsBegin,    nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride, includeLimits, maxCount
        );
    }
}

CASA_STATD
void HingesFencesQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    if (_hasRange) {
        ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,    nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
    else {
        ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
            arys, currentCount, dataBegin, weightsBegin,    nr, dataStride,
            maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
        );
    }
}

CASA_STATD
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    uInt maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin,    nr, dataStride, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin,    nr, dataStride, maxElements
        );
    }
}

CASA_STATD
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    uInt maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin,    nr, dataStride, ranges, isInclude, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin,    nr, dataStride, ranges, isInclude, maxElements
        );
    }
}

CASA_STATD
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    uInt maxElements
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
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
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
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    uInt maxElements
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
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    if (_hasRange) {
        return ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude, maxElements
        );
    }
    else {
        return ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
            ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude, maxElements
        );
    }
}

CASA_STATD
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
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
Bool HingesFencesQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
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
