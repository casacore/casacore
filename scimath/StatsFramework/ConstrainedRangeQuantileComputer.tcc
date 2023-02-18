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

#ifndef SCIMATH_CONSTRAINEDRANGEQUANTILECOMPUTER_TCC
#define SCIMATH_CONSTRAINEDRANGEQUANTILECOMPUTER_TCC

#include <casacore/scimath/StatsFramework/ConstrainedRangeQuantileComputer.h>

namespace casacore {

CASA_STATD
ConstrainedRangeQuantileComputer<CASA_STATP>::ConstrainedRangeQuantileComputer(
    StatisticsDataset<CASA_STATP>* dataset
) : ClassicalQuantileComputer<CASA_STATP>(dataset) {}


CASA_STATD
ConstrainedRangeQuantileComputer<CASA_STATP>::ConstrainedRangeQuantileComputer(
    const ConstrainedRangeQuantileComputer<CASA_STATP>& other
) : ClassicalQuantileComputer<CASA_STATP>(other),
    _doMedAbsDevMed(other._doMedAbsDevMed),
    _myMedian(other._myMedian), _range(other._range) {}

CASA_STATD
ConstrainedRangeQuantileComputer<CASA_STATP>
::~ConstrainedRangeQuantileComputer() {}

CASA_STATD
ConstrainedRangeQuantileComputer<CASA_STATP>&
ConstrainedRangeQuantileComputer<CASA_STATP>::operator=(
    const ConstrainedRangeQuantileComputer<CASA_STATP>& other
) {
    if (this == &other) {
        return *this;
    }
    ClassicalQuantileComputer<CASA_STATP>::operator=(other);
    _doMedAbsDevMed = other._doMedAbsDevMed;
    _myMedian = other._myMedian;
    _range = other._range;
    return *this;
}

CASA_STATD
StatisticsAlgorithmQuantileComputer<CASA_STATP>*
ConstrainedRangeQuantileComputer<CASA_STATP>::clone() const {
    return new ConstrainedRangeQuantileComputer<CASA_STATP>(*this);
}

CASA_STATD
AccumType ConstrainedRangeQuantileComputer<CASA_STATP>::getMedian(
    uInt64 mynpts, AccumType mymin, AccumType mymax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
) {
    auto median = this->_getMedian();
    if (! median) {
        median.reset (new AccumType(
            ClassicalQuantileComputer<CASA_STATP>::getMedian(
                mynpts, mymin, mymax, binningThreshholdSizeBytes,
                persistSortedArray, nBins
            )
        ));
        this->setMedian(median);
    }
    return *median;
}

CASA_STATD
AccumType ConstrainedRangeQuantileComputer<CASA_STATP>::getMedianAbsDevMed(
    uInt64 mynpts, AccumType mymin, AccumType mymax,
    uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
) {
    auto medabsdevmed = this->_getMedianAbsDevMedian();
    if (! medabsdevmed) {
        std::shared_ptr<AccumType> median = this->_getMedian();
        if (! median) {
            this->getMedian(
                mynpts, mymin, mymax, binningThreshholdSizeBytes,
                persistSortedArray, nBins
            );
        }
        _doMedAbsDevMed = True;
        _myMedian = *this->_getMedian();
        medabsdevmed.reset(
            new AccumType(
                ClassicalQuantileComputer<CASA_STATP>::getMedianAbsDevMed(
                    mynpts, mymin, mymax, binningThreshholdSizeBytes,
                    persistSortedArray, nBins
                )
            )
        );
        _doMedAbsDevMed = False;
    }
    return *medabsdevmed;
}

#define _findBinCodeCR \
    if (*datum >= _range.first && *datum <= _range.second) { \
        AccumType myDatum = _doMedAbsDevMed \
            ? abs((AccumType)*datum - _myMedian) : *datum; \
        if ( \
            myDatum >= bBinDesc->getMinHistLimit() \
            && myDatum < *maxLimit.rbegin() \
        ) { \
            iCounts = bCounts; \
            iSameVal = bSameVal; \
            iAllSame = bAllSame; \
            iBinDesc = bBinDesc; \
            iMaxLimit = bMaxLimit; \
            while (iBinDesc != eBinDesc) { \
                if ( \
                    myDatum >= iBinDesc->getMinHistLimit() \
                    && myDatum < *iMaxLimit \
                ) { \
                    uInt idx = iBinDesc->getIndex(myDatum); \
                    ++(*iCounts)[idx]; \
                    if (*iAllSame) { \
                        if (! *iSameVal) { \
                            iSameVal->reset (new AccumType(myDatum)); \
                        } \
                        else { \
                            *iAllSame = myDatum == *(*iSameVal); \
                            if (! *iAllSame) { \
                                iSameVal->reset(); \
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
        } \
    }

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _findBinCodeCR
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
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
            _findBinCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _findBinCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
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
            _findBinCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _findBinCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
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
            _findBinCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
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
            _findBinCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<StatsHistogram<AccumType>>& binDesc,
    const DataArray& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bBinDesc = binDesc.cbegin();
    auto iBinDesc = bBinDesc;
    auto eBinDesc = binDesc.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _findBinCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize
// performance
#define _populateArrayCodeCR1 \
    if (*datum >= _range.first && *datum <= _range.second) { \
        AccumType myDatum = _doMedAbsDevMed \
            ? abs((AccumType)*datum - _myMedian) : *datum; \
        ary.push_back(myDatum); \
    }

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    uInt64 count = 0;
    auto datum = dataBegin;
    while (count < nr) {
        _populateArrayCodeCR1
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    uInt64 count = 0;
    auto datum = dataBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCodeCR1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    uInt64 count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArrayCodeCR1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    uInt64 count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCodeCR1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArrayCodeCR1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
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
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCodeCR1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArrayCodeCR1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
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
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCodeCR1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _populateArraysCodeCR \
    if (*datum >= _range.first && *datum <= _range.second) { \
        AccumType myDatum = _doMedAbsDevMed \
            ? abs((AccumType)*datum - _myMedian) : *datum; \
        if ( \
            myDatum >= includeLimits.cbegin()->first \
            && myDatum < includeLimits.rbegin()->second \
        ) { \
            iIncludeLimits = bIncludeLimits; \
            iArys = bArys; \
            while (iIncludeLimits != eIncludeLimits) { \
                if ( \
                    myDatum >= iIncludeLimits->first \
                    && myDatum < iIncludeLimits->second \
                ) { \
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
        } \
    }

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const IncludeLimits& includeLimits, uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uInt64 count = 0;
    auto datum = dataBegin;
    while (count < nr) {
        _populateArraysCodeCR
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const IncludeLimits& includeLimits, uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uInt64 count = 0;
    auto datum = dataBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const IncludeLimits& includeLimits, uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uInt64 count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArraysCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude, const IncludeLimits& includeLimits, uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uInt64 count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const IncludeLimits& includeLimits,
    uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArraysCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const IncludeLimits& includeLimits, uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
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
            _populateArraysCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const IncludeLimits& includeLimits, uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArraysCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ConstrainedRangeQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uInt64& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const IncludeLimits& includeLimits, uInt64 maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
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
            _populateArraysCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize
// performance
#define _PopulateTestArrayCodeCR \
    if (*datum >= _range.first && *datum <= _range.second) { \
        ary.push_back( \
            _doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum \
        ); \
        ++npts; \
        if (npts > maxElements) { \
            return True; \
        } \
    }

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    uInt maxElements
) const {
    uInt64 count = 0;
    auto npts = ary.size();
    auto datum = dataBegin;
    while (count < nr) {
        _PopulateTestArrayCodeCR
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
    return False;
}

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    uInt64 count = 0;
    auto npts = ary.size();
    auto datum = dataBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
    return False;
}

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
    uInt64 count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto npts = ary.size();
    while (count < nr) {
        if (*mask) {
            _PopulateTestArrayCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude, uInt maxElements
) const {
    uInt64 count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto npts = ary.size();
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr,
    uInt dataStride, uInt maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    auto npts = ary.size();
    while (count < nr) {
        if (*weight > 0) {
            _PopulateTestArrayCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    auto npts = ary.size();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    auto npts = ary.size();
    while (count < nr) {
        if (*mask && *weight > 0) {
            _PopulateTestArrayCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ConstrainedRangeQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude, uInt maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uInt64 count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    auto npts = ary.size();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCodeCR
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return False;
}

}

#endif
