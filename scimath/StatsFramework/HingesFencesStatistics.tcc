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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#ifndef SCIMATH_HINGESFENCESSTATISTICS_TCC
#define SCIMATH_HINGESFENCESSTATISTICS_TCC

#include <casacore/scimath/StatsFramework/HingesFencesStatistics.h>

#include <casacore/scimath/StatsFramework/HingesFencesQuantileComputer.h>
#include <casacore/scimath/StatsFramework/StatisticsIncrementer.h>
#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>

namespace casacore {

// min > max indicates that these quantities have not be calculated
CASA_STATD
HingesFencesStatistics<CASA_STATP>::HingesFencesStatistics(
    Double f
) : ConstrainedRangeStatistics<CASA_STATP>(
        std::make_shared<HingesFencesQuantileComputer<CASA_STATP>>(
            &this->_getDataset()
        )
    ), _f(f) {
    reset();
}

CASA_STATD
HingesFencesStatistics<CASA_STATP>::HingesFencesStatistics(
    const HingesFencesStatistics<CASA_STATP>& other
) : ConstrainedRangeStatistics<CASA_STATP>(other), _f(other._f),
    _rangeIsSet(other._rangeIsSet), _hasRange(other._hasRange) {}

CASA_STATD
HingesFencesStatistics<CASA_STATP>::~HingesFencesStatistics() {}

CASA_STATD
HingesFencesStatistics<CASA_STATP>&
HingesFencesStatistics<CASA_STATP>::operator=(
    const HingesFencesStatistics<CASA_STATP>& other
) {
    if (this == &other) {
        return *this;
    }
    ConstrainedRangeStatistics<CASA_STATP>::operator=(other);
    _f = other._f;
    _rangeIsSet = other._rangeIsSet;
    _hasRange = other._hasRange;
    return *this;
}

CASA_STATD
StatisticsAlgorithm<CASA_STATP>*
HingesFencesStatistics<CASA_STATP>::clone() const {
    return new HingesFencesStatistics<CASA_STATP>(*this);
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::reset() {
    _rangeIsSet = False;
    _hasRange = False;
    ConstrainedRangeStatistics<CASA_STATP>::reset();
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::setCalculateAsAdded(
    Bool c
) {
    ThrowIf(
        c, "HingesFencesStatistics does not support calculating "
        "statistics incrementally as data sets are added"
    );
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride, maskBegin,
            maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, nr, dataStride, maskBegin,
            maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin,weightsBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin,weightsBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_accumNpts(
    uInt64& npts, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, weightsBegin, nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_accumNpts(
            npts, dataBegin, weightsBegin, nr, dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMax(
    std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMax(
            mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride
        );
    }
}


CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_minMaxNpts(
    uInt64& npts, std::shared_ptr<AccumType>& mymin, std::shared_ptr<AccumType>& mymax,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_minMaxNpts(
            npts, mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_setRange() {
    if (_rangeIsSet) {
        return;
    }
    _hasRange = _f >= 0;
    if (_hasRange) {
        std::set<Double> quantiles;
        quantiles.insert(0.25);
        quantiles.insert(0.75);
        ClassicalStatistics<CASA_STATP> cs(*this);
        std::map<Double, AccumType> quartiles = cs.getQuantiles(quantiles);
        auto iqr = quartiles[0.75] - quartiles[0.25];
        auto range = std::make_shared<std::pair<AccumType, AccumType>>(
                quartiles[0.25] - _f*iqr, quartiles[0.75] + _f*iqr
        );
        ConstrainedRangeStatistics<CASA_STATP>::_setRange(range);
    }
    _rangeIsSet = True;
    (
        (HingesFencesQuantileComputer<CASA_STATP> *)(
            this->_getQuantileComputer().get()
        )
    )->setHasRange(_hasRange);
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr,
            dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr,
            dataStride, maskBegin, maskStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_unweightedStats(
    StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_unweightedStats(
            stats, ngood, location, dataBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
            stats, location, dataBegin, weightsBegin, nr, dataStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_weightedStats(
            stats, location,  dataBegin, weightsBegin, nr, dataStride
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
            stats, location, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_weightedStats(
            stats, location, dataBegin, weightsBegin,
            nr, dataStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
            stats, location, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_weightedStats(
            stats, location, dataBegin, weightsBegin, nr, dataStride,
            maskBegin, maskStride, ranges, isInclude
        );
    }
}

CASA_STATD
void HingesFencesStatistics<CASA_STATP>::_weightedStats(
    StatsData<AccumType>& stats, LocationType& location,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
    if (_hasRange) {
        ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
            stats, location, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride
        );
    }
    else {
        ClassicalStatistics<CASA_STATP>::_weightedStats(
            stats, location, dataBegin, weightsBegin,
            nr, dataStride, maskBegin, maskStride
        );
    }
}

}

#endif
