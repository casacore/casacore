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

#ifndef SCIMATH_HINGESFENCESSTATISTICS_TCC
#define SCIMATH_HINGESFENCESSTATISTICS_TCC

#include <casacore/scimath/Mathematics/HingesFencesStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsIncrementer.h>
#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iomanip>

namespace casacore {

// min > max indicates that these quantities have not be calculated
template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::HingesFencesStatistics(
	Double f
)
	: ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>(),
	  _f(f), _rangeIsSet(False), _hasRange(False) {
	reset();
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::~HingesFencesStatistics() {}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>&
HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::operator=(
	const HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>& other
) {
    if (this == &other) {
        return *this;
    }
    ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::operator=(other);
    _f = other._f;
    _rangeIsSet = other._rangeIsSet;
    _hasRange = other._hasRange;
    return *this;
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::reset() {
	_rangeIsSet = False;
	_hasRange = False;
	//_range = NULL;
	//_median = NULL;
	//_doMedAbsDevMed = False;
	//_settingRange = False;
	ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::reset();
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::setCalculateAsAdded(
	Bool c
) {
	ThrowIf(
		c, "HingesFencesStatistics does not support calculating statistics "
			"incrementally as data sets are added"
	);
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin,
			maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin,
			maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin,weightsBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin,weightsBegin, nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude,
			binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude,
			binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArray(
			ary, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin, maskStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin, maskStride, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin,
			maskStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin,
			maskStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
Bool HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_setRange() {
	if (_rangeIsSet) {
		return;
	}
	if (_f < 0) {
		_rangeIsSet = True;
		_hasRange = False;
		return;
	}
	std::set<Double> quantiles;
	quantiles.insert(0.25);
	quantiles.insert(0.75);
	ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator> cs(*this);
	std::map<Double, AccumType> quartiles = cs.getQuantiles(quantiles);
	//ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_clearStats();
	AccumType iqr = quartiles[0.75] - quartiles[0.25];
	CountedPtr<std::pair<AccumType, AccumType> > range = new std::pair<AccumType, AccumType>(
		quartiles[0.25] - _f*iqr, quartiles[0.75] + _f*iqr
	);
	ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_setRange(range);
	_rangeIsSet = True;
	_hasRange = True;
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin,
			weightsBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin,
			weightsBegin, nr, dataStride
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class DataIterator, class MaskIterator, class WeightsIterator>
void HingesFencesStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, DataIterator, MaskIterator, WeightsIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
}

}

#endif
