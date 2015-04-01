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
template <class AccumType, class InputIterator, class MaskIterator>
HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::HingesFencesStatistics(
	Double f
)
	: ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>(),
	  _f(f), _rangeIsSet(False), _hasRange(False) {
	reset();
}

template <class AccumType, class InputIterator, class MaskIterator>
HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::~HingesFencesStatistics() {}

template <class AccumType, class InputIterator, class MaskIterator>
HingesFencesStatistics<AccumType, InputIterator, MaskIterator>&
HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::operator=(
	const HingesFencesStatistics<AccumType, InputIterator, MaskIterator>& other
) {
    if (this == &other) {
        return *this;
    }
    ClassicalStatistics<AccumType, InputIterator, MaskIterator>::operator=(other);
    _f = other._f;
    _rangeIsSet = other._rangeIsSet;
    _hasRange = other._hasRange;
    return *this;
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::reset() {
	_rangeIsSet = False;
	_hasRange = False;
	//_range = NULL;
	//_median = NULL;
	//_doMedAbsDevMed = False;
	//_settingRange = False;
	ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::reset();
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::setCalculateAsAdded(
	Bool c
) {
	ThrowIf(
		c, "HingesFencesStatistics does not support calculating statistics "
			"incrementally as data sets are added"
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin,
			maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, nr, dataStride, maskBegin,
			maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin,weightsBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin,weightsBegin, nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr,
			dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude,
			binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude,
			binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
			arys, currentCount, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin, maskStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin, maskStride, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin,
			maskStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin,	nr, dataStride, maskBegin,
			maskStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
	if (_hasRange) {
		return ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, maxElements
		);
	}
	else {
		return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
			ary, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, maxElements
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_setRange() {
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
	ClassicalStatistics<AccumType, InputIterator, MaskIterator> cs(*this);
	std::map<Double, AccumType> quartiles = cs.getQuantiles(quantiles);
	//ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_clearStats();
	AccumType iqr = quartiles[0.75] - quartiles[0.25];
	CountedPtr<std::pair<AccumType, AccumType> > range = new std::pair<AccumType, AccumType>(
		quartiles[0.25] - _f*iqr, quartiles[0.75] + _f*iqr
	);
	ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_setRange(range);
	_rangeIsSet = True;
	_hasRange = True;
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos,
			dataBegin, nr, dataStride, maskBegin, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin,
			weightsBegin, nr, dataStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin,
			weightsBegin, nr, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
	if (_hasRange) {
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
}

}

#endif
