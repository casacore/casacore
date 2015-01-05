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

#include <casacore/scimath/Mathematics/HingesFencesStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iomanip>

namespace casacore {

// min > max indicates that these quantities have not be calculated
template <class AccumType, class InputIterator, class MaskIterator>
HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::HingesFencesStatistics(
	Double f
)
	: ClassicalStatistics<AccumType, InputIterator, MaskIterator>(),
	  _f(f), _doMedAbsDevMed(False), _rangeIsSet(False), _hasRange(False),
	  _settingRange(False), _range(), _median() {
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
    _range = other._range;
    _median = other._median.isnull() ? NULL : new AccumType(*other._median);
    _doMedAbsDevMed = other._doMedAbsDevMed;
    _settingRange = other._settingRange;
    return *this;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::getMedian(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
	Bool persistSortedArray
) {
	if (_median.null()) {
		_setRange();
		_median = new AccumType(
			ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMedian(
				knownNpts, knownMin, knownMax, binningThreshholdSizeBytes, persistSortedArray
			)
		);
	}
	return *_median;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::getMedianAndQuantiles(
	std::map<Double, AccumType>& quantileToValue, const std::set<Double>& quantiles,
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax,
	uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMedianAndQuantiles(
		quantileToValue, quantiles, knownNpts, knownMin, knownMax,
		binningThreshholdSizeBytes, persistSortedArray
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::getMedianAbsDevMed(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	_setRange();
	if (_median.null()) {
		// sets _median, we can discard the return value
		this->getMedian();
	}
	_doMedAbsDevMed = True;
	AccumType medabsdevmed = ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMedianAbsDevMed(
		knownNpts, knownMin, knownMax, binningThreshholdSizeBytes, persistSortedArray
	);
	_doMedAbsDevMed = False;
	return medabsdevmed;
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<Double, AccumType> HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::getQuantiles(
	const std::set<Double>& quantiles, CountedPtr<uInt64> knownNpts,
	CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax,
	uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getQuantiles(
		quantiles, knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
		persistSortedArray
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::getMinMax(
	AccumType& mymin, AccumType& mymax
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMinMax(
		mymin, mymax
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
uInt64 HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::getNPts() {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getNPts();
}

template <class AccumType, class InputIterator, class MaskIterator>
std::pair<uInt, uInt> HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::getStatisticIndex(
	StatisticsData::STATS stat
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getStatisticIndex(stat);
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::reset() {
	_rangeIsSet = False;
	_hasRange = False;
	_range = NULL;
	_median = NULL;
	_doMedAbsDevMed = False;
	_settingRange = False;
	ClassicalStatistics<AccumType, InputIterator, MaskIterator>::reset();
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
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (_isInRange(*datum)) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				_isInRange(*datum)
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && _isInRange(*datum)) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
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
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && _isInRange(*datum)
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (_isInRange(*datum) && *weight > 0) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				_isInRange(*datum) && *weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && _isInRange(*datum) && *weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && _isInRange(*datum) && *weight > 0) {
				++npts;
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
			npts, dataBegin, weightsBegin, nr,
			dataStride, maskBegin, maskStride
		);
	}
}

#define _findBinCodeHF \
	if (_isInRange(*datum)) { \
		AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *_median) : *datum; \
		if (myDatum >= binDesc.minLimit && myDatum < maxLimit) { \
			AccumType idx = (myDatum - binDesc.minLimit)/binDesc.binWidth; \
			++binCounts[StatisticsUtilities<AccumType>::getInt(idx)]; \
			if (allSame) { \
				if (sameVal.null()) { \
					sameVal = new AccumType(myDatum); \
				} \
				else { \
					allSame = myDatum == *sameVal; \
					if (! allSame) { \
						sameVal = NULL; \
					} \
				} \
			} \
		} \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc,
	AccumType maxLimit
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_findBinCodeHF
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
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
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc, AccumType maxLimit
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_findBinCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
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
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc, AccumType maxLimit
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask) {
				_findBinCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
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
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc, AccumType maxLimit
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_findBinCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
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
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc, AccumType maxLimit
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_findBinCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
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
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc, AccumType maxLimit
) const {
	if (_hasRange) {
	InputIterator datum = dataBegin;
	InputIterator weight = weightsBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	typename DataRanges::const_iterator beginRange = ranges.begin();
	typename DataRanges::const_iterator endRange = ranges.end();
	while (count < nr) {
		if (
			*weight > 0
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_findBinCodeHF
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
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
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc, AccumType maxLimit
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && *weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_findBinCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
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
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const typename ClassicalStatistics<AccumType, InputIterator, MaskIterator>::BinDesc& binDesc, AccumType maxLimit
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_findBinCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
			binCounts, sameVal, allSame, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, binDesc, maxLimit
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_getStatistic(
	StatisticsData::STATS stat
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_getStatistic(stat);
}

template <class AccumType, class InputIterator, class MaskIterator>
Record HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_getStatistics() {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_getStatistics();
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_isInRange(
	const AccumType& datum
) const {
	return datum >= _range->first && datum <= _range->second;
}


#define _minMaxCodeHF \
	if (_isInRange(*datum)) { \
		if (! mymin.null()) { \
			if (*datum < *mymin) { \
				mymin = new AccumType(*datum); \
			} \
			else if (*datum > *mymax) { \
				mymax = new AccumType(*datum); \
			} \
		} \
		else { \
			mymin = new AccumType(*datum); \
			mymax = new AccumType(*datum); \
		} \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_minMaxCodeHF
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_minMaxCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
	}
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
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask) {
				_minMaxCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
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
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_minMaxCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_minMaxCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_minMaxCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && *weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_minMaxCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_minMaxCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
			mymin, mymax, dataBegin, weightsBegin, nr, dataStride,
			maskBegin, maskStride
		);
	}
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _populateArrayCodeHF \
	if (_isInRange(*datum)) { \
		AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *_median) : *datum; \
		if (includeLimits) { \
			if (myDatum >= includeLimits->first && myDatum < includeLimits->second) { \
				ary.push_back(myDatum); \
				++currentCount; \
				if (currentCount == maxCount) { \
					return; \
				} \
			} \
		} \
		else { \
			ary.push_back(myDatum); \
		} \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_populateArrayCodeHF
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, nr, dataStride,
			includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, nr, dataStride,
			ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		while (count < nr) {
			if (*mask) {
				_populateArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
	InputIterator datum = dataBegin;
	InputIterator weight = weightsBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		if (*weight > 0) {
			_populateArrayCodeHF
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, weightsBegin,
			nr, dataStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, weightsBegin,
			nr, dataStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_populateArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride, includeLimits, maxCount
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && *weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
			ary, currentCount, dataBegin, weightsBegin,	nr, dataStride,
			maskBegin, maskStride, ranges, isInclude, includeLimits, maxCount
		);
	}
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _PopulateTestArrayCodeHF \
	if (_isInRange(*datum)) { \
		ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - *_median) : *datum); \
		++npts; \
		if (npts > maxElements) { \
			return True; \
		} \
	}


template <class AccumType, class InputIterator, class MaskIterator>
Bool HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
	if (_hasRange) {
		Int64 count = 0;
		uInt npts = ary.size();
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_PopulateTestArrayCodeHF
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
		return False;
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
		Int64 count = 0;
		uInt npts = ary.size();
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
		return False;
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
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		uInt npts = ary.size();
		while (count < nr) {
			if (*mask) {
				_PopulateTestArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
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
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		uInt npts = ary.size();
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		uInt npts = ary.size();
		while (count < nr) {
			if (*weight > 0) {
				_PopulateTestArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
		return False;
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		uInt npts = ary.size();
		while (count < nr) {
			if (
				*weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
		return False;
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride;
		uInt npts = ary.size();
		while (count < nr) {
			if (*mask && *weight > 0) {
				_PopulateTestArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		uInt npts = ary.size();
		while (count < nr) {
			if (
				*mask && *weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
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
	if (_settingRange) {
		// prevent infinite recursion
		return;
	}
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
	_settingRange = True;
	std::map<Double, AccumType> quartiles
		= ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getQuantiles(quantiles);
	_settingRange = False;
	ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_clearStats();
	AccumType iqr = quartiles[0.75] - quartiles[0.25];
	_range = new std::pair<AccumType, AccumType>(
		quartiles[0.25] - _f*iqr, quartiles[0.75] + _f*iqr
	);
	_rangeIsSet = True;
	_hasRange = True;
}

// use a define to ensure code is compiled inline

#define _unweightedStatsCodeHF \
	if (_isInRange(*datum)) { \
		this->_accumulate (mymin, mymax, minpos, maxpos, *datum, count); \
		++ngood; \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_unweightedStatsCodeHF
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_unweightedStatsCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, unityStride, dataStride
			);
		}
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
	InputIterator datum = dataBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask) {
			_unweightedStatsCodeHF
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
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
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_unweightedStatsCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
			ngood, mymin, mymax, minpos, maxpos, dataBegin, nr,
			dataStride, maskBegin, maskStride, ranges, isInclude
		);
	}
}

// use #define to ensure code is compiled inline

#define _weightedStatsCodeHF \
	if (_isInRange(*datum)) { \
		this->_accumulate (mymin, mymax, minpos, maxpos, *datum, *weight, count); \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void HingesFencesStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) {
	if (_hasRange) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_weightedStatsCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_weightedStatsCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, unityStride, dataStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && *weight > 0
				&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_weightedStatsCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
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
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_weightedStatsCodeHF
			}
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
	}
	else {
		ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
			mymin, mymax, minpos, maxpos, dataBegin, weightsBegin,
			nr, dataStride, maskBegin, maskStride
		);
	}
}

}

