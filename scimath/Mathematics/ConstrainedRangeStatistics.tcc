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

#ifndef SCIMATH_CONSTRAINEDRANGESTATISTICS_TCC
#define SCIMATH_CONSTRAINEDRANGESTATISTICS_TCC

#include <casacore/scimath/Mathematics/ConstrainedRangeStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iomanip>

namespace casacore {

// min > max indicates that these quantities have not be calculated
template <class AccumType, class InputIterator, class MaskIterator>
ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::ConstrainedRangeStatistics()
	: ClassicalStatistics<AccumType, InputIterator, MaskIterator>(),
	 _range(), _doMedAbsDevMed(False) /*, _median()*/ /*, _npts(0),
	  _max(), _min(), _maxpos(-1, -1), _minpos(-1, -1) */ {
	reset();
}

template <class AccumType, class InputIterator, class MaskIterator>
ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::~ConstrainedRangeStatistics() {}

template <class AccumType, class InputIterator, class MaskIterator>
ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>&
ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::operator=(
	const ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>& other
) {
    if (this == &other) {
        return *this;
    }
    ClassicalStatistics<AccumType, InputIterator, MaskIterator>::operator=(other);
    _range = other._range;
    _doMedAbsDevMed = other._doMedAbsDevMed;
    //_median = other._median.null() ? NULL : new AccumType(*other._median);
    return *this;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getMedian(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
	Bool persistSortedArray
) {
	if (this->_getStatsData().median.null()) {
		_setRange();
		this->_getStatsData().median = new AccumType(
			ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMedian(
				knownNpts, knownMin, knownMax, binningThreshholdSizeBytes, persistSortedArray
			)
		);
	}
	return *this->_getStatsData().median;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getMedianAbsDevMed(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	_setRange();
	if (this->_getStatsData().median.null()) {
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
AccumType ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getMedianAndQuantiles(
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
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getMinMax(
	AccumType& mymin, AccumType& mymax
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMinMax(
		mymin, mymax
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
uInt64 ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getNPts() {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getNPts();
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<Double, AccumType> ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getQuantiles(
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
std::pair<Int64, Int64> ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getStatisticIndex(
	StatisticsData::STATS stat
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getStatisticIndex(stat);
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::reset() {
	_range = NULL;
	_doMedAbsDevMed = False;
	//_median = NULL;
	ClassicalStatistics<AccumType, InputIterator, MaskIterator>::reset();
}
template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (_isInRange(*datum)) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				_isInRange(*datum)
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) const {
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && _isInRange(*datum)) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) const {
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && _isInRange(*datum)
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (_isInRange(*datum) && *weight > 0) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				_isInRange(*datum) && *weight > 0
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && _isInRange(*datum) && *weight > 0) {
				++npts;
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_isInRange(
	const AccumType& datum
) const {
	return datum >= _range->first && datum <= _range->second;
}

#define _findBinCodeCR \
	if (_isInRange(*datum)) { \
		AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *this->_getStatsData().median) : *datum; \
		if (myDatum >= bBinDesc->minLimit && myDatum < *maxLimit.rbegin()) { \
			iCounts = bCounts; \
			iSameVal = bSameVal; \
			iAllSame = bAllSame; \
			iBinDesc = bBinDesc; \
			iMaxLimit = bMaxLimit; \
			while (iBinDesc != eBinDesc) { \
				if (myDatum >= iBinDesc->minLimit && myDatum < *iMaxLimit) { \
					AccumType idx = (myDatum - iBinDesc->minLimit)/iBinDesc->binWidth; \
					++(*iCounts)[StatisticsUtilities<AccumType>::getInt(idx)]; \
					if (*iAllSame) { \
						if (iSameVal->null()) { \
							*iSameVal = new AccumType(myDatum); \
						} \
						else { \
							*iAllSame = myDatum == *(*iSameVal); \
							if (! *iAllSame) { \
								*iSameVal = NULL; \
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

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator bAllSame = allSame.begin();
		vector<Bool>::iterator iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_findBinCodeCR
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator  bAllSame = allSame.begin();
		vector<Bool>::iterator  iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_findBinCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator bAllSame = allSame.begin();
		vector<Bool>::iterator iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask) {
				_findBinCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator bAllSame = allSame.begin();
		vector<Bool>::iterator iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask && StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_findBinCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator bAllSame = allSame.begin();
		vector<Bool>::iterator iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_findBinCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator bAllSame = allSame.begin();
		vector<Bool>::iterator iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*weight > 0
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_findBinCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator bAllSame = allSame.begin();
		vector<Bool>::iterator iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_findBinCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<typename StatisticsUtilities<AccumType>::BinDesc>& binDesc,
	const vector<AccumType>& maxLimit
) const {
		vector<vector<uInt64> >::iterator bCounts = binCounts.begin();
		vector<vector<uInt64> >::iterator iCounts = bCounts;
		typename vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
		typename vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
		vector<Bool>::iterator bAllSame = allSame.begin();
		vector<Bool>::iterator iAllSame = bAllSame;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator bBinDesc = binDesc.begin();
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator iBinDesc = bBinDesc;
		typename vector<typename StatisticsUtilities<AccumType>::BinDesc>::const_iterator eBinDesc = binDesc.end();
		typename vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
		typename vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_findBinCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_getStatistic(
	StatisticsData::STATS stat
) {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_getStatistic(stat);
}

template <class AccumType, class InputIterator, class MaskIterator>
StatsData<AccumType> ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_getStatistics() {
	_setRange();
	return ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_getStatistics();
}

#define _minMaxCodeCR \
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
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_minMaxCodeCR
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
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
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) const {
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask) {
				_minMaxCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) const {
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
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
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_minMaxCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
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
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_minMaxCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_minMaxCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _populateArrayCodeCR1 \
	if (_isInRange(*datum)) { \
		AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *this->_getStatsData().median) : *datum; \
		ary.push_back(myDatum); \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_populateArrayCodeCR1
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		while (count < nr) {
			if (*mask) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*weight > 0
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _populateArraysCodeCR \
	if (_isInRange(*datum)) { \
		AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *this->_getStatsData().median) : *datum; \
		if (myDatum >= includeLimits.begin()->first && myDatum < includeLimits.rbegin()->second) { \
			iIncludeLimits = bIncludeLimits; \
			iArys = bArys; \
			while (iIncludeLimits != eIncludeLimits) { \
				if (myDatum >= iIncludeLimits->first && myDatum < iIncludeLimits->second) { \
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

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_populateArraysCodeCR
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		while (count < nr) {
			if (*mask) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*mask
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				*weight > 0
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _PopulateTestArrayCodeCR \
	if (_isInRange(*datum)) { \
		ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - *this->_getStatsData().median) : *datum); \
		++npts; \
		if (npts > maxElements) { \
			return True; \
		} \
	}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
		Int64 count = 0;
		uInt npts = ary.size();
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_PopulateTestArrayCodeCR
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
		return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	uInt maxElements
) const {
		Int64 count = 0;
		uInt npts = ary.size();
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		typename DataRanges::const_iterator beginRange = ranges.begin();
		typename DataRanges::const_iterator endRange = ranges.end();
		while (count < nr) {
			if (
				StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
		return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	uInt maxElements
) const {
		Int64 count = 0;
		InputIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		uInt npts = ary.size();
		while (count < nr) {
			if (*mask) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		uInt npts = ary.size();
		while (count < nr) {
			if (*weight > 0) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
		return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
		return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride;
		uInt npts = ary.size();
		while (count < nr) {
			if (*mask && *weight > 0) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
}

// use a define to ensure code is compiled inline

#define _unweightedStatsCodeCR \
	if (_isInRange(*datum)) { \
		this->_accumulate (mymin, mymax, minpos, maxpos, *datum, count); \
		++ngood; \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_unweightedStatsCodeCR
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) {
		InputIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
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
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) {
	InputIterator datum = dataBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask) {
			_unweightedStatsCodeCR
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) {
		InputIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
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
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

// use #define to ensure code is compiled inline

#define _weightedStatsCodeCR \
	if (_isInRange(*datum)) { \
		this->_accumulate (mymin, mymax, minpos, maxpos, *datum, *weight, count); \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_weightedStatsCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
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
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) {
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
				&& StatisticsUtilities<AccumType>::includeDatum(
					*datum, beginRange, endRange, isInclude
				)
			) {
				_weightedStatsCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
		InputIterator datum = dataBegin;
		InputIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_weightedStatsCodeCR
			}
			StatisticsIncrementer<InputIterator, MaskIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

}

#endif
