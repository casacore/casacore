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
CASA_STATD
ConstrainedRangeStatistics<CASA_STATP>::ConstrainedRangeStatistics()
	: ClassicalStatistics<CASA_STATP>(),
	 _range(), _doMedAbsDevMed(False) /*, _median()*/ /*, _npts(0),
	  _max(), _min(), _maxpos(-1, -1), _minpos(-1, -1) */ {
	reset();
}

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
    _doMedAbsDevMed = other._doMedAbsDevMed;
    //_median = other._median.null() ? NULL : new AccumType(*other._median);
    return *this;
}

CASA_STATD
AccumType ConstrainedRangeStatistics<CASA_STATP>::getMedian(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
	Bool persistSortedArray, uInt64 nBins
) {
	if (this->_getStatsData().median.null()) {
		_setRange();
		this->_getStatsData().median = new AccumType(
			ClassicalStatistics<CASA_STATP>::getMedian(
				knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
				persistSortedArray, nBins
			)
		);
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
	if (this->_getStatsData().median.null()) {
		// sets _median, we can discard the return value
		this->getMedian();
	}
	_doMedAbsDevMed = True;
	AccumType medabsdevmed = ClassicalStatistics<CASA_STATP>::getMedianAbsDevMed(
		knownNpts, knownMin, knownMax, binningThreshholdSizeBytes,
		persistSortedArray, nBins
	);
	_doMedAbsDevMed = False;
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
	return ClassicalStatistics<CASA_STATP>::getMinMax(
		mymin, mymax
	);
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
	_doMedAbsDevMed = False;
	//_median = NULL;
	ClassicalStatistics<CASA_STATP>::reset();
}
CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_accumNpts(
	uInt64& npts,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
		DataIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (_isInRange(*datum)) {
				++npts;
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
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
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && _isInRange(*datum)) {
				++npts;
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
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
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (_isInRange(*datum) && *weight > 0) {
				++npts;
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
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
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && _isInRange(*datum) && *weight > 0) {
				++npts;
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_isInRange(
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

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
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
		DataIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_findBinCodeCR
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
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
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
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
		DataIterator datum = dataBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask) {
				_findBinCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, Int64 nr, uInt dataStride,
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
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
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
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_findBinCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
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
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
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
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_findBins(
	vector<vector<uInt64> >& binCounts,
    vector<CountedPtr<AccumType> >& sameVal, vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
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
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_findBinCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
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

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
		DataIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_minMaxCodeCR
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
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
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask) {
				_minMaxCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
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
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_minMaxCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
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
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_minMaxCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
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

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride
) const {
		Int64 count = 0;
		DataIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_populateArrayCodeCR1
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
		Int64 count = 0;
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
		Int64 count = 0;
		DataIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		while (count < nr) {
			if (*mask) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
		Int64 count = 0;
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_populateArrayCodeCR1
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
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

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		Int64 count = 0;
		DataIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_populateArraysCodeCR
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		Int64 count = 0;
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		Int64 count = 0;
		DataIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		while (count < nr) {
			if (*mask) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, Int64 nr,
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
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_populateArraysCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_populateArrays(
	vector<vector<AccumType> >& arys, uInt& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const vector<std::pair<AccumType, AccumType> > &includeLimits, uInt maxCount
) const {
		typename vector<vector<AccumType> >::iterator bArys = arys.begin();
		typename vector<vector<AccumType> >::iterator iArys = bArys;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
		typename vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
		typename vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
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

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
		Int64 count = 0;
		uInt npts = ary.size();
		DataIterator datum = dataBegin;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_PopulateTestArrayCodeCR
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
		return False;
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	uInt maxElements
) const {
		Int64 count = 0;
		uInt npts = ary.size();
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
		return False;
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	uInt maxElements
) const {
		Int64 count = 0;
		DataIterator datum = dataBegin;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		MaskIterator mask = maskBegin;
		uInt npts = ary.size();
		while (count < nr) {
			if (*mask) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
		Int64 count = 0;
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		uInt npts = ary.size();
		while (count < nr) {
			if (*weight > 0) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
		return False;
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
		return False;
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin,
	const WeightsIterator& weightsBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride;
		uInt npts = ary.size();
		while (count < nr) {
			if (*mask && *weight > 0) {
				_PopulateTestArrayCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
		return False;
}

CASA_STATD
Bool ConstrainedRangeStatistics<CASA_STATP>::_populateTestArray(
	vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
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

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride
) {
		DataIterator datum = dataBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			_unweightedStatsCodeCR
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude
) {
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) {
	DataIterator datum = dataBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask) {
			_unweightedStatsCodeCR
		}
		StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude
) {
		DataIterator datum = dataBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, mask, unityStride, dataStride, maskStride
			);
		}
}

// use #define to ensure code is compiled inline

#define _weightedStatsCodeCR \
	if (_isInRange(*datum)) { \
		this->_accumulate (mymin, mymax, minpos, maxpos, *datum, *weight, count); \
	}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride
) {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1;
		while (count < nr) {
			if (*weight > 0) {
				_weightedStatsCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, unityStride, dataStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude
) {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
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
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

CASA_STATD
void ConstrainedRangeStatistics<CASA_STATP>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
		DataIterator datum = dataBegin;
		WeightsIterator weight = weightsBegin;
		MaskIterator mask = maskBegin;
		Int64 count = 0;
		Bool unityStride = dataStride == 1 && maskStride == 1;
		while (count < nr) {
			if (*mask && *weight > 0) {
				_weightedStatsCodeCR
			}
			StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
				datum, count, weight, mask, unityStride, dataStride, maskStride
			);
		}
}

}

#endif
