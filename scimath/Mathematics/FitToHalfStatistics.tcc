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

#ifndef SCIMATH_FITTOHALFSTATISTICS_TCC
#define SCIMATH_FITTOHALFSTATISTICS_TCC

#include <casacore/scimath/Mathematics/FitToHalfStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iomanip>

namespace casacore {

template <class AccumType, class InputIterator, class MaskIterator>
const AccumType FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::TWO = AccumType(2);

// min > max indicates that these quantities have not be calculated
template <class AccumType, class InputIterator, class MaskIterator>
FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::FitToHalfStatistics(
	FitToHalfStatisticsData::CENTER centerType,
	FitToHalfStatisticsData::USE_DATA useData,
	AccumType centerValue
)
	: ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>(),
	  _centerType(centerType), _useLower(useData == FitToHalfStatisticsData::LE_CENTER), _centerValue(centerValue),
	  _statsData(initializeStatsData<AccumType>()), _doMedAbsDevMed(False), _rangeIsSet(False),
	  _realMax(), _realMin() {
	reset();
}

template <class AccumType, class InputIterator, class MaskIterator>
FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::~FitToHalfStatistics() {}

template <class AccumType, class InputIterator, class MaskIterator>
FitToHalfStatistics<AccumType, InputIterator, MaskIterator>&
FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::operator=(
	const FitToHalfStatistics<AccumType, InputIterator, MaskIterator>& other
) {
    if (this == &other) {
        return *this;
    }
    ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::operator=(other);
    _centerType = other._centerType;
    _useLower = other._useLower;
    _centerValue = other._centerValue;
    _statsData = copy(other._statsData);
    _doMedAbsDevMed = other._doMedAbsDevMed;
    _rangeIsSet = other._rangeIsSet;
    _realMax = other._realMax.null() ? NULL : new AccumType(*other._realMax);
    _realMin = other._realMin.null() ? NULL : new AccumType(*other._realMin);
    return *this;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::getMedian(
	CountedPtr<uInt64> , CountedPtr<AccumType> ,
	CountedPtr<AccumType> , uInt , Bool
) {
	if (_getStatsData().median.null()) {
		_getStatsData().median = new AccumType(_centerValue);
	}
	return *_getStatsData().median;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::getMedianAndQuantiles(
	std::map<Double, AccumType>& quantileToValue, const std::set<Double>& quantiles,
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax,
	uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	// The median is trivial, we just need to compute the quantiles
	quantileToValue = getQuantiles(
		quantiles, knownNpts, knownMin, knownMax,
		binningThreshholdSizeBytes, persistSortedArray
	);
	return getMedian();
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::getMedianAbsDevMed(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	if (_getStatsData().medAbsDevMed.null()) {
		// The number of points to hand to the base class is the number of real data points,
		// or exactly half of the total number of points
		CountedPtr<uInt64> realNPts = knownNpts.null()
			? new uInt64(getNPts()/2) : new uInt64(*knownNpts/2);
		CountedPtr<AccumType> realMin, realMax;
		//_getRealMinMax(realMin, realMax, knownMin, knownMax);
		_getStatsData().medAbsDevMed = new AccumType(
			ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getMedianAbsDevMed(
				realNPts, knownMin, knownMax, binningThreshholdSizeBytes, persistSortedArray
			)
		);
	}
	return *_getStatsData().medAbsDevMed;
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_getRealMinMax(
		CountedPtr<AccumType>& realMin, CountedPtr<AccumType>& realMax,
	CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax
) {
	realMin = new AccumType(_centerValue);
	realMax = new AccumType(_centerValue);
	if (knownMin.null() || knownMax.null()) {
		AccumType mymin, mymax;
		getMinMax(mymin, mymax);
		if (_useLower) {
			realMin = new AccumType(mymin);
		}
		else {
			realMax = new AccumType(mymax);
		}
	}
	else {
		if (_useLower) {
			realMin = new AccumType(*knownMin);
		}
		else {
			realMax = new AccumType(*knownMax);
		}
	}
}


template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::getMinMax(
	AccumType& mymin, AccumType& mymax
) {
	if ( _getStatsData().min.null() || _getStatsData().max.null()) {
		// This call returns the min and max of the real portion of the dataset
		ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getMinMax(mymin, mymax);
		_realMin = new AccumType(mymin);
		_realMax = new AccumType(mymax);
		if (_useLower) {
			mymax = TWO*_centerValue - mymin;
		}
		else {
			mymin = TWO*_centerValue - mymax;
		}
		_getStatsData().min = new AccumType(mymin);
		_getStatsData().max = new AccumType(mymax);
	}
	else {
		mymin = *_getStatsData().min;
		mymax = *_getStatsData().max;
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<Double, AccumType> FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::getQuantiles(
	const std::set<Double>& fractions, CountedPtr<uInt64> knownNpts,
	CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax,
	uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	ThrowIf(
		*fractions.begin() <= 0 || *fractions.rbegin() >= 1,
		"Value of all quantiles must be between 0 and 1 (noninclusive)"
	);
	ThrowIf (
		! knownNpts.null() && *knownNpts % 2 != 0,
		"knownNpts must be even for this class"
	);
	// fractions that exist in the virtual part of the dataset are determined from the
	// real fractions reflected about the center point.
	std::set<Double> realPortionFractions;
	std::set<Double>::const_iterator fiter = fractions.begin();
	std::set<Double>::const_iterator fend = fractions.end();
	// map the actual (full dataset) fractions to the real portion fractions
	std::map<Double, Double> actualToReal;
	Double freal = 0;
	std::map<Double, AccumType> actual;
	for ( ; fiter != fend; ++fiter) {
		if (near(*fiter, 0.5)) {
			if (_realMin.null() || _realMax.null()) {
				AccumType mymin, mymax;
				getMinMax(mymin, mymax);
			}
			actual[*fiter] = _useLower
				? *_realMax
				: TWO*_centerValue - *_realMin;
			continue;
		}
		Bool isVirtualQ = (_useLower && *fiter > 0.5)
			|| (! _useLower && *fiter < 0.5);
		if (isVirtualQ) {
			std::set<Double> actualF;
			actualF.insert(*fiter);
			uInt64 allNPts = knownNpts.null()
				? getNPts() : *knownNpts;
			std::map<Double, uInt64> actualFToI = StatisticsData::indicesFromFractions(
				allNPts, actualF
			);
			uInt64 actualIdx = actualFToI[*fiter];
			uInt64 realIdx = 0;
			if (_useLower) {
				realIdx = allNPts - (actualIdx + 1);
			}
			else {
				realIdx = allNPts/2 - (actualIdx + 1);
			}
			if (_useLower && (realIdx == allNPts/2 - 1)) {
                // the actual index is the reflection of the maximum
                // value of the real portion of the dataset
				if (_realMax.null()) {
					AccumType mymin, mymax;
					getMinMax(mymin, mymax);
				}
				actual[*fiter] = TWO*_centerValue - *_realMax;
				continue;
			}
			else if (! _useLower && realIdx == 0) {
                // the actual index is the reflection of the minimum
                // value of the real portion ofthe dataset
				if (_realMin.null()) {
					AccumType mymin, mymax;
					getMinMax(mymin, mymax);
				}
				actual[*fiter] = TWO*_centerValue - *_realMin;
				continue;
			}
			else {
				freal = Double(realIdx + 1)/Double(allNPts/2);
				if (freal == 1) {
					if (_realMin.null() || _realMax.null()) {
						AccumType mymin, mymax;
						getMinMax(mymin, mymax);
					}
					actual[*fiter] = *_getStatsData().min;
					continue;
				}
			}
		}
		else {
			// quantile is in the real part of the dataset
			freal = _useLower ? 2*(*fiter) : 2*(*fiter - 0.5);
		}
		realPortionFractions.insert(freal);
		actualToReal[*fiter] = freal;
	}
	if (realPortionFractions.empty()) {
		return actual;
	}
	// if given, knownNpts should be the number of points in the full dataset, or twice
	// the number in the real portion of the dataset. Points in only the real portion
	// is what scanning will find, so we need to cut the number of points in half. This
	// is also true if we have to compute using getNPts(), so we need our own value
	// to pass in to the call of the base class' method.
	CountedPtr<uInt64> realNPts = knownNpts.null()
		? new uInt64(getNPts()/2) : new uInt64(*knownNpts/2);
	CountedPtr<AccumType> realMin, realMax;
	_getRealMinMax(realMin, realMax, knownMin, knownMax);
	std::map<Double, AccumType> realPart = ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getQuantiles(
		realPortionFractions, realNPts, realMin, realMax, binningThreshholdSizeBytes,
		persistSortedArray
	);
	fiter = fractions.begin();
	while (fiter != fend) {
		if (actual.find(*fiter) == actual.end()) {
			Double realF = actualToReal[*fiter];
			AccumType actualValue = realPart[realF];
			if (
				(_useLower && *fiter > 0.5)
				|| (! _useLower && *fiter < 0.5)
			) {
				// quantile in virtual part of the data set, reflect corresponding
				// real value to get actual value
				actualValue = TWO*_centerValue - actualValue;
			}
			actual[*fiter] = actualValue;
		}
		++fiter;
	}
	return actual;
}

template <class AccumType, class InputIterator, class MaskIterator>
uInt64 FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::getNPts() {
	if (_getStatsData().npts == 0) {
		// guard against subsequent calls multiplying by two
		_getStatsData().npts = 2*ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::getNPts();
	}
	return (uInt64)_getStatsData().npts;
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::reset() {
	_clearData();
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::setCalculateAsAdded(
	Bool c
) {
	ThrowIf(
		c, "FitToHalfStatistics does not support calculating statistics "
			"incrementally as data sets are added"
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_clearData() {
	_doMedAbsDevMed = False;
	StatsData<AccumType> oldStats = copy(_statsData);
	_statsData = initializeStatsData<AccumType>();
	_statsData.mean = oldStats.mean;
	_statsData.median = oldStats.median.null() ? NULL : new AccumType(*oldStats.median);
	ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_clearData();

}


template <class AccumType, class InputIterator, class MaskIterator>
StatsData<AccumType> FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_getStatistics() {
	ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_getStatistics();
	_getStatsData().sum = _getStatsData().mean * _getStatsData().sumweights;
	return copy(_getStatsData());
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_setRange() {
	if (_rangeIsSet) {
		return;
	}
	//this->_setRangeIsBeingSet(True);
	ClassicalStatistics<AccumType, InputIterator, MaskIterator> cs(*this);
	if (_centerType == FitToHalfStatisticsData::CMEAN || _centerType == FitToHalfStatisticsData::CMEDIAN) {
		_centerValue = _centerType == FitToHalfStatisticsData::CMEAN
			? cs.getStatistic(StatisticsData::MEAN)
			: cs.getMedian();
	}
	_getStatsData().mean = _centerValue;
	_getStatsData().median = new AccumType(_centerValue);
	AccumType mymin, mymax;
	cs.getMinMax(mymin, mymax);
	CountedPtr<std::pair<AccumType, AccumType> > range = _useLower
		? new std::pair<AccumType, AccumType>(mymin, _centerValue)
		: new std::pair<AccumType, AccumType>(_centerValue, mymax);
	ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator>::_setRange(range);
	_rangeIsSet = True;
}

// use a define to ensure code is compiled inline
#define _unweightedStatsCodeFH \
	if (this->_isInRange(*datum)) { \
		StatisticsUtilities<AccumType>::accumulateSym( \
			_statsData.npts, _statsData.nvariance, _statsData.sumsq, mymin, mymax, minpos, \
			maxpos, *datum, count, _centerValue \
		); \
		ngood += 2; \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) {
	InputIterator datum = dataBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		_unweightedStatsCodeFH
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
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
			_unweightedStatsCodeFH
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
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
			_unweightedStatsCodeFH
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
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
			_unweightedStatsCodeFH
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_updateMaxMin(
	AccumType mymin, AccumType mymax, Int64 minpos, Int64 maxpos, uInt dataStride,
	const Int64& currentDataset
) {
	StatsDataProvider<AccumType, InputIterator, MaskIterator> *dataProvider
		= this->_getDataProvider();
	if (maxpos >= 0) {
		_realMax = new AccumType(mymax);
		if (! _useLower) {
			_getStatsData().maxpos.first = currentDataset;
			_getStatsData().maxpos.second = maxpos * dataStride;
			_getStatsData().minpos.first = -1;
			_getStatsData().minpos.second = -1;
			if (dataProvider) {
				dataProvider->updateMaxPos(_getStatsData().maxpos);
			}
			_getStatsData().max = new AccumType(mymax);
			_getStatsData().min = new AccumType(TWO*_centerValue - mymax);
		}
	}
	if (minpos >= 0) {
		_realMin = new AccumType(mymin);
		if (_useLower) {
			_getStatsData().minpos.first = currentDataset;
			_getStatsData().minpos.second = minpos * dataStride;
			_getStatsData().maxpos.first = -1;
			_getStatsData().maxpos.second = -1;
			if (dataProvider) {
				dataProvider->updateMinPos(_getStatsData().minpos);
			}
			_getStatsData().min = new AccumType(mymin);
			_getStatsData().max = new AccumType(TWO*_centerValue - mymin);
		}
	}
}

// use #define to ensure code is compiled inline
#define _weightedStatsCodeFH \
	if (this->_isInRange(*datum)) { \
		StatisticsUtilities<AccumType>::waccumulateSym( \
			_getStatsData().npts, _getStatsData().sumweights, _getStatsData().nvariance, \
			_getStatsData().sumsq, mymin, mymax, minpos, maxpos, *datum, \
			*weight, count, _centerValue \
		); \
	}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
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
			_weightedStatsCodeFH
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
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
			_weightedStatsCodeFH
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
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
			_weightedStatsCodeFH
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void FitToHalfStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
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
			_weightedStatsCodeFH
		}
		StatisticsIncrementer<InputIterator, MaskIterator>::increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

}


#endif
