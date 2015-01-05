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

#include <casacore/scimath/Mathematics/ClassicalStatistics.h>

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <iomanip>

namespace casacore {

// min > max indicates that these quantities have not be calculated
template <class AccumType, class InputIterator, class MaskIterator>
ClassicalStatistics<AccumType, InputIterator, MaskIterator>::ClassicalStatistics()
	: StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>(),
	  _npts(0), _idataset(0), _sum(0), _sumsq(0), _max(), _min(),
	  _minpos(0, 0), _maxpos(0,0), _calculateAsAdded(False), _doMaxMin(True),
	  _doMedAbsDevMed(False), _currentStats(), _median(), _medAbsDevMed() {
	reset();
}

template <class AccumType, class InputIterator, class MaskIterator>
ClassicalStatistics<AccumType, InputIterator, MaskIterator>::~ClassicalStatistics() {}

template <class AccumType, class InputIterator, class MaskIterator>
ClassicalStatistics<AccumType, InputIterator, MaskIterator>&
ClassicalStatistics<AccumType, InputIterator, MaskIterator>::operator=(
	const ClassicalStatistics<AccumType, InputIterator, MaskIterator>& other
) {
    if (this == &other) {
        return *this;
    }
    StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::operator=(other);
    _npts = other._npts;
    _idataset = other._idataset;
    _mean = other._mean;
    _nvariance = other._nvariance;
    _sum = other._sum;
    _sumsq = other._sumsq;
    _sumofweights = other._sumofweights;
    _max = other._max.null() ? NULL : new AccumType(*other._max);
    _min = other._min.null() ? NULL : new AccumType(*other._min);
    _minpos = other._minpos;
    _maxpos = other._maxpos;
    _calculateAsAdded = other._calculateAsAdded;
    _doMaxMin = other._doMaxMin;
    _doMedAbsDevMed = False;
    _currentStats = other._currentStats;
    _median = other._median.null() ? NULL : new AccumType(*other._median);
    _medAbsDevMed = other._medAbsDevMed.null()
    	? NULL : new AccumType(*other._medAbsDevMed);
    return *this;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMedian(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
	Bool persistSortedArray
) {
	if (! _median.null()) {
		return *_median;
	}
	std::set<uInt64> indices = _medianIndices(knownNpts);
	std::map<uInt64, AccumType> indexToValue = _indicesToValues(
		knownNpts, knownMin, knownMax,
		binningThreshholdSizeBytes/sizeof(AccumType),
		indices, persistSortedArray
	);
	_median = indexToValue.size() == 1
		? new AccumType(indexToValue[*indices.begin()])
		: new AccumType(
			(
				indexToValue[*indices.begin()]
				+ indexToValue[*indices.rbegin()]
			)/AccumType(2)
		);
	return *_median;
}

template <class AccumType, class InputIterator, class MaskIterator>
std::set<uInt64> ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_medianIndices(
	CountedPtr<uInt64> knownNpts
) {
	std::set<uInt64> indices;
	uInt64 mynpts = knownNpts.null() ? getNPts() : *knownNpts;
	if (mynpts % 2 == 0) {
		indices.insert(mynpts/2 - 1);
		indices.insert(mynpts/2);
	}
	else {
		indices.insert(mynpts/2);
	}
	return indices;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMedianAbsDevMed(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
	Bool persistSortedArray
) {
	if (! _medAbsDevMed.null()) {
		return *_medAbsDevMed;
	}

	// This call calculates the _median of the data set which is stored internally and
	// used, but is not necessary to be captured in the return value here.
	getMedian(
		knownNpts, knownMin, knownMax, binningThreshholdSizeBytes, persistSortedArray
	);
	std::set<uInt64> indices = _medianIndices(knownNpts);
	// throw the proper switch
	_doMedAbsDevMed = True;
	std::map<uInt64, AccumType> indexToValue = _indicesToValues(
		knownNpts, knownMin, knownMax,
		binningThreshholdSizeBytes/sizeof(AccumType),
		indices, persistSortedArray
	);
	_doMedAbsDevMed = False;
	_medAbsDevMed = indexToValue.size() == 1
		? new AccumType(indexToValue[*indices.begin()])
		: new AccumType(
			(
				indexToValue[*indices.begin()]
				+ indexToValue[*indices.rbegin()]
			)/AccumType(2)
		);
	return *_medAbsDevMed;
}


template <class AccumType, class InputIterator, class MaskIterator>
AccumType ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMedianAndQuantiles(
	std::map<Double, AccumType>& quantileToValue, const std::set<Double>& quantiles,
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
	Bool persistSortedArray
) {
	std::set<uInt64> medianIndices;
	quantileToValue.clear();
	CountedPtr<uInt64> mynpts = knownNpts.null() ? new uInt64(getNPts()) : knownNpts;
	if (_median.null()) {
		medianIndices = _medianIndices(mynpts);
	}
	std::map<Double, uInt64> quantileToIndex = StatisticsAlgorithm<
			AccumType, InputIterator, MaskIterator
		>::_indicesFromQuantiles(
			*mynpts, quantiles
		);
	std::set<uInt64> indices = medianIndices;
	std::map<Double, uInt64>::const_iterator qToIIter = quantileToIndex.begin();
	std::map<Double, uInt64>::const_iterator qToIEnd = quantileToIndex.end();
	while(qToIIter != qToIEnd) {
		indices.insert(qToIIter->second);
		++qToIIter;
	}
	std::map<uInt64, AccumType> indexToValue = _indicesToValues(
		mynpts, knownMin, knownMax,
		binningThreshholdSizeBytes/sizeof(AccumType),
		indices, persistSortedArray
	);
	if (_median.null()) {
		_median = *mynpts % 2 == 0
			? new AccumType(
				(
					indexToValue[*medianIndices.begin()]
					+ indexToValue[*medianIndices.rbegin()]
			    )/AccumType(2)
			)
			: new AccumType(indexToValue[*medianIndices.begin()]);
	}
	std::set<Double>::const_iterator qIter = quantiles.begin();
	std::set<Double>::const_iterator qEnd = quantiles.end();
	while (qIter != qEnd) {
		quantileToValue[*qIter] = indexToValue[quantileToIndex[*qIter]];
		++qIter;
	}
	return *_median;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getMinMax(
	AccumType& mymin, AccumType& mymax
) {
	if (_currentStats.empty() || ! _doMaxMin || _min.null() || _max.null()) {
		ThrowIf(
			_calculateAsAdded,
			"Min and max cannot be calculated unless all data are available "
			"simultaneously. To ensure that will be the case, call "
			"setCalculateAsAdded(False) on this object"
		);
		_doMinMax();
	}
	mymax = *_max;
	mymin = *_min;
}

template <class AccumType, class InputIterator, class MaskIterator>
uInt64 ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getNPts() {
	if (_currentStats.empty() || _npts == 0) {
		ThrowIf(
			_calculateAsAdded,
			"npts cannot be calculated unless all data are available "
			"simultaneously. To ensure that will be the case, call "
			"setCalculateAsAdded(False) on this object"
		);
		_doNpts();
	}
	return (uInt64)_npts;
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<Double, AccumType> ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getQuantiles(
	const std::set<Double>& quantiles, CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt binningThreshholdSizeBytes,
	Bool persistSortedArray
) {
	if (quantiles.empty()) {
		return std::map<Double, AccumType>();
	}
	ThrowIf(
		_calculateAsAdded,
		"Quantiles cannot be calculated unless all data are available "
		"simultaneously. To ensure that will be the case, call "
		"setCalculateAsAdded(False) on this object"
	);
	ThrowIf(
		*quantiles.begin() <= 0 || *quantiles.rbegin() >= 1,
		"Value of all quantiles must be between 0 and 1 (noninclusive)"
	);
	uInt64 mynpts = knownNpts.null() ? getNPts() : *knownNpts;
	std::map<Double, uInt64> quantileToIndexMap = StatisticsAlgorithm<
			AccumType, InputIterator, MaskIterator
		>::_indicesFromQuantiles(
			mynpts, quantiles
		);
	// This seemingly convoluted way of doing things with maps is necessary because
	// multiple quantiles can map to the same sorted array index, and multiple array
	// indices can map the same value if the values in the array are not unique.
	std::map<Double, AccumType> quantileToValue;
	std::set<uInt64> uniqueIndices;
	std::map<Double, uInt64>::const_iterator qToIIter = quantileToIndexMap.begin();
	std::map<Double, uInt64>::const_iterator qToIEnd = quantileToIndexMap.end();
	while(qToIIter != qToIEnd) {
		uniqueIndices.insert(qToIIter->second);
		++qToIIter;
	}
	std::map<uInt64, AccumType> indexToValue = _indicesToValues(
		knownNpts, knownMin, knownMax,
		binningThreshholdSizeBytes/sizeof(AccumType),
		uniqueIndices, persistSortedArray
	);
	qToIIter = quantileToIndexMap.begin();
	while (qToIIter != qToIEnd) {
		Double quantile = qToIIter->first;
		uInt64 index = qToIIter->second;
		quantileToValue[quantile] = indexToValue[index];
		++qToIIter;
	}
	return quantileToValue;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::reset() {
	_clearData();
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::setCalculateAsAdded(
	Bool c
) {
	ThrowIf (
		! this->_getDataProvider().null() && c,
		"Logic Error: It is nonsensical to call " + String(__func__) + " method "
		"with a True value if one is using a data provider"
	);
	ThrowIf(
		_idataset > 0,
		"Logic Error: " + String(__func__)
		+ " cannot be called after the first dataset has been set"
	);
	_calculateAsAdded = c;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::setDataProvider(
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
) {
	ThrowIf(
		_calculateAsAdded,
		"Logic Error: setCalculateAsAdded(True) has previously been called, "
		"in which case it is nonsensical to use a data provider. Please call "
		"setCalculateAsAdded(False), and then set the data provider"
	);
	StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setDataProvider(dataProvider);
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::setStatsToCalculate(
	std::set<StatisticsData::STATS>& stats
) {
	ThrowIf(
		_calculateAsAdded && _idataset > 0,
		"Cannot set stats to be calculated after setting the first dataset when "
		"stats are to be calculated as data are added"
	);
	_doMaxMin = stats.empty()
		|| stats.find(StatisticsData::MAX) != stats.end()
		|| stats.find(StatisticsData::MIN) != stats.end();
	StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setStatsToCalculate(stats);
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_addData() {
	this->_setSortedArray(vector<AccumType>());
	_median = NULL;
	if (! _currentStats.empty()) {
		_currentStats = Record();
	}
	if (_calculateAsAdded) {
		_getStatistics();
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_clearData();
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_clearData() {
	_clearStats();
	StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_clearData();
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_clearStats() {
	_currentStats = Record();
	_npts = 0;
	_sumofweights = 0;
	_sum = 0;
	_mean = 0;
	_nvariance = 0;
	_sumsq = 0;
	_max = NULL;
	_min = NULL;
	_maxpos = std::pair<uInt, uInt>(0, 0);
	_minpos = std::pair<uInt, uInt>(0, 0);
    _idataset = 0;
	_median = NULL;
	_doMedAbsDevMed = False;
}

template <class AccumType, class InputIterator, class MaskIterator>
std::pair<uInt, uInt> ClassicalStatistics<AccumType, InputIterator, MaskIterator>::getStatisticIndex(
	StatisticsData::STATS stat
) {
	ThrowIf(
		! (stat == StatisticsData::MAX || stat == StatisticsData::MIN),
		"Index only available for max and min"
	);
	ThrowIf(
		! _doMaxMin,
		"You must specify to calculate the max "
		"and/or min if you want this index"
	);
	std::set<StatisticsData::STATS> stats = this->_getStatsToCalculate();
	ThrowIf(
		! stats.empty()
		&& (
			(
				stat == StatisticsData::MAX
				&& stats.find(StatisticsData::MAX) == stats.end()
			)
			|| (
				stat == StatisticsData::MIN
				&& stats.find(StatisticsData::MIN) == stats.end()
			)
		),
		"You did not request to compute this statistic"
	);
	// this call will calculate maxpos and minpos
	_getStatistics();
	if (stat == StatisticsData::MAX) {
		return _maxpos;
	}
	else if (stat == StatisticsData::MIN) {
		return _minpos;
	}
	else {
		ThrowCc(
			"Logic Error: This branch should never be "
			"executed. Please file a defect report."
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_getStatistic(
	StatisticsData::STATS stat
) {
	AccumType value;
	Record r = _getStatistics();
	String statString = StatisticsData::toString(stat);
	ThrowIf(
		! r.isDefined(statString),
		"Logic Error: stat " + statString + " is not defined. "
		"Please file a defect report"
	);
	r.get(statString, value);
	return value;
}

template <class AccumType, class InputIterator, class MaskIterator>
Record ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_getStatistics() {
	if (! _currentStats.empty()) {
		return _currentStats;
	}
	//cout << __func__ << endl;
	_initIterators();
	Bool isMasked = False;
	Bool isWeighted = False;
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	while (True) {
		_initLoopVars();
		AccumType mymin = _min.null() ? AccumType(0) : *_min;
		AccumType mymax = _max.null() ? AccumType(0) : *_max;
		Int64 minpos = -1;
		Int64 maxpos = -1;
		uInt64 ngood = 0;
		if (_hasWeights) {
			isWeighted = True;
			if (_hasMask) {
				if (_hasRanges) {
					_weightedStats(
						mymin, mymax, minpos, maxpos,
						_myData, _myWeights, _myCount, _myStride,
						_myMask, _maskStride, _myRanges, _myIsInclude
					);
				}
				else {
					_weightedStats(
						mymin, mymax, minpos, maxpos,
						_myData, _myWeights, _myCount, _myStride,
						_myMask, _maskStride
					);
				}
			}
			else if (_hasRanges) {
				_weightedStats(
					mymin, mymax, minpos, maxpos,
					_myData, _myWeights, _myCount,
					_myStride, _myRanges, _myIsInclude
				);
			}
			else {
				// has weights, but no mask nor ranges
				_weightedStats(
					mymin, mymax, minpos, maxpos,
					_myData, _myWeights, _myCount, _myStride
				);
			}
		}
		else if (_hasMask) {
			// this data set has no weights, but does have a mask
			isMasked = True;
			if (_hasRanges) {
				_unweightedStats(
					ngood, mymin, mymax, minpos, maxpos,
					_myData, _myCount, _myStride, _myMask,
					_maskStride, _myRanges, _myIsInclude
				);
			}
			else {
				_unweightedStats(
					ngood, mymin, mymax, minpos, maxpos,
					_myData, _myCount, _myStride, _myMask, _maskStride
				);
			}
		}
		else if (_hasRanges) {
			// this data set has no weights no mask, but does have a set of ranges
			// associated with it
			_unweightedStats(
				ngood, mymin, mymax, minpos, maxpos,
				_myData, _myCount, _myStride, _myRanges, _myIsInclude
			);
		}
		else {
			// simplest case, this data set has no weights, no mask, nor any ranges associated
			// with it, and its stride is 1. No filtering of the data is necessary.
			_unweightedStats(
				ngood, mymin, mymax, minpos, maxpos,
				_myData, _myCount, _myStride
			);
		}
		if (! _hasWeights) {
			_sumofweights += ngood;
		}
		if (_doMaxMin) {
			_updateMaxMin(mymin, mymax, minpos, maxpos, _myStride);
		}
		++_idataset;
		if (! dataProvider.null()) {
			++(*dataProvider);
			if (dataProvider->atEnd()) {
				dataProvider->finalize();
				break;
			}
		}
		else {
			++_diter;
			if (_diter == _dend) {
				break;
			}
			++_citer;
			++_dsiter;
			++_dataCount;
		}
	}
	std::set<StatisticsData::STATS> stats = this->_getStatsToCalculate();
	_currentStats.define("isMasked", isMasked);
	_currentStats.define("isWeighted", isWeighted);
	if (isWeighted) {
		_currentStats.define(
			StatisticsData::toString(StatisticsData::SUMWEIGHTS),
			_sumofweights
		);
	}
	_currentStats.define(
		StatisticsData::toString(StatisticsData::MEAN), _mean
	);
	_currentStats.define(
		StatisticsData::toString(StatisticsData::NPTS), (Int64)_npts
	);
	_currentStats.define(
		StatisticsData::toString(StatisticsData::RMS),
		sqrt(_sumsq/_sumofweights)
	);
	AccumType one = 1;
	AccumType variance = _sumofweights > one ? _nvariance/(_sumofweights - one) : 0;
	_currentStats.define(
		StatisticsData::toString(StatisticsData::STDDEV),
		sqrt(variance)
	);
	_currentStats.define(
		StatisticsData::toString(StatisticsData::SUM), _sum
	);
	_currentStats.define(
		StatisticsData::toString(StatisticsData::SUMSQ), _sumsq
	);
	_currentStats.define(
		StatisticsData::toString(StatisticsData::VARIANCE), variance
	);
	if (
		! _max.null() && (
			stats.empty()
			|| stats.find(StatisticsData::MAX) != stats.end()
		)
	) {
		_currentStats.define(
			StatisticsData::toString(StatisticsData::MAX), *_max
		);
		_currentStats.define("maxDatasetIndex", _maxpos.first);
		_currentStats.define("maxIndex", _maxpos.second);
	}
	if (
		! _min.null() &&
			(
				stats.empty()
				|| stats.find(StatisticsData::MIN) != stats.end()
			)
	) {
		_currentStats.define(
			StatisticsData::toString(StatisticsData::MIN), *_min
		);
		_currentStats.define("minDatasetIndex", _minpos.first);
		_currentStats.define("minIndex", _minpos.second);
	}
	return _currentStats;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& /*dataBegin*/, Int64 nr, uInt /*dataStride*/
) const {
	npts += nr;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
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
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
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

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride
) const {
	InputIterator datum = dataBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask) {
			++npts;
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
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
			*mask && StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
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

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightsBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		if (*weight > 0) {
			++npts;
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
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
			*weight > 0
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

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
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
			*mask && *weight > 0
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

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumNpts(
	uInt64& npts,
	const InputIterator& dataBegin, const InputIterator& weightBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask && *weight > 0) {
			++npts;
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumulate(
	AccumType& mymin, AccumType& mymax, Int64& minpos, Int64& maxpos, const AccumType& datum, Int64 count
) {
	if (_doMaxMin) {
		StatisticsUtilities<AccumType>::accumulate (
			_npts, _sum, _mean, _nvariance,
			_sumsq, mymin, mymax, minpos,
			maxpos, datum, count
		);
	}
	else {
		StatisticsUtilities<AccumType>::accumulate (
			_npts, _sum, _mean, _nvariance,
			_sumsq, datum
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_accumulate(
	AccumType& mymin, AccumType& mymax, Int64& minpos, Int64& maxpos,
    const AccumType& datum, const AccumType& weight, Int64 count
) {
	if (_doMaxMin) {
		StatisticsUtilities<AccumType>::waccumulate (
			_npts, _sumofweights, _sum, _mean, _nvariance,
			_sumsq, mymin, mymax, minpos,
			maxpos, datum, weight, count
		);
	}
	else {
		StatisticsUtilities<AccumType>::waccumulate (
			_npts, _sumofweights, _sum, _mean, _nvariance,
			_sumsq, weight, datum
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
vector<uInt64> ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_binCounts(
	CountedPtr<AccumType>& sameVal, const BinDesc& binDesc
) {
	//cout << __func__ << endl;
    sameVal = NULL;
    Bool allSame = True;
    vector<uInt64> bins(binDesc.nBins, 0);
	AccumType maxLimit = binDesc.minLimit + (AccumType)binDesc.nBins*binDesc.binWidth;
	_initIterators();
    CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	while (True) {
		_initLoopVars();
		if (_hasWeights) {
			if (_hasMask) {
				if (_hasRanges) {
					_findBins(
						bins, sameVal, allSame, _myData, _myWeights, _myCount,
						_myStride, _myMask, _maskStride, _myRanges, _myIsInclude,
						binDesc, maxLimit
					);
				}
				else {
					_findBins(
						bins, sameVal, allSame, _myData, _myWeights,
						_myCount, _myStride, _myMask, _maskStride,
						binDesc, maxLimit
					);
				}
			}
			else if (_hasRanges) {
				_findBins(
					bins, sameVal, allSame, _myData, _myWeights, _myCount,
					_myStride, _myRanges, _myIsInclude,
					binDesc, maxLimit
				);
			}
			else {
				// has weights, but no mask nor ranges
				_findBins(
					bins, sameVal, allSame, _myData, _myWeights, _myCount, _myStride,
					binDesc, maxLimit
				);
			}
		}
		else if (_hasMask) {
			// this data set has no weights, but does have a mask
			if (_hasRanges) {
				_findBins(
					bins, sameVal, allSame, _myData, _myCount, _myStride,
					_myMask, _maskStride, _myRanges, _myIsInclude,
					binDesc, maxLimit
				);
			}
			else {
				_findBins(
					bins, sameVal, allSame, _myData, _myCount, _myStride, _myMask, _maskStride,
					binDesc, maxLimit
				);
			}
		}
		else if (_hasRanges) {
			// this data set has no weights no mask, but does have a set of ranges
			// associated with it
			_findBins(
				bins, sameVal, allSame, _myData, _myCount, _myStride,
				_myRanges, _myIsInclude,
				binDesc, maxLimit
			);
		}
		else {
			// simplest case, this data set has no weights, no mask, nor any ranges associated
			// with it. No filtering of the data is necessary.
			_findBins(
				bins, sameVal, allSame, _myData, _myCount, _myStride,
				binDesc, maxLimit
			);
		}
		if (! dataProvider.null()) {
			++(*dataProvider);
			if (dataProvider->atEnd()) {
				dataProvider->finalize();
				break;
			}
		}
		else {
			++_diter;
			if (_diter == _dend) {
				break;
			}
			++_citer;
			++_dsiter;
			++_dataCount;
		}
	}
	return bins;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_createDataArray(
	vector<AccumType>& ary, const std::pair<AccumType, AccumType> *const &includeLimits,
	uInt maxCount
) {
	//cout << __func__ << endl;
	_initIterators();
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	uInt currentCount = 0;
	while (True) {
		_initLoopVars();
		if (_hasWeights) {
			if (_hasMask) {
				if (_hasRanges) {
					_populateArray(
						ary, currentCount, _myData, _myWeights, _myCount,
						_myStride, _myMask, _maskStride, _myRanges, _myIsInclude,
						includeLimits, maxCount
					);
				}
				else {
					_populateArray(
						ary, currentCount, _myData, _myWeights,
						_myCount, _myStride, _myMask, _maskStride,
						includeLimits, maxCount
					);
				}
			}
			else if (_hasRanges) {
				_populateArray(
					ary, currentCount, _myData, _myWeights, _myCount,
					_myStride, _myRanges, _myIsInclude,
					includeLimits, maxCount
				);
			}
			else {
				// has weights, but no mask nor ranges
				_populateArray(
					ary, currentCount, _myData, _myWeights, _myCount, _myStride,
					includeLimits, maxCount
				);
			}
		}
		else if (_hasMask) {
			// this data set has no weights, but does have a mask
			if (_hasRanges) {
				_populateArray(
					ary, currentCount, _myData, _myCount, _myStride,
					_myMask, _maskStride, _myRanges, _myIsInclude,
					includeLimits, maxCount
				);
			}
			else {
				_populateArray(
					ary, currentCount, _myData, _myCount, _myStride, _myMask, _maskStride,
					includeLimits, maxCount
				);
			}
		}
		else if (_hasRanges) {
			// this data set has no weights no mask, but does have a set of ranges
			// associated with it
			_populateArray(
				ary, currentCount, _myData, _myCount, _myStride,
				_myRanges, _myIsInclude, includeLimits, maxCount
			);
		}
		else {
			// simplest case, this data set has no weights, no mask, nor any ranges associated
			// with it, and its stride is 1. No filtering of the data is necessary.
			_populateArray(
				ary, currentCount, _myData, _myCount, _myStride,
				includeLimits, maxCount
			);
		}
		if (! dataProvider.null()) {
			++(*dataProvider);
			if (dataProvider->atEnd()) {
				dataProvider->finalize();
				break;
			}
		}
		else {
			++_diter;
			if (_diter == _dend) {
				break;
			}
			++_citer;
			++_dsiter;
			++_dataCount;
		}
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<uInt64, AccumType> ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_dataFromMultipleBins(
	const BinDesc& binDesc, uInt maxArraySize,
	const std::set<uInt64>& dataIndices
) {
	// dataIndices are relative to minimum bin minimum border
    CountedPtr<AccumType> sameVal;
    vector<uInt64> binCounts = _binCounts(sameVal, binDesc);
	std::set<uInt64>::const_iterator initer = dataIndices.begin();
	std::set<uInt64>::const_iterator inend = dataIndices.end();
	std::map<uInt64, AccumType> ret;
    if (! sameVal.null()) {
        // all points are the same value
        while (initer != inend) {
            ret[*initer] = *sameVal;
            ++initer;
        }
        return ret;
    }
	vector<uInt64>::const_iterator biter = binCounts.begin();
	vector<uInt64>::const_iterator bend = binCounts.end();
	uInt dataCount = 0;
	uInt prevDataCount = 0;
	uInt loopCount = 0;
	while (initer != inend) {
		ThrowIf(biter == bend, "Logic Error: ran out of bins, accounting error");
		dataCount += *biter;
        if (*initer < dataCount) {
			// datum at index exists in current bin
			std::pair<AccumType, AccumType> binLimits;
			binLimits.first = binDesc.minLimit + (AccumType)loopCount*binDesc.binWidth;
			binLimits.second = binLimits.first + binDesc.binWidth;
			std::set<uInt64> newDataIndices;
			std::map<uInt64, uInt64> newToOld;
			while(initer != inend && *initer < dataCount) {
				uInt oldIdx = *initer;
				uInt newIdx = oldIdx - prevDataCount;
				newDataIndices.insert(newIdx);
				newToOld[newIdx] = oldIdx;
				++initer;
			}
			std::map<uInt64, AccumType> dataForCurrentBin = _dataFromSingleBin(
				*biter, maxArraySize, binLimits, newDataIndices
			);
			typename std::map<uInt64, AccumType>::const_iterator miter = dataForCurrentBin.begin();
			typename std::map<uInt64, AccumType>::const_iterator mend = dataForCurrentBin.end();
			while (miter != mend) {
				uInt newIdx = miter->first;
				uInt oldIdx = newToOld[newIdx];
				ret[oldIdx] = miter->second;
				++miter;
			}

		}
		prevDataCount = dataCount;
		++biter;
		++loopCount;
    }
	return ret;
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<uInt64, AccumType> ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_dataFromSingleBin(
	uInt64 binNpts, uInt maxArraySize, const std::pair<AccumType, AccumType>& binLimits,
	const std::set<uInt64>& dataIndices
) {
	if(binLimits.first >= binLimits.second) {
		ostringstream os;
		os << "Logic Error: bin limits are nonsensical: " << binLimits;
		ThrowCc(os.str());
	}
	std::map<uInt64, AccumType> ret;
	if (binNpts <= maxArraySize) {
        // contents of bin is small enough to be sorted in memory, so
		// get the bin limits and stuff the good points within those limits
		// in an array and sort it
		vector<AccumType> dataArray;
		_createDataArray(dataArray, &binLimits, binNpts);
		ThrowIf(
			dataArray.size() != binNpts,
			"Logic Error: data array has " + String::toString(dataArray.size())
			+ " elements but it should have " + String::toString(binNpts)
			+ ". Please file a bug report and include your dataset and your inputs"
		);
		std::set<uInt64>::const_iterator initer = dataIndices.begin();
		std::set<uInt64>::const_iterator inend = dataIndices.end();
		while (initer != inend) {
			ThrowIf(
				*initer >= binNpts,
				"Logic Error: aryIdx " + String::toString(*initer) + " is too large. "
				"It should be no larger than " + String::toString(binNpts-1)
				+ ". Please file a defect report and include your dataset and your inputs"
			);
			ret[*initer] = GenSort<AccumType>::kthLargest(
				&dataArray[0], binNpts, *initer
			);
			++initer;
		}
		return ret;
	}
	else {
		// bin contents are too large to be sorted in memory, this bin must be sub-binned
		BinDesc binDesc;
		_makeBins(
			binDesc, binLimits.first, binLimits.second,
			10000, False
		);
		return _dataFromMultipleBins(binDesc, maxArraySize, dataIndices);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_convertToAbsDevMedArray(
	vector<AccumType>& myArray, AccumType median
) {
	typename vector<AccumType>::iterator iter = myArray.begin();
	typename vector<AccumType>::iterator end = myArray.end();
	while (iter != end) {
		*iter = abs(*iter - median);
		++iter;
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_doMinMax() {
	//cout << __func__ << endl;
    _initIterators();
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	CountedPtr<AccumType> mymax;
	CountedPtr<AccumType> mymin;
	while (True) {
		_initLoopVars();
		if (_hasWeights) {
			if (_hasMask) {
				if (_hasRanges) {
					_minMax(
						mymin, mymax, _myData, _myWeights, _myCount, _myStride,
						_myMask, _maskStride, _myRanges, _myIsInclude
					);
				}
				else {
					_minMax(
						mymin, mymax, _myData, _myWeights, _myCount,
						_myStride, _myMask, _maskStride
					);
				}
			}
			else if (_hasRanges) {
				_minMax(
					mymin, mymax, _myData, _myWeights, _myCount,
					_myStride, _myRanges, _myIsInclude
				);
			}
			else {
				// has weights, but no mask nor ranges
				_minMax(
					mymin, mymax, _myData, _myWeights, _myCount, _myStride
				);
			}
		}
		else if (_hasMask) {
			// this data set has no weights, but does have a mask
			if (_hasRanges) {
				_minMax(
					mymin, mymax, _myData, _myCount, _myStride, _myMask,
					_maskStride, _myRanges, _myIsInclude
				);
			}
			else {
				_minMax(
					mymin, mymax, _myData, _myCount,
					_myStride, _myMask, _maskStride
				);
			}
		}
		else if (_hasRanges) {
			// this data set has no weights no mask, but does have a set of ranges
			// associated with it
			_minMax(
				mymin, mymax, _myData, _myCount,
				_myStride, _myRanges, _myIsInclude
			);
		}
		else {
			// simplest case, this data set has no weights, no mask, nor any ranges associated
			// with it. No filtering of the data is necessary.
			_minMax(mymin, mymax, _myData, _myCount, _myStride);
		}
		if (! dataProvider.null()) {
			++(*dataProvider);
			if (dataProvider->atEnd()) {
				dataProvider->finalize();
				break;
			}
		}
		else {
			++_diter;
			if (_diter == _dend) {
				break;
			}
			++_citer;
			++_dsiter;
			++_dataCount;
		}
	}
	ThrowIf (
		mymax.null() || mymin.null(),
		"No valid data found"
	);
	_max = mymax;
	_min = mymin;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_doNpts() {
	//cout << __func__ << endl;
	_initIterators();
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	uInt64 npts = 0;
	while (True) {
		_initLoopVars();
		if (_hasWeights) {
			if (_hasMask) {
				if (_hasRanges) {
					_accumNpts(
						npts, _myData, _myWeights, _myCount, _myStride,
						_myMask, _maskStride, _myRanges, _myIsInclude
					);
				}
				else {
					_accumNpts(
						npts, _myData, _myWeights, _myCount,
						_myStride, _myMask, _maskStride
					);
				}
			}
			else if (_hasRanges) {
				_accumNpts(
					npts, _myData, _myWeights, _myCount,
					_myStride, _myRanges, _myIsInclude
				);
			}
			else {
				// has weights, but no mask nor ranges
				_accumNpts(
					npts, _myData, _myWeights, _myCount, _myStride
				);
			}
		}
		else if (_hasMask) {
			// this data set has no weights, but does have a mask
			if (_hasRanges) {
				_accumNpts(
					npts, _myData, _myCount, _myStride, _myMask,
					_maskStride, _myRanges, _myIsInclude
				);
			}
			else {
				_accumNpts(
					npts, _myData, _myCount,
					_myStride, _myMask, _maskStride
				);
			}
		}
		else if (_hasRanges) {
			// this data set has no weights no mask, but does have a set of ranges
			// associated with it
			_accumNpts(
				npts, _myData, _myCount,
				_myStride, _myRanges, _myIsInclude
			);
		}
		else {
			// simplest case, this data set has no weights, no mask, nor any ranges associated
			// with it.
			_accumNpts(
				npts, _myData, _myCount, _myStride
			);
		}
		if (! dataProvider.null()) {
			++(*dataProvider);
			if (dataProvider->atEnd()) {
				dataProvider->finalize();
				break;
			}
		}
		else {
			++_diter;
			if (_diter == _dend) {
				break;
			}
			++_citer;
			++_dsiter;
			++_dataCount;
		}
	}
	ThrowIf (npts == 0, "No valid data found");
	_npts = npts;
	/*
	_currentStats.define(
		StatisticsData::toString(StatisticsData::NPTS), _npts
	);
	*/
}

// Tried making this into an inline method, but performance decreased by 20 - 25% when
// finding the median and quartiles on a 200 Mpix image. So the #define seems to be
// the better choice from a performance standpoint.
#define _findBinCode \
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
	}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const BinDesc& binDesc, AccumType maxLimit
) const {
	InputIterator datum = dataBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;

	while (count < nr) {
		 _findBinCode
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const BinDesc& binDesc, AccumType maxLimit
) const {
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
			_findBinCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride,
	const BinDesc& binDesc, AccumType maxLimit
) const {
	InputIterator datum = dataBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask) {
			_findBinCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
	Bool isInclude,
	const BinDesc& binDesc, AccumType maxLimit
) const {
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
			_findBinCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride,
	const BinDesc& binDesc, AccumType maxLimit
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightsBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		if (*weight > 0) {
			_findBinCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const BinDesc& binDesc, AccumType maxLimit
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_findBinCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightsBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const BinDesc& binDesc, AccumType maxLimit
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_findBinCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_findBins(
	vector<uInt64>& binCounts,
    CountedPtr<AccumType>& sameVal, Bool& allSame,
    const InputIterator& dataBegin, const InputIterator& weightBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const BinDesc& binDesc, AccumType maxLimit
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask && *weight > 0) {
			_findBinCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<uInt64, AccumType> ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_indicesToValues(
	CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
	CountedPtr<AccumType> knownMax, uInt maxArraySize,
	const std::set<uInt64>& indices, Bool persistSortedArray
) {
	std::map<uInt64, AccumType> indexToValue;
	if (
		_valuesFromSortedArray(
			indexToValue, knownNpts, indices, maxArraySize, persistSortedArray
		)
	) {
		return indexToValue;
	}
	AccumType mymin, mymax;
	if (knownMin.null() || knownMax.null()) {
		getMinMax(mymin, mymax);
	}
	else {
		mymin = *knownMin;
		mymax = *knownMax;
	}
	if (_doMedAbsDevMed) {
		mymax -= *_median;
		mymin = AccumType(0);
	}
	if (mymax == mymin) {
		// data set values are all the same
		std::set<uInt64>::const_iterator iter=indices.begin();
		std::set<uInt64>::const_iterator end=indices.end();
		while(iter != end) {
			indexToValue[*iter] = mymin;
			++iter;
		}
		return indexToValue;
	}
	AccumType pad = 1e-6*(mymax - mymin);
	std::pair<AccumType, AccumType> limits(mymin - pad, mymax + pad);
	uInt64 mynpts = knownNpts.null() ? getNPts() : *knownNpts;
	return _dataFromSingleBin(
		mynpts, maxArraySize, limits, indices
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_initIterators() {
	ThrowIf(
		this->_getData().size() == 0 && this->_getDataProvider().null(),
		"No data sets have been added"
	);
	if (! this->_getDataProvider().null()) {
		this->_getDataProvider()->reset();
	}
	else {
		_dataCount = 0;
		const vector<InputIterator>& data = this->_getData();
		_diter = data.begin();
		_dend = data.end();
		const vector<uInt>& dataStrides = this->_getDataStrides();
		_dsiter = dataStrides.begin();
		const vector<Int64>& counts = this->_getCounts();
		_citer = counts.begin();
		_masks = this->_getMasks();
		_weights = this->_getWeights();
		_ranges = this->_getRanges();
		_isIncludeRanges = this->_getIsIncludeRanges();
	}
	_hasRanges = False;
	_myRanges.clear();
	_myIsInclude = False;
	_hasMask = False;
	_hasWeights = False;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_initLoopVars() {
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	if (! dataProvider.null()) {
		_myData = dataProvider->getData();
		_myCount = dataProvider->getCount();
		_myStride = dataProvider->getStride();
		_hasRanges = dataProvider->hasRanges();
		if (_hasRanges) {
			_myRanges = dataProvider->getRanges();
			_myIsInclude = dataProvider->isInclude();
		}
		_hasMask = dataProvider->hasMask();
		if (_hasMask) {
			_myMask = dataProvider->getMask();
			_maskStride = dataProvider->getMaskStride();
		}
		_hasWeights = dataProvider->hasWeights();
		if (_hasWeights) {
			_myWeights = dataProvider->getWeights();
		}
	}
	else {
		_myData = *_diter;
		_myCount = *_citer;
		_myStride = *_dsiter;
		typename std::map<uInt, DataRanges>::const_iterator rangeI = _ranges.find(_dataCount);
		_hasRanges = rangeI != _ranges.end();
		if (_hasRanges) {
			_myRanges = rangeI->second;
			_myIsInclude = _isIncludeRanges.find(_dataCount)->second;
		}
		typename std::map<uInt, MaskIterator>::const_iterator maskI = _masks.find(_dataCount);
		_hasMask = maskI != _masks.end();
		if (_hasMask) {
			_myMask = maskI->second;
			_maskStride = this->_getMaskStrides().find(_dataCount)->second;
		}
		_hasWeights = _weights.find(_dataCount) != _weights.end();
		if (_hasWeights) {
			_myWeights = _weights.find(_dataCount)->second;
		}
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_isNptsSmallerThan(
	vector<AccumType>& unsortedAry, uInt maxArraySize
) {
	//cout << __func__ << endl;
	_initIterators();
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	Bool limitReached = False;
	while (True) {
		_initLoopVars();
		if (_hasWeights) {
			if (_hasMask) {
				if (_hasRanges) {
					limitReached = _populateTestArray(
						unsortedAry, _myData, _myWeights, _myCount,
						_myStride, _myMask, _maskStride, _myRanges, _myIsInclude,
						maxArraySize
					);
				}
				else {
					limitReached = _populateTestArray(
						unsortedAry, _myData, _myWeights,
						_myCount, _myStride, _myMask, _maskStride,
						maxArraySize
					);
				}
			}
			else if (_hasRanges) {
				limitReached = _populateTestArray(
					unsortedAry, _myData, _myWeights, _myCount,
					_myStride, _myRanges, _myIsInclude,
					maxArraySize
				);
			}
			else {
				// has weights, but no mask nor ranges
				limitReached = _populateTestArray(
					unsortedAry, _myData, _myWeights,
					_myCount, _myStride, maxArraySize
				);
			}
		}
		else if (_hasMask) {
			// this data set has no weights, but does have a mask
			if (_hasRanges) {
				limitReached = _populateTestArray(
					unsortedAry, _myData, _myCount, _myStride,
					_myMask, _maskStride, _myRanges, _myIsInclude,
					maxArraySize
				);
			}
			else {
				limitReached = _populateTestArray(
					unsortedAry, _myData, _myCount, _myStride, _myMask,
					_maskStride, maxArraySize
				);
			}
		}
		else if (_hasRanges) {
			// this data set has no weights no mask, but does have a set of ranges
			// associated with it
			limitReached = _populateTestArray(
				unsortedAry, _myData, _myCount, _myStride,
				_myRanges, _myIsInclude, maxArraySize
			);
		}
		else {
			// simplest case, this data set has no weights, no mask, nor any ranges associated
			// with it, and its stride is 1. No filtering of the data is necessary.
			limitReached = _populateTestArray(
				unsortedAry, _myData, _myCount, _myStride, maxArraySize
			);
		}
		if (limitReached) {
			unsortedAry.clear();
			return False;
		}
		if (! dataProvider.null()) {
			++(*dataProvider);
			if (dataProvider->atEnd()) {
				dataProvider->finalize();
				break;
			}
		}
		else {
			++_diter;
			if (_diter == _dend) {
				break;
			}
			++_citer;
			++_dsiter;
			++_dataCount;
		}
	}
	_npts = unsortedAry.size();
	return True;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_makeBins(
	BinDesc& bins, AccumType minData, AccumType maxData, uInt maxBins, Bool allowPad
) {

	bins.nBins = maxBins;
	bins.minLimit = minData;
	AccumType maxLimit = maxData;
	if (allowPad) {
		AccumType pad = (maxData - minData)/1e3;
		if (pad == (AccumType)0) {
			// try to handle Int like AccumTypes
			pad = AccumType(1);
		}
		bins.minLimit -= pad;
		maxLimit += pad;
	}
	bins.binWidth = (maxLimit - bins.minLimit)/(AccumType)bins.nBins;
}

#define _minMaxCode \
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
	}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride
) const {
	InputIterator datum = dataBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		_minMaxCode
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
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
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_minMaxCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
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
			_minMaxCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
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
			*mask && StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_minMaxCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
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
			_minMaxCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_minMaxCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_minMaxCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_minMax(
	CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
	const InputIterator& dataBegin, const InputIterator& weightBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask && *weight > 0) {
			_minMaxCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _populateArrayCode \
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

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	Int64 count = 0;
	InputIterator datum = dataBegin;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		_populateArrayCode
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
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
			_populateArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	Int64 count = 0;
	InputIterator datum = dataBegin;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	MaskIterator mask = maskBegin;
	while (count < nr) {
		if (*mask) {
			_populateArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
	uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_populateArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightsBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		if (*weight > 0) {
			_populateArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
	const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
	const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_populateArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask && *weight > 0) {
			_populateArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateArray(
	vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude,
	const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
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
			_populateArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
	uInt maxElements
) const {
	if (ary.size() + nr > maxElements) {
		return True;
	}
	Int64 count = 0;
	InputIterator datum = dataBegin;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - *_median) : *datum);
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
	return False;
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _PopulateTestArrayCode \
	ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - *_median) : *datum); \
	++npts; \
	if (npts > maxElements) { \
		return True; \
	}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
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
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_PopulateTestArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
	return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
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
			_PopulateTestArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
	return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_PopulateTestArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
	return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
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
			_PopulateTestArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
	return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_PopulateTestArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
	return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin,
	const InputIterator& weightBegin, Int64 nr, uInt dataStride,
	const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride;
	uInt npts = ary.size();
	while (count < nr) {
		if (*mask && *weight > 0) {
			_PopulateTestArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
	return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_populateTestArray(
	vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
	const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
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
			_PopulateTestArrayCode
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
	return False;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_updateMaxMin(
	AccumType mymin, AccumType mymax, Int64 minpos, Int64 maxpos, uInt dataStride
) {
	CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider
		= this->_getDataProvider();
	if (maxpos >= 0) {
		_maxpos.first = _idataset;
		_maxpos.second = maxpos * dataStride;
		if (! dataProvider.null()) {
			dataProvider->updateMaxPos(_maxpos);
		}
        _max = new AccumType(mymax);
	}
	if (minpos >= 0) {
		_minpos.first = _idataset;
		_minpos.second = minpos * dataStride;
		if (! dataProvider.null()) {
			dataProvider->updateMinPos(_minpos);
		}
        _min = new AccumType(mymin);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
	uInt64& ngood, AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, Int64 nr, uInt dataStride

) {
	InputIterator datum = dataBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1;
	while (count < nr) {
		_accumulate (
			mymin, mymax, minpos, maxpos, *datum, count
		);
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
	ngood = nr;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
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
			StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_accumulate (
				mymin, mymax, minpos, maxpos, *datum, count
			);
			++ngood;
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
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
			_accumulate (
				mymin, mymax, minpos, maxpos, *datum, count
			);
			++ngood;
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_unweightedStats(
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
			*mask && StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_accumulate (
				mymin, mymax, minpos, maxpos, *datum, count
			);
			++ngood;
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
Bool ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_valuesFromSortedArray(
	std::map<uInt64, AccumType>& values, CountedPtr<uInt64> knownNpts,
	const std::set<uInt64>& indices, uInt maxArraySize, Bool persistSortedArray
) {
	values.clear();
	// I need a little wiggle room, the caller can't make the maximum array size
	// ridiculously small
	//uInt maxArraySize = binningThreshholdSizeBytes/sizeof(AccumType(0));
	maxArraySize = max(maxArraySize, (uInt)1000);
	vector<AccumType> myArray;
	if (_doMedAbsDevMed && ! this->_getSortedArray().empty()) {
		// make a copy
		vector<AccumType> pSorted = this->_getSortedArray();
		myArray = pSorted;
		_convertToAbsDevMedArray(myArray, *_median);
	}
	if (! _doMedAbsDevMed) {
		myArray = this->_getSortedArray();
	}
	uInt64 myNpts = _npts > 0
		? (uInt64)_npts
		: knownNpts.null()
		  ? 0 : *knownNpts;
	if (myArray.empty()) {
		if (myNpts > 0) {
			// we have already computed npts
			if (myNpts <= maxArraySize) {
				// npts is smaller than the max array size, so create the array and sort
				// it in memory
				_createDataArray(myArray);
			}
			else {
				// data is too large to be sorted in memory
				return False;
			}
		}
		else {
			// we have to calculate the number of good points
			if (this->_getDataProvider().null()) {
				// we first get an upper limit by adding up the counts
				uInt nr = 0;
				const vector<Int64>& counts = this->_getCounts();
				vector<Int64>::const_iterator citer = counts.begin();
				vector<Int64>::const_iterator cend = counts.end();
				while (citer != cend) {
					nr += *citer;
					++citer;
				}
				if (nr <= maxArraySize) {
					// data can be sorted in memory
					_createDataArray(myArray);
				}
				else {
					return False;
				}
			}
			// last resort. scan through the dataset to determine if npts is small enough
			// if it is, myArray will be populated with unsorted data
			if (myArray.empty() && ! _isNptsSmallerThan(myArray, maxArraySize)) {
				return False;
			}
		}
	}
	values = StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_valuesFromArray(
		myArray, indices
	);
	if (! _doMedAbsDevMed) {
		if (persistSortedArray) {
			this->_setSortedArray(myArray);
		}
		else {
			this->_setSortedArray(vector<AccumType>());
		}
	}
	return True;
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
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
			_accumulate (
				mymin, mymax, minpos, maxpos, *datum, *weight, count
			);
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_accumulate (
				mymin, mymax, minpos, maxpos, *datum, *weight, count
			);
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, unityStride, dataStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
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
			&& StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_includeDatum(
				*datum, beginRange, endRange, isInclude
			)
		) {
			_accumulate(
				mymin, mymax, minpos, maxpos, *datum, *weight, count
			);
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

template <class AccumType, class InputIterator, class MaskIterator>
void ClassicalStatistics<AccumType, InputIterator, MaskIterator>::_weightedStats(
	AccumType& mymin, AccumType& mymax,
	Int64& minpos, Int64& maxpos,
	const InputIterator& dataBegin, const InputIterator& weightBegin,
	Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) {
	InputIterator datum = dataBegin;
	InputIterator weight = weightBegin;
	MaskIterator mask = maskBegin;
	Int64 count = 0;
	Bool unityStride = dataStride == 1 && maskStride == 1;
	while (count < nr) {
		if (*mask && *weight > 0) {
			_accumulate (
				mymin, mymax, minpos, maxpos, *datum, *weight, count
			);
		}
		StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_increment(
			datum, count, weight, mask, unityStride, dataStride, maskStride
		);
	}
}

}

