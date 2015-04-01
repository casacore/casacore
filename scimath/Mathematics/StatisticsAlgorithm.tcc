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

#ifndef SCIMATH_STATISTICSALGORITHM_TCC
#define SCIMATH_STATISTICSALGORITHM_TCC
 
#include <casacore/scimath/Mathematics/StatisticsAlgorithm.h>
 
#include <casacore/casa/BasicSL/STLIO.h>

namespace casacore {

template <class AccumType, class InputIterator, class MaskIterator>
StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::StatisticsAlgorithm()
: _data(), _weights(), _masks(), _counts(), _dataStrides(), _maskStrides(),
  _isIncludeRanges(), _dataRanges(), _sortedArray(), _statsToCalculate(),
  _unsupportedStats(), _dataProvider(NULL) {}

template <class AccumType, class InputIterator, class MaskIterator>
StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>&
StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::operator= (
	const StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>& other
) {
	 if (this == &other) {
		 return *this;
	 }
	 _data = other._data;
	 _weights = other._weights;
	 _masks = other._masks;
	 _counts = other._counts;
	 _dataStrides = other._dataStrides;
	 _maskStrides = other._maskStrides;
	 _isIncludeRanges = other._isIncludeRanges;
	 _dataRanges = other._dataRanges;
	 _sortedArray = other._sortedArray;
	 _statsToCalculate = other._statsToCalculate;
	 _unsupportedStats = other._unsupportedStats;
	 // WARN reference sementics
	 _dataProvider = other._dataProvider;
	 return *this;
}

template <class AccumType, class InputIterator, class MaskIterator>
StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::~StatisticsAlgorithm() {}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, uInt nr, uInt dataStride, Bool nrAccountsForStride
) {
	_throwIfDataProviderDefined();
	_data.push_back(first);
	// internally we store the number of strided points

	_counts.push_back(
		nrAccountsForStride ? nr
			: nr % dataStride == 0
			  ? nr/dataStride
				: nr/dataStride + 1
	);
	_dataStrides.push_back(dataStride);
	_addData();
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, uInt nr,
	const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
	Bool nrAccountsForStride
) {
	_throwIfDataProviderDefined();
	typename DataRanges::const_iterator riter = dataRanges.begin();
	typename DataRanges::const_iterator rend = dataRanges.end();
	while (riter != rend) {
		ThrowIf(
			(*riter).first > (*riter).second,
			"The first value in a range pair cannot be greater than the second"
		);
		++riter;
	}
	uInt n = _data.size();
	_isIncludeRanges[n] = isInclude;
	_dataRanges[n] = dataRanges;
	this->addData(first, nr, dataStride, nrAccountsForStride);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, const MaskIterator& maskFirst,
	uInt nr, uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
	_throwIfDataProviderDefined();
	uInt key = _data.size();
	_maskStrides[key] = maskStride;
	_masks[key] = maskFirst;
	this->addData(first, nr, dataStride, nrAccountsForStride);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, const MaskIterator& maskFirst,
	uInt nr, const DataRanges& dataRanges,
	Bool isInclude, uInt dataStride, Bool nrAccountsForStride,
	uInt maskStride
) {
	_throwIfDataProviderDefined();
	uInt key = _data.size();
	_maskStrides[key] = maskStride;
	_masks[key] = maskFirst;
	this->addData(
		first, nr, dataRanges, isInclude,
		dataStride, nrAccountsForStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, const InputIterator& weightFirst,
	uInt nr, uInt dataStride, Bool nrAccountsForStride
) {
	_throwIfDataProviderDefined();
	_weights[_data.size()] = weightFirst;
	this->addData(first, nr, dataStride, nrAccountsForStride);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, const InputIterator& weightFirst,
	uInt nr, const DataRanges& dataRanges,
	Bool isInclude, uInt dataStride, Bool nrAccountsForStride
) {
	_throwIfDataProviderDefined();
	_weights[_data.size()] = weightFirst;
	this->addData(
		first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, const InputIterator& weightFirst,
	const MaskIterator& maskFirst, uInt nr, uInt dataStride,
	Bool nrAccountsForStride, uInt maskStride
) {
	_throwIfDataProviderDefined();
	_weights[_data.size()] = weightFirst;
	this->addData(
		first, maskFirst, nr, dataStride, nrAccountsForStride, maskStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::addData(
	const InputIterator& first, const InputIterator& weightFirst,
	const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
	Bool isInclude, uInt dataStride, Bool nrAccountsForStride,
	uInt maskStride
) {
	_throwIfDataProviderDefined();
	_weights[_data.size()] = weightFirst;
	this->addData(
		first, maskFirst, nr, dataRanges, isInclude, dataStride,
		nrAccountsForStride, maskStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::deleteSortedArray() {
	_sortedArray.clear();
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::getQuantile(
	Double quantile, CountedPtr<uInt64> knownNpts,
	CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax,
	uInt binningThreshholdSizeBytes, Bool persistSortedArray
) {
	std::set<Double> qs;
	qs.insert(quantile);
	return getQuantiles(
		qs, knownNpts, knownMin, knownMax, binningThreshholdSizeBytes, persistSortedArray
	).begin()->second;
}

template <class AccumType, class InputIterator, class MaskIterator>
AccumType StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::getStatistic(
	StatisticsData::STATS stat
) {
	ThrowIf(
		_unsupportedStats.find(stat) != _unsupportedStats.end(),
		StatisticsData::toString(stat) + " is not a supported statistic for this algorithm"
	);
	ThrowIf(
		! _statsToCalculate.empty()
		&& _statsToCalculate.find(stat) == _statsToCalculate.end(),
		"You did not explicitly request to compute "
		+ StatisticsData::toString(stat)
	);
	return this->_getStatistic(stat);
}

template <class AccumType, class InputIterator, class MaskIterator>
StatsData<AccumType> StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::getStatistics() {
	return this->_getStatistics();
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, uInt nr, uInt dataStride, Bool nrAccountsForStride
) {
	_clearData();
	addData(first, nr, dataStride, nrAccountsForStride);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, uInt nr,
	const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
	Bool nrAccountsForStride
) {
	_clearData();
	addData(
		first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, const MaskIterator& maskFirst,
	uInt nr, uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
	_clearData();
	addData(
		first, maskFirst, nr, dataStride, nrAccountsForStride, maskStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, const MaskIterator& maskFirst,
	uInt nr, const DataRanges& dataRanges,
	Bool isInclude, uInt dataStride, Bool nrAccountsForStride,
	uInt maskStride
) {
	_clearData();
	addData(
		first, maskFirst, nr, dataRanges, isInclude, dataStride,
		nrAccountsForStride, maskStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, const InputIterator& weightFirst,
	uInt nr, uInt dataStride, Bool nrAccountsForStride
) {
	_clearData();
	addData(first, weightFirst, nr, dataStride, nrAccountsForStride);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, const InputIterator& weightFirst,
	uInt nr, const DataRanges& dataRanges,
	Bool isInclude, uInt dataStride, Bool nrAccountsForStride
) {
	_clearData();
	addData(
		first, weightFirst, nr, dataRanges, isInclude,
		dataStride, nrAccountsForStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, const InputIterator& weightFirst,
	const MaskIterator& maskFirst, uInt nr, uInt dataStride,
	Bool nrAccountsForStride, uInt maskStride
) {
	_clearData();
	addData(
		first, weightFirst, maskFirst, nr, dataStride,
		nrAccountsForStride, maskStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setData(
	const InputIterator& first, const InputIterator& weightFirst,
	const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
	Bool isInclude, uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
	_clearData();
	addData(
		first, weightFirst, maskFirst, nr, dataRanges, isInclude,
		dataStride, nrAccountsForStride, maskStride
	);
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::setStatsToCalculate(
	std::set<StatisticsData::STATS>& stats
) {
	_statsToCalculate = stats;
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_clearData() {
	_data.clear();
	_counts.clear();
	_masks.clear();
	_weights.clear();
	_dataRanges.clear();
	_dataStrides.clear();
	_maskStrides.clear();
	_sortedArray.clear();
	_dataProvider = NULL;
}

template <class AccumType, class InputIterator, class MaskIterator>
std::map<uInt64, AccumType> StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_valuesFromArray(
	vector<AccumType>& myArray, const std::set<uInt64>& indices
) {
	//uInt64 largestIdx = *indices.rbegin();
	uInt64 arySize = myArray.size();
	ThrowIf(
		*indices.rbegin() >= arySize,
		"Logic Error: Index " + String::toString(*indices.rbegin()) + " is too large. "
		"The sorted array has size " + String::toString(arySize)
	);
	std::map<uInt64, AccumType> indexToValuesMap;
	std::set<uInt64>::const_iterator initer = indices.begin();
	std::set<uInt64>::const_iterator inend = indices.end();
	Int64 lastIndex = 0;
	while(initer != inend) {
		GenSort<AccumType>::kthLargest(
			&myArray[lastIndex], arySize - lastIndex, *initer - lastIndex
		);
		lastIndex = *initer;
		++initer;
	}
	std::set<uInt64>::const_iterator iter = indices.begin();
	std::set<uInt64>::const_iterator end = indices.end();
	while (iter != end) {
		indexToValuesMap[*iter] = myArray[*iter];
		++iter;
	}
	return indexToValuesMap;
}

template <class AccumType, class InputIterator, class MaskIterator>
void StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>::_throwIfDataProviderDefined() const {
	ThrowIf(
		_dataProvider,
		"Logic Error: Cannot add data after a data provider has been set. Call setData() to clear "
		"the existing data provider and to add this new data set"
	);
}

}

#endif
