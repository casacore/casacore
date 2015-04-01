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

#ifndef SCIMATH_STATSALGORITHM_H
#define SCIMATH_STATSALGORITHM_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/scimath/Mathematics/StatsDataProvider.h>
#include <casacore/scimath/Mathematics/StatisticsData.h>
#include <casacore/scimath/Mathematics/StatisticsTypes.h>

#include <map>
#include <set>
#include <vector>

namespace casacore {

// Base class of statistics algorithm class hierarchy.

// The default implementation is such that statistics are only calculated when
// getStatistic() or getStatistics() is called. Until then, the iterators which
// point to the beginning of data sets, masks, etc. are held in memory. Thus,
// the caller must keep all data sets available for the statistics object until
// these methods are called, and of course, if the actual data values are changed
// between adding data and calculating statistics, the updated values are used when
// calculating statistics. Derived classes may override this behavior.
//
// PRECISION CONSIDERATIONS
// Many statistics are computed via accumulators. This can lead to precision issues,
// especially for large datasets. For this reason, it is highly recommended that the
// data type one uses as the AccumType be of higher precision, if possible, than the
// data type pointed to by input iterator. So for example, if one has a data set of
// Float values (to which the InputIterator type points to), then one should use type
// Double for the AccumType. In this case, the Float data values will be converted to
// Doubles before they are accumulated.
//
// METHODS OF PROVIDING DATA
// Data may be provided in one of two mutually exclusive ways. The first way is
// simpler, and that is to use the setData()/addData() methods. Calling setData() will
// clear any previous data that was added via these methods or via a data provider (see
// below). Calling addData() subsequently to setData() will add a data set to the set
// of data sets on which statistics will be calculated. In order for this to work
// correctly, the iterators which are passed into these methods must still be valid when
// statistics are calculated (although note that some derived classes allow certain
// statistics to be updated as data sets are added via these methods. See specific
// classes for details).
//
// The second way to provide data is via a data provider. This takes the form of
// a derived class of StatsDataProvider, in which various methods are implemented for
// retrieving various information about the data sets. Such an interface is necessary for
// data which does not easily lend itself to be provided via the setData()/addData()
// methods. For example, in the case of iterating through a lattice, a lattice iterator
// will overwrite the memory location of the previous chunk of data with the current chunk
// of data. Therefore, if one does not wish to load data from the entire lattice into
// memory (which is why LatticeIterator was designed in this way), one must the
// LatticeStatsDataProvider class, which the statistics framework will use to iteratate
// through the lattice, only keeping one chunk of the data of the lattice in memory at one
// time.
//
// QUANTILES
// A quantile is a value contained in a data set, such that, it has a zero-based
// index of ceil(q*n)-1 in the equivalent ordered dataset, where 0 < q < 1 specifies
// the fractional location within the ordered dataset and n is the total number of elements.
// Note that, for a dataset with an odd number of elements, the median is the same as
// the quantile value when q = 0.5. However, there is no such correspondance between the
// median in a dataset with an even number of elements, since the median in that case is
// given by the mean of the elements of zero-based indeces n/2-1 and n/2 in the equivalent
// ordered dataset. Thus, in the case of a dataset with an even number of values, the
// median may not even exist in the dataset, while a quantile value must exist in the
// dataset.  Note when calculating quantile values, a dataset that does not fall in
// specified dataset ranges, is not included via a stride specification, is masked, or
// has a weight of zero is not considered a member of the dataset for the pruposes of
// quantile calculations.

template <class AccumType, class InputIterator, class MaskIterator=const Bool*> class StatisticsAlgorithm {

public:

	virtual ~StatisticsAlgorithm();

	// <group>
	// Add a dataset to an existing set of datasets on which statistics are
	// to be calculated. nr is the number of points to be considered.
	// If <src>dataStride</src> is greater than 1, when <src>nrAccountsForStride</src>=True indicates
	// that the stride has been taken into account in the value of <src>nr</src>. Otherwise, it has
	// not so that the actual number of points to include is nr/dataStride if nr % dataStride == 0 or
	// (int)(nr/dataStride) + 1 otherwise.
	// If one calls this method after a data provider has been set, an exception will be
	// thrown. In this case, one should call setData(), rather than addData(), to indicate
	// that the underlying data provider should be removed.
	// <src>dataRanges</src> provide the ranges of data to include if <src>isInclude</src> is True,
	// or ranges of data to exclude if <src>isInclude</src> is False. If a datum equals the end point
	// of a data range, it is considered good (included) if <src>isInclude</src> is True, and it is
	// considered bad (excluded) if <src>isInclude</src> is False.

	virtual void addData(
		const InputIterator& first, uInt nr, uInt dataStride=1,
		Bool nrAccountsForStride=False
	);

	virtual void addData(
		const InputIterator& first, uInt nr,
		const DataRanges& dataRanges, Bool isInclude=True, uInt dataStride=1,
		Bool nrAccountsForStride=False
	);

	virtual void addData(
		const InputIterator& first, const MaskIterator& maskFirst,
		uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False, uInt maskStride=1
	);

	virtual void addData(
		const InputIterator& first, const MaskIterator& maskFirst,
		uInt nr, const DataRanges& dataRanges,
		Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
		uInt maskStride=1
	);

	virtual void addData(
		const InputIterator& first, const InputIterator& weightFirst,
		uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False
	);

	virtual void addData(
		const InputIterator& first, const InputIterator& weightFirst,
		uInt nr, const DataRanges& dataRanges,
		Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False
	);

	virtual void addData(
		const InputIterator& first, const InputIterator& weightFirst,
		const MaskIterator& maskFirst, uInt nr, uInt dataStride=1,
		Bool nrAccountsForStride=False,
		uInt maskStride=1
	);

	virtual void addData(
		const InputIterator& first, const InputIterator& weightFirst,
		const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
		Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
		uInt maskStride=1
	);
	// </group>

	// get the algorithm that this object uses for computing stats
	virtual StatisticsData::ALGORITHM algorithm() const = 0;

	// delete any (partially) sorted array
	void deleteSortedArray();

	virtual AccumType getMedian(
		CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
		CountedPtr<AccumType> knownMax=NULL, uInt binningThreshholdSizeBytes=4096*4096,
		Bool persistSortedArray=False
	) = 0;

	// The return value is the median; the quantiles are returned in the <src>quantileToValue</src> map.
	virtual AccumType getMedianAndQuantiles(
		std::map<Double, AccumType>& quantileToValue, const std::set<Double>& quantiles,
		CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
		CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	) = 0;

	// get the median of the absolute deviation about the median of the data.
	virtual AccumType getMedianAbsDevMed(
		CountedPtr<uInt64> knownNpts=NULL,
		CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	) = 0;


	// get a quantile value. quantile takes values of 0 to 1 exclusive.
	// If the dataset is greater than binningThreshholdSizeBytes bytes in size,
	// the data will not be sorted but binned. The returned value in this case is
	// only approximate.
	AccumType getQuantile(
		Double quantile, CountedPtr<uInt64> knownNpts=NULL,
		CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096,
		Bool persistSortedArray=False
	);


	// get a map of quantiles to values.
	virtual std::map<Double, AccumType> getQuantiles(
		const std::set<Double>& quantiles, CountedPtr<uInt64> npts=NULL,
		CountedPtr<AccumType> min=NULL, CountedPtr<AccumType> max=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	) = 0;

	// get the value of the specified statistic
	virtual AccumType getStatistic(StatisticsData::STATS stat);

	// certain statistics such as max and min have locations in the dataset
	// associated with them. This method gets those locations. The first value
	// in the returned pair is the zero-based dataset number that was set or
	// added. The second value is the zero-based index in that dataset. A data stride
	// of greater than one is not accounted for, so the index represents the actual
	// location in the data set, independent of the dataStride value.
	virtual std::pair<Int64, Int64> getStatisticIndex(StatisticsData::STATS stat) = 0;

	virtual StatsData<AccumType> getStatistics();

	// <group>
	// setdata() clears any current datasets or data provider and then adds the specified data set as
	// the first dataset in the (possibly new) set of data sets for which statistics are
	// to be calculated. See addData() for parameter meanings.
	virtual void setData(const InputIterator& first, uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False);

	virtual void setData(
		const InputIterator& first, uInt nr,
		const DataRanges& dataRanges, Bool isInclude=True, uInt dataStride=1,
		Bool nrAccountsForStride=False
	);

	virtual void setData(
		const InputIterator& first, const MaskIterator& maskFirst,
		uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False,
		uInt maskStride=1
	);

	virtual void setData(
		const InputIterator& first, const MaskIterator& maskFirst,
		uInt nr, const DataRanges& dataRanges,
		Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
		uInt maskStride=1
	);

	virtual void setData(
		const InputIterator& first, const InputIterator& weightFirst,
		uInt nr, uInt dataStride=1,
		Bool nrAccountsForStride=False
	);

	virtual void setData(
		const InputIterator& first, const InputIterator& weightFirst,
		uInt nr, const DataRanges& dataRanges,
		Bool isInclude=True, uInt dataStride=1,
		Bool nrAccountsForStride=False
	);

	virtual void setData(
		const InputIterator& first, const InputIterator& weightFirst,
		const MaskIterator& maskFirst, uInt nr, uInt dataStride=1,
		Bool nrAccountsForStride=False,
		uInt maskStride=1
	);

	virtual void setData(
		const InputIterator& first, const InputIterator& weightFirst,
		const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
		Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
		uInt maskStride=1
	);
	// </group>

	// instead of settng and adding data "by hand", set the data provider that will provide
	// all the data sets. Calling this method will clear any other data sets that have
	// previously been set or added.
	virtual void setDataProvider(StatsDataProvider<AccumType, InputIterator, MaskIterator> *dataProvider) {
		ThrowIf(! dataProvider, "Logic Error: data provider cannot be NULL");
		_clearData();
		_dataProvider = dataProvider;
	}

	// Provide guidance to algorithms by specifying a priori which statistics the
	// caller would like calculated.
	virtual void setStatsToCalculate(std::set<StatisticsData::STATS>& stats);

protected:
	StatisticsAlgorithm();

	// use copy semantics
	StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>& operator=(
		const StatisticsAlgorithm<AccumType, InputIterator, MaskIterator>& other
	);

	// Allows derived classes to do things after data is set or added.
	// Default implementation does nothing.
	virtual void _addData() {}

	virtual void _clearData();

	const vector<Int64>& _getCounts() const { return _counts; }

	const vector<InputIterator>& _getData() const { return _data; }

	StatsDataProvider<AccumType, InputIterator, MaskIterator>* _getDataProvider() {
		return _dataProvider;
	}

	const vector<uInt>& _getDataStrides() const { return _dataStrides; }

	const std::map<uInt, Bool>& _getIsIncludeRanges() const { return _isIncludeRanges; }

	const std::map<uInt, MaskIterator> _getMasks() const { return _masks; }

	const std::map<uInt, uInt>& _getMaskStrides() const { return _maskStrides; }

	const std::map<uInt, DataRanges>& _getRanges() const { return _dataRanges; }

	virtual AccumType _getStatistic(StatisticsData::STATS stat) = 0;

	virtual StatsData<AccumType> _getStatistics() = 0;

	const std::set<StatisticsData::STATS> _getStatsToCalculate() const {
		return _statsToCalculate;
	}

	std::vector<AccumType>& _getSortedArray() { return _sortedArray; }

	virtual const std::set<StatisticsData::STATS>& _getUnsupportedStatistics() const {
		return _unsupportedStats;
	}

	const std::map<uInt, InputIterator>& _getWeights() const {
		return _weights;
	}

	/*
	// get the zero-based indices of the specified quantiles in sorted dataset with npts
	// number of good points. The returned map maps quantiles to indices.
	static std::map<Double, uInt64> _indicesFromQuantiles(
		uInt64 npts, const std::set<Double>& quantiles
	);
	*/

	// The array can be changed by paritally sorting it up to the largest index. Return
	// a map of index to value in the sorted array.
	static std::map<uInt64, AccumType> _valuesFromArray(
		vector<AccumType>& myArray, const std::set<uInt64>& indices
	);

	void _setSortedArray(const vector<AccumType>& v) { _sortedArray = v; }

private:
	vector<InputIterator> _data;
	// maps data to weights
	std::map<uInt, InputIterator> _weights;
	// maps data to masks
	std::map<uInt, MaskIterator> _masks;
	vector<Int64> _counts;
	vector<uInt> _dataStrides;
	std::map<uInt, uInt> _maskStrides;
	std::map<uInt, Bool> _isIncludeRanges;
	std::map<uInt, DataRanges> _dataRanges;
	vector<AccumType> _sortedArray;
	std::set<StatisticsData::STATS> _statsToCalculate, _unsupportedStats;
	StatsDataProvider<AccumType, InputIterator, MaskIterator> *_dataProvider;

	void _throwIfDataProviderDefined() const;
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/StatisticsAlgorithm.tcc>
#endif

#endif
