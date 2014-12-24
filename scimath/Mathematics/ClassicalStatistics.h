//# ClassicalStatistics.h: Class to calculate statistics in a "classical" sense 
//# Copyright (C) 2014
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

#ifndef SCIMATH_CLASSICALSTATS_H
#define SCIMATH_CLASSICALSTATS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/Mathematics/StatisticsAlgorithm.h>

// #include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <set>
#include <vector>
#include <utility>

namespace casacore {

// Class to calculate statistics in a "classical" sense, ie using accumulators with no
// special filtering beyond optional range filtering etc.
//
// setCalculateAsAdded() allows one to specify if statistics should be calculated and updated
// on upon each call to set/addData(). If False, statistics will be calculated only when
// getStatistic(), getStatistics(), or similar methods are called. Setting this value to True
// allows the caller to not have to keep all the data accessible at once. Note however, that all
// data must be simultaneously accessible if quantile (eg median) calculations are desired.

template <class AccumType, class InputIterator, class MaskIterator=const Bool*> class ClassicalStatistics
	: public StatisticsAlgorithm<AccumType, InputIterator, MaskIterator> {
public:

	// description of a regularly spaced bins with the first bin having lower limit
	// of minLimit and having nBins equally spaced bins of width binWidth, so that
	// the upper limit of the last bin is given by minLimit + nBins*binWidth
	struct BinDesc {
		AccumType binWidth;
		AccumType minLimit;
		uInt nBins;
	};

	ClassicalStatistics();

	virtual ~ClassicalStatistics();

	// copy semantics
	ClassicalStatistics<AccumType, InputIterator, MaskIterator>& operator=(
		const ClassicalStatistics<AccumType, InputIterator, MaskIterator>& other
	);

	// <group>
	// In the following group of methods, if the size of the composite dataset
	// is smaller than
	// <src>binningThreshholdSizeBytes</src>, the composite dataset
	// will be (perhaps partially) sorted and persisted in memory during the
	// call. In that case, and if <src>persistSortedArray</src> is True, this
	// sorted array will remain in memory after the call and will be used on
	// subsequent calls of this method when <src>binningThreshholdSizeBytes</src>
	// is greater than the size of the composite dataset. If
	// <src>persistSortedArray</src> is False, the sorted array will not be
	// stored after this call completes and so any subsequent calls for which the
	// dataset size is less than <src>binningThreshholdSizeBytes</src>, the
	// dataset will be sorted from scratch. Values which are not included due to
	// non-unity strides, are not included in any specified ranges, are masked,
	// or have associated weights of zero are not considered as dataset members
	// for quantile computations.
	// If one has a priori information regarding
	// the number of points (npts) and/or the minimum and maximum values of the data
	// set, these can be supplied to improve performance. Note however, that if these
	// values are not correct, the resulting median
	// and/or quantile values will also not be correct (although see the following notes regarding
	// max/min). Note that if this object has already had getStatistics()
	// called, and the min and max were calculated, there is no need to pass these values in
	// as they have been stored internally and used (although passing them in shouldn't hurt
	// anything). If provided, npts, the number of points falling in the specified ranges which are
	// not masked and have weights > 0, should be exactly correct. <src>min</src> can be less than
	// the true minimum, and <src>max</src> can be greater than the True maximum, but for best
	// performance, these should be as close to the actual min and max as possible.
	// In order for quantile computations to occur over multiple datasets, all datasets
	// must be available. This means that if setCalculateAsAdded()
	// was previously called by passing in a value of True, these methods will throw
	// an exception as the previous call indicates that there is no guarantee that
	// all datasets will be available. If one uses a data provider (by having called
	// setDataProvider()), then this should not be an issue.

	// get the median of the distribution.
	// For a dataset with an odd number of good points, the median is just the value
	// at index int(N/2) in the equivalent sorted dataset, where N is the number of points.
	// For a dataset with an even number of points, the median is the mean of the values at
	// indices int(N/2)-1 and int(N/2) in the sorted dataset.
	virtual AccumType getMedian(
		CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
		CountedPtr<AccumType> knownMax=NULL, uInt binningThreshholdSizeBytes=4096*4096,
		Bool persistSortedArray=False
	);

	// If one needs to compute both the median and quantile values, it is better to call
	// getMedianAndQuantiles() rather than getMedian() and getQuantiles() seperately, as the
	// first will scan large data sets fewer times than calling the seperate methods.
	// The return value is the median; the quantiles are returned in the <src>quantileToValue</src> map.
	virtual AccumType getMedianAndQuantiles(
		std::map<Double, AccumType>& quantileToValue, const std::set<Double>& quantiles,
		CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
		CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	);

	// get the median of the absolute deviation about the median of the data.
	virtual AccumType getMedianAbsDevMed(
		CountedPtr<uInt64> knownNpts=NULL,
		CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	);

	// Get the specified quantiles. <src>quantiles</src> must be between 0 and 1,
	// noninclusive.
	virtual std::map<Double, AccumType> getQuantiles(
		const std::set<Double>& quantiles, CountedPtr<uInt64> knownNpts=NULL,
		CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	);

	// </group>



	// scan the dataset(s) that have been added, and find the min and max.
	// This method may be called even if setStatsToCaclulate has been called and
	// MAX and MIN has been excluded. If setCalculateAsAdded(True) has previously been
	// called after this object has been (re)initialized, an exception will be thrown.
	virtual void getMinMax(AccumType& mymin, AccumType& mymax);

	// scan the dataset(s) that have been added, and find the number of good points.
	// This method may be called even if setStatsToCaclulate has been called and
	// NPTS has been excluded. If setCalculateAsAdded(True) has previously been
	// called after this object has been (re)initialized, an exception will be thrown.
	virtual uInt64 getNPts();

	// see base class description
	virtual std::pair<uInt, uInt> getStatisticIndex(StatisticsData::STATS stat);

	// reset object to initial state. Clears all private fields including data,
	// accumulators, etc.
	virtual void reset();

	// Should statistics be updated with calls to addData or should they only be calculated
	// upon calls to getStatistics etc? Beware that calling this will automatically reinitialize
	// the object, so that it will contain no references to data et al. after this method has
	// been called.
	virtual void setCalculateAsAdded(Bool c);

	// An exception will be thrown if setCalculateAsAdded(True) has been called.
	void setDataProvider(CountedPtr<StatsDataProvider<AccumType, InputIterator, MaskIterator> > dataProvider);

	void setStatsToCalculate(std::set<StatisticsData::STATS>& stats);

protected:

	// <group>
	// scan through the data set to determine the number of good (unmasked, weight > 0,
	// within range) points. The first with no mask, no
	// ranges, and no weights is trivial with npts = nr in this class, but is implemented here
	// so that derived classes may override it.
	inline virtual void _accumNpts(
		uInt64& npts,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride
	) const;

	virtual void _accumNpts(
		uInt64& npts,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const DataRanges& ranges, Bool isInclude
	) const;

	virtual void _accumNpts(
		uInt64& npts,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride
	) const;

	virtual void _accumNpts(
		uInt64& npts,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
		Bool isInclude
	) const;

	virtual void _accumNpts(
		uInt64& npts,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride
	) const;

	virtual void _accumNpts(
		uInt64& npts,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
	) const;

	virtual void _accumNpts(
		uInt64& npts,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude
	) const;

	virtual void _accumNpts(
			uInt64& npts,
		const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
	) const;
	// </group>

	// <group>
	inline void _accumulate(
		AccumType& mymin, AccumType& mymax, Int64& minpos, Int64& maxpos,
		const AccumType& datum , Int64 count
	);

	inline void _accumulate(
		AccumType& mymin, AccumType& mymax, Int64& minpos, Int64& maxpos, const AccumType& datum,
		const AccumType& weight, Int64 count
	);
	// </group>

	void _addData();

	void _clearData();

	void _clearStats();

	// <group>
	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const BinDesc& binDesc, AccumType maxLimit
	) const ;

	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const DataRanges& ranges, Bool isInclude,
		const BinDesc& binDesc, AccumType maxLimit
	) const;

	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
        const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride,
		const BinDesc& binDesc, AccumType maxLimit
	) const;

	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
        const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
		Bool isInclude,
		const BinDesc& binDesc, AccumType maxLimit
	) const;

	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
        const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride,
		const BinDesc& binDesc, AccumType maxLimit
	) const ;

	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
        const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
		const BinDesc& binDesc, AccumType maxLimit
	) const;

	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
        const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude,
		const BinDesc& binDesc, AccumType maxLimit
	) const;

	virtual void _findBins(
		vector<uInt64>& binCounts,
        CountedPtr<AccumType>& sameVal, Bool& allSame,
        const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const BinDesc& binDesc, AccumType maxLimit
	) const;
	// </group>

	AccumType _getStatistic(StatisticsData::STATS stat);

	Record _getStatistics();

	// <group>
	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride
	) const;

	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const DataRanges& ranges, Bool isInclude
	) const;

	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride
	) const;

	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
		Bool isInclude
	) const;

	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride
	) const;

	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
	) const;

	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude
	) const;

	virtual void _minMax(
		CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
		const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
	) const;
	// </group>

	//<group>
	// populate an unsorted array with valid data. If <src>includeLimits</src> is defined,
	// then restrict values that are entered in the array to those limits (inclusive of the
	// minimum, exclusive of the maximum). <src>maxCount</src> and <src>currentCount</src> are
	// used only if <src>includeLimits</src> is defined. In this case, the method will return
	// when currentCount == maxCount, thus avoiding scanning remaining data unnecessarily.

	// no weights, no mask, no ranges
	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;

	// ranges
	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
		uInt dataStride, const DataRanges& ranges, Bool isInclude,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;

	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin,
		uInt maskStride,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;

	// mask and ranges
	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, Int64 nr,
		uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;

	// weights
	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
		const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;

	// weights and ranges
	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
		const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
		const DataRanges& ranges, Bool isInclude,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;

	// weights and mask
	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin,
		const InputIterator& weightBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;

	// weights, mask, ranges
	virtual void _populateArray(
		vector<AccumType>& ary, uInt& currentCount, const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude,
		const std::pair<AccumType, AccumType> *const &includeLimits, uInt maxCount
	) const;
	// </group>

	// <group>
	// no weights, no mask, no ranges
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin,
		Int64 nr, uInt dataStride, uInt maxElements
	) const;

	// ranges
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
		uInt dataStride, const DataRanges& ranges, Bool isInclude,
		uInt maxElements
	) const;

	// mask
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin,
		uInt maskStride, uInt maxElements
	) const;

	// mask and ranges
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin, Int64 nr,
		uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude, uInt maxElements
	) const;

	// weights
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin,
		const InputIterator& weightBegin, Int64 nr, uInt dataStride,
		uInt maxElements
	) const;

	// weights and ranges
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin,
		const InputIterator& weightsBegin, Int64 nr, uInt dataStride,
		const DataRanges& ranges, Bool isInclude, uInt maxElements
	) const;

	// weights and mask
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin,
		const InputIterator& weightBegin, Int64 nr,
		uInt dataStride, const MaskIterator& maskBegin,
		uInt maskStride, uInt maxElements
	) const;

	// weights, mask, ranges
	virtual Bool _populateTestArray(
		vector<AccumType>& ary, const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude,
		uInt maxElements
	) const;
	// </group>

	// <group>
	// no weights, no mask, no ranges
	virtual void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride
	);

	// no weights, no mask
	virtual void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const DataRanges& ranges, Bool isInclude

	);

	virtual void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride

	);

	virtual void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude
	);
	// </group>

	// <group>
	// has weights, but no mask, no ranges
	virtual void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride
	);

	virtual void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
	);

	virtual void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
	);

	virtual void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude
	);
	// </group>


private:
	Double _npts;
	uInt _idataset;
	AccumType _mean, _nvariance, _sum, _sumsq, _sumofweights;
    CountedPtr<AccumType> _max, _min;
	// first element of pair is zero-based dataset number, second element
	// is zero-based index of location in that dataset
	std::pair<uInt, Int64> _minpos, _maxpos;
	Bool _calculateAsAdded, _doMaxMin, _doMedAbsDevMed;
	Record _currentStats;
	CountedPtr<AccumType> _median, _medAbsDevMed;

	// mutables, used to mitigate repeated code
	mutable typename vector<InputIterator>::const_iterator _dend, _diter;
	mutable vector<Int64>::const_iterator _citer;
	mutable vector<uInt>::const_iterator _dsiter;
	mutable std::map<uInt, MaskIterator> _masks;
	mutable uInt _maskStride;
	mutable std::map<uInt, InputIterator> _weights;
	mutable std::map<uInt, DataRanges> _ranges;
	mutable std::map<uInt, Bool> _isIncludeRanges;
	mutable Bool _hasMask, _hasRanges, _hasWeights, _myIsInclude;
	mutable DataRanges _myRanges;
	mutable MaskIterator _myMask;
	mutable InputIterator _myData, _myWeights;
	mutable uInt _dataCount, _myStride;
	mutable uInt64 _myCount;

	// tally the number of data points that fall into each bin provided by <src>binDesc</src>
	// Any points that are less than binDesc.minLimit or greater than
	// binDesc.minLimit + binDesc.nBins*binDesc.binWidth are not included in the counts. A data
	// point that falls exactly on a bin boundary is considered to be in the higher index bin.
    // <src>sameVal</src> will be non-null if all the good values in the histogram range are the
	// same. In that case, the value held will be the value of each of those data points.
	vector<uInt64> _binCounts(CountedPtr<AccumType>& sameVal, const BinDesc& binDesc);

	// convert in place by taking the absolute value of the difference of the vector and the median
	static void _convertToAbsDevMedArray(vector<AccumType>& myArray, AccumType median);

	// Create an unsorted array of the complete data set. If <src>includeLimits</src> is specified,
	// only points within those limits (including min but excluding max, as per definition of bins),
	// are included.
	void _createDataArray(
		vector<AccumType>& array,
		const std::pair<AccumType, AccumType> *const &includeLimits=0,
		uInt maxCount=0
	);

	// extract data from multiple bins given by binDesc that have the specified sorted dataIndices relative
	// to the lowest bin
	std::map<uInt64, AccumType> _dataFromMultipleBins(
		const BinDesc& binDesc, uInt maxArraySize,
		const std::set<uInt64>& dataIndices
	);

	// extract data from the specified bin that have the specified sorted dataIndices within
	// that bin
	std::map<uInt64, AccumType> _dataFromSingleBin(
		uInt64 binNpts, uInt maxArraySize, const std::pair<AccumType, AccumType>& binLimits,
		const std::set<uInt64>& dataIndices
	);

	void _doMinMax();

	void _doNpts();

	/*
	inline void _findBinCode(
		vector<uInt64>& binCounts, CountedPtr<AccumType>& sameVal, Bool& allSame,
		const InputIterator& datum, const BinDesc& binDesc, const AccumType& maxLimit
	) const {
		AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - *_median) : *datum;
		if (myDatum >= binDesc.minLimit && myDatum < maxLimit) {
			AccumType idx = (myDatum - binDesc.minLimit)/binDesc.binWidth;
			++binCounts[StatisticsUtilities<AccumType>::getInt(idx)];
			if (allSame) {
				if (sameVal.null()) {
					sameVal = new AccumType(myDatum);
				}
				else {
					allSame = myDatum == *sameVal;
					if (! allSame) {
						sameVal = NULL;
					}
				}
			}
		}
	}
	*/


	// get the values for the specified indices in the sorted array of all good data
	std::map<uInt64, AccumType> _indicesToValues(
		CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
		CountedPtr<AccumType> knownMax, uInt maxArraySize,
		const std::set<uInt64>& dataIndices, Bool persistSortedArray
	);

	void _initIterators();

	void _initLoopVars();

	// Determine by scanning the dataset if the number of good points is smaller than
	// <src>maxArraySize</src>. If so, <src>arrayToSort</src> will contain the unsorted
	// data values. If not, this vector will be empty.
	Bool _isNptsSmallerThan(vector<AccumType>& arrayToSort, uInt maxArraySize);

	// If <src>allowPad</src> is True, then pad the lower side of the lowest bin and the
	// higher side of the highest bin so that minData and maxData do not fall on the edge
	// of their respective bins. If false, no padding so that minData and maxData are also
	// exactly the histogram abscissa limits.
	static void _makeBins(
		BinDesc& bins, AccumType minData, AccumType maxData, uInt maxBins,
		Bool allowPad
	);

	// If input set has one value, that is the median, if it has two, the median is the average
	// of those.
	// static AccumType _medianFromSet(const std::map<uInt64, AccumType>& values);

	// get the index (for odd npts) or indices (for even npts) of the median of the sorted array.
	// If knownNpts is not null, it will be used and must be correct. If it is null, the value of
	// _npts will be used if it has been previously calculated. If not, the data sets will
	// be scanned to determine npts.
	std::set<uInt64> _medianIndices(CountedPtr<uInt64> knownNpts);




	// update min and max if necessary
    void _updateMaxMin(AccumType mymin, AccumType mymax, Int64 minpos, Int64 maxpos);
	
	// get values from sorted array if the array is small enough to be held in
	// memory. Note that this is the array containing all good data, not data in
	// just a single bin representing a subset of good data.
	// Returns True if the data were successfully retrieved.
	// If True is returned, the values map will contain a map of index to value.
	Bool _valuesFromSortedArray(
		std::map<uInt64, AccumType>& values, CountedPtr<uInt64> knownNpts,
		const std::set<uInt64>& indices, uInt maxArraySize,
		Bool persistSortedArray
	);


};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/ClassicalStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
