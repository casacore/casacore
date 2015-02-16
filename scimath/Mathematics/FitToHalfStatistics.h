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

#ifndef SCIMATH_FITTOHALFSTATISTICS_H
#define SCIMATH_FITTOHALFSTATISTICS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/Mathematics/ConstrainedRangeStatistics.h>
#include <casacore/scimath/Mathematics/FitToHalfStatisticsData.h>

namespace casacore {

// Class to calculate statistics using the so-called fit to half algorithm. In this
// algorithm, a center value is specified, and only points greater or equal or less or equal
// this value are included. Furthermore, each of the included points is reflected about
// the center value, and these virtual points are added to the included points and
// the union of sets of included real points and virtual points are used for computing statistics.
// The specified center point is therefore the mean and median of the resulting
// distribution, and the total number of points is exactly twice the number of real
// data points that are included.

template <class AccumType, class InputIterator, class MaskIterator=const Bool*> class FitToHalfStatistics
	: public ConstrainedRangeStatistics<AccumType, InputIterator, MaskIterator> {
public:

	const static AccumType TWO;

	// <src>value</src> is only used if <src>center</src>=CVALUE
	FitToHalfStatistics(
		FitToHalfStatisticsData::CENTER center=FitToHalfStatisticsData::CMEAN,
		FitToHalfStatisticsData::USE_DATA useData=FitToHalfStatisticsData::LE_CENTER,
		AccumType value=0
	);

	virtual ~FitToHalfStatistics();

	// copy semantics
	FitToHalfStatistics<AccumType, InputIterator, MaskIterator>& operator=(
		const FitToHalfStatistics<AccumType, InputIterator, MaskIterator>& other
	);

	// get the algorithm that this object uses for computing stats
	virtual StatisticsData::ALGORITHM algorithm() const {
		return StatisticsData::FITTOHALF;
	};

	// The median is just the center value, so none of the parameters to this method are used.
	AccumType getMedian(
		CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
		CountedPtr<AccumType> knownMax=NULL, uInt binningThreshholdSizeBytes=4096*4096,
		Bool persistSortedArray=False
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

	AccumType getMedianAndQuantiles(
		std::map<Double, AccumType>& quantiles, const std::set<Double>& fractions,
		CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
		CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	);

	// get the median of the absolute deviation about the median of the data.
	AccumType getMedianAbsDevMed(
		CountedPtr<uInt64> knownNpts=NULL,
		CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	);

	// Get the specified quantiles. <src>fractions</src> must be between 0 and 1,
	// noninclusive.
	std::map<Double, AccumType> getQuantiles(
		const std::set<Double>& fractions, CountedPtr<uInt64> knownNpts=NULL,
		CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
		uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False
	);
	// </group>

	// scan the dataset(s) that have been added, and find the min and max.
	// This method may be called even if setStatsToCaclulate has been called and
	// MAX and MIN has been excluded.
	virtual void getMinMax(AccumType& mymin, AccumType& mymax);

	// scan the dataset(s) that have been added, and find the number of good points.
	// This method may be called even if setStatsToCaclulate has been called and
	// NPTS has been excluded. If setCalculateAsAdded(True) has previously been
	// called after this object has been (re)initialized, an exception will be thrown.
	uInt64 getNPts();

	// reset object to initial state. Clears all private fields including data,
	// accumulators, global range. It does not affect the fence factor (_f), which was
	// set at object construction.
	virtual void reset();

	// This class does not allow statistics to be calculated as datasets are added, so
	// an exception will be thrown if <src>c</src> is True.
	void setCalculateAsAdded(Bool c);

protected:

	virtual void _clearData();

	StatsData<AccumType> _getStatistics();

	inline StatsData<AccumType>& _getStatsData() { return _statsData; }

	inline const StatsData<AccumType>& _getStatsData() const { return _statsData; }

	// <group>
	// no weights, no mask, no ranges
	void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride
	);

	// no weights, no mask
	void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const DataRanges& ranges, Bool isInclude
	);

	void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride
	);

	void _unweightedStats(
		uInt64& ngood, AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, Int64 nr, uInt dataStride,
		const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude
	);
	// </group>

	virtual void _updateMaxMin(
		AccumType mymin, AccumType mymax, Int64 minpos,
		Int64 maxpos, uInt dataStride, const Int64& currentDataset
	);

	// <group>
	// has weights, but no mask, no ranges
	void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride
	);

	void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightsBegin,
		Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
	);

	void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
	);

	void _weightedStats(
		AccumType& mymin, AccumType& mymax,
		Int64& minpos, Int64& maxpos,
		const InputIterator& dataBegin, const InputIterator& weightBegin,
		Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
		const DataRanges& ranges, Bool isInclude
	);
	// </group>

private:
	FitToHalfStatisticsData::CENTER _centerType;
	Bool _useLower;
	AccumType _centerValue;
	StatsData<AccumType> _statsData;
	Bool _doMedAbsDevMed, _rangeIsSet;
	// these are the max and min for the real portion of the dataset
	CountedPtr<AccumType> _realMax, _realMin;

	void _getRealMinMax(
			CountedPtr<AccumType>& realMin, CountedPtr<AccumType>& realMax,
		CountedPtr<AccumType> knownMin, CountedPtr<AccumType> knownMax
	);

	void _setRange();
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/FitToHalfStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
