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

#ifndef SCIMATH_STATISTICSUTILITIES_H
#define SCIMATH_STATISTICSUTILITIES_H

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/StatsFramework/StatisticsTypes.h>
#include <casacore/scimath/StatsFramework/StatsHistogram.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/aips.h>

#include <iostream>
#include <casacore/casa/iosfwd.h>

namespace casacore {

template <class T> class PtrHolder;

CASA_STATD class StatsDataProvider;

// Various statistics related methods for the statistics framework.

template <class AccumType> class StatisticsUtilities {
public:

    /*
	// description of a regularly spaced bins with the first bin having lower limit
	// of minLimit and having nBins equally spaced bins of width binWidth, so that
	// the upper limit of the last bin is given by minLimit + nBins*binWidth
	struct BinDesc {
		AccumType binWidth;
		AccumType minLimit;
		uInt nBins;
		// Carrying along the bin boundaries is necessary because precision
		// issues of different ways of computing them can cause discrepancies,
		// leading to accounting errors. These values are the max limits for
		// each bin.
		std::vector<AccumType> maxLimits;
	};
	*/

	~StatisticsUtilities() {}

	// <group>
	// accumulate values. It is the responsibility of the caller to keep track
	// of the accumulated values after each call. This class does not since it
	// has no state.
	// The accumulation derivation for mean and variance can be found at
	// www.itl.nist.gov/div898/software/dataplot/refman2/ch2/weighvar.pdf
	// nvariance is an accumulated value. It is related to the variance via
	// variance = nvariance/npts or nvariance/(npts-1) depending on your preferred definition
	// in the non-weighted case and
	// wvariance = wnvariance/sumofweights or wnvariance/(sumofweights-1) in the weighted case
	// It's basic definition is nvariance = sum((x_i - mean)**2),
	// wnvariance = sum((weight_i*(x_i - mean)**2)
	// npts is a Double rather than an Int64 because of compilation issues when T is a Complex
	inline static void accumulate (
		Double& npts, AccumType& sum, AccumType& mean, const AccumType& datum
	);

	// in order to optimize performance, no checking is done for the weight == 0 case
	// callers should ensure that the weigth is not zero before calling this method,
	// and shouldn't call this method if the weight is 0. Expect a segfault because of
	// division by zero if sumweights and weight are both zero.
	inline static void waccumulate (
		Double& npts, AccumType& sumweights, AccumType& wsum, AccumType& wmean,
		const AccumType& datum, const AccumType& weight
	);

	inline static void accumulate (
		Double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
		AccumType& sumsq, const AccumType& datum
	);

	// wsumsq is the weighted sum of squares, sum(w_i*x_i*x_i)
	inline static void waccumulate (
		Double& npts, AccumType& sumweights, AccumType& wsum,
		AccumType& wmean, AccumType& wnvariance, AccumType& wsumsq,
		const AccumType& datum, const AccumType& weight
	);
	// </group>

	// <group>
	// The assignment operator of class LocationType should use copy, not reference,
	// semantics.
	template <class LocationType>
	inline static void accumulate (
		Double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
		AccumType& sumsq, AccumType& datamin,
		AccumType& datamax, LocationType& minpos, LocationType& maxpos,
		const AccumType& datum, const LocationType& location
	);

	template <class LocationType, class DataType>
	inline static void accumulate (
	    Double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
	    AccumType& sumsq, DataType& datamin,
	    DataType& datamax, LocationType& minpos, LocationType& maxpos,
	    const DataType& datum, const LocationType& location
	);

	template <class LocationType>
	inline static void waccumulate (
		Double& npts, AccumType& sumofweights, AccumType& sum, AccumType& mean,
		AccumType& nvariance, AccumType& sumsq, AccumType& datamin, AccumType& datamax,
		LocationType& minpos, LocationType& maxpos,
		const AccumType& datum, const AccumType& weight, const LocationType& location
	);
	// </group>

	// <group>
	// return True if the max or min was updated, False otherwise.
	template <class LocationType>
	inline static Bool doMax(
		AccumType& datamax, LocationType& maxpos, Bool isFirst,
		const AccumType& datum, const LocationType& location
	);

	template <class LocationType>
	inline static Bool doMin(
		AccumType& datamin, LocationType& minpos, Bool isFirst,
		const AccumType& datum, const LocationType& location
	);
	// </group>

	// <group>
	// These versions are for symmetric accumulation about a specified center
	// point. The actual point is accumulated, as is a "virtual" point that is
	// symmetric about the specified center. Of course, the trivial relationship
	// that the mean is the specified center is used to simplify things.
	inline static void accumulateSym (
		Double& npts, AccumType& nvariance,
		AccumType& sumsq, const AccumType& datum, const AccumType& center
	);

	// wsumsq is the weighted sum of squares, sum(w_i*x_i*x_i)
	inline static void waccumulateSym (
		Double& npts, AccumType& sumweights,
		AccumType& wnvariance, AccumType& wsumsq,
		const AccumType& datum, const AccumType& weight, const AccumType& center
	);

	// <src>maxpos</src> and <src>minpos</src> refer to actual, not
	// virtually created, data only.
	template <class LocationType>
	inline static void accumulateSym (
		Double& npts, AccumType& nvariance,
		AccumType& sumsq, AccumType& datamin,
		AccumType& datamax, LocationType& minpos, LocationType& maxpos,
		const AccumType& datum, const LocationType& location, const AccumType& center
	);

	template <class LocationType>
	inline static void waccumulateSym (
		Double& npts, AccumType& sumofweights,
		AccumType& nvariance, AccumType& sumsq, AccumType& datamin, AccumType& datamax,
		LocationType& minpos, LocationType& maxpos,
		const AccumType& datum, const AccumType& weight, const LocationType& location,
		const AccumType& center
	);

    // convert in place by taking the absolute value of the difference of the std::vector and the median
    inline static void convertToAbsDevMedArray(std::vector<AccumType>& myArray, AccumType median);
	// </group>

	inline static Bool includeDatum(
		const AccumType& datum, typename DataRanges::const_iterator beginRange,
		typename DataRanges::const_iterator endRange, Bool isInclude
	);


    // The array can be changed by partially sorting it up to the largest index. Return
    // a map of index to value in the sorted array.
    static std::map<uInt64, AccumType> indicesToValues(
        std::vector<AccumType>& myArray, const std::set<uInt64>& indices
    );

    static void mergeResults(
        std::vector<std::vector<uInt64> >& bins, std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<Bool>& allSame, const PtrHolder<std::vector<std::vector<uInt64> > >& tBins,
        const PtrHolder<std::vector<CountedPtr<AccumType> > >& tSameVal,
        const PtrHolder<std::vector<Bool> >& tAllSame, uInt nThreadsMax
    );

    // use two statistics sets to get the statistics set that would
    // result in combining the two data sets used to produce the
    // individual statistics sets. The quantile related stats are
    // not considered, since it is not in general possible to determine
    // the resultant quantiles from the information provided; only
    // the aggregate statistics make sense.
    static StatsData<AccumType> combine(
        const std::vector<StatsData<AccumType> >& stats
    );

    template <class DataIterator, class MaskIterator, class WeightsIterator>
    static uInt nThreadsMax(const StatsDataProvider<CASA_STATP> *const dataProvider);

    static uInt threadIdx();

private:

	const static AccumType TWO;

	StatisticsUtilities() {}

};
/*
// for use in debugging
template <class T>
ostream &operator<<(ostream &os, const typename StatisticsUtilities<T>::BinDesc &desc) {
	os << "min limit " << desc.minLimit << " bin width " << desc.binWidth
		<< " nbins " << desc.nBins;
	return os;
}
*/

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/StatisticsUtilities.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
