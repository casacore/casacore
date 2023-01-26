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

#ifndef SCIMATH_STATISTICSUTILITIES_TCC
#define SCIMATH_STATISTICSUTILITIES_TCC

#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>

#include <casacore/casa/OS/OMP.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/scimath/StatsFramework/ClassicalStatisticsData.h>

#include <iostream>

namespace casacore {

template <class AccumType>
const AccumType StatisticsUtilities<AccumType>::TWO = AccumType(2);

// For performance reasons, we ensure code is inlined rather than
// calling other functions. The performance
// benefits become important for very large datasets

#define _NLINEAR \
	npts++; \
	sum += datum; \
	mean += (datum - mean)/npts;

#define _WLINEAR \
	npts++; \
	sumweights += weight; \
	wsum += weight*datum; \
	wmean += weight/sumweights*(datum - wmean);

#define _NQUAD \
	sumsq += datum*datum; \
	auto prevMean = mean; \
	_NLINEAR \
	nvariance += (datum - prevMean)*(datum - mean);

#define _WQUAD \
	wsumsq += weight*datum*datum; \
	auto prevMean = wmean; \
	_WLINEAR \
	wnvariance += weight*(datum - prevMean)*(datum - wmean);

#define _MAXMIN \
	if (npts == 1) { \
		datamax = datum; \
		maxpos = location; \
		datamin = datum; \
		minpos = location; \
	} \
	else if (datum > datamax) { \
		datamax = datum; \
		maxpos = location; \
	} \
	else if (datum < datamin) { \
		datamin = datum; \
		minpos = location; \
	}

template <class AccumType> void StatisticsUtilities<AccumType>::accumulate (
	double& npts, AccumType& sum, AccumType& mean, const AccumType& datum
) {
	_NLINEAR
}

template <class AccumType> void StatisticsUtilities<AccumType>::waccumulate (
	double& npts, AccumType& sumweights, AccumType& wsum, AccumType& wmean,
	const AccumType& datum, const AccumType& weight
) {
	_WLINEAR
}

template <class AccumType> void StatisticsUtilities<AccumType>::accumulate (
	double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
	AccumType& sumsq, const AccumType& datum
) {
	_NQUAD
}

template <class AccumType> void StatisticsUtilities<AccumType>::waccumulate (
	double& npts, AccumType& sumweights, AccumType& wsum, AccumType& wmean,
	AccumType& wnvariance, AccumType& wsumsq, const AccumType& datum,
	const AccumType& weight
) {
	_WQUAD
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::accumulate (
	double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
	AccumType& sumsq, AccumType& datamin, AccumType& datamax,
	LocationType& minpos, LocationType& maxpos, const AccumType& datum,
	const LocationType& location
) {
	_NQUAD
	_MAXMIN
}

template <class AccumType> template <class LocationType, class DataType>
void StatisticsUtilities<AccumType>::accumulate (
    double& npts, AccumType& sum, AccumType& mean, AccumType& nvariance,
    AccumType& sumsq, DataType& datamin, DataType& datamax,
    LocationType& minpos, LocationType& maxpos, const DataType& datum,
    const LocationType& location
) {
    _NQUAD
    _MAXMIN
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::waccumulate (
	double& npts, AccumType& sumweights, AccumType& wsum, AccumType& wmean,
	AccumType& wnvariance, AccumType& wsumsq, AccumType& datamin,
	AccumType& datamax, LocationType& minpos, LocationType& maxpos,
	const AccumType& datum, const AccumType& weight,
	const LocationType& location
) {
	_WQUAD
	_MAXMIN
}

template <class AccumType> template <class LocationType>
bool StatisticsUtilities<AccumType>::doMax(
	AccumType& datamax, LocationType& maxpos, bool isFirst,
	const AccumType& datum, const LocationType& location
) {
	if (isFirst || datum > datamax) {
		datamax = datum;
		maxpos = location;
		return true;
	}
	return false;
}

template <class AccumType> template <class LocationType>
bool StatisticsUtilities<AccumType>::doMin(
	AccumType& datamin, LocationType& minpos, bool isFirst,
	const AccumType& datum, const LocationType& location
) {
	if (isFirst || datum < datamin) {
		datamin = datum;
		minpos = location;
		return true;
	}
	return false;
}

#define _NQUADSYM \
	npts += 2; \
	auto reflect = TWO*center - datum; \
	sumsq += datum*datum + reflect*reflect; \
	auto diff = datum - center; \
	nvariance += TWO*diff*diff;

#define _WQUADSYM \
	npts += 2; \
	sumweights += TWO*weight; \
	auto reflect = TWO*center - datum; \
	wsumsq += weight*(datum*datum + reflect*reflect); \
	auto diff = datum - center; \
	wnvariance += TWO*weight*diff*diff;

#define _MAXMINSYM \
	if (npts == 2) { \
		datamax = datum; \
		maxpos = location; \
		datamin = datum; \
		minpos = location; \
	} \
	else if (datum > datamax) { \
		datamax = datum; \
		maxpos = location; \
	} \
	else if (datum < datamin) { \
		datamin = datum; \
		minpos = location; \
	}

template <class AccumType> void StatisticsUtilities<AccumType>::accumulateSym (
	double& npts, AccumType& nvariance, AccumType& sumsq,
	const AccumType& datum, const AccumType& center
) {
	_NQUADSYM
}

template <class AccumType> void StatisticsUtilities<AccumType>::waccumulateSym (
	double& npts, AccumType& sumweights, AccumType& wnvariance,
	AccumType& wsumsq, const AccumType& datum, const AccumType& weight,
	const AccumType& center
) {
	_WQUADSYM
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::accumulateSym (
	double& npts, AccumType& nvariance, AccumType& sumsq, AccumType& datamin,
	AccumType& datamax, LocationType& minpos, LocationType& maxpos,
	const AccumType& datum, const LocationType& location,
	const AccumType& center
) {
	_NQUADSYM
	_MAXMINSYM
}

template <class AccumType> template <class LocationType>
void StatisticsUtilities<AccumType>::waccumulateSym (
	double& npts, AccumType& sumweights, AccumType& wnvariance,
	AccumType& wsumsq, AccumType& datamin, AccumType& datamax,
	LocationType& minpos, LocationType& maxpos, const AccumType& datum,
	const AccumType& weight, const LocationType& location,
	const AccumType& center
) {
	_WQUADSYM
	_MAXMINSYM
}

template <class AccumType>
bool StatisticsUtilities<AccumType>::includeDatum(
    const AccumType& datum, typename DataRanges::const_iterator beginRange,
    typename DataRanges::const_iterator endRange, bool isInclude
) {
    // can't use a lambda because the loop can end early via return
    for (auto iter=beginRange; iter!=endRange; ++iter) {
        if (datum >= iter->first && datum <= iter->second) {
            return isInclude;
        }
    }
    return ! isInclude;
}

template <class AccumType>
void StatisticsUtilities<AccumType>::convertToAbsDevMedArray(
    DataArray& myArray, AccumType median
) {
    for_each(myArray.begin(), myArray.end(), [median](AccumType& datum) {
        datum = abs(datum - median);
    });
}

template <class AccumType>
std::map<uint64_t, AccumType> StatisticsUtilities<AccumType>::indicesToValues(
    DataArray& myArray, const std::set<uint64_t>& indices
) {
    auto arySize = myArray.size();
    ThrowIf(
        *indices.rbegin() >= arySize,
        "Logic Error: Index " + String::toString(*indices.rbegin()) + " is too "
        "large. The sorted array has size " + String::toString(arySize)
    );
    std::map<uint64_t, AccumType> indexToValuesMap;
    uint64_t lastIndex = 0;
    for_each(
        indices.cbegin(), indices.cend(),
        [&myArray, &lastIndex, &arySize](uint64_t index) {
        GenSort<AccumType>::kthLargest(
            &myArray[lastIndex], arySize - lastIndex, index - lastIndex
        );
        lastIndex = index;
    });
    for_each(
        indices.cbegin(), indices.cend(),
        [&myArray, &indexToValuesMap](uint64_t index) {
        indexToValuesMap[index] = myArray[index];
    });
    return indexToValuesMap;
}

template <class AccumType>
void StatisticsUtilities<AccumType>::mergeResults(
    std::vector<BinCountArray>& bins,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const PtrHolder<std::vector<BinCountArray>>& tBins,
    const PtrHolder<std::vector<CountedPtr<AccumType>>>& tSameVal,
    const PtrHolder<std::vector<bool>>& tAllSame, uint32_t nThreadsMax
) {
    // merge results from individual threads (tBins, tSameVal, tAllSame)
    // into single data structures (bins, sameVal, allSame)
    for (uint32_t tid=0; tid<nThreadsMax; ++tid) {
        auto idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        auto titer = tBins[idx8].cbegin();
        for_each(bins.begin(), bins.end(), [&titer](BinCountArray& bcArray) {
            std::transform(
                bcArray.begin(), bcArray.end(), titer->begin(),
                bcArray.begin(), std::plus<int64_t>()
            );
            ++titer;
        });
        //typename std::vector<CountedPtr<AccumType> >::iterator siter;
        //auto send = sameVal.end();
        std::vector<bool>::iterator aiter = allSame.begin();
        auto viter = tSameVal[idx8].cbegin();
        auto witer = tAllSame[idx8].cbegin();
        for_each(
            sameVal.begin(), sameVal.end(),
            [&aiter, &viter, &witer](CountedPtr<AccumType>& svalue) {
            if (! *aiter) {
                // won't have the same values, do nothing
            }
            if (*witer && *aiter) {
                if (
                    viter->null()
                    || (! svalue.null() && *svalue == *(*viter))
                ) {
                    // no unflagged values in this chunk or both
                    // have the all the same values, do nothing
                }
                else if (svalue.null()) {
                    svalue.reset(new AccumType(*(*viter)));
                }
                else {
                    // both are not null, and they do not have the same values
                    svalue.reset();
                    *aiter = false;
                }
            }
            else {
                // *aiter = true, *witer = false, all values are not the same
                svalue.reset();
                *aiter = false;
            }
            ++aiter;
            ++viter;
            ++witer;
        });
    }
}

template <class AccumType>
StatsData<AccumType> StatisticsUtilities<AccumType>::combine(
    const std::vector<StatsData<AccumType>>& stats
) {
    auto n = stats.size();
    auto res = n == 1 ? stats[0] : initializeStatsData<AccumType>();
    if (n == 0) {
        // null set
        return res;
    }
    static const AccumType zero = 0;
    static const AccumType one = 1;
    if (n > 1) {
        for_each(
            stats.cbegin(), stats.cend(),
            [&res](const StatsData<AccumType>& s) {
            if (! s.max.null() && (res.max.null() || *(s.max) > *res.max)) {
                // pointer copy
                res.max = s.max;
                res.maxpos = s.maxpos;
            }
            if (! s.min.null() && (res.min.null() || *(s.min) < *res.min)) {
                // pointer copy
                res.min = s.min;
                res.minpos = s.minpos;
            }
            auto sumweights = s.sumweights + res.sumweights;
            auto mean = sumweights == zero ? zero
                : (s.sumweights*s.mean + res.sumweights*res.mean)/sumweights;
            auto nvariance = zero;
            if (sumweights > zero) {
                auto diff1 = s.mean - mean;
                auto diff2 = res.mean - mean;
                nvariance = s.nvariance + res.nvariance
                    + s.sumweights*diff1*diff1 + res.sumweights*diff2*diff2;
            }
            res.masked = s.masked || res.masked;
            res.mean = mean;
            res.npts += s.npts;
            res.nvariance = nvariance;
            res.sum += s.sum;
            res.sumsq += s.sumsq;
            res.sumweights = sumweights;
            res.weighted = s.weighted || res.weighted;
        });
    }
    // In the n = 1 case, the stats which are computed from other stats are
    // not guaranteed to be in stats[0], so compute and fill them here, also
    // compute them for the n > 1 case
    // in any reasonable statistical dataset, sumsq should be zero if
    // sumweights is 0
    res.variance = res.sumweights > one
        ? res.nvariance/(res.sumweights - one) : 0;
    res.rms = res.sumweights == zero ? zero : sqrt(res.sumsq/res.sumweights);
    res.stddev = sqrt(res.variance);
    return res;
}

template <class AccumType>
template <class DataIterator, class MaskIterator, class WeightsIterator>
uint32_t StatisticsUtilities<AccumType>::nThreadsMax(
    const StatsDataProvider<CASA_STATP> *const dataProvider
) {
    auto nthr = OMP::nMaxThreads();
    if (nthr > 1 && dataProvider) {
        auto n = dataProvider->getNMaxThreads();
        if (n > 0) {
            return n;
        }
    }
    return nthr;
}

template <class AccumType>
uint32_t StatisticsUtilities<AccumType>::threadIdx() {
#ifdef _OPENMP
    uint32_t tid = omp_get_thread_num();
#else
    uint32_t tid = 0;
#endif
    return tid * ClassicalStatisticsData::CACHE_PADDING;
}

}

#endif
