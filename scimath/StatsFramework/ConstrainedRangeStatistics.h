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

#ifndef SCIMATH_CONSTRAINEDRANGESTATISTICS_H
#define SCIMATH_CONSTRAINEDRANGESTATISTICS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/StatsFramework/ClassicalStatistics.h>
#include <casacore/scimath/StatsFramework/ConstrainedRangeQuantileComputer.h>

#include <set>
#include <vector>
#include <utility>

namespace casacore {

// Abstract base class for statistics algorithms which are characterized by
// a range of good values. The range is usually calculated dynamically based
// on the entire distribution. The specifics of such calculations are
// delegated to derived classes.

template <class AccumType, class DataIterator, class MaskIterator=const Bool*, class WeightsIterator=DataIterator>
class ConstrainedRangeStatistics
    : public ClassicalStatistics<CASA_STATP> {
public:

    virtual ~ConstrainedRangeStatistics();

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
        Bool persistSortedArray=False, uInt nBins=10000
    );

    // get the median of the absolute deviation about the median of the data.
    virtual AccumType getMedianAbsDevMed(
        CountedPtr<uInt64> knownNpts=NULL,
        CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
        uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False,
        uInt nBins=10000
    );

    // If one needs to compute both the median and quantile values, it is better to call
    // getMedianAndQuantiles() rather than getMedian() and getQuantiles() seperately, as the
    // first will scan large data sets fewer times than calling the seperate methods.
    // The return value is the median; the quantiles are returned in the <src>quantileToValue</src> map.
    virtual AccumType getMedianAndQuantiles(
        std::map<Double, AccumType>& quantileToValue, const std::set<Double>& quantiles,
        CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
        CountedPtr<AccumType> knownMax=NULL,
        uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False,
        uInt nBins=10000
    );

    // Get the specified quantiles. <src>quantiles</src> must be between 0 and 1,
    // noninclusive.
    virtual std::map<Double, AccumType> getQuantiles(
        const std::set<Double>& quantiles, CountedPtr<uInt64> knownNpts=NULL,
        CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
        uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False,
        uInt nBins=10000
    );
    // </group>

    // get the min and max of the data set
    virtual void getMinMax(AccumType& mymin, AccumType& mymax);

    // scan the dataset(s) that have been added, and find the number of good points.
    // This method may be called even if setStatsToCaclulate has been called and
    // NPTS has been excluded. If setCalculateAsAdded(True) has previously been
    // called after this object has been (re)initialized, an exception will be thrown.
    virtual uInt64 getNPts();

    // see base class description
    virtual std::pair<Int64, Int64> getStatisticIndex(StatisticsData::STATS stat);

    // reset object to initial state. Clears all private fields including data,
    // accumulators, global range. It does not affect the fence factor (_f), which was
    // set at object construction.
    virtual void reset();

protected:

    // Concrete derived classes are responsible for providing an appropriate
    // QuantileComputer object to the constructor, which is ultimately passed
    // up the instantiation hierarchy and stored at the StatisticsAlgorithm level.
    ConstrainedRangeStatistics(
       CountedPtr<ConstrainedRangeQuantileComputer<CASA_STATP> > qc
    );

    // copy semantics
    ConstrainedRangeStatistics(const ConstrainedRangeStatistics<CASA_STATP>& other);

    // copy semantics
    ConstrainedRangeStatistics<CASA_STATP>& operator=(
        const ConstrainedRangeStatistics<CASA_STATP>& other
    );

    // <group>
    // scan through the data set to determine the number of good (unmasked, weight > 0,
    // within range) points. The first with no mask, no
    // ranges, and no weights is trivial with npts = nr in this class, but is implemented here
    // so that derived classes may override it.
    virtual void _accumNpts(
        uInt64& npts, const DataIterator& dataStart, uInt64 nr, uInt dataStride
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataStart, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
        Bool isInclude
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    ) const;
    // </group>

    virtual AccumType _getStatistic(StatisticsData::STATS stat);

    virtual StatsData<AccumType> _getStatistics();

    // <group>
    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
        Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    ) const;
    // </group>

    // <group>
    // Sometimes we want the min, max, and npts all in one scan.
    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
        Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    ) const;
    // </group>

    // This method is purposefully non-virtual. Derived classes
    // should implement the version with no parameters.
    void _setRange(CountedPtr<std::pair<AccumType, AccumType> > r);

    // derived classes need to implement how to set their respective range
    virtual void _setRange() = 0;

    // <group>
    // no weights, no mask, no ranges
    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood,
        LocationType& location, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride
    );

    // no weights, no mask
    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood,
        LocationType& location, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const DataRanges& ranges, Bool isInclude
    );

    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood,
        LocationType& location, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    );

    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood,
        LocationType& location, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    );
    // </group>

    // <group>
    // has weights, but no mask, no ranges
    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride
    );

    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    );

    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    );

    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    );
    // </group>

private:

    // Disallow default constructor
    ConstrainedRangeStatistics();

    CountedPtr<std::pair<AccumType, AccumType> > _range;

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/ConstrainedRangeStatistics.tcc>
#endif

#endif
