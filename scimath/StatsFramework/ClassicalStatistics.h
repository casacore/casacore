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

#ifndef SCIMATH_CLASSICALSTATISTICSS_H
#define SCIMATH_CLASSICALSTATISTICSS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/StatsFramework/StatisticsAlgorithm.h>

#include <casacore/scimath/StatsFramework/ClassicalQuantileComputer.h>
#include <casacore/scimath/StatsFramework/StatisticsTypes.h>
#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>
#include <set>
#include <vector>
#include <utility>

namespace casacore {

template <class T> class PtrHolder;

// Class to calculate statistics in a "classical" sense, ie using accumulators
// with no special filtering beyond optional range filtering etc.
//
// setCalculateAsAdded() allows one to specify if statistics should be
// calculated and updated on upon each call to set/addData(). If False,
// statistics will be calculated only when getStatistic(), getStatistics(), or
// similar statistics computing methods are called. Setting this value to True
// allows the caller to not have to keep all the data accessible at once. Note
// however, that all data must be simultaneously accessible if quantile-like
// (eg median) calculations are desired.
//
// Objects of this class are instantiated using a ClassicalQuantileComputer
// object for computation of quantile-like statistics. See the documentation
// of StatisticsAlgorithm for details relating QuantileComputer classes.

template <class AccumType, class DataIterator, class MaskIterator=const Bool*, class WeightsIterator=DataIterator> 
class ClassicalStatistics
    : public StatisticsAlgorithm<CASA_STATP> {
public:

    ClassicalStatistics();

    // copy semantics
    ClassicalStatistics(const ClassicalStatistics& cs);

    virtual ~ClassicalStatistics();

    // copy semantics
    ClassicalStatistics& operator=(
        const ClassicalStatistics& other
    );

    // Clone this instance
    virtual StatisticsAlgorithm<CASA_STATP>* clone() const;
    
    // get the algorithm that this object uses for computing stats
    virtual StatisticsData::ALGORITHM algorithm() const {
        return StatisticsData::CLASSICAL;
    };

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
    // If one has a priori information regarding the number of points (npts) and/or
    // the minimum and maximum values of the data set, these can be supplied to
    // improve performance. Note however, that if these values are not correct, the
    // resulting median
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
    // <src>nBins</src> is the number of bins, per histogram, to use to bin the data. More
    // bins decrease the likelihood that multiple passes of the data set will be necessary, but
    // also increase the amount of memory used. If nBins is set to less than 1,000, it is
    // automatically increased to 1,000; there should be no reason to ever set nBins to be
    // this small.
    virtual AccumType getMedian(
        CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
        CountedPtr<AccumType> knownMax=NULL, uInt binningThreshholdSizeBytes=4096*4096,
        Bool persistSortedArray=False, uInt nBins=10000
    );

    // If one needs to compute both the median and quantile values, it is better to call
    // getMedianAndQuantiles() rather than getMedian() and getQuantiles() separately, as the
    // first will scan large data sets fewer times than calling the separate methods.
    // The return value is the median; the quantiles are returned in the <src>quantiles</src> map.
    // Values in the <src>fractions</src> set represent the locations in the CDF and should be
    // between 0 and 1, exclusive.
    virtual AccumType getMedianAndQuantiles(
        std::map<Double, AccumType>& quantiles, const std::set<Double>& fractions,
        CountedPtr<uInt64> knownNpts=NULL, CountedPtr<AccumType> knownMin=NULL,
        CountedPtr<AccumType> knownMax=NULL,
        uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False,
        uInt nBins=10000
    );

    // get the median of the absolute deviation about the median of the data.
    virtual AccumType getMedianAbsDevMed(
        CountedPtr<uInt64> knownNpts=NULL,
        CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
        uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False,
        uInt nBins=10000
    );

    // Get the specified quantiles. <src>fractions</src> must be between 0 and 1,
    // noninclusive.
    virtual std::map<Double, AccumType> getQuantiles(
        const std::set<Double>& fractions, CountedPtr<uInt64> knownNpts=NULL,
        CountedPtr<AccumType> knownMin=NULL, CountedPtr<AccumType> knownMax=NULL,
        uInt binningThreshholdSizeBytes=4096*4096, Bool persistSortedArray=False,
        uInt nBins=10000
    );
    // </group>

    // <group>
    // scan the dataset(s) that have been added, and find the min and max.
    // This method may be called even if setStatsToCaclulate has been called and
    // MAX and MIN has been excluded. If setCalculateAsAdded(True) has previously been
    // called after this object has been (re)initialized, an exception will be thrown.
    // The second version also determines npts in the same scan.
    virtual void getMinMax(AccumType& mymin, AccumType& mymax);

    virtual void getMinMaxNpts(uInt64& npts, AccumType& mymin, AccumType& mymax);
    // </group>

    // scan the dataset(s) that have been added, and find the number of good points.
    // This method may be called even if setStatsToCaclulate has been called and
    // NPTS has been excluded. If setCalculateAsAdded(True) has previously been
    // called after this object has been (re)initialized, an exception will be thrown.
    virtual uInt64 getNPts();

    // see base class description
    virtual std::pair<Int64, Int64> getStatisticIndex(StatisticsData::STATS stat);

    // reset object to initial state. Clears all private fields including data,
    // accumulators, etc.
    virtual void reset();

    // Should statistics be updated with calls to addData or should they only be calculated
    // upon calls to getStatistics etc? Beware that calling this will automatically reinitialize
    // the object, so that it will contain no references to data et al. after this method has
    // been called.
    virtual void setCalculateAsAdded(Bool c);

    // An exception will be thrown if setCalculateAsAdded(True) has been called.
    virtual void setDataProvider(StatsDataProvider<CASA_STATP> *dataProvider);

    // Allow derived objects to set the quantile computer object. API developers
    // shouldn't need to call this, unless they are writing derived classes
    // of ClassicalStatistics. Purposefully non-virtual. Derived classes should not
    // implement.
    void setQuantileComputer(CountedPtr<ClassicalQuantileComputer<CASA_STATP> > qc) {
        _qComputer = qc;
    }

    virtual void setStatsToCalculate(std::set<StatisticsData::STATS>& stats);

protected:

    // This constructor should be used by derived objects in order to set
    // the proper quantile computer object
    ClassicalStatistics(CountedPtr<ClassicalQuantileComputer<CASA_STATP> > qc);

    // <group>
    // scan through the data set to determine the number of good (unmasked, weight > 0,
    // within range) points. The first with no mask, no
    // ranges, and no weights is trivial with npts = nr in this class, but is implemented here
    // so that derived classes may override it.
    virtual void _accumNpts(
        uInt64& npts, const DataIterator& dataBegin,
        uInt64 nr, uInt dataStride
    ) const;

    virtual void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
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

    // <group>
    inline void _accumulate(
        StatsData<AccumType>& stats, const AccumType& datum,
        const LocationType& location
    );
 
    inline void _accumulate(
        StatsData<AccumType>& stats, const AccumType& datum,
        const AccumType& weight, const LocationType& location
    );
    // </group>

    void _addData();

    void _clearStats();

    Bool _getDoMaxMin() const { return _doMaxMin; }

    virtual StatsData<AccumType> _getInitialStats() const;
    
    virtual AccumType _getStatistic(StatisticsData::STATS stat);

    virtual StatsData<AccumType> _getStatistics();

    // Retrieve stats structure. Allows derived classes to maintain their own
    // StatsData structs.
    virtual StatsData<AccumType>& _getStatsData() { return _statsData; }

    virtual const StatsData<AccumType>& _getStatsData() const { return _statsData; }
    
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

    CountedPtr<StatisticsAlgorithmQuantileComputer<CASA_STATP> > _getQuantileComputer() {
        return _qComputer;
    }

    // <group>
    // no weights, no mask, no ranges
    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride
    );

    // no weights, no mask
    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    );

    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    );

    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    );

    // </group>
    virtual void _updateDataProviderMaxMin(
        const StatsData<AccumType>& threadStats
    );

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
    StatsData<AccumType> _statsData;
    Bool _calculateAsAdded, _doMaxMin, _mustAccumulate;

    CountedPtr<ClassicalQuantileComputer<CASA_STATP> > _qComputer;

    void _computeMinMax(
        CountedPtr<AccumType>& mymax, CountedPtr<AccumType>& mymin,
        DataIterator dataIter, MaskIterator maskIter,
        WeightsIterator weightsIter, uInt64 dataCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    void _computeMinMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymax, CountedPtr<AccumType>& mymin,
        DataIterator dataIter, MaskIterator maskIter,
        WeightsIterator weightsIter, uInt64 dataCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    void _computeNpts(
        uInt64& npts, DataIterator dataIter, MaskIterator maskIter,
        WeightsIterator weightsIter, uInt64 dataCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    void _computeStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        DataIterator dataIter, MaskIterator maskIter,
        WeightsIterator weightsIter, uInt64 count,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    // scan dataset(s) to find min and max
    void _doMinMax(AccumType& vmin, AccumType& vmax);

    uInt64 _doMinMaxNpts(AccumType& vmin, AccumType& vmax);

    uInt64 _doNpts();

    // for quantile computations, if necessary, determines npts, min, max to send
    // to quantile calculator methods
    void _doNptsMinMax(
        uInt64& mynpts, AccumType& mymin, AccumType& mymax,
        CountedPtr<uInt64> knownNpts, CountedPtr<AccumType> knownMin,
        CountedPtr<AccumType> knownMax
    );

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/ClassicalStatistics.tcc>
#endif 

#endif
