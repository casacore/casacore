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

#ifndef SCIMATH_CLASSICALQUANTILECOMPUTER_H
#define SCIMATH_CLASSICALQUANTILECOMPUTER_H

#include <casacore/scimath/StatsFramework/StatisticsAlgorithmQuantileComputer.h>

#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>

#include <casacore/casa/aips.h>

#include <map>
#include <set>
#include <utility>
#include <vector>

namespace casacore {

// This class is used internally by ClassicalStatistics objects. It should never
// be explicitly instantiated by an API developer. See the documentation of
// StatisticsAlgorithm for details regarding QuantileComputer classes.

template <
    class AccumType, class DataIterator, class MaskIterator=const bool*,
    class WeightsIterator=DataIterator
> class ClassicalQuantileComputer
    : public StatisticsAlgorithmQuantileComputer<CASA_STATP> {

    using LimitPair = std::pair<AccumType, AccumType>;
    using LimitPairVectorIter = typename std::vector<LimitPair>::const_iterator;
    using IndexValueMap = typename std::map<uint64_t, AccumType>;
    using IndexSet = std::set<uint64_t>;

public:

    ClassicalQuantileComputer() = delete;

    ClassicalQuantileComputer(StatisticsDataset<CASA_STATP>* dataset);

    // copy semantics
    ClassicalQuantileComputer(const ClassicalQuantileComputer& other);

    virtual ~ClassicalQuantileComputer();

    // copy semantics
    ClassicalQuantileComputer& operator=(
        const ClassicalQuantileComputer& other
    );

    // clone this object by returning a pointer to a copy
    virtual StatisticsAlgorithmQuantileComputer<CASA_STATP>* clone() const;

    // Caller is responsible for passing correct values of mynpts, mymin, and
    // mymax; no checking is done for correctness in this method.
    virtual AccumType getMedian(
        uint64_t mynpts, AccumType mymin, AccumType mymax,
        uint32_t binningThreshholdSizeBytes, bool persistSortedArray, uint32_t nBins
    );

    // get the median of the absolute deviation about the median of the data.
    virtual AccumType getMedianAbsDevMed(
        uint64_t mynpts, AccumType mymin, AccumType mymax,
        uint32_t binningThreshholdSizeBytes, bool persistSortedArray, uint32_t nBins
    );

    // If one needs to compute both the median and QuantileComputer values, it
    // is better to call getMedianAndQuantiles() rather than getMedian() and
    // getQuantiles() separately, as the first will scan large data sets fewer
    // times than calling the separate methods. The return value is the median;
    // the quantiles are returned in the <src>quantiles</src> map. Values in the
    // <src>fractions</src> set represent the locations in the CDF and should be
    // between 0 and 1, exclusive.
    virtual AccumType getMedianAndQuantiles(
        std::map<double, AccumType>& quantiles,
        const std::set<double>& fractions, uint64_t mynpts, AccumType mymin,
        AccumType mymax, uint32_t binningThreshholdSizeBytes,
        bool persistSortedArray, uint32_t nBins
    );

    // Get the specified Quantiles. <src>fractions</src> must be between 0 and
    // 1, noninclusive.
    virtual std::map<double, AccumType> getQuantiles(
        const std::set<double>& fractions, uint64_t mynpts, AccumType mymin,
        AccumType mymax, uint32_t binningThreshholdSizeBytes,
        bool persistSortedArray, uint32_t nBins
    );

    // reset the private fields
    virtual void reset();

protected:

    // <group>
    // Get the counts of data within the specified histogram bins. The number of
    // arrays within binCounts will be equal to the number of histograms in
    // <src>hist</src>. Each array within <src>binCounts</src> will have the
    // same number of elements as the number of bins in its corresponding
    // histogram in <src>hist</src>.
    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const ;

    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<std::vector<uint64_t> >& binCounts,
        std::vector<CountedPtr<AccumType> >& sameVal,
        std::vector<bool>& allSame, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const std::vector<StatsHistogram<AccumType> >& hist,
        const std::vector<AccumType>& maxLimit
    ) const;
    // </group>

    //<group>
    // populate an unsorted array with valid data.
    // no weights, no mask, no ranges
    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride
    ) const;

    // ranges
    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride
    ) const;

    // mask and ranges
    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    // weights
    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
    ) const;

    // weights and ranges
    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    // weights and mask
    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;

    // weights, mask, ranges
    virtual void _populateArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;
    // </group>

    // <group>
    // Create a std::vector of unsorted arrays, one array for each bin defined
    // by <src>includeLimits</src>. <src>includeLimits</src> should be
    // non-overlapping and should be given in ascending order (the algorithm
    // used assumes this). Once the sum of the lengths of all arrays equals
    // <src>maxCount</src> the method will return with no further processing.
    // no weights, no mask, no ranges
    virtual void _populateArrays(
        std::vector<std::vector<AccumType>>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // ranges
    virtual void _populateArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    virtual void _populateArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // mask and ranges
    virtual void _populateArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // weights
    virtual void _populateArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const IncludeLimits& includeLimits,
        uint64_t maxCount
    ) const;

    // weights and ranges
    virtual void _populateArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // weights and mask
    virtual void _populateArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // weights, mask, ranges
    virtual void _populateArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;
    // </group>

    // <group>
    // no weights, no mask, no ranges
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, uint32_t maxElements
    ) const;

    // ranges
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const DataRanges& ranges, bool isInclude,
        uint32_t maxElements
    ) const;

    // mask
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, uint32_t maxElements
    ) const;

    // mask and ranges
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude, uint32_t maxElements
    ) const;

    // weights
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        uint32_t maxElements
    ) const;

    // weights and ranges
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude, uint32_t maxElements
    ) const;

    // weights and mask
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride, uint32_t maxElements
    ) const;

    // weights, mask, ranges
    virtual bool _populateTestArray(
        std::vector<AccumType>& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude, uint32_t maxElements
    ) const;
    // </group>

    // get values from sorted array if the array is small enough to be held in
    // memory. Note that this is the array containing all good data, not data in
    // just a single bin representing a subset of good data.
    // Returns true if the data were successfully retrieved.
    // If true is returned, the values map will contain a map of index to value.
    // It is the caller's responsibility to check that <src>mynpts</src> is not
    // 0; no checking is done here.
    bool _valuesFromSortedArray(
        std::map<uint64_t, AccumType>& values, uint64_t mynpts,
        const std::set<uint64_t>& indices, uint64_t maxArraySize,
        bool persistSortedArray
    );

private:

    bool _doMedAbsDevMed{false};
    // for use in often repeatedly run macros
    AccumType _myMedian{0};

    // tally the number of data points that fall into each bin provided by
    // <src>hist</src>. Any points that are less than hist.minLimit or greater
    // than hist.minLimit + hist.nBins*hist.binWidth are not included in the
    // counts. A data point that falls exactly on a bin boundary is considered
    // to be in the higher index bin. <src>sameVal</src> will be non-null if all
    // the good values in the histogram range are the same. In that case, the
    // value held will be the value of each of those data points.
    std::vector<std::vector<uint64_t> > _binCounts(
        std::vector<CountedPtr<AccumType> >& sameVal,
        const std::vector<StatsHistogram<AccumType> >& hist
    );

    void _computeBins(
        std::vector<std::vector<uint64_t>>& bins,
        std::vector<CountedPtr<AccumType>>& sameVal,
        std::vector<bool>& allSame, DataIterator dataIter,
        MaskIterator maskIter, WeightsIterator weightsIter, uint64_t count,
        const std::vector<StatsHistogram<AccumType>>& hist,
        const std::vector<AccumType>& maxLimit,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    void _computeDataArray(
        std::vector<AccumType>& ary, DataIterator dataIter,
        MaskIterator maskIter, WeightsIterator weightsIter, uint64_t dataCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    void _computeDataArrays(
        std::vector<std::vector<AccumType> >& arys, uint64_t& currentCount,
        DataIterator dataIter, MaskIterator maskIter,
        WeightsIterator weightsIter, uint64_t dataCount,
        const IncludeLimits& includeLimits, uint64_t maxCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    // Create an unsorted array of the complete data set. If
    // <src>includeLimits</src> is specified, only points within those limits
    // (including min but excluding max, as per definition of bins), are
    // included.
    void _createDataArray(std::vector<AccumType>& array);

    void _createDataArrays(
        std::vector<std::vector<AccumType>>& arrays,
        const IncludeLimits& includeLimits, uint64_t maxCount
    );

    // extract data from multiple histograms given by <src>hist</src>.
    // <src>dataIndices</src> represent the indices of the sorted arrays of
    // values to extract. There should be exactly one set of data indices to
    // extract for each supplied histogram. The data indices are relative to the
    // minimum value of the minimum bin in their respective histograms. The
    // ordering of the maps in the returned std::vector represent the ordering
    // of histograms in <src>hist</src>. <src>hist</src> should contain
    // non-overlapping histograms and the histograms should be specified in
    // ascending order.
    std::vector<IndexValueMap> _dataFromMultipleBins(
        const std::vector<StatsHistogram<AccumType> >& hist,
        uint64_t maxArraySize, const std::vector<IndexSet>& dataIndices,
        uint32_t nBins
    );

    std::vector<IndexValueMap> _dataFromSingleBins(
        const std::vector<uint64_t>& binNpts, uint64_t maxArraySize,
        const IncludeLimits& binLimits,
        const std::vector<IndexSet>& dataIndices, uint32_t nBins
    );

    // get the values for the specified indices in the sorted array of all good
    // data
    IndexValueMap _indicesToValues(
        uint64_t mynpts, AccumType mymin, AccumType mymax, uint64_t maxArraySize,
        const IndexSet& dataIndices, bool persistSortedArray, uint32_t nBins
    );

    // get the index (for odd npts) or indices (for even npts) of the median of
    // the sorted array.
    static IndexSet _medianIndices(uint64_t mynpts);

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/ClassicalQuantileComputer.tcc>
#endif 

#endif
