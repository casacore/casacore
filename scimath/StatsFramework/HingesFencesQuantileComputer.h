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

#ifndef SCIMATH_HINGESFENCESQUANTILECOMPUTER_H
#define SCIMATH_HINGESFENCESQUANTILECOMPUTER_H

#include <casacore/scimath/StatsFramework/ConstrainedRangeQuantileComputer.h>

#include <casacore/scimath/StatsFramework/StatisticsTypes.h>

#include <casacore/casa/aips.h>

namespace casacore {

// QuantileComputer used by HingesFencesStatistics for computing quantile-like
// statistics. API developers should never explicitly instantiate this class.
// See class documentation for StatisticsAlgorithm for details regarding
// QuantileComputer classes.

template <
    class AccumType, class DataIterator, class MaskIterator=const bool*,
    class WeightsIterator=DataIterator
>
class HingesFencesQuantileComputer
    : public ConstrainedRangeQuantileComputer<CASA_STATP> {
public:

    HingesFencesQuantileComputer() = delete;

    HingesFencesQuantileComputer(StatisticsDataset<CASA_STATP>* dataset);

    // copy semantics
    HingesFencesQuantileComputer(const HingesFencesQuantileComputer& other);

    virtual ~HingesFencesQuantileComputer();

    // copy semantics
    HingesFencesQuantileComputer& operator=(
        const HingesFencesQuantileComputer& other
    );

    // clone this object by returning a pointer to a copy
    virtual StatisticsAlgorithmQuantileComputer<CASA_STATP>* clone() const;

    // reset private fields
    virtual void reset();

    void setHasRange(bool hr) { _hasRange = hr; }

protected:

    // <group>
    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const ;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;
    // </group>

    //<group>
    // populate an unsorted array with valid data. If <src>includeLimits</src>
    // is defined, then restrict values that are entered in the array to those
    // limits (inclusive of the minimum, exclusive of the maximum).
    // <src>maxCount</src> and <src>currentCount</src> are used only if
    // <src>includeLimits</src> is defined. In this case, the method will return
    // when currentCount == maxCount, thus avoiding scanning remaining data
    // unnecessarily.

    // no weights, no mask, no ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride
    ) const;

    // ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride
    ) const;

    // mask and ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    // weights
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
    ) const;

    // weights and ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    // weights and mask
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;

    // weights, mask, ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    // no weights, no mask, no ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const IncludeLimits& includeLimits,
        uint64_t maxCount
    ) const;

    // ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // mask and ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // weights
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const IncludeLimits& includeLimits,
        uint64_t maxCount
    ) const;

    // weights and ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // weights and mask
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;

    // weights, mask, ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uint64_t& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude,
        const IncludeLimits& includeLimits, uint64_t maxCount
    ) const;
    // </group>

    // <group>
    // no weights, no mask, no ranges
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, uint32_t maxElements
    ) const;

    // ranges
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const DataRanges& ranges, bool isInclude,
        uint32_t maxElements
    ) const;

    // mask
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        uint32_t maxElements
    ) const;

    // mask and ranges
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude, uint32_t maxElements
    ) const;

    // weights
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        uint32_t maxElements
    ) const;

    // weights and ranges
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude, uint32_t maxElements
    ) const;

    // weights and mask
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride, uint32_t maxElements
    ) const;

    // weights, mask, ranges
    virtual bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude, uint32_t maxElements
    ) const;
    // </group>

private:

    bool _hasRange{false};

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/HingesFencesQuantileComputer.tcc>
#endif 

#endif
