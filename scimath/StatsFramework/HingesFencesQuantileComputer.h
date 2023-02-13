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
    class AccumType, class DataIterator, class MaskIterator=const Bool*,
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

    void setHasRange(Bool hr) { _hasRange = hr; }

protected:

    // <group>
    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const ;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin,
        uInt maskStride, const DataRanges& ranges, Bool isInclude,
        const std::vector<StatsHistogram<AccumType>>& binDesc,
        const std::vector<AccumType>& maxLimit
    ) const;

    virtual void _findBins(
        std::vector<BinCountArray>& binCounts,
        std::vector<std::shared_ptr<AccumType>>& sameVal, std::vector<Bool>& allSame,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin,
        uInt maskStride, const std::vector<StatsHistogram<AccumType>>& binDesc,
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
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride
    ) const;

    // ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    ) const;

    // mask and ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    // weights
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
    ) const;

    // weights and ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    // weights and mask
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    ) const;

    // weights, mask, ranges
    virtual void _populateArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    // no weights, no mask, no ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const IncludeLimits& includeLimits,
        uInt64 maxCount
    ) const;

    // ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude,
        const IncludeLimits& includeLimits, uInt64 maxCount
    ) const;

    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const IncludeLimits& includeLimits, uInt64 maxCount
    ) const;

    // mask and ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude,
        const IncludeLimits& includeLimits, uInt64 maxCount
    ) const;

    // weights
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const IncludeLimits& includeLimits,
        uInt64 maxCount
    ) const;

    // weights and ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
        const IncludeLimits& includeLimits, uInt64 maxCount
    ) const;

    // weights and mask
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin,
        uInt maskStride, const IncludeLimits& includeLimits, uInt64 maxCount
    ) const;

    // weights, mask, ranges
    virtual void _populateArrays(
        std::vector<DataArray>& arys, uInt64& currentCount,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uInt64 nr, uInt dataStride, const MaskIterator& maskBegin,
        uInt maskStride, const DataRanges& ranges, Bool isInclude,
        const IncludeLimits& includeLimits, uInt64 maxCount
    ) const;
    // </group>

    // <group>
    // no weights, no mask, no ranges
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        uInt64 nr, uInt dataStride, uInt maxElements
    ) const;

    // ranges
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const DataRanges& ranges, Bool isInclude,
        uInt maxElements
    ) const;

    // mask
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        uInt maxElements
    ) const;

    // mask and ranges
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude, uInt maxElements
    ) const;

    // weights
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uInt64 nr, uInt dataStride,
        uInt maxElements
    ) const;

    // weights and ranges
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude, uInt maxElements
    ) const;

    // weights and mask
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
    ) const;

    // weights, mask, ranges
    virtual Bool _populateTestArray(
        DataArray& ary, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uInt64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude, uInt maxElements
    ) const;
    // </group>

private:

    Bool _hasRange{False};

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/HingesFencesQuantileComputer.tcc>
#endif 

#endif
