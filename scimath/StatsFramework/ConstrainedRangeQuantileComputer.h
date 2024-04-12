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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#ifndef SCIMATH_CONSTRAINEDRANGEQUANTILECOMPUTER_H
#define SCIMATH_CONSTRAINEDRANGEQUANTILECOMPUTER_H

#include <casacore/scimath/StatsFramework/ClassicalQuantileComputer.h>

#include <casacore/casa/aips.h>

namespace casacore {

// Basic concrete QuantileComputer class for data constrained to be
// in a specified range. Some derived classes of ConstrainedRangeStatistics
// use it. It should never be explicitly instantiated by an API developer.
// See the class documentation of StatisticsAlgorithm for details on
// QuantileComputer classes.

template <
    class AccumType, class DataIterator, class MaskIterator=const Bool*,
    class WeightsIterator=DataIterator
>
class ConstrainedRangeQuantileComputer
    : public ClassicalQuantileComputer<CASA_STATP> {
public:

    ConstrainedRangeQuantileComputer() = delete;

    ConstrainedRangeQuantileComputer(StatisticsDataset<CASA_STATP>* dataset);

    // copy semantics
    ConstrainedRangeQuantileComputer(
        const ConstrainedRangeQuantileComputer& csq
    );

    virtual ~ConstrainedRangeQuantileComputer();

    // copy semantics
    ConstrainedRangeQuantileComputer& operator=(
        const ConstrainedRangeQuantileComputer& other
    );

    // clone this object by returning a pointer to a copy
    virtual StatisticsAlgorithmQuantileComputer<CASA_STATP>* clone() const;

    // Caller is responsible for passing correct values of mynpts, mymin, and
    // mymax; no checking is done for correctness in this method.
    virtual AccumType getMedian(
        uInt64 mynpts, AccumType mymin, AccumType mymax,
        uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
    );

    // get the median of the absolute deviation about the median of the data.
    virtual AccumType getMedianAbsDevMed(
        uInt64 mynpts, AccumType mymin, AccumType mymax,
        uInt binningThreshholdSizeBytes, Bool persistSortedArray, uInt nBins
    );

    void setRange(const std::pair<AccumType, AccumType>& r) {
        _range = r;
    }

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
        const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude,
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
        const IncludeLimits& includeLimits, uInt64 maxCount
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
        DataArray& ary, const DataIterator& dataBegin, uInt64 nr,
        uInt dataStride, uInt maxElements
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

    Bool _doMedAbsDevMed{False};
    // for use in macros of often repeatedly run methods
    AccumType _myMedian{0};
    std::pair<AccumType, AccumType> _range{};

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/ConstrainedRangeQuantileComputer.tcc>
#endif 

#endif
