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

#ifndef SCIMATH_HINGESFENCESSTATISTICS_H
#define SCIMATH_HINGESFENCESSTATISTICS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/StatsFramework/ConstrainedRangeStatistics.h>

#include <set>
#include <vector>
#include <utility>

namespace casacore {

// Class to calculate statistics using the so-called hinges and fences
// algorithm. In this algorithm, the data on which the statistics are computed
// from is limited to the range of values between Q1 - f*D and Q3 + f*D,
// inclusive, where D = Q3 - Q1 and Q1 and Q3 are the first and third quartiles,
// respectively.
//
// This class uses a HingesFencesQuantileComputer object for computing quantile-
// like statistics. See class documentation for StatisticsAlgorithm for details
// regarding QuantileComputer classes.

template <
    class AccumType, class DataIterator, class MaskIterator=const bool *,
    class WeightsIterator=DataIterator
>
class HingesFencesStatistics
    : public ConstrainedRangeStatistics<CASA_STATP> {
public:

    // If <src>f</src> is negative, the full dataset is used; ie the object has
    // the same behavior as a ClassicalStatistics object
    HingesFencesStatistics(double f=-1.0);

    // copy semantics
    HingesFencesStatistics(const HingesFencesStatistics<CASA_STATP>& other);

    virtual ~HingesFencesStatistics();

    // copy semantics
    HingesFencesStatistics<CASA_STATP>& operator=(
        const HingesFencesStatistics<CASA_STATP>& other
    );

    // Clone this instance. Caller is responsible for deleting the returned
    // pointer.
    virtual StatisticsAlgorithm<CASA_STATP>* clone() const;
    
    // get the algorithm that this object uses for computing stats
    virtual StatisticsData::ALGORITHM algorithm() const {
        return StatisticsData::HINGESFENCES;
    };

    // reset object to initial state. Clears all private fields including data,
    // accumulators, global range. It does not affect the fence factor (_f),
    // which was set at object construction.
    virtual void reset();

    // This class does not allow statistics to be calculated as datasets are
    // added, so an exception will be thrown if <src>c</src> is true.
    void setCalculateAsAdded(bool c);

protected:
    // <group>
    // scan through the data set to determine the number of good (unmasked,
    // weight > 0, within range) points. The first with no mask, no ranges, and
    // no weights is trivial with npts = nr in this class, but is implemented
    // here so that derived classes may override it.
    virtual void _accumNpts(
        uint64_t& npts, const DataIterator& dataStart, uint64_t nr, uint32_t dataStride
    ) const;

    virtual void _accumNpts(
        uint64_t& npts, const DataIterator& dataStart, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _accumNpts(
        uint64_t& npts, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;

    virtual void _accumNpts(
        uint64_t& npts, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _accumNpts(
        uint64_t& npts, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
    ) const;

    virtual void _accumNpts(
        uint64_t& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _accumNpts(
        uint64_t& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _accumNpts(
        uint64_t& npts, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;
    // </group>

    // <group>
    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride
    ) const;

    // <group>
    // Sometimes we want the min, max, and npts all in one scan.
    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride
    ) const;

    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride
    ) const;

    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin, uint64_t nr,
        uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
    ) const;

    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uint64_t& npts, CountedPtr<AccumType>& mymin,
        CountedPtr<AccumType>& mymax, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;
    // </group>

    // <group>
    // no weights, no mask, no ranges
    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uint64_t& ngood, LocationType& location,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride
    );

    // no weights, no mask
    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uint64_t& ngood, LocationType& location,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    );

    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uint64_t& ngood, LocationType& location,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    );

    virtual void _unweightedStats(
        StatsData<AccumType>& stats, uint64_t& ngood, LocationType& location,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    );
    // </group>

    // <group>
    // has weights, but no mask, no ranges
    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride
    );

    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude
    );

    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride
    );

    virtual void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude
    );
    // </group>

private:

    // _f defined in inclusion range between Q1 - _f*D and Q3 + _f*D, where
    // D = Q3 - Q1 and Q1 and Q3 are the first and third quartiles, respectively
    double _f;
    bool _rangeIsSet{false}, _hasRange{false};

    void _setRange();

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/HingesFencesStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
