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

// Class to calculate statistics using the so-called hinges and fences algorithm. In this
// algorithm, the data on which the statistics are computed from is limited to the range
// of values between Q1 - f*D and Q3 + f*D, inclusive, where D = Q3 - Q1 and Q1 and Q3 are
// the first and third quartiles, respectively.
//
// This class uses a HingesFencesQuantileComputer object for computing quantile-
// like statistics. See class documentation for StatisticsAlgorithm for details
// regarding QuantileComputer classes.

template <class AccumType, class DataIterator, class MaskIterator=const Bool *, class WeightsIterator=DataIterator>
class HingesFencesStatistics
    : public ConstrainedRangeStatistics<CASA_STATP> {
public:

    // If <src>f</src> is negative, the full dataset is used; ie the object has the same
    // behavior as a ClassicalStatistics object
    HingesFencesStatistics(Double f=-1.0);

    // copy semantics
    HingesFencesStatistics(const HingesFencesStatistics<CASA_STATP>& other);

    virtual ~HingesFencesStatistics();

    // copy semantics
    HingesFencesStatistics<CASA_STATP>& operator=(
        const HingesFencesStatistics<CASA_STATP>& other
    );

    // Clone this instance. Caller is responsible for deleting the returned pointer.
    virtual StatisticsAlgorithm<CASA_STATP>* clone() const;
    
    // get the algorithm that this object uses for computing stats
    virtual StatisticsData::ALGORITHM algorithm() const {
        return StatisticsData::HINGESFENCES;
    };

    // reset object to initial state. Clears all private fields including data,
    // accumulators, global range. It does not affect the fence factor (_f), which was
    // set at object construction.
    virtual void reset();

    // This class does not allow statistics to be calculated as datasets are added, so
    // an exception will be thrown if <src>c</src> is True.
    void setCalculateAsAdded(Bool c);

protected:
    // <group>
    // scan through the data set to determine the number of good (unmasked, weight > 0,
    // within range) points. The first with no mask, no
    // ranges, and no weights is trivial with npts = nr in this class, but is implemented here
    // so that derived classes may override it.
    inline void _accumNpts(
        uInt64& npts,
        const DataIterator& dataStart, Int64 nr, uInt dataStride
    ) const;

    void _accumNpts(
        uInt64& npts,
        const DataIterator& dataStart, Int64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    ) const;

    void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
        Bool isInclude
    ) const;

    void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride
    ) const;

    void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    ) const;

    void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    void _accumNpts(
        uInt64& npts,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    ) const;
    // </group>

    // <group>
    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
        Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMax(
        CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    ) const;

    // <group>
    // Sometimes we want the min, max, and npts all in one scan.
    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
        Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    ) const;

    virtual void _minMaxNpts(
        uInt64& npts, CountedPtr<AccumType>& mymin, CountedPtr<AccumType>& mymax,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    ) const;
    // </group>

    // <group>
    // no weights, no mask, no ranges
    void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride
    );

    // no weights, no mask
    void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const DataRanges& ranges, Bool isInclude
    );

    void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride
    );

    void _unweightedStats(
        StatsData<AccumType>& stats, uInt64& ngood, LocationType& location,
        const DataIterator& dataBegin, Int64 nr, uInt dataStride,
        const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    );
    // </group>

    // <group>
    // has weights, but no mask, no ranges
    void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride
    );

    void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        Int64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
    );

    void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
    );

    void _weightedStats(
        StatsData<AccumType>& stats, LocationType& location,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        Int64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
        const DataRanges& ranges, Bool isInclude
    );
    // </group>

private:

    // _f defined in inclusion range between Q1 - _f*D and Q3 + _f*D, where
    // D = Q3 - Q1 and Q1 and Q3 are the first and third quartiles, respectively
    Double _f;
    Bool _rangeIsSet, _hasRange;

    void _setRange();

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/HingesFencesStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
