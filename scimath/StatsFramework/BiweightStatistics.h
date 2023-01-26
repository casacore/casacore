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

#ifndef SCIMATH_BIWEIGHTSTATISTICS_H
#define SCIMATH_BIWEIGHTSTATISTICS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/StatsFramework/ClassicalStatistics.h>

#include <set>
#include <vector>
#include <utility>

namespace casacore {

// The biweight algorithm is a robust iterative algorithm that computes two
// quantities called the "location" and the "scale", which are analogous to
// the mean and the standard deviation. Important equations are
//
// A. How to compute u_i values, which are related to the weights,
//    w_i = (1 - u_i*u_i) if abs(u_i) < 1, 0 otherwise, using the equation
//
//      u_i = (x_i - c_bi)/(c*s_bi)                   (1)
//
//    where x_i are the data values, c_bi is the biweight location, c is a
//    configurable constant, and s_bi is the biweight scale. For the initial
//    computation of the u_i values, c_bi is set equal to the median of the
//    distribution and s_bi is set equal to the normalized median of the
//    absolute deviation about the median (that is the median of the absolute
//    deviation about the median multiplied by the value of the probit function
//    at 0.75).
// B  The location, c_bi, is computed from
//
//      c_bi = sum(x_i * w_i^2)/sum(w_i^2)            (2)
//
//    where only values of u_i which satisfy abs(u_i) < 1 (w_i > 0) are used in
//    the sums.
// C. The scale value is computed using
//
//               n * sum((x_i - c_bi)^2 * w_i^4)
//      s_bi^2 = _______________________________      (3)
//                      p * max(1, p - 1)
//
// where n is the number of points for the entire distribution (which includes
// all the data for which abs(u_i) >= 1) and p is given by
//
//    p = abs(sum((w_i) * (w_i - 4*u_i^2)))
//
// Again, the sums include only data for which abs(u_i) < 1.
//
// The algorithm proceeds as follows.
// 1. Compute initial u_i values from equation (1), setting c_bi equal to the
//    median of the distribution and s_bi equal to the normalized median of the
//    absolute deviation about the median.
// 2. Compute the initial value of the scale using the u_i values computed in
//    step 1. using equation 3.
// 3. Recompute u_i values using the most recent previous scale and location
//    values.
// 4. Compute the location using the u_i values from step 3 and equation (2).
// 5. Recompute u_i values using the most recent previous scale and location
//    values.
// 6. Compute the new scale value using the the u_i values computed in step 5
//    and the value of the location computed in step 4.
// 7. Steps 3. - 6. are repeated until convergence occurs or the maximum number
//    of iterations (a configurable parameter) is reached. The convergence
//    criterion is given by
//
//    abs(1 - s_bi/s_bi,prev) < 0.03 * sqrt(0.5/(n - 1))
//
//    where s_bi,prev is the value of the scale computed in the previous
//    iteration.
//
// SPECIAL CASE TO FACILITATE SPEED
//
// In the special case where maxNiter is specified to be negative, the algorithm
// proceeds as follows
// 1. Compute u_i values using the median for the location and the normalized
//    median of the absolute deviation about the median as the scale
// 2. Compute the location and scale (which can be carried out simultaneously)
//    using the u_i values computed in step 1. The value of the location is just
//    the median that is used in equation (3) to compute the scale
//
// IMPORTANT NOTE REGARDING USER SPECIFIED WEIGHTS
//
// Although user-specified weights can be supplied, they are effectively ignored
// by this algorithm, except for data which have weights of zero, which are
// ignored.

// This is a derived class of ClassicalStatistics, rather than
// ConstrainedRangeStatistics, because if behaves differently from
// ConstrainedRangeStatistics and does not need to use any methods in that
// class, so making it a specialization of the higher level ClassicalStatistics
// seems the better choice.
template <
    class AccumType, class DataIterator, class MaskIterator=const bool*,
    class WeightsIterator=DataIterator
>
class BiweightStatistics
    : public ClassicalStatistics<CASA_STATP> {
public:

    BiweightStatistics(int32_t maxNiter=3, double c=6.0);

    // copy semantics
    BiweightStatistics(const BiweightStatistics<CASA_STATP>& other);

    virtual ~BiweightStatistics();

    // copy semantics
    BiweightStatistics<CASA_STATP>& operator=(
        const BiweightStatistics<CASA_STATP>& other
    );

    virtual StatisticsData::ALGORITHM algorithm() const;

    // Clone this instance
    virtual StatisticsAlgorithm<CASA_STATP>* clone() const;

    // <group>
    // these statistics are not supported. The methods, which override
    // the virtual ancestor versions, throw exceptions.
    virtual AccumType getMedian(
        CountedPtr<uint64_t> knownNpts=nullptr,
        CountedPtr<AccumType> knownMin=nullptr,
        CountedPtr<AccumType> knownMax=nullptr,
        uint32_t binningThreshholdSizeBytes=4096*4096,
        bool persistSortedArray=false, uint32_t nBins=10000
    );

    virtual AccumType getMedianAndQuantiles(
        std::map<double, AccumType>& quantileToValue,
        const std::set<double>& quantiles, CountedPtr<uint64_t> knownNpts=nullptr,
        CountedPtr<AccumType> knownMin=nullptr,
        CountedPtr<AccumType> knownMax=nullptr,
        uint32_t binningThreshholdSizeBytes=4096*4096,
        bool persistSortedArray=false, uint32_t nBins=10000
    );

    virtual AccumType getMedianAbsDevMed(
        CountedPtr<uint64_t> knownNpts=nullptr,
        CountedPtr<AccumType> knownMin=nullptr,
        CountedPtr<AccumType> knownMax=nullptr,
        uint32_t binningThreshholdSizeBytes=4096*4096,
        bool persistSortedArray=false, uint32_t nBins=10000
    );

    virtual std::map<double, AccumType> getQuantiles(
        const std::set<double>& quantiles, CountedPtr<uint64_t> npts=nullptr,
        CountedPtr<AccumType> min=nullptr, CountedPtr<AccumType> max=nullptr,
        uint32_t binningThreshholdSizeBytes=4096*4096,
        bool persistSortedArray=false, uint32_t nBins=10000
    );

    virtual std::pair<int64_t, int64_t> getStatisticIndex(
        StatisticsData::STATS stat
    );
    // </group>

    // returns the number of iterations performed to
    // compute the current location and scale values
    int32_t getNiter() const;

    // reset object to initial state. Clears all private fields including data,
    // accumulators, etc.
    virtual void reset();

    // If c is true, an exception is thrown; this algorithm does not support
    // computing stats as data are added.
    virtual void setCalculateAsAdded(bool c);

    // Provide guidance to algorithms by specifying a priori which statistics
    // the caller would like calculated. This algorithm always needs to compute
    // the location (MEAN) and the scale (STDDEV) so these statistics are always
    // added to the input set, which is why this method overrides the base class
    // version.
    virtual void setStatsToCalculate(std::set<StatisticsData::STATS>& stats);

protected:

    void _computeStats();

    virtual StatsData<AccumType> _getStatistics();

private:
    double _c{0};
    int32_t _niter{0}, _maxNiter{0};
    AccumType _location{0}, _scale{0};
    std::pair<AccumType, AccumType> _range{};
    // _npts is the number of points computed using ClassicalStatistics
    uint64_t _npts{0};

    // because the compiler gets confused if these aren't explicitly typed
    static const AccumType FOUR;
    static const AccumType FIVE;

    void _computeLocationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
        AccumType& ww_4u2, DataIterator dataIter, MaskIterator maskIter,
        WeightsIterator weightsIter, uint64_t dataCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    void _computeLocationSums(
        AccumType& sxw2, AccumType& sw2, DataIterator dataIter,
        MaskIterator maskIter, WeightsIterator weightsIter, uint64_t dataCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    );

    void _computeScaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, DataIterator dataIter,
        MaskIterator maskIter, WeightsIterator weightsIter, uint64_t dataCount,
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
    ) const;

    void _doLocationAndScale();

    void _doLocation();

    void _doScale();

    // <group>
    // sxw2 = sum(x_i*(1 - u_i^2)^2)
    // sw2 = sum((1-u_i^2)^2)
    // sx_M2w4 = sum((x_i - _location)^2 * (1 - u_i^2)^4) = sum((x_i - _location)^2 * w_i^4)
    // ww_4u2 = sum((1 - u_i^2) * (1 - 5*u_i^2)) = sum(w_i * (w_i - 4*u_i^2))
    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride
    ) const;

    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;

    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride
    ) const;

    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude
    ) const;

    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude
    ) const;

    void _locationAndScaleSums(
        AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
        const DataIterator& dataBegin, const WeightsIterator& weightBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride
    ) const;
    // </group>

    // <group>
    // sxw2 = sum(x_i*(1 - u_i^2)^2)
    // sw2 = sum((1-u_i^2)^2)
    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride
    ) const;

    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude
    ) const;

    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride
    ) const;

    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude
    ) const;

    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
    ) const;

    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    void _locationSums(
        AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;
    // </group>

    // <group>
    // sx_M2w4 = sum((x_i - _location)^2 * (1 - u_i^2)^4) = sum((x_i - _location)^2 * w_i^4)
    // ww_4u2 = sum((1 - u_i^2) * (1 - 5*u_i^2)) = sum(w_i * (w_i - 4*u_i^2))
    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride
    ) const;

    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const DataRanges& ranges,
        bool isInclude
    ) const;

    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride
    ) const;

    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin,
        uint32_t maskStride, const DataRanges& ranges, bool isInclude
    ) const;

    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
    ) const;

    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride,
        const DataRanges& ranges, bool isInclude
    ) const;

    void _scaleSums(
        AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
        const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
        const MaskIterator& maskBegin, uint32_t maskStride
    ) const;
    // </group>

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/BiweightStatistics.tcc>
#endif

#endif
