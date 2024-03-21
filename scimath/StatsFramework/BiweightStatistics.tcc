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

#ifndef SCIMATH_BIWEIGHTESTATISTICS_TCC
#define SCIMATH_BIWEIGHTESTATISTICS_TCC

#include <casacore/scimath/StatsFramework/BiweightStatistics.h>

#include <casacore/casa/aipsxtype.h>
#include <casacore/scimath/StatsFramework/BiweightStatisticsData.h>
#include <casacore/scimath/StatsFramework/ClassicalQuantileComputer.h>
#include <casacore/scimath/StatsFramework/ClassicalStatisticsData.h>
#include <casacore/scimath/StatsFramework/StatisticsDataset.h>

#include <utility>

namespace casacore {

CASA_STATD const AccumType BiweightStatistics<CASA_STATP>::FOUR = 4;
CASA_STATD const AccumType BiweightStatistics<CASA_STATP>::FIVE = 5;

CASA_STATD
BiweightStatistics<CASA_STATP>::BiweightStatistics(Int maxNiter, Double c)
    : ClassicalStatistics<CASA_STATP>(),
    _c(c), _maxNiter(maxNiter) {
    this->_setUnsupportedStatistics(
        BiweightStatisticsData::getUnsupportedStats()
    );
}

CASA_STATD
BiweightStatistics<CASA_STATP>::BiweightStatistics(
    const BiweightStatistics<CASA_STATP>& other
) : ClassicalStatistics<CASA_STATP>(other), _c(other._c), _niter(other._niter),
    _maxNiter(other._maxNiter), _location(other._location),
    _scale(other._scale), _range(other._range), _npts(other._npts) {}

CASA_STATD
BiweightStatistics<CASA_STATP>::~BiweightStatistics() {}

CASA_STATD
BiweightStatistics<CASA_STATP>& BiweightStatistics<CASA_STATP>::operator=(
    const BiweightStatistics<CASA_STATP>& other
) {
    if (this != &other) {
        ClassicalStatistics<CASA_STATP>::operator=(other);
        _c = other._c;
        _niter = other._niter;
        _maxNiter = other._maxNiter;
        _location = other._location;
        _scale = other._scale;
        _range = other._range;
        _npts = other._npts;
    }
    return *this;
}

CASA_STATD
StatisticsData::ALGORITHM BiweightStatistics<CASA_STATP>::algorithm() const {
    return StatisticsData::BIWEIGHT;
}

CASA_STATD
StatisticsAlgorithm<CASA_STATP>* BiweightStatistics<CASA_STATP>::clone() const {
    return new BiweightStatistics<CASA_STATP>(*this);
}

CASA_STATD
AccumType BiweightStatistics<CASA_STATP>::getMedian(
    std::shared_ptr<uInt64>, std::shared_ptr<AccumType>,
    std::shared_ptr<AccumType>, uInt, Bool, uInt
) {
    ThrowCc(
        "The biweight algorithm does not support computation of the median"
    );
}

CASA_STATD
AccumType BiweightStatistics<CASA_STATP>::getMedianAndQuantiles(
    std::map<Double, AccumType>&, const std::set<Double>&, std::shared_ptr<uInt64>,
    std::shared_ptr<AccumType>, std::shared_ptr<AccumType>, uInt, Bool, uInt
) {
    ThrowCc(
        "The biweight algorithm does not support computation "
        "of the median nor quantile values"
    );
}

CASA_STATD
AccumType BiweightStatistics<CASA_STATP>::getMedianAbsDevMed(
    std::shared_ptr<uInt64>, std::shared_ptr<AccumType>, std::shared_ptr<AccumType>,
    uInt, Bool, uInt
) {
    ThrowCc(
        "The biweight algorithm does not support computation "
        "of the median of the absolute deviation from the median"
    );
}

CASA_STATD
Int BiweightStatistics<CASA_STATP>::getNiter() const {
    return _niter;
}


CASA_STATD
std::map<Double, AccumType> BiweightStatistics<CASA_STATP>::getQuantiles(
    const std::set<Double>&, std::shared_ptr<uInt64>, std::shared_ptr<AccumType>,
    std::shared_ptr<AccumType>, uInt, Bool, uInt
) {
    ThrowCc(
        "The biweight algorithm does not support computation of quantile values"
    );
}

CASA_STATD
std::pair<Int64, Int64> BiweightStatistics<CASA_STATP>::getStatisticIndex(
    StatisticsData::STATS
) {
    ThrowCc(
        "The biweight algorithm does not support "
        "computation of statistics index values"
    );
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::setCalculateAsAdded(Bool c) {
    ThrowIf(
        c, "BiweightStatistics does not support calculating "
        "statistics incrementally as data sets are added"
    );
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::setStatsToCalculate(
    std::set<StatisticsData::STATS>& stats
) {
    // we always must compute the location (MEAN) and scale (STDDEV)
    stats.insert(StatisticsData::MEAN);
    stats.insert(StatisticsData::STDDEV);
    ClassicalStatistics<CASA_STATP>::setStatsToCalculate(stats);
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::reset() {
    ClassicalStatistics<CASA_STATP>::reset();
    _location = 0;
    _scale = 0;
    _range = std::pair<AccumType, AccumType>();
    _npts = 0;
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_computeLocationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4, AccumType& ww_4u2,
    DataIterator dataIter, MaskIterator maskIter, WeightsIterator weightsIter,
    uInt64 dataCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        this->_getStatsData().weighted = True;
        if (chunk.mask) {
            this->_getStatsData().masked = True;
            if (chunk.ranges) {
                _locationAndScaleSums(
                    sxw2, sw2, sx_M2w4, ww_4u2, dataIter, weightsIter,
                    dataCount, chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _locationAndScaleSums(
                    sxw2, sw2, sx_M2w4, ww_4u2, dataIter, weightsIter,
                    dataCount, chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _locationAndScaleSums(
                sxw2, sw2, sx_M2w4, ww_4u2, dataIter, weightsIter, dataCount,
                chunk.dataStride, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _locationAndScaleSums(
                sxw2, sw2, sx_M2w4, ww_4u2, dataIter, weightsIter,
                dataCount, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        this->_getStatsData().masked = True;
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _locationAndScaleSums(
                sxw2, sw2, sx_M2w4, ww_4u2, dataIter, dataCount,
                chunk.dataStride, maskIter, chunk.mask->second,
                chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            _locationAndScaleSums(
                sxw2, sw2, sx_M2w4, ww_4u2, dataIter, dataCount,
                chunk.dataStride, maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _locationAndScaleSums(
            sxw2, sw2, sx_M2w4, ww_4u2, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges
        // associated with it, and its stride is 1. No filtering of the data is
        // necessary.
        _locationAndScaleSums(
            sxw2, sw2, sx_M2w4, ww_4u2, dataIter, dataCount, chunk.dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_computeLocationSums(
    AccumType& sxw2, AccumType& sw2, DataIterator dataIter,
    MaskIterator maskIter, WeightsIterator weightsIter, uInt64 dataCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        // no need to put these in atomic or critical blocks because
        // they always get set to True here
        this->_getStatsData().weighted = True;
        if (chunk.mask) {
            this->_getStatsData().masked = True;
            if (chunk.ranges) {
                _locationSums(
                    sxw2, sw2, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _locationSums(
                    sxw2, sw2, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _locationSums(
                sxw2, sw2, dataIter, weightsIter, dataCount,
                chunk.dataStride, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _locationSums(
                sxw2, sw2, dataIter, weightsIter, dataCount, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        this->_getStatsData().masked = True;
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _locationSums(
                sxw2, sw2, dataIter, dataCount, chunk.dataStride, maskIter,
                chunk.mask->second, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            _locationSums(
                sxw2, sw2, dataIter, dataCount, chunk.dataStride,
                maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _locationSums(
            sxw2, sw2, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges
        // associated with it, and its stride is 1. No filtering of the data is
        // necessary.
        _locationSums(
            sxw2, sw2, dataIter, dataCount, chunk.dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_computeScaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, DataIterator dataIter,
    MaskIterator maskIter, WeightsIterator weightsIter, uInt64 dataCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) const {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _scaleSums(
                    sx_M2w4, ww_4u2, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _scaleSums(
                    sx_M2w4, ww_4u2, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _scaleSums(
                sx_M2w4, ww_4u2, dataIter, weightsIter, dataCount,
                chunk.dataStride, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _scaleSums(
                sx_M2w4, ww_4u2, dataIter, weightsIter,
                dataCount, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _scaleSums(
                sx_M2w4, ww_4u2, dataIter, dataCount,
                chunk.dataStride, maskIter, chunk.mask->second,
                chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            _scaleSums(
                sx_M2w4, ww_4u2, dataIter, dataCount, chunk.dataStride,
                maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _scaleSums(
            sx_M2w4, ww_4u2, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges
        // associated with it, and its stride is 1. No filtering of the data is
        // necessary.
        _scaleSums(
            sx_M2w4, ww_4u2, dataIter, dataCount, chunk.dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_computeStats() {
    ClassicalStatistics<CASA_STATP> cs(*this);
    _location = cs.getMedian();
    _scale = C::probit_3_4 * cs.getMedianAbsDevMed();
    _npts = cs.getNPts();
    ThrowIf (
        _npts <= 1,
        "npts is " + String::toString(_npts) + ". There must be at least two "
        "points to compute the biweight location and scale"
    );
    StatsData<AccumType>& stats = this->_getStatsData();
    stats.npts = _npts;
    AccumType mymin, mymax;
    cs.getMinMax(mymin, mymax);
    stats.min.reset(new AccumType(mymin));
    stats.max.reset(new AccumType(mymax));
    AccumType spread = _c * _scale;
    _range = std::pair<AccumType, AccumType>(
        _location - spread, _location + spread
    );
    if (_maxNiter >= 0) {
        // initial scale estimation before iteration begins
        _doScale();
        const AccumType epsilon = 0.03*C::_1_sqrt2/sqrt(_npts - 1);
        AccumType prevScale = 0;
        for (_niter=1; _niter <= _maxNiter; ++_niter) {
            prevScale = _scale;
            _doLocation();
            // The range must be reset after the location has been computed in
            // this iteration. note that spread doesn't change after the
            // _location computation
            _range = std::pair<AccumType, AccumType>(
                _location - spread, _location + spread
            );
            _doScale();
            if (
                abs(1 - _scale/prevScale) < epsilon
                || _niter == _maxNiter
            ) {
                break;
            }
            spread = _c * _scale;
            _range = std::pair<AccumType, AccumType>(
                _location - spread, _location + spread
            );
        }
    }
    else {
        _doLocationAndScale();
        _niter = -1;
    }
    stats.mean = _location;
    stats.stddev = _scale;
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_doLocation() {
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ds.initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds.getDataProvider()
    );
    const uInt dim = ClassicalStatisticsData::CACHE_PADDING*nThreadsMax;
    std::unique_ptr<AccumType[]> tsxw2(new AccumType[dim]);
    std::unique_ptr<AccumType[]> tsw2(new AccumType[dim]);
    // initialize the thread-based sums to 0
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        tsxw2[idx8] = 0;
        tsw2[idx8] = 0;
    }
    const uInt& blockSize = ClassicalStatisticsData::BLOCK_SIZE;
    while (True) {
        const auto& chunk = ds.initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        std::unique_ptr<DataIterator[]> dataIter;
        std::unique_ptr<MaskIterator[]> maskIter;
        std::unique_ptr<WeightsIterator[]> weightsIter;
        std::unique_ptr<uInt64[]> offset;
        ds.initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < blockSize
                ? extra : blockSize;
            _computeLocationSums(
                tsxw2[idx8], tsw2[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, chunk
            );
            ds.incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds.increment(False)) {
            break;
        }
    }
    AccumType psxw2 = 0;
    AccumType psw2 = 0;
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        psxw2 += tsxw2[idx8];
        psw2 += tsw2[idx8];
    }
    _location = psxw2/psw2;
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_doScale() {
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ds.initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds.getDataProvider()
    );
    const uInt dim = ClassicalStatisticsData::CACHE_PADDING*nThreadsMax;
    std::unique_ptr<AccumType[]> tsx_M2w4(new AccumType[dim]);
    std::unique_ptr<AccumType[]> tww_4u2(new AccumType[dim]);
    // initialize the thread-based sums to 0
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        tsx_M2w4[idx8] = 0;
        tww_4u2[idx8] = 0;
    }
    const uInt& blockSize = ClassicalStatisticsData::BLOCK_SIZE;
    while (True) {
        const auto& chunk = ds.initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        std::unique_ptr<DataIterator[]> dataIter;
        std::unique_ptr<MaskIterator[]> maskIter;
        std::unique_ptr<WeightsIterator[]> weightsIter;
        std::unique_ptr<uInt64[]> offset;
        ds.initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < blockSize
                ? extra : blockSize;
            _computeScaleSums(
                tsx_M2w4[idx8], tww_4u2[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, chunk
            );
            ds.incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds.increment(False)) {
            break;
        }
    }
    AccumType psx_M2w4 = 0;
    AccumType pww_4u2 = 0;
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        psx_M2w4 += tsx_M2w4[idx8];
        pww_4u2 += tww_4u2[idx8];
    }
    AccumType p = abs(pww_4u2);
    AccumType denomFactor2 = max(AccumType(1), p - 1);
    _scale = sqrt((Double)_npts * psx_M2w4/(p * denomFactor2));
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_doLocationAndScale() {
    StatisticsDataset<CASA_STATP>& ds = this->_getDataset();
    ds.initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds.getDataProvider()
    );
    const uInt dim = ClassicalStatisticsData::CACHE_PADDING*nThreadsMax;
    std::unique_ptr<AccumType[]> tsxw2(new AccumType[dim]);
    std::unique_ptr<AccumType[]> tsw2(new AccumType[dim]);
    std::unique_ptr<AccumType[]> tsx_M2w4(new AccumType[dim]);
    std::unique_ptr<AccumType[]> tww_4u2(new AccumType[dim]);
    // initialize the thread-based sums to 0
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        tsxw2[idx8] = 0;
        tsw2[idx8] = 0;
        tsx_M2w4[idx8] = 0;
        tww_4u2[idx8] = 0;
    }
    const uInt& blockSize = ClassicalStatisticsData::BLOCK_SIZE;
    while (True) {
        const auto& chunk = ds.initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        std::unique_ptr<DataIterator[]> dataIter;
        std::unique_ptr<MaskIterator[]> maskIter;
        std::unique_ptr<WeightsIterator[]> weightsIter;
        std::unique_ptr<uInt64[]> offset;
        ds.initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < blockSize
                ? extra : blockSize;
            _computeLocationAndScaleSums(
                tsxw2[idx8], tsw2[idx8], tsx_M2w4[idx8], tww_4u2[idx8],
                dataIter[idx8], maskIter[idx8], weightsIter[idx8], dataCount,
                chunk
            );
            ds.incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds.increment(False)) {
            break;
        }
    }
    AccumType psxw2 = 0;
    AccumType psw2 = 0;
    AccumType psx_M2w4 = 0;
    AccumType pww_4u2 = 0;
    for (uInt i=0; i<nThreadsMax; ++i) {
        uInt idx8 = i * ClassicalStatisticsData::CACHE_PADDING;
        psxw2 += tsxw2[idx8];
        psw2 += tsw2[idx8];
        psx_M2w4 += tsx_M2w4[idx8];
        pww_4u2 += tww_4u2[idx8];
    }
    _location = psxw2/psw2;
    AccumType f = abs(pww_4u2);
    AccumType denomFactor2 = max(1.0, (f - 1));
    _scale = sqrt(((Double)_npts * psx_M2w4)/(f * denomFactor2));
}

CASA_STATD
StatsData<AccumType> BiweightStatistics<CASA_STATP>::_getStatistics() {
    StatsData<AccumType>& stats = this->_getStatsData();
    if (stats.npts == 0) {
        _computeStats();
        stats = this->_getStatsData();
    }
    return copy(stats);
}


// Note we purposefully use > and <, rather than >= and <= to agree with
// Amanda Kepley's requirement.
#define _locationAndScaleSumsCodeBW \
    AccumType x = *datum; \
    if (x > _range.first && x < _range.second) { \
        AccumType x_M = x - _location; \
        AccumType u = x_M/(_c*_scale); \
        AccumType w = 1 - u*u; \
        AccumType w2 = w * w; \
        sxw2 += x * w2; \
        sw2 += w2; \
        AccumType x_M2 = x_M * x_M; \
        AccumType w4 = w2 * w2; \
        sx_M2w4 += x_M2 * w4; \
        ww_4u2 += w * (FIVE*w - FOUR); \
    }

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _locationAndScaleSumsCodeBW
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationAndScaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _locationAndScaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationAndScaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _locationAndScaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationAndScaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationAndScaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationAndScaleSums(
    AccumType& sxw2, AccumType& sw2, AccumType& sx_M2w4,
    AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _locationAndScaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// Note we purposefully use > and <, rather than >= and <= to agree with
// Amanda Kepley's requirement.
#define _locationSumsCodeBW \
    AccumType x = *datum; \
    if (x > _range.first && x < _range.second) { \
        AccumType x_M = x - _location; \
        AccumType u = x_M/(_c*_scale); \
        AccumType w = 1 - u*u; \
        AccumType w2 = w * w; \
        sxw2 += x * w2; \
        sw2 += w2; \
    }

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _locationSumsCodeBW
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _locationSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin,
    uInt maskStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _locationSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _locationSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_locationSums(
    AccumType& sxw2, AccumType& sw2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _locationSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// Note we purposefully use > and <, rather than >= and <= to agree with
// Amanda Kepley's requirement.
#define _scaleSumsCodeBW \
    AccumType x = *datum; \
    if (x > _range.first && x < _range.second) { \
        AccumType x_M = x - _location; \
        AccumType x_M2 = x_M * x_M; \
        AccumType u = x_M/(_c*_scale); \
        AccumType w = 1 - u*u; \
        AccumType w2 = w * w; \
        AccumType w4 = w2 * w2; \
        sx_M2w4 += x_M2 * w4; \
        ww_4u2 += w * (FIVE*w - FOUR); \
    }

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
        _scaleSumsCodeBW
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _scaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin,
    uInt maskStride
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _scaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin,
    uInt maskStride, const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _scaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _scaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _scaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _scaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void BiweightStatistics<CASA_STATP>::_scaleSums(
    AccumType& sx_M2w4, AccumType& ww_4u2, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _scaleSumsCodeBW
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

}

#endif
