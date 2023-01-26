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

#ifndef SCIMATH_CLASSICALQUANTILECOMPUTER_TCC
#define SCIMATH_CLASSICALQUANTILECOMPUTER_TCC

#include <casacore/scimath/StatsFramework/ClassicalQuantileComputer.h>

#include <casacore/casa/aipsxtype.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/scimath/StatsFramework/StatisticsData.h>
#include <casacore/scimath/StatsFramework/StatisticsIncrementer.h>

#include <iterator>

namespace casacore {

CASA_STATD
ClassicalQuantileComputer<CASA_STATP>::ClassicalQuantileComputer(
    StatisticsDataset<CASA_STATP>* dataset
) : StatisticsAlgorithmQuantileComputer<CASA_STATP>(dataset) {}

CASA_STATD
ClassicalQuantileComputer<CASA_STATP>::~ClassicalQuantileComputer() {}

CASA_STATD
ClassicalQuantileComputer<CASA_STATP>::ClassicalQuantileComputer(
    const ClassicalQuantileComputer<CASA_STATP>& other
) : StatisticsAlgorithmQuantileComputer<CASA_STATP>(other),
    _doMedAbsDevMed(other._doMedAbsDevMed), _myMedian(other._myMedian)  {}

CASA_STATD
ClassicalQuantileComputer<CASA_STATP>&
ClassicalQuantileComputer<CASA_STATP>::operator=(
    const ClassicalQuantileComputer<CASA_STATP>& other
) {
    if (this == &other) {
        return *this;
    }
    StatisticsAlgorithmQuantileComputer<CASA_STATP>::operator=(other);
    _doMedAbsDevMed = other._doMedAbsDevMed;
    _myMedian = other._myMedian;
    return *this;
}

CASA_STATD
StatisticsAlgorithmQuantileComputer<CASA_STATP>*
ClassicalQuantileComputer<CASA_STATP>::clone() const {
    return new ClassicalQuantileComputer<CASA_STATP>(*this);
}

CASA_STATD
AccumType ClassicalQuantileComputer<CASA_STATP>::getMedian(
    uint64_t mynpts, AccumType mymin, AccumType mymax,
    uint32_t binningThreshholdSizeBytes, bool persistSortedArray, uint32_t nBins
) {
    auto median = this->_getMedian();
    if (! median) {
        auto indices = _medianIndices(mynpts);
        auto indexToValue = _indicesToValues(
            mynpts, mymin, mymax, binningThreshholdSizeBytes/sizeof(AccumType),
            indices, persistSortedArray, nBins
        );
        median = new AccumType(
            indexToValue.size() == 1
            ? indexToValue[*indices.begin()]
            : (
                indexToValue[*indices.begin()]
                + indexToValue[*indices.rbegin()]
            )/AccumType(2)
        );
        this->setMedian(median);
    }
    return *median;
}

CASA_STATD
AccumType ClassicalQuantileComputer<CASA_STATP>::getMedianAbsDevMed(
    uint64_t mynpts, AccumType mymin, AccumType mymax,
    uint32_t binningThreshholdSizeBytes, bool persistSortedArray, uint32_t nBins
) {
    auto medAbsDevMed = this->_getMedianAbsDevMedian();
    if (! medAbsDevMed) {
        // This call calculates the median of the data set which is stored internally and
        // used, but is not necessary to be captured in the return value here.
        getMedian(
            mynpts, mymin, mymax, binningThreshholdSizeBytes,
            persistSortedArray, nBins
        );
        auto indices = _medianIndices(mynpts);
        // throw the proper switch
        _doMedAbsDevMed = true;
        _myMedian = *this->_getMedian();
        auto indexToValue = _indicesToValues(
            mynpts, mymin, mymax,
            binningThreshholdSizeBytes/sizeof(AccumType),
            indices, persistSortedArray, nBins
        );
        _doMedAbsDevMed = false;
        medAbsDevMed.reset(
            indexToValue.size() == 1
            ? new AccumType(indexToValue[*indices.begin()])
            : new AccumType(
                (
                    indexToValue[*indices.begin()]
                    + indexToValue[*indices.rbegin()]
                )/AccumType(2)
            )
        );
        this->_setMedianAbsDevMedian(medAbsDevMed);
    }
    return *medAbsDevMed;
}

CASA_STATD
AccumType ClassicalQuantileComputer<CASA_STATP>::getMedianAndQuantiles(
    std::map<double, AccumType>& quantiles, const std::set<double>& fractions,
    uint64_t mynpts, AccumType mymin, AccumType mymax,
    uint32_t binningThreshholdSizeBytes, bool persistSortedArray, uint32_t nBins
) {
    std::set<uint64_t> medianIndices;
    quantiles.clear();
    auto median = this->_getMedian();
    if (! median) {
        medianIndices = _medianIndices(mynpts);
    }
    auto quantileToIndex = StatisticsData::indicesFromFractions(
        mynpts, fractions
    );
    auto indices = medianIndices;
    for_each(
        quantileToIndex.cbegin(), quantileToIndex.cend(),
        [&indices](const std::pair<double, uint64_t>& mypair) {
        indices.insert(mypair.second);
    });
    auto indexToValue = _indicesToValues(
        mynpts, mymin, mymax, binningThreshholdSizeBytes/sizeof(AccumType),
        indices, persistSortedArray, nBins
    );
    if (! median) {
        median.reset(
            mynpts % 2 == 0
            ? new AccumType(
                (
                    indexToValue[*medianIndices.begin()]
                    + indexToValue[*medianIndices.rbegin()]
                )/AccumType(2)
            )
            : new AccumType(indexToValue[*medianIndices.begin()])
        );
        this->setMedian(median);
    }
    for_each(
        fractions.cbegin(), fractions.cend(),
        [&quantiles, &indexToValue, &quantileToIndex](double q) {
        quantiles[q] = indexToValue[quantileToIndex[q]];
    });
    return *median;
}

CASA_STATD
std::map<double, AccumType> ClassicalQuantileComputer<CASA_STATP>::getQuantiles(
    const std::set<double>& fractions, uint64_t mynpts, AccumType mymin,
    AccumType mymax, uint32_t binningThreshholdSizeBytes,
    bool persistSortedArray, uint32_t nBins
) {
    if (fractions.empty()) {
        return std::map<double, AccumType>();
    }
    ThrowIf(
        *fractions.begin() <= 0 || *fractions.rbegin() >= 1,
        "Value of all quantiles must be between 0 and 1 (noninclusive)"
    );
    auto quantileToIndex = StatisticsData::indicesFromFractions(
        mynpts, fractions
    );
    // This seemingly convoluted way of doing things with maps is necessary
    // because multiple quantiles can map to the same sorted array index, and
    // multiple array indices can map the same value if the values in the array
    // are not unique.
    std::set<uint64_t> uniqueIndices;
    for_each(
        quantileToIndex.cbegin(), quantileToIndex.cend(),
        [&uniqueIndices](const std::pair<double, uint64_t>& mypair) {
        uniqueIndices.insert(mypair.second);
    });
    auto indexToValue = _indicesToValues(
        mynpts, mymin, mymax, binningThreshholdSizeBytes/sizeof(AccumType),
        uniqueIndices, persistSortedArray, nBins
    );
    std::map<double, AccumType> quantileToValue;
    for_each(
        quantileToIndex.cbegin(), quantileToIndex.cend(),
        [&quantileToValue, &indexToValue]
        (const std::pair<double, uint64_t>& mypair) {
        quantileToValue[mypair.first] = indexToValue[mypair.second];
    });
    return quantileToValue;
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::reset() {
    StatisticsAlgorithmQuantileComputer<CASA_STATP>::reset();
    _doMedAbsDevMed = false;
}

CASA_STATD std::vector<std::vector<uint64_t>>
ClassicalQuantileComputer<CASA_STATP>::_binCounts(
    std::vector<CountedPtr<AccumType> >& sameVal,
    const std::vector<StatsHistogram<AccumType>>& hist
) {
    auto bDesc = hist.cbegin();
    auto iDesc = bDesc;
    auto eDesc = hist.cend();
    if (hist.size() > 1) {
        // initialize only to squash compiler warning
        auto prevDesc = *bDesc;
        // sanity check
        while (iDesc != eDesc) {
            if (iDesc != bDesc) {
                ThrowIf (
                    iDesc->getMinHistLimit() <= prevDesc.getMinHistLimit(),
                    "Logic Error: histograms are not monotonically increasing"
                );
            }
            prevDesc = *iDesc;
            ++iDesc;
        }
    }
    std::vector<bool> allSame(hist.size(), true);
    // the elements in the outer vector are histograms. The elements in the
    // inner vector are the bins in the corresponding histograms. The int64_t
    // values are the number of data points in those bins
    std::vector<std::vector<uint64_t>> bins(hist.size());
    // initialize all bin counts to 0
    iDesc = bDesc;
    for_each(bins.begin(), bins.end(), [&iDesc](std::vector<uint64_t>& hist) {
        hist = std::vector<uint64_t>(iDesc->getNBins(), 0);
        ++iDesc;
    });
    // sameVal indicates if all values in a histogram
    // (the vector elements) are the same
    sameVal = std::vector<CountedPtr<AccumType>>(hist.size(), nullptr);
    // maxLimit are the maximum limits for each histogram. set them here.
    std::vector<AccumType> maxLimit(hist.size());
    iDesc = bDesc;
    for_each(maxLimit.begin(), maxLimit.end(), [&iDesc](AccumType& myMax) {
        myMax = iDesc->getMaxHistLimit();
        ++iDesc;
    });
    auto* ds = this->_getDataset();
    ds->initIterators();
    const uint32_t nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds->getDataProvider()
    );
    // The PtrHolders hold references to C arrays of length
    // ClassicalStatisticsData::CACHE_PADDING*nThreadsMax.
    // Only every CACHE_PADDING*nth element will be populated
    PtrHolder<std::vector<std::vector<uint64_t>>> tBins(
        new std::vector<std::vector<uint64_t> >[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], true
    );
    PtrHolder<std::vector<CountedPtr<AccumType>>> tSameVal(
        new std::vector<CountedPtr<AccumType>>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], true
    );
    PtrHolder<std::vector<bool>> tAllSame(
        new std::vector<bool>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], true
    );
    for (uint32_t tid=0; tid<nThreadsMax; ++tid) {
        uint32_t idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        tBins[idx8] = bins;
        tSameVal[idx8] = sameVal;
        tAllSame[idx8] = allSame;
    }
    while (true) {
        const auto& chunk = ds->initLoopVars();
        uint32_t nBlocks, nthreads;
        uint64_t extra;
        std::unique_ptr<DataIterator[]> dataIter;
        std::unique_ptr<MaskIterator[]> maskIter;
        std::unique_ptr<WeightsIterator[]> weightsIter;
        std::unique_ptr<uint64_t[]> offset;
        ds->initThreadVars(
            nBlocks, extra, nthreads, dataIter, maskIter,
            weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uint32_t i=0; i<nBlocks; ++i) {
            uint32_t idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uint64_t dataCount = (chunk.count - offset[idx8])
                < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeBins(
                tBins[idx8], tSameVal[idx8], tAllSame[idx8], dataIter[idx8],
                maskIter[idx8], weightsIter[idx8], dataCount, hist,
                maxLimit, chunk
            );
            ds->incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds->increment(false)) {
            break;
        }
    }
    StatisticsUtilities<AccumType>::mergeResults(
        bins, sameVal, allSame, tBins, tSameVal, tAllSame, nThreadsMax
    );
    return bins;
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_computeBins(
    std::vector<BinCountArray>& bins,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uint64_t count,
    const std::vector<StatsHistogram<AccumType>>& hist,
    const std::vector<AccumType>& maxLimit,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _findBins(
                    bins, sameVal, allSame, dataIter, weightsIter, count,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second, hist, maxLimit
                );
            }
            else {
                _findBins(
                    bins, sameVal, allSame, dataIter, weightsIter,
                    count, chunk.dataStride, maskIter, chunk.mask->second,
                    hist, maxLimit
                );
            }
        }
        else if (chunk.ranges) {
            _findBins(
                bins, sameVal, allSame, dataIter, weightsIter, count,
                chunk.dataStride, chunk.ranges->first, chunk.ranges->second,
                hist, maxLimit
            );
        }
        else {
            // has weights, but no mask nor ranges
            _findBins(
                bins, sameVal, allSame, dataIter, weightsIter,
                count, chunk.dataStride, hist, maxLimit
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _findBins(
                bins, sameVal, allSame, dataIter, count, chunk.dataStride,
                maskIter, chunk.mask->second, chunk.ranges->first,
                chunk.ranges->second, hist, maxLimit
            );
        }
        else {
            _findBins(
                bins, sameVal, allSame, dataIter, count, chunk.dataStride,
                maskIter, chunk.mask->second, hist, maxLimit
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _findBins(
            bins, sameVal, allSame, dataIter, count, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second, hist, maxLimit
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges
        // associated with it. No filtering of the data is necessary.
        _findBins(
            bins, sameVal, allSame, dataIter, count, chunk.dataStride,
            hist, maxLimit
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_createDataArray(DataArray& ary) {
    auto* ds = this->_getDataset();
    ds->initIterators();
    const auto nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds->getDataProvider()
    );
    PtrHolder<std::vector<AccumType>> tAry(
        new std::vector<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], true
    );
    while (true) {
        const auto& chunk = ds->initLoopVars();
        uint32_t nBlocks, nthreads;
        uint64_t extra;
        std::unique_ptr<DataIterator[]> dataIter;
        std::unique_ptr<MaskIterator[]> maskIter;
        std::unique_ptr<WeightsIterator[]> weightsIter;
        std::unique_ptr<uint64_t[]> offset;
        ds->initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uint32_t i=0; i<nBlocks; ++i) {
            uint32_t idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uint64_t dataCount = (chunk.count - offset[idx8])
                < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeDataArray(
                tAry[idx8], dataIter[idx8], maskIter[idx8],
                weightsIter[idx8], dataCount, chunk
            );
            ds->incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds->increment(false)) {
            break;
        }
    }
    // merge the per-thread arrays
    for (uint32_t tid=0; tid<nThreadsMax; ++tid) {
        const auto& v = tAry[ClassicalStatisticsData::CACHE_PADDING*tid];
        ary.insert(ary.end(), v.begin(), v.end());
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_computeDataArray(
    DataArray& ary, DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uint64_t dataCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _populateArray(
                    ary, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second
                );
            }
            else {
                _populateArray(
                    ary, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second
                );
            }
        }
        else if (chunk.ranges) {
            _populateArray(
                ary, dataIter, weightsIter, dataCount, chunk.dataStride,
                chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            // has weights, but no mask nor ranges
            _populateArray(
                ary, dataIter, weightsIter, dataCount, chunk.dataStride
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _populateArray(
                ary, dataIter, dataCount, chunk.dataStride, maskIter,
                chunk.mask->second, chunk.ranges->first, chunk.ranges->second
            );
        }
        else {
            _populateArray(
                ary, dataIter, dataCount, chunk.dataStride,
                maskIter, chunk.mask->second
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _populateArray(
            ary, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges
        // associated with it, and its stride is 1. No filtering of the data is
        // necessary.
        _populateArray(
            ary, dataIter, dataCount, chunk.dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_computeDataArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount, DataIterator dataIter,
    MaskIterator maskIter, WeightsIterator weightsIter, uint64_t dataCount,
    const IncludeLimits& includeLimits, uint64_t maxCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _populateArrays(
                    arys, currentCount, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second,
                    chunk.ranges->first, chunk.ranges->second, includeLimits,
                    maxCount
                );
            }
            else {
                _populateArrays(
                    arys, currentCount, dataIter, weightsIter,
                    dataCount, chunk.dataStride, maskIter, chunk.mask->second,
                    includeLimits, maxCount
                );
            }
        }
        else if (chunk.ranges) {
            _populateArrays(
                arys, currentCount, dataIter, weightsIter, dataCount,
                chunk.dataStride, chunk.ranges->first, chunk.ranges->second,
                includeLimits, maxCount
            );
        }
        else {
            // has weights, but no mask nor ranges
            _populateArrays(
                arys, currentCount, dataIter, weightsIter,
                dataCount, chunk.dataStride, includeLimits, maxCount
            );
        }
    }
    else if (chunk.mask) {
        // this data set has no weights, but does have a mask
        if (chunk.ranges) {
            _populateArrays(
                arys, currentCount, dataIter, dataCount, chunk.dataStride,
                maskIter, chunk.mask->second, chunk.ranges->first,
                chunk.ranges->second, includeLimits, maxCount
            );
        }
        else {
            _populateArrays(
                arys, currentCount, dataIter, dataCount, chunk.dataStride,
                maskIter, chunk.mask->second, includeLimits, maxCount
            );
        }
    }
    else if (chunk.ranges) {
        // this data set has no weights no mask, but does have a set of ranges
        // associated with it
        _populateArrays(
            arys, currentCount, dataIter, dataCount, chunk.dataStride,
            chunk.ranges->first, chunk.ranges->second, includeLimits, maxCount
        );
    }
    else {
        // simplest case, this data set has no weights, no mask, nor any ranges
        // associated with it, and its stride is 1. No filtering of the data is
        // necessary.
        _populateArrays(
            arys, currentCount, dataIter, dataCount,
            chunk.dataStride, includeLimits, maxCount
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_createDataArrays(
    std::vector<DataArray>& arys, const IncludeLimits& includeLimits,
    uint64_t maxCount
) {
    std::pair<AccumType, AccumType> prevLimits;
    auto first = true;
    for_each(
        includeLimits.cbegin(), includeLimits.cend(),
        [&first, &prevLimits]
         (const std::pair<AccumType, AccumType>& limitPair) {
        if (limitPair.first >= limitPair.second) {
            ostringstream os;
            os << "Logic Error: bin limits are nonsensical: " << limitPair;
            ThrowCc(os.str());
        }
        if (first) {
            first = false;
        }
        else if (
            limitPair.first <= prevLimits.first
            || limitPair.second <= prevLimits.second
        ) {
            ostringstream os;
            os << "Logic Error: bin limits are not in order: " << prevLimits
                << " , " << limitPair;
            ThrowCc(os.str());
        }
        prevLimits = limitPair;
    });
    auto* ds = this->_getDataset();
    ds->initIterators();
    const uint32_t nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds->getDataProvider()
    );
    PtrHolder<std::vector<std::vector<AccumType>>> tArys(
        new std::vector<std::vector<AccumType>>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], true
    );
    PtrHolder<uint64_t> tCurrentCount(
        new uint64_t[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], true
    );
    for (uint32_t tid=0; tid<nThreadsMax; ++tid) {
        uint32_t idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        tArys[idx8] = arys;
    }
    uint64_t currentCount = 0;
    while (currentCount < maxCount) {
        const auto& chunk = ds->initLoopVars();
        uint32_t nBlocks, nthreads;
        uint64_t extra;
        std::unique_ptr<DataIterator[]> dataIter;
        std::unique_ptr<MaskIterator[]> maskIter;
        std::unique_ptr<WeightsIterator[]> weightsIter;
        std::unique_ptr<uint64_t[]> offset;
        ds->initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
        for (uint32_t tid=0; tid<nThreadsMax; ++tid) {
            uint32_t idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
            tCurrentCount[idx8] = currentCount;
        }
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uint32_t i=0; i<nBlocks; ++i) {
            uint32_t idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uint64_t dataCount = (chunk.count - offset[idx8])
                < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeDataArrays(
                tArys[idx8], tCurrentCount[idx8], dataIter[idx8],
                maskIter[idx8], weightsIter[idx8], dataCount,
                includeLimits, maxCount, chunk
            );
            ds->incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        // currentCount could be updated inside the threaded loop for finer
        // granularity, but that would require a critical block which
        // might negatively affect performance. Doing it after the main
        // loop seems a reasonable trade off between possibly short
        // circuiting earlier vs performance hits if that does not happen
        uint64_t prevCount = currentCount;
        for (uint32_t tid=0; tid<nThreadsMax; ++tid) {
            uint32_t idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
            currentCount += (tCurrentCount[idx8] - prevCount);
        }
        if (ds->increment(false)) {
            break;
        }
    }
    // The accounting issue seems to have been fixed in
    // CAS-11504, but leave check just in case
    ThrowIf(currentCount != maxCount, "Accounting error");
    // merge the per-thread arrays
    for (uint32_t tid=0; tid<nThreadsMax; ++tid) {
        uint32_t idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        auto titer = tArys[idx8].cbegin();
        for_each(
            arys.begin(), arys.end(),
            [&titer](std::vector<AccumType>& ary) {
            ary.insert(ary.end(), titer->cbegin(), titer->cend());
            ++titer;
        });
    }
}

CASA_STATD std::vector<std::map<uint64_t, AccumType>>
ClassicalQuantileComputer<CASA_STATP>::_dataFromMultipleBins(
    const std::vector<StatsHistogram<AccumType>>& hist, uint64_t maxArraySize,
    const std::vector<IndexSet>& dataIndices, uint32_t nBins
) {
    // dataIndices are relative to minimum bin minimum border
    std::vector<CountedPtr<AccumType>> sameVal(hist.size(), nullptr);
    auto binCounts = _binCounts(sameVal, hist);
    auto iSameVal = sameVal.cbegin();
    auto bCountSet = binCounts.cbegin();
    auto iCountSet = bCountSet;
    auto iDesc = hist.cbegin();;
    std::map<AccumType, IndexValueMap> histToIdxValMap;
    std::vector<uint64_t> vnpts;
    std::vector<LimitPair> vlimits;
    std::vector<IndexSet> vindices;
    std::vector<std::map<uint64_t, uint64_t> > vNewToOld;
    // This is necessary for accounting. Map the lower limit of
    // a single bin to the lower limit of its associated histogram
    std::map<AccumType, AccumType> binToHistogramMap;
    // loop over sets of data indices
    for_each(
        dataIndices.cbegin(), dataIndices.cend(), [
            &iSameVal, &iDesc, &iCountSet, &vNewToOld, &vnpts, &vlimits,
            &binToHistogramMap, &vindices, &histToIdxValMap
        ](const IndexSet& idxSet) {
        auto iIdx = idxSet.cbegin();
        auto eIdx = idxSet.cend();
        const auto& maxBinLims = iDesc->getMaxBinLimits();
        if (iSameVal->null()) {
            // values in this histogram are not all the same
            auto iCounts = iCountSet->cbegin();
            auto eCounts = iCountSet->cend();
            uint64_t dataCount = 0;
            uint64_t prevDataCount = 0;
            uint64_t loopCount = 0;
            // loop over data indices pertaining to a single histogram
            // this cannot be made into a for_each loop, because iIdx can be
            // incremented multiple times inside the loop
            while (iIdx != eIdx) {
                ThrowIf(
                    iCounts == eCounts,
                    "Logic Error: ran out of bins, accounting error"
                );
                dataCount += *iCounts;
                if (*iIdx < dataCount) {
                    // datum at index exists in current bin
                    LimitPair histLimits;
                    histLimits.first = loopCount == 0
                        ? iDesc->getMinHistLimit() : maxBinLims[loopCount - 1];
                    histLimits.second = maxBinLims[loopCount];
                    IndexSet newDataIndices;
                    std::map<uint64_t, uint64_t> newToOld;
                    while(iIdx != eIdx && *iIdx < dataCount) {
                        // this loop takes into account that multiple
                        // indices could fall in the same bin
                        uint64_t oldIdx = *iIdx;
                        uint64_t newIdx = oldIdx - prevDataCount;
                        newDataIndices.insert(newIdx);
                        newToOld[newIdx] = oldIdx;
                        ++iIdx;
                    }
                    vNewToOld.push_back(newToOld);
                    vnpts.push_back(*iCounts);
                    vlimits.push_back(histLimits);
                    // because multiple single bins can be in the same
                    // histogram, we need to keep track of which bins belong
                    // to which histogram for accounting below
                    binToHistogramMap[histLimits.first]
                        = iDesc->getMinHistLimit();
                    vindices.push_back(newDataIndices);
                }
                prevDataCount = dataCount;
                ++iCounts;
                ++loopCount;
            }
        }
        else {
            // values in this histogram are all the same
            IndexValueMap mymap;
            for_each(
                idxSet.cbegin(), idxSet.cend(), [&mymap, &iSameVal]
                (uint64_t index) {
                mymap[index] = *(*iSameVal);
            });
            histToIdxValMap[iDesc->getMinHistLimit()] = mymap;
        }
        ++iSameVal;
        ++iCountSet;
        ++iDesc;
    });
    if (! vnpts.empty()) {
        auto dataFromBins = _dataFromSingleBins(
            vnpts, maxArraySize, vlimits, vindices, nBins
        );
        auto iNewToOld = vNewToOld.cbegin();
        auto iVLimits = vlimits.cbegin();
        for_each(
            dataFromBins.cbegin(), dataFromBins.cend(),
            [&iVLimits, &binToHistogramMap, &iNewToOld, &histToIdxValMap]
             (const IndexValueMap& idxValMap) {
            auto myHistKey = binToHistogramMap[iVLimits->first];
            IndexValueMap mymap;
            for_each(
                idxValMap.cbegin(), idxValMap.cend(), [&iNewToOld, &mymap]
                 (const std::pair<int64_t, AccumType>& mypair) {
                auto newIdx = mypair.first;
                auto oldIdx = iNewToOld->find(newIdx)->second;
                mymap[oldIdx] = mypair.second;
            });
            histToIdxValMap[myHistKey].insert(mymap.begin(), mymap.end());
            ++iNewToOld;
            ++iVLimits;
        });
    }
    std::vector<IndexValueMap> ret;
    for_each(
        hist.cbegin(), hist.cend(), [&ret, &histToIdxValMap]
         (const StatsHistogram<AccumType>& myhist) {
        ret.push_back(histToIdxValMap[myhist.getMinHistLimit()]);
    });
    return ret;
}

CASA_STATD std::vector<std::map<uint64_t, AccumType>>
ClassicalQuantileComputer<CASA_STATP>::_dataFromSingleBins(
    const BinCountArray& binNpts, uint64_t maxArraySize,
    const std::vector<LimitPair>& binLimits,
    const std::vector<IndexSet>& dataIndices, uint32_t nBins
) {
    // The uint64_t specification is required or else 0 will be interpreted as a
    // uint32_t and there will be overflow issues for totalNpts > (2**32)-1
    auto totalPts = std::accumulate(binNpts.begin(), binNpts.end(), uint64_t(0));
    if (totalPts <= maxArraySize) {
        // contents of bin is small enough to be sorted in memory, so
        // get the bin limits and stuff the good points within those limits
        // in an array and sort it
        std::vector<DataArray> dataArrays(binLimits.size(), DataArray(0));
        _createDataArrays(dataArrays, binLimits, totalPts);
        auto iNpts = binNpts.cbegin();
        for_each(
            dataArrays.cbegin(), dataArrays.cend(),
            [&iNpts](const DataArray& ary) {
            ThrowIf(
                ary.size() != *iNpts,
                "Logic Error: data array has " + String::toString(ary.size())
                + " elements but it should have " + String::toString(*iNpts)
                + ". Please file a bug report and include your dataset and "
                "your inputs"
            );
            ++iNpts;
        });
        iNpts = binNpts.begin();
        std::vector<IndexValueMap> ivMaps(binLimits.size());
        typename std::vector<IndexValueMap>::iterator iIVMaps = ivMaps.begin();
        auto iArrays = dataArrays.begin();
        for_each (
            dataIndices.cbegin(), dataIndices.cend(),
            [&iIVMaps, &iNpts, &iArrays](const IndexSet& idxSet) {
            uint64_t prevIdx = 0;
            for_each(
                idxSet.cbegin(), idxSet.cend(),
                [&iNpts, &iIVMaps, &iArrays, &prevIdx](uint64_t idx) {
                ThrowIf(
                    idx >= *iNpts,
                    "Logic Error: aryIdx " + String::toString(idx) + " is too "
                    "large. It should be no larger than "
                    + String::toString(*iNpts-1) + ". Please file a defect "
                    + "report and include your dataset and your inputs"
                );
                (*iIVMaps)[idx] = GenSort<AccumType>::kthLargest(
                    &((*iArrays)[prevIdx]), *iNpts - prevIdx, idx - prevIdx
                );
                prevIdx = idx;
            });
            ++iNpts;
            ++iArrays;
            ++iIVMaps;
        });
        return ivMaps;
    }
    else {
        // number of points is too large to fit in an array to be sorted, so
        // rebin those points into smaller bins
        // we want at least 1000 bins
        nBins = max(nBins, (uint32_t)1000);
        std::vector<StatsHistogram<AccumType>> hist;
        for_each(
            binLimits.cbegin(), binLimits.cend(),
            [&hist, &nBins](const LimitPair& myLimits) {
            StatsHistogram<AccumType> histogram(
                myLimits.first, myLimits.second, nBins
            );
            hist.push_back(histogram);
        });
        try {
            return _dataFromMultipleBins(
                hist, maxArraySize, dataIndices, nBins
            );
        }
        catch (const AipsError& x) {
            ThrowCc("Binning accounting error");
        }
    }
}

CASA_STATD
std::map<uint64_t, AccumType>
ClassicalQuantileComputer<CASA_STATP>::_indicesToValues(
    uint64_t mynpts, AccumType mymin, AccumType mymax, uint64_t maxArraySize,
    const IndexSet& indices, bool persistSortedArray, uint32_t nBins
) {
    IndexValueMap indexToValue;
    if (
        _valuesFromSortedArray(
            indexToValue, mynpts, indices, maxArraySize, persistSortedArray
        )
    ) {
        return indexToValue;
    }
    if (_doMedAbsDevMed) {
        auto median = this->_getMedian();
        ThrowIf(! median, "median is null");
        mymax = max(abs(mymax - *median), abs(mymin - *median));
        mymin = AccumType(0);
    }
    if (mymax == mymin) {
        // data set values are all the same
        for_each(
            indices.cbegin(), indices.cend(),
            [&indexToValue, mymin](uint64_t idx) {
            indexToValue[idx] = mymin;
        });
        return indexToValue;
    }
    std::vector<IndexSet> vindices(1, indices);
    // Avoiding having exceptions thrown over a wide range of use cases is
    // surprisingly dependent on the padding factor. 1e-2 seems a reasonable
    // setting to prevent this. It probably should not be set lower than this,
    // unless the factor is made dependent on the use case parameters eg,
    // the mymax - mymin difference.
    AccumType pad = 1e-2*(mymax - mymin);
    LimitPair limits(mymin - pad, mymax + pad);
    std::vector<LimitPair> vlimits(1, limits);
    BinCountArray vmynpts(1, mynpts);
    return _dataFromSingleBins(
        vmynpts, maxArraySize, vlimits, vindices, nBins
    )[0];
}

CASA_STATD
std::set<uint64_t> ClassicalQuantileComputer<CASA_STATP>::_medianIndices(
    uint64_t mynpts
) {
    IndexSet indices;
    if (mynpts % 2 == 0) {
        indices.insert(mynpts/2 - 1);
        indices.insert(mynpts/2);
    }
    else {
        indices.insert(mynpts/2);
    }
    return indices;
}

// Tried making this into an inline method, but performance decreased by 20 - 25% when
// finding the median and quartiles on a 200 Mpix image. So the #define seems to be
// the better choice from a performance standpoint.
#define _findBinCode \
    AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum; \
    if (myDatum >= bhist->getMinHistLimit() && myDatum < *maxLimit.rbegin()) { \
        /* datum may fall in one of the histograms */ \
        iCounts = bCounts; \
        iSameVal = bSameVal; \
        iAllSame = bAllSame; \
        ihist = bhist; \
        iMaxLimit = bMaxLimit; \
        /* loop over histograms */ \
        for (; ihist != ehist; ++ihist, ++iCounts, ++iSameVal, ++iAllSame, ++iMaxLimit) { \
            if (myDatum >= ihist->getMinHistLimit() && myDatum < *iMaxLimit) { \
                /* datum falls within the current histogram */ \
                auto idx = ihist->getIndex(myDatum); \
                ++(*iCounts)[idx]; \
                if (*iAllSame) { \
                    if (iSameVal->null()) { \
                        *iSameVal = new AccumType(myDatum); \
                    } \
                    else { \
                        *iAllSame = myDatum == *(*iSameVal); \
                        if (! *iAllSame) { \
                            *iSameVal = nullptr; \
                        } \
                    } \
                } \
                /* datum accounted for, so break */ \
                break; \
            } \
        } \
    }

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const std::vector<StatsHistogram<AccumType> >& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    uint64_t count = 0;
    while (count < nr) {
         _findBinCode
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude,
    const std::vector<StatsHistogram<AccumType>>& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride,
    const std::vector<StatsHistogram<AccumType> >& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    while (count < nr) {
        if (*mask) {
            _findBinCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude, const std::vector<StatsHistogram<AccumType> >& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride,
    const std::vector<StatsHistogram<AccumType>>& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _findBinCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude,
    const std::vector<StatsHistogram<AccumType> >& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType>>& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude,
    const std::vector<StatsHistogram<AccumType> >& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto weight = weightsBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _findBinCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<BinCountArray>& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const std::vector<StatsHistogram<AccumType> >& hist,
    const std::vector<AccumType>& maxLimit
) const {
    auto bCounts = binCounts.begin();
    auto iCounts = bCounts;
    auto bSameVal = sameVal.begin();
    auto iSameVal = bSameVal;
    auto bAllSame = allSame.begin();
    auto iAllSame = bAllSame;
    auto bhist = hist.cbegin();
    auto ihist = bhist;
    auto ehist = hist.cend();
    auto bMaxLimit = maxLimit.cbegin();
    auto iMaxLimit = bMaxLimit;
    auto datum = dataBegin;
    auto weight = weightBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _findBinCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is
// called inline to maximize performance
#define _populateArrayCode1 \
    AccumType myDatum = _doMedAbsDevMed \
        ? abs((AccumType)*datum - _myMedian) : *datum; \
    ary.push_back(myDatum);

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride
) const {
    uint64_t count = 0;
    auto datum = dataBegin;
    while (count < nr) {
        _populateArrayCode1
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr,
    uint32_t dataStride, const DataRanges& ranges, bool isInclude
) const {
    uint64_t count = 0;
    auto datum = dataBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride
) const {
    uint64_t count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArrayCode1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude
) const {
    uint64_t count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArrayCode1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride
) const {
    auto datum = dataBegin;
    auto weight = weightBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArrayCode1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude
) const {
    auto datum = dataBegin;
    auto weight = weightBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize
// performance. We make use of the fact that bins are in ascending order, so if
// datum is less than current bin minimum value, it will not be in any remaining
// bins and so we can break out of the loop without having to test each bin.
#define _populateArraysCode \
    AccumType myDatum = _doMedAbsDevMed \
        ? abs((AccumType)*datum - _myMedian) : *datum; \
    if ( \
        myDatum >= includeLimits.begin()->first \
        && myDatum < includeLimits.rbegin()->second \
    ) { \
        iIncludeLimits = bIncludeLimits; \
        iArys = bArys; \
        while (iIncludeLimits != eIncludeLimits) { \
            if (myDatum < iIncludeLimits->first) { \
                break; \
            } \
            if (myDatum < iIncludeLimits->second) { \
                iArys->push_back(myDatum); \
                ++currentCount; \
                if (currentCount == maxCount) { \
                    return; \
                } \
                break; \
            } \
            ++iIncludeLimits; \
            ++iArys; \
        } \
    }

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const IncludeLimits &includeLimits, uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uint64_t count = 0;
    auto datum = dataBegin;
    while (count < nr) {
        _populateArraysCode
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uint64_t count = 0;
    auto datum = dataBegin;
    auto beginRange = ranges.begin();
    auto endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uint64_t count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArraysCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude, const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    uint64_t count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const IncludeLimits& includeLimits,
    uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArraysCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uint64_t nr, uint32_t dataStride, const DataRanges& ranges, bool isInclude,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    auto datum = dataBegin;
    auto weight = weightBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArraysCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<DataArray>& arys, uint64_t& currentCount,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uint64_t nr, uint32_t dataStride, const MaskIterator& maskBegin, uint32_t maskStride,
    const DataRanges& ranges, bool isInclude,
    const IncludeLimits& includeLimits, uint64_t maxCount
) const {
    auto bArys = arys.begin();
    auto iArys = bArys;
    auto bIncludeLimits = includeLimits.cbegin();
    auto iIncludeLimits = bIncludeLimits;
    auto eIncludeLimits = includeLimits.cend();
    auto datum = dataBegin;
    auto weight = weightBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    uint32_t maxElements
) const {
    if (ary.size() + nr > maxElements) {
        return true;
    }
    uint64_t count = 0;
    auto datum = dataBegin;
    while (count < nr) {
        ary.push_back(
            _doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum
        );
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, dataStride
        );
    }
    return false;
}

// define rather than make a method to ensure this is called inline to maximize
// performance
#define _PopulateTestArrayCode \
    ary.push_back( \
        _doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum \
    ); \
    ++npts; \
    if (npts > maxElements) { \
        return true; \
    }

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude, uint32_t maxElements
) const {
    uint64_t count = 0;
    auto npts = ary.size();
    auto datum = dataBegin;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(datum, count, dataStride);
    }
    return false;
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, uint32_t maxElements
) const {
    uint64_t count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto npts = ary.size();
    while (count < nr) {
        if (*mask) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return false;
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude, uint32_t maxElements
) const {
    uint64_t count = 0;
    auto datum = dataBegin;
    auto mask = maskBegin;
    auto npts = ary.size();
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return false;
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    uint32_t maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    auto npts = ary.size();
    while (count < nr) {
        if (*weight > 0) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
    return false;
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uint64_t nr, uint32_t dataStride,
    const DataRanges& ranges, bool isInclude, uint32_t maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightsBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    uint32_t npts = ary.size();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, dataStride
        );
    }
    return false;
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, uint32_t maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    auto npts = ary.size();
    while (count < nr) {
        if (*mask && *weight > 0) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return false;
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    DataArray& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightBegin, uint64_t nr, uint32_t dataStride,
    const MaskIterator& maskBegin, uint32_t maskStride, const DataRanges& ranges,
    bool isInclude, uint32_t maxElements
) const {
    auto datum = dataBegin;
    auto weight = weightBegin;
    auto mask = maskBegin;
    uint64_t count = 0;
    auto beginRange = ranges.cbegin();
    auto endRange = ranges.cend();
    uint32_t npts = ary.size();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<CASA_STATQ>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return false;
}

CASA_STATD
bool ClassicalQuantileComputer<CASA_STATP>::_valuesFromSortedArray(
    IndexValueMap& values, uint64_t mynpts, const std::set<uint64_t>& indices,
    uint64_t maxArraySize, bool persistSortedArray
) {
    values.clear();
    // I need a little wiggle room, the caller can't make the maximum array size
    // ridiculously small
    maxArraySize = max(maxArraySize, (uint64_t)1000);
    DataArray myArray;
    if (_doMedAbsDevMed && ! this->_getSortedArray().empty()) {
        // make a copy
        auto pSorted = this->_getSortedArray();
        myArray = pSorted;
        StatisticsUtilities<AccumType>::convertToAbsDevMedArray(
            myArray, *this->_getMedian()
        );
    }
    if (! _doMedAbsDevMed) {
        myArray = this->_getSortedArray();
    }
    if (myArray.empty()) {
        // object that contains this object is always responsible for passing
        // correct value of npts, so npts should be set properly here and should
        // be greater than zero
        if (mynpts <= maxArraySize) {
            // npts is smaller than the max array size, so create the array and
            // sort it in memory
            _createDataArray(myArray);
        }
        else {
            // data is too large to be sorted in memory
            return false;
        }
    }
    values = StatisticsUtilities<AccumType>::indicesToValues(myArray, indices);
    if (! _doMedAbsDevMed) {
        this->_setSortedArray(persistSortedArray ? myArray : DataArray());
    }
    return true;
}

}

#endif
