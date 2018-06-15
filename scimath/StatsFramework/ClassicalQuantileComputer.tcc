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

#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/scimath/StatsFramework/StatisticsIncrementer.h>

namespace casacore {

// The default constructor is disallowed
CASA_STATD
ClassicalQuantileComputer<CASA_STATP>::ClassicalQuantileComputer()
    : StatisticsAlgorithmQuantileComputer<CASA_STATP>(NULL), _doMedAbsDevMed(False),
      _myMedian(0) {}

CASA_STATD
ClassicalQuantileComputer<CASA_STATP>::ClassicalQuantileComputer(
    StatisticsDataset<CASA_STATP>* dataset
) : StatisticsAlgorithmQuantileComputer<CASA_STATP>(dataset), _doMedAbsDevMed(False),
    _myMedian(0) {}

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
    uInt64 mynpts, AccumType mymin,
    AccumType mymax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt nBins
) {
    cout << "getMedian mymin/mymax " << mymin << " " << mymax << endl;
    CountedPtr<AccumType> median = this->_getMedian();
    if (! median) {
        std::set<uInt64> indices = _medianIndices(mynpts);
        std::map<uInt64, AccumType> indexToValue = _indicesToValues(
            mynpts, mymin, mymax,
            binningThreshholdSizeBytes/sizeof(AccumType),
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
    uInt64 mynpts, AccumType mymin, AccumType mymax,
    uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt nBins
) {
    CountedPtr<AccumType> medAbsDevMed = this->_getMedianAbsDevMedian();
    if (! medAbsDevMed) {
        // This call calculates the median of the data set which is stored internally and
        // used, but is not necessary to be captured in the return value here.
        getMedian(
            mynpts, mymin, mymax, binningThreshholdSizeBytes,
            persistSortedArray, nBins
        );
        std::set<uInt64> indices = _medianIndices(mynpts);
        // throw the proper switch
        _doMedAbsDevMed = True;
        _myMedian = *this->_getMedian();
        std::map<uInt64, AccumType> indexToValue = _indicesToValues(
            mynpts, mymin, mymax,
            binningThreshholdSizeBytes/sizeof(AccumType),
            indices, persistSortedArray, nBins
        );
        _doMedAbsDevMed = False;
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
    std::map<Double, AccumType>& quantiles, const std::set<Double>& fractions,
    uInt64 mynpts, AccumType mymin, AccumType mymax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt nBins
) {
    std::set<uInt64> medianIndices;
    quantiles.clear();
    CountedPtr<AccumType> median = this->_getMedian();
    if (! median) {
        medianIndices = _medianIndices(mynpts);
    }
    std::map<Double, uInt64> QuantileComputerToIndex = StatisticsData::indicesFromFractions(
        mynpts, fractions
    );
    std::set<uInt64> indices = medianIndices;
    std::map<Double, uInt64>::const_iterator qToIIter = QuantileComputerToIndex.begin();
    std::map<Double, uInt64>::const_iterator qToIEnd = QuantileComputerToIndex.end();
    for(; qToIIter != qToIEnd; ++qToIIter) {
        indices.insert(qToIIter->second);
    }
    std::map<uInt64, AccumType> indexToValue = _indicesToValues(
        mynpts, mymin, mymax,
        binningThreshholdSizeBytes/sizeof(AccumType),
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
    std::set<Double>::const_iterator fIter = fractions.begin();
    std::set<Double>::const_iterator fEnd = fractions.end();
    for (; fIter != fEnd; ++fIter) {
        quantiles[*fIter] = indexToValue[QuantileComputerToIndex[*fIter]];
    }
    return *median;
}

CASA_STATD
std::map<Double, AccumType> ClassicalQuantileComputer<CASA_STATP>::getQuantiles(
    const std::set<Double>& fractions, uInt64 mynpts, AccumType mymin,
    AccumType mymax, uInt binningThreshholdSizeBytes,
    Bool persistSortedArray, uInt nBins
) {
    if (fractions.empty()) {
        return std::map<Double, AccumType>();
    }
    ThrowIf(
        *fractions.begin() <= 0 || *fractions.rbegin() >= 1,
        "Value of all quantiles must be between 0 and 1 (noninclusive)"
    );
    std::map<Double, uInt64> QuantileComputerToIndexMap = StatisticsData::indicesFromFractions(
        mynpts, fractions
    );
    // This seemingly convoluted way of doing things with maps is necessary because
    // multiple quantiles can map to the same sorted array index, and multiple array
    // indices can map the same value if the values in the array are not unique.
    std::map<Double, AccumType> QuantileComputerToValue;
    std::set<uInt64> uniqueIndices;
    std::map<Double, uInt64>::const_iterator qToIIter = QuantileComputerToIndexMap.begin();
    std::map<Double, uInt64>::const_iterator qToIEnd = QuantileComputerToIndexMap.end();
    for (; qToIIter != qToIEnd; ++qToIIter) {
        uniqueIndices.insert(qToIIter->second);
    }
    std::map<uInt64, AccumType> indexToValue = _indicesToValues(
        mynpts, mymin, mymax,
        binningThreshholdSizeBytes/sizeof(AccumType),
        uniqueIndices, persistSortedArray, nBins
    );
    for (qToIIter = QuantileComputerToIndexMap.begin(); qToIIter != qToIEnd; ++qToIIter) {
        Double QuantileComputer = qToIIter->first;
        uInt64 index = qToIIter->second;
        QuantileComputerToValue[QuantileComputer] = indexToValue[index];
    }
    return QuantileComputerToValue;
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::reset() {
    StatisticsAlgorithmQuantileComputer<CASA_STATP>::reset();
    _doMedAbsDevMed = False;
}

CASA_STATD
std::vector<std::vector<uInt64> > ClassicalQuantileComputer<CASA_STATP>::_binCounts(
    std::vector<CountedPtr<AccumType> >& sameVal,
    const std::vector<StatsHistogram<AccumType> >& hist
) {
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bDesc = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator iDesc = bDesc;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator eDesc = hist.end();
    if (hist.size() > 1) {
        // initialize only to squash compiler warning
        StatsHistogram<AccumType> prevDesc = *bDesc;
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
    std::vector<Bool> allSame(hist.size(), True);
    // the elements in the outer vector are histograms. The elements in the inner
    // vector are the bins in the corresponding histograms. The Int64 elements
    // are the number of data points in those bins
    std::vector<std::vector<uInt64> > bins(hist.size());
    std::vector<std::vector<uInt64> >::iterator iBins = bins.begin();
    std::vector<std::vector<uInt64> >::iterator eBins = bins.end();
    // initialize all bin counts to 0
    for (iDesc = bDesc; iBins!=eBins; ++iBins, ++iDesc) {
        *iBins = std::vector<uInt64>(iDesc->getNBins(), 0);
    }
    // same val indicates if all values in a histogram (the vector elements) are the same
    sameVal = std::vector<CountedPtr<AccumType> >(hist.size(), NULL);
    // maxLimit are the maximum limits for each histogram. set them here.
    std::vector<AccumType> maxLimit(hist.size());
    typename std::vector<AccumType>::iterator iMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::iterator eMaxLimit = maxLimit.end();
    for (iDesc=bDesc; iMaxLimit!=eMaxLimit; ++iMaxLimit, ++iDesc) {
        *iMaxLimit = iDesc->getMaxHistLimit();
    }
    StatisticsDataset<CASA_STATP>* ds = this->_getDataset();
    ds->initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds->getDataProvider()
    );
    // The PtrHolders hold references to C arrays of length
    // ClassicalStatisticsData::CACHE_PADDING*nThreadsMax.
    // Only every CACHE_PADDING*nth element will be populated
    PtrHolder<std::vector<std::vector<uInt64> > > tBins(
        new std::vector<std::vector<uInt64> >[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<std::vector<CountedPtr<AccumType> > > tSameVal(
        new std::vector<CountedPtr<AccumType> >[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<std::vector<Bool> > tAllSame(
        new std::vector<Bool>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        tBins[idx8] = bins;
        tSameVal[idx8] = sameVal;
        tAllSame[idx8] = allSame;
    }
    while (True) {
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk = ds->initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        ds->initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
                ? extra : ClassicalStatisticsData::BLOCK_SIZE;
            _computeBins(
                tBins[idx8], tSameVal[idx8], tAllSame[idx8],
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                dataCount, hist, maxLimit, chunk
            );
            ds->incrementThreadIters(
                dataIter[idx8], maskIter[idx8], weightsIter[idx8],
                offset[idx8], nthreads
            );
        }
        if (ds->increment(False)) {
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
    std::vector<std::vector<uInt64> >& bins, std::vector<CountedPtr<AccumType> >& sameVal,
    std::vector<Bool>& allSame, DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 count,
    const std::vector<StatsHistogram<AccumType> >& hist,
    const std::vector<AccumType>& maxLimit,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _findBins(
                    bins, sameVal, allSame, dataIter, weightsIter, count,
                    chunk.dataStride, maskIter, chunk.mask->second, chunk.ranges->first,
                    chunk.ranges->second, hist, maxLimit
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
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it. No filtering of the data is necessary.
        _findBins(
            bins, sameVal, allSame, dataIter,
            count, chunk.dataStride, hist, maxLimit
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_createDataArray(
    std::vector<AccumType>& ary
) {
    StatisticsDataset<CASA_STATP>* ds = this->_getDataset();
    ds->initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds->getDataProvider()
    );
    PtrHolder<std::vector<AccumType> > tAry(
        new std::vector<AccumType>[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    while (True) {
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk = ds->initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        ds->initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
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
        if (ds->increment(False)) {
            break;
        }
    }
    // merge the per-thread arrays
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        const std::vector<AccumType>& v = tAry[ClassicalStatisticsData::CACHE_PADDING*tid];
        ary.insert(ary.end(), v.begin(), v.end());
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_computeDataArray(
    std::vector<AccumType>& ary, DataIterator dataIter,
    MaskIterator maskIter, WeightsIterator weightsIter,
    uInt64 dataCount,
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
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it, and its stride is 1. No filtering of the data is necessary.
        _populateArray(
            ary, dataIter, dataCount, chunk.dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_computeDataArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount,
    DataIterator dataIter, MaskIterator maskIter,
    WeightsIterator weightsIter, uInt64 dataCount,
    const std::vector<std::pair<AccumType, AccumType> >& includeLimits,
    uInt64 maxCount,
    const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk
) {
    if (chunk.weights) {
        if (chunk.mask) {
            if (chunk.ranges) {
                _populateArrays(
                    arys, currentCount, dataIter, weightsIter, dataCount,
                    chunk.dataStride, maskIter, chunk.mask->second, chunk.ranges->first,
                    chunk.ranges->second, includeLimits, maxCount
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
        // simplest case, this data set has no weights, no mask, nor any ranges associated
        // with it, and its stride is 1. No filtering of the data is necessary.
        _populateArrays(
            arys, currentCount, dataIter, dataCount,
            chunk.dataStride, includeLimits, maxCount
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_createDataArrays(
    std::vector<std::vector<AccumType> >& arys,
    const std::vector<std::pair<AccumType, AccumType> >& includeLimits,
    uInt64 maxCount
) {
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iLimits = bLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eLimits = includeLimits.end();
    std::pair<AccumType, AccumType> prevLimits;
    while(iLimits != eLimits) {
        // sanity checks
        if (iLimits->first >= iLimits->second) {
            ostringstream os;
            os << "Logic Error: bin limits are nonsensical: " << *iLimits;
            ThrowCc(os.str());
        }
        if (iLimits != bLimits) {
            if (
                iLimits->first <= prevLimits.first
                || iLimits->second <= prevLimits.second
            ) {
                ostringstream os;
                os << "Logic Error: bin limits are not in order: " << prevLimits << " , " << *iLimits;
                ThrowCc(os.str());
            }
        }
        prevLimits = *iLimits;
        ++iLimits;
    }
    StatisticsDataset<CASA_STATP>* ds = this->_getDataset();
    ds->initIterators();
    const uInt nThreadsMax = StatisticsUtilities<AccumType>::nThreadsMax(
        ds->getDataProvider()
    );
    PtrHolder<std::vector<std::vector<AccumType> > > tArys(
        new std::vector<std::vector<AccumType> >[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    PtrHolder<uInt64> tCurrentCount(
        new uInt64[
            ClassicalStatisticsData::CACHE_PADDING*nThreadsMax
        ], True
    );
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        tArys[idx8] = arys;
    }
    uInt64 currentCount = 0;
    while (currentCount < maxCount) {
        const typename StatisticsDataset<CASA_STATP>::ChunkData& chunk = ds->initLoopVars();
        uInt nBlocks, nthreads;
        uInt64 extra;
        PtrHolder<DataIterator> dataIter;
        PtrHolder<MaskIterator> maskIter;
        PtrHolder<WeightsIterator> weightsIter;
        PtrHolder<uInt64> offset;
        ds->initThreadVars(
            nBlocks, extra, nthreads, dataIter,
            maskIter, weightsIter, offset, nThreadsMax
        );
        for (uInt tid=0; tid<nThreadsMax; ++tid) {
            uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
            tCurrentCount[idx8] = currentCount;
        }
#ifdef _OPENMP
#pragma omp parallel for num_threads(nthreads)
#endif
        for (uInt i=0; i<nBlocks; ++i) {
            uInt idx8 = StatisticsUtilities<AccumType>::threadIdx();
            uInt64 dataCount = chunk.count - offset[idx8] < ClassicalStatisticsData::BLOCK_SIZE
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
        uInt64 prevCount = currentCount;
        for (uInt tid=0; tid<nThreadsMax; ++tid) {
            uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
            currentCount += (tCurrentCount[idx8] - prevCount);
        }
        if (ds->increment(False)) {
            break;
        }
    }
    // CAS-10906 This appears to happen extremely rarely (one report in several years).
    // To the best of my determination, it happens when the data iterator type and
    // the AccumType are not the same (eg Float and Double, respectively), because
    // the data are implicitly cast to AccumType. This slight value change at the data type
    // precision level apparently can, in very rare instances, cause a data point that
    // would fall exactly at the upper edge of a bin (and so under normal circumstances
    // would not be included in that bin), to fall below the upper edge and so be erroneously
    // be included in that bin, leading to an unexpected data point count in that bin.
    // The perfect solution would probably be not to do the casting and also make the bin limits
    // the same type as the data type, but in practice this requires changes in many, many places
    // in this code, and some of these changes may not be internally consistent. And in the end,
    // I'm not 100% certain this would resolve the issue. The simplest solution for such an
    // infrequently occurring problem is simply to change the binning configuration (which is
    // already pretty arbitrary) so that the bin edges fall elsewhere and try again. This
    // is what is done in _dataFromSingleBins() which catches this exception and then changes
    // the binning configuration and tries again.
    // Smarter binning at the outset might also help, for example, bins now have uniform widths,
    // but most of the datasets we deal with have a pseudo Gaussian distribution. So, this issue
    // is more likely to occur in densely populated bins (and in fact, the one report of this
    // issue was tied to computing the median of such a distribution where the associated bin
    // would have had (nearly) the largest number of points of all other bins. So, a binning
    // configuration where bins are narrower near the distribution center so that the number of
    // points per bin is about constant for all bins might make more sense. But again, that's a
    // significant undertaking, and for a single event every few years, seems over engineering
    // at this point, not to mention that such a configuration is potentially computationally
    // expensive. In the end, this code may, at some very low probability, always be vulnerable
    // to these types of machine precision issues. - dmehring 16nov2017
    ThrowIf(
        currentCount != maxCount, "Accounting error"
    );
    // merge the per-thread arrays
    for (uInt tid=0; tid<nThreadsMax; ++tid) {
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        typename std::vector<std::vector<AccumType> >::iterator iter = arys.begin();
        typename std::vector<std::vector<AccumType> >::iterator end = arys.end();
        typename std::vector<std::vector<AccumType> >::const_iterator titer = tArys[idx8].begin();
        for (; iter!=end; ++iter, ++titer) {
            iter->insert(iter->end(), titer->begin(), titer->end());
        }
    }
}

// TODO for better clarity, a struct composed of hist and dataIndices should
// be made to more clearly indicate that those vectors are tied together
// TODO make nBins (everywhere) a uInt rather than a uInt64. If we ever need
// more than 2 billion bins, something is very wrong.
CASA_STATD std::vector<std::map<uInt64, AccumType> >
ClassicalQuantileComputer<CASA_STATP>::_dataFromMultipleBins(
    const std::vector<StatsHistogram<AccumType> >& hist,
    uInt64 maxArraySize, const std::vector<std::set<uInt64> >& dataIndices, uInt nBins
) {
    // dataIndices are relative to minimum bin minimum border
    std::vector<CountedPtr<AccumType> > sameVal(hist.size(), NULL);
    std::vector<std::vector<uInt64> > binCounts = _binCounts(sameVal, hist);
    std::vector<std::set<uInt64> >::const_iterator bIdxSet = dataIndices.begin();
    std::vector<std::set<uInt64> >::const_iterator iIdxSet = bIdxSet;
    std::vector<std::set<uInt64> >::const_iterator eIdxSet = dataIndices.end();
    typename std::vector<CountedPtr<AccumType> >::const_iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::const_iterator iSameVal = bSameVal;
    std::vector<std::vector<uInt64> >::const_iterator bCountSet = binCounts.begin();
    std::vector<std::vector<uInt64> >::const_iterator iCountSet = bCountSet;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bDesc = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator iDesc = bDesc;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator eDesc = hist.end();
    std::map<AccumType, std::map<uInt64, AccumType> > histToIdxValMap;
    std::vector<uInt64> vnpts;
    std::vector<std::pair<AccumType, AccumType> > vlimits;
    std::vector<std::set<uInt64> > vindices;
    std::vector<std::map<uInt64, uInt64> > vNewToOld;
    // This is necessary for accounting. Map the lower limit of
    // a single bin to the lower limit of its associated histogram
    std::map<AccumType, AccumType> binToHistogramMap;
    // loop over sets of data indices
    while (iIdxSet != eIdxSet) {
        std::set<uInt64>::const_iterator iIdx = iIdxSet->begin();
        std::set<uInt64>::const_iterator eIdx = iIdxSet->end();
        const std::vector<AccumType>& maxBinLims = iDesc->getMaxBinLimits();
        if (iSameVal->null()) {
            // values in this histogram are not all the same
            std::vector<uInt64>::const_iterator bCounts = iCountSet->begin();
            std::vector<uInt64>::const_iterator iCounts = bCounts;
            std::vector<uInt64>::const_iterator eCounts = iCountSet->end();
            uInt64 dataCount = 0;
            uInt64 prevDataCount = 0;
            uInt64 loopCount = 0;
            // loop over data indices pertaining to a single histogram
            while (iIdx != eIdx) {
                ThrowIf(iCounts == eCounts, "Logic Error: ran out of bins, accounting error");
                dataCount += *iCounts;
                if (*iIdx < dataCount) {
                    // datum at index exists in current bin
                    std::pair<AccumType, AccumType> histLimits;
                    if (loopCount == 0) {
                        histLimits.first = iDesc->getMinHistLimit();
                    }
                    else {
                        histLimits.first = maxBinLims[loopCount - 1];
                    }
                    histLimits.second = maxBinLims[loopCount];
                    std::set<uInt64> newDataIndices;
                    std::map<uInt64, uInt64> newToOld;
                    while(iIdx != eIdx && *iIdx < dataCount) {
                        // this loop takes into account that multiple indices
                        // could fall in the same bin
                        uInt64 oldIdx = *iIdx;
                        uInt64 newIdx = oldIdx - prevDataCount;
                        newDataIndices.insert(newIdx);
                        newToOld[newIdx] = oldIdx;
                        ++iIdx;
                    }
                    vNewToOld.push_back(newToOld);
                    vnpts.push_back(*iCounts);
                    vlimits.push_back(histLimits);
                    // because multiple single bins can be in the same histogram,
                    // we need to keep track of which bins belong to which histogram
                    // for accounting below
                    binToHistogramMap[histLimits.first] = iDesc->getMinHistLimit();
                    vindices.push_back(newDataIndices);
                }
                prevDataCount = dataCount;
                ++iCounts;
                ++loopCount;
            }
        }
        else {
            // values in this histogram are all the same
            std::map<uInt64, AccumType> mymap;
            while (iIdx != eIdx) {
                mymap[*iIdx] = *(*iSameVal);
                ++iIdx;
            }
            histToIdxValMap[iDesc->getMinHistLimit()] = mymap;
        }
        ++iIdxSet;
        ++iSameVal;
        ++iCountSet;
        ++iDesc;
    }
    if (! vnpts.empty()) {
        std::vector<std::map<uInt64, AccumType> > dataFromBins = _dataFromSingleBins(
            vnpts, maxArraySize, vlimits, vindices, nBins
        );
        typename std::vector<std::map<uInt64, AccumType> >::const_iterator iDataSet = dataFromBins.begin();
        typename std::vector<std::map<uInt64, AccumType> >::const_iterator eDataSet = dataFromBins.end();
        std::vector<std::map<uInt64, uInt64> >::iterator iNewToOld = vNewToOld.begin();
        typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iVLimits = vlimits.begin();
        while(iDataSet != eDataSet) {
            AccumType myHistKey = binToHistogramMap[iVLimits->first];
            std::map<uInt64, AccumType> mymap;
            typename std::map<uInt64, AccumType>::const_iterator iData = iDataSet->begin();
            typename std::map<uInt64, AccumType>::const_iterator eData = iDataSet->end();
            while(iData != eData) {
                uInt64 newIdx = iData->first;
                uInt64 oldIdx = (*iNewToOld)[newIdx];
                mymap[oldIdx] = iData->second;
                ++iData;
            }
            histToIdxValMap[myHistKey].insert(mymap.begin(), mymap.end());
            ++iNewToOld;
            ++iDataSet;
            ++iVLimits;
        }
    }
    std::vector<std::map<uInt64, AccumType> > ret;
    iDesc = bDesc;
    while (iDesc != eDesc) {
        ret.push_back(histToIdxValMap[iDesc->getMinHistLimit()]);
        ++iDesc;
    }
    return ret;
}

CASA_STATD std::vector<std::map<uInt64, AccumType> >
ClassicalQuantileComputer<CASA_STATP>::_dataFromSingleBins(
    const std::vector<uInt64>& binNpts, uInt64 maxArraySize,
    const std::vector<LimitPair>& binLimits,
    const std::vector<IndexSet>& dataIndices, uInt nBins
) {
    uInt64 totalPts = std::accumulate(binNpts.begin(), binNpts.end(), 0);
    if (totalPts <= maxArraySize) {
        // contents of bin is small enough to be sorted in memory, so
        // get the bin limits and stuff the good points within those limits
        // in an array and sort it
        std::vector<DataArray> dataArrays(binLimits.size(), DataArray(0));
        _createDataArrays(dataArrays, binLimits, totalPts);
        std::vector<uInt64>::const_iterator iNpts = binNpts.begin();
        typename std::vector<DataArray>::iterator iArrays = dataArrays.begin();
        typename std::vector<DataArray>::iterator eArrays = dataArrays.end();
        for (; iArrays != eArrays; ++iArrays, ++iNpts) {
            ThrowIf(
                iArrays->size() != *iNpts,
                "Logic Error: data array has "
                + String::toString(iArrays->size()) + " elements but it should "
                + "have " + String::toString(*iNpts)   + ". Please file a bug "
                + "report and include your dataset and your inputs"
            );
        }
        std::vector<IndexSet>::const_iterator iIdxSet = dataIndices.begin();
        std::vector<IndexSet>::const_iterator eIdxSet = dataIndices.end();
        iNpts = binNpts.begin();
        std::vector<IndexValueMap> ivMaps(binLimits.size());
        typename std::vector<IndexValueMap>::iterator iIVMaps = ivMaps.begin();
        iArrays = dataArrays.begin();
        for(; iIdxSet != eIdxSet; ++iIdxSet, ++iNpts, ++iArrays, ++iIVMaps) {
            IndexSet::const_iterator initer = iIdxSet->begin();
            IndexSet::const_iterator inend = iIdxSet->end();
            uInt64 prevIdx = 0;
            for (; initer != inend; ++initer) {
                ThrowIf(
                    *initer >= *iNpts,
                    "Logic Error: aryIdx " + String::toString(*initer)
                    + " is too large. It should be no larger than "
                    + String::toString(*iNpts-1) + ". Please file a defect "
                    + "report and include your dataset and your inputs"
                );
                (*iIVMaps)[*initer] = GenSort<AccumType>::kthLargest(
                    &((*iArrays)[prevIdx]), *iNpts - prevIdx, *initer - prevIdx
                );
                prevIdx = *initer;
            }
        }
        return ivMaps;
    }
    else {
        // number of points is too large to fit in an array to be sorted, so
        // rebin those points into smaller bins
        // static uInt maxLoopCount = 5;
        // uInt loopCount = 0;
        // we want at least 1000 bins
        nBins = max(nBins, (uInt)1000);
        // while (True) {
            LimitPairVectorIter iLimits = binLimits.begin();
            LimitPairVectorIter eLimits = binLimits.end();
            std::vector<StatsHistogram<AccumType> > hist;
            for (; iLimits != eLimits; ++iLimits) {
                StatsHistogram<AccumType> histogram(
                    iLimits->first, iLimits->second, nBins
                );
                hist.push_back(histogram);
            }
            try {
                return _dataFromMultipleBins(
                    hist, maxArraySize, dataIndices, nBins
                );
            }
            catch (const AipsError& x) {
                // TODO this now appears to be fixed, hopefully.
                // reconfigure bins and try again. This happens very rarely.
                // See comment in _createDataArrays() at the source of the issue
                // ThrowIf(loopCount == maxLoopCount, "Tried 5 times, giving up");
                // increase nBins by an irrational multiplier slightly greater than 1
                // uInt nBinsPrev = nBins;
                // nBins *= (C::pi/3.0);
                // LogIO log;
                // log << LogIO::WARN << "Accounting error, changing number of bins from "
                 //   << nBinsPrev << " to " << nBins << " and recomputing." << LogIO::POST;
                // ++loopCount;
                ThrowCc("Binning accounting error");
            }
        // }
    }
}

CASA_STATD
std::map<uInt64, AccumType> ClassicalQuantileComputer<CASA_STATP>::_indicesToValues(
    uInt64 mynpts, AccumType mymin, AccumType mymax, uInt64 maxArraySize,
    const IndexSet& indices, Bool persistSortedArray, uInt nBins
) {
    std::map<uInt64, AccumType> indexToValue;
    if (
        _valuesFromSortedArray(
            indexToValue, mynpts, indices, maxArraySize,
            persistSortedArray
        )
    ) {
        return indexToValue;
    }
    if (_doMedAbsDevMed) {
        CountedPtr<AccumType> median = this->_getMedian();
        ThrowIf(! median, "median is null");
        mymax = max(abs(mymax - *median), abs(mymin - *median));
        mymin = AccumType(0);
    }
    if (mymax == mymin) {
        // data set values are all the same
        IndexSet::const_iterator iter = indices.begin();
        IndexSet::const_iterator end = indices.end();
        for(; iter!=end; ++iter) {
            indexToValue[*iter] = mymin;
        }
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
    std::vector<uInt64> vmynpts(1, mynpts);
    return _dataFromSingleBins(
        vmynpts, maxArraySize, vlimits, vindices, nBins
    )[0];
}

CASA_STATD
std::set<uInt64> ClassicalQuantileComputer<CASA_STATP>::_medianIndices(
    uInt64 mynpts
) {
    std::set<uInt64> indices;
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
    /* TODO should now be able to get maxLimit directly from the last histogram */ \
    if (myDatum >= bhist->getMinHistLimit() && myDatum < *maxLimit.rbegin()) { \
        /* datum may fall in one of the histograms */ \
        iCounts = bCounts; \
        iSameVal = bSameVal; \
        iAllSame = bAllSame; \
        ihist = bhist; \
        iMaxLimit = bMaxLimit; \
        /* loop over histograms */ \
        while (ihist != ehist) { \
            if (myDatum >= ihist->getMinHistLimit() && myDatum < *iMaxLimit) { \
                /* datum falls within the current histogram */ \
                uInt idx = ihist->getIndex(myDatum); \
                ++(*iCounts)[idx]; \
                if (*iAllSame) { \
                    if (iSameVal->null()) { \
                        *iSameVal = new AccumType(myDatum); \
                    } \
                    else { \
                        *iAllSame = myDatum == *(*iSameVal); \
                        if (! *iAllSame) { \
                            *iSameVal = NULL; \
                        } \
                    } \
                } \
                break; \
            } \
            ++iCounts; \
            ++iSameVal; \
            ++iAllSame; \
            ++ihist; \
            ++iMaxLimit; \
        } \
    }

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    uInt64 count = 0;
    while (count < nr) {
         _findBinCode
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, const DataRanges& ranges,
    Bool isInclude,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightsBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
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
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_findBins(
    std::vector<std::vector<uInt64> >& binCounts,
    std::vector<CountedPtr<AccumType> >& sameVal, std::vector<Bool>& allSame,
    const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<StatsHistogram<AccumType> >& hist, const std::vector<AccumType>& maxLimit
) const {
    std::vector<std::vector<uInt64> >::iterator bCounts = binCounts.begin();
    std::vector<std::vector<uInt64> >::iterator iCounts = bCounts;
    typename std::vector<CountedPtr<AccumType> >::iterator bSameVal = sameVal.begin();
    typename std::vector<CountedPtr<AccumType> >::iterator iSameVal = bSameVal;
    std::vector<Bool>::iterator bAllSame = allSame.begin();
    std::vector<Bool>::iterator iAllSame = bAllSame;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator bhist = hist.begin();
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ihist = bhist;
    typename std::vector<StatsHistogram<AccumType> >::const_iterator ehist = hist.end();
    typename std::vector<AccumType>::const_iterator bMaxLimit = maxLimit.begin();
    typename std::vector<AccumType>::const_iterator iMaxLimit = bMaxLimit;
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _findBinCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _populateArrayCode1 \
    AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum; \
    ary.push_back(myDatum);

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride
) const {
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    while (count < nr) {
        _populateArrayCode1
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude
) const {
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
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
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
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
            _populateArrayCode1
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

// define rather than make a method to ensure this is called inline to maximize performance
// We make use of the fact that bins are in ascending order, so if datum is
// less than current bin minimum value, it will not be in any remaining bins and
// so we can break out of the loop without having to test each bin.
#define _populateArraysCode \
    AccumType myDatum = _doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum; \
    if (myDatum >= includeLimits.begin()->first && myDatum < includeLimits.rbegin()->second) { \
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
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    while (count < nr) {
        _populateArraysCode
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    while (count < nr) {
        if (*mask) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*weight > 0) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
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
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    while (count < nr) {
        if (*mask && *weight > 0) {
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
void ClassicalQuantileComputer<CASA_STATP>::_populateArrays(
    std::vector<std::vector<AccumType> >& arys, uInt64& currentCount, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude,
    const std::vector<std::pair<AccumType, AccumType> > &includeLimits, uInt64 maxCount
) const {
    typename std::vector<std::vector<AccumType> >::iterator bArys = arys.begin();
    typename std::vector<std::vector<AccumType> >::iterator iArys = bArys;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator bIncludeLimits = includeLimits.begin();
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator iIncludeLimits = bIncludeLimits;
    typename std::vector<std::pair<AccumType, AccumType> >::const_iterator eIncludeLimits = includeLimits.end();
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
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
            _populateArraysCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr, uInt dataStride,
    uInt maxElements
) const {
    if (ary.size() + nr > maxElements) {
        return True;
    }
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    while (count < nr) {
        ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum);
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
    return False;
}

// define rather than make a method to ensure this is called inline to maximize performance
#define _PopulateTestArrayCode \
    ary.push_back(_doMedAbsDevMed ? abs((AccumType)*datum - _myMedian) : *datum); \
    ++npts; \
    if (npts > maxElements) { \
        return True; \
    }

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const DataRanges& ranges, Bool isInclude,
    uInt maxElements
) const {
    uInt64 count = 0;
    uInt npts = ary.size();
    DataIterator datum = dataBegin;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    uInt maxElements
) const {
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt npts = ary.size();
    while (count < nr) {
        if (*mask) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, uInt64 nr,
    uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    uInt64 count = 0;
    DataIterator datum = dataBegin;
    MaskIterator mask = maskBegin;
    uInt npts = ary.size();
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    while (count < nr) {
        if (
            *mask
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    uInt npts = ary.size();
    while (count < nr) {
        if (*weight > 0) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightsBegin, uInt64 nr, uInt dataStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightsBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    uInt npts = ary.size();
    while (count < nr) {
        if (
            *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, dataStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin,
    const WeightsIterator& weightBegin, uInt64 nr, uInt dataStride,
    const MaskIterator& maskBegin, uInt maskStride, uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    uInt npts = ary.size();
    while (count < nr) {
        if (*mask && *weight > 0) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_populateTestArray(
    std::vector<AccumType>& ary, const DataIterator& dataBegin, const WeightsIterator& weightBegin,
    uInt64 nr, uInt dataStride, const MaskIterator& maskBegin, uInt maskStride,
    const DataRanges& ranges, Bool isInclude, uInt maxElements
) const {
    DataIterator datum = dataBegin;
    WeightsIterator weight = weightBegin;
    MaskIterator mask = maskBegin;
    uInt64 count = 0;
    typename DataRanges::const_iterator beginRange = ranges.begin();
    typename DataRanges::const_iterator endRange = ranges.end();
    uInt npts = ary.size();
    while (count < nr) {
        if (
            *mask && *weight > 0
            && StatisticsUtilities<AccumType>::includeDatum(
                *datum, beginRange, endRange, isInclude
            )
        ) {
            _PopulateTestArrayCode
        }
        StatisticsIncrementer<DataIterator, MaskIterator, WeightsIterator>::increment(
            datum, count, weight, mask, dataStride, maskStride
        );
    }
    return False;
}

CASA_STATD
Bool ClassicalQuantileComputer<CASA_STATP>::_valuesFromSortedArray(
    std::map<uInt64, AccumType>& values, uInt64 mynpts,
    const std::set<uInt64>& indices, uInt64 maxArraySize, Bool persistSortedArray
) {
    values.clear();
    // I need a little wiggle room, the caller can't make the maximum array size
    // ridiculously small
    maxArraySize = max(maxArraySize, (uInt64)1000);
    std::vector<AccumType> myArray;
    if (_doMedAbsDevMed && ! this->_getSortedArray().empty()) {
        // make a copy
        std::vector<AccumType> pSorted = this->_getSortedArray();
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
            // npts is smaller than the max array size, so create the array and sort
            // it in memory
            _createDataArray(myArray);
        }
        else {
            // data is too large to be sorted in memory
            return False;
        }
    }
    values = StatisticsUtilities<AccumType>::indicesToValues(
        myArray, indices
    );
    if (! _doMedAbsDevMed) {
        this->_setSortedArray(persistSortedArray ? myArray : std::vector<AccumType>());
    }
    return True;
}

}

#endif
