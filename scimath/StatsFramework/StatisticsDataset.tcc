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

#ifndef SCIMATH_STATISTICSDATASET_TCC
#define SCIMATH_STATISTICSDATASET_TCC

#include <casacore/scimath/StatsFramework/StatisticsDataset.h>

#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/scimath/StatsFramework/ClassicalStatisticsData.h>

namespace casacore {

CASA_STATD StatisticsDataset<CASA_STATP>::StatisticsDataset() {}

CASA_STATD
StatisticsDataset<CASA_STATP>::StatisticsDataset(const StatisticsDataset& other)
    : _data(other._data), _weights(other._weights), _masks(other._masks),
      _counts(other._counts), _dataStrides(other._dataStrides),
      _maskStrides(other._maskStrides),
      _isIncludeRanges(other._isIncludeRanges), _dataRanges(other._dataRanges),
      // WARN reference semantics
      _dataProvider(other._dataProvider), _idataset(0), _dataCount(0), _chunk() {}

CASA_STATD StatisticsDataset<CASA_STATP>::~StatisticsDataset() {}

CASA_STATD StatisticsDataset<CASA_STATP>&
StatisticsDataset<CASA_STATP>::operator=(
    const StatisticsDataset<CASA_STATP>& other
) {
     if (this == &other) {
         return *this;
     }
     _data = other._data;
     _weights = other._weights;
     _masks = other._masks;
     _counts = other._counts;
     _dataStrides = other._dataStrides;
     _maskStrides = other._maskStrides;
     _isIncludeRanges = other._isIncludeRanges;
     _dataRanges = other._dataRanges;
     // WARN reference semantics
     _dataProvider = other._dataProvider;
     _idataset = other._idataset;
     return *this;
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, uint32_t nr, uint32_t dataStride,
    bool nrAccountsForStride
) {
    _throwIfDataProviderDefined();
    _data.push_back(first);
    // internally we store the number of strided points
    _counts.push_back(
        nrAccountsForStride ? nr
            : nr % dataStride == 0
              ? nr/dataStride
                : nr/dataStride + 1
    );
    _dataStrides.push_back(dataStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, uint32_t nr, const DataRanges& dataRanges,
    bool isInclude, uint32_t dataStride, bool nrAccountsForStride
) {
    _throwIfDataProviderDefined();
    for_each(
        dataRanges.cbegin(), dataRanges.cend(),
        [](const std::pair<AccumType, AccumType>& range) {
        ThrowIf(
            range.first > range.second,
            "The first value in a range pair cannot be greater than the second"
        );
    });
    auto n = _data.size();
    _isIncludeRanges[n] = isInclude;
    _dataRanges[n] = dataRanges;
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const MaskIterator& maskFirst, uint32_t nr,
    uint32_t dataStride, bool nrAccountsForStride, uint32_t maskStride
) {
    _throwIfDataProviderDefined();
    uint32_t key = _data.size();
    _maskStrides[key] = maskStride;
    _masks[key] = maskFirst;
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const MaskIterator& maskFirst, uint32_t nr,
    const DataRanges& dataRanges, bool isInclude, uint32_t dataStride,
    bool nrAccountsForStride, uint32_t maskStride
) {
    _throwIfDataProviderDefined();
    uint32_t key = _data.size();
    _maskStrides[key] = maskStride;
    _masks[key] = maskFirst;
    addData(
        first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    uint32_t nr, uint32_t dataStride, bool nrAccountsForStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst, uint32_t nr,
    const DataRanges& dataRanges, bool isInclude, uint32_t dataStride,
    bool nrAccountsForStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(
        first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uint32_t nr, uint32_t dataStride,
    bool nrAccountsForStride, uint32_t maskStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(
        first, maskFirst, nr, dataStride, nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uint32_t nr, const DataRanges& dataRanges,
    bool isInclude, uint32_t dataStride, bool nrAccountsForStride, uint32_t maskStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(
        first, maskFirst, nr, dataRanges, isInclude, dataStride,
        nrAccountsForStride, maskStride
    );
}

CASA_STATD bool StatisticsDataset<CASA_STATP>::empty() const {
    return ! _dataProvider && _data.empty();
}

CASA_STATD
bool StatisticsDataset<CASA_STATP>::increment(bool includeIDataset) {
    if (includeIDataset) {
        ++_idataset;
    }
    if (_dataProvider) {
        ++(*_dataProvider);
        if (_dataProvider->atEnd()) {
            _dataProvider->finalize();
            return true;
        }
    }
    else {
        ++_diter;
        if (_diter == _dend) {
            return true;
        }
        ++_citer;
        ++_dsiter;
        ++_dataCount;
    }
    return false;
}

CASA_STATD
void StatisticsDataset<CASA_STATP>::incrementThreadIters(
    DataIterator& dataIter, MaskIterator& maskIter,
    WeightsIterator& weightsIter, uint64_t& offset, uint32_t nthreads
) const {
    auto increment = nthreads * ClassicalStatisticsData::BLOCK_SIZE
        * _chunk.dataStride;
    if (offset+increment >= _chunk.count*_chunk.dataStride) {
        // necessary because in some cases std::advance will segfault
        // if advanced past the end of the data structure
        return;
    }
    std::advance(dataIter, increment);
    if (_chunk.weights) {
        std::advance(weightsIter, increment);
    }
    if (_chunk.mask) {
        std::advance(
            maskIter,
            nthreads*ClassicalStatisticsData::BLOCK_SIZE*_chunk.mask->second
        );
    }
    offset += increment;
}

CASA_STATD void StatisticsDataset<CASA_STATP>::initIterators() {
    ThrowIf(empty(), "No data sets have been added");
    if (_dataProvider) {
        _dataProvider->reset();
    }
    else {
        _dataCount = 0;
        _diter = _data.begin();
        _dend = _data.end();
        _dsiter = _dataStrides.begin();
        _citer = _counts.begin();
    }
    _chunk.ranges.clear();
    _chunk.mask.clear();
    _chunk.weights.clear();
}

CASA_STATD
const typename StatisticsDataset<CASA_STATP>::ChunkData&
StatisticsDataset<CASA_STATP>::initLoopVars() {
    if (_dataProvider) {
        _chunk.data = _dataProvider->getData();
        _chunk.count = _dataProvider->getCount();
        _chunk.dataStride = _dataProvider->getStride();
        _chunk.ranges.set(
            _dataProvider->hasRanges()
            ? new std::pair<DataRanges, bool>(
                _dataProvider->getRanges(), _dataProvider->isInclude()
            ) : nullptr
        );
        _chunk.mask.set(
            _dataProvider->hasMask()
            ? new std::pair<MaskIterator, uint32_t>(
                _dataProvider->getMask(), _dataProvider->getMaskStride()
            )
            : nullptr
        );
        _chunk.weights.set(
            _dataProvider->hasWeights()
            ? new WeightsIterator(_dataProvider->getWeights()) : nullptr
        );
    }
    else {
        _chunk.data = *_diter;
        _chunk.count = *_citer;
        _chunk.dataStride = *_dsiter;
        auto rangeI = _dataRanges.find(_dataCount);
        _chunk.ranges.set(
            rangeI == _dataRanges.end() ? nullptr
            : new std::pair<DataRanges, bool>(
                rangeI->second, _isIncludeRanges.find(_dataCount)->second
            )
        );
        auto maskI = _masks.find(_dataCount);
        _chunk.mask.set(
            maskI == _masks.end() ? nullptr
            : new std::pair<MaskIterator, uint32_t>(
                maskI->second, _maskStrides.find(_dataCount)->second
            )
        );
        _chunk.weights.set(
            _weights.find(_dataCount) == _weights.end()
            ? nullptr : new WeightsIterator(_weights.find(_dataCount)->second)
        );

    }
    return _chunk;
}

CASA_STATD
void StatisticsDataset<CASA_STATP>::initThreadVars(
    uint32_t& nBlocks, uint64_t& extra, uint32_t& nthreads,
    std::unique_ptr<DataIterator[]>& dataIter,  std::unique_ptr<MaskIterator[]>& maskIter,
    std::unique_ptr<WeightsIterator[]>& weightsIter, std::unique_ptr<uint64_t[]>& offset,
    uint32_t nThreadsMax
) const {
    ThrowIf(nThreadsMax == 0, "Logic error: nThreadsMax should never be 0");
    auto n = ClassicalStatisticsData::CACHE_PADDING*nThreadsMax;
    dataIter.reset(new DataIterator[n]);
    maskIter.reset(new MaskIterator[n]);
    weightsIter.reset(new WeightsIterator[n]);
    offset.reset(new uint64_t[n]);
    nBlocks = _chunk.count/ClassicalStatisticsData::BLOCK_SIZE;
    extra = _chunk.count % ClassicalStatisticsData::BLOCK_SIZE;
    if (extra > 0) {
        ++nBlocks;
    }
    ThrowIf(nBlocks == 0, "Logic error: nBlocks should never be 0");
    nthreads = std::min(nThreadsMax, nBlocks);
    ThrowIf(nthreads == 0, "Logic error: nthreads should never be 0");
    for (uint32_t tid=0; tid<nthreads; ++tid) {
        // advance the per-thread iterators to their correct starting
        // locations
        uint32_t idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
        dataIter[idx8] = _chunk.data;
        offset[idx8] = tid * ClassicalStatisticsData::BLOCK_SIZE
            * _chunk.dataStride;
        std::advance(dataIter[idx8], offset[idx8]);
        if (_chunk.weights) {
            weightsIter[idx8] = *_chunk.weights;
        }
        if (_chunk.mask) {
            maskIter[idx8] = _chunk.mask->first;
            std::advance(
                maskIter[idx8],
                tid*ClassicalStatisticsData::BLOCK_SIZE*_chunk.mask->second
            );
        }
    }
}

CASA_STATD void StatisticsDataset<CASA_STATP>::reset() {
    _data.clear();
    _counts.clear();
    _masks.clear();
    _weights.clear();
    _dataRanges.clear();
    _dataStrides.clear();
    _maskStrides.clear();
    _dataProvider = nullptr;
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, uint32_t nr, uint32_t dataStride,
    bool nrAccountsForStride
) {
    reset();
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, uint32_t nr, const DataRanges& dataRanges,
    bool isInclude, uint32_t dataStride, bool nrAccountsForStride
) {
    reset();
    addData(
        first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const MaskIterator& maskFirst, uint32_t nr,
    uint32_t dataStride, bool nrAccountsForStride, uint32_t maskStride
) {
    reset();
    addData(
        first, maskFirst, nr, dataStride, nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const MaskIterator& maskFirst, uint32_t nr,
    const DataRanges& dataRanges, bool isInclude, uint32_t dataStride,
    bool nrAccountsForStride, uint32_t maskStride
) {
    reset();
    addData(
        first, maskFirst, nr, dataRanges, isInclude, dataStride,
        nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    uint32_t nr, uint32_t dataStride, bool nrAccountsForStride
) {
    reset();
    addData(first, weightFirst, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst, uint32_t nr,
    const DataRanges& dataRanges, bool isInclude, uint32_t dataStride,
    bool nrAccountsForStride
) {
    reset();
    addData(
        first, weightFirst, nr, dataRanges, isInclude,
        dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uint32_t nr, uint32_t dataStride,
    bool nrAccountsForStride, uint32_t maskStride
) {
    reset();
    addData(
        first, weightFirst, maskFirst, nr, dataStride,
        nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uint32_t nr, const DataRanges& dataRanges,
    bool isInclude, uint32_t dataStride, bool nrAccountsForStride, uint32_t maskStride
) {
    reset();
    addData(
        first, weightFirst, maskFirst, nr, dataRanges, isInclude,
        dataStride, nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setDataProvider(
    StatsDataProvider<CASA_STATP> *dataProvider
) {
    ThrowIf(! dataProvider, "Logic Error: data provider cannot be nullptr");
    reset();
    _dataProvider = dataProvider;
}

CASA_STATD
void StatisticsDataset<CASA_STATP>::_throwIfDataProviderDefined() const {
    ThrowIf(
        _dataProvider,
        "Logic Error: Cannot add data after a data provider has been set. Call "
        "setData() to clear the existing data provider and to add this new "
        "data set"
    );
}

}

#endif
