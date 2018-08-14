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
    const DataIterator& first, uInt nr, uInt dataStride,
    Bool nrAccountsForStride
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
    const DataIterator& first, uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride
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
    const DataIterator& first, const MaskIterator& maskFirst, uInt nr,
    uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
    _throwIfDataProviderDefined();
    uInt key = _data.size();
    _maskStrides[key] = maskStride;
    _masks[key] = maskFirst;
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const MaskIterator& maskFirst, uInt nr,
    const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
    Bool nrAccountsForStride, uInt maskStride
) {
    _throwIfDataProviderDefined();
    uInt key = _data.size();
    _maskStrides[key] = maskStride;
    _masks[key] = maskFirst;
    addData(
        first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    uInt nr, uInt dataStride, Bool nrAccountsForStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst, uInt nr,
    const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
    Bool nrAccountsForStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(
        first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uInt nr, uInt dataStride,
    Bool nrAccountsForStride, uInt maskStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(
        first, maskFirst, nr, dataStride, nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
    _throwIfDataProviderDefined();
    _weights[_data.size()] = weightFirst;
    addData(
        first, maskFirst, nr, dataRanges, isInclude, dataStride,
        nrAccountsForStride, maskStride
    );
}

CASA_STATD Bool StatisticsDataset<CASA_STATP>::empty() const {
    return ! _dataProvider && _data.empty();
}

CASA_STATD
Bool StatisticsDataset<CASA_STATP>::increment(Bool includeIDataset) {
    if (includeIDataset) {
        ++_idataset;
    }
    if (_dataProvider) {
        ++(*_dataProvider);
        if (_dataProvider->atEnd()) {
            _dataProvider->finalize();
            return True;
        }
    }
    else {
        ++_diter;
        if (_diter == _dend) {
            return True;
        }
        ++_citer;
        ++_dsiter;
        ++_dataCount;
    }
    return False;
}

CASA_STATD
void StatisticsDataset<CASA_STATP>::incrementThreadIters(
    DataIterator& dataIter, MaskIterator& maskIter,
    WeightsIterator& weightsIter, uInt64& offset, uInt nthreads
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
            ? new std::pair<DataRanges, Bool>(
                _dataProvider->getRanges(), _dataProvider->isInclude()
            ) : nullptr
        );
        _chunk.mask.set(
            _dataProvider->hasMask()
            ? new std::pair<MaskIterator, uInt>(
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
            : new std::pair<DataRanges, Bool>(
                rangeI->second, _isIncludeRanges.find(_dataCount)->second
            )
        );
        auto maskI = _masks.find(_dataCount);
        _chunk.mask.set(
            maskI == _masks.end() ? nullptr
            : new std::pair<MaskIterator, uInt>(
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
    uInt& nBlocks, uInt64& extra, uInt& nthreads,
    PtrHolder<DataIterator>& dataIter,  PtrHolder<MaskIterator>& maskIter,
    PtrHolder<WeightsIterator>& weightsIter, PtrHolder<uInt64>& offset,
    uInt nThreadsMax
) const {
    auto n = ClassicalStatisticsData::CACHE_PADDING*nThreadsMax;
    dataIter.set(new DataIterator[n], True);
    maskIter.set(new MaskIterator[n], True);
    weightsIter.set(new WeightsIterator[n], True);
    offset.set(new uInt64[n], True);
    nBlocks = _chunk.count/ClassicalStatisticsData::BLOCK_SIZE;
    extra = _chunk.count % ClassicalStatisticsData::BLOCK_SIZE;
    if (extra > 0) {
        ++nBlocks;
    }
    nthreads = min(nThreadsMax, nBlocks);
    for (uInt tid=0; tid<nthreads; ++tid) {
        // advance the per-thread iterators to their correct starting
        // locations
        uInt idx8 = ClassicalStatisticsData::CACHE_PADDING*tid;
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
    const DataIterator& first, uInt nr, uInt dataStride,
    Bool nrAccountsForStride
) {
    reset();
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride
) {
    reset();
    addData(
        first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const MaskIterator& maskFirst, uInt nr,
    uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
    reset();
    addData(
        first, maskFirst, nr, dataStride, nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const MaskIterator& maskFirst, uInt nr,
    const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
    Bool nrAccountsForStride, uInt maskStride
) {
    reset();
    addData(
        first, maskFirst, nr, dataRanges, isInclude, dataStride,
        nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    uInt nr, uInt dataStride, Bool nrAccountsForStride
) {
    reset();
    addData(first, weightFirst, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst, uInt nr,
    const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
    Bool nrAccountsForStride
) {
    reset();
    addData(
        first, weightFirst, nr, dataRanges, isInclude,
        dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uInt nr, uInt dataStride,
    Bool nrAccountsForStride, uInt maskStride
) {
    reset();
    addData(
        first, weightFirst, maskFirst, nr, dataStride,
        nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const WeightsIterator& weightFirst,
    const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride, uInt maskStride
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
