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

namespace casacore {

CASA_STATD StatisticsDataset<CASA_STATP>::StatisticsDataset()
    : _data(), _weights(), _masks(), _counts(), _dataStrides(), _maskStrides(),
      _isIncludeRanges(), _dataRanges(), _dataProvider(NULL)
{}

CASA_STATD
StatisticsDataset<CASA_STATP>::StatisticsDataset(const StatisticsDataset& other)
    : _data(other._data), _weights(other._weights), _masks(other._masks),
      _counts(other._counts), _dataStrides(other._dataStrides),
      _maskStrides(other._maskStrides),
      _isIncludeRanges(other._isIncludeRanges), _dataRanges(other._dataRanges),
      // WARN reference semantics
      _dataProvider(other._dataProvider) {}

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
     return *this;
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, uInt nr, uInt dataStride, Bool nrAccountsForStride
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
    const DataIterator& first, uInt nr,
    const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
    Bool nrAccountsForStride
) {
    _throwIfDataProviderDefined();
    typename DataRanges::const_iterator riter = dataRanges.begin();
    typename DataRanges::const_iterator rend = dataRanges.end();
    while (riter != rend) {
        ThrowIf(
            (*riter).first > (*riter).second,
            "The first value in a range pair cannot be greater than the second"
        );
        ++riter;
    }
    uInt n = _data.size();
    _isIncludeRanges[n] = isInclude;
    _dataRanges[n] = dataRanges;
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const MaskIterator& maskFirst,
    uInt nr, uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
    _throwIfDataProviderDefined();
    uInt key = _data.size();
    _maskStrides[key] = maskStride;
    _masks[key] = maskFirst;
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::addData(
    const DataIterator& first, const MaskIterator& maskFirst,
    uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride,
    uInt maskStride
) {
    _throwIfDataProviderDefined();
    uInt key = _data.size();
    _maskStrides[key] = maskStride;
    _masks[key] = maskFirst;
    addData(
        first, nr, dataRanges, isInclude,
        dataStride, nrAccountsForStride
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
    const DataIterator& first, const WeightsIterator& weightFirst,
    uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride
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
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride,
    uInt maskStride
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

CASA_STATD void StatisticsDataset<CASA_STATP>::reset() {
    _data.clear();
    _counts.clear();
    _masks.clear();
    _weights.clear();
    _dataRanges.clear();
    _dataStrides.clear();
    _maskStrides.clear();
    _dataProvider = NULL;
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, uInt nr, uInt dataStride, Bool nrAccountsForStride
) {
    reset();
    addData(first, nr, dataStride, nrAccountsForStride);
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, uInt nr,
    const DataRanges& dataRanges, Bool isInclude, uInt dataStride,
    Bool nrAccountsForStride
) {
    reset();
    addData(
        first, nr, dataRanges, isInclude, dataStride, nrAccountsForStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const MaskIterator& maskFirst,
    uInt nr, uInt dataStride, Bool nrAccountsForStride, uInt maskStride
) {
    reset();
    addData(
        first, maskFirst, nr, dataStride, nrAccountsForStride, maskStride
    );
}

CASA_STATD void StatisticsDataset<CASA_STATP>::setData(
    const DataIterator& first, const MaskIterator& maskFirst,
    uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride,
    uInt maskStride
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
    const DataIterator& first, const WeightsIterator& weightFirst,
    uInt nr, const DataRanges& dataRanges,
    Bool isInclude, uInt dataStride, Bool nrAccountsForStride
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
    ThrowIf(! dataProvider, "Logic Error: data provider cannot be NULL");
    reset();
    _dataProvider = dataProvider;
}

CASA_STATD void StatisticsDataset<CASA_STATP>::_throwIfDataProviderDefined() const {
    ThrowIf(
        _dataProvider,
        "Logic Error: Cannot add data after a data provider has been set. Call setData() to clear "
        "the existing data provider and to add this new data set"
    );
}

}

#endif
