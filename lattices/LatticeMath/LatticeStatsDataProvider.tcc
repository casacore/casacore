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

#ifndef LATTICES_LATTICESTATSDATAPROVIDER_TCC
#define LATTICES_LATTICESTATSDATAPROVIDER_TCC

#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.h>
#include <casacore/scimath/StatsFramework/ClassicalStatistics.h>

namespace casacore {

template <class T>
LatticeStatsDataProvider<T>::LatticeStatsDataProvider()
    : LatticeStatsDataProviderBase<T>(),
    _iter(), _currentSlice(),
    _currentPtr(0), _delData(false), _atEnd(false), _nMaxThreads(0) {}

template <class T>
LatticeStatsDataProvider<T>::LatticeStatsDataProvider(
    const Lattice<T>& lattice, uint32_t iteratorLimitBytes
) : LatticeStatsDataProviderBase<T>(),
    _iter(), _currentSlice(),
    _currentPtr(0), _delData(false), _atEnd(false) {
    setLattice(lattice, iteratorLimitBytes);
}

template <class T>
LatticeStatsDataProvider<T>::~LatticeStatsDataProvider() {}

template <class T>
void LatticeStatsDataProvider<T>::operator++() {
    _freeStorage();
    if (_iter.null()) {
        _atEnd = true;
    }
    else {
        ++(*_iter);
    }
    this->_updateProgress();
}

template <class T>
uint32_t LatticeStatsDataProvider<T>::estimatedSteps() const {
    if (_iter.null()) {
        return 1;
    }
    IPosition lattShape = _iter->latticeShape();
    IPosition cursShape = _iter->cursor().shape();
    uint32_t ndim = lattShape.size();
    uint32_t count = 1;
    for (uint32_t i=0; i<ndim; i++) {
        uint32_t nsteps = lattShape[i]/cursShape[i];
        if (lattShape[i] % cursShape[i] != 0) {
            ++nsteps;
        }
        count *= nsteps;
    }
    return count;
}

template <class T>
bool LatticeStatsDataProvider<T>::atEnd() const {
    if (_iter.null()) {
        return _atEnd;
    }
    return _iter->atEnd();
}

template <class T>
void LatticeStatsDataProvider<T>::finalize() {
    _freeStorage();
    LatticeStatsDataProviderBase<T>::finalize();
}

template <class T>
uint64_t LatticeStatsDataProvider<T>::getCount() {
    if (_iter.null()) {
        return _currentSlice.size();
    }
    return _iter->cursor().size();
}

template <class T>
const T* LatticeStatsDataProvider<T>::getData() {
    if (! _iter.null()) {
        _currentSlice.assign(_iter->cursor());
    }
    _currentPtr = _currentSlice.getStorage(_delData);
    return _currentPtr;
}

template <class T>
const bool* LatticeStatsDataProvider<T>::getMask() {
    return NULL;
}

template <class T>
uint32_t LatticeStatsDataProvider<T>::getNMaxThreads() const {
#ifdef _OPENMP
    return _nMaxThreads;
#else
    return 0;
#endif
}

template <class T>
bool LatticeStatsDataProvider<T>::hasMask() const {
    return false;
}

template <class T>
void LatticeStatsDataProvider<T>::reset() {
    LatticeStatsDataProviderBase<T>::reset();
    if (! _iter.null()) {
        _iter->reset();
    }
}

template <class T>
void LatticeStatsDataProvider<T>::setLattice(
    const Lattice<T>& lattice, uint32_t iteratorLimitBytes
) {
    finalize();
    if (lattice.size() > iteratorLimitBytes/sizeof(T)) {
        TileStepper stepper(
            lattice.shape(), lattice.niceCursorShape(
                lattice.advisedMaxPixels()
            )
        );
        _iter = new RO_LatticeIterator<T>(lattice, stepper);
    }
    else {
        _iter = NULL;
        _currentSlice.assign(lattice.get());
        _atEnd = false;
    }
#ifdef _OPENMP
    _nMaxThreads = min(
        omp_get_max_threads(),
        (int32_t)ceil((float)lattice.size()/ClassicalStatisticsData::BLOCK_SIZE)
    );
#endif
}

template <class T>
void LatticeStatsDataProvider<T>::updateMaxPos(
    const std::pair<int64_t, int64_t>& maxpos
) {
    IPosition p = toIPositionInArray(maxpos.second, _currentSlice.shape());
    if (! _iter.null()) {
        p += _iter->position();
    }
    this->_updateMaxPos(p);
}

template <class T>
void LatticeStatsDataProvider<T>::updateMinPos(
    const std::pair<int64_t, int64_t>& minpos
) {
    IPosition p = toIPositionInArray(minpos.second, _currentSlice.shape());
    if (! _iter.null()) {
        p += _iter->position();
    }
    this->_updateMinPos(p);
}

template <class T>
void LatticeStatsDataProvider<T>::_freeStorage() {
    _currentSlice.freeStorage (_currentPtr, _delData);
    _delData = false;
}
}

#endif
