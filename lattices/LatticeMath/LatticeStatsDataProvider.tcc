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
//# $Id: Array.h 21545 2015-01-22 19:36:35Z gervandiepen $

#ifndef LATTICES_LATTICESTATSDATAPROVIDER_TCC
#define LATTICES_LATTICESTATSDATAPROVIDER_TCC

#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.h>

namespace casacore {

template <class T>
LatticeStatsDataProvider<T>::LatticeStatsDataProvider()
	: LatticeStatsDataProviderBase<T>(),
	_iter(), _currentSlice(),
	_currentPtr(0), _delData(False), _atEnd(False) {}

template <class T>
LatticeStatsDataProvider<T>::LatticeStatsDataProvider(
	const Lattice<T>& lattice, uInt iteratorLimitBytes
) : LatticeStatsDataProviderBase<T>(),
	_iter(), _currentSlice(),
	_currentPtr(0), _delData(False), _atEnd(False) {
	setLattice(lattice, iteratorLimitBytes);
}

template <class T>
LatticeStatsDataProvider<T>::~LatticeStatsDataProvider() {}

template <class T>
void LatticeStatsDataProvider<T>::operator++() {
	_freeStorage();
	if (_iter.null()) {
		_atEnd = True;
	}
	else {
		++(*_iter);
	}
	this->_updateProgress();
}

template <class T>
uInt LatticeStatsDataProvider<T>::estimatedSteps() const {
	if (_iter.null()) {
		return 1;
	}
	IPosition lattShape = _iter->latticeShape();
	IPosition cursShape = _iter->cursor().shape();
	uInt ndim = lattShape.size();
	uInt count = 1;
	for (uInt i=0; i<ndim; i++) {
		uInt nsteps = lattShape[i]/cursShape[i];
		if (lattShape[i] % cursShape[i] != 0) {
			++nsteps;
		}
		count *= nsteps;
	}
	return count;
}

template <class T>
Bool LatticeStatsDataProvider<T>::atEnd() const {
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
uInt64 LatticeStatsDataProvider<T>::getCount() {
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
const Bool* LatticeStatsDataProvider<T>::getMask() {
	return NULL;
}

template <class T>
Bool LatticeStatsDataProvider<T>::hasMask() const {
	return False;
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
	const Lattice<T>& lattice, uInt iteratorLimitBytes
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
		_atEnd = False;
	}
}

template <class T>
void LatticeStatsDataProvider<T>::updateMaxPos(
	const std::pair<Int64, Int64>& maxpos
) {
	IPosition p = toIPositionInArray(maxpos.second, _currentSlice.shape());
	if (! _iter.null()) {
		p += _iter->position();
	}
	this->_updateMaxPos(p);
}

template <class T>
void LatticeStatsDataProvider<T>::updateMinPos(
	const std::pair<Int64, Int64>& minpos
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
	_delData = False;
}
}

#endif
