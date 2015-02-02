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

#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.h>

namespace casacore {

template <class T>
LatticeStatsDataProvider<T>::LatticeStatsDataProvider(
	Lattice<T>& lattice
) : LatticeStatsDataProviderBase<T>(),
	_iter(RO_LatticeIterator<T>(lattice)), _currentSlice(),
	_currentPtr(0), _delData(False) {}

template <class T>
LatticeStatsDataProvider<T>::~LatticeStatsDataProvider() {}

template <class T>
void LatticeStatsDataProvider<T>::operator++() {
	_freeStorage();
	++_iter;
	this->_updateProgress();
}

template <class T>
uInt LatticeStatsDataProvider<T>::estimatedSteps() const {
	IPosition lattShape = _iter.latticeShape();
	IPosition cursShape = _iter.cursor().shape();
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
	return _iter.atEnd();
}

template <class T>
void LatticeStatsDataProvider<T>::finalize() {
	_freeStorage();
	LatticeStatsDataProviderBase<T>::finalize();
}

template <class T>
uInt64 LatticeStatsDataProvider<T>::getCount() {
	return _iter.cursor().size();
}

template <class T>
const T* LatticeStatsDataProvider<T>::getData() {
	_currentSlice.assign(_iter.cursor());
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
	_iter.reset();
}

template <class T>
void LatticeStatsDataProvider<T>::updateMaxPos(
	const std::pair<Int64, Int64>& maxpos
) {
	this->_updateMaxPos(
		_iter.position() + toIPositionInArray(maxpos.second, _currentSlice.shape())
	);
}

template <class T>
void LatticeStatsDataProvider<T>::updateMinPos(
	const std::pair<Int64, Int64>& minpos
) {
	this->_updateMinPos(
		_iter.position() + toIPositionInArray(minpos.second, _currentSlice.shape())
	);
}

template <class T>
void LatticeStatsDataProvider<T>::_freeStorage() {
	_currentSlice.freeStorage (_currentPtr, _delData);
	_delData = False;
}

}

