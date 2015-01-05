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

#include <casacore/lattices/LatticeMath/MaskedLatticeStatsDataProvider.h>

namespace casacore {

template <class AccumType, class T, class InputIterator>
MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::MaskedLatticeStatsDataProvider(
	MaskedLattice<T>& lattice
) : LatticeStatsDataProviderBase<AccumType, T, InputIterator>(),
	_iter(RO_MaskedLatticeIterator<T>(lattice)), _currentSlice(), _currentMaskSlice(),
	_currentPtr(0), _currentMaskPtr(0), _delData(False), _delMask(False) {}

template <class AccumType, class T, class InputIterator>
MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::~MaskedLatticeStatsDataProvider() {}

template <class AccumType, class T, class InputIterator>
void MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::operator++() {
	_freeStorage();
	++_iter;
	this->_updateProgress(_iter.nsteps());
}

template <class AccumType, class T, class InputIterator>
Bool MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::atEnd() const {
	return _iter.atEnd();
}

template <class AccumType, class T, class InputIterator>
void MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::finalize() {
	LatticeStatsDataProviderBase<AccumType, T, InputIterator>::finalize();
	_freeStorage();
}

template <class AccumType, class T, class InputIterator>
uInt64 MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::getCount() {
	return _iter.cursor().size();
}

template <class AccumType, class T, class InputIterator>
InputIterator MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::getData() {
	_currentSlice.assign(_iter.cursor());
	_currentPtr = _currentSlice.getStorage(_delData);
	return _currentPtr;
}

template <class AccumType, class T, class InputIterator>
const Bool* MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::getMask() {
	_currentMaskSlice.assign(_iter.getMask());
	_currentMaskPtr = _currentMaskSlice.getStorage(_delMask);
	return _currentMaskPtr;
}

template <class AccumType, class T, class InputIterator>
Bool MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::hasMask() const {
	return True;
}

template <class AccumType, class T, class InputIterator>
void MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::reset() {
	_iter.reset();
}

template <class AccumType, class T, class InputIterator>
void MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::updateMaxPos(
	const std::pair<uInt, Int64>& maxpos
) {
	this->_updateMaxPos(
		_iter.position() + toIPositionInArray(maxpos.second, _currentSlice.shape())
	);
}

template <class AccumType, class T, class InputIterator>
void MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::updateMinPos(
	const std::pair<uInt, Int64>& minpos
) {
	this->_updateMinPos(
		_iter.position() + toIPositionInArray(minpos.second, _currentSlice.shape())
	);
}

template <class AccumType, class T, class InputIterator>
void MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::_freeStorage() {
	_currentSlice.freeStorage (_currentPtr, _delData);
	_delData = False;
	_currentMaskSlice.freeStorage(_currentMaskPtr, _delMask);
	_delMask = False;
}

template <class AccumType, class T, class InputIterator>
uInt MaskedLatticeStatsDataProvider<AccumType, T, InputIterator>::_nsteps() const {
	const IPosition trc = _iter.latticeShape() - 1;
	uInt ndim = trc.size();
	const IPosition blc(ndim, 0);
	const IPosition tileShape = _iter.lattice().niceCursorShape();
	uInt nsteps = 1;
	for (uInt j=0; j<ndim; j++) {
		nsteps *= 1 + trc(j)/tileShape(j) - blc(j)/tileShape(j);
	}
	return nsteps;
}

}

