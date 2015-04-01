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

#ifndef LATTICES_MASKEDLATTICESTATSDATAPROVIDER_TCC
#define LATTICES_MASKEDLATTICESTATSDATAPROVIDER_TCC

#include <casacore/lattices/LatticeMath/MaskedLatticeStatsDataProvider.h>

namespace casacore {

template <class T>
MaskedLatticeStatsDataProvider<T>::MaskedLatticeStatsDataProvider()
	: LatticeStatsDataProviderBase<T>(),
	_iter(), /* _ary(), _mask(), */ _currentSlice(), _currentMaskSlice(),
	_currentPtr(0), _currentMaskPtr(0), _delData(False), _delMask(False), _atEnd(False) {}

template <class T>
MaskedLatticeStatsDataProvider<T>::MaskedLatticeStatsDataProvider(
	MaskedLattice<T>& lattice, uInt
) : LatticeStatsDataProviderBase<T>(),
	_iter(), _currentSlice(), _currentMaskSlice(),
	_currentPtr(0), _currentMaskPtr(0), _delData(False), _delMask(False) {
	setLattice(lattice);
}

template <class T>
MaskedLatticeStatsDataProvider<T>::~MaskedLatticeStatsDataProvider() {}

template <class T>
void MaskedLatticeStatsDataProvider<T>::operator++() {
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
uInt MaskedLatticeStatsDataProvider<T>::estimatedSteps() const {
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
Bool MaskedLatticeStatsDataProvider<T>::atEnd() const {
	if (_iter.null()) {
		return _atEnd;

	}
	else {
		return _iter->atEnd();
	}
}

template <class T>
void MaskedLatticeStatsDataProvider<T>::finalize() {
	LatticeStatsDataProviderBase<T>::finalize();
	_freeStorage();
}

template <class T>
uInt64 MaskedLatticeStatsDataProvider<T>::getCount() {
	if (_iter.null()) {
		return _currentSlice.size();
	}
	else {
		return _iter->cursor().size();
	}
}

template <class T>
const T* MaskedLatticeStatsDataProvider<T>::getData() {
	if (! _iter.null()) {
		_currentSlice.assign(_iter->cursor());
	}
	_currentPtr = _currentSlice.getStorage(_delData);
	return _currentPtr;
}

template <class T>
const Bool* MaskedLatticeStatsDataProvider<T>::getMask() {
	if (! _iter.null()) {
		_currentMaskSlice.assign(_iter->getMask());
	}
	_currentMaskPtr = _currentMaskSlice.getStorage(_delMask);
	return _currentMaskPtr;
}

template <class T>
Bool MaskedLatticeStatsDataProvider<T>::hasMask() const {
	return True;
}

template <class T>
void MaskedLatticeStatsDataProvider<T>::reset() {
	LatticeStatsDataProviderBase<T>::reset();
	if (! _iter.null()) {
		_iter->reset();
	}
}

template <class T>
void MaskedLatticeStatsDataProvider<T>::setLattice(
	const MaskedLattice<T>& lattice, uInt iteratorLimitBytes
) {
	finalize();
	if (lattice.size() > iteratorLimitBytes/sizeof(T)) {
		TileStepper stepper(
			lattice.shape(), lattice.niceCursorShape(
				lattice.advisedMaxPixels()
			)
		);
		_iter = new RO_MaskedLatticeIterator<T>(lattice, stepper);
	}
	else {
		_iter = NULL;
		_currentSlice.assign(lattice.get());
		_currentMaskSlice.assign(lattice.getMask());
		_atEnd = False;
	}
}


template <class T>
void MaskedLatticeStatsDataProvider<T>::updateMaxPos(
	const std::pair<Int64, Int64>& maxpos
) {
	IPosition p = toIPositionInArray(maxpos.second, _currentSlice.shape());
	if (! _iter.null()) {
		p += _iter->position();
	}
	this->_updateMaxPos(p);
}

template <class T>
void MaskedLatticeStatsDataProvider<T>::updateMinPos(
	const std::pair<Int64, Int64>& minpos
) {
	IPosition p = toIPositionInArray(minpos.second, _currentSlice.shape());
	if (! _iter.null()) {
		p += _iter->position();
	}
	this->_updateMinPos(p);
}

template <class T>
void MaskedLatticeStatsDataProvider<T>::_freeStorage() {
	_currentSlice.freeStorage (_currentPtr, _delData);
	_delData = False;
	_currentMaskSlice.freeStorage(_currentMaskPtr, _delMask);
	_delMask = False;
}

/*
template <class T>
uInt MaskedLatticeStatsDataProvider<T>::_nsteps() const {
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
*/

}


#endif
