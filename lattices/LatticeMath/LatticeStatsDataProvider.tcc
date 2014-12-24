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

template <class AccumType, class T, class InputIterator>
LatticeStatsDataProvider<AccumType, T, InputIterator>::LatticeStatsDataProvider(
	Lattice<T>& lattice
) : LatticeStatsDataProviderBase<AccumType, T, InputIterator>(),
	_iter(RO_LatticeIterator<T>(lattice)), _currentSlice(),
	_currentPtr(0), _delData(False) {}

template <class AccumType, class T, class InputIterator>
LatticeStatsDataProvider<AccumType, T, InputIterator>::~LatticeStatsDataProvider() {}

template <class AccumType, class T, class InputIterator>
void LatticeStatsDataProvider<AccumType, T, InputIterator>::operator++() {
	_freeStorage();
	++_iter;
}

template <class AccumType, class T, class InputIterator>
Bool LatticeStatsDataProvider<AccumType, T, InputIterator>::atEnd() const {
	return _iter.atEnd();
}

template <class AccumType, class T, class InputIterator>
void LatticeStatsDataProvider<AccumType, T, InputIterator>::finalize() {
	_freeStorage();
}

template <class AccumType, class T, class InputIterator>
uInt64 LatticeStatsDataProvider<AccumType, T, InputIterator>::getCount() {
	return _iter.cursor().size();
}

template <class AccumType, class T, class InputIterator>
InputIterator LatticeStatsDataProvider<AccumType, T, InputIterator>::getData() {
	_currentSlice.assign(_iter.cursor());
	_currentPtr = _currentSlice.getStorage(_delData);
	return _currentPtr;
}

template <class AccumType, class T, class InputIterator>
const Bool* LatticeStatsDataProvider<AccumType, T, InputIterator>::getMask() {
	return NULL;
}

template <class AccumType, class T, class InputIterator>
Bool LatticeStatsDataProvider<AccumType, T, InputIterator>::hasMask() const {
	return False;
}

template <class AccumType, class T, class InputIterator>
void LatticeStatsDataProvider<AccumType, T, InputIterator>::reset() {
	_iter.reset();
}

template <class AccumType, class T, class InputIterator>
void LatticeStatsDataProvider<AccumType, T, InputIterator>::_freeStorage() {
	_currentSlice.freeStorage (_currentPtr, _delData);
	_delData = False;
}

}

